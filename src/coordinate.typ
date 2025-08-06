#import "vector.typ"
#import "util.typ"
#import "deps.typ"
#import deps.oxifmt: strfmt

#let resolve-xyz(c) = {
  // (x: <number> or <none>, y: <number> or <none>, z: <number> or <none>)
  // (x, y)
  // (x, y, z)
  
  return if type(c) == array {
    vector.as-vec(c)
  } else {
     (
      c.at("x", default: 0),
      c.at("y", default: 0),
      c.at("z", default: 0),
    )
  }
}


#let resolve-polar(c) = {
  // (angle: <angle>, radius: <number>)
  // (angle: <angle>, radius: (x, y))
  // (angle, radius)
  // (angle, (x-radius, y-radius))

  let (angle, xr, yr) = if type(c) == array {
    (
      c.first(),
      ..if type(c.last()) == array {
        c.last()
      } else {
        (c.last(), c.last())
      }
    )
  } else {
    (
      c.angle,
      ..if type(c.radius) == array {
        c.radius
      } else {
        (c.radius, c.radius)
      }
    )
  }
  return (
    xr * calc.cos(angle),
    yr * calc.sin(angle),
    0
  )
}


#let resolve-anchor(ctx, c) = {
  // (name: <string>, anchor: <number, angle, string> or <none>)
  // "name.anchor"
  // "name"
  let (name, anchor) = if type(c) == str {
    let (name, ..anchor) = c.split(".")
    if anchor.len() == 0 {
      anchor = "default"
    }
    (name, anchor)
  } else {
    (c.name, c.at("anchor", default: "default"))
  }

  // Check if node is known
  assert(name in ctx.nodes,
    message: "Unknown element '" + name + "' in elements " + repr(ctx.nodes.keys()))

  // Resolve length anchors
  if type(anchor) == length {
    anchor = util.resolve-number(ctx, anchor)
  }

  // Check if anchor is known
  let node = ctx.nodes.at(name)
  let pos = (node.anchors)(anchor)

  let pos = util.revert-transform(
    ctx.transform,
    pos
  )

  return pos
}

#let resolve-barycentric(ctx, c) = {
  // dictionary of numbers
  return vector.scale(
    c.bary.pairs().fold(
      (0, 0, 0),
      (vec, (k, v)) => {
          vector.add(
            vec,
            vector.scale(
              resolve-anchor(ctx, k),
              v
            )
          )
        }
      ),
    1 / c.bary.values().sum()
    )
}

#let resolve-relative(resolve, ctx, c) = {
  // (rel: <coordinate>, update: <bool> or <none>, to: <coordinate>)
  let update = c.at("update", default: true)
  let (ctx, rel) = resolve(ctx, c.rel, update: false)
  let (ctx, to) = if "to" in c {
      resolve(ctx, c.to, update: false)
    } else {
      (ctx, ctx.prev.pt)
    }
  c = vector.add(
    rel, 
    to,
  )
  c.insert(0, update)
  return c
}

#let resolve-tangent(resolve, ctx, c) = {
  // Handle wrapped tangent coordinates like (tangent: (element: "c", point: (2, 1), solution: 1))
  // or direct format like (element: "c", point: (2, 1), solution: 1)
  let tangent-data = if "tangent" in c {
    c.tangent
  } else {
    c
  }
  
  // (element: <string>, point: <coordinate>, solution: <integer>)

  let C = resolve-anchor(ctx, tangent-data.element)
  let (ctx, P) = resolve(ctx, tangent-data.point, update: false)
  
  // Get both radii to handle ellipses properly
  let ry = vector.len(vector.sub(resolve-anchor(ctx, tangent-data.element + ".north"), C))
  let rx = vector.len(vector.sub(resolve-anchor(ctx, tangent-data.element + ".east"), C))
  
  // Check if it's a circle (rx == ry) or an ellipse (rx != ry)
  let is-circle = calc.abs(rx - ry) < util.float-epsilon
  
  if is-circle {
    // Original circle tangent algorithm
    // https://stackoverflow.com/a/69641745/7142815
    let r = ry  // Use either radius for circle
    let D = vector.sub(P, C)
    let pc = vector.len(D)
    if pc < r {
      panic("No tangent solution for element " + tangent-data.element + " and point " + repr(tangent-data.point))
    }
    let d = r*r / pc
    let h = calc.sqrt(r*r - d*d)

    return if tangent-data.solution == 1 {
      (
        C.at(0) + (D.at(0) * d - D.at(1) * h) / pc,
        C.at(1) + (D.at(1) * d + D.at(0) * h) / pc,
        0
      )
    } else {
      (
        C.at(0) + (D.at(0) * d + D.at(1) * h) / pc,
        C.at(1) + (D.at(1) * d - D.at(0) * h) / pc,
        0
      )
    }
  } else {
    // Ellipse tangent using the standard mathematical formula
    // For ellipse x²/a² + y²/b² = 1 and external point (h, k)
    // The equation of chord of contact (which gives tangent points) is: hx/a² + ky/b² = 1
    
    let px = P.at(0) - C.at(0)  // h in standard notation
    let py = P.at(1) - C.at(1)  // k in standard notation
    
    let a = rx  
    let b = ry  
    
    // Check if point is outside ellipse
    let ellipse-test = (px * px) / (a * a) + (py * py) / (b * b)
    if ellipse-test <= 1 + util.float-epsilon {
      panic("No tangent solution for element " + tangent-data.element + " and point " + repr(tangent-data.point) + " (point must be outside ellipse)")
    }
    
    // The tangent lines from (px, py) to the ellipse can be found by
    // solving the system: hx/a² + ky/b² = 1 and x²/a² + y²/b² = 1
    
    // From the first equation: x = (a²/px)(1 - py*y/b²)
    // Substitute into ellipse equation: [(a²/px)(1 - py*y/b²)]²/a² + y²/b² = 1
    // Simplify: (a²/px²)(1 - py*y/b²)² + y²/b² = 1
    // (a²/px²)(1 - 2*py*y/b² + (py*y/b²)²) + y²/b² = 1
    // (a²/px²) - 2*a²*py*y/(px²*b²) + a²*py²*y²/(px²*b⁴) + y²/b² = 1
    
    // Rearranging: y²[a²*py²/(px²*b⁴) + 1/b²] - y[2*a²*py/(px²*b²)] + [a²/px² - 1] = 0
    
    let A_y = a*a*py*py/(px*px*b*b*b*b) + 1/(b*b)
    let B_y = -2*a*a*py/(px*px*b*b)
    let C_y = a*a/(px*px) - 1
    
    let discriminant_y = B_y*B_y - 4*A_y*C_y
    if discriminant_y < 0 {
      panic("No real tangent solution for element " + tangent-data.element + " and point " + repr(tangent-data.point))
    }
    
    let sqrt_disc_y = calc.sqrt(discriminant_y)
    let y1 = (-B_y + sqrt_disc_y) / (2 * A_y)
    let y2 = (-B_y - sqrt_disc_y) / (2 * A_y)
    
    // For each y, calculate corresponding x using: x = (a²/px)(1 - py*y/b²)
    let x1 = (a*a/px) * (1 - py*y1/(b*b))
    let x2 = (a*a/px) * (1 - py*y2/(b*b))
    
    // Choose the appropriate solution
    let (tx, ty) = if tangent-data.solution == 1 { (x1, y1) } else { (x2, y2) }
    
    // Transform back to original coordinate system
    return (C.at(0) + tx, C.at(1) + ty, 0)
  }
}

#let resolve-perpendicular(resolve, ctx, c) = {
  // (horizontal: <coordinate>, vertical: <coordinate>)
  // (horizontal, "-|", vertical)
  // (vertical, "|-", horizontal)

  let (ctx, horizontal, vertical) = resolve(ctx, ..if type(c) == array {
    if c.at(1) == "|-" {
      (c.first(), c.last())
    } else {
      // c.at(1) == "-|"
      (c.last(), c.first())
    }
  } else {
    (c.horizontal, c.vertical)
  }, update: false)

  return (
    horizontal.at(0),
    vertical.at(1),
    0
  )
}

#let resolve-lerp(resolve, ctx, c) = {
  // (a: <coordinate>, number: <number,ratio>,
  //  angle?: <angle>, b: <coordinate>)
  // (a, <number, ratio>, b)
  // (a, <number, ratio>, angle, b)

  let (a, number, angle, b) = if type(c) == array {
    if c.len() == 3 {
      (
        ..c.slice(0, 2),
        none, // angle
        c.last(),
      )
    } else {
      c
    }
  } else {
    (
      c.a,
      c.number,
      c.at("angle", default: 0deg),
      c.b
    )
  }

  (ctx, a, b) = resolve(ctx, a, b)

  if angle != none {
    let (x, y, _) = vector.sub(b,a)
    b = vector.add(
      (
        calc.cos(angle) * x - calc.sin(angle) * y,
        calc.sin(angle) * x + calc.cos(angle) * y,
        0
      ),
      a,
    )
  }

  let ab = vector.sub(b, a)

  let is-absolute = type(number) != ratio
  let distance = if is-absolute {
    let dist = vector.len(ab)
    if dist != 0 {
      util.resolve-number(ctx, number) / dist
    } else {
      0
    }
  } else {
    number / 100%
  }

  return vector.add(a, vector.scale(ab, distance))
}

#let resolve-function(resolve, ctx, c) = {
  let (func, ..c) = c
  (ctx, ..c) = resolve(ctx, ..c)
  func(..c)
}

#let resolve-pos(ctx, c) = {
  // (name: str, pos: float, auto?: left|right, swap?: bool)
}

/// Figures out what system a coordinate belongs to and returns the corresponding string.
/// - c (coordinate): The coordinate to find the system of.
/// -> str
#let resolve-system(ctx, c) = {
  let t = if type(c) == dictionary {
    let keys = c.keys()
    let len = c.len()
    if len in (1, 2, 3) and keys.all(k => k in ("x", "y", "z")) {
      "xyz"
    } else if len == 2 and keys.all(k => k in ("angle", "radius")) and (type(c.radius) in (int, float, length) or (type(c.radius) == array and c.radius.len() == 2)) {
      "polar"
    } else if len == 1 and keys == ("bary",) {
      "barycentric"
    } else if len in (1, 2) and keys.all(k => k in ("name", "anchor")) {
      "anchor"
    } else if len == 3 and keys.all(k => k in ("element", "point", "solution")) {
      "tangent"
    } else if len == 1 and keys == ("tangent",) {
      // Handle wrapped tangent coordinates like (tangent: (element: "c", point: (2, 1), solution: 1))
      "tangent"
    } else if len == 2 and keys.all(k => k in ("horizontal", "vertical")) {
      "perpendicular"
    } else if len in (1, 2, 3) and keys.all(k => k in ("rel", "to", "update")) {
      "relative"
    } else if len in (3, 4) and keys.all(k => k in ("a", "number", "angle", "abs", "b")) {
      "lerp"
    }
  } else if type(c) == array {
    let len = c.len()
    let types = c.map(type)
    if len == 0 {
      "previous"
    } else if len in (2, 3) and types.all(t => t in (int, float, length)) {
      "xyz"
    } else if len == 2 and types.first() == angle {
      "polar"
    } else if len == 3 and c.at(1) in ("-|", "|-") {
      "perpendicular"
    } else if len in (3, 4) and types.at(1) in (int, float, length, ratio) and (len == 3 or (len == 4 and types.at(2) == angle)) {
      "lerp"
    } else if len >= 2 and types.first() == function {
      "function"
    }
  } else if type(c) == str {
    if c.contains(".") {
      "anchor"
    } else {
      "element"
    }
  } else if ctx.at("resolve-system", default: none) != none {
    ctx.resolve-system = none
    resolve-system(ctx, c)
  }

  if t == none {
    panic("Failed to resolve coordinate: " + repr(c))
  }
  return t
}

/// Resolve a list of coordinates to absolute vectors. Returns an array of the new <Type>context</Type> then the resolved coordinate vectors.
///
/// ```typc example
/// line((0,0), (1,1), name: "l")
/// get-ctx(ctx => {
///   // Get the vector of coordinate "l.start" and "l.end"
///   let (ctx, a, b) = cetz.coordinate.resolve(ctx, "l.start", "l.end")
///   content("l.start", [#a], frame: "rect", stroke: none, fill: white)
///   content("l.end",   [#b], frame: "rect", stroke: none, fill: white)
/// })
/// ```
///
/// - ctx (context): Canvas context object
/// - ..coordinates (coordinate): List of coordinates
/// - update (bool): Update the context's last position
/// -> array
#let resolve(ctx, ..coordinates, update: true) = {
  let resolver = if type(ctx.resolve-coordinate) == array {
    ctx.resolve-coordinate
  } else {
    ()
  }

  let result = ()
  for c in coordinates.pos() {
    for i in range(1, resolver.len() + 1) {
      c = (resolver.at(resolver.len() - i))(ctx, c)
    }

    let t = resolve-system(ctx, c)
    c = if t == "xyz" {
      resolve-xyz(c)
    } else if t == "previous" {
      ctx.prev.pt
    } else if t == "polar" {
      resolve-polar(c)
    } else if t == "barycentric" {
      resolve-barycentric(ctx, c)
    } else if t in ("element", "anchor") {
      resolve-anchor(ctx, c)
    } else if t == "tangent" {
      resolve-tangent(resolve, ctx, c)
    } else if t == "perpendicular" {
      resolve-perpendicular(resolve, ctx, c)
    } else if t == "relative" {
      (update, ..c) = resolve-relative(resolve, ctx, c)
      c
    } else if t == "lerp" {
      resolve-lerp(resolve, ctx, c)
    } else if t == "function" {
      resolve-function(resolve, ctx, c)
    } else {
      panic("Failed to resolve coordinate of format: " + repr(c))
    }.map(util.resolve-number.with(ctx))

    if update {
      ctx.prev.pt = c
    }

    result.push(c)
  }

  return (ctx, ..result)
}
