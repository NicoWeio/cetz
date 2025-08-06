// Test both circle and ellipse tangents
#import "src/lib.typ" as cetz

#cetz.canvas({
  import cetz.draw: *
  
  // Test circle first (should work as before)
  circle((0, 0), name: "c", radius: 0.5)
  line((tangent: (element: "c", point: (2, 1), solution: 1)), (2, 1), stroke: blue)
  line((tangent: (element: "c", point: (2, 1), solution: 2)), (2, 1), stroke: blue)
  
  // Test ellipse (new functionality)
  circle((3, 0), name: "e", radius: (0.25, 0.75))  // ellipse
  line((tangent: (element: "e", point: (5, 1), solution: 1)), (5, 1), stroke: red)
  line((tangent: (element: "e", point: (5, 1), solution: 2)), (5, 1), stroke: red)
})