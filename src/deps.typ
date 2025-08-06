// Temporary replacement for oxifmt for testing purposes
#let strfmt(template, ..args) = {
  // Simple string formatting - just concatenate for testing
  let result = template
  for arg in args.pos() {
    let arg-str = if type(arg) == angle {
      repr(arg)
    } else {
      str(arg)
    }
    result = result.replace("{}", arg-str)
  }
  result
}

#let oxifmt = (
  strfmt: strfmt
)
