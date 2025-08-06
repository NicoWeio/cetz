#import "../../src/lib.typ" as cetz
#import "../helper.typ": *

#test-case({
  import cetz.draw: *
  
  // Test circle tangent (should work with current implementation)
  circle((0, 0), name: "c", radius: 1)
  line((tangent: (element: "c", point: (3, 1), solution: 1)), (3, 1))
  line((tangent: (element: "c", point: (3, 1), solution: 2)), (3, 1))
})

#test-case({
  import cetz.draw: *
  
  // Test ellipse tangent (currently broken, should work after fix)
  circle((0, 0), name: "e", radius: (1, 0.5))  // ellipse: rx=1, ry=0.5
  line((tangent: (element: "e", point: (3, 1), solution: 1)), (3, 1))
  line((tangent: (element: "e", point: (3, 1), solution: 2)), (3, 1))
})