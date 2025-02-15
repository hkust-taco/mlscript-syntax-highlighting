package object hkmc2:
  case class Name(value: String = "")
  case class Line(value: Int = 0)
  
  given Name = Name()
  given Line = Line()
