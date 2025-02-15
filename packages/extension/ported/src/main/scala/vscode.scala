package ported

import scala.scalajs.js
import scala.scalajs.js.annotation.*

package object vscode:
  @js.native @JSImport("vscode", "Position")
  class Position(line: Int, character: Int) extends js.Object

  @js.native @JSImport("vscode", "Range")
  class Range(start: Position, end: Position) extends js.Object

  @js.native @JSImport("vscode", "SymbolKind")
  object SymbolKind extends js.Object:
    val File: Int = js.native
    val Module: Int = js.native
    val Namespace: Int = js.native
    val Package: Int = js.native
    val Class: Int = js.native
    val Method: Int = js.native
    val Property: Int = js.native
    val Field: Int = js.native
    val Constructor: Int = js.native
    val Enum: Int = js.native
    val Interface: Int = js.native
    val Function: Int = js.native
    val Variable: Int = js.native
    val Constant: Int = js.native
    val String: Int = js.native
    val Number: Int = js.native
    val Boolean: Int = js.native
    val Array: Int = js.native
    val Object: Int = js.native
    val Key: Int = js.native
    val Null: Int = js.native
    val EnumMember: Int = js.native
    val Struct: Int = js.native
    val Event: Int = js.native
    val Operator: Int = js.native
    val TypeParameter: Int = js.native

  @js.native @JSImport("vscode", "DocumentSymbol")
  class DocumentSymbol(
      name: String,
      detail: String,
      symbolKind: Int,
      range: Range,
      selectionRange: Range
  ) extends js.Object:
    var children: js.Array[DocumentSymbol] = js.native

  @js.native @JSImport("vscode", "Uri")
  class Uri extends js.Object

  @js.native @JSImport("vscode", "Location")
  class Location(uri: Uri, range: Range) extends js.Object

  @js.native @JSImport("vscode", "SymbolInformation")
  class SymbolInformation(
      name: String,
      kind: Int,
      containerName: String,
      location: Location
  ) extends js.Object

  @js.native
  trait TextDocument extends js.Object:
    def uri: Uri
    def getText(): String
    def fileName: String

  @js.native
  trait CancellationToken extends js.Object

  @js.native @JSImport("vscode", "LogLevel")
  object LogLevel extends js.Object:
    val Off: Int = js.native
    val Trace: Int = js.native
    val Debug: Int = js.native
    val Info: Int = js.native
    val Warning: Int = js.native
    val Error: Int = js.native

  @js.native
  trait LogOutputChannel extends js.Object:
    def logLevel: Int
    def trace(message: String, args: js.Any*): Unit
    def debug(message: String, args: js.Any*): Unit
    def info(message: String, args: js.Any*): Unit
    def warn(message: String, args: js.Any*): Unit
    def error(message: String, args: js.Any*): Unit
