package ported

import scala.scalajs.js
import scala.scalajs.js.annotation.*
import vscode.*
import hkmc2.{FastParseHelpers, Loc, syntax}
import syntax.{Apps, Keyword, Tree, TypeDefKind}

@JSExportTopLevel("MLscriptSymbolProvider")
class MLscriptSymbolProvider(
    extensionLogOutput: LogOutputChannel,
    providerLogOutput: LogOutputChannel
):
  extension (loc: Loc)
    def toRange(using fph: FastParseHelpers): Range =
      val (startLine, _, startCol) = fph.getLineColAt(loc.spanStart)
      val (endLine, _, endCol) = fph.getLineColAt(loc.spanEnd)
      val start = new Position(startLine - 1, startCol - 1)
      val end = new Position(endLine - 1, endCol - 1)
      new Range(start, end)

  def collectSymbols(parentKind: Option[TypeDefKind], trees: List[Tree])(using FastParseHelpers): js.Array[DocumentSymbol] =
    val symbols = js.Array[DocumentSymbol]()
    def add(name: String, detail: String, kind: Int, nameLocOpt: Option[Loc], treeLocOpt: Option[Loc], children: js.Array[DocumentSymbol]): Unit =
      (nameLocOpt, treeLocOpt.orElse(nameLocOpt)) match
        case (Some(nameLoc), Some(treeLoc)) =>
          providerLogOutput.info(s"Adding symbol for $name")
          val symbol = new DocumentSymbol(name, detail, kind, treeLoc.toRange, nameLoc.toRange)
          symbol.children = children
          symbols.push(symbol)
        case _ =>
          providerLogOutput.warn(s"Failed to add symbol for $name")
    def processOpen(tree: Tree): Unit = tree match
      case id @ Tree.Ident(name) =>
        add(name, "opened module", SymbolKind.Module, id.toLoc, tree.toLoc, js.Array())
      case Tree.Jux(id @ Tree.Ident(name), body: Tree.Block) =>
        add(name, "opened module", SymbolKind.Module, id.toLoc, tree.toLoc, js.Array())
      case _ => ()
    def go(tree: Tree): Unit = tree match
      case Tree.Empty() => ()
      case Tree.Error() => ()
      case Tree.Dummy => ()
      case Tree.Under() => ()
      case Tree.Unt() => ()
      case Tree.Ident(name) => ()
      case Tree.Keywrd(kw) => ()
      case Tree.IntLit(value) => ()
      case Tree.DecLit(value) => ()
      case Tree.StrLit(value) => ()
      case Tree.UnitLit(isNullNotUndefined) => ()
      case Tree.BoolLit(value) => ()
      case Tree.Bra(k, inner) => ()
      case Tree.Block(stmts) => stmts.foreach(go)
      case Tree.OpBlock(items) => ()
      case Tree.LetLike(Keyword.`let`, Apps(id @ Tree.Ident(name), _), rhs, body) =>
        add(name, "let binding", SymbolKind.Variable, id.toLoc, tree.toLoc, js.Array())
        rhs.foreach(go)
        body.foreach(go)
      case Tree.LetLike(_, _, _, _) => ()
      case Tree.Hndl(lhs, cls, defs, body) => ()
      case Tree.Def(lhs, rhs) => ()
      case td @ Tree.TermDef(k, head, rhs) =>
        providerLogOutput.info(s"TermDef: ${td.describe}")
        val kind = k match
          case syntax.ImmutVal => SymbolKind.Variable
          case syntax.MutVal => SymbolKind.Variable
          case syntax.LetBind => SymbolKind.Variable
          case syntax.HandlerBind => SymbolKind.Variable
          case syntax.ParamBind => SymbolKind.Variable
          case syntax.Fun => parentKind match
            case Some(syntax.Cls) =>
              rhs.foreach(go)
              SymbolKind.Method
            case _ => SymbolKind.Function
          case syntax.Ins => SymbolKind.Method
        providerLogOutput.info(s"kind = $k, name = ${td.name}")
        td.name match
          case Right(id @ Tree.Ident(name)) =>
            add(name, k.desc, kind, id.toLoc, td.toLoc, js.Array())
          case Left(_) => ()
      case td @ Tree.TypeDef(k, head, _, body) =>
        providerLogOutput.info(s"TermDef: ${td.describe}")
        val kind = k match
          case syntax.Cls => SymbolKind.Class
          case syntax.Trt => SymbolKind.Interface
          case syntax.Mxn => SymbolKind.Interface
          case syntax.Als => SymbolKind.Interface
          case syntax.Mod => SymbolKind.Module
          case syntax.Obj => SymbolKind.Object
          case syntax.Pat => SymbolKind.Variable
        providerLogOutput.info(s"kind = $k, name = ${td.name}")
        td.name match
          case Right(id @ Tree.Ident(name)) =>
            val children = collectSymbols(Some(k), body.toList)
            add(name, k.desc, kind, id.toLoc, td.toLoc, children)
          case Left(_) => ()
      case Tree.Open(opened) => processOpen(opened)
      case Tree.OpenIn(opened, body) => processOpen(opened)
      case Tree.DynAccess(obj, fld, arrayIdx) => ()
      case Tree.Modified(modifier, modLoc, body) => go(body)
      case Tree.Quoted(body) => ()
      case Tree.Unquoted(body) => ()
      case Tree.Tup(fields) => ()
      case Tree.TyTup(tys) => ()
      case Tree.App(lhs, rhs) => ()
      case Tree.Jux(lhs, rhs) => ()
      case Tree.SynthSel(prefix, name) => ()
      case Tree.Sel(prefix, name) => ()
      case Tree.MemberProj(cls, name) => ()
      case Tree.InfixApp(lhs, kw, rhs) => ()
      case Tree.New(body, rft) => ()
      case Tree.IfLike(kw, kwLoc, split) => ()
      case Tree.IfElse(cond, alt) => ()
      case Tree.Case(kwLoc, branches) => ()
      case Tree.Region(name, body) => ()
      case Tree.RegRef(reg, value) => ()
      case Tree.Effectful(eff, body) => ()
      case Tree.Outer(name) => ()
      case Tree.Spread(kw, kwLoc, body) => ()
      case Tree.Annotated(_, target) => go(target)
    trees.foreach(go)
    providerLogOutput.info(s"Collected ${symbols.length} symbols")
    symbols
  
  private val flagsPattern = js.RegExp("""^:.+$""", "gm")
  
  /** Remove DiffTests flags from the document text. This is necessary because
    * the parser does not recognize them as valid syntax.
    */
  private def getCleanSourceText(document: TextDocument): String =
    val originalText: js.Dynamic = document.getText().asInstanceOf[js.Dynamic]
    originalText.replaceAll(flagsPattern, "").asInstanceOf[String]
  
  @JSExport
  def provideDocumentSymbols(document: TextDocument, token: CancellationToken): js.Array[DocumentSymbol] =
    import hkmc2.{Diagnostic, FastParseHelpers, Origin, Raise, syntax, semantics}
    extensionLogOutput.info(s"Provide symbols for ${document.fileName}")
    val startTime = js.Date.now()
    val text = getCleanSourceText(document)
    given fph: FastParseHelpers = new FastParseHelpers(text)
    val origin = Origin(text, 0, fph)
    given raise: Raise = d => d.kind match
      case Diagnostic.Kind.Error => extensionLogOutput.error(d.toString)
      case Diagnostic.Kind.Warning => extensionLogOutput.warn(d.toString)
      case Diagnostic.Kind.Internal => extensionLogOutput.error(d.toString)
    val lexer = new hkmc2.syntax.Lexer(origin, dbg = false)
    val tokens = lexer.bracketedTokens
    providerLogOutput.info(s"Lexer produces ${tokens.length} tokens in ${js.Date.now() - startTime} ms")
    given state: semantics.Elaborator.State = semantics.Elaborator.State()
    val rules = syntax.ParseRules()
    val p = new syntax.Parser(origin, tokens, rules, raise, dbg = false):
      def doPrintDbg(msg: => String): Unit = if dbg then providerLogOutput.debug(msg)
    given line: hkmc2.Line = hkmc2.Line()
    val res = p.parseAll(p.block(allowNewlines = true))
    extensionLogOutput.info(s"Parsing done: ${res.length} top-level trees")
    val symbols = collectSymbols(None, res)
    val elapsed = js.Date.now() - startTime
    extensionLogOutput.info(s"Collected ${symbols.length} symbols in $elapsed ms")
    symbols
