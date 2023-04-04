import jsffi, macros
import sequtils, strutils
import std/compilesettings
import std/os

var module {.importc, nodecl.}: JsObject

var
  types {.compiletime.}: seq[string] = newSeq[string]()
  typesExports {.compiletime.}: seq[string] = newSeq[string]()

proc exportStatement(node: NimNode): NimNode =
  var name, id: NimNode
  if node.kind == nnkAsgn:
    name = node[0]
    id = node[1]
  elif node.kind == nnkIdent:
    name = node.toStrLit
    id = node
  else:
    echo "node: " & $node.treeRepr
    raise newException(ValueError, "not implemented")
  result = quote do:
    module.exports[`name`] = `id`


proc mapPrimitiveType(typeName: string): string =
  case typeName
  of "int", "int8", "int16", "int32", "int64", "uint", "uint8", "uint16", "uint32", "uint64":
    "number" & "/* " & typeName & " */"
  of "float", "float32", "float64":
    "number" & "/* " & typeName & " */"
  of "cstring":
    "string"
  of "bool":
    "boolean"
  of "JsObject":
    "any"
  else:
    if types.contains(typeName):
      typeName
    else:
      "any"

macro genTypeScript(body: typed) =
  var exportsStatements = newSeq[string]()
  
  for stm in body:
    if stm.kind == nnkCall and $stm[0].toStrLit == "[]=" :
      #echo "---------\n" & $stm[0].toStrLit
      let exportNameStr = $stm[2].toStrLit
      let exportName = exportNameStr[1 ..< exportNameStr.len - 1]
      #echo "export name: " & exportName
      #echo "proc   impl: " & $stm[3].getImpl.treeRepr

      let impl = stm[3].getImpl
      if impl.kind == nnkProcDef or impl.kind == nnkFuncDef:
        let formalParams = impl[3]
        let returnType = mapPrimitiveType($formalParams[0])
        var params = newSeq[string]()
        for i in 1 ..< formalParams.len:
          let indenDefs = formalParams[i]
          let argName = $indenDefs[0]
          let argType = if indenDefs[1].kind == nnkSym: $indenDefs[1].toStrLit else: $indenDefs[1][0].toStrLit
          params.add(argName & ": " & mapPrimitiveType(argType))
        exportsStatements.add "export function " & exportName & "(" & params.join(", ") & "): " & returnType 
      elif impl.kind == nnkIdentDefs:
        var constTypeSymbol = impl[0].getTypeImpl
        if (constTypeSymbol.kind == nnkRefTy):
          constTypeSymbol = constTypeSymbol[0]
        exportsStatements.add "export const " & exportName & ": " & mapPrimitiveType($constTypeSymbol.toStrLit)
      else:

        raise newException(ValueError, "not implemented")
      
  #  create the typescript declatation file
  let outDir = querySetting(SingleValueSetting.outDir)
  let outFile = querySetting(SingleValueSetting.outFile)
  # declaration file name is the same as the nim file name with .d.ts extension instead of .js
  let declarationFile = outDir & "/" & outFile.changeFileExt("d.ts")

  let content = typesExports.join("\n") & "\n" & exportsStatements.join("\n")

  writeFile(declarationFile, content)

  result = body



macro jsExportUntyped*(body: untyped): untyped =
  result = newStmtList()
  result.add(quote do:
    if module.exports == jsNull or module.exports == jsUndefined:
      module.exports = newJsObject()
  )

  if body.len == 0:
    result.add(exportStatement(body))
  for node in body:
    if node.len > 0:
      if node.kind == nnkAsgn:
        result.add(exportStatement(node))
      elif node.kind == nnkPar:
        for child in node:
          result.add(exportStatement(child))
    else:
      result.add(exportStatement(node))

proc exportType(node: NimNode): NimNode =
  var name, id: NimNode
  if node.kind == nnkAsgn:
    name = node[0]
    id = node[1]
  elif node.kind == nnkIdent:
    name = genSym(nskType, "xxxxType")
    id = node
  else:
    echo "node: " & $node.treeRepr
    raise newException(ValueError, "not implemented")
  result = quote do:
    type `name` = `id`

  


macro  gatherTypescriptTypes(body: untyped): untyped =
  result = newStmtList()
  if body.len == 0:
    result.add(exportType(body))
  for node in body:
    if node.len > 0:
      if node.kind == nnkAsgn:
        result.add(exportType(node))
      elif node.kind == nnkPar:
        for child in node:
          result.add(exportType(child))
    else:
      result.add(exportType(node))

macro genTypeScriptTypeDef(body: typed): untyped =
  result = newStmtList()
  for node in body:
    echo "======= " & node.treeRepr
    if node.kind == nnkTypeDef:
      let replacer = $node[0].symbol
      let typeName = if replacer == "xxxxType": $node[2].toStrLit else: replacer
      if not types.contains(typeName):
        types.add(typeName)
        typesExports.add("export interface " & typeName & " { \n/* \n" & $node[2].getTypeImpl.treeRepr & "\n */\n}")
    elif node.kind == nnkTypeSection:
      for child in node:
        let replacer = $child[0].symbol
        echo "replacer: " & replacer
        let typeName = if replacer == "xxxxType": $child[2].toStrLit else: replacer
        if not types.contains(typeName):
          types.add(typeName)
          typesExports.add("export interface " & typeName & " { \n/* \n" & $child[2].getTypeImpl.treeRepr & "\n */\n}")
    else:
      echo "not supported: " & node.treeRepr
      raise newException(ValueError, "not supported")
  



template jsExportTypes*(body: untyped): untyped =
  genTypeScriptTypeDef(gatherTypescriptTypes(body))

template jsExport*(body: untyped): untyped =
  genTypeScript(jsExportUntyped(body))

template jsSingleExport*(exported: untyped): untyped =
  module.exports = exported
