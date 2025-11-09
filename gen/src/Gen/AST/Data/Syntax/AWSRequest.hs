module Gen.AST.Data.Syntax.AWSRequest where

import qualified Control.Comonad as Comonad
import qualified Control.Lens as Lens
import qualified Data.HashMap.Strict as HashMap
import Gen.AST.Data.Field (Field, fieldBody, fieldIsParam, fieldLit, fieldLitPayload, fieldLocation, fieldMaybe, fieldPayload, fieldStream)
import Gen.AST.Data.Instance (Inst (..))
import Gen.AST.Data.Syntax
  ( ctorE,
    decodeE,
    funArgsD,
    funD,
    instD,
    justE,
    memberName,
    pH,
    pHMap,
    pHMay,
    pJE,
    pJEDef,
    pJEMay,
    parseJSONE,
    parseXMLE,
    pureE,
    str,
    tyapp,
    tycon,
    var,
  )
import Gen.Prelude
import qualified Gen.Protocol as Proto
import Gen.Types
import qualified Language.Haskell.Exts as Exts

instanceD ::
  (HasMetadata a Identity) =>
  Config ->
  a ->
  HTTP ->
  (Ref, [Inst]) ->
  (Ref, [Field]) ->
  Exts.Decl ()
instanceD c m h (requestRef, requestInstances) (responseRef, responseFields) =
  instD
    "Core.AWSRequest"
    (identifier requestRef)
    $ Just
      [ assocD (identifier requestRef) "AWSResponse" (typeId (identifier responseRef)),
        funArgsD "request" ["overrides"] (requestF c m h requestRef requestInstances),
        funD "response" (responseE (m ^. protocol) responseRef responseFields)
      ]

assocD :: Id -> Text -> Text -> Exts.InstDecl ()
assocD n x y = Exts.InsType () (tyapp (tycon x) (tycon (typeId n))) (tycon y)

requestF ::
  (HasMetadata a Identity) =>
  Config ->
  a ->
  HTTP ->
  Ref ->
  [Inst] ->
  Exts.Exp ()
requestF c meta h r is =
  maybe e (foldr applyPlugin e) selectedPlugins
  where
    applyPlugin x =
      -- Plugin functions are of the form :: Request a -> Request a
      Exts.infixApp (var x) "Prelude.."

    selectedPlugins =
      -- Lookup a specific operationPlugins key before the wildcard.
      HashMap.lookup (identifier r) (c ^. operationPlugins)
        <|> HashMap.lookup (mkId "*") (c ^. operationPlugins)

    e = Exts.app v (Exts.app (var "overrides") (var $ meta ^. serviceConfig))

    v =
      var
        . mappend ("Request." <> methodToText m)
        . fromMaybe mempty
        . listToMaybe
        $ mapMaybe f is

    f = \case
      ToBody {} -> Just "Body"
      ToJSON {} -> Just "JSON"
      ToElement {} -> Just "XML"
      _
        | p == Query,
          m == POST ->
            Just "Query"
      _
        | p == EC2,
          m == POST ->
            Just "Query"
      _ -> Nothing

    m = h ^. method
    p = meta ^. protocol

responseE :: Protocol -> Ref -> [Field] -> Exts.Exp ()
responseE p r fs = Exts.app (responseF p r fs) bdy
  where
    n = r ^. Lens.to identifier
    s = r ^. refAnn . Lens.to Comonad.extract

    bdy :: Exts.Exp ()
    bdy
      | null fs = var (ctorId n)
      | isShared s, all fieldBody fs = lam parseAll
      | otherwise = lam . ctorE n $ map parseField fs

    lam :: Exts.Exp () -> Exts.Exp ()
    lam = Exts.lamE [Exts.pvar "s", Exts.pvar "h", Exts.pvar "x"]

    parseField :: Field -> Exts.Exp ()
    parseField x =
      case fieldLocation x of
        Just Headers -> parseHeadersE p x
        Just Header -> parseHeadersE p x
        Just StatusCode -> parseStatusE x
        Just Body | body -> Exts.app pureE (var "x")
        Nothing | body -> Exts.app pureE (var "x")
        _ -> parseProto x

    parseProto :: Field -> Exts.Exp ()
    parseProto f =
      case p of
        _ | f ^. fieldPayload -> parseOne f
        JSON -> parseJSONE p pJE pJEMay pJEDef f
        RestJSON -> parseJSONE p pJE pJEMay pJEDef f
        APIGateway -> parseJSONE p pJE pJEMay pJEDef f
        _ -> parseXMLE p f

    parseOne :: Field -> Exts.Exp ()
    parseOne f
      | fieldLit f =
          if fieldIsParam f
            then Exts.app (var "Prelude.pure") (var "x")
            else -- Coerce is inserted here to handle newtypes such as Sensitive.

              Exts.app (var "Prelude.pure")
                . Exts.paren
                . Exts.app justE
                . Exts.paren
                . Exts.app (var "Prelude.coerce")
                $ var "x"
      -- This ensures anything which is set as a payload,
      -- but is a primitive type is just consumed as a bytestring.
      | otherwise = parseAll

    parseAll :: Exts.Exp ()
    parseAll =
      flip Exts.app (var "x") $
        if any fieldLitPayload fs
          then var "Prelude.pure"
          else case p of
            JSON -> var "Data.eitherParseJSON"
            RestJSON -> var "Data.eitherParseJSON"
            APIGateway -> var "Data.eitherParseJSON"
            _ -> var "Data.parseXML"

    body = any fieldStream fs

-- FIXME: take method into account for responses, such as HEAD etc, particuarly
-- when the body might be totally empty.
responseF :: Protocol -> RefF a -> [Field] -> Exts.Exp ()
responseF p r fs
  | null fs = var "Response.receiveNull"
  | any fieldStream fs = var "Response.receiveBody"
  | any fieldLitPayload fs = var "Response.receiveBytes"
  | Just x <- r ^. refResultWrapper = Exts.app (var (suf <> "Wrapper")) (str x)
  | not $ any fieldBody fs = var "Response.receiveEmpty"
  | otherwise = var suf
  where
    suf = "Response.receive" <> Proto.suffix p

parseHeadersE :: Protocol -> Field -> Exts.Exp ()
parseHeadersE p f
  | TMap {} <- typeOf f = Exts.appFun pHMap [str n, h]
  | fieldMaybe f = decodeE h pHMay n
  | otherwise = decodeE h pH n
  where
    n = memberName p Output f
    h = var "h"

parseStatusE :: Field -> Exts.Exp ()
parseStatusE f
  | fieldMaybe f = Exts.app pureE (Exts.app justE v)
  | otherwise = Exts.app pureE v
  where
    v = Exts.paren $ Exts.app (var "Prelude.fromEnum") (var "s")
