module Gen.AST.Data.Syntax.AWSRequest where

import qualified Control.Comonad as Comonad
import qualified Control.Lens as Lens
import Gen.AST.Data.Field (Field, fieldBody, fieldIsParam, fieldLit, fieldLitPayload, fieldLocation, fieldMaybe, fieldPayload, fieldStream)
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
import Gen.Types hiding (Config, operationPlugins, serviceConfig)
import qualified Language.Haskell.Exts as Exts

data Config = Config
  { -- | Name of the data type we generate to represent the input
    -- shape from botocore.
    requestType :: Id,
    -- | Name of the request function to call (@putBody@, @postQuery@, ...).
    -- from the @"Request"@ module.
    requestFunction :: Text,
    -- | List of function names to apply to the computed request in
    -- the implementation of the @request@ function.
    operationPlugins :: [Text],
    -- | Name of the data type we generate for the botocore output
    -- shape that corresponds to 'requestType'.
    responseType :: Id,
    -- | Name of the service config value to use by default. As of
    -- 2025-11, always @"defaultService"@; the parser for 'Metadata'
    -- would override the service abbrev in all cases.
    serviceConfig :: Text
  }

instanceD ::
  Config ->
  Metadata f ->
  (Ref, [Field]) ->
  Exts.Decl ()
instanceD c@Config {requestType} m (responseRef, responseFields) =
  instD "Core.AWSRequest" requestType $
    Just
      [ awsResponseD c,
        requestD c,
        funD "response" (responseE (m ^. protocol) responseRef responseFields)
      ]

awsResponseD :: Config -> Exts.InstDecl ()
awsResponseD Config {..} =
  Exts.InsType
    ()
    (tycon "AWSResponse" `tyapp` tycon (typeId requestType))
    (tycon (typeId responseType))

requestD :: Config -> Exts.InstDecl ()
requestD Config {..} =
  funArgsD "request" ["overrides"] $ foldr applyPlugin e operationPlugins
  where
    -- Plugin functions are of the form :: Request a -> Request a
    applyPlugin x = Exts.infixApp (var x) "Prelude.."

    e =
      Exts.app
        (var $ "Request." <> requestFunction)
        (Exts.app (var "overrides") (var serviceConfig))

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
