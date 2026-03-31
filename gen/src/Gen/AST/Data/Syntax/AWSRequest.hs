module Gen.AST.Data.Syntax.AWSRequest
  ( instanceD,
    Config (..),
    ResponseReceiver (..),
    ResponseFieldParser (..),
    HeaderFieldParser (..),
  )
where

import Gen.AST.Data.Field (Field, fieldBody, fieldIsParam, fieldLit, fieldLitPayload, fieldLocation, fieldMaybe, fieldPayload, fieldStream)
import Gen.AST.Data.Syntax
  ( ctorE,
    funArgsD,
    funD,
    instD,
    justE,
    memberName,
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
import Gen.Types hiding (Config, serviceConfig)
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
    requestOperationPlugins :: [Text],
    -- | Name of the data type we generate for the botocore output
    -- shape that corresponds to 'requestType'.
    responseType :: Id,
    -- | How to receive and parse the response from AWS.
    responseReceiver :: ResponseReceiver,
    -- | Name of the service config value to use by default. As of
    -- 2025-11, always @"defaultService"@; the parser for 'Metadata'
    -- would override the service abbrev in all cases.
    serviceConfig :: Text
  }
  deriving (Show)

-- | Which Amazonka function should be used to parse AWS's response to
-- an API call, and additional arguments are necessary to actually
-- perform the parse.
data ResponseReceiver
  = -- | Perform no parsing and return an empty response using
    -- @receiveNull@. The response constructor in 'responseType' must
    -- take no arguments.
    ReceiveNull
  | -- | Parse the entire response as JSON using @receiveJSON@.
    ReceiveJsonAll
  | -- | Parse the entire response as (optionally wrapped) XML, using
    -- @receiveXML@ or @receiveXMLWrapper@.
    ReceiveXmlAll (Maybe Text)
  | -- | Parse a streaming response from AWS using @receiveStreamingBody@.
    ReceiveStreamingBody [ResponseFieldParser]
  | -- | Parse a response from AWS, accepting the body as an unparsed
    -- 'ByteString'. Uses @receiveBytes@.
    ReceiveBytes [ResponseFieldParser]
  | -- | Parse (optionally wrapped) XML response by field using either
    -- @receiveXML@ or @receiveXMLWrapper@.
    ReceiveXml (Maybe Text) [ResponseFieldParser]
  | FigureItOut
  deriving (Show)

-- | How to generate the parser for a single field, for response
-- parsers which do per-field parsing.
data ResponseFieldParser
  = -- | Parse the repsonse field from the HTTP response headers named
    -- like the given 'Text'.
    ParseHeaderField Text HeaderFieldParser
  | FigureTheFieldOut Field
  deriving (Show)

-- | How to parse a single field from the HTTP response headers.
--
-- A 'HeaderFieldParser' does not parse a single header, because some
-- AWS services represent structured data by breaking it across
-- multiple similarly-named headers.
data HeaderFieldParser
  = -- | Parse a required field from headers using @(.#)@.
    HeaderFieldRequired
  | -- | Parse an optional field from headers using @(.#?)@.
    HeaderFieldOptional
  | -- | Parse a map field from headers using @parseHeadersMap@.
    HeaderFieldMap
  deriving (Show)

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
        funD "response" (responseE c (m ^. protocol) responseRef responseFields)
      ]

awsResponseD :: Config -> Exts.InstDecl ()
awsResponseD Config {..} =
  Exts.InsType
    ()
    (tycon "AWSResponse" `tyapp` tycon (typeId requestType))
    (tycon (typeId responseType))

requestD :: Config -> Exts.InstDecl ()
requestD Config {..} =
  funArgsD "request" ["overrides"] $ foldr applyPlugin e requestOperationPlugins
  where
    -- Plugin functions are of the form :: Request a -> Request a
    applyPlugin x = Exts.infixApp (var x) "Prelude.."

    e =
      Exts.app
        (var $ "Request." <> requestFunction)
        (Exts.app (var "overrides") (var serviceConfig))

responseE :: Config -> Protocol -> Ref -> [Field] -> Exts.Exp ()
responseE Config {..} p r fs =
  case responseReceiver of
    ReceiveNull ->
      var "Response.receiveNull" `Exts.app` var (ctorId responseType)
    ReceiveJsonAll ->
      var "Response.receiveJSON"
        `Exts.app` lam (var "Data.eitherParseJSON" `Exts.app` var "x")
    ReceiveXmlAll Nothing ->
      var "Response.receiveXML"
        `Exts.app` lam (var "Data.parseXML" `Exts.app` var "x")
    ReceiveXmlAll (Just wrapper) ->
      var "Response.receiveXMLWrapper"
        `Exts.app` str wrapper
        `Exts.app` lam (var "Data.parseXML" `Exts.app` var "x")
    ReceiveStreamingBody fieldParsers ->
      var "Response.receiveStreamingBody"
        `Exts.app` lam (ctorE responseType $ map parseField fieldParsers)
    ReceiveBytes fieldParsers ->
      var "Response.receiveBytes"
        `Exts.app` lam (ctorE responseType $ map parseField fieldParsers)
    ReceiveXml Nothing fieldParsers ->
      var "Response.receiveXML"
        `Exts.app` lam (ctorE responseType $ map parseField fieldParsers)
    ReceiveXml (Just wrapper) fieldParsers ->
      var "Response.receiveXMLWrapper"
        `Exts.app` str wrapper
        `Exts.app` lam (ctorE responseType $ map parseField fieldParsers)
    FigureItOut -> Exts.app responseF bdy
  where
    bdy :: Exts.Exp ()
    bdy = lam . ctorE (identifier r) $ map parseField' fs

    lam :: Exts.Exp () -> Exts.Exp ()
    lam = Exts.lamE [Exts.pvar "s", Exts.pvar "h", Exts.pvar "x"]

    parseField :: ResponseFieldParser -> Exts.Exp ()
    parseField = \case
      ParseHeaderField hName hField -> case hField of
        HeaderFieldRequired -> Exts.infixApp (var "h") "Data..#" (str hName)
        HeaderFieldOptional -> Exts.infixApp (var "h") "Data..#?" (str hName)
        HeaderFieldMap ->
          Exts.appFun (var "Data.parseHeadersMap") [str hName, var "h"]
      FigureTheFieldOut field -> parseField' field

    parseField' :: Field -> Exts.Exp ()
    parseField' x =
      case fieldLocation x of
        Just Headers -> parseHeadersE (memberName p Output x) (typeOf x)
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

    -- FIXME: take method into account for responses, such as HEAD
    -- etc, particuarly when the body might be totally empty.
    responseF :: Exts.Exp ()
    responseF
      | not $ any fieldBody fs = trace "responseF(receiveEmpty)" $ var "Response.receiveEmpty"
      | otherwise = trace "responseF(var suf)" $ var suf
      where
        suf = "Response.receive" <> Proto.suffix p

parseHeadersE :: Text -> TType -> Exts.Exp ()
parseHeadersE headerName = \case
  TMap {} -> Exts.appFun (var "Data.parseHeadersMap") [n, h]
  TMaybe {} -> Exts.infixApp h "Data..#?" n
  _ -> Exts.infixApp h "Data..#" n
  where
    n = str headerName
    h = var "h"

parseStatusE :: Field -> Exts.Exp ()
parseStatusE f
  | fieldMaybe f = Exts.app pureE (Exts.app justE v)
  | otherwise = Exts.app pureE v
  where
    v = Exts.paren $ Exts.app (var "Prelude.fromEnum") (var "s")
