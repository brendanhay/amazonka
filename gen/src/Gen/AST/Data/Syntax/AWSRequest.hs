module Gen.AST.Data.Syntax.AWSRequest
  ( instanceD,
    Config (..),
    ResponseReceiver (..),
    ResponseField (..),
    HeaderField (..),
    BodyField (..),
    JsonField (..),
    XmlField (..),
    XmlCollection (..),
    Presence (..),
    Sensitivity (..),
  )
where

import Gen.AST.Data.Syntax
  ( ctorE,
    funArgsD,
    funD,
    instD,
    justE,
    pureE,
    str,
    tyapp,
    tycon,
    var,
  )
import Gen.Prelude
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
    ReceiveStreamingBody [ResponseField]
  | -- | Parse a response from AWS, accepting the body as an unparsed
    -- 'ByteString'. Uses @receiveBytes@.
    ReceiveBytes [ResponseField]
  | -- | Parse a JSON response from AWS fieldwise using @receiveJSON@.
    ReceiveJson [ResponseField]
  | -- | Parse (optionally wrapped) XML response fieldwise using either
    -- @receiveXML@ or @receiveXMLWrapper@.
    ReceiveXml (Maybe Text) [ResponseField]
  | -- | Parse an empty response using @receiveEmpty@. The body will
    -- not be available to parse and all fields will come from headers
    -- or status.
    ReceiveEmpty [ResponseField]
  deriving (Show)

-- | How to generate the parser for a single field, for response
-- parsers which do per-field parsing.
data ResponseField
  = -- | Parse the repsonse field from the HTTP response headers named
    -- like the given 'Text'.
    ResponseFieldHeader HeaderField
  | -- | Parse the status code from the HTTP response.
    ResponseFieldStatus
  | -- | Parse the field from the response body as a whole.
    ResponseFieldBody BodyField
  | -- | Parse one member from a JSON response body.
    ResponseFieldJson JsonField
  | -- | Parse one member from an XML response body.
    ResponseFieldXml XmlField
  deriving (Show)

-- | How to parse a single field from the HTTP response headers.
--
-- A 'HeaderFieldParser' does not parse a single header, because some
-- AWS services represent structured data by breaking it across
-- multiple similarly-named headers.
data HeaderField
  = -- | Parse a single field from headers using @(.#)@ or @(.#?)@.
    HeaderSingle Text Presence
  | -- | Parse a map field from headers using @parseHeadersMap@.
    HeaderMap Text
  deriving (Show)

-- | How to consume the HTTP response body when installing it into one
-- field of an Amazonka response record.
data BodyField
  = -- | @pure x@
    PureBody
  | -- | @pure (Just (coerce x))@
    PureCoercedBody
  | -- | Parse the entire body as a single JSON document using
    -- @eitherParseJSON x@
    JsonBody
  | -- | Parse the enture body as a single XML document using
    -- @parseXML x@
    XmlBody
  deriving (Show)

-- | How to parse one member of a JSON response body.
--
-- 'JsonMonoid' emits a @.!\@ mempty@ for list and map members even
-- when the member is required.
data JsonField
  = -- | Single items: @x .:> name@ or @x .?> name@
    JsonSingle Text Presence
  | -- | @x .?> name .!\@ mempty@
    JsonMonoid Text
  deriving (Show)

-- | How to parse one member of an XML response body.
data XmlField
  = -- | Single items: @x .\@ name@ or @x .\@? name@.
    --
    -- We do not carry a 'Sensitivity' here because
    -- 'Amazonka.Data.Sensitive.Sensitive' has a @newtype@-derived
    -- instance of 'Amazonka.Data.XML.FromXML'.
    XmlElement Text Presence
  | -- | Collections, optionally inside a wrapper element.
    XmlCollection (Maybe Text) XmlCollection Sensitivity Presence
  deriving (Show)

data XmlCollection
  = XmlList Text
  | XmlList1 Text
  | XmlMap {entry :: Text, key :: Text, value :: Text}
  deriving (Show)

data Presence = Optional | Required deriving (Show)

data Sensitivity = NotSensitive | Sensitive deriving (Show)

instanceD :: Config -> Exts.Decl ()
instanceD c@Config {requestType} =
  instD "Core.AWSRequest" requestType $
    Just
      [ awsResponseD c,
        requestD c,
        funD "response" (responseE c)
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

responseE :: Config -> Exts.Exp ()
responseE Config {..} =
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
    ReceiveJson fieldParsers ->
      var "Response.receiveJSON"
        `Exts.app` lam (ctorE responseType $ map parseField fieldParsers)
    ReceiveXml Nothing fieldParsers ->
      var "Response.receiveXML"
        `Exts.app` lam (ctorE responseType $ map parseField fieldParsers)
    ReceiveXml (Just wrapper) fieldParsers ->
      var "Response.receiveXMLWrapper"
        `Exts.app` str wrapper
        `Exts.app` lam (ctorE responseType $ map parseField fieldParsers)
    ReceiveEmpty fieldParsers ->
      var "Response.receiveEmpty"
        `Exts.app` lam (ctorE responseType $ map parseField fieldParsers)
  where
    lam :: Exts.Exp () -> Exts.Exp ()
    lam = Exts.lamE [Exts.pvar "s", Exts.pvar "h", Exts.pvar "x"]

    parseField :: ResponseField -> Exts.Exp ()
    parseField = \case
      ResponseFieldHeader hField -> case hField of
        HeaderSingle hName Required ->
          Exts.infixApp (var "h") "Data..#" (str hName)
        HeaderSingle hName Optional ->
          Exts.infixApp (var "h") "Data..#?" (str hName)
        HeaderMap hName ->
          Exts.appFun (var "Data.parseHeadersMap") [str hName, var "h"]
      ResponseFieldStatus ->
        pureE `Exts.app` Exts.paren (var "Prelude.fromEnum" `Exts.app` var "s")
      ResponseFieldBody PureBody ->
        pureE `Exts.app` var "x"
      ResponseFieldBody PureCoercedBody ->
        -- TODO: If we unconditionally inject a `Just`, can we instead
        -- ensure these response fields are never Maybe?
        pureE
          `Exts.app` ( justE
                         `Exts.app` (var "Prelude.coerce" `Exts.app` var "x")
                     )
      ResponseFieldBody JsonBody ->
        var "Data.eitherParseJSON" `Exts.app` var "x"
      ResponseFieldBody XmlBody ->
        var "Data.parseXML" `Exts.app` var "x"
      ResponseFieldJson jField -> case jField of
        JsonSingle name Required ->
          Exts.infixApp (var "x") "Data..:>" (str name)
        JsonSingle name Optional ->
          Exts.infixApp (var "x") "Data..?>" (str name)
        JsonMonoid name ->
          Exts.infixApp
            (Exts.infixApp (var "x") "Data..?>" (str name))
            "Core..!@"
            (var "Prelude.mempty")
      ResponseFieldXml xField -> case xField of
        XmlElement name Required ->
          Exts.infixApp (var "x") "Data..@" (str name)
        XmlElement name Optional ->
          Exts.infixApp (var "x") "Data..@?" (str name)
        XmlCollection mOuter collection sensitivity presence ->
          case mOuter of
            Nothing -> parser `Exts.app` var "x"
            Just outer ->
              bindE
                ( Exts.infixApp
                    (Exts.infixApp (var "x") "Data..@?" (str outer))
                    "Core..!@"
                    (var "Prelude.mempty")
                )
                parser
          where
            bindE m = Exts.infixApp m "Prelude.>>="

            parser = case presence of
              Required -> sensitiveParser
              Optional -> var "Core.may" `Exts.app` sensitiveParser

            sensitiveParser = case sensitivity of
              NotSensitive -> baseParser
              Sensitive ->
                ( var "Prelude.fmap"
                    `Exts.app` ( var "Prelude.fmap"
                                   `Exts.app` var "Data.Sensitive"
                               )
                )
                  `Exts.app` baseParser

            baseParser = case collection of
              XmlList item -> var "Data.parseXMLList" `Exts.app` str item
              XmlList1 item -> var "Data.parseXMLList1" `Exts.app` str item
              XmlMap {..} ->
                var "Data.parseXMLMap"
                  `Exts.app` str entry
                  `Exts.app` str key
                  `Exts.app` str value
