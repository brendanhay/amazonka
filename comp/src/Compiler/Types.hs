{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE StandaloneDeriving     #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TupleSections          #-}

-- Module      : Compiler.Types
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla xtPublic License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Compiler.Types
    ( module Compiler.Types
    , module Compiler.Types.Map
    , module Compiler.Types.Id
    , module Compiler.Types.URI
    , module Compiler.Types.NS
    , module Compiler.Types.Help
    ) where

import           Compiler.Text
import           Compiler.TH
import           Compiler.Types.Help
import           Compiler.Types.Id
import           Compiler.Types.Map
import           Compiler.Types.NS
import           Compiler.Types.Orphans       ()
import           Compiler.Types.URI
import           Control.Error
import           Control.Lens                 hiding ((.=))
import           Data.Aeson                   (ToJSON (..), object, (.=))
import qualified Data.Aeson                   as A
import           Data.Hashable
import qualified Data.HashMap.Strict          as Map
import qualified Data.HashSet                 as Set
import           Data.Jason                   hiding (Bool, ToJSON (..), object,
                                               (.=))
import           Data.List                    (sortOn)
import           Data.Monoid                  hiding (Product, Sum)
import           Data.Ord
import qualified Data.SemVer                  as SemVer
import           Data.Text                    (Text)
import qualified Data.Text                    as Text
import qualified Data.Text.Lazy               as LText
import qualified Data.Text.Lazy.Builder       as Build
import           Data.Time
import qualified Filesystem.Path.CurrentOS    as Path
import           Formatting
import           GHC.Generics                 (Generic)
import           GHC.TypeLits
import           Language.Haskell.Exts.Syntax (Name)
import           Numeric.Natural
import           Text.EDE                     (Template)

type Compiler = EitherT LazyText
type LazyText = LText.Text
type Set      = Set.HashSet
type Path     = Path.FilePath

toTextIgnore :: Path -> Text
toTextIgnore = either id id . Path.toText

data Model = Model
    { _modelName    :: Text
    , _modelVersion :: UTCTime
    , _modelPath    :: Path
    } deriving (Eq, Show)

makeLenses ''Model

configFile, annexFile :: Getter Model Path
configFile = to (flip Path.addExtension "json" . Path.fromText . _modelName)
annexFile  = configFile

serviceFile, waitersFile, pagersFile :: Getter Model Path
serviceFile = to (flip Path.append "service-2.json"    . _modelPath)
waitersFile = to (flip Path.append "waiters-2.json"    . _modelPath)
pagersFile  = to (flip Path.append "paginators-1.json" . _modelPath)

loadModel :: Monad m => Path -> [Path] -> Compiler m Model
loadModel p xs = uncurry (Model n) <$>
    tryHead (format ("No valid model versions found in " % string) (show xs)) vs
  where
    vs = sortOn Down (mapMaybe parse xs)
    n  = toTextIgnore (Path.filename p)

    parse d = (,d) <$> parseTimeM True defaultTimeLocale
        (iso8601DateFormat Nothing)
        (Path.encodeString (Path.filename d))

data Templates = Templates
    { cabalTemplate           :: Template
    , serviceTemplate         :: Template
    , waitersTemplate         :: Template
    , readmeTemplate          :: Template
    , exampleCabalTemplate    :: Template
    , exampleMakefileTemplate :: Template
    , operationTemplate       :: Template
    , typesTemplate           :: Template
    }

makePrisms ''Identity

data Signature
    = V2
    | V3
    | V3HTTPS
    | V4
    | S3
      deriving (Eq, Show, Generic)

instance FromJSON Signature where
    parseJSON = gParseJSON' lower

instance ToJSON Signature where
    toJSON = gToJSON' lower

data Protocol
    = JSON
    | RestJSON
    | XML
    | RestXML
    | Query
    | EC2
      deriving (Eq, Show, Generic)

instance FromJSON Protocol where
    parseJSON = gParseJSON' spinal

instance ToJSON Protocol where
    toJSON = gToJSON' spinal

data Timestamp
    = RFC822
    | ISO8601
    | POSIX
      deriving (Eq, Show, Generic)

instance FromJSON Timestamp where
    parseJSON = withText "timestamp" $ \case
        "rfc822"        -> pure RFC822
        "iso8601"       -> pure ISO8601
        "unixTimestamp" -> pure POSIX
        e               -> fail ("Unknown Timestamp: " ++ Text.unpack e)

instance ToJSON Timestamp where
    toJSON = gToJSON' lower

data Checksum
    = MD5
    | SHA256
      deriving (Eq, Show, Generic)

instance FromJSON Checksum where
    parseJSON = gParseJSON' lower

instance ToJSON Checksum where
    toJSON = gToJSON' lower

data Location
    = Headers
    | Header
    | URI
    | Querystring
    | StatusCode
    | Body
      deriving (Eq, Show, Generic)

instance FromJSON Location where
    parseJSON = gParseJSON' camel

data Method
    = GET
    | POST
    | HEAD
    | PUT
    | DELETE
      deriving (Eq, Show, Generic)

instance FromJSON Method where
    parseJSON = gParseJSON' upper

data XML = XML'
    { _xmlPrefix :: Text
    , _xmlUri    :: Text
    } deriving (Eq, Show, Generic)

makeClassy ''XML

instance FromJSON XML where
    parseJSON = gParseJSON' (camel & lenses .~ True)

data Ref f = Ref
    { _refShape         :: Id
    , _refDocumentation :: f Help
    , _refLocation      :: f Location
    , _refLocationName  :: f Text
    , _refQueryName     :: f Text
    , _refStreaming     :: !Bool
    , _refWrapper       :: !Bool
    , _refXMLAttribute  :: !Bool
    , _refXMLNamespace  :: f XML
    } deriving (Generic)

deriving instance Show (Ref Maybe)
deriving instance Show (Ref Identity)

makeLenses ''Ref

instance FromJSON (Ref Maybe) where
    parseJSON = withObject "ref" $ \o -> Ref
        <$> o .:  "shape"
        <*> o .:? "documentation"
        <*> o .:? "location"
        <*> o .:? "locationName"
        <*> o .:? "queryName"
        <*> o .:? "streaming"    .!= False
        <*> o .:? "wrapper"      .!= False
        <*> o .:? "xmlAttribute" .!= False
        <*> o .:? "xmlnamespace"

instance HasXML (Ref Identity) where
    xML = refXMLNamespace . _Wrapped

data Info f = Info
    { _infoDocumentation :: f Help
    , _infoMin           :: !Natural
    , _infoMax           :: Maybe Natural
    , _infoFlattened     :: !Bool
    , _infoSensitive     :: !Bool
    , _infoStreaming     :: !Bool
    , _infoException     :: !Bool
    } deriving (Generic)

deriving instance Show (Info Maybe)
deriving instance Show (Info Identity)

makeClassy ''Info

instance FromJSON (Info Maybe) where
    parseJSON = withObject "info" $ \o -> Info
        <$> o .:? "documentation"
        <*> o .:? "min"       .!= 0
        <*> o .:? "max"
        <*> o .:? "flattened" .!= False
        <*> o .:? "sensitive" .!= False
        <*> o .:? "streaming" .!= False
        <*> o .:? "exception" .!= False

data Lit
    = Int
    | Long
    | Double
    | Text
    | Blob
    | Time
    | Bool
      deriving (Eq, Show)

class HasRefs f a | a -> f where
    references :: Traversal' a (Ref f)

data Struct f = Struct'
    { _members  :: Map Id (Ref f)
    , _required :: Set Id
    , _payload  :: Maybe Id
    }

deriving instance Show (Struct Maybe)
deriving instance Show (Struct Identity)

makeLenses ''Struct

instance HasRefs f (Struct f) where
    references = members . each

instance FromJSON (Struct Maybe) where
    parseJSON = withObject "struct" $ \o -> Struct'
        <$> o .:  "members"
        <*> o .:? "required" .!= mempty
        <*> o .:? "payload"

data Shape f
    = List   (Info f) (Ref f)
    | Map    (Info f) (Ref f) (Ref f)
    | Struct (Info f) (Struct f)
    | Enum   (Info f) (Map Text Text)
    | Lit    (Info f) Lit

deriving instance Show (Shape Maybe)
deriving instance Show (Shape Identity)

makePrisms ''Shape

instance HasRefs f (Shape f) where
    references f = \case
        List   i e   -> List   i <$> f e
        Map    i k v -> Map    i <$> f k <*> f v
        Struct i s   -> Struct i <$> references f s
        s            -> pure s

instance HasInfo (Shape f) f where
    info = lens f (flip g)
      where
        f = \case
            List   i _   -> i
            Map    i _ _ -> i
            Struct i _   -> i
            Enum   i _   -> i
            Lit    i _   -> i

        g i = \case
            List   _ e   -> List   i e
            Map    _ k v -> Map    i k v
            Struct _ s   -> Struct i s
            Enum   _ m   -> Enum   i m
            Lit    _ l   -> Lit    i l

instance FromJSON (Shape Maybe) where
    parseJSON = withObject "shape" $ \o -> do
        i <- parseJSON (Object o)
        t <- o .:  "type"
        m <- o .:? "enum"
        case t of
            "list"      -> List   i <$> o .: "member"
            "map"       -> Map    i <$> o .: "key" <*> o .: "value"
            "structure" -> Struct i <$> parseJSON (Object o)
            "integer"   -> pure (Lit i Int)
            "long"      -> pure (Lit i Long)
            "double"    -> pure (Lit i Double)
            "float"     -> pure (Lit i Double)
            "blob"      -> pure (Lit i Blob)
            "boolean"   -> pure (Lit i Bool)
            "timestamp" -> pure (Lit i Time)
            "string"    -> pure $
                maybe (Lit i Text)
                      (Enum i . Map.fromList . map renameBranch)
                      m
            _           -> fail $ "Unknown Shape type: " ++ Text.unpack t


data HTTP f = HTTP
    { _method       :: !Method
    , _requestURI   :: URI
    , _responseCode :: f Int
    } deriving (Generic)

makeClassy ''HTTP

instance FromJSON (HTTP Maybe) where
    parseJSON = gParseJSON' camel

data Operation f a = Operation
    { _opName          :: Id
    , _opDocumentation :: f Text
    , _opHTTP          :: HTTP f
    , _opInput         :: f (a f)
    , _opOutput        :: f (a f)
    }

makeLenses ''Operation

requestName, responseName :: Getter (Operation f a) Id
requestName  = to _opName
responseName = to ((`keyAppend` "Response") . _opName)

instance FromJSON (Operation Maybe Ref) where
    parseJSON = withObject "operation" $ \o -> Operation
        <$> o .: "name"
        <*> o .:? "documentation"
        <*> o .:  "http"
        <*> o .:? "input"
        <*> o .:? "output"

instance HasHTTP (Operation f a) f where
    hTTP = opHTTP

data Metadata f = Metadata
    { _protocol         :: !Protocol
    , _serviceAbbrev    :: Text
    , _serviceFullName  :: Text
    , _apiVersion       :: Text
    , _signatureVersion :: !Signature
    , _endpointPrefix   :: Text
    , _timestampFormat  :: f Timestamp
    , _checksumFormat   :: f Checksum
    , _jsonVersion      :: Text
    , _targetPrefix     :: Maybe Text
    } deriving (Generic)

makeClassy ''Metadata

instance FromJSON (Metadata Maybe) where
    parseJSON = withObject "meta" $ \o -> Metadata
        <$> o .:  "protocol"
        <*> o .:  "serviceAbbreviation"
        <*> o .:  "serviceFullName"
        <*> o .:  "apiVersion"
        <*> o .:  "signatureVersion"
        <*> o .:  "endpointPrefix"
        <*> o .:? "timestampFormat"
        <*> o .:? "checksumFormat"
        <*> o .:? "jsonVersion"     .!= "1.0"
        <*> o .:? "targetPrefix"

instance ToJSON (Metadata Identity) where
    toJSON = gToJSON' camel

data Service f a b = Service
    { _metadata'     :: Metadata f
    , _documentation :: Help
    , _operations    :: Map Id (Operation f a)
    , _shapes        :: Map Id (b f)
    } deriving (Generic)

makeClassy ''Service

instance HasMetadata (Service f a b) f where
    metadata = metadata'

instance FromJSON (Service Maybe Ref Shape) where
    parseJSON = gParseJSON' lower

data Constraint
    = CEq
    | COrd
    | CRead
    | CShow
    | CGeneric
    | CEnum
    | CNum
    | CIntegral
    | CReal
    | CRealFrac
    | CRealFloat
    | CMonoid
    | CSemigroup
    | CIsString
    | CToQuery
    | CToJSON
    | CFromJSON
    | CToXML
    | CFromXML
      deriving (Eq, Ord, Show, Generic)

instance Hashable Constraint

instance FromJSON Constraint where
    parseJSON = gParseJSON' (spinal & ctor %~ (. Text.drop 1))

data Fun = Fun Name Help LazyText LazyText

data Data f
    = Product (Info f) (Struct f)      LazyText [Constraint] Fun (Map Text Fun)
    | Sum     (Info f) (Map Text Text) LazyText [Constraint]

makePrisms ''Data

instance HasInfo (Data f) f where
    info = lens f (flip g)
      where
        f = \case
            Product i _ _ _ _ _  -> i
            Sum     i _ _ _      -> i

        g i = \case
            Product _ s  d is c ls -> Product i s  d is c ls
            Sum     _ vs d is      -> Sum     i vs d is

instance ToJSON (Data f) where
    toJSON = \case
        Product i s d is c ls -> object
            [ "type" .= Text.pack "product"
            , "declaration" .= d
            ]

        Sum i vs d is -> object
            [ "type" .= Text.pack "sum"
            , "declaration" .= d
            ]

           --             [ "type"        .- Text.pack "product"
        --     , "constructor" .- ctor
        --     , "comment"     .- Above 0 doc
        --     , "declaration" .- decl
        --     , "fields"      .- fieldPairs (x ^. structMembers)
        --     , "lenses"      .- ls
        --     , "instances"   .- is
        --     ]
        -- Sum x doc decl is ->
        --     [ "type"         .- Text.pack "sum"
        --     , "comment"      .- Above 0 doc
        --     , "declaration"  .- decl
        --     , "constructors" .- view enumValues x
        --     , "instances"    .- is
        --     ]

data Replace = Replace
    { _replaceName        :: Id
    , _replaceConstraints :: Set Constraint
    } deriving (Eq, Show, Generic)

makeLenses ''Replace

instance FromJSON Replace where
    parseJSON = gParseJSON' (lower & field %~ (. stripPrefix "replace"))

data Override = Override
    { _renamedTo      :: Maybe Id      -- ^ Rename type
    , _replacedBy     :: Maybe Replace -- ^ Existing type that supplants this type
    , _enumPrefix     :: Maybe Text    -- ^ Enum constructor prefix
    , _requiredFields :: Set Id        -- ^ Required fields
    , _optionalFields :: Set Id        -- ^ Optional fields
    , _renamedFields  :: Map Id Id     -- ^ Rename fields
    } deriving (Eq, Show)

makeLenses ''Override

instance FromJSON Override where
    parseJSON = withObject "override" $ \o -> Override
        <$> o .:? "renamedTo"
        <*> o .:? "replacedBy"
        <*> o .:? "enumPrefix"
        <*> o .:? "requiredFields" .!= mempty
        <*> o .:? "optionalFields" .!= mempty
        <*> o .:? "renamedFields"  .!= mempty

newtype Version (v :: Symbol) = Version SemVer.Version
    deriving (Eq, Show)

instance A.ToJSON (Version v) where
    toJSON (Version v) = A.toJSON (SemVer.toText v)

semver :: Format a (Version v -> a)
semver = later (\(Version v) -> Build.fromText (SemVer.toText v))

type LibraryVer = Version "library"
type ClientVer  = Version "client"
type CoreVer    = Version "core"

data Versions = Versions
    { _libraryVersion :: LibraryVer
    , _clientVersion  :: ClientVer
    , _coreVersion    :: CoreVer
    } deriving (Show)

makeClassy ''Versions

data Config = Config
    { _libraryName      :: Text
    , _referenceUrl     :: Text
    , _operationUrl     :: Text
    , _operationImports :: [NS]
    , _typeImports      :: [NS]
    , _typeOverrides    :: Map Id Override
    }

makeClassy ''Config

instance FromJSON Config where
    parseJSON = withObject "config" $ \o -> Config
        <$> o .:  "libraryName"
        <*> o .:  "referenceUrl"
        <*> o .:  "operationUrl"
        <*> o .:? "operationImports" .!= mempty
        <*> o .:? "typeImports"      .!= mempty
        <*> o .:? "typeOverrides"    .!= mempty

data Library = Library
    { _versions'      :: Versions
    , _config'        :: Config
    , _service'       :: Service Identity Data Data
    , _namespace      :: NS
    , _exposedModules :: [NS]
    , _otherModules   :: [NS]
    } deriving (Generic)

makeLenses ''Library

instance HasMetadata Library Identity           where metadata = service' . metadata'
instance HasService  Library Identity Data Data where service  = service'
instance HasConfig   Library                    where config   = config'
instance HasVersions Library                    where versions = versions'

instance ToJSON Library where
    toJSON l = A.Object (x <> y)
      where
        A.Object y = toJSON (l ^. metadata)
        A.Object x = object
            [ "referenceUrl"   .= (l ^. referenceUrl)
            , "operationUrl"   .= (l ^. operationUrl)
            , "description"    .= (l ^. documentation . asDesc)
            , "documentation"  .= (l ^. documentation)
            , "libraryName"    .= (l ^. libraryName)
            , "libraryVersion" .= (l ^. libraryVersion)
            , "clientVersion"  .= (l ^. clientVersion)
            , "coreVersion"    .= (l ^. coreVersion)
            , "exposedModules" .= (l ^. exposedModules)
            , "otherModules"   .= (l ^. otherModules)
            , "shapes"         .= (l ^. shapes)
            ]

