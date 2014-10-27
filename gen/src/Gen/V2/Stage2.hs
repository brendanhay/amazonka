{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}

-- Module      : Gen.V2.Stage2
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Gen.V2.Stage2 where

import           Control.Applicative
import           Control.Error
import           Control.Lens        (makeLenses)
import           Data.Char
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Map
import           Data.Jason.Types
import           Data.Monoid
import           Data.SemVer
import           Data.String
import           Data.Text           (Text)
import qualified Data.Text           as Text
import           Gen.V2.JSON
import           Gen.V2.Log
import           Gen.V2.Naming
import           Gen.V2.TH
import           Gen.V2.Types
import           System.FilePath

newtype Doc = Doc Text
    deriving (Eq, Show, ToJSON, IsString)

documentation :: Maybe Text -> Doc
documentation = Doc . fromMaybe ""

newtype NS = NS [Text]
    deriving (Eq, Show)

-- instance FromJSON NS where
--     parseJSON = withText "namespace" (pure . NS . Text.splitOn ".")

instance ToJSON NS where
    toJSON (NS xs) = String (Text.intercalate "." xs)

instance ToFilePath NS where
    toFilePath (NS xs) = Text.unpack (Text.intercalate "/" xs) <.> "hs"

namespace :: [Text] -> NS
namespace = NS . ("Network":) . ("AWS":)

-- Wrapper types

-- data List a
-- data List1 a
-- data Map k v
-- data Optional a
-- data Box a
-- data Sensitive a

data Prim
    = PBlob
    | PBool
    | PText
    | PInt
    | PInteger
    | PDouble
    | PTime !Timestamp
      deriving (Eq, Show)

primitive :: Prim -> Text
primitive = \case
    PBlob    -> "Blob"
    PBool    -> "Bool"
    PText    -> "Text"
    PInt     -> "Int"
    PInteger -> "Integer"
    PDouble  -> "Double"
    PTime ts -> "Time " <> timestamp ts

data Type
    = Prim  !Prim
    | Type  Text
    | List  !Type
    | List1 !Type
    | Map   !Type !Type
    | Maybe !Type
      deriving (Eq, Show)

instance ToJSON Type where
    toJSON = toJSON . go
      where
        go = \case
            Prim  p   -> primitive p
            Type  t   -> t
            List  x   -> "List "  <> wrap (go x)
            List1 x   -> "List1 " <> wrap (go x)
            Map   k v -> "Map "   <> wrap (go k) <> " " <> wrap (go v)
            Maybe x   -> "Maybe " <> wrap (go x)

        wrap   t = maybe t (const (parens t)) (Text.findIndex isSpace t)
        parens t = "(" <> t <> ")"

required :: Type -> Bool
required (Maybe _) = True
required _         = False

data Named a = Named Text !a
    deriving (Eq, Show)

instance ToJSON a => ToJSON (Named a) where
    toJSON (Named k v) = Object . Obj $ ("name", String k) : unwrap (toJSON v)
      where
        unwrap = \case
            Object (Obj x) -> x
            x              -> unwrap (object ["value" .= x])

data Field = Field
    { _fType         :: !Type
    , _fLocation     :: !Location
    , _fLocationName :: Text
    } deriving (Eq, Show)

instance ToJSON Field where
    toJSON Field{..} = object
        [ "type"         .= _fType
        , "required"     .= required _fType
        , "location"     .= _fLocation
--        , "locationName" .= _fLocationName
        ]

data Data
    = Newtype { _ntField   :: Named  Field }
    | Record  { _recFields :: OrdMap Field }
    | Nullary { _nFields   :: OrdMap Field }
    | Empty
      deriving (Eq, Show)

record stage2 ''Data

data Operation = Operation
    { _opDocumentation    :: Doc
    , _opDocumentationUrl :: Maybe Text
    , _opMethod           :: !Method
    , _opUri              :: URI
    , _opRequest          :: Named Data
    , _opResponse         :: Named Data
    } deriving (Eq, Show)

-- Errors? Pagination? Result/Request inline and not part of the types module?

record stage2 ''Operation

data Endpoint
    = Global
    | Regional
      deriving (Eq, Show)

nullary stage2 ''Endpoint

data Service = Service
    { _svName           :: Text
    , _svAbbrev         :: Text
    , _svVersion        :: Text
    , _svDocumentation  :: Doc
    , _svProtocol       :: !Protocol
    , _svEndpoint       :: !Endpoint
    , _svEndpointPrefix :: Text
    , _svSignature      :: !Signature
    , _svChecksum       :: !Checksum
    , _svXmlNamespace   :: Text
    , _svTargetPrefix   :: Maybe Text
    , _svError          :: Text
    } deriving (Eq, Show)

classy stage2 ''Service

data Cabal = Cabal
    { _cLibrary      :: Text
    , _cVersion      :: Version
    , _cSynopsis     :: Doc
    , _cDescription  :: Doc
    , _cModules      :: [NS]
    , _cDependencies :: [Named Version]
    } deriving (Eq, Show)

classy stage2 ''Cabal

instance ToFilePath Cabal where
    toFilePath c = Text.unpack (_cLibrary c) <.> "cabal"

data Mod a = Mod
    { _mModule    :: !a
    , _mNamespace :: NS
    , _mImports   :: [NS]
    } deriving (Eq, Show)

makeLenses ''Mod

instance ToJSON a => ToJSON (Mod a) where
    toJSON Mod{..} = Object (x <> y)
      where
        Object x = toJSON _mModule
        Object y = object
            [ "namespace" .= _mNamespace
            , "imports"   .= _mImports
            ]

instance ToFilePath (Mod a) where
    toFilePath = toFilePath . _mNamespace

data Stage2 = Stage2
    { _s2Cabal      :: Cabal
    , _s2Service    :: Mod Service
    , _s2Operations :: HashMap Text (Mod Operation)
    , _s2Types      :: Mod (HashMap Text Data)
    } deriving (Eq, Show)

record stage2 ''Stage2

instance HasCabal Stage2 where
    cabal = s2Cabal

instance HasService Stage2 where
    service = s2Service.mModule

-- decodeS2 :: Model S2 -> Script Stage2
-- decodeS2 Model{..} = do
--     say "Decode Model" _mPath
--     hoistEither (parseEither parseJSON (Object _mModel))
