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

import           Control.Error
import           Data.HashMap.Strict (HashMap)
import           Data.Jason.Types
import           Data.Monoid
import           Data.SemVer
import           Data.String
import           Data.Text           (Text)
import qualified Data.Text           as Text
import           Gen.V2.JSON
import           Gen.V2.Log
import           Gen.V2.TH
import           Gen.V2.Types

newtype Doc = Doc Text
    deriving (Eq, Show, FromJSON, ToJSON, IsString)

documentation :: Maybe Text -> Doc
documentation = Doc . fromMaybe ""

newtype NS = NS Text
    deriving (Eq, Show, FromJSON, ToJSON)

namespace :: [Text] -> NS
namespace = NS . Text.intercalate "." . ("Network.AWS":)

data Type = Type
    { _tName :: Text
    } deriving (Eq, Show)

record stage2 ''Type

data Operation = Operation
    { _oName             :: Text
    , _oDocumentation    :: Doc
    , _oDocumentationUrl :: Text
    } deriving (Eq, Show)

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
    , _svTimestamp      :: !Timestamp
    , _svChecksum       :: !Checksum
    , _svXmlNamespace   :: Maybe Text
    , _svTargetPrefix   :: Maybe Text
    , _svError          :: Text
    } deriving (Eq, Show)

classy stage2 ''Service

data Dep = Dep
    { _dName    :: Text
    , _dVersion :: Version
    } deriving (Eq, Show)

record stage2 ''Dep

data Cabal = Cabal
    { _cLibrary      :: Text
    , _cVersion      :: Version
    , _cSynopsis     :: Doc
    , _cDescription  :: Doc
    , _cModules      :: [NS]
    , _cDependencies :: [Dep]
    } deriving (Eq, Show)

classy stage2 ''Cabal

data Mod a = Mod
    { _mModule    :: !a
    , _mNamespace :: NS
    , _mImports   :: [NS]
    } deriving (Eq, Show)

record stage2 ''Mod

data Stage2 = Stage2
    { _s2Cabal      :: Cabal
    , _s2Service    :: Mod Service
    , _s2Operations :: [Mod Operation]
    , _s2Types      :: Mod [Type]
    } deriving (Eq, Show)

record stage2 ''Stage2

instance HasCabal Stage2 where
    cabal = s2Cabal

instance HasService Stage2 where
    service = s2Service.mModule

decodeS2 :: Model S2 -> Script Stage2
decodeS2 Model{..} = do
    say "Decode Model" _mPath
    hoistEither (parseEither parseJSON (Object _mModel))
