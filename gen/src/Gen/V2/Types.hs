{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

-- Module      : Gen.V2.Types
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Gen.V2.Types where

import           Control.Applicative
import           Data.Jason.Types    hiding (object)
import           Data.Monoid
import           Data.Ord
import           Data.Text           (Text)
import qualified Data.Text           as Text
import           Gen.V2.TH

-- NOTE: Keep the boto json structure completely intact.
-- FIXME: _retry.json
-- FIXME: _endpoints.json

data Signature
    = V2
    | V3
    | V4
      deriving (Eq, Show)

instance FromJSON Signature where
    parseJSON = withText "signature" $ \case
        "v2"      -> pure V2
        "v3"      -> pure V3
        "v3https" -> pure V3
        "v4"      -> pure V4
        "s3"      -> pure V4
        e         -> fail ("Unknown Signature: " ++ Text.unpack e)

nullary stage2 ''Signature

data Protocol
    = JSON
    | RestJSON
    | RestXML
    | Query
      deriving (Eq, Show)

instance FromJSON Protocol where
    parseJSON = withText "protocol" $ \case
        "json"      -> pure JSON
        "rest-json" -> pure RestJSON
        "rest-xml"  -> pure RestXML
        "query"     -> pure Query
        "ec2"       -> pure Query
        e           -> fail ("Unknown Protocol: " ++ Text.unpack e)

nullary stage2 ''Protocol

data Timestamp
    = RFC822
    | ISO8601
    | POSIX
      deriving (Eq, Show)

instance FromJSON Timestamp where
    parseJSON = withText "timestamp" $ \case
        "rfc822"        -> pure RFC822
        "unixTimestamp" -> pure POSIX
        e               -> fail ("Unknown Timestamp: " ++ Text.unpack e)

nullary stage2 ''Timestamp

data Checksum
    = MD5
    | SHA256
      deriving (Eq, Show)

nullary stage1 ''Checksum
nullary stage2 ''Checksum

data Method
    = GET
    | POST
    | HEAD
    | PUT
    | DELETE
      deriving (Eq, Show)

instance FromJSON Method where
    parseJSON = withText "method" $ \case
        "GET"    -> pure GET
        "POST"   -> pure POST
        "HEAD"   -> pure HEAD
        "PUT"    -> pure PUT
        "DELETE" -> pure DELETE
        e        -> fail ("Unknown Method: " ++ Text.unpack e)

nullary stage2 ''Method

data Model = Model
    { _mName    :: String
    , _mVersion :: String
    , _mPath    :: FilePath
    , _mModel   :: Object
    } deriving (Show, Eq)

instance Ord Model where
    compare a b = comparing _mName a b <> comparing _mVersion a b

data Path
    = Const Text
    | Var   Text
      deriving (Eq, Show)

data URI = URI [Path] [(Text, Maybe Text)]
    deriving (Eq, Show)

dots :: FilePath -> Bool
dots "."  = False
dots ".." = False
dots _    = True
