{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}

-- |
-- Module      : Network.AWS.Route53
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.Route53 where

import           Control.Applicative
import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.ByteString        (ByteString)
import           Data.String
import           Data.Time
import           Network.AWS.Internal
import           Network.Http.Client
import qualified System.IO.Streams      as Streams
import           Text.Hastache
import           Text.Hastache.Aeson

newtype CallerRef = CallerRef String
    deriving (Show, IsString)

instance ToJSON CallerRef where
    toJSON (CallerRef s) = toJSON s

data Protocol = HTTP | TCP
    deriving (Show)

instance ToJSON Protocol where
    toJSON = toJSON . show

data CreateHealthCheck = CreateHealthCheck
    { chcCallerRef :: !CallerRef
    , chcIpAddress :: !String
    , chcPort      :: !Int
    , chcProtocol  :: !Protocol
    , chcResource  :: !String
    , chcFQDN      :: !String
    } deriving (Show)

$(deriveTemplate "chc" ''CreateHealthCheck)

instance GlobalRequest CreateHealthCheck where
    signGlobal = signer POST "healthcheck"

--
-- Internal
--

signer :: Template a => Method -> ByteString -> a -> AWS SignedRequest
signer meth path tmpl = render >>= version3
    . emptyRequest meth version endpoint path
    . Just
  where
    render = liftIO $ do
        bstr <- hastacheStr defaultConfig (readTemplate tmpl) (jsonContext tmpl)
        Streams.fromLazyByteString bstr

    version  = "2012-12-12"
    endpoint = "route53.amazonaws.com"

callerRef :: IO CallerRef
callerRef = fromString . show <$> getCurrentTime
