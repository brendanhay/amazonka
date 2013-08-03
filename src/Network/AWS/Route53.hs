{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE DeriveDataTypeable         #-}
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

import Control.Applicative
import Data.ByteString     (ByteString)
import Data.Data
import Data.String
import Data.Time
import Network.AWS.Request
import Network.AWS.TH
import Network.AWS.Types

newtype CallerRef = CallerRef String
    deriving (Show, IsString, Data, Typeable)

data Protocol = HTTP | TCP
    deriving (Show, Data, Typeable)

data CreateHealthCheck = CreateHealthCheck
    { chcCallerRef :: !CallerRef
    , chcIpAddress :: !String
    , chcPort      :: !Int
    , chcProtocol  :: !Protocol
    , chcResource  :: !String
    , chcFQDN      :: !String
    } deriving (Show, Data, Typeable)

$(embedTemplate ''CreateHealthCheck)

instance AWSRequest CreateHealthCheck where
    signRequest = sign Version3 . post route53Endpoint "healthcheck"

--
-- Internal
--

route53Endpoint :: ByteString
route53Endpoint = "route53.amazonaws.com"

callerRef :: IO CallerRef
callerRef = fromString . show <$> getCurrentTime
