{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE RecordWildCards            #-}
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

import Data.Data
import GHC.Word
import Network.AWS.Request
import Network.AWS.TH
import Network.Http.Client

data CreateHealthCheck = CreateHealthCheck
    { chcCallerRef :: String
    , chcIpAddress :: String
    , chcPort      :: Word16
    , chcProtocol  :: String
    , chcResource  :: String
    , chcFQDN      :: String
    } deriving (Data, Typeable)

-- FIXME: Use template haskell for instances
-- Function which determines the template from the class name underscored
-- and takes the uri as an argument
-- use TH to read in the actual template at compile time and set it to
-- the template typeclass function :: ByteString
instance AWSRequest CreateHealthCheck where
    template _ = $(embedTemplate "route53/create_health_check")
    endpoint _ = route53Base
    request  _ = version3 POST route53Base "healthcheck" []

route53Base :: ByteString
route53Base = "route53.amazonaws.com"
