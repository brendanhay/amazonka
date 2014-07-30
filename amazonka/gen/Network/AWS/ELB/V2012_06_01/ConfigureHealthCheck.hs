{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ELB.V2012_06_01.ConfigureHealthCheck
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Specifies the health check settings to use for evaluating the health state
-- of your back-end instances. For more information, see Health Check in the
-- Elastic Load Balancing Developer Guide.
-- https://elasticloadbalancing.amazonaws.com/?HealthCheck.HealthyThreshold=2
-- &HealthCheck.UnhealthyThreshold=2 &LoadBalancerName=MyLoadBalancer
-- &HealthCheck.Target=HTTP:80/ping &HealthCheck.Interval=30
-- &HealthCheck.Timeout=3 &Version=2012-06-01 &Action=ConfigureHealthCheck
-- &AUTHPARAMS 30 HTTP:80/ping 2 3 2 83c88b9d-12b7-11e3-8b82-87b12EXAMPLE.
module Network.AWS.ELB.V2012_06_01.ConfigureHealthCheck where

import           Control.Applicative
import           Data.ByteString      (ByteString)
import           Data.Default
import           Data.HashMap.Strict  (HashMap)
import           Data.Monoid
import           Data.Text            (Text)
import qualified Data.Text            as Text
import           GHC.Generics
import           Network.AWS.Data
import           Network.AWS.Response
import           Network.AWS.Types    hiding (Error, Endpoint, Region)
import           Network.AWS.Request.Query
import           Network.AWS.ELB.V2012_06_01.Types
import           Network.HTTP.Client  (RequestBody, Response)
import           Prelude              hiding (head)

data ConfigureHealthCheck = ConfigureHealthCheck
    { _chciLoadBalancerName :: Text
      -- ^ The mnemonic name associated with the load balancer. The name
      -- must be unique within the set of load balancers associated with
      -- your AWS account.
    , _chciHealthCheck :: HealthCheck
      -- ^ A structure containing the configuration information for the new
      -- healthcheck.
    } deriving (Generic)

instance ToQuery ConfigureHealthCheck where
    toQuery = genericToQuery def

instance AWSRequest ConfigureHealthCheck where
    type Sv ConfigureHealthCheck = ELB
    type Rs ConfigureHealthCheck = ConfigureHealthCheckResponse

    request = post "ConfigureHealthCheck"
    response _ = xmlResponse

data ConfigureHealthCheckResponse = ConfigureHealthCheckResponse
    { _chcoHealthCheck :: Maybe HealthCheck
      -- ^ The updated healthcheck for the instances.
    } deriving (Generic)

instance FromXML ConfigureHealthCheckResponse where
    fromXMLOptions = xmlOptions
