{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
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
module Network.AWS.ELB.V2012_06_01.ConfigureHealthCheck
    (
    -- * Request
      ConfigureHealthCheck
    -- ** Request constructor
    , mkConfigureHealthCheckInput
    -- ** Request lenses
    , chciLoadBalancerName
    , chciHealthCheck

    -- * Response
    , ConfigureHealthCheckResponse
    -- ** Response lenses
    , chcoHealthCheck
    ) where

import Network.AWS.Request.Query
import Network.AWS.ELB.V2012_06_01.Types
import Network.AWS.Prelude

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ConfigureHealthCheck' request.
mkConfigureHealthCheckInput :: Text -- ^ 'chciLoadBalancerName'
                            -> HealthCheck -- ^ 'chciHealthCheck'
                            -> ConfigureHealthCheck
mkConfigureHealthCheckInput p1 p2 = ConfigureHealthCheck
    { _chciLoadBalancerName = p1
    , _chciHealthCheck = p2
    }
{-# INLINE mkConfigureHealthCheckInput #-}

data ConfigureHealthCheck = ConfigureHealthCheck
    { _chciLoadBalancerName :: Text
      -- ^ The mnemonic name associated with the load balancer. The name
      -- must be unique within the set of load balancers associated with
      -- your AWS account.
    , _chciHealthCheck :: HealthCheck
      -- ^ A structure containing the configuration information for the new
      -- healthcheck.
    } deriving (Show, Generic)

-- | The mnemonic name associated with the load balancer. The name must be
-- unique within the set of load balancers associated with your AWS account.
chciLoadBalancerName :: Lens' ConfigureHealthCheck (Text)
chciLoadBalancerName = lens _chciLoadBalancerName (\s a -> s { _chciLoadBalancerName = a })
{-# INLINE chciLoadBalancerName #-}

-- | A structure containing the configuration information for the new
-- healthcheck.
chciHealthCheck :: Lens' ConfigureHealthCheck (HealthCheck)
chciHealthCheck = lens _chciHealthCheck (\s a -> s { _chciHealthCheck = a })
{-# INLINE chciHealthCheck #-}

instance ToQuery ConfigureHealthCheck where
    toQuery = genericQuery def

newtype ConfigureHealthCheckResponse = ConfigureHealthCheckResponse
    { _chcoHealthCheck :: Maybe HealthCheck
      -- ^ The updated healthcheck for the instances.
    } deriving (Show, Generic)

-- | The updated healthcheck for the instances.
chcoHealthCheck :: Lens' ConfigureHealthCheckResponse (Maybe HealthCheck)
chcoHealthCheck = lens _chcoHealthCheck (\s a -> s { _chcoHealthCheck = a })
{-# INLINE chcoHealthCheck #-}

instance FromXML ConfigureHealthCheckResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest ConfigureHealthCheck where
    type Sv ConfigureHealthCheck = ELB
    type Rs ConfigureHealthCheck = ConfigureHealthCheckResponse

    request = post "ConfigureHealthCheck"
    response _ = xmlResponse
