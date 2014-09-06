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
    , mkConfigureHealthCheck
    -- ** Request lenses
    , chcLoadBalancerName
    , chcHealthCheck

    -- * Response
    , ConfigureHealthCheckResponse
    -- ** Response lenses
    , chcrsHealthCheck
    ) where

import Network.AWS.Request.Query
import Network.AWS.ELB.V2012_06_01.Types
import Network.AWS.Prelude

-- | Input for the ConfigureHealthCheck action.
data ConfigureHealthCheck = ConfigureHealthCheck
    { _chcLoadBalancerName :: Text
    , _chcHealthCheck :: HealthCheck
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ConfigureHealthCheck' request.
mkConfigureHealthCheck :: Text -- ^ 'chcLoadBalancerName'
                       -> HealthCheck -- ^ 'chcHealthCheck'
                       -> ConfigureHealthCheck
mkConfigureHealthCheck p1 p2 = ConfigureHealthCheck
    { _chcLoadBalancerName = p1
    , _chcHealthCheck = p2
    }
{-# INLINE mkConfigureHealthCheck #-}

-- | The mnemonic name associated with the load balancer. The name must be
-- unique within the set of load balancers associated with your AWS account.
chcLoadBalancerName :: Lens' ConfigureHealthCheck Text
chcLoadBalancerName =
    lens _chcLoadBalancerName (\s a -> s { _chcLoadBalancerName = a })
{-# INLINE chcLoadBalancerName #-}

-- | A structure containing the configuration information for the new
-- healthcheck.
chcHealthCheck :: Lens' ConfigureHealthCheck HealthCheck
chcHealthCheck = lens _chcHealthCheck (\s a -> s { _chcHealthCheck = a })
{-# INLINE chcHealthCheck #-}

instance ToQuery ConfigureHealthCheck where
    toQuery = genericQuery def

-- | The output for the ConfigureHealthCheck action.
newtype ConfigureHealthCheckResponse = ConfigureHealthCheckResponse
    { _chcrsHealthCheck :: Maybe HealthCheck
    } deriving (Show, Generic)

-- | The updated healthcheck for the instances.
chcrsHealthCheck :: Lens' ConfigureHealthCheckResponse (Maybe HealthCheck)
chcrsHealthCheck =
    lens _chcrsHealthCheck (\s a -> s { _chcrsHealthCheck = a })
{-# INLINE chcrsHealthCheck #-}

instance FromXML ConfigureHealthCheckResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest ConfigureHealthCheck where
    type Sv ConfigureHealthCheck = ELB
    type Rs ConfigureHealthCheck = ConfigureHealthCheckResponse

    request = post "ConfigureHealthCheck"
    response _ = xmlResponse
