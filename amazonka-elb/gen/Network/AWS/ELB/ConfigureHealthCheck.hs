{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-unused-binds  #-} doesnt work if wall is used
{-# OPTIONS_GHC -w #-}

-- Module      : Network.AWS.ELB.ConfigureHealthCheck
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
module Network.AWS.ELB.ConfigureHealthCheck
    (
    -- * Request
      ConfigureHealthCheckInput
    -- ** Request constructor
    , configureHealthCheckInput
    -- ** Request lenses
    , chciHealthCheck
    , chciLoadBalancerName

    -- * Response
    , ConfigureHealthCheckOutput
    -- ** Response constructor
    , configureHealthCheckOutput
    -- ** Response lenses
    , chcoHealthCheck
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.ELB.Types

data ConfigureHealthCheckInput = ConfigureHealthCheckInput
    { _chciHealthCheck      :: HealthCheck
    , _chciLoadBalancerName :: Text
    } deriving (Eq, Show, Generic)

-- | 'ConfigureHealthCheckInput' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'chciHealthCheck' @::@ 'HealthCheck'
--
-- * 'chciLoadBalancerName' @::@ 'Text'
--
configureHealthCheckInput :: Text -- ^ 'chciLoadBalancerName'
                          -> HealthCheck -- ^ 'chciHealthCheck'
                          -> ConfigureHealthCheckInput
configureHealthCheckInput p1 p2 = ConfigureHealthCheckInput
    { _chciLoadBalancerName = p1
    , _chciHealthCheck      = p2
    }

-- | A structure containing the configuration information for the new
-- healthcheck.
chciHealthCheck :: Lens' ConfigureHealthCheckInput HealthCheck
chciHealthCheck = lens _chciHealthCheck (\s a -> s { _chciHealthCheck = a })

-- | The mnemonic name associated with the load balancer. The name must be
-- unique within the set of load balancers associated with your AWS account.
chciLoadBalancerName :: Lens' ConfigureHealthCheckInput Text
chciLoadBalancerName =
    lens _chciLoadBalancerName (\s a -> s { _chciLoadBalancerName = a })

instance ToPath ConfigureHealthCheckInput where
    toPath = const "/"

instance ToQuery ConfigureHealthCheckInput

newtype ConfigureHealthCheckOutput = ConfigureHealthCheckOutput
    { _chcoHealthCheck :: Maybe HealthCheck
    } deriving (Eq, Show, Generic)

-- | 'ConfigureHealthCheckOutput' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'chcoHealthCheck' @::@ 'Maybe' 'HealthCheck'
--
configureHealthCheckOutput :: ConfigureHealthCheckOutput
configureHealthCheckOutput = ConfigureHealthCheckOutput
    { _chcoHealthCheck = Nothing
    }

-- | The updated healthcheck for the instances.
chcoHealthCheck :: Lens' ConfigureHealthCheckOutput (Maybe HealthCheck)
chcoHealthCheck = lens _chcoHealthCheck (\s a -> s { _chcoHealthCheck = a })

instance AWSRequest ConfigureHealthCheckInput where
    type Sv ConfigureHealthCheckInput = ELB
    type Rs ConfigureHealthCheckInput = ConfigureHealthCheckOutput

    request  = post "ConfigureHealthCheck"
    response = const . xmlResponse $ \h x -> ConfigureHealthCheckOutput
newtype
