{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

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
--
-- <http://docs.aws.amazon.com/ElasticLoadBalancing/latest/APIReference/API_ConfigureHealthCheck.html>
module Network.AWS.ELB.ConfigureHealthCheck
    (
    -- * Request
      ConfigureHealthCheck
    -- ** Request constructor
    , configureHealthCheck
    -- ** Request lenses
    , chcHealthCheck
    , chcLoadBalancerName

    -- * Response
    , ConfigureHealthCheckResponse
    -- ** Response constructor
    , configureHealthCheckResponse
    -- ** Response lenses
    , chcrHealthCheck
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.ELB.Types
import qualified GHC.Exts

data ConfigureHealthCheck = ConfigureHealthCheck
    { _chcHealthCheck      :: HealthCheck
    , _chcLoadBalancerName :: Text
    } deriving (Eq, Show, Generic)

-- | 'ConfigureHealthCheck' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'chcHealthCheck' @::@ 'HealthCheck'
--
-- * 'chcLoadBalancerName' @::@ 'Text'
--
configureHealthCheck :: Text -- ^ 'chcLoadBalancerName'
                     -> HealthCheck -- ^ 'chcHealthCheck'
                     -> ConfigureHealthCheck
configureHealthCheck p1 p2 = ConfigureHealthCheck
    { _chcLoadBalancerName = p1
    , _chcHealthCheck      = p2
    }

-- | A structure containing the configuration information for the new
-- healthcheck.
chcHealthCheck :: Lens' ConfigureHealthCheck HealthCheck
chcHealthCheck = lens _chcHealthCheck (\s a -> s { _chcHealthCheck = a })

-- | The mnemonic name associated with the load balancer. The name must be
-- unique within the set of load balancers associated with your AWS account.
chcLoadBalancerName :: Lens' ConfigureHealthCheck Text
chcLoadBalancerName =
    lens _chcLoadBalancerName (\s a -> s { _chcLoadBalancerName = a })

newtype ConfigureHealthCheckResponse = ConfigureHealthCheckResponse
    { _chcrHealthCheck :: Maybe HealthCheck
    } deriving (Eq, Show, Generic)

-- | 'ConfigureHealthCheckResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'chcrHealthCheck' @::@ 'Maybe' 'HealthCheck'
--
configureHealthCheckResponse :: ConfigureHealthCheckResponse
configureHealthCheckResponse = ConfigureHealthCheckResponse
    { _chcrHealthCheck = Nothing
    }

-- | The updated healthcheck for the instances.
chcrHealthCheck :: Lens' ConfigureHealthCheckResponse (Maybe HealthCheck)
chcrHealthCheck = lens _chcrHealthCheck (\s a -> s { _chcrHealthCheck = a })

instance ToPath ConfigureHealthCheck where
    toPath = const "/"

instance ToQuery ConfigureHealthCheck

instance ToHeaders ConfigureHealthCheck

instance AWSRequest ConfigureHealthCheck where
    type Sv ConfigureHealthCheck = ELB
    type Rs ConfigureHealthCheck = ConfigureHealthCheckResponse

    request  = post "ConfigureHealthCheck"
    response = xmlResponse

instance FromXML ConfigureHealthCheckResponse where
    parseXML c = ConfigureHealthCheckResponse
        <$> c .:? "HealthCheck"
