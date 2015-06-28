{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.ELB.ConfigureHealthCheck
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Specifies the health check settings to use when evaluating the health
-- state of your back-end instances.
--
-- For more information, see
-- <http://docs.aws.amazon.com/ElasticLoadBalancing/latest/DeveloperGuide/elb-healthchecks.html Configure Health Checks>
-- in the /Elastic Load Balancing Developer Guide/.
--
-- <http://docs.aws.amazon.com/ElasticLoadBalancing/latest/APIReference/API_ConfigureHealthCheck.html>
module Network.AWS.ELB.ConfigureHealthCheck
    (
    -- * Request
      ConfigureHealthCheck
    -- ** Request constructor
    , configureHealthCheck
    -- ** Request lenses
    , chcLoadBalancerName
    , chcHealthCheck

    -- * Response
    , ConfigureHealthCheckResponse
    -- ** Response constructor
    , configureHealthCheckResponse
    -- ** Response lenses
    , chcrHealthCheck
    , chcrStatus
    ) where

import           Network.AWS.ELB.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'configureHealthCheck' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'chcLoadBalancerName'
--
-- * 'chcHealthCheck'
data ConfigureHealthCheck = ConfigureHealthCheck'
    { _chcLoadBalancerName :: !Text
    , _chcHealthCheck      :: !HealthCheck
    } deriving (Eq,Read,Show)

-- | 'ConfigureHealthCheck' smart constructor.
configureHealthCheck :: Text -> HealthCheck -> ConfigureHealthCheck
configureHealthCheck pLoadBalancerName pHealthCheck =
    ConfigureHealthCheck'
    { _chcLoadBalancerName = pLoadBalancerName
    , _chcHealthCheck = pHealthCheck
    }

-- | The name of the load balancer.
chcLoadBalancerName :: Lens' ConfigureHealthCheck Text
chcLoadBalancerName = lens _chcLoadBalancerName (\ s a -> s{_chcLoadBalancerName = a});

-- | The configuration information for the new health check.
chcHealthCheck :: Lens' ConfigureHealthCheck HealthCheck
chcHealthCheck = lens _chcHealthCheck (\ s a -> s{_chcHealthCheck = a});

instance AWSRequest ConfigureHealthCheck where
        type Sv ConfigureHealthCheck = ELB
        type Rs ConfigureHealthCheck =
             ConfigureHealthCheckResponse
        request = post
        response
          = receiveXMLWrapper "ConfigureHealthCheckResult"
              (\ s h x ->
                 ConfigureHealthCheckResponse' <$>
                   (x .@? "HealthCheck") <*> (pure s))

instance ToHeaders ConfigureHealthCheck where
        toHeaders = const mempty

instance ToPath ConfigureHealthCheck where
        toPath = const "/"

instance ToQuery ConfigureHealthCheck where
        toQuery ConfigureHealthCheck'{..}
          = mconcat
              ["Action" =: ("ConfigureHealthCheck" :: ByteString),
               "Version" =: ("2012-06-01" :: ByteString),
               "LoadBalancerName" =: _chcLoadBalancerName,
               "HealthCheck" =: _chcHealthCheck]

-- | /See:/ 'configureHealthCheckResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'chcrHealthCheck'
--
-- * 'chcrStatus'
data ConfigureHealthCheckResponse = ConfigureHealthCheckResponse'
    { _chcrHealthCheck :: !(Maybe HealthCheck)
    , _chcrStatus      :: !Status
    } deriving (Eq,Show)

-- | 'ConfigureHealthCheckResponse' smart constructor.
configureHealthCheckResponse :: Status -> ConfigureHealthCheckResponse
configureHealthCheckResponse pStatus =
    ConfigureHealthCheckResponse'
    { _chcrHealthCheck = Nothing
    , _chcrStatus = pStatus
    }

-- | The updated health check.
chcrHealthCheck :: Lens' ConfigureHealthCheckResponse (Maybe HealthCheck)
chcrHealthCheck = lens _chcrHealthCheck (\ s a -> s{_chcrHealthCheck = a});

-- | FIXME: Undocumented member.
chcrStatus :: Lens' ConfigureHealthCheckResponse Status
chcrStatus = lens _chcrStatus (\ s a -> s{_chcrStatus = a});
