{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELB.ConfigureHealthCheck
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Specifies the health check settings to use when evaluating the health
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
    , chcrqLoadBalancerName
    , chcrqHealthCheck

    -- * Response
    , ConfigureHealthCheckResponse
    -- ** Response constructor
    , configureHealthCheckResponse
    -- ** Response lenses
    , chcrsHealthCheck
    , chcrsStatus
    ) where

import           Network.AWS.ELB.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'configureHealthCheck' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'chcrqLoadBalancerName'
--
-- * 'chcrqHealthCheck'
data ConfigureHealthCheck = ConfigureHealthCheck'
    { _chcrqLoadBalancerName :: !Text
    , _chcrqHealthCheck      :: !HealthCheck
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ConfigureHealthCheck' smart constructor.
configureHealthCheck :: Text -> HealthCheck -> ConfigureHealthCheck
configureHealthCheck pLoadBalancerName pHealthCheck =
    ConfigureHealthCheck'
    { _chcrqLoadBalancerName = pLoadBalancerName
    , _chcrqHealthCheck = pHealthCheck
    }

-- | The name of the load balancer.
chcrqLoadBalancerName :: Lens' ConfigureHealthCheck Text
chcrqLoadBalancerName = lens _chcrqLoadBalancerName (\ s a -> s{_chcrqLoadBalancerName = a});

-- | The configuration information for the new health check.
chcrqHealthCheck :: Lens' ConfigureHealthCheck HealthCheck
chcrqHealthCheck = lens _chcrqHealthCheck (\ s a -> s{_chcrqHealthCheck = a});

instance AWSRequest ConfigureHealthCheck where
        type Sv ConfigureHealthCheck = ELB
        type Rs ConfigureHealthCheck =
             ConfigureHealthCheckResponse
        request = post
        response
          = receiveXMLWrapper "ConfigureHealthCheckResult"
              (\ s h x ->
                 ConfigureHealthCheckResponse' <$>
                   (x .@? "HealthCheck") <*> (pure (fromEnum s)))

instance ToHeaders ConfigureHealthCheck where
        toHeaders = const mempty

instance ToPath ConfigureHealthCheck where
        toPath = const "/"

instance ToQuery ConfigureHealthCheck where
        toQuery ConfigureHealthCheck'{..}
          = mconcat
              ["Action" =: ("ConfigureHealthCheck" :: ByteString),
               "Version" =: ("2012-06-01" :: ByteString),
               "LoadBalancerName" =: _chcrqLoadBalancerName,
               "HealthCheck" =: _chcrqHealthCheck]

-- | /See:/ 'configureHealthCheckResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'chcrsHealthCheck'
--
-- * 'chcrsStatus'
data ConfigureHealthCheckResponse = ConfigureHealthCheckResponse'
    { _chcrsHealthCheck :: !(Maybe HealthCheck)
    , _chcrsStatus      :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ConfigureHealthCheckResponse' smart constructor.
configureHealthCheckResponse :: Int -> ConfigureHealthCheckResponse
configureHealthCheckResponse pStatus =
    ConfigureHealthCheckResponse'
    { _chcrsHealthCheck = Nothing
    , _chcrsStatus = pStatus
    }

-- | The updated health check.
chcrsHealthCheck :: Lens' ConfigureHealthCheckResponse (Maybe HealthCheck)
chcrsHealthCheck = lens _chcrsHealthCheck (\ s a -> s{_chcrsHealthCheck = a});

-- | FIXME: Undocumented member.
chcrsStatus :: Lens' ConfigureHealthCheckResponse Int
chcrsStatus = lens _chcrsStatus (\ s a -> s{_chcrsStatus = a});
