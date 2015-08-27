{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELB.ConfigureHealthCheck
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Specifies the health check settings to use when evaluating the health
-- state of your back-end instances.
--
-- For more information, see
-- <http://docs.aws.amazon.com/ElasticLoadBalancing/latest/DeveloperGuide/elb-healthchecks.html Configure Health Checks>
-- in the /Elastic Load Balancing Developer Guide/.
--
-- /See:/ <http://docs.aws.amazon.com/ElasticLoadBalancing/latest/APIReference/API_ConfigureHealthCheck.html AWS API Reference> for ConfigureHealthCheck.
module Network.AWS.ELB.ConfigureHealthCheck
    (
    -- * Creating a Request
      configureHealthCheck
    , ConfigureHealthCheck
    -- * Request Lenses
    , chcLoadBalancerName
    , chcHealthCheck

    -- * Destructuring the Response
    , configureHealthCheckResponse
    , ConfigureHealthCheckResponse
    -- * Response Lenses
    , chcrsHealthCheck
    , chcrsStatus
    ) where

import           Network.AWS.ELB.Types
import           Network.AWS.ELB.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'configureHealthCheck' smart constructor.
data ConfigureHealthCheck = ConfigureHealthCheck'
    { _chcLoadBalancerName :: !Text
    , _chcHealthCheck      :: !HealthCheck
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ConfigureHealthCheck' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'chcLoadBalancerName'
--
-- * 'chcHealthCheck'
configureHealthCheck
    :: Text -- ^ 'chcLoadBalancerName'
    -> HealthCheck -- ^ 'chcHealthCheck'
    -> ConfigureHealthCheck
configureHealthCheck pLoadBalancerName_ pHealthCheck_ =
    ConfigureHealthCheck'
    { _chcLoadBalancerName = pLoadBalancerName_
    , _chcHealthCheck = pHealthCheck_
    }

-- | The name of the load balancer.
chcLoadBalancerName :: Lens' ConfigureHealthCheck Text
chcLoadBalancerName = lens _chcLoadBalancerName (\ s a -> s{_chcLoadBalancerName = a});

-- | The configuration information for the new health check.
chcHealthCheck :: Lens' ConfigureHealthCheck HealthCheck
chcHealthCheck = lens _chcHealthCheck (\ s a -> s{_chcHealthCheck = a});

instance AWSRequest ConfigureHealthCheck where
        type Rs ConfigureHealthCheck =
             ConfigureHealthCheckResponse
        request = postQuery eLB
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
               "LoadBalancerName" =: _chcLoadBalancerName,
               "HealthCheck" =: _chcHealthCheck]

-- | /See:/ 'configureHealthCheckResponse' smart constructor.
data ConfigureHealthCheckResponse = ConfigureHealthCheckResponse'
    { _chcrsHealthCheck :: !(Maybe HealthCheck)
    , _chcrsStatus      :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ConfigureHealthCheckResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'chcrsHealthCheck'
--
-- * 'chcrsStatus'
configureHealthCheckResponse
    :: Int -- ^ 'chcrsStatus'
    -> ConfigureHealthCheckResponse
configureHealthCheckResponse pStatus_ =
    ConfigureHealthCheckResponse'
    { _chcrsHealthCheck = Nothing
    , _chcrsStatus = pStatus_
    }

-- | The updated health check.
chcrsHealthCheck :: Lens' ConfigureHealthCheckResponse (Maybe HealthCheck)
chcrsHealthCheck = lens _chcrsHealthCheck (\ s a -> s{_chcrsHealthCheck = a});

-- | The response status code.
chcrsStatus :: Lens' ConfigureHealthCheckResponse Int
chcrsStatus = lens _chcrsStatus (\ s a -> s{_chcrsStatus = a});
