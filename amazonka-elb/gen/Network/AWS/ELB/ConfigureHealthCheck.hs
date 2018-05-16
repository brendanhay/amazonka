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
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Specifies the health check settings to use when evaluating the health state of your EC2 instances.
--
--
-- For more information, see <http://docs.aws.amazon.com/elasticloadbalancing/latest/classic/elb-healthchecks.html Configure Health Checks for Your Load Balancer> in the /Classic Load Balancer Guide/ .
--
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
    , chcrsResponseStatus
    ) where

import Network.AWS.ELB.Types
import Network.AWS.ELB.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Contains the parameters for ConfigureHealthCheck.
--
--
--
-- /See:/ 'configureHealthCheck' smart constructor.
data ConfigureHealthCheck = ConfigureHealthCheck'
  { _chcLoadBalancerName :: !Text
  , _chcHealthCheck      :: !HealthCheck
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ConfigureHealthCheck' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'chcLoadBalancerName' - The name of the load balancer.
--
-- * 'chcHealthCheck' - The configuration information.
configureHealthCheck
    :: Text -- ^ 'chcLoadBalancerName'
    -> HealthCheck -- ^ 'chcHealthCheck'
    -> ConfigureHealthCheck
configureHealthCheck pLoadBalancerName_ pHealthCheck_ =
  ConfigureHealthCheck'
    {_chcLoadBalancerName = pLoadBalancerName_, _chcHealthCheck = pHealthCheck_}


-- | The name of the load balancer.
chcLoadBalancerName :: Lens' ConfigureHealthCheck Text
chcLoadBalancerName = lens _chcLoadBalancerName (\ s a -> s{_chcLoadBalancerName = a})

-- | The configuration information.
chcHealthCheck :: Lens' ConfigureHealthCheck HealthCheck
chcHealthCheck = lens _chcHealthCheck (\ s a -> s{_chcHealthCheck = a})

instance AWSRequest ConfigureHealthCheck where
        type Rs ConfigureHealthCheck =
             ConfigureHealthCheckResponse
        request = postQuery elb
        response
          = receiveXMLWrapper "ConfigureHealthCheckResult"
              (\ s h x ->
                 ConfigureHealthCheckResponse' <$>
                   (x .@? "HealthCheck") <*> (pure (fromEnum s)))

instance Hashable ConfigureHealthCheck where

instance NFData ConfigureHealthCheck where

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

-- | Contains the output of ConfigureHealthCheck.
--
--
--
-- /See:/ 'configureHealthCheckResponse' smart constructor.
data ConfigureHealthCheckResponse = ConfigureHealthCheckResponse'
  { _chcrsHealthCheck    :: !(Maybe HealthCheck)
  , _chcrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ConfigureHealthCheckResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'chcrsHealthCheck' - The updated health check.
--
-- * 'chcrsResponseStatus' - -- | The response status code.
configureHealthCheckResponse
    :: Int -- ^ 'chcrsResponseStatus'
    -> ConfigureHealthCheckResponse
configureHealthCheckResponse pResponseStatus_ =
  ConfigureHealthCheckResponse'
    {_chcrsHealthCheck = Nothing, _chcrsResponseStatus = pResponseStatus_}


-- | The updated health check.
chcrsHealthCheck :: Lens' ConfigureHealthCheckResponse (Maybe HealthCheck)
chcrsHealthCheck = lens _chcrsHealthCheck (\ s a -> s{_chcrsHealthCheck = a})

-- | -- | The response status code.
chcrsResponseStatus :: Lens' ConfigureHealthCheckResponse Int
chcrsResponseStatus = lens _chcrsResponseStatus (\ s a -> s{_chcrsResponseStatus = a})

instance NFData ConfigureHealthCheckResponse where
