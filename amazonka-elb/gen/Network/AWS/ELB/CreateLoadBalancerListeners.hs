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
-- Module      : Network.AWS.ELB.CreateLoadBalancerListeners
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates one or more listeners for the specified load balancer. If a listener with the specified port does not already exist, it is created; otherwise, the properties of the new listener must match the properties of the existing listener.
--
--
-- For more information, see <http://docs.aws.amazon.com/elasticloadbalancing/latest/classic/elb-listener-config.html Listeners for Your Classic Load Balancer> in the /Classic Load Balancer Guide/ .
--
module Network.AWS.ELB.CreateLoadBalancerListeners
    (
    -- * Creating a Request
      createLoadBalancerListeners
    , CreateLoadBalancerListeners
    -- * Request Lenses
    , clblLoadBalancerName
    , clblListeners

    -- * Destructuring the Response
    , createLoadBalancerListenersResponse
    , CreateLoadBalancerListenersResponse
    -- * Response Lenses
    , clblrsResponseStatus
    ) where

import Network.AWS.ELB.Types
import Network.AWS.ELB.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Contains the parameters for CreateLoadBalancerListeners.
--
--
--
-- /See:/ 'createLoadBalancerListeners' smart constructor.
data CreateLoadBalancerListeners = CreateLoadBalancerListeners'
  { _clblLoadBalancerName :: !Text
  , _clblListeners        :: ![Listener]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateLoadBalancerListeners' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'clblLoadBalancerName' - The name of the load balancer.
--
-- * 'clblListeners' - The listeners.
createLoadBalancerListeners
    :: Text -- ^ 'clblLoadBalancerName'
    -> CreateLoadBalancerListeners
createLoadBalancerListeners pLoadBalancerName_ =
  CreateLoadBalancerListeners'
    {_clblLoadBalancerName = pLoadBalancerName_, _clblListeners = mempty}


-- | The name of the load balancer.
clblLoadBalancerName :: Lens' CreateLoadBalancerListeners Text
clblLoadBalancerName = lens _clblLoadBalancerName (\ s a -> s{_clblLoadBalancerName = a})

-- | The listeners.
clblListeners :: Lens' CreateLoadBalancerListeners [Listener]
clblListeners = lens _clblListeners (\ s a -> s{_clblListeners = a}) . _Coerce

instance AWSRequest CreateLoadBalancerListeners where
        type Rs CreateLoadBalancerListeners =
             CreateLoadBalancerListenersResponse
        request = postQuery elb
        response
          = receiveXMLWrapper
              "CreateLoadBalancerListenersResult"
              (\ s h x ->
                 CreateLoadBalancerListenersResponse' <$>
                   (pure (fromEnum s)))

instance Hashable CreateLoadBalancerListeners where

instance NFData CreateLoadBalancerListeners where

instance ToHeaders CreateLoadBalancerListeners where
        toHeaders = const mempty

instance ToPath CreateLoadBalancerListeners where
        toPath = const "/"

instance ToQuery CreateLoadBalancerListeners where
        toQuery CreateLoadBalancerListeners'{..}
          = mconcat
              ["Action" =:
                 ("CreateLoadBalancerListeners" :: ByteString),
               "Version" =: ("2012-06-01" :: ByteString),
               "LoadBalancerName" =: _clblLoadBalancerName,
               "Listeners" =: toQueryList "member" _clblListeners]

-- | Contains the parameters for CreateLoadBalancerListener.
--
--
--
-- /See:/ 'createLoadBalancerListenersResponse' smart constructor.
newtype CreateLoadBalancerListenersResponse = CreateLoadBalancerListenersResponse'
  { _clblrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateLoadBalancerListenersResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'clblrsResponseStatus' - -- | The response status code.
createLoadBalancerListenersResponse
    :: Int -- ^ 'clblrsResponseStatus'
    -> CreateLoadBalancerListenersResponse
createLoadBalancerListenersResponse pResponseStatus_ =
  CreateLoadBalancerListenersResponse'
    {_clblrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
clblrsResponseStatus :: Lens' CreateLoadBalancerListenersResponse Int
clblrsResponseStatus = lens _clblrsResponseStatus (\ s a -> s{_clblrsResponseStatus = a})

instance NFData CreateLoadBalancerListenersResponse
         where
