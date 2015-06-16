{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.ELB.CreateLoadBalancerListeners
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

-- | Creates one or more listeners for the specified load balancer. If a
-- listener with the specified port does not already exist, it is created;
-- otherwise, the properties of the new listener must match the properties
-- of the existing listener.
--
-- For more information, see
-- <http://docs.aws.amazon.com/ElasticLoadBalancing/latest/DeveloperGuide/us-add-listener.html Add a Listener to Your Load Balancer>
-- in the /Elastic Load Balancing Developer Guide/.
--
-- <http://docs.aws.amazon.com/ElasticLoadBalancing/latest/APIReference/API_CreateLoadBalancerListeners.html>
module Network.AWS.ELB.CreateLoadBalancerListeners
    (
    -- * Request
      CreateLoadBalancerListeners
    -- ** Request constructor
    , createLoadBalancerListeners
    -- ** Request lenses
    , clblLoadBalancerName
    , clblListeners

    -- * Response
    , CreateLoadBalancerListenersResponse
    -- ** Response constructor
    , createLoadBalancerListenersResponse
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.ELB.Types

-- | /See:/ 'createLoadBalancerListeners' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'clblLoadBalancerName'
--
-- * 'clblListeners'
data CreateLoadBalancerListeners = CreateLoadBalancerListeners'{_clblLoadBalancerName :: Text, _clblListeners :: [Listener]} deriving (Eq, Read, Show)

-- | 'CreateLoadBalancerListeners' smart constructor.
createLoadBalancerListeners :: Text -> CreateLoadBalancerListeners
createLoadBalancerListeners pLoadBalancerName = CreateLoadBalancerListeners'{_clblLoadBalancerName = pLoadBalancerName, _clblListeners = mempty};

-- | The name of the load balancer.
clblLoadBalancerName :: Lens' CreateLoadBalancerListeners Text
clblLoadBalancerName = lens _clblLoadBalancerName (\ s a -> s{_clblLoadBalancerName = a});

-- | The listeners.
clblListeners :: Lens' CreateLoadBalancerListeners [Listener]
clblListeners = lens _clblListeners (\ s a -> s{_clblListeners = a});

instance AWSRequest CreateLoadBalancerListeners where
        type Sv CreateLoadBalancerListeners = ELB
        type Rs CreateLoadBalancerListeners =
             CreateLoadBalancerListenersResponse
        request = post
        response
          = receiveNull CreateLoadBalancerListenersResponse'

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

-- | /See:/ 'createLoadBalancerListenersResponse' smart constructor.
data CreateLoadBalancerListenersResponse = CreateLoadBalancerListenersResponse' deriving (Eq, Read, Show)

-- | 'CreateLoadBalancerListenersResponse' smart constructor.
createLoadBalancerListenersResponse :: CreateLoadBalancerListenersResponse
createLoadBalancerListenersResponse = CreateLoadBalancerListenersResponse';
