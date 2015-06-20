{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.ELB.DeleteLoadBalancerListeners
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

-- | Deletes the specified listeners from the specified load balancer.
--
-- <http://docs.aws.amazon.com/ElasticLoadBalancing/latest/APIReference/API_DeleteLoadBalancerListeners.html>
module Network.AWS.ELB.DeleteLoadBalancerListeners
    (
    -- * Request
      DeleteLoadBalancerListeners
    -- ** Request constructor
    , deleteLoadBalancerListeners
    -- ** Request lenses
    , dlblLoadBalancerName
    , dlblLoadBalancerPorts

    -- * Response
    , DeleteLoadBalancerListenersResponse
    -- ** Response constructor
    , deleteLoadBalancerListenersResponse
    ) where

import Network.AWS.ELB.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteLoadBalancerListeners' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dlblLoadBalancerName'
--
-- * 'dlblLoadBalancerPorts'
data DeleteLoadBalancerListeners = DeleteLoadBalancerListeners'{_dlblLoadBalancerName :: Text, _dlblLoadBalancerPorts :: [Int]} deriving (Eq, Read, Show)

-- | 'DeleteLoadBalancerListeners' smart constructor.
deleteLoadBalancerListeners :: Text -> DeleteLoadBalancerListeners
deleteLoadBalancerListeners pLoadBalancerName = DeleteLoadBalancerListeners'{_dlblLoadBalancerName = pLoadBalancerName, _dlblLoadBalancerPorts = mempty};

-- | The name of the load balancer.
dlblLoadBalancerName :: Lens' DeleteLoadBalancerListeners Text
dlblLoadBalancerName = lens _dlblLoadBalancerName (\ s a -> s{_dlblLoadBalancerName = a});

-- | The client port numbers of the listeners.
dlblLoadBalancerPorts :: Lens' DeleteLoadBalancerListeners [Int]
dlblLoadBalancerPorts = lens _dlblLoadBalancerPorts (\ s a -> s{_dlblLoadBalancerPorts = a});

instance AWSPager A where
        page rq rs
          | stop True = Nothing
          | otherwise = Just

instance AWSRequest DeleteLoadBalancerListeners where
        type Sv DeleteLoadBalancerListeners = ELB
        type Rs DeleteLoadBalancerListeners =
             DeleteLoadBalancerListenersResponse
        request = post
        response
          = receiveNull DeleteLoadBalancerListenersResponse'

instance ToHeaders DeleteLoadBalancerListeners where
        toHeaders = const mempty

instance ToPath DeleteLoadBalancerListeners where
        toPath = const "/"

instance ToQuery DeleteLoadBalancerListeners where
        toQuery DeleteLoadBalancerListeners'{..}
          = mconcat
              ["Action" =:
                 ("DeleteLoadBalancerListeners" :: ByteString),
               "Version" =: ("2012-06-01" :: ByteString),
               "LoadBalancerName" =: _dlblLoadBalancerName,
               "LoadBalancerPorts" =:
                 toQueryList "member" _dlblLoadBalancerPorts]

-- | /See:/ 'deleteLoadBalancerListenersResponse' smart constructor.
data DeleteLoadBalancerListenersResponse = DeleteLoadBalancerListenersResponse' deriving (Eq, Read, Show)

-- | 'DeleteLoadBalancerListenersResponse' smart constructor.
deleteLoadBalancerListenersResponse :: DeleteLoadBalancerListenersResponse
deleteLoadBalancerListenersResponse = DeleteLoadBalancerListenersResponse';
