{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELB.DeleteLoadBalancerListeners
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified listeners from the specified load balancer.
--
-- <http://docs.aws.amazon.com/ElasticLoadBalancing/latest/APIReference/API_DeleteLoadBalancerListeners.html>
module Network.AWS.ELB.DeleteLoadBalancerListeners
    (
    -- * Request
      DeleteLoadBalancerListeners
    -- ** Request constructor
    , deleteLoadBalancerListeners
    -- ** Request lenses
    , dlblrqLoadBalancerName
    , dlblrqLoadBalancerPorts

    -- * Response
    , DeleteLoadBalancerListenersResponse
    -- ** Response constructor
    , deleteLoadBalancerListenersResponse
    -- ** Response lenses
    , dlblrsStatus
    ) where

import           Network.AWS.ELB.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'deleteLoadBalancerListeners' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dlblrqLoadBalancerName'
--
-- * 'dlblrqLoadBalancerPorts'
data DeleteLoadBalancerListeners = DeleteLoadBalancerListeners'
    { _dlblrqLoadBalancerName  :: !Text
    , _dlblrqLoadBalancerPorts :: ![Int]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteLoadBalancerListeners' smart constructor.
deleteLoadBalancerListeners :: Text -> DeleteLoadBalancerListeners
deleteLoadBalancerListeners pLoadBalancerName =
    DeleteLoadBalancerListeners'
    { _dlblrqLoadBalancerName = pLoadBalancerName
    , _dlblrqLoadBalancerPorts = mempty
    }

-- | The name of the load balancer.
dlblrqLoadBalancerName :: Lens' DeleteLoadBalancerListeners Text
dlblrqLoadBalancerName = lens _dlblrqLoadBalancerName (\ s a -> s{_dlblrqLoadBalancerName = a});

-- | The client port numbers of the listeners.
dlblrqLoadBalancerPorts :: Lens' DeleteLoadBalancerListeners [Int]
dlblrqLoadBalancerPorts = lens _dlblrqLoadBalancerPorts (\ s a -> s{_dlblrqLoadBalancerPorts = a});

instance AWSRequest DeleteLoadBalancerListeners where
        type Sv DeleteLoadBalancerListeners = ELB
        type Rs DeleteLoadBalancerListeners =
             DeleteLoadBalancerListenersResponse
        request = post
        response
          = receiveXMLWrapper
              "DeleteLoadBalancerListenersResult"
              (\ s h x ->
                 DeleteLoadBalancerListenersResponse' <$>
                   (pure (fromEnum s)))

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
               "LoadBalancerName" =: _dlblrqLoadBalancerName,
               "LoadBalancerPorts" =:
                 toQueryList "member" _dlblrqLoadBalancerPorts]

-- | /See:/ 'deleteLoadBalancerListenersResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dlblrsStatus'
newtype DeleteLoadBalancerListenersResponse = DeleteLoadBalancerListenersResponse'
    { _dlblrsStatus :: Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteLoadBalancerListenersResponse' smart constructor.
deleteLoadBalancerListenersResponse :: Int -> DeleteLoadBalancerListenersResponse
deleteLoadBalancerListenersResponse pStatus =
    DeleteLoadBalancerListenersResponse'
    { _dlblrsStatus = pStatus
    }

-- | FIXME: Undocumented member.
dlblrsStatus :: Lens' DeleteLoadBalancerListenersResponse Int
dlblrsStatus = lens _dlblrsStatus (\ s a -> s{_dlblrsStatus = a});
