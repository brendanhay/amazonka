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
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified listeners from the specified load balancer.
--
-- /See:/ <http://docs.aws.amazon.com/ElasticLoadBalancing/latest/APIReference/API_DeleteLoadBalancerListeners.html AWS API Reference> for DeleteLoadBalancerListeners.
module Network.AWS.ELB.DeleteLoadBalancerListeners
    (
    -- * Creating a Request
      DeleteLoadBalancerListeners
    , deleteLoadBalancerListeners
    -- * Request Lenses
    , dlblLoadBalancerName
    , dlblLoadBalancerPorts

    -- * Destructuring the Response
    , DeleteLoadBalancerListenersResponse
    , deleteLoadBalancerListenersResponse
    -- * Response Lenses
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
-- * 'dlblLoadBalancerName'
--
-- * 'dlblLoadBalancerPorts'
data DeleteLoadBalancerListeners = DeleteLoadBalancerListeners'
    { _dlblLoadBalancerName  :: !Text
    , _dlblLoadBalancerPorts :: ![Int]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteLoadBalancerListeners' smart constructor.
deleteLoadBalancerListeners :: Text -> DeleteLoadBalancerListeners
deleteLoadBalancerListeners pLoadBalancerName_ =
    DeleteLoadBalancerListeners'
    { _dlblLoadBalancerName = pLoadBalancerName_
    , _dlblLoadBalancerPorts = mempty
    }

-- | The name of the load balancer.
dlblLoadBalancerName :: Lens' DeleteLoadBalancerListeners Text
dlblLoadBalancerName = lens _dlblLoadBalancerName (\ s a -> s{_dlblLoadBalancerName = a});

-- | The client port numbers of the listeners.
dlblLoadBalancerPorts :: Lens' DeleteLoadBalancerListeners [Int]
dlblLoadBalancerPorts = lens _dlblLoadBalancerPorts (\ s a -> s{_dlblLoadBalancerPorts = a}) . _Coerce;

instance AWSRequest DeleteLoadBalancerListeners where
        type Sv DeleteLoadBalancerListeners = ELB
        type Rs DeleteLoadBalancerListeners =
             DeleteLoadBalancerListenersResponse
        request = postQuery
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
               "LoadBalancerName" =: _dlblLoadBalancerName,
               "LoadBalancerPorts" =:
                 toQueryList "member" _dlblLoadBalancerPorts]

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
deleteLoadBalancerListenersResponse pStatus_ =
    DeleteLoadBalancerListenersResponse'
    { _dlblrsStatus = pStatus_
    }

-- | Undocumented member.
dlblrsStatus :: Lens' DeleteLoadBalancerListenersResponse Int
dlblrsStatus = lens _dlblrsStatus (\ s a -> s{_dlblrsStatus = a});
