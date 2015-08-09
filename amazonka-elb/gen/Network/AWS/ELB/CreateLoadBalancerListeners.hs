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
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates one or more listeners for the specified load balancer. If a
-- listener with the specified port does not already exist, it is created;
-- otherwise, the properties of the new listener must match the properties
-- of the existing listener.
--
-- For more information, see
-- <http://docs.aws.amazon.com/ElasticLoadBalancing/latest/DeveloperGuide/us-add-listener.html Add a Listener to Your Load Balancer>
-- in the /Elastic Load Balancing Developer Guide/.
--
-- /See:/ <http://docs.aws.amazon.com/ElasticLoadBalancing/latest/APIReference/API_CreateLoadBalancerListeners.html AWS API Reference> for CreateLoadBalancerListeners.
module Network.AWS.ELB.CreateLoadBalancerListeners
    (
    -- * Creating a Request
      CreateLoadBalancerListeners
    , createLoadBalancerListeners
    -- * Request Lenses
    , clblLoadBalancerName
    , clblListeners

    -- * Destructuring the Response
    , CreateLoadBalancerListenersResponse
    , createLoadBalancerListenersResponse
    -- * Response Lenses
    , clblrsStatus
    ) where

import           Network.AWS.ELB.Types
import           Network.AWS.ELB.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'createLoadBalancerListeners' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'clblLoadBalancerName'
--
-- * 'clblListeners'
data CreateLoadBalancerListeners = CreateLoadBalancerListeners'
    { _clblLoadBalancerName :: !Text
    , _clblListeners        :: ![Listener]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateLoadBalancerListeners' smart constructor.
createLoadBalancerListeners :: Text -> CreateLoadBalancerListeners
createLoadBalancerListeners pLoadBalancerName_ =
    CreateLoadBalancerListeners'
    { _clblLoadBalancerName = pLoadBalancerName_
    , _clblListeners = mempty
    }

-- | The name of the load balancer.
clblLoadBalancerName :: Lens' CreateLoadBalancerListeners Text
clblLoadBalancerName = lens _clblLoadBalancerName (\ s a -> s{_clblLoadBalancerName = a});

-- | The listeners.
clblListeners :: Lens' CreateLoadBalancerListeners [Listener]
clblListeners = lens _clblListeners (\ s a -> s{_clblListeners = a}) . _Coerce;

instance AWSRequest CreateLoadBalancerListeners where
        type Sv CreateLoadBalancerListeners = ELB
        type Rs CreateLoadBalancerListeners =
             CreateLoadBalancerListenersResponse
        request = postQuery
        response
          = receiveXMLWrapper
              "CreateLoadBalancerListenersResult"
              (\ s h x ->
                 CreateLoadBalancerListenersResponse' <$>
                   (pure (fromEnum s)))

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
--
-- The fields accessible through corresponding lenses are:
--
-- * 'clblrsStatus'
newtype CreateLoadBalancerListenersResponse = CreateLoadBalancerListenersResponse'
    { _clblrsStatus :: Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateLoadBalancerListenersResponse' smart constructor.
createLoadBalancerListenersResponse :: Int -> CreateLoadBalancerListenersResponse
createLoadBalancerListenersResponse pStatus_ =
    CreateLoadBalancerListenersResponse'
    { _clblrsStatus = pStatus_
    }

-- | Undocumented member.
clblrsStatus :: Lens' CreateLoadBalancerListenersResponse Int
clblrsStatus = lens _clblrsStatus (\ s a -> s{_clblrsStatus = a});
