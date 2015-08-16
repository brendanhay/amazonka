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
-- Module      : Network.AWS.ELB.SetLoadBalancerPoliciesOfListener
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates, updates, or disables a policy with a listener for the
-- specified load balancer. You can associate multiple policies with a
-- listener.
--
-- /See:/ <http://docs.aws.amazon.com/ElasticLoadBalancing/latest/APIReference/API_SetLoadBalancerPoliciesOfListener.html AWS API Reference> for SetLoadBalancerPoliciesOfListener.
module Network.AWS.ELB.SetLoadBalancerPoliciesOfListener
    (
    -- * Creating a Request
      setLoadBalancerPoliciesOfListener
    , SetLoadBalancerPoliciesOfListener
    -- * Request Lenses
    , slbpolLoadBalancerName
    , slbpolLoadBalancerPort
    , slbpolPolicyNames

    -- * Destructuring the Response
    , setLoadBalancerPoliciesOfListenerResponse
    , SetLoadBalancerPoliciesOfListenerResponse
    -- * Response Lenses
    , slbpolrsStatus
    ) where

import           Network.AWS.ELB.Types
import           Network.AWS.ELB.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'setLoadBalancerPoliciesOfListener' smart constructor.
data SetLoadBalancerPoliciesOfListener = SetLoadBalancerPoliciesOfListener'
    { _slbpolLoadBalancerName :: !Text
    , _slbpolLoadBalancerPort :: !Int
    , _slbpolPolicyNames      :: ![Text]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'SetLoadBalancerPoliciesOfListener' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'slbpolLoadBalancerName'
--
-- * 'slbpolLoadBalancerPort'
--
-- * 'slbpolPolicyNames'
setLoadBalancerPoliciesOfListener
    :: Text -- ^ 'slbpolLoadBalancerName'
    -> Int -- ^ 'slbpolLoadBalancerPort'
    -> SetLoadBalancerPoliciesOfListener
setLoadBalancerPoliciesOfListener pLoadBalancerName_ pLoadBalancerPort_ =
    SetLoadBalancerPoliciesOfListener'
    { _slbpolLoadBalancerName = pLoadBalancerName_
    , _slbpolLoadBalancerPort = pLoadBalancerPort_
    , _slbpolPolicyNames = mempty
    }

-- | The name of the load balancer.
slbpolLoadBalancerName :: Lens' SetLoadBalancerPoliciesOfListener Text
slbpolLoadBalancerName = lens _slbpolLoadBalancerName (\ s a -> s{_slbpolLoadBalancerName = a});

-- | The external port of the load balancer for the policy.
slbpolLoadBalancerPort :: Lens' SetLoadBalancerPoliciesOfListener Int
slbpolLoadBalancerPort = lens _slbpolLoadBalancerPort (\ s a -> s{_slbpolLoadBalancerPort = a});

-- | The names of the policies. If the list is empty, the current policy is
-- removed from the listener.
slbpolPolicyNames :: Lens' SetLoadBalancerPoliciesOfListener [Text]
slbpolPolicyNames = lens _slbpolPolicyNames (\ s a -> s{_slbpolPolicyNames = a}) . _Coerce;

instance AWSRequest SetLoadBalancerPoliciesOfListener
         where
        type Sv SetLoadBalancerPoliciesOfListener = ELB
        type Rs SetLoadBalancerPoliciesOfListener =
             SetLoadBalancerPoliciesOfListenerResponse
        request = postQuery
        response
          = receiveXMLWrapper
              "SetLoadBalancerPoliciesOfListenerResult"
              (\ s h x ->
                 SetLoadBalancerPoliciesOfListenerResponse' <$>
                   (pure (fromEnum s)))

instance ToHeaders SetLoadBalancerPoliciesOfListener
         where
        toHeaders = const mempty

instance ToPath SetLoadBalancerPoliciesOfListener
         where
        toPath = const "/"

instance ToQuery SetLoadBalancerPoliciesOfListener
         where
        toQuery SetLoadBalancerPoliciesOfListener'{..}
          = mconcat
              ["Action" =:
                 ("SetLoadBalancerPoliciesOfListener" :: ByteString),
               "Version" =: ("2012-06-01" :: ByteString),
               "LoadBalancerName" =: _slbpolLoadBalancerName,
               "LoadBalancerPort" =: _slbpolLoadBalancerPort,
               "PolicyNames" =:
                 toQueryList "member" _slbpolPolicyNames]

-- | /See:/ 'setLoadBalancerPoliciesOfListenerResponse' smart constructor.
newtype SetLoadBalancerPoliciesOfListenerResponse = SetLoadBalancerPoliciesOfListenerResponse'
    { _slbpolrsStatus :: Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'SetLoadBalancerPoliciesOfListenerResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'slbpolrsStatus'
setLoadBalancerPoliciesOfListenerResponse
    :: Int -- ^ 'slbpolrsStatus'
    -> SetLoadBalancerPoliciesOfListenerResponse
setLoadBalancerPoliciesOfListenerResponse pStatus_ =
    SetLoadBalancerPoliciesOfListenerResponse'
    { _slbpolrsStatus = pStatus_
    }

-- | The response status code.
slbpolrsStatus :: Lens' SetLoadBalancerPoliciesOfListenerResponse Int
slbpolrsStatus = lens _slbpolrsStatus (\ s a -> s{_slbpolrsStatus = a});
