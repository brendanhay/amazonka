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
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Replaces the current set of policies for the specified load balancer port with the specified set of policies.
--
--
-- To enable back-end server authentication, use 'SetLoadBalancerPoliciesForBackendServer' .
--
-- For more information about setting policies, see <http://docs.aws.amazon.com/elasticloadbalancing/latest/classic/ssl-config-update.html Update the SSL Negotiation Configuration> , <http://docs.aws.amazon.com/elasticloadbalancing/latest/classic/elb-sticky-sessions.html#enable-sticky-sessions-duration Duration-Based Session Stickiness> , and <http://docs.aws.amazon.com/elasticloadbalancing/latest/classic/elb-sticky-sessions.html#enable-sticky-sessions-application Application-Controlled Session Stickiness> in the /Classic Load Balancer Guide/ .
--
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
    , slbpolrsResponseStatus
    ) where

import Network.AWS.ELB.Types
import Network.AWS.ELB.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Contains the parameters for SetLoadBalancePoliciesOfListener.
--
--
--
-- /See:/ 'setLoadBalancerPoliciesOfListener' smart constructor.
data SetLoadBalancerPoliciesOfListener = SetLoadBalancerPoliciesOfListener'
  { _slbpolLoadBalancerName :: !Text
  , _slbpolLoadBalancerPort :: !Int
  , _slbpolPolicyNames      :: ![Text]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SetLoadBalancerPoliciesOfListener' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'slbpolLoadBalancerName' - The name of the load balancer.
--
-- * 'slbpolLoadBalancerPort' - The external port of the load balancer.
--
-- * 'slbpolPolicyNames' - The names of the policies. This list must include all policies to be enabled. If you omit a policy that is currently enabled, it is disabled. If the list is empty, all current policies are disabled.
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
slbpolLoadBalancerName = lens _slbpolLoadBalancerName (\ s a -> s{_slbpolLoadBalancerName = a})

-- | The external port of the load balancer.
slbpolLoadBalancerPort :: Lens' SetLoadBalancerPoliciesOfListener Int
slbpolLoadBalancerPort = lens _slbpolLoadBalancerPort (\ s a -> s{_slbpolLoadBalancerPort = a})

-- | The names of the policies. This list must include all policies to be enabled. If you omit a policy that is currently enabled, it is disabled. If the list is empty, all current policies are disabled.
slbpolPolicyNames :: Lens' SetLoadBalancerPoliciesOfListener [Text]
slbpolPolicyNames = lens _slbpolPolicyNames (\ s a -> s{_slbpolPolicyNames = a}) . _Coerce

instance AWSRequest SetLoadBalancerPoliciesOfListener
         where
        type Rs SetLoadBalancerPoliciesOfListener =
             SetLoadBalancerPoliciesOfListenerResponse
        request = postQuery elb
        response
          = receiveXMLWrapper
              "SetLoadBalancerPoliciesOfListenerResult"
              (\ s h x ->
                 SetLoadBalancerPoliciesOfListenerResponse' <$>
                   (pure (fromEnum s)))

instance Hashable SetLoadBalancerPoliciesOfListener
         where

instance NFData SetLoadBalancerPoliciesOfListener
         where

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

-- | Contains the output of SetLoadBalancePoliciesOfListener.
--
--
--
-- /See:/ 'setLoadBalancerPoliciesOfListenerResponse' smart constructor.
newtype SetLoadBalancerPoliciesOfListenerResponse = SetLoadBalancerPoliciesOfListenerResponse'
  { _slbpolrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SetLoadBalancerPoliciesOfListenerResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'slbpolrsResponseStatus' - -- | The response status code.
setLoadBalancerPoliciesOfListenerResponse
    :: Int -- ^ 'slbpolrsResponseStatus'
    -> SetLoadBalancerPoliciesOfListenerResponse
setLoadBalancerPoliciesOfListenerResponse pResponseStatus_ =
  SetLoadBalancerPoliciesOfListenerResponse'
    {_slbpolrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
slbpolrsResponseStatus :: Lens' SetLoadBalancerPoliciesOfListenerResponse Int
slbpolrsResponseStatus = lens _slbpolrsResponseStatus (\ s a -> s{_slbpolrsResponseStatus = a})

instance NFData
           SetLoadBalancerPoliciesOfListenerResponse
         where
