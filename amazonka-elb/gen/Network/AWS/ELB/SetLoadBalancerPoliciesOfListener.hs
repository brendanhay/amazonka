{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.ELB.SetLoadBalancerPoliciesOfListener
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

-- | Associates, updates, or disables a policy with a listener for the
-- specified load balancer. You can associate multiple policies with a
-- listener.
--
-- <http://docs.aws.amazon.com/ElasticLoadBalancing/latest/APIReference/API_SetLoadBalancerPoliciesOfListener.html>
module Network.AWS.ELB.SetLoadBalancerPoliciesOfListener
    (
    -- * Request
      SetLoadBalancerPoliciesOfListener
    -- ** Request constructor
    , setLoadBalancerPoliciesOfListener
    -- ** Request lenses
    , slbpolLoadBalancerName
    , slbpolLoadBalancerPort
    , slbpolPolicyNames

    -- * Response
    , SetLoadBalancerPoliciesOfListenerResponse
    -- ** Response constructor
    , setLoadBalancerPoliciesOfListenerResponse
    ) where

import Network.AWS.ELB.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'setLoadBalancerPoliciesOfListener' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'slbpolLoadBalancerName'
--
-- * 'slbpolLoadBalancerPort'
--
-- * 'slbpolPolicyNames'
data SetLoadBalancerPoliciesOfListener = SetLoadBalancerPoliciesOfListener'{_slbpolLoadBalancerName :: Text, _slbpolLoadBalancerPort :: Int, _slbpolPolicyNames :: [Text]} deriving (Eq, Read, Show)

-- | 'SetLoadBalancerPoliciesOfListener' smart constructor.
setLoadBalancerPoliciesOfListener :: Text -> Int -> SetLoadBalancerPoliciesOfListener
setLoadBalancerPoliciesOfListener pLoadBalancerName pLoadBalancerPort = SetLoadBalancerPoliciesOfListener'{_slbpolLoadBalancerName = pLoadBalancerName, _slbpolLoadBalancerPort = pLoadBalancerPort, _slbpolPolicyNames = mempty};

-- | The name of the load balancer.
slbpolLoadBalancerName :: Lens' SetLoadBalancerPoliciesOfListener Text
slbpolLoadBalancerName = lens _slbpolLoadBalancerName (\ s a -> s{_slbpolLoadBalancerName = a});

-- | The external port of the load balancer for the policy.
slbpolLoadBalancerPort :: Lens' SetLoadBalancerPoliciesOfListener Int
slbpolLoadBalancerPort = lens _slbpolLoadBalancerPort (\ s a -> s{_slbpolLoadBalancerPort = a});

-- | The names of the policies. If the list is empty, the current policy is
-- removed from the listener.
slbpolPolicyNames :: Lens' SetLoadBalancerPoliciesOfListener [Text]
slbpolPolicyNames = lens _slbpolPolicyNames (\ s a -> s{_slbpolPolicyNames = a});

instance AWSRequest SetLoadBalancerPoliciesOfListener
         where
        type Sv SetLoadBalancerPoliciesOfListener = ELB
        type Rs SetLoadBalancerPoliciesOfListener =
             SetLoadBalancerPoliciesOfListenerResponse
        request = post
        response
          = receiveNull
              SetLoadBalancerPoliciesOfListenerResponse'

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
data SetLoadBalancerPoliciesOfListenerResponse = SetLoadBalancerPoliciesOfListenerResponse' deriving (Eq, Read, Show)

-- | 'SetLoadBalancerPoliciesOfListenerResponse' smart constructor.
setLoadBalancerPoliciesOfListenerResponse :: SetLoadBalancerPoliciesOfListenerResponse
setLoadBalancerPoliciesOfListenerResponse = SetLoadBalancerPoliciesOfListenerResponse';
