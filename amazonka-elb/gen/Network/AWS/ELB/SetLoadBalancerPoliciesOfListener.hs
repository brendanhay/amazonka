{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELB.SetLoadBalancerPoliciesOfListener
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Associates, updates, or disables a policy with a listener for the
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
    , slbpolrqLoadBalancerName
    , slbpolrqLoadBalancerPort
    , slbpolrqPolicyNames

    -- * Response
    , SetLoadBalancerPoliciesOfListenerResponse
    -- ** Response constructor
    , setLoadBalancerPoliciesOfListenerResponse
    -- ** Response lenses
    , slbpolrsStatus
    ) where

import           Network.AWS.ELB.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'setLoadBalancerPoliciesOfListener' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'slbpolrqLoadBalancerName'
--
-- * 'slbpolrqLoadBalancerPort'
--
-- * 'slbpolrqPolicyNames'
data SetLoadBalancerPoliciesOfListener = SetLoadBalancerPoliciesOfListener'
    { _slbpolrqLoadBalancerName :: !Text
    , _slbpolrqLoadBalancerPort :: !Int
    , _slbpolrqPolicyNames      :: ![Text]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'SetLoadBalancerPoliciesOfListener' smart constructor.
setLoadBalancerPoliciesOfListener :: Text -> Int -> SetLoadBalancerPoliciesOfListener
setLoadBalancerPoliciesOfListener pLoadBalancerName pLoadBalancerPort =
    SetLoadBalancerPoliciesOfListener'
    { _slbpolrqLoadBalancerName = pLoadBalancerName
    , _slbpolrqLoadBalancerPort = pLoadBalancerPort
    , _slbpolrqPolicyNames = mempty
    }

-- | The name of the load balancer.
slbpolrqLoadBalancerName :: Lens' SetLoadBalancerPoliciesOfListener Text
slbpolrqLoadBalancerName = lens _slbpolrqLoadBalancerName (\ s a -> s{_slbpolrqLoadBalancerName = a});

-- | The external port of the load balancer for the policy.
slbpolrqLoadBalancerPort :: Lens' SetLoadBalancerPoliciesOfListener Int
slbpolrqLoadBalancerPort = lens _slbpolrqLoadBalancerPort (\ s a -> s{_slbpolrqLoadBalancerPort = a});

-- | The names of the policies. If the list is empty, the current policy is
-- removed from the listener.
slbpolrqPolicyNames :: Lens' SetLoadBalancerPoliciesOfListener [Text]
slbpolrqPolicyNames = lens _slbpolrqPolicyNames (\ s a -> s{_slbpolrqPolicyNames = a});

instance AWSRequest SetLoadBalancerPoliciesOfListener
         where
        type Sv SetLoadBalancerPoliciesOfListener = ELB
        type Rs SetLoadBalancerPoliciesOfListener =
             SetLoadBalancerPoliciesOfListenerResponse
        request = post
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
               "LoadBalancerName" =: _slbpolrqLoadBalancerName,
               "LoadBalancerPort" =: _slbpolrqLoadBalancerPort,
               "PolicyNames" =:
                 toQueryList "member" _slbpolrqPolicyNames]

-- | /See:/ 'setLoadBalancerPoliciesOfListenerResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'slbpolrsStatus'
newtype SetLoadBalancerPoliciesOfListenerResponse = SetLoadBalancerPoliciesOfListenerResponse'
    { _slbpolrsStatus :: Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'SetLoadBalancerPoliciesOfListenerResponse' smart constructor.
setLoadBalancerPoliciesOfListenerResponse :: Int -> SetLoadBalancerPoliciesOfListenerResponse
setLoadBalancerPoliciesOfListenerResponse pStatus =
    SetLoadBalancerPoliciesOfListenerResponse'
    { _slbpolrsStatus = pStatus
    }

-- | FIXME: Undocumented member.
slbpolrsStatus :: Lens' SetLoadBalancerPoliciesOfListenerResponse Int
slbpolrsStatus = lens _slbpolrsStatus (\ s a -> s{_slbpolrsStatus = a});
