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
-- Module      : Network.AWS.ELB.SetLoadBalancerPoliciesForBackendServer
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Replaces the set of policies associated with the specified port on which the back-end server is listening with a new set of policies. At this time, only the back-end server authentication policy type can be applied to the back-end ports; this policy type is composed of multiple public key policies.
--
-- Each time you use 'SetLoadBalancerPoliciesForBackendServer' to enable the policies, use the 'PolicyNames' parameter to list the policies that you want to enable.
--
-- You can use < DescribeLoadBalancers> or < DescribeLoadBalancerPolicies> to verify that the policy is associated with the back-end server.
module Network.AWS.ELB.SetLoadBalancerPoliciesForBackendServer
    (
    -- * Creating a Request
      setLoadBalancerPoliciesForBackendServer
    , SetLoadBalancerPoliciesForBackendServer
    -- * Request Lenses
    , slbpfbsLoadBalancerName
    , slbpfbsInstancePort
    , slbpfbsPolicyNames

    -- * Destructuring the Response
    , setLoadBalancerPoliciesForBackendServerResponse
    , SetLoadBalancerPoliciesForBackendServerResponse
    -- * Response Lenses
    , slbpfbsrsResponseStatus
    ) where

import           Network.AWS.ELB.Types
import           Network.AWS.ELB.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'setLoadBalancerPoliciesForBackendServer' smart constructor.
data SetLoadBalancerPoliciesForBackendServer = SetLoadBalancerPoliciesForBackendServer'
    { _slbpfbsLoadBalancerName :: !Text
    , _slbpfbsInstancePort     :: !Int
    , _slbpfbsPolicyNames      :: ![Text]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'SetLoadBalancerPoliciesForBackendServer' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'slbpfbsLoadBalancerName'
--
-- * 'slbpfbsInstancePort'
--
-- * 'slbpfbsPolicyNames'
setLoadBalancerPoliciesForBackendServer
    :: Text -- ^ 'slbpfbsLoadBalancerName'
    -> Int -- ^ 'slbpfbsInstancePort'
    -> SetLoadBalancerPoliciesForBackendServer
setLoadBalancerPoliciesForBackendServer pLoadBalancerName_ pInstancePort_ =
    SetLoadBalancerPoliciesForBackendServer'
    { _slbpfbsLoadBalancerName = pLoadBalancerName_
    , _slbpfbsInstancePort = pInstancePort_
    , _slbpfbsPolicyNames = mempty
    }

-- | The name of the load balancer.
slbpfbsLoadBalancerName :: Lens' SetLoadBalancerPoliciesForBackendServer Text
slbpfbsLoadBalancerName = lens _slbpfbsLoadBalancerName (\ s a -> s{_slbpfbsLoadBalancerName = a});

-- | The port number associated with the back-end server.
slbpfbsInstancePort :: Lens' SetLoadBalancerPoliciesForBackendServer Int
slbpfbsInstancePort = lens _slbpfbsInstancePort (\ s a -> s{_slbpfbsInstancePort = a});

-- | The names of the policies. If the list is empty, then all current polices are removed from the back-end server.
slbpfbsPolicyNames :: Lens' SetLoadBalancerPoliciesForBackendServer [Text]
slbpfbsPolicyNames = lens _slbpfbsPolicyNames (\ s a -> s{_slbpfbsPolicyNames = a}) . _Coerce;

instance AWSRequest
         SetLoadBalancerPoliciesForBackendServer where
        type Rs SetLoadBalancerPoliciesForBackendServer =
             SetLoadBalancerPoliciesForBackendServerResponse
        request = postQuery elb
        response
          = receiveXMLWrapper
              "SetLoadBalancerPoliciesForBackendServerResult"
              (\ s h x ->
                 SetLoadBalancerPoliciesForBackendServerResponse' <$>
                   (pure (fromEnum s)))

instance Hashable
         SetLoadBalancerPoliciesForBackendServer

instance NFData
         SetLoadBalancerPoliciesForBackendServer

instance ToHeaders
         SetLoadBalancerPoliciesForBackendServer where
        toHeaders = const mempty

instance ToPath
         SetLoadBalancerPoliciesForBackendServer where
        toPath = const "/"

instance ToQuery
         SetLoadBalancerPoliciesForBackendServer where
        toQuery SetLoadBalancerPoliciesForBackendServer'{..}
          = mconcat
              ["Action" =:
                 ("SetLoadBalancerPoliciesForBackendServer" ::
                    ByteString),
               "Version" =: ("2012-06-01" :: ByteString),
               "LoadBalancerName" =: _slbpfbsLoadBalancerName,
               "InstancePort" =: _slbpfbsInstancePort,
               "PolicyNames" =:
                 toQueryList "member" _slbpfbsPolicyNames]

-- | /See:/ 'setLoadBalancerPoliciesForBackendServerResponse' smart constructor.
newtype SetLoadBalancerPoliciesForBackendServerResponse = SetLoadBalancerPoliciesForBackendServerResponse'
    { _slbpfbsrsResponseStatus :: Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'SetLoadBalancerPoliciesForBackendServerResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'slbpfbsrsResponseStatus'
setLoadBalancerPoliciesForBackendServerResponse
    :: Int -- ^ 'slbpfbsrsResponseStatus'
    -> SetLoadBalancerPoliciesForBackendServerResponse
setLoadBalancerPoliciesForBackendServerResponse pResponseStatus_ =
    SetLoadBalancerPoliciesForBackendServerResponse'
    { _slbpfbsrsResponseStatus = pResponseStatus_
    }

-- | The response status code.
slbpfbsrsResponseStatus :: Lens' SetLoadBalancerPoliciesForBackendServerResponse Int
slbpfbsrsResponseStatus = lens _slbpfbsrsResponseStatus (\ s a -> s{_slbpfbsrsResponseStatus = a});

instance NFData
         SetLoadBalancerPoliciesForBackendServerResponse
