{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ELB.V2012_06_01.SetLoadBalancerPoliciesForBackendServer
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Replaces the current set of policies associated with a port on which the
-- back-end server is listening with a new set of policies. After the policies
-- have been created using CreateLoadBalancerPolicy, they can be applied here
-- as a list. At this time, only the back-end server authentication policy
-- type can be applied to the back-end ports; this policy type is composed of
-- multiple public key policies. The SetLoadBalancerPoliciesForBackendServer
-- replaces the current set of policies associated with the specified instance
-- port. Every time you use this action to enable the policies, use the
-- PolicyNames parameter to list all the policies you want to enable.
-- https://elasticloadbalancing.amazonaws.com/?InstancePort=80
-- &PolicyNames.member.1=EnableProxyProtocol
-- &PolicyNames.member.2=MyPolicyName2 &PolicyNames.member.3=MyPolicyName3
-- &LoadBalancerName=my-test-loadbalancer &Version=2012-06-01
-- &Action=SetLoadBalancerPoliciesForBackendServer &AUTHPARAMS
-- 0eb9b381-dde0-11e2-8d78-6ddbaEXAMPLE You can use DescribeLoadBalancers or
-- DescribeLoadBalancerPolicies action to verify that the policy has been
-- associated with the back-end server.
module Network.AWS.ELB.V2012_06_01.SetLoadBalancerPoliciesForBackendServer
    (
    -- * Request
      SetLoadBalancerPoliciesForBackendServer
    -- ** Request constructor
    , mkSetLoadBalancerPoliciesForBackendServer
    -- ** Request lenses
    , slbpfbsLoadBalancerName
    , slbpfbsInstancePort
    , slbpfbsPolicyNames

    -- * Response
    , SetLoadBalancerPoliciesForBackendServerResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.ELB.V2012_06_01.Types
import Network.AWS.Prelude

-- | The input for the SetLoadBalancerPoliciesForBackendServer action.
data SetLoadBalancerPoliciesForBackendServer = SetLoadBalancerPoliciesForBackendServer
    { _slbpfbsLoadBalancerName :: Text
    , _slbpfbsInstancePort :: Integer
    , _slbpfbsPolicyNames :: [Text]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'SetLoadBalancerPoliciesForBackendServer' request.
mkSetLoadBalancerPoliciesForBackendServer :: Text -- ^ 'slbpfbsLoadBalancerName'
                                          -> Integer -- ^ 'slbpfbsInstancePort'
                                          -> [Text] -- ^ 'slbpfbsPolicyNames'
                                          -> SetLoadBalancerPoliciesForBackendServer
mkSetLoadBalancerPoliciesForBackendServer p1 p2 p3 = SetLoadBalancerPoliciesForBackendServer
    { _slbpfbsLoadBalancerName = p1
    , _slbpfbsInstancePort = p2
    , _slbpfbsPolicyNames = p3
    }
{-# INLINE mkSetLoadBalancerPoliciesForBackendServer #-}

-- | The mnemonic name associated with the load balancer. This name must be
-- unique within the set of your load balancers.
slbpfbsLoadBalancerName :: Lens' SetLoadBalancerPoliciesForBackendServer Text
slbpfbsLoadBalancerName =
    lens _slbpfbsLoadBalancerName
         (\s a -> s { _slbpfbsLoadBalancerName = a })
{-# INLINE slbpfbsLoadBalancerName #-}

-- | The port number associated with the back-end server.
slbpfbsInstancePort :: Lens' SetLoadBalancerPoliciesForBackendServer Integer
slbpfbsInstancePort =
    lens _slbpfbsInstancePort (\s a -> s { _slbpfbsInstancePort = a })
{-# INLINE slbpfbsInstancePort #-}

-- | List of policy names to be set. If the list is empty, then all current
-- polices are removed from the back-end server.
slbpfbsPolicyNames :: Lens' SetLoadBalancerPoliciesForBackendServer [Text]
slbpfbsPolicyNames =
    lens _slbpfbsPolicyNames (\s a -> s { _slbpfbsPolicyNames = a })
{-# INLINE slbpfbsPolicyNames #-}

instance ToQuery SetLoadBalancerPoliciesForBackendServer where
    toQuery = genericQuery def

-- | The output for the SetLoadBalancerPoliciesForBackendServer action.
data SetLoadBalancerPoliciesForBackendServerResponse = SetLoadBalancerPoliciesForBackendServerResponse
    deriving (Eq, Show, Generic)

instance AWSRequest SetLoadBalancerPoliciesForBackendServer where
    type Sv SetLoadBalancerPoliciesForBackendServer = ELB
    type Rs SetLoadBalancerPoliciesForBackendServer = SetLoadBalancerPoliciesForBackendServerResponse

    request = post "SetLoadBalancerPoliciesForBackendServer"
    response _ = nullaryResponse SetLoadBalancerPoliciesForBackendServerResponse
