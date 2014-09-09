{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ELB.V2012_06_01.CreateLoadBalancerPolicy
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Creates a new policy that contains the necessary attributes depending on
-- the policy type. Policies are settings that are saved for your load
-- balancer and that can be applied to the front-end listener, or the back-end
-- application server, depending on your policy type.
-- https://elasticloadbalancing.amazonaws.com/?PolicyAttributes.member.1.AttributeName=ProxyProtocol
-- &PolicyAttributes.member.1.AttributeValue=true
-- &PolicyTypeName=ProxyProtocolPolicyType
-- &LoadBalancerName=my-test-loadbalancer &PolicyName=EnableProxyProtocol
-- &Version=2012-06-01 &Action=CreateLoadBalancerPolicy &AUTHPARAMS
-- 83c88b9d-12b7-11e3-8b82-87b12EXAMPLE.
module Network.AWS.ELB.V2012_06_01.CreateLoadBalancerPolicy
    (
    -- * Request
      CreateLoadBalancerPolicy
    -- ** Request constructor
    , mkCreateLoadBalancerPolicy
    -- ** Request lenses
    , clbpLoadBalancerName
    , clbpPolicyName
    , clbpPolicyTypeName
    , clbpPolicyAttributes

    -- * Response
    , CreateLoadBalancerPolicyResponse
    -- ** Response constructor
    , mkCreateLoadBalancerPolicyResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.ELB.V2012_06_01.Types
import Network.AWS.Prelude

data CreateLoadBalancerPolicy = CreateLoadBalancerPolicy
    { _clbpLoadBalancerName :: Text
    , _clbpPolicyName :: Text
    , _clbpPolicyTypeName :: Text
    , _clbpPolicyAttributes :: [PolicyAttribute]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CreateLoadBalancerPolicy' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @LoadBalancerName ::@ @Text@
--
-- * @PolicyName ::@ @Text@
--
-- * @PolicyTypeName ::@ @Text@
--
-- * @PolicyAttributes ::@ @[PolicyAttribute]@
--
mkCreateLoadBalancerPolicy :: Text -- ^ 'clbpLoadBalancerName'
                           -> Text -- ^ 'clbpPolicyName'
                           -> Text -- ^ 'clbpPolicyTypeName'
                           -> CreateLoadBalancerPolicy
mkCreateLoadBalancerPolicy p1 p2 p3 = CreateLoadBalancerPolicy
    { _clbpLoadBalancerName = p1
    , _clbpPolicyName = p2
    , _clbpPolicyTypeName = p3
    , _clbpPolicyAttributes = mempty
    }

-- | The name associated with the LoadBalancer for which the policy is being
-- created.
clbpLoadBalancerName :: Lens' CreateLoadBalancerPolicy Text
clbpLoadBalancerName =
    lens _clbpLoadBalancerName (\s a -> s { _clbpLoadBalancerName = a })

-- | The name of the load balancer policy being created. The name must be unique
-- within the set of policies for this load balancer.
clbpPolicyName :: Lens' CreateLoadBalancerPolicy Text
clbpPolicyName = lens _clbpPolicyName (\s a -> s { _clbpPolicyName = a })

-- | The name of the base policy type being used to create this policy. To get
-- the list of policy types, use the DescribeLoadBalancerPolicyTypes action.
clbpPolicyTypeName :: Lens' CreateLoadBalancerPolicy Text
clbpPolicyTypeName =
    lens _clbpPolicyTypeName (\s a -> s { _clbpPolicyTypeName = a })

-- | A list of attributes associated with the policy being created.
clbpPolicyAttributes :: Lens' CreateLoadBalancerPolicy [PolicyAttribute]
clbpPolicyAttributes =
    lens _clbpPolicyAttributes (\s a -> s { _clbpPolicyAttributes = a })

instance ToQuery CreateLoadBalancerPolicy where
    toQuery = genericQuery def

-- | The output for the CreateLoadBalancerPolicy action.
data CreateLoadBalancerPolicyResponse = CreateLoadBalancerPolicyResponse
    deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CreateLoadBalancerPolicyResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
mkCreateLoadBalancerPolicyResponse :: CreateLoadBalancerPolicyResponse
mkCreateLoadBalancerPolicyResponse = CreateLoadBalancerPolicyResponse

instance AWSRequest CreateLoadBalancerPolicy where
    type Sv CreateLoadBalancerPolicy = ELB
    type Rs CreateLoadBalancerPolicy = CreateLoadBalancerPolicyResponse

    request = post "CreateLoadBalancerPolicy"
    response _ = nullaryResponse CreateLoadBalancerPolicyResponse
