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
    , mkCreateLoadBalancerPolicyInput
    -- ** Request lenses
    , clbpiLoadBalancerName
    , clbpiPolicyName
    , clbpiPolicyTypeName
    , clbpiPolicyAttributes

    -- * Response
    , CreateLoadBalancerPolicyResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.ELB.V2012_06_01.Types
import Network.AWS.Prelude

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CreateLoadBalancerPolicy' request.
mkCreateLoadBalancerPolicyInput :: Text -- ^ 'clbpiLoadBalancerName'
                                -> Text -- ^ 'clbpiPolicyName'
                                -> Text -- ^ 'clbpiPolicyTypeName'
                                -> CreateLoadBalancerPolicy
mkCreateLoadBalancerPolicyInput p1 p2 p3 = CreateLoadBalancerPolicy
    { _clbpiLoadBalancerName = p1
    , _clbpiPolicyName = p2
    , _clbpiPolicyTypeName = p3
    , _clbpiPolicyAttributes = mempty
    }
{-# INLINE mkCreateLoadBalancerPolicyInput #-}

data CreateLoadBalancerPolicy = CreateLoadBalancerPolicy
    { _clbpiLoadBalancerName :: Text
      -- ^ The name associated with the LoadBalancer for which the policy is
      -- being created.
    , _clbpiPolicyName :: Text
      -- ^ The name of the load balancer policy being created. The name must
      -- be unique within the set of policies for this load balancer.
    , _clbpiPolicyTypeName :: Text
      -- ^ The name of the base policy type being used to create this
      -- policy. To get the list of policy types, use the
      -- DescribeLoadBalancerPolicyTypes action.
    , _clbpiPolicyAttributes :: [PolicyAttribute]
      -- ^ A list of attributes associated with the policy being created.
    } deriving (Show, Generic)

-- | The name associated with the LoadBalancer for which the policy is being
-- created.
clbpiLoadBalancerName :: Lens' CreateLoadBalancerPolicy (Text)
clbpiLoadBalancerName = lens _clbpiLoadBalancerName (\s a -> s { _clbpiLoadBalancerName = a })
{-# INLINE clbpiLoadBalancerName #-}

-- | The name of the load balancer policy being created. The name must be unique
-- within the set of policies for this load balancer.
clbpiPolicyName :: Lens' CreateLoadBalancerPolicy (Text)
clbpiPolicyName = lens _clbpiPolicyName (\s a -> s { _clbpiPolicyName = a })
{-# INLINE clbpiPolicyName #-}

-- | The name of the base policy type being used to create this policy. To get
-- the list of policy types, use the DescribeLoadBalancerPolicyTypes action.
clbpiPolicyTypeName :: Lens' CreateLoadBalancerPolicy (Text)
clbpiPolicyTypeName = lens _clbpiPolicyTypeName (\s a -> s { _clbpiPolicyTypeName = a })
{-# INLINE clbpiPolicyTypeName #-}

-- | A list of attributes associated with the policy being created.
clbpiPolicyAttributes :: Lens' CreateLoadBalancerPolicy ([PolicyAttribute])
clbpiPolicyAttributes = lens _clbpiPolicyAttributes (\s a -> s { _clbpiPolicyAttributes = a })
{-# INLINE clbpiPolicyAttributes #-}

instance ToQuery CreateLoadBalancerPolicy where
    toQuery = genericQuery def

    deriving (Eq, Show, Generic)

instance AWSRequest CreateLoadBalancerPolicy where
    type Sv CreateLoadBalancerPolicy = ELB
    type Rs CreateLoadBalancerPolicy = CreateLoadBalancerPolicyResponse

    request = post "CreateLoadBalancerPolicy"
    response _ = nullaryResponse CreateLoadBalancerPolicyResponse
