{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

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
module Network.AWS.ELB.V2012_06_01.CreateLoadBalancerPolicy where

import Control.Lens.TH (makeLenses)
import Network.AWS.Request.Query
import Network.AWS.ELB.V2012_06_01.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'CreateLoadBalancerPolicy' request.
createLoadBalancerPolicy :: Text -- ^ '_clbpiLoadBalancerName'
                         -> Text -- ^ '_clbpiPolicyName'
                         -> Text -- ^ '_clbpiPolicyTypeName'
                         -> CreateLoadBalancerPolicy
createLoadBalancerPolicy p1 p2 p3 = CreateLoadBalancerPolicy
    { _clbpiLoadBalancerName = p1
    , _clbpiPolicyName = p2
    , _clbpiPolicyTypeName = p3
    , _clbpiPolicyAttributes = mempty
    }

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

makeLenses ''CreateLoadBalancerPolicy

instance ToQuery CreateLoadBalancerPolicy where
    toQuery = genericToQuery def

data CreateLoadBalancerPolicyResponse = CreateLoadBalancerPolicyResponse
    deriving (Eq, Show, Generic)

makeLenses ''CreateLoadBalancerPolicyResponse

instance AWSRequest CreateLoadBalancerPolicy where
    type Sv CreateLoadBalancerPolicy = ELB
    type Rs CreateLoadBalancerPolicy = CreateLoadBalancerPolicyResponse

    request = post "CreateLoadBalancerPolicy"
    response _ _ = return (Right CreateLoadBalancerPolicyResponse)
