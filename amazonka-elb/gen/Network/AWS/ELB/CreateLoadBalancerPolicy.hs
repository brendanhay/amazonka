{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-unused-binds  #-} doesnt work if wall is used
{-# OPTIONS_GHC -w #-}

-- Module      : Network.AWS.ELB.CreateLoadBalancerPolicy
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
module Network.AWS.ELB.CreateLoadBalancerPolicy
    (
    -- * Request
      CreateLoadBalancerPolicyInput
    -- ** Request constructor
    , createLoadBalancerPolicy
    -- ** Request lenses
    , clbpiLoadBalancerName
    , clbpiPolicyAttributes
    , clbpiPolicyName
    , clbpiPolicyTypeName

    -- * Response
    , CreateLoadBalancerPolicyResponse
    -- ** Response constructor
    , createLoadBalancerPolicyResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.ELB.Types

data CreateLoadBalancerPolicyInput = CreateLoadBalancerPolicyInput
    { _clbpiLoadBalancerName :: Text
    , _clbpiPolicyAttributes :: [PolicyAttribute]
    , _clbpiPolicyName       :: Text
    , _clbpiPolicyTypeName   :: Text
    } deriving (Eq, Show, Generic)

-- | 'CreateLoadBalancerPolicyInput' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'clbpiLoadBalancerName' @::@ 'Text'
--
-- * 'clbpiPolicyAttributes' @::@ ['PolicyAttribute']
--
-- * 'clbpiPolicyName' @::@ 'Text'
--
-- * 'clbpiPolicyTypeName' @::@ 'Text'
--
createLoadBalancerPolicy :: Text -- ^ 'clbpiLoadBalancerName'
                         -> Text -- ^ 'clbpiPolicyName'
                         -> Text -- ^ 'clbpiPolicyTypeName'
                         -> CreateLoadBalancerPolicyInput
createLoadBalancerPolicy p1 p2 p3 = CreateLoadBalancerPolicyInput
    { _clbpiLoadBalancerName = p1
    , _clbpiPolicyName       = p2
    , _clbpiPolicyTypeName   = p3
    , _clbpiPolicyAttributes = mempty
    }

-- | The name associated with the LoadBalancer for which the policy is being
-- created.
clbpiLoadBalancerName :: Lens' CreateLoadBalancerPolicyInput Text
clbpiLoadBalancerName =
    lens _clbpiLoadBalancerName (\s a -> s { _clbpiLoadBalancerName = a })

-- | A list of attributes associated with the policy being created.
clbpiPolicyAttributes :: Lens' CreateLoadBalancerPolicyInput [PolicyAttribute]
clbpiPolicyAttributes =
    lens _clbpiPolicyAttributes (\s a -> s { _clbpiPolicyAttributes = a })

-- | The name of the load balancer policy being created. The name must be
-- unique within the set of policies for this load balancer.
clbpiPolicyName :: Lens' CreateLoadBalancerPolicyInput Text
clbpiPolicyName = lens _clbpiPolicyName (\s a -> s { _clbpiPolicyName = a })

-- | The name of the base policy type being used to create this policy. To get
-- the list of policy types, use the DescribeLoadBalancerPolicyTypes action.
clbpiPolicyTypeName :: Lens' CreateLoadBalancerPolicyInput Text
clbpiPolicyTypeName =
    lens _clbpiPolicyTypeName (\s a -> s { _clbpiPolicyTypeName = a })

instance ToPath CreateLoadBalancerPolicyInput where
    toPath = const "/"

instance ToQuery CreateLoadBalancerPolicyInput

data CreateLoadBalancerPolicyResponse = CreateLoadBalancerPolicyResponse

-- | 'CreateLoadBalancerPolicyResponse' constructor.
createLoadBalancerPolicyResponse :: CreateLoadBalancerPolicyResponse
createLoadBalancerPolicyResponse = CreateLoadBalancerPolicyResponse

instance AWSRequest CreateLoadBalancerPolicyInput where
    type Sv CreateLoadBalancerPolicyInput = ELB
    type Rs CreateLoadBalancerPolicyInput = CreateLoadBalancerPolicyResponse

    request  = post "CreateLoadBalancerPolicy"
    response = const (nullaryResponse CreateLoadBalancerPolicyResponse)
