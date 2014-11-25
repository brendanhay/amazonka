{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ELB.CreateLoadBalancerPolicy
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Creates a new policy that contains the necessary attributes depending on the
-- policy type. Policies are settings that are saved for your load balancer and
-- that can be applied to the front-end listener, or the back-end application
-- server, depending on your policy type.
--
-- <http://docs.aws.amazon.com/ElasticLoadBalancing/latest/APIReference/API_CreateLoadBalancerPolicy.html>
module Network.AWS.ELB.CreateLoadBalancerPolicy
    (
    -- * Request
      CreateLoadBalancerPolicy
    -- ** Request constructor
    , createLoadBalancerPolicy
    -- ** Request lenses
    , clbpLoadBalancerName
    , clbpPolicyAttributes
    , clbpPolicyName
    , clbpPolicyTypeName

    -- * Response
    , CreateLoadBalancerPolicyResponse
    -- ** Response constructor
    , createLoadBalancerPolicyResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.ELB.Types
import qualified GHC.Exts

data CreateLoadBalancerPolicy = CreateLoadBalancerPolicy
    { _clbpLoadBalancerName :: Text
    , _clbpPolicyAttributes :: List "PolicyAttributes" PolicyAttribute
    , _clbpPolicyName       :: Text
    , _clbpPolicyTypeName   :: Text
    } deriving (Eq, Show)

-- | 'CreateLoadBalancerPolicy' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'clbpLoadBalancerName' @::@ 'Text'
--
-- * 'clbpPolicyAttributes' @::@ ['PolicyAttribute']
--
-- * 'clbpPolicyName' @::@ 'Text'
--
-- * 'clbpPolicyTypeName' @::@ 'Text'
--
createLoadBalancerPolicy :: Text -- ^ 'clbpLoadBalancerName'
                         -> Text -- ^ 'clbpPolicyName'
                         -> Text -- ^ 'clbpPolicyTypeName'
                         -> CreateLoadBalancerPolicy
createLoadBalancerPolicy p1 p2 p3 = CreateLoadBalancerPolicy
    { _clbpLoadBalancerName = p1
    , _clbpPolicyName       = p2
    , _clbpPolicyTypeName   = p3
    , _clbpPolicyAttributes = mempty
    }

-- | The name associated with the LoadBalancer for which the policy is being
-- created.
--
clbpLoadBalancerName :: Lens' CreateLoadBalancerPolicy Text
clbpLoadBalancerName =
    lens _clbpLoadBalancerName (\s a -> s { _clbpLoadBalancerName = a })

-- | A list of attributes associated with the policy being created.
--
clbpPolicyAttributes :: Lens' CreateLoadBalancerPolicy [PolicyAttribute]
clbpPolicyAttributes =
    lens _clbpPolicyAttributes (\s a -> s { _clbpPolicyAttributes = a })
        . _List

-- | The name of the load balancer policy being created. The name must be unique
-- within the set of policies for this load balancer.
--
clbpPolicyName :: Lens' CreateLoadBalancerPolicy Text
clbpPolicyName = lens _clbpPolicyName (\s a -> s { _clbpPolicyName = a })

-- | The name of the base policy type being used to create this policy. To get
-- the list of policy types, use the 'DescribeLoadBalancerPolicyTypes' action.
--
clbpPolicyTypeName :: Lens' CreateLoadBalancerPolicy Text
clbpPolicyTypeName =
    lens _clbpPolicyTypeName (\s a -> s { _clbpPolicyTypeName = a })

data CreateLoadBalancerPolicyResponse = CreateLoadBalancerPolicyResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'CreateLoadBalancerPolicyResponse' constructor.
createLoadBalancerPolicyResponse :: CreateLoadBalancerPolicyResponse
createLoadBalancerPolicyResponse = CreateLoadBalancerPolicyResponse

instance ToPath CreateLoadBalancerPolicy where
    toPath = const "/"

instance ToQuery CreateLoadBalancerPolicy where
    toQuery CreateLoadBalancerPolicy{..} = mconcat
        [ "LoadBalancerName" =? _clbpLoadBalancerName
        , "PolicyAttributes" =? _clbpPolicyAttributes
        , "PolicyName"       =? _clbpPolicyName
        , "PolicyTypeName"   =? _clbpPolicyTypeName
        ]

instance ToHeaders CreateLoadBalancerPolicy

instance AWSRequest CreateLoadBalancerPolicy where
    type Sv CreateLoadBalancerPolicy = ELB
    type Rs CreateLoadBalancerPolicy = CreateLoadBalancerPolicyResponse

    request  = post "CreateLoadBalancerPolicy"
    response = nullResponse CreateLoadBalancerPolicyResponse
