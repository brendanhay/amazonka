{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ELB.V2012_06_01.AddTags
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Adds one or more tags for the specified load balancer. Each load balancer
-- can have a maximum of 10 tags. Each tag consists of a key and an optional
-- value. Tag keys must be unique for each load balancer. If a tag with the
-- same key is already associated with the load balancer, this action will
-- update the value of the key. For more information, see Tagging in the
-- Elastic Load Balancing Developer Guide.
-- https://elasticloadbalancing.amazonaws.com/?LoadBalancerNames.member.1=my-test-loadbalancer
-- &Action=AddTags &Tags.member.1.Key=project
-- &Tags.member.1.Value=my-test-project &Version=2012-06-01 &AUTHPARAMS
-- 360e81f7-1100-11e4-b6ed-0f30EXAMPLE.
module Network.AWS.ELB.V2012_06_01.AddTags
    (
    -- * Request
      AddTags
    -- ** Request constructor
    , addTags
    -- ** Request lenses
    , atiLoadBalancerNames
    , atiTags

    -- * Response
    , AddTagsResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.ELB.V2012_06_01.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'AddTags' request.
addTags :: [Text] -- ^ 'atiLoadBalancerNames'
        -> [Tag] -- ^ 'atiTags'
        -> AddTags
addTags p1 p2 = AddTags
    { _atiLoadBalancerNames = p1
    , _atiTags = p2
    }

data AddTags = AddTags
    { _atiLoadBalancerNames :: [Text]
      -- ^ The name of the load balancer to tag. You can specify a maximum
      -- of one load balancer name.
    , _atiTags :: [Tag]
      -- ^ A list of tags for each load balancer.
    } deriving (Show, Generic)

-- | The name of the load balancer to tag. You can specify a maximum of one load
-- balancer name.
atiLoadBalancerNames
    :: Functor f
    => ([Text]
    -> f ([Text]))
    -> AddTags
    -> f AddTags
atiLoadBalancerNames f x =
    (\y -> x { _atiLoadBalancerNames = y })
       <$> f (_atiLoadBalancerNames x)
{-# INLINE atiLoadBalancerNames #-}

-- | A list of tags for each load balancer.
atiTags
    :: Functor f
    => ([Tag]
    -> f ([Tag]))
    -> AddTags
    -> f AddTags
atiTags f x =
    (\y -> x { _atiTags = y })
       <$> f (_atiTags x)
{-# INLINE atiTags #-}

instance ToQuery AddTags where
    toQuery = genericQuery def

data AddTagsResponse = AddTagsResponse
    deriving (Eq, Show, Generic)

instance AWSRequest AddTags where
    type Sv AddTags = ELB
    type Rs AddTags = AddTagsResponse

    request = post "AddTags"
    response _ = nullaryResponse AddTagsResponse
