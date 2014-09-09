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
    , mkAddTags
    -- ** Request lenses
    , atLoadBalancerNames
    , atTags

    -- * Response
    , AddTagsResponse
    -- ** Response constructor
    , mkAddTagsResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.ELB.V2012_06_01.Types
import Network.AWS.Prelude

-- | The input for the AddTags action.
data AddTags = AddTags
    { _atLoadBalancerNames :: [Text]
    , _atTags :: List1 Tag
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'AddTags' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @LoadBalancerNames ::@ @[Text]@
--
-- * @Tags ::@ @List1 Tag@
--
mkAddTags :: [Text] -- ^ 'atLoadBalancerNames'
          -> List1 Tag -- ^ 'atTags'
          -> AddTags
mkAddTags p1 p2 = AddTags
    { _atLoadBalancerNames = p1
    , _atTags = p2
    }

-- | The name of the load balancer to tag. You can specify a maximum of one load
-- balancer name.
atLoadBalancerNames :: Lens' AddTags [Text]
atLoadBalancerNames =
    lens _atLoadBalancerNames (\s a -> s { _atLoadBalancerNames = a })

-- | A list of tags for each load balancer.
atTags :: Lens' AddTags (List1 Tag)
atTags = lens _atTags (\s a -> s { _atTags = a })

instance ToQuery AddTags where
    toQuery = genericQuery def

-- | The output for the AddTags action.
data AddTagsResponse = AddTagsResponse
    deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'AddTagsResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
mkAddTagsResponse :: AddTagsResponse
mkAddTagsResponse = AddTagsResponse

instance AWSRequest AddTags where
    type Sv AddTags = ELB
    type Rs AddTags = AddTagsResponse

    request = post "AddTags"
    response _ = nullaryResponse AddTagsResponse
