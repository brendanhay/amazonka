{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ELB.V2012_06_01.RemoveTags
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Removes one or more tags from the specified load balancer. Remove Two Tag
-- Keys from the Load Balancer
-- https://elasticloadbalancing.amazonaws.com/?LoadBalancerName=my-test-loadbalancer
-- &Tags.member.1.Key=owner &Tags.member.2.Key=project &Action=RemoveTags
-- &Version=2012-06-01 &AUTHPARAMS 83c88b9d-12b7-11e3-8b82-87b12EXAMPLE.
module Network.AWS.ELB.V2012_06_01.RemoveTags
    (
    -- * Request
      RemoveTags
    -- ** Request constructor
    , mkRemoveTags
    -- ** Request lenses
    , rtLoadBalancerNames
    , rtTags

    -- * Response
    , RemoveTagsResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.ELB.V2012_06_01.Types
import Network.AWS.Prelude

-- | The input for the RemoveTags action.
data RemoveTags = RemoveTags
    { _rtLoadBalancerNames :: [Text]
    , _rtTags :: [TagKeyOnly]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'RemoveTags' request.
mkRemoveTags :: [Text] -- ^ 'rtLoadBalancerNames'
             -> [TagKeyOnly] -- ^ 'rtTags'
             -> RemoveTags
mkRemoveTags p1 p2 = RemoveTags
    { _rtLoadBalancerNames = p1
    , _rtTags = p2
    }
{-# INLINE mkRemoveTags #-}

-- | The name of the load balancer. You can specify a maximum of one load
-- balancer name.
rtLoadBalancerNames :: Lens' RemoveTags [Text]
rtLoadBalancerNames =
    lens _rtLoadBalancerNames (\s a -> s { _rtLoadBalancerNames = a })
{-# INLINE rtLoadBalancerNames #-}

-- | A list of tag keys to remove.
rtTags :: Lens' RemoveTags [TagKeyOnly]
rtTags = lens _rtTags (\s a -> s { _rtTags = a })
{-# INLINE rtTags #-}

instance ToQuery RemoveTags where
    toQuery = genericQuery def

-- | The output for the RemoveTags action.
data RemoveTagsResponse = RemoveTagsResponse
    deriving (Eq, Show, Generic)

instance AWSRequest RemoveTags where
    type Sv RemoveTags = ELB
    type Rs RemoveTags = RemoveTagsResponse

    request = post "RemoveTags"
    response _ = nullaryResponse RemoveTagsResponse
