{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE StandaloneDeriving          #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ELB.RemoveTags
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
module Network.AWS.ELB.RemoveTags
    (
    -- * Request
      RemoveTags
    -- ** Request constructor
    , removeTags
    -- ** Request lenses
    , rtLoadBalancerNames
    , rtTags

    -- * Response
    , RemoveTagsResponse
    -- ** Response constructor
    , removeTagsResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.ELB.Types
import Network.AWS.Prelude

-- | The input for the RemoveTags action.
data RemoveTags = RemoveTags
    { _rtLoadBalancerNames :: [Text]
    , _rtTags :: List1 TagKeyOnly
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'RemoveTags' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @LoadBalancerNames ::@ @[Text]@
--
-- * @Tags ::@ @List1 TagKeyOnly@
--
removeTags :: [Text] -- ^ 'rtLoadBalancerNames'
           -> List1 TagKeyOnly -- ^ 'rtTags'
           -> RemoveTags
removeTags p1 p2 = RemoveTags
    { _rtLoadBalancerNames = p1
    , _rtTags = p2
    }

-- | The name of the load balancer. You can specify a maximum of one load
-- balancer name.
rtLoadBalancerNames :: Lens' RemoveTags [Text]
rtLoadBalancerNames =
    lens _rtLoadBalancerNames (\s a -> s { _rtLoadBalancerNames = a })

-- | A list of tag keys to remove.
rtTags :: Lens' RemoveTags (List1 TagKeyOnly)
rtTags = lens _rtTags (\s a -> s { _rtTags = a })

instance ToQuery RemoveTags where
    toQuery = genericQuery def

-- | The output for the RemoveTags action.
data RemoveTagsResponse = RemoveTagsResponse
    deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'RemoveTagsResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
removeTagsResponse :: RemoveTagsResponse
removeTagsResponse = RemoveTagsResponse

instance AWSRequest RemoveTags where
    type Sv RemoveTags = ELB
    type Rs RemoveTags = RemoveTagsResponse

    request = post "RemoveTags"
    response _ = nullaryResponse RemoveTagsResponse
