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

-- Module      : Network.AWS.ELB.AddTags
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
module Network.AWS.ELB.AddTags
    (
    -- * Request
      AddTagsInput
    -- ** Request constructor
    , addTags
    -- ** Request lenses
    , atiLoadBalancerNames
    , atiTags

    -- * Response
    , AddTagsResponse
    -- ** Response constructor
    , addTagsResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.ELB.Types

data AddTagsInput = AddTagsInput
    { _atiLoadBalancerNames :: [Text]
    , _atiTags              :: List1 Tag
    } deriving (Eq, Show, Generic)

-- | 'AddTagsInput' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'atiLoadBalancerNames' @::@ ['Text']
--
-- * 'atiTags' @::@ 'NonEmpty' 'Tag'
--
addTags :: NonEmpty Tag -- ^ 'atiTags'
        -> AddTagsInput
addTags p1 = AddTagsInput
    { _atiTags              = withIso _List1 (const id) p1
    , _atiLoadBalancerNames = mempty
    }

-- | The name of the load balancer to tag. You can specify a maximum of one
-- load balancer name.
atiLoadBalancerNames :: Lens' AddTagsInput [Text]
atiLoadBalancerNames =
    lens _atiLoadBalancerNames (\s a -> s { _atiLoadBalancerNames = a })

-- | A list of tags for each load balancer.
atiTags :: Lens' AddTagsInput (NonEmpty Tag)
atiTags = lens _atiTags (\s a -> s { _atiTags = a })
    . _List1

instance ToQuery AddTagsInput

instance ToPath AddTagsInput where
    toPath = const "/"

data AddTagsResponse = AddTagsResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'AddTagsResponse' constructor.
addTagsResponse :: AddTagsResponse
addTagsResponse = AddTagsResponse

instance FromXML AddTagsResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "AddTagsResponse"

instance AWSRequest AddTagsInput where
    type Sv AddTagsInput = ELB
    type Rs AddTagsInput = AddTagsResponse

    request  = post "AddTags"
    response = nullaryResponse AddTagsResponse
