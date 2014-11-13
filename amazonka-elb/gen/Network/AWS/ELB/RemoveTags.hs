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

-- Module      : Network.AWS.ELB.RemoveTags
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Removes one or more tags from the specified load balancer.
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

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.ELB.Types

data RemoveTags = RemoveTags
    { _rtLoadBalancerNames :: [Text]
    , _rtTags              :: List1 TagKeyOnly
    } deriving (Eq, Show, Generic)

-- | 'RemoveTags' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rtLoadBalancerNames' @::@ ['Text']
--
-- * 'rtTags' @::@ 'NonEmpty' 'TagKeyOnly'
--
removeTags :: NonEmpty TagKeyOnly -- ^ 'rtTags'
           -> RemoveTags
removeTags p1 = RemoveTags
    { _rtTags              = withIso _List1 (const id) p1
    , _rtLoadBalancerNames = mempty
    }

-- | The name of the load balancer. You can specify a maximum of one load
-- balancer name.
rtLoadBalancerNames :: Lens' RemoveTags [Text]
rtLoadBalancerNames =
    lens _rtLoadBalancerNames (\s a -> s { _rtLoadBalancerNames = a })

-- | A list of tag keys to remove.
rtTags :: Lens' RemoveTags (NonEmpty TagKeyOnly)
rtTags = lens _rtTags (\s a -> s { _rtTags = a })
    . _List1

instance ToQuery RemoveTags

instance ToPath RemoveTags where
    toPath = const "/"

data RemoveTagsResponse = RemoveTagsResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'RemoveTagsResponse' constructor.
removeTagsResponse :: RemoveTagsResponse
removeTagsResponse = RemoveTagsResponse

instance FromXML RemoveTagsResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "RemoveTagsResponse"

instance AWSRequest RemoveTags where
    type Sv RemoveTags = ELB
    type Rs RemoveTags = RemoveTagsResponse

    request  = post "RemoveTags"
    response = nullaryResponse RemoveTagsResponse
