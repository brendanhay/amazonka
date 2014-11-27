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

-- Module      : Network.AWS.ELB.RemoveTags
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Removes one or more tags from the specified load balancer.
--
-- <http://docs.aws.amazon.com/ElasticLoadBalancing/latest/APIReference/API_RemoveTags.html>
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
import qualified GHC.Exts

data RemoveTags = RemoveTags
    { _rtLoadBalancerNames :: List "member" Text
    , _rtTags              :: List1 "member" TagKeyOnly
    } deriving (Eq, Show)

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

-- | The name of the load balancer. You can specify a maximum of one load balancer
-- name.
rtLoadBalancerNames :: Lens' RemoveTags [Text]
rtLoadBalancerNames =
    lens _rtLoadBalancerNames (\s a -> s { _rtLoadBalancerNames = a })
        . _List

-- | A list of tag keys to remove.
rtTags :: Lens' RemoveTags (NonEmpty TagKeyOnly)
rtTags = lens _rtTags (\s a -> s { _rtTags = a }) . _List1

data RemoveTagsResponse = RemoveTagsResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'RemoveTagsResponse' constructor.
removeTagsResponse :: RemoveTagsResponse
removeTagsResponse = RemoveTagsResponse

instance ToPath RemoveTags where
    toPath = const "/"

instance ToQuery RemoveTags where
    toQuery RemoveTags{..} = mconcat
        [ "LoadBalancerNames" =? _rtLoadBalancerNames
        , "Tags"              =? _rtTags
        ]

instance ToHeaders RemoveTags

instance AWSRequest RemoveTags where
    type Sv RemoveTags = ELB
    type Rs RemoveTags = RemoveTagsResponse

    request  = post "RemoveTags"
    response = nullResponse RemoveTagsResponse
