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

-- Module      : Network.AWS.ELB.AddTags
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

-- | Adds the specified tags to the specified load balancer. Each load balancer
-- can have a maximum of 10 tags.
--
-- Each tag consists of a key and an optional value. If a tag with the same key
-- is already associated with the load balancer, 'AddTags' updates its value.
--
-- For more information, see <http://docs.aws.amazon.com/ElasticLoadBalancing/latest/DeveloperGuide/TerminologyandKeyConcepts.html#tagging-elb Tagging> in the /Elastic Load Balancing DeveloperGuide/.
--
-- <http://docs.aws.amazon.com/ElasticLoadBalancing/latest/APIReference/API_AddTags.html>
module Network.AWS.ELB.AddTags
    (
    -- * Request
      AddTags
    -- ** Request constructor
    , addTags
    -- ** Request lenses
    , atLoadBalancerNames
    , atTags

    -- * Response
    , AddTagsResponse
    -- ** Response constructor
    , addTagsResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.ELB.Types
import qualified GHC.Exts

data AddTags = AddTags
    { _atLoadBalancerNames :: List "member" Text
    , _atTags              :: List1 "member" Tag
    } deriving (Eq, Read, Show)

-- | 'AddTags' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'atLoadBalancerNames' @::@ ['Text']
--
-- * 'atTags' @::@ 'NonEmpty' 'Tag'
--
addTags :: NonEmpty Tag -- ^ 'atTags'
        -> AddTags
addTags p1 = AddTags
    { _atTags              = withIso _List1 (const id) p1
    , _atLoadBalancerNames = mempty
    }

-- | The name of the load balancer. You can specify one load balancer only.
atLoadBalancerNames :: Lens' AddTags [Text]
atLoadBalancerNames =
    lens _atLoadBalancerNames (\s a -> s { _atLoadBalancerNames = a })
        . _List

-- | The tags.
atTags :: Lens' AddTags (NonEmpty Tag)
atTags = lens _atTags (\s a -> s { _atTags = a }) . _List1

data AddTagsResponse = AddTagsResponse
    deriving (Eq, Ord, Read, Show, Generic)

-- | 'AddTagsResponse' constructor.
addTagsResponse :: AddTagsResponse
addTagsResponse = AddTagsResponse

instance ToPath AddTags where
    toPath = const "/"

instance ToQuery AddTags where
    toQuery AddTags{..} = mconcat
        [ "LoadBalancerNames" =? _atLoadBalancerNames
        , "Tags"              =? _atTags
        ]

instance ToHeaders AddTags

instance AWSRequest AddTags where
    type Sv AddTags = ELB
    type Rs AddTags = AddTagsResponse

    request  = post "AddTags"
    response = nullResponse AddTagsResponse
