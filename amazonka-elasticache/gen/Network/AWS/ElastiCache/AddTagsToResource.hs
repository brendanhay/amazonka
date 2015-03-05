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

-- Module      : Network.AWS.ElastiCache.AddTagsToResource
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

-- | The /AddTagsToResource/ action adds up to 10 cost allocation tags to the named
-- resource. A /cost allocation tag/ is a key-value pair where the key and value
-- are case-sensitive. Cost allocation tags can be used to categorize and track
-- your AWS costs.
--
-- When you apply tags to your ElastiCache resources, AWS generates a cost
-- allocation report as a comma-separated value (CSV) file with your usage and
-- costs aggregated by your tags. You can apply tags that represent business
-- categories (such as cost centers, application names, or owners) to organize
-- your costs across multiple services. For more information, see <http://docs.aws.amazon.com/AmazonElastiCache/latest/UserGuide/Tagging.html Using CostAllocation Tags in Amazon ElastiCache>.
--
-- <http://docs.aws.amazon.com/AmazonElastiCache/latest/APIReference/API_AddTagsToResource.html>
module Network.AWS.ElastiCache.AddTagsToResource
    (
    -- * Request
      AddTagsToResource
    -- ** Request constructor
    , addTagsToResource
    -- ** Request lenses
    , attrResourceName
    , attrTags

    -- * Response
    , AddTagsToResourceResponse
    -- ** Response constructor
    , addTagsToResourceResponse
    -- ** Response lenses
    , attrrTagList
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.ElastiCache.Types
import qualified GHC.Exts

data AddTagsToResource = AddTagsToResource
    { _attrResourceName :: Text
    , _attrTags         :: List "member" Tag
    } deriving (Eq, Read, Show)

-- | 'AddTagsToResource' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'attrResourceName' @::@ 'Text'
--
-- * 'attrTags' @::@ ['Tag']
--
addTagsToResource :: Text -- ^ 'attrResourceName'
                  -> AddTagsToResource
addTagsToResource p1 = AddTagsToResource
    { _attrResourceName = p1
    , _attrTags         = mempty
    }

-- | The name of the resource to which the tags are to be added, for example 'arn:aws:elasticache:us-west-2:0123456789:cluster:myCluster'.
attrResourceName :: Lens' AddTagsToResource Text
attrResourceName = lens _attrResourceName (\s a -> s { _attrResourceName = a })

-- | A list of cost allocation tags to be added to this resource. A tag is a
-- key-value pair. A tag key must be accompanied by a tag value.
attrTags :: Lens' AddTagsToResource [Tag]
attrTags = lens _attrTags (\s a -> s { _attrTags = a }) . _List

newtype AddTagsToResourceResponse = AddTagsToResourceResponse
    { _attrrTagList :: List "member" Tag
    } deriving (Eq, Read, Show, Monoid, Semigroup)

instance GHC.Exts.IsList AddTagsToResourceResponse where
    type Item AddTagsToResourceResponse = Tag

    fromList = AddTagsToResourceResponse . GHC.Exts.fromList
    toList   = GHC.Exts.toList . _attrrTagList

-- | 'AddTagsToResourceResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'attrrTagList' @::@ ['Tag']
--
addTagsToResourceResponse :: AddTagsToResourceResponse
addTagsToResourceResponse = AddTagsToResourceResponse
    { _attrrTagList = mempty
    }

-- | A list of cost allocation tags as key-value pairs.
attrrTagList :: Lens' AddTagsToResourceResponse [Tag]
attrrTagList = lens _attrrTagList (\s a -> s { _attrrTagList = a }) . _List

instance ToPath AddTagsToResource where
    toPath = const "/"

instance ToQuery AddTagsToResource where
    toQuery AddTagsToResource{..} = mconcat
        [ "ResourceName" =? _attrResourceName
        , "Tags"         =? _attrTags
        ]

instance ToHeaders AddTagsToResource

instance AWSRequest AddTagsToResource where
    type Sv AddTagsToResource = ElastiCache
    type Rs AddTagsToResource = AddTagsToResourceResponse

    request  = post "AddTagsToResource"
    response = xmlResponse

instance FromXML AddTagsToResourceResponse where
    parseXML = withElement "AddTagsToResourceResult" $ \x -> AddTagsToResourceResponse
        <$> x .@? "TagList" .!@ mempty
