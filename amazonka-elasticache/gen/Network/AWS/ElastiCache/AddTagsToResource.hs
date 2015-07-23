{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.AddTagsToResource
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- The /AddTagsToResource/ action adds up to 10 cost allocation tags to the
-- named resource. A /cost allocation tag/ is a key-value pair where the
-- key and value are case-sensitive. Cost allocation tags can be used to
-- categorize and track your AWS costs.
--
-- When you apply tags to your ElastiCache resources, AWS generates a cost
-- allocation report as a comma-separated value (CSV) file with your usage
-- and costs aggregated by your tags. You can apply tags that represent
-- business categories (such as cost centers, application names, or owners)
-- to organize your costs across multiple services. For more information,
-- see
-- <http://docs.aws.amazon.com/AmazonElastiCache/latest/UserGuide/Tagging.html Using Cost Allocation Tags in Amazon ElastiCache>.
--
-- <http://docs.aws.amazon.com/AmazonElastiCache/latest/APIReference/API_AddTagsToResource.html>
module Network.AWS.ElastiCache.AddTagsToResource
    (
    -- * Request
      AddTagsToResource
    -- ** Request constructor
    , addTagsToResource
    -- ** Request lenses
    , attrrqResourceName
    , attrrqTags

    -- * Response
    , TagListMessage
    -- ** Response constructor
    , tagListMessage
    -- ** Response lenses
    , tlmTagList
    ) where

import           Network.AWS.ElastiCache.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Represents the input of an /AddTagsToResource/ action.
--
-- /See:/ 'addTagsToResource' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'attrrqResourceName'
--
-- * 'attrrqTags'
data AddTagsToResource = AddTagsToResource'
    { _attrrqResourceName :: !Text
    , _attrrqTags         :: ![Tag]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'AddTagsToResource' smart constructor.
addTagsToResource :: Text -> AddTagsToResource
addTagsToResource pResourceName_ =
    AddTagsToResource'
    { _attrrqResourceName = pResourceName_
    , _attrrqTags = mempty
    }

-- | The name of the resource to which the tags are to be added, for example
-- @arn:aws:elasticache:us-west-2:0123456789:cluster:myCluster@.
attrrqResourceName :: Lens' AddTagsToResource Text
attrrqResourceName = lens _attrrqResourceName (\ s a -> s{_attrrqResourceName = a});

-- | A list of cost allocation tags to be added to this resource. A tag is a
-- key-value pair. A tag key must be accompanied by a tag value.
attrrqTags :: Lens' AddTagsToResource [Tag]
attrrqTags = lens _attrrqTags (\ s a -> s{_attrrqTags = a});

instance AWSRequest AddTagsToResource where
        type Sv AddTagsToResource = ElastiCache
        type Rs AddTagsToResource = TagListMessage
        request = post
        response
          = receiveXMLWrapper "AddTagsToResourceResult"
              (\ s h x -> parseXML x)

instance ToHeaders AddTagsToResource where
        toHeaders = const mempty

instance ToPath AddTagsToResource where
        toPath = const "/"

instance ToQuery AddTagsToResource where
        toQuery AddTagsToResource'{..}
          = mconcat
              ["Action" =: ("AddTagsToResource" :: ByteString),
               "Version" =: ("2015-02-02" :: ByteString),
               "ResourceName" =: _attrrqResourceName,
               "Tags" =: toQueryList "Tag" _attrrqTags]
