{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.AddTagsToResource
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds up to 50 cost allocation tags to the named resource. A cost allocation tag is a key-value pair where the key and value are case-sensitive. You can use cost allocation tags to categorize and track your AWS costs.
--
--
-- When you apply tags to your ElastiCache resources, AWS generates a cost allocation report as a comma-separated value (CSV) file with your usage and costs aggregated by your tags. You can apply tags that represent business categories (such as cost centers, application names, or owners) to organize your costs across multiple services. For more information, see <http://docs.aws.amazon.com/AmazonElastiCache/latest/UserGuide/Tagging.html Using Cost Allocation Tags in Amazon ElastiCache> in the /ElastiCache User Guide/ .
--
module Network.AWS.ElastiCache.AddTagsToResource
    (
    -- * Creating a Request
      addTagsToResource
    , AddTagsToResource
    -- * Request Lenses
    , attrResourceName
    , attrTags

    -- * Destructuring the Response
    , tagListMessage
    , TagListMessage
    -- * Response Lenses
    , tlmTagList
    ) where

import Network.AWS.ElastiCache.Types
import Network.AWS.ElastiCache.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents the input of an AddTagsToResource operation.
--
--
--
-- /See:/ 'addTagsToResource' smart constructor.
data AddTagsToResource = AddTagsToResource'
  { _attrResourceName :: !Text
  , _attrTags         :: ![Tag]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AddTagsToResource' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'attrResourceName' - The Amazon Resource Name (ARN) of the resource to which the tags are to be added, for example @arn:aws:elasticache:us-west-2:0123456789:cluster:myCluster@ or @arn:aws:elasticache:us-west-2:0123456789:snapshot:mySnapshot@ . ElastiCache resources are /cluster/ and /snapshot/ . For more information about ARNs, see <http://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
--
-- * 'attrTags' - A list of cost allocation tags to be added to this resource. A tag is a key-value pair. A tag key must be accompanied by a tag value.
addTagsToResource
    :: Text -- ^ 'attrResourceName'
    -> AddTagsToResource
addTagsToResource pResourceName_ =
  AddTagsToResource' {_attrResourceName = pResourceName_, _attrTags = mempty}


-- | The Amazon Resource Name (ARN) of the resource to which the tags are to be added, for example @arn:aws:elasticache:us-west-2:0123456789:cluster:myCluster@ or @arn:aws:elasticache:us-west-2:0123456789:snapshot:mySnapshot@ . ElastiCache resources are /cluster/ and /snapshot/ . For more information about ARNs, see <http://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
attrResourceName :: Lens' AddTagsToResource Text
attrResourceName = lens _attrResourceName (\ s a -> s{_attrResourceName = a})

-- | A list of cost allocation tags to be added to this resource. A tag is a key-value pair. A tag key must be accompanied by a tag value.
attrTags :: Lens' AddTagsToResource [Tag]
attrTags = lens _attrTags (\ s a -> s{_attrTags = a}) . _Coerce

instance AWSRequest AddTagsToResource where
        type Rs AddTagsToResource = TagListMessage
        request = postQuery elastiCache
        response
          = receiveXMLWrapper "AddTagsToResourceResult"
              (\ s h x -> parseXML x)

instance Hashable AddTagsToResource where

instance NFData AddTagsToResource where

instance ToHeaders AddTagsToResource where
        toHeaders = const mempty

instance ToPath AddTagsToResource where
        toPath = const "/"

instance ToQuery AddTagsToResource where
        toQuery AddTagsToResource'{..}
          = mconcat
              ["Action" =: ("AddTagsToResource" :: ByteString),
               "Version" =: ("2015-02-02" :: ByteString),
               "ResourceName" =: _attrResourceName,
               "Tags" =: toQueryList "Tag" _attrTags]
