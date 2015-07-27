{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.AddTagsToResource
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Adds metadata tags to an Amazon RDS resource. These tags can also be
-- used with cost allocation reporting to track cost associated with Amazon
-- RDS resources, or used in Condition statement in IAM policy for Amazon
-- RDS.
--
-- For an overview on tagging Amazon RDS resources, see
-- <http://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Overview.Tagging.html Tagging Amazon RDS Resources>.
--
-- <http://docs.aws.amazon.com/AmazonRDS/latest/APIReference/API_AddTagsToResource.html>
module Network.AWS.RDS.AddTagsToResource
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
    ) where

import           Network.AWS.Prelude
import           Network.AWS.RDS.Types
import           Network.AWS.Request
import           Network.AWS.Response

-- |
--
-- /See:/ 'addTagsToResource' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'attrResourceName'
--
-- * 'attrTags'
data AddTagsToResource = AddTagsToResource'
    { _attrResourceName :: !Text
    , _attrTags         :: ![Tag]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'AddTagsToResource' smart constructor.
addTagsToResource :: Text -> AddTagsToResource
addTagsToResource pResourceName_ =
    AddTagsToResource'
    { _attrResourceName = pResourceName_
    , _attrTags = mempty
    }

-- | The Amazon RDS resource the tags will be added to. This value is an
-- Amazon Resource Name (ARN). For information about creating an ARN, see
-- <http://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_Tagging.html#USER_Tagging.ARN Constructing an RDS Amazon Resource Name (ARN)>.
attrResourceName :: Lens' AddTagsToResource Text
attrResourceName = lens _attrResourceName (\ s a -> s{_attrResourceName = a});

-- | The tags to be assigned to the Amazon RDS resource.
attrTags :: Lens' AddTagsToResource [Tag]
attrTags = lens _attrTags (\ s a -> s{_attrTags = a}) . _Coerce;

instance AWSRequest AddTagsToResource where
        type Sv AddTagsToResource = RDS
        type Rs AddTagsToResource = AddTagsToResourceResponse
        request = postQuery
        response = receiveNull AddTagsToResourceResponse'

instance ToHeaders AddTagsToResource where
        toHeaders = const mempty

instance ToPath AddTagsToResource where
        toPath = const "/"

instance ToQuery AddTagsToResource where
        toQuery AddTagsToResource'{..}
          = mconcat
              ["Action" =: ("AddTagsToResource" :: ByteString),
               "Version" =: ("2014-10-31" :: ByteString),
               "ResourceName" =: _attrResourceName,
               "Tags" =: toQueryList "Tag" _attrTags]

-- | /See:/ 'addTagsToResourceResponse' smart constructor.
data AddTagsToResourceResponse =
    AddTagsToResourceResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'AddTagsToResourceResponse' smart constructor.
addTagsToResourceResponse :: AddTagsToResourceResponse
addTagsToResourceResponse = AddTagsToResourceResponse'
