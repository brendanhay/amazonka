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
-- Module      : Network.AWS.RDS.AddTagsToResource
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds metadata tags to an Amazon RDS resource. These tags can also be used with cost allocation reporting to track cost associated with Amazon RDS resources, or used in a Condition statement in an IAM policy for Amazon RDS.
--
--
-- For an overview on tagging Amazon RDS resources, see <http://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Overview.Tagging.html Tagging Amazon RDS Resources> .
--
module Network.AWS.RDS.AddTagsToResource
    (
    -- * Creating a Request
      addTagsToResource
    , AddTagsToResource
    -- * Request Lenses
    , attrResourceName
    , attrTags

    -- * Destructuring the Response
    , addTagsToResourceResponse
    , AddTagsToResourceResponse
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.RDS.Types
import Network.AWS.RDS.Types.Product
import Network.AWS.Request
import Network.AWS.Response

-- |
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
-- * 'attrResourceName' - The Amazon RDS resource that the tags are added to. This value is an Amazon Resource Name (ARN). For information about creating an ARN, see <http://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_Tagging.ARN.html#USER_Tagging.ARN.Constructing Constructing an RDS Amazon Resource Name (ARN)> .
--
-- * 'attrTags' - The tags to be assigned to the Amazon RDS resource.
addTagsToResource
    :: Text -- ^ 'attrResourceName'
    -> AddTagsToResource
addTagsToResource pResourceName_ =
  AddTagsToResource' {_attrResourceName = pResourceName_, _attrTags = mempty}


-- | The Amazon RDS resource that the tags are added to. This value is an Amazon Resource Name (ARN). For information about creating an ARN, see <http://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_Tagging.ARN.html#USER_Tagging.ARN.Constructing Constructing an RDS Amazon Resource Name (ARN)> .
attrResourceName :: Lens' AddTagsToResource Text
attrResourceName = lens _attrResourceName (\ s a -> s{_attrResourceName = a})

-- | The tags to be assigned to the Amazon RDS resource.
attrTags :: Lens' AddTagsToResource [Tag]
attrTags = lens _attrTags (\ s a -> s{_attrTags = a}) . _Coerce

instance AWSRequest AddTagsToResource where
        type Rs AddTagsToResource = AddTagsToResourceResponse
        request = postQuery rds
        response = receiveNull AddTagsToResourceResponse'

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
               "Version" =: ("2014-10-31" :: ByteString),
               "ResourceName" =: _attrResourceName,
               "Tags" =: toQueryList "Tag" _attrTags]

-- | /See:/ 'addTagsToResourceResponse' smart constructor.
data AddTagsToResourceResponse =
  AddTagsToResourceResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AddTagsToResourceResponse' with the minimum fields required to make a request.
--
addTagsToResourceResponse
    :: AddTagsToResourceResponse
addTagsToResourceResponse = AddTagsToResourceResponse'


instance NFData AddTagsToResourceResponse where
