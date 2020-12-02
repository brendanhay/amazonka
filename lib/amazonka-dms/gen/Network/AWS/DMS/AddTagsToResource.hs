{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.AddTagsToResource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds metadata tags to an AWS DMS resource, including replication instance, endpoint, security group, and migration task. These tags can also be used with cost allocation reporting to track cost associated with DMS resources, or used in a Condition statement in an IAM policy for DMS. For more information, see <https://docs.aws.amazon.com/dms/latest/APIReference/API_Tag.html @Tag@ > data type description.
module Network.AWS.DMS.AddTagsToResource
  ( -- * Creating a Request
    addTagsToResource,
    AddTagsToResource,

    -- * Request Lenses
    attrResourceARN,
    attrTags,

    -- * Destructuring the Response
    addTagsToResourceResponse,
    AddTagsToResourceResponse,

    -- * Response Lenses
    attrrsResponseStatus,
  )
where

import Network.AWS.DMS.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Associates a set of tags with an AWS DMS resource.
--
--
--
-- /See:/ 'addTagsToResource' smart constructor.
data AddTagsToResource = AddTagsToResource'
  { _attrResourceARN ::
      !Text,
    _attrTags :: ![Tag]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AddTagsToResource' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'attrResourceARN' - Identifies the AWS DMS resource to which tags should be added. The value for this parameter is an Amazon Resource Name (ARN). For AWS DMS, you can tag a replication instance, an endpoint, or a replication task.
--
-- * 'attrTags' - One or more tags to be assigned to the resource.
addTagsToResource ::
  -- | 'attrResourceARN'
  Text ->
  AddTagsToResource
addTagsToResource pResourceARN_ =
  AddTagsToResource'
    { _attrResourceARN = pResourceARN_,
      _attrTags = mempty
    }

-- | Identifies the AWS DMS resource to which tags should be added. The value for this parameter is an Amazon Resource Name (ARN). For AWS DMS, you can tag a replication instance, an endpoint, or a replication task.
attrResourceARN :: Lens' AddTagsToResource Text
attrResourceARN = lens _attrResourceARN (\s a -> s {_attrResourceARN = a})

-- | One or more tags to be assigned to the resource.
attrTags :: Lens' AddTagsToResource [Tag]
attrTags = lens _attrTags (\s a -> s {_attrTags = a}) . _Coerce

instance AWSRequest AddTagsToResource where
  type Rs AddTagsToResource = AddTagsToResourceResponse
  request = postJSON dms
  response =
    receiveEmpty
      (\s h x -> AddTagsToResourceResponse' <$> (pure (fromEnum s)))

instance Hashable AddTagsToResource

instance NFData AddTagsToResource

instance ToHeaders AddTagsToResource where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("AmazonDMSv20160101.AddTagsToResource" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON AddTagsToResource where
  toJSON AddTagsToResource' {..} =
    object
      ( catMaybes
          [ Just ("ResourceArn" .= _attrResourceARN),
            Just ("Tags" .= _attrTags)
          ]
      )

instance ToPath AddTagsToResource where
  toPath = const "/"

instance ToQuery AddTagsToResource where
  toQuery = const mempty

-- |
--
--
--
-- /See:/ 'addTagsToResourceResponse' smart constructor.
newtype AddTagsToResourceResponse = AddTagsToResourceResponse'
  { _attrrsResponseStatus ::
      Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AddTagsToResourceResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'attrrsResponseStatus' - -- | The response status code.
addTagsToResourceResponse ::
  -- | 'attrrsResponseStatus'
  Int ->
  AddTagsToResourceResponse
addTagsToResourceResponse pResponseStatus_ =
  AddTagsToResourceResponse'
    { _attrrsResponseStatus =
        pResponseStatus_
    }

-- | -- | The response status code.
attrrsResponseStatus :: Lens' AddTagsToResourceResponse Int
attrrsResponseStatus = lens _attrrsResponseStatus (\s a -> s {_attrrsResponseStatus = a})

instance NFData AddTagsToResourceResponse
