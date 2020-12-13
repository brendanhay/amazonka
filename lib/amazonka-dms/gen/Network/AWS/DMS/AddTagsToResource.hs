{-# OPTIONS_GHC -fno-warn-deprecations #-}
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
  ( -- * Creating a request
    AddTagsToResource (..),
    mkAddTagsToResource,

    -- ** Request lenses
    attrResourceARN,
    attrTags,

    -- * Destructuring the response
    AddTagsToResourceResponse (..),
    mkAddTagsToResourceResponse,

    -- ** Response lenses
    attrrsResponseStatus,
  )
where

import Network.AWS.DMS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Associates a set of tags with an AWS DMS resource.
--
-- /See:/ 'mkAddTagsToResource' smart constructor.
data AddTagsToResource = AddTagsToResource'
  { -- | Identifies the AWS DMS resource to which tags should be added. The value for this parameter is an Amazon Resource Name (ARN).
    --
    -- For AWS DMS, you can tag a replication instance, an endpoint, or a replication task.
    resourceARN :: Lude.Text,
    -- | One or more tags to be assigned to the resource.
    tags :: [Tag]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AddTagsToResource' with the minimum fields required to make a request.
--
-- * 'resourceARN' - Identifies the AWS DMS resource to which tags should be added. The value for this parameter is an Amazon Resource Name (ARN).
--
-- For AWS DMS, you can tag a replication instance, an endpoint, or a replication task.
-- * 'tags' - One or more tags to be assigned to the resource.
mkAddTagsToResource ::
  -- | 'resourceARN'
  Lude.Text ->
  AddTagsToResource
mkAddTagsToResource pResourceARN_ =
  AddTagsToResource'
    { resourceARN = pResourceARN_,
      tags = Lude.mempty
    }

-- | Identifies the AWS DMS resource to which tags should be added. The value for this parameter is an Amazon Resource Name (ARN).
--
-- For AWS DMS, you can tag a replication instance, an endpoint, or a replication task.
--
-- /Note:/ Consider using 'resourceARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
attrResourceARN :: Lens.Lens' AddTagsToResource Lude.Text
attrResourceARN = Lens.lens (resourceARN :: AddTagsToResource -> Lude.Text) (\s a -> s {resourceARN = a} :: AddTagsToResource)
{-# DEPRECATED attrResourceARN "Use generic-lens or generic-optics with 'resourceARN' instead." #-}

-- | One or more tags to be assigned to the resource.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
attrTags :: Lens.Lens' AddTagsToResource [Tag]
attrTags = Lens.lens (tags :: AddTagsToResource -> [Tag]) (\s a -> s {tags = a} :: AddTagsToResource)
{-# DEPRECATED attrTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.AWSRequest AddTagsToResource where
  type Rs AddTagsToResource = AddTagsToResourceResponse
  request = Req.postJSON dmsService
  response =
    Res.receiveEmpty
      ( \s h x ->
          AddTagsToResourceResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders AddTagsToResource where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AmazonDMSv20160101.AddTagsToResource" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON AddTagsToResource where
  toJSON AddTagsToResource' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("ResourceArn" Lude..= resourceARN),
            Lude.Just ("Tags" Lude..= tags)
          ]
      )

instance Lude.ToPath AddTagsToResource where
  toPath = Lude.const "/"

instance Lude.ToQuery AddTagsToResource where
  toQuery = Lude.const Lude.mempty

-- |
--
-- /See:/ 'mkAddTagsToResourceResponse' smart constructor.
newtype AddTagsToResourceResponse = AddTagsToResourceResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AddTagsToResourceResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkAddTagsToResourceResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  AddTagsToResourceResponse
mkAddTagsToResourceResponse pResponseStatus_ =
  AddTagsToResourceResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
attrrsResponseStatus :: Lens.Lens' AddTagsToResourceResponse Lude.Int
attrrsResponseStatus = Lens.lens (responseStatus :: AddTagsToResourceResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: AddTagsToResourceResponse)
{-# DEPRECATED attrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
