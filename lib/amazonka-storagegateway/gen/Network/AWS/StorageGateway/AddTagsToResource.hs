{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.AddTagsToResource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds one or more tags to the specified resource. You use tags to add metadata to resources, which you can use to categorize these resources. For example, you can categorize resources by purpose, owner, environment, or team. Each tag consists of a key and a value, which you define. You can add tags to the following AWS Storage Gateway resources:
--
--
--     * Storage gateways of all types
--
--
--     * Storage volumes
--
--
--     * Virtual tapes
--
--
--     * NFS and SMB file shares
--
--
-- You can create a maximum of 50 tags for each resource. Virtual tapes and storage volumes that are recovered to a new gateway maintain their tags.
module Network.AWS.StorageGateway.AddTagsToResource
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
    attrrsResourceARN,
    attrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.StorageGateway.Types

-- | AddTagsToResourceInput
--
-- /See:/ 'mkAddTagsToResource' smart constructor.
data AddTagsToResource = AddTagsToResource'
  { resourceARN ::
      Lude.Text,
    tags :: [Tag]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AddTagsToResource' with the minimum fields required to make a request.
--
-- * 'resourceARN' - The Amazon Resource Name (ARN) of the resource you want to add tags to.
-- * 'tags' - The key-value pair that represents the tag you want to add to the resource. The value can be an empty string.
mkAddTagsToResource ::
  -- | 'resourceARN'
  Lude.Text ->
  AddTagsToResource
mkAddTagsToResource pResourceARN_ =
  AddTagsToResource'
    { resourceARN = pResourceARN_,
      tags = Lude.mempty
    }

-- | The Amazon Resource Name (ARN) of the resource you want to add tags to.
--
-- /Note:/ Consider using 'resourceARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
attrResourceARN :: Lens.Lens' AddTagsToResource Lude.Text
attrResourceARN = Lens.lens (resourceARN :: AddTagsToResource -> Lude.Text) (\s a -> s {resourceARN = a} :: AddTagsToResource)
{-# DEPRECATED attrResourceARN "Use generic-lens or generic-optics with 'resourceARN' instead." #-}

-- | The key-value pair that represents the tag you want to add to the resource. The value can be an empty string.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
attrTags :: Lens.Lens' AddTagsToResource [Tag]
attrTags = Lens.lens (tags :: AddTagsToResource -> [Tag]) (\s a -> s {tags = a} :: AddTagsToResource)
{-# DEPRECATED attrTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.AWSRequest AddTagsToResource where
  type Rs AddTagsToResource = AddTagsToResourceResponse
  request = Req.postJSON storageGatewayService
  response =
    Res.receiveJSON
      ( \s h x ->
          AddTagsToResourceResponse'
            Lude.<$> (x Lude..?> "ResourceARN") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders AddTagsToResource where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("StorageGateway_20130630.AddTagsToResource" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON AddTagsToResource where
  toJSON AddTagsToResource' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("ResourceARN" Lude..= resourceARN),
            Lude.Just ("Tags" Lude..= tags)
          ]
      )

instance Lude.ToPath AddTagsToResource where
  toPath = Lude.const "/"

instance Lude.ToQuery AddTagsToResource where
  toQuery = Lude.const Lude.mempty

-- | AddTagsToResourceOutput
--
-- /See:/ 'mkAddTagsToResourceResponse' smart constructor.
data AddTagsToResourceResponse = AddTagsToResourceResponse'
  { resourceARN ::
      Lude.Maybe Lude.Text,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AddTagsToResourceResponse' with the minimum fields required to make a request.
--
-- * 'resourceARN' - The Amazon Resource Name (ARN) of the resource you want to add tags to.
-- * 'responseStatus' - The response status code.
mkAddTagsToResourceResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  AddTagsToResourceResponse
mkAddTagsToResourceResponse pResponseStatus_ =
  AddTagsToResourceResponse'
    { resourceARN = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The Amazon Resource Name (ARN) of the resource you want to add tags to.
--
-- /Note:/ Consider using 'resourceARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
attrrsResourceARN :: Lens.Lens' AddTagsToResourceResponse (Lude.Maybe Lude.Text)
attrrsResourceARN = Lens.lens (resourceARN :: AddTagsToResourceResponse -> Lude.Maybe Lude.Text) (\s a -> s {resourceARN = a} :: AddTagsToResourceResponse)
{-# DEPRECATED attrrsResourceARN "Use generic-lens or generic-optics with 'resourceARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
attrrsResponseStatus :: Lens.Lens' AddTagsToResourceResponse Lude.Int
attrrsResponseStatus = Lens.lens (responseStatus :: AddTagsToResourceResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: AddTagsToResourceResponse)
{-# DEPRECATED attrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
