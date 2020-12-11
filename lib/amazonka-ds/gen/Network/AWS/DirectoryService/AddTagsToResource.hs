{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.AddTagsToResource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds or overwrites one or more tags for the specified directory. Each directory can have a maximum of 50 tags. Each tag consists of a key and optional value. Tag keys must be unique to each resource.
module Network.AWS.DirectoryService.AddTagsToResource
  ( -- * Creating a request
    AddTagsToResource (..),
    mkAddTagsToResource,

    -- ** Request lenses
    attrResourceId,
    attrTags,

    -- * Destructuring the response
    AddTagsToResourceResponse (..),
    mkAddTagsToResourceResponse,

    -- ** Response lenses
    attrrsResponseStatus,
  )
where

import Network.AWS.DirectoryService.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkAddTagsToResource' smart constructor.
data AddTagsToResource = AddTagsToResource'
  { resourceId ::
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
-- * 'resourceId' - Identifier (ID) for the directory to which to add the tag.
-- * 'tags' - The tags to be assigned to the directory.
mkAddTagsToResource ::
  -- | 'resourceId'
  Lude.Text ->
  AddTagsToResource
mkAddTagsToResource pResourceId_ =
  AddTagsToResource' {resourceId = pResourceId_, tags = Lude.mempty}

-- | Identifier (ID) for the directory to which to add the tag.
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
attrResourceId :: Lens.Lens' AddTagsToResource Lude.Text
attrResourceId = Lens.lens (resourceId :: AddTagsToResource -> Lude.Text) (\s a -> s {resourceId = a} :: AddTagsToResource)
{-# DEPRECATED attrResourceId "Use generic-lens or generic-optics with 'resourceId' instead." #-}

-- | The tags to be assigned to the directory.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
attrTags :: Lens.Lens' AddTagsToResource [Tag]
attrTags = Lens.lens (tags :: AddTagsToResource -> [Tag]) (\s a -> s {tags = a} :: AddTagsToResource)
{-# DEPRECATED attrTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.AWSRequest AddTagsToResource where
  type Rs AddTagsToResource = AddTagsToResourceResponse
  request = Req.postJSON directoryServiceService
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
              Lude.=# ("DirectoryService_20150416.AddTagsToResource" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON AddTagsToResource where
  toJSON AddTagsToResource' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("ResourceId" Lude..= resourceId),
            Lude.Just ("Tags" Lude..= tags)
          ]
      )

instance Lude.ToPath AddTagsToResource where
  toPath = Lude.const "/"

instance Lude.ToQuery AddTagsToResource where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkAddTagsToResourceResponse' smart constructor.
newtype AddTagsToResourceResponse = AddTagsToResourceResponse'
  { responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
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
