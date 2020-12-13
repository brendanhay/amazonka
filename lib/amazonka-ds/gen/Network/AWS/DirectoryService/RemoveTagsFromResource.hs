{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.RemoveTagsFromResource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes tags from a directory.
module Network.AWS.DirectoryService.RemoveTagsFromResource
  ( -- * Creating a request
    RemoveTagsFromResource (..),
    mkRemoveTagsFromResource,

    -- ** Request lenses
    rtfrResourceId,
    rtfrTagKeys,

    -- * Destructuring the response
    RemoveTagsFromResourceResponse (..),
    mkRemoveTagsFromResourceResponse,

    -- ** Response lenses
    rtfrrsResponseStatus,
  )
where

import Network.AWS.DirectoryService.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkRemoveTagsFromResource' smart constructor.
data RemoveTagsFromResource = RemoveTagsFromResource'
  { -- | Identifier (ID) of the directory from which to remove the tag.
    resourceId :: Lude.Text,
    -- | The tag key (name) of the tag to be removed.
    tagKeys :: [Lude.Text]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RemoveTagsFromResource' with the minimum fields required to make a request.
--
-- * 'resourceId' - Identifier (ID) of the directory from which to remove the tag.
-- * 'tagKeys' - The tag key (name) of the tag to be removed.
mkRemoveTagsFromResource ::
  -- | 'resourceId'
  Lude.Text ->
  RemoveTagsFromResource
mkRemoveTagsFromResource pResourceId_ =
  RemoveTagsFromResource'
    { resourceId = pResourceId_,
      tagKeys = Lude.mempty
    }

-- | Identifier (ID) of the directory from which to remove the tag.
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtfrResourceId :: Lens.Lens' RemoveTagsFromResource Lude.Text
rtfrResourceId = Lens.lens (resourceId :: RemoveTagsFromResource -> Lude.Text) (\s a -> s {resourceId = a} :: RemoveTagsFromResource)
{-# DEPRECATED rtfrResourceId "Use generic-lens or generic-optics with 'resourceId' instead." #-}

-- | The tag key (name) of the tag to be removed.
--
-- /Note:/ Consider using 'tagKeys' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtfrTagKeys :: Lens.Lens' RemoveTagsFromResource [Lude.Text]
rtfrTagKeys = Lens.lens (tagKeys :: RemoveTagsFromResource -> [Lude.Text]) (\s a -> s {tagKeys = a} :: RemoveTagsFromResource)
{-# DEPRECATED rtfrTagKeys "Use generic-lens or generic-optics with 'tagKeys' instead." #-}

instance Lude.AWSRequest RemoveTagsFromResource where
  type Rs RemoveTagsFromResource = RemoveTagsFromResourceResponse
  request = Req.postJSON directoryServiceService
  response =
    Res.receiveEmpty
      ( \s h x ->
          RemoveTagsFromResourceResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders RemoveTagsFromResource where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "DirectoryService_20150416.RemoveTagsFromResource" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON RemoveTagsFromResource where
  toJSON RemoveTagsFromResource' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("ResourceId" Lude..= resourceId),
            Lude.Just ("TagKeys" Lude..= tagKeys)
          ]
      )

instance Lude.ToPath RemoveTagsFromResource where
  toPath = Lude.const "/"

instance Lude.ToQuery RemoveTagsFromResource where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkRemoveTagsFromResourceResponse' smart constructor.
newtype RemoveTagsFromResourceResponse = RemoveTagsFromResourceResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RemoveTagsFromResourceResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkRemoveTagsFromResourceResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  RemoveTagsFromResourceResponse
mkRemoveTagsFromResourceResponse pResponseStatus_ =
  RemoveTagsFromResourceResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtfrrsResponseStatus :: Lens.Lens' RemoveTagsFromResourceResponse Lude.Int
rtfrrsResponseStatus = Lens.lens (responseStatus :: RemoveTagsFromResourceResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: RemoveTagsFromResourceResponse)
{-# DEPRECATED rtfrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
