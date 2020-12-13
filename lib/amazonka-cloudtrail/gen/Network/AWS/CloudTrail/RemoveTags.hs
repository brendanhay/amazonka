{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudTrail.RemoveTags
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes the specified tags from a trail.
module Network.AWS.CloudTrail.RemoveTags
  ( -- * Creating a request
    RemoveTags (..),
    mkRemoveTags,

    -- ** Request lenses
    rResourceId,
    rTagsList,

    -- * Destructuring the response
    RemoveTagsResponse (..),
    mkRemoveTagsResponse,

    -- ** Response lenses
    rtrsResponseStatus,
  )
where

import Network.AWS.CloudTrail.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Specifies the tags to remove from a trail.
--
-- /See:/ 'mkRemoveTags' smart constructor.
data RemoveTags = RemoveTags'
  { -- | Specifies the ARN of the trail from which tags should be removed. The format of a trail ARN is:
    --
    -- @arn:aws:cloudtrail:us-east-2:123456789012:trail/MyTrail@
    resourceId :: Lude.Text,
    -- | Specifies a list of tags to be removed.
    tagsList :: Lude.Maybe [Tag]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RemoveTags' with the minimum fields required to make a request.
--
-- * 'resourceId' - Specifies the ARN of the trail from which tags should be removed. The format of a trail ARN is:
--
-- @arn:aws:cloudtrail:us-east-2:123456789012:trail/MyTrail@
-- * 'tagsList' - Specifies a list of tags to be removed.
mkRemoveTags ::
  -- | 'resourceId'
  Lude.Text ->
  RemoveTags
mkRemoveTags pResourceId_ =
  RemoveTags' {resourceId = pResourceId_, tagsList = Lude.Nothing}

-- | Specifies the ARN of the trail from which tags should be removed. The format of a trail ARN is:
--
-- @arn:aws:cloudtrail:us-east-2:123456789012:trail/MyTrail@
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rResourceId :: Lens.Lens' RemoveTags Lude.Text
rResourceId = Lens.lens (resourceId :: RemoveTags -> Lude.Text) (\s a -> s {resourceId = a} :: RemoveTags)
{-# DEPRECATED rResourceId "Use generic-lens or generic-optics with 'resourceId' instead." #-}

-- | Specifies a list of tags to be removed.
--
-- /Note:/ Consider using 'tagsList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rTagsList :: Lens.Lens' RemoveTags (Lude.Maybe [Tag])
rTagsList = Lens.lens (tagsList :: RemoveTags -> Lude.Maybe [Tag]) (\s a -> s {tagsList = a} :: RemoveTags)
{-# DEPRECATED rTagsList "Use generic-lens or generic-optics with 'tagsList' instead." #-}

instance Lude.AWSRequest RemoveTags where
  type Rs RemoveTags = RemoveTagsResponse
  request = Req.postJSON cloudTrailService
  response =
    Res.receiveEmpty
      ( \s h x ->
          RemoveTagsResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders RemoveTags where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "com.amazonaws.cloudtrail.v20131101.CloudTrail_20131101.RemoveTags" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON RemoveTags where
  toJSON RemoveTags' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("ResourceId" Lude..= resourceId),
            ("TagsList" Lude..=) Lude.<$> tagsList
          ]
      )

instance Lude.ToPath RemoveTags where
  toPath = Lude.const "/"

instance Lude.ToQuery RemoveTags where
  toQuery = Lude.const Lude.mempty

-- | Returns the objects or data listed below if successful. Otherwise, returns an error.
--
-- /See:/ 'mkRemoveTagsResponse' smart constructor.
newtype RemoveTagsResponse = RemoveTagsResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RemoveTagsResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkRemoveTagsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  RemoveTagsResponse
mkRemoveTagsResponse pResponseStatus_ =
  RemoveTagsResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtrsResponseStatus :: Lens.Lens' RemoveTagsResponse Lude.Int
rtrsResponseStatus = Lens.lens (responseStatus :: RemoveTagsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: RemoveTagsResponse)
{-# DEPRECATED rtrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
