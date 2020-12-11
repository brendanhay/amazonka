{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DataPipeline.RemoveTags
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes existing tags from the specified pipeline.
module Network.AWS.DataPipeline.RemoveTags
  ( -- * Creating a request
    RemoveTags (..),
    mkRemoveTags,

    -- ** Request lenses
    rtPipelineId,
    rtTagKeys,

    -- * Destructuring the response
    RemoveTagsResponse (..),
    mkRemoveTagsResponse,

    -- ** Response lenses
    rtrsResponseStatus,
  )
where

import Network.AWS.DataPipeline.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Contains the parameters for RemoveTags.
--
-- /See:/ 'mkRemoveTags' smart constructor.
data RemoveTags = RemoveTags'
  { pipelineId :: Lude.Text,
    tagKeys :: [Lude.Text]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RemoveTags' with the minimum fields required to make a request.
--
-- * 'pipelineId' - The ID of the pipeline.
-- * 'tagKeys' - The keys of the tags to remove.
mkRemoveTags ::
  -- | 'pipelineId'
  Lude.Text ->
  RemoveTags
mkRemoveTags pPipelineId_ =
  RemoveTags' {pipelineId = pPipelineId_, tagKeys = Lude.mempty}

-- | The ID of the pipeline.
--
-- /Note:/ Consider using 'pipelineId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtPipelineId :: Lens.Lens' RemoveTags Lude.Text
rtPipelineId = Lens.lens (pipelineId :: RemoveTags -> Lude.Text) (\s a -> s {pipelineId = a} :: RemoveTags)
{-# DEPRECATED rtPipelineId "Use generic-lens or generic-optics with 'pipelineId' instead." #-}

-- | The keys of the tags to remove.
--
-- /Note:/ Consider using 'tagKeys' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtTagKeys :: Lens.Lens' RemoveTags [Lude.Text]
rtTagKeys = Lens.lens (tagKeys :: RemoveTags -> [Lude.Text]) (\s a -> s {tagKeys = a} :: RemoveTags)
{-# DEPRECATED rtTagKeys "Use generic-lens or generic-optics with 'tagKeys' instead." #-}

instance Lude.AWSRequest RemoveTags where
  type Rs RemoveTags = RemoveTagsResponse
  request = Req.postJSON dataPipelineService
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
              Lude.=# ("DataPipeline.RemoveTags" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON RemoveTags where
  toJSON RemoveTags' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("pipelineId" Lude..= pipelineId),
            Lude.Just ("tagKeys" Lude..= tagKeys)
          ]
      )

instance Lude.ToPath RemoveTags where
  toPath = Lude.const "/"

instance Lude.ToQuery RemoveTags where
  toQuery = Lude.const Lude.mempty

-- | Contains the output of RemoveTags.
--
-- /See:/ 'mkRemoveTagsResponse' smart constructor.
newtype RemoveTagsResponse = RemoveTagsResponse'
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
