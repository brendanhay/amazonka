{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DataPipeline.AddTags
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds or modifies tags for the specified pipeline.
module Network.AWS.DataPipeline.AddTags
  ( -- * Creating a request
    AddTags (..),
    mkAddTags,

    -- ** Request lenses
    atPipelineId,
    atTags,

    -- * Destructuring the response
    AddTagsResponse (..),
    mkAddTagsResponse,

    -- ** Response lenses
    atrsResponseStatus,
  )
where

import Network.AWS.DataPipeline.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Contains the parameters for AddTags.
--
-- /See:/ 'mkAddTags' smart constructor.
data AddTags = AddTags'
  { -- | The ID of the pipeline.
    pipelineId :: Lude.Text,
    -- | The tags to add, as key/value pairs.
    tags :: [Tag]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AddTags' with the minimum fields required to make a request.
--
-- * 'pipelineId' - The ID of the pipeline.
-- * 'tags' - The tags to add, as key/value pairs.
mkAddTags ::
  -- | 'pipelineId'
  Lude.Text ->
  AddTags
mkAddTags pPipelineId_ =
  AddTags' {pipelineId = pPipelineId_, tags = Lude.mempty}

-- | The ID of the pipeline.
--
-- /Note:/ Consider using 'pipelineId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atPipelineId :: Lens.Lens' AddTags Lude.Text
atPipelineId = Lens.lens (pipelineId :: AddTags -> Lude.Text) (\s a -> s {pipelineId = a} :: AddTags)
{-# DEPRECATED atPipelineId "Use generic-lens or generic-optics with 'pipelineId' instead." #-}

-- | The tags to add, as key/value pairs.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atTags :: Lens.Lens' AddTags [Tag]
atTags = Lens.lens (tags :: AddTags -> [Tag]) (\s a -> s {tags = a} :: AddTags)
{-# DEPRECATED atTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.AWSRequest AddTags where
  type Rs AddTags = AddTagsResponse
  request = Req.postJSON dataPipelineService
  response =
    Res.receiveEmpty
      ( \s h x ->
          AddTagsResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders AddTags where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("DataPipeline.AddTags" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON AddTags where
  toJSON AddTags' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("pipelineId" Lude..= pipelineId),
            Lude.Just ("tags" Lude..= tags)
          ]
      )

instance Lude.ToPath AddTags where
  toPath = Lude.const "/"

instance Lude.ToQuery AddTags where
  toQuery = Lude.const Lude.mempty

-- | Contains the output of AddTags.
--
-- /See:/ 'mkAddTagsResponse' smart constructor.
newtype AddTagsResponse = AddTagsResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AddTagsResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkAddTagsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  AddTagsResponse
mkAddTagsResponse pResponseStatus_ =
  AddTagsResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atrsResponseStatus :: Lens.Lens' AddTagsResponse Lude.Int
atrsResponseStatus = Lens.lens (responseStatus :: AddTagsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: AddTagsResponse)
{-# DEPRECATED atrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
