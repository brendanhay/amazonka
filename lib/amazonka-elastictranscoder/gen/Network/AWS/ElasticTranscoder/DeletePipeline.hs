{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticTranscoder.DeletePipeline
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The DeletePipeline operation removes a pipeline.
--
-- You can only delete a pipeline that has never been used or that is not currently in use (doesn't contain any active jobs). If the pipeline is currently in use, @DeletePipeline@ returns an error.
module Network.AWS.ElasticTranscoder.DeletePipeline
  ( -- * Creating a request
    DeletePipeline (..),
    mkDeletePipeline,

    -- ** Request lenses
    dId,

    -- * Destructuring the response
    DeletePipelineResponse (..),
    mkDeletePipelineResponse,

    -- ** Response lenses
    drsResponseStatus,
  )
where

import Network.AWS.ElasticTranscoder.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | The @DeletePipelineRequest@ structure.
--
-- /See:/ 'mkDeletePipeline' smart constructor.
newtype DeletePipeline = DeletePipeline' {id :: Lude.Text}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeletePipeline' with the minimum fields required to make a request.
--
-- * 'id' - The identifier of the pipeline that you want to delete.
mkDeletePipeline ::
  -- | 'id'
  Lude.Text ->
  DeletePipeline
mkDeletePipeline pId_ = DeletePipeline' {id = pId_}

-- | The identifier of the pipeline that you want to delete.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dId :: Lens.Lens' DeletePipeline Lude.Text
dId = Lens.lens (id :: DeletePipeline -> Lude.Text) (\s a -> s {id = a} :: DeletePipeline)
{-# DEPRECATED dId "Use generic-lens or generic-optics with 'id' instead." #-}

instance Lude.AWSRequest DeletePipeline where
  type Rs DeletePipeline = DeletePipelineResponse
  request = Req.delete elasticTranscoderService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DeletePipelineResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeletePipeline where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeletePipeline where
  toPath DeletePipeline' {..} =
    Lude.mconcat ["/2012-09-25/pipelines/", Lude.toBS id]

instance Lude.ToQuery DeletePipeline where
  toQuery = Lude.const Lude.mempty

-- | The @DeletePipelineResponse@ structure.
--
-- /See:/ 'mkDeletePipelineResponse' smart constructor.
newtype DeletePipelineResponse = DeletePipelineResponse'
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

-- | Creates a value of 'DeletePipelineResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeletePipelineResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeletePipelineResponse
mkDeletePipelineResponse pResponseStatus_ =
  DeletePipelineResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsResponseStatus :: Lens.Lens' DeletePipelineResponse Lude.Int
drsResponseStatus = Lens.lens (responseStatus :: DeletePipelineResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeletePipelineResponse)
{-# DEPRECATED drsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
