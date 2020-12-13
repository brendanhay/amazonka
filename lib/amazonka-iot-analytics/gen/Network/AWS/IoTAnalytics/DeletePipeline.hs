{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.DeletePipeline
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified pipeline.
module Network.AWS.IoTAnalytics.DeletePipeline
  ( -- * Creating a request
    DeletePipeline (..),
    mkDeletePipeline,

    -- ** Request lenses
    dpPipelineName,

    -- * Destructuring the response
    DeletePipelineResponse (..),
    mkDeletePipelineResponse,
  )
where

import Network.AWS.IoTAnalytics.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeletePipeline' smart constructor.
newtype DeletePipeline = DeletePipeline'
  { -- | The name of the pipeline to delete.
    pipelineName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeletePipeline' with the minimum fields required to make a request.
--
-- * 'pipelineName' - The name of the pipeline to delete.
mkDeletePipeline ::
  -- | 'pipelineName'
  Lude.Text ->
  DeletePipeline
mkDeletePipeline pPipelineName_ =
  DeletePipeline' {pipelineName = pPipelineName_}

-- | The name of the pipeline to delete.
--
-- /Note:/ Consider using 'pipelineName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpPipelineName :: Lens.Lens' DeletePipeline Lude.Text
dpPipelineName = Lens.lens (pipelineName :: DeletePipeline -> Lude.Text) (\s a -> s {pipelineName = a} :: DeletePipeline)
{-# DEPRECATED dpPipelineName "Use generic-lens or generic-optics with 'pipelineName' instead." #-}

instance Lude.AWSRequest DeletePipeline where
  type Rs DeletePipeline = DeletePipelineResponse
  request = Req.delete ioTAnalyticsService
  response = Res.receiveNull DeletePipelineResponse'

instance Lude.ToHeaders DeletePipeline where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeletePipeline where
  toPath DeletePipeline' {..} =
    Lude.mconcat ["/pipelines/", Lude.toBS pipelineName]

instance Lude.ToQuery DeletePipeline where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeletePipelineResponse' smart constructor.
data DeletePipelineResponse = DeletePipelineResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeletePipelineResponse' with the minimum fields required to make a request.
mkDeletePipelineResponse ::
  DeletePipelineResponse
mkDeletePipelineResponse = DeletePipelineResponse'
