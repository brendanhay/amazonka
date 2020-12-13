{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.CancelPipelineReprocessing
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels the reprocessing of data through the pipeline.
module Network.AWS.IoTAnalytics.CancelPipelineReprocessing
  ( -- * Creating a request
    CancelPipelineReprocessing (..),
    mkCancelPipelineReprocessing,

    -- ** Request lenses
    cprPipelineName,
    cprReprocessingId,

    -- * Destructuring the response
    CancelPipelineReprocessingResponse (..),
    mkCancelPipelineReprocessingResponse,

    -- ** Response lenses
    cprrsResponseStatus,
  )
where

import Network.AWS.IoTAnalytics.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCancelPipelineReprocessing' smart constructor.
data CancelPipelineReprocessing = CancelPipelineReprocessing'
  { -- | The name of pipeline for which data reprocessing is canceled.
    pipelineName :: Lude.Text,
    -- | The ID of the reprocessing task (returned by @StartPipelineReprocessing@ ).
    reprocessingId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CancelPipelineReprocessing' with the minimum fields required to make a request.
--
-- * 'pipelineName' - The name of pipeline for which data reprocessing is canceled.
-- * 'reprocessingId' - The ID of the reprocessing task (returned by @StartPipelineReprocessing@ ).
mkCancelPipelineReprocessing ::
  -- | 'pipelineName'
  Lude.Text ->
  -- | 'reprocessingId'
  Lude.Text ->
  CancelPipelineReprocessing
mkCancelPipelineReprocessing pPipelineName_ pReprocessingId_ =
  CancelPipelineReprocessing'
    { pipelineName = pPipelineName_,
      reprocessingId = pReprocessingId_
    }

-- | The name of pipeline for which data reprocessing is canceled.
--
-- /Note:/ Consider using 'pipelineName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cprPipelineName :: Lens.Lens' CancelPipelineReprocessing Lude.Text
cprPipelineName = Lens.lens (pipelineName :: CancelPipelineReprocessing -> Lude.Text) (\s a -> s {pipelineName = a} :: CancelPipelineReprocessing)
{-# DEPRECATED cprPipelineName "Use generic-lens or generic-optics with 'pipelineName' instead." #-}

-- | The ID of the reprocessing task (returned by @StartPipelineReprocessing@ ).
--
-- /Note:/ Consider using 'reprocessingId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cprReprocessingId :: Lens.Lens' CancelPipelineReprocessing Lude.Text
cprReprocessingId = Lens.lens (reprocessingId :: CancelPipelineReprocessing -> Lude.Text) (\s a -> s {reprocessingId = a} :: CancelPipelineReprocessing)
{-# DEPRECATED cprReprocessingId "Use generic-lens or generic-optics with 'reprocessingId' instead." #-}

instance Lude.AWSRequest CancelPipelineReprocessing where
  type
    Rs CancelPipelineReprocessing =
      CancelPipelineReprocessingResponse
  request = Req.delete ioTAnalyticsService
  response =
    Res.receiveEmpty
      ( \s h x ->
          CancelPipelineReprocessingResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CancelPipelineReprocessing where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath CancelPipelineReprocessing where
  toPath CancelPipelineReprocessing' {..} =
    Lude.mconcat
      [ "/pipelines/",
        Lude.toBS pipelineName,
        "/reprocessing/",
        Lude.toBS reprocessingId
      ]

instance Lude.ToQuery CancelPipelineReprocessing where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCancelPipelineReprocessingResponse' smart constructor.
newtype CancelPipelineReprocessingResponse = CancelPipelineReprocessingResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CancelPipelineReprocessingResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkCancelPipelineReprocessingResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CancelPipelineReprocessingResponse
mkCancelPipelineReprocessingResponse pResponseStatus_ =
  CancelPipelineReprocessingResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cprrsResponseStatus :: Lens.Lens' CancelPipelineReprocessingResponse Lude.Int
cprrsResponseStatus = Lens.lens (responseStatus :: CancelPipelineReprocessingResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CancelPipelineReprocessingResponse)
{-# DEPRECATED cprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
