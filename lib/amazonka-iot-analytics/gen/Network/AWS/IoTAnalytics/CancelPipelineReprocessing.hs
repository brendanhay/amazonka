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
    cprrrsResponseStatus,
  )
where

import qualified Network.AWS.IoTAnalytics.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCancelPipelineReprocessing' smart constructor.
data CancelPipelineReprocessing = CancelPipelineReprocessing'
  { -- | The name of pipeline for which data reprocessing is canceled.
    pipelineName :: Types.PipelineName,
    -- | The ID of the reprocessing task (returned by @StartPipelineReprocessing@ ).
    reprocessingId :: Types.ReprocessingId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CancelPipelineReprocessing' value with any optional fields omitted.
mkCancelPipelineReprocessing ::
  -- | 'pipelineName'
  Types.PipelineName ->
  -- | 'reprocessingId'
  Types.ReprocessingId ->
  CancelPipelineReprocessing
mkCancelPipelineReprocessing pipelineName reprocessingId =
  CancelPipelineReprocessing' {pipelineName, reprocessingId}

-- | The name of pipeline for which data reprocessing is canceled.
--
-- /Note:/ Consider using 'pipelineName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cprPipelineName :: Lens.Lens' CancelPipelineReprocessing Types.PipelineName
cprPipelineName = Lens.field @"pipelineName"
{-# DEPRECATED cprPipelineName "Use generic-lens or generic-optics with 'pipelineName' instead." #-}

-- | The ID of the reprocessing task (returned by @StartPipelineReprocessing@ ).
--
-- /Note:/ Consider using 'reprocessingId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cprReprocessingId :: Lens.Lens' CancelPipelineReprocessing Types.ReprocessingId
cprReprocessingId = Lens.field @"reprocessingId"
{-# DEPRECATED cprReprocessingId "Use generic-lens or generic-optics with 'reprocessingId' instead." #-}

instance Core.AWSRequest CancelPipelineReprocessing where
  type
    Rs CancelPipelineReprocessing =
      CancelPipelineReprocessingResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.DELETE,
        Core._rqPath =
          Core.rawPath
            ( "/pipelines/" Core.<> (Core.toText pipelineName)
                Core.<> ("/reprocessing/")
                Core.<> (Core.toText reprocessingId)
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = ""
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          CancelPipelineReprocessingResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCancelPipelineReprocessingResponse' smart constructor.
newtype CancelPipelineReprocessingResponse = CancelPipelineReprocessingResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'CancelPipelineReprocessingResponse' value with any optional fields omitted.
mkCancelPipelineReprocessingResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CancelPipelineReprocessingResponse
mkCancelPipelineReprocessingResponse responseStatus =
  CancelPipelineReprocessingResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cprrrsResponseStatus :: Lens.Lens' CancelPipelineReprocessingResponse Core.Int
cprrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED cprrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
