{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      CancelPipelineReprocessing (..)
    , mkCancelPipelineReprocessing
    -- ** Request lenses
    , cprPipelineName
    , cprReprocessingId

    -- * Destructuring the response
    , CancelPipelineReprocessingResponse (..)
    , mkCancelPipelineReprocessingResponse
    -- ** Response lenses
    , cprrrsResponseStatus
    ) where

import qualified Network.AWS.IoTAnalytics.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCancelPipelineReprocessing' smart constructor.
data CancelPipelineReprocessing = CancelPipelineReprocessing'
  { pipelineName :: Types.PipelineName
    -- ^ The name of pipeline for which data reprocessing is canceled.
  , reprocessingId :: Types.ReprocessingId
    -- ^ The ID of the reprocessing task (returned by @StartPipelineReprocessing@ ).
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CancelPipelineReprocessing' value with any optional fields omitted.
mkCancelPipelineReprocessing
    :: Types.PipelineName -- ^ 'pipelineName'
    -> Types.ReprocessingId -- ^ 'reprocessingId'
    -> CancelPipelineReprocessing
mkCancelPipelineReprocessing pipelineName reprocessingId
  = CancelPipelineReprocessing'{pipelineName, reprocessingId}

-- | The name of pipeline for which data reprocessing is canceled.
--
-- /Note:/ Consider using 'pipelineName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cprPipelineName :: Lens.Lens' CancelPipelineReprocessing Types.PipelineName
cprPipelineName = Lens.field @"pipelineName"
{-# INLINEABLE cprPipelineName #-}
{-# DEPRECATED pipelineName "Use generic-lens or generic-optics with 'pipelineName' instead"  #-}

-- | The ID of the reprocessing task (returned by @StartPipelineReprocessing@ ).
--
-- /Note:/ Consider using 'reprocessingId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cprReprocessingId :: Lens.Lens' CancelPipelineReprocessing Types.ReprocessingId
cprReprocessingId = Lens.field @"reprocessingId"
{-# INLINEABLE cprReprocessingId #-}
{-# DEPRECATED reprocessingId "Use generic-lens or generic-optics with 'reprocessingId' instead"  #-}

instance Core.ToQuery CancelPipelineReprocessing where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CancelPipelineReprocessing where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest CancelPipelineReprocessing where
        type Rs CancelPipelineReprocessing =
             CancelPipelineReprocessingResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.DELETE,
                         Core._rqPath =
                           "/pipelines/" Core.<> Core.toText pipelineName Core.<>
                             "/reprocessing/"
                             Core.<> Core.toText reprocessingId,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 CancelPipelineReprocessingResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCancelPipelineReprocessingResponse' smart constructor.
newtype CancelPipelineReprocessingResponse = CancelPipelineReprocessingResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'CancelPipelineReprocessingResponse' value with any optional fields omitted.
mkCancelPipelineReprocessingResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CancelPipelineReprocessingResponse
mkCancelPipelineReprocessingResponse responseStatus
  = CancelPipelineReprocessingResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cprrrsResponseStatus :: Lens.Lens' CancelPipelineReprocessingResponse Core.Int
cprrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE cprrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
