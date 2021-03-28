{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticTranscoder.UpdatePipelineStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The UpdatePipelineStatus operation pauses or reactivates a pipeline, so that the pipeline stops or restarts the processing of jobs.
--
-- Changing the pipeline status is useful if you want to cancel one or more jobs. You can't cancel jobs after Elastic Transcoder has started processing them; if you pause the pipeline to which you submitted the jobs, you have more time to get the job IDs for the jobs that you want to cancel, and to send a 'CancelJob' request. 
module Network.AWS.ElasticTranscoder.UpdatePipelineStatus
    (
    -- * Creating a request
      UpdatePipelineStatus (..)
    , mkUpdatePipelineStatus
    -- ** Request lenses
    , upsId
    , upsStatus

    -- * Destructuring the response
    , UpdatePipelineStatusResponse (..)
    , mkUpdatePipelineStatusResponse
    -- ** Response lenses
    , upsrrsPipeline
    , upsrrsResponseStatus
    ) where

import qualified Network.AWS.ElasticTranscoder.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The @UpdatePipelineStatusRequest@ structure.
--
-- /See:/ 'mkUpdatePipelineStatus' smart constructor.
data UpdatePipelineStatus = UpdatePipelineStatus'
  { id :: Types.Id
    -- ^ The identifier of the pipeline to update.
  , status :: Types.PipelineStatus
    -- ^ The desired status of the pipeline:
--
--
--     * @Active@ : The pipeline is processing jobs.
--
--
--     * @Paused@ : The pipeline is not currently processing jobs.
--
--
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdatePipelineStatus' value with any optional fields omitted.
mkUpdatePipelineStatus
    :: Types.Id -- ^ 'id'
    -> Types.PipelineStatus -- ^ 'status'
    -> UpdatePipelineStatus
mkUpdatePipelineStatus id status
  = UpdatePipelineStatus'{id, status}

-- | The identifier of the pipeline to update.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upsId :: Lens.Lens' UpdatePipelineStatus Types.Id
upsId = Lens.field @"id"
{-# INLINEABLE upsId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

-- | The desired status of the pipeline:
--
--
--     * @Active@ : The pipeline is processing jobs.
--
--
--     * @Paused@ : The pipeline is not currently processing jobs.
--
--
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upsStatus :: Lens.Lens' UpdatePipelineStatus Types.PipelineStatus
upsStatus = Lens.field @"status"
{-# INLINEABLE upsStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

instance Core.ToQuery UpdatePipelineStatus where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdatePipelineStatus where
        toHeaders _ = Core.pure Core.mempty

instance Core.FromJSON UpdatePipelineStatus where
        toJSON UpdatePipelineStatus{..}
          = Core.object
              (Core.catMaybes [Core.Just ("Status" Core..= status)])

instance Core.AWSRequest UpdatePipelineStatus where
        type Rs UpdatePipelineStatus = UpdatePipelineStatusResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST,
                         Core._rqPath =
                           "/2012-09-25/pipelines/" Core.<> Core.toText id Core.<> "/status",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 UpdatePipelineStatusResponse' Core.<$>
                   (x Core..:? "Pipeline") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | When you update status for a pipeline, Elastic Transcoder returns the values that you specified in the request.
--
-- /See:/ 'mkUpdatePipelineStatusResponse' smart constructor.
data UpdatePipelineStatusResponse = UpdatePipelineStatusResponse'
  { pipeline :: Core.Maybe Types.Pipeline
    -- ^ A section of the response body that provides information about the pipeline.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdatePipelineStatusResponse' value with any optional fields omitted.
mkUpdatePipelineStatusResponse
    :: Core.Int -- ^ 'responseStatus'
    -> UpdatePipelineStatusResponse
mkUpdatePipelineStatusResponse responseStatus
  = UpdatePipelineStatusResponse'{pipeline = Core.Nothing,
                                  responseStatus}

-- | A section of the response body that provides information about the pipeline.
--
-- /Note:/ Consider using 'pipeline' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upsrrsPipeline :: Lens.Lens' UpdatePipelineStatusResponse (Core.Maybe Types.Pipeline)
upsrrsPipeline = Lens.field @"pipeline"
{-# INLINEABLE upsrrsPipeline #-}
{-# DEPRECATED pipeline "Use generic-lens or generic-optics with 'pipeline' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upsrrsResponseStatus :: Lens.Lens' UpdatePipelineStatusResponse Core.Int
upsrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE upsrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
