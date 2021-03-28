{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DataPipeline.DeactivatePipeline
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deactivates the specified running pipeline. The pipeline is set to the @DEACTIVATING@ state until the deactivation process completes.
--
-- To resume a deactivated pipeline, use 'ActivatePipeline' . By default, the pipeline resumes from the last completed execution. Optionally, you can specify the date and time to resume the pipeline.
module Network.AWS.DataPipeline.DeactivatePipeline
    (
    -- * Creating a request
      DeactivatePipeline (..)
    , mkDeactivatePipeline
    -- ** Request lenses
    , dPipelineId
    , dCancelActive

    -- * Destructuring the response
    , DeactivatePipelineResponse (..)
    , mkDeactivatePipelineResponse
    -- ** Response lenses
    , drsResponseStatus
    ) where

import qualified Network.AWS.DataPipeline.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for DeactivatePipeline.
--
-- /See:/ 'mkDeactivatePipeline' smart constructor.
data DeactivatePipeline = DeactivatePipeline'
  { pipelineId :: Types.PipelineId
    -- ^ The ID of the pipeline.
  , cancelActive :: Core.Maybe Core.Bool
    -- ^ Indicates whether to cancel any running objects. The default is true, which sets the state of any running objects to @CANCELED@ . If this value is false, the pipeline is deactivated after all running objects finish.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeactivatePipeline' value with any optional fields omitted.
mkDeactivatePipeline
    :: Types.PipelineId -- ^ 'pipelineId'
    -> DeactivatePipeline
mkDeactivatePipeline pipelineId
  = DeactivatePipeline'{pipelineId, cancelActive = Core.Nothing}

-- | The ID of the pipeline.
--
-- /Note:/ Consider using 'pipelineId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dPipelineId :: Lens.Lens' DeactivatePipeline Types.PipelineId
dPipelineId = Lens.field @"pipelineId"
{-# INLINEABLE dPipelineId #-}
{-# DEPRECATED pipelineId "Use generic-lens or generic-optics with 'pipelineId' instead"  #-}

-- | Indicates whether to cancel any running objects. The default is true, which sets the state of any running objects to @CANCELED@ . If this value is false, the pipeline is deactivated after all running objects finish.
--
-- /Note:/ Consider using 'cancelActive' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dCancelActive :: Lens.Lens' DeactivatePipeline (Core.Maybe Core.Bool)
dCancelActive = Lens.field @"cancelActive"
{-# INLINEABLE dCancelActive #-}
{-# DEPRECATED cancelActive "Use generic-lens or generic-optics with 'cancelActive' instead"  #-}

instance Core.ToQuery DeactivatePipeline where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeactivatePipeline where
        toHeaders DeactivatePipeline{..}
          = Core.pure ("X-Amz-Target", "DataPipeline.DeactivatePipeline")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeactivatePipeline where
        toJSON DeactivatePipeline{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("pipelineId" Core..= pipelineId),
                  ("cancelActive" Core..=) Core.<$> cancelActive])

instance Core.AWSRequest DeactivatePipeline where
        type Rs DeactivatePipeline = DeactivatePipelineResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 DeactivatePipelineResponse' Core.<$> (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | Contains the output of DeactivatePipeline.
--
-- /See:/ 'mkDeactivatePipelineResponse' smart constructor.
newtype DeactivatePipelineResponse = DeactivatePipelineResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeactivatePipelineResponse' value with any optional fields omitted.
mkDeactivatePipelineResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeactivatePipelineResponse
mkDeactivatePipelineResponse responseStatus
  = DeactivatePipelineResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsResponseStatus :: Lens.Lens' DeactivatePipelineResponse Core.Int
drsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE drsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
