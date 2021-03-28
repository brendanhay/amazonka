{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.CancelMLTaskRun
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels (stops) a task run. Machine learning task runs are asynchronous tasks that AWS Glue runs on your behalf as part of various machine learning workflows. You can cancel a machine learning task run at any time by calling @CancelMLTaskRun@ with a task run's parent transform's @TransformID@ and the task run's @TaskRunId@ . 
module Network.AWS.Glue.CancelMLTaskRun
    (
    -- * Creating a request
      CancelMLTaskRun (..)
    , mkCancelMLTaskRun
    -- ** Request lenses
    , cmltrTransformId
    , cmltrTaskRunId

    -- * Destructuring the response
    , CancelMLTaskRunResponse (..)
    , mkCancelMLTaskRunResponse
    -- ** Response lenses
    , cmltrrrsStatus
    , cmltrrrsTaskRunId
    , cmltrrrsTransformId
    , cmltrrrsResponseStatus
    ) where

import qualified Network.AWS.Glue.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCancelMLTaskRun' smart constructor.
data CancelMLTaskRun = CancelMLTaskRun'
  { transformId :: Types.HashString
    -- ^ The unique identifier of the machine learning transform.
  , taskRunId :: Types.HashString
    -- ^ A unique identifier for the task run.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CancelMLTaskRun' value with any optional fields omitted.
mkCancelMLTaskRun
    :: Types.HashString -- ^ 'transformId'
    -> Types.HashString -- ^ 'taskRunId'
    -> CancelMLTaskRun
mkCancelMLTaskRun transformId taskRunId
  = CancelMLTaskRun'{transformId, taskRunId}

-- | The unique identifier of the machine learning transform.
--
-- /Note:/ Consider using 'transformId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmltrTransformId :: Lens.Lens' CancelMLTaskRun Types.HashString
cmltrTransformId = Lens.field @"transformId"
{-# INLINEABLE cmltrTransformId #-}
{-# DEPRECATED transformId "Use generic-lens or generic-optics with 'transformId' instead"  #-}

-- | A unique identifier for the task run.
--
-- /Note:/ Consider using 'taskRunId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmltrTaskRunId :: Lens.Lens' CancelMLTaskRun Types.HashString
cmltrTaskRunId = Lens.field @"taskRunId"
{-# INLINEABLE cmltrTaskRunId #-}
{-# DEPRECATED taskRunId "Use generic-lens or generic-optics with 'taskRunId' instead"  #-}

instance Core.ToQuery CancelMLTaskRun where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CancelMLTaskRun where
        toHeaders CancelMLTaskRun{..}
          = Core.pure ("X-Amz-Target", "AWSGlue.CancelMLTaskRun") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CancelMLTaskRun where
        toJSON CancelMLTaskRun{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("TransformId" Core..= transformId),
                  Core.Just ("TaskRunId" Core..= taskRunId)])

instance Core.AWSRequest CancelMLTaskRun where
        type Rs CancelMLTaskRun = CancelMLTaskRunResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CancelMLTaskRunResponse' Core.<$>
                   (x Core..:? "Status") Core.<*> x Core..:? "TaskRunId" Core.<*>
                     x Core..:? "TransformId"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCancelMLTaskRunResponse' smart constructor.
data CancelMLTaskRunResponse = CancelMLTaskRunResponse'
  { status :: Core.Maybe Types.TaskStatusType
    -- ^ The status for this run.
  , taskRunId :: Core.Maybe Types.HashString
    -- ^ The unique identifier for the task run.
  , transformId :: Core.Maybe Types.HashString
    -- ^ The unique identifier of the machine learning transform.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CancelMLTaskRunResponse' value with any optional fields omitted.
mkCancelMLTaskRunResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CancelMLTaskRunResponse
mkCancelMLTaskRunResponse responseStatus
  = CancelMLTaskRunResponse'{status = Core.Nothing,
                             taskRunId = Core.Nothing, transformId = Core.Nothing,
                             responseStatus}

-- | The status for this run.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmltrrrsStatus :: Lens.Lens' CancelMLTaskRunResponse (Core.Maybe Types.TaskStatusType)
cmltrrrsStatus = Lens.field @"status"
{-# INLINEABLE cmltrrrsStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

-- | The unique identifier for the task run.
--
-- /Note:/ Consider using 'taskRunId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmltrrrsTaskRunId :: Lens.Lens' CancelMLTaskRunResponse (Core.Maybe Types.HashString)
cmltrrrsTaskRunId = Lens.field @"taskRunId"
{-# INLINEABLE cmltrrrsTaskRunId #-}
{-# DEPRECATED taskRunId "Use generic-lens or generic-optics with 'taskRunId' instead"  #-}

-- | The unique identifier of the machine learning transform.
--
-- /Note:/ Consider using 'transformId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmltrrrsTransformId :: Lens.Lens' CancelMLTaskRunResponse (Core.Maybe Types.HashString)
cmltrrrsTransformId = Lens.field @"transformId"
{-# INLINEABLE cmltrrrsTransformId #-}
{-# DEPRECATED transformId "Use generic-lens or generic-optics with 'transformId' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmltrrrsResponseStatus :: Lens.Lens' CancelMLTaskRunResponse Core.Int
cmltrrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE cmltrrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
