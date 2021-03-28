{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.StartMLEvaluationTaskRun
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts a task to estimate the quality of the transform. 
--
-- When you provide label sets as examples of truth, AWS Glue machine learning uses some of those examples to learn from them. The rest of the labels are used as a test to estimate quality.
-- Returns a unique identifier for the run. You can call @GetMLTaskRun@ to get more information about the stats of the @EvaluationTaskRun@ .
module Network.AWS.Glue.StartMLEvaluationTaskRun
    (
    -- * Creating a request
      StartMLEvaluationTaskRun (..)
    , mkStartMLEvaluationTaskRun
    -- ** Request lenses
    , smletrTransformId

    -- * Destructuring the response
    , StartMLEvaluationTaskRunResponse (..)
    , mkStartMLEvaluationTaskRunResponse
    -- ** Response lenses
    , smletrrrsTaskRunId
    , smletrrrsResponseStatus
    ) where

import qualified Network.AWS.Glue.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkStartMLEvaluationTaskRun' smart constructor.
newtype StartMLEvaluationTaskRun = StartMLEvaluationTaskRun'
  { transformId :: Types.HashString
    -- ^ The unique identifier of the machine learning transform.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'StartMLEvaluationTaskRun' value with any optional fields omitted.
mkStartMLEvaluationTaskRun
    :: Types.HashString -- ^ 'transformId'
    -> StartMLEvaluationTaskRun
mkStartMLEvaluationTaskRun transformId
  = StartMLEvaluationTaskRun'{transformId}

-- | The unique identifier of the machine learning transform.
--
-- /Note:/ Consider using 'transformId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smletrTransformId :: Lens.Lens' StartMLEvaluationTaskRun Types.HashString
smletrTransformId = Lens.field @"transformId"
{-# INLINEABLE smletrTransformId #-}
{-# DEPRECATED transformId "Use generic-lens or generic-optics with 'transformId' instead"  #-}

instance Core.ToQuery StartMLEvaluationTaskRun where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders StartMLEvaluationTaskRun where
        toHeaders StartMLEvaluationTaskRun{..}
          = Core.pure ("X-Amz-Target", "AWSGlue.StartMLEvaluationTaskRun")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON StartMLEvaluationTaskRun where
        toJSON StartMLEvaluationTaskRun{..}
          = Core.object
              (Core.catMaybes [Core.Just ("TransformId" Core..= transformId)])

instance Core.AWSRequest StartMLEvaluationTaskRun where
        type Rs StartMLEvaluationTaskRun = StartMLEvaluationTaskRunResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 StartMLEvaluationTaskRunResponse' Core.<$>
                   (x Core..:? "TaskRunId") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkStartMLEvaluationTaskRunResponse' smart constructor.
data StartMLEvaluationTaskRunResponse = StartMLEvaluationTaskRunResponse'
  { taskRunId :: Core.Maybe Types.HashString
    -- ^ The unique identifier associated with this run.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StartMLEvaluationTaskRunResponse' value with any optional fields omitted.
mkStartMLEvaluationTaskRunResponse
    :: Core.Int -- ^ 'responseStatus'
    -> StartMLEvaluationTaskRunResponse
mkStartMLEvaluationTaskRunResponse responseStatus
  = StartMLEvaluationTaskRunResponse'{taskRunId = Core.Nothing,
                                      responseStatus}

-- | The unique identifier associated with this run.
--
-- /Note:/ Consider using 'taskRunId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smletrrrsTaskRunId :: Lens.Lens' StartMLEvaluationTaskRunResponse (Core.Maybe Types.HashString)
smletrrrsTaskRunId = Lens.field @"taskRunId"
{-# INLINEABLE smletrrrsTaskRunId #-}
{-# DEPRECATED taskRunId "Use generic-lens or generic-optics with 'taskRunId' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smletrrrsResponseStatus :: Lens.Lens' StartMLEvaluationTaskRunResponse Core.Int
smletrrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE smletrrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
