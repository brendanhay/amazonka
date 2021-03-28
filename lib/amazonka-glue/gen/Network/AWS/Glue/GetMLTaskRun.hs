{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.GetMLTaskRun
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets details for a specific task run on a machine learning transform. Machine learning task runs are asynchronous tasks that AWS Glue runs on your behalf as part of various machine learning workflows. You can check the stats of any task run by calling @GetMLTaskRun@ with the @TaskRunID@ and its parent transform's @TransformID@ .
module Network.AWS.Glue.GetMLTaskRun
    (
    -- * Creating a request
      GetMLTaskRun (..)
    , mkGetMLTaskRun
    -- ** Request lenses
    , gTransformId
    , gTaskRunId

    -- * Destructuring the response
    , GetMLTaskRunResponse (..)
    , mkGetMLTaskRunResponse
    -- ** Response lenses
    , gmltrrrsCompletedOn
    , gmltrrrsErrorString
    , gmltrrrsExecutionTime
    , gmltrrrsLastModifiedOn
    , gmltrrrsLogGroupName
    , gmltrrrsProperties
    , gmltrrrsStartedOn
    , gmltrrrsStatus
    , gmltrrrsTaskRunId
    , gmltrrrsTransformId
    , gmltrrrsResponseStatus
    ) where

import qualified Network.AWS.Glue.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetMLTaskRun' smart constructor.
data GetMLTaskRun = GetMLTaskRun'
  { transformId :: Types.HashString
    -- ^ The unique identifier of the machine learning transform.
  , taskRunId :: Types.HashString
    -- ^ The unique identifier of the task run.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetMLTaskRun' value with any optional fields omitted.
mkGetMLTaskRun
    :: Types.HashString -- ^ 'transformId'
    -> Types.HashString -- ^ 'taskRunId'
    -> GetMLTaskRun
mkGetMLTaskRun transformId taskRunId
  = GetMLTaskRun'{transformId, taskRunId}

-- | The unique identifier of the machine learning transform.
--
-- /Note:/ Consider using 'transformId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gTransformId :: Lens.Lens' GetMLTaskRun Types.HashString
gTransformId = Lens.field @"transformId"
{-# INLINEABLE gTransformId #-}
{-# DEPRECATED transformId "Use generic-lens or generic-optics with 'transformId' instead"  #-}

-- | The unique identifier of the task run.
--
-- /Note:/ Consider using 'taskRunId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gTaskRunId :: Lens.Lens' GetMLTaskRun Types.HashString
gTaskRunId = Lens.field @"taskRunId"
{-# INLINEABLE gTaskRunId #-}
{-# DEPRECATED taskRunId "Use generic-lens or generic-optics with 'taskRunId' instead"  #-}

instance Core.ToQuery GetMLTaskRun where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetMLTaskRun where
        toHeaders GetMLTaskRun{..}
          = Core.pure ("X-Amz-Target", "AWSGlue.GetMLTaskRun") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetMLTaskRun where
        toJSON GetMLTaskRun{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("TransformId" Core..= transformId),
                  Core.Just ("TaskRunId" Core..= taskRunId)])

instance Core.AWSRequest GetMLTaskRun where
        type Rs GetMLTaskRun = GetMLTaskRunResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetMLTaskRunResponse' Core.<$>
                   (x Core..:? "CompletedOn") Core.<*> x Core..:? "ErrorString"
                     Core.<*> x Core..:? "ExecutionTime"
                     Core.<*> x Core..:? "LastModifiedOn"
                     Core.<*> x Core..:? "LogGroupName"
                     Core.<*> x Core..:? "Properties"
                     Core.<*> x Core..:? "StartedOn"
                     Core.<*> x Core..:? "Status"
                     Core.<*> x Core..:? "TaskRunId"
                     Core.<*> x Core..:? "TransformId"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetMLTaskRunResponse' smart constructor.
data GetMLTaskRunResponse = GetMLTaskRunResponse'
  { completedOn :: Core.Maybe Core.NominalDiffTime
    -- ^ The date and time when this task run was completed.
  , errorString :: Core.Maybe Types.GenericString
    -- ^ The error strings that are associated with the task run.
  , executionTime :: Core.Maybe Core.Int
    -- ^ The amount of time (in seconds) that the task run consumed resources.
  , lastModifiedOn :: Core.Maybe Core.NominalDiffTime
    -- ^ The date and time when this task run was last modified.
  , logGroupName :: Core.Maybe Types.GenericString
    -- ^ The names of the log groups that are associated with the task run.
  , properties :: Core.Maybe Types.TaskRunProperties
    -- ^ The list of properties that are associated with the task run.
  , startedOn :: Core.Maybe Core.NominalDiffTime
    -- ^ The date and time when this task run started.
  , status :: Core.Maybe Types.TaskStatusType
    -- ^ The status for this task run.
  , taskRunId :: Core.Maybe Types.HashString
    -- ^ The unique run identifier associated with this run.
  , transformId :: Core.Maybe Types.HashString
    -- ^ The unique identifier of the task run.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'GetMLTaskRunResponse' value with any optional fields omitted.
mkGetMLTaskRunResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetMLTaskRunResponse
mkGetMLTaskRunResponse responseStatus
  = GetMLTaskRunResponse'{completedOn = Core.Nothing,
                          errorString = Core.Nothing, executionTime = Core.Nothing,
                          lastModifiedOn = Core.Nothing, logGroupName = Core.Nothing,
                          properties = Core.Nothing, startedOn = Core.Nothing,
                          status = Core.Nothing, taskRunId = Core.Nothing,
                          transformId = Core.Nothing, responseStatus}

-- | The date and time when this task run was completed.
--
-- /Note:/ Consider using 'completedOn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmltrrrsCompletedOn :: Lens.Lens' GetMLTaskRunResponse (Core.Maybe Core.NominalDiffTime)
gmltrrrsCompletedOn = Lens.field @"completedOn"
{-# INLINEABLE gmltrrrsCompletedOn #-}
{-# DEPRECATED completedOn "Use generic-lens or generic-optics with 'completedOn' instead"  #-}

-- | The error strings that are associated with the task run.
--
-- /Note:/ Consider using 'errorString' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmltrrrsErrorString :: Lens.Lens' GetMLTaskRunResponse (Core.Maybe Types.GenericString)
gmltrrrsErrorString = Lens.field @"errorString"
{-# INLINEABLE gmltrrrsErrorString #-}
{-# DEPRECATED errorString "Use generic-lens or generic-optics with 'errorString' instead"  #-}

-- | The amount of time (in seconds) that the task run consumed resources.
--
-- /Note:/ Consider using 'executionTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmltrrrsExecutionTime :: Lens.Lens' GetMLTaskRunResponse (Core.Maybe Core.Int)
gmltrrrsExecutionTime = Lens.field @"executionTime"
{-# INLINEABLE gmltrrrsExecutionTime #-}
{-# DEPRECATED executionTime "Use generic-lens or generic-optics with 'executionTime' instead"  #-}

-- | The date and time when this task run was last modified.
--
-- /Note:/ Consider using 'lastModifiedOn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmltrrrsLastModifiedOn :: Lens.Lens' GetMLTaskRunResponse (Core.Maybe Core.NominalDiffTime)
gmltrrrsLastModifiedOn = Lens.field @"lastModifiedOn"
{-# INLINEABLE gmltrrrsLastModifiedOn #-}
{-# DEPRECATED lastModifiedOn "Use generic-lens or generic-optics with 'lastModifiedOn' instead"  #-}

-- | The names of the log groups that are associated with the task run.
--
-- /Note:/ Consider using 'logGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmltrrrsLogGroupName :: Lens.Lens' GetMLTaskRunResponse (Core.Maybe Types.GenericString)
gmltrrrsLogGroupName = Lens.field @"logGroupName"
{-# INLINEABLE gmltrrrsLogGroupName #-}
{-# DEPRECATED logGroupName "Use generic-lens or generic-optics with 'logGroupName' instead"  #-}

-- | The list of properties that are associated with the task run.
--
-- /Note:/ Consider using 'properties' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmltrrrsProperties :: Lens.Lens' GetMLTaskRunResponse (Core.Maybe Types.TaskRunProperties)
gmltrrrsProperties = Lens.field @"properties"
{-# INLINEABLE gmltrrrsProperties #-}
{-# DEPRECATED properties "Use generic-lens or generic-optics with 'properties' instead"  #-}

-- | The date and time when this task run started.
--
-- /Note:/ Consider using 'startedOn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmltrrrsStartedOn :: Lens.Lens' GetMLTaskRunResponse (Core.Maybe Core.NominalDiffTime)
gmltrrrsStartedOn = Lens.field @"startedOn"
{-# INLINEABLE gmltrrrsStartedOn #-}
{-# DEPRECATED startedOn "Use generic-lens or generic-optics with 'startedOn' instead"  #-}

-- | The status for this task run.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmltrrrsStatus :: Lens.Lens' GetMLTaskRunResponse (Core.Maybe Types.TaskStatusType)
gmltrrrsStatus = Lens.field @"status"
{-# INLINEABLE gmltrrrsStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

-- | The unique run identifier associated with this run.
--
-- /Note:/ Consider using 'taskRunId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmltrrrsTaskRunId :: Lens.Lens' GetMLTaskRunResponse (Core.Maybe Types.HashString)
gmltrrrsTaskRunId = Lens.field @"taskRunId"
{-# INLINEABLE gmltrrrsTaskRunId #-}
{-# DEPRECATED taskRunId "Use generic-lens or generic-optics with 'taskRunId' instead"  #-}

-- | The unique identifier of the task run.
--
-- /Note:/ Consider using 'transformId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmltrrrsTransformId :: Lens.Lens' GetMLTaskRunResponse (Core.Maybe Types.HashString)
gmltrrrsTransformId = Lens.field @"transformId"
{-# INLINEABLE gmltrrrsTransformId #-}
{-# DEPRECATED transformId "Use generic-lens or generic-optics with 'transformId' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmltrrrsResponseStatus :: Lens.Lens' GetMLTaskRunResponse Core.Int
gmltrrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gmltrrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
