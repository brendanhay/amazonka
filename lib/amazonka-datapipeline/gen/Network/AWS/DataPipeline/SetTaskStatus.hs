{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DataPipeline.SetTaskStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Task runners call @SetTaskStatus@ to notify AWS Data Pipeline that a task is completed and provide information about the final status. A task runner makes this call regardless of whether the task was sucessful. A task runner does not need to call @SetTaskStatus@ for tasks that are canceled by the web service during a call to 'ReportTaskProgress' .
module Network.AWS.DataPipeline.SetTaskStatus
    (
    -- * Creating a request
      SetTaskStatus (..)
    , mkSetTaskStatus
    -- ** Request lenses
    , stsTaskId
    , stsTaskStatus
    , stsErrorId
    , stsErrorMessage
    , stsErrorStackTrace

    -- * Destructuring the response
    , SetTaskStatusResponse (..)
    , mkSetTaskStatusResponse
    -- ** Response lenses
    , stsrrsResponseStatus
    ) where

import qualified Network.AWS.DataPipeline.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for SetTaskStatus.
--
-- /See:/ 'mkSetTaskStatus' smart constructor.
data SetTaskStatus = SetTaskStatus'
  { taskId :: Types.TaskId
    -- ^ The ID of the task assigned to the task runner. This value is provided in the response for 'PollForTask' .
  , taskStatus :: Types.TaskStatus
    -- ^ If @FINISHED@ , the task successfully completed. If @FAILED@ , the task ended unsuccessfully. Preconditions use false.
  , errorId :: Core.Maybe Core.Text
    -- ^ If an error occurred during the task, this value specifies the error code. This value is set on the physical attempt object. It is used to display error information to the user. It should not start with string "Service_" which is reserved by the system.
  , errorMessage :: Core.Maybe Types.ErrorMessage
    -- ^ If an error occurred during the task, this value specifies a text description of the error. This value is set on the physical attempt object. It is used to display error information to the user. The web service does not parse this value.
  , errorStackTrace :: Core.Maybe Core.Text
    -- ^ If an error occurred during the task, this value specifies the stack trace associated with the error. This value is set on the physical attempt object. It is used to display error information to the user. The web service does not parse this value.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SetTaskStatus' value with any optional fields omitted.
mkSetTaskStatus
    :: Types.TaskId -- ^ 'taskId'
    -> Types.TaskStatus -- ^ 'taskStatus'
    -> SetTaskStatus
mkSetTaskStatus taskId taskStatus
  = SetTaskStatus'{taskId, taskStatus, errorId = Core.Nothing,
                   errorMessage = Core.Nothing, errorStackTrace = Core.Nothing}

-- | The ID of the task assigned to the task runner. This value is provided in the response for 'PollForTask' .
--
-- /Note:/ Consider using 'taskId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stsTaskId :: Lens.Lens' SetTaskStatus Types.TaskId
stsTaskId = Lens.field @"taskId"
{-# INLINEABLE stsTaskId #-}
{-# DEPRECATED taskId "Use generic-lens or generic-optics with 'taskId' instead"  #-}

-- | If @FINISHED@ , the task successfully completed. If @FAILED@ , the task ended unsuccessfully. Preconditions use false.
--
-- /Note:/ Consider using 'taskStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stsTaskStatus :: Lens.Lens' SetTaskStatus Types.TaskStatus
stsTaskStatus = Lens.field @"taskStatus"
{-# INLINEABLE stsTaskStatus #-}
{-# DEPRECATED taskStatus "Use generic-lens or generic-optics with 'taskStatus' instead"  #-}

-- | If an error occurred during the task, this value specifies the error code. This value is set on the physical attempt object. It is used to display error information to the user. It should not start with string "Service_" which is reserved by the system.
--
-- /Note:/ Consider using 'errorId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stsErrorId :: Lens.Lens' SetTaskStatus (Core.Maybe Core.Text)
stsErrorId = Lens.field @"errorId"
{-# INLINEABLE stsErrorId #-}
{-# DEPRECATED errorId "Use generic-lens or generic-optics with 'errorId' instead"  #-}

-- | If an error occurred during the task, this value specifies a text description of the error. This value is set on the physical attempt object. It is used to display error information to the user. The web service does not parse this value.
--
-- /Note:/ Consider using 'errorMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stsErrorMessage :: Lens.Lens' SetTaskStatus (Core.Maybe Types.ErrorMessage)
stsErrorMessage = Lens.field @"errorMessage"
{-# INLINEABLE stsErrorMessage #-}
{-# DEPRECATED errorMessage "Use generic-lens or generic-optics with 'errorMessage' instead"  #-}

-- | If an error occurred during the task, this value specifies the stack trace associated with the error. This value is set on the physical attempt object. It is used to display error information to the user. The web service does not parse this value.
--
-- /Note:/ Consider using 'errorStackTrace' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stsErrorStackTrace :: Lens.Lens' SetTaskStatus (Core.Maybe Core.Text)
stsErrorStackTrace = Lens.field @"errorStackTrace"
{-# INLINEABLE stsErrorStackTrace #-}
{-# DEPRECATED errorStackTrace "Use generic-lens or generic-optics with 'errorStackTrace' instead"  #-}

instance Core.ToQuery SetTaskStatus where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders SetTaskStatus where
        toHeaders SetTaskStatus{..}
          = Core.pure ("X-Amz-Target", "DataPipeline.SetTaskStatus") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON SetTaskStatus where
        toJSON SetTaskStatus{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("taskId" Core..= taskId),
                  Core.Just ("taskStatus" Core..= taskStatus),
                  ("errorId" Core..=) Core.<$> errorId,
                  ("errorMessage" Core..=) Core.<$> errorMessage,
                  ("errorStackTrace" Core..=) Core.<$> errorStackTrace])

instance Core.AWSRequest SetTaskStatus where
        type Rs SetTaskStatus = SetTaskStatusResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 SetTaskStatusResponse' Core.<$> (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | Contains the output of SetTaskStatus.
--
-- /See:/ 'mkSetTaskStatusResponse' smart constructor.
newtype SetTaskStatusResponse = SetTaskStatusResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'SetTaskStatusResponse' value with any optional fields omitted.
mkSetTaskStatusResponse
    :: Core.Int -- ^ 'responseStatus'
    -> SetTaskStatusResponse
mkSetTaskStatusResponse responseStatus
  = SetTaskStatusResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stsrrsResponseStatus :: Lens.Lens' SetTaskStatusResponse Core.Int
stsrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE stsrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
