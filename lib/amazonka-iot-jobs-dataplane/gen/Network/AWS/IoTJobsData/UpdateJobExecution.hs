{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTJobsData.UpdateJobExecution
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the status of a job execution.
module Network.AWS.IoTJobsData.UpdateJobExecution
    (
    -- * Creating a request
      UpdateJobExecution (..)
    , mkUpdateJobExecution
    -- ** Request lenses
    , ujeJobId
    , ujeThingName
    , ujeStatus
    , ujeExecutionNumber
    , ujeExpectedVersion
    , ujeIncludeJobDocument
    , ujeIncludeJobExecutionState
    , ujeStatusDetails
    , ujeStepTimeoutInMinutes

    -- * Destructuring the response
    , UpdateJobExecutionResponse (..)
    , mkUpdateJobExecutionResponse
    -- ** Response lenses
    , ujerrsExecutionState
    , ujerrsJobDocument
    , ujerrsResponseStatus
    ) where

import qualified Network.AWS.IoTJobsData.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateJobExecution' smart constructor.
data UpdateJobExecution = UpdateJobExecution'
  { jobId :: Types.JobId
    -- ^ The unique identifier assigned to this job when it was created.
  , thingName :: Types.ThingName
    -- ^ The name of the thing associated with the device.
  , status :: Types.JobExecutionStatus
    -- ^ The new status for the job execution (IN_PROGRESS, FAILED, SUCCESS, or REJECTED). This must be specified on every update.
  , executionNumber :: Core.Maybe Core.Integer
    -- ^ Optional. A number that identifies a particular job execution on a particular device.
  , expectedVersion :: Core.Maybe Core.Integer
    -- ^ Optional. The expected current version of the job execution. Each time you update the job execution, its version is incremented. If the version of the job execution stored in Jobs does not match, the update is rejected with a VersionMismatch error, and an ErrorResponse that contains the current job execution status data is returned. (This makes it unnecessary to perform a separate DescribeJobExecution request in order to obtain the job execution status data.)
  , includeJobDocument :: Core.Maybe Core.Bool
    -- ^ Optional. When set to true, the response contains the job document. The default is false.
  , includeJobExecutionState :: Core.Maybe Core.Bool
    -- ^ Optional. When included and set to true, the response contains the JobExecutionState data. The default is false.
  , statusDetails :: Core.Maybe (Core.HashMap Types.DetailsKey Types.DetailsValue)
    -- ^ Optional. A collection of name/value pairs that describe the status of the job execution. If not specified, the statusDetails are unchanged.
  , stepTimeoutInMinutes :: Core.Maybe Core.Integer
    -- ^ Specifies the amount of time this device has to finish execution of this job. If the job execution status is not set to a terminal state before this timer expires, or before the timer is reset (by again calling @UpdateJobExecution@ , setting the status to @IN_PROGRESS@ and specifying a new timeout value in this field) the job execution status will be automatically set to @TIMED_OUT@ . Note that setting or resetting this timeout has no effect on that job execution timeout which may have been specified when the job was created (@CreateJob@ using field @timeoutConfig@ ).
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateJobExecution' value with any optional fields omitted.
mkUpdateJobExecution
    :: Types.JobId -- ^ 'jobId'
    -> Types.ThingName -- ^ 'thingName'
    -> Types.JobExecutionStatus -- ^ 'status'
    -> UpdateJobExecution
mkUpdateJobExecution jobId thingName status
  = UpdateJobExecution'{jobId, thingName, status,
                        executionNumber = Core.Nothing, expectedVersion = Core.Nothing,
                        includeJobDocument = Core.Nothing,
                        includeJobExecutionState = Core.Nothing,
                        statusDetails = Core.Nothing, stepTimeoutInMinutes = Core.Nothing}

-- | The unique identifier assigned to this job when it was created.
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ujeJobId :: Lens.Lens' UpdateJobExecution Types.JobId
ujeJobId = Lens.field @"jobId"
{-# INLINEABLE ujeJobId #-}
{-# DEPRECATED jobId "Use generic-lens or generic-optics with 'jobId' instead"  #-}

-- | The name of the thing associated with the device.
--
-- /Note:/ Consider using 'thingName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ujeThingName :: Lens.Lens' UpdateJobExecution Types.ThingName
ujeThingName = Lens.field @"thingName"
{-# INLINEABLE ujeThingName #-}
{-# DEPRECATED thingName "Use generic-lens or generic-optics with 'thingName' instead"  #-}

-- | The new status for the job execution (IN_PROGRESS, FAILED, SUCCESS, or REJECTED). This must be specified on every update.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ujeStatus :: Lens.Lens' UpdateJobExecution Types.JobExecutionStatus
ujeStatus = Lens.field @"status"
{-# INLINEABLE ujeStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

-- | Optional. A number that identifies a particular job execution on a particular device.
--
-- /Note:/ Consider using 'executionNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ujeExecutionNumber :: Lens.Lens' UpdateJobExecution (Core.Maybe Core.Integer)
ujeExecutionNumber = Lens.field @"executionNumber"
{-# INLINEABLE ujeExecutionNumber #-}
{-# DEPRECATED executionNumber "Use generic-lens or generic-optics with 'executionNumber' instead"  #-}

-- | Optional. The expected current version of the job execution. Each time you update the job execution, its version is incremented. If the version of the job execution stored in Jobs does not match, the update is rejected with a VersionMismatch error, and an ErrorResponse that contains the current job execution status data is returned. (This makes it unnecessary to perform a separate DescribeJobExecution request in order to obtain the job execution status data.)
--
-- /Note:/ Consider using 'expectedVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ujeExpectedVersion :: Lens.Lens' UpdateJobExecution (Core.Maybe Core.Integer)
ujeExpectedVersion = Lens.field @"expectedVersion"
{-# INLINEABLE ujeExpectedVersion #-}
{-# DEPRECATED expectedVersion "Use generic-lens or generic-optics with 'expectedVersion' instead"  #-}

-- | Optional. When set to true, the response contains the job document. The default is false.
--
-- /Note:/ Consider using 'includeJobDocument' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ujeIncludeJobDocument :: Lens.Lens' UpdateJobExecution (Core.Maybe Core.Bool)
ujeIncludeJobDocument = Lens.field @"includeJobDocument"
{-# INLINEABLE ujeIncludeJobDocument #-}
{-# DEPRECATED includeJobDocument "Use generic-lens or generic-optics with 'includeJobDocument' instead"  #-}

-- | Optional. When included and set to true, the response contains the JobExecutionState data. The default is false.
--
-- /Note:/ Consider using 'includeJobExecutionState' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ujeIncludeJobExecutionState :: Lens.Lens' UpdateJobExecution (Core.Maybe Core.Bool)
ujeIncludeJobExecutionState = Lens.field @"includeJobExecutionState"
{-# INLINEABLE ujeIncludeJobExecutionState #-}
{-# DEPRECATED includeJobExecutionState "Use generic-lens or generic-optics with 'includeJobExecutionState' instead"  #-}

-- | Optional. A collection of name/value pairs that describe the status of the job execution. If not specified, the statusDetails are unchanged.
--
-- /Note:/ Consider using 'statusDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ujeStatusDetails :: Lens.Lens' UpdateJobExecution (Core.Maybe (Core.HashMap Types.DetailsKey Types.DetailsValue))
ujeStatusDetails = Lens.field @"statusDetails"
{-# INLINEABLE ujeStatusDetails #-}
{-# DEPRECATED statusDetails "Use generic-lens or generic-optics with 'statusDetails' instead"  #-}

-- | Specifies the amount of time this device has to finish execution of this job. If the job execution status is not set to a terminal state before this timer expires, or before the timer is reset (by again calling @UpdateJobExecution@ , setting the status to @IN_PROGRESS@ and specifying a new timeout value in this field) the job execution status will be automatically set to @TIMED_OUT@ . Note that setting or resetting this timeout has no effect on that job execution timeout which may have been specified when the job was created (@CreateJob@ using field @timeoutConfig@ ).
--
-- /Note:/ Consider using 'stepTimeoutInMinutes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ujeStepTimeoutInMinutes :: Lens.Lens' UpdateJobExecution (Core.Maybe Core.Integer)
ujeStepTimeoutInMinutes = Lens.field @"stepTimeoutInMinutes"
{-# INLINEABLE ujeStepTimeoutInMinutes #-}
{-# DEPRECATED stepTimeoutInMinutes "Use generic-lens or generic-optics with 'stepTimeoutInMinutes' instead"  #-}

instance Core.ToQuery UpdateJobExecution where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateJobExecution where
        toHeaders _ = Core.pure Core.mempty

instance Core.FromJSON UpdateJobExecution where
        toJSON UpdateJobExecution{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("status" Core..= status),
                  ("executionNumber" Core..=) Core.<$> executionNumber,
                  ("expectedVersion" Core..=) Core.<$> expectedVersion,
                  ("includeJobDocument" Core..=) Core.<$> includeJobDocument,
                  ("includeJobExecutionState" Core..=) Core.<$>
                    includeJobExecutionState,
                  ("statusDetails" Core..=) Core.<$> statusDetails,
                  ("stepTimeoutInMinutes" Core..=) Core.<$> stepTimeoutInMinutes])

instance Core.AWSRequest UpdateJobExecution where
        type Rs UpdateJobExecution = UpdateJobExecutionResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST,
                         Core._rqPath =
                           "/things/" Core.<> Core.toText thingName Core.<> "/jobs/" Core.<>
                             Core.toText jobId,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 UpdateJobExecutionResponse' Core.<$>
                   (x Core..:? "executionState") Core.<*> x Core..:? "jobDocument"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkUpdateJobExecutionResponse' smart constructor.
data UpdateJobExecutionResponse = UpdateJobExecutionResponse'
  { executionState :: Core.Maybe Types.JobExecutionState
    -- ^ A JobExecutionState object.
  , jobDocument :: Core.Maybe Types.JobDocument
    -- ^ The contents of the Job Documents.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateJobExecutionResponse' value with any optional fields omitted.
mkUpdateJobExecutionResponse
    :: Core.Int -- ^ 'responseStatus'
    -> UpdateJobExecutionResponse
mkUpdateJobExecutionResponse responseStatus
  = UpdateJobExecutionResponse'{executionState = Core.Nothing,
                                jobDocument = Core.Nothing, responseStatus}

-- | A JobExecutionState object.
--
-- /Note:/ Consider using 'executionState' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ujerrsExecutionState :: Lens.Lens' UpdateJobExecutionResponse (Core.Maybe Types.JobExecutionState)
ujerrsExecutionState = Lens.field @"executionState"
{-# INLINEABLE ujerrsExecutionState #-}
{-# DEPRECATED executionState "Use generic-lens or generic-optics with 'executionState' instead"  #-}

-- | The contents of the Job Documents.
--
-- /Note:/ Consider using 'jobDocument' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ujerrsJobDocument :: Lens.Lens' UpdateJobExecutionResponse (Core.Maybe Types.JobDocument)
ujerrsJobDocument = Lens.field @"jobDocument"
{-# INLINEABLE ujerrsJobDocument #-}
{-# DEPRECATED jobDocument "Use generic-lens or generic-optics with 'jobDocument' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ujerrsResponseStatus :: Lens.Lens' UpdateJobExecutionResponse Core.Int
ujerrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ujerrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
