{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTJobsData.StartNextPendingJobExecution
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets and starts the next pending (status IN_PROGRESS or QUEUED) job execution for a thing.
module Network.AWS.IoTJobsData.StartNextPendingJobExecution
    (
    -- * Creating a request
      StartNextPendingJobExecution (..)
    , mkStartNextPendingJobExecution
    -- ** Request lenses
    , snpjeThingName
    , snpjeStatusDetails
    , snpjeStepTimeoutInMinutes

    -- * Destructuring the response
    , StartNextPendingJobExecutionResponse (..)
    , mkStartNextPendingJobExecutionResponse
    -- ** Response lenses
    , snpjerrsExecution
    , snpjerrsResponseStatus
    ) where

import qualified Network.AWS.IoTJobsData.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkStartNextPendingJobExecution' smart constructor.
data StartNextPendingJobExecution = StartNextPendingJobExecution'
  { thingName :: Types.ThingName
    -- ^ The name of the thing associated with the device.
  , statusDetails :: Core.Maybe (Core.HashMap Types.DetailsKey Types.DetailsValue)
    -- ^ A collection of name/value pairs that describe the status of the job execution. If not specified, the statusDetails are unchanged.
  , stepTimeoutInMinutes :: Core.Maybe Core.Integer
    -- ^ Specifies the amount of time this device has to finish execution of this job. If the job execution status is not set to a terminal state before this timer expires, or before the timer is reset (by calling @UpdateJobExecution@ , setting the status to @IN_PROGRESS@ and specifying a new timeout value in field @stepTimeoutInMinutes@ ) the job execution status will be automatically set to @TIMED_OUT@ . Note that setting this timeout has no effect on that job execution timeout which may have been specified when the job was created (@CreateJob@ using field @timeoutConfig@ ).
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StartNextPendingJobExecution' value with any optional fields omitted.
mkStartNextPendingJobExecution
    :: Types.ThingName -- ^ 'thingName'
    -> StartNextPendingJobExecution
mkStartNextPendingJobExecution thingName
  = StartNextPendingJobExecution'{thingName,
                                  statusDetails = Core.Nothing, stepTimeoutInMinutes = Core.Nothing}

-- | The name of the thing associated with the device.
--
-- /Note:/ Consider using 'thingName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
snpjeThingName :: Lens.Lens' StartNextPendingJobExecution Types.ThingName
snpjeThingName = Lens.field @"thingName"
{-# INLINEABLE snpjeThingName #-}
{-# DEPRECATED thingName "Use generic-lens or generic-optics with 'thingName' instead"  #-}

-- | A collection of name/value pairs that describe the status of the job execution. If not specified, the statusDetails are unchanged.
--
-- /Note:/ Consider using 'statusDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
snpjeStatusDetails :: Lens.Lens' StartNextPendingJobExecution (Core.Maybe (Core.HashMap Types.DetailsKey Types.DetailsValue))
snpjeStatusDetails = Lens.field @"statusDetails"
{-# INLINEABLE snpjeStatusDetails #-}
{-# DEPRECATED statusDetails "Use generic-lens or generic-optics with 'statusDetails' instead"  #-}

-- | Specifies the amount of time this device has to finish execution of this job. If the job execution status is not set to a terminal state before this timer expires, or before the timer is reset (by calling @UpdateJobExecution@ , setting the status to @IN_PROGRESS@ and specifying a new timeout value in field @stepTimeoutInMinutes@ ) the job execution status will be automatically set to @TIMED_OUT@ . Note that setting this timeout has no effect on that job execution timeout which may have been specified when the job was created (@CreateJob@ using field @timeoutConfig@ ).
--
-- /Note:/ Consider using 'stepTimeoutInMinutes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
snpjeStepTimeoutInMinutes :: Lens.Lens' StartNextPendingJobExecution (Core.Maybe Core.Integer)
snpjeStepTimeoutInMinutes = Lens.field @"stepTimeoutInMinutes"
{-# INLINEABLE snpjeStepTimeoutInMinutes #-}
{-# DEPRECATED stepTimeoutInMinutes "Use generic-lens or generic-optics with 'stepTimeoutInMinutes' instead"  #-}

instance Core.ToQuery StartNextPendingJobExecution where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders StartNextPendingJobExecution where
        toHeaders _ = Core.pure Core.mempty

instance Core.FromJSON StartNextPendingJobExecution where
        toJSON StartNextPendingJobExecution{..}
          = Core.object
              (Core.catMaybes
                 [("statusDetails" Core..=) Core.<$> statusDetails,
                  ("stepTimeoutInMinutes" Core..=) Core.<$> stepTimeoutInMinutes])

instance Core.AWSRequest StartNextPendingJobExecution where
        type Rs StartNextPendingJobExecution =
             StartNextPendingJobExecutionResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.PUT,
                         Core._rqPath =
                           "/things/" Core.<> Core.toText thingName Core.<> "/jobs/$next",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 StartNextPendingJobExecutionResponse' Core.<$>
                   (x Core..:? "execution") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkStartNextPendingJobExecutionResponse' smart constructor.
data StartNextPendingJobExecutionResponse = StartNextPendingJobExecutionResponse'
  { execution :: Core.Maybe Types.JobExecution
    -- ^ A JobExecution object.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StartNextPendingJobExecutionResponse' value with any optional fields omitted.
mkStartNextPendingJobExecutionResponse
    :: Core.Int -- ^ 'responseStatus'
    -> StartNextPendingJobExecutionResponse
mkStartNextPendingJobExecutionResponse responseStatus
  = StartNextPendingJobExecutionResponse'{execution = Core.Nothing,
                                          responseStatus}

-- | A JobExecution object.
--
-- /Note:/ Consider using 'execution' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
snpjerrsExecution :: Lens.Lens' StartNextPendingJobExecutionResponse (Core.Maybe Types.JobExecution)
snpjerrsExecution = Lens.field @"execution"
{-# INLINEABLE snpjerrsExecution #-}
{-# DEPRECATED execution "Use generic-lens or generic-optics with 'execution' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
snpjerrsResponseStatus :: Lens.Lens' StartNextPendingJobExecutionResponse Core.Int
snpjerrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE snpjerrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
