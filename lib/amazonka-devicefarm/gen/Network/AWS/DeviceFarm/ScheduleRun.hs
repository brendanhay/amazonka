{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.ScheduleRun
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Schedules a run.
module Network.AWS.DeviceFarm.ScheduleRun
    (
    -- * Creating a request
      ScheduleRun (..)
    , mkScheduleRun
    -- ** Request lenses
    , srProjectArn
    , srTest
    , srAppArn
    , srConfiguration
    , srDevicePoolArn
    , srDeviceSelectionConfiguration
    , srExecutionConfiguration
    , srName

    -- * Destructuring the response
    , ScheduleRunResponse (..)
    , mkScheduleRunResponse
    -- ** Response lenses
    , srrrsRun
    , srrrsResponseStatus
    ) where

import qualified Network.AWS.DeviceFarm.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents a request to the schedule run operation.
--
-- /See:/ 'mkScheduleRun' smart constructor.
data ScheduleRun = ScheduleRun'
  { projectArn :: Types.ProjectArn
    -- ^ The ARN of the project for the run to be scheduled.
  , test :: Types.ScheduleRunTest
    -- ^ Information about the test for the run to be scheduled.
  , appArn :: Core.Maybe Types.AppArn
    -- ^ The ARN of an application package to run tests against, created with 'CreateUpload' . See 'ListUploads' .
  , configuration :: Core.Maybe Types.ScheduleRunConfiguration
    -- ^ Information about the settings for the run to be scheduled.
  , devicePoolArn :: Core.Maybe Types.DevicePoolArn
    -- ^ The ARN of the device pool for the run to be scheduled.
  , deviceSelectionConfiguration :: Core.Maybe Types.DeviceSelectionConfiguration
    -- ^ The filter criteria used to dynamically select a set of devices for a test run and the maximum number of devices to be included in the run.
--
-- Either __@devicePoolArn@ __ or __@deviceSelectionConfiguration@ __ is required in a request.
  , executionConfiguration :: Core.Maybe Types.ExecutionConfiguration
    -- ^ Specifies configuration information about a test run, such as the execution timeout (in minutes).
  , name :: Core.Maybe Types.Name
    -- ^ The name for the run to be scheduled.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ScheduleRun' value with any optional fields omitted.
mkScheduleRun
    :: Types.ProjectArn -- ^ 'projectArn'
    -> Types.ScheduleRunTest -- ^ 'test'
    -> ScheduleRun
mkScheduleRun projectArn test
  = ScheduleRun'{projectArn, test, appArn = Core.Nothing,
                 configuration = Core.Nothing, devicePoolArn = Core.Nothing,
                 deviceSelectionConfiguration = Core.Nothing,
                 executionConfiguration = Core.Nothing, name = Core.Nothing}

-- | The ARN of the project for the run to be scheduled.
--
-- /Note:/ Consider using 'projectArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srProjectArn :: Lens.Lens' ScheduleRun Types.ProjectArn
srProjectArn = Lens.field @"projectArn"
{-# INLINEABLE srProjectArn #-}
{-# DEPRECATED projectArn "Use generic-lens or generic-optics with 'projectArn' instead"  #-}

-- | Information about the test for the run to be scheduled.
--
-- /Note:/ Consider using 'test' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srTest :: Lens.Lens' ScheduleRun Types.ScheduleRunTest
srTest = Lens.field @"test"
{-# INLINEABLE srTest #-}
{-# DEPRECATED test "Use generic-lens or generic-optics with 'test' instead"  #-}

-- | The ARN of an application package to run tests against, created with 'CreateUpload' . See 'ListUploads' .
--
-- /Note:/ Consider using 'appArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srAppArn :: Lens.Lens' ScheduleRun (Core.Maybe Types.AppArn)
srAppArn = Lens.field @"appArn"
{-# INLINEABLE srAppArn #-}
{-# DEPRECATED appArn "Use generic-lens or generic-optics with 'appArn' instead"  #-}

-- | Information about the settings for the run to be scheduled.
--
-- /Note:/ Consider using 'configuration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srConfiguration :: Lens.Lens' ScheduleRun (Core.Maybe Types.ScheduleRunConfiguration)
srConfiguration = Lens.field @"configuration"
{-# INLINEABLE srConfiguration #-}
{-# DEPRECATED configuration "Use generic-lens or generic-optics with 'configuration' instead"  #-}

-- | The ARN of the device pool for the run to be scheduled.
--
-- /Note:/ Consider using 'devicePoolArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srDevicePoolArn :: Lens.Lens' ScheduleRun (Core.Maybe Types.DevicePoolArn)
srDevicePoolArn = Lens.field @"devicePoolArn"
{-# INLINEABLE srDevicePoolArn #-}
{-# DEPRECATED devicePoolArn "Use generic-lens or generic-optics with 'devicePoolArn' instead"  #-}

-- | The filter criteria used to dynamically select a set of devices for a test run and the maximum number of devices to be included in the run.
--
-- Either __@devicePoolArn@ __ or __@deviceSelectionConfiguration@ __ is required in a request.
--
-- /Note:/ Consider using 'deviceSelectionConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srDeviceSelectionConfiguration :: Lens.Lens' ScheduleRun (Core.Maybe Types.DeviceSelectionConfiguration)
srDeviceSelectionConfiguration = Lens.field @"deviceSelectionConfiguration"
{-# INLINEABLE srDeviceSelectionConfiguration #-}
{-# DEPRECATED deviceSelectionConfiguration "Use generic-lens or generic-optics with 'deviceSelectionConfiguration' instead"  #-}

-- | Specifies configuration information about a test run, such as the execution timeout (in minutes).
--
-- /Note:/ Consider using 'executionConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srExecutionConfiguration :: Lens.Lens' ScheduleRun (Core.Maybe Types.ExecutionConfiguration)
srExecutionConfiguration = Lens.field @"executionConfiguration"
{-# INLINEABLE srExecutionConfiguration #-}
{-# DEPRECATED executionConfiguration "Use generic-lens or generic-optics with 'executionConfiguration' instead"  #-}

-- | The name for the run to be scheduled.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srName :: Lens.Lens' ScheduleRun (Core.Maybe Types.Name)
srName = Lens.field @"name"
{-# INLINEABLE srName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

instance Core.ToQuery ScheduleRun where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ScheduleRun where
        toHeaders ScheduleRun{..}
          = Core.pure ("X-Amz-Target", "DeviceFarm_20150623.ScheduleRun")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ScheduleRun where
        toJSON ScheduleRun{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("projectArn" Core..= projectArn),
                  Core.Just ("test" Core..= test),
                  ("appArn" Core..=) Core.<$> appArn,
                  ("configuration" Core..=) Core.<$> configuration,
                  ("devicePoolArn" Core..=) Core.<$> devicePoolArn,
                  ("deviceSelectionConfiguration" Core..=) Core.<$>
                    deviceSelectionConfiguration,
                  ("executionConfiguration" Core..=) Core.<$> executionConfiguration,
                  ("name" Core..=) Core.<$> name])

instance Core.AWSRequest ScheduleRun where
        type Rs ScheduleRun = ScheduleRunResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ScheduleRunResponse' Core.<$>
                   (x Core..:? "run") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Represents the result of a schedule run request.
--
-- /See:/ 'mkScheduleRunResponse' smart constructor.
data ScheduleRunResponse = ScheduleRunResponse'
  { run :: Core.Maybe Types.Run
    -- ^ Information about the scheduled run.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ScheduleRunResponse' value with any optional fields omitted.
mkScheduleRunResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ScheduleRunResponse
mkScheduleRunResponse responseStatus
  = ScheduleRunResponse'{run = Core.Nothing, responseStatus}

-- | Information about the scheduled run.
--
-- /Note:/ Consider using 'run' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srrrsRun :: Lens.Lens' ScheduleRunResponse (Core.Maybe Types.Run)
srrrsRun = Lens.field @"run"
{-# INLINEABLE srrrsRun #-}
{-# DEPRECATED run "Use generic-lens or generic-optics with 'run' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srrrsResponseStatus :: Lens.Lens' ScheduleRunResponse Core.Int
srrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE srrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
