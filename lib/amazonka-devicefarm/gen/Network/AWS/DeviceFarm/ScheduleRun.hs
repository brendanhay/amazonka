{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    ScheduleRun (..),
    mkScheduleRun,

    -- ** Request lenses
    srProjectArn,
    srTest,
    srAppArn,
    srConfiguration,
    srDevicePoolArn,
    srDeviceSelectionConfiguration,
    srExecutionConfiguration,
    srName,

    -- * Destructuring the response
    ScheduleRunResponse (..),
    mkScheduleRunResponse,

    -- ** Response lenses
    srrrsRun,
    srrrsResponseStatus,
  )
where

import qualified Network.AWS.DeviceFarm.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents a request to the schedule run operation.
--
-- /See:/ 'mkScheduleRun' smart constructor.
data ScheduleRun = ScheduleRun'
  { -- | The ARN of the project for the run to be scheduled.
    projectArn :: Types.ProjectArn,
    -- | Information about the test for the run to be scheduled.
    test :: Types.ScheduleRunTest,
    -- | The ARN of an application package to run tests against, created with 'CreateUpload' . See 'ListUploads' .
    appArn :: Core.Maybe Types.AppArn,
    -- | Information about the settings for the run to be scheduled.
    configuration :: Core.Maybe Types.ScheduleRunConfiguration,
    -- | The ARN of the device pool for the run to be scheduled.
    devicePoolArn :: Core.Maybe Types.DevicePoolArn,
    -- | The filter criteria used to dynamically select a set of devices for a test run and the maximum number of devices to be included in the run.
    --
    -- Either __@devicePoolArn@ __ or __@deviceSelectionConfiguration@ __ is required in a request.
    deviceSelectionConfiguration :: Core.Maybe Types.DeviceSelectionConfiguration,
    -- | Specifies configuration information about a test run, such as the execution timeout (in minutes).
    executionConfiguration :: Core.Maybe Types.ExecutionConfiguration,
    -- | The name for the run to be scheduled.
    name :: Core.Maybe Types.Name
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ScheduleRun' value with any optional fields omitted.
mkScheduleRun ::
  -- | 'projectArn'
  Types.ProjectArn ->
  -- | 'test'
  Types.ScheduleRunTest ->
  ScheduleRun
mkScheduleRun projectArn test =
  ScheduleRun'
    { projectArn,
      test,
      appArn = Core.Nothing,
      configuration = Core.Nothing,
      devicePoolArn = Core.Nothing,
      deviceSelectionConfiguration = Core.Nothing,
      executionConfiguration = Core.Nothing,
      name = Core.Nothing
    }

-- | The ARN of the project for the run to be scheduled.
--
-- /Note:/ Consider using 'projectArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srProjectArn :: Lens.Lens' ScheduleRun Types.ProjectArn
srProjectArn = Lens.field @"projectArn"
{-# DEPRECATED srProjectArn "Use generic-lens or generic-optics with 'projectArn' instead." #-}

-- | Information about the test for the run to be scheduled.
--
-- /Note:/ Consider using 'test' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srTest :: Lens.Lens' ScheduleRun Types.ScheduleRunTest
srTest = Lens.field @"test"
{-# DEPRECATED srTest "Use generic-lens or generic-optics with 'test' instead." #-}

-- | The ARN of an application package to run tests against, created with 'CreateUpload' . See 'ListUploads' .
--
-- /Note:/ Consider using 'appArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srAppArn :: Lens.Lens' ScheduleRun (Core.Maybe Types.AppArn)
srAppArn = Lens.field @"appArn"
{-# DEPRECATED srAppArn "Use generic-lens or generic-optics with 'appArn' instead." #-}

-- | Information about the settings for the run to be scheduled.
--
-- /Note:/ Consider using 'configuration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srConfiguration :: Lens.Lens' ScheduleRun (Core.Maybe Types.ScheduleRunConfiguration)
srConfiguration = Lens.field @"configuration"
{-# DEPRECATED srConfiguration "Use generic-lens or generic-optics with 'configuration' instead." #-}

-- | The ARN of the device pool for the run to be scheduled.
--
-- /Note:/ Consider using 'devicePoolArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srDevicePoolArn :: Lens.Lens' ScheduleRun (Core.Maybe Types.DevicePoolArn)
srDevicePoolArn = Lens.field @"devicePoolArn"
{-# DEPRECATED srDevicePoolArn "Use generic-lens or generic-optics with 'devicePoolArn' instead." #-}

-- | The filter criteria used to dynamically select a set of devices for a test run and the maximum number of devices to be included in the run.
--
-- Either __@devicePoolArn@ __ or __@deviceSelectionConfiguration@ __ is required in a request.
--
-- /Note:/ Consider using 'deviceSelectionConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srDeviceSelectionConfiguration :: Lens.Lens' ScheduleRun (Core.Maybe Types.DeviceSelectionConfiguration)
srDeviceSelectionConfiguration = Lens.field @"deviceSelectionConfiguration"
{-# DEPRECATED srDeviceSelectionConfiguration "Use generic-lens or generic-optics with 'deviceSelectionConfiguration' instead." #-}

-- | Specifies configuration information about a test run, such as the execution timeout (in minutes).
--
-- /Note:/ Consider using 'executionConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srExecutionConfiguration :: Lens.Lens' ScheduleRun (Core.Maybe Types.ExecutionConfiguration)
srExecutionConfiguration = Lens.field @"executionConfiguration"
{-# DEPRECATED srExecutionConfiguration "Use generic-lens or generic-optics with 'executionConfiguration' instead." #-}

-- | The name for the run to be scheduled.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srName :: Lens.Lens' ScheduleRun (Core.Maybe Types.Name)
srName = Lens.field @"name"
{-# DEPRECATED srName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Core.FromJSON ScheduleRun where
  toJSON ScheduleRun {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("projectArn" Core..= projectArn),
            Core.Just ("test" Core..= test),
            ("appArn" Core..=) Core.<$> appArn,
            ("configuration" Core..=) Core.<$> configuration,
            ("devicePoolArn" Core..=) Core.<$> devicePoolArn,
            ("deviceSelectionConfiguration" Core..=)
              Core.<$> deviceSelectionConfiguration,
            ("executionConfiguration" Core..=) Core.<$> executionConfiguration,
            ("name" Core..=) Core.<$> name
          ]
      )

instance Core.AWSRequest ScheduleRun where
  type Rs ScheduleRun = ScheduleRunResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "DeviceFarm_20150623.ScheduleRun")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ScheduleRunResponse'
            Core.<$> (x Core..:? "run") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Represents the result of a schedule run request.
--
-- /See:/ 'mkScheduleRunResponse' smart constructor.
data ScheduleRunResponse = ScheduleRunResponse'
  { -- | Information about the scheduled run.
    run :: Core.Maybe Types.Run,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ScheduleRunResponse' value with any optional fields omitted.
mkScheduleRunResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ScheduleRunResponse
mkScheduleRunResponse responseStatus =
  ScheduleRunResponse' {run = Core.Nothing, responseStatus}

-- | Information about the scheduled run.
--
-- /Note:/ Consider using 'run' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srrrsRun :: Lens.Lens' ScheduleRunResponse (Core.Maybe Types.Run)
srrrsRun = Lens.field @"run"
{-# DEPRECATED srrrsRun "Use generic-lens or generic-optics with 'run' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srrrsResponseStatus :: Lens.Lens' ScheduleRunResponse Core.Int
srrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED srrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
