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
    srExecutionConfiguration,
    srDeviceSelectionConfiguration,
    srTest,
    srAppARN,
    srName,
    srProjectARN,
    srConfiguration,
    srDevicePoolARN,

    -- * Destructuring the response
    ScheduleRunResponse (..),
    mkScheduleRunResponse,

    -- ** Response lenses
    srrsRun,
    srrsResponseStatus,
  )
where

import Network.AWS.DeviceFarm.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents a request to the schedule run operation.
--
-- /See:/ 'mkScheduleRun' smart constructor.
data ScheduleRun = ScheduleRun'
  { -- | Specifies configuration information about a test run, such as the execution timeout (in minutes).
    executionConfiguration :: Lude.Maybe ExecutionConfiguration,
    -- | The filter criteria used to dynamically select a set of devices for a test run and the maximum number of devices to be included in the run.
    --
    -- Either __@devicePoolArn@ __ or __@deviceSelectionConfiguration@ __ is required in a request.
    deviceSelectionConfiguration :: Lude.Maybe DeviceSelectionConfiguration,
    -- | Information about the test for the run to be scheduled.
    test :: ScheduleRunTest,
    -- | The ARN of an application package to run tests against, created with 'CreateUpload' . See 'ListUploads' .
    appARN :: Lude.Maybe Lude.Text,
    -- | The name for the run to be scheduled.
    name :: Lude.Maybe Lude.Text,
    -- | The ARN of the project for the run to be scheduled.
    projectARN :: Lude.Text,
    -- | Information about the settings for the run to be scheduled.
    configuration :: Lude.Maybe ScheduleRunConfiguration,
    -- | The ARN of the device pool for the run to be scheduled.
    devicePoolARN :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ScheduleRun' with the minimum fields required to make a request.
--
-- * 'executionConfiguration' - Specifies configuration information about a test run, such as the execution timeout (in minutes).
-- * 'deviceSelectionConfiguration' - The filter criteria used to dynamically select a set of devices for a test run and the maximum number of devices to be included in the run.
--
-- Either __@devicePoolArn@ __ or __@deviceSelectionConfiguration@ __ is required in a request.
-- * 'test' - Information about the test for the run to be scheduled.
-- * 'appARN' - The ARN of an application package to run tests against, created with 'CreateUpload' . See 'ListUploads' .
-- * 'name' - The name for the run to be scheduled.
-- * 'projectARN' - The ARN of the project for the run to be scheduled.
-- * 'configuration' - Information about the settings for the run to be scheduled.
-- * 'devicePoolARN' - The ARN of the device pool for the run to be scheduled.
mkScheduleRun ::
  -- | 'test'
  ScheduleRunTest ->
  -- | 'projectARN'
  Lude.Text ->
  ScheduleRun
mkScheduleRun pTest_ pProjectARN_ =
  ScheduleRun'
    { executionConfiguration = Lude.Nothing,
      deviceSelectionConfiguration = Lude.Nothing,
      test = pTest_,
      appARN = Lude.Nothing,
      name = Lude.Nothing,
      projectARN = pProjectARN_,
      configuration = Lude.Nothing,
      devicePoolARN = Lude.Nothing
    }

-- | Specifies configuration information about a test run, such as the execution timeout (in minutes).
--
-- /Note:/ Consider using 'executionConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srExecutionConfiguration :: Lens.Lens' ScheduleRun (Lude.Maybe ExecutionConfiguration)
srExecutionConfiguration = Lens.lens (executionConfiguration :: ScheduleRun -> Lude.Maybe ExecutionConfiguration) (\s a -> s {executionConfiguration = a} :: ScheduleRun)
{-# DEPRECATED srExecutionConfiguration "Use generic-lens or generic-optics with 'executionConfiguration' instead." #-}

-- | The filter criteria used to dynamically select a set of devices for a test run and the maximum number of devices to be included in the run.
--
-- Either __@devicePoolArn@ __ or __@deviceSelectionConfiguration@ __ is required in a request.
--
-- /Note:/ Consider using 'deviceSelectionConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srDeviceSelectionConfiguration :: Lens.Lens' ScheduleRun (Lude.Maybe DeviceSelectionConfiguration)
srDeviceSelectionConfiguration = Lens.lens (deviceSelectionConfiguration :: ScheduleRun -> Lude.Maybe DeviceSelectionConfiguration) (\s a -> s {deviceSelectionConfiguration = a} :: ScheduleRun)
{-# DEPRECATED srDeviceSelectionConfiguration "Use generic-lens or generic-optics with 'deviceSelectionConfiguration' instead." #-}

-- | Information about the test for the run to be scheduled.
--
-- /Note:/ Consider using 'test' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srTest :: Lens.Lens' ScheduleRun ScheduleRunTest
srTest = Lens.lens (test :: ScheduleRun -> ScheduleRunTest) (\s a -> s {test = a} :: ScheduleRun)
{-# DEPRECATED srTest "Use generic-lens or generic-optics with 'test' instead." #-}

-- | The ARN of an application package to run tests against, created with 'CreateUpload' . See 'ListUploads' .
--
-- /Note:/ Consider using 'appARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srAppARN :: Lens.Lens' ScheduleRun (Lude.Maybe Lude.Text)
srAppARN = Lens.lens (appARN :: ScheduleRun -> Lude.Maybe Lude.Text) (\s a -> s {appARN = a} :: ScheduleRun)
{-# DEPRECATED srAppARN "Use generic-lens or generic-optics with 'appARN' instead." #-}

-- | The name for the run to be scheduled.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srName :: Lens.Lens' ScheduleRun (Lude.Maybe Lude.Text)
srName = Lens.lens (name :: ScheduleRun -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: ScheduleRun)
{-# DEPRECATED srName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The ARN of the project for the run to be scheduled.
--
-- /Note:/ Consider using 'projectARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srProjectARN :: Lens.Lens' ScheduleRun Lude.Text
srProjectARN = Lens.lens (projectARN :: ScheduleRun -> Lude.Text) (\s a -> s {projectARN = a} :: ScheduleRun)
{-# DEPRECATED srProjectARN "Use generic-lens or generic-optics with 'projectARN' instead." #-}

-- | Information about the settings for the run to be scheduled.
--
-- /Note:/ Consider using 'configuration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srConfiguration :: Lens.Lens' ScheduleRun (Lude.Maybe ScheduleRunConfiguration)
srConfiguration = Lens.lens (configuration :: ScheduleRun -> Lude.Maybe ScheduleRunConfiguration) (\s a -> s {configuration = a} :: ScheduleRun)
{-# DEPRECATED srConfiguration "Use generic-lens or generic-optics with 'configuration' instead." #-}

-- | The ARN of the device pool for the run to be scheduled.
--
-- /Note:/ Consider using 'devicePoolARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srDevicePoolARN :: Lens.Lens' ScheduleRun (Lude.Maybe Lude.Text)
srDevicePoolARN = Lens.lens (devicePoolARN :: ScheduleRun -> Lude.Maybe Lude.Text) (\s a -> s {devicePoolARN = a} :: ScheduleRun)
{-# DEPRECATED srDevicePoolARN "Use generic-lens or generic-optics with 'devicePoolARN' instead." #-}

instance Lude.AWSRequest ScheduleRun where
  type Rs ScheduleRun = ScheduleRunResponse
  request = Req.postJSON deviceFarmService
  response =
    Res.receiveJSON
      ( \s h x ->
          ScheduleRunResponse'
            Lude.<$> (x Lude..?> "run") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ScheduleRun where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("DeviceFarm_20150623.ScheduleRun" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ScheduleRun where
  toJSON ScheduleRun' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("executionConfiguration" Lude..=)
              Lude.<$> executionConfiguration,
            ("deviceSelectionConfiguration" Lude..=)
              Lude.<$> deviceSelectionConfiguration,
            Lude.Just ("test" Lude..= test),
            ("appArn" Lude..=) Lude.<$> appARN,
            ("name" Lude..=) Lude.<$> name,
            Lude.Just ("projectArn" Lude..= projectARN),
            ("configuration" Lude..=) Lude.<$> configuration,
            ("devicePoolArn" Lude..=) Lude.<$> devicePoolARN
          ]
      )

instance Lude.ToPath ScheduleRun where
  toPath = Lude.const "/"

instance Lude.ToQuery ScheduleRun where
  toQuery = Lude.const Lude.mempty

-- | Represents the result of a schedule run request.
--
-- /See:/ 'mkScheduleRunResponse' smart constructor.
data ScheduleRunResponse = ScheduleRunResponse'
  { -- | Information about the scheduled run.
    run :: Lude.Maybe Run,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ScheduleRunResponse' with the minimum fields required to make a request.
--
-- * 'run' - Information about the scheduled run.
-- * 'responseStatus' - The response status code.
mkScheduleRunResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ScheduleRunResponse
mkScheduleRunResponse pResponseStatus_ =
  ScheduleRunResponse'
    { run = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the scheduled run.
--
-- /Note:/ Consider using 'run' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srrsRun :: Lens.Lens' ScheduleRunResponse (Lude.Maybe Run)
srrsRun = Lens.lens (run :: ScheduleRunResponse -> Lude.Maybe Run) (\s a -> s {run = a} :: ScheduleRunResponse)
{-# DEPRECATED srrsRun "Use generic-lens or generic-optics with 'run' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srrsResponseStatus :: Lens.Lens' ScheduleRunResponse Lude.Int
srrsResponseStatus = Lens.lens (responseStatus :: ScheduleRunResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ScheduleRunResponse)
{-# DEPRECATED srrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
