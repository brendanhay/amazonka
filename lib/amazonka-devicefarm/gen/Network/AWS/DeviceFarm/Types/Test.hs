{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.Types.Test
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DeviceFarm.Types.Test
  ( Test (..),

    -- * Smart constructor
    mkTest,

    -- * Lenses
    tStatus,
    tCounters,
    tArn,
    tCreated,
    tStopped,
    tResult,
    tName,
    tDeviceMinutes,
    tType,
    tMessage,
    tStarted,
  )
where

import Network.AWS.DeviceFarm.Types.Counters
import Network.AWS.DeviceFarm.Types.DeviceMinutes
import Network.AWS.DeviceFarm.Types.ExecutionResult
import Network.AWS.DeviceFarm.Types.ExecutionStatus
import Network.AWS.DeviceFarm.Types.TestType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents a condition that is evaluated.
--
-- /See:/ 'mkTest' smart constructor.
data Test = Test'
  { status :: Lude.Maybe ExecutionStatus,
    counters :: Lude.Maybe Counters,
    arn :: Lude.Maybe Lude.Text,
    created :: Lude.Maybe Lude.Timestamp,
    stopped :: Lude.Maybe Lude.Timestamp,
    result :: Lude.Maybe ExecutionResult,
    name :: Lude.Maybe Lude.Text,
    deviceMinutes :: Lude.Maybe DeviceMinutes,
    type' :: Lude.Maybe TestType,
    message :: Lude.Maybe Lude.Text,
    started :: Lude.Maybe Lude.Timestamp
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Test' with the minimum fields required to make a request.
--
-- * 'arn' - The test's ARN.
-- * 'counters' - The test's result counters.
-- * 'created' - When the test was created.
-- * 'deviceMinutes' - Represents the total (metered or unmetered) minutes used by the test.
-- * 'message' - A message about the test's result.
-- * 'name' - The test's name.
-- * 'result' - The test's result.
--
-- Allowed values include:
--
--     * PENDING
--
--
--     * PASSED
--
--
--     * WARNED
--
--
--     * FAILED
--
--
--     * SKIPPED
--
--
--     * ERRORED
--
--
--     * STOPPED
--
--
-- * 'started' - The test's start time.
-- * 'status' - The test's status.
--
-- Allowed values include:
--
--     * PENDING
--
--
--     * PENDING_CONCURRENCY
--
--
--     * PENDING_DEVICE
--
--
--     * PROCESSING
--
--
--     * SCHEDULING
--
--
--     * PREPARING
--
--
--     * RUNNING
--
--
--     * COMPLETED
--
--
--     * STOPPING
--
--
-- * 'stopped' - The test's stop time.
-- * 'type'' - The test's type.
--
-- Must be one of the following values:
--
--     * BUILTIN_FUZZ
--
--
--     * BUILTIN_EXPLORER
--
--
--     * APPIUM_JAVA_JUNIT
--
--
--     * APPIUM_JAVA_TESTNG
--
--
--     * APPIUM_PYTHON
--
--
--     * APPIUM_NODE
--
--
--     * APPIUM_RUBY
--
--
--     * APPIUM_WEB_JAVA_JUNIT
--
--
--     * APPIUM_WEB_JAVA_TESTNG
--
--
--     * APPIUM_WEB_PYTHON
--
--
--     * APPIUM_WEB_NODE
--
--
--     * APPIUM_WEB_RUBY
--
--
--     * CALABASH
--
--
--     * INSTRUMENTATION
--
--
--     * UIAUTOMATION
--
--
--     * UIAUTOMATOR
--
--
--     * XCTEST
--
--
--     * XCTEST_UI
mkTest ::
  Test
mkTest =
  Test'
    { status = Lude.Nothing,
      counters = Lude.Nothing,
      arn = Lude.Nothing,
      created = Lude.Nothing,
      stopped = Lude.Nothing,
      result = Lude.Nothing,
      name = Lude.Nothing,
      deviceMinutes = Lude.Nothing,
      type' = Lude.Nothing,
      message = Lude.Nothing,
      started = Lude.Nothing
    }

-- | The test's status.
--
-- Allowed values include:
--
--     * PENDING
--
--
--     * PENDING_CONCURRENCY
--
--
--     * PENDING_DEVICE
--
--
--     * PROCESSING
--
--
--     * SCHEDULING
--
--
--     * PREPARING
--
--
--     * RUNNING
--
--
--     * COMPLETED
--
--
--     * STOPPING
--
--
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tStatus :: Lens.Lens' Test (Lude.Maybe ExecutionStatus)
tStatus = Lens.lens (status :: Test -> Lude.Maybe ExecutionStatus) (\s a -> s {status = a} :: Test)
{-# DEPRECATED tStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The test's result counters.
--
-- /Note:/ Consider using 'counters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tCounters :: Lens.Lens' Test (Lude.Maybe Counters)
tCounters = Lens.lens (counters :: Test -> Lude.Maybe Counters) (\s a -> s {counters = a} :: Test)
{-# DEPRECATED tCounters "Use generic-lens or generic-optics with 'counters' instead." #-}

-- | The test's ARN.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tArn :: Lens.Lens' Test (Lude.Maybe Lude.Text)
tArn = Lens.lens (arn :: Test -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: Test)
{-# DEPRECATED tArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | When the test was created.
--
-- /Note:/ Consider using 'created' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tCreated :: Lens.Lens' Test (Lude.Maybe Lude.Timestamp)
tCreated = Lens.lens (created :: Test -> Lude.Maybe Lude.Timestamp) (\s a -> s {created = a} :: Test)
{-# DEPRECATED tCreated "Use generic-lens or generic-optics with 'created' instead." #-}

-- | The test's stop time.
--
-- /Note:/ Consider using 'stopped' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tStopped :: Lens.Lens' Test (Lude.Maybe Lude.Timestamp)
tStopped = Lens.lens (stopped :: Test -> Lude.Maybe Lude.Timestamp) (\s a -> s {stopped = a} :: Test)
{-# DEPRECATED tStopped "Use generic-lens or generic-optics with 'stopped' instead." #-}

-- | The test's result.
--
-- Allowed values include:
--
--     * PENDING
--
--
--     * PASSED
--
--
--     * WARNED
--
--
--     * FAILED
--
--
--     * SKIPPED
--
--
--     * ERRORED
--
--
--     * STOPPED
--
--
--
-- /Note:/ Consider using 'result' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tResult :: Lens.Lens' Test (Lude.Maybe ExecutionResult)
tResult = Lens.lens (result :: Test -> Lude.Maybe ExecutionResult) (\s a -> s {result = a} :: Test)
{-# DEPRECATED tResult "Use generic-lens or generic-optics with 'result' instead." #-}

-- | The test's name.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tName :: Lens.Lens' Test (Lude.Maybe Lude.Text)
tName = Lens.lens (name :: Test -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: Test)
{-# DEPRECATED tName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | Represents the total (metered or unmetered) minutes used by the test.
--
-- /Note:/ Consider using 'deviceMinutes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tDeviceMinutes :: Lens.Lens' Test (Lude.Maybe DeviceMinutes)
tDeviceMinutes = Lens.lens (deviceMinutes :: Test -> Lude.Maybe DeviceMinutes) (\s a -> s {deviceMinutes = a} :: Test)
{-# DEPRECATED tDeviceMinutes "Use generic-lens or generic-optics with 'deviceMinutes' instead." #-}

-- | The test's type.
--
-- Must be one of the following values:
--
--     * BUILTIN_FUZZ
--
--
--     * BUILTIN_EXPLORER
--
--
--     * APPIUM_JAVA_JUNIT
--
--
--     * APPIUM_JAVA_TESTNG
--
--
--     * APPIUM_PYTHON
--
--
--     * APPIUM_NODE
--
--
--     * APPIUM_RUBY
--
--
--     * APPIUM_WEB_JAVA_JUNIT
--
--
--     * APPIUM_WEB_JAVA_TESTNG
--
--
--     * APPIUM_WEB_PYTHON
--
--
--     * APPIUM_WEB_NODE
--
--
--     * APPIUM_WEB_RUBY
--
--
--     * CALABASH
--
--
--     * INSTRUMENTATION
--
--
--     * UIAUTOMATION
--
--
--     * UIAUTOMATOR
--
--
--     * XCTEST
--
--
--     * XCTEST_UI
--
--
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tType :: Lens.Lens' Test (Lude.Maybe TestType)
tType = Lens.lens (type' :: Test -> Lude.Maybe TestType) (\s a -> s {type' = a} :: Test)
{-# DEPRECATED tType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | A message about the test's result.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tMessage :: Lens.Lens' Test (Lude.Maybe Lude.Text)
tMessage = Lens.lens (message :: Test -> Lude.Maybe Lude.Text) (\s a -> s {message = a} :: Test)
{-# DEPRECATED tMessage "Use generic-lens or generic-optics with 'message' instead." #-}

-- | The test's start time.
--
-- /Note:/ Consider using 'started' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tStarted :: Lens.Lens' Test (Lude.Maybe Lude.Timestamp)
tStarted = Lens.lens (started :: Test -> Lude.Maybe Lude.Timestamp) (\s a -> s {started = a} :: Test)
{-# DEPRECATED tStarted "Use generic-lens or generic-optics with 'started' instead." #-}

instance Lude.FromJSON Test where
  parseJSON =
    Lude.withObject
      "Test"
      ( \x ->
          Test'
            Lude.<$> (x Lude..:? "status")
            Lude.<*> (x Lude..:? "counters")
            Lude.<*> (x Lude..:? "arn")
            Lude.<*> (x Lude..:? "created")
            Lude.<*> (x Lude..:? "stopped")
            Lude.<*> (x Lude..:? "result")
            Lude.<*> (x Lude..:? "name")
            Lude.<*> (x Lude..:? "deviceMinutes")
            Lude.<*> (x Lude..:? "type")
            Lude.<*> (x Lude..:? "message")
            Lude.<*> (x Lude..:? "started")
      )
