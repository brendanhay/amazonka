-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.Types.Suite
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DeviceFarm.Types.Suite
  ( Suite (..),

    -- * Smart constructor
    mkSuite,

    -- * Lenses
    sStatus,
    sCounters,
    sArn,
    sCreated,
    sStopped,
    sResult,
    sName,
    sDeviceMinutes,
    sType,
    sMessage,
    sStarted,
  )
where

import Network.AWS.DeviceFarm.Types.Counters
import Network.AWS.DeviceFarm.Types.DeviceMinutes
import Network.AWS.DeviceFarm.Types.ExecutionResult
import Network.AWS.DeviceFarm.Types.ExecutionStatus
import Network.AWS.DeviceFarm.Types.TestType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents a collection of one or more tests.
--
-- /See:/ 'mkSuite' smart constructor.
data Suite = Suite'
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

-- | Creates a value of 'Suite' with the minimum fields required to make a request.
--
-- * 'arn' - The suite's ARN.
-- * 'counters' - The suite's result counters.
-- * 'created' - When the suite was created.
-- * 'deviceMinutes' - Represents the total (metered or unmetered) minutes used by the test suite.
-- * 'message' - A message about the suite's result.
-- * 'name' - The suite's name.
-- * 'result' - The suite's result.
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
-- * 'started' - The suite's start time.
-- * 'status' - The suite's status.
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
-- * 'stopped' - The suite's stop time.
-- * 'type'' - The suite's type.
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
mkSuite ::
  Suite
mkSuite =
  Suite'
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

-- | The suite's status.
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
sStatus :: Lens.Lens' Suite (Lude.Maybe ExecutionStatus)
sStatus = Lens.lens (status :: Suite -> Lude.Maybe ExecutionStatus) (\s a -> s {status = a} :: Suite)
{-# DEPRECATED sStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The suite's result counters.
--
-- /Note:/ Consider using 'counters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sCounters :: Lens.Lens' Suite (Lude.Maybe Counters)
sCounters = Lens.lens (counters :: Suite -> Lude.Maybe Counters) (\s a -> s {counters = a} :: Suite)
{-# DEPRECATED sCounters "Use generic-lens or generic-optics with 'counters' instead." #-}

-- | The suite's ARN.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sArn :: Lens.Lens' Suite (Lude.Maybe Lude.Text)
sArn = Lens.lens (arn :: Suite -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: Suite)
{-# DEPRECATED sArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | When the suite was created.
--
-- /Note:/ Consider using 'created' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sCreated :: Lens.Lens' Suite (Lude.Maybe Lude.Timestamp)
sCreated = Lens.lens (created :: Suite -> Lude.Maybe Lude.Timestamp) (\s a -> s {created = a} :: Suite)
{-# DEPRECATED sCreated "Use generic-lens or generic-optics with 'created' instead." #-}

-- | The suite's stop time.
--
-- /Note:/ Consider using 'stopped' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sStopped :: Lens.Lens' Suite (Lude.Maybe Lude.Timestamp)
sStopped = Lens.lens (stopped :: Suite -> Lude.Maybe Lude.Timestamp) (\s a -> s {stopped = a} :: Suite)
{-# DEPRECATED sStopped "Use generic-lens or generic-optics with 'stopped' instead." #-}

-- | The suite's result.
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
sResult :: Lens.Lens' Suite (Lude.Maybe ExecutionResult)
sResult = Lens.lens (result :: Suite -> Lude.Maybe ExecutionResult) (\s a -> s {result = a} :: Suite)
{-# DEPRECATED sResult "Use generic-lens or generic-optics with 'result' instead." #-}

-- | The suite's name.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sName :: Lens.Lens' Suite (Lude.Maybe Lude.Text)
sName = Lens.lens (name :: Suite -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: Suite)
{-# DEPRECATED sName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | Represents the total (metered or unmetered) minutes used by the test suite.
--
-- /Note:/ Consider using 'deviceMinutes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sDeviceMinutes :: Lens.Lens' Suite (Lude.Maybe DeviceMinutes)
sDeviceMinutes = Lens.lens (deviceMinutes :: Suite -> Lude.Maybe DeviceMinutes) (\s a -> s {deviceMinutes = a} :: Suite)
{-# DEPRECATED sDeviceMinutes "Use generic-lens or generic-optics with 'deviceMinutes' instead." #-}

-- | The suite's type.
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
sType :: Lens.Lens' Suite (Lude.Maybe TestType)
sType = Lens.lens (type' :: Suite -> Lude.Maybe TestType) (\s a -> s {type' = a} :: Suite)
{-# DEPRECATED sType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | A message about the suite's result.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sMessage :: Lens.Lens' Suite (Lude.Maybe Lude.Text)
sMessage = Lens.lens (message :: Suite -> Lude.Maybe Lude.Text) (\s a -> s {message = a} :: Suite)
{-# DEPRECATED sMessage "Use generic-lens or generic-optics with 'message' instead." #-}

-- | The suite's start time.
--
-- /Note:/ Consider using 'started' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sStarted :: Lens.Lens' Suite (Lude.Maybe Lude.Timestamp)
sStarted = Lens.lens (started :: Suite -> Lude.Maybe Lude.Timestamp) (\s a -> s {started = a} :: Suite)
{-# DEPRECATED sStarted "Use generic-lens or generic-optics with 'started' instead." #-}

instance Lude.FromJSON Suite where
  parseJSON =
    Lude.withObject
      "Suite"
      ( \x ->
          Suite'
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
