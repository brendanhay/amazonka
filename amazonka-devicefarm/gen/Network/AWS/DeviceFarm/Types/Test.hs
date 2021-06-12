{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.Types.Test
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DeviceFarm.Types.Test where

import qualified Network.AWS.Core as Core
import Network.AWS.DeviceFarm.Types.Counters
import Network.AWS.DeviceFarm.Types.DeviceMinutes
import Network.AWS.DeviceFarm.Types.ExecutionResult
import Network.AWS.DeviceFarm.Types.ExecutionStatus
import Network.AWS.DeviceFarm.Types.TestType
import qualified Network.AWS.Lens as Lens

-- | Represents a condition that is evaluated.
--
-- /See:/ 'newTest' smart constructor.
data Test = Test'
  { -- | The test\'s result counters.
    counters :: Core.Maybe Counters,
    -- | The test\'s status.
    --
    -- Allowed values include:
    --
    -- -   PENDING
    --
    -- -   PENDING_CONCURRENCY
    --
    -- -   PENDING_DEVICE
    --
    -- -   PROCESSING
    --
    -- -   SCHEDULING
    --
    -- -   PREPARING
    --
    -- -   RUNNING
    --
    -- -   COMPLETED
    --
    -- -   STOPPING
    status :: Core.Maybe ExecutionStatus,
    -- | The test\'s result.
    --
    -- Allowed values include:
    --
    -- -   PENDING
    --
    -- -   PASSED
    --
    -- -   WARNED
    --
    -- -   FAILED
    --
    -- -   SKIPPED
    --
    -- -   ERRORED
    --
    -- -   STOPPED
    result :: Core.Maybe ExecutionResult,
    -- | The test\'s start time.
    started :: Core.Maybe Core.POSIX,
    -- | A message about the test\'s result.
    message :: Core.Maybe Core.Text,
    -- | The test\'s ARN.
    arn :: Core.Maybe Core.Text,
    -- | The test\'s name.
    name :: Core.Maybe Core.Text,
    -- | The test\'s stop time.
    stopped :: Core.Maybe Core.POSIX,
    -- | When the test was created.
    created :: Core.Maybe Core.POSIX,
    -- | The test\'s type.
    --
    -- Must be one of the following values:
    --
    -- -   BUILTIN_FUZZ
    --
    -- -   BUILTIN_EXPLORER
    --
    --     For Android, an app explorer that traverses an Android app,
    --     interacting with it and capturing screenshots at the same time.
    --
    -- -   APPIUM_JAVA_JUNIT
    --
    -- -   APPIUM_JAVA_TESTNG
    --
    -- -   APPIUM_PYTHON
    --
    -- -   APPIUM_NODE
    --
    -- -   APPIUM_RUBY
    --
    -- -   APPIUM_WEB_JAVA_JUNIT
    --
    -- -   APPIUM_WEB_JAVA_TESTNG
    --
    -- -   APPIUM_WEB_PYTHON
    --
    -- -   APPIUM_WEB_NODE
    --
    -- -   APPIUM_WEB_RUBY
    --
    -- -   CALABASH
    --
    -- -   INSTRUMENTATION
    --
    -- -   UIAUTOMATION
    --
    -- -   UIAUTOMATOR
    --
    -- -   XCTEST
    --
    -- -   XCTEST_UI
    type' :: Core.Maybe TestType,
    -- | Represents the total (metered or unmetered) minutes used by the test.
    deviceMinutes :: Core.Maybe DeviceMinutes
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Test' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'counters', 'test_counters' - The test\'s result counters.
--
-- 'status', 'test_status' - The test\'s status.
--
-- Allowed values include:
--
-- -   PENDING
--
-- -   PENDING_CONCURRENCY
--
-- -   PENDING_DEVICE
--
-- -   PROCESSING
--
-- -   SCHEDULING
--
-- -   PREPARING
--
-- -   RUNNING
--
-- -   COMPLETED
--
-- -   STOPPING
--
-- 'result', 'test_result' - The test\'s result.
--
-- Allowed values include:
--
-- -   PENDING
--
-- -   PASSED
--
-- -   WARNED
--
-- -   FAILED
--
-- -   SKIPPED
--
-- -   ERRORED
--
-- -   STOPPED
--
-- 'started', 'test_started' - The test\'s start time.
--
-- 'message', 'test_message' - A message about the test\'s result.
--
-- 'arn', 'test_arn' - The test\'s ARN.
--
-- 'name', 'test_name' - The test\'s name.
--
-- 'stopped', 'test_stopped' - The test\'s stop time.
--
-- 'created', 'test_created' - When the test was created.
--
-- 'type'', 'test_type' - The test\'s type.
--
-- Must be one of the following values:
--
-- -   BUILTIN_FUZZ
--
-- -   BUILTIN_EXPLORER
--
--     For Android, an app explorer that traverses an Android app,
--     interacting with it and capturing screenshots at the same time.
--
-- -   APPIUM_JAVA_JUNIT
--
-- -   APPIUM_JAVA_TESTNG
--
-- -   APPIUM_PYTHON
--
-- -   APPIUM_NODE
--
-- -   APPIUM_RUBY
--
-- -   APPIUM_WEB_JAVA_JUNIT
--
-- -   APPIUM_WEB_JAVA_TESTNG
--
-- -   APPIUM_WEB_PYTHON
--
-- -   APPIUM_WEB_NODE
--
-- -   APPIUM_WEB_RUBY
--
-- -   CALABASH
--
-- -   INSTRUMENTATION
--
-- -   UIAUTOMATION
--
-- -   UIAUTOMATOR
--
-- -   XCTEST
--
-- -   XCTEST_UI
--
-- 'deviceMinutes', 'test_deviceMinutes' - Represents the total (metered or unmetered) minutes used by the test.
newTest ::
  Test
newTest =
  Test'
    { counters = Core.Nothing,
      status = Core.Nothing,
      result = Core.Nothing,
      started = Core.Nothing,
      message = Core.Nothing,
      arn = Core.Nothing,
      name = Core.Nothing,
      stopped = Core.Nothing,
      created = Core.Nothing,
      type' = Core.Nothing,
      deviceMinutes = Core.Nothing
    }

-- | The test\'s result counters.
test_counters :: Lens.Lens' Test (Core.Maybe Counters)
test_counters = Lens.lens (\Test' {counters} -> counters) (\s@Test' {} a -> s {counters = a} :: Test)

-- | The test\'s status.
--
-- Allowed values include:
--
-- -   PENDING
--
-- -   PENDING_CONCURRENCY
--
-- -   PENDING_DEVICE
--
-- -   PROCESSING
--
-- -   SCHEDULING
--
-- -   PREPARING
--
-- -   RUNNING
--
-- -   COMPLETED
--
-- -   STOPPING
test_status :: Lens.Lens' Test (Core.Maybe ExecutionStatus)
test_status = Lens.lens (\Test' {status} -> status) (\s@Test' {} a -> s {status = a} :: Test)

-- | The test\'s result.
--
-- Allowed values include:
--
-- -   PENDING
--
-- -   PASSED
--
-- -   WARNED
--
-- -   FAILED
--
-- -   SKIPPED
--
-- -   ERRORED
--
-- -   STOPPED
test_result :: Lens.Lens' Test (Core.Maybe ExecutionResult)
test_result = Lens.lens (\Test' {result} -> result) (\s@Test' {} a -> s {result = a} :: Test)

-- | The test\'s start time.
test_started :: Lens.Lens' Test (Core.Maybe Core.UTCTime)
test_started = Lens.lens (\Test' {started} -> started) (\s@Test' {} a -> s {started = a} :: Test) Core.. Lens.mapping Core._Time

-- | A message about the test\'s result.
test_message :: Lens.Lens' Test (Core.Maybe Core.Text)
test_message = Lens.lens (\Test' {message} -> message) (\s@Test' {} a -> s {message = a} :: Test)

-- | The test\'s ARN.
test_arn :: Lens.Lens' Test (Core.Maybe Core.Text)
test_arn = Lens.lens (\Test' {arn} -> arn) (\s@Test' {} a -> s {arn = a} :: Test)

-- | The test\'s name.
test_name :: Lens.Lens' Test (Core.Maybe Core.Text)
test_name = Lens.lens (\Test' {name} -> name) (\s@Test' {} a -> s {name = a} :: Test)

-- | The test\'s stop time.
test_stopped :: Lens.Lens' Test (Core.Maybe Core.UTCTime)
test_stopped = Lens.lens (\Test' {stopped} -> stopped) (\s@Test' {} a -> s {stopped = a} :: Test) Core.. Lens.mapping Core._Time

-- | When the test was created.
test_created :: Lens.Lens' Test (Core.Maybe Core.UTCTime)
test_created = Lens.lens (\Test' {created} -> created) (\s@Test' {} a -> s {created = a} :: Test) Core.. Lens.mapping Core._Time

-- | The test\'s type.
--
-- Must be one of the following values:
--
-- -   BUILTIN_FUZZ
--
-- -   BUILTIN_EXPLORER
--
--     For Android, an app explorer that traverses an Android app,
--     interacting with it and capturing screenshots at the same time.
--
-- -   APPIUM_JAVA_JUNIT
--
-- -   APPIUM_JAVA_TESTNG
--
-- -   APPIUM_PYTHON
--
-- -   APPIUM_NODE
--
-- -   APPIUM_RUBY
--
-- -   APPIUM_WEB_JAVA_JUNIT
--
-- -   APPIUM_WEB_JAVA_TESTNG
--
-- -   APPIUM_WEB_PYTHON
--
-- -   APPIUM_WEB_NODE
--
-- -   APPIUM_WEB_RUBY
--
-- -   CALABASH
--
-- -   INSTRUMENTATION
--
-- -   UIAUTOMATION
--
-- -   UIAUTOMATOR
--
-- -   XCTEST
--
-- -   XCTEST_UI
test_type :: Lens.Lens' Test (Core.Maybe TestType)
test_type = Lens.lens (\Test' {type'} -> type') (\s@Test' {} a -> s {type' = a} :: Test)

-- | Represents the total (metered or unmetered) minutes used by the test.
test_deviceMinutes :: Lens.Lens' Test (Core.Maybe DeviceMinutes)
test_deviceMinutes = Lens.lens (\Test' {deviceMinutes} -> deviceMinutes) (\s@Test' {} a -> s {deviceMinutes = a} :: Test)

instance Core.FromJSON Test where
  parseJSON =
    Core.withObject
      "Test"
      ( \x ->
          Test'
            Core.<$> (x Core..:? "counters")
            Core.<*> (x Core..:? "status")
            Core.<*> (x Core..:? "result")
            Core.<*> (x Core..:? "started")
            Core.<*> (x Core..:? "message")
            Core.<*> (x Core..:? "arn")
            Core.<*> (x Core..:? "name")
            Core.<*> (x Core..:? "stopped")
            Core.<*> (x Core..:? "created")
            Core.<*> (x Core..:? "type")
            Core.<*> (x Core..:? "deviceMinutes")
      )

instance Core.Hashable Test

instance Core.NFData Test
