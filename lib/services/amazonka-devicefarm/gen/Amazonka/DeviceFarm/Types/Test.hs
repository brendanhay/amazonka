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
-- Module      : Amazonka.DeviceFarm.Types.Test
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DeviceFarm.Types.Test where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DeviceFarm.Types.Counters
import Amazonka.DeviceFarm.Types.DeviceMinutes
import Amazonka.DeviceFarm.Types.ExecutionResult
import Amazonka.DeviceFarm.Types.ExecutionStatus
import Amazonka.DeviceFarm.Types.TestType
import qualified Amazonka.Prelude as Prelude

-- | Represents a condition that is evaluated.
--
-- /See:/ 'newTest' smart constructor.
data Test = Test'
  { -- | The test\'s ARN.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The test\'s result counters.
    counters :: Prelude.Maybe Counters,
    -- | When the test was created.
    created :: Prelude.Maybe Data.POSIX,
    -- | Represents the total (metered or unmetered) minutes used by the test.
    deviceMinutes :: Prelude.Maybe DeviceMinutes,
    -- | A message about the test\'s result.
    message :: Prelude.Maybe Prelude.Text,
    -- | The test\'s name.
    name :: Prelude.Maybe Prelude.Text,
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
    result :: Prelude.Maybe ExecutionResult,
    -- | The test\'s start time.
    started :: Prelude.Maybe Data.POSIX,
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
    status :: Prelude.Maybe ExecutionStatus,
    -- | The test\'s stop time.
    stopped :: Prelude.Maybe Data.POSIX,
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
    type' :: Prelude.Maybe TestType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Test' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'test_arn' - The test\'s ARN.
--
-- 'counters', 'test_counters' - The test\'s result counters.
--
-- 'created', 'test_created' - When the test was created.
--
-- 'deviceMinutes', 'test_deviceMinutes' - Represents the total (metered or unmetered) minutes used by the test.
--
-- 'message', 'test_message' - A message about the test\'s result.
--
-- 'name', 'test_name' - The test\'s name.
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
-- 'stopped', 'test_stopped' - The test\'s stop time.
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
newTest ::
  Test
newTest =
  Test'
    { arn = Prelude.Nothing,
      counters = Prelude.Nothing,
      created = Prelude.Nothing,
      deviceMinutes = Prelude.Nothing,
      message = Prelude.Nothing,
      name = Prelude.Nothing,
      result = Prelude.Nothing,
      started = Prelude.Nothing,
      status = Prelude.Nothing,
      stopped = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | The test\'s ARN.
test_arn :: Lens.Lens' Test (Prelude.Maybe Prelude.Text)
test_arn = Lens.lens (\Test' {arn} -> arn) (\s@Test' {} a -> s {arn = a} :: Test)

-- | The test\'s result counters.
test_counters :: Lens.Lens' Test (Prelude.Maybe Counters)
test_counters = Lens.lens (\Test' {counters} -> counters) (\s@Test' {} a -> s {counters = a} :: Test)

-- | When the test was created.
test_created :: Lens.Lens' Test (Prelude.Maybe Prelude.UTCTime)
test_created = Lens.lens (\Test' {created} -> created) (\s@Test' {} a -> s {created = a} :: Test) Prelude.. Lens.mapping Data._Time

-- | Represents the total (metered or unmetered) minutes used by the test.
test_deviceMinutes :: Lens.Lens' Test (Prelude.Maybe DeviceMinutes)
test_deviceMinutes = Lens.lens (\Test' {deviceMinutes} -> deviceMinutes) (\s@Test' {} a -> s {deviceMinutes = a} :: Test)

-- | A message about the test\'s result.
test_message :: Lens.Lens' Test (Prelude.Maybe Prelude.Text)
test_message = Lens.lens (\Test' {message} -> message) (\s@Test' {} a -> s {message = a} :: Test)

-- | The test\'s name.
test_name :: Lens.Lens' Test (Prelude.Maybe Prelude.Text)
test_name = Lens.lens (\Test' {name} -> name) (\s@Test' {} a -> s {name = a} :: Test)

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
test_result :: Lens.Lens' Test (Prelude.Maybe ExecutionResult)
test_result = Lens.lens (\Test' {result} -> result) (\s@Test' {} a -> s {result = a} :: Test)

-- | The test\'s start time.
test_started :: Lens.Lens' Test (Prelude.Maybe Prelude.UTCTime)
test_started = Lens.lens (\Test' {started} -> started) (\s@Test' {} a -> s {started = a} :: Test) Prelude.. Lens.mapping Data._Time

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
test_status :: Lens.Lens' Test (Prelude.Maybe ExecutionStatus)
test_status = Lens.lens (\Test' {status} -> status) (\s@Test' {} a -> s {status = a} :: Test)

-- | The test\'s stop time.
test_stopped :: Lens.Lens' Test (Prelude.Maybe Prelude.UTCTime)
test_stopped = Lens.lens (\Test' {stopped} -> stopped) (\s@Test' {} a -> s {stopped = a} :: Test) Prelude.. Lens.mapping Data._Time

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
test_type :: Lens.Lens' Test (Prelude.Maybe TestType)
test_type = Lens.lens (\Test' {type'} -> type') (\s@Test' {} a -> s {type' = a} :: Test)

instance Data.FromJSON Test where
  parseJSON =
    Data.withObject
      "Test"
      ( \x ->
          Test'
            Prelude.<$> (x Data..:? "arn")
            Prelude.<*> (x Data..:? "counters")
            Prelude.<*> (x Data..:? "created")
            Prelude.<*> (x Data..:? "deviceMinutes")
            Prelude.<*> (x Data..:? "message")
            Prelude.<*> (x Data..:? "name")
            Prelude.<*> (x Data..:? "result")
            Prelude.<*> (x Data..:? "started")
            Prelude.<*> (x Data..:? "status")
            Prelude.<*> (x Data..:? "stopped")
            Prelude.<*> (x Data..:? "type")
      )

instance Prelude.Hashable Test where
  hashWithSalt _salt Test' {..} =
    _salt
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` counters
      `Prelude.hashWithSalt` created
      `Prelude.hashWithSalt` deviceMinutes
      `Prelude.hashWithSalt` message
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` result
      `Prelude.hashWithSalt` started
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` stopped
      `Prelude.hashWithSalt` type'

instance Prelude.NFData Test where
  rnf Test' {..} =
    Prelude.rnf arn `Prelude.seq`
      Prelude.rnf counters `Prelude.seq`
        Prelude.rnf created `Prelude.seq`
          Prelude.rnf deviceMinutes `Prelude.seq`
            Prelude.rnf message `Prelude.seq`
              Prelude.rnf name `Prelude.seq`
                Prelude.rnf result `Prelude.seq`
                  Prelude.rnf started `Prelude.seq`
                    Prelude.rnf status `Prelude.seq`
                      Prelude.rnf stopped `Prelude.seq`
                        Prelude.rnf type'
