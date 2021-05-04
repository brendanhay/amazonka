{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.DeviceFarm.Types.Suite
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DeviceFarm.Types.Suite where

import Network.AWS.DeviceFarm.Types.Counters
import Network.AWS.DeviceFarm.Types.DeviceMinutes
import Network.AWS.DeviceFarm.Types.ExecutionResult
import Network.AWS.DeviceFarm.Types.ExecutionStatus
import Network.AWS.DeviceFarm.Types.TestType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Represents a collection of one or more tests.
--
-- /See:/ 'newSuite' smart constructor.
data Suite = Suite'
  { -- | The suite\'s result counters.
    counters :: Prelude.Maybe Counters,
    -- | The suite\'s status.
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
    -- | The suite\'s result.
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
    -- | The suite\'s start time.
    started :: Prelude.Maybe Prelude.POSIX,
    -- | A message about the suite\'s result.
    message :: Prelude.Maybe Prelude.Text,
    -- | The suite\'s ARN.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The suite\'s name.
    name :: Prelude.Maybe Prelude.Text,
    -- | The suite\'s stop time.
    stopped :: Prelude.Maybe Prelude.POSIX,
    -- | When the suite was created.
    created :: Prelude.Maybe Prelude.POSIX,
    -- | The suite\'s type.
    --
    -- Must be one of the following values:
    --
    -- -   BUILTIN_FUZZ
    --
    -- -   BUILTIN_EXPLORER
    --
    --     Only available for Android; an app explorer that traverses an
    --     Android app, interacting with it and capturing screenshots at the
    --     same time.
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
    type' :: Prelude.Maybe TestType,
    -- | Represents the total (metered or unmetered) minutes used by the test
    -- suite.
    deviceMinutes :: Prelude.Maybe DeviceMinutes
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'Suite' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'counters', 'suite_counters' - The suite\'s result counters.
--
-- 'status', 'suite_status' - The suite\'s status.
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
-- 'result', 'suite_result' - The suite\'s result.
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
-- 'started', 'suite_started' - The suite\'s start time.
--
-- 'message', 'suite_message' - A message about the suite\'s result.
--
-- 'arn', 'suite_arn' - The suite\'s ARN.
--
-- 'name', 'suite_name' - The suite\'s name.
--
-- 'stopped', 'suite_stopped' - The suite\'s stop time.
--
-- 'created', 'suite_created' - When the suite was created.
--
-- 'type'', 'suite_type' - The suite\'s type.
--
-- Must be one of the following values:
--
-- -   BUILTIN_FUZZ
--
-- -   BUILTIN_EXPLORER
--
--     Only available for Android; an app explorer that traverses an
--     Android app, interacting with it and capturing screenshots at the
--     same time.
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
-- 'deviceMinutes', 'suite_deviceMinutes' - Represents the total (metered or unmetered) minutes used by the test
-- suite.
newSuite ::
  Suite
newSuite =
  Suite'
    { counters = Prelude.Nothing,
      status = Prelude.Nothing,
      result = Prelude.Nothing,
      started = Prelude.Nothing,
      message = Prelude.Nothing,
      arn = Prelude.Nothing,
      name = Prelude.Nothing,
      stopped = Prelude.Nothing,
      created = Prelude.Nothing,
      type' = Prelude.Nothing,
      deviceMinutes = Prelude.Nothing
    }

-- | The suite\'s result counters.
suite_counters :: Lens.Lens' Suite (Prelude.Maybe Counters)
suite_counters = Lens.lens (\Suite' {counters} -> counters) (\s@Suite' {} a -> s {counters = a} :: Suite)

-- | The suite\'s status.
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
suite_status :: Lens.Lens' Suite (Prelude.Maybe ExecutionStatus)
suite_status = Lens.lens (\Suite' {status} -> status) (\s@Suite' {} a -> s {status = a} :: Suite)

-- | The suite\'s result.
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
suite_result :: Lens.Lens' Suite (Prelude.Maybe ExecutionResult)
suite_result = Lens.lens (\Suite' {result} -> result) (\s@Suite' {} a -> s {result = a} :: Suite)

-- | The suite\'s start time.
suite_started :: Lens.Lens' Suite (Prelude.Maybe Prelude.UTCTime)
suite_started = Lens.lens (\Suite' {started} -> started) (\s@Suite' {} a -> s {started = a} :: Suite) Prelude.. Lens.mapping Prelude._Time

-- | A message about the suite\'s result.
suite_message :: Lens.Lens' Suite (Prelude.Maybe Prelude.Text)
suite_message = Lens.lens (\Suite' {message} -> message) (\s@Suite' {} a -> s {message = a} :: Suite)

-- | The suite\'s ARN.
suite_arn :: Lens.Lens' Suite (Prelude.Maybe Prelude.Text)
suite_arn = Lens.lens (\Suite' {arn} -> arn) (\s@Suite' {} a -> s {arn = a} :: Suite)

-- | The suite\'s name.
suite_name :: Lens.Lens' Suite (Prelude.Maybe Prelude.Text)
suite_name = Lens.lens (\Suite' {name} -> name) (\s@Suite' {} a -> s {name = a} :: Suite)

-- | The suite\'s stop time.
suite_stopped :: Lens.Lens' Suite (Prelude.Maybe Prelude.UTCTime)
suite_stopped = Lens.lens (\Suite' {stopped} -> stopped) (\s@Suite' {} a -> s {stopped = a} :: Suite) Prelude.. Lens.mapping Prelude._Time

-- | When the suite was created.
suite_created :: Lens.Lens' Suite (Prelude.Maybe Prelude.UTCTime)
suite_created = Lens.lens (\Suite' {created} -> created) (\s@Suite' {} a -> s {created = a} :: Suite) Prelude.. Lens.mapping Prelude._Time

-- | The suite\'s type.
--
-- Must be one of the following values:
--
-- -   BUILTIN_FUZZ
--
-- -   BUILTIN_EXPLORER
--
--     Only available for Android; an app explorer that traverses an
--     Android app, interacting with it and capturing screenshots at the
--     same time.
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
suite_type :: Lens.Lens' Suite (Prelude.Maybe TestType)
suite_type = Lens.lens (\Suite' {type'} -> type') (\s@Suite' {} a -> s {type' = a} :: Suite)

-- | Represents the total (metered or unmetered) minutes used by the test
-- suite.
suite_deviceMinutes :: Lens.Lens' Suite (Prelude.Maybe DeviceMinutes)
suite_deviceMinutes = Lens.lens (\Suite' {deviceMinutes} -> deviceMinutes) (\s@Suite' {} a -> s {deviceMinutes = a} :: Suite)

instance Prelude.FromJSON Suite where
  parseJSON =
    Prelude.withObject
      "Suite"
      ( \x ->
          Suite'
            Prelude.<$> (x Prelude..:? "counters")
            Prelude.<*> (x Prelude..:? "status")
            Prelude.<*> (x Prelude..:? "result")
            Prelude.<*> (x Prelude..:? "started")
            Prelude.<*> (x Prelude..:? "message")
            Prelude.<*> (x Prelude..:? "arn")
            Prelude.<*> (x Prelude..:? "name")
            Prelude.<*> (x Prelude..:? "stopped")
            Prelude.<*> (x Prelude..:? "created")
            Prelude.<*> (x Prelude..:? "type")
            Prelude.<*> (x Prelude..:? "deviceMinutes")
      )

instance Prelude.Hashable Suite

instance Prelude.NFData Suite
