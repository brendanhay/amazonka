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
-- Module      : Amazonka.DeviceFarm.Types.ScheduleRunTest
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DeviceFarm.Types.ScheduleRunTest where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DeviceFarm.Types.TestType
import qualified Amazonka.Prelude as Prelude

-- | Represents test settings. This data structure is passed in as the test
-- parameter to ScheduleRun. For an example of the JSON request syntax, see
-- ScheduleRun.
--
-- /See:/ 'newScheduleRunTest' smart constructor.
data ScheduleRunTest = ScheduleRunTest'
  { -- | The test\'s filter.
    filter' :: Prelude.Maybe Prelude.Text,
    -- | The test\'s parameters, such as test framework parameters and fixture
    -- settings. Parameters are represented by name-value pairs of strings.
    --
    -- For all tests:
    --
    -- -   @app_performance_monitoring@: Performance monitoring is enabled by
    --     default. Set this parameter to false to disable it.
    --
    -- For Calabash tests:
    --
    -- -   profile: A cucumber profile (for example, @my_profile_name@).
    --
    -- -   tags: You can limit execution to features or scenarios that have (or
    --     don\'t have) certain tags (for example, \@smoke or \@smoke,~\@wip).
    --
    -- For Appium tests (all types):
    --
    -- -   appium_version: The Appium version. Currently supported values are
    --     1.6.5 (and later), latest, and default.
    --
    --     -   latest runs the latest Appium version supported by Device Farm
    --         (1.9.1).
    --
    --     -   For default, Device Farm selects a compatible version of Appium
    --         for the device. The current behavior is to run 1.7.2 on Android
    --         devices and iOS 9 and earlier and 1.7.2 for iOS 10 and later.
    --
    --     -   This behavior is subject to change.
    --
    -- For fuzz tests (Android only):
    --
    -- -   event_count: The number of events, between 1 and 10000, that the UI
    --     fuzz test should perform.
    --
    -- -   throttle: The time, in ms, between 0 and 1000, that the UI fuzz test
    --     should wait between events.
    --
    -- -   seed: A seed to use for randomizing the UI fuzz test. Using the same
    --     seed value between tests ensures identical event sequences.
    --
    -- For Explorer tests:
    --
    -- -   username: A user name to use if the Explorer encounters a login
    --     form. If not supplied, no user name is inserted.
    --
    -- -   password: A password to use if the Explorer encounters a login form.
    --     If not supplied, no password is inserted.
    --
    -- For Instrumentation:
    --
    -- -   filter: A test filter string. Examples:
    --
    --     -   Running a single test case: @com.android.abc.Test1@
    --
    --     -   Running a single test: @com.android.abc.Test1#smoke@
    --
    --     -   Running multiple tests:
    --         @com.android.abc.Test1,com.android.abc.Test2@
    --
    -- For XCTest and XCTestUI:
    --
    -- -   filter: A test filter string. Examples:
    --
    --     -   Running a single test class: @LoginTests@
    --
    --     -   Running a multiple test classes: @LoginTests,SmokeTests@
    --
    --     -   Running a single test: @LoginTests\/testValid@
    --
    --     -   Running multiple tests:
    --         @LoginTests\/testValid,LoginTests\/testInvalid@
    --
    -- For UIAutomator:
    --
    -- -   filter: A test filter string. Examples:
    --
    --     -   Running a single test case: @com.android.abc.Test1@
    --
    --     -   Running a single test: @com.android.abc.Test1#smoke@
    --
    --     -   Running multiple tests:
    --         @com.android.abc.Test1,com.android.abc.Test2@
    parameters :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The ARN of the uploaded test to be run.
    testPackageArn :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the YAML-formatted test specification.
    testSpecArn :: Prelude.Maybe Prelude.Text,
    -- | The test\'s type.
    --
    -- Must be one of the following values:
    --
    -- -   BUILTIN_FUZZ
    --
    -- -   BUILTIN_EXPLORER. For Android, an app explorer that traverses an
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
    type' :: TestType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ScheduleRunTest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filter'', 'scheduleRunTest_filter' - The test\'s filter.
--
-- 'parameters', 'scheduleRunTest_parameters' - The test\'s parameters, such as test framework parameters and fixture
-- settings. Parameters are represented by name-value pairs of strings.
--
-- For all tests:
--
-- -   @app_performance_monitoring@: Performance monitoring is enabled by
--     default. Set this parameter to false to disable it.
--
-- For Calabash tests:
--
-- -   profile: A cucumber profile (for example, @my_profile_name@).
--
-- -   tags: You can limit execution to features or scenarios that have (or
--     don\'t have) certain tags (for example, \@smoke or \@smoke,~\@wip).
--
-- For Appium tests (all types):
--
-- -   appium_version: The Appium version. Currently supported values are
--     1.6.5 (and later), latest, and default.
--
--     -   latest runs the latest Appium version supported by Device Farm
--         (1.9.1).
--
--     -   For default, Device Farm selects a compatible version of Appium
--         for the device. The current behavior is to run 1.7.2 on Android
--         devices and iOS 9 and earlier and 1.7.2 for iOS 10 and later.
--
--     -   This behavior is subject to change.
--
-- For fuzz tests (Android only):
--
-- -   event_count: The number of events, between 1 and 10000, that the UI
--     fuzz test should perform.
--
-- -   throttle: The time, in ms, between 0 and 1000, that the UI fuzz test
--     should wait between events.
--
-- -   seed: A seed to use for randomizing the UI fuzz test. Using the same
--     seed value between tests ensures identical event sequences.
--
-- For Explorer tests:
--
-- -   username: A user name to use if the Explorer encounters a login
--     form. If not supplied, no user name is inserted.
--
-- -   password: A password to use if the Explorer encounters a login form.
--     If not supplied, no password is inserted.
--
-- For Instrumentation:
--
-- -   filter: A test filter string. Examples:
--
--     -   Running a single test case: @com.android.abc.Test1@
--
--     -   Running a single test: @com.android.abc.Test1#smoke@
--
--     -   Running multiple tests:
--         @com.android.abc.Test1,com.android.abc.Test2@
--
-- For XCTest and XCTestUI:
--
-- -   filter: A test filter string. Examples:
--
--     -   Running a single test class: @LoginTests@
--
--     -   Running a multiple test classes: @LoginTests,SmokeTests@
--
--     -   Running a single test: @LoginTests\/testValid@
--
--     -   Running multiple tests:
--         @LoginTests\/testValid,LoginTests\/testInvalid@
--
-- For UIAutomator:
--
-- -   filter: A test filter string. Examples:
--
--     -   Running a single test case: @com.android.abc.Test1@
--
--     -   Running a single test: @com.android.abc.Test1#smoke@
--
--     -   Running multiple tests:
--         @com.android.abc.Test1,com.android.abc.Test2@
--
-- 'testPackageArn', 'scheduleRunTest_testPackageArn' - The ARN of the uploaded test to be run.
--
-- 'testSpecArn', 'scheduleRunTest_testSpecArn' - The ARN of the YAML-formatted test specification.
--
-- 'type'', 'scheduleRunTest_type' - The test\'s type.
--
-- Must be one of the following values:
--
-- -   BUILTIN_FUZZ
--
-- -   BUILTIN_EXPLORER. For Android, an app explorer that traverses an
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
newScheduleRunTest ::
  -- | 'type''
  TestType ->
  ScheduleRunTest
newScheduleRunTest pType_ =
  ScheduleRunTest'
    { filter' = Prelude.Nothing,
      parameters = Prelude.Nothing,
      testPackageArn = Prelude.Nothing,
      testSpecArn = Prelude.Nothing,
      type' = pType_
    }

-- | The test\'s filter.
scheduleRunTest_filter :: Lens.Lens' ScheduleRunTest (Prelude.Maybe Prelude.Text)
scheduleRunTest_filter = Lens.lens (\ScheduleRunTest' {filter'} -> filter') (\s@ScheduleRunTest' {} a -> s {filter' = a} :: ScheduleRunTest)

-- | The test\'s parameters, such as test framework parameters and fixture
-- settings. Parameters are represented by name-value pairs of strings.
--
-- For all tests:
--
-- -   @app_performance_monitoring@: Performance monitoring is enabled by
--     default. Set this parameter to false to disable it.
--
-- For Calabash tests:
--
-- -   profile: A cucumber profile (for example, @my_profile_name@).
--
-- -   tags: You can limit execution to features or scenarios that have (or
--     don\'t have) certain tags (for example, \@smoke or \@smoke,~\@wip).
--
-- For Appium tests (all types):
--
-- -   appium_version: The Appium version. Currently supported values are
--     1.6.5 (and later), latest, and default.
--
--     -   latest runs the latest Appium version supported by Device Farm
--         (1.9.1).
--
--     -   For default, Device Farm selects a compatible version of Appium
--         for the device. The current behavior is to run 1.7.2 on Android
--         devices and iOS 9 and earlier and 1.7.2 for iOS 10 and later.
--
--     -   This behavior is subject to change.
--
-- For fuzz tests (Android only):
--
-- -   event_count: The number of events, between 1 and 10000, that the UI
--     fuzz test should perform.
--
-- -   throttle: The time, in ms, between 0 and 1000, that the UI fuzz test
--     should wait between events.
--
-- -   seed: A seed to use for randomizing the UI fuzz test. Using the same
--     seed value between tests ensures identical event sequences.
--
-- For Explorer tests:
--
-- -   username: A user name to use if the Explorer encounters a login
--     form. If not supplied, no user name is inserted.
--
-- -   password: A password to use if the Explorer encounters a login form.
--     If not supplied, no password is inserted.
--
-- For Instrumentation:
--
-- -   filter: A test filter string. Examples:
--
--     -   Running a single test case: @com.android.abc.Test1@
--
--     -   Running a single test: @com.android.abc.Test1#smoke@
--
--     -   Running multiple tests:
--         @com.android.abc.Test1,com.android.abc.Test2@
--
-- For XCTest and XCTestUI:
--
-- -   filter: A test filter string. Examples:
--
--     -   Running a single test class: @LoginTests@
--
--     -   Running a multiple test classes: @LoginTests,SmokeTests@
--
--     -   Running a single test: @LoginTests\/testValid@
--
--     -   Running multiple tests:
--         @LoginTests\/testValid,LoginTests\/testInvalid@
--
-- For UIAutomator:
--
-- -   filter: A test filter string. Examples:
--
--     -   Running a single test case: @com.android.abc.Test1@
--
--     -   Running a single test: @com.android.abc.Test1#smoke@
--
--     -   Running multiple tests:
--         @com.android.abc.Test1,com.android.abc.Test2@
scheduleRunTest_parameters :: Lens.Lens' ScheduleRunTest (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
scheduleRunTest_parameters = Lens.lens (\ScheduleRunTest' {parameters} -> parameters) (\s@ScheduleRunTest' {} a -> s {parameters = a} :: ScheduleRunTest) Prelude.. Lens.mapping Lens.coerced

-- | The ARN of the uploaded test to be run.
scheduleRunTest_testPackageArn :: Lens.Lens' ScheduleRunTest (Prelude.Maybe Prelude.Text)
scheduleRunTest_testPackageArn = Lens.lens (\ScheduleRunTest' {testPackageArn} -> testPackageArn) (\s@ScheduleRunTest' {} a -> s {testPackageArn = a} :: ScheduleRunTest)

-- | The ARN of the YAML-formatted test specification.
scheduleRunTest_testSpecArn :: Lens.Lens' ScheduleRunTest (Prelude.Maybe Prelude.Text)
scheduleRunTest_testSpecArn = Lens.lens (\ScheduleRunTest' {testSpecArn} -> testSpecArn) (\s@ScheduleRunTest' {} a -> s {testSpecArn = a} :: ScheduleRunTest)

-- | The test\'s type.
--
-- Must be one of the following values:
--
-- -   BUILTIN_FUZZ
--
-- -   BUILTIN_EXPLORER. For Android, an app explorer that traverses an
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
scheduleRunTest_type :: Lens.Lens' ScheduleRunTest TestType
scheduleRunTest_type = Lens.lens (\ScheduleRunTest' {type'} -> type') (\s@ScheduleRunTest' {} a -> s {type' = a} :: ScheduleRunTest)

instance Prelude.Hashable ScheduleRunTest where
  hashWithSalt _salt ScheduleRunTest' {..} =
    _salt `Prelude.hashWithSalt` filter'
      `Prelude.hashWithSalt` parameters
      `Prelude.hashWithSalt` testPackageArn
      `Prelude.hashWithSalt` testSpecArn
      `Prelude.hashWithSalt` type'

instance Prelude.NFData ScheduleRunTest where
  rnf ScheduleRunTest' {..} =
    Prelude.rnf filter'
      `Prelude.seq` Prelude.rnf parameters
      `Prelude.seq` Prelude.rnf testPackageArn
      `Prelude.seq` Prelude.rnf testSpecArn
      `Prelude.seq` Prelude.rnf type'

instance Data.ToJSON ScheduleRunTest where
  toJSON ScheduleRunTest' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("filter" Data..=) Prelude.<$> filter',
            ("parameters" Data..=) Prelude.<$> parameters,
            ("testPackageArn" Data..=)
              Prelude.<$> testPackageArn,
            ("testSpecArn" Data..=) Prelude.<$> testSpecArn,
            Prelude.Just ("type" Data..= type')
          ]
      )
