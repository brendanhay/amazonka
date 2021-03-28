{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.Types.ScheduleRunTest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DeviceFarm.Types.ScheduleRunTest
  ( ScheduleRunTest (..)
  -- * Smart constructor
  , mkScheduleRunTest
  -- * Lenses
  , srtType
  , srtFilter
  , srtParameters
  , srtTestPackageArn
  , srtTestSpecArn
  ) where

import qualified Network.AWS.DeviceFarm.Types.Filter as Types
import qualified Network.AWS.DeviceFarm.Types.TestPackageArn as Types
import qualified Network.AWS.DeviceFarm.Types.TestSpecArn as Types
import qualified Network.AWS.DeviceFarm.Types.TestType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents test settings. This data structure is passed in as the test parameter to ScheduleRun. For an example of the JSON request syntax, see 'ScheduleRun' .
--
-- /See:/ 'mkScheduleRunTest' smart constructor.
data ScheduleRunTest = ScheduleRunTest'
  { type' :: Types.TestType
    -- ^ The test's type.
--
-- Must be one of the following values:
--
--     * BUILTIN_FUZZ
--
--
--     * BUILTIN_EXPLORER. For Android, an app explorer that traverses an Android app, interacting with it and capturing screenshots at the same time.
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
  , filter :: Core.Maybe Types.Filter
    -- ^ The test's filter.
  , parameters :: Core.Maybe (Core.HashMap Core.Text Core.Text)
    -- ^ The test's parameters, such as test framework parameters and fixture settings. Parameters are represented by name-value pairs of strings.
--
-- For all tests:
--
--     * @app_performance_monitoring@ : Performance monitoring is enabled by default. Set this parameter to false to disable it.
--
--
-- For Calabash tests:
--
--     * profile: A cucumber profile (for example, @my_profile_name@ ).
--
--
--     * tags: You can limit execution to features or scenarios that have (or don't have) certain tags (for example, @smoke or @smoke,~@wip).
--
--
-- For Appium tests (all types):
--
--     * appium_version: The Appium version. Currently supported values are 1.6.5 (and later), latest, and default.
--
--     * latest runs the latest Appium version supported by Device Farm (1.9.1).
--
--
--     * For default, Device Farm selects a compatible version of Appium for the device. The current behavior is to run 1.7.2 on Android devices and iOS 9 and earlier and 1.7.2 for iOS 10 and later.
--
--
--     * This behavior is subject to change.
--
--
--
--
-- For fuzz tests (Android only):
--
--     * event_count: The number of events, between 1 and 10000, that the UI fuzz test should perform.
--
--
--     * throttle: The time, in ms, between 0 and 1000, that the UI fuzz test should wait between events.
--
--
--     * seed: A seed to use for randomizing the UI fuzz test. Using the same seed value between tests ensures identical event sequences.
--
--
-- For Explorer tests:
--
--     * username: A user name to use if the Explorer encounters a login form. If not supplied, no user name is inserted.
--
--
--     * password: A password to use if the Explorer encounters a login form. If not supplied, no password is inserted.
--
--
-- For Instrumentation:
--
--     * filter: A test filter string. Examples:
--
--     * Running a single test case: @com.android.abc.Test1@ 
--
--
--     * Running a single test: @com.android.abc.Test1#smoke@ 
--
--
--     * Running multiple tests: @com.android.abc.Test1,com.android.abc.Test2@ 
--
--
--
--
-- For XCTest and XCTestUI:
--
--     * filter: A test filter string. Examples:
--
--     * Running a single test class: @LoginTests@ 
--
--
--     * Running a multiple test classes: @LoginTests,SmokeTests@ 
--
--
--     * Running a single test: @LoginTests/testValid@ 
--
--
--     * Running multiple tests: @LoginTests/testValid,LoginTests/testInvalid@ 
--
--
--
--
-- For UIAutomator:
--
--     * filter: A test filter string. Examples:
--
--     * Running a single test case: @com.android.abc.Test1@ 
--
--
--     * Running a single test: @com.android.abc.Test1#smoke@ 
--
--
--     * Running multiple tests: @com.android.abc.Test1,com.android.abc.Test2@ 
--
--
--
--
  , testPackageArn :: Core.Maybe Types.TestPackageArn
    -- ^ The ARN of the uploaded test to be run.
  , testSpecArn :: Core.Maybe Types.TestSpecArn
    -- ^ The ARN of the YAML-formatted test specification.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ScheduleRunTest' value with any optional fields omitted.
mkScheduleRunTest
    :: Types.TestType -- ^ 'type\''
    -> ScheduleRunTest
mkScheduleRunTest type'
  = ScheduleRunTest'{type', filter = Core.Nothing,
                     parameters = Core.Nothing, testPackageArn = Core.Nothing,
                     testSpecArn = Core.Nothing}

-- | The test's type.
--
-- Must be one of the following values:
--
--     * BUILTIN_FUZZ
--
--
--     * BUILTIN_EXPLORER. For Android, an app explorer that traverses an Android app, interacting with it and capturing screenshots at the same time.
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
srtType :: Lens.Lens' ScheduleRunTest Types.TestType
srtType = Lens.field @"type'"
{-# INLINEABLE srtType #-}
{-# DEPRECATED type' "Use generic-lens or generic-optics with 'type'' instead"  #-}

-- | The test's filter.
--
-- /Note:/ Consider using 'filter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srtFilter :: Lens.Lens' ScheduleRunTest (Core.Maybe Types.Filter)
srtFilter = Lens.field @"filter"
{-# INLINEABLE srtFilter #-}
{-# DEPRECATED filter "Use generic-lens or generic-optics with 'filter' instead"  #-}

-- | The test's parameters, such as test framework parameters and fixture settings. Parameters are represented by name-value pairs of strings.
--
-- For all tests:
--
--     * @app_performance_monitoring@ : Performance monitoring is enabled by default. Set this parameter to false to disable it.
--
--
-- For Calabash tests:
--
--     * profile: A cucumber profile (for example, @my_profile_name@ ).
--
--
--     * tags: You can limit execution to features or scenarios that have (or don't have) certain tags (for example, @smoke or @smoke,~@wip).
--
--
-- For Appium tests (all types):
--
--     * appium_version: The Appium version. Currently supported values are 1.6.5 (and later), latest, and default.
--
--     * latest runs the latest Appium version supported by Device Farm (1.9.1).
--
--
--     * For default, Device Farm selects a compatible version of Appium for the device. The current behavior is to run 1.7.2 on Android devices and iOS 9 and earlier and 1.7.2 for iOS 10 and later.
--
--
--     * This behavior is subject to change.
--
--
--
--
-- For fuzz tests (Android only):
--
--     * event_count: The number of events, between 1 and 10000, that the UI fuzz test should perform.
--
--
--     * throttle: The time, in ms, between 0 and 1000, that the UI fuzz test should wait between events.
--
--
--     * seed: A seed to use for randomizing the UI fuzz test. Using the same seed value between tests ensures identical event sequences.
--
--
-- For Explorer tests:
--
--     * username: A user name to use if the Explorer encounters a login form. If not supplied, no user name is inserted.
--
--
--     * password: A password to use if the Explorer encounters a login form. If not supplied, no password is inserted.
--
--
-- For Instrumentation:
--
--     * filter: A test filter string. Examples:
--
--     * Running a single test case: @com.android.abc.Test1@ 
--
--
--     * Running a single test: @com.android.abc.Test1#smoke@ 
--
--
--     * Running multiple tests: @com.android.abc.Test1,com.android.abc.Test2@ 
--
--
--
--
-- For XCTest and XCTestUI:
--
--     * filter: A test filter string. Examples:
--
--     * Running a single test class: @LoginTests@ 
--
--
--     * Running a multiple test classes: @LoginTests,SmokeTests@ 
--
--
--     * Running a single test: @LoginTests/testValid@ 
--
--
--     * Running multiple tests: @LoginTests/testValid,LoginTests/testInvalid@ 
--
--
--
--
-- For UIAutomator:
--
--     * filter: A test filter string. Examples:
--
--     * Running a single test case: @com.android.abc.Test1@ 
--
--
--     * Running a single test: @com.android.abc.Test1#smoke@ 
--
--
--     * Running multiple tests: @com.android.abc.Test1,com.android.abc.Test2@ 
--
--
--
--
--
-- /Note:/ Consider using 'parameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srtParameters :: Lens.Lens' ScheduleRunTest (Core.Maybe (Core.HashMap Core.Text Core.Text))
srtParameters = Lens.field @"parameters"
{-# INLINEABLE srtParameters #-}
{-# DEPRECATED parameters "Use generic-lens or generic-optics with 'parameters' instead"  #-}

-- | The ARN of the uploaded test to be run.
--
-- /Note:/ Consider using 'testPackageArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srtTestPackageArn :: Lens.Lens' ScheduleRunTest (Core.Maybe Types.TestPackageArn)
srtTestPackageArn = Lens.field @"testPackageArn"
{-# INLINEABLE srtTestPackageArn #-}
{-# DEPRECATED testPackageArn "Use generic-lens or generic-optics with 'testPackageArn' instead"  #-}

-- | The ARN of the YAML-formatted test specification.
--
-- /Note:/ Consider using 'testSpecArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srtTestSpecArn :: Lens.Lens' ScheduleRunTest (Core.Maybe Types.TestSpecArn)
srtTestSpecArn = Lens.field @"testSpecArn"
{-# INLINEABLE srtTestSpecArn #-}
{-# DEPRECATED testSpecArn "Use generic-lens or generic-optics with 'testSpecArn' instead"  #-}

instance Core.FromJSON ScheduleRunTest where
        toJSON ScheduleRunTest{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("type" Core..= type'),
                  ("filter" Core..=) Core.<$> filter,
                  ("parameters" Core..=) Core.<$> parameters,
                  ("testPackageArn" Core..=) Core.<$> testPackageArn,
                  ("testSpecArn" Core..=) Core.<$> testSpecArn])
