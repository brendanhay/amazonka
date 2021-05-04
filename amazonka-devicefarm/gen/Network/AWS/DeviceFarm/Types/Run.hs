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
-- Module      : Network.AWS.DeviceFarm.Types.Run
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DeviceFarm.Types.Run where

import Network.AWS.DeviceFarm.Types.BillingMethod
import Network.AWS.DeviceFarm.Types.Counters
import Network.AWS.DeviceFarm.Types.CustomerArtifactPaths
import Network.AWS.DeviceFarm.Types.DeviceMinutes
import Network.AWS.DeviceFarm.Types.DevicePlatform
import Network.AWS.DeviceFarm.Types.DeviceSelectionResult
import Network.AWS.DeviceFarm.Types.ExecutionResult
import Network.AWS.DeviceFarm.Types.ExecutionResultCode
import Network.AWS.DeviceFarm.Types.ExecutionStatus
import Network.AWS.DeviceFarm.Types.Location
import Network.AWS.DeviceFarm.Types.NetworkProfile
import Network.AWS.DeviceFarm.Types.Radios
import Network.AWS.DeviceFarm.Types.TestType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Represents a test run on a set of devices with a given app package, test
-- parameters, and so on.
--
-- /See:/ 'newRun' smart constructor.
data Run = Run'
  { -- | For fuzz tests, this is a seed to use for randomizing the UI fuzz test.
    -- Using the same seed value between tests ensures identical event
    -- sequences.
    seed :: Prelude.Maybe Prelude.Int,
    -- | For fuzz tests, this is the number of events, between 1 and 10000, that
    -- the UI fuzz test should perform.
    eventCount :: Prelude.Maybe Prelude.Int,
    -- | The run\'s result counters.
    counters :: Prelude.Maybe Counters,
    -- | The run\'s platform.
    --
    -- Allowed values include:
    --
    -- -   ANDROID
    --
    -- -   IOS
    platform :: Prelude.Maybe DevicePlatform,
    -- | The run\'s status.
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
    -- | The run\'s result.
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
    -- | The ARN of the device pool for the run.
    devicePoolArn :: Prelude.Maybe Prelude.Text,
    -- | The results of a device filter used to select the devices for a test
    -- run.
    deviceSelectionResult :: Prelude.Maybe DeviceSelectionResult,
    -- | The run\'s start time.
    started :: Prelude.Maybe Prelude.POSIX,
    -- | The ARN of the YAML-formatted test specification for the run.
    testSpecArn :: Prelude.Maybe Prelude.Text,
    -- | A message about the run\'s result.
    message :: Prelude.Maybe Prelude.Text,
    -- | Information about the locale that is used for the run.
    locale :: Prelude.Maybe Prelude.Text,
    -- | The run\'s ARN.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The network profile being used for a test run.
    networkProfile :: Prelude.Maybe NetworkProfile,
    -- | An app to upload or that has been uploaded.
    appUpload :: Prelude.Maybe Prelude.Text,
    -- | Information about the radio states for the run.
    radios :: Prelude.Maybe Radios,
    -- | The run\'s name.
    name :: Prelude.Maybe Prelude.Text,
    -- | Specifies the billing method for a test run: @metered@ or @unmetered@.
    -- If the parameter is not specified, the default value is @metered@.
    --
    -- If you have unmetered device slots, you must set this to @unmetered@ to
    -- use them. Otherwise, the run is counted toward metered device minutes.
    billingMethod :: Prelude.Maybe BillingMethod,
    -- | Output @CustomerArtifactPaths@ object for the test run.
    customerArtifactPaths :: Prelude.Maybe CustomerArtifactPaths,
    -- | Supporting field for the result field. Set only if @result@ is
    -- @SKIPPED@. @PARSING_FAILED@ if the result is skipped because of test
    -- package parsing failure.
    resultCode :: Prelude.Maybe ExecutionResultCode,
    -- | When set to @true@, for private devices, Device Farm does not sign your
    -- app again. For public devices, Device Farm always signs your apps again.
    --
    -- For more information about how Device Farm re-signs your apps, see
    -- <https://aws.amazon.com/device-farm/faq/ Do you modify my app?> in the
    -- /AWS Device Farm FAQs/.
    skipAppResign :: Prelude.Maybe Prelude.Bool,
    -- | The total number of completed jobs.
    completedJobs :: Prelude.Maybe Prelude.Int,
    -- | The run\'s stop time.
    stopped :: Prelude.Maybe Prelude.POSIX,
    -- | The number of minutes the job executes before it times out.
    jobTimeoutMinutes :: Prelude.Maybe Prelude.Int,
    -- | The total number of jobs for the run.
    totalJobs :: Prelude.Maybe Prelude.Int,
    -- | The Device Farm console URL for the recording of the run.
    webUrl :: Prelude.Maybe Prelude.Text,
    -- | When the run was created.
    created :: Prelude.Maybe Prelude.POSIX,
    -- | The run\'s type.
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
    type' :: Prelude.Maybe TestType,
    -- | Represents the total (metered or unmetered) minutes used by the test
    -- run.
    deviceMinutes :: Prelude.Maybe DeviceMinutes,
    -- | Information about the location that is used for the run.
    location :: Prelude.Maybe Location,
    -- | Read-only URL for an object in an S3 bucket where you can get the
    -- parsing results of the test package. If the test package doesn\'t parse,
    -- the reason why it doesn\'t parse appears in the file that this URL
    -- points to.
    parsingResultUrl :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'Run' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'seed', 'run_seed' - For fuzz tests, this is a seed to use for randomizing the UI fuzz test.
-- Using the same seed value between tests ensures identical event
-- sequences.
--
-- 'eventCount', 'run_eventCount' - For fuzz tests, this is the number of events, between 1 and 10000, that
-- the UI fuzz test should perform.
--
-- 'counters', 'run_counters' - The run\'s result counters.
--
-- 'platform', 'run_platform' - The run\'s platform.
--
-- Allowed values include:
--
-- -   ANDROID
--
-- -   IOS
--
-- 'status', 'run_status' - The run\'s status.
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
-- 'result', 'run_result' - The run\'s result.
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
-- 'devicePoolArn', 'run_devicePoolArn' - The ARN of the device pool for the run.
--
-- 'deviceSelectionResult', 'run_deviceSelectionResult' - The results of a device filter used to select the devices for a test
-- run.
--
-- 'started', 'run_started' - The run\'s start time.
--
-- 'testSpecArn', 'run_testSpecArn' - The ARN of the YAML-formatted test specification for the run.
--
-- 'message', 'run_message' - A message about the run\'s result.
--
-- 'locale', 'run_locale' - Information about the locale that is used for the run.
--
-- 'arn', 'run_arn' - The run\'s ARN.
--
-- 'networkProfile', 'run_networkProfile' - The network profile being used for a test run.
--
-- 'appUpload', 'run_appUpload' - An app to upload or that has been uploaded.
--
-- 'radios', 'run_radios' - Information about the radio states for the run.
--
-- 'name', 'run_name' - The run\'s name.
--
-- 'billingMethod', 'run_billingMethod' - Specifies the billing method for a test run: @metered@ or @unmetered@.
-- If the parameter is not specified, the default value is @metered@.
--
-- If you have unmetered device slots, you must set this to @unmetered@ to
-- use them. Otherwise, the run is counted toward metered device minutes.
--
-- 'customerArtifactPaths', 'run_customerArtifactPaths' - Output @CustomerArtifactPaths@ object for the test run.
--
-- 'resultCode', 'run_resultCode' - Supporting field for the result field. Set only if @result@ is
-- @SKIPPED@. @PARSING_FAILED@ if the result is skipped because of test
-- package parsing failure.
--
-- 'skipAppResign', 'run_skipAppResign' - When set to @true@, for private devices, Device Farm does not sign your
-- app again. For public devices, Device Farm always signs your apps again.
--
-- For more information about how Device Farm re-signs your apps, see
-- <https://aws.amazon.com/device-farm/faq/ Do you modify my app?> in the
-- /AWS Device Farm FAQs/.
--
-- 'completedJobs', 'run_completedJobs' - The total number of completed jobs.
--
-- 'stopped', 'run_stopped' - The run\'s stop time.
--
-- 'jobTimeoutMinutes', 'run_jobTimeoutMinutes' - The number of minutes the job executes before it times out.
--
-- 'totalJobs', 'run_totalJobs' - The total number of jobs for the run.
--
-- 'webUrl', 'run_webUrl' - The Device Farm console URL for the recording of the run.
--
-- 'created', 'run_created' - When the run was created.
--
-- 'type'', 'run_type' - The run\'s type.
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
-- 'deviceMinutes', 'run_deviceMinutes' - Represents the total (metered or unmetered) minutes used by the test
-- run.
--
-- 'location', 'run_location' - Information about the location that is used for the run.
--
-- 'parsingResultUrl', 'run_parsingResultUrl' - Read-only URL for an object in an S3 bucket where you can get the
-- parsing results of the test package. If the test package doesn\'t parse,
-- the reason why it doesn\'t parse appears in the file that this URL
-- points to.
newRun ::
  Run
newRun =
  Run'
    { seed = Prelude.Nothing,
      eventCount = Prelude.Nothing,
      counters = Prelude.Nothing,
      platform = Prelude.Nothing,
      status = Prelude.Nothing,
      result = Prelude.Nothing,
      devicePoolArn = Prelude.Nothing,
      deviceSelectionResult = Prelude.Nothing,
      started = Prelude.Nothing,
      testSpecArn = Prelude.Nothing,
      message = Prelude.Nothing,
      locale = Prelude.Nothing,
      arn = Prelude.Nothing,
      networkProfile = Prelude.Nothing,
      appUpload = Prelude.Nothing,
      radios = Prelude.Nothing,
      name = Prelude.Nothing,
      billingMethod = Prelude.Nothing,
      customerArtifactPaths = Prelude.Nothing,
      resultCode = Prelude.Nothing,
      skipAppResign = Prelude.Nothing,
      completedJobs = Prelude.Nothing,
      stopped = Prelude.Nothing,
      jobTimeoutMinutes = Prelude.Nothing,
      totalJobs = Prelude.Nothing,
      webUrl = Prelude.Nothing,
      created = Prelude.Nothing,
      type' = Prelude.Nothing,
      deviceMinutes = Prelude.Nothing,
      location = Prelude.Nothing,
      parsingResultUrl = Prelude.Nothing
    }

-- | For fuzz tests, this is a seed to use for randomizing the UI fuzz test.
-- Using the same seed value between tests ensures identical event
-- sequences.
run_seed :: Lens.Lens' Run (Prelude.Maybe Prelude.Int)
run_seed = Lens.lens (\Run' {seed} -> seed) (\s@Run' {} a -> s {seed = a} :: Run)

-- | For fuzz tests, this is the number of events, between 1 and 10000, that
-- the UI fuzz test should perform.
run_eventCount :: Lens.Lens' Run (Prelude.Maybe Prelude.Int)
run_eventCount = Lens.lens (\Run' {eventCount} -> eventCount) (\s@Run' {} a -> s {eventCount = a} :: Run)

-- | The run\'s result counters.
run_counters :: Lens.Lens' Run (Prelude.Maybe Counters)
run_counters = Lens.lens (\Run' {counters} -> counters) (\s@Run' {} a -> s {counters = a} :: Run)

-- | The run\'s platform.
--
-- Allowed values include:
--
-- -   ANDROID
--
-- -   IOS
run_platform :: Lens.Lens' Run (Prelude.Maybe DevicePlatform)
run_platform = Lens.lens (\Run' {platform} -> platform) (\s@Run' {} a -> s {platform = a} :: Run)

-- | The run\'s status.
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
run_status :: Lens.Lens' Run (Prelude.Maybe ExecutionStatus)
run_status = Lens.lens (\Run' {status} -> status) (\s@Run' {} a -> s {status = a} :: Run)

-- | The run\'s result.
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
run_result :: Lens.Lens' Run (Prelude.Maybe ExecutionResult)
run_result = Lens.lens (\Run' {result} -> result) (\s@Run' {} a -> s {result = a} :: Run)

-- | The ARN of the device pool for the run.
run_devicePoolArn :: Lens.Lens' Run (Prelude.Maybe Prelude.Text)
run_devicePoolArn = Lens.lens (\Run' {devicePoolArn} -> devicePoolArn) (\s@Run' {} a -> s {devicePoolArn = a} :: Run)

-- | The results of a device filter used to select the devices for a test
-- run.
run_deviceSelectionResult :: Lens.Lens' Run (Prelude.Maybe DeviceSelectionResult)
run_deviceSelectionResult = Lens.lens (\Run' {deviceSelectionResult} -> deviceSelectionResult) (\s@Run' {} a -> s {deviceSelectionResult = a} :: Run)

-- | The run\'s start time.
run_started :: Lens.Lens' Run (Prelude.Maybe Prelude.UTCTime)
run_started = Lens.lens (\Run' {started} -> started) (\s@Run' {} a -> s {started = a} :: Run) Prelude.. Lens.mapping Prelude._Time

-- | The ARN of the YAML-formatted test specification for the run.
run_testSpecArn :: Lens.Lens' Run (Prelude.Maybe Prelude.Text)
run_testSpecArn = Lens.lens (\Run' {testSpecArn} -> testSpecArn) (\s@Run' {} a -> s {testSpecArn = a} :: Run)

-- | A message about the run\'s result.
run_message :: Lens.Lens' Run (Prelude.Maybe Prelude.Text)
run_message = Lens.lens (\Run' {message} -> message) (\s@Run' {} a -> s {message = a} :: Run)

-- | Information about the locale that is used for the run.
run_locale :: Lens.Lens' Run (Prelude.Maybe Prelude.Text)
run_locale = Lens.lens (\Run' {locale} -> locale) (\s@Run' {} a -> s {locale = a} :: Run)

-- | The run\'s ARN.
run_arn :: Lens.Lens' Run (Prelude.Maybe Prelude.Text)
run_arn = Lens.lens (\Run' {arn} -> arn) (\s@Run' {} a -> s {arn = a} :: Run)

-- | The network profile being used for a test run.
run_networkProfile :: Lens.Lens' Run (Prelude.Maybe NetworkProfile)
run_networkProfile = Lens.lens (\Run' {networkProfile} -> networkProfile) (\s@Run' {} a -> s {networkProfile = a} :: Run)

-- | An app to upload or that has been uploaded.
run_appUpload :: Lens.Lens' Run (Prelude.Maybe Prelude.Text)
run_appUpload = Lens.lens (\Run' {appUpload} -> appUpload) (\s@Run' {} a -> s {appUpload = a} :: Run)

-- | Information about the radio states for the run.
run_radios :: Lens.Lens' Run (Prelude.Maybe Radios)
run_radios = Lens.lens (\Run' {radios} -> radios) (\s@Run' {} a -> s {radios = a} :: Run)

-- | The run\'s name.
run_name :: Lens.Lens' Run (Prelude.Maybe Prelude.Text)
run_name = Lens.lens (\Run' {name} -> name) (\s@Run' {} a -> s {name = a} :: Run)

-- | Specifies the billing method for a test run: @metered@ or @unmetered@.
-- If the parameter is not specified, the default value is @metered@.
--
-- If you have unmetered device slots, you must set this to @unmetered@ to
-- use them. Otherwise, the run is counted toward metered device minutes.
run_billingMethod :: Lens.Lens' Run (Prelude.Maybe BillingMethod)
run_billingMethod = Lens.lens (\Run' {billingMethod} -> billingMethod) (\s@Run' {} a -> s {billingMethod = a} :: Run)

-- | Output @CustomerArtifactPaths@ object for the test run.
run_customerArtifactPaths :: Lens.Lens' Run (Prelude.Maybe CustomerArtifactPaths)
run_customerArtifactPaths = Lens.lens (\Run' {customerArtifactPaths} -> customerArtifactPaths) (\s@Run' {} a -> s {customerArtifactPaths = a} :: Run)

-- | Supporting field for the result field. Set only if @result@ is
-- @SKIPPED@. @PARSING_FAILED@ if the result is skipped because of test
-- package parsing failure.
run_resultCode :: Lens.Lens' Run (Prelude.Maybe ExecutionResultCode)
run_resultCode = Lens.lens (\Run' {resultCode} -> resultCode) (\s@Run' {} a -> s {resultCode = a} :: Run)

-- | When set to @true@, for private devices, Device Farm does not sign your
-- app again. For public devices, Device Farm always signs your apps again.
--
-- For more information about how Device Farm re-signs your apps, see
-- <https://aws.amazon.com/device-farm/faq/ Do you modify my app?> in the
-- /AWS Device Farm FAQs/.
run_skipAppResign :: Lens.Lens' Run (Prelude.Maybe Prelude.Bool)
run_skipAppResign = Lens.lens (\Run' {skipAppResign} -> skipAppResign) (\s@Run' {} a -> s {skipAppResign = a} :: Run)

-- | The total number of completed jobs.
run_completedJobs :: Lens.Lens' Run (Prelude.Maybe Prelude.Int)
run_completedJobs = Lens.lens (\Run' {completedJobs} -> completedJobs) (\s@Run' {} a -> s {completedJobs = a} :: Run)

-- | The run\'s stop time.
run_stopped :: Lens.Lens' Run (Prelude.Maybe Prelude.UTCTime)
run_stopped = Lens.lens (\Run' {stopped} -> stopped) (\s@Run' {} a -> s {stopped = a} :: Run) Prelude.. Lens.mapping Prelude._Time

-- | The number of minutes the job executes before it times out.
run_jobTimeoutMinutes :: Lens.Lens' Run (Prelude.Maybe Prelude.Int)
run_jobTimeoutMinutes = Lens.lens (\Run' {jobTimeoutMinutes} -> jobTimeoutMinutes) (\s@Run' {} a -> s {jobTimeoutMinutes = a} :: Run)

-- | The total number of jobs for the run.
run_totalJobs :: Lens.Lens' Run (Prelude.Maybe Prelude.Int)
run_totalJobs = Lens.lens (\Run' {totalJobs} -> totalJobs) (\s@Run' {} a -> s {totalJobs = a} :: Run)

-- | The Device Farm console URL for the recording of the run.
run_webUrl :: Lens.Lens' Run (Prelude.Maybe Prelude.Text)
run_webUrl = Lens.lens (\Run' {webUrl} -> webUrl) (\s@Run' {} a -> s {webUrl = a} :: Run)

-- | When the run was created.
run_created :: Lens.Lens' Run (Prelude.Maybe Prelude.UTCTime)
run_created = Lens.lens (\Run' {created} -> created) (\s@Run' {} a -> s {created = a} :: Run) Prelude.. Lens.mapping Prelude._Time

-- | The run\'s type.
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
run_type :: Lens.Lens' Run (Prelude.Maybe TestType)
run_type = Lens.lens (\Run' {type'} -> type') (\s@Run' {} a -> s {type' = a} :: Run)

-- | Represents the total (metered or unmetered) minutes used by the test
-- run.
run_deviceMinutes :: Lens.Lens' Run (Prelude.Maybe DeviceMinutes)
run_deviceMinutes = Lens.lens (\Run' {deviceMinutes} -> deviceMinutes) (\s@Run' {} a -> s {deviceMinutes = a} :: Run)

-- | Information about the location that is used for the run.
run_location :: Lens.Lens' Run (Prelude.Maybe Location)
run_location = Lens.lens (\Run' {location} -> location) (\s@Run' {} a -> s {location = a} :: Run)

-- | Read-only URL for an object in an S3 bucket where you can get the
-- parsing results of the test package. If the test package doesn\'t parse,
-- the reason why it doesn\'t parse appears in the file that this URL
-- points to.
run_parsingResultUrl :: Lens.Lens' Run (Prelude.Maybe Prelude.Text)
run_parsingResultUrl = Lens.lens (\Run' {parsingResultUrl} -> parsingResultUrl) (\s@Run' {} a -> s {parsingResultUrl = a} :: Run)

instance Prelude.FromJSON Run where
  parseJSON =
    Prelude.withObject
      "Run"
      ( \x ->
          Run'
            Prelude.<$> (x Prelude..:? "seed")
            Prelude.<*> (x Prelude..:? "eventCount")
            Prelude.<*> (x Prelude..:? "counters")
            Prelude.<*> (x Prelude..:? "platform")
            Prelude.<*> (x Prelude..:? "status")
            Prelude.<*> (x Prelude..:? "result")
            Prelude.<*> (x Prelude..:? "devicePoolArn")
            Prelude.<*> (x Prelude..:? "deviceSelectionResult")
            Prelude.<*> (x Prelude..:? "started")
            Prelude.<*> (x Prelude..:? "testSpecArn")
            Prelude.<*> (x Prelude..:? "message")
            Prelude.<*> (x Prelude..:? "locale")
            Prelude.<*> (x Prelude..:? "arn")
            Prelude.<*> (x Prelude..:? "networkProfile")
            Prelude.<*> (x Prelude..:? "appUpload")
            Prelude.<*> (x Prelude..:? "radios")
            Prelude.<*> (x Prelude..:? "name")
            Prelude.<*> (x Prelude..:? "billingMethod")
            Prelude.<*> (x Prelude..:? "customerArtifactPaths")
            Prelude.<*> (x Prelude..:? "resultCode")
            Prelude.<*> (x Prelude..:? "skipAppResign")
            Prelude.<*> (x Prelude..:? "completedJobs")
            Prelude.<*> (x Prelude..:? "stopped")
            Prelude.<*> (x Prelude..:? "jobTimeoutMinutes")
            Prelude.<*> (x Prelude..:? "totalJobs")
            Prelude.<*> (x Prelude..:? "webUrl")
            Prelude.<*> (x Prelude..:? "created")
            Prelude.<*> (x Prelude..:? "type")
            Prelude.<*> (x Prelude..:? "deviceMinutes")
            Prelude.<*> (x Prelude..:? "location")
            Prelude.<*> (x Prelude..:? "parsingResultUrl")
      )

instance Prelude.Hashable Run

instance Prelude.NFData Run
