{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.Types.Run
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DeviceFarm.Types.Run
  ( Run (..),

    -- * Smart constructor
    mkRun,

    -- * Lenses
    rBillingMethod,
    rSkipAppResign,
    rStatus,
    rCustomerArtifactPaths,
    rEventCount,
    rCounters,
    rPlatform,
    rSeed,
    rRadios,
    rArn,
    rLocation,
    rCreated,
    rLocale,
    rTestSpecARN,
    rStopped,
    rResult,
    rJobTimeoutMinutes,
    rCompletedJobs,
    rResultCode,
    rName,
    rAppUpload,
    rParsingResultURL,
    rNetworkProfile,
    rDeviceMinutes,
    rType,
    rMessage,
    rWebURL,
    rTotalJobs,
    rDevicePoolARN,
    rStarted,
    rDeviceSelectionResult,
  )
where

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
import qualified Network.AWS.Prelude as Lude

-- | Represents a test run on a set of devices with a given app package, test parameters, and so on.
--
-- /See:/ 'mkRun' smart constructor.
data Run = Run'
  { -- | Specifies the billing method for a test run: @metered@ or @unmetered@ . If the parameter is not specified, the default value is @metered@ .
    billingMethod :: Lude.Maybe BillingMethod,
    -- | When set to @true@ , for private devices, Device Farm does not sign your app again. For public devices, Device Farm always signs your apps again.
    --
    -- For more information about how Device Farm re-signs your apps, see <https://aws.amazon.com/device-farm/faq/ Do you modify my app?> in the /AWS Device Farm FAQs/ .
    skipAppResign :: Lude.Maybe Lude.Bool,
    -- | The run's status.
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
    status :: Lude.Maybe ExecutionStatus,
    -- | Output @CustomerArtifactPaths@ object for the test run.
    customerArtifactPaths :: Lude.Maybe CustomerArtifactPaths,
    -- | For fuzz tests, this is the number of events, between 1 and 10000, that the UI fuzz test should perform.
    eventCount :: Lude.Maybe Lude.Int,
    -- | The run's result counters.
    counters :: Lude.Maybe Counters,
    -- | The run's platform.
    --
    -- Allowed values include:
    --
    --     * ANDROID
    --
    --
    --     * IOS
    platform :: Lude.Maybe DevicePlatform,
    -- | For fuzz tests, this is a seed to use for randomizing the UI fuzz test. Using the same seed value between tests ensures identical event sequences.
    seed :: Lude.Maybe Lude.Int,
    -- | Information about the radio states for the run.
    radios :: Lude.Maybe Radios,
    -- | The run's ARN.
    arn :: Lude.Maybe Lude.Text,
    -- | Information about the location that is used for the run.
    location :: Lude.Maybe Location,
    -- | When the run was created.
    created :: Lude.Maybe Lude.Timestamp,
    -- | Information about the locale that is used for the run.
    locale :: Lude.Maybe Lude.Text,
    -- | The ARN of the YAML-formatted test specification for the run.
    testSpecARN :: Lude.Maybe Lude.Text,
    -- | The run's stop time.
    stopped :: Lude.Maybe Lude.Timestamp,
    -- | The run's result.
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
    result :: Lude.Maybe ExecutionResult,
    -- | The number of minutes the job executes before it times out.
    jobTimeoutMinutes :: Lude.Maybe Lude.Int,
    -- | The total number of completed jobs.
    completedJobs :: Lude.Maybe Lude.Int,
    -- | Supporting field for the result field. Set only if @result@ is @SKIPPED@ . @PARSING_FAILED@ if the result is skipped because of test package parsing failure.
    resultCode :: Lude.Maybe ExecutionResultCode,
    -- | The run's name.
    name :: Lude.Maybe Lude.Text,
    -- | An app to upload or that has been uploaded.
    appUpload :: Lude.Maybe Lude.Text,
    -- | Read-only URL for an object in an S3 bucket where you can get the parsing results of the test package. If the test package doesn't parse, the reason why it doesn't parse appears in the file that this URL points to.
    parsingResultURL :: Lude.Maybe Lude.Text,
    -- | The network profile being used for a test run.
    networkProfile :: Lude.Maybe NetworkProfile,
    -- | Represents the total (metered or unmetered) minutes used by the test run.
    deviceMinutes :: Lude.Maybe DeviceMinutes,
    -- | The run's type.
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
    type' :: Lude.Maybe TestType,
    -- | A message about the run's result.
    message :: Lude.Maybe Lude.Text,
    -- | The Device Farm console URL for the recording of the run.
    webURL :: Lude.Maybe Lude.Text,
    -- | The total number of jobs for the run.
    totalJobs :: Lude.Maybe Lude.Int,
    -- | The ARN of the device pool for the run.
    devicePoolARN :: Lude.Maybe Lude.Text,
    -- | The run's start time.
    started :: Lude.Maybe Lude.Timestamp,
    -- | The results of a device filter used to select the devices for a test run.
    deviceSelectionResult :: Lude.Maybe DeviceSelectionResult
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Run' with the minimum fields required to make a request.
--
-- * 'billingMethod' - Specifies the billing method for a test run: @metered@ or @unmetered@ . If the parameter is not specified, the default value is @metered@ .
-- * 'skipAppResign' - When set to @true@ , for private devices, Device Farm does not sign your app again. For public devices, Device Farm always signs your apps again.
--
-- For more information about how Device Farm re-signs your apps, see <https://aws.amazon.com/device-farm/faq/ Do you modify my app?> in the /AWS Device Farm FAQs/ .
-- * 'status' - The run's status.
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
-- * 'customerArtifactPaths' - Output @CustomerArtifactPaths@ object for the test run.
-- * 'eventCount' - For fuzz tests, this is the number of events, between 1 and 10000, that the UI fuzz test should perform.
-- * 'counters' - The run's result counters.
-- * 'platform' - The run's platform.
--
-- Allowed values include:
--
--     * ANDROID
--
--
--     * IOS
--
--
-- * 'seed' - For fuzz tests, this is a seed to use for randomizing the UI fuzz test. Using the same seed value between tests ensures identical event sequences.
-- * 'radios' - Information about the radio states for the run.
-- * 'arn' - The run's ARN.
-- * 'location' - Information about the location that is used for the run.
-- * 'created' - When the run was created.
-- * 'locale' - Information about the locale that is used for the run.
-- * 'testSpecARN' - The ARN of the YAML-formatted test specification for the run.
-- * 'stopped' - The run's stop time.
-- * 'result' - The run's result.
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
-- * 'jobTimeoutMinutes' - The number of minutes the job executes before it times out.
-- * 'completedJobs' - The total number of completed jobs.
-- * 'resultCode' - Supporting field for the result field. Set only if @result@ is @SKIPPED@ . @PARSING_FAILED@ if the result is skipped because of test package parsing failure.
-- * 'name' - The run's name.
-- * 'appUpload' - An app to upload or that has been uploaded.
-- * 'parsingResultURL' - Read-only URL for an object in an S3 bucket where you can get the parsing results of the test package. If the test package doesn't parse, the reason why it doesn't parse appears in the file that this URL points to.
-- * 'networkProfile' - The network profile being used for a test run.
-- * 'deviceMinutes' - Represents the total (metered or unmetered) minutes used by the test run.
-- * 'type'' - The run's type.
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
-- * 'message' - A message about the run's result.
-- * 'webURL' - The Device Farm console URL for the recording of the run.
-- * 'totalJobs' - The total number of jobs for the run.
-- * 'devicePoolARN' - The ARN of the device pool for the run.
-- * 'started' - The run's start time.
-- * 'deviceSelectionResult' - The results of a device filter used to select the devices for a test run.
mkRun ::
  Run
mkRun =
  Run'
    { billingMethod = Lude.Nothing,
      skipAppResign = Lude.Nothing,
      status = Lude.Nothing,
      customerArtifactPaths = Lude.Nothing,
      eventCount = Lude.Nothing,
      counters = Lude.Nothing,
      platform = Lude.Nothing,
      seed = Lude.Nothing,
      radios = Lude.Nothing,
      arn = Lude.Nothing,
      location = Lude.Nothing,
      created = Lude.Nothing,
      locale = Lude.Nothing,
      testSpecARN = Lude.Nothing,
      stopped = Lude.Nothing,
      result = Lude.Nothing,
      jobTimeoutMinutes = Lude.Nothing,
      completedJobs = Lude.Nothing,
      resultCode = Lude.Nothing,
      name = Lude.Nothing,
      appUpload = Lude.Nothing,
      parsingResultURL = Lude.Nothing,
      networkProfile = Lude.Nothing,
      deviceMinutes = Lude.Nothing,
      type' = Lude.Nothing,
      message = Lude.Nothing,
      webURL = Lude.Nothing,
      totalJobs = Lude.Nothing,
      devicePoolARN = Lude.Nothing,
      started = Lude.Nothing,
      deviceSelectionResult = Lude.Nothing
    }

-- | Specifies the billing method for a test run: @metered@ or @unmetered@ . If the parameter is not specified, the default value is @metered@ .
--
-- /Note:/ Consider using 'billingMethod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rBillingMethod :: Lens.Lens' Run (Lude.Maybe BillingMethod)
rBillingMethod = Lens.lens (billingMethod :: Run -> Lude.Maybe BillingMethod) (\s a -> s {billingMethod = a} :: Run)
{-# DEPRECATED rBillingMethod "Use generic-lens or generic-optics with 'billingMethod' instead." #-}

-- | When set to @true@ , for private devices, Device Farm does not sign your app again. For public devices, Device Farm always signs your apps again.
--
-- For more information about how Device Farm re-signs your apps, see <https://aws.amazon.com/device-farm/faq/ Do you modify my app?> in the /AWS Device Farm FAQs/ .
--
-- /Note:/ Consider using 'skipAppResign' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rSkipAppResign :: Lens.Lens' Run (Lude.Maybe Lude.Bool)
rSkipAppResign = Lens.lens (skipAppResign :: Run -> Lude.Maybe Lude.Bool) (\s a -> s {skipAppResign = a} :: Run)
{-# DEPRECATED rSkipAppResign "Use generic-lens or generic-optics with 'skipAppResign' instead." #-}

-- | The run's status.
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
rStatus :: Lens.Lens' Run (Lude.Maybe ExecutionStatus)
rStatus = Lens.lens (status :: Run -> Lude.Maybe ExecutionStatus) (\s a -> s {status = a} :: Run)
{-# DEPRECATED rStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | Output @CustomerArtifactPaths@ object for the test run.
--
-- /Note:/ Consider using 'customerArtifactPaths' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rCustomerArtifactPaths :: Lens.Lens' Run (Lude.Maybe CustomerArtifactPaths)
rCustomerArtifactPaths = Lens.lens (customerArtifactPaths :: Run -> Lude.Maybe CustomerArtifactPaths) (\s a -> s {customerArtifactPaths = a} :: Run)
{-# DEPRECATED rCustomerArtifactPaths "Use generic-lens or generic-optics with 'customerArtifactPaths' instead." #-}

-- | For fuzz tests, this is the number of events, between 1 and 10000, that the UI fuzz test should perform.
--
-- /Note:/ Consider using 'eventCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rEventCount :: Lens.Lens' Run (Lude.Maybe Lude.Int)
rEventCount = Lens.lens (eventCount :: Run -> Lude.Maybe Lude.Int) (\s a -> s {eventCount = a} :: Run)
{-# DEPRECATED rEventCount "Use generic-lens or generic-optics with 'eventCount' instead." #-}

-- | The run's result counters.
--
-- /Note:/ Consider using 'counters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rCounters :: Lens.Lens' Run (Lude.Maybe Counters)
rCounters = Lens.lens (counters :: Run -> Lude.Maybe Counters) (\s a -> s {counters = a} :: Run)
{-# DEPRECATED rCounters "Use generic-lens or generic-optics with 'counters' instead." #-}

-- | The run's platform.
--
-- Allowed values include:
--
--     * ANDROID
--
--
--     * IOS
--
--
--
-- /Note:/ Consider using 'platform' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rPlatform :: Lens.Lens' Run (Lude.Maybe DevicePlatform)
rPlatform = Lens.lens (platform :: Run -> Lude.Maybe DevicePlatform) (\s a -> s {platform = a} :: Run)
{-# DEPRECATED rPlatform "Use generic-lens or generic-optics with 'platform' instead." #-}

-- | For fuzz tests, this is a seed to use for randomizing the UI fuzz test. Using the same seed value between tests ensures identical event sequences.
--
-- /Note:/ Consider using 'seed' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rSeed :: Lens.Lens' Run (Lude.Maybe Lude.Int)
rSeed = Lens.lens (seed :: Run -> Lude.Maybe Lude.Int) (\s a -> s {seed = a} :: Run)
{-# DEPRECATED rSeed "Use generic-lens or generic-optics with 'seed' instead." #-}

-- | Information about the radio states for the run.
--
-- /Note:/ Consider using 'radios' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rRadios :: Lens.Lens' Run (Lude.Maybe Radios)
rRadios = Lens.lens (radios :: Run -> Lude.Maybe Radios) (\s a -> s {radios = a} :: Run)
{-# DEPRECATED rRadios "Use generic-lens or generic-optics with 'radios' instead." #-}

-- | The run's ARN.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rArn :: Lens.Lens' Run (Lude.Maybe Lude.Text)
rArn = Lens.lens (arn :: Run -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: Run)
{-# DEPRECATED rArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | Information about the location that is used for the run.
--
-- /Note:/ Consider using 'location' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rLocation :: Lens.Lens' Run (Lude.Maybe Location)
rLocation = Lens.lens (location :: Run -> Lude.Maybe Location) (\s a -> s {location = a} :: Run)
{-# DEPRECATED rLocation "Use generic-lens or generic-optics with 'location' instead." #-}

-- | When the run was created.
--
-- /Note:/ Consider using 'created' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rCreated :: Lens.Lens' Run (Lude.Maybe Lude.Timestamp)
rCreated = Lens.lens (created :: Run -> Lude.Maybe Lude.Timestamp) (\s a -> s {created = a} :: Run)
{-# DEPRECATED rCreated "Use generic-lens or generic-optics with 'created' instead." #-}

-- | Information about the locale that is used for the run.
--
-- /Note:/ Consider using 'locale' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rLocale :: Lens.Lens' Run (Lude.Maybe Lude.Text)
rLocale = Lens.lens (locale :: Run -> Lude.Maybe Lude.Text) (\s a -> s {locale = a} :: Run)
{-# DEPRECATED rLocale "Use generic-lens or generic-optics with 'locale' instead." #-}

-- | The ARN of the YAML-formatted test specification for the run.
--
-- /Note:/ Consider using 'testSpecARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rTestSpecARN :: Lens.Lens' Run (Lude.Maybe Lude.Text)
rTestSpecARN = Lens.lens (testSpecARN :: Run -> Lude.Maybe Lude.Text) (\s a -> s {testSpecARN = a} :: Run)
{-# DEPRECATED rTestSpecARN "Use generic-lens or generic-optics with 'testSpecARN' instead." #-}

-- | The run's stop time.
--
-- /Note:/ Consider using 'stopped' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rStopped :: Lens.Lens' Run (Lude.Maybe Lude.Timestamp)
rStopped = Lens.lens (stopped :: Run -> Lude.Maybe Lude.Timestamp) (\s a -> s {stopped = a} :: Run)
{-# DEPRECATED rStopped "Use generic-lens or generic-optics with 'stopped' instead." #-}

-- | The run's result.
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
rResult :: Lens.Lens' Run (Lude.Maybe ExecutionResult)
rResult = Lens.lens (result :: Run -> Lude.Maybe ExecutionResult) (\s a -> s {result = a} :: Run)
{-# DEPRECATED rResult "Use generic-lens or generic-optics with 'result' instead." #-}

-- | The number of minutes the job executes before it times out.
--
-- /Note:/ Consider using 'jobTimeoutMinutes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rJobTimeoutMinutes :: Lens.Lens' Run (Lude.Maybe Lude.Int)
rJobTimeoutMinutes = Lens.lens (jobTimeoutMinutes :: Run -> Lude.Maybe Lude.Int) (\s a -> s {jobTimeoutMinutes = a} :: Run)
{-# DEPRECATED rJobTimeoutMinutes "Use generic-lens or generic-optics with 'jobTimeoutMinutes' instead." #-}

-- | The total number of completed jobs.
--
-- /Note:/ Consider using 'completedJobs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rCompletedJobs :: Lens.Lens' Run (Lude.Maybe Lude.Int)
rCompletedJobs = Lens.lens (completedJobs :: Run -> Lude.Maybe Lude.Int) (\s a -> s {completedJobs = a} :: Run)
{-# DEPRECATED rCompletedJobs "Use generic-lens or generic-optics with 'completedJobs' instead." #-}

-- | Supporting field for the result field. Set only if @result@ is @SKIPPED@ . @PARSING_FAILED@ if the result is skipped because of test package parsing failure.
--
-- /Note:/ Consider using 'resultCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rResultCode :: Lens.Lens' Run (Lude.Maybe ExecutionResultCode)
rResultCode = Lens.lens (resultCode :: Run -> Lude.Maybe ExecutionResultCode) (\s a -> s {resultCode = a} :: Run)
{-# DEPRECATED rResultCode "Use generic-lens or generic-optics with 'resultCode' instead." #-}

-- | The run's name.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rName :: Lens.Lens' Run (Lude.Maybe Lude.Text)
rName = Lens.lens (name :: Run -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: Run)
{-# DEPRECATED rName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | An app to upload or that has been uploaded.
--
-- /Note:/ Consider using 'appUpload' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rAppUpload :: Lens.Lens' Run (Lude.Maybe Lude.Text)
rAppUpload = Lens.lens (appUpload :: Run -> Lude.Maybe Lude.Text) (\s a -> s {appUpload = a} :: Run)
{-# DEPRECATED rAppUpload "Use generic-lens or generic-optics with 'appUpload' instead." #-}

-- | Read-only URL for an object in an S3 bucket where you can get the parsing results of the test package. If the test package doesn't parse, the reason why it doesn't parse appears in the file that this URL points to.
--
-- /Note:/ Consider using 'parsingResultURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rParsingResultURL :: Lens.Lens' Run (Lude.Maybe Lude.Text)
rParsingResultURL = Lens.lens (parsingResultURL :: Run -> Lude.Maybe Lude.Text) (\s a -> s {parsingResultURL = a} :: Run)
{-# DEPRECATED rParsingResultURL "Use generic-lens or generic-optics with 'parsingResultURL' instead." #-}

-- | The network profile being used for a test run.
--
-- /Note:/ Consider using 'networkProfile' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rNetworkProfile :: Lens.Lens' Run (Lude.Maybe NetworkProfile)
rNetworkProfile = Lens.lens (networkProfile :: Run -> Lude.Maybe NetworkProfile) (\s a -> s {networkProfile = a} :: Run)
{-# DEPRECATED rNetworkProfile "Use generic-lens or generic-optics with 'networkProfile' instead." #-}

-- | Represents the total (metered or unmetered) minutes used by the test run.
--
-- /Note:/ Consider using 'deviceMinutes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rDeviceMinutes :: Lens.Lens' Run (Lude.Maybe DeviceMinutes)
rDeviceMinutes = Lens.lens (deviceMinutes :: Run -> Lude.Maybe DeviceMinutes) (\s a -> s {deviceMinutes = a} :: Run)
{-# DEPRECATED rDeviceMinutes "Use generic-lens or generic-optics with 'deviceMinutes' instead." #-}

-- | The run's type.
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
rType :: Lens.Lens' Run (Lude.Maybe TestType)
rType = Lens.lens (type' :: Run -> Lude.Maybe TestType) (\s a -> s {type' = a} :: Run)
{-# DEPRECATED rType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | A message about the run's result.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rMessage :: Lens.Lens' Run (Lude.Maybe Lude.Text)
rMessage = Lens.lens (message :: Run -> Lude.Maybe Lude.Text) (\s a -> s {message = a} :: Run)
{-# DEPRECATED rMessage "Use generic-lens or generic-optics with 'message' instead." #-}

-- | The Device Farm console URL for the recording of the run.
--
-- /Note:/ Consider using 'webURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rWebURL :: Lens.Lens' Run (Lude.Maybe Lude.Text)
rWebURL = Lens.lens (webURL :: Run -> Lude.Maybe Lude.Text) (\s a -> s {webURL = a} :: Run)
{-# DEPRECATED rWebURL "Use generic-lens or generic-optics with 'webURL' instead." #-}

-- | The total number of jobs for the run.
--
-- /Note:/ Consider using 'totalJobs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rTotalJobs :: Lens.Lens' Run (Lude.Maybe Lude.Int)
rTotalJobs = Lens.lens (totalJobs :: Run -> Lude.Maybe Lude.Int) (\s a -> s {totalJobs = a} :: Run)
{-# DEPRECATED rTotalJobs "Use generic-lens or generic-optics with 'totalJobs' instead." #-}

-- | The ARN of the device pool for the run.
--
-- /Note:/ Consider using 'devicePoolARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rDevicePoolARN :: Lens.Lens' Run (Lude.Maybe Lude.Text)
rDevicePoolARN = Lens.lens (devicePoolARN :: Run -> Lude.Maybe Lude.Text) (\s a -> s {devicePoolARN = a} :: Run)
{-# DEPRECATED rDevicePoolARN "Use generic-lens or generic-optics with 'devicePoolARN' instead." #-}

-- | The run's start time.
--
-- /Note:/ Consider using 'started' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rStarted :: Lens.Lens' Run (Lude.Maybe Lude.Timestamp)
rStarted = Lens.lens (started :: Run -> Lude.Maybe Lude.Timestamp) (\s a -> s {started = a} :: Run)
{-# DEPRECATED rStarted "Use generic-lens or generic-optics with 'started' instead." #-}

-- | The results of a device filter used to select the devices for a test run.
--
-- /Note:/ Consider using 'deviceSelectionResult' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rDeviceSelectionResult :: Lens.Lens' Run (Lude.Maybe DeviceSelectionResult)
rDeviceSelectionResult = Lens.lens (deviceSelectionResult :: Run -> Lude.Maybe DeviceSelectionResult) (\s a -> s {deviceSelectionResult = a} :: Run)
{-# DEPRECATED rDeviceSelectionResult "Use generic-lens or generic-optics with 'deviceSelectionResult' instead." #-}

instance Lude.FromJSON Run where
  parseJSON =
    Lude.withObject
      "Run"
      ( \x ->
          Run'
            Lude.<$> (x Lude..:? "billingMethod")
            Lude.<*> (x Lude..:? "skipAppResign")
            Lude.<*> (x Lude..:? "status")
            Lude.<*> (x Lude..:? "customerArtifactPaths")
            Lude.<*> (x Lude..:? "eventCount")
            Lude.<*> (x Lude..:? "counters")
            Lude.<*> (x Lude..:? "platform")
            Lude.<*> (x Lude..:? "seed")
            Lude.<*> (x Lude..:? "radios")
            Lude.<*> (x Lude..:? "arn")
            Lude.<*> (x Lude..:? "location")
            Lude.<*> (x Lude..:? "created")
            Lude.<*> (x Lude..:? "locale")
            Lude.<*> (x Lude..:? "testSpecArn")
            Lude.<*> (x Lude..:? "stopped")
            Lude.<*> (x Lude..:? "result")
            Lude.<*> (x Lude..:? "jobTimeoutMinutes")
            Lude.<*> (x Lude..:? "completedJobs")
            Lude.<*> (x Lude..:? "resultCode")
            Lude.<*> (x Lude..:? "name")
            Lude.<*> (x Lude..:? "appUpload")
            Lude.<*> (x Lude..:? "parsingResultUrl")
            Lude.<*> (x Lude..:? "networkProfile")
            Lude.<*> (x Lude..:? "deviceMinutes")
            Lude.<*> (x Lude..:? "type")
            Lude.<*> (x Lude..:? "message")
            Lude.<*> (x Lude..:? "webUrl")
            Lude.<*> (x Lude..:? "totalJobs")
            Lude.<*> (x Lude..:? "devicePoolArn")
            Lude.<*> (x Lude..:? "started")
            Lude.<*> (x Lude..:? "deviceSelectionResult")
      )
