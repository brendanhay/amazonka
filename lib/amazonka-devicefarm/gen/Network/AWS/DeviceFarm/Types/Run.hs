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
    rAppUpload,
    rArn,
    rBillingMethod,
    rCompletedJobs,
    rCounters,
    rCreated,
    rCustomerArtifactPaths,
    rDeviceMinutes,
    rDevicePoolArn,
    rDeviceSelectionResult,
    rEventCount,
    rJobTimeoutMinutes,
    rLocale,
    rLocation,
    rMessage,
    rName,
    rNetworkProfile,
    rParsingResultUrl,
    rPlatform,
    rRadios,
    rResult,
    rResultCode,
    rSeed,
    rSkipAppResign,
    rStarted,
    rStatus,
    rStopped,
    rTestSpecArn,
    rTotalJobs,
    rType,
    rWebUrl,
  )
where

import qualified Network.AWS.DeviceFarm.Types.AppUpload as Types
import qualified Network.AWS.DeviceFarm.Types.Arn as Types
import qualified Network.AWS.DeviceFarm.Types.BillingMethod as Types
import qualified Network.AWS.DeviceFarm.Types.Counters as Types
import qualified Network.AWS.DeviceFarm.Types.CustomerArtifactPaths as Types
import qualified Network.AWS.DeviceFarm.Types.DeviceMinutes as Types
import qualified Network.AWS.DeviceFarm.Types.DevicePlatform as Types
import qualified Network.AWS.DeviceFarm.Types.DevicePoolArn as Types
import qualified Network.AWS.DeviceFarm.Types.DeviceSelectionResult as Types
import qualified Network.AWS.DeviceFarm.Types.ExecutionResult as Types
import qualified Network.AWS.DeviceFarm.Types.ExecutionResultCode as Types
import qualified Network.AWS.DeviceFarm.Types.ExecutionStatus as Types
import qualified Network.AWS.DeviceFarm.Types.Location as Types
import qualified Network.AWS.DeviceFarm.Types.Message as Types
import qualified Network.AWS.DeviceFarm.Types.Name as Types
import qualified Network.AWS.DeviceFarm.Types.NetworkProfile as Types
import qualified Network.AWS.DeviceFarm.Types.Radios as Types
import qualified Network.AWS.DeviceFarm.Types.String as Types
import qualified Network.AWS.DeviceFarm.Types.TestSpecArn as Types
import qualified Network.AWS.DeviceFarm.Types.TestType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents a test run on a set of devices with a given app package, test parameters, and so on.
--
-- /See:/ 'mkRun' smart constructor.
data Run = Run'
  { -- | An app to upload or that has been uploaded.
    appUpload :: Core.Maybe Types.AppUpload,
    -- | The run's ARN.
    arn :: Core.Maybe Types.Arn,
    -- | Specifies the billing method for a test run: @metered@ or @unmetered@ . If the parameter is not specified, the default value is @metered@ .
    billingMethod :: Core.Maybe Types.BillingMethod,
    -- | The total number of completed jobs.
    completedJobs :: Core.Maybe Core.Int,
    -- | The run's result counters.
    counters :: Core.Maybe Types.Counters,
    -- | When the run was created.
    created :: Core.Maybe Core.NominalDiffTime,
    -- | Output @CustomerArtifactPaths@ object for the test run.
    customerArtifactPaths :: Core.Maybe Types.CustomerArtifactPaths,
    -- | Represents the total (metered or unmetered) minutes used by the test run.
    deviceMinutes :: Core.Maybe Types.DeviceMinutes,
    -- | The ARN of the device pool for the run.
    devicePoolArn :: Core.Maybe Types.DevicePoolArn,
    -- | The results of a device filter used to select the devices for a test run.
    deviceSelectionResult :: Core.Maybe Types.DeviceSelectionResult,
    -- | For fuzz tests, this is the number of events, between 1 and 10000, that the UI fuzz test should perform.
    eventCount :: Core.Maybe Core.Int,
    -- | The number of minutes the job executes before it times out.
    jobTimeoutMinutes :: Core.Maybe Core.Int,
    -- | Information about the locale that is used for the run.
    locale :: Core.Maybe Types.String,
    -- | Information about the location that is used for the run.
    location :: Core.Maybe Types.Location,
    -- | A message about the run's result.
    message :: Core.Maybe Types.Message,
    -- | The run's name.
    name :: Core.Maybe Types.Name,
    -- | The network profile being used for a test run.
    networkProfile :: Core.Maybe Types.NetworkProfile,
    -- | Read-only URL for an object in an S3 bucket where you can get the parsing results of the test package. If the test package doesn't parse, the reason why it doesn't parse appears in the file that this URL points to.
    parsingResultUrl :: Core.Maybe Types.String,
    -- | The run's platform.
    --
    -- Allowed values include:
    --
    --     * ANDROID
    --
    --
    --     * IOS
    platform :: Core.Maybe Types.DevicePlatform,
    -- | Information about the radio states for the run.
    radios :: Core.Maybe Types.Radios,
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
    result :: Core.Maybe Types.ExecutionResult,
    -- | Supporting field for the result field. Set only if @result@ is @SKIPPED@ . @PARSING_FAILED@ if the result is skipped because of test package parsing failure.
    resultCode :: Core.Maybe Types.ExecutionResultCode,
    -- | For fuzz tests, this is a seed to use for randomizing the UI fuzz test. Using the same seed value between tests ensures identical event sequences.
    seed :: Core.Maybe Core.Int,
    -- | When set to @true@ , for private devices, Device Farm does not sign your app again. For public devices, Device Farm always signs your apps again.
    --
    -- For more information about how Device Farm re-signs your apps, see <https://aws.amazon.com/device-farm/faq/ Do you modify my app?> in the /AWS Device Farm FAQs/ .
    skipAppResign :: Core.Maybe Core.Bool,
    -- | The run's start time.
    started :: Core.Maybe Core.NominalDiffTime,
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
    status :: Core.Maybe Types.ExecutionStatus,
    -- | The run's stop time.
    stopped :: Core.Maybe Core.NominalDiffTime,
    -- | The ARN of the YAML-formatted test specification for the run.
    testSpecArn :: Core.Maybe Types.TestSpecArn,
    -- | The total number of jobs for the run.
    totalJobs :: Core.Maybe Core.Int,
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
    type' :: Core.Maybe Types.TestType,
    -- | The Device Farm console URL for the recording of the run.
    webUrl :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'Run' value with any optional fields omitted.
mkRun ::
  Run
mkRun =
  Run'
    { appUpload = Core.Nothing,
      arn = Core.Nothing,
      billingMethod = Core.Nothing,
      completedJobs = Core.Nothing,
      counters = Core.Nothing,
      created = Core.Nothing,
      customerArtifactPaths = Core.Nothing,
      deviceMinutes = Core.Nothing,
      devicePoolArn = Core.Nothing,
      deviceSelectionResult = Core.Nothing,
      eventCount = Core.Nothing,
      jobTimeoutMinutes = Core.Nothing,
      locale = Core.Nothing,
      location = Core.Nothing,
      message = Core.Nothing,
      name = Core.Nothing,
      networkProfile = Core.Nothing,
      parsingResultUrl = Core.Nothing,
      platform = Core.Nothing,
      radios = Core.Nothing,
      result = Core.Nothing,
      resultCode = Core.Nothing,
      seed = Core.Nothing,
      skipAppResign = Core.Nothing,
      started = Core.Nothing,
      status = Core.Nothing,
      stopped = Core.Nothing,
      testSpecArn = Core.Nothing,
      totalJobs = Core.Nothing,
      type' = Core.Nothing,
      webUrl = Core.Nothing
    }

-- | An app to upload or that has been uploaded.
--
-- /Note:/ Consider using 'appUpload' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rAppUpload :: Lens.Lens' Run (Core.Maybe Types.AppUpload)
rAppUpload = Lens.field @"appUpload"
{-# DEPRECATED rAppUpload "Use generic-lens or generic-optics with 'appUpload' instead." #-}

-- | The run's ARN.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rArn :: Lens.Lens' Run (Core.Maybe Types.Arn)
rArn = Lens.field @"arn"
{-# DEPRECATED rArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | Specifies the billing method for a test run: @metered@ or @unmetered@ . If the parameter is not specified, the default value is @metered@ .
--
-- /Note:/ Consider using 'billingMethod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rBillingMethod :: Lens.Lens' Run (Core.Maybe Types.BillingMethod)
rBillingMethod = Lens.field @"billingMethod"
{-# DEPRECATED rBillingMethod "Use generic-lens or generic-optics with 'billingMethod' instead." #-}

-- | The total number of completed jobs.
--
-- /Note:/ Consider using 'completedJobs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rCompletedJobs :: Lens.Lens' Run (Core.Maybe Core.Int)
rCompletedJobs = Lens.field @"completedJobs"
{-# DEPRECATED rCompletedJobs "Use generic-lens or generic-optics with 'completedJobs' instead." #-}

-- | The run's result counters.
--
-- /Note:/ Consider using 'counters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rCounters :: Lens.Lens' Run (Core.Maybe Types.Counters)
rCounters = Lens.field @"counters"
{-# DEPRECATED rCounters "Use generic-lens or generic-optics with 'counters' instead." #-}

-- | When the run was created.
--
-- /Note:/ Consider using 'created' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rCreated :: Lens.Lens' Run (Core.Maybe Core.NominalDiffTime)
rCreated = Lens.field @"created"
{-# DEPRECATED rCreated "Use generic-lens or generic-optics with 'created' instead." #-}

-- | Output @CustomerArtifactPaths@ object for the test run.
--
-- /Note:/ Consider using 'customerArtifactPaths' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rCustomerArtifactPaths :: Lens.Lens' Run (Core.Maybe Types.CustomerArtifactPaths)
rCustomerArtifactPaths = Lens.field @"customerArtifactPaths"
{-# DEPRECATED rCustomerArtifactPaths "Use generic-lens or generic-optics with 'customerArtifactPaths' instead." #-}

-- | Represents the total (metered or unmetered) minutes used by the test run.
--
-- /Note:/ Consider using 'deviceMinutes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rDeviceMinutes :: Lens.Lens' Run (Core.Maybe Types.DeviceMinutes)
rDeviceMinutes = Lens.field @"deviceMinutes"
{-# DEPRECATED rDeviceMinutes "Use generic-lens or generic-optics with 'deviceMinutes' instead." #-}

-- | The ARN of the device pool for the run.
--
-- /Note:/ Consider using 'devicePoolArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rDevicePoolArn :: Lens.Lens' Run (Core.Maybe Types.DevicePoolArn)
rDevicePoolArn = Lens.field @"devicePoolArn"
{-# DEPRECATED rDevicePoolArn "Use generic-lens or generic-optics with 'devicePoolArn' instead." #-}

-- | The results of a device filter used to select the devices for a test run.
--
-- /Note:/ Consider using 'deviceSelectionResult' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rDeviceSelectionResult :: Lens.Lens' Run (Core.Maybe Types.DeviceSelectionResult)
rDeviceSelectionResult = Lens.field @"deviceSelectionResult"
{-# DEPRECATED rDeviceSelectionResult "Use generic-lens or generic-optics with 'deviceSelectionResult' instead." #-}

-- | For fuzz tests, this is the number of events, between 1 and 10000, that the UI fuzz test should perform.
--
-- /Note:/ Consider using 'eventCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rEventCount :: Lens.Lens' Run (Core.Maybe Core.Int)
rEventCount = Lens.field @"eventCount"
{-# DEPRECATED rEventCount "Use generic-lens or generic-optics with 'eventCount' instead." #-}

-- | The number of minutes the job executes before it times out.
--
-- /Note:/ Consider using 'jobTimeoutMinutes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rJobTimeoutMinutes :: Lens.Lens' Run (Core.Maybe Core.Int)
rJobTimeoutMinutes = Lens.field @"jobTimeoutMinutes"
{-# DEPRECATED rJobTimeoutMinutes "Use generic-lens or generic-optics with 'jobTimeoutMinutes' instead." #-}

-- | Information about the locale that is used for the run.
--
-- /Note:/ Consider using 'locale' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rLocale :: Lens.Lens' Run (Core.Maybe Types.String)
rLocale = Lens.field @"locale"
{-# DEPRECATED rLocale "Use generic-lens or generic-optics with 'locale' instead." #-}

-- | Information about the location that is used for the run.
--
-- /Note:/ Consider using 'location' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rLocation :: Lens.Lens' Run (Core.Maybe Types.Location)
rLocation = Lens.field @"location"
{-# DEPRECATED rLocation "Use generic-lens or generic-optics with 'location' instead." #-}

-- | A message about the run's result.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rMessage :: Lens.Lens' Run (Core.Maybe Types.Message)
rMessage = Lens.field @"message"
{-# DEPRECATED rMessage "Use generic-lens or generic-optics with 'message' instead." #-}

-- | The run's name.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rName :: Lens.Lens' Run (Core.Maybe Types.Name)
rName = Lens.field @"name"
{-# DEPRECATED rName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The network profile being used for a test run.
--
-- /Note:/ Consider using 'networkProfile' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rNetworkProfile :: Lens.Lens' Run (Core.Maybe Types.NetworkProfile)
rNetworkProfile = Lens.field @"networkProfile"
{-# DEPRECATED rNetworkProfile "Use generic-lens or generic-optics with 'networkProfile' instead." #-}

-- | Read-only URL for an object in an S3 bucket where you can get the parsing results of the test package. If the test package doesn't parse, the reason why it doesn't parse appears in the file that this URL points to.
--
-- /Note:/ Consider using 'parsingResultUrl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rParsingResultUrl :: Lens.Lens' Run (Core.Maybe Types.String)
rParsingResultUrl = Lens.field @"parsingResultUrl"
{-# DEPRECATED rParsingResultUrl "Use generic-lens or generic-optics with 'parsingResultUrl' instead." #-}

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
rPlatform :: Lens.Lens' Run (Core.Maybe Types.DevicePlatform)
rPlatform = Lens.field @"platform"
{-# DEPRECATED rPlatform "Use generic-lens or generic-optics with 'platform' instead." #-}

-- | Information about the radio states for the run.
--
-- /Note:/ Consider using 'radios' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rRadios :: Lens.Lens' Run (Core.Maybe Types.Radios)
rRadios = Lens.field @"radios"
{-# DEPRECATED rRadios "Use generic-lens or generic-optics with 'radios' instead." #-}

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
rResult :: Lens.Lens' Run (Core.Maybe Types.ExecutionResult)
rResult = Lens.field @"result"
{-# DEPRECATED rResult "Use generic-lens or generic-optics with 'result' instead." #-}

-- | Supporting field for the result field. Set only if @result@ is @SKIPPED@ . @PARSING_FAILED@ if the result is skipped because of test package parsing failure.
--
-- /Note:/ Consider using 'resultCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rResultCode :: Lens.Lens' Run (Core.Maybe Types.ExecutionResultCode)
rResultCode = Lens.field @"resultCode"
{-# DEPRECATED rResultCode "Use generic-lens or generic-optics with 'resultCode' instead." #-}

-- | For fuzz tests, this is a seed to use for randomizing the UI fuzz test. Using the same seed value between tests ensures identical event sequences.
--
-- /Note:/ Consider using 'seed' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rSeed :: Lens.Lens' Run (Core.Maybe Core.Int)
rSeed = Lens.field @"seed"
{-# DEPRECATED rSeed "Use generic-lens or generic-optics with 'seed' instead." #-}

-- | When set to @true@ , for private devices, Device Farm does not sign your app again. For public devices, Device Farm always signs your apps again.
--
-- For more information about how Device Farm re-signs your apps, see <https://aws.amazon.com/device-farm/faq/ Do you modify my app?> in the /AWS Device Farm FAQs/ .
--
-- /Note:/ Consider using 'skipAppResign' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rSkipAppResign :: Lens.Lens' Run (Core.Maybe Core.Bool)
rSkipAppResign = Lens.field @"skipAppResign"
{-# DEPRECATED rSkipAppResign "Use generic-lens or generic-optics with 'skipAppResign' instead." #-}

-- | The run's start time.
--
-- /Note:/ Consider using 'started' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rStarted :: Lens.Lens' Run (Core.Maybe Core.NominalDiffTime)
rStarted = Lens.field @"started"
{-# DEPRECATED rStarted "Use generic-lens or generic-optics with 'started' instead." #-}

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
rStatus :: Lens.Lens' Run (Core.Maybe Types.ExecutionStatus)
rStatus = Lens.field @"status"
{-# DEPRECATED rStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The run's stop time.
--
-- /Note:/ Consider using 'stopped' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rStopped :: Lens.Lens' Run (Core.Maybe Core.NominalDiffTime)
rStopped = Lens.field @"stopped"
{-# DEPRECATED rStopped "Use generic-lens or generic-optics with 'stopped' instead." #-}

-- | The ARN of the YAML-formatted test specification for the run.
--
-- /Note:/ Consider using 'testSpecArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rTestSpecArn :: Lens.Lens' Run (Core.Maybe Types.TestSpecArn)
rTestSpecArn = Lens.field @"testSpecArn"
{-# DEPRECATED rTestSpecArn "Use generic-lens or generic-optics with 'testSpecArn' instead." #-}

-- | The total number of jobs for the run.
--
-- /Note:/ Consider using 'totalJobs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rTotalJobs :: Lens.Lens' Run (Core.Maybe Core.Int)
rTotalJobs = Lens.field @"totalJobs"
{-# DEPRECATED rTotalJobs "Use generic-lens or generic-optics with 'totalJobs' instead." #-}

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
rType :: Lens.Lens' Run (Core.Maybe Types.TestType)
rType = Lens.field @"type'"
{-# DEPRECATED rType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | The Device Farm console URL for the recording of the run.
--
-- /Note:/ Consider using 'webUrl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rWebUrl :: Lens.Lens' Run (Core.Maybe Types.String)
rWebUrl = Lens.field @"webUrl"
{-# DEPRECATED rWebUrl "Use generic-lens or generic-optics with 'webUrl' instead." #-}

instance Core.FromJSON Run where
  parseJSON =
    Core.withObject "Run" Core.$
      \x ->
        Run'
          Core.<$> (x Core..:? "appUpload")
          Core.<*> (x Core..:? "arn")
          Core.<*> (x Core..:? "billingMethod")
          Core.<*> (x Core..:? "completedJobs")
          Core.<*> (x Core..:? "counters")
          Core.<*> (x Core..:? "created")
          Core.<*> (x Core..:? "customerArtifactPaths")
          Core.<*> (x Core..:? "deviceMinutes")
          Core.<*> (x Core..:? "devicePoolArn")
          Core.<*> (x Core..:? "deviceSelectionResult")
          Core.<*> (x Core..:? "eventCount")
          Core.<*> (x Core..:? "jobTimeoutMinutes")
          Core.<*> (x Core..:? "locale")
          Core.<*> (x Core..:? "location")
          Core.<*> (x Core..:? "message")
          Core.<*> (x Core..:? "name")
          Core.<*> (x Core..:? "networkProfile")
          Core.<*> (x Core..:? "parsingResultUrl")
          Core.<*> (x Core..:? "platform")
          Core.<*> (x Core..:? "radios")
          Core.<*> (x Core..:? "result")
          Core.<*> (x Core..:? "resultCode")
          Core.<*> (x Core..:? "seed")
          Core.<*> (x Core..:? "skipAppResign")
          Core.<*> (x Core..:? "started")
          Core.<*> (x Core..:? "status")
          Core.<*> (x Core..:? "stopped")
          Core.<*> (x Core..:? "testSpecArn")
          Core.<*> (x Core..:? "totalJobs")
          Core.<*> (x Core..:? "type")
          Core.<*> (x Core..:? "webUrl")
