{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.Types.Run
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DeviceFarm.Types.Run
  ( Run (..)
  -- * Smart constructor
  , mkRun
  -- * Lenses
  , rAppUpload
  , rArn
  , rBillingMethod
  , rCompletedJobs
  , rCounters
  , rCreated
  , rCustomerArtifactPaths
  , rDeviceMinutes
  , rDevicePoolArn
  , rDeviceSelectionResult
  , rEventCount
  , rJobTimeoutMinutes
  , rLocale
  , rLocation
  , rMessage
  , rName
  , rNetworkProfile
  , rParsingResultUrl
  , rPlatform
  , rRadios
  , rResult
  , rResultCode
  , rSeed
  , rSkipAppResign
  , rStarted
  , rStatus
  , rStopped
  , rTestSpecArn
  , rTotalJobs
  , rType
  , rWebUrl
  ) where

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
import qualified Network.AWS.DeviceFarm.Types.TestSpecArn as Types
import qualified Network.AWS.DeviceFarm.Types.TestType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents a test run on a set of devices with a given app package, test parameters, and so on.
--
-- /See:/ 'mkRun' smart constructor.
data Run = Run'
  { appUpload :: Core.Maybe Types.AppUpload
    -- ^ An app to upload or that has been uploaded.
  , arn :: Core.Maybe Types.Arn
    -- ^ The run's ARN.
  , billingMethod :: Core.Maybe Types.BillingMethod
    -- ^ Specifies the billing method for a test run: @metered@ or @unmetered@ . If the parameter is not specified, the default value is @metered@ .
  , completedJobs :: Core.Maybe Core.Int
    -- ^ The total number of completed jobs.
  , counters :: Core.Maybe Types.Counters
    -- ^ The run's result counters.
  , created :: Core.Maybe Core.NominalDiffTime
    -- ^ When the run was created.
  , customerArtifactPaths :: Core.Maybe Types.CustomerArtifactPaths
    -- ^ Output @CustomerArtifactPaths@ object for the test run.
  , deviceMinutes :: Core.Maybe Types.DeviceMinutes
    -- ^ Represents the total (metered or unmetered) minutes used by the test run.
  , devicePoolArn :: Core.Maybe Types.DevicePoolArn
    -- ^ The ARN of the device pool for the run.
  , deviceSelectionResult :: Core.Maybe Types.DeviceSelectionResult
    -- ^ The results of a device filter used to select the devices for a test run.
  , eventCount :: Core.Maybe Core.Int
    -- ^ For fuzz tests, this is the number of events, between 1 and 10000, that the UI fuzz test should perform.
  , jobTimeoutMinutes :: Core.Maybe Core.Int
    -- ^ The number of minutes the job executes before it times out.
  , locale :: Core.Maybe Core.Text
    -- ^ Information about the locale that is used for the run.
  , location :: Core.Maybe Types.Location
    -- ^ Information about the location that is used for the run.
  , message :: Core.Maybe Types.Message
    -- ^ A message about the run's result.
  , name :: Core.Maybe Types.Name
    -- ^ The run's name.
  , networkProfile :: Core.Maybe Types.NetworkProfile
    -- ^ The network profile being used for a test run.
  , parsingResultUrl :: Core.Maybe Core.Text
    -- ^ Read-only URL for an object in an S3 bucket where you can get the parsing results of the test package. If the test package doesn't parse, the reason why it doesn't parse appears in the file that this URL points to.
  , platform :: Core.Maybe Types.DevicePlatform
    -- ^ The run's platform.
--
-- Allowed values include:
--
--     * ANDROID
--
--
--     * IOS
--
--
  , radios :: Core.Maybe Types.Radios
    -- ^ Information about the radio states for the run.
  , result :: Core.Maybe Types.ExecutionResult
    -- ^ The run's result.
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
  , resultCode :: Core.Maybe Types.ExecutionResultCode
    -- ^ Supporting field for the result field. Set only if @result@ is @SKIPPED@ . @PARSING_FAILED@ if the result is skipped because of test package parsing failure.
  , seed :: Core.Maybe Core.Int
    -- ^ For fuzz tests, this is a seed to use for randomizing the UI fuzz test. Using the same seed value between tests ensures identical event sequences.
  , skipAppResign :: Core.Maybe Core.Bool
    -- ^ When set to @true@ , for private devices, Device Farm does not sign your app again. For public devices, Device Farm always signs your apps again.
--
-- For more information about how Device Farm re-signs your apps, see <https://aws.amazon.com/device-farm/faq/ Do you modify my app?> in the /AWS Device Farm FAQs/ .
  , started :: Core.Maybe Core.NominalDiffTime
    -- ^ The run's start time.
  , status :: Core.Maybe Types.ExecutionStatus
    -- ^ The run's status.
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
  , stopped :: Core.Maybe Core.NominalDiffTime
    -- ^ The run's stop time.
  , testSpecArn :: Core.Maybe Types.TestSpecArn
    -- ^ The ARN of the YAML-formatted test specification for the run.
  , totalJobs :: Core.Maybe Core.Int
    -- ^ The total number of jobs for the run.
  , type' :: Core.Maybe Types.TestType
    -- ^ The run's type.
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
  , webUrl :: Core.Maybe Core.Text
    -- ^ The Device Farm console URL for the recording of the run.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'Run' value with any optional fields omitted.
mkRun
    :: Run
mkRun
  = Run'{appUpload = Core.Nothing, arn = Core.Nothing,
         billingMethod = Core.Nothing, completedJobs = Core.Nothing,
         counters = Core.Nothing, created = Core.Nothing,
         customerArtifactPaths = Core.Nothing, deviceMinutes = Core.Nothing,
         devicePoolArn = Core.Nothing, deviceSelectionResult = Core.Nothing,
         eventCount = Core.Nothing, jobTimeoutMinutes = Core.Nothing,
         locale = Core.Nothing, location = Core.Nothing,
         message = Core.Nothing, name = Core.Nothing,
         networkProfile = Core.Nothing, parsingResultUrl = Core.Nothing,
         platform = Core.Nothing, radios = Core.Nothing,
         result = Core.Nothing, resultCode = Core.Nothing,
         seed = Core.Nothing, skipAppResign = Core.Nothing,
         started = Core.Nothing, status = Core.Nothing,
         stopped = Core.Nothing, testSpecArn = Core.Nothing,
         totalJobs = Core.Nothing, type' = Core.Nothing,
         webUrl = Core.Nothing}

-- | An app to upload or that has been uploaded.
--
-- /Note:/ Consider using 'appUpload' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rAppUpload :: Lens.Lens' Run (Core.Maybe Types.AppUpload)
rAppUpload = Lens.field @"appUpload"
{-# INLINEABLE rAppUpload #-}
{-# DEPRECATED appUpload "Use generic-lens or generic-optics with 'appUpload' instead"  #-}

-- | The run's ARN.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rArn :: Lens.Lens' Run (Core.Maybe Types.Arn)
rArn = Lens.field @"arn"
{-# INLINEABLE rArn #-}
{-# DEPRECATED arn "Use generic-lens or generic-optics with 'arn' instead"  #-}

-- | Specifies the billing method for a test run: @metered@ or @unmetered@ . If the parameter is not specified, the default value is @metered@ .
--
-- /Note:/ Consider using 'billingMethod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rBillingMethod :: Lens.Lens' Run (Core.Maybe Types.BillingMethod)
rBillingMethod = Lens.field @"billingMethod"
{-# INLINEABLE rBillingMethod #-}
{-# DEPRECATED billingMethod "Use generic-lens or generic-optics with 'billingMethod' instead"  #-}

-- | The total number of completed jobs.
--
-- /Note:/ Consider using 'completedJobs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rCompletedJobs :: Lens.Lens' Run (Core.Maybe Core.Int)
rCompletedJobs = Lens.field @"completedJobs"
{-# INLINEABLE rCompletedJobs #-}
{-# DEPRECATED completedJobs "Use generic-lens or generic-optics with 'completedJobs' instead"  #-}

-- | The run's result counters.
--
-- /Note:/ Consider using 'counters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rCounters :: Lens.Lens' Run (Core.Maybe Types.Counters)
rCounters = Lens.field @"counters"
{-# INLINEABLE rCounters #-}
{-# DEPRECATED counters "Use generic-lens or generic-optics with 'counters' instead"  #-}

-- | When the run was created.
--
-- /Note:/ Consider using 'created' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rCreated :: Lens.Lens' Run (Core.Maybe Core.NominalDiffTime)
rCreated = Lens.field @"created"
{-# INLINEABLE rCreated #-}
{-# DEPRECATED created "Use generic-lens or generic-optics with 'created' instead"  #-}

-- | Output @CustomerArtifactPaths@ object for the test run.
--
-- /Note:/ Consider using 'customerArtifactPaths' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rCustomerArtifactPaths :: Lens.Lens' Run (Core.Maybe Types.CustomerArtifactPaths)
rCustomerArtifactPaths = Lens.field @"customerArtifactPaths"
{-# INLINEABLE rCustomerArtifactPaths #-}
{-# DEPRECATED customerArtifactPaths "Use generic-lens or generic-optics with 'customerArtifactPaths' instead"  #-}

-- | Represents the total (metered or unmetered) minutes used by the test run.
--
-- /Note:/ Consider using 'deviceMinutes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rDeviceMinutes :: Lens.Lens' Run (Core.Maybe Types.DeviceMinutes)
rDeviceMinutes = Lens.field @"deviceMinutes"
{-# INLINEABLE rDeviceMinutes #-}
{-# DEPRECATED deviceMinutes "Use generic-lens or generic-optics with 'deviceMinutes' instead"  #-}

-- | The ARN of the device pool for the run.
--
-- /Note:/ Consider using 'devicePoolArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rDevicePoolArn :: Lens.Lens' Run (Core.Maybe Types.DevicePoolArn)
rDevicePoolArn = Lens.field @"devicePoolArn"
{-# INLINEABLE rDevicePoolArn #-}
{-# DEPRECATED devicePoolArn "Use generic-lens or generic-optics with 'devicePoolArn' instead"  #-}

-- | The results of a device filter used to select the devices for a test run.
--
-- /Note:/ Consider using 'deviceSelectionResult' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rDeviceSelectionResult :: Lens.Lens' Run (Core.Maybe Types.DeviceSelectionResult)
rDeviceSelectionResult = Lens.field @"deviceSelectionResult"
{-# INLINEABLE rDeviceSelectionResult #-}
{-# DEPRECATED deviceSelectionResult "Use generic-lens or generic-optics with 'deviceSelectionResult' instead"  #-}

-- | For fuzz tests, this is the number of events, between 1 and 10000, that the UI fuzz test should perform.
--
-- /Note:/ Consider using 'eventCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rEventCount :: Lens.Lens' Run (Core.Maybe Core.Int)
rEventCount = Lens.field @"eventCount"
{-# INLINEABLE rEventCount #-}
{-# DEPRECATED eventCount "Use generic-lens or generic-optics with 'eventCount' instead"  #-}

-- | The number of minutes the job executes before it times out.
--
-- /Note:/ Consider using 'jobTimeoutMinutes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rJobTimeoutMinutes :: Lens.Lens' Run (Core.Maybe Core.Int)
rJobTimeoutMinutes = Lens.field @"jobTimeoutMinutes"
{-# INLINEABLE rJobTimeoutMinutes #-}
{-# DEPRECATED jobTimeoutMinutes "Use generic-lens or generic-optics with 'jobTimeoutMinutes' instead"  #-}

-- | Information about the locale that is used for the run.
--
-- /Note:/ Consider using 'locale' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rLocale :: Lens.Lens' Run (Core.Maybe Core.Text)
rLocale = Lens.field @"locale"
{-# INLINEABLE rLocale #-}
{-# DEPRECATED locale "Use generic-lens or generic-optics with 'locale' instead"  #-}

-- | Information about the location that is used for the run.
--
-- /Note:/ Consider using 'location' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rLocation :: Lens.Lens' Run (Core.Maybe Types.Location)
rLocation = Lens.field @"location"
{-# INLINEABLE rLocation #-}
{-# DEPRECATED location "Use generic-lens or generic-optics with 'location' instead"  #-}

-- | A message about the run's result.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rMessage :: Lens.Lens' Run (Core.Maybe Types.Message)
rMessage = Lens.field @"message"
{-# INLINEABLE rMessage #-}
{-# DEPRECATED message "Use generic-lens or generic-optics with 'message' instead"  #-}

-- | The run's name.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rName :: Lens.Lens' Run (Core.Maybe Types.Name)
rName = Lens.field @"name"
{-# INLINEABLE rName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The network profile being used for a test run.
--
-- /Note:/ Consider using 'networkProfile' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rNetworkProfile :: Lens.Lens' Run (Core.Maybe Types.NetworkProfile)
rNetworkProfile = Lens.field @"networkProfile"
{-# INLINEABLE rNetworkProfile #-}
{-# DEPRECATED networkProfile "Use generic-lens or generic-optics with 'networkProfile' instead"  #-}

-- | Read-only URL for an object in an S3 bucket where you can get the parsing results of the test package. If the test package doesn't parse, the reason why it doesn't parse appears in the file that this URL points to.
--
-- /Note:/ Consider using 'parsingResultUrl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rParsingResultUrl :: Lens.Lens' Run (Core.Maybe Core.Text)
rParsingResultUrl = Lens.field @"parsingResultUrl"
{-# INLINEABLE rParsingResultUrl #-}
{-# DEPRECATED parsingResultUrl "Use generic-lens or generic-optics with 'parsingResultUrl' instead"  #-}

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
{-# INLINEABLE rPlatform #-}
{-# DEPRECATED platform "Use generic-lens or generic-optics with 'platform' instead"  #-}

-- | Information about the radio states for the run.
--
-- /Note:/ Consider using 'radios' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rRadios :: Lens.Lens' Run (Core.Maybe Types.Radios)
rRadios = Lens.field @"radios"
{-# INLINEABLE rRadios #-}
{-# DEPRECATED radios "Use generic-lens or generic-optics with 'radios' instead"  #-}

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
{-# INLINEABLE rResult #-}
{-# DEPRECATED result "Use generic-lens or generic-optics with 'result' instead"  #-}

-- | Supporting field for the result field. Set only if @result@ is @SKIPPED@ . @PARSING_FAILED@ if the result is skipped because of test package parsing failure.
--
-- /Note:/ Consider using 'resultCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rResultCode :: Lens.Lens' Run (Core.Maybe Types.ExecutionResultCode)
rResultCode = Lens.field @"resultCode"
{-# INLINEABLE rResultCode #-}
{-# DEPRECATED resultCode "Use generic-lens or generic-optics with 'resultCode' instead"  #-}

-- | For fuzz tests, this is a seed to use for randomizing the UI fuzz test. Using the same seed value between tests ensures identical event sequences.
--
-- /Note:/ Consider using 'seed' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rSeed :: Lens.Lens' Run (Core.Maybe Core.Int)
rSeed = Lens.field @"seed"
{-# INLINEABLE rSeed #-}
{-# DEPRECATED seed "Use generic-lens or generic-optics with 'seed' instead"  #-}

-- | When set to @true@ , for private devices, Device Farm does not sign your app again. For public devices, Device Farm always signs your apps again.
--
-- For more information about how Device Farm re-signs your apps, see <https://aws.amazon.com/device-farm/faq/ Do you modify my app?> in the /AWS Device Farm FAQs/ .
--
-- /Note:/ Consider using 'skipAppResign' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rSkipAppResign :: Lens.Lens' Run (Core.Maybe Core.Bool)
rSkipAppResign = Lens.field @"skipAppResign"
{-# INLINEABLE rSkipAppResign #-}
{-# DEPRECATED skipAppResign "Use generic-lens or generic-optics with 'skipAppResign' instead"  #-}

-- | The run's start time.
--
-- /Note:/ Consider using 'started' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rStarted :: Lens.Lens' Run (Core.Maybe Core.NominalDiffTime)
rStarted = Lens.field @"started"
{-# INLINEABLE rStarted #-}
{-# DEPRECATED started "Use generic-lens or generic-optics with 'started' instead"  #-}

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
{-# INLINEABLE rStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

-- | The run's stop time.
--
-- /Note:/ Consider using 'stopped' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rStopped :: Lens.Lens' Run (Core.Maybe Core.NominalDiffTime)
rStopped = Lens.field @"stopped"
{-# INLINEABLE rStopped #-}
{-# DEPRECATED stopped "Use generic-lens or generic-optics with 'stopped' instead"  #-}

-- | The ARN of the YAML-formatted test specification for the run.
--
-- /Note:/ Consider using 'testSpecArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rTestSpecArn :: Lens.Lens' Run (Core.Maybe Types.TestSpecArn)
rTestSpecArn = Lens.field @"testSpecArn"
{-# INLINEABLE rTestSpecArn #-}
{-# DEPRECATED testSpecArn "Use generic-lens or generic-optics with 'testSpecArn' instead"  #-}

-- | The total number of jobs for the run.
--
-- /Note:/ Consider using 'totalJobs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rTotalJobs :: Lens.Lens' Run (Core.Maybe Core.Int)
rTotalJobs = Lens.field @"totalJobs"
{-# INLINEABLE rTotalJobs #-}
{-# DEPRECATED totalJobs "Use generic-lens or generic-optics with 'totalJobs' instead"  #-}

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
{-# INLINEABLE rType #-}
{-# DEPRECATED type' "Use generic-lens or generic-optics with 'type'' instead"  #-}

-- | The Device Farm console URL for the recording of the run.
--
-- /Note:/ Consider using 'webUrl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rWebUrl :: Lens.Lens' Run (Core.Maybe Core.Text)
rWebUrl = Lens.field @"webUrl"
{-# INLINEABLE rWebUrl #-}
{-# DEPRECATED webUrl "Use generic-lens or generic-optics with 'webUrl' instead"  #-}

instance Core.FromJSON Run where
        parseJSON
          = Core.withObject "Run" Core.$
              \ x ->
                Run' Core.<$>
                  (x Core..:? "appUpload") Core.<*> x Core..:? "arn" Core.<*>
                    x Core..:? "billingMethod"
                    Core.<*> x Core..:? "completedJobs"
                    Core.<*> x Core..:? "counters"
                    Core.<*> x Core..:? "created"
                    Core.<*> x Core..:? "customerArtifactPaths"
                    Core.<*> x Core..:? "deviceMinutes"
                    Core.<*> x Core..:? "devicePoolArn"
                    Core.<*> x Core..:? "deviceSelectionResult"
                    Core.<*> x Core..:? "eventCount"
                    Core.<*> x Core..:? "jobTimeoutMinutes"
                    Core.<*> x Core..:? "locale"
                    Core.<*> x Core..:? "location"
                    Core.<*> x Core..:? "message"
                    Core.<*> x Core..:? "name"
                    Core.<*> x Core..:? "networkProfile"
                    Core.<*> x Core..:? "parsingResultUrl"
                    Core.<*> x Core..:? "platform"
                    Core.<*> x Core..:? "radios"
                    Core.<*> x Core..:? "result"
                    Core.<*> x Core..:? "resultCode"
                    Core.<*> x Core..:? "seed"
                    Core.<*> x Core..:? "skipAppResign"
                    Core.<*> x Core..:? "started"
                    Core.<*> x Core..:? "status"
                    Core.<*> x Core..:? "stopped"
                    Core.<*> x Core..:? "testSpecArn"
                    Core.<*> x Core..:? "totalJobs"
                    Core.<*> x Core..:? "type"
                    Core.<*> x Core..:? "webUrl"
