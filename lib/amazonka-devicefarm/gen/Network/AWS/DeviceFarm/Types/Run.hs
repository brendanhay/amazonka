{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.Types.Run
-- Copyright   : (c) 2013-2020 Brendan Hay
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
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents a test run on a set of devices with a given app package, test parameters, and so on.
--
--
--
-- /See:/ 'run' smart constructor.
data Run = Run'
  { _runBillingMethod :: !(Maybe BillingMethod),
    _runSkipAppResign :: !(Maybe Bool),
    _runStatus :: !(Maybe ExecutionStatus),
    _runCustomerArtifactPaths :: !(Maybe CustomerArtifactPaths),
    _runEventCount :: !(Maybe Int),
    _runCounters :: !(Maybe Counters),
    _runPlatform :: !(Maybe DevicePlatform),
    _runSeed :: !(Maybe Int),
    _runRadios :: !(Maybe Radios),
    _runArn :: !(Maybe Text),
    _runLocation :: !(Maybe Location),
    _runCreated :: !(Maybe POSIX),
    _runLocale :: !(Maybe Text),
    _runTestSpecARN :: !(Maybe Text),
    _runStopped :: !(Maybe POSIX),
    _runResult :: !(Maybe ExecutionResult),
    _runJobTimeoutMinutes :: !(Maybe Int),
    _runCompletedJobs :: !(Maybe Int),
    _runResultCode :: !(Maybe ExecutionResultCode),
    _runName :: !(Maybe Text),
    _runAppUpload :: !(Maybe Text),
    _runParsingResultURL :: !(Maybe Text),
    _runNetworkProfile :: !(Maybe NetworkProfile),
    _runDeviceMinutes :: !(Maybe DeviceMinutes),
    _runType :: !(Maybe TestType),
    _runMessage :: !(Maybe Text),
    _runWebURL :: !(Maybe Text),
    _runTotalJobs :: !(Maybe Int),
    _runDevicePoolARN :: !(Maybe Text),
    _runStarted :: !(Maybe POSIX),
    _runDeviceSelectionResult :: !(Maybe DeviceSelectionResult)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Run' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'runBillingMethod' - Specifies the billing method for a test run: @metered@ or @unmetered@ . If the parameter is not specified, the default value is @metered@ .
--
-- * 'runSkipAppResign' - When set to @true@ , for private devices, Device Farm does not sign your app again. For public devices, Device Farm always signs your apps again. For more information about how Device Farm re-signs your apps, see <https://aws.amazon.com/device-farm/faq/ Do you modify my app?> in the /AWS Device Farm FAQs/ .
--
-- * 'runStatus' - The run's status. Allowed values include:     * PENDING     * PENDING_CONCURRENCY     * PENDING_DEVICE     * PROCESSING     * SCHEDULING     * PREPARING     * RUNNING     * COMPLETED     * STOPPING
--
-- * 'runCustomerArtifactPaths' - Output @CustomerArtifactPaths@ object for the test run.
--
-- * 'runEventCount' - For fuzz tests, this is the number of events, between 1 and 10000, that the UI fuzz test should perform.
--
-- * 'runCounters' - The run's result counters.
--
-- * 'runPlatform' - The run's platform. Allowed values include:     * ANDROID     * IOS
--
-- * 'runSeed' - For fuzz tests, this is a seed to use for randomizing the UI fuzz test. Using the same seed value between tests ensures identical event sequences.
--
-- * 'runRadios' - Information about the radio states for the run.
--
-- * 'runArn' - The run's ARN.
--
-- * 'runLocation' - Information about the location that is used for the run.
--
-- * 'runCreated' - When the run was created.
--
-- * 'runLocale' - Information about the locale that is used for the run.
--
-- * 'runTestSpecARN' - The ARN of the YAML-formatted test specification for the run.
--
-- * 'runStopped' - The run's stop time.
--
-- * 'runResult' - The run's result. Allowed values include:     * PENDING     * PASSED     * WARNED     * FAILED     * SKIPPED     * ERRORED     * STOPPED
--
-- * 'runJobTimeoutMinutes' - The number of minutes the job executes before it times out.
--
-- * 'runCompletedJobs' - The total number of completed jobs.
--
-- * 'runResultCode' - Supporting field for the result field. Set only if @result@ is @SKIPPED@ . @PARSING_FAILED@ if the result is skipped because of test package parsing failure.
--
-- * 'runName' - The run's name.
--
-- * 'runAppUpload' - An app to upload or that has been uploaded.
--
-- * 'runParsingResultURL' - Read-only URL for an object in an S3 bucket where you can get the parsing results of the test package. If the test package doesn't parse, the reason why it doesn't parse appears in the file that this URL points to.
--
-- * 'runNetworkProfile' - The network profile being used for a test run.
--
-- * 'runDeviceMinutes' - Represents the total (metered or unmetered) minutes used by the test run.
--
-- * 'runType' - The run's type. Must be one of the following values:     * BUILTIN_FUZZ     * BUILTIN_EXPLORER     * APPIUM_JAVA_JUNIT     * APPIUM_JAVA_TESTNG     * APPIUM_PYTHON     * APPIUM_NODE     * APPIUM_RUBY     * APPIUM_WEB_JAVA_JUNIT     * APPIUM_WEB_JAVA_TESTNG     * APPIUM_WEB_PYTHON     * APPIUM_WEB_NODE     * APPIUM_WEB_RUBY     * CALABASH     * INSTRUMENTATION     * UIAUTOMATION     * UIAUTOMATOR     * XCTEST     * XCTEST_UI
--
-- * 'runMessage' - A message about the run's result.
--
-- * 'runWebURL' - The Device Farm console URL for the recording of the run.
--
-- * 'runTotalJobs' - The total number of jobs for the run.
--
-- * 'runDevicePoolARN' - The ARN of the device pool for the run.
--
-- * 'runStarted' - The run's start time.
--
-- * 'runDeviceSelectionResult' - The results of a device filter used to select the devices for a test run.
run ::
  Run
run =
  Run'
    { _runBillingMethod = Nothing,
      _runSkipAppResign = Nothing,
      _runStatus = Nothing,
      _runCustomerArtifactPaths = Nothing,
      _runEventCount = Nothing,
      _runCounters = Nothing,
      _runPlatform = Nothing,
      _runSeed = Nothing,
      _runRadios = Nothing,
      _runArn = Nothing,
      _runLocation = Nothing,
      _runCreated = Nothing,
      _runLocale = Nothing,
      _runTestSpecARN = Nothing,
      _runStopped = Nothing,
      _runResult = Nothing,
      _runJobTimeoutMinutes = Nothing,
      _runCompletedJobs = Nothing,
      _runResultCode = Nothing,
      _runName = Nothing,
      _runAppUpload = Nothing,
      _runParsingResultURL = Nothing,
      _runNetworkProfile = Nothing,
      _runDeviceMinutes = Nothing,
      _runType = Nothing,
      _runMessage = Nothing,
      _runWebURL = Nothing,
      _runTotalJobs = Nothing,
      _runDevicePoolARN = Nothing,
      _runStarted = Nothing,
      _runDeviceSelectionResult = Nothing
    }

-- | Specifies the billing method for a test run: @metered@ or @unmetered@ . If the parameter is not specified, the default value is @metered@ .
runBillingMethod :: Lens' Run (Maybe BillingMethod)
runBillingMethod = lens _runBillingMethod (\s a -> s {_runBillingMethod = a})

-- | When set to @true@ , for private devices, Device Farm does not sign your app again. For public devices, Device Farm always signs your apps again. For more information about how Device Farm re-signs your apps, see <https://aws.amazon.com/device-farm/faq/ Do you modify my app?> in the /AWS Device Farm FAQs/ .
runSkipAppResign :: Lens' Run (Maybe Bool)
runSkipAppResign = lens _runSkipAppResign (\s a -> s {_runSkipAppResign = a})

-- | The run's status. Allowed values include:     * PENDING     * PENDING_CONCURRENCY     * PENDING_DEVICE     * PROCESSING     * SCHEDULING     * PREPARING     * RUNNING     * COMPLETED     * STOPPING
runStatus :: Lens' Run (Maybe ExecutionStatus)
runStatus = lens _runStatus (\s a -> s {_runStatus = a})

-- | Output @CustomerArtifactPaths@ object for the test run.
runCustomerArtifactPaths :: Lens' Run (Maybe CustomerArtifactPaths)
runCustomerArtifactPaths = lens _runCustomerArtifactPaths (\s a -> s {_runCustomerArtifactPaths = a})

-- | For fuzz tests, this is the number of events, between 1 and 10000, that the UI fuzz test should perform.
runEventCount :: Lens' Run (Maybe Int)
runEventCount = lens _runEventCount (\s a -> s {_runEventCount = a})

-- | The run's result counters.
runCounters :: Lens' Run (Maybe Counters)
runCounters = lens _runCounters (\s a -> s {_runCounters = a})

-- | The run's platform. Allowed values include:     * ANDROID     * IOS
runPlatform :: Lens' Run (Maybe DevicePlatform)
runPlatform = lens _runPlatform (\s a -> s {_runPlatform = a})

-- | For fuzz tests, this is a seed to use for randomizing the UI fuzz test. Using the same seed value between tests ensures identical event sequences.
runSeed :: Lens' Run (Maybe Int)
runSeed = lens _runSeed (\s a -> s {_runSeed = a})

-- | Information about the radio states for the run.
runRadios :: Lens' Run (Maybe Radios)
runRadios = lens _runRadios (\s a -> s {_runRadios = a})

-- | The run's ARN.
runArn :: Lens' Run (Maybe Text)
runArn = lens _runArn (\s a -> s {_runArn = a})

-- | Information about the location that is used for the run.
runLocation :: Lens' Run (Maybe Location)
runLocation = lens _runLocation (\s a -> s {_runLocation = a})

-- | When the run was created.
runCreated :: Lens' Run (Maybe UTCTime)
runCreated = lens _runCreated (\s a -> s {_runCreated = a}) . mapping _Time

-- | Information about the locale that is used for the run.
runLocale :: Lens' Run (Maybe Text)
runLocale = lens _runLocale (\s a -> s {_runLocale = a})

-- | The ARN of the YAML-formatted test specification for the run.
runTestSpecARN :: Lens' Run (Maybe Text)
runTestSpecARN = lens _runTestSpecARN (\s a -> s {_runTestSpecARN = a})

-- | The run's stop time.
runStopped :: Lens' Run (Maybe UTCTime)
runStopped = lens _runStopped (\s a -> s {_runStopped = a}) . mapping _Time

-- | The run's result. Allowed values include:     * PENDING     * PASSED     * WARNED     * FAILED     * SKIPPED     * ERRORED     * STOPPED
runResult :: Lens' Run (Maybe ExecutionResult)
runResult = lens _runResult (\s a -> s {_runResult = a})

-- | The number of minutes the job executes before it times out.
runJobTimeoutMinutes :: Lens' Run (Maybe Int)
runJobTimeoutMinutes = lens _runJobTimeoutMinutes (\s a -> s {_runJobTimeoutMinutes = a})

-- | The total number of completed jobs.
runCompletedJobs :: Lens' Run (Maybe Int)
runCompletedJobs = lens _runCompletedJobs (\s a -> s {_runCompletedJobs = a})

-- | Supporting field for the result field. Set only if @result@ is @SKIPPED@ . @PARSING_FAILED@ if the result is skipped because of test package parsing failure.
runResultCode :: Lens' Run (Maybe ExecutionResultCode)
runResultCode = lens _runResultCode (\s a -> s {_runResultCode = a})

-- | The run's name.
runName :: Lens' Run (Maybe Text)
runName = lens _runName (\s a -> s {_runName = a})

-- | An app to upload or that has been uploaded.
runAppUpload :: Lens' Run (Maybe Text)
runAppUpload = lens _runAppUpload (\s a -> s {_runAppUpload = a})

-- | Read-only URL for an object in an S3 bucket where you can get the parsing results of the test package. If the test package doesn't parse, the reason why it doesn't parse appears in the file that this URL points to.
runParsingResultURL :: Lens' Run (Maybe Text)
runParsingResultURL = lens _runParsingResultURL (\s a -> s {_runParsingResultURL = a})

-- | The network profile being used for a test run.
runNetworkProfile :: Lens' Run (Maybe NetworkProfile)
runNetworkProfile = lens _runNetworkProfile (\s a -> s {_runNetworkProfile = a})

-- | Represents the total (metered or unmetered) minutes used by the test run.
runDeviceMinutes :: Lens' Run (Maybe DeviceMinutes)
runDeviceMinutes = lens _runDeviceMinutes (\s a -> s {_runDeviceMinutes = a})

-- | The run's type. Must be one of the following values:     * BUILTIN_FUZZ     * BUILTIN_EXPLORER     * APPIUM_JAVA_JUNIT     * APPIUM_JAVA_TESTNG     * APPIUM_PYTHON     * APPIUM_NODE     * APPIUM_RUBY     * APPIUM_WEB_JAVA_JUNIT     * APPIUM_WEB_JAVA_TESTNG     * APPIUM_WEB_PYTHON     * APPIUM_WEB_NODE     * APPIUM_WEB_RUBY     * CALABASH     * INSTRUMENTATION     * UIAUTOMATION     * UIAUTOMATOR     * XCTEST     * XCTEST_UI
runType :: Lens' Run (Maybe TestType)
runType = lens _runType (\s a -> s {_runType = a})

-- | A message about the run's result.
runMessage :: Lens' Run (Maybe Text)
runMessage = lens _runMessage (\s a -> s {_runMessage = a})

-- | The Device Farm console URL for the recording of the run.
runWebURL :: Lens' Run (Maybe Text)
runWebURL = lens _runWebURL (\s a -> s {_runWebURL = a})

-- | The total number of jobs for the run.
runTotalJobs :: Lens' Run (Maybe Int)
runTotalJobs = lens _runTotalJobs (\s a -> s {_runTotalJobs = a})

-- | The ARN of the device pool for the run.
runDevicePoolARN :: Lens' Run (Maybe Text)
runDevicePoolARN = lens _runDevicePoolARN (\s a -> s {_runDevicePoolARN = a})

-- | The run's start time.
runStarted :: Lens' Run (Maybe UTCTime)
runStarted = lens _runStarted (\s a -> s {_runStarted = a}) . mapping _Time

-- | The results of a device filter used to select the devices for a test run.
runDeviceSelectionResult :: Lens' Run (Maybe DeviceSelectionResult)
runDeviceSelectionResult = lens _runDeviceSelectionResult (\s a -> s {_runDeviceSelectionResult = a})

instance FromJSON Run where
  parseJSON =
    withObject
      "Run"
      ( \x ->
          Run'
            <$> (x .:? "billingMethod")
            <*> (x .:? "skipAppResign")
            <*> (x .:? "status")
            <*> (x .:? "customerArtifactPaths")
            <*> (x .:? "eventCount")
            <*> (x .:? "counters")
            <*> (x .:? "platform")
            <*> (x .:? "seed")
            <*> (x .:? "radios")
            <*> (x .:? "arn")
            <*> (x .:? "location")
            <*> (x .:? "created")
            <*> (x .:? "locale")
            <*> (x .:? "testSpecArn")
            <*> (x .:? "stopped")
            <*> (x .:? "result")
            <*> (x .:? "jobTimeoutMinutes")
            <*> (x .:? "completedJobs")
            <*> (x .:? "resultCode")
            <*> (x .:? "name")
            <*> (x .:? "appUpload")
            <*> (x .:? "parsingResultUrl")
            <*> (x .:? "networkProfile")
            <*> (x .:? "deviceMinutes")
            <*> (x .:? "type")
            <*> (x .:? "message")
            <*> (x .:? "webUrl")
            <*> (x .:? "totalJobs")
            <*> (x .:? "devicePoolArn")
            <*> (x .:? "started")
            <*> (x .:? "deviceSelectionResult")
      )

instance Hashable Run

instance NFData Run
