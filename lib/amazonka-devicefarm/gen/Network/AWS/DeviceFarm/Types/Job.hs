{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.Types.Job
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DeviceFarm.Types.Job where

import Network.AWS.DeviceFarm.Types.Counters
import Network.AWS.DeviceFarm.Types.Device
import Network.AWS.DeviceFarm.Types.DeviceMinutes
import Network.AWS.DeviceFarm.Types.ExecutionResult
import Network.AWS.DeviceFarm.Types.ExecutionStatus
import Network.AWS.DeviceFarm.Types.TestType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents a device.
--
--
--
-- /See:/ 'job' smart constructor.
data Job = Job'
  { _jobInstanceARN :: !(Maybe Text),
    _jobStatus :: !(Maybe ExecutionStatus),
    _jobCounters :: !(Maybe Counters),
    _jobArn :: !(Maybe Text),
    _jobCreated :: !(Maybe POSIX),
    _jobDevice :: !(Maybe Device),
    _jobStopped :: !(Maybe POSIX),
    _jobResult :: !(Maybe ExecutionResult),
    _jobName :: !(Maybe Text),
    _jobVideoEndpoint :: !(Maybe Text),
    _jobDeviceMinutes :: !(Maybe DeviceMinutes),
    _jobVideoCapture :: !(Maybe Bool),
    _jobType :: !(Maybe TestType),
    _jobMessage :: !(Maybe Text),
    _jobStarted :: !(Maybe POSIX)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Job' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'jobInstanceARN' - The ARN of the instance.
--
-- * 'jobStatus' - The job's status. Allowed values include:     * PENDING     * PENDING_CONCURRENCY     * PENDING_DEVICE     * PROCESSING     * SCHEDULING     * PREPARING     * RUNNING     * COMPLETED     * STOPPING
--
-- * 'jobCounters' - The job's result counters.
--
-- * 'jobArn' - The job's ARN.
--
-- * 'jobCreated' - When the job was created.
--
-- * 'jobDevice' - The device (phone or tablet).
--
-- * 'jobStopped' - The job's stop time.
--
-- * 'jobResult' - The job's result. Allowed values include:     * PENDING     * PASSED     * WARNED     * FAILED     * SKIPPED     * ERRORED     * STOPPED
--
-- * 'jobName' - The job's name.
--
-- * 'jobVideoEndpoint' - The endpoint for streaming device video.
--
-- * 'jobDeviceMinutes' - Represents the total (metered or unmetered) minutes used by the job.
--
-- * 'jobVideoCapture' - This value is set to true if video capture is enabled. Otherwise, it is set to false.
--
-- * 'jobType' - The job's type. Allowed values include the following:     * BUILTIN_FUZZ     * BUILTIN_EXPLORER. For Android, an app explorer that traverses an Android app, interacting with it and capturing screenshots at the same time.     * APPIUM_JAVA_JUNIT     * APPIUM_JAVA_TESTNG     * APPIUM_PYTHON     * APPIUM_NODE     * APPIUM_RUBY     * APPIUM_WEB_JAVA_JUNIT     * APPIUM_WEB_JAVA_TESTNG     * APPIUM_WEB_PYTHON     * APPIUM_WEB_NODE     * APPIUM_WEB_RUBY     * CALABASH     * INSTRUMENTATION     * UIAUTOMATION     * UIAUTOMATOR     * XCTEST     * XCTEST_UI
--
-- * 'jobMessage' - A message about the job's result.
--
-- * 'jobStarted' - The job's start time.
job ::
  Job
job =
  Job'
    { _jobInstanceARN = Nothing,
      _jobStatus = Nothing,
      _jobCounters = Nothing,
      _jobArn = Nothing,
      _jobCreated = Nothing,
      _jobDevice = Nothing,
      _jobStopped = Nothing,
      _jobResult = Nothing,
      _jobName = Nothing,
      _jobVideoEndpoint = Nothing,
      _jobDeviceMinutes = Nothing,
      _jobVideoCapture = Nothing,
      _jobType = Nothing,
      _jobMessage = Nothing,
      _jobStarted = Nothing
    }

-- | The ARN of the instance.
jobInstanceARN :: Lens' Job (Maybe Text)
jobInstanceARN = lens _jobInstanceARN (\s a -> s {_jobInstanceARN = a})

-- | The job's status. Allowed values include:     * PENDING     * PENDING_CONCURRENCY     * PENDING_DEVICE     * PROCESSING     * SCHEDULING     * PREPARING     * RUNNING     * COMPLETED     * STOPPING
jobStatus :: Lens' Job (Maybe ExecutionStatus)
jobStatus = lens _jobStatus (\s a -> s {_jobStatus = a})

-- | The job's result counters.
jobCounters :: Lens' Job (Maybe Counters)
jobCounters = lens _jobCounters (\s a -> s {_jobCounters = a})

-- | The job's ARN.
jobArn :: Lens' Job (Maybe Text)
jobArn = lens _jobArn (\s a -> s {_jobArn = a})

-- | When the job was created.
jobCreated :: Lens' Job (Maybe UTCTime)
jobCreated = lens _jobCreated (\s a -> s {_jobCreated = a}) . mapping _Time

-- | The device (phone or tablet).
jobDevice :: Lens' Job (Maybe Device)
jobDevice = lens _jobDevice (\s a -> s {_jobDevice = a})

-- | The job's stop time.
jobStopped :: Lens' Job (Maybe UTCTime)
jobStopped = lens _jobStopped (\s a -> s {_jobStopped = a}) . mapping _Time

-- | The job's result. Allowed values include:     * PENDING     * PASSED     * WARNED     * FAILED     * SKIPPED     * ERRORED     * STOPPED
jobResult :: Lens' Job (Maybe ExecutionResult)
jobResult = lens _jobResult (\s a -> s {_jobResult = a})

-- | The job's name.
jobName :: Lens' Job (Maybe Text)
jobName = lens _jobName (\s a -> s {_jobName = a})

-- | The endpoint for streaming device video.
jobVideoEndpoint :: Lens' Job (Maybe Text)
jobVideoEndpoint = lens _jobVideoEndpoint (\s a -> s {_jobVideoEndpoint = a})

-- | Represents the total (metered or unmetered) minutes used by the job.
jobDeviceMinutes :: Lens' Job (Maybe DeviceMinutes)
jobDeviceMinutes = lens _jobDeviceMinutes (\s a -> s {_jobDeviceMinutes = a})

-- | This value is set to true if video capture is enabled. Otherwise, it is set to false.
jobVideoCapture :: Lens' Job (Maybe Bool)
jobVideoCapture = lens _jobVideoCapture (\s a -> s {_jobVideoCapture = a})

-- | The job's type. Allowed values include the following:     * BUILTIN_FUZZ     * BUILTIN_EXPLORER. For Android, an app explorer that traverses an Android app, interacting with it and capturing screenshots at the same time.     * APPIUM_JAVA_JUNIT     * APPIUM_JAVA_TESTNG     * APPIUM_PYTHON     * APPIUM_NODE     * APPIUM_RUBY     * APPIUM_WEB_JAVA_JUNIT     * APPIUM_WEB_JAVA_TESTNG     * APPIUM_WEB_PYTHON     * APPIUM_WEB_NODE     * APPIUM_WEB_RUBY     * CALABASH     * INSTRUMENTATION     * UIAUTOMATION     * UIAUTOMATOR     * XCTEST     * XCTEST_UI
jobType :: Lens' Job (Maybe TestType)
jobType = lens _jobType (\s a -> s {_jobType = a})

-- | A message about the job's result.
jobMessage :: Lens' Job (Maybe Text)
jobMessage = lens _jobMessage (\s a -> s {_jobMessage = a})

-- | The job's start time.
jobStarted :: Lens' Job (Maybe UTCTime)
jobStarted = lens _jobStarted (\s a -> s {_jobStarted = a}) . mapping _Time

instance FromJSON Job where
  parseJSON =
    withObject
      "Job"
      ( \x ->
          Job'
            <$> (x .:? "instanceArn")
            <*> (x .:? "status")
            <*> (x .:? "counters")
            <*> (x .:? "arn")
            <*> (x .:? "created")
            <*> (x .:? "device")
            <*> (x .:? "stopped")
            <*> (x .:? "result")
            <*> (x .:? "name")
            <*> (x .:? "videoEndpoint")
            <*> (x .:? "deviceMinutes")
            <*> (x .:? "videoCapture")
            <*> (x .:? "type")
            <*> (x .:? "message")
            <*> (x .:? "started")
      )

instance Hashable Job

instance NFData Job
