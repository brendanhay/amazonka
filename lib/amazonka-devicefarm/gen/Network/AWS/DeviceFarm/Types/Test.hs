{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.Types.Test
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DeviceFarm.Types.Test where

import Network.AWS.DeviceFarm.Types.Counters
import Network.AWS.DeviceFarm.Types.DeviceMinutes
import Network.AWS.DeviceFarm.Types.ExecutionResult
import Network.AWS.DeviceFarm.Types.ExecutionStatus
import Network.AWS.DeviceFarm.Types.TestType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents a condition that is evaluated.
--
--
--
-- /See:/ 'test' smart constructor.
data Test = Test'
  { _tStatus :: !(Maybe ExecutionStatus),
    _tCounters :: !(Maybe Counters),
    _tArn :: !(Maybe Text),
    _tCreated :: !(Maybe POSIX),
    _tStopped :: !(Maybe POSIX),
    _tResult :: !(Maybe ExecutionResult),
    _tName :: !(Maybe Text),
    _tDeviceMinutes :: !(Maybe DeviceMinutes),
    _tType :: !(Maybe TestType),
    _tMessage :: !(Maybe Text),
    _tStarted :: !(Maybe POSIX)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Test' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tStatus' - The test's status. Allowed values include:     * PENDING     * PENDING_CONCURRENCY     * PENDING_DEVICE     * PROCESSING     * SCHEDULING     * PREPARING     * RUNNING     * COMPLETED     * STOPPING
--
-- * 'tCounters' - The test's result counters.
--
-- * 'tArn' - The test's ARN.
--
-- * 'tCreated' - When the test was created.
--
-- * 'tStopped' - The test's stop time.
--
-- * 'tResult' - The test's result. Allowed values include:     * PENDING     * PASSED     * WARNED     * FAILED     * SKIPPED     * ERRORED     * STOPPED
--
-- * 'tName' - The test's name.
--
-- * 'tDeviceMinutes' - Represents the total (metered or unmetered) minutes used by the test.
--
-- * 'tType' - The test's type. Must be one of the following values:     * BUILTIN_FUZZ     * BUILTIN_EXPLORER     * APPIUM_JAVA_JUNIT     * APPIUM_JAVA_TESTNG     * APPIUM_PYTHON     * APPIUM_NODE     * APPIUM_RUBY     * APPIUM_WEB_JAVA_JUNIT     * APPIUM_WEB_JAVA_TESTNG     * APPIUM_WEB_PYTHON     * APPIUM_WEB_NODE     * APPIUM_WEB_RUBY     * CALABASH     * INSTRUMENTATION     * UIAUTOMATION     * UIAUTOMATOR     * XCTEST     * XCTEST_UI
--
-- * 'tMessage' - A message about the test's result.
--
-- * 'tStarted' - The test's start time.
test ::
  Test
test =
  Test'
    { _tStatus = Nothing,
      _tCounters = Nothing,
      _tArn = Nothing,
      _tCreated = Nothing,
      _tStopped = Nothing,
      _tResult = Nothing,
      _tName = Nothing,
      _tDeviceMinutes = Nothing,
      _tType = Nothing,
      _tMessage = Nothing,
      _tStarted = Nothing
    }

-- | The test's status. Allowed values include:     * PENDING     * PENDING_CONCURRENCY     * PENDING_DEVICE     * PROCESSING     * SCHEDULING     * PREPARING     * RUNNING     * COMPLETED     * STOPPING
tStatus :: Lens' Test (Maybe ExecutionStatus)
tStatus = lens _tStatus (\s a -> s {_tStatus = a})

-- | The test's result counters.
tCounters :: Lens' Test (Maybe Counters)
tCounters = lens _tCounters (\s a -> s {_tCounters = a})

-- | The test's ARN.
tArn :: Lens' Test (Maybe Text)
tArn = lens _tArn (\s a -> s {_tArn = a})

-- | When the test was created.
tCreated :: Lens' Test (Maybe UTCTime)
tCreated = lens _tCreated (\s a -> s {_tCreated = a}) . mapping _Time

-- | The test's stop time.
tStopped :: Lens' Test (Maybe UTCTime)
tStopped = lens _tStopped (\s a -> s {_tStopped = a}) . mapping _Time

-- | The test's result. Allowed values include:     * PENDING     * PASSED     * WARNED     * FAILED     * SKIPPED     * ERRORED     * STOPPED
tResult :: Lens' Test (Maybe ExecutionResult)
tResult = lens _tResult (\s a -> s {_tResult = a})

-- | The test's name.
tName :: Lens' Test (Maybe Text)
tName = lens _tName (\s a -> s {_tName = a})

-- | Represents the total (metered or unmetered) minutes used by the test.
tDeviceMinutes :: Lens' Test (Maybe DeviceMinutes)
tDeviceMinutes = lens _tDeviceMinutes (\s a -> s {_tDeviceMinutes = a})

-- | The test's type. Must be one of the following values:     * BUILTIN_FUZZ     * BUILTIN_EXPLORER     * APPIUM_JAVA_JUNIT     * APPIUM_JAVA_TESTNG     * APPIUM_PYTHON     * APPIUM_NODE     * APPIUM_RUBY     * APPIUM_WEB_JAVA_JUNIT     * APPIUM_WEB_JAVA_TESTNG     * APPIUM_WEB_PYTHON     * APPIUM_WEB_NODE     * APPIUM_WEB_RUBY     * CALABASH     * INSTRUMENTATION     * UIAUTOMATION     * UIAUTOMATOR     * XCTEST     * XCTEST_UI
tType :: Lens' Test (Maybe TestType)
tType = lens _tType (\s a -> s {_tType = a})

-- | A message about the test's result.
tMessage :: Lens' Test (Maybe Text)
tMessage = lens _tMessage (\s a -> s {_tMessage = a})

-- | The test's start time.
tStarted :: Lens' Test (Maybe UTCTime)
tStarted = lens _tStarted (\s a -> s {_tStarted = a}) . mapping _Time

instance FromJSON Test where
  parseJSON =
    withObject
      "Test"
      ( \x ->
          Test'
            <$> (x .:? "status")
            <*> (x .:? "counters")
            <*> (x .:? "arn")
            <*> (x .:? "created")
            <*> (x .:? "stopped")
            <*> (x .:? "result")
            <*> (x .:? "name")
            <*> (x .:? "deviceMinutes")
            <*> (x .:? "type")
            <*> (x .:? "message")
            <*> (x .:? "started")
      )

instance Hashable Test

instance NFData Test
