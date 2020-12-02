{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.Types.Suite
-- Copyright   : (c) 2013-2020 Brendan Hay
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
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents a collection of one or more tests.
--
--
--
-- /See:/ 'suite' smart constructor.
data Suite = Suite'
  { _sStatus :: !(Maybe ExecutionStatus),
    _sCounters :: !(Maybe Counters),
    _sArn :: !(Maybe Text),
    _sCreated :: !(Maybe POSIX),
    _sStopped :: !(Maybe POSIX),
    _sResult :: !(Maybe ExecutionResult),
    _sName :: !(Maybe Text),
    _sDeviceMinutes :: !(Maybe DeviceMinutes),
    _sType :: !(Maybe TestType),
    _sMessage :: !(Maybe Text),
    _sStarted :: !(Maybe POSIX)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Suite' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sStatus' - The suite's status. Allowed values include:     * PENDING     * PENDING_CONCURRENCY     * PENDING_DEVICE     * PROCESSING     * SCHEDULING     * PREPARING     * RUNNING     * COMPLETED     * STOPPING
--
-- * 'sCounters' - The suite's result counters.
--
-- * 'sArn' - The suite's ARN.
--
-- * 'sCreated' - When the suite was created.
--
-- * 'sStopped' - The suite's stop time.
--
-- * 'sResult' - The suite's result. Allowed values include:     * PENDING     * PASSED     * WARNED     * FAILED     * SKIPPED     * ERRORED     * STOPPED
--
-- * 'sName' - The suite's name.
--
-- * 'sDeviceMinutes' - Represents the total (metered or unmetered) minutes used by the test suite.
--
-- * 'sType' - The suite's type. Must be one of the following values:     * BUILTIN_FUZZ     * BUILTIN_EXPLORER      * APPIUM_JAVA_JUNIT     * APPIUM_JAVA_TESTNG     * APPIUM_PYTHON     * APPIUM_NODE     * APPIUM_RUBY     * APPIUM_WEB_JAVA_JUNIT     * APPIUM_WEB_JAVA_TESTNG     * APPIUM_WEB_PYTHON     * APPIUM_WEB_NODE     * APPIUM_WEB_RUBY     * CALABASH     * INSTRUMENTATION     * UIAUTOMATION     * UIAUTOMATOR     * XCTEST     * XCTEST_UI
--
-- * 'sMessage' - A message about the suite's result.
--
-- * 'sStarted' - The suite's start time.
suite ::
  Suite
suite =
  Suite'
    { _sStatus = Nothing,
      _sCounters = Nothing,
      _sArn = Nothing,
      _sCreated = Nothing,
      _sStopped = Nothing,
      _sResult = Nothing,
      _sName = Nothing,
      _sDeviceMinutes = Nothing,
      _sType = Nothing,
      _sMessage = Nothing,
      _sStarted = Nothing
    }

-- | The suite's status. Allowed values include:     * PENDING     * PENDING_CONCURRENCY     * PENDING_DEVICE     * PROCESSING     * SCHEDULING     * PREPARING     * RUNNING     * COMPLETED     * STOPPING
sStatus :: Lens' Suite (Maybe ExecutionStatus)
sStatus = lens _sStatus (\s a -> s {_sStatus = a})

-- | The suite's result counters.
sCounters :: Lens' Suite (Maybe Counters)
sCounters = lens _sCounters (\s a -> s {_sCounters = a})

-- | The suite's ARN.
sArn :: Lens' Suite (Maybe Text)
sArn = lens _sArn (\s a -> s {_sArn = a})

-- | When the suite was created.
sCreated :: Lens' Suite (Maybe UTCTime)
sCreated = lens _sCreated (\s a -> s {_sCreated = a}) . mapping _Time

-- | The suite's stop time.
sStopped :: Lens' Suite (Maybe UTCTime)
sStopped = lens _sStopped (\s a -> s {_sStopped = a}) . mapping _Time

-- | The suite's result. Allowed values include:     * PENDING     * PASSED     * WARNED     * FAILED     * SKIPPED     * ERRORED     * STOPPED
sResult :: Lens' Suite (Maybe ExecutionResult)
sResult = lens _sResult (\s a -> s {_sResult = a})

-- | The suite's name.
sName :: Lens' Suite (Maybe Text)
sName = lens _sName (\s a -> s {_sName = a})

-- | Represents the total (metered or unmetered) minutes used by the test suite.
sDeviceMinutes :: Lens' Suite (Maybe DeviceMinutes)
sDeviceMinutes = lens _sDeviceMinutes (\s a -> s {_sDeviceMinutes = a})

-- | The suite's type. Must be one of the following values:     * BUILTIN_FUZZ     * BUILTIN_EXPLORER      * APPIUM_JAVA_JUNIT     * APPIUM_JAVA_TESTNG     * APPIUM_PYTHON     * APPIUM_NODE     * APPIUM_RUBY     * APPIUM_WEB_JAVA_JUNIT     * APPIUM_WEB_JAVA_TESTNG     * APPIUM_WEB_PYTHON     * APPIUM_WEB_NODE     * APPIUM_WEB_RUBY     * CALABASH     * INSTRUMENTATION     * UIAUTOMATION     * UIAUTOMATOR     * XCTEST     * XCTEST_UI
sType :: Lens' Suite (Maybe TestType)
sType = lens _sType (\s a -> s {_sType = a})

-- | A message about the suite's result.
sMessage :: Lens' Suite (Maybe Text)
sMessage = lens _sMessage (\s a -> s {_sMessage = a})

-- | The suite's start time.
sStarted :: Lens' Suite (Maybe UTCTime)
sStarted = lens _sStarted (\s a -> s {_sStarted = a}) . mapping _Time

instance FromJSON Suite where
  parseJSON =
    withObject
      "Suite"
      ( \x ->
          Suite'
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

instance Hashable Suite

instance NFData Suite
