{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.ScheduleRun
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Schedules a run.
module Network.AWS.DeviceFarm.ScheduleRun
  ( -- * Creating a Request
    scheduleRun,
    ScheduleRun,

    -- * Request Lenses
    srExecutionConfiguration,
    srDeviceSelectionConfiguration,
    srAppARN,
    srName,
    srConfiguration,
    srDevicePoolARN,
    srProjectARN,
    srTest,

    -- * Destructuring the Response
    scheduleRunResponse,
    ScheduleRunResponse,

    -- * Response Lenses
    srrsRun,
    srrsResponseStatus,
  )
where

import Network.AWS.DeviceFarm.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents a request to the schedule run operation.
--
--
--
-- /See:/ 'scheduleRun' smart constructor.
data ScheduleRun = ScheduleRun'
  { _srExecutionConfiguration ::
      !(Maybe ExecutionConfiguration),
    _srDeviceSelectionConfiguration ::
      !(Maybe DeviceSelectionConfiguration),
    _srAppARN :: !(Maybe Text),
    _srName :: !(Maybe Text),
    _srConfiguration :: !(Maybe ScheduleRunConfiguration),
    _srDevicePoolARN :: !(Maybe Text),
    _srProjectARN :: !Text,
    _srTest :: !ScheduleRunTest
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ScheduleRun' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'srExecutionConfiguration' - Specifies configuration information about a test run, such as the execution timeout (in minutes).
--
-- * 'srDeviceSelectionConfiguration' - The filter criteria used to dynamically select a set of devices for a test run and the maximum number of devices to be included in the run. Either __@devicePoolArn@ __ or __@deviceSelectionConfiguration@ __ is required in a request.
--
-- * 'srAppARN' - The ARN of an application package to run tests against, created with 'CreateUpload' . See 'ListUploads' .
--
-- * 'srName' - The name for the run to be scheduled.
--
-- * 'srConfiguration' - Information about the settings for the run to be scheduled.
--
-- * 'srDevicePoolARN' - The ARN of the device pool for the run to be scheduled.
--
-- * 'srProjectARN' - The ARN of the project for the run to be scheduled.
--
-- * 'srTest' - Information about the test for the run to be scheduled.
scheduleRun ::
  -- | 'srProjectARN'
  Text ->
  -- | 'srTest'
  ScheduleRunTest ->
  ScheduleRun
scheduleRun pProjectARN_ pTest_ =
  ScheduleRun'
    { _srExecutionConfiguration = Nothing,
      _srDeviceSelectionConfiguration = Nothing,
      _srAppARN = Nothing,
      _srName = Nothing,
      _srConfiguration = Nothing,
      _srDevicePoolARN = Nothing,
      _srProjectARN = pProjectARN_,
      _srTest = pTest_
    }

-- | Specifies configuration information about a test run, such as the execution timeout (in minutes).
srExecutionConfiguration :: Lens' ScheduleRun (Maybe ExecutionConfiguration)
srExecutionConfiguration = lens _srExecutionConfiguration (\s a -> s {_srExecutionConfiguration = a})

-- | The filter criteria used to dynamically select a set of devices for a test run and the maximum number of devices to be included in the run. Either __@devicePoolArn@ __ or __@deviceSelectionConfiguration@ __ is required in a request.
srDeviceSelectionConfiguration :: Lens' ScheduleRun (Maybe DeviceSelectionConfiguration)
srDeviceSelectionConfiguration = lens _srDeviceSelectionConfiguration (\s a -> s {_srDeviceSelectionConfiguration = a})

-- | The ARN of an application package to run tests against, created with 'CreateUpload' . See 'ListUploads' .
srAppARN :: Lens' ScheduleRun (Maybe Text)
srAppARN = lens _srAppARN (\s a -> s {_srAppARN = a})

-- | The name for the run to be scheduled.
srName :: Lens' ScheduleRun (Maybe Text)
srName = lens _srName (\s a -> s {_srName = a})

-- | Information about the settings for the run to be scheduled.
srConfiguration :: Lens' ScheduleRun (Maybe ScheduleRunConfiguration)
srConfiguration = lens _srConfiguration (\s a -> s {_srConfiguration = a})

-- | The ARN of the device pool for the run to be scheduled.
srDevicePoolARN :: Lens' ScheduleRun (Maybe Text)
srDevicePoolARN = lens _srDevicePoolARN (\s a -> s {_srDevicePoolARN = a})

-- | The ARN of the project for the run to be scheduled.
srProjectARN :: Lens' ScheduleRun Text
srProjectARN = lens _srProjectARN (\s a -> s {_srProjectARN = a})

-- | Information about the test for the run to be scheduled.
srTest :: Lens' ScheduleRun ScheduleRunTest
srTest = lens _srTest (\s a -> s {_srTest = a})

instance AWSRequest ScheduleRun where
  type Rs ScheduleRun = ScheduleRunResponse
  request = postJSON deviceFarm
  response =
    receiveJSON
      ( \s h x ->
          ScheduleRunResponse' <$> (x .?> "run") <*> (pure (fromEnum s))
      )

instance Hashable ScheduleRun

instance NFData ScheduleRun

instance ToHeaders ScheduleRun where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("DeviceFarm_20150623.ScheduleRun" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON ScheduleRun where
  toJSON ScheduleRun' {..} =
    object
      ( catMaybes
          [ ("executionConfiguration" .=) <$> _srExecutionConfiguration,
            ("deviceSelectionConfiguration" .=)
              <$> _srDeviceSelectionConfiguration,
            ("appArn" .=) <$> _srAppARN,
            ("name" .=) <$> _srName,
            ("configuration" .=) <$> _srConfiguration,
            ("devicePoolArn" .=) <$> _srDevicePoolARN,
            Just ("projectArn" .= _srProjectARN),
            Just ("test" .= _srTest)
          ]
      )

instance ToPath ScheduleRun where
  toPath = const "/"

instance ToQuery ScheduleRun where
  toQuery = const mempty

-- | Represents the result of a schedule run request.
--
--
--
-- /See:/ 'scheduleRunResponse' smart constructor.
data ScheduleRunResponse = ScheduleRunResponse'
  { _srrsRun ::
      !(Maybe Run),
    _srrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ScheduleRunResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'srrsRun' - Information about the scheduled run.
--
-- * 'srrsResponseStatus' - -- | The response status code.
scheduleRunResponse ::
  -- | 'srrsResponseStatus'
  Int ->
  ScheduleRunResponse
scheduleRunResponse pResponseStatus_ =
  ScheduleRunResponse'
    { _srrsRun = Nothing,
      _srrsResponseStatus = pResponseStatus_
    }

-- | Information about the scheduled run.
srrsRun :: Lens' ScheduleRunResponse (Maybe Run)
srrsRun = lens _srrsRun (\s a -> s {_srrsRun = a})

-- | -- | The response status code.
srrsResponseStatus :: Lens' ScheduleRunResponse Int
srrsResponseStatus = lens _srrsResponseStatus (\s a -> s {_srrsResponseStatus = a})

instance NFData ScheduleRunResponse
