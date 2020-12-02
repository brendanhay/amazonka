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
-- Module      : Network.AWS.Lightsail.TestAlarm
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Tests an alarm by displaying a banner on the Amazon Lightsail console. If a notification trigger is configured for the specified alarm, the test also sends a notification to the notification protocol (@Email@ and/or @SMS@ ) configured for the alarm.
--
--
-- An alarm is used to monitor a single metric for one of your resources. When a metric condition is met, the alarm can notify you by email, SMS text message, and a banner displayed on the Amazon Lightsail console. For more information, see <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-alarms Alarms in Amazon Lightsail> .
module Network.AWS.Lightsail.TestAlarm
  ( -- * Creating a Request
    testAlarm,
    TestAlarm,

    -- * Request Lenses
    taAlarmName,
    taState,

    -- * Destructuring the Response
    testAlarmResponse,
    TestAlarmResponse,

    -- * Response Lenses
    tarsOperations,
    tarsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Lightsail.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'testAlarm' smart constructor.
data TestAlarm = TestAlarm'
  { _taAlarmName :: !Text,
    _taState :: !AlarmState
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TestAlarm' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'taAlarmName' - The name of the alarm to test.
--
-- * 'taState' - The alarm state to test. An alarm has the following possible states that can be tested:     * @ALARM@ - The metric is outside of the defined threshold.     * @INSUFFICIENT_DATA@ - The alarm has just started, the metric is not available, or not enough data is available for the metric to determine the alarm state.     * @OK@ - The metric is within the defined threshold.
testAlarm ::
  -- | 'taAlarmName'
  Text ->
  -- | 'taState'
  AlarmState ->
  TestAlarm
testAlarm pAlarmName_ pState_ =
  TestAlarm' {_taAlarmName = pAlarmName_, _taState = pState_}

-- | The name of the alarm to test.
taAlarmName :: Lens' TestAlarm Text
taAlarmName = lens _taAlarmName (\s a -> s {_taAlarmName = a})

-- | The alarm state to test. An alarm has the following possible states that can be tested:     * @ALARM@ - The metric is outside of the defined threshold.     * @INSUFFICIENT_DATA@ - The alarm has just started, the metric is not available, or not enough data is available for the metric to determine the alarm state.     * @OK@ - The metric is within the defined threshold.
taState :: Lens' TestAlarm AlarmState
taState = lens _taState (\s a -> s {_taState = a})

instance AWSRequest TestAlarm where
  type Rs TestAlarm = TestAlarmResponse
  request = postJSON lightsail
  response =
    receiveJSON
      ( \s h x ->
          TestAlarmResponse'
            <$> (x .?> "operations" .!@ mempty) <*> (pure (fromEnum s))
      )

instance Hashable TestAlarm

instance NFData TestAlarm

instance ToHeaders TestAlarm where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target" =# ("Lightsail_20161128.TestAlarm" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON TestAlarm where
  toJSON TestAlarm' {..} =
    object
      ( catMaybes
          [Just ("alarmName" .= _taAlarmName), Just ("state" .= _taState)]
      )

instance ToPath TestAlarm where
  toPath = const "/"

instance ToQuery TestAlarm where
  toQuery = const mempty

-- | /See:/ 'testAlarmResponse' smart constructor.
data TestAlarmResponse = TestAlarmResponse'
  { _tarsOperations ::
      !(Maybe [Operation]),
    _tarsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TestAlarmResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tarsOperations' - An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
--
-- * 'tarsResponseStatus' - -- | The response status code.
testAlarmResponse ::
  -- | 'tarsResponseStatus'
  Int ->
  TestAlarmResponse
testAlarmResponse pResponseStatus_ =
  TestAlarmResponse'
    { _tarsOperations = Nothing,
      _tarsResponseStatus = pResponseStatus_
    }

-- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
tarsOperations :: Lens' TestAlarmResponse [Operation]
tarsOperations = lens _tarsOperations (\s a -> s {_tarsOperations = a}) . _Default . _Coerce

-- | -- | The response status code.
tarsResponseStatus :: Lens' TestAlarmResponse Int
tarsResponseStatus = lens _tarsResponseStatus (\s a -> s {_tarsResponseStatus = a})

instance NFData TestAlarmResponse
