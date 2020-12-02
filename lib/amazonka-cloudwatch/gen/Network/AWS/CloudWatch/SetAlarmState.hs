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
-- Module      : Network.AWS.CloudWatch.SetAlarmState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Temporarily sets the state of an alarm for testing purposes. When the updated state differs from the previous value, the action configured for the appropriate state is invoked. For example, if your alarm is configured to send an Amazon SNS message when an alarm is triggered, temporarily changing the alarm state to @ALARM@ sends an SNS message.
--
--
-- Metric alarms returns to their actual state quickly, often within seconds. Because the metric alarm state change happens quickly, it is typically only visible in the alarm's __History__ tab in the Amazon CloudWatch console or through <https://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/API_DescribeAlarmHistory.html DescribeAlarmHistory> .
--
-- If you use @SetAlarmState@ on a composite alarm, the composite alarm is not guaranteed to return to its actual state. It returns to its actual state only once any of its children alarms change state. It is also reevaluated if you update its configuration.
--
-- If an alarm triggers EC2 Auto Scaling policies or application Auto Scaling policies, you must include information in the @StateReasonData@ parameter to enable the policy to take the correct action.
module Network.AWS.CloudWatch.SetAlarmState
  ( -- * Creating a Request
    setAlarmState,
    SetAlarmState,

    -- * Request Lenses
    sasStateReasonData,
    sasAlarmName,
    sasStateValue,
    sasStateReason,

    -- * Destructuring the Response
    setAlarmStateResponse,
    SetAlarmStateResponse,
  )
where

import Network.AWS.CloudWatch.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'setAlarmState' smart constructor.
data SetAlarmState = SetAlarmState'
  { _sasStateReasonData ::
      !(Maybe Text),
    _sasAlarmName :: !Text,
    _sasStateValue :: !StateValue,
    _sasStateReason :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SetAlarmState' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sasStateReasonData' - The reason that this alarm is set to this specific state, in JSON format. For SNS or EC2 alarm actions, this is just informational. But for EC2 Auto Scaling or application Auto Scaling alarm actions, the Auto Scaling policy uses the information in this field to take the correct action.
--
-- * 'sasAlarmName' - The name of the alarm.
--
-- * 'sasStateValue' - The value of the state.
--
-- * 'sasStateReason' - The reason that this alarm is set to this specific state, in text format.
setAlarmState ::
  -- | 'sasAlarmName'
  Text ->
  -- | 'sasStateValue'
  StateValue ->
  -- | 'sasStateReason'
  Text ->
  SetAlarmState
setAlarmState pAlarmName_ pStateValue_ pStateReason_ =
  SetAlarmState'
    { _sasStateReasonData = Nothing,
      _sasAlarmName = pAlarmName_,
      _sasStateValue = pStateValue_,
      _sasStateReason = pStateReason_
    }

-- | The reason that this alarm is set to this specific state, in JSON format. For SNS or EC2 alarm actions, this is just informational. But for EC2 Auto Scaling or application Auto Scaling alarm actions, the Auto Scaling policy uses the information in this field to take the correct action.
sasStateReasonData :: Lens' SetAlarmState (Maybe Text)
sasStateReasonData = lens _sasStateReasonData (\s a -> s {_sasStateReasonData = a})

-- | The name of the alarm.
sasAlarmName :: Lens' SetAlarmState Text
sasAlarmName = lens _sasAlarmName (\s a -> s {_sasAlarmName = a})

-- | The value of the state.
sasStateValue :: Lens' SetAlarmState StateValue
sasStateValue = lens _sasStateValue (\s a -> s {_sasStateValue = a})

-- | The reason that this alarm is set to this specific state, in text format.
sasStateReason :: Lens' SetAlarmState Text
sasStateReason = lens _sasStateReason (\s a -> s {_sasStateReason = a})

instance AWSRequest SetAlarmState where
  type Rs SetAlarmState = SetAlarmStateResponse
  request = postQuery cloudWatch
  response = receiveNull SetAlarmStateResponse'

instance Hashable SetAlarmState

instance NFData SetAlarmState

instance ToHeaders SetAlarmState where
  toHeaders = const mempty

instance ToPath SetAlarmState where
  toPath = const "/"

instance ToQuery SetAlarmState where
  toQuery SetAlarmState' {..} =
    mconcat
      [ "Action" =: ("SetAlarmState" :: ByteString),
        "Version" =: ("2010-08-01" :: ByteString),
        "StateReasonData" =: _sasStateReasonData,
        "AlarmName" =: _sasAlarmName,
        "StateValue" =: _sasStateValue,
        "StateReason" =: _sasStateReason
      ]

-- | /See:/ 'setAlarmStateResponse' smart constructor.
data SetAlarmStateResponse = SetAlarmStateResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SetAlarmStateResponse' with the minimum fields required to make a request.
setAlarmStateResponse ::
  SetAlarmStateResponse
setAlarmStateResponse = SetAlarmStateResponse'

instance NFData SetAlarmStateResponse
