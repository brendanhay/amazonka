{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.CloudwatchAlarmAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.CloudwatchAlarmAction where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes an action that updates a CloudWatch alarm.
--
--
--
-- /See:/ 'cloudwatchAlarmAction' smart constructor.
data CloudwatchAlarmAction = CloudwatchAlarmAction'
  { _caaRoleARN ::
      !Text,
    _caaAlarmName :: !Text,
    _caaStateReason :: !Text,
    _caaStateValue :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CloudwatchAlarmAction' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'caaRoleARN' - The IAM role that allows access to the CloudWatch alarm.
--
-- * 'caaAlarmName' - The CloudWatch alarm name.
--
-- * 'caaStateReason' - The reason for the alarm change.
--
-- * 'caaStateValue' - The value of the alarm state. Acceptable values are: OK, ALARM, INSUFFICIENT_DATA.
cloudwatchAlarmAction ::
  -- | 'caaRoleARN'
  Text ->
  -- | 'caaAlarmName'
  Text ->
  -- | 'caaStateReason'
  Text ->
  -- | 'caaStateValue'
  Text ->
  CloudwatchAlarmAction
cloudwatchAlarmAction
  pRoleARN_
  pAlarmName_
  pStateReason_
  pStateValue_ =
    CloudwatchAlarmAction'
      { _caaRoleARN = pRoleARN_,
        _caaAlarmName = pAlarmName_,
        _caaStateReason = pStateReason_,
        _caaStateValue = pStateValue_
      }

-- | The IAM role that allows access to the CloudWatch alarm.
caaRoleARN :: Lens' CloudwatchAlarmAction Text
caaRoleARN = lens _caaRoleARN (\s a -> s {_caaRoleARN = a})

-- | The CloudWatch alarm name.
caaAlarmName :: Lens' CloudwatchAlarmAction Text
caaAlarmName = lens _caaAlarmName (\s a -> s {_caaAlarmName = a})

-- | The reason for the alarm change.
caaStateReason :: Lens' CloudwatchAlarmAction Text
caaStateReason = lens _caaStateReason (\s a -> s {_caaStateReason = a})

-- | The value of the alarm state. Acceptable values are: OK, ALARM, INSUFFICIENT_DATA.
caaStateValue :: Lens' CloudwatchAlarmAction Text
caaStateValue = lens _caaStateValue (\s a -> s {_caaStateValue = a})

instance FromJSON CloudwatchAlarmAction where
  parseJSON =
    withObject
      "CloudwatchAlarmAction"
      ( \x ->
          CloudwatchAlarmAction'
            <$> (x .: "roleArn")
            <*> (x .: "alarmName")
            <*> (x .: "stateReason")
            <*> (x .: "stateValue")
      )

instance Hashable CloudwatchAlarmAction

instance NFData CloudwatchAlarmAction

instance ToJSON CloudwatchAlarmAction where
  toJSON CloudwatchAlarmAction' {..} =
    object
      ( catMaybes
          [ Just ("roleArn" .= _caaRoleARN),
            Just ("alarmName" .= _caaAlarmName),
            Just ("stateReason" .= _caaStateReason),
            Just ("stateValue" .= _caaStateValue)
          ]
      )
