{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.Types.AlarmConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeDeploy.Types.AlarmConfiguration where

import Network.AWS.CodeDeploy.Types.Alarm
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about alarms associated with the deployment group.
--
--
--
-- /See:/ 'alarmConfiguration' smart constructor.
data AlarmConfiguration = AlarmConfiguration'
  { _acIgnorePollAlarmFailure ::
      !(Maybe Bool),
    _acEnabled :: !(Maybe Bool),
    _acAlarms :: !(Maybe [Alarm])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AlarmConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'acIgnorePollAlarmFailure' - Indicates whether a deployment should continue if information about the current state of alarms cannot be retrieved from Amazon CloudWatch. The default value is false.     * @true@ : The deployment proceeds even if alarm status information can't be retrieved from Amazon CloudWatch.     * @false@ : The deployment stops if alarm status information can't be retrieved from Amazon CloudWatch.
--
-- * 'acEnabled' - Indicates whether the alarm configuration is enabled.
--
-- * 'acAlarms' - A list of alarms configured for the deployment group. A maximum of 10 alarms can be added to a deployment group.
alarmConfiguration ::
  AlarmConfiguration
alarmConfiguration =
  AlarmConfiguration'
    { _acIgnorePollAlarmFailure = Nothing,
      _acEnabled = Nothing,
      _acAlarms = Nothing
    }

-- | Indicates whether a deployment should continue if information about the current state of alarms cannot be retrieved from Amazon CloudWatch. The default value is false.     * @true@ : The deployment proceeds even if alarm status information can't be retrieved from Amazon CloudWatch.     * @false@ : The deployment stops if alarm status information can't be retrieved from Amazon CloudWatch.
acIgnorePollAlarmFailure :: Lens' AlarmConfiguration (Maybe Bool)
acIgnorePollAlarmFailure = lens _acIgnorePollAlarmFailure (\s a -> s {_acIgnorePollAlarmFailure = a})

-- | Indicates whether the alarm configuration is enabled.
acEnabled :: Lens' AlarmConfiguration (Maybe Bool)
acEnabled = lens _acEnabled (\s a -> s {_acEnabled = a})

-- | A list of alarms configured for the deployment group. A maximum of 10 alarms can be added to a deployment group.
acAlarms :: Lens' AlarmConfiguration [Alarm]
acAlarms = lens _acAlarms (\s a -> s {_acAlarms = a}) . _Default . _Coerce

instance FromJSON AlarmConfiguration where
  parseJSON =
    withObject
      "AlarmConfiguration"
      ( \x ->
          AlarmConfiguration'
            <$> (x .:? "ignorePollAlarmFailure")
            <*> (x .:? "enabled")
            <*> (x .:? "alarms" .!= mempty)
      )

instance Hashable AlarmConfiguration

instance NFData AlarmConfiguration

instance ToJSON AlarmConfiguration where
  toJSON AlarmConfiguration' {..} =
    object
      ( catMaybes
          [ ("ignorePollAlarmFailure" .=) <$> _acIgnorePollAlarmFailure,
            ("enabled" .=) <$> _acEnabled,
            ("alarms" .=) <$> _acAlarms
          ]
      )
