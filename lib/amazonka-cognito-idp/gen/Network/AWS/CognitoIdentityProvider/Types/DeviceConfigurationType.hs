{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.DeviceConfigurationType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.DeviceConfigurationType where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The configuration for the user pool's device tracking.
--
--
--
-- /See:/ 'deviceConfigurationType' smart constructor.
data DeviceConfigurationType = DeviceConfigurationType'
  { _dctChallengeRequiredOnNewDevice ::
      !(Maybe Bool),
    _dctDeviceOnlyRememberedOnUserPrompt ::
      !(Maybe Bool)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeviceConfigurationType' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dctChallengeRequiredOnNewDevice' - Indicates whether a challenge is required on a new device. Only applicable to a new device.
--
-- * 'dctDeviceOnlyRememberedOnUserPrompt' - If true, a device is only remembered on user prompt.
deviceConfigurationType ::
  DeviceConfigurationType
deviceConfigurationType =
  DeviceConfigurationType'
    { _dctChallengeRequiredOnNewDevice =
        Nothing,
      _dctDeviceOnlyRememberedOnUserPrompt = Nothing
    }

-- | Indicates whether a challenge is required on a new device. Only applicable to a new device.
dctChallengeRequiredOnNewDevice :: Lens' DeviceConfigurationType (Maybe Bool)
dctChallengeRequiredOnNewDevice = lens _dctChallengeRequiredOnNewDevice (\s a -> s {_dctChallengeRequiredOnNewDevice = a})

-- | If true, a device is only remembered on user prompt.
dctDeviceOnlyRememberedOnUserPrompt :: Lens' DeviceConfigurationType (Maybe Bool)
dctDeviceOnlyRememberedOnUserPrompt = lens _dctDeviceOnlyRememberedOnUserPrompt (\s a -> s {_dctDeviceOnlyRememberedOnUserPrompt = a})

instance FromJSON DeviceConfigurationType where
  parseJSON =
    withObject
      "DeviceConfigurationType"
      ( \x ->
          DeviceConfigurationType'
            <$> (x .:? "ChallengeRequiredOnNewDevice")
            <*> (x .:? "DeviceOnlyRememberedOnUserPrompt")
      )

instance Hashable DeviceConfigurationType

instance NFData DeviceConfigurationType

instance ToJSON DeviceConfigurationType where
  toJSON DeviceConfigurationType' {..} =
    object
      ( catMaybes
          [ ("ChallengeRequiredOnNewDevice" .=)
              <$> _dctChallengeRequiredOnNewDevice,
            ("DeviceOnlyRememberedOnUserPrompt" .=)
              <$> _dctDeviceOnlyRememberedOnUserPrompt
          ]
      )
