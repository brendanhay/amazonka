{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.SMSMFASettingsType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.SMSMFASettingsType where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The type used for enabling SMS MFA at the user level. Phone numbers don't need to be verified to be used for SMS MFA. If an MFA type is enabled for a user, the user will be prompted for MFA during all sign in attempts, unless device tracking is turned on and the device has been trusted. If you would like MFA to be applied selectively based on the assessed risk level of sign in attempts, disable MFA for users and turn on Adaptive Authentication for the user pool.
--
--
--
-- /See:/ 'sMSMFASettingsType' smart constructor.
data SMSMFASettingsType = SMSMFASettingsType'
  { _smsmstEnabled ::
      !(Maybe Bool),
    _smsmstPreferredMFA :: !(Maybe Bool)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SMSMFASettingsType' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'smsmstEnabled' - Specifies whether SMS text message MFA is enabled. If an MFA type is enabled for a user, the user will be prompted for MFA during all sign in attempts, unless device tracking is turned on and the device has been trusted.
--
-- * 'smsmstPreferredMFA' - Specifies whether SMS is the preferred MFA method.
sMSMFASettingsType ::
  SMSMFASettingsType
sMSMFASettingsType =
  SMSMFASettingsType'
    { _smsmstEnabled = Nothing,
      _smsmstPreferredMFA = Nothing
    }

-- | Specifies whether SMS text message MFA is enabled. If an MFA type is enabled for a user, the user will be prompted for MFA during all sign in attempts, unless device tracking is turned on and the device has been trusted.
smsmstEnabled :: Lens' SMSMFASettingsType (Maybe Bool)
smsmstEnabled = lens _smsmstEnabled (\s a -> s {_smsmstEnabled = a})

-- | Specifies whether SMS is the preferred MFA method.
smsmstPreferredMFA :: Lens' SMSMFASettingsType (Maybe Bool)
smsmstPreferredMFA = lens _smsmstPreferredMFA (\s a -> s {_smsmstPreferredMFA = a})

instance Hashable SMSMFASettingsType

instance NFData SMSMFASettingsType

instance ToJSON SMSMFASettingsType where
  toJSON SMSMFASettingsType' {..} =
    object
      ( catMaybes
          [ ("Enabled" .=) <$> _smsmstEnabled,
            ("PreferredMfa" .=) <$> _smsmstPreferredMFA
          ]
      )
