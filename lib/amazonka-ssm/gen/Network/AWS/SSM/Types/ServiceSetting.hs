{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.ServiceSetting
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.ServiceSetting where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The service setting data structure.
--
--
-- @ServiceSetting@ is an account-level setting for an AWS service. This setting defines how a user interacts with or uses a service or a feature of a service. For example, if an AWS service charges money to the account based on feature or service usage, then the AWS service team might create a default setting of "false". This means the user can't use this feature unless they change the setting to "true" and intentionally opt in for a paid feature.
--
-- Services map a @SettingId@ object to a setting value. AWS services teams define the default value for a @SettingId@ . You can't create a new @SettingId@ , but you can overwrite the default value if you have the @ssm:UpdateServiceSetting@ permission for the setting. Use the 'UpdateServiceSetting' API action to change the default setting. Or, use the 'ResetServiceSetting' to change the value back to the original value defined by the AWS service team.
--
--
-- /See:/ 'serviceSetting' smart constructor.
data ServiceSetting = ServiceSetting'
  { _ssStatus :: !(Maybe Text),
    _ssLastModifiedDate :: !(Maybe POSIX),
    _ssARN :: !(Maybe Text),
    _ssSettingId :: !(Maybe Text),
    _ssLastModifiedUser :: !(Maybe Text),
    _ssSettingValue :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ServiceSetting' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ssStatus' - The status of the service setting. The value can be Default, Customized or PendingUpdate.     * Default: The current setting uses a default value provisioned by the AWS service team.     * Customized: The current setting use a custom value specified by the customer.     * PendingUpdate: The current setting uses a default or custom value, but a setting change request is pending approval.
--
-- * 'ssLastModifiedDate' - The last time the service setting was modified.
--
-- * 'ssARN' - The ARN of the service setting.
--
-- * 'ssSettingId' - The ID of the service setting.
--
-- * 'ssLastModifiedUser' - The ARN of the last modified user. This field is populated only if the setting value was overwritten.
--
-- * 'ssSettingValue' - The value of the service setting.
serviceSetting ::
  ServiceSetting
serviceSetting =
  ServiceSetting'
    { _ssStatus = Nothing,
      _ssLastModifiedDate = Nothing,
      _ssARN = Nothing,
      _ssSettingId = Nothing,
      _ssLastModifiedUser = Nothing,
      _ssSettingValue = Nothing
    }

-- | The status of the service setting. The value can be Default, Customized or PendingUpdate.     * Default: The current setting uses a default value provisioned by the AWS service team.     * Customized: The current setting use a custom value specified by the customer.     * PendingUpdate: The current setting uses a default or custom value, but a setting change request is pending approval.
ssStatus :: Lens' ServiceSetting (Maybe Text)
ssStatus = lens _ssStatus (\s a -> s {_ssStatus = a})

-- | The last time the service setting was modified.
ssLastModifiedDate :: Lens' ServiceSetting (Maybe UTCTime)
ssLastModifiedDate = lens _ssLastModifiedDate (\s a -> s {_ssLastModifiedDate = a}) . mapping _Time

-- | The ARN of the service setting.
ssARN :: Lens' ServiceSetting (Maybe Text)
ssARN = lens _ssARN (\s a -> s {_ssARN = a})

-- | The ID of the service setting.
ssSettingId :: Lens' ServiceSetting (Maybe Text)
ssSettingId = lens _ssSettingId (\s a -> s {_ssSettingId = a})

-- | The ARN of the last modified user. This field is populated only if the setting value was overwritten.
ssLastModifiedUser :: Lens' ServiceSetting (Maybe Text)
ssLastModifiedUser = lens _ssLastModifiedUser (\s a -> s {_ssLastModifiedUser = a})

-- | The value of the service setting.
ssSettingValue :: Lens' ServiceSetting (Maybe Text)
ssSettingValue = lens _ssSettingValue (\s a -> s {_ssSettingValue = a})

instance FromJSON ServiceSetting where
  parseJSON =
    withObject
      "ServiceSetting"
      ( \x ->
          ServiceSetting'
            <$> (x .:? "Status")
            <*> (x .:? "LastModifiedDate")
            <*> (x .:? "ARN")
            <*> (x .:? "SettingId")
            <*> (x .:? "LastModifiedUser")
            <*> (x .:? "SettingValue")
      )

instance Hashable ServiceSetting

instance NFData ServiceSetting
