{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.ServiceSetting
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.ServiceSetting
  ( ServiceSetting (..),

    -- * Smart constructor
    mkServiceSetting,

    -- * Lenses
    ssStatus,
    ssLastModifiedDate,
    ssARN,
    ssSettingId,
    ssLastModifiedUser,
    ssSettingValue,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The service setting data structure.
--
-- @ServiceSetting@ is an account-level setting for an AWS service. This setting defines how a user interacts with or uses a service or a feature of a service. For example, if an AWS service charges money to the account based on feature or service usage, then the AWS service team might create a default setting of "false". This means the user can't use this feature unless they change the setting to "true" and intentionally opt in for a paid feature.
-- Services map a @SettingId@ object to a setting value. AWS services teams define the default value for a @SettingId@ . You can't create a new @SettingId@ , but you can overwrite the default value if you have the @ssm:UpdateServiceSetting@ permission for the setting. Use the 'UpdateServiceSetting' API action to change the default setting. Or, use the 'ResetServiceSetting' to change the value back to the original value defined by the AWS service team.
--
-- /See:/ 'mkServiceSetting' smart constructor.
data ServiceSetting = ServiceSetting'
  { status ::
      Lude.Maybe Lude.Text,
    lastModifiedDate :: Lude.Maybe Lude.Timestamp,
    arn :: Lude.Maybe Lude.Text,
    settingId :: Lude.Maybe Lude.Text,
    lastModifiedUser :: Lude.Maybe Lude.Text,
    settingValue :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ServiceSetting' with the minimum fields required to make a request.
--
-- * 'arn' - The ARN of the service setting.
-- * 'lastModifiedDate' - The last time the service setting was modified.
-- * 'lastModifiedUser' - The ARN of the last modified user. This field is populated only if the setting value was overwritten.
-- * 'settingId' - The ID of the service setting.
-- * 'settingValue' - The value of the service setting.
-- * 'status' - The status of the service setting. The value can be Default, Customized or PendingUpdate.
--
--
--     * Default: The current setting uses a default value provisioned by the AWS service team.
--
--
--     * Customized: The current setting use a custom value specified by the customer.
--
--
--     * PendingUpdate: The current setting uses a default or custom value, but a setting change request is pending approval.
mkServiceSetting ::
  ServiceSetting
mkServiceSetting =
  ServiceSetting'
    { status = Lude.Nothing,
      lastModifiedDate = Lude.Nothing,
      arn = Lude.Nothing,
      settingId = Lude.Nothing,
      lastModifiedUser = Lude.Nothing,
      settingValue = Lude.Nothing
    }

-- | The status of the service setting. The value can be Default, Customized or PendingUpdate.
--
--
--     * Default: The current setting uses a default value provisioned by the AWS service team.
--
--
--     * Customized: The current setting use a custom value specified by the customer.
--
--
--     * PendingUpdate: The current setting uses a default or custom value, but a setting change request is pending approval.
--
--
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssStatus :: Lens.Lens' ServiceSetting (Lude.Maybe Lude.Text)
ssStatus = Lens.lens (status :: ServiceSetting -> Lude.Maybe Lude.Text) (\s a -> s {status = a} :: ServiceSetting)
{-# DEPRECATED ssStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The last time the service setting was modified.
--
-- /Note:/ Consider using 'lastModifiedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssLastModifiedDate :: Lens.Lens' ServiceSetting (Lude.Maybe Lude.Timestamp)
ssLastModifiedDate = Lens.lens (lastModifiedDate :: ServiceSetting -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastModifiedDate = a} :: ServiceSetting)
{-# DEPRECATED ssLastModifiedDate "Use generic-lens or generic-optics with 'lastModifiedDate' instead." #-}

-- | The ARN of the service setting.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssARN :: Lens.Lens' ServiceSetting (Lude.Maybe Lude.Text)
ssARN = Lens.lens (arn :: ServiceSetting -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: ServiceSetting)
{-# DEPRECATED ssARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The ID of the service setting.
--
-- /Note:/ Consider using 'settingId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssSettingId :: Lens.Lens' ServiceSetting (Lude.Maybe Lude.Text)
ssSettingId = Lens.lens (settingId :: ServiceSetting -> Lude.Maybe Lude.Text) (\s a -> s {settingId = a} :: ServiceSetting)
{-# DEPRECATED ssSettingId "Use generic-lens or generic-optics with 'settingId' instead." #-}

-- | The ARN of the last modified user. This field is populated only if the setting value was overwritten.
--
-- /Note:/ Consider using 'lastModifiedUser' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssLastModifiedUser :: Lens.Lens' ServiceSetting (Lude.Maybe Lude.Text)
ssLastModifiedUser = Lens.lens (lastModifiedUser :: ServiceSetting -> Lude.Maybe Lude.Text) (\s a -> s {lastModifiedUser = a} :: ServiceSetting)
{-# DEPRECATED ssLastModifiedUser "Use generic-lens or generic-optics with 'lastModifiedUser' instead." #-}

-- | The value of the service setting.
--
-- /Note:/ Consider using 'settingValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssSettingValue :: Lens.Lens' ServiceSetting (Lude.Maybe Lude.Text)
ssSettingValue = Lens.lens (settingValue :: ServiceSetting -> Lude.Maybe Lude.Text) (\s a -> s {settingValue = a} :: ServiceSetting)
{-# DEPRECATED ssSettingValue "Use generic-lens or generic-optics with 'settingValue' instead." #-}

instance Lude.FromJSON ServiceSetting where
  parseJSON =
    Lude.withObject
      "ServiceSetting"
      ( \x ->
          ServiceSetting'
            Lude.<$> (x Lude..:? "Status")
            Lude.<*> (x Lude..:? "LastModifiedDate")
            Lude.<*> (x Lude..:? "ARN")
            Lude.<*> (x Lude..:? "SettingId")
            Lude.<*> (x Lude..:? "LastModifiedUser")
            Lude.<*> (x Lude..:? "SettingValue")
      )
