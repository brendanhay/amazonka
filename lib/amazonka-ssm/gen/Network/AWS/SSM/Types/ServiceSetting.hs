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
    ssARN,
    ssLastModifiedDate,
    ssLastModifiedUser,
    ssSettingId,
    ssSettingValue,
    ssStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SSM.Types.SettingId as Types
import qualified Network.AWS.SSM.Types.SettingValue as Types
import qualified Network.AWS.SSM.Types.String as Types

-- | The service setting data structure.
--
-- @ServiceSetting@ is an account-level setting for an AWS service. This setting defines how a user interacts with or uses a service or a feature of a service. For example, if an AWS service charges money to the account based on feature or service usage, then the AWS service team might create a default setting of "false". This means the user can't use this feature unless they change the setting to "true" and intentionally opt in for a paid feature.
-- Services map a @SettingId@ object to a setting value. AWS services teams define the default value for a @SettingId@ . You can't create a new @SettingId@ , but you can overwrite the default value if you have the @ssm:UpdateServiceSetting@ permission for the setting. Use the 'UpdateServiceSetting' API action to change the default setting. Or, use the 'ResetServiceSetting' to change the value back to the original value defined by the AWS service team.
--
-- /See:/ 'mkServiceSetting' smart constructor.
data ServiceSetting = ServiceSetting'
  { -- | The ARN of the service setting.
    arn :: Core.Maybe Types.String,
    -- | The last time the service setting was modified.
    lastModifiedDate :: Core.Maybe Core.NominalDiffTime,
    -- | The ARN of the last modified user. This field is populated only if the setting value was overwritten.
    lastModifiedUser :: Core.Maybe Types.String,
    -- | The ID of the service setting.
    settingId :: Core.Maybe Types.SettingId,
    -- | The value of the service setting.
    settingValue :: Core.Maybe Types.SettingValue,
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
    status :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ServiceSetting' value with any optional fields omitted.
mkServiceSetting ::
  ServiceSetting
mkServiceSetting =
  ServiceSetting'
    { arn = Core.Nothing,
      lastModifiedDate = Core.Nothing,
      lastModifiedUser = Core.Nothing,
      settingId = Core.Nothing,
      settingValue = Core.Nothing,
      status = Core.Nothing
    }

-- | The ARN of the service setting.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssARN :: Lens.Lens' ServiceSetting (Core.Maybe Types.String)
ssARN = Lens.field @"arn"
{-# DEPRECATED ssARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The last time the service setting was modified.
--
-- /Note:/ Consider using 'lastModifiedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssLastModifiedDate :: Lens.Lens' ServiceSetting (Core.Maybe Core.NominalDiffTime)
ssLastModifiedDate = Lens.field @"lastModifiedDate"
{-# DEPRECATED ssLastModifiedDate "Use generic-lens or generic-optics with 'lastModifiedDate' instead." #-}

-- | The ARN of the last modified user. This field is populated only if the setting value was overwritten.
--
-- /Note:/ Consider using 'lastModifiedUser' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssLastModifiedUser :: Lens.Lens' ServiceSetting (Core.Maybe Types.String)
ssLastModifiedUser = Lens.field @"lastModifiedUser"
{-# DEPRECATED ssLastModifiedUser "Use generic-lens or generic-optics with 'lastModifiedUser' instead." #-}

-- | The ID of the service setting.
--
-- /Note:/ Consider using 'settingId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssSettingId :: Lens.Lens' ServiceSetting (Core.Maybe Types.SettingId)
ssSettingId = Lens.field @"settingId"
{-# DEPRECATED ssSettingId "Use generic-lens or generic-optics with 'settingId' instead." #-}

-- | The value of the service setting.
--
-- /Note:/ Consider using 'settingValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssSettingValue :: Lens.Lens' ServiceSetting (Core.Maybe Types.SettingValue)
ssSettingValue = Lens.field @"settingValue"
{-# DEPRECATED ssSettingValue "Use generic-lens or generic-optics with 'settingValue' instead." #-}

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
ssStatus :: Lens.Lens' ServiceSetting (Core.Maybe Types.String)
ssStatus = Lens.field @"status"
{-# DEPRECATED ssStatus "Use generic-lens or generic-optics with 'status' instead." #-}

instance Core.FromJSON ServiceSetting where
  parseJSON =
    Core.withObject "ServiceSetting" Core.$
      \x ->
        ServiceSetting'
          Core.<$> (x Core..:? "ARN")
          Core.<*> (x Core..:? "LastModifiedDate")
          Core.<*> (x Core..:? "LastModifiedUser")
          Core.<*> (x Core..:? "SettingId")
          Core.<*> (x Core..:? "SettingValue")
          Core.<*> (x Core..:? "Status")
