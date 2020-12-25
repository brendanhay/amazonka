{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.ResetServiceSetting
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- @ServiceSetting@ is an account-level setting for an AWS service. This setting defines how a user interacts with or uses a service or a feature of a service. For example, if an AWS service charges money to the account based on feature or service usage, then the AWS service team might create a default setting of "false". This means the user can't use this feature unless they change the setting to "true" and intentionally opt in for a paid feature.
--
-- Services map a @SettingId@ object to a setting value. AWS services teams define the default value for a @SettingId@ . You can't create a new @SettingId@ , but you can overwrite the default value if you have the @ssm:UpdateServiceSetting@ permission for the setting. Use the 'GetServiceSetting' API action to view the current value. Use the 'UpdateServiceSetting' API action to change the default setting.
-- Reset the service setting for the account to the default value as provisioned by the AWS service team.
module Network.AWS.SSM.ResetServiceSetting
  ( -- * Creating a request
    ResetServiceSetting (..),
    mkResetServiceSetting,

    -- ** Request lenses
    rssSettingId,

    -- * Destructuring the response
    ResetServiceSettingResponse (..),
    mkResetServiceSettingResponse,

    -- ** Response lenses
    rssrrsServiceSetting,
    rssrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SSM.Types as Types

-- | The request body of the ResetServiceSetting API action.
--
-- /See:/ 'mkResetServiceSetting' smart constructor.
newtype ResetServiceSetting = ResetServiceSetting'
  { -- | The Amazon Resource Name (ARN) of the service setting to reset. The setting ID can be @/ssm/parameter-store/default-parameter-tier@ , @/ssm/parameter-store/high-throughput-enabled@ , or @/ssm/managed-instance/activation-tier@ . For example, @arn:aws:ssm:us-east-1:111122223333:servicesetting/ssm/parameter-store/high-throughput-enabled@ .
    settingId :: Types.SettingId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'ResetServiceSetting' value with any optional fields omitted.
mkResetServiceSetting ::
  -- | 'settingId'
  Types.SettingId ->
  ResetServiceSetting
mkResetServiceSetting settingId = ResetServiceSetting' {settingId}

-- | The Amazon Resource Name (ARN) of the service setting to reset. The setting ID can be @/ssm/parameter-store/default-parameter-tier@ , @/ssm/parameter-store/high-throughput-enabled@ , or @/ssm/managed-instance/activation-tier@ . For example, @arn:aws:ssm:us-east-1:111122223333:servicesetting/ssm/parameter-store/high-throughput-enabled@ .
--
-- /Note:/ Consider using 'settingId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rssSettingId :: Lens.Lens' ResetServiceSetting Types.SettingId
rssSettingId = Lens.field @"settingId"
{-# DEPRECATED rssSettingId "Use generic-lens or generic-optics with 'settingId' instead." #-}

instance Core.FromJSON ResetServiceSetting where
  toJSON ResetServiceSetting {..} =
    Core.object
      (Core.catMaybes [Core.Just ("SettingId" Core..= settingId)])

instance Core.AWSRequest ResetServiceSetting where
  type Rs ResetServiceSetting = ResetServiceSettingResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AmazonSSM.ResetServiceSetting")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ResetServiceSettingResponse'
            Core.<$> (x Core..:? "ServiceSetting")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | The result body of the ResetServiceSetting API action.
--
-- /See:/ 'mkResetServiceSettingResponse' smart constructor.
data ResetServiceSettingResponse = ResetServiceSettingResponse'
  { -- | The current, effective service setting after calling the ResetServiceSetting API action.
    serviceSetting :: Core.Maybe Types.ServiceSetting,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ResetServiceSettingResponse' value with any optional fields omitted.
mkResetServiceSettingResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ResetServiceSettingResponse
mkResetServiceSettingResponse responseStatus =
  ResetServiceSettingResponse'
    { serviceSetting = Core.Nothing,
      responseStatus
    }

-- | The current, effective service setting after calling the ResetServiceSetting API action.
--
-- /Note:/ Consider using 'serviceSetting' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rssrrsServiceSetting :: Lens.Lens' ResetServiceSettingResponse (Core.Maybe Types.ServiceSetting)
rssrrsServiceSetting = Lens.field @"serviceSetting"
{-# DEPRECATED rssrrsServiceSetting "Use generic-lens or generic-optics with 'serviceSetting' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rssrrsResponseStatus :: Lens.Lens' ResetServiceSettingResponse Core.Int
rssrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED rssrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
