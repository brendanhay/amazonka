{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.DeleteAccountSetting
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disables an account setting for a specified IAM user, IAM role, or the root user for an account.
module Network.AWS.ECS.DeleteAccountSetting
  ( -- * Creating a request
    DeleteAccountSetting (..),
    mkDeleteAccountSetting,

    -- ** Request lenses
    dasName,
    dasPrincipalArn,

    -- * Destructuring the response
    DeleteAccountSettingResponse (..),
    mkDeleteAccountSettingResponse,

    -- ** Response lenses
    dasrrsSetting,
    dasrrsResponseStatus,
  )
where

import qualified Network.AWS.ECS.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteAccountSetting' smart constructor.
data DeleteAccountSetting = DeleteAccountSetting'
  { -- | The resource name for which to disable the account setting. If @serviceLongArnFormat@ is specified, the ARN for your Amazon ECS services is affected. If @taskLongArnFormat@ is specified, the ARN and resource ID for your Amazon ECS tasks is affected. If @containerInstanceLongArnFormat@ is specified, the ARN and resource ID for your Amazon ECS container instances is affected. If @awsvpcTrunking@ is specified, the ENI limit for your Amazon ECS container instances is affected.
    name :: Types.SettingName,
    -- | The ARN of the principal, which can be an IAM user, IAM role, or the root user. If you specify the root user, it disables the account setting for all IAM users, IAM roles, and the root user of the account unless an IAM user or role explicitly overrides these settings. If this field is omitted, the setting is changed only for the authenticated user.
    principalArn :: Core.Maybe Types.PrincipalArn
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteAccountSetting' value with any optional fields omitted.
mkDeleteAccountSetting ::
  -- | 'name'
  Types.SettingName ->
  DeleteAccountSetting
mkDeleteAccountSetting name =
  DeleteAccountSetting' {name, principalArn = Core.Nothing}

-- | The resource name for which to disable the account setting. If @serviceLongArnFormat@ is specified, the ARN for your Amazon ECS services is affected. If @taskLongArnFormat@ is specified, the ARN and resource ID for your Amazon ECS tasks is affected. If @containerInstanceLongArnFormat@ is specified, the ARN and resource ID for your Amazon ECS container instances is affected. If @awsvpcTrunking@ is specified, the ENI limit for your Amazon ECS container instances is affected.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dasName :: Lens.Lens' DeleteAccountSetting Types.SettingName
dasName = Lens.field @"name"
{-# DEPRECATED dasName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The ARN of the principal, which can be an IAM user, IAM role, or the root user. If you specify the root user, it disables the account setting for all IAM users, IAM roles, and the root user of the account unless an IAM user or role explicitly overrides these settings. If this field is omitted, the setting is changed only for the authenticated user.
--
-- /Note:/ Consider using 'principalArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dasPrincipalArn :: Lens.Lens' DeleteAccountSetting (Core.Maybe Types.PrincipalArn)
dasPrincipalArn = Lens.field @"principalArn"
{-# DEPRECATED dasPrincipalArn "Use generic-lens or generic-optics with 'principalArn' instead." #-}

instance Core.FromJSON DeleteAccountSetting where
  toJSON DeleteAccountSetting {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("name" Core..= name),
            ("principalArn" Core..=) Core.<$> principalArn
          ]
      )

instance Core.AWSRequest DeleteAccountSetting where
  type Rs DeleteAccountSetting = DeleteAccountSettingResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "AmazonEC2ContainerServiceV20141113.DeleteAccountSetting"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteAccountSettingResponse'
            Core.<$> (x Core..:? "setting") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDeleteAccountSettingResponse' smart constructor.
data DeleteAccountSettingResponse = DeleteAccountSettingResponse'
  { -- | The account setting for the specified principal ARN.
    setting :: Core.Maybe Types.Setting,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteAccountSettingResponse' value with any optional fields omitted.
mkDeleteAccountSettingResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteAccountSettingResponse
mkDeleteAccountSettingResponse responseStatus =
  DeleteAccountSettingResponse'
    { setting = Core.Nothing,
      responseStatus
    }

-- | The account setting for the specified principal ARN.
--
-- /Note:/ Consider using 'setting' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dasrrsSetting :: Lens.Lens' DeleteAccountSettingResponse (Core.Maybe Types.Setting)
dasrrsSetting = Lens.field @"setting"
{-# DEPRECATED dasrrsSetting "Use generic-lens or generic-optics with 'setting' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dasrrsResponseStatus :: Lens.Lens' DeleteAccountSettingResponse Core.Int
dasrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dasrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
