{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.PutAccountSetting
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies an account setting. Account settings are set on a per-Region basis.
--
-- If you change the account setting for the root user, the default settings for all of the IAM users and roles for which no individual account setting has been specified are reset. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-account-settings.html Account Settings> in the /Amazon Elastic Container Service Developer Guide/ .
-- When @serviceLongArnFormat@ , @taskLongArnFormat@ , or @containerInstanceLongArnFormat@ are specified, the Amazon Resource Name (ARN) and resource ID format of the resource type for a specified IAM user, IAM role, or the root user for an account is affected. The opt-in and opt-out account setting must be set for each Amazon ECS resource separately. The ARN and resource ID format of a resource will be defined by the opt-in status of the IAM user or role that created the resource. You must enable this setting to use Amazon ECS features such as resource tagging.
-- When @awsvpcTrunking@ is specified, the elastic network interface (ENI) limit for any new container instances that support the feature is changed. If @awsvpcTrunking@ is enabled, any new container instances that support the feature are launched have the increased ENI limits available to them. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/container-instance-eni.html Elastic Network Interface Trunking> in the /Amazon Elastic Container Service Developer Guide/ .
-- When @containerInsights@ is specified, the default setting indicating whether CloudWatch Container Insights is enabled for your clusters is changed. If @containerInsights@ is enabled, any new clusters that are created will have Container Insights enabled unless you disable it during cluster creation. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/cloudwatch-container-insights.html CloudWatch Container Insights> in the /Amazon Elastic Container Service Developer Guide/ .
module Network.AWS.ECS.PutAccountSetting
  ( -- * Creating a request
    PutAccountSetting (..),
    mkPutAccountSetting,

    -- ** Request lenses
    pasName,
    pasValue,
    pasPrincipalArn,

    -- * Destructuring the response
    PutAccountSettingResponse (..),
    mkPutAccountSettingResponse,

    -- ** Response lenses
    pasrrsSetting,
    pasrrsResponseStatus,
  )
where

import qualified Network.AWS.ECS.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkPutAccountSetting' smart constructor.
data PutAccountSetting = PutAccountSetting'
  { -- | The Amazon ECS resource name for which to modify the account setting. If @serviceLongArnFormat@ is specified, the ARN for your Amazon ECS services is affected. If @taskLongArnFormat@ is specified, the ARN and resource ID for your Amazon ECS tasks is affected. If @containerInstanceLongArnFormat@ is specified, the ARN and resource ID for your Amazon ECS container instances is affected. If @awsvpcTrunking@ is specified, the elastic network interface (ENI) limit for your Amazon ECS container instances is affected. If @containerInsights@ is specified, the default setting for CloudWatch Container Insights for your clusters is affected.
    name :: Types.SettingName,
    -- | The account setting value for the specified principal ARN. Accepted values are @enabled@ and @disabled@ .
    value :: Types.Value,
    -- | The ARN of the principal, which can be an IAM user, IAM role, or the root user. If you specify the root user, it modifies the account setting for all IAM users, IAM roles, and the root user of the account unless an IAM user or role explicitly overrides these settings. If this field is omitted, the setting is changed only for the authenticated user.
    principalArn :: Core.Maybe Types.PrincipalArn
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutAccountSetting' value with any optional fields omitted.
mkPutAccountSetting ::
  -- | 'name'
  Types.SettingName ->
  -- | 'value'
  Types.Value ->
  PutAccountSetting
mkPutAccountSetting name value =
  PutAccountSetting' {name, value, principalArn = Core.Nothing}

-- | The Amazon ECS resource name for which to modify the account setting. If @serviceLongArnFormat@ is specified, the ARN for your Amazon ECS services is affected. If @taskLongArnFormat@ is specified, the ARN and resource ID for your Amazon ECS tasks is affected. If @containerInstanceLongArnFormat@ is specified, the ARN and resource ID for your Amazon ECS container instances is affected. If @awsvpcTrunking@ is specified, the elastic network interface (ENI) limit for your Amazon ECS container instances is affected. If @containerInsights@ is specified, the default setting for CloudWatch Container Insights for your clusters is affected.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pasName :: Lens.Lens' PutAccountSetting Types.SettingName
pasName = Lens.field @"name"
{-# DEPRECATED pasName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The account setting value for the specified principal ARN. Accepted values are @enabled@ and @disabled@ .
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pasValue :: Lens.Lens' PutAccountSetting Types.Value
pasValue = Lens.field @"value"
{-# DEPRECATED pasValue "Use generic-lens or generic-optics with 'value' instead." #-}

-- | The ARN of the principal, which can be an IAM user, IAM role, or the root user. If you specify the root user, it modifies the account setting for all IAM users, IAM roles, and the root user of the account unless an IAM user or role explicitly overrides these settings. If this field is omitted, the setting is changed only for the authenticated user.
--
-- /Note:/ Consider using 'principalArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pasPrincipalArn :: Lens.Lens' PutAccountSetting (Core.Maybe Types.PrincipalArn)
pasPrincipalArn = Lens.field @"principalArn"
{-# DEPRECATED pasPrincipalArn "Use generic-lens or generic-optics with 'principalArn' instead." #-}

instance Core.FromJSON PutAccountSetting where
  toJSON PutAccountSetting {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("name" Core..= name),
            Core.Just ("value" Core..= value),
            ("principalArn" Core..=) Core.<$> principalArn
          ]
      )

instance Core.AWSRequest PutAccountSetting where
  type Rs PutAccountSetting = PutAccountSettingResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "AmazonEC2ContainerServiceV20141113.PutAccountSetting"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          PutAccountSettingResponse'
            Core.<$> (x Core..:? "setting") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkPutAccountSettingResponse' smart constructor.
data PutAccountSettingResponse = PutAccountSettingResponse'
  { -- | The current account setting for a resource.
    setting :: Core.Maybe Types.Setting,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutAccountSettingResponse' value with any optional fields omitted.
mkPutAccountSettingResponse ::
  -- | 'responseStatus'
  Core.Int ->
  PutAccountSettingResponse
mkPutAccountSettingResponse responseStatus =
  PutAccountSettingResponse'
    { setting = Core.Nothing,
      responseStatus
    }

-- | The current account setting for a resource.
--
-- /Note:/ Consider using 'setting' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pasrrsSetting :: Lens.Lens' PutAccountSettingResponse (Core.Maybe Types.Setting)
pasrrsSetting = Lens.field @"setting"
{-# DEPRECATED pasrrsSetting "Use generic-lens or generic-optics with 'setting' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pasrrsResponseStatus :: Lens.Lens' PutAccountSettingResponse Core.Int
pasrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED pasrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
