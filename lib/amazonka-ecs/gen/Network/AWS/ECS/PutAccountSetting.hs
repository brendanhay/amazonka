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
    pasValue,
    pasName,
    pasPrincipalARN,

    -- * Destructuring the response
    PutAccountSettingResponse (..),
    mkPutAccountSettingResponse,

    -- ** Response lenses
    pasrsSetting,
    pasrsResponseStatus,
  )
where

import Network.AWS.ECS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkPutAccountSetting' smart constructor.
data PutAccountSetting = PutAccountSetting'
  { -- | The account setting value for the specified principal ARN. Accepted values are @enabled@ and @disabled@ .
    value :: Lude.Text,
    -- | The Amazon ECS resource name for which to modify the account setting. If @serviceLongArnFormat@ is specified, the ARN for your Amazon ECS services is affected. If @taskLongArnFormat@ is specified, the ARN and resource ID for your Amazon ECS tasks is affected. If @containerInstanceLongArnFormat@ is specified, the ARN and resource ID for your Amazon ECS container instances is affected. If @awsvpcTrunking@ is specified, the elastic network interface (ENI) limit for your Amazon ECS container instances is affected. If @containerInsights@ is specified, the default setting for CloudWatch Container Insights for your clusters is affected.
    name :: SettingName,
    -- | The ARN of the principal, which can be an IAM user, IAM role, or the root user. If you specify the root user, it modifies the account setting for all IAM users, IAM roles, and the root user of the account unless an IAM user or role explicitly overrides these settings. If this field is omitted, the setting is changed only for the authenticated user.
    principalARN :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutAccountSetting' with the minimum fields required to make a request.
--
-- * 'value' - The account setting value for the specified principal ARN. Accepted values are @enabled@ and @disabled@ .
-- * 'name' - The Amazon ECS resource name for which to modify the account setting. If @serviceLongArnFormat@ is specified, the ARN for your Amazon ECS services is affected. If @taskLongArnFormat@ is specified, the ARN and resource ID for your Amazon ECS tasks is affected. If @containerInstanceLongArnFormat@ is specified, the ARN and resource ID for your Amazon ECS container instances is affected. If @awsvpcTrunking@ is specified, the elastic network interface (ENI) limit for your Amazon ECS container instances is affected. If @containerInsights@ is specified, the default setting for CloudWatch Container Insights for your clusters is affected.
-- * 'principalARN' - The ARN of the principal, which can be an IAM user, IAM role, or the root user. If you specify the root user, it modifies the account setting for all IAM users, IAM roles, and the root user of the account unless an IAM user or role explicitly overrides these settings. If this field is omitted, the setting is changed only for the authenticated user.
mkPutAccountSetting ::
  -- | 'value'
  Lude.Text ->
  -- | 'name'
  SettingName ->
  PutAccountSetting
mkPutAccountSetting pValue_ pName_ =
  PutAccountSetting'
    { value = pValue_,
      name = pName_,
      principalARN = Lude.Nothing
    }

-- | The account setting value for the specified principal ARN. Accepted values are @enabled@ and @disabled@ .
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pasValue :: Lens.Lens' PutAccountSetting Lude.Text
pasValue = Lens.lens (value :: PutAccountSetting -> Lude.Text) (\s a -> s {value = a} :: PutAccountSetting)
{-# DEPRECATED pasValue "Use generic-lens or generic-optics with 'value' instead." #-}

-- | The Amazon ECS resource name for which to modify the account setting. If @serviceLongArnFormat@ is specified, the ARN for your Amazon ECS services is affected. If @taskLongArnFormat@ is specified, the ARN and resource ID for your Amazon ECS tasks is affected. If @containerInstanceLongArnFormat@ is specified, the ARN and resource ID for your Amazon ECS container instances is affected. If @awsvpcTrunking@ is specified, the elastic network interface (ENI) limit for your Amazon ECS container instances is affected. If @containerInsights@ is specified, the default setting for CloudWatch Container Insights for your clusters is affected.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pasName :: Lens.Lens' PutAccountSetting SettingName
pasName = Lens.lens (name :: PutAccountSetting -> SettingName) (\s a -> s {name = a} :: PutAccountSetting)
{-# DEPRECATED pasName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The ARN of the principal, which can be an IAM user, IAM role, or the root user. If you specify the root user, it modifies the account setting for all IAM users, IAM roles, and the root user of the account unless an IAM user or role explicitly overrides these settings. If this field is omitted, the setting is changed only for the authenticated user.
--
-- /Note:/ Consider using 'principalARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pasPrincipalARN :: Lens.Lens' PutAccountSetting (Lude.Maybe Lude.Text)
pasPrincipalARN = Lens.lens (principalARN :: PutAccountSetting -> Lude.Maybe Lude.Text) (\s a -> s {principalARN = a} :: PutAccountSetting)
{-# DEPRECATED pasPrincipalARN "Use generic-lens or generic-optics with 'principalARN' instead." #-}

instance Lude.AWSRequest PutAccountSetting where
  type Rs PutAccountSetting = PutAccountSettingResponse
  request = Req.postJSON ecsService
  response =
    Res.receiveJSON
      ( \s h x ->
          PutAccountSettingResponse'
            Lude.<$> (x Lude..?> "setting") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders PutAccountSetting where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AmazonEC2ContainerServiceV20141113.PutAccountSetting" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON PutAccountSetting where
  toJSON PutAccountSetting' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("value" Lude..= value),
            Lude.Just ("name" Lude..= name),
            ("principalArn" Lude..=) Lude.<$> principalARN
          ]
      )

instance Lude.ToPath PutAccountSetting where
  toPath = Lude.const "/"

instance Lude.ToQuery PutAccountSetting where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkPutAccountSettingResponse' smart constructor.
data PutAccountSettingResponse = PutAccountSettingResponse'
  { -- | The current account setting for a resource.
    setting :: Lude.Maybe Setting,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutAccountSettingResponse' with the minimum fields required to make a request.
--
-- * 'setting' - The current account setting for a resource.
-- * 'responseStatus' - The response status code.
mkPutAccountSettingResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  PutAccountSettingResponse
mkPutAccountSettingResponse pResponseStatus_ =
  PutAccountSettingResponse'
    { setting = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The current account setting for a resource.
--
-- /Note:/ Consider using 'setting' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pasrsSetting :: Lens.Lens' PutAccountSettingResponse (Lude.Maybe Setting)
pasrsSetting = Lens.lens (setting :: PutAccountSettingResponse -> Lude.Maybe Setting) (\s a -> s {setting = a} :: PutAccountSettingResponse)
{-# DEPRECATED pasrsSetting "Use generic-lens or generic-optics with 'setting' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pasrsResponseStatus :: Lens.Lens' PutAccountSettingResponse Lude.Int
pasrsResponseStatus = Lens.lens (responseStatus :: PutAccountSettingResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: PutAccountSettingResponse)
{-# DEPRECATED pasrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
