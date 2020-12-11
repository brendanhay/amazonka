{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    rssrsServiceSetting,
    rssrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SSM.Types

-- | The request body of the ResetServiceSetting API action.
--
-- /See:/ 'mkResetServiceSetting' smart constructor.
newtype ResetServiceSetting = ResetServiceSetting'
  { settingId ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ResetServiceSetting' with the minimum fields required to make a request.
--
-- * 'settingId' - The Amazon Resource Name (ARN) of the service setting to reset. The setting ID can be @/ssm/parameter-store/default-parameter-tier@ , @/ssm/parameter-store/high-throughput-enabled@ , or @/ssm/managed-instance/activation-tier@ . For example, @arn:aws:ssm:us-east-1:111122223333:servicesetting/ssm/parameter-store/high-throughput-enabled@ .
mkResetServiceSetting ::
  -- | 'settingId'
  Lude.Text ->
  ResetServiceSetting
mkResetServiceSetting pSettingId_ =
  ResetServiceSetting' {settingId = pSettingId_}

-- | The Amazon Resource Name (ARN) of the service setting to reset. The setting ID can be @/ssm/parameter-store/default-parameter-tier@ , @/ssm/parameter-store/high-throughput-enabled@ , or @/ssm/managed-instance/activation-tier@ . For example, @arn:aws:ssm:us-east-1:111122223333:servicesetting/ssm/parameter-store/high-throughput-enabled@ .
--
-- /Note:/ Consider using 'settingId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rssSettingId :: Lens.Lens' ResetServiceSetting Lude.Text
rssSettingId = Lens.lens (settingId :: ResetServiceSetting -> Lude.Text) (\s a -> s {settingId = a} :: ResetServiceSetting)
{-# DEPRECATED rssSettingId "Use generic-lens or generic-optics with 'settingId' instead." #-}

instance Lude.AWSRequest ResetServiceSetting where
  type Rs ResetServiceSetting = ResetServiceSettingResponse
  request = Req.postJSON ssmService
  response =
    Res.receiveJSON
      ( \s h x ->
          ResetServiceSettingResponse'
            Lude.<$> (x Lude..?> "ServiceSetting")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ResetServiceSetting where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AmazonSSM.ResetServiceSetting" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ResetServiceSetting where
  toJSON ResetServiceSetting' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("SettingId" Lude..= settingId)])

instance Lude.ToPath ResetServiceSetting where
  toPath = Lude.const "/"

instance Lude.ToQuery ResetServiceSetting where
  toQuery = Lude.const Lude.mempty

-- | The result body of the ResetServiceSetting API action.
--
-- /See:/ 'mkResetServiceSettingResponse' smart constructor.
data ResetServiceSettingResponse = ResetServiceSettingResponse'
  { serviceSetting ::
      Lude.Maybe ServiceSetting,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ResetServiceSettingResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'serviceSetting' - The current, effective service setting after calling the ResetServiceSetting API action.
mkResetServiceSettingResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ResetServiceSettingResponse
mkResetServiceSettingResponse pResponseStatus_ =
  ResetServiceSettingResponse'
    { serviceSetting = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The current, effective service setting after calling the ResetServiceSetting API action.
--
-- /Note:/ Consider using 'serviceSetting' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rssrsServiceSetting :: Lens.Lens' ResetServiceSettingResponse (Lude.Maybe ServiceSetting)
rssrsServiceSetting = Lens.lens (serviceSetting :: ResetServiceSettingResponse -> Lude.Maybe ServiceSetting) (\s a -> s {serviceSetting = a} :: ResetServiceSettingResponse)
{-# DEPRECATED rssrsServiceSetting "Use generic-lens or generic-optics with 'serviceSetting' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rssrsResponseStatus :: Lens.Lens' ResetServiceSettingResponse Lude.Int
rssrsResponseStatus = Lens.lens (responseStatus :: ResetServiceSettingResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ResetServiceSettingResponse)
{-# DEPRECATED rssrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
