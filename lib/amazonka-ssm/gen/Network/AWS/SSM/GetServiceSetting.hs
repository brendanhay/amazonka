{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.GetServiceSetting
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- @ServiceSetting@ is an account-level setting for an AWS service. This setting defines how a user interacts with or uses a service or a feature of a service. For example, if an AWS service charges money to the account based on feature or service usage, then the AWS service team might create a default setting of "false". This means the user can't use this feature unless they change the setting to "true" and intentionally opt in for a paid feature.
--
-- Services map a @SettingId@ object to a setting value. AWS services teams define the default value for a @SettingId@ . You can't create a new @SettingId@ , but you can overwrite the default value if you have the @ssm:UpdateServiceSetting@ permission for the setting. Use the 'UpdateServiceSetting' API action to change the default setting. Or use the 'ResetServiceSetting' to change the value back to the original value defined by the AWS service team.
-- Query the current service setting for the account.
module Network.AWS.SSM.GetServiceSetting
  ( -- * Creating a request
    GetServiceSetting (..),
    mkGetServiceSetting,

    -- ** Request lenses
    gssSettingId,

    -- * Destructuring the response
    GetServiceSettingResponse (..),
    mkGetServiceSettingResponse,

    -- ** Response lenses
    gssrsServiceSetting,
    gssrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SSM.Types

-- | The request body of the GetServiceSetting API action.
--
-- /See:/ 'mkGetServiceSetting' smart constructor.
newtype GetServiceSetting = GetServiceSetting'
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

-- | Creates a value of 'GetServiceSetting' with the minimum fields required to make a request.
--
-- * 'settingId' - The ID of the service setting to get. The setting ID can be @/ssm/parameter-store/default-parameter-tier@ , @/ssm/parameter-store/high-throughput-enabled@ , or @/ssm/managed-instance/activation-tier@ .
mkGetServiceSetting ::
  -- | 'settingId'
  Lude.Text ->
  GetServiceSetting
mkGetServiceSetting pSettingId_ =
  GetServiceSetting' {settingId = pSettingId_}

-- | The ID of the service setting to get. The setting ID can be @/ssm/parameter-store/default-parameter-tier@ , @/ssm/parameter-store/high-throughput-enabled@ , or @/ssm/managed-instance/activation-tier@ .
--
-- /Note:/ Consider using 'settingId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gssSettingId :: Lens.Lens' GetServiceSetting Lude.Text
gssSettingId = Lens.lens (settingId :: GetServiceSetting -> Lude.Text) (\s a -> s {settingId = a} :: GetServiceSetting)
{-# DEPRECATED gssSettingId "Use generic-lens or generic-optics with 'settingId' instead." #-}

instance Lude.AWSRequest GetServiceSetting where
  type Rs GetServiceSetting = GetServiceSettingResponse
  request = Req.postJSON ssmService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetServiceSettingResponse'
            Lude.<$> (x Lude..?> "ServiceSetting")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetServiceSetting where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AmazonSSM.GetServiceSetting" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetServiceSetting where
  toJSON GetServiceSetting' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("SettingId" Lude..= settingId)])

instance Lude.ToPath GetServiceSetting where
  toPath = Lude.const "/"

instance Lude.ToQuery GetServiceSetting where
  toQuery = Lude.const Lude.mempty

-- | The query result body of the GetServiceSetting API action.
--
-- /See:/ 'mkGetServiceSettingResponse' smart constructor.
data GetServiceSettingResponse = GetServiceSettingResponse'
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

-- | Creates a value of 'GetServiceSettingResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'serviceSetting' - The query result of the current service setting.
mkGetServiceSettingResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetServiceSettingResponse
mkGetServiceSettingResponse pResponseStatus_ =
  GetServiceSettingResponse'
    { serviceSetting = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The query result of the current service setting.
--
-- /Note:/ Consider using 'serviceSetting' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gssrsServiceSetting :: Lens.Lens' GetServiceSettingResponse (Lude.Maybe ServiceSetting)
gssrsServiceSetting = Lens.lens (serviceSetting :: GetServiceSettingResponse -> Lude.Maybe ServiceSetting) (\s a -> s {serviceSetting = a} :: GetServiceSettingResponse)
{-# DEPRECATED gssrsServiceSetting "Use generic-lens or generic-optics with 'serviceSetting' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gssrsResponseStatus :: Lens.Lens' GetServiceSettingResponse Lude.Int
gssrsResponseStatus = Lens.lens (responseStatus :: GetServiceSettingResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetServiceSettingResponse)
{-# DEPRECATED gssrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
