{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    dasPrincipalARN,
    dasName,

    -- * Destructuring the response
    DeleteAccountSettingResponse (..),
    mkDeleteAccountSettingResponse,

    -- ** Response lenses
    dasrsSetting,
    dasrsResponseStatus,
  )
where

import Network.AWS.ECS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteAccountSetting' smart constructor.
data DeleteAccountSetting = DeleteAccountSetting'
  { principalARN ::
      Lude.Maybe Lude.Text,
    name :: SettingName
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteAccountSetting' with the minimum fields required to make a request.
--
-- * 'name' - The resource name for which to disable the account setting. If @serviceLongArnFormat@ is specified, the ARN for your Amazon ECS services is affected. If @taskLongArnFormat@ is specified, the ARN and resource ID for your Amazon ECS tasks is affected. If @containerInstanceLongArnFormat@ is specified, the ARN and resource ID for your Amazon ECS container instances is affected. If @awsvpcTrunking@ is specified, the ENI limit for your Amazon ECS container instances is affected.
-- * 'principalARN' - The ARN of the principal, which can be an IAM user, IAM role, or the root user. If you specify the root user, it disables the account setting for all IAM users, IAM roles, and the root user of the account unless an IAM user or role explicitly overrides these settings. If this field is omitted, the setting is changed only for the authenticated user.
mkDeleteAccountSetting ::
  -- | 'name'
  SettingName ->
  DeleteAccountSetting
mkDeleteAccountSetting pName_ =
  DeleteAccountSetting' {principalARN = Lude.Nothing, name = pName_}

-- | The ARN of the principal, which can be an IAM user, IAM role, or the root user. If you specify the root user, it disables the account setting for all IAM users, IAM roles, and the root user of the account unless an IAM user or role explicitly overrides these settings. If this field is omitted, the setting is changed only for the authenticated user.
--
-- /Note:/ Consider using 'principalARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dasPrincipalARN :: Lens.Lens' DeleteAccountSetting (Lude.Maybe Lude.Text)
dasPrincipalARN = Lens.lens (principalARN :: DeleteAccountSetting -> Lude.Maybe Lude.Text) (\s a -> s {principalARN = a} :: DeleteAccountSetting)
{-# DEPRECATED dasPrincipalARN "Use generic-lens or generic-optics with 'principalARN' instead." #-}

-- | The resource name for which to disable the account setting. If @serviceLongArnFormat@ is specified, the ARN for your Amazon ECS services is affected. If @taskLongArnFormat@ is specified, the ARN and resource ID for your Amazon ECS tasks is affected. If @containerInstanceLongArnFormat@ is specified, the ARN and resource ID for your Amazon ECS container instances is affected. If @awsvpcTrunking@ is specified, the ENI limit for your Amazon ECS container instances is affected.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dasName :: Lens.Lens' DeleteAccountSetting SettingName
dasName = Lens.lens (name :: DeleteAccountSetting -> SettingName) (\s a -> s {name = a} :: DeleteAccountSetting)
{-# DEPRECATED dasName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.AWSRequest DeleteAccountSetting where
  type Rs DeleteAccountSetting = DeleteAccountSettingResponse
  request = Req.postJSON ecsService
  response =
    Res.receiveJSON
      ( \s h x ->
          DeleteAccountSettingResponse'
            Lude.<$> (x Lude..?> "setting") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteAccountSetting where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AmazonEC2ContainerServiceV20141113.DeleteAccountSetting" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteAccountSetting where
  toJSON DeleteAccountSetting' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("principalArn" Lude..=) Lude.<$> principalARN,
            Lude.Just ("name" Lude..= name)
          ]
      )

instance Lude.ToPath DeleteAccountSetting where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteAccountSetting where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteAccountSettingResponse' smart constructor.
data DeleteAccountSettingResponse = DeleteAccountSettingResponse'
  { setting ::
      Lude.Maybe Setting,
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

-- | Creates a value of 'DeleteAccountSettingResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'setting' - The account setting for the specified principal ARN.
mkDeleteAccountSettingResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteAccountSettingResponse
mkDeleteAccountSettingResponse pResponseStatus_ =
  DeleteAccountSettingResponse'
    { setting = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The account setting for the specified principal ARN.
--
-- /Note:/ Consider using 'setting' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dasrsSetting :: Lens.Lens' DeleteAccountSettingResponse (Lude.Maybe Setting)
dasrsSetting = Lens.lens (setting :: DeleteAccountSettingResponse -> Lude.Maybe Setting) (\s a -> s {setting = a} :: DeleteAccountSettingResponse)
{-# DEPRECATED dasrsSetting "Use generic-lens or generic-optics with 'setting' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dasrsResponseStatus :: Lens.Lens' DeleteAccountSettingResponse Lude.Int
dasrsResponseStatus = Lens.lens (responseStatus :: DeleteAccountSettingResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteAccountSettingResponse)
{-# DEPRECATED dasrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
