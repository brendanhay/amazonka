{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.AdminUpdateDeviceStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the device status as an administrator.
--
-- Calling this action requires developer credentials.
module Network.AWS.CognitoIdentityProvider.AdminUpdateDeviceStatus
  ( -- * Creating a request
    AdminUpdateDeviceStatus (..),
    mkAdminUpdateDeviceStatus,

    -- ** Request lenses
    audsDeviceRememberedStatus,
    audsUserPoolId,
    audsUsername,
    audsDeviceKey,

    -- * Destructuring the response
    AdminUpdateDeviceStatusResponse (..),
    mkAdminUpdateDeviceStatusResponse,

    -- ** Response lenses
    audsrsResponseStatus,
  )
where

import Network.AWS.CognitoIdentityProvider.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | The request to update the device status, as an administrator.
--
-- /See:/ 'mkAdminUpdateDeviceStatus' smart constructor.
data AdminUpdateDeviceStatus = AdminUpdateDeviceStatus'
  { deviceRememberedStatus ::
      Lude.Maybe DeviceRememberedStatusType,
    userPoolId :: Lude.Text,
    username :: Lude.Sensitive Lude.Text,
    deviceKey :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AdminUpdateDeviceStatus' with the minimum fields required to make a request.
--
-- * 'deviceKey' - The device key.
-- * 'deviceRememberedStatus' - The status indicating whether a device has been remembered or not.
-- * 'userPoolId' - The user pool ID.
-- * 'username' - The user name.
mkAdminUpdateDeviceStatus ::
  -- | 'userPoolId'
  Lude.Text ->
  -- | 'username'
  Lude.Sensitive Lude.Text ->
  -- | 'deviceKey'
  Lude.Text ->
  AdminUpdateDeviceStatus
mkAdminUpdateDeviceStatus pUserPoolId_ pUsername_ pDeviceKey_ =
  AdminUpdateDeviceStatus'
    { deviceRememberedStatus = Lude.Nothing,
      userPoolId = pUserPoolId_,
      username = pUsername_,
      deviceKey = pDeviceKey_
    }

-- | The status indicating whether a device has been remembered or not.
--
-- /Note:/ Consider using 'deviceRememberedStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
audsDeviceRememberedStatus :: Lens.Lens' AdminUpdateDeviceStatus (Lude.Maybe DeviceRememberedStatusType)
audsDeviceRememberedStatus = Lens.lens (deviceRememberedStatus :: AdminUpdateDeviceStatus -> Lude.Maybe DeviceRememberedStatusType) (\s a -> s {deviceRememberedStatus = a} :: AdminUpdateDeviceStatus)
{-# DEPRECATED audsDeviceRememberedStatus "Use generic-lens or generic-optics with 'deviceRememberedStatus' instead." #-}

-- | The user pool ID.
--
-- /Note:/ Consider using 'userPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
audsUserPoolId :: Lens.Lens' AdminUpdateDeviceStatus Lude.Text
audsUserPoolId = Lens.lens (userPoolId :: AdminUpdateDeviceStatus -> Lude.Text) (\s a -> s {userPoolId = a} :: AdminUpdateDeviceStatus)
{-# DEPRECATED audsUserPoolId "Use generic-lens or generic-optics with 'userPoolId' instead." #-}

-- | The user name.
--
-- /Note:/ Consider using 'username' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
audsUsername :: Lens.Lens' AdminUpdateDeviceStatus (Lude.Sensitive Lude.Text)
audsUsername = Lens.lens (username :: AdminUpdateDeviceStatus -> Lude.Sensitive Lude.Text) (\s a -> s {username = a} :: AdminUpdateDeviceStatus)
{-# DEPRECATED audsUsername "Use generic-lens or generic-optics with 'username' instead." #-}

-- | The device key.
--
-- /Note:/ Consider using 'deviceKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
audsDeviceKey :: Lens.Lens' AdminUpdateDeviceStatus Lude.Text
audsDeviceKey = Lens.lens (deviceKey :: AdminUpdateDeviceStatus -> Lude.Text) (\s a -> s {deviceKey = a} :: AdminUpdateDeviceStatus)
{-# DEPRECATED audsDeviceKey "Use generic-lens or generic-optics with 'deviceKey' instead." #-}

instance Lude.AWSRequest AdminUpdateDeviceStatus where
  type Rs AdminUpdateDeviceStatus = AdminUpdateDeviceStatusResponse
  request = Req.postJSON cognitoIdentityProviderService
  response =
    Res.receiveEmpty
      ( \s h x ->
          AdminUpdateDeviceStatusResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders AdminUpdateDeviceStatus where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSCognitoIdentityProviderService.AdminUpdateDeviceStatus" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON AdminUpdateDeviceStatus where
  toJSON AdminUpdateDeviceStatus' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("DeviceRememberedStatus" Lude..=)
              Lude.<$> deviceRememberedStatus,
            Lude.Just ("UserPoolId" Lude..= userPoolId),
            Lude.Just ("Username" Lude..= username),
            Lude.Just ("DeviceKey" Lude..= deviceKey)
          ]
      )

instance Lude.ToPath AdminUpdateDeviceStatus where
  toPath = Lude.const "/"

instance Lude.ToQuery AdminUpdateDeviceStatus where
  toQuery = Lude.const Lude.mempty

-- | The status response from the request to update the device, as an administrator.
--
-- /See:/ 'mkAdminUpdateDeviceStatusResponse' smart constructor.
newtype AdminUpdateDeviceStatusResponse = AdminUpdateDeviceStatusResponse'
  { responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AdminUpdateDeviceStatusResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkAdminUpdateDeviceStatusResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  AdminUpdateDeviceStatusResponse
mkAdminUpdateDeviceStatusResponse pResponseStatus_ =
  AdminUpdateDeviceStatusResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
audsrsResponseStatus :: Lens.Lens' AdminUpdateDeviceStatusResponse Lude.Int
audsrsResponseStatus = Lens.lens (responseStatus :: AdminUpdateDeviceStatusResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: AdminUpdateDeviceStatusResponse)
{-# DEPRECATED audsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
