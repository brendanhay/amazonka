{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.AdminGetDevice
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the device, as an administrator.
--
-- Calling this action requires developer credentials.
module Network.AWS.CognitoIdentityProvider.AdminGetDevice
  ( -- * Creating a request
    AdminGetDevice (..),
    mkAdminGetDevice,

    -- ** Request lenses
    agdDeviceKey,
    agdUserPoolId,
    agdUsername,

    -- * Destructuring the response
    AdminGetDeviceResponse (..),
    mkAdminGetDeviceResponse,

    -- ** Response lenses
    agdrsResponseStatus,
    agdrsDevice,
  )
where

import Network.AWS.CognitoIdentityProvider.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the request to get the device, as an administrator.
--
-- /See:/ 'mkAdminGetDevice' smart constructor.
data AdminGetDevice = AdminGetDevice'
  { deviceKey :: Lude.Text,
    userPoolId :: Lude.Text,
    username :: Lude.Sensitive Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AdminGetDevice' with the minimum fields required to make a request.
--
-- * 'deviceKey' - The device key.
-- * 'userPoolId' - The user pool ID.
-- * 'username' - The user name.
mkAdminGetDevice ::
  -- | 'deviceKey'
  Lude.Text ->
  -- | 'userPoolId'
  Lude.Text ->
  -- | 'username'
  Lude.Sensitive Lude.Text ->
  AdminGetDevice
mkAdminGetDevice pDeviceKey_ pUserPoolId_ pUsername_ =
  AdminGetDevice'
    { deviceKey = pDeviceKey_,
      userPoolId = pUserPoolId_,
      username = pUsername_
    }

-- | The device key.
--
-- /Note:/ Consider using 'deviceKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
agdDeviceKey :: Lens.Lens' AdminGetDevice Lude.Text
agdDeviceKey = Lens.lens (deviceKey :: AdminGetDevice -> Lude.Text) (\s a -> s {deviceKey = a} :: AdminGetDevice)
{-# DEPRECATED agdDeviceKey "Use generic-lens or generic-optics with 'deviceKey' instead." #-}

-- | The user pool ID.
--
-- /Note:/ Consider using 'userPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
agdUserPoolId :: Lens.Lens' AdminGetDevice Lude.Text
agdUserPoolId = Lens.lens (userPoolId :: AdminGetDevice -> Lude.Text) (\s a -> s {userPoolId = a} :: AdminGetDevice)
{-# DEPRECATED agdUserPoolId "Use generic-lens or generic-optics with 'userPoolId' instead." #-}

-- | The user name.
--
-- /Note:/ Consider using 'username' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
agdUsername :: Lens.Lens' AdminGetDevice (Lude.Sensitive Lude.Text)
agdUsername = Lens.lens (username :: AdminGetDevice -> Lude.Sensitive Lude.Text) (\s a -> s {username = a} :: AdminGetDevice)
{-# DEPRECATED agdUsername "Use generic-lens or generic-optics with 'username' instead." #-}

instance Lude.AWSRequest AdminGetDevice where
  type Rs AdminGetDevice = AdminGetDeviceResponse
  request = Req.postJSON cognitoIdentityProviderService
  response =
    Res.receiveJSON
      ( \s h x ->
          AdminGetDeviceResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s)) Lude.<*> (x Lude..:> "Device")
      )

instance Lude.ToHeaders AdminGetDevice where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSCognitoIdentityProviderService.AdminGetDevice" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON AdminGetDevice where
  toJSON AdminGetDevice' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("DeviceKey" Lude..= deviceKey),
            Lude.Just ("UserPoolId" Lude..= userPoolId),
            Lude.Just ("Username" Lude..= username)
          ]
      )

instance Lude.ToPath AdminGetDevice where
  toPath = Lude.const "/"

instance Lude.ToQuery AdminGetDevice where
  toQuery = Lude.const Lude.mempty

-- | Gets the device response, as an administrator.
--
-- /See:/ 'mkAdminGetDeviceResponse' smart constructor.
data AdminGetDeviceResponse = AdminGetDeviceResponse'
  { responseStatus ::
      Lude.Int,
    device :: DeviceType
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AdminGetDeviceResponse' with the minimum fields required to make a request.
--
-- * 'device' - The device.
-- * 'responseStatus' - The response status code.
mkAdminGetDeviceResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'device'
  DeviceType ->
  AdminGetDeviceResponse
mkAdminGetDeviceResponse pResponseStatus_ pDevice_ =
  AdminGetDeviceResponse'
    { responseStatus = pResponseStatus_,
      device = pDevice_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
agdrsResponseStatus :: Lens.Lens' AdminGetDeviceResponse Lude.Int
agdrsResponseStatus = Lens.lens (responseStatus :: AdminGetDeviceResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: AdminGetDeviceResponse)
{-# DEPRECATED agdrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | The device.
--
-- /Note:/ Consider using 'device' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
agdrsDevice :: Lens.Lens' AdminGetDeviceResponse DeviceType
agdrsDevice = Lens.lens (device :: AdminGetDeviceResponse -> DeviceType) (\s a -> s {device = a} :: AdminGetDeviceResponse)
{-# DEPRECATED agdrsDevice "Use generic-lens or generic-optics with 'device' instead." #-}
