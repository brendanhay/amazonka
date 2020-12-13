{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.AdminForgetDevice
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Forgets the device, as an administrator.
--
-- Calling this action requires developer credentials.
module Network.AWS.CognitoIdentityProvider.AdminForgetDevice
  ( -- * Creating a request
    AdminForgetDevice (..),
    mkAdminForgetDevice,

    -- ** Request lenses
    afdUserPoolId,
    afdUsername,
    afdDeviceKey,

    -- * Destructuring the response
    AdminForgetDeviceResponse (..),
    mkAdminForgetDeviceResponse,
  )
where

import Network.AWS.CognitoIdentityProvider.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Sends the forgot device request, as an administrator.
--
-- /See:/ 'mkAdminForgetDevice' smart constructor.
data AdminForgetDevice = AdminForgetDevice'
  { -- | The user pool ID.
    userPoolId :: Lude.Text,
    -- | The user name.
    username :: Lude.Sensitive Lude.Text,
    -- | The device key.
    deviceKey :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AdminForgetDevice' with the minimum fields required to make a request.
--
-- * 'userPoolId' - The user pool ID.
-- * 'username' - The user name.
-- * 'deviceKey' - The device key.
mkAdminForgetDevice ::
  -- | 'userPoolId'
  Lude.Text ->
  -- | 'username'
  Lude.Sensitive Lude.Text ->
  -- | 'deviceKey'
  Lude.Text ->
  AdminForgetDevice
mkAdminForgetDevice pUserPoolId_ pUsername_ pDeviceKey_ =
  AdminForgetDevice'
    { userPoolId = pUserPoolId_,
      username = pUsername_,
      deviceKey = pDeviceKey_
    }

-- | The user pool ID.
--
-- /Note:/ Consider using 'userPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
afdUserPoolId :: Lens.Lens' AdminForgetDevice Lude.Text
afdUserPoolId = Lens.lens (userPoolId :: AdminForgetDevice -> Lude.Text) (\s a -> s {userPoolId = a} :: AdminForgetDevice)
{-# DEPRECATED afdUserPoolId "Use generic-lens or generic-optics with 'userPoolId' instead." #-}

-- | The user name.
--
-- /Note:/ Consider using 'username' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
afdUsername :: Lens.Lens' AdminForgetDevice (Lude.Sensitive Lude.Text)
afdUsername = Lens.lens (username :: AdminForgetDevice -> Lude.Sensitive Lude.Text) (\s a -> s {username = a} :: AdminForgetDevice)
{-# DEPRECATED afdUsername "Use generic-lens or generic-optics with 'username' instead." #-}

-- | The device key.
--
-- /Note:/ Consider using 'deviceKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
afdDeviceKey :: Lens.Lens' AdminForgetDevice Lude.Text
afdDeviceKey = Lens.lens (deviceKey :: AdminForgetDevice -> Lude.Text) (\s a -> s {deviceKey = a} :: AdminForgetDevice)
{-# DEPRECATED afdDeviceKey "Use generic-lens or generic-optics with 'deviceKey' instead." #-}

instance Lude.AWSRequest AdminForgetDevice where
  type Rs AdminForgetDevice = AdminForgetDeviceResponse
  request = Req.postJSON cognitoIdentityProviderService
  response = Res.receiveNull AdminForgetDeviceResponse'

instance Lude.ToHeaders AdminForgetDevice where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSCognitoIdentityProviderService.AdminForgetDevice" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON AdminForgetDevice where
  toJSON AdminForgetDevice' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("UserPoolId" Lude..= userPoolId),
            Lude.Just ("Username" Lude..= username),
            Lude.Just ("DeviceKey" Lude..= deviceKey)
          ]
      )

instance Lude.ToPath AdminForgetDevice where
  toPath = Lude.const "/"

instance Lude.ToQuery AdminForgetDevice where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkAdminForgetDeviceResponse' smart constructor.
data AdminForgetDeviceResponse = AdminForgetDeviceResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AdminForgetDeviceResponse' with the minimum fields required to make a request.
mkAdminForgetDeviceResponse ::
  AdminForgetDeviceResponse
mkAdminForgetDeviceResponse = AdminForgetDeviceResponse'
