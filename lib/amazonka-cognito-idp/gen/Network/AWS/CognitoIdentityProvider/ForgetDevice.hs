{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.ForgetDevice
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Forgets the specified device.
module Network.AWS.CognitoIdentityProvider.ForgetDevice
  ( -- * Creating a request
    ForgetDevice (..),
    mkForgetDevice,

    -- ** Request lenses
    fdAccessToken,
    fdDeviceKey,

    -- * Destructuring the response
    ForgetDeviceResponse (..),
    mkForgetDeviceResponse,
  )
where

import Network.AWS.CognitoIdentityProvider.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the request to forget the device.
--
-- /See:/ 'mkForgetDevice' smart constructor.
data ForgetDevice = ForgetDevice'
  { accessToken ::
      Lude.Maybe (Lude.Sensitive Lude.Text),
    deviceKey :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ForgetDevice' with the minimum fields required to make a request.
--
-- * 'accessToken' - The access token for the forgotten device request.
-- * 'deviceKey' - The device key.
mkForgetDevice ::
  -- | 'deviceKey'
  Lude.Text ->
  ForgetDevice
mkForgetDevice pDeviceKey_ =
  ForgetDevice'
    { accessToken = Lude.Nothing,
      deviceKey = pDeviceKey_
    }

-- | The access token for the forgotten device request.
--
-- /Note:/ Consider using 'accessToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fdAccessToken :: Lens.Lens' ForgetDevice (Lude.Maybe (Lude.Sensitive Lude.Text))
fdAccessToken = Lens.lens (accessToken :: ForgetDevice -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {accessToken = a} :: ForgetDevice)
{-# DEPRECATED fdAccessToken "Use generic-lens or generic-optics with 'accessToken' instead." #-}

-- | The device key.
--
-- /Note:/ Consider using 'deviceKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fdDeviceKey :: Lens.Lens' ForgetDevice Lude.Text
fdDeviceKey = Lens.lens (deviceKey :: ForgetDevice -> Lude.Text) (\s a -> s {deviceKey = a} :: ForgetDevice)
{-# DEPRECATED fdDeviceKey "Use generic-lens or generic-optics with 'deviceKey' instead." #-}

instance Lude.AWSRequest ForgetDevice where
  type Rs ForgetDevice = ForgetDeviceResponse
  request = Req.postJSON cognitoIdentityProviderService
  response = Res.receiveNull ForgetDeviceResponse'

instance Lude.ToHeaders ForgetDevice where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSCognitoIdentityProviderService.ForgetDevice" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ForgetDevice where
  toJSON ForgetDevice' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("AccessToken" Lude..=) Lude.<$> accessToken,
            Lude.Just ("DeviceKey" Lude..= deviceKey)
          ]
      )

instance Lude.ToPath ForgetDevice where
  toPath = Lude.const "/"

instance Lude.ToQuery ForgetDevice where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkForgetDeviceResponse' smart constructor.
data ForgetDeviceResponse = ForgetDeviceResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ForgetDeviceResponse' with the minimum fields required to make a request.
mkForgetDeviceResponse ::
  ForgetDeviceResponse
mkForgetDeviceResponse = ForgetDeviceResponse'
