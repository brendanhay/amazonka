{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoSync.RegisterDevice
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Registers a device to receive push sync notifications.
--
-- This API can only be called with temporary credentials provided by Cognito Identity. You cannot call this API with developer credentials.
module Network.AWS.CognitoSync.RegisterDevice
  ( -- * Creating a request
    RegisterDevice (..),
    mkRegisterDevice,

    -- ** Request lenses
    rdIdentityPoolId,
    rdIdentityId,
    rdPlatform,
    rdToken,

    -- * Destructuring the response
    RegisterDeviceResponse (..),
    mkRegisterDeviceResponse,

    -- ** Response lenses
    rdrsDeviceId,
    rdrsResponseStatus,
  )
where

import Network.AWS.CognitoSync.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | A request to RegisterDevice.
--
-- /See:/ 'mkRegisterDevice' smart constructor.
data RegisterDevice = RegisterDevice'
  { identityPoolId :: Lude.Text,
    identityId :: Lude.Text,
    platform :: Platform,
    token :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RegisterDevice' with the minimum fields required to make a request.
--
-- * 'identityId' - The unique ID for this identity.
-- * 'identityPoolId' - A name-spaced GUID (for example, us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon Cognito. Here, the ID of the pool that the identity belongs to.
-- * 'platform' - The SNS platform type (e.g. GCM, SDM, APNS, APNS_SANDBOX).
-- * 'token' - The push token.
mkRegisterDevice ::
  -- | 'identityPoolId'
  Lude.Text ->
  -- | 'identityId'
  Lude.Text ->
  -- | 'platform'
  Platform ->
  -- | 'token'
  Lude.Text ->
  RegisterDevice
mkRegisterDevice pIdentityPoolId_ pIdentityId_ pPlatform_ pToken_ =
  RegisterDevice'
    { identityPoolId = pIdentityPoolId_,
      identityId = pIdentityId_,
      platform = pPlatform_,
      token = pToken_
    }

-- | A name-spaced GUID (for example, us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon Cognito. Here, the ID of the pool that the identity belongs to.
--
-- /Note:/ Consider using 'identityPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdIdentityPoolId :: Lens.Lens' RegisterDevice Lude.Text
rdIdentityPoolId = Lens.lens (identityPoolId :: RegisterDevice -> Lude.Text) (\s a -> s {identityPoolId = a} :: RegisterDevice)
{-# DEPRECATED rdIdentityPoolId "Use generic-lens or generic-optics with 'identityPoolId' instead." #-}

-- | The unique ID for this identity.
--
-- /Note:/ Consider using 'identityId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdIdentityId :: Lens.Lens' RegisterDevice Lude.Text
rdIdentityId = Lens.lens (identityId :: RegisterDevice -> Lude.Text) (\s a -> s {identityId = a} :: RegisterDevice)
{-# DEPRECATED rdIdentityId "Use generic-lens or generic-optics with 'identityId' instead." #-}

-- | The SNS platform type (e.g. GCM, SDM, APNS, APNS_SANDBOX).
--
-- /Note:/ Consider using 'platform' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdPlatform :: Lens.Lens' RegisterDevice Platform
rdPlatform = Lens.lens (platform :: RegisterDevice -> Platform) (\s a -> s {platform = a} :: RegisterDevice)
{-# DEPRECATED rdPlatform "Use generic-lens or generic-optics with 'platform' instead." #-}

-- | The push token.
--
-- /Note:/ Consider using 'token' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdToken :: Lens.Lens' RegisterDevice Lude.Text
rdToken = Lens.lens (token :: RegisterDevice -> Lude.Text) (\s a -> s {token = a} :: RegisterDevice)
{-# DEPRECATED rdToken "Use generic-lens or generic-optics with 'token' instead." #-}

instance Lude.AWSRequest RegisterDevice where
  type Rs RegisterDevice = RegisterDeviceResponse
  request = Req.postJSON cognitoSyncService
  response =
    Res.receiveJSON
      ( \s h x ->
          RegisterDeviceResponse'
            Lude.<$> (x Lude..?> "DeviceId") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders RegisterDevice where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON RegisterDevice where
  toJSON RegisterDevice' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("Platform" Lude..= platform),
            Lude.Just ("Token" Lude..= token)
          ]
      )

instance Lude.ToPath RegisterDevice where
  toPath RegisterDevice' {..} =
    Lude.mconcat
      [ "/identitypools/",
        Lude.toBS identityPoolId,
        "/identity/",
        Lude.toBS identityId,
        "/device"
      ]

instance Lude.ToQuery RegisterDevice where
  toQuery = Lude.const Lude.mempty

-- | Response to a RegisterDevice request.
--
-- /See:/ 'mkRegisterDeviceResponse' smart constructor.
data RegisterDeviceResponse = RegisterDeviceResponse'
  { deviceId ::
      Lude.Maybe Lude.Text,
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

-- | Creates a value of 'RegisterDeviceResponse' with the minimum fields required to make a request.
--
-- * 'deviceId' - The unique ID generated for this device by Cognito.
-- * 'responseStatus' - The response status code.
mkRegisterDeviceResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  RegisterDeviceResponse
mkRegisterDeviceResponse pResponseStatus_ =
  RegisterDeviceResponse'
    { deviceId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The unique ID generated for this device by Cognito.
--
-- /Note:/ Consider using 'deviceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdrsDeviceId :: Lens.Lens' RegisterDeviceResponse (Lude.Maybe Lude.Text)
rdrsDeviceId = Lens.lens (deviceId :: RegisterDeviceResponse -> Lude.Maybe Lude.Text) (\s a -> s {deviceId = a} :: RegisterDeviceResponse)
{-# DEPRECATED rdrsDeviceId "Use generic-lens or generic-optics with 'deviceId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdrsResponseStatus :: Lens.Lens' RegisterDeviceResponse Lude.Int
rdrsResponseStatus = Lens.lens (responseStatus :: RegisterDeviceResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: RegisterDeviceResponse)
{-# DEPRECATED rdrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
