{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
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
    fdDeviceKey,
    fdAccessToken,

    -- * Destructuring the response
    ForgetDeviceResponse (..),
    mkForgetDeviceResponse,
  )
where

import qualified Network.AWS.CognitoIdentityProvider.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the request to forget the device.
--
-- /See:/ 'mkForgetDevice' smart constructor.
data ForgetDevice = ForgetDevice'
  { -- | The device key.
    deviceKey :: Types.DeviceKeyType,
    -- | The access token for the forgotten device request.
    accessToken :: Core.Maybe Types.AccessToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ForgetDevice' value with any optional fields omitted.
mkForgetDevice ::
  -- | 'deviceKey'
  Types.DeviceKeyType ->
  ForgetDevice
mkForgetDevice deviceKey =
  ForgetDevice' {deviceKey, accessToken = Core.Nothing}

-- | The device key.
--
-- /Note:/ Consider using 'deviceKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fdDeviceKey :: Lens.Lens' ForgetDevice Types.DeviceKeyType
fdDeviceKey = Lens.field @"deviceKey"
{-# DEPRECATED fdDeviceKey "Use generic-lens or generic-optics with 'deviceKey' instead." #-}

-- | The access token for the forgotten device request.
--
-- /Note:/ Consider using 'accessToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fdAccessToken :: Lens.Lens' ForgetDevice (Core.Maybe Types.AccessToken)
fdAccessToken = Lens.field @"accessToken"
{-# DEPRECATED fdAccessToken "Use generic-lens or generic-optics with 'accessToken' instead." #-}

instance Core.FromJSON ForgetDevice where
  toJSON ForgetDevice {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("DeviceKey" Core..= deviceKey),
            ("AccessToken" Core..=) Core.<$> accessToken
          ]
      )

instance Core.AWSRequest ForgetDevice where
  type Rs ForgetDevice = ForgetDeviceResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "AWSCognitoIdentityProviderService.ForgetDevice")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response = Response.receiveNull ForgetDeviceResponse'

-- | /See:/ 'mkForgetDeviceResponse' smart constructor.
data ForgetDeviceResponse = ForgetDeviceResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ForgetDeviceResponse' value with any optional fields omitted.
mkForgetDeviceResponse ::
  ForgetDeviceResponse
mkForgetDeviceResponse = ForgetDeviceResponse'
