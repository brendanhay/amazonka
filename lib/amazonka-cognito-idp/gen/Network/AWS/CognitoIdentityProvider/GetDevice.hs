{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.GetDevice
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the device.
module Network.AWS.CognitoIdentityProvider.GetDevice
    (
    -- * Creating a request
      GetDevice (..)
    , mkGetDevice
    -- ** Request lenses
    , gdDeviceKey
    , gdAccessToken

    -- * Destructuring the response
    , GetDeviceResponse (..)
    , mkGetDeviceResponse
    -- ** Response lenses
    , gdrrsDevice
    , gdrrsResponseStatus
    ) where

import qualified Network.AWS.CognitoIdentityProvider.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the request to get the device.
--
-- /See:/ 'mkGetDevice' smart constructor.
data GetDevice = GetDevice'
  { deviceKey :: Types.DeviceKeyType
    -- ^ The device key.
  , accessToken :: Core.Maybe Types.AccessToken
    -- ^ The access token.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetDevice' value with any optional fields omitted.
mkGetDevice
    :: Types.DeviceKeyType -- ^ 'deviceKey'
    -> GetDevice
mkGetDevice deviceKey
  = GetDevice'{deviceKey, accessToken = Core.Nothing}

-- | The device key.
--
-- /Note:/ Consider using 'deviceKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdDeviceKey :: Lens.Lens' GetDevice Types.DeviceKeyType
gdDeviceKey = Lens.field @"deviceKey"
{-# INLINEABLE gdDeviceKey #-}
{-# DEPRECATED deviceKey "Use generic-lens or generic-optics with 'deviceKey' instead"  #-}

-- | The access token.
--
-- /Note:/ Consider using 'accessToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdAccessToken :: Lens.Lens' GetDevice (Core.Maybe Types.AccessToken)
gdAccessToken = Lens.field @"accessToken"
{-# INLINEABLE gdAccessToken #-}
{-# DEPRECATED accessToken "Use generic-lens or generic-optics with 'accessToken' instead"  #-}

instance Core.ToQuery GetDevice where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetDevice where
        toHeaders GetDevice{..}
          = Core.pure
              ("X-Amz-Target", "AWSCognitoIdentityProviderService.GetDevice")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetDevice where
        toJSON GetDevice{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("DeviceKey" Core..= deviceKey),
                  ("AccessToken" Core..=) Core.<$> accessToken])

instance Core.AWSRequest GetDevice where
        type Rs GetDevice = GetDeviceResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetDeviceResponse' Core.<$>
                   (x Core..: "Device") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Gets the device response.
--
-- /See:/ 'mkGetDeviceResponse' smart constructor.
data GetDeviceResponse = GetDeviceResponse'
  { device :: Types.DeviceType
    -- ^ The device.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'GetDeviceResponse' value with any optional fields omitted.
mkGetDeviceResponse
    :: Types.DeviceType -- ^ 'device'
    -> Core.Int -- ^ 'responseStatus'
    -> GetDeviceResponse
mkGetDeviceResponse device responseStatus
  = GetDeviceResponse'{device, responseStatus}

-- | The device.
--
-- /Note:/ Consider using 'device' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdrrsDevice :: Lens.Lens' GetDeviceResponse Types.DeviceType
gdrrsDevice = Lens.field @"device"
{-# INLINEABLE gdrrsDevice #-}
{-# DEPRECATED device "Use generic-lens or generic-optics with 'device' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdrrsResponseStatus :: Lens.Lens' GetDeviceResponse Core.Int
gdrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gdrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
