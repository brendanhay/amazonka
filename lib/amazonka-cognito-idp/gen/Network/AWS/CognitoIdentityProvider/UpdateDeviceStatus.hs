{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.UpdateDeviceStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the device status.
module Network.AWS.CognitoIdentityProvider.UpdateDeviceStatus
    (
    -- * Creating a request
      UpdateDeviceStatus (..)
    , mkUpdateDeviceStatus
    -- ** Request lenses
    , udsAccessToken
    , udsDeviceKey
    , udsDeviceRememberedStatus

    -- * Destructuring the response
    , UpdateDeviceStatusResponse (..)
    , mkUpdateDeviceStatusResponse
    -- ** Response lenses
    , udsrrsResponseStatus
    ) where

import qualified Network.AWS.CognitoIdentityProvider.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the request to update the device status.
--
-- /See:/ 'mkUpdateDeviceStatus' smart constructor.
data UpdateDeviceStatus = UpdateDeviceStatus'
  { accessToken :: Types.TokenModelType
    -- ^ The access token.
  , deviceKey :: Types.DeviceKeyType
    -- ^ The device key.
  , deviceRememberedStatus :: Core.Maybe Types.DeviceRememberedStatusType
    -- ^ The status of whether a device is remembered.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateDeviceStatus' value with any optional fields omitted.
mkUpdateDeviceStatus
    :: Types.TokenModelType -- ^ 'accessToken'
    -> Types.DeviceKeyType -- ^ 'deviceKey'
    -> UpdateDeviceStatus
mkUpdateDeviceStatus accessToken deviceKey
  = UpdateDeviceStatus'{accessToken, deviceKey,
                        deviceRememberedStatus = Core.Nothing}

-- | The access token.
--
-- /Note:/ Consider using 'accessToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udsAccessToken :: Lens.Lens' UpdateDeviceStatus Types.TokenModelType
udsAccessToken = Lens.field @"accessToken"
{-# INLINEABLE udsAccessToken #-}
{-# DEPRECATED accessToken "Use generic-lens or generic-optics with 'accessToken' instead"  #-}

-- | The device key.
--
-- /Note:/ Consider using 'deviceKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udsDeviceKey :: Lens.Lens' UpdateDeviceStatus Types.DeviceKeyType
udsDeviceKey = Lens.field @"deviceKey"
{-# INLINEABLE udsDeviceKey #-}
{-# DEPRECATED deviceKey "Use generic-lens or generic-optics with 'deviceKey' instead"  #-}

-- | The status of whether a device is remembered.
--
-- /Note:/ Consider using 'deviceRememberedStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udsDeviceRememberedStatus :: Lens.Lens' UpdateDeviceStatus (Core.Maybe Types.DeviceRememberedStatusType)
udsDeviceRememberedStatus = Lens.field @"deviceRememberedStatus"
{-# INLINEABLE udsDeviceRememberedStatus #-}
{-# DEPRECATED deviceRememberedStatus "Use generic-lens or generic-optics with 'deviceRememberedStatus' instead"  #-}

instance Core.ToQuery UpdateDeviceStatus where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateDeviceStatus where
        toHeaders UpdateDeviceStatus{..}
          = Core.pure
              ("X-Amz-Target",
               "AWSCognitoIdentityProviderService.UpdateDeviceStatus")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON UpdateDeviceStatus where
        toJSON UpdateDeviceStatus{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("AccessToken" Core..= accessToken),
                  Core.Just ("DeviceKey" Core..= deviceKey),
                  ("DeviceRememberedStatus" Core..=) Core.<$>
                    deviceRememberedStatus])

instance Core.AWSRequest UpdateDeviceStatus where
        type Rs UpdateDeviceStatus = UpdateDeviceStatusResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 UpdateDeviceStatusResponse' Core.<$> (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | The response to the request to update the device status.
--
-- /See:/ 'mkUpdateDeviceStatusResponse' smart constructor.
newtype UpdateDeviceStatusResponse = UpdateDeviceStatusResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateDeviceStatusResponse' value with any optional fields omitted.
mkUpdateDeviceStatusResponse
    :: Core.Int -- ^ 'responseStatus'
    -> UpdateDeviceStatusResponse
mkUpdateDeviceStatusResponse responseStatus
  = UpdateDeviceStatusResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udsrrsResponseStatus :: Lens.Lens' UpdateDeviceStatusResponse Core.Int
udsrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE udsrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
