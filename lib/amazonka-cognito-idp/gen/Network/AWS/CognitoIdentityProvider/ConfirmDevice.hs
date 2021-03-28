{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.ConfirmDevice
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Confirms tracking of the device. This API call is the call that begins device tracking.
module Network.AWS.CognitoIdentityProvider.ConfirmDevice
    (
    -- * Creating a request
      ConfirmDevice (..)
    , mkConfirmDevice
    -- ** Request lenses
    , cdAccessToken
    , cdDeviceKey
    , cdDeviceName
    , cdDeviceSecretVerifierConfig

    -- * Destructuring the response
    , ConfirmDeviceResponse (..)
    , mkConfirmDeviceResponse
    -- ** Response lenses
    , cdrrsUserConfirmationNecessary
    , cdrrsResponseStatus
    ) where

import qualified Network.AWS.CognitoIdentityProvider.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Confirms the device request.
--
-- /See:/ 'mkConfirmDevice' smart constructor.
data ConfirmDevice = ConfirmDevice'
  { accessToken :: Types.TokenModelType
    -- ^ The access token.
  , deviceKey :: Types.DeviceKeyType
    -- ^ The device key.
  , deviceName :: Core.Maybe Types.DeviceNameType
    -- ^ The device name.
  , deviceSecretVerifierConfig :: Core.Maybe Types.DeviceSecretVerifierConfigType
    -- ^ The configuration of the device secret verifier.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ConfirmDevice' value with any optional fields omitted.
mkConfirmDevice
    :: Types.TokenModelType -- ^ 'accessToken'
    -> Types.DeviceKeyType -- ^ 'deviceKey'
    -> ConfirmDevice
mkConfirmDevice accessToken deviceKey
  = ConfirmDevice'{accessToken, deviceKey, deviceName = Core.Nothing,
                   deviceSecretVerifierConfig = Core.Nothing}

-- | The access token.
--
-- /Note:/ Consider using 'accessToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdAccessToken :: Lens.Lens' ConfirmDevice Types.TokenModelType
cdAccessToken = Lens.field @"accessToken"
{-# INLINEABLE cdAccessToken #-}
{-# DEPRECATED accessToken "Use generic-lens or generic-optics with 'accessToken' instead"  #-}

-- | The device key.
--
-- /Note:/ Consider using 'deviceKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdDeviceKey :: Lens.Lens' ConfirmDevice Types.DeviceKeyType
cdDeviceKey = Lens.field @"deviceKey"
{-# INLINEABLE cdDeviceKey #-}
{-# DEPRECATED deviceKey "Use generic-lens or generic-optics with 'deviceKey' instead"  #-}

-- | The device name.
--
-- /Note:/ Consider using 'deviceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdDeviceName :: Lens.Lens' ConfirmDevice (Core.Maybe Types.DeviceNameType)
cdDeviceName = Lens.field @"deviceName"
{-# INLINEABLE cdDeviceName #-}
{-# DEPRECATED deviceName "Use generic-lens or generic-optics with 'deviceName' instead"  #-}

-- | The configuration of the device secret verifier.
--
-- /Note:/ Consider using 'deviceSecretVerifierConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdDeviceSecretVerifierConfig :: Lens.Lens' ConfirmDevice (Core.Maybe Types.DeviceSecretVerifierConfigType)
cdDeviceSecretVerifierConfig = Lens.field @"deviceSecretVerifierConfig"
{-# INLINEABLE cdDeviceSecretVerifierConfig #-}
{-# DEPRECATED deviceSecretVerifierConfig "Use generic-lens or generic-optics with 'deviceSecretVerifierConfig' instead"  #-}

instance Core.ToQuery ConfirmDevice where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ConfirmDevice where
        toHeaders ConfirmDevice{..}
          = Core.pure
              ("X-Amz-Target", "AWSCognitoIdentityProviderService.ConfirmDevice")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ConfirmDevice where
        toJSON ConfirmDevice{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("AccessToken" Core..= accessToken),
                  Core.Just ("DeviceKey" Core..= deviceKey),
                  ("DeviceName" Core..=) Core.<$> deviceName,
                  ("DeviceSecretVerifierConfig" Core..=) Core.<$>
                    deviceSecretVerifierConfig])

instance Core.AWSRequest ConfirmDevice where
        type Rs ConfirmDevice = ConfirmDeviceResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ConfirmDeviceResponse' Core.<$>
                   (x Core..:? "UserConfirmationNecessary") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Confirms the device response.
--
-- /See:/ 'mkConfirmDeviceResponse' smart constructor.
data ConfirmDeviceResponse = ConfirmDeviceResponse'
  { userConfirmationNecessary :: Core.Maybe Core.Bool
    -- ^ Indicates whether the user confirmation is necessary to confirm the device response.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ConfirmDeviceResponse' value with any optional fields omitted.
mkConfirmDeviceResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ConfirmDeviceResponse
mkConfirmDeviceResponse responseStatus
  = ConfirmDeviceResponse'{userConfirmationNecessary = Core.Nothing,
                           responseStatus}

-- | Indicates whether the user confirmation is necessary to confirm the device response.
--
-- /Note:/ Consider using 'userConfirmationNecessary' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdrrsUserConfirmationNecessary :: Lens.Lens' ConfirmDeviceResponse (Core.Maybe Core.Bool)
cdrrsUserConfirmationNecessary = Lens.field @"userConfirmationNecessary"
{-# INLINEABLE cdrrsUserConfirmationNecessary #-}
{-# DEPRECATED userConfirmationNecessary "Use generic-lens or generic-optics with 'userConfirmationNecessary' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdrrsResponseStatus :: Lens.Lens' ConfirmDeviceResponse Core.Int
cdrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE cdrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
