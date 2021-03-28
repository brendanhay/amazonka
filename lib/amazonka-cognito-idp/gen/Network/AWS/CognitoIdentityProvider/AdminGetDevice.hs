{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      AdminGetDevice (..)
    , mkAdminGetDevice
    -- ** Request lenses
    , agdDeviceKey
    , agdUserPoolId
    , agdUsername

    -- * Destructuring the response
    , AdminGetDeviceResponse (..)
    , mkAdminGetDeviceResponse
    -- ** Response lenses
    , agdrrsDevice
    , agdrrsResponseStatus
    ) where

import qualified Network.AWS.CognitoIdentityProvider.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the request to get the device, as an administrator.
--
-- /See:/ 'mkAdminGetDevice' smart constructor.
data AdminGetDevice = AdminGetDevice'
  { deviceKey :: Types.DeviceKeyType
    -- ^ The device key.
  , userPoolId :: Types.UserPoolId
    -- ^ The user pool ID.
  , username :: Types.Username
    -- ^ The user name.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AdminGetDevice' value with any optional fields omitted.
mkAdminGetDevice
    :: Types.DeviceKeyType -- ^ 'deviceKey'
    -> Types.UserPoolId -- ^ 'userPoolId'
    -> Types.Username -- ^ 'username'
    -> AdminGetDevice
mkAdminGetDevice deviceKey userPoolId username
  = AdminGetDevice'{deviceKey, userPoolId, username}

-- | The device key.
--
-- /Note:/ Consider using 'deviceKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
agdDeviceKey :: Lens.Lens' AdminGetDevice Types.DeviceKeyType
agdDeviceKey = Lens.field @"deviceKey"
{-# INLINEABLE agdDeviceKey #-}
{-# DEPRECATED deviceKey "Use generic-lens or generic-optics with 'deviceKey' instead"  #-}

-- | The user pool ID.
--
-- /Note:/ Consider using 'userPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
agdUserPoolId :: Lens.Lens' AdminGetDevice Types.UserPoolId
agdUserPoolId = Lens.field @"userPoolId"
{-# INLINEABLE agdUserPoolId #-}
{-# DEPRECATED userPoolId "Use generic-lens or generic-optics with 'userPoolId' instead"  #-}

-- | The user name.
--
-- /Note:/ Consider using 'username' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
agdUsername :: Lens.Lens' AdminGetDevice Types.Username
agdUsername = Lens.field @"username"
{-# INLINEABLE agdUsername #-}
{-# DEPRECATED username "Use generic-lens or generic-optics with 'username' instead"  #-}

instance Core.ToQuery AdminGetDevice where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders AdminGetDevice where
        toHeaders AdminGetDevice{..}
          = Core.pure
              ("X-Amz-Target",
               "AWSCognitoIdentityProviderService.AdminGetDevice")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON AdminGetDevice where
        toJSON AdminGetDevice{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("DeviceKey" Core..= deviceKey),
                  Core.Just ("UserPoolId" Core..= userPoolId),
                  Core.Just ("Username" Core..= username)])

instance Core.AWSRequest AdminGetDevice where
        type Rs AdminGetDevice = AdminGetDeviceResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 AdminGetDeviceResponse' Core.<$>
                   (x Core..: "Device") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Gets the device response, as an administrator.
--
-- /See:/ 'mkAdminGetDeviceResponse' smart constructor.
data AdminGetDeviceResponse = AdminGetDeviceResponse'
  { device :: Types.DeviceType
    -- ^ The device.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'AdminGetDeviceResponse' value with any optional fields omitted.
mkAdminGetDeviceResponse
    :: Types.DeviceType -- ^ 'device'
    -> Core.Int -- ^ 'responseStatus'
    -> AdminGetDeviceResponse
mkAdminGetDeviceResponse device responseStatus
  = AdminGetDeviceResponse'{device, responseStatus}

-- | The device.
--
-- /Note:/ Consider using 'device' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
agdrrsDevice :: Lens.Lens' AdminGetDeviceResponse Types.DeviceType
agdrrsDevice = Lens.field @"device"
{-# INLINEABLE agdrrsDevice #-}
{-# DEPRECATED device "Use generic-lens or generic-optics with 'device' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
agdrrsResponseStatus :: Lens.Lens' AdminGetDeviceResponse Core.Int
agdrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE agdrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
