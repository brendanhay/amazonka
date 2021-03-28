{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      AdminUpdateDeviceStatus (..)
    , mkAdminUpdateDeviceStatus
    -- ** Request lenses
    , audsUserPoolId
    , audsUsername
    , audsDeviceKey
    , audsDeviceRememberedStatus

    -- * Destructuring the response
    , AdminUpdateDeviceStatusResponse (..)
    , mkAdminUpdateDeviceStatusResponse
    -- ** Response lenses
    , audsrrsResponseStatus
    ) where

import qualified Network.AWS.CognitoIdentityProvider.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The request to update the device status, as an administrator.
--
-- /See:/ 'mkAdminUpdateDeviceStatus' smart constructor.
data AdminUpdateDeviceStatus = AdminUpdateDeviceStatus'
  { userPoolId :: Types.UserPoolId
    -- ^ The user pool ID.
  , username :: Types.Username
    -- ^ The user name.
  , deviceKey :: Types.DeviceKeyType
    -- ^ The device key.
  , deviceRememberedStatus :: Core.Maybe Types.DeviceRememberedStatusType
    -- ^ The status indicating whether a device has been remembered or not.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AdminUpdateDeviceStatus' value with any optional fields omitted.
mkAdminUpdateDeviceStatus
    :: Types.UserPoolId -- ^ 'userPoolId'
    -> Types.Username -- ^ 'username'
    -> Types.DeviceKeyType -- ^ 'deviceKey'
    -> AdminUpdateDeviceStatus
mkAdminUpdateDeviceStatus userPoolId username deviceKey
  = AdminUpdateDeviceStatus'{userPoolId, username, deviceKey,
                             deviceRememberedStatus = Core.Nothing}

-- | The user pool ID.
--
-- /Note:/ Consider using 'userPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
audsUserPoolId :: Lens.Lens' AdminUpdateDeviceStatus Types.UserPoolId
audsUserPoolId = Lens.field @"userPoolId"
{-# INLINEABLE audsUserPoolId #-}
{-# DEPRECATED userPoolId "Use generic-lens or generic-optics with 'userPoolId' instead"  #-}

-- | The user name.
--
-- /Note:/ Consider using 'username' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
audsUsername :: Lens.Lens' AdminUpdateDeviceStatus Types.Username
audsUsername = Lens.field @"username"
{-# INLINEABLE audsUsername #-}
{-# DEPRECATED username "Use generic-lens or generic-optics with 'username' instead"  #-}

-- | The device key.
--
-- /Note:/ Consider using 'deviceKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
audsDeviceKey :: Lens.Lens' AdminUpdateDeviceStatus Types.DeviceKeyType
audsDeviceKey = Lens.field @"deviceKey"
{-# INLINEABLE audsDeviceKey #-}
{-# DEPRECATED deviceKey "Use generic-lens or generic-optics with 'deviceKey' instead"  #-}

-- | The status indicating whether a device has been remembered or not.
--
-- /Note:/ Consider using 'deviceRememberedStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
audsDeviceRememberedStatus :: Lens.Lens' AdminUpdateDeviceStatus (Core.Maybe Types.DeviceRememberedStatusType)
audsDeviceRememberedStatus = Lens.field @"deviceRememberedStatus"
{-# INLINEABLE audsDeviceRememberedStatus #-}
{-# DEPRECATED deviceRememberedStatus "Use generic-lens or generic-optics with 'deviceRememberedStatus' instead"  #-}

instance Core.ToQuery AdminUpdateDeviceStatus where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders AdminUpdateDeviceStatus where
        toHeaders AdminUpdateDeviceStatus{..}
          = Core.pure
              ("X-Amz-Target",
               "AWSCognitoIdentityProviderService.AdminUpdateDeviceStatus")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON AdminUpdateDeviceStatus where
        toJSON AdminUpdateDeviceStatus{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("UserPoolId" Core..= userPoolId),
                  Core.Just ("Username" Core..= username),
                  Core.Just ("DeviceKey" Core..= deviceKey),
                  ("DeviceRememberedStatus" Core..=) Core.<$>
                    deviceRememberedStatus])

instance Core.AWSRequest AdminUpdateDeviceStatus where
        type Rs AdminUpdateDeviceStatus = AdminUpdateDeviceStatusResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 AdminUpdateDeviceStatusResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | The status response from the request to update the device, as an administrator.
--
-- /See:/ 'mkAdminUpdateDeviceStatusResponse' smart constructor.
newtype AdminUpdateDeviceStatusResponse = AdminUpdateDeviceStatusResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'AdminUpdateDeviceStatusResponse' value with any optional fields omitted.
mkAdminUpdateDeviceStatusResponse
    :: Core.Int -- ^ 'responseStatus'
    -> AdminUpdateDeviceStatusResponse
mkAdminUpdateDeviceStatusResponse responseStatus
  = AdminUpdateDeviceStatusResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
audsrrsResponseStatus :: Lens.Lens' AdminUpdateDeviceStatusResponse Core.Int
audsrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE audsrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
