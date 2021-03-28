{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      RegisterDevice (..)
    , mkRegisterDevice
    -- ** Request lenses
    , rdIdentityPoolId
    , rdIdentityId
    , rdPlatform
    , rdToken

    -- * Destructuring the response
    , RegisterDeviceResponse (..)
    , mkRegisterDeviceResponse
    -- ** Response lenses
    , rdrrsDeviceId
    , rdrrsResponseStatus
    ) where

import qualified Network.AWS.CognitoSync.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | A request to RegisterDevice.
--
-- /See:/ 'mkRegisterDevice' smart constructor.
data RegisterDevice = RegisterDevice'
  { identityPoolId :: Types.IdentityPoolId
    -- ^ A name-spaced GUID (for example, us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon Cognito. Here, the ID of the pool that the identity belongs to.
  , identityId :: Types.IdentityId
    -- ^ The unique ID for this identity.
  , platform :: Types.Platform
    -- ^ The SNS platform type (e.g. GCM, SDM, APNS, APNS_SANDBOX).
  , token :: Types.PushToken
    -- ^ The push token.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RegisterDevice' value with any optional fields omitted.
mkRegisterDevice
    :: Types.IdentityPoolId -- ^ 'identityPoolId'
    -> Types.IdentityId -- ^ 'identityId'
    -> Types.Platform -- ^ 'platform'
    -> Types.PushToken -- ^ 'token'
    -> RegisterDevice
mkRegisterDevice identityPoolId identityId platform token
  = RegisterDevice'{identityPoolId, identityId, platform, token}

-- | A name-spaced GUID (for example, us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon Cognito. Here, the ID of the pool that the identity belongs to.
--
-- /Note:/ Consider using 'identityPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdIdentityPoolId :: Lens.Lens' RegisterDevice Types.IdentityPoolId
rdIdentityPoolId = Lens.field @"identityPoolId"
{-# INLINEABLE rdIdentityPoolId #-}
{-# DEPRECATED identityPoolId "Use generic-lens or generic-optics with 'identityPoolId' instead"  #-}

-- | The unique ID for this identity.
--
-- /Note:/ Consider using 'identityId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdIdentityId :: Lens.Lens' RegisterDevice Types.IdentityId
rdIdentityId = Lens.field @"identityId"
{-# INLINEABLE rdIdentityId #-}
{-# DEPRECATED identityId "Use generic-lens or generic-optics with 'identityId' instead"  #-}

-- | The SNS platform type (e.g. GCM, SDM, APNS, APNS_SANDBOX).
--
-- /Note:/ Consider using 'platform' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdPlatform :: Lens.Lens' RegisterDevice Types.Platform
rdPlatform = Lens.field @"platform"
{-# INLINEABLE rdPlatform #-}
{-# DEPRECATED platform "Use generic-lens or generic-optics with 'platform' instead"  #-}

-- | The push token.
--
-- /Note:/ Consider using 'token' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdToken :: Lens.Lens' RegisterDevice Types.PushToken
rdToken = Lens.field @"token"
{-# INLINEABLE rdToken #-}
{-# DEPRECATED token "Use generic-lens or generic-optics with 'token' instead"  #-}

instance Core.ToQuery RegisterDevice where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders RegisterDevice where
        toHeaders RegisterDevice{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON RegisterDevice where
        toJSON RegisterDevice{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("Platform" Core..= platform),
                  Core.Just ("Token" Core..= token)])

instance Core.AWSRequest RegisterDevice where
        type Rs RegisterDevice = RegisterDeviceResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST,
                         Core._rqPath =
                           "/identitypools/" Core.<> Core.toText identityPoolId Core.<>
                             "/identity/"
                             Core.<> Core.toText identityId
                             Core.<> "/device",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 RegisterDeviceResponse' Core.<$>
                   (x Core..:? "DeviceId") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Response to a RegisterDevice request.
--
-- /See:/ 'mkRegisterDeviceResponse' smart constructor.
data RegisterDeviceResponse = RegisterDeviceResponse'
  { deviceId :: Core.Maybe Types.DeviceId
    -- ^ The unique ID generated for this device by Cognito.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RegisterDeviceResponse' value with any optional fields omitted.
mkRegisterDeviceResponse
    :: Core.Int -- ^ 'responseStatus'
    -> RegisterDeviceResponse
mkRegisterDeviceResponse responseStatus
  = RegisterDeviceResponse'{deviceId = Core.Nothing, responseStatus}

-- | The unique ID generated for this device by Cognito.
--
-- /Note:/ Consider using 'deviceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdrrsDeviceId :: Lens.Lens' RegisterDeviceResponse (Core.Maybe Types.DeviceId)
rdrrsDeviceId = Lens.field @"deviceId"
{-# INLINEABLE rdrrsDeviceId #-}
{-# DEPRECATED deviceId "Use generic-lens or generic-optics with 'deviceId' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdrrsResponseStatus :: Lens.Lens' RegisterDeviceResponse Core.Int
rdrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE rdrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
