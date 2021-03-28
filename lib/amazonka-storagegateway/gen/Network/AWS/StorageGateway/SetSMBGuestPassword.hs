{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.SetSMBGuestPassword
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the password for the guest user @smbguest@ . The @smbguest@ user is the user when the authentication method for the file share is set to @GuestAccess@ .
module Network.AWS.StorageGateway.SetSMBGuestPassword
    (
    -- * Creating a request
      SetSMBGuestPassword (..)
    , mkSetSMBGuestPassword
    -- ** Request lenses
    , ssmbgpGatewayARN
    , ssmbgpPassword

    -- * Destructuring the response
    , SetSMBGuestPasswordResponse (..)
    , mkSetSMBGuestPasswordResponse
    -- ** Response lenses
    , ssmbgprrsGatewayARN
    , ssmbgprrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.StorageGateway.Types as Types

-- | SetSMBGuestPasswordInput
--
-- /See:/ 'mkSetSMBGuestPassword' smart constructor.
data SetSMBGuestPassword = SetSMBGuestPassword'
  { gatewayARN :: Types.GatewayARN
    -- ^ The Amazon Resource Name (ARN) of the file gateway the SMB file share is associated with.
  , password :: Types.Password
    -- ^ The password that you want to set for your SMB server.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SetSMBGuestPassword' value with any optional fields omitted.
mkSetSMBGuestPassword
    :: Types.GatewayARN -- ^ 'gatewayARN'
    -> Types.Password -- ^ 'password'
    -> SetSMBGuestPassword
mkSetSMBGuestPassword gatewayARN password
  = SetSMBGuestPassword'{gatewayARN, password}

-- | The Amazon Resource Name (ARN) of the file gateway the SMB file share is associated with.
--
-- /Note:/ Consider using 'gatewayARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssmbgpGatewayARN :: Lens.Lens' SetSMBGuestPassword Types.GatewayARN
ssmbgpGatewayARN = Lens.field @"gatewayARN"
{-# INLINEABLE ssmbgpGatewayARN #-}
{-# DEPRECATED gatewayARN "Use generic-lens or generic-optics with 'gatewayARN' instead"  #-}

-- | The password that you want to set for your SMB server.
--
-- /Note:/ Consider using 'password' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssmbgpPassword :: Lens.Lens' SetSMBGuestPassword Types.Password
ssmbgpPassword = Lens.field @"password"
{-# INLINEABLE ssmbgpPassword #-}
{-# DEPRECATED password "Use generic-lens or generic-optics with 'password' instead"  #-}

instance Core.ToQuery SetSMBGuestPassword where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders SetSMBGuestPassword where
        toHeaders SetSMBGuestPassword{..}
          = Core.pure
              ("X-Amz-Target", "StorageGateway_20130630.SetSMBGuestPassword")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON SetSMBGuestPassword where
        toJSON SetSMBGuestPassword{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("GatewayARN" Core..= gatewayARN),
                  Core.Just ("Password" Core..= password)])

instance Core.AWSRequest SetSMBGuestPassword where
        type Rs SetSMBGuestPassword = SetSMBGuestPasswordResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 SetSMBGuestPasswordResponse' Core.<$>
                   (x Core..:? "GatewayARN") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkSetSMBGuestPasswordResponse' smart constructor.
data SetSMBGuestPasswordResponse = SetSMBGuestPasswordResponse'
  { gatewayARN :: Core.Maybe Types.GatewayARN
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SetSMBGuestPasswordResponse' value with any optional fields omitted.
mkSetSMBGuestPasswordResponse
    :: Core.Int -- ^ 'responseStatus'
    -> SetSMBGuestPasswordResponse
mkSetSMBGuestPasswordResponse responseStatus
  = SetSMBGuestPasswordResponse'{gatewayARN = Core.Nothing,
                                 responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'gatewayARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssmbgprrsGatewayARN :: Lens.Lens' SetSMBGuestPasswordResponse (Core.Maybe Types.GatewayARN)
ssmbgprrsGatewayARN = Lens.field @"gatewayARN"
{-# INLINEABLE ssmbgprrsGatewayARN #-}
{-# DEPRECATED gatewayARN "Use generic-lens or generic-optics with 'gatewayARN' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssmbgprrsResponseStatus :: Lens.Lens' SetSMBGuestPasswordResponse Core.Int
ssmbgprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ssmbgprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
