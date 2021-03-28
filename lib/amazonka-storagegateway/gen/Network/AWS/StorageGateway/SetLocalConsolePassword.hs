{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.SetLocalConsolePassword
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the password for your VM local console. When you log in to the local console for the first time, you log in to the VM with the default credentials. We recommend that you set a new password. You don't need to know the default password to set a new password.
module Network.AWS.StorageGateway.SetLocalConsolePassword
    (
    -- * Creating a request
      SetLocalConsolePassword (..)
    , mkSetLocalConsolePassword
    -- ** Request lenses
    , slcpGatewayARN
    , slcpLocalConsolePassword

    -- * Destructuring the response
    , SetLocalConsolePasswordResponse (..)
    , mkSetLocalConsolePasswordResponse
    -- ** Response lenses
    , slcprrsGatewayARN
    , slcprrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.StorageGateway.Types as Types

-- | SetLocalConsolePasswordInput
--
-- /See:/ 'mkSetLocalConsolePassword' smart constructor.
data SetLocalConsolePassword = SetLocalConsolePassword'
  { gatewayARN :: Types.GatewayARN
  , localConsolePassword :: Types.LocalConsolePassword
    -- ^ The password you want to set for your VM local console.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SetLocalConsolePassword' value with any optional fields omitted.
mkSetLocalConsolePassword
    :: Types.GatewayARN -- ^ 'gatewayARN'
    -> Types.LocalConsolePassword -- ^ 'localConsolePassword'
    -> SetLocalConsolePassword
mkSetLocalConsolePassword gatewayARN localConsolePassword
  = SetLocalConsolePassword'{gatewayARN, localConsolePassword}

-- | Undocumented field.
--
-- /Note:/ Consider using 'gatewayARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slcpGatewayARN :: Lens.Lens' SetLocalConsolePassword Types.GatewayARN
slcpGatewayARN = Lens.field @"gatewayARN"
{-# INLINEABLE slcpGatewayARN #-}
{-# DEPRECATED gatewayARN "Use generic-lens or generic-optics with 'gatewayARN' instead"  #-}

-- | The password you want to set for your VM local console.
--
-- /Note:/ Consider using 'localConsolePassword' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slcpLocalConsolePassword :: Lens.Lens' SetLocalConsolePassword Types.LocalConsolePassword
slcpLocalConsolePassword = Lens.field @"localConsolePassword"
{-# INLINEABLE slcpLocalConsolePassword #-}
{-# DEPRECATED localConsolePassword "Use generic-lens or generic-optics with 'localConsolePassword' instead"  #-}

instance Core.ToQuery SetLocalConsolePassword where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders SetLocalConsolePassword where
        toHeaders SetLocalConsolePassword{..}
          = Core.pure
              ("X-Amz-Target", "StorageGateway_20130630.SetLocalConsolePassword")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON SetLocalConsolePassword where
        toJSON SetLocalConsolePassword{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("GatewayARN" Core..= gatewayARN),
                  Core.Just ("LocalConsolePassword" Core..= localConsolePassword)])

instance Core.AWSRequest SetLocalConsolePassword where
        type Rs SetLocalConsolePassword = SetLocalConsolePasswordResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 SetLocalConsolePasswordResponse' Core.<$>
                   (x Core..:? "GatewayARN") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkSetLocalConsolePasswordResponse' smart constructor.
data SetLocalConsolePasswordResponse = SetLocalConsolePasswordResponse'
  { gatewayARN :: Core.Maybe Types.GatewayARN
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SetLocalConsolePasswordResponse' value with any optional fields omitted.
mkSetLocalConsolePasswordResponse
    :: Core.Int -- ^ 'responseStatus'
    -> SetLocalConsolePasswordResponse
mkSetLocalConsolePasswordResponse responseStatus
  = SetLocalConsolePasswordResponse'{gatewayARN = Core.Nothing,
                                     responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'gatewayARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slcprrsGatewayARN :: Lens.Lens' SetLocalConsolePasswordResponse (Core.Maybe Types.GatewayARN)
slcprrsGatewayARN = Lens.field @"gatewayARN"
{-# INLINEABLE slcprrsGatewayARN #-}
{-# DEPRECATED gatewayARN "Use generic-lens or generic-optics with 'gatewayARN' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slcprrsResponseStatus :: Lens.Lens' SetLocalConsolePasswordResponse Core.Int
slcprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE slcprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
