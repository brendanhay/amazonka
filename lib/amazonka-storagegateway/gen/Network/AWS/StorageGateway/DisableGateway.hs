{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.DisableGateway
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disables a tape gateway when the gateway is no longer functioning. For example, if your gateway VM is damaged, you can disable the gateway so you can recover virtual tapes.
--
-- Use this operation for a tape gateway that is not reachable or not functioning. This operation is only supported in the tape gateway type.
-- /Important:/ After a gateway is disabled, it cannot be enabled.
module Network.AWS.StorageGateway.DisableGateway
    (
    -- * Creating a request
      DisableGateway (..)
    , mkDisableGateway
    -- ** Request lenses
    , dGatewayARN

    -- * Destructuring the response
    , DisableGatewayResponse (..)
    , mkDisableGatewayResponse
    -- ** Response lenses
    , dgrfrsGatewayARN
    , dgrfrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.StorageGateway.Types as Types

-- | DisableGatewayInput
--
-- /See:/ 'mkDisableGateway' smart constructor.
newtype DisableGateway = DisableGateway'
  { gatewayARN :: Types.GatewayARN
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DisableGateway' value with any optional fields omitted.
mkDisableGateway
    :: Types.GatewayARN -- ^ 'gatewayARN'
    -> DisableGateway
mkDisableGateway gatewayARN = DisableGateway'{gatewayARN}

-- | Undocumented field.
--
-- /Note:/ Consider using 'gatewayARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dGatewayARN :: Lens.Lens' DisableGateway Types.GatewayARN
dGatewayARN = Lens.field @"gatewayARN"
{-# INLINEABLE dGatewayARN #-}
{-# DEPRECATED gatewayARN "Use generic-lens or generic-optics with 'gatewayARN' instead"  #-}

instance Core.ToQuery DisableGateway where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DisableGateway where
        toHeaders DisableGateway{..}
          = Core.pure
              ("X-Amz-Target", "StorageGateway_20130630.DisableGateway")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DisableGateway where
        toJSON DisableGateway{..}
          = Core.object
              (Core.catMaybes [Core.Just ("GatewayARN" Core..= gatewayARN)])

instance Core.AWSRequest DisableGateway where
        type Rs DisableGateway = DisableGatewayResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DisableGatewayResponse' Core.<$>
                   (x Core..:? "GatewayARN") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | DisableGatewayOutput
--
-- /See:/ 'mkDisableGatewayResponse' smart constructor.
data DisableGatewayResponse = DisableGatewayResponse'
  { gatewayARN :: Core.Maybe Types.GatewayARN
    -- ^ The unique Amazon Resource Name (ARN) of the disabled gateway.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DisableGatewayResponse' value with any optional fields omitted.
mkDisableGatewayResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DisableGatewayResponse
mkDisableGatewayResponse responseStatus
  = DisableGatewayResponse'{gatewayARN = Core.Nothing,
                            responseStatus}

-- | The unique Amazon Resource Name (ARN) of the disabled gateway.
--
-- /Note:/ Consider using 'gatewayARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgrfrsGatewayARN :: Lens.Lens' DisableGatewayResponse (Core.Maybe Types.GatewayARN)
dgrfrsGatewayARN = Lens.field @"gatewayARN"
{-# INLINEABLE dgrfrsGatewayARN #-}
{-# DEPRECATED gatewayARN "Use generic-lens or generic-optics with 'gatewayARN' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgrfrsResponseStatus :: Lens.Lens' DisableGatewayResponse Core.Int
dgrfrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dgrfrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
