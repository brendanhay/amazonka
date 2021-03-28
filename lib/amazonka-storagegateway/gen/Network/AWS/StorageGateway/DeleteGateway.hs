{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.DeleteGateway
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a gateway. To specify which gateway to delete, use the Amazon Resource Name (ARN) of the gateway in your request. The operation deletes the gateway; however, it does not delete the gateway virtual machine (VM) from your host computer.
--
-- After you delete a gateway, you cannot reactivate it. Completed snapshots of the gateway volumes are not deleted upon deleting the gateway, however, pending snapshots will not complete. After you delete a gateway, your next step is to remove it from your environment.
-- /Important:/ You no longer pay software charges after the gateway is deleted; however, your existing Amazon EBS snapshots persist and you will continue to be billed for these snapshots. You can choose to remove all remaining Amazon EBS snapshots by canceling your Amazon EC2 subscription.  If you prefer not to cancel your Amazon EC2 subscription, you can delete your snapshots using the Amazon EC2 console. For more information, see the <http://aws.amazon.com/storagegateway AWS Storage Gateway detail page> .
module Network.AWS.StorageGateway.DeleteGateway
    (
    -- * Creating a request
      DeleteGateway (..)
    , mkDeleteGateway
    -- ** Request lenses
    , dgGatewayARN

    -- * Destructuring the response
    , DeleteGatewayResponse (..)
    , mkDeleteGatewayResponse
    -- ** Response lenses
    , dgrrsGatewayARN
    , dgrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.StorageGateway.Types as Types

-- | A JSON object containing the ID of the gateway to delete.
--
-- /See:/ 'mkDeleteGateway' smart constructor.
newtype DeleteGateway = DeleteGateway'
  { gatewayARN :: Types.GatewayARN
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteGateway' value with any optional fields omitted.
mkDeleteGateway
    :: Types.GatewayARN -- ^ 'gatewayARN'
    -> DeleteGateway
mkDeleteGateway gatewayARN = DeleteGateway'{gatewayARN}

-- | Undocumented field.
--
-- /Note:/ Consider using 'gatewayARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgGatewayARN :: Lens.Lens' DeleteGateway Types.GatewayARN
dgGatewayARN = Lens.field @"gatewayARN"
{-# INLINEABLE dgGatewayARN #-}
{-# DEPRECATED gatewayARN "Use generic-lens or generic-optics with 'gatewayARN' instead"  #-}

instance Core.ToQuery DeleteGateway where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteGateway where
        toHeaders DeleteGateway{..}
          = Core.pure
              ("X-Amz-Target", "StorageGateway_20130630.DeleteGateway")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeleteGateway where
        toJSON DeleteGateway{..}
          = Core.object
              (Core.catMaybes [Core.Just ("GatewayARN" Core..= gatewayARN)])

instance Core.AWSRequest DeleteGateway where
        type Rs DeleteGateway = DeleteGatewayResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DeleteGatewayResponse' Core.<$>
                   (x Core..:? "GatewayARN") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | A JSON object containing the ID of the deleted gateway.
--
-- /See:/ 'mkDeleteGatewayResponse' smart constructor.
data DeleteGatewayResponse = DeleteGatewayResponse'
  { gatewayARN :: Core.Maybe Types.GatewayARN
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteGatewayResponse' value with any optional fields omitted.
mkDeleteGatewayResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteGatewayResponse
mkDeleteGatewayResponse responseStatus
  = DeleteGatewayResponse'{gatewayARN = Core.Nothing, responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'gatewayARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgrrsGatewayARN :: Lens.Lens' DeleteGatewayResponse (Core.Maybe Types.GatewayARN)
dgrrsGatewayARN = Lens.field @"gatewayARN"
{-# INLINEABLE dgrrsGatewayARN #-}
{-# DEPRECATED gatewayARN "Use generic-lens or generic-optics with 'gatewayARN' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgrrsResponseStatus :: Lens.Lens' DeleteGatewayResponse Core.Int
dgrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dgrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
