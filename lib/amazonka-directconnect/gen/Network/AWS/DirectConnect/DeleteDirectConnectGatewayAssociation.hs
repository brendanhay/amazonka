{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectConnect.DeleteDirectConnectGatewayAssociation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the association between the specified Direct Connect gateway and virtual private gateway.
--
-- We recommend that you specify the @associationID@ to delete the association. Alternatively, if you own virtual gateway and a Direct Connect gateway association, you can specify the @virtualGatewayId@ and @directConnectGatewayId@ to delete an association.
module Network.AWS.DirectConnect.DeleteDirectConnectGatewayAssociation
    (
    -- * Creating a request
      DeleteDirectConnectGatewayAssociation (..)
    , mkDeleteDirectConnectGatewayAssociation
    -- ** Request lenses
    , ddcgafAssociationId
    , ddcgafDirectConnectGatewayId
    , ddcgafVirtualGatewayId

    -- * Destructuring the response
    , DeleteDirectConnectGatewayAssociationResponse (..)
    , mkDeleteDirectConnectGatewayAssociationResponse
    -- ** Response lenses
    , ddcgarfrsDirectConnectGatewayAssociation
    , ddcgarfrsResponseStatus
    ) where

import qualified Network.AWS.DirectConnect.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteDirectConnectGatewayAssociation' smart constructor.
data DeleteDirectConnectGatewayAssociation = DeleteDirectConnectGatewayAssociation'
  { associationId :: Core.Maybe Types.AssociationId
    -- ^ The ID of the Direct Connect gateway association.
  , directConnectGatewayId :: Core.Maybe Types.DirectConnectGatewayId
    -- ^ The ID of the Direct Connect gateway.
  , virtualGatewayId :: Core.Maybe Types.VirtualGatewayId
    -- ^ The ID of the virtual private gateway.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteDirectConnectGatewayAssociation' value with any optional fields omitted.
mkDeleteDirectConnectGatewayAssociation
    :: DeleteDirectConnectGatewayAssociation
mkDeleteDirectConnectGatewayAssociation
  = DeleteDirectConnectGatewayAssociation'{associationId =
                                             Core.Nothing,
                                           directConnectGatewayId = Core.Nothing,
                                           virtualGatewayId = Core.Nothing}

-- | The ID of the Direct Connect gateway association.
--
-- /Note:/ Consider using 'associationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddcgafAssociationId :: Lens.Lens' DeleteDirectConnectGatewayAssociation (Core.Maybe Types.AssociationId)
ddcgafAssociationId = Lens.field @"associationId"
{-# INLINEABLE ddcgafAssociationId #-}
{-# DEPRECATED associationId "Use generic-lens or generic-optics with 'associationId' instead"  #-}

-- | The ID of the Direct Connect gateway.
--
-- /Note:/ Consider using 'directConnectGatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddcgafDirectConnectGatewayId :: Lens.Lens' DeleteDirectConnectGatewayAssociation (Core.Maybe Types.DirectConnectGatewayId)
ddcgafDirectConnectGatewayId = Lens.field @"directConnectGatewayId"
{-# INLINEABLE ddcgafDirectConnectGatewayId #-}
{-# DEPRECATED directConnectGatewayId "Use generic-lens or generic-optics with 'directConnectGatewayId' instead"  #-}

-- | The ID of the virtual private gateway.
--
-- /Note:/ Consider using 'virtualGatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddcgafVirtualGatewayId :: Lens.Lens' DeleteDirectConnectGatewayAssociation (Core.Maybe Types.VirtualGatewayId)
ddcgafVirtualGatewayId = Lens.field @"virtualGatewayId"
{-# INLINEABLE ddcgafVirtualGatewayId #-}
{-# DEPRECATED virtualGatewayId "Use generic-lens or generic-optics with 'virtualGatewayId' instead"  #-}

instance Core.ToQuery DeleteDirectConnectGatewayAssociation where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteDirectConnectGatewayAssociation where
        toHeaders DeleteDirectConnectGatewayAssociation{..}
          = Core.pure
              ("X-Amz-Target",
               "OvertureService.DeleteDirectConnectGatewayAssociation")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeleteDirectConnectGatewayAssociation where
        toJSON DeleteDirectConnectGatewayAssociation{..}
          = Core.object
              (Core.catMaybes
                 [("associationId" Core..=) Core.<$> associationId,
                  ("directConnectGatewayId" Core..=) Core.<$> directConnectGatewayId,
                  ("virtualGatewayId" Core..=) Core.<$> virtualGatewayId])

instance Core.AWSRequest DeleteDirectConnectGatewayAssociation
         where
        type Rs DeleteDirectConnectGatewayAssociation =
             DeleteDirectConnectGatewayAssociationResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DeleteDirectConnectGatewayAssociationResponse' Core.<$>
                   (x Core..:? "directConnectGatewayAssociation") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteDirectConnectGatewayAssociationResponse' smart constructor.
data DeleteDirectConnectGatewayAssociationResponse = DeleteDirectConnectGatewayAssociationResponse'
  { directConnectGatewayAssociation :: Core.Maybe Types.DirectConnectGatewayAssociation
    -- ^ Information about the deleted association.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteDirectConnectGatewayAssociationResponse' value with any optional fields omitted.
mkDeleteDirectConnectGatewayAssociationResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteDirectConnectGatewayAssociationResponse
mkDeleteDirectConnectGatewayAssociationResponse responseStatus
  = DeleteDirectConnectGatewayAssociationResponse'{directConnectGatewayAssociation
                                                     = Core.Nothing,
                                                   responseStatus}

-- | Information about the deleted association.
--
-- /Note:/ Consider using 'directConnectGatewayAssociation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddcgarfrsDirectConnectGatewayAssociation :: Lens.Lens' DeleteDirectConnectGatewayAssociationResponse (Core.Maybe Types.DirectConnectGatewayAssociation)
ddcgarfrsDirectConnectGatewayAssociation = Lens.field @"directConnectGatewayAssociation"
{-# INLINEABLE ddcgarfrsDirectConnectGatewayAssociation #-}
{-# DEPRECATED directConnectGatewayAssociation "Use generic-lens or generic-optics with 'directConnectGatewayAssociation' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddcgarfrsResponseStatus :: Lens.Lens' DeleteDirectConnectGatewayAssociationResponse Core.Int
ddcgarfrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ddcgarfrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
