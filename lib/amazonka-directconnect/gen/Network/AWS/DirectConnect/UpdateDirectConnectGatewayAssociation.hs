{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectConnect.UpdateDirectConnectGatewayAssociation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the specified attributes of the Direct Connect gateway association.
--
-- Add or remove prefixes from the association.
module Network.AWS.DirectConnect.UpdateDirectConnectGatewayAssociation
    (
    -- * Creating a request
      UpdateDirectConnectGatewayAssociation (..)
    , mkUpdateDirectConnectGatewayAssociation
    -- ** Request lenses
    , udcgaAddAllowedPrefixesToDirectConnectGateway
    , udcgaAssociationId
    , udcgaRemoveAllowedPrefixesToDirectConnectGateway

    -- * Destructuring the response
    , UpdateDirectConnectGatewayAssociationResponse (..)
    , mkUpdateDirectConnectGatewayAssociationResponse
    -- ** Response lenses
    , udcgarrsDirectConnectGatewayAssociation
    , udcgarrsResponseStatus
    ) where

import qualified Network.AWS.DirectConnect.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateDirectConnectGatewayAssociation' smart constructor.
data UpdateDirectConnectGatewayAssociation = UpdateDirectConnectGatewayAssociation'
  { addAllowedPrefixesToDirectConnectGateway :: Core.Maybe [Types.RouteFilterPrefix]
    -- ^ The Amazon VPC prefixes to advertise to the Direct Connect gateway.
  , associationId :: Core.Maybe Types.AssociationId
    -- ^ The ID of the Direct Connect gateway association.
  , removeAllowedPrefixesToDirectConnectGateway :: Core.Maybe [Types.RouteFilterPrefix]
    -- ^ The Amazon VPC prefixes to no longer advertise to the Direct Connect gateway.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateDirectConnectGatewayAssociation' value with any optional fields omitted.
mkUpdateDirectConnectGatewayAssociation
    :: UpdateDirectConnectGatewayAssociation
mkUpdateDirectConnectGatewayAssociation
  = UpdateDirectConnectGatewayAssociation'{addAllowedPrefixesToDirectConnectGateway
                                             = Core.Nothing,
                                           associationId = Core.Nothing,
                                           removeAllowedPrefixesToDirectConnectGateway =
                                             Core.Nothing}

-- | The Amazon VPC prefixes to advertise to the Direct Connect gateway.
--
-- /Note:/ Consider using 'addAllowedPrefixesToDirectConnectGateway' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udcgaAddAllowedPrefixesToDirectConnectGateway :: Lens.Lens' UpdateDirectConnectGatewayAssociation (Core.Maybe [Types.RouteFilterPrefix])
udcgaAddAllowedPrefixesToDirectConnectGateway = Lens.field @"addAllowedPrefixesToDirectConnectGateway"
{-# INLINEABLE udcgaAddAllowedPrefixesToDirectConnectGateway #-}
{-# DEPRECATED addAllowedPrefixesToDirectConnectGateway "Use generic-lens or generic-optics with 'addAllowedPrefixesToDirectConnectGateway' instead"  #-}

-- | The ID of the Direct Connect gateway association.
--
-- /Note:/ Consider using 'associationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udcgaAssociationId :: Lens.Lens' UpdateDirectConnectGatewayAssociation (Core.Maybe Types.AssociationId)
udcgaAssociationId = Lens.field @"associationId"
{-# INLINEABLE udcgaAssociationId #-}
{-# DEPRECATED associationId "Use generic-lens or generic-optics with 'associationId' instead"  #-}

-- | The Amazon VPC prefixes to no longer advertise to the Direct Connect gateway.
--
-- /Note:/ Consider using 'removeAllowedPrefixesToDirectConnectGateway' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udcgaRemoveAllowedPrefixesToDirectConnectGateway :: Lens.Lens' UpdateDirectConnectGatewayAssociation (Core.Maybe [Types.RouteFilterPrefix])
udcgaRemoveAllowedPrefixesToDirectConnectGateway = Lens.field @"removeAllowedPrefixesToDirectConnectGateway"
{-# INLINEABLE udcgaRemoveAllowedPrefixesToDirectConnectGateway #-}
{-# DEPRECATED removeAllowedPrefixesToDirectConnectGateway "Use generic-lens or generic-optics with 'removeAllowedPrefixesToDirectConnectGateway' instead"  #-}

instance Core.ToQuery UpdateDirectConnectGatewayAssociation where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateDirectConnectGatewayAssociation where
        toHeaders UpdateDirectConnectGatewayAssociation{..}
          = Core.pure
              ("X-Amz-Target",
               "OvertureService.UpdateDirectConnectGatewayAssociation")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON UpdateDirectConnectGatewayAssociation where
        toJSON UpdateDirectConnectGatewayAssociation{..}
          = Core.object
              (Core.catMaybes
                 [("addAllowedPrefixesToDirectConnectGateway" Core..=) Core.<$>
                    addAllowedPrefixesToDirectConnectGateway,
                  ("associationId" Core..=) Core.<$> associationId,
                  ("removeAllowedPrefixesToDirectConnectGateway" Core..=) Core.<$>
                    removeAllowedPrefixesToDirectConnectGateway])

instance Core.AWSRequest UpdateDirectConnectGatewayAssociation
         where
        type Rs UpdateDirectConnectGatewayAssociation =
             UpdateDirectConnectGatewayAssociationResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 UpdateDirectConnectGatewayAssociationResponse' Core.<$>
                   (x Core..:? "directConnectGatewayAssociation") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkUpdateDirectConnectGatewayAssociationResponse' smart constructor.
data UpdateDirectConnectGatewayAssociationResponse = UpdateDirectConnectGatewayAssociationResponse'
  { directConnectGatewayAssociation :: Core.Maybe Types.DirectConnectGatewayAssociation
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateDirectConnectGatewayAssociationResponse' value with any optional fields omitted.
mkUpdateDirectConnectGatewayAssociationResponse
    :: Core.Int -- ^ 'responseStatus'
    -> UpdateDirectConnectGatewayAssociationResponse
mkUpdateDirectConnectGatewayAssociationResponse responseStatus
  = UpdateDirectConnectGatewayAssociationResponse'{directConnectGatewayAssociation
                                                     = Core.Nothing,
                                                   responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'directConnectGatewayAssociation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udcgarrsDirectConnectGatewayAssociation :: Lens.Lens' UpdateDirectConnectGatewayAssociationResponse (Core.Maybe Types.DirectConnectGatewayAssociation)
udcgarrsDirectConnectGatewayAssociation = Lens.field @"directConnectGatewayAssociation"
{-# INLINEABLE udcgarrsDirectConnectGatewayAssociation #-}
{-# DEPRECATED directConnectGatewayAssociation "Use generic-lens or generic-optics with 'directConnectGatewayAssociation' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udcgarrsResponseStatus :: Lens.Lens' UpdateDirectConnectGatewayAssociationResponse Core.Int
udcgarrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE udcgarrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
