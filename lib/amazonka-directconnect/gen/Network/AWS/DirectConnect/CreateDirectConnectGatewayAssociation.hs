{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectConnect.CreateDirectConnectGatewayAssociation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an association between a Direct Connect gateway and a virtual private gateway. The virtual private gateway must be attached to a VPC and must not be associated with another Direct Connect gateway.
module Network.AWS.DirectConnect.CreateDirectConnectGatewayAssociation
    (
    -- * Creating a request
      CreateDirectConnectGatewayAssociation (..)
    , mkCreateDirectConnectGatewayAssociation
    -- ** Request lenses
    , cdcgaDirectConnectGatewayId
    , cdcgaAddAllowedPrefixesToDirectConnectGateway
    , cdcgaGatewayId
    , cdcgaVirtualGatewayId

    -- * Destructuring the response
    , CreateDirectConnectGatewayAssociationResponse (..)
    , mkCreateDirectConnectGatewayAssociationResponse
    -- ** Response lenses
    , cdcgarrsDirectConnectGatewayAssociation
    , cdcgarrsResponseStatus
    ) where

import qualified Network.AWS.DirectConnect.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateDirectConnectGatewayAssociation' smart constructor.
data CreateDirectConnectGatewayAssociation = CreateDirectConnectGatewayAssociation'
  { directConnectGatewayId :: Types.DirectConnectGatewayId
    -- ^ The ID of the Direct Connect gateway.
  , addAllowedPrefixesToDirectConnectGateway :: Core.Maybe [Types.RouteFilterPrefix]
    -- ^ The Amazon VPC prefixes to advertise to the Direct Connect gateway
--
-- This parameter is required when you create an association to a transit gateway.
-- For information about how to set the prefixes, see <https://docs.aws.amazon.com/directconnect/latest/UserGuide/multi-account-associate-vgw.html#allowed-prefixes Allowed Prefixes> in the /AWS Direct Connect User Guide/ .
  , gatewayId :: Core.Maybe Types.GatewayId
    -- ^ The ID of the virtual private gateway or transit gateway.
  , virtualGatewayId :: Core.Maybe Types.VirtualGatewayId
    -- ^ The ID of the virtual private gateway.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateDirectConnectGatewayAssociation' value with any optional fields omitted.
mkCreateDirectConnectGatewayAssociation
    :: Types.DirectConnectGatewayId -- ^ 'directConnectGatewayId'
    -> CreateDirectConnectGatewayAssociation
mkCreateDirectConnectGatewayAssociation directConnectGatewayId
  = CreateDirectConnectGatewayAssociation'{directConnectGatewayId,
                                           addAllowedPrefixesToDirectConnectGateway = Core.Nothing,
                                           gatewayId = Core.Nothing,
                                           virtualGatewayId = Core.Nothing}

-- | The ID of the Direct Connect gateway.
--
-- /Note:/ Consider using 'directConnectGatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdcgaDirectConnectGatewayId :: Lens.Lens' CreateDirectConnectGatewayAssociation Types.DirectConnectGatewayId
cdcgaDirectConnectGatewayId = Lens.field @"directConnectGatewayId"
{-# INLINEABLE cdcgaDirectConnectGatewayId #-}
{-# DEPRECATED directConnectGatewayId "Use generic-lens or generic-optics with 'directConnectGatewayId' instead"  #-}

-- | The Amazon VPC prefixes to advertise to the Direct Connect gateway
--
-- This parameter is required when you create an association to a transit gateway.
-- For information about how to set the prefixes, see <https://docs.aws.amazon.com/directconnect/latest/UserGuide/multi-account-associate-vgw.html#allowed-prefixes Allowed Prefixes> in the /AWS Direct Connect User Guide/ .
--
-- /Note:/ Consider using 'addAllowedPrefixesToDirectConnectGateway' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdcgaAddAllowedPrefixesToDirectConnectGateway :: Lens.Lens' CreateDirectConnectGatewayAssociation (Core.Maybe [Types.RouteFilterPrefix])
cdcgaAddAllowedPrefixesToDirectConnectGateway = Lens.field @"addAllowedPrefixesToDirectConnectGateway"
{-# INLINEABLE cdcgaAddAllowedPrefixesToDirectConnectGateway #-}
{-# DEPRECATED addAllowedPrefixesToDirectConnectGateway "Use generic-lens or generic-optics with 'addAllowedPrefixesToDirectConnectGateway' instead"  #-}

-- | The ID of the virtual private gateway or transit gateway.
--
-- /Note:/ Consider using 'gatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdcgaGatewayId :: Lens.Lens' CreateDirectConnectGatewayAssociation (Core.Maybe Types.GatewayId)
cdcgaGatewayId = Lens.field @"gatewayId"
{-# INLINEABLE cdcgaGatewayId #-}
{-# DEPRECATED gatewayId "Use generic-lens or generic-optics with 'gatewayId' instead"  #-}

-- | The ID of the virtual private gateway.
--
-- /Note:/ Consider using 'virtualGatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdcgaVirtualGatewayId :: Lens.Lens' CreateDirectConnectGatewayAssociation (Core.Maybe Types.VirtualGatewayId)
cdcgaVirtualGatewayId = Lens.field @"virtualGatewayId"
{-# INLINEABLE cdcgaVirtualGatewayId #-}
{-# DEPRECATED virtualGatewayId "Use generic-lens or generic-optics with 'virtualGatewayId' instead"  #-}

instance Core.ToQuery CreateDirectConnectGatewayAssociation where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateDirectConnectGatewayAssociation where
        toHeaders CreateDirectConnectGatewayAssociation{..}
          = Core.pure
              ("X-Amz-Target",
               "OvertureService.CreateDirectConnectGatewayAssociation")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreateDirectConnectGatewayAssociation where
        toJSON CreateDirectConnectGatewayAssociation{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just
                    ("directConnectGatewayId" Core..= directConnectGatewayId),
                  ("addAllowedPrefixesToDirectConnectGateway" Core..=) Core.<$>
                    addAllowedPrefixesToDirectConnectGateway,
                  ("gatewayId" Core..=) Core.<$> gatewayId,
                  ("virtualGatewayId" Core..=) Core.<$> virtualGatewayId])

instance Core.AWSRequest CreateDirectConnectGatewayAssociation
         where
        type Rs CreateDirectConnectGatewayAssociation =
             CreateDirectConnectGatewayAssociationResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreateDirectConnectGatewayAssociationResponse' Core.<$>
                   (x Core..:? "directConnectGatewayAssociation") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateDirectConnectGatewayAssociationResponse' smart constructor.
data CreateDirectConnectGatewayAssociationResponse = CreateDirectConnectGatewayAssociationResponse'
  { directConnectGatewayAssociation :: Core.Maybe Types.DirectConnectGatewayAssociation
    -- ^ The association to be created.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateDirectConnectGatewayAssociationResponse' value with any optional fields omitted.
mkCreateDirectConnectGatewayAssociationResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateDirectConnectGatewayAssociationResponse
mkCreateDirectConnectGatewayAssociationResponse responseStatus
  = CreateDirectConnectGatewayAssociationResponse'{directConnectGatewayAssociation
                                                     = Core.Nothing,
                                                   responseStatus}

-- | The association to be created.
--
-- /Note:/ Consider using 'directConnectGatewayAssociation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdcgarrsDirectConnectGatewayAssociation :: Lens.Lens' CreateDirectConnectGatewayAssociationResponse (Core.Maybe Types.DirectConnectGatewayAssociation)
cdcgarrsDirectConnectGatewayAssociation = Lens.field @"directConnectGatewayAssociation"
{-# INLINEABLE cdcgarrsDirectConnectGatewayAssociation #-}
{-# DEPRECATED directConnectGatewayAssociation "Use generic-lens or generic-optics with 'directConnectGatewayAssociation' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdcgarrsResponseStatus :: Lens.Lens' CreateDirectConnectGatewayAssociationResponse Core.Int
cdcgarrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE cdcgarrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
