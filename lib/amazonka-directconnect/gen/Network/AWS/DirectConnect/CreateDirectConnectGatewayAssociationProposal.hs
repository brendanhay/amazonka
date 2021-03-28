{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectConnect.CreateDirectConnectGatewayAssociationProposal
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a proposal to associate the specified virtual private gateway or transit gateway with the specified Direct Connect gateway.
--
-- You can associate a Direct Connect gateway and virtual private gateway or transit gateway that is owned by any AWS account. 
module Network.AWS.DirectConnect.CreateDirectConnectGatewayAssociationProposal
    (
    -- * Creating a request
      CreateDirectConnectGatewayAssociationProposal (..)
    , mkCreateDirectConnectGatewayAssociationProposal
    -- ** Request lenses
    , cdcgapDirectConnectGatewayId
    , cdcgapDirectConnectGatewayOwnerAccount
    , cdcgapGatewayId
    , cdcgapAddAllowedPrefixesToDirectConnectGateway
    , cdcgapRemoveAllowedPrefixesToDirectConnectGateway

    -- * Destructuring the response
    , CreateDirectConnectGatewayAssociationProposalResponse (..)
    , mkCreateDirectConnectGatewayAssociationProposalResponse
    -- ** Response lenses
    , cdcgaprrsDirectConnectGatewayAssociationProposal
    , cdcgaprrsResponseStatus
    ) where

import qualified Network.AWS.DirectConnect.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateDirectConnectGatewayAssociationProposal' smart constructor.
data CreateDirectConnectGatewayAssociationProposal = CreateDirectConnectGatewayAssociationProposal'
  { directConnectGatewayId :: Types.DirectConnectGatewayId
    -- ^ The ID of the Direct Connect gateway.
  , directConnectGatewayOwnerAccount :: Types.OwnerAccount
    -- ^ The ID of the AWS account that owns the Direct Connect gateway.
  , gatewayId :: Types.GatewayIdToAssociate
    -- ^ The ID of the virtual private gateway or transit gateway.
  , addAllowedPrefixesToDirectConnectGateway :: Core.Maybe [Types.RouteFilterPrefix]
    -- ^ The Amazon VPC prefixes to advertise to the Direct Connect gateway.
  , removeAllowedPrefixesToDirectConnectGateway :: Core.Maybe [Types.RouteFilterPrefix]
    -- ^ The Amazon VPC prefixes to no longer advertise to the Direct Connect gateway.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateDirectConnectGatewayAssociationProposal' value with any optional fields omitted.
mkCreateDirectConnectGatewayAssociationProposal
    :: Types.DirectConnectGatewayId -- ^ 'directConnectGatewayId'
    -> Types.OwnerAccount -- ^ 'directConnectGatewayOwnerAccount'
    -> Types.GatewayIdToAssociate -- ^ 'gatewayId'
    -> CreateDirectConnectGatewayAssociationProposal
mkCreateDirectConnectGatewayAssociationProposal
  directConnectGatewayId directConnectGatewayOwnerAccount gatewayId
  = CreateDirectConnectGatewayAssociationProposal'{directConnectGatewayId,
                                                   directConnectGatewayOwnerAccount, gatewayId,
                                                   addAllowedPrefixesToDirectConnectGateway =
                                                     Core.Nothing,
                                                   removeAllowedPrefixesToDirectConnectGateway =
                                                     Core.Nothing}

-- | The ID of the Direct Connect gateway.
--
-- /Note:/ Consider using 'directConnectGatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdcgapDirectConnectGatewayId :: Lens.Lens' CreateDirectConnectGatewayAssociationProposal Types.DirectConnectGatewayId
cdcgapDirectConnectGatewayId = Lens.field @"directConnectGatewayId"
{-# INLINEABLE cdcgapDirectConnectGatewayId #-}
{-# DEPRECATED directConnectGatewayId "Use generic-lens or generic-optics with 'directConnectGatewayId' instead"  #-}

-- | The ID of the AWS account that owns the Direct Connect gateway.
--
-- /Note:/ Consider using 'directConnectGatewayOwnerAccount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdcgapDirectConnectGatewayOwnerAccount :: Lens.Lens' CreateDirectConnectGatewayAssociationProposal Types.OwnerAccount
cdcgapDirectConnectGatewayOwnerAccount = Lens.field @"directConnectGatewayOwnerAccount"
{-# INLINEABLE cdcgapDirectConnectGatewayOwnerAccount #-}
{-# DEPRECATED directConnectGatewayOwnerAccount "Use generic-lens or generic-optics with 'directConnectGatewayOwnerAccount' instead"  #-}

-- | The ID of the virtual private gateway or transit gateway.
--
-- /Note:/ Consider using 'gatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdcgapGatewayId :: Lens.Lens' CreateDirectConnectGatewayAssociationProposal Types.GatewayIdToAssociate
cdcgapGatewayId = Lens.field @"gatewayId"
{-# INLINEABLE cdcgapGatewayId #-}
{-# DEPRECATED gatewayId "Use generic-lens or generic-optics with 'gatewayId' instead"  #-}

-- | The Amazon VPC prefixes to advertise to the Direct Connect gateway.
--
-- /Note:/ Consider using 'addAllowedPrefixesToDirectConnectGateway' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdcgapAddAllowedPrefixesToDirectConnectGateway :: Lens.Lens' CreateDirectConnectGatewayAssociationProposal (Core.Maybe [Types.RouteFilterPrefix])
cdcgapAddAllowedPrefixesToDirectConnectGateway = Lens.field @"addAllowedPrefixesToDirectConnectGateway"
{-# INLINEABLE cdcgapAddAllowedPrefixesToDirectConnectGateway #-}
{-# DEPRECATED addAllowedPrefixesToDirectConnectGateway "Use generic-lens or generic-optics with 'addAllowedPrefixesToDirectConnectGateway' instead"  #-}

-- | The Amazon VPC prefixes to no longer advertise to the Direct Connect gateway.
--
-- /Note:/ Consider using 'removeAllowedPrefixesToDirectConnectGateway' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdcgapRemoveAllowedPrefixesToDirectConnectGateway :: Lens.Lens' CreateDirectConnectGatewayAssociationProposal (Core.Maybe [Types.RouteFilterPrefix])
cdcgapRemoveAllowedPrefixesToDirectConnectGateway = Lens.field @"removeAllowedPrefixesToDirectConnectGateway"
{-# INLINEABLE cdcgapRemoveAllowedPrefixesToDirectConnectGateway #-}
{-# DEPRECATED removeAllowedPrefixesToDirectConnectGateway "Use generic-lens or generic-optics with 'removeAllowedPrefixesToDirectConnectGateway' instead"  #-}

instance Core.ToQuery CreateDirectConnectGatewayAssociationProposal
         where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders
           CreateDirectConnectGatewayAssociationProposal
         where
        toHeaders CreateDirectConnectGatewayAssociationProposal{..}
          = Core.pure
              ("X-Amz-Target",
               "OvertureService.CreateDirectConnectGatewayAssociationProposal")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON
           CreateDirectConnectGatewayAssociationProposal
         where
        toJSON CreateDirectConnectGatewayAssociationProposal{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just
                    ("directConnectGatewayId" Core..= directConnectGatewayId),
                  Core.Just
                    ("directConnectGatewayOwnerAccount" Core..=
                       directConnectGatewayOwnerAccount),
                  Core.Just ("gatewayId" Core..= gatewayId),
                  ("addAllowedPrefixesToDirectConnectGateway" Core..=) Core.<$>
                    addAllowedPrefixesToDirectConnectGateway,
                  ("removeAllowedPrefixesToDirectConnectGateway" Core..=) Core.<$>
                    removeAllowedPrefixesToDirectConnectGateway])

instance Core.AWSRequest
           CreateDirectConnectGatewayAssociationProposal
         where
        type Rs CreateDirectConnectGatewayAssociationProposal =
             CreateDirectConnectGatewayAssociationProposalResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreateDirectConnectGatewayAssociationProposalResponse' Core.<$>
                   (x Core..:? "directConnectGatewayAssociationProposal") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateDirectConnectGatewayAssociationProposalResponse' smart constructor.
data CreateDirectConnectGatewayAssociationProposalResponse = CreateDirectConnectGatewayAssociationProposalResponse'
  { directConnectGatewayAssociationProposal :: Core.Maybe Types.DirectConnectGatewayAssociationProposal
    -- ^ Information about the Direct Connect gateway proposal.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateDirectConnectGatewayAssociationProposalResponse' value with any optional fields omitted.
mkCreateDirectConnectGatewayAssociationProposalResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateDirectConnectGatewayAssociationProposalResponse
mkCreateDirectConnectGatewayAssociationProposalResponse
  responseStatus
  = CreateDirectConnectGatewayAssociationProposalResponse'{directConnectGatewayAssociationProposal
                                                             = Core.Nothing,
                                                           responseStatus}

-- | Information about the Direct Connect gateway proposal.
--
-- /Note:/ Consider using 'directConnectGatewayAssociationProposal' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdcgaprrsDirectConnectGatewayAssociationProposal :: Lens.Lens' CreateDirectConnectGatewayAssociationProposalResponse (Core.Maybe Types.DirectConnectGatewayAssociationProposal)
cdcgaprrsDirectConnectGatewayAssociationProposal = Lens.field @"directConnectGatewayAssociationProposal"
{-# INLINEABLE cdcgaprrsDirectConnectGatewayAssociationProposal #-}
{-# DEPRECATED directConnectGatewayAssociationProposal "Use generic-lens or generic-optics with 'directConnectGatewayAssociationProposal' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdcgaprrsResponseStatus :: Lens.Lens' CreateDirectConnectGatewayAssociationProposalResponse Core.Int
cdcgaprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE cdcgaprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
