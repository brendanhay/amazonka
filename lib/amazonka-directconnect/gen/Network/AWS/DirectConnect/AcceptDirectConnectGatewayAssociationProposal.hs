{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectConnect.AcceptDirectConnectGatewayAssociationProposal
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Accepts a proposal request to attach a virtual private gateway or transit gateway to a Direct Connect gateway.
module Network.AWS.DirectConnect.AcceptDirectConnectGatewayAssociationProposal
    (
    -- * Creating a request
      AcceptDirectConnectGatewayAssociationProposal (..)
    , mkAcceptDirectConnectGatewayAssociationProposal
    -- ** Request lenses
    , adcgapDirectConnectGatewayId
    , adcgapProposalId
    , adcgapAssociatedGatewayOwnerAccount
    , adcgapOverrideAllowedPrefixesToDirectConnectGateway

    -- * Destructuring the response
    , AcceptDirectConnectGatewayAssociationProposalResponse (..)
    , mkAcceptDirectConnectGatewayAssociationProposalResponse
    -- ** Response lenses
    , adcgaprrsDirectConnectGatewayAssociation
    , adcgaprrsResponseStatus
    ) where

import qualified Network.AWS.DirectConnect.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkAcceptDirectConnectGatewayAssociationProposal' smart constructor.
data AcceptDirectConnectGatewayAssociationProposal = AcceptDirectConnectGatewayAssociationProposal'
  { directConnectGatewayId :: Types.DirectConnectGatewayId
    -- ^ The ID of the Direct Connect gateway.
  , proposalId :: Types.ProposalId
    -- ^ The ID of the request proposal.
  , associatedGatewayOwnerAccount :: Types.AssociatedGatewayOwnerAccount
    -- ^ The ID of the AWS account that owns the virtual private gateway or transit gateway.
  , overrideAllowedPrefixesToDirectConnectGateway :: Core.Maybe [Types.RouteFilterPrefix]
    -- ^ Overrides the Amazon VPC prefixes advertised to the Direct Connect gateway.
--
-- For information about how to set the prefixes, see <https://docs.aws.amazon.com/directconnect/latest/UserGuide/multi-account-associate-vgw.html#allowed-prefixes Allowed Prefixes> in the /AWS Direct Connect User Guide/ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AcceptDirectConnectGatewayAssociationProposal' value with any optional fields omitted.
mkAcceptDirectConnectGatewayAssociationProposal
    :: Types.DirectConnectGatewayId -- ^ 'directConnectGatewayId'
    -> Types.ProposalId -- ^ 'proposalId'
    -> Types.AssociatedGatewayOwnerAccount -- ^ 'associatedGatewayOwnerAccount'
    -> AcceptDirectConnectGatewayAssociationProposal
mkAcceptDirectConnectGatewayAssociationProposal
  directConnectGatewayId proposalId associatedGatewayOwnerAccount
  = AcceptDirectConnectGatewayAssociationProposal'{directConnectGatewayId,
                                                   proposalId, associatedGatewayOwnerAccount,
                                                   overrideAllowedPrefixesToDirectConnectGateway =
                                                     Core.Nothing}

-- | The ID of the Direct Connect gateway.
--
-- /Note:/ Consider using 'directConnectGatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adcgapDirectConnectGatewayId :: Lens.Lens' AcceptDirectConnectGatewayAssociationProposal Types.DirectConnectGatewayId
adcgapDirectConnectGatewayId = Lens.field @"directConnectGatewayId"
{-# INLINEABLE adcgapDirectConnectGatewayId #-}
{-# DEPRECATED directConnectGatewayId "Use generic-lens or generic-optics with 'directConnectGatewayId' instead"  #-}

-- | The ID of the request proposal.
--
-- /Note:/ Consider using 'proposalId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adcgapProposalId :: Lens.Lens' AcceptDirectConnectGatewayAssociationProposal Types.ProposalId
adcgapProposalId = Lens.field @"proposalId"
{-# INLINEABLE adcgapProposalId #-}
{-# DEPRECATED proposalId "Use generic-lens or generic-optics with 'proposalId' instead"  #-}

-- | The ID of the AWS account that owns the virtual private gateway or transit gateway.
--
-- /Note:/ Consider using 'associatedGatewayOwnerAccount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adcgapAssociatedGatewayOwnerAccount :: Lens.Lens' AcceptDirectConnectGatewayAssociationProposal Types.AssociatedGatewayOwnerAccount
adcgapAssociatedGatewayOwnerAccount = Lens.field @"associatedGatewayOwnerAccount"
{-# INLINEABLE adcgapAssociatedGatewayOwnerAccount #-}
{-# DEPRECATED associatedGatewayOwnerAccount "Use generic-lens or generic-optics with 'associatedGatewayOwnerAccount' instead"  #-}

-- | Overrides the Amazon VPC prefixes advertised to the Direct Connect gateway.
--
-- For information about how to set the prefixes, see <https://docs.aws.amazon.com/directconnect/latest/UserGuide/multi-account-associate-vgw.html#allowed-prefixes Allowed Prefixes> in the /AWS Direct Connect User Guide/ .
--
-- /Note:/ Consider using 'overrideAllowedPrefixesToDirectConnectGateway' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adcgapOverrideAllowedPrefixesToDirectConnectGateway :: Lens.Lens' AcceptDirectConnectGatewayAssociationProposal (Core.Maybe [Types.RouteFilterPrefix])
adcgapOverrideAllowedPrefixesToDirectConnectGateway = Lens.field @"overrideAllowedPrefixesToDirectConnectGateway"
{-# INLINEABLE adcgapOverrideAllowedPrefixesToDirectConnectGateway #-}
{-# DEPRECATED overrideAllowedPrefixesToDirectConnectGateway "Use generic-lens or generic-optics with 'overrideAllowedPrefixesToDirectConnectGateway' instead"  #-}

instance Core.ToQuery AcceptDirectConnectGatewayAssociationProposal
         where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders
           AcceptDirectConnectGatewayAssociationProposal
         where
        toHeaders AcceptDirectConnectGatewayAssociationProposal{..}
          = Core.pure
              ("X-Amz-Target",
               "OvertureService.AcceptDirectConnectGatewayAssociationProposal")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON
           AcceptDirectConnectGatewayAssociationProposal
         where
        toJSON AcceptDirectConnectGatewayAssociationProposal{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just
                    ("directConnectGatewayId" Core..= directConnectGatewayId),
                  Core.Just ("proposalId" Core..= proposalId),
                  Core.Just
                    ("associatedGatewayOwnerAccount" Core..=
                       associatedGatewayOwnerAccount),
                  ("overrideAllowedPrefixesToDirectConnectGateway" Core..=) Core.<$>
                    overrideAllowedPrefixesToDirectConnectGateway])

instance Core.AWSRequest
           AcceptDirectConnectGatewayAssociationProposal
         where
        type Rs AcceptDirectConnectGatewayAssociationProposal =
             AcceptDirectConnectGatewayAssociationProposalResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 AcceptDirectConnectGatewayAssociationProposalResponse' Core.<$>
                   (x Core..:? "directConnectGatewayAssociation") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkAcceptDirectConnectGatewayAssociationProposalResponse' smart constructor.
data AcceptDirectConnectGatewayAssociationProposalResponse = AcceptDirectConnectGatewayAssociationProposalResponse'
  { directConnectGatewayAssociation :: Core.Maybe Types.DirectConnectGatewayAssociation
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AcceptDirectConnectGatewayAssociationProposalResponse' value with any optional fields omitted.
mkAcceptDirectConnectGatewayAssociationProposalResponse
    :: Core.Int -- ^ 'responseStatus'
    -> AcceptDirectConnectGatewayAssociationProposalResponse
mkAcceptDirectConnectGatewayAssociationProposalResponse
  responseStatus
  = AcceptDirectConnectGatewayAssociationProposalResponse'{directConnectGatewayAssociation
                                                             = Core.Nothing,
                                                           responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'directConnectGatewayAssociation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adcgaprrsDirectConnectGatewayAssociation :: Lens.Lens' AcceptDirectConnectGatewayAssociationProposalResponse (Core.Maybe Types.DirectConnectGatewayAssociation)
adcgaprrsDirectConnectGatewayAssociation = Lens.field @"directConnectGatewayAssociation"
{-# INLINEABLE adcgaprrsDirectConnectGatewayAssociation #-}
{-# DEPRECATED directConnectGatewayAssociation "Use generic-lens or generic-optics with 'directConnectGatewayAssociation' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adcgaprrsResponseStatus :: Lens.Lens' AcceptDirectConnectGatewayAssociationProposalResponse Core.Int
adcgaprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE adcgaprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
