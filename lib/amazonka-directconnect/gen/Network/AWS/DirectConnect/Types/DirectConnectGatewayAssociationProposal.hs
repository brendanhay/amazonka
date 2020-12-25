{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectConnect.Types.DirectConnectGatewayAssociationProposal
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectConnect.Types.DirectConnectGatewayAssociationProposal
  ( DirectConnectGatewayAssociationProposal (..),

    -- * Smart constructor
    mkDirectConnectGatewayAssociationProposal,

    -- * Lenses
    dcgapAssociatedGateway,
    dcgapDirectConnectGatewayId,
    dcgapDirectConnectGatewayOwnerAccount,
    dcgapExistingAllowedPrefixesToDirectConnectGateway,
    dcgapProposalId,
    dcgapProposalState,
    dcgapRequestedAllowedPrefixesToDirectConnectGateway,
  )
where

import qualified Network.AWS.DirectConnect.Types.AssociatedGateway as Types
import qualified Network.AWS.DirectConnect.Types.DirectConnectGatewayAssociationProposalId as Types
import qualified Network.AWS.DirectConnect.Types.DirectConnectGatewayAssociationProposalState as Types
import qualified Network.AWS.DirectConnect.Types.DirectConnectGatewayId as Types
import qualified Network.AWS.DirectConnect.Types.DirectConnectGatewayOwnerAccount as Types
import qualified Network.AWS.DirectConnect.Types.RouteFilterPrefix as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about the proposal request to attach a virtual private gateway to a Direct Connect gateway.
--
-- /See:/ 'mkDirectConnectGatewayAssociationProposal' smart constructor.
data DirectConnectGatewayAssociationProposal = DirectConnectGatewayAssociationProposal'
  { -- | Information about the associated gateway.
    associatedGateway :: Core.Maybe Types.AssociatedGateway,
    -- | The ID of the Direct Connect gateway.
    directConnectGatewayId :: Core.Maybe Types.DirectConnectGatewayId,
    -- | The ID of the AWS account that owns the Direct Connect gateway.
    directConnectGatewayOwnerAccount :: Core.Maybe Types.DirectConnectGatewayOwnerAccount,
    -- | The existing Amazon VPC prefixes advertised to the Direct Connect gateway.
    existingAllowedPrefixesToDirectConnectGateway :: Core.Maybe [Types.RouteFilterPrefix],
    -- | The ID of the association proposal.
    proposalId :: Core.Maybe Types.DirectConnectGatewayAssociationProposalId,
    -- | The state of the proposal. The following are possible values:
    --
    --
    --     * @accepted@ : The proposal has been accepted. The Direct Connect gateway association is available to use in this state.
    --
    --
    --     * @deleted@ : The proposal has been deleted by the owner that made the proposal. The Direct Connect gateway association cannot be used in this state.
    --
    --
    --     * @requested@ : The proposal has been requested. The Direct Connect gateway association cannot be used in this state.
    proposalState :: Core.Maybe Types.DirectConnectGatewayAssociationProposalState,
    -- | The Amazon VPC prefixes to advertise to the Direct Connect gateway.
    requestedAllowedPrefixesToDirectConnectGateway :: Core.Maybe [Types.RouteFilterPrefix]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DirectConnectGatewayAssociationProposal' value with any optional fields omitted.
mkDirectConnectGatewayAssociationProposal ::
  DirectConnectGatewayAssociationProposal
mkDirectConnectGatewayAssociationProposal =
  DirectConnectGatewayAssociationProposal'
    { associatedGateway =
        Core.Nothing,
      directConnectGatewayId = Core.Nothing,
      directConnectGatewayOwnerAccount = Core.Nothing,
      existingAllowedPrefixesToDirectConnectGateway =
        Core.Nothing,
      proposalId = Core.Nothing,
      proposalState = Core.Nothing,
      requestedAllowedPrefixesToDirectConnectGateway =
        Core.Nothing
    }

-- | Information about the associated gateway.
--
-- /Note:/ Consider using 'associatedGateway' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcgapAssociatedGateway :: Lens.Lens' DirectConnectGatewayAssociationProposal (Core.Maybe Types.AssociatedGateway)
dcgapAssociatedGateway = Lens.field @"associatedGateway"
{-# DEPRECATED dcgapAssociatedGateway "Use generic-lens or generic-optics with 'associatedGateway' instead." #-}

-- | The ID of the Direct Connect gateway.
--
-- /Note:/ Consider using 'directConnectGatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcgapDirectConnectGatewayId :: Lens.Lens' DirectConnectGatewayAssociationProposal (Core.Maybe Types.DirectConnectGatewayId)
dcgapDirectConnectGatewayId = Lens.field @"directConnectGatewayId"
{-# DEPRECATED dcgapDirectConnectGatewayId "Use generic-lens or generic-optics with 'directConnectGatewayId' instead." #-}

-- | The ID of the AWS account that owns the Direct Connect gateway.
--
-- /Note:/ Consider using 'directConnectGatewayOwnerAccount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcgapDirectConnectGatewayOwnerAccount :: Lens.Lens' DirectConnectGatewayAssociationProposal (Core.Maybe Types.DirectConnectGatewayOwnerAccount)
dcgapDirectConnectGatewayOwnerAccount = Lens.field @"directConnectGatewayOwnerAccount"
{-# DEPRECATED dcgapDirectConnectGatewayOwnerAccount "Use generic-lens or generic-optics with 'directConnectGatewayOwnerAccount' instead." #-}

-- | The existing Amazon VPC prefixes advertised to the Direct Connect gateway.
--
-- /Note:/ Consider using 'existingAllowedPrefixesToDirectConnectGateway' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcgapExistingAllowedPrefixesToDirectConnectGateway :: Lens.Lens' DirectConnectGatewayAssociationProposal (Core.Maybe [Types.RouteFilterPrefix])
dcgapExistingAllowedPrefixesToDirectConnectGateway = Lens.field @"existingAllowedPrefixesToDirectConnectGateway"
{-# DEPRECATED dcgapExistingAllowedPrefixesToDirectConnectGateway "Use generic-lens or generic-optics with 'existingAllowedPrefixesToDirectConnectGateway' instead." #-}

-- | The ID of the association proposal.
--
-- /Note:/ Consider using 'proposalId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcgapProposalId :: Lens.Lens' DirectConnectGatewayAssociationProposal (Core.Maybe Types.DirectConnectGatewayAssociationProposalId)
dcgapProposalId = Lens.field @"proposalId"
{-# DEPRECATED dcgapProposalId "Use generic-lens or generic-optics with 'proposalId' instead." #-}

-- | The state of the proposal. The following are possible values:
--
--
--     * @accepted@ : The proposal has been accepted. The Direct Connect gateway association is available to use in this state.
--
--
--     * @deleted@ : The proposal has been deleted by the owner that made the proposal. The Direct Connect gateway association cannot be used in this state.
--
--
--     * @requested@ : The proposal has been requested. The Direct Connect gateway association cannot be used in this state.
--
--
--
-- /Note:/ Consider using 'proposalState' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcgapProposalState :: Lens.Lens' DirectConnectGatewayAssociationProposal (Core.Maybe Types.DirectConnectGatewayAssociationProposalState)
dcgapProposalState = Lens.field @"proposalState"
{-# DEPRECATED dcgapProposalState "Use generic-lens or generic-optics with 'proposalState' instead." #-}

-- | The Amazon VPC prefixes to advertise to the Direct Connect gateway.
--
-- /Note:/ Consider using 'requestedAllowedPrefixesToDirectConnectGateway' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcgapRequestedAllowedPrefixesToDirectConnectGateway :: Lens.Lens' DirectConnectGatewayAssociationProposal (Core.Maybe [Types.RouteFilterPrefix])
dcgapRequestedAllowedPrefixesToDirectConnectGateway = Lens.field @"requestedAllowedPrefixesToDirectConnectGateway"
{-# DEPRECATED dcgapRequestedAllowedPrefixesToDirectConnectGateway "Use generic-lens or generic-optics with 'requestedAllowedPrefixesToDirectConnectGateway' instead." #-}

instance Core.FromJSON DirectConnectGatewayAssociationProposal where
  parseJSON =
    Core.withObject "DirectConnectGatewayAssociationProposal" Core.$
      \x ->
        DirectConnectGatewayAssociationProposal'
          Core.<$> (x Core..:? "associatedGateway")
          Core.<*> (x Core..:? "directConnectGatewayId")
          Core.<*> (x Core..:? "directConnectGatewayOwnerAccount")
          Core.<*> (x Core..:? "existingAllowedPrefixesToDirectConnectGateway")
          Core.<*> (x Core..:? "proposalId")
          Core.<*> (x Core..:? "proposalState")
          Core.<*> (x Core..:? "requestedAllowedPrefixesToDirectConnectGateway")
