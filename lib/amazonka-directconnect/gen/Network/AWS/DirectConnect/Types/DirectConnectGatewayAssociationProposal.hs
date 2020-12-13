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
    dcgapExistingAllowedPrefixesToDirectConnectGateway,
    dcgapDirectConnectGatewayId,
    dcgapProposalId,
    dcgapAssociatedGateway,
    dcgapProposalState,
    dcgapDirectConnectGatewayOwnerAccount,
    dcgapRequestedAllowedPrefixesToDirectConnectGateway,
  )
where

import Network.AWS.DirectConnect.Types.AssociatedGateway
import Network.AWS.DirectConnect.Types.DirectConnectGatewayAssociationProposalState
import Network.AWS.DirectConnect.Types.RouteFilterPrefix
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about the proposal request to attach a virtual private gateway to a Direct Connect gateway.
--
-- /See:/ 'mkDirectConnectGatewayAssociationProposal' smart constructor.
data DirectConnectGatewayAssociationProposal = DirectConnectGatewayAssociationProposal'
  { -- | The existing Amazon VPC prefixes advertised to the Direct Connect gateway.
    existingAllowedPrefixesToDirectConnectGateway :: Lude.Maybe [RouteFilterPrefix],
    -- | The ID of the Direct Connect gateway.
    directConnectGatewayId :: Lude.Maybe Lude.Text,
    -- | The ID of the association proposal.
    proposalId :: Lude.Maybe Lude.Text,
    -- | Information about the associated gateway.
    associatedGateway :: Lude.Maybe AssociatedGateway,
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
    proposalState :: Lude.Maybe DirectConnectGatewayAssociationProposalState,
    -- | The ID of the AWS account that owns the Direct Connect gateway.
    directConnectGatewayOwnerAccount :: Lude.Maybe Lude.Text,
    -- | The Amazon VPC prefixes to advertise to the Direct Connect gateway.
    requestedAllowedPrefixesToDirectConnectGateway :: Lude.Maybe [RouteFilterPrefix]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DirectConnectGatewayAssociationProposal' with the minimum fields required to make a request.
--
-- * 'existingAllowedPrefixesToDirectConnectGateway' - The existing Amazon VPC prefixes advertised to the Direct Connect gateway.
-- * 'directConnectGatewayId' - The ID of the Direct Connect gateway.
-- * 'proposalId' - The ID of the association proposal.
-- * 'associatedGateway' - Information about the associated gateway.
-- * 'proposalState' - The state of the proposal. The following are possible values:
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
-- * 'directConnectGatewayOwnerAccount' - The ID of the AWS account that owns the Direct Connect gateway.
-- * 'requestedAllowedPrefixesToDirectConnectGateway' - The Amazon VPC prefixes to advertise to the Direct Connect gateway.
mkDirectConnectGatewayAssociationProposal ::
  DirectConnectGatewayAssociationProposal
mkDirectConnectGatewayAssociationProposal =
  DirectConnectGatewayAssociationProposal'
    { existingAllowedPrefixesToDirectConnectGateway =
        Lude.Nothing,
      directConnectGatewayId = Lude.Nothing,
      proposalId = Lude.Nothing,
      associatedGateway = Lude.Nothing,
      proposalState = Lude.Nothing,
      directConnectGatewayOwnerAccount = Lude.Nothing,
      requestedAllowedPrefixesToDirectConnectGateway =
        Lude.Nothing
    }

-- | The existing Amazon VPC prefixes advertised to the Direct Connect gateway.
--
-- /Note:/ Consider using 'existingAllowedPrefixesToDirectConnectGateway' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcgapExistingAllowedPrefixesToDirectConnectGateway :: Lens.Lens' DirectConnectGatewayAssociationProposal (Lude.Maybe [RouteFilterPrefix])
dcgapExistingAllowedPrefixesToDirectConnectGateway = Lens.lens (existingAllowedPrefixesToDirectConnectGateway :: DirectConnectGatewayAssociationProposal -> Lude.Maybe [RouteFilterPrefix]) (\s a -> s {existingAllowedPrefixesToDirectConnectGateway = a} :: DirectConnectGatewayAssociationProposal)
{-# DEPRECATED dcgapExistingAllowedPrefixesToDirectConnectGateway "Use generic-lens or generic-optics with 'existingAllowedPrefixesToDirectConnectGateway' instead." #-}

-- | The ID of the Direct Connect gateway.
--
-- /Note:/ Consider using 'directConnectGatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcgapDirectConnectGatewayId :: Lens.Lens' DirectConnectGatewayAssociationProposal (Lude.Maybe Lude.Text)
dcgapDirectConnectGatewayId = Lens.lens (directConnectGatewayId :: DirectConnectGatewayAssociationProposal -> Lude.Maybe Lude.Text) (\s a -> s {directConnectGatewayId = a} :: DirectConnectGatewayAssociationProposal)
{-# DEPRECATED dcgapDirectConnectGatewayId "Use generic-lens or generic-optics with 'directConnectGatewayId' instead." #-}

-- | The ID of the association proposal.
--
-- /Note:/ Consider using 'proposalId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcgapProposalId :: Lens.Lens' DirectConnectGatewayAssociationProposal (Lude.Maybe Lude.Text)
dcgapProposalId = Lens.lens (proposalId :: DirectConnectGatewayAssociationProposal -> Lude.Maybe Lude.Text) (\s a -> s {proposalId = a} :: DirectConnectGatewayAssociationProposal)
{-# DEPRECATED dcgapProposalId "Use generic-lens or generic-optics with 'proposalId' instead." #-}

-- | Information about the associated gateway.
--
-- /Note:/ Consider using 'associatedGateway' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcgapAssociatedGateway :: Lens.Lens' DirectConnectGatewayAssociationProposal (Lude.Maybe AssociatedGateway)
dcgapAssociatedGateway = Lens.lens (associatedGateway :: DirectConnectGatewayAssociationProposal -> Lude.Maybe AssociatedGateway) (\s a -> s {associatedGateway = a} :: DirectConnectGatewayAssociationProposal)
{-# DEPRECATED dcgapAssociatedGateway "Use generic-lens or generic-optics with 'associatedGateway' instead." #-}

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
dcgapProposalState :: Lens.Lens' DirectConnectGatewayAssociationProposal (Lude.Maybe DirectConnectGatewayAssociationProposalState)
dcgapProposalState = Lens.lens (proposalState :: DirectConnectGatewayAssociationProposal -> Lude.Maybe DirectConnectGatewayAssociationProposalState) (\s a -> s {proposalState = a} :: DirectConnectGatewayAssociationProposal)
{-# DEPRECATED dcgapProposalState "Use generic-lens or generic-optics with 'proposalState' instead." #-}

-- | The ID of the AWS account that owns the Direct Connect gateway.
--
-- /Note:/ Consider using 'directConnectGatewayOwnerAccount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcgapDirectConnectGatewayOwnerAccount :: Lens.Lens' DirectConnectGatewayAssociationProposal (Lude.Maybe Lude.Text)
dcgapDirectConnectGatewayOwnerAccount = Lens.lens (directConnectGatewayOwnerAccount :: DirectConnectGatewayAssociationProposal -> Lude.Maybe Lude.Text) (\s a -> s {directConnectGatewayOwnerAccount = a} :: DirectConnectGatewayAssociationProposal)
{-# DEPRECATED dcgapDirectConnectGatewayOwnerAccount "Use generic-lens or generic-optics with 'directConnectGatewayOwnerAccount' instead." #-}

-- | The Amazon VPC prefixes to advertise to the Direct Connect gateway.
--
-- /Note:/ Consider using 'requestedAllowedPrefixesToDirectConnectGateway' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcgapRequestedAllowedPrefixesToDirectConnectGateway :: Lens.Lens' DirectConnectGatewayAssociationProposal (Lude.Maybe [RouteFilterPrefix])
dcgapRequestedAllowedPrefixesToDirectConnectGateway = Lens.lens (requestedAllowedPrefixesToDirectConnectGateway :: DirectConnectGatewayAssociationProposal -> Lude.Maybe [RouteFilterPrefix]) (\s a -> s {requestedAllowedPrefixesToDirectConnectGateway = a} :: DirectConnectGatewayAssociationProposal)
{-# DEPRECATED dcgapRequestedAllowedPrefixesToDirectConnectGateway "Use generic-lens or generic-optics with 'requestedAllowedPrefixesToDirectConnectGateway' instead." #-}

instance Lude.FromJSON DirectConnectGatewayAssociationProposal where
  parseJSON =
    Lude.withObject
      "DirectConnectGatewayAssociationProposal"
      ( \x ->
          DirectConnectGatewayAssociationProposal'
            Lude.<$> ( x Lude..:? "existingAllowedPrefixesToDirectConnectGateway"
                         Lude..!= Lude.mempty
                     )
            Lude.<*> (x Lude..:? "directConnectGatewayId")
            Lude.<*> (x Lude..:? "proposalId")
            Lude.<*> (x Lude..:? "associatedGateway")
            Lude.<*> (x Lude..:? "proposalState")
            Lude.<*> (x Lude..:? "directConnectGatewayOwnerAccount")
            Lude.<*> ( x Lude..:? "requestedAllowedPrefixesToDirectConnectGateway"
                         Lude..!= Lude.mempty
                     )
      )
