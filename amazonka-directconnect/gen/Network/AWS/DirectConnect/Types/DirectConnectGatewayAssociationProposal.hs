{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectConnect.Types.DirectConnectGatewayAssociationProposal
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectConnect.Types.DirectConnectGatewayAssociationProposal where

import qualified Network.AWS.Core as Core
import Network.AWS.DirectConnect.Types.AssociatedGateway
import Network.AWS.DirectConnect.Types.DirectConnectGatewayAssociationProposalState
import Network.AWS.DirectConnect.Types.RouteFilterPrefix
import qualified Network.AWS.Lens as Lens

-- | Information about the proposal request to attach a virtual private
-- gateway to a Direct Connect gateway.
--
-- /See:/ 'newDirectConnectGatewayAssociationProposal' smart constructor.
data DirectConnectGatewayAssociationProposal = DirectConnectGatewayAssociationProposal'
  { -- | The ID of the association proposal.
    proposalId :: Core.Maybe Core.Text,
    -- | The Amazon VPC prefixes to advertise to the Direct Connect gateway.
    requestedAllowedPrefixesToDirectConnectGateway :: Core.Maybe [RouteFilterPrefix],
    -- | The state of the proposal. The following are possible values:
    --
    -- -   @accepted@: The proposal has been accepted. The Direct Connect
    --     gateway association is available to use in this state.
    --
    -- -   @deleted@: The proposal has been deleted by the owner that made the
    --     proposal. The Direct Connect gateway association cannot be used in
    --     this state.
    --
    -- -   @requested@: The proposal has been requested. The Direct Connect
    --     gateway association cannot be used in this state.
    proposalState :: Core.Maybe DirectConnectGatewayAssociationProposalState,
    -- | Information about the associated gateway.
    associatedGateway :: Core.Maybe AssociatedGateway,
    -- | The existing Amazon VPC prefixes advertised to the Direct Connect
    -- gateway.
    existingAllowedPrefixesToDirectConnectGateway :: Core.Maybe [RouteFilterPrefix],
    -- | The ID of the Direct Connect gateway.
    directConnectGatewayId :: Core.Maybe Core.Text,
    -- | The ID of the AWS account that owns the Direct Connect gateway.
    directConnectGatewayOwnerAccount :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DirectConnectGatewayAssociationProposal' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'proposalId', 'directConnectGatewayAssociationProposal_proposalId' - The ID of the association proposal.
--
-- 'requestedAllowedPrefixesToDirectConnectGateway', 'directConnectGatewayAssociationProposal_requestedAllowedPrefixesToDirectConnectGateway' - The Amazon VPC prefixes to advertise to the Direct Connect gateway.
--
-- 'proposalState', 'directConnectGatewayAssociationProposal_proposalState' - The state of the proposal. The following are possible values:
--
-- -   @accepted@: The proposal has been accepted. The Direct Connect
--     gateway association is available to use in this state.
--
-- -   @deleted@: The proposal has been deleted by the owner that made the
--     proposal. The Direct Connect gateway association cannot be used in
--     this state.
--
-- -   @requested@: The proposal has been requested. The Direct Connect
--     gateway association cannot be used in this state.
--
-- 'associatedGateway', 'directConnectGatewayAssociationProposal_associatedGateway' - Information about the associated gateway.
--
-- 'existingAllowedPrefixesToDirectConnectGateway', 'directConnectGatewayAssociationProposal_existingAllowedPrefixesToDirectConnectGateway' - The existing Amazon VPC prefixes advertised to the Direct Connect
-- gateway.
--
-- 'directConnectGatewayId', 'directConnectGatewayAssociationProposal_directConnectGatewayId' - The ID of the Direct Connect gateway.
--
-- 'directConnectGatewayOwnerAccount', 'directConnectGatewayAssociationProposal_directConnectGatewayOwnerAccount' - The ID of the AWS account that owns the Direct Connect gateway.
newDirectConnectGatewayAssociationProposal ::
  DirectConnectGatewayAssociationProposal
newDirectConnectGatewayAssociationProposal =
  DirectConnectGatewayAssociationProposal'
    { proposalId =
        Core.Nothing,
      requestedAllowedPrefixesToDirectConnectGateway =
        Core.Nothing,
      proposalState = Core.Nothing,
      associatedGateway = Core.Nothing,
      existingAllowedPrefixesToDirectConnectGateway =
        Core.Nothing,
      directConnectGatewayId =
        Core.Nothing,
      directConnectGatewayOwnerAccount =
        Core.Nothing
    }

-- | The ID of the association proposal.
directConnectGatewayAssociationProposal_proposalId :: Lens.Lens' DirectConnectGatewayAssociationProposal (Core.Maybe Core.Text)
directConnectGatewayAssociationProposal_proposalId = Lens.lens (\DirectConnectGatewayAssociationProposal' {proposalId} -> proposalId) (\s@DirectConnectGatewayAssociationProposal' {} a -> s {proposalId = a} :: DirectConnectGatewayAssociationProposal)

-- | The Amazon VPC prefixes to advertise to the Direct Connect gateway.
directConnectGatewayAssociationProposal_requestedAllowedPrefixesToDirectConnectGateway :: Lens.Lens' DirectConnectGatewayAssociationProposal (Core.Maybe [RouteFilterPrefix])
directConnectGatewayAssociationProposal_requestedAllowedPrefixesToDirectConnectGateway = Lens.lens (\DirectConnectGatewayAssociationProposal' {requestedAllowedPrefixesToDirectConnectGateway} -> requestedAllowedPrefixesToDirectConnectGateway) (\s@DirectConnectGatewayAssociationProposal' {} a -> s {requestedAllowedPrefixesToDirectConnectGateway = a} :: DirectConnectGatewayAssociationProposal) Core.. Lens.mapping Lens._Coerce

-- | The state of the proposal. The following are possible values:
--
-- -   @accepted@: The proposal has been accepted. The Direct Connect
--     gateway association is available to use in this state.
--
-- -   @deleted@: The proposal has been deleted by the owner that made the
--     proposal. The Direct Connect gateway association cannot be used in
--     this state.
--
-- -   @requested@: The proposal has been requested. The Direct Connect
--     gateway association cannot be used in this state.
directConnectGatewayAssociationProposal_proposalState :: Lens.Lens' DirectConnectGatewayAssociationProposal (Core.Maybe DirectConnectGatewayAssociationProposalState)
directConnectGatewayAssociationProposal_proposalState = Lens.lens (\DirectConnectGatewayAssociationProposal' {proposalState} -> proposalState) (\s@DirectConnectGatewayAssociationProposal' {} a -> s {proposalState = a} :: DirectConnectGatewayAssociationProposal)

-- | Information about the associated gateway.
directConnectGatewayAssociationProposal_associatedGateway :: Lens.Lens' DirectConnectGatewayAssociationProposal (Core.Maybe AssociatedGateway)
directConnectGatewayAssociationProposal_associatedGateway = Lens.lens (\DirectConnectGatewayAssociationProposal' {associatedGateway} -> associatedGateway) (\s@DirectConnectGatewayAssociationProposal' {} a -> s {associatedGateway = a} :: DirectConnectGatewayAssociationProposal)

-- | The existing Amazon VPC prefixes advertised to the Direct Connect
-- gateway.
directConnectGatewayAssociationProposal_existingAllowedPrefixesToDirectConnectGateway :: Lens.Lens' DirectConnectGatewayAssociationProposal (Core.Maybe [RouteFilterPrefix])
directConnectGatewayAssociationProposal_existingAllowedPrefixesToDirectConnectGateway = Lens.lens (\DirectConnectGatewayAssociationProposal' {existingAllowedPrefixesToDirectConnectGateway} -> existingAllowedPrefixesToDirectConnectGateway) (\s@DirectConnectGatewayAssociationProposal' {} a -> s {existingAllowedPrefixesToDirectConnectGateway = a} :: DirectConnectGatewayAssociationProposal) Core.. Lens.mapping Lens._Coerce

-- | The ID of the Direct Connect gateway.
directConnectGatewayAssociationProposal_directConnectGatewayId :: Lens.Lens' DirectConnectGatewayAssociationProposal (Core.Maybe Core.Text)
directConnectGatewayAssociationProposal_directConnectGatewayId = Lens.lens (\DirectConnectGatewayAssociationProposal' {directConnectGatewayId} -> directConnectGatewayId) (\s@DirectConnectGatewayAssociationProposal' {} a -> s {directConnectGatewayId = a} :: DirectConnectGatewayAssociationProposal)

-- | The ID of the AWS account that owns the Direct Connect gateway.
directConnectGatewayAssociationProposal_directConnectGatewayOwnerAccount :: Lens.Lens' DirectConnectGatewayAssociationProposal (Core.Maybe Core.Text)
directConnectGatewayAssociationProposal_directConnectGatewayOwnerAccount = Lens.lens (\DirectConnectGatewayAssociationProposal' {directConnectGatewayOwnerAccount} -> directConnectGatewayOwnerAccount) (\s@DirectConnectGatewayAssociationProposal' {} a -> s {directConnectGatewayOwnerAccount = a} :: DirectConnectGatewayAssociationProposal)

instance
  Core.FromJSON
    DirectConnectGatewayAssociationProposal
  where
  parseJSON =
    Core.withObject
      "DirectConnectGatewayAssociationProposal"
      ( \x ->
          DirectConnectGatewayAssociationProposal'
            Core.<$> (x Core..:? "proposalId")
            Core.<*> ( x
                         Core..:? "requestedAllowedPrefixesToDirectConnectGateway"
                         Core..!= Core.mempty
                     )
            Core.<*> (x Core..:? "proposalState")
            Core.<*> (x Core..:? "associatedGateway")
            Core.<*> ( x
                         Core..:? "existingAllowedPrefixesToDirectConnectGateway"
                         Core..!= Core.mempty
                     )
            Core.<*> (x Core..:? "directConnectGatewayId")
            Core.<*> (x Core..:? "directConnectGatewayOwnerAccount")
      )

instance
  Core.Hashable
    DirectConnectGatewayAssociationProposal

instance
  Core.NFData
    DirectConnectGatewayAssociationProposal
