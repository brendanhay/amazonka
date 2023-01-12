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
-- Module      : Amazonka.DirectConnect.Types.DirectConnectGatewayAssociationProposal
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DirectConnect.Types.DirectConnectGatewayAssociationProposal where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DirectConnect.Types.AssociatedGateway
import Amazonka.DirectConnect.Types.DirectConnectGatewayAssociationProposalState
import Amazonka.DirectConnect.Types.RouteFilterPrefix
import qualified Amazonka.Prelude as Prelude

-- | Information about the proposal request to attach a virtual private
-- gateway to a Direct Connect gateway.
--
-- /See:/ 'newDirectConnectGatewayAssociationProposal' smart constructor.
data DirectConnectGatewayAssociationProposal = DirectConnectGatewayAssociationProposal'
  { -- | Information about the associated gateway.
    associatedGateway :: Prelude.Maybe AssociatedGateway,
    -- | The ID of the Direct Connect gateway.
    directConnectGatewayId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Amazon Web Services account that owns the Direct Connect
    -- gateway.
    directConnectGatewayOwnerAccount :: Prelude.Maybe Prelude.Text,
    -- | The existing Amazon VPC prefixes advertised to the Direct Connect
    -- gateway.
    existingAllowedPrefixesToDirectConnectGateway :: Prelude.Maybe [RouteFilterPrefix],
    -- | The ID of the association proposal.
    proposalId :: Prelude.Maybe Prelude.Text,
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
    proposalState :: Prelude.Maybe DirectConnectGatewayAssociationProposalState,
    -- | The Amazon VPC prefixes to advertise to the Direct Connect gateway.
    requestedAllowedPrefixesToDirectConnectGateway :: Prelude.Maybe [RouteFilterPrefix]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DirectConnectGatewayAssociationProposal' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'associatedGateway', 'directConnectGatewayAssociationProposal_associatedGateway' - Information about the associated gateway.
--
-- 'directConnectGatewayId', 'directConnectGatewayAssociationProposal_directConnectGatewayId' - The ID of the Direct Connect gateway.
--
-- 'directConnectGatewayOwnerAccount', 'directConnectGatewayAssociationProposal_directConnectGatewayOwnerAccount' - The ID of the Amazon Web Services account that owns the Direct Connect
-- gateway.
--
-- 'existingAllowedPrefixesToDirectConnectGateway', 'directConnectGatewayAssociationProposal_existingAllowedPrefixesToDirectConnectGateway' - The existing Amazon VPC prefixes advertised to the Direct Connect
-- gateway.
--
-- 'proposalId', 'directConnectGatewayAssociationProposal_proposalId' - The ID of the association proposal.
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
-- 'requestedAllowedPrefixesToDirectConnectGateway', 'directConnectGatewayAssociationProposal_requestedAllowedPrefixesToDirectConnectGateway' - The Amazon VPC prefixes to advertise to the Direct Connect gateway.
newDirectConnectGatewayAssociationProposal ::
  DirectConnectGatewayAssociationProposal
newDirectConnectGatewayAssociationProposal =
  DirectConnectGatewayAssociationProposal'
    { associatedGateway =
        Prelude.Nothing,
      directConnectGatewayId =
        Prelude.Nothing,
      directConnectGatewayOwnerAccount =
        Prelude.Nothing,
      existingAllowedPrefixesToDirectConnectGateway =
        Prelude.Nothing,
      proposalId = Prelude.Nothing,
      proposalState = Prelude.Nothing,
      requestedAllowedPrefixesToDirectConnectGateway =
        Prelude.Nothing
    }

-- | Information about the associated gateway.
directConnectGatewayAssociationProposal_associatedGateway :: Lens.Lens' DirectConnectGatewayAssociationProposal (Prelude.Maybe AssociatedGateway)
directConnectGatewayAssociationProposal_associatedGateway = Lens.lens (\DirectConnectGatewayAssociationProposal' {associatedGateway} -> associatedGateway) (\s@DirectConnectGatewayAssociationProposal' {} a -> s {associatedGateway = a} :: DirectConnectGatewayAssociationProposal)

-- | The ID of the Direct Connect gateway.
directConnectGatewayAssociationProposal_directConnectGatewayId :: Lens.Lens' DirectConnectGatewayAssociationProposal (Prelude.Maybe Prelude.Text)
directConnectGatewayAssociationProposal_directConnectGatewayId = Lens.lens (\DirectConnectGatewayAssociationProposal' {directConnectGatewayId} -> directConnectGatewayId) (\s@DirectConnectGatewayAssociationProposal' {} a -> s {directConnectGatewayId = a} :: DirectConnectGatewayAssociationProposal)

-- | The ID of the Amazon Web Services account that owns the Direct Connect
-- gateway.
directConnectGatewayAssociationProposal_directConnectGatewayOwnerAccount :: Lens.Lens' DirectConnectGatewayAssociationProposal (Prelude.Maybe Prelude.Text)
directConnectGatewayAssociationProposal_directConnectGatewayOwnerAccount = Lens.lens (\DirectConnectGatewayAssociationProposal' {directConnectGatewayOwnerAccount} -> directConnectGatewayOwnerAccount) (\s@DirectConnectGatewayAssociationProposal' {} a -> s {directConnectGatewayOwnerAccount = a} :: DirectConnectGatewayAssociationProposal)

-- | The existing Amazon VPC prefixes advertised to the Direct Connect
-- gateway.
directConnectGatewayAssociationProposal_existingAllowedPrefixesToDirectConnectGateway :: Lens.Lens' DirectConnectGatewayAssociationProposal (Prelude.Maybe [RouteFilterPrefix])
directConnectGatewayAssociationProposal_existingAllowedPrefixesToDirectConnectGateway = Lens.lens (\DirectConnectGatewayAssociationProposal' {existingAllowedPrefixesToDirectConnectGateway} -> existingAllowedPrefixesToDirectConnectGateway) (\s@DirectConnectGatewayAssociationProposal' {} a -> s {existingAllowedPrefixesToDirectConnectGateway = a} :: DirectConnectGatewayAssociationProposal) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the association proposal.
directConnectGatewayAssociationProposal_proposalId :: Lens.Lens' DirectConnectGatewayAssociationProposal (Prelude.Maybe Prelude.Text)
directConnectGatewayAssociationProposal_proposalId = Lens.lens (\DirectConnectGatewayAssociationProposal' {proposalId} -> proposalId) (\s@DirectConnectGatewayAssociationProposal' {} a -> s {proposalId = a} :: DirectConnectGatewayAssociationProposal)

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
directConnectGatewayAssociationProposal_proposalState :: Lens.Lens' DirectConnectGatewayAssociationProposal (Prelude.Maybe DirectConnectGatewayAssociationProposalState)
directConnectGatewayAssociationProposal_proposalState = Lens.lens (\DirectConnectGatewayAssociationProposal' {proposalState} -> proposalState) (\s@DirectConnectGatewayAssociationProposal' {} a -> s {proposalState = a} :: DirectConnectGatewayAssociationProposal)

-- | The Amazon VPC prefixes to advertise to the Direct Connect gateway.
directConnectGatewayAssociationProposal_requestedAllowedPrefixesToDirectConnectGateway :: Lens.Lens' DirectConnectGatewayAssociationProposal (Prelude.Maybe [RouteFilterPrefix])
directConnectGatewayAssociationProposal_requestedAllowedPrefixesToDirectConnectGateway = Lens.lens (\DirectConnectGatewayAssociationProposal' {requestedAllowedPrefixesToDirectConnectGateway} -> requestedAllowedPrefixesToDirectConnectGateway) (\s@DirectConnectGatewayAssociationProposal' {} a -> s {requestedAllowedPrefixesToDirectConnectGateway = a} :: DirectConnectGatewayAssociationProposal) Prelude.. Lens.mapping Lens.coerced

instance
  Data.FromJSON
    DirectConnectGatewayAssociationProposal
  where
  parseJSON =
    Data.withObject
      "DirectConnectGatewayAssociationProposal"
      ( \x ->
          DirectConnectGatewayAssociationProposal'
            Prelude.<$> (x Data..:? "associatedGateway")
            Prelude.<*> (x Data..:? "directConnectGatewayId")
            Prelude.<*> (x Data..:? "directConnectGatewayOwnerAccount")
            Prelude.<*> ( x
                            Data..:? "existingAllowedPrefixesToDirectConnectGateway"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "proposalId")
            Prelude.<*> (x Data..:? "proposalState")
            Prelude.<*> ( x
                            Data..:? "requestedAllowedPrefixesToDirectConnectGateway"
                            Data..!= Prelude.mempty
                        )
      )

instance
  Prelude.Hashable
    DirectConnectGatewayAssociationProposal
  where
  hashWithSalt
    _salt
    DirectConnectGatewayAssociationProposal' {..} =
      _salt `Prelude.hashWithSalt` associatedGateway
        `Prelude.hashWithSalt` directConnectGatewayId
        `Prelude.hashWithSalt` directConnectGatewayOwnerAccount
        `Prelude.hashWithSalt` existingAllowedPrefixesToDirectConnectGateway
        `Prelude.hashWithSalt` proposalId
        `Prelude.hashWithSalt` proposalState
        `Prelude.hashWithSalt` requestedAllowedPrefixesToDirectConnectGateway

instance
  Prelude.NFData
    DirectConnectGatewayAssociationProposal
  where
  rnf DirectConnectGatewayAssociationProposal' {..} =
    Prelude.rnf associatedGateway
      `Prelude.seq` Prelude.rnf directConnectGatewayId
      `Prelude.seq` Prelude.rnf directConnectGatewayOwnerAccount
      `Prelude.seq` Prelude.rnf
        existingAllowedPrefixesToDirectConnectGateway
      `Prelude.seq` Prelude.rnf proposalId
      `Prelude.seq` Prelude.rnf proposalState
      `Prelude.seq` Prelude.rnf
        requestedAllowedPrefixesToDirectConnectGateway
