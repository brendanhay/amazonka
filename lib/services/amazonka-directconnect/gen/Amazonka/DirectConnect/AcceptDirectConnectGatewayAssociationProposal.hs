{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.DirectConnect.AcceptDirectConnectGatewayAssociationProposal
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Accepts a proposal request to attach a virtual private gateway or
-- transit gateway to a Direct Connect gateway.
module Amazonka.DirectConnect.AcceptDirectConnectGatewayAssociationProposal
  ( -- * Creating a Request
    AcceptDirectConnectGatewayAssociationProposal (..),
    newAcceptDirectConnectGatewayAssociationProposal,

    -- * Request Lenses
    acceptDirectConnectGatewayAssociationProposal_overrideAllowedPrefixesToDirectConnectGateway,
    acceptDirectConnectGatewayAssociationProposal_directConnectGatewayId,
    acceptDirectConnectGatewayAssociationProposal_proposalId,
    acceptDirectConnectGatewayAssociationProposal_associatedGatewayOwnerAccount,

    -- * Destructuring the Response
    AcceptDirectConnectGatewayAssociationProposalResponse (..),
    newAcceptDirectConnectGatewayAssociationProposalResponse,

    -- * Response Lenses
    acceptDirectConnectGatewayAssociationProposalResponse_directConnectGatewayAssociation,
    acceptDirectConnectGatewayAssociationProposalResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DirectConnect.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newAcceptDirectConnectGatewayAssociationProposal' smart constructor.
data AcceptDirectConnectGatewayAssociationProposal = AcceptDirectConnectGatewayAssociationProposal'
  { -- | Overrides the Amazon VPC prefixes advertised to the Direct Connect
    -- gateway.
    --
    -- For information about how to set the prefixes, see
    -- <https://docs.aws.amazon.com/directconnect/latest/UserGuide/multi-account-associate-vgw.html#allowed-prefixes Allowed Prefixes>
    -- in the /Direct Connect User Guide/.
    overrideAllowedPrefixesToDirectConnectGateway :: Prelude.Maybe [RouteFilterPrefix],
    -- | The ID of the Direct Connect gateway.
    directConnectGatewayId :: Prelude.Text,
    -- | The ID of the request proposal.
    proposalId :: Prelude.Text,
    -- | The ID of the Amazon Web Services account that owns the virtual private
    -- gateway or transit gateway.
    associatedGatewayOwnerAccount :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AcceptDirectConnectGatewayAssociationProposal' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'overrideAllowedPrefixesToDirectConnectGateway', 'acceptDirectConnectGatewayAssociationProposal_overrideAllowedPrefixesToDirectConnectGateway' - Overrides the Amazon VPC prefixes advertised to the Direct Connect
-- gateway.
--
-- For information about how to set the prefixes, see
-- <https://docs.aws.amazon.com/directconnect/latest/UserGuide/multi-account-associate-vgw.html#allowed-prefixes Allowed Prefixes>
-- in the /Direct Connect User Guide/.
--
-- 'directConnectGatewayId', 'acceptDirectConnectGatewayAssociationProposal_directConnectGatewayId' - The ID of the Direct Connect gateway.
--
-- 'proposalId', 'acceptDirectConnectGatewayAssociationProposal_proposalId' - The ID of the request proposal.
--
-- 'associatedGatewayOwnerAccount', 'acceptDirectConnectGatewayAssociationProposal_associatedGatewayOwnerAccount' - The ID of the Amazon Web Services account that owns the virtual private
-- gateway or transit gateway.
newAcceptDirectConnectGatewayAssociationProposal ::
  -- | 'directConnectGatewayId'
  Prelude.Text ->
  -- | 'proposalId'
  Prelude.Text ->
  -- | 'associatedGatewayOwnerAccount'
  Prelude.Text ->
  AcceptDirectConnectGatewayAssociationProposal
newAcceptDirectConnectGatewayAssociationProposal
  pDirectConnectGatewayId_
  pProposalId_
  pAssociatedGatewayOwnerAccount_ =
    AcceptDirectConnectGatewayAssociationProposal'
      { overrideAllowedPrefixesToDirectConnectGateway =
          Prelude.Nothing,
        directConnectGatewayId =
          pDirectConnectGatewayId_,
        proposalId = pProposalId_,
        associatedGatewayOwnerAccount =
          pAssociatedGatewayOwnerAccount_
      }

-- | Overrides the Amazon VPC prefixes advertised to the Direct Connect
-- gateway.
--
-- For information about how to set the prefixes, see
-- <https://docs.aws.amazon.com/directconnect/latest/UserGuide/multi-account-associate-vgw.html#allowed-prefixes Allowed Prefixes>
-- in the /Direct Connect User Guide/.
acceptDirectConnectGatewayAssociationProposal_overrideAllowedPrefixesToDirectConnectGateway :: Lens.Lens' AcceptDirectConnectGatewayAssociationProposal (Prelude.Maybe [RouteFilterPrefix])
acceptDirectConnectGatewayAssociationProposal_overrideAllowedPrefixesToDirectConnectGateway = Lens.lens (\AcceptDirectConnectGatewayAssociationProposal' {overrideAllowedPrefixesToDirectConnectGateway} -> overrideAllowedPrefixesToDirectConnectGateway) (\s@AcceptDirectConnectGatewayAssociationProposal' {} a -> s {overrideAllowedPrefixesToDirectConnectGateway = a} :: AcceptDirectConnectGatewayAssociationProposal) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the Direct Connect gateway.
acceptDirectConnectGatewayAssociationProposal_directConnectGatewayId :: Lens.Lens' AcceptDirectConnectGatewayAssociationProposal Prelude.Text
acceptDirectConnectGatewayAssociationProposal_directConnectGatewayId = Lens.lens (\AcceptDirectConnectGatewayAssociationProposal' {directConnectGatewayId} -> directConnectGatewayId) (\s@AcceptDirectConnectGatewayAssociationProposal' {} a -> s {directConnectGatewayId = a} :: AcceptDirectConnectGatewayAssociationProposal)

-- | The ID of the request proposal.
acceptDirectConnectGatewayAssociationProposal_proposalId :: Lens.Lens' AcceptDirectConnectGatewayAssociationProposal Prelude.Text
acceptDirectConnectGatewayAssociationProposal_proposalId = Lens.lens (\AcceptDirectConnectGatewayAssociationProposal' {proposalId} -> proposalId) (\s@AcceptDirectConnectGatewayAssociationProposal' {} a -> s {proposalId = a} :: AcceptDirectConnectGatewayAssociationProposal)

-- | The ID of the Amazon Web Services account that owns the virtual private
-- gateway or transit gateway.
acceptDirectConnectGatewayAssociationProposal_associatedGatewayOwnerAccount :: Lens.Lens' AcceptDirectConnectGatewayAssociationProposal Prelude.Text
acceptDirectConnectGatewayAssociationProposal_associatedGatewayOwnerAccount = Lens.lens (\AcceptDirectConnectGatewayAssociationProposal' {associatedGatewayOwnerAccount} -> associatedGatewayOwnerAccount) (\s@AcceptDirectConnectGatewayAssociationProposal' {} a -> s {associatedGatewayOwnerAccount = a} :: AcceptDirectConnectGatewayAssociationProposal)

instance
  Core.AWSRequest
    AcceptDirectConnectGatewayAssociationProposal
  where
  type
    AWSResponse
      AcceptDirectConnectGatewayAssociationProposal =
      AcceptDirectConnectGatewayAssociationProposalResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          AcceptDirectConnectGatewayAssociationProposalResponse'
            Prelude.<$> (x Data..?> "directConnectGatewayAssociation")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    AcceptDirectConnectGatewayAssociationProposal
  where
  hashWithSalt
    _salt
    AcceptDirectConnectGatewayAssociationProposal' {..} =
      _salt
        `Prelude.hashWithSalt` overrideAllowedPrefixesToDirectConnectGateway
        `Prelude.hashWithSalt` directConnectGatewayId
        `Prelude.hashWithSalt` proposalId
        `Prelude.hashWithSalt` associatedGatewayOwnerAccount

instance
  Prelude.NFData
    AcceptDirectConnectGatewayAssociationProposal
  where
  rnf
    AcceptDirectConnectGatewayAssociationProposal' {..} =
      Prelude.rnf
        overrideAllowedPrefixesToDirectConnectGateway
        `Prelude.seq` Prelude.rnf directConnectGatewayId
        `Prelude.seq` Prelude.rnf proposalId
        `Prelude.seq` Prelude.rnf associatedGatewayOwnerAccount

instance
  Data.ToHeaders
    AcceptDirectConnectGatewayAssociationProposal
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "OvertureService.AcceptDirectConnectGatewayAssociationProposal" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Data.ToJSON
    AcceptDirectConnectGatewayAssociationProposal
  where
  toJSON
    AcceptDirectConnectGatewayAssociationProposal' {..} =
      Data.object
        ( Prelude.catMaybes
            [ ( "overrideAllowedPrefixesToDirectConnectGateway"
                  Data..=
              )
                Prelude.<$> overrideAllowedPrefixesToDirectConnectGateway,
              Prelude.Just
                ( "directConnectGatewayId"
                    Data..= directConnectGatewayId
                ),
              Prelude.Just ("proposalId" Data..= proposalId),
              Prelude.Just
                ( "associatedGatewayOwnerAccount"
                    Data..= associatedGatewayOwnerAccount
                )
            ]
        )

instance
  Data.ToPath
    AcceptDirectConnectGatewayAssociationProposal
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    AcceptDirectConnectGatewayAssociationProposal
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAcceptDirectConnectGatewayAssociationProposalResponse' smart constructor.
data AcceptDirectConnectGatewayAssociationProposalResponse = AcceptDirectConnectGatewayAssociationProposalResponse'
  { directConnectGatewayAssociation :: Prelude.Maybe DirectConnectGatewayAssociation,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AcceptDirectConnectGatewayAssociationProposalResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'directConnectGatewayAssociation', 'acceptDirectConnectGatewayAssociationProposalResponse_directConnectGatewayAssociation' - Undocumented member.
--
-- 'httpStatus', 'acceptDirectConnectGatewayAssociationProposalResponse_httpStatus' - The response's http status code.
newAcceptDirectConnectGatewayAssociationProposalResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  AcceptDirectConnectGatewayAssociationProposalResponse
newAcceptDirectConnectGatewayAssociationProposalResponse
  pHttpStatus_ =
    AcceptDirectConnectGatewayAssociationProposalResponse'
      { directConnectGatewayAssociation =
          Prelude.Nothing,
        httpStatus =
          pHttpStatus_
      }

-- | Undocumented member.
acceptDirectConnectGatewayAssociationProposalResponse_directConnectGatewayAssociation :: Lens.Lens' AcceptDirectConnectGatewayAssociationProposalResponse (Prelude.Maybe DirectConnectGatewayAssociation)
acceptDirectConnectGatewayAssociationProposalResponse_directConnectGatewayAssociation = Lens.lens (\AcceptDirectConnectGatewayAssociationProposalResponse' {directConnectGatewayAssociation} -> directConnectGatewayAssociation) (\s@AcceptDirectConnectGatewayAssociationProposalResponse' {} a -> s {directConnectGatewayAssociation = a} :: AcceptDirectConnectGatewayAssociationProposalResponse)

-- | The response's http status code.
acceptDirectConnectGatewayAssociationProposalResponse_httpStatus :: Lens.Lens' AcceptDirectConnectGatewayAssociationProposalResponse Prelude.Int
acceptDirectConnectGatewayAssociationProposalResponse_httpStatus = Lens.lens (\AcceptDirectConnectGatewayAssociationProposalResponse' {httpStatus} -> httpStatus) (\s@AcceptDirectConnectGatewayAssociationProposalResponse' {} a -> s {httpStatus = a} :: AcceptDirectConnectGatewayAssociationProposalResponse)

instance
  Prelude.NFData
    AcceptDirectConnectGatewayAssociationProposalResponse
  where
  rnf
    AcceptDirectConnectGatewayAssociationProposalResponse' {..} =
      Prelude.rnf directConnectGatewayAssociation
        `Prelude.seq` Prelude.rnf httpStatus
