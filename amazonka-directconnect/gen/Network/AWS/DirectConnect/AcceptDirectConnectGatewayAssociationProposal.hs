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
-- Module      : Network.AWS.DirectConnect.AcceptDirectConnectGatewayAssociationProposal
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Accepts a proposal request to attach a virtual private gateway or
-- transit gateway to a Direct Connect gateway.
module Network.AWS.DirectConnect.AcceptDirectConnectGatewayAssociationProposal
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

import qualified Network.AWS.Core as Core
import Network.AWS.DirectConnect.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newAcceptDirectConnectGatewayAssociationProposal' smart constructor.
data AcceptDirectConnectGatewayAssociationProposal = AcceptDirectConnectGatewayAssociationProposal'
  { -- | Overrides the Amazon VPC prefixes advertised to the Direct Connect
    -- gateway.
    --
    -- For information about how to set the prefixes, see
    -- <https://docs.aws.amazon.com/directconnect/latest/UserGuide/multi-account-associate-vgw.html#allowed-prefixes Allowed Prefixes>
    -- in the /AWS Direct Connect User Guide/.
    overrideAllowedPrefixesToDirectConnectGateway :: Core.Maybe [RouteFilterPrefix],
    -- | The ID of the Direct Connect gateway.
    directConnectGatewayId :: Core.Text,
    -- | The ID of the request proposal.
    proposalId :: Core.Text,
    -- | The ID of the AWS account that owns the virtual private gateway or
    -- transit gateway.
    associatedGatewayOwnerAccount :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
-- in the /AWS Direct Connect User Guide/.
--
-- 'directConnectGatewayId', 'acceptDirectConnectGatewayAssociationProposal_directConnectGatewayId' - The ID of the Direct Connect gateway.
--
-- 'proposalId', 'acceptDirectConnectGatewayAssociationProposal_proposalId' - The ID of the request proposal.
--
-- 'associatedGatewayOwnerAccount', 'acceptDirectConnectGatewayAssociationProposal_associatedGatewayOwnerAccount' - The ID of the AWS account that owns the virtual private gateway or
-- transit gateway.
newAcceptDirectConnectGatewayAssociationProposal ::
  -- | 'directConnectGatewayId'
  Core.Text ->
  -- | 'proposalId'
  Core.Text ->
  -- | 'associatedGatewayOwnerAccount'
  Core.Text ->
  AcceptDirectConnectGatewayAssociationProposal
newAcceptDirectConnectGatewayAssociationProposal
  pDirectConnectGatewayId_
  pProposalId_
  pAssociatedGatewayOwnerAccount_ =
    AcceptDirectConnectGatewayAssociationProposal'
      { overrideAllowedPrefixesToDirectConnectGateway =
          Core.Nothing,
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
-- in the /AWS Direct Connect User Guide/.
acceptDirectConnectGatewayAssociationProposal_overrideAllowedPrefixesToDirectConnectGateway :: Lens.Lens' AcceptDirectConnectGatewayAssociationProposal (Core.Maybe [RouteFilterPrefix])
acceptDirectConnectGatewayAssociationProposal_overrideAllowedPrefixesToDirectConnectGateway = Lens.lens (\AcceptDirectConnectGatewayAssociationProposal' {overrideAllowedPrefixesToDirectConnectGateway} -> overrideAllowedPrefixesToDirectConnectGateway) (\s@AcceptDirectConnectGatewayAssociationProposal' {} a -> s {overrideAllowedPrefixesToDirectConnectGateway = a} :: AcceptDirectConnectGatewayAssociationProposal) Core.. Lens.mapping Lens._Coerce

-- | The ID of the Direct Connect gateway.
acceptDirectConnectGatewayAssociationProposal_directConnectGatewayId :: Lens.Lens' AcceptDirectConnectGatewayAssociationProposal Core.Text
acceptDirectConnectGatewayAssociationProposal_directConnectGatewayId = Lens.lens (\AcceptDirectConnectGatewayAssociationProposal' {directConnectGatewayId} -> directConnectGatewayId) (\s@AcceptDirectConnectGatewayAssociationProposal' {} a -> s {directConnectGatewayId = a} :: AcceptDirectConnectGatewayAssociationProposal)

-- | The ID of the request proposal.
acceptDirectConnectGatewayAssociationProposal_proposalId :: Lens.Lens' AcceptDirectConnectGatewayAssociationProposal Core.Text
acceptDirectConnectGatewayAssociationProposal_proposalId = Lens.lens (\AcceptDirectConnectGatewayAssociationProposal' {proposalId} -> proposalId) (\s@AcceptDirectConnectGatewayAssociationProposal' {} a -> s {proposalId = a} :: AcceptDirectConnectGatewayAssociationProposal)

-- | The ID of the AWS account that owns the virtual private gateway or
-- transit gateway.
acceptDirectConnectGatewayAssociationProposal_associatedGatewayOwnerAccount :: Lens.Lens' AcceptDirectConnectGatewayAssociationProposal Core.Text
acceptDirectConnectGatewayAssociationProposal_associatedGatewayOwnerAccount = Lens.lens (\AcceptDirectConnectGatewayAssociationProposal' {associatedGatewayOwnerAccount} -> associatedGatewayOwnerAccount) (\s@AcceptDirectConnectGatewayAssociationProposal' {} a -> s {associatedGatewayOwnerAccount = a} :: AcceptDirectConnectGatewayAssociationProposal)

instance
  Core.AWSRequest
    AcceptDirectConnectGatewayAssociationProposal
  where
  type
    AWSResponse
      AcceptDirectConnectGatewayAssociationProposal =
      AcceptDirectConnectGatewayAssociationProposalResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          AcceptDirectConnectGatewayAssociationProposalResponse'
            Core.<$> (x Core..?> "directConnectGatewayAssociation")
              Core.<*> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    AcceptDirectConnectGatewayAssociationProposal

instance
  Core.NFData
    AcceptDirectConnectGatewayAssociationProposal

instance
  Core.ToHeaders
    AcceptDirectConnectGatewayAssociationProposal
  where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "OvertureService.AcceptDirectConnectGatewayAssociationProposal" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance
  Core.ToJSON
    AcceptDirectConnectGatewayAssociationProposal
  where
  toJSON
    AcceptDirectConnectGatewayAssociationProposal' {..} =
      Core.object
        ( Core.catMaybes
            [ ( "overrideAllowedPrefixesToDirectConnectGateway"
                  Core..=
              )
                Core.<$> overrideAllowedPrefixesToDirectConnectGateway,
              Core.Just
                ( "directConnectGatewayId"
                    Core..= directConnectGatewayId
                ),
              Core.Just ("proposalId" Core..= proposalId),
              Core.Just
                ( "associatedGatewayOwnerAccount"
                    Core..= associatedGatewayOwnerAccount
                )
            ]
        )

instance
  Core.ToPath
    AcceptDirectConnectGatewayAssociationProposal
  where
  toPath = Core.const "/"

instance
  Core.ToQuery
    AcceptDirectConnectGatewayAssociationProposal
  where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newAcceptDirectConnectGatewayAssociationProposalResponse' smart constructor.
data AcceptDirectConnectGatewayAssociationProposalResponse = AcceptDirectConnectGatewayAssociationProposalResponse'
  { directConnectGatewayAssociation :: Core.Maybe DirectConnectGatewayAssociation,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  AcceptDirectConnectGatewayAssociationProposalResponse
newAcceptDirectConnectGatewayAssociationProposalResponse
  pHttpStatus_ =
    AcceptDirectConnectGatewayAssociationProposalResponse'
      { directConnectGatewayAssociation =
          Core.Nothing,
        httpStatus =
          pHttpStatus_
      }

-- | Undocumented member.
acceptDirectConnectGatewayAssociationProposalResponse_directConnectGatewayAssociation :: Lens.Lens' AcceptDirectConnectGatewayAssociationProposalResponse (Core.Maybe DirectConnectGatewayAssociation)
acceptDirectConnectGatewayAssociationProposalResponse_directConnectGatewayAssociation = Lens.lens (\AcceptDirectConnectGatewayAssociationProposalResponse' {directConnectGatewayAssociation} -> directConnectGatewayAssociation) (\s@AcceptDirectConnectGatewayAssociationProposalResponse' {} a -> s {directConnectGatewayAssociation = a} :: AcceptDirectConnectGatewayAssociationProposalResponse)

-- | The response's http status code.
acceptDirectConnectGatewayAssociationProposalResponse_httpStatus :: Lens.Lens' AcceptDirectConnectGatewayAssociationProposalResponse Core.Int
acceptDirectConnectGatewayAssociationProposalResponse_httpStatus = Lens.lens (\AcceptDirectConnectGatewayAssociationProposalResponse' {httpStatus} -> httpStatus) (\s@AcceptDirectConnectGatewayAssociationProposalResponse' {} a -> s {httpStatus = a} :: AcceptDirectConnectGatewayAssociationProposalResponse)

instance
  Core.NFData
    AcceptDirectConnectGatewayAssociationProposalResponse
