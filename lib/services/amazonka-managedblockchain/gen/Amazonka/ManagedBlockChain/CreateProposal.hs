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
-- Module      : Amazonka.ManagedBlockChain.CreateProposal
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a proposal for a change to the network that other members of the
-- network can vote on, for example, a proposal to add a new member to the
-- network. Any member can create a proposal.
--
-- Applies only to Hyperledger Fabric.
module Amazonka.ManagedBlockChain.CreateProposal
  ( -- * Creating a Request
    CreateProposal (..),
    newCreateProposal,

    -- * Request Lenses
    createProposal_description,
    createProposal_tags,
    createProposal_clientRequestToken,
    createProposal_networkId,
    createProposal_memberId,
    createProposal_actions,

    -- * Destructuring the Response
    CreateProposalResponse (..),
    newCreateProposalResponse,

    -- * Response Lenses
    createProposalResponse_proposalId,
    createProposalResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ManagedBlockChain.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateProposal' smart constructor.
data CreateProposal = CreateProposal'
  { -- | A description for the proposal that is visible to voting members, for
    -- example, \"Proposal to add Example Corp. as member.\"
    description :: Prelude.Maybe Prelude.Text,
    -- | Tags to assign to the proposal.
    --
    -- Each tag consists of a key and an optional value. You can specify
    -- multiple key-value pairs in a single request with an overall maximum of
    -- 50 tags allowed per resource.
    --
    -- For more information about tags, see
    -- <https://docs.aws.amazon.com/managed-blockchain/latest/ethereum-dev/tagging-resources.html Tagging Resources>
    -- in the /Amazon Managed Blockchain Ethereum Developer Guide/, or
    -- <https://docs.aws.amazon.com/managed-blockchain/latest/hyperledger-fabric-dev/tagging-resources.html Tagging Resources>
    -- in the /Amazon Managed Blockchain Hyperledger Fabric Developer Guide/.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | A unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the operation. An idempotent operation completes no more
    -- than one time. This identifier is required only if you make a service
    -- request directly using an HTTP client. It is generated automatically if
    -- you use an Amazon Web Services SDK or the CLI.
    clientRequestToken :: Prelude.Text,
    -- | The unique identifier of the network for which the proposal is made.
    networkId :: Prelude.Text,
    -- | The unique identifier of the member that is creating the proposal. This
    -- identifier is especially useful for identifying the member making the
    -- proposal when multiple members exist in a single Amazon Web Services
    -- account.
    memberId :: Prelude.Text,
    -- | The type of actions proposed, such as inviting a member or removing a
    -- member. The types of @Actions@ in a proposal are mutually exclusive. For
    -- example, a proposal with @Invitations@ actions cannot also contain
    -- @Removals@ actions.
    actions :: ProposalActions
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateProposal' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'createProposal_description' - A description for the proposal that is visible to voting members, for
-- example, \"Proposal to add Example Corp. as member.\"
--
-- 'tags', 'createProposal_tags' - Tags to assign to the proposal.
--
-- Each tag consists of a key and an optional value. You can specify
-- multiple key-value pairs in a single request with an overall maximum of
-- 50 tags allowed per resource.
--
-- For more information about tags, see
-- <https://docs.aws.amazon.com/managed-blockchain/latest/ethereum-dev/tagging-resources.html Tagging Resources>
-- in the /Amazon Managed Blockchain Ethereum Developer Guide/, or
-- <https://docs.aws.amazon.com/managed-blockchain/latest/hyperledger-fabric-dev/tagging-resources.html Tagging Resources>
-- in the /Amazon Managed Blockchain Hyperledger Fabric Developer Guide/.
--
-- 'clientRequestToken', 'createProposal_clientRequestToken' - A unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the operation. An idempotent operation completes no more
-- than one time. This identifier is required only if you make a service
-- request directly using an HTTP client. It is generated automatically if
-- you use an Amazon Web Services SDK or the CLI.
--
-- 'networkId', 'createProposal_networkId' - The unique identifier of the network for which the proposal is made.
--
-- 'memberId', 'createProposal_memberId' - The unique identifier of the member that is creating the proposal. This
-- identifier is especially useful for identifying the member making the
-- proposal when multiple members exist in a single Amazon Web Services
-- account.
--
-- 'actions', 'createProposal_actions' - The type of actions proposed, such as inviting a member or removing a
-- member. The types of @Actions@ in a proposal are mutually exclusive. For
-- example, a proposal with @Invitations@ actions cannot also contain
-- @Removals@ actions.
newCreateProposal ::
  -- | 'clientRequestToken'
  Prelude.Text ->
  -- | 'networkId'
  Prelude.Text ->
  -- | 'memberId'
  Prelude.Text ->
  -- | 'actions'
  ProposalActions ->
  CreateProposal
newCreateProposal
  pClientRequestToken_
  pNetworkId_
  pMemberId_
  pActions_ =
    CreateProposal'
      { description = Prelude.Nothing,
        tags = Prelude.Nothing,
        clientRequestToken = pClientRequestToken_,
        networkId = pNetworkId_,
        memberId = pMemberId_,
        actions = pActions_
      }

-- | A description for the proposal that is visible to voting members, for
-- example, \"Proposal to add Example Corp. as member.\"
createProposal_description :: Lens.Lens' CreateProposal (Prelude.Maybe Prelude.Text)
createProposal_description = Lens.lens (\CreateProposal' {description} -> description) (\s@CreateProposal' {} a -> s {description = a} :: CreateProposal)

-- | Tags to assign to the proposal.
--
-- Each tag consists of a key and an optional value. You can specify
-- multiple key-value pairs in a single request with an overall maximum of
-- 50 tags allowed per resource.
--
-- For more information about tags, see
-- <https://docs.aws.amazon.com/managed-blockchain/latest/ethereum-dev/tagging-resources.html Tagging Resources>
-- in the /Amazon Managed Blockchain Ethereum Developer Guide/, or
-- <https://docs.aws.amazon.com/managed-blockchain/latest/hyperledger-fabric-dev/tagging-resources.html Tagging Resources>
-- in the /Amazon Managed Blockchain Hyperledger Fabric Developer Guide/.
createProposal_tags :: Lens.Lens' CreateProposal (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createProposal_tags = Lens.lens (\CreateProposal' {tags} -> tags) (\s@CreateProposal' {} a -> s {tags = a} :: CreateProposal) Prelude.. Lens.mapping Lens.coerced

-- | A unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the operation. An idempotent operation completes no more
-- than one time. This identifier is required only if you make a service
-- request directly using an HTTP client. It is generated automatically if
-- you use an Amazon Web Services SDK or the CLI.
createProposal_clientRequestToken :: Lens.Lens' CreateProposal Prelude.Text
createProposal_clientRequestToken = Lens.lens (\CreateProposal' {clientRequestToken} -> clientRequestToken) (\s@CreateProposal' {} a -> s {clientRequestToken = a} :: CreateProposal)

-- | The unique identifier of the network for which the proposal is made.
createProposal_networkId :: Lens.Lens' CreateProposal Prelude.Text
createProposal_networkId = Lens.lens (\CreateProposal' {networkId} -> networkId) (\s@CreateProposal' {} a -> s {networkId = a} :: CreateProposal)

-- | The unique identifier of the member that is creating the proposal. This
-- identifier is especially useful for identifying the member making the
-- proposal when multiple members exist in a single Amazon Web Services
-- account.
createProposal_memberId :: Lens.Lens' CreateProposal Prelude.Text
createProposal_memberId = Lens.lens (\CreateProposal' {memberId} -> memberId) (\s@CreateProposal' {} a -> s {memberId = a} :: CreateProposal)

-- | The type of actions proposed, such as inviting a member or removing a
-- member. The types of @Actions@ in a proposal are mutually exclusive. For
-- example, a proposal with @Invitations@ actions cannot also contain
-- @Removals@ actions.
createProposal_actions :: Lens.Lens' CreateProposal ProposalActions
createProposal_actions = Lens.lens (\CreateProposal' {actions} -> actions) (\s@CreateProposal' {} a -> s {actions = a} :: CreateProposal)

instance Core.AWSRequest CreateProposal where
  type
    AWSResponse CreateProposal =
      CreateProposalResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateProposalResponse'
            Prelude.<$> (x Data..?> "ProposalId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateProposal where
  hashWithSalt _salt CreateProposal' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` clientRequestToken
      `Prelude.hashWithSalt` networkId
      `Prelude.hashWithSalt` memberId
      `Prelude.hashWithSalt` actions

instance Prelude.NFData CreateProposal where
  rnf CreateProposal' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf clientRequestToken
      `Prelude.seq` Prelude.rnf networkId
      `Prelude.seq` Prelude.rnf memberId
      `Prelude.seq` Prelude.rnf actions

instance Data.ToHeaders CreateProposal where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateProposal where
  toJSON CreateProposal' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Description" Data..=) Prelude.<$> description,
            ("Tags" Data..=) Prelude.<$> tags,
            Prelude.Just
              ("ClientRequestToken" Data..= clientRequestToken),
            Prelude.Just ("MemberId" Data..= memberId),
            Prelude.Just ("Actions" Data..= actions)
          ]
      )

instance Data.ToPath CreateProposal where
  toPath CreateProposal' {..} =
    Prelude.mconcat
      ["/networks/", Data.toBS networkId, "/proposals"]

instance Data.ToQuery CreateProposal where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateProposalResponse' smart constructor.
data CreateProposalResponse = CreateProposalResponse'
  { -- | The unique identifier of the proposal.
    proposalId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateProposalResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'proposalId', 'createProposalResponse_proposalId' - The unique identifier of the proposal.
--
-- 'httpStatus', 'createProposalResponse_httpStatus' - The response's http status code.
newCreateProposalResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateProposalResponse
newCreateProposalResponse pHttpStatus_ =
  CreateProposalResponse'
    { proposalId =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The unique identifier of the proposal.
createProposalResponse_proposalId :: Lens.Lens' CreateProposalResponse (Prelude.Maybe Prelude.Text)
createProposalResponse_proposalId = Lens.lens (\CreateProposalResponse' {proposalId} -> proposalId) (\s@CreateProposalResponse' {} a -> s {proposalId = a} :: CreateProposalResponse)

-- | The response's http status code.
createProposalResponse_httpStatus :: Lens.Lens' CreateProposalResponse Prelude.Int
createProposalResponse_httpStatus = Lens.lens (\CreateProposalResponse' {httpStatus} -> httpStatus) (\s@CreateProposalResponse' {} a -> s {httpStatus = a} :: CreateProposalResponse)

instance Prelude.NFData CreateProposalResponse where
  rnf CreateProposalResponse' {..} =
    Prelude.rnf proposalId
      `Prelude.seq` Prelude.rnf httpStatus
