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
-- Module      : Amazonka.ManagedBlockChain.GetProposal
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns detailed information about a proposal.
--
-- Applies only to Hyperledger Fabric.
module Amazonka.ManagedBlockChain.GetProposal
  ( -- * Creating a Request
    GetProposal (..),
    newGetProposal,

    -- * Request Lenses
    getProposal_networkId,
    getProposal_proposalId,

    -- * Destructuring the Response
    GetProposalResponse (..),
    newGetProposalResponse,

    -- * Response Lenses
    getProposalResponse_proposal,
    getProposalResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ManagedBlockChain.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetProposal' smart constructor.
data GetProposal = GetProposal'
  { -- | The unique identifier of the network for which the proposal is made.
    networkId :: Prelude.Text,
    -- | The unique identifier of the proposal.
    proposalId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetProposal' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'networkId', 'getProposal_networkId' - The unique identifier of the network for which the proposal is made.
--
-- 'proposalId', 'getProposal_proposalId' - The unique identifier of the proposal.
newGetProposal ::
  -- | 'networkId'
  Prelude.Text ->
  -- | 'proposalId'
  Prelude.Text ->
  GetProposal
newGetProposal pNetworkId_ pProposalId_ =
  GetProposal'
    { networkId = pNetworkId_,
      proposalId = pProposalId_
    }

-- | The unique identifier of the network for which the proposal is made.
getProposal_networkId :: Lens.Lens' GetProposal Prelude.Text
getProposal_networkId = Lens.lens (\GetProposal' {networkId} -> networkId) (\s@GetProposal' {} a -> s {networkId = a} :: GetProposal)

-- | The unique identifier of the proposal.
getProposal_proposalId :: Lens.Lens' GetProposal Prelude.Text
getProposal_proposalId = Lens.lens (\GetProposal' {proposalId} -> proposalId) (\s@GetProposal' {} a -> s {proposalId = a} :: GetProposal)

instance Core.AWSRequest GetProposal where
  type AWSResponse GetProposal = GetProposalResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetProposalResponse'
            Prelude.<$> (x Data..?> "Proposal")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetProposal where
  hashWithSalt _salt GetProposal' {..} =
    _salt
      `Prelude.hashWithSalt` networkId
      `Prelude.hashWithSalt` proposalId

instance Prelude.NFData GetProposal where
  rnf GetProposal' {..} =
    Prelude.rnf networkId
      `Prelude.seq` Prelude.rnf proposalId

instance Data.ToHeaders GetProposal where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetProposal where
  toPath GetProposal' {..} =
    Prelude.mconcat
      [ "/networks/",
        Data.toBS networkId,
        "/proposals/",
        Data.toBS proposalId
      ]

instance Data.ToQuery GetProposal where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetProposalResponse' smart constructor.
data GetProposalResponse = GetProposalResponse'
  { -- | Information about a proposal.
    proposal :: Prelude.Maybe Proposal,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetProposalResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'proposal', 'getProposalResponse_proposal' - Information about a proposal.
--
-- 'httpStatus', 'getProposalResponse_httpStatus' - The response's http status code.
newGetProposalResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetProposalResponse
newGetProposalResponse pHttpStatus_ =
  GetProposalResponse'
    { proposal = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about a proposal.
getProposalResponse_proposal :: Lens.Lens' GetProposalResponse (Prelude.Maybe Proposal)
getProposalResponse_proposal = Lens.lens (\GetProposalResponse' {proposal} -> proposal) (\s@GetProposalResponse' {} a -> s {proposal = a} :: GetProposalResponse)

-- | The response's http status code.
getProposalResponse_httpStatus :: Lens.Lens' GetProposalResponse Prelude.Int
getProposalResponse_httpStatus = Lens.lens (\GetProposalResponse' {httpStatus} -> httpStatus) (\s@GetProposalResponse' {} a -> s {httpStatus = a} :: GetProposalResponse)

instance Prelude.NFData GetProposalResponse where
  rnf GetProposalResponse' {..} =
    Prelude.rnf proposal
      `Prelude.seq` Prelude.rnf httpStatus
