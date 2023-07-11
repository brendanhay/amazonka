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
-- Module      : Amazonka.ManagedBlockChain.ListProposalVotes
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the list of votes for a specified proposal, including the value
-- of each vote and the unique identifier of the member that cast the vote.
--
-- Applies only to Hyperledger Fabric.
module Amazonka.ManagedBlockChain.ListProposalVotes
  ( -- * Creating a Request
    ListProposalVotes (..),
    newListProposalVotes,

    -- * Request Lenses
    listProposalVotes_maxResults,
    listProposalVotes_nextToken,
    listProposalVotes_networkId,
    listProposalVotes_proposalId,

    -- * Destructuring the Response
    ListProposalVotesResponse (..),
    newListProposalVotesResponse,

    -- * Response Lenses
    listProposalVotesResponse_nextToken,
    listProposalVotesResponse_proposalVotes,
    listProposalVotesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ManagedBlockChain.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListProposalVotes' smart constructor.
data ListProposalVotes = ListProposalVotes'
  { -- | The maximum number of votes to return.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The pagination token that indicates the next set of results to retrieve.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier of the network.
    networkId :: Prelude.Text,
    -- | The unique identifier of the proposal.
    proposalId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListProposalVotes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listProposalVotes_maxResults' - The maximum number of votes to return.
--
-- 'nextToken', 'listProposalVotes_nextToken' - The pagination token that indicates the next set of results to retrieve.
--
-- 'networkId', 'listProposalVotes_networkId' - The unique identifier of the network.
--
-- 'proposalId', 'listProposalVotes_proposalId' - The unique identifier of the proposal.
newListProposalVotes ::
  -- | 'networkId'
  Prelude.Text ->
  -- | 'proposalId'
  Prelude.Text ->
  ListProposalVotes
newListProposalVotes pNetworkId_ pProposalId_ =
  ListProposalVotes'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      networkId = pNetworkId_,
      proposalId = pProposalId_
    }

-- | The maximum number of votes to return.
listProposalVotes_maxResults :: Lens.Lens' ListProposalVotes (Prelude.Maybe Prelude.Natural)
listProposalVotes_maxResults = Lens.lens (\ListProposalVotes' {maxResults} -> maxResults) (\s@ListProposalVotes' {} a -> s {maxResults = a} :: ListProposalVotes)

-- | The pagination token that indicates the next set of results to retrieve.
listProposalVotes_nextToken :: Lens.Lens' ListProposalVotes (Prelude.Maybe Prelude.Text)
listProposalVotes_nextToken = Lens.lens (\ListProposalVotes' {nextToken} -> nextToken) (\s@ListProposalVotes' {} a -> s {nextToken = a} :: ListProposalVotes)

-- | The unique identifier of the network.
listProposalVotes_networkId :: Lens.Lens' ListProposalVotes Prelude.Text
listProposalVotes_networkId = Lens.lens (\ListProposalVotes' {networkId} -> networkId) (\s@ListProposalVotes' {} a -> s {networkId = a} :: ListProposalVotes)

-- | The unique identifier of the proposal.
listProposalVotes_proposalId :: Lens.Lens' ListProposalVotes Prelude.Text
listProposalVotes_proposalId = Lens.lens (\ListProposalVotes' {proposalId} -> proposalId) (\s@ListProposalVotes' {} a -> s {proposalId = a} :: ListProposalVotes)

instance Core.AWSRequest ListProposalVotes where
  type
    AWSResponse ListProposalVotes =
      ListProposalVotesResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListProposalVotesResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "ProposalVotes" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListProposalVotes where
  hashWithSalt _salt ListProposalVotes' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` networkId
      `Prelude.hashWithSalt` proposalId

instance Prelude.NFData ListProposalVotes where
  rnf ListProposalVotes' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf networkId
      `Prelude.seq` Prelude.rnf proposalId

instance Data.ToHeaders ListProposalVotes where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListProposalVotes where
  toPath ListProposalVotes' {..} =
    Prelude.mconcat
      [ "/networks/",
        Data.toBS networkId,
        "/proposals/",
        Data.toBS proposalId,
        "/votes"
      ]

instance Data.ToQuery ListProposalVotes where
  toQuery ListProposalVotes' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken
      ]

-- | /See:/ 'newListProposalVotesResponse' smart constructor.
data ListProposalVotesResponse = ListProposalVotesResponse'
  { -- | The pagination token that indicates the next set of results to retrieve.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The list of votes.
    proposalVotes :: Prelude.Maybe [VoteSummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListProposalVotesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listProposalVotesResponse_nextToken' - The pagination token that indicates the next set of results to retrieve.
--
-- 'proposalVotes', 'listProposalVotesResponse_proposalVotes' - The list of votes.
--
-- 'httpStatus', 'listProposalVotesResponse_httpStatus' - The response's http status code.
newListProposalVotesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListProposalVotesResponse
newListProposalVotesResponse pHttpStatus_ =
  ListProposalVotesResponse'
    { nextToken =
        Prelude.Nothing,
      proposalVotes = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The pagination token that indicates the next set of results to retrieve.
listProposalVotesResponse_nextToken :: Lens.Lens' ListProposalVotesResponse (Prelude.Maybe Prelude.Text)
listProposalVotesResponse_nextToken = Lens.lens (\ListProposalVotesResponse' {nextToken} -> nextToken) (\s@ListProposalVotesResponse' {} a -> s {nextToken = a} :: ListProposalVotesResponse)

-- | The list of votes.
listProposalVotesResponse_proposalVotes :: Lens.Lens' ListProposalVotesResponse (Prelude.Maybe [VoteSummary])
listProposalVotesResponse_proposalVotes = Lens.lens (\ListProposalVotesResponse' {proposalVotes} -> proposalVotes) (\s@ListProposalVotesResponse' {} a -> s {proposalVotes = a} :: ListProposalVotesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listProposalVotesResponse_httpStatus :: Lens.Lens' ListProposalVotesResponse Prelude.Int
listProposalVotesResponse_httpStatus = Lens.lens (\ListProposalVotesResponse' {httpStatus} -> httpStatus) (\s@ListProposalVotesResponse' {} a -> s {httpStatus = a} :: ListProposalVotesResponse)

instance Prelude.NFData ListProposalVotesResponse where
  rnf ListProposalVotesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf proposalVotes
      `Prelude.seq` Prelude.rnf httpStatus
