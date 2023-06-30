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
-- Module      : Amazonka.ManagedBlockChain.ListProposals
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of proposals for the network.
--
-- Applies only to Hyperledger Fabric.
module Amazonka.ManagedBlockChain.ListProposals
  ( -- * Creating a Request
    ListProposals (..),
    newListProposals,

    -- * Request Lenses
    listProposals_maxResults,
    listProposals_nextToken,
    listProposals_networkId,

    -- * Destructuring the Response
    ListProposalsResponse (..),
    newListProposalsResponse,

    -- * Response Lenses
    listProposalsResponse_nextToken,
    listProposalsResponse_proposals,
    listProposalsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ManagedBlockChain.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListProposals' smart constructor.
data ListProposals = ListProposals'
  { -- | The maximum number of proposals to return.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The pagination token that indicates the next set of results to retrieve.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier of the network.
    networkId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListProposals' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listProposals_maxResults' - The maximum number of proposals to return.
--
-- 'nextToken', 'listProposals_nextToken' - The pagination token that indicates the next set of results to retrieve.
--
-- 'networkId', 'listProposals_networkId' - The unique identifier of the network.
newListProposals ::
  -- | 'networkId'
  Prelude.Text ->
  ListProposals
newListProposals pNetworkId_ =
  ListProposals'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      networkId = pNetworkId_
    }

-- | The maximum number of proposals to return.
listProposals_maxResults :: Lens.Lens' ListProposals (Prelude.Maybe Prelude.Natural)
listProposals_maxResults = Lens.lens (\ListProposals' {maxResults} -> maxResults) (\s@ListProposals' {} a -> s {maxResults = a} :: ListProposals)

-- | The pagination token that indicates the next set of results to retrieve.
listProposals_nextToken :: Lens.Lens' ListProposals (Prelude.Maybe Prelude.Text)
listProposals_nextToken = Lens.lens (\ListProposals' {nextToken} -> nextToken) (\s@ListProposals' {} a -> s {nextToken = a} :: ListProposals)

-- | The unique identifier of the network.
listProposals_networkId :: Lens.Lens' ListProposals Prelude.Text
listProposals_networkId = Lens.lens (\ListProposals' {networkId} -> networkId) (\s@ListProposals' {} a -> s {networkId = a} :: ListProposals)

instance Core.AWSRequest ListProposals where
  type
    AWSResponse ListProposals =
      ListProposalsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListProposalsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "Proposals" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListProposals where
  hashWithSalt _salt ListProposals' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` networkId

instance Prelude.NFData ListProposals where
  rnf ListProposals' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf networkId

instance Data.ToHeaders ListProposals where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListProposals where
  toPath ListProposals' {..} =
    Prelude.mconcat
      ["/networks/", Data.toBS networkId, "/proposals"]

instance Data.ToQuery ListProposals where
  toQuery ListProposals' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken
      ]

-- | /See:/ 'newListProposalsResponse' smart constructor.
data ListProposalsResponse = ListProposalsResponse'
  { -- | The pagination token that indicates the next set of results to retrieve.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The summary of each proposal made on the network.
    proposals :: Prelude.Maybe [ProposalSummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListProposalsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listProposalsResponse_nextToken' - The pagination token that indicates the next set of results to retrieve.
--
-- 'proposals', 'listProposalsResponse_proposals' - The summary of each proposal made on the network.
--
-- 'httpStatus', 'listProposalsResponse_httpStatus' - The response's http status code.
newListProposalsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListProposalsResponse
newListProposalsResponse pHttpStatus_ =
  ListProposalsResponse'
    { nextToken = Prelude.Nothing,
      proposals = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The pagination token that indicates the next set of results to retrieve.
listProposalsResponse_nextToken :: Lens.Lens' ListProposalsResponse (Prelude.Maybe Prelude.Text)
listProposalsResponse_nextToken = Lens.lens (\ListProposalsResponse' {nextToken} -> nextToken) (\s@ListProposalsResponse' {} a -> s {nextToken = a} :: ListProposalsResponse)

-- | The summary of each proposal made on the network.
listProposalsResponse_proposals :: Lens.Lens' ListProposalsResponse (Prelude.Maybe [ProposalSummary])
listProposalsResponse_proposals = Lens.lens (\ListProposalsResponse' {proposals} -> proposals) (\s@ListProposalsResponse' {} a -> s {proposals = a} :: ListProposalsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listProposalsResponse_httpStatus :: Lens.Lens' ListProposalsResponse Prelude.Int
listProposalsResponse_httpStatus = Lens.lens (\ListProposalsResponse' {httpStatus} -> httpStatus) (\s@ListProposalsResponse' {} a -> s {httpStatus = a} :: ListProposalsResponse)

instance Prelude.NFData ListProposalsResponse where
  rnf ListProposalsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf proposals
      `Prelude.seq` Prelude.rnf httpStatus
