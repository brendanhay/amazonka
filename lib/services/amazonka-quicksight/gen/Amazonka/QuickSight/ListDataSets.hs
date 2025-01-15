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
-- Module      : Amazonka.QuickSight.ListDataSets
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all of the datasets belonging to the current Amazon Web Services
-- account in an Amazon Web Services Region.
--
-- The permissions resource is
-- @arn:aws:quicksight:region:aws-account-id:dataset\/*@.
--
-- This operation returns paginated results.
module Amazonka.QuickSight.ListDataSets
  ( -- * Creating a Request
    ListDataSets (..),
    newListDataSets,

    -- * Request Lenses
    listDataSets_maxResults,
    listDataSets_nextToken,
    listDataSets_awsAccountId,

    -- * Destructuring the Response
    ListDataSetsResponse (..),
    newListDataSetsResponse,

    -- * Response Lenses
    listDataSetsResponse_dataSetSummaries,
    listDataSetsResponse_nextToken,
    listDataSetsResponse_requestId,
    listDataSetsResponse_status,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListDataSets' smart constructor.
data ListDataSets = ListDataSets'
  { -- | The maximum number of results to be returned per request.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token for the next set of results, or null if there are no more
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services account ID.
    awsAccountId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListDataSets' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listDataSets_maxResults' - The maximum number of results to be returned per request.
--
-- 'nextToken', 'listDataSets_nextToken' - The token for the next set of results, or null if there are no more
-- results.
--
-- 'awsAccountId', 'listDataSets_awsAccountId' - The Amazon Web Services account ID.
newListDataSets ::
  -- | 'awsAccountId'
  Prelude.Text ->
  ListDataSets
newListDataSets pAwsAccountId_ =
  ListDataSets'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      awsAccountId = pAwsAccountId_
    }

-- | The maximum number of results to be returned per request.
listDataSets_maxResults :: Lens.Lens' ListDataSets (Prelude.Maybe Prelude.Natural)
listDataSets_maxResults = Lens.lens (\ListDataSets' {maxResults} -> maxResults) (\s@ListDataSets' {} a -> s {maxResults = a} :: ListDataSets)

-- | The token for the next set of results, or null if there are no more
-- results.
listDataSets_nextToken :: Lens.Lens' ListDataSets (Prelude.Maybe Prelude.Text)
listDataSets_nextToken = Lens.lens (\ListDataSets' {nextToken} -> nextToken) (\s@ListDataSets' {} a -> s {nextToken = a} :: ListDataSets)

-- | The Amazon Web Services account ID.
listDataSets_awsAccountId :: Lens.Lens' ListDataSets Prelude.Text
listDataSets_awsAccountId = Lens.lens (\ListDataSets' {awsAccountId} -> awsAccountId) (\s@ListDataSets' {} a -> s {awsAccountId = a} :: ListDataSets)

instance Core.AWSPager ListDataSets where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listDataSetsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listDataSetsResponse_dataSetSummaries
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just Prelude.$
          rq
            Prelude.& listDataSets_nextToken
              Lens..~ rs
              Lens.^? listDataSetsResponse_nextToken
              Prelude.. Lens._Just

instance Core.AWSRequest ListDataSets where
  type AWSResponse ListDataSets = ListDataSetsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListDataSetsResponse'
            Prelude.<$> ( x
                            Data..?> "DataSetSummaries"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "RequestId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListDataSets where
  hashWithSalt _salt ListDataSets' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` awsAccountId

instance Prelude.NFData ListDataSets where
  rnf ListDataSets' {..} =
    Prelude.rnf maxResults `Prelude.seq`
      Prelude.rnf nextToken `Prelude.seq`
        Prelude.rnf awsAccountId

instance Data.ToHeaders ListDataSets where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListDataSets where
  toPath ListDataSets' {..} =
    Prelude.mconcat
      ["/accounts/", Data.toBS awsAccountId, "/data-sets"]

instance Data.ToQuery ListDataSets where
  toQuery ListDataSets' {..} =
    Prelude.mconcat
      [ "max-results" Data.=: maxResults,
        "next-token" Data.=: nextToken
      ]

-- | /See:/ 'newListDataSetsResponse' smart constructor.
data ListDataSetsResponse = ListDataSetsResponse'
  { -- | The list of dataset summaries.
    dataSetSummaries :: Prelude.Maybe [DataSetSummary],
    -- | The token for the next set of results, or null if there are no more
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services request ID for this operation.
    requestId :: Prelude.Maybe Prelude.Text,
    -- | The HTTP status of the request.
    status :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListDataSetsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dataSetSummaries', 'listDataSetsResponse_dataSetSummaries' - The list of dataset summaries.
--
-- 'nextToken', 'listDataSetsResponse_nextToken' - The token for the next set of results, or null if there are no more
-- results.
--
-- 'requestId', 'listDataSetsResponse_requestId' - The Amazon Web Services request ID for this operation.
--
-- 'status', 'listDataSetsResponse_status' - The HTTP status of the request.
newListDataSetsResponse ::
  -- | 'status'
  Prelude.Int ->
  ListDataSetsResponse
newListDataSetsResponse pStatus_ =
  ListDataSetsResponse'
    { dataSetSummaries =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      requestId = Prelude.Nothing,
      status = pStatus_
    }

-- | The list of dataset summaries.
listDataSetsResponse_dataSetSummaries :: Lens.Lens' ListDataSetsResponse (Prelude.Maybe [DataSetSummary])
listDataSetsResponse_dataSetSummaries = Lens.lens (\ListDataSetsResponse' {dataSetSummaries} -> dataSetSummaries) (\s@ListDataSetsResponse' {} a -> s {dataSetSummaries = a} :: ListDataSetsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token for the next set of results, or null if there are no more
-- results.
listDataSetsResponse_nextToken :: Lens.Lens' ListDataSetsResponse (Prelude.Maybe Prelude.Text)
listDataSetsResponse_nextToken = Lens.lens (\ListDataSetsResponse' {nextToken} -> nextToken) (\s@ListDataSetsResponse' {} a -> s {nextToken = a} :: ListDataSetsResponse)

-- | The Amazon Web Services request ID for this operation.
listDataSetsResponse_requestId :: Lens.Lens' ListDataSetsResponse (Prelude.Maybe Prelude.Text)
listDataSetsResponse_requestId = Lens.lens (\ListDataSetsResponse' {requestId} -> requestId) (\s@ListDataSetsResponse' {} a -> s {requestId = a} :: ListDataSetsResponse)

-- | The HTTP status of the request.
listDataSetsResponse_status :: Lens.Lens' ListDataSetsResponse Prelude.Int
listDataSetsResponse_status = Lens.lens (\ListDataSetsResponse' {status} -> status) (\s@ListDataSetsResponse' {} a -> s {status = a} :: ListDataSetsResponse)

instance Prelude.NFData ListDataSetsResponse where
  rnf ListDataSetsResponse' {..} =
    Prelude.rnf dataSetSummaries `Prelude.seq`
      Prelude.rnf nextToken `Prelude.seq`
        Prelude.rnf requestId `Prelude.seq`
          Prelude.rnf status
