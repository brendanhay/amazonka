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
-- Module      : Amazonka.QuickSight.ListIngestions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the history of SPICE ingestions for a dataset.
--
-- This operation returns paginated results.
module Amazonka.QuickSight.ListIngestions
  ( -- * Creating a Request
    ListIngestions (..),
    newListIngestions,

    -- * Request Lenses
    listIngestions_maxResults,
    listIngestions_nextToken,
    listIngestions_dataSetId,
    listIngestions_awsAccountId,

    -- * Destructuring the Response
    ListIngestionsResponse (..),
    newListIngestionsResponse,

    -- * Response Lenses
    listIngestionsResponse_ingestions,
    listIngestionsResponse_nextToken,
    listIngestionsResponse_requestId,
    listIngestionsResponse_status,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListIngestions' smart constructor.
data ListIngestions = ListIngestions'
  { -- | The maximum number of results to be returned per request.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token for the next set of results, or null if there are no more
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The ID of the dataset used in the ingestion.
    dataSetId :: Prelude.Text,
    -- | The Amazon Web Services account ID.
    awsAccountId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListIngestions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listIngestions_maxResults' - The maximum number of results to be returned per request.
--
-- 'nextToken', 'listIngestions_nextToken' - The token for the next set of results, or null if there are no more
-- results.
--
-- 'dataSetId', 'listIngestions_dataSetId' - The ID of the dataset used in the ingestion.
--
-- 'awsAccountId', 'listIngestions_awsAccountId' - The Amazon Web Services account ID.
newListIngestions ::
  -- | 'dataSetId'
  Prelude.Text ->
  -- | 'awsAccountId'
  Prelude.Text ->
  ListIngestions
newListIngestions pDataSetId_ pAwsAccountId_ =
  ListIngestions'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      dataSetId = pDataSetId_,
      awsAccountId = pAwsAccountId_
    }

-- | The maximum number of results to be returned per request.
listIngestions_maxResults :: Lens.Lens' ListIngestions (Prelude.Maybe Prelude.Natural)
listIngestions_maxResults = Lens.lens (\ListIngestions' {maxResults} -> maxResults) (\s@ListIngestions' {} a -> s {maxResults = a} :: ListIngestions)

-- | The token for the next set of results, or null if there are no more
-- results.
listIngestions_nextToken :: Lens.Lens' ListIngestions (Prelude.Maybe Prelude.Text)
listIngestions_nextToken = Lens.lens (\ListIngestions' {nextToken} -> nextToken) (\s@ListIngestions' {} a -> s {nextToken = a} :: ListIngestions)

-- | The ID of the dataset used in the ingestion.
listIngestions_dataSetId :: Lens.Lens' ListIngestions Prelude.Text
listIngestions_dataSetId = Lens.lens (\ListIngestions' {dataSetId} -> dataSetId) (\s@ListIngestions' {} a -> s {dataSetId = a} :: ListIngestions)

-- | The Amazon Web Services account ID.
listIngestions_awsAccountId :: Lens.Lens' ListIngestions Prelude.Text
listIngestions_awsAccountId = Lens.lens (\ListIngestions' {awsAccountId} -> awsAccountId) (\s@ListIngestions' {} a -> s {awsAccountId = a} :: ListIngestions)

instance Core.AWSPager ListIngestions where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listIngestionsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listIngestionsResponse_ingestions
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just Prelude.$
          rq
            Prelude.& listIngestions_nextToken
              Lens..~ rs
              Lens.^? listIngestionsResponse_nextToken
              Prelude.. Lens._Just

instance Core.AWSRequest ListIngestions where
  type
    AWSResponse ListIngestions =
      ListIngestionsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListIngestionsResponse'
            Prelude.<$> (x Data..?> "Ingestions" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "RequestId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListIngestions where
  hashWithSalt _salt ListIngestions' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` dataSetId
      `Prelude.hashWithSalt` awsAccountId

instance Prelude.NFData ListIngestions where
  rnf ListIngestions' {..} =
    Prelude.rnf maxResults `Prelude.seq`
      Prelude.rnf nextToken `Prelude.seq`
        Prelude.rnf dataSetId `Prelude.seq`
          Prelude.rnf awsAccountId

instance Data.ToHeaders ListIngestions where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListIngestions where
  toPath ListIngestions' {..} =
    Prelude.mconcat
      [ "/accounts/",
        Data.toBS awsAccountId,
        "/data-sets/",
        Data.toBS dataSetId,
        "/ingestions"
      ]

instance Data.ToQuery ListIngestions where
  toQuery ListIngestions' {..} =
    Prelude.mconcat
      [ "max-results" Data.=: maxResults,
        "next-token" Data.=: nextToken
      ]

-- | /See:/ 'newListIngestionsResponse' smart constructor.
data ListIngestionsResponse = ListIngestionsResponse'
  { -- | A list of the ingestions.
    ingestions :: Prelude.Maybe [Ingestion],
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
-- Create a value of 'ListIngestionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ingestions', 'listIngestionsResponse_ingestions' - A list of the ingestions.
--
-- 'nextToken', 'listIngestionsResponse_nextToken' - The token for the next set of results, or null if there are no more
-- results.
--
-- 'requestId', 'listIngestionsResponse_requestId' - The Amazon Web Services request ID for this operation.
--
-- 'status', 'listIngestionsResponse_status' - The HTTP status of the request.
newListIngestionsResponse ::
  -- | 'status'
  Prelude.Int ->
  ListIngestionsResponse
newListIngestionsResponse pStatus_ =
  ListIngestionsResponse'
    { ingestions =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      requestId = Prelude.Nothing,
      status = pStatus_
    }

-- | A list of the ingestions.
listIngestionsResponse_ingestions :: Lens.Lens' ListIngestionsResponse (Prelude.Maybe [Ingestion])
listIngestionsResponse_ingestions = Lens.lens (\ListIngestionsResponse' {ingestions} -> ingestions) (\s@ListIngestionsResponse' {} a -> s {ingestions = a} :: ListIngestionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token for the next set of results, or null if there are no more
-- results.
listIngestionsResponse_nextToken :: Lens.Lens' ListIngestionsResponse (Prelude.Maybe Prelude.Text)
listIngestionsResponse_nextToken = Lens.lens (\ListIngestionsResponse' {nextToken} -> nextToken) (\s@ListIngestionsResponse' {} a -> s {nextToken = a} :: ListIngestionsResponse)

-- | The Amazon Web Services request ID for this operation.
listIngestionsResponse_requestId :: Lens.Lens' ListIngestionsResponse (Prelude.Maybe Prelude.Text)
listIngestionsResponse_requestId = Lens.lens (\ListIngestionsResponse' {requestId} -> requestId) (\s@ListIngestionsResponse' {} a -> s {requestId = a} :: ListIngestionsResponse)

-- | The HTTP status of the request.
listIngestionsResponse_status :: Lens.Lens' ListIngestionsResponse Prelude.Int
listIngestionsResponse_status = Lens.lens (\ListIngestionsResponse' {status} -> status) (\s@ListIngestionsResponse' {} a -> s {status = a} :: ListIngestionsResponse)

instance Prelude.NFData ListIngestionsResponse where
  rnf ListIngestionsResponse' {..} =
    Prelude.rnf ingestions `Prelude.seq`
      Prelude.rnf nextToken `Prelude.seq`
        Prelude.rnf requestId `Prelude.seq`
          Prelude.rnf status
