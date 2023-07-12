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
-- Module      : Amazonka.QuickSight.ListDataSources
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists data sources in current Amazon Web Services Region that belong to
-- this Amazon Web Services account.
--
-- This operation returns paginated results.
module Amazonka.QuickSight.ListDataSources
  ( -- * Creating a Request
    ListDataSources (..),
    newListDataSources,

    -- * Request Lenses
    listDataSources_maxResults,
    listDataSources_nextToken,
    listDataSources_awsAccountId,

    -- * Destructuring the Response
    ListDataSourcesResponse (..),
    newListDataSourcesResponse,

    -- * Response Lenses
    listDataSourcesResponse_dataSources,
    listDataSourcesResponse_nextToken,
    listDataSourcesResponse_requestId,
    listDataSourcesResponse_status,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListDataSources' smart constructor.
data ListDataSources = ListDataSources'
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
-- Create a value of 'ListDataSources' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listDataSources_maxResults' - The maximum number of results to be returned per request.
--
-- 'nextToken', 'listDataSources_nextToken' - The token for the next set of results, or null if there are no more
-- results.
--
-- 'awsAccountId', 'listDataSources_awsAccountId' - The Amazon Web Services account ID.
newListDataSources ::
  -- | 'awsAccountId'
  Prelude.Text ->
  ListDataSources
newListDataSources pAwsAccountId_ =
  ListDataSources'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      awsAccountId = pAwsAccountId_
    }

-- | The maximum number of results to be returned per request.
listDataSources_maxResults :: Lens.Lens' ListDataSources (Prelude.Maybe Prelude.Natural)
listDataSources_maxResults = Lens.lens (\ListDataSources' {maxResults} -> maxResults) (\s@ListDataSources' {} a -> s {maxResults = a} :: ListDataSources)

-- | The token for the next set of results, or null if there are no more
-- results.
listDataSources_nextToken :: Lens.Lens' ListDataSources (Prelude.Maybe Prelude.Text)
listDataSources_nextToken = Lens.lens (\ListDataSources' {nextToken} -> nextToken) (\s@ListDataSources' {} a -> s {nextToken = a} :: ListDataSources)

-- | The Amazon Web Services account ID.
listDataSources_awsAccountId :: Lens.Lens' ListDataSources Prelude.Text
listDataSources_awsAccountId = Lens.lens (\ListDataSources' {awsAccountId} -> awsAccountId) (\s@ListDataSources' {} a -> s {awsAccountId = a} :: ListDataSources)

instance Core.AWSPager ListDataSources where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listDataSourcesResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listDataSourcesResponse_dataSources
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listDataSources_nextToken
          Lens..~ rs
          Lens.^? listDataSourcesResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListDataSources where
  type
    AWSResponse ListDataSources =
      ListDataSourcesResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListDataSourcesResponse'
            Prelude.<$> (x Data..?> "DataSources" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "RequestId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListDataSources where
  hashWithSalt _salt ListDataSources' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` awsAccountId

instance Prelude.NFData ListDataSources where
  rnf ListDataSources' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf awsAccountId

instance Data.ToHeaders ListDataSources where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListDataSources where
  toPath ListDataSources' {..} =
    Prelude.mconcat
      [ "/accounts/",
        Data.toBS awsAccountId,
        "/data-sources"
      ]

instance Data.ToQuery ListDataSources where
  toQuery ListDataSources' {..} =
    Prelude.mconcat
      [ "max-results" Data.=: maxResults,
        "next-token" Data.=: nextToken
      ]

-- | /See:/ 'newListDataSourcesResponse' smart constructor.
data ListDataSourcesResponse = ListDataSourcesResponse'
  { -- | A list of data sources.
    dataSources :: Prelude.Maybe [DataSource],
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
-- Create a value of 'ListDataSourcesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dataSources', 'listDataSourcesResponse_dataSources' - A list of data sources.
--
-- 'nextToken', 'listDataSourcesResponse_nextToken' - The token for the next set of results, or null if there are no more
-- results.
--
-- 'requestId', 'listDataSourcesResponse_requestId' - The Amazon Web Services request ID for this operation.
--
-- 'status', 'listDataSourcesResponse_status' - The HTTP status of the request.
newListDataSourcesResponse ::
  -- | 'status'
  Prelude.Int ->
  ListDataSourcesResponse
newListDataSourcesResponse pStatus_ =
  ListDataSourcesResponse'
    { dataSources =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      requestId = Prelude.Nothing,
      status = pStatus_
    }

-- | A list of data sources.
listDataSourcesResponse_dataSources :: Lens.Lens' ListDataSourcesResponse (Prelude.Maybe [DataSource])
listDataSourcesResponse_dataSources = Lens.lens (\ListDataSourcesResponse' {dataSources} -> dataSources) (\s@ListDataSourcesResponse' {} a -> s {dataSources = a} :: ListDataSourcesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token for the next set of results, or null if there are no more
-- results.
listDataSourcesResponse_nextToken :: Lens.Lens' ListDataSourcesResponse (Prelude.Maybe Prelude.Text)
listDataSourcesResponse_nextToken = Lens.lens (\ListDataSourcesResponse' {nextToken} -> nextToken) (\s@ListDataSourcesResponse' {} a -> s {nextToken = a} :: ListDataSourcesResponse)

-- | The Amazon Web Services request ID for this operation.
listDataSourcesResponse_requestId :: Lens.Lens' ListDataSourcesResponse (Prelude.Maybe Prelude.Text)
listDataSourcesResponse_requestId = Lens.lens (\ListDataSourcesResponse' {requestId} -> requestId) (\s@ListDataSourcesResponse' {} a -> s {requestId = a} :: ListDataSourcesResponse)

-- | The HTTP status of the request.
listDataSourcesResponse_status :: Lens.Lens' ListDataSourcesResponse Prelude.Int
listDataSourcesResponse_status = Lens.lens (\ListDataSourcesResponse' {status} -> status) (\s@ListDataSourcesResponse' {} a -> s {status = a} :: ListDataSourcesResponse)

instance Prelude.NFData ListDataSourcesResponse where
  rnf ListDataSourcesResponse' {..} =
    Prelude.rnf dataSources
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf requestId
      `Prelude.seq` Prelude.rnf status
