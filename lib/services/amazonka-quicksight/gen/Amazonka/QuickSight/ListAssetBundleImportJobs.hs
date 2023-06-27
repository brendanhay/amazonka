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
-- Module      : Amazonka.QuickSight.ListAssetBundleImportJobs
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all asset bundle import jobs that have taken place in the last 14
-- days. Jobs created more than 14 days ago are deleted forever and are not
-- returned. If you are using the same job ID for multiple jobs,
-- @ListAssetBundleImportJobs@ only returns the most recent job that uses
-- the repeated job ID.
--
-- This operation returns paginated results.
module Amazonka.QuickSight.ListAssetBundleImportJobs
  ( -- * Creating a Request
    ListAssetBundleImportJobs (..),
    newListAssetBundleImportJobs,

    -- * Request Lenses
    listAssetBundleImportJobs_maxResults,
    listAssetBundleImportJobs_nextToken,
    listAssetBundleImportJobs_awsAccountId,

    -- * Destructuring the Response
    ListAssetBundleImportJobsResponse (..),
    newListAssetBundleImportJobsResponse,

    -- * Response Lenses
    listAssetBundleImportJobsResponse_assetBundleImportJobSummaryList,
    listAssetBundleImportJobsResponse_nextToken,
    listAssetBundleImportJobsResponse_requestId,
    listAssetBundleImportJobsResponse_status,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListAssetBundleImportJobs' smart constructor.
data ListAssetBundleImportJobs = ListAssetBundleImportJobs'
  { -- | The maximum number of results to be returned per request.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token for the next set of results, or null if there are no more
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Amazon Web Services account that the import jobs were
    -- executed in.
    awsAccountId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListAssetBundleImportJobs' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listAssetBundleImportJobs_maxResults' - The maximum number of results to be returned per request.
--
-- 'nextToken', 'listAssetBundleImportJobs_nextToken' - The token for the next set of results, or null if there are no more
-- results.
--
-- 'awsAccountId', 'listAssetBundleImportJobs_awsAccountId' - The ID of the Amazon Web Services account that the import jobs were
-- executed in.
newListAssetBundleImportJobs ::
  -- | 'awsAccountId'
  Prelude.Text ->
  ListAssetBundleImportJobs
newListAssetBundleImportJobs pAwsAccountId_ =
  ListAssetBundleImportJobs'
    { maxResults =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      awsAccountId = pAwsAccountId_
    }

-- | The maximum number of results to be returned per request.
listAssetBundleImportJobs_maxResults :: Lens.Lens' ListAssetBundleImportJobs (Prelude.Maybe Prelude.Natural)
listAssetBundleImportJobs_maxResults = Lens.lens (\ListAssetBundleImportJobs' {maxResults} -> maxResults) (\s@ListAssetBundleImportJobs' {} a -> s {maxResults = a} :: ListAssetBundleImportJobs)

-- | The token for the next set of results, or null if there are no more
-- results.
listAssetBundleImportJobs_nextToken :: Lens.Lens' ListAssetBundleImportJobs (Prelude.Maybe Prelude.Text)
listAssetBundleImportJobs_nextToken = Lens.lens (\ListAssetBundleImportJobs' {nextToken} -> nextToken) (\s@ListAssetBundleImportJobs' {} a -> s {nextToken = a} :: ListAssetBundleImportJobs)

-- | The ID of the Amazon Web Services account that the import jobs were
-- executed in.
listAssetBundleImportJobs_awsAccountId :: Lens.Lens' ListAssetBundleImportJobs Prelude.Text
listAssetBundleImportJobs_awsAccountId = Lens.lens (\ListAssetBundleImportJobs' {awsAccountId} -> awsAccountId) (\s@ListAssetBundleImportJobs' {} a -> s {awsAccountId = a} :: ListAssetBundleImportJobs)

instance Core.AWSPager ListAssetBundleImportJobs where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listAssetBundleImportJobsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listAssetBundleImportJobsResponse_assetBundleImportJobSummaryList
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listAssetBundleImportJobs_nextToken
          Lens..~ rs
          Lens.^? listAssetBundleImportJobsResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListAssetBundleImportJobs where
  type
    AWSResponse ListAssetBundleImportJobs =
      ListAssetBundleImportJobsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListAssetBundleImportJobsResponse'
            Prelude.<$> ( x
                            Data..?> "AssetBundleImportJobSummaryList"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "RequestId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListAssetBundleImportJobs where
  hashWithSalt _salt ListAssetBundleImportJobs' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` awsAccountId

instance Prelude.NFData ListAssetBundleImportJobs where
  rnf ListAssetBundleImportJobs' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf awsAccountId

instance Data.ToHeaders ListAssetBundleImportJobs where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListAssetBundleImportJobs where
  toPath ListAssetBundleImportJobs' {..} =
    Prelude.mconcat
      [ "/accounts/",
        Data.toBS awsAccountId,
        "/asset-bundle-import-jobs"
      ]

instance Data.ToQuery ListAssetBundleImportJobs where
  toQuery ListAssetBundleImportJobs' {..} =
    Prelude.mconcat
      [ "max-results" Data.=: maxResults,
        "next-token" Data.=: nextToken
      ]

-- | /See:/ 'newListAssetBundleImportJobsResponse' smart constructor.
data ListAssetBundleImportJobsResponse = ListAssetBundleImportJobsResponse'
  { -- | A list of import job summaries.
    assetBundleImportJobSummaryList :: Prelude.Maybe [AssetBundleImportJobSummary],
    -- | The token for the next set of results, or null if there are no more
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services request ID for this operation.
    requestId :: Prelude.Maybe Prelude.Text,
    -- | The HTTP status of the response.
    status :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListAssetBundleImportJobsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'assetBundleImportJobSummaryList', 'listAssetBundleImportJobsResponse_assetBundleImportJobSummaryList' - A list of import job summaries.
--
-- 'nextToken', 'listAssetBundleImportJobsResponse_nextToken' - The token for the next set of results, or null if there are no more
-- results.
--
-- 'requestId', 'listAssetBundleImportJobsResponse_requestId' - The Amazon Web Services request ID for this operation.
--
-- 'status', 'listAssetBundleImportJobsResponse_status' - The HTTP status of the response.
newListAssetBundleImportJobsResponse ::
  -- | 'status'
  Prelude.Int ->
  ListAssetBundleImportJobsResponse
newListAssetBundleImportJobsResponse pStatus_ =
  ListAssetBundleImportJobsResponse'
    { assetBundleImportJobSummaryList =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      requestId = Prelude.Nothing,
      status = pStatus_
    }

-- | A list of import job summaries.
listAssetBundleImportJobsResponse_assetBundleImportJobSummaryList :: Lens.Lens' ListAssetBundleImportJobsResponse (Prelude.Maybe [AssetBundleImportJobSummary])
listAssetBundleImportJobsResponse_assetBundleImportJobSummaryList = Lens.lens (\ListAssetBundleImportJobsResponse' {assetBundleImportJobSummaryList} -> assetBundleImportJobSummaryList) (\s@ListAssetBundleImportJobsResponse' {} a -> s {assetBundleImportJobSummaryList = a} :: ListAssetBundleImportJobsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token for the next set of results, or null if there are no more
-- results.
listAssetBundleImportJobsResponse_nextToken :: Lens.Lens' ListAssetBundleImportJobsResponse (Prelude.Maybe Prelude.Text)
listAssetBundleImportJobsResponse_nextToken = Lens.lens (\ListAssetBundleImportJobsResponse' {nextToken} -> nextToken) (\s@ListAssetBundleImportJobsResponse' {} a -> s {nextToken = a} :: ListAssetBundleImportJobsResponse)

-- | The Amazon Web Services request ID for this operation.
listAssetBundleImportJobsResponse_requestId :: Lens.Lens' ListAssetBundleImportJobsResponse (Prelude.Maybe Prelude.Text)
listAssetBundleImportJobsResponse_requestId = Lens.lens (\ListAssetBundleImportJobsResponse' {requestId} -> requestId) (\s@ListAssetBundleImportJobsResponse' {} a -> s {requestId = a} :: ListAssetBundleImportJobsResponse)

-- | The HTTP status of the response.
listAssetBundleImportJobsResponse_status :: Lens.Lens' ListAssetBundleImportJobsResponse Prelude.Int
listAssetBundleImportJobsResponse_status = Lens.lens (\ListAssetBundleImportJobsResponse' {status} -> status) (\s@ListAssetBundleImportJobsResponse' {} a -> s {status = a} :: ListAssetBundleImportJobsResponse)

instance
  Prelude.NFData
    ListAssetBundleImportJobsResponse
  where
  rnf ListAssetBundleImportJobsResponse' {..} =
    Prelude.rnf assetBundleImportJobSummaryList
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf requestId
      `Prelude.seq` Prelude.rnf status
