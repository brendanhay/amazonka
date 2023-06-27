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
-- Module      : Amazonka.QuickSight.ListAssetBundleExportJobs
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all asset bundle export jobs that have been taken place in the
-- last 14 days. Jobs created more than 14 days ago are deleted forever and
-- are not returned. If you are using the same job ID for multiple jobs,
-- @ListAssetBundleExportJobs@ only returns the most recent job that uses
-- the repeated job ID.
--
-- This operation returns paginated results.
module Amazonka.QuickSight.ListAssetBundleExportJobs
  ( -- * Creating a Request
    ListAssetBundleExportJobs (..),
    newListAssetBundleExportJobs,

    -- * Request Lenses
    listAssetBundleExportJobs_maxResults,
    listAssetBundleExportJobs_nextToken,
    listAssetBundleExportJobs_awsAccountId,

    -- * Destructuring the Response
    ListAssetBundleExportJobsResponse (..),
    newListAssetBundleExportJobsResponse,

    -- * Response Lenses
    listAssetBundleExportJobsResponse_assetBundleExportJobSummaryList,
    listAssetBundleExportJobsResponse_nextToken,
    listAssetBundleExportJobsResponse_requestId,
    listAssetBundleExportJobsResponse_status,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListAssetBundleExportJobs' smart constructor.
data ListAssetBundleExportJobs = ListAssetBundleExportJobs'
  { -- | The maximum number of results to be returned per request.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token for the next set of results, or null if there are no more
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Amazon Web Services account that the export jobs were
    -- executed in.
    awsAccountId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListAssetBundleExportJobs' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listAssetBundleExportJobs_maxResults' - The maximum number of results to be returned per request.
--
-- 'nextToken', 'listAssetBundleExportJobs_nextToken' - The token for the next set of results, or null if there are no more
-- results.
--
-- 'awsAccountId', 'listAssetBundleExportJobs_awsAccountId' - The ID of the Amazon Web Services account that the export jobs were
-- executed in.
newListAssetBundleExportJobs ::
  -- | 'awsAccountId'
  Prelude.Text ->
  ListAssetBundleExportJobs
newListAssetBundleExportJobs pAwsAccountId_ =
  ListAssetBundleExportJobs'
    { maxResults =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      awsAccountId = pAwsAccountId_
    }

-- | The maximum number of results to be returned per request.
listAssetBundleExportJobs_maxResults :: Lens.Lens' ListAssetBundleExportJobs (Prelude.Maybe Prelude.Natural)
listAssetBundleExportJobs_maxResults = Lens.lens (\ListAssetBundleExportJobs' {maxResults} -> maxResults) (\s@ListAssetBundleExportJobs' {} a -> s {maxResults = a} :: ListAssetBundleExportJobs)

-- | The token for the next set of results, or null if there are no more
-- results.
listAssetBundleExportJobs_nextToken :: Lens.Lens' ListAssetBundleExportJobs (Prelude.Maybe Prelude.Text)
listAssetBundleExportJobs_nextToken = Lens.lens (\ListAssetBundleExportJobs' {nextToken} -> nextToken) (\s@ListAssetBundleExportJobs' {} a -> s {nextToken = a} :: ListAssetBundleExportJobs)

-- | The ID of the Amazon Web Services account that the export jobs were
-- executed in.
listAssetBundleExportJobs_awsAccountId :: Lens.Lens' ListAssetBundleExportJobs Prelude.Text
listAssetBundleExportJobs_awsAccountId = Lens.lens (\ListAssetBundleExportJobs' {awsAccountId} -> awsAccountId) (\s@ListAssetBundleExportJobs' {} a -> s {awsAccountId = a} :: ListAssetBundleExportJobs)

instance Core.AWSPager ListAssetBundleExportJobs where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listAssetBundleExportJobsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listAssetBundleExportJobsResponse_assetBundleExportJobSummaryList
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listAssetBundleExportJobs_nextToken
          Lens..~ rs
          Lens.^? listAssetBundleExportJobsResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListAssetBundleExportJobs where
  type
    AWSResponse ListAssetBundleExportJobs =
      ListAssetBundleExportJobsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListAssetBundleExportJobsResponse'
            Prelude.<$> ( x
                            Data..?> "AssetBundleExportJobSummaryList"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "RequestId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListAssetBundleExportJobs where
  hashWithSalt _salt ListAssetBundleExportJobs' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` awsAccountId

instance Prelude.NFData ListAssetBundleExportJobs where
  rnf ListAssetBundleExportJobs' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf awsAccountId

instance Data.ToHeaders ListAssetBundleExportJobs where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListAssetBundleExportJobs where
  toPath ListAssetBundleExportJobs' {..} =
    Prelude.mconcat
      [ "/accounts/",
        Data.toBS awsAccountId,
        "/asset-bundle-export-jobs"
      ]

instance Data.ToQuery ListAssetBundleExportJobs where
  toQuery ListAssetBundleExportJobs' {..} =
    Prelude.mconcat
      [ "max-results" Data.=: maxResults,
        "next-token" Data.=: nextToken
      ]

-- | /See:/ 'newListAssetBundleExportJobsResponse' smart constructor.
data ListAssetBundleExportJobsResponse = ListAssetBundleExportJobsResponse'
  { -- | A list of export job summaries.
    assetBundleExportJobSummaryList :: Prelude.Maybe [AssetBundleExportJobSummary],
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
-- Create a value of 'ListAssetBundleExportJobsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'assetBundleExportJobSummaryList', 'listAssetBundleExportJobsResponse_assetBundleExportJobSummaryList' - A list of export job summaries.
--
-- 'nextToken', 'listAssetBundleExportJobsResponse_nextToken' - The token for the next set of results, or null if there are no more
-- results.
--
-- 'requestId', 'listAssetBundleExportJobsResponse_requestId' - The Amazon Web Services request ID for this operation.
--
-- 'status', 'listAssetBundleExportJobsResponse_status' - The HTTP status of the request.
newListAssetBundleExportJobsResponse ::
  -- | 'status'
  Prelude.Int ->
  ListAssetBundleExportJobsResponse
newListAssetBundleExportJobsResponse pStatus_ =
  ListAssetBundleExportJobsResponse'
    { assetBundleExportJobSummaryList =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      requestId = Prelude.Nothing,
      status = pStatus_
    }

-- | A list of export job summaries.
listAssetBundleExportJobsResponse_assetBundleExportJobSummaryList :: Lens.Lens' ListAssetBundleExportJobsResponse (Prelude.Maybe [AssetBundleExportJobSummary])
listAssetBundleExportJobsResponse_assetBundleExportJobSummaryList = Lens.lens (\ListAssetBundleExportJobsResponse' {assetBundleExportJobSummaryList} -> assetBundleExportJobSummaryList) (\s@ListAssetBundleExportJobsResponse' {} a -> s {assetBundleExportJobSummaryList = a} :: ListAssetBundleExportJobsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token for the next set of results, or null if there are no more
-- results.
listAssetBundleExportJobsResponse_nextToken :: Lens.Lens' ListAssetBundleExportJobsResponse (Prelude.Maybe Prelude.Text)
listAssetBundleExportJobsResponse_nextToken = Lens.lens (\ListAssetBundleExportJobsResponse' {nextToken} -> nextToken) (\s@ListAssetBundleExportJobsResponse' {} a -> s {nextToken = a} :: ListAssetBundleExportJobsResponse)

-- | The Amazon Web Services request ID for this operation.
listAssetBundleExportJobsResponse_requestId :: Lens.Lens' ListAssetBundleExportJobsResponse (Prelude.Maybe Prelude.Text)
listAssetBundleExportJobsResponse_requestId = Lens.lens (\ListAssetBundleExportJobsResponse' {requestId} -> requestId) (\s@ListAssetBundleExportJobsResponse' {} a -> s {requestId = a} :: ListAssetBundleExportJobsResponse)

-- | The HTTP status of the request.
listAssetBundleExportJobsResponse_status :: Lens.Lens' ListAssetBundleExportJobsResponse Prelude.Int
listAssetBundleExportJobsResponse_status = Lens.lens (\ListAssetBundleExportJobsResponse' {status} -> status) (\s@ListAssetBundleExportJobsResponse' {} a -> s {status = a} :: ListAssetBundleExportJobsResponse)

instance
  Prelude.NFData
    ListAssetBundleExportJobsResponse
  where
  rnf ListAssetBundleExportJobsResponse' {..} =
    Prelude.rnf assetBundleExportJobSummaryList
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf requestId
      `Prelude.seq` Prelude.rnf status
