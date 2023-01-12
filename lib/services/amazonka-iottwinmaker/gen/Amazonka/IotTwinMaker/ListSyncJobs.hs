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
-- Module      : Amazonka.IotTwinMaker.ListSyncJobs
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List all SyncJobs.
module Amazonka.IotTwinMaker.ListSyncJobs
  ( -- * Creating a Request
    ListSyncJobs (..),
    newListSyncJobs,

    -- * Request Lenses
    listSyncJobs_maxResults,
    listSyncJobs_nextToken,
    listSyncJobs_workspaceId,

    -- * Destructuring the Response
    ListSyncJobsResponse (..),
    newListSyncJobsResponse,

    -- * Response Lenses
    listSyncJobsResponse_nextToken,
    listSyncJobsResponse_syncJobSummaries,
    listSyncJobsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IotTwinMaker.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListSyncJobs' smart constructor.
data ListSyncJobs = ListSyncJobs'
  { -- | The maximum number of results to return at one time. The default is 50.
    --
    -- Valid Range: Minimum value of 0. Maximum value of 200.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The string that specifies the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The ID of the workspace that contains the sync job.
    workspaceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListSyncJobs' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listSyncJobs_maxResults' - The maximum number of results to return at one time. The default is 50.
--
-- Valid Range: Minimum value of 0. Maximum value of 200.
--
-- 'nextToken', 'listSyncJobs_nextToken' - The string that specifies the next page of results.
--
-- 'workspaceId', 'listSyncJobs_workspaceId' - The ID of the workspace that contains the sync job.
newListSyncJobs ::
  -- | 'workspaceId'
  Prelude.Text ->
  ListSyncJobs
newListSyncJobs pWorkspaceId_ =
  ListSyncJobs'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      workspaceId = pWorkspaceId_
    }

-- | The maximum number of results to return at one time. The default is 50.
--
-- Valid Range: Minimum value of 0. Maximum value of 200.
listSyncJobs_maxResults :: Lens.Lens' ListSyncJobs (Prelude.Maybe Prelude.Natural)
listSyncJobs_maxResults = Lens.lens (\ListSyncJobs' {maxResults} -> maxResults) (\s@ListSyncJobs' {} a -> s {maxResults = a} :: ListSyncJobs)

-- | The string that specifies the next page of results.
listSyncJobs_nextToken :: Lens.Lens' ListSyncJobs (Prelude.Maybe Prelude.Text)
listSyncJobs_nextToken = Lens.lens (\ListSyncJobs' {nextToken} -> nextToken) (\s@ListSyncJobs' {} a -> s {nextToken = a} :: ListSyncJobs)

-- | The ID of the workspace that contains the sync job.
listSyncJobs_workspaceId :: Lens.Lens' ListSyncJobs Prelude.Text
listSyncJobs_workspaceId = Lens.lens (\ListSyncJobs' {workspaceId} -> workspaceId) (\s@ListSyncJobs' {} a -> s {workspaceId = a} :: ListSyncJobs)

instance Core.AWSRequest ListSyncJobs where
  type AWSResponse ListSyncJobs = ListSyncJobsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListSyncJobsResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> ( x Data..?> "syncJobSummaries"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListSyncJobs where
  hashWithSalt _salt ListSyncJobs' {..} =
    _salt `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` workspaceId

instance Prelude.NFData ListSyncJobs where
  rnf ListSyncJobs' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf workspaceId

instance Data.ToHeaders ListSyncJobs where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListSyncJobs where
  toJSON ListSyncJobs' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("maxResults" Data..=) Prelude.<$> maxResults,
            ("nextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance Data.ToPath ListSyncJobs where
  toPath ListSyncJobs' {..} =
    Prelude.mconcat
      [ "/workspaces/",
        Data.toBS workspaceId,
        "/sync-jobs-list"
      ]

instance Data.ToQuery ListSyncJobs where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListSyncJobsResponse' smart constructor.
data ListSyncJobsResponse = ListSyncJobsResponse'
  { -- | The string that specifies the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The listed SyncJob summaries.
    syncJobSummaries :: Prelude.Maybe [SyncJobSummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListSyncJobsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listSyncJobsResponse_nextToken' - The string that specifies the next page of results.
--
-- 'syncJobSummaries', 'listSyncJobsResponse_syncJobSummaries' - The listed SyncJob summaries.
--
-- 'httpStatus', 'listSyncJobsResponse_httpStatus' - The response's http status code.
newListSyncJobsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListSyncJobsResponse
newListSyncJobsResponse pHttpStatus_ =
  ListSyncJobsResponse'
    { nextToken = Prelude.Nothing,
      syncJobSummaries = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The string that specifies the next page of results.
listSyncJobsResponse_nextToken :: Lens.Lens' ListSyncJobsResponse (Prelude.Maybe Prelude.Text)
listSyncJobsResponse_nextToken = Lens.lens (\ListSyncJobsResponse' {nextToken} -> nextToken) (\s@ListSyncJobsResponse' {} a -> s {nextToken = a} :: ListSyncJobsResponse)

-- | The listed SyncJob summaries.
listSyncJobsResponse_syncJobSummaries :: Lens.Lens' ListSyncJobsResponse (Prelude.Maybe [SyncJobSummary])
listSyncJobsResponse_syncJobSummaries = Lens.lens (\ListSyncJobsResponse' {syncJobSummaries} -> syncJobSummaries) (\s@ListSyncJobsResponse' {} a -> s {syncJobSummaries = a} :: ListSyncJobsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listSyncJobsResponse_httpStatus :: Lens.Lens' ListSyncJobsResponse Prelude.Int
listSyncJobsResponse_httpStatus = Lens.lens (\ListSyncJobsResponse' {httpStatus} -> httpStatus) (\s@ListSyncJobsResponse' {} a -> s {httpStatus = a} :: ListSyncJobsResponse)

instance Prelude.NFData ListSyncJobsResponse where
  rnf ListSyncJobsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf syncJobSummaries
      `Prelude.seq` Prelude.rnf httpStatus
