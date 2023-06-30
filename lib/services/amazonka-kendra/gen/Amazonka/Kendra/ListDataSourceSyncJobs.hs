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
-- Module      : Amazonka.Kendra.ListDataSourceSyncJobs
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets statistics about synchronizing a data source connector.
module Amazonka.Kendra.ListDataSourceSyncJobs
  ( -- * Creating a Request
    ListDataSourceSyncJobs (..),
    newListDataSourceSyncJobs,

    -- * Request Lenses
    listDataSourceSyncJobs_maxResults,
    listDataSourceSyncJobs_nextToken,
    listDataSourceSyncJobs_startTimeFilter,
    listDataSourceSyncJobs_statusFilter,
    listDataSourceSyncJobs_id,
    listDataSourceSyncJobs_indexId,

    -- * Destructuring the Response
    ListDataSourceSyncJobsResponse (..),
    newListDataSourceSyncJobsResponse,

    -- * Response Lenses
    listDataSourceSyncJobsResponse_history,
    listDataSourceSyncJobsResponse_nextToken,
    listDataSourceSyncJobsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Kendra.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListDataSourceSyncJobs' smart constructor.
data ListDataSourceSyncJobs = ListDataSourceSyncJobs'
  { -- | The maximum number of synchronization jobs to return in the response. If
    -- there are fewer results in the list, this response contains only the
    -- actual results.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | If the previous response was incomplete (because there is more data to
    -- retrieve), Amazon Kendra returns a pagination token in the response. You
    -- can use this pagination token to retrieve the next set of jobs.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | When specified, the synchronization jobs returned in the list are
    -- limited to jobs between the specified dates.
    startTimeFilter :: Prelude.Maybe TimeRange,
    -- | Only returns synchronization jobs with the @Status@ field equal to the
    -- specified status.
    statusFilter :: Prelude.Maybe DataSourceSyncJobStatus,
    -- | The identifier of the data source connector.
    id :: Prelude.Text,
    -- | The identifier of the index used with the data source connector.
    indexId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListDataSourceSyncJobs' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listDataSourceSyncJobs_maxResults' - The maximum number of synchronization jobs to return in the response. If
-- there are fewer results in the list, this response contains only the
-- actual results.
--
-- 'nextToken', 'listDataSourceSyncJobs_nextToken' - If the previous response was incomplete (because there is more data to
-- retrieve), Amazon Kendra returns a pagination token in the response. You
-- can use this pagination token to retrieve the next set of jobs.
--
-- 'startTimeFilter', 'listDataSourceSyncJobs_startTimeFilter' - When specified, the synchronization jobs returned in the list are
-- limited to jobs between the specified dates.
--
-- 'statusFilter', 'listDataSourceSyncJobs_statusFilter' - Only returns synchronization jobs with the @Status@ field equal to the
-- specified status.
--
-- 'id', 'listDataSourceSyncJobs_id' - The identifier of the data source connector.
--
-- 'indexId', 'listDataSourceSyncJobs_indexId' - The identifier of the index used with the data source connector.
newListDataSourceSyncJobs ::
  -- | 'id'
  Prelude.Text ->
  -- | 'indexId'
  Prelude.Text ->
  ListDataSourceSyncJobs
newListDataSourceSyncJobs pId_ pIndexId_ =
  ListDataSourceSyncJobs'
    { maxResults =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      startTimeFilter = Prelude.Nothing,
      statusFilter = Prelude.Nothing,
      id = pId_,
      indexId = pIndexId_
    }

-- | The maximum number of synchronization jobs to return in the response. If
-- there are fewer results in the list, this response contains only the
-- actual results.
listDataSourceSyncJobs_maxResults :: Lens.Lens' ListDataSourceSyncJobs (Prelude.Maybe Prelude.Natural)
listDataSourceSyncJobs_maxResults = Lens.lens (\ListDataSourceSyncJobs' {maxResults} -> maxResults) (\s@ListDataSourceSyncJobs' {} a -> s {maxResults = a} :: ListDataSourceSyncJobs)

-- | If the previous response was incomplete (because there is more data to
-- retrieve), Amazon Kendra returns a pagination token in the response. You
-- can use this pagination token to retrieve the next set of jobs.
listDataSourceSyncJobs_nextToken :: Lens.Lens' ListDataSourceSyncJobs (Prelude.Maybe Prelude.Text)
listDataSourceSyncJobs_nextToken = Lens.lens (\ListDataSourceSyncJobs' {nextToken} -> nextToken) (\s@ListDataSourceSyncJobs' {} a -> s {nextToken = a} :: ListDataSourceSyncJobs)

-- | When specified, the synchronization jobs returned in the list are
-- limited to jobs between the specified dates.
listDataSourceSyncJobs_startTimeFilter :: Lens.Lens' ListDataSourceSyncJobs (Prelude.Maybe TimeRange)
listDataSourceSyncJobs_startTimeFilter = Lens.lens (\ListDataSourceSyncJobs' {startTimeFilter} -> startTimeFilter) (\s@ListDataSourceSyncJobs' {} a -> s {startTimeFilter = a} :: ListDataSourceSyncJobs)

-- | Only returns synchronization jobs with the @Status@ field equal to the
-- specified status.
listDataSourceSyncJobs_statusFilter :: Lens.Lens' ListDataSourceSyncJobs (Prelude.Maybe DataSourceSyncJobStatus)
listDataSourceSyncJobs_statusFilter = Lens.lens (\ListDataSourceSyncJobs' {statusFilter} -> statusFilter) (\s@ListDataSourceSyncJobs' {} a -> s {statusFilter = a} :: ListDataSourceSyncJobs)

-- | The identifier of the data source connector.
listDataSourceSyncJobs_id :: Lens.Lens' ListDataSourceSyncJobs Prelude.Text
listDataSourceSyncJobs_id = Lens.lens (\ListDataSourceSyncJobs' {id} -> id) (\s@ListDataSourceSyncJobs' {} a -> s {id = a} :: ListDataSourceSyncJobs)

-- | The identifier of the index used with the data source connector.
listDataSourceSyncJobs_indexId :: Lens.Lens' ListDataSourceSyncJobs Prelude.Text
listDataSourceSyncJobs_indexId = Lens.lens (\ListDataSourceSyncJobs' {indexId} -> indexId) (\s@ListDataSourceSyncJobs' {} a -> s {indexId = a} :: ListDataSourceSyncJobs)

instance Core.AWSRequest ListDataSourceSyncJobs where
  type
    AWSResponse ListDataSourceSyncJobs =
      ListDataSourceSyncJobsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListDataSourceSyncJobsResponse'
            Prelude.<$> (x Data..?> "History" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListDataSourceSyncJobs where
  hashWithSalt _salt ListDataSourceSyncJobs' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` startTimeFilter
      `Prelude.hashWithSalt` statusFilter
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` indexId

instance Prelude.NFData ListDataSourceSyncJobs where
  rnf ListDataSourceSyncJobs' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf startTimeFilter
      `Prelude.seq` Prelude.rnf statusFilter
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf indexId

instance Data.ToHeaders ListDataSourceSyncJobs where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSKendraFrontendService.ListDataSourceSyncJobs" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListDataSourceSyncJobs where
  toJSON ListDataSourceSyncJobs' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            ("StartTimeFilter" Data..=)
              Prelude.<$> startTimeFilter,
            ("StatusFilter" Data..=) Prelude.<$> statusFilter,
            Prelude.Just ("Id" Data..= id),
            Prelude.Just ("IndexId" Data..= indexId)
          ]
      )

instance Data.ToPath ListDataSourceSyncJobs where
  toPath = Prelude.const "/"

instance Data.ToQuery ListDataSourceSyncJobs where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListDataSourceSyncJobsResponse' smart constructor.
data ListDataSourceSyncJobsResponse = ListDataSourceSyncJobsResponse'
  { -- | A history of synchronization jobs for the data source connector.
    history :: Prelude.Maybe [DataSourceSyncJob],
    -- | If the response is truncated, Amazon Kendra returns this token that you
    -- can use in the subsequent request to retrieve the next set of jobs.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListDataSourceSyncJobsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'history', 'listDataSourceSyncJobsResponse_history' - A history of synchronization jobs for the data source connector.
--
-- 'nextToken', 'listDataSourceSyncJobsResponse_nextToken' - If the response is truncated, Amazon Kendra returns this token that you
-- can use in the subsequent request to retrieve the next set of jobs.
--
-- 'httpStatus', 'listDataSourceSyncJobsResponse_httpStatus' - The response's http status code.
newListDataSourceSyncJobsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListDataSourceSyncJobsResponse
newListDataSourceSyncJobsResponse pHttpStatus_ =
  ListDataSourceSyncJobsResponse'
    { history =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A history of synchronization jobs for the data source connector.
listDataSourceSyncJobsResponse_history :: Lens.Lens' ListDataSourceSyncJobsResponse (Prelude.Maybe [DataSourceSyncJob])
listDataSourceSyncJobsResponse_history = Lens.lens (\ListDataSourceSyncJobsResponse' {history} -> history) (\s@ListDataSourceSyncJobsResponse' {} a -> s {history = a} :: ListDataSourceSyncJobsResponse) Prelude.. Lens.mapping Lens.coerced

-- | If the response is truncated, Amazon Kendra returns this token that you
-- can use in the subsequent request to retrieve the next set of jobs.
listDataSourceSyncJobsResponse_nextToken :: Lens.Lens' ListDataSourceSyncJobsResponse (Prelude.Maybe Prelude.Text)
listDataSourceSyncJobsResponse_nextToken = Lens.lens (\ListDataSourceSyncJobsResponse' {nextToken} -> nextToken) (\s@ListDataSourceSyncJobsResponse' {} a -> s {nextToken = a} :: ListDataSourceSyncJobsResponse)

-- | The response's http status code.
listDataSourceSyncJobsResponse_httpStatus :: Lens.Lens' ListDataSourceSyncJobsResponse Prelude.Int
listDataSourceSyncJobsResponse_httpStatus = Lens.lens (\ListDataSourceSyncJobsResponse' {httpStatus} -> httpStatus) (\s@ListDataSourceSyncJobsResponse' {} a -> s {httpStatus = a} :: ListDataSourceSyncJobsResponse)

instance
  Prelude.NFData
    ListDataSourceSyncJobsResponse
  where
  rnf ListDataSourceSyncJobsResponse' {..} =
    Prelude.rnf history
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
