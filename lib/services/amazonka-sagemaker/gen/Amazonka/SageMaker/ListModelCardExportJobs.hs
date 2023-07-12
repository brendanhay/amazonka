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
-- Module      : Amazonka.SageMaker.ListModelCardExportJobs
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List the export jobs for the Amazon SageMaker Model Card.
--
-- This operation returns paginated results.
module Amazonka.SageMaker.ListModelCardExportJobs
  ( -- * Creating a Request
    ListModelCardExportJobs (..),
    newListModelCardExportJobs,

    -- * Request Lenses
    listModelCardExportJobs_creationTimeAfter,
    listModelCardExportJobs_creationTimeBefore,
    listModelCardExportJobs_maxResults,
    listModelCardExportJobs_modelCardExportJobNameContains,
    listModelCardExportJobs_modelCardVersion,
    listModelCardExportJobs_nextToken,
    listModelCardExportJobs_sortBy,
    listModelCardExportJobs_sortOrder,
    listModelCardExportJobs_statusEquals,
    listModelCardExportJobs_modelCardName,

    -- * Destructuring the Response
    ListModelCardExportJobsResponse (..),
    newListModelCardExportJobsResponse,

    -- * Response Lenses
    listModelCardExportJobsResponse_nextToken,
    listModelCardExportJobsResponse_httpStatus,
    listModelCardExportJobsResponse_modelCardExportJobSummaries,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newListModelCardExportJobs' smart constructor.
data ListModelCardExportJobs = ListModelCardExportJobs'
  { -- | Only list model card export jobs that were created after the time
    -- specified.
    creationTimeAfter :: Prelude.Maybe Data.POSIX,
    -- | Only list model card export jobs that were created before the time
    -- specified.
    creationTimeBefore :: Prelude.Maybe Data.POSIX,
    -- | The maximum number of model card export jobs to list.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | Only list model card export jobs with names that contain the specified
    -- string.
    modelCardExportJobNameContains :: Prelude.Maybe Prelude.Text,
    -- | List export jobs for the model card with the specified version.
    modelCardVersion :: Prelude.Maybe Prelude.Int,
    -- | If the response to a previous @ListModelCardExportJobs@ request was
    -- truncated, the response includes a @NextToken@. To retrieve the next set
    -- of model card export jobs, use the token in the next request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Sort model card export jobs by either name or creation time. Sorts by
    -- creation time by default.
    sortBy :: Prelude.Maybe ModelCardExportJobSortBy,
    -- | Sort model card export jobs by ascending or descending order.
    sortOrder :: Prelude.Maybe ModelCardExportJobSortOrder,
    -- | Only list model card export jobs with the specified status.
    statusEquals :: Prelude.Maybe ModelCardExportJobStatus,
    -- | List export jobs for the model card with the specified name.
    modelCardName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListModelCardExportJobs' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTimeAfter', 'listModelCardExportJobs_creationTimeAfter' - Only list model card export jobs that were created after the time
-- specified.
--
-- 'creationTimeBefore', 'listModelCardExportJobs_creationTimeBefore' - Only list model card export jobs that were created before the time
-- specified.
--
-- 'maxResults', 'listModelCardExportJobs_maxResults' - The maximum number of model card export jobs to list.
--
-- 'modelCardExportJobNameContains', 'listModelCardExportJobs_modelCardExportJobNameContains' - Only list model card export jobs with names that contain the specified
-- string.
--
-- 'modelCardVersion', 'listModelCardExportJobs_modelCardVersion' - List export jobs for the model card with the specified version.
--
-- 'nextToken', 'listModelCardExportJobs_nextToken' - If the response to a previous @ListModelCardExportJobs@ request was
-- truncated, the response includes a @NextToken@. To retrieve the next set
-- of model card export jobs, use the token in the next request.
--
-- 'sortBy', 'listModelCardExportJobs_sortBy' - Sort model card export jobs by either name or creation time. Sorts by
-- creation time by default.
--
-- 'sortOrder', 'listModelCardExportJobs_sortOrder' - Sort model card export jobs by ascending or descending order.
--
-- 'statusEquals', 'listModelCardExportJobs_statusEquals' - Only list model card export jobs with the specified status.
--
-- 'modelCardName', 'listModelCardExportJobs_modelCardName' - List export jobs for the model card with the specified name.
newListModelCardExportJobs ::
  -- | 'modelCardName'
  Prelude.Text ->
  ListModelCardExportJobs
newListModelCardExportJobs pModelCardName_ =
  ListModelCardExportJobs'
    { creationTimeAfter =
        Prelude.Nothing,
      creationTimeBefore = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      modelCardExportJobNameContains = Prelude.Nothing,
      modelCardVersion = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      sortBy = Prelude.Nothing,
      sortOrder = Prelude.Nothing,
      statusEquals = Prelude.Nothing,
      modelCardName = pModelCardName_
    }

-- | Only list model card export jobs that were created after the time
-- specified.
listModelCardExportJobs_creationTimeAfter :: Lens.Lens' ListModelCardExportJobs (Prelude.Maybe Prelude.UTCTime)
listModelCardExportJobs_creationTimeAfter = Lens.lens (\ListModelCardExportJobs' {creationTimeAfter} -> creationTimeAfter) (\s@ListModelCardExportJobs' {} a -> s {creationTimeAfter = a} :: ListModelCardExportJobs) Prelude.. Lens.mapping Data._Time

-- | Only list model card export jobs that were created before the time
-- specified.
listModelCardExportJobs_creationTimeBefore :: Lens.Lens' ListModelCardExportJobs (Prelude.Maybe Prelude.UTCTime)
listModelCardExportJobs_creationTimeBefore = Lens.lens (\ListModelCardExportJobs' {creationTimeBefore} -> creationTimeBefore) (\s@ListModelCardExportJobs' {} a -> s {creationTimeBefore = a} :: ListModelCardExportJobs) Prelude.. Lens.mapping Data._Time

-- | The maximum number of model card export jobs to list.
listModelCardExportJobs_maxResults :: Lens.Lens' ListModelCardExportJobs (Prelude.Maybe Prelude.Natural)
listModelCardExportJobs_maxResults = Lens.lens (\ListModelCardExportJobs' {maxResults} -> maxResults) (\s@ListModelCardExportJobs' {} a -> s {maxResults = a} :: ListModelCardExportJobs)

-- | Only list model card export jobs with names that contain the specified
-- string.
listModelCardExportJobs_modelCardExportJobNameContains :: Lens.Lens' ListModelCardExportJobs (Prelude.Maybe Prelude.Text)
listModelCardExportJobs_modelCardExportJobNameContains = Lens.lens (\ListModelCardExportJobs' {modelCardExportJobNameContains} -> modelCardExportJobNameContains) (\s@ListModelCardExportJobs' {} a -> s {modelCardExportJobNameContains = a} :: ListModelCardExportJobs)

-- | List export jobs for the model card with the specified version.
listModelCardExportJobs_modelCardVersion :: Lens.Lens' ListModelCardExportJobs (Prelude.Maybe Prelude.Int)
listModelCardExportJobs_modelCardVersion = Lens.lens (\ListModelCardExportJobs' {modelCardVersion} -> modelCardVersion) (\s@ListModelCardExportJobs' {} a -> s {modelCardVersion = a} :: ListModelCardExportJobs)

-- | If the response to a previous @ListModelCardExportJobs@ request was
-- truncated, the response includes a @NextToken@. To retrieve the next set
-- of model card export jobs, use the token in the next request.
listModelCardExportJobs_nextToken :: Lens.Lens' ListModelCardExportJobs (Prelude.Maybe Prelude.Text)
listModelCardExportJobs_nextToken = Lens.lens (\ListModelCardExportJobs' {nextToken} -> nextToken) (\s@ListModelCardExportJobs' {} a -> s {nextToken = a} :: ListModelCardExportJobs)

-- | Sort model card export jobs by either name or creation time. Sorts by
-- creation time by default.
listModelCardExportJobs_sortBy :: Lens.Lens' ListModelCardExportJobs (Prelude.Maybe ModelCardExportJobSortBy)
listModelCardExportJobs_sortBy = Lens.lens (\ListModelCardExportJobs' {sortBy} -> sortBy) (\s@ListModelCardExportJobs' {} a -> s {sortBy = a} :: ListModelCardExportJobs)

-- | Sort model card export jobs by ascending or descending order.
listModelCardExportJobs_sortOrder :: Lens.Lens' ListModelCardExportJobs (Prelude.Maybe ModelCardExportJobSortOrder)
listModelCardExportJobs_sortOrder = Lens.lens (\ListModelCardExportJobs' {sortOrder} -> sortOrder) (\s@ListModelCardExportJobs' {} a -> s {sortOrder = a} :: ListModelCardExportJobs)

-- | Only list model card export jobs with the specified status.
listModelCardExportJobs_statusEquals :: Lens.Lens' ListModelCardExportJobs (Prelude.Maybe ModelCardExportJobStatus)
listModelCardExportJobs_statusEquals = Lens.lens (\ListModelCardExportJobs' {statusEquals} -> statusEquals) (\s@ListModelCardExportJobs' {} a -> s {statusEquals = a} :: ListModelCardExportJobs)

-- | List export jobs for the model card with the specified name.
listModelCardExportJobs_modelCardName :: Lens.Lens' ListModelCardExportJobs Prelude.Text
listModelCardExportJobs_modelCardName = Lens.lens (\ListModelCardExportJobs' {modelCardName} -> modelCardName) (\s@ListModelCardExportJobs' {} a -> s {modelCardName = a} :: ListModelCardExportJobs)

instance Core.AWSPager ListModelCardExportJobs where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listModelCardExportJobsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^. listModelCardExportJobsResponse_modelCardExportJobSummaries
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listModelCardExportJobs_nextToken
          Lens..~ rs
          Lens.^? listModelCardExportJobsResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListModelCardExportJobs where
  type
    AWSResponse ListModelCardExportJobs =
      ListModelCardExportJobsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListModelCardExportJobsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x
                            Data..?> "ModelCardExportJobSummaries"
                            Core..!@ Prelude.mempty
                        )
      )

instance Prelude.Hashable ListModelCardExportJobs where
  hashWithSalt _salt ListModelCardExportJobs' {..} =
    _salt
      `Prelude.hashWithSalt` creationTimeAfter
      `Prelude.hashWithSalt` creationTimeBefore
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` modelCardExportJobNameContains
      `Prelude.hashWithSalt` modelCardVersion
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` sortBy
      `Prelude.hashWithSalt` sortOrder
      `Prelude.hashWithSalt` statusEquals
      `Prelude.hashWithSalt` modelCardName

instance Prelude.NFData ListModelCardExportJobs where
  rnf ListModelCardExportJobs' {..} =
    Prelude.rnf creationTimeAfter
      `Prelude.seq` Prelude.rnf creationTimeBefore
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf modelCardExportJobNameContains
      `Prelude.seq` Prelude.rnf modelCardVersion
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf sortBy
      `Prelude.seq` Prelude.rnf sortOrder
      `Prelude.seq` Prelude.rnf statusEquals
      `Prelude.seq` Prelude.rnf modelCardName

instance Data.ToHeaders ListModelCardExportJobs where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "SageMaker.ListModelCardExportJobs" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListModelCardExportJobs where
  toJSON ListModelCardExportJobs' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CreationTimeAfter" Data..=)
              Prelude.<$> creationTimeAfter,
            ("CreationTimeBefore" Data..=)
              Prelude.<$> creationTimeBefore,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("ModelCardExportJobNameContains" Data..=)
              Prelude.<$> modelCardExportJobNameContains,
            ("ModelCardVersion" Data..=)
              Prelude.<$> modelCardVersion,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            ("SortBy" Data..=) Prelude.<$> sortBy,
            ("SortOrder" Data..=) Prelude.<$> sortOrder,
            ("StatusEquals" Data..=) Prelude.<$> statusEquals,
            Prelude.Just
              ("ModelCardName" Data..= modelCardName)
          ]
      )

instance Data.ToPath ListModelCardExportJobs where
  toPath = Prelude.const "/"

instance Data.ToQuery ListModelCardExportJobs where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListModelCardExportJobsResponse' smart constructor.
data ListModelCardExportJobsResponse = ListModelCardExportJobsResponse'
  { -- | If the response is truncated, SageMaker returns this token. To retrieve
    -- the next set of model card export jobs, use it in the subsequent
    -- request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The summaries of the listed model card export jobs.
    modelCardExportJobSummaries :: [ModelCardExportJobSummary]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListModelCardExportJobsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listModelCardExportJobsResponse_nextToken' - If the response is truncated, SageMaker returns this token. To retrieve
-- the next set of model card export jobs, use it in the subsequent
-- request.
--
-- 'httpStatus', 'listModelCardExportJobsResponse_httpStatus' - The response's http status code.
--
-- 'modelCardExportJobSummaries', 'listModelCardExportJobsResponse_modelCardExportJobSummaries' - The summaries of the listed model card export jobs.
newListModelCardExportJobsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListModelCardExportJobsResponse
newListModelCardExportJobsResponse pHttpStatus_ =
  ListModelCardExportJobsResponse'
    { nextToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      modelCardExportJobSummaries =
        Prelude.mempty
    }

-- | If the response is truncated, SageMaker returns this token. To retrieve
-- the next set of model card export jobs, use it in the subsequent
-- request.
listModelCardExportJobsResponse_nextToken :: Lens.Lens' ListModelCardExportJobsResponse (Prelude.Maybe Prelude.Text)
listModelCardExportJobsResponse_nextToken = Lens.lens (\ListModelCardExportJobsResponse' {nextToken} -> nextToken) (\s@ListModelCardExportJobsResponse' {} a -> s {nextToken = a} :: ListModelCardExportJobsResponse)

-- | The response's http status code.
listModelCardExportJobsResponse_httpStatus :: Lens.Lens' ListModelCardExportJobsResponse Prelude.Int
listModelCardExportJobsResponse_httpStatus = Lens.lens (\ListModelCardExportJobsResponse' {httpStatus} -> httpStatus) (\s@ListModelCardExportJobsResponse' {} a -> s {httpStatus = a} :: ListModelCardExportJobsResponse)

-- | The summaries of the listed model card export jobs.
listModelCardExportJobsResponse_modelCardExportJobSummaries :: Lens.Lens' ListModelCardExportJobsResponse [ModelCardExportJobSummary]
listModelCardExportJobsResponse_modelCardExportJobSummaries = Lens.lens (\ListModelCardExportJobsResponse' {modelCardExportJobSummaries} -> modelCardExportJobSummaries) (\s@ListModelCardExportJobsResponse' {} a -> s {modelCardExportJobSummaries = a} :: ListModelCardExportJobsResponse) Prelude.. Lens.coerced

instance
  Prelude.NFData
    ListModelCardExportJobsResponse
  where
  rnf ListModelCardExportJobsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf modelCardExportJobSummaries
