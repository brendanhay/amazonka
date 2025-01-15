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
-- Module      : Amazonka.SageMaker.ListTransformJobs
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists transform jobs.
--
-- This operation returns paginated results.
module Amazonka.SageMaker.ListTransformJobs
  ( -- * Creating a Request
    ListTransformJobs (..),
    newListTransformJobs,

    -- * Request Lenses
    listTransformJobs_creationTimeAfter,
    listTransformJobs_creationTimeBefore,
    listTransformJobs_lastModifiedTimeAfter,
    listTransformJobs_lastModifiedTimeBefore,
    listTransformJobs_maxResults,
    listTransformJobs_nameContains,
    listTransformJobs_nextToken,
    listTransformJobs_sortBy,
    listTransformJobs_sortOrder,
    listTransformJobs_statusEquals,

    -- * Destructuring the Response
    ListTransformJobsResponse (..),
    newListTransformJobsResponse,

    -- * Response Lenses
    listTransformJobsResponse_nextToken,
    listTransformJobsResponse_httpStatus,
    listTransformJobsResponse_transformJobSummaries,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newListTransformJobs' smart constructor.
data ListTransformJobs = ListTransformJobs'
  { -- | A filter that returns only transform jobs created after the specified
    -- time.
    creationTimeAfter :: Prelude.Maybe Data.POSIX,
    -- | A filter that returns only transform jobs created before the specified
    -- time.
    creationTimeBefore :: Prelude.Maybe Data.POSIX,
    -- | A filter that returns only transform jobs modified after the specified
    -- time.
    lastModifiedTimeAfter :: Prelude.Maybe Data.POSIX,
    -- | A filter that returns only transform jobs modified before the specified
    -- time.
    lastModifiedTimeBefore :: Prelude.Maybe Data.POSIX,
    -- | The maximum number of transform jobs to return in the response. The
    -- default value is @10@.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | A string in the transform job name. This filter returns only transform
    -- jobs whose name contains the specified string.
    nameContains :: Prelude.Maybe Prelude.Text,
    -- | If the result of the previous @ListTransformJobs@ request was truncated,
    -- the response includes a @NextToken@. To retrieve the next set of
    -- transform jobs, use the token in the next request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The field to sort results by. The default is @CreationTime@.
    sortBy :: Prelude.Maybe SortBy,
    -- | The sort order for results. The default is @Descending@.
    sortOrder :: Prelude.Maybe SortOrder,
    -- | A filter that retrieves only transform jobs with a specific status.
    statusEquals :: Prelude.Maybe TransformJobStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListTransformJobs' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTimeAfter', 'listTransformJobs_creationTimeAfter' - A filter that returns only transform jobs created after the specified
-- time.
--
-- 'creationTimeBefore', 'listTransformJobs_creationTimeBefore' - A filter that returns only transform jobs created before the specified
-- time.
--
-- 'lastModifiedTimeAfter', 'listTransformJobs_lastModifiedTimeAfter' - A filter that returns only transform jobs modified after the specified
-- time.
--
-- 'lastModifiedTimeBefore', 'listTransformJobs_lastModifiedTimeBefore' - A filter that returns only transform jobs modified before the specified
-- time.
--
-- 'maxResults', 'listTransformJobs_maxResults' - The maximum number of transform jobs to return in the response. The
-- default value is @10@.
--
-- 'nameContains', 'listTransformJobs_nameContains' - A string in the transform job name. This filter returns only transform
-- jobs whose name contains the specified string.
--
-- 'nextToken', 'listTransformJobs_nextToken' - If the result of the previous @ListTransformJobs@ request was truncated,
-- the response includes a @NextToken@. To retrieve the next set of
-- transform jobs, use the token in the next request.
--
-- 'sortBy', 'listTransformJobs_sortBy' - The field to sort results by. The default is @CreationTime@.
--
-- 'sortOrder', 'listTransformJobs_sortOrder' - The sort order for results. The default is @Descending@.
--
-- 'statusEquals', 'listTransformJobs_statusEquals' - A filter that retrieves only transform jobs with a specific status.
newListTransformJobs ::
  ListTransformJobs
newListTransformJobs =
  ListTransformJobs'
    { creationTimeAfter =
        Prelude.Nothing,
      creationTimeBefore = Prelude.Nothing,
      lastModifiedTimeAfter = Prelude.Nothing,
      lastModifiedTimeBefore = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nameContains = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      sortBy = Prelude.Nothing,
      sortOrder = Prelude.Nothing,
      statusEquals = Prelude.Nothing
    }

-- | A filter that returns only transform jobs created after the specified
-- time.
listTransformJobs_creationTimeAfter :: Lens.Lens' ListTransformJobs (Prelude.Maybe Prelude.UTCTime)
listTransformJobs_creationTimeAfter = Lens.lens (\ListTransformJobs' {creationTimeAfter} -> creationTimeAfter) (\s@ListTransformJobs' {} a -> s {creationTimeAfter = a} :: ListTransformJobs) Prelude.. Lens.mapping Data._Time

-- | A filter that returns only transform jobs created before the specified
-- time.
listTransformJobs_creationTimeBefore :: Lens.Lens' ListTransformJobs (Prelude.Maybe Prelude.UTCTime)
listTransformJobs_creationTimeBefore = Lens.lens (\ListTransformJobs' {creationTimeBefore} -> creationTimeBefore) (\s@ListTransformJobs' {} a -> s {creationTimeBefore = a} :: ListTransformJobs) Prelude.. Lens.mapping Data._Time

-- | A filter that returns only transform jobs modified after the specified
-- time.
listTransformJobs_lastModifiedTimeAfter :: Lens.Lens' ListTransformJobs (Prelude.Maybe Prelude.UTCTime)
listTransformJobs_lastModifiedTimeAfter = Lens.lens (\ListTransformJobs' {lastModifiedTimeAfter} -> lastModifiedTimeAfter) (\s@ListTransformJobs' {} a -> s {lastModifiedTimeAfter = a} :: ListTransformJobs) Prelude.. Lens.mapping Data._Time

-- | A filter that returns only transform jobs modified before the specified
-- time.
listTransformJobs_lastModifiedTimeBefore :: Lens.Lens' ListTransformJobs (Prelude.Maybe Prelude.UTCTime)
listTransformJobs_lastModifiedTimeBefore = Lens.lens (\ListTransformJobs' {lastModifiedTimeBefore} -> lastModifiedTimeBefore) (\s@ListTransformJobs' {} a -> s {lastModifiedTimeBefore = a} :: ListTransformJobs) Prelude.. Lens.mapping Data._Time

-- | The maximum number of transform jobs to return in the response. The
-- default value is @10@.
listTransformJobs_maxResults :: Lens.Lens' ListTransformJobs (Prelude.Maybe Prelude.Natural)
listTransformJobs_maxResults = Lens.lens (\ListTransformJobs' {maxResults} -> maxResults) (\s@ListTransformJobs' {} a -> s {maxResults = a} :: ListTransformJobs)

-- | A string in the transform job name. This filter returns only transform
-- jobs whose name contains the specified string.
listTransformJobs_nameContains :: Lens.Lens' ListTransformJobs (Prelude.Maybe Prelude.Text)
listTransformJobs_nameContains = Lens.lens (\ListTransformJobs' {nameContains} -> nameContains) (\s@ListTransformJobs' {} a -> s {nameContains = a} :: ListTransformJobs)

-- | If the result of the previous @ListTransformJobs@ request was truncated,
-- the response includes a @NextToken@. To retrieve the next set of
-- transform jobs, use the token in the next request.
listTransformJobs_nextToken :: Lens.Lens' ListTransformJobs (Prelude.Maybe Prelude.Text)
listTransformJobs_nextToken = Lens.lens (\ListTransformJobs' {nextToken} -> nextToken) (\s@ListTransformJobs' {} a -> s {nextToken = a} :: ListTransformJobs)

-- | The field to sort results by. The default is @CreationTime@.
listTransformJobs_sortBy :: Lens.Lens' ListTransformJobs (Prelude.Maybe SortBy)
listTransformJobs_sortBy = Lens.lens (\ListTransformJobs' {sortBy} -> sortBy) (\s@ListTransformJobs' {} a -> s {sortBy = a} :: ListTransformJobs)

-- | The sort order for results. The default is @Descending@.
listTransformJobs_sortOrder :: Lens.Lens' ListTransformJobs (Prelude.Maybe SortOrder)
listTransformJobs_sortOrder = Lens.lens (\ListTransformJobs' {sortOrder} -> sortOrder) (\s@ListTransformJobs' {} a -> s {sortOrder = a} :: ListTransformJobs)

-- | A filter that retrieves only transform jobs with a specific status.
listTransformJobs_statusEquals :: Lens.Lens' ListTransformJobs (Prelude.Maybe TransformJobStatus)
listTransformJobs_statusEquals = Lens.lens (\ListTransformJobs' {statusEquals} -> statusEquals) (\s@ListTransformJobs' {} a -> s {statusEquals = a} :: ListTransformJobs)

instance Core.AWSPager ListTransformJobs where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listTransformJobsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^. listTransformJobsResponse_transformJobSummaries
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just Prelude.$
          rq
            Prelude.& listTransformJobs_nextToken
              Lens..~ rs
              Lens.^? listTransformJobsResponse_nextToken
              Prelude.. Lens._Just

instance Core.AWSRequest ListTransformJobs where
  type
    AWSResponse ListTransformJobs =
      ListTransformJobsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListTransformJobsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x
                            Data..?> "TransformJobSummaries"
                            Core..!@ Prelude.mempty
                        )
      )

instance Prelude.Hashable ListTransformJobs where
  hashWithSalt _salt ListTransformJobs' {..} =
    _salt
      `Prelude.hashWithSalt` creationTimeAfter
      `Prelude.hashWithSalt` creationTimeBefore
      `Prelude.hashWithSalt` lastModifiedTimeAfter
      `Prelude.hashWithSalt` lastModifiedTimeBefore
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nameContains
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` sortBy
      `Prelude.hashWithSalt` sortOrder
      `Prelude.hashWithSalt` statusEquals

instance Prelude.NFData ListTransformJobs where
  rnf ListTransformJobs' {..} =
    Prelude.rnf creationTimeAfter `Prelude.seq`
      Prelude.rnf creationTimeBefore `Prelude.seq`
        Prelude.rnf lastModifiedTimeAfter `Prelude.seq`
          Prelude.rnf lastModifiedTimeBefore `Prelude.seq`
            Prelude.rnf maxResults `Prelude.seq`
              Prelude.rnf nameContains `Prelude.seq`
                Prelude.rnf nextToken `Prelude.seq`
                  Prelude.rnf sortBy `Prelude.seq`
                    Prelude.rnf sortOrder `Prelude.seq`
                      Prelude.rnf statusEquals

instance Data.ToHeaders ListTransformJobs where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "SageMaker.ListTransformJobs" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListTransformJobs where
  toJSON ListTransformJobs' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CreationTimeAfter" Data..=)
              Prelude.<$> creationTimeAfter,
            ("CreationTimeBefore" Data..=)
              Prelude.<$> creationTimeBefore,
            ("LastModifiedTimeAfter" Data..=)
              Prelude.<$> lastModifiedTimeAfter,
            ("LastModifiedTimeBefore" Data..=)
              Prelude.<$> lastModifiedTimeBefore,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NameContains" Data..=) Prelude.<$> nameContains,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            ("SortBy" Data..=) Prelude.<$> sortBy,
            ("SortOrder" Data..=) Prelude.<$> sortOrder,
            ("StatusEquals" Data..=) Prelude.<$> statusEquals
          ]
      )

instance Data.ToPath ListTransformJobs where
  toPath = Prelude.const "/"

instance Data.ToQuery ListTransformJobs where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListTransformJobsResponse' smart constructor.
data ListTransformJobsResponse = ListTransformJobsResponse'
  { -- | If the response is truncated, Amazon SageMaker returns this token. To
    -- retrieve the next set of transform jobs, use it in the next request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | An array of @TransformJobSummary@ objects.
    transformJobSummaries :: [TransformJobSummary]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListTransformJobsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listTransformJobsResponse_nextToken' - If the response is truncated, Amazon SageMaker returns this token. To
-- retrieve the next set of transform jobs, use it in the next request.
--
-- 'httpStatus', 'listTransformJobsResponse_httpStatus' - The response's http status code.
--
-- 'transformJobSummaries', 'listTransformJobsResponse_transformJobSummaries' - An array of @TransformJobSummary@ objects.
newListTransformJobsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListTransformJobsResponse
newListTransformJobsResponse pHttpStatus_ =
  ListTransformJobsResponse'
    { nextToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      transformJobSummaries = Prelude.mempty
    }

-- | If the response is truncated, Amazon SageMaker returns this token. To
-- retrieve the next set of transform jobs, use it in the next request.
listTransformJobsResponse_nextToken :: Lens.Lens' ListTransformJobsResponse (Prelude.Maybe Prelude.Text)
listTransformJobsResponse_nextToken = Lens.lens (\ListTransformJobsResponse' {nextToken} -> nextToken) (\s@ListTransformJobsResponse' {} a -> s {nextToken = a} :: ListTransformJobsResponse)

-- | The response's http status code.
listTransformJobsResponse_httpStatus :: Lens.Lens' ListTransformJobsResponse Prelude.Int
listTransformJobsResponse_httpStatus = Lens.lens (\ListTransformJobsResponse' {httpStatus} -> httpStatus) (\s@ListTransformJobsResponse' {} a -> s {httpStatus = a} :: ListTransformJobsResponse)

-- | An array of @TransformJobSummary@ objects.
listTransformJobsResponse_transformJobSummaries :: Lens.Lens' ListTransformJobsResponse [TransformJobSummary]
listTransformJobsResponse_transformJobSummaries = Lens.lens (\ListTransformJobsResponse' {transformJobSummaries} -> transformJobSummaries) (\s@ListTransformJobsResponse' {} a -> s {transformJobSummaries = a} :: ListTransformJobsResponse) Prelude.. Lens.coerced

instance Prelude.NFData ListTransformJobsResponse where
  rnf ListTransformJobsResponse' {..} =
    Prelude.rnf nextToken `Prelude.seq`
      Prelude.rnf httpStatus `Prelude.seq`
        Prelude.rnf transformJobSummaries
