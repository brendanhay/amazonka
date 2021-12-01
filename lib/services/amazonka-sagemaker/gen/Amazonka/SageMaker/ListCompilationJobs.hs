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
-- Module      : Amazonka.SageMaker.ListCompilationJobs
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists model compilation jobs that satisfy various filters.
--
-- To create a model compilation job, use CreateCompilationJob. To get
-- information about a particular model compilation job you have created,
-- use DescribeCompilationJob.
--
-- This operation returns paginated results.
module Amazonka.SageMaker.ListCompilationJobs
  ( -- * Creating a Request
    ListCompilationJobs (..),
    newListCompilationJobs,

    -- * Request Lenses
    listCompilationJobs_nameContains,
    listCompilationJobs_lastModifiedTimeBefore,
    listCompilationJobs_creationTimeAfter,
    listCompilationJobs_nextToken,
    listCompilationJobs_sortOrder,
    listCompilationJobs_lastModifiedTimeAfter,
    listCompilationJobs_creationTimeBefore,
    listCompilationJobs_statusEquals,
    listCompilationJobs_maxResults,
    listCompilationJobs_sortBy,

    -- * Destructuring the Response
    ListCompilationJobsResponse (..),
    newListCompilationJobsResponse,

    -- * Response Lenses
    listCompilationJobsResponse_nextToken,
    listCompilationJobsResponse_httpStatus,
    listCompilationJobsResponse_compilationJobSummaries,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newListCompilationJobs' smart constructor.
data ListCompilationJobs = ListCompilationJobs'
  { -- | A filter that returns the model compilation jobs whose name contains a
    -- specified string.
    nameContains :: Prelude.Maybe Prelude.Text,
    -- | A filter that returns the model compilation jobs that were modified
    -- before a specified time.
    lastModifiedTimeBefore :: Prelude.Maybe Core.POSIX,
    -- | A filter that returns the model compilation jobs that were created after
    -- a specified time.
    creationTimeAfter :: Prelude.Maybe Core.POSIX,
    -- | If the result of the previous @ListCompilationJobs@ request was
    -- truncated, the response includes a @NextToken@. To retrieve the next set
    -- of model compilation jobs, use the token in the next request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The sort order for results. The default is @Ascending@.
    sortOrder :: Prelude.Maybe SortOrder,
    -- | A filter that returns the model compilation jobs that were modified
    -- after a specified time.
    lastModifiedTimeAfter :: Prelude.Maybe Core.POSIX,
    -- | A filter that returns the model compilation jobs that were created
    -- before a specified time.
    creationTimeBefore :: Prelude.Maybe Core.POSIX,
    -- | A filter that retrieves model compilation jobs with a specific
    -- DescribeCompilationJobResponse$CompilationJobStatus status.
    statusEquals :: Prelude.Maybe CompilationJobStatus,
    -- | The maximum number of model compilation jobs to return in the response.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The field by which to sort results. The default is @CreationTime@.
    sortBy :: Prelude.Maybe ListCompilationJobsSortBy
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListCompilationJobs' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nameContains', 'listCompilationJobs_nameContains' - A filter that returns the model compilation jobs whose name contains a
-- specified string.
--
-- 'lastModifiedTimeBefore', 'listCompilationJobs_lastModifiedTimeBefore' - A filter that returns the model compilation jobs that were modified
-- before a specified time.
--
-- 'creationTimeAfter', 'listCompilationJobs_creationTimeAfter' - A filter that returns the model compilation jobs that were created after
-- a specified time.
--
-- 'nextToken', 'listCompilationJobs_nextToken' - If the result of the previous @ListCompilationJobs@ request was
-- truncated, the response includes a @NextToken@. To retrieve the next set
-- of model compilation jobs, use the token in the next request.
--
-- 'sortOrder', 'listCompilationJobs_sortOrder' - The sort order for results. The default is @Ascending@.
--
-- 'lastModifiedTimeAfter', 'listCompilationJobs_lastModifiedTimeAfter' - A filter that returns the model compilation jobs that were modified
-- after a specified time.
--
-- 'creationTimeBefore', 'listCompilationJobs_creationTimeBefore' - A filter that returns the model compilation jobs that were created
-- before a specified time.
--
-- 'statusEquals', 'listCompilationJobs_statusEquals' - A filter that retrieves model compilation jobs with a specific
-- DescribeCompilationJobResponse$CompilationJobStatus status.
--
-- 'maxResults', 'listCompilationJobs_maxResults' - The maximum number of model compilation jobs to return in the response.
--
-- 'sortBy', 'listCompilationJobs_sortBy' - The field by which to sort results. The default is @CreationTime@.
newListCompilationJobs ::
  ListCompilationJobs
newListCompilationJobs =
  ListCompilationJobs'
    { nameContains =
        Prelude.Nothing,
      lastModifiedTimeBefore = Prelude.Nothing,
      creationTimeAfter = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      sortOrder = Prelude.Nothing,
      lastModifiedTimeAfter = Prelude.Nothing,
      creationTimeBefore = Prelude.Nothing,
      statusEquals = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      sortBy = Prelude.Nothing
    }

-- | A filter that returns the model compilation jobs whose name contains a
-- specified string.
listCompilationJobs_nameContains :: Lens.Lens' ListCompilationJobs (Prelude.Maybe Prelude.Text)
listCompilationJobs_nameContains = Lens.lens (\ListCompilationJobs' {nameContains} -> nameContains) (\s@ListCompilationJobs' {} a -> s {nameContains = a} :: ListCompilationJobs)

-- | A filter that returns the model compilation jobs that were modified
-- before a specified time.
listCompilationJobs_lastModifiedTimeBefore :: Lens.Lens' ListCompilationJobs (Prelude.Maybe Prelude.UTCTime)
listCompilationJobs_lastModifiedTimeBefore = Lens.lens (\ListCompilationJobs' {lastModifiedTimeBefore} -> lastModifiedTimeBefore) (\s@ListCompilationJobs' {} a -> s {lastModifiedTimeBefore = a} :: ListCompilationJobs) Prelude.. Lens.mapping Core._Time

-- | A filter that returns the model compilation jobs that were created after
-- a specified time.
listCompilationJobs_creationTimeAfter :: Lens.Lens' ListCompilationJobs (Prelude.Maybe Prelude.UTCTime)
listCompilationJobs_creationTimeAfter = Lens.lens (\ListCompilationJobs' {creationTimeAfter} -> creationTimeAfter) (\s@ListCompilationJobs' {} a -> s {creationTimeAfter = a} :: ListCompilationJobs) Prelude.. Lens.mapping Core._Time

-- | If the result of the previous @ListCompilationJobs@ request was
-- truncated, the response includes a @NextToken@. To retrieve the next set
-- of model compilation jobs, use the token in the next request.
listCompilationJobs_nextToken :: Lens.Lens' ListCompilationJobs (Prelude.Maybe Prelude.Text)
listCompilationJobs_nextToken = Lens.lens (\ListCompilationJobs' {nextToken} -> nextToken) (\s@ListCompilationJobs' {} a -> s {nextToken = a} :: ListCompilationJobs)

-- | The sort order for results. The default is @Ascending@.
listCompilationJobs_sortOrder :: Lens.Lens' ListCompilationJobs (Prelude.Maybe SortOrder)
listCompilationJobs_sortOrder = Lens.lens (\ListCompilationJobs' {sortOrder} -> sortOrder) (\s@ListCompilationJobs' {} a -> s {sortOrder = a} :: ListCompilationJobs)

-- | A filter that returns the model compilation jobs that were modified
-- after a specified time.
listCompilationJobs_lastModifiedTimeAfter :: Lens.Lens' ListCompilationJobs (Prelude.Maybe Prelude.UTCTime)
listCompilationJobs_lastModifiedTimeAfter = Lens.lens (\ListCompilationJobs' {lastModifiedTimeAfter} -> lastModifiedTimeAfter) (\s@ListCompilationJobs' {} a -> s {lastModifiedTimeAfter = a} :: ListCompilationJobs) Prelude.. Lens.mapping Core._Time

-- | A filter that returns the model compilation jobs that were created
-- before a specified time.
listCompilationJobs_creationTimeBefore :: Lens.Lens' ListCompilationJobs (Prelude.Maybe Prelude.UTCTime)
listCompilationJobs_creationTimeBefore = Lens.lens (\ListCompilationJobs' {creationTimeBefore} -> creationTimeBefore) (\s@ListCompilationJobs' {} a -> s {creationTimeBefore = a} :: ListCompilationJobs) Prelude.. Lens.mapping Core._Time

-- | A filter that retrieves model compilation jobs with a specific
-- DescribeCompilationJobResponse$CompilationJobStatus status.
listCompilationJobs_statusEquals :: Lens.Lens' ListCompilationJobs (Prelude.Maybe CompilationJobStatus)
listCompilationJobs_statusEquals = Lens.lens (\ListCompilationJobs' {statusEquals} -> statusEquals) (\s@ListCompilationJobs' {} a -> s {statusEquals = a} :: ListCompilationJobs)

-- | The maximum number of model compilation jobs to return in the response.
listCompilationJobs_maxResults :: Lens.Lens' ListCompilationJobs (Prelude.Maybe Prelude.Natural)
listCompilationJobs_maxResults = Lens.lens (\ListCompilationJobs' {maxResults} -> maxResults) (\s@ListCompilationJobs' {} a -> s {maxResults = a} :: ListCompilationJobs)

-- | The field by which to sort results. The default is @CreationTime@.
listCompilationJobs_sortBy :: Lens.Lens' ListCompilationJobs (Prelude.Maybe ListCompilationJobsSortBy)
listCompilationJobs_sortBy = Lens.lens (\ListCompilationJobs' {sortBy} -> sortBy) (\s@ListCompilationJobs' {} a -> s {sortBy = a} :: ListCompilationJobs)

instance Core.AWSPager ListCompilationJobs where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listCompilationJobsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^. listCompilationJobsResponse_compilationJobSummaries
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listCompilationJobs_nextToken
          Lens..~ rs
          Lens.^? listCompilationJobsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListCompilationJobs where
  type
    AWSResponse ListCompilationJobs =
      ListCompilationJobsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListCompilationJobsResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x Core..?> "CompilationJobSummaries"
                            Core..!@ Prelude.mempty
                        )
      )

instance Prelude.Hashable ListCompilationJobs where
  hashWithSalt salt' ListCompilationJobs' {..} =
    salt' `Prelude.hashWithSalt` sortBy
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` statusEquals
      `Prelude.hashWithSalt` creationTimeBefore
      `Prelude.hashWithSalt` lastModifiedTimeAfter
      `Prelude.hashWithSalt` sortOrder
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` creationTimeAfter
      `Prelude.hashWithSalt` lastModifiedTimeBefore
      `Prelude.hashWithSalt` nameContains

instance Prelude.NFData ListCompilationJobs where
  rnf ListCompilationJobs' {..} =
    Prelude.rnf nameContains
      `Prelude.seq` Prelude.rnf sortBy
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf statusEquals
      `Prelude.seq` Prelude.rnf creationTimeBefore
      `Prelude.seq` Prelude.rnf lastModifiedTimeAfter
      `Prelude.seq` Prelude.rnf sortOrder
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf creationTimeAfter
      `Prelude.seq` Prelude.rnf lastModifiedTimeBefore

instance Core.ToHeaders ListCompilationJobs where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "SageMaker.ListCompilationJobs" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListCompilationJobs where
  toJSON ListCompilationJobs' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NameContains" Core..=) Prelude.<$> nameContains,
            ("LastModifiedTimeBefore" Core..=)
              Prelude.<$> lastModifiedTimeBefore,
            ("CreationTimeAfter" Core..=)
              Prelude.<$> creationTimeAfter,
            ("NextToken" Core..=) Prelude.<$> nextToken,
            ("SortOrder" Core..=) Prelude.<$> sortOrder,
            ("LastModifiedTimeAfter" Core..=)
              Prelude.<$> lastModifiedTimeAfter,
            ("CreationTimeBefore" Core..=)
              Prelude.<$> creationTimeBefore,
            ("StatusEquals" Core..=) Prelude.<$> statusEquals,
            ("MaxResults" Core..=) Prelude.<$> maxResults,
            ("SortBy" Core..=) Prelude.<$> sortBy
          ]
      )

instance Core.ToPath ListCompilationJobs where
  toPath = Prelude.const "/"

instance Core.ToQuery ListCompilationJobs where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListCompilationJobsResponse' smart constructor.
data ListCompilationJobsResponse = ListCompilationJobsResponse'
  { -- | If the response is truncated, Amazon SageMaker returns this @NextToken@.
    -- To retrieve the next set of model compilation jobs, use this token in
    -- the next request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | An array of CompilationJobSummary objects, each describing a model
    -- compilation job.
    compilationJobSummaries :: [CompilationJobSummary]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListCompilationJobsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listCompilationJobsResponse_nextToken' - If the response is truncated, Amazon SageMaker returns this @NextToken@.
-- To retrieve the next set of model compilation jobs, use this token in
-- the next request.
--
-- 'httpStatus', 'listCompilationJobsResponse_httpStatus' - The response's http status code.
--
-- 'compilationJobSummaries', 'listCompilationJobsResponse_compilationJobSummaries' - An array of CompilationJobSummary objects, each describing a model
-- compilation job.
newListCompilationJobsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListCompilationJobsResponse
newListCompilationJobsResponse pHttpStatus_ =
  ListCompilationJobsResponse'
    { nextToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      compilationJobSummaries = Prelude.mempty
    }

-- | If the response is truncated, Amazon SageMaker returns this @NextToken@.
-- To retrieve the next set of model compilation jobs, use this token in
-- the next request.
listCompilationJobsResponse_nextToken :: Lens.Lens' ListCompilationJobsResponse (Prelude.Maybe Prelude.Text)
listCompilationJobsResponse_nextToken = Lens.lens (\ListCompilationJobsResponse' {nextToken} -> nextToken) (\s@ListCompilationJobsResponse' {} a -> s {nextToken = a} :: ListCompilationJobsResponse)

-- | The response's http status code.
listCompilationJobsResponse_httpStatus :: Lens.Lens' ListCompilationJobsResponse Prelude.Int
listCompilationJobsResponse_httpStatus = Lens.lens (\ListCompilationJobsResponse' {httpStatus} -> httpStatus) (\s@ListCompilationJobsResponse' {} a -> s {httpStatus = a} :: ListCompilationJobsResponse)

-- | An array of CompilationJobSummary objects, each describing a model
-- compilation job.
listCompilationJobsResponse_compilationJobSummaries :: Lens.Lens' ListCompilationJobsResponse [CompilationJobSummary]
listCompilationJobsResponse_compilationJobSummaries = Lens.lens (\ListCompilationJobsResponse' {compilationJobSummaries} -> compilationJobSummaries) (\s@ListCompilationJobsResponse' {} a -> s {compilationJobSummaries = a} :: ListCompilationJobsResponse) Prelude.. Lens.coerced

instance Prelude.NFData ListCompilationJobsResponse where
  rnf ListCompilationJobsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf compilationJobSummaries
      `Prelude.seq` Prelude.rnf httpStatus
