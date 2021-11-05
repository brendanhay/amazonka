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
-- Module      : Amazonka.SageMaker.ListProjects
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of the projects in an Amazon Web Services account.
module Amazonka.SageMaker.ListProjects
  ( -- * Creating a Request
    ListProjects (..),
    newListProjects,

    -- * Request Lenses
    listProjects_nameContains,
    listProjects_creationTimeAfter,
    listProjects_nextToken,
    listProjects_sortOrder,
    listProjects_creationTimeBefore,
    listProjects_maxResults,
    listProjects_sortBy,

    -- * Destructuring the Response
    ListProjectsResponse (..),
    newListProjectsResponse,

    -- * Response Lenses
    listProjectsResponse_nextToken,
    listProjectsResponse_httpStatus,
    listProjectsResponse_projectSummaryList,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newListProjects' smart constructor.
data ListProjects = ListProjects'
  { -- | A filter that returns the projects whose name contains a specified
    -- string.
    nameContains :: Prelude.Maybe Prelude.Text,
    -- | A filter that returns the projects that were created after a specified
    -- time.
    creationTimeAfter :: Prelude.Maybe Core.POSIX,
    -- | If the result of the previous @ListProjects@ request was truncated, the
    -- response includes a @NextToken@. To retrieve the next set of projects,
    -- use the token in the next request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The sort order for results. The default is @Ascending@.
    sortOrder :: Prelude.Maybe ProjectSortOrder,
    -- | A filter that returns the projects that were created before a specified
    -- time.
    creationTimeBefore :: Prelude.Maybe Core.POSIX,
    -- | The maximum number of projects to return in the response.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The field by which to sort results. The default is @CreationTime@.
    sortBy :: Prelude.Maybe ProjectSortBy
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListProjects' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nameContains', 'listProjects_nameContains' - A filter that returns the projects whose name contains a specified
-- string.
--
-- 'creationTimeAfter', 'listProjects_creationTimeAfter' - A filter that returns the projects that were created after a specified
-- time.
--
-- 'nextToken', 'listProjects_nextToken' - If the result of the previous @ListProjects@ request was truncated, the
-- response includes a @NextToken@. To retrieve the next set of projects,
-- use the token in the next request.
--
-- 'sortOrder', 'listProjects_sortOrder' - The sort order for results. The default is @Ascending@.
--
-- 'creationTimeBefore', 'listProjects_creationTimeBefore' - A filter that returns the projects that were created before a specified
-- time.
--
-- 'maxResults', 'listProjects_maxResults' - The maximum number of projects to return in the response.
--
-- 'sortBy', 'listProjects_sortBy' - The field by which to sort results. The default is @CreationTime@.
newListProjects ::
  ListProjects
newListProjects =
  ListProjects'
    { nameContains = Prelude.Nothing,
      creationTimeAfter = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      sortOrder = Prelude.Nothing,
      creationTimeBefore = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      sortBy = Prelude.Nothing
    }

-- | A filter that returns the projects whose name contains a specified
-- string.
listProjects_nameContains :: Lens.Lens' ListProjects (Prelude.Maybe Prelude.Text)
listProjects_nameContains = Lens.lens (\ListProjects' {nameContains} -> nameContains) (\s@ListProjects' {} a -> s {nameContains = a} :: ListProjects)

-- | A filter that returns the projects that were created after a specified
-- time.
listProjects_creationTimeAfter :: Lens.Lens' ListProjects (Prelude.Maybe Prelude.UTCTime)
listProjects_creationTimeAfter = Lens.lens (\ListProjects' {creationTimeAfter} -> creationTimeAfter) (\s@ListProjects' {} a -> s {creationTimeAfter = a} :: ListProjects) Prelude.. Lens.mapping Core._Time

-- | If the result of the previous @ListProjects@ request was truncated, the
-- response includes a @NextToken@. To retrieve the next set of projects,
-- use the token in the next request.
listProjects_nextToken :: Lens.Lens' ListProjects (Prelude.Maybe Prelude.Text)
listProjects_nextToken = Lens.lens (\ListProjects' {nextToken} -> nextToken) (\s@ListProjects' {} a -> s {nextToken = a} :: ListProjects)

-- | The sort order for results. The default is @Ascending@.
listProjects_sortOrder :: Lens.Lens' ListProjects (Prelude.Maybe ProjectSortOrder)
listProjects_sortOrder = Lens.lens (\ListProjects' {sortOrder} -> sortOrder) (\s@ListProjects' {} a -> s {sortOrder = a} :: ListProjects)

-- | A filter that returns the projects that were created before a specified
-- time.
listProjects_creationTimeBefore :: Lens.Lens' ListProjects (Prelude.Maybe Prelude.UTCTime)
listProjects_creationTimeBefore = Lens.lens (\ListProjects' {creationTimeBefore} -> creationTimeBefore) (\s@ListProjects' {} a -> s {creationTimeBefore = a} :: ListProjects) Prelude.. Lens.mapping Core._Time

-- | The maximum number of projects to return in the response.
listProjects_maxResults :: Lens.Lens' ListProjects (Prelude.Maybe Prelude.Natural)
listProjects_maxResults = Lens.lens (\ListProjects' {maxResults} -> maxResults) (\s@ListProjects' {} a -> s {maxResults = a} :: ListProjects)

-- | The field by which to sort results. The default is @CreationTime@.
listProjects_sortBy :: Lens.Lens' ListProjects (Prelude.Maybe ProjectSortBy)
listProjects_sortBy = Lens.lens (\ListProjects' {sortBy} -> sortBy) (\s@ListProjects' {} a -> s {sortBy = a} :: ListProjects)

instance Core.AWSRequest ListProjects where
  type AWSResponse ListProjects = ListProjectsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListProjectsResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x Core..?> "ProjectSummaryList"
                            Core..!@ Prelude.mempty
                        )
      )

instance Prelude.Hashable ListProjects

instance Prelude.NFData ListProjects

instance Core.ToHeaders ListProjects where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ("SageMaker.ListProjects" :: Prelude.ByteString),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListProjects where
  toJSON ListProjects' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NameContains" Core..=) Prelude.<$> nameContains,
            ("CreationTimeAfter" Core..=)
              Prelude.<$> creationTimeAfter,
            ("NextToken" Core..=) Prelude.<$> nextToken,
            ("SortOrder" Core..=) Prelude.<$> sortOrder,
            ("CreationTimeBefore" Core..=)
              Prelude.<$> creationTimeBefore,
            ("MaxResults" Core..=) Prelude.<$> maxResults,
            ("SortBy" Core..=) Prelude.<$> sortBy
          ]
      )

instance Core.ToPath ListProjects where
  toPath = Prelude.const "/"

instance Core.ToQuery ListProjects where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListProjectsResponse' smart constructor.
data ListProjectsResponse = ListProjectsResponse'
  { -- | If the result of the previous @ListCompilationJobs@ request was
    -- truncated, the response includes a @NextToken@. To retrieve the next set
    -- of model compilation jobs, use the token in the next request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A list of summaries of projects.
    projectSummaryList :: [ProjectSummary]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListProjectsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listProjectsResponse_nextToken' - If the result of the previous @ListCompilationJobs@ request was
-- truncated, the response includes a @NextToken@. To retrieve the next set
-- of model compilation jobs, use the token in the next request.
--
-- 'httpStatus', 'listProjectsResponse_httpStatus' - The response's http status code.
--
-- 'projectSummaryList', 'listProjectsResponse_projectSummaryList' - A list of summaries of projects.
newListProjectsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListProjectsResponse
newListProjectsResponse pHttpStatus_ =
  ListProjectsResponse'
    { nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_,
      projectSummaryList = Prelude.mempty
    }

-- | If the result of the previous @ListCompilationJobs@ request was
-- truncated, the response includes a @NextToken@. To retrieve the next set
-- of model compilation jobs, use the token in the next request.
listProjectsResponse_nextToken :: Lens.Lens' ListProjectsResponse (Prelude.Maybe Prelude.Text)
listProjectsResponse_nextToken = Lens.lens (\ListProjectsResponse' {nextToken} -> nextToken) (\s@ListProjectsResponse' {} a -> s {nextToken = a} :: ListProjectsResponse)

-- | The response's http status code.
listProjectsResponse_httpStatus :: Lens.Lens' ListProjectsResponse Prelude.Int
listProjectsResponse_httpStatus = Lens.lens (\ListProjectsResponse' {httpStatus} -> httpStatus) (\s@ListProjectsResponse' {} a -> s {httpStatus = a} :: ListProjectsResponse)

-- | A list of summaries of projects.
listProjectsResponse_projectSummaryList :: Lens.Lens' ListProjectsResponse [ProjectSummary]
listProjectsResponse_projectSummaryList = Lens.lens (\ListProjectsResponse' {projectSummaryList} -> projectSummaryList) (\s@ListProjectsResponse' {} a -> s {projectSummaryList = a} :: ListProjectsResponse) Prelude.. Lens.coerced

instance Prelude.NFData ListProjectsResponse
