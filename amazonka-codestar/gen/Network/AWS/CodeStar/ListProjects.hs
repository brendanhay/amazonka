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
-- Module      : Network.AWS.CodeStar.ListProjects
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all projects in AWS CodeStar associated with your AWS account.
--
-- This operation returns paginated results.
module Network.AWS.CodeStar.ListProjects
  ( -- * Creating a Request
    ListProjects (..),
    newListProjects,

    -- * Request Lenses
    listProjects_nextToken,
    listProjects_maxResults,

    -- * Destructuring the Response
    ListProjectsResponse (..),
    newListProjectsResponse,

    -- * Response Lenses
    listProjectsResponse_nextToken,
    listProjectsResponse_httpStatus,
    listProjectsResponse_projects,
  )
where

import Network.AWS.CodeStar.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListProjects' smart constructor.
data ListProjects = ListProjects'
  { -- | The continuation token to be used to return the next set of results, if
    -- the results cannot be returned in one response.
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum amount of data that can be contained in a single set of
    -- results.
    maxResults :: Core.Maybe Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListProjects' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listProjects_nextToken' - The continuation token to be used to return the next set of results, if
-- the results cannot be returned in one response.
--
-- 'maxResults', 'listProjects_maxResults' - The maximum amount of data that can be contained in a single set of
-- results.
newListProjects ::
  ListProjects
newListProjects =
  ListProjects'
    { nextToken = Core.Nothing,
      maxResults = Core.Nothing
    }

-- | The continuation token to be used to return the next set of results, if
-- the results cannot be returned in one response.
listProjects_nextToken :: Lens.Lens' ListProjects (Core.Maybe Core.Text)
listProjects_nextToken = Lens.lens (\ListProjects' {nextToken} -> nextToken) (\s@ListProjects' {} a -> s {nextToken = a} :: ListProjects)

-- | The maximum amount of data that can be contained in a single set of
-- results.
listProjects_maxResults :: Lens.Lens' ListProjects (Core.Maybe Core.Natural)
listProjects_maxResults = Lens.lens (\ListProjects' {maxResults} -> maxResults) (\s@ListProjects' {} a -> s {maxResults = a} :: ListProjects)

instance Core.AWSPager ListProjects where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listProjectsResponse_nextToken Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        (rs Lens.^. listProjectsResponse_projects) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listProjects_nextToken
          Lens..~ rs
          Lens.^? listProjectsResponse_nextToken Core.. Lens._Just

instance Core.AWSRequest ListProjects where
  type AWSResponse ListProjects = ListProjectsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListProjectsResponse'
            Core.<$> (x Core..?> "nextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
            Core.<*> (x Core..?> "projects" Core..!@ Core.mempty)
      )

instance Core.Hashable ListProjects

instance Core.NFData ListProjects

instance Core.ToHeaders ListProjects where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "CodeStar_20170419.ListProjects" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListProjects where
  toJSON ListProjects' {..} =
    Core.object
      ( Core.catMaybes
          [ ("nextToken" Core..=) Core.<$> nextToken,
            ("maxResults" Core..=) Core.<$> maxResults
          ]
      )

instance Core.ToPath ListProjects where
  toPath = Core.const "/"

instance Core.ToQuery ListProjects where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListProjectsResponse' smart constructor.
data ListProjectsResponse = ListProjectsResponse'
  { -- | The continuation token to use when requesting the next set of results,
    -- if there are more results to be returned.
    nextToken :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int,
    -- | A list of projects.
    projects :: [ProjectSummary]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListProjectsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listProjectsResponse_nextToken' - The continuation token to use when requesting the next set of results,
-- if there are more results to be returned.
--
-- 'httpStatus', 'listProjectsResponse_httpStatus' - The response's http status code.
--
-- 'projects', 'listProjectsResponse_projects' - A list of projects.
newListProjectsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListProjectsResponse
newListProjectsResponse pHttpStatus_ =
  ListProjectsResponse'
    { nextToken = Core.Nothing,
      httpStatus = pHttpStatus_,
      projects = Core.mempty
    }

-- | The continuation token to use when requesting the next set of results,
-- if there are more results to be returned.
listProjectsResponse_nextToken :: Lens.Lens' ListProjectsResponse (Core.Maybe Core.Text)
listProjectsResponse_nextToken = Lens.lens (\ListProjectsResponse' {nextToken} -> nextToken) (\s@ListProjectsResponse' {} a -> s {nextToken = a} :: ListProjectsResponse)

-- | The response's http status code.
listProjectsResponse_httpStatus :: Lens.Lens' ListProjectsResponse Core.Int
listProjectsResponse_httpStatus = Lens.lens (\ListProjectsResponse' {httpStatus} -> httpStatus) (\s@ListProjectsResponse' {} a -> s {httpStatus = a} :: ListProjectsResponse)

-- | A list of projects.
listProjectsResponse_projects :: Lens.Lens' ListProjectsResponse [ProjectSummary]
listProjectsResponse_projects = Lens.lens (\ListProjectsResponse' {projects} -> projects) (\s@ListProjectsResponse' {} a -> s {projects = a} :: ListProjectsResponse) Core.. Lens._Coerce

instance Core.NFData ListProjectsResponse
