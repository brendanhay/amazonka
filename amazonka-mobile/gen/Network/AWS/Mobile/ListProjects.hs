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
-- Module      : Network.AWS.Mobile.ListProjects
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists projects in AWS Mobile Hub.
--
-- This operation returns paginated results.
module Network.AWS.Mobile.ListProjects
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
    listProjectsResponse_projects,
    listProjectsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Mobile.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Request structure used to request projects list in AWS Mobile Hub.
--
-- /See:/ 'newListProjects' smart constructor.
data ListProjects = ListProjects'
  { -- | Pagination token. Set to null to start listing projects from start. If
    -- non-null pagination token is returned in a result, then pass its value
    -- in here in another request to list more projects.
    nextToken :: Core.Maybe Core.Text,
    -- | Maximum number of records to list in a single response.
    maxResults :: Core.Maybe Core.Int
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
-- 'nextToken', 'listProjects_nextToken' - Pagination token. Set to null to start listing projects from start. If
-- non-null pagination token is returned in a result, then pass its value
-- in here in another request to list more projects.
--
-- 'maxResults', 'listProjects_maxResults' - Maximum number of records to list in a single response.
newListProjects ::
  ListProjects
newListProjects =
  ListProjects'
    { nextToken = Core.Nothing,
      maxResults = Core.Nothing
    }

-- | Pagination token. Set to null to start listing projects from start. If
-- non-null pagination token is returned in a result, then pass its value
-- in here in another request to list more projects.
listProjects_nextToken :: Lens.Lens' ListProjects (Core.Maybe Core.Text)
listProjects_nextToken = Lens.lens (\ListProjects' {nextToken} -> nextToken) (\s@ListProjects' {} a -> s {nextToken = a} :: ListProjects)

-- | Maximum number of records to list in a single response.
listProjects_maxResults :: Lens.Lens' ListProjects (Core.Maybe Core.Int)
listProjects_maxResults = Lens.lens (\ListProjects' {maxResults} -> maxResults) (\s@ListProjects' {} a -> s {maxResults = a} :: ListProjects)

instance Core.AWSPager ListProjects where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listProjectsResponse_nextToken Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listProjectsResponse_projects Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listProjects_nextToken
          Lens..~ rs
          Lens.^? listProjectsResponse_nextToken Core.. Lens._Just

instance Core.AWSRequest ListProjects where
  type AWSResponse ListProjects = ListProjectsResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListProjectsResponse'
            Core.<$> (x Core..?> "nextToken")
            Core.<*> (x Core..?> "projects" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListProjects

instance Core.NFData ListProjects

instance Core.ToHeaders ListProjects where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToPath ListProjects where
  toPath = Core.const "/projects"

instance Core.ToQuery ListProjects where
  toQuery ListProjects' {..} =
    Core.mconcat
      [ "nextToken" Core.=: nextToken,
        "maxResults" Core.=: maxResults
      ]

-- | Result structure used for requests to list projects in AWS Mobile Hub.
--
-- /See:/ 'newListProjectsResponse' smart constructor.
data ListProjectsResponse = ListProjectsResponse'
  { nextToken :: Core.Maybe Core.Text,
    projects :: Core.Maybe [ProjectSummary],
    -- | The response's http status code.
    httpStatus :: Core.Int
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
-- 'nextToken', 'listProjectsResponse_nextToken' - Undocumented member.
--
-- 'projects', 'listProjectsResponse_projects' - Undocumented member.
--
-- 'httpStatus', 'listProjectsResponse_httpStatus' - The response's http status code.
newListProjectsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListProjectsResponse
newListProjectsResponse pHttpStatus_ =
  ListProjectsResponse'
    { nextToken = Core.Nothing,
      projects = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
listProjectsResponse_nextToken :: Lens.Lens' ListProjectsResponse (Core.Maybe Core.Text)
listProjectsResponse_nextToken = Lens.lens (\ListProjectsResponse' {nextToken} -> nextToken) (\s@ListProjectsResponse' {} a -> s {nextToken = a} :: ListProjectsResponse)

-- | Undocumented member.
listProjectsResponse_projects :: Lens.Lens' ListProjectsResponse (Core.Maybe [ProjectSummary])
listProjectsResponse_projects = Lens.lens (\ListProjectsResponse' {projects} -> projects) (\s@ListProjectsResponse' {} a -> s {projects = a} :: ListProjectsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listProjectsResponse_httpStatus :: Lens.Lens' ListProjectsResponse Core.Int
listProjectsResponse_httpStatus = Lens.lens (\ListProjectsResponse' {httpStatus} -> httpStatus) (\s@ListProjectsResponse' {} a -> s {httpStatus = a} :: ListProjectsResponse)

instance Core.NFData ListProjectsResponse
