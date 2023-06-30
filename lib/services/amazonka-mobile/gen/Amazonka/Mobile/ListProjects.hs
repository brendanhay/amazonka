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
-- Module      : Amazonka.Mobile.ListProjects
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists projects in AWS Mobile Hub.
--
-- This operation returns paginated results.
module Amazonka.Mobile.ListProjects
  ( -- * Creating a Request
    ListProjects (..),
    newListProjects,

    -- * Request Lenses
    listProjects_maxResults,
    listProjects_nextToken,

    -- * Destructuring the Response
    ListProjectsResponse (..),
    newListProjectsResponse,

    -- * Response Lenses
    listProjectsResponse_nextToken,
    listProjectsResponse_projects,
    listProjectsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Mobile.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Request structure used to request projects list in AWS Mobile Hub.
--
-- /See:/ 'newListProjects' smart constructor.
data ListProjects = ListProjects'
  { -- | Maximum number of records to list in a single response.
    maxResults :: Prelude.Maybe Prelude.Int,
    -- | Pagination token. Set to null to start listing projects from start. If
    -- non-null pagination token is returned in a result, then pass its value
    -- in here in another request to list more projects.
    nextToken :: Prelude.Maybe Prelude.Text
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
-- 'maxResults', 'listProjects_maxResults' - Maximum number of records to list in a single response.
--
-- 'nextToken', 'listProjects_nextToken' - Pagination token. Set to null to start listing projects from start. If
-- non-null pagination token is returned in a result, then pass its value
-- in here in another request to list more projects.
newListProjects ::
  ListProjects
newListProjects =
  ListProjects'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | Maximum number of records to list in a single response.
listProjects_maxResults :: Lens.Lens' ListProjects (Prelude.Maybe Prelude.Int)
listProjects_maxResults = Lens.lens (\ListProjects' {maxResults} -> maxResults) (\s@ListProjects' {} a -> s {maxResults = a} :: ListProjects)

-- | Pagination token. Set to null to start listing projects from start. If
-- non-null pagination token is returned in a result, then pass its value
-- in here in another request to list more projects.
listProjects_nextToken :: Lens.Lens' ListProjects (Prelude.Maybe Prelude.Text)
listProjects_nextToken = Lens.lens (\ListProjects' {nextToken} -> nextToken) (\s@ListProjects' {} a -> s {nextToken = a} :: ListProjects)

instance Core.AWSPager ListProjects where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listProjectsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listProjectsResponse_projects
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listProjects_nextToken
          Lens..~ rs
          Lens.^? listProjectsResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListProjects where
  type AWSResponse ListProjects = ListProjectsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListProjectsResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (x Data..?> "projects" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListProjects where
  hashWithSalt _salt ListProjects' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListProjects where
  rnf ListProjects' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders ListProjects where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListProjects where
  toPath = Prelude.const "/projects"

instance Data.ToQuery ListProjects where
  toQuery ListProjects' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken
      ]

-- | Result structure used for requests to list projects in AWS Mobile Hub.
--
-- /See:/ 'newListProjectsResponse' smart constructor.
data ListProjectsResponse = ListProjectsResponse'
  { nextToken :: Prelude.Maybe Prelude.Text,
    projects :: Prelude.Maybe [ProjectSummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
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
-- 'nextToken', 'listProjectsResponse_nextToken' - Undocumented member.
--
-- 'projects', 'listProjectsResponse_projects' - Undocumented member.
--
-- 'httpStatus', 'listProjectsResponse_httpStatus' - The response's http status code.
newListProjectsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListProjectsResponse
newListProjectsResponse pHttpStatus_ =
  ListProjectsResponse'
    { nextToken = Prelude.Nothing,
      projects = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
listProjectsResponse_nextToken :: Lens.Lens' ListProjectsResponse (Prelude.Maybe Prelude.Text)
listProjectsResponse_nextToken = Lens.lens (\ListProjectsResponse' {nextToken} -> nextToken) (\s@ListProjectsResponse' {} a -> s {nextToken = a} :: ListProjectsResponse)

-- | Undocumented member.
listProjectsResponse_projects :: Lens.Lens' ListProjectsResponse (Prelude.Maybe [ProjectSummary])
listProjectsResponse_projects = Lens.lens (\ListProjectsResponse' {projects} -> projects) (\s@ListProjectsResponse' {} a -> s {projects = a} :: ListProjectsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listProjectsResponse_httpStatus :: Lens.Lens' ListProjectsResponse Prelude.Int
listProjectsResponse_httpStatus = Lens.lens (\ListProjectsResponse' {httpStatus} -> httpStatus) (\s@ListProjectsResponse' {} a -> s {httpStatus = a} :: ListProjectsResponse)

instance Prelude.NFData ListProjectsResponse where
  rnf ListProjectsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf projects
      `Prelude.seq` Prelude.rnf httpStatus
