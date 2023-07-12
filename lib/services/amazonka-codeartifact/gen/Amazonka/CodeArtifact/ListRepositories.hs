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
-- Module      : Amazonka.CodeArtifact.ListRepositories
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of
-- <https://docs.aws.amazon.com/codeartifact/latest/APIReference/API_RepositorySummary.html RepositorySummary>
-- objects. Each @RepositorySummary@ contains information about a
-- repository in the specified Amazon Web Services account and that matches
-- the input parameters.
--
-- This operation returns paginated results.
module Amazonka.CodeArtifact.ListRepositories
  ( -- * Creating a Request
    ListRepositories (..),
    newListRepositories,

    -- * Request Lenses
    listRepositories_maxResults,
    listRepositories_nextToken,
    listRepositories_repositoryPrefix,

    -- * Destructuring the Response
    ListRepositoriesResponse (..),
    newListRepositoriesResponse,

    -- * Response Lenses
    listRepositoriesResponse_nextToken,
    listRepositoriesResponse_repositories,
    listRepositoriesResponse_httpStatus,
  )
where

import Amazonka.CodeArtifact.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListRepositories' smart constructor.
data ListRepositories = ListRepositories'
  { -- | The maximum number of results to return per page.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token for the next set of results. Use the value returned in the
    -- previous response in the next request to retrieve the next set of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A prefix used to filter returned repositories. Only repositories with
    -- names that start with @repositoryPrefix@ are returned.
    repositoryPrefix :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListRepositories' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listRepositories_maxResults' - The maximum number of results to return per page.
--
-- 'nextToken', 'listRepositories_nextToken' - The token for the next set of results. Use the value returned in the
-- previous response in the next request to retrieve the next set of
-- results.
--
-- 'repositoryPrefix', 'listRepositories_repositoryPrefix' - A prefix used to filter returned repositories. Only repositories with
-- names that start with @repositoryPrefix@ are returned.
newListRepositories ::
  ListRepositories
newListRepositories =
  ListRepositories'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      repositoryPrefix = Prelude.Nothing
    }

-- | The maximum number of results to return per page.
listRepositories_maxResults :: Lens.Lens' ListRepositories (Prelude.Maybe Prelude.Natural)
listRepositories_maxResults = Lens.lens (\ListRepositories' {maxResults} -> maxResults) (\s@ListRepositories' {} a -> s {maxResults = a} :: ListRepositories)

-- | The token for the next set of results. Use the value returned in the
-- previous response in the next request to retrieve the next set of
-- results.
listRepositories_nextToken :: Lens.Lens' ListRepositories (Prelude.Maybe Prelude.Text)
listRepositories_nextToken = Lens.lens (\ListRepositories' {nextToken} -> nextToken) (\s@ListRepositories' {} a -> s {nextToken = a} :: ListRepositories)

-- | A prefix used to filter returned repositories. Only repositories with
-- names that start with @repositoryPrefix@ are returned.
listRepositories_repositoryPrefix :: Lens.Lens' ListRepositories (Prelude.Maybe Prelude.Text)
listRepositories_repositoryPrefix = Lens.lens (\ListRepositories' {repositoryPrefix} -> repositoryPrefix) (\s@ListRepositories' {} a -> s {repositoryPrefix = a} :: ListRepositories)

instance Core.AWSPager ListRepositories where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listRepositoriesResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listRepositoriesResponse_repositories
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listRepositories_nextToken
          Lens..~ rs
          Lens.^? listRepositoriesResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListRepositories where
  type
    AWSResponse ListRepositories =
      ListRepositoriesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListRepositoriesResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (x Data..?> "repositories" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListRepositories where
  hashWithSalt _salt ListRepositories' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` repositoryPrefix

instance Prelude.NFData ListRepositories where
  rnf ListRepositories' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf repositoryPrefix

instance Data.ToHeaders ListRepositories where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListRepositories where
  toJSON = Prelude.const (Data.Object Prelude.mempty)

instance Data.ToPath ListRepositories where
  toPath = Prelude.const "/v1/repositories"

instance Data.ToQuery ListRepositories where
  toQuery ListRepositories' {..} =
    Prelude.mconcat
      [ "max-results" Data.=: maxResults,
        "next-token" Data.=: nextToken,
        "repository-prefix" Data.=: repositoryPrefix
      ]

-- | /See:/ 'newListRepositoriesResponse' smart constructor.
data ListRepositoriesResponse = ListRepositoriesResponse'
  { -- | If there are additional results, this is the token for the next set of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The returned list of
    -- <https://docs.aws.amazon.com/codeartifact/latest/APIReference/API_RepositorySummary.html RepositorySummary>
    -- objects.
    repositories :: Prelude.Maybe [RepositorySummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListRepositoriesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listRepositoriesResponse_nextToken' - If there are additional results, this is the token for the next set of
-- results.
--
-- 'repositories', 'listRepositoriesResponse_repositories' - The returned list of
-- <https://docs.aws.amazon.com/codeartifact/latest/APIReference/API_RepositorySummary.html RepositorySummary>
-- objects.
--
-- 'httpStatus', 'listRepositoriesResponse_httpStatus' - The response's http status code.
newListRepositoriesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListRepositoriesResponse
newListRepositoriesResponse pHttpStatus_ =
  ListRepositoriesResponse'
    { nextToken =
        Prelude.Nothing,
      repositories = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If there are additional results, this is the token for the next set of
-- results.
listRepositoriesResponse_nextToken :: Lens.Lens' ListRepositoriesResponse (Prelude.Maybe Prelude.Text)
listRepositoriesResponse_nextToken = Lens.lens (\ListRepositoriesResponse' {nextToken} -> nextToken) (\s@ListRepositoriesResponse' {} a -> s {nextToken = a} :: ListRepositoriesResponse)

-- | The returned list of
-- <https://docs.aws.amazon.com/codeartifact/latest/APIReference/API_RepositorySummary.html RepositorySummary>
-- objects.
listRepositoriesResponse_repositories :: Lens.Lens' ListRepositoriesResponse (Prelude.Maybe [RepositorySummary])
listRepositoriesResponse_repositories = Lens.lens (\ListRepositoriesResponse' {repositories} -> repositories) (\s@ListRepositoriesResponse' {} a -> s {repositories = a} :: ListRepositoriesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listRepositoriesResponse_httpStatus :: Lens.Lens' ListRepositoriesResponse Prelude.Int
listRepositoriesResponse_httpStatus = Lens.lens (\ListRepositoriesResponse' {httpStatus} -> httpStatus) (\s@ListRepositoriesResponse' {} a -> s {httpStatus = a} :: ListRepositoriesResponse)

instance Prelude.NFData ListRepositoriesResponse where
  rnf ListRepositoriesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf repositories
      `Prelude.seq` Prelude.rnf httpStatus
