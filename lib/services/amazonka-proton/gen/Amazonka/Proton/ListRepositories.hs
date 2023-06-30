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
-- Module      : Amazonka.Proton.ListRepositories
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List linked repositories with detail data.
--
-- This operation returns paginated results.
module Amazonka.Proton.ListRepositories
  ( -- * Creating a Request
    ListRepositories (..),
    newListRepositories,

    -- * Request Lenses
    listRepositories_maxResults,
    listRepositories_nextToken,

    -- * Destructuring the Response
    ListRepositoriesResponse (..),
    newListRepositoriesResponse,

    -- * Response Lenses
    listRepositoriesResponse_nextToken,
    listRepositoriesResponse_httpStatus,
    listRepositoriesResponse_repositories,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Proton.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListRepositories' smart constructor.
data ListRepositories = ListRepositories'
  { -- | The maximum number of repositories to list.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | A token that indicates the location of the next repository in the array
    -- of repositories, after the list of repositories previously requested.
    nextToken :: Prelude.Maybe Prelude.Text
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
-- 'maxResults', 'listRepositories_maxResults' - The maximum number of repositories to list.
--
-- 'nextToken', 'listRepositories_nextToken' - A token that indicates the location of the next repository in the array
-- of repositories, after the list of repositories previously requested.
newListRepositories ::
  ListRepositories
newListRepositories =
  ListRepositories'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | The maximum number of repositories to list.
listRepositories_maxResults :: Lens.Lens' ListRepositories (Prelude.Maybe Prelude.Natural)
listRepositories_maxResults = Lens.lens (\ListRepositories' {maxResults} -> maxResults) (\s@ListRepositories' {} a -> s {maxResults = a} :: ListRepositories)

-- | A token that indicates the location of the next repository in the array
-- of repositories, after the list of repositories previously requested.
listRepositories_nextToken :: Lens.Lens' ListRepositories (Prelude.Maybe Prelude.Text)
listRepositories_nextToken = Lens.lens (\ListRepositories' {nextToken} -> nextToken) (\s@ListRepositories' {} a -> s {nextToken = a} :: ListRepositories)

instance Core.AWSPager ListRepositories where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listRepositoriesResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        (rs Lens.^. listRepositoriesResponse_repositories) =
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
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..?> "repositories" Core..!@ Prelude.mempty)
      )

instance Prelude.Hashable ListRepositories where
  hashWithSalt _salt ListRepositories' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListRepositories where
  rnf ListRepositories' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders ListRepositories where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AwsProton20200720.ListRepositories" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListRepositories where
  toJSON ListRepositories' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("maxResults" Data..=) Prelude.<$> maxResults,
            ("nextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance Data.ToPath ListRepositories where
  toPath = Prelude.const "/"

instance Data.ToQuery ListRepositories where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListRepositoriesResponse' smart constructor.
data ListRepositoriesResponse = ListRepositoriesResponse'
  { -- | A token that indicates the location of the next repository in the array
    -- of repositories, after the current requested list of repositories.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | An array of repository links.
    repositories :: [RepositorySummary]
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
-- 'nextToken', 'listRepositoriesResponse_nextToken' - A token that indicates the location of the next repository in the array
-- of repositories, after the current requested list of repositories.
--
-- 'httpStatus', 'listRepositoriesResponse_httpStatus' - The response's http status code.
--
-- 'repositories', 'listRepositoriesResponse_repositories' - An array of repository links.
newListRepositoriesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListRepositoriesResponse
newListRepositoriesResponse pHttpStatus_ =
  ListRepositoriesResponse'
    { nextToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      repositories = Prelude.mempty
    }

-- | A token that indicates the location of the next repository in the array
-- of repositories, after the current requested list of repositories.
listRepositoriesResponse_nextToken :: Lens.Lens' ListRepositoriesResponse (Prelude.Maybe Prelude.Text)
listRepositoriesResponse_nextToken = Lens.lens (\ListRepositoriesResponse' {nextToken} -> nextToken) (\s@ListRepositoriesResponse' {} a -> s {nextToken = a} :: ListRepositoriesResponse)

-- | The response's http status code.
listRepositoriesResponse_httpStatus :: Lens.Lens' ListRepositoriesResponse Prelude.Int
listRepositoriesResponse_httpStatus = Lens.lens (\ListRepositoriesResponse' {httpStatus} -> httpStatus) (\s@ListRepositoriesResponse' {} a -> s {httpStatus = a} :: ListRepositoriesResponse)

-- | An array of repository links.
listRepositoriesResponse_repositories :: Lens.Lens' ListRepositoriesResponse [RepositorySummary]
listRepositoriesResponse_repositories = Lens.lens (\ListRepositoriesResponse' {repositories} -> repositories) (\s@ListRepositoriesResponse' {} a -> s {repositories = a} :: ListRepositoriesResponse) Prelude.. Lens.coerced

instance Prelude.NFData ListRepositoriesResponse where
  rnf ListRepositoriesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf repositories
