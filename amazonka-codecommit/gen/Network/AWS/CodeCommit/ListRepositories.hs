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
-- Module      : Network.AWS.CodeCommit.ListRepositories
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about one or more repositories.
--
-- This operation returns paginated results.
module Network.AWS.CodeCommit.ListRepositories
  ( -- * Creating a Request
    ListRepositories (..),
    newListRepositories,

    -- * Request Lenses
    listRepositories_nextToken,
    listRepositories_sortBy,
    listRepositories_order,

    -- * Destructuring the Response
    ListRepositoriesResponse (..),
    newListRepositoriesResponse,

    -- * Response Lenses
    listRepositoriesResponse_nextToken,
    listRepositoriesResponse_repositories,
    listRepositoriesResponse_httpStatus,
  )
where

import Network.AWS.CodeCommit.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a list repositories operation.
--
-- /See:/ 'newListRepositories' smart constructor.
data ListRepositories = ListRepositories'
  { -- | An enumeration token that allows the operation to batch the results of
    -- the operation. Batch sizes are 1,000 for list repository operations.
    -- When the client sends the token back to AWS CodeCommit, another page of
    -- 1,000 records is retrieved.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The criteria used to sort the results of a list repositories operation.
    sortBy :: Prelude.Maybe SortByEnum,
    -- | The order in which to sort the results of a list repositories operation.
    order :: Prelude.Maybe OrderEnum
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
-- 'nextToken', 'listRepositories_nextToken' - An enumeration token that allows the operation to batch the results of
-- the operation. Batch sizes are 1,000 for list repository operations.
-- When the client sends the token back to AWS CodeCommit, another page of
-- 1,000 records is retrieved.
--
-- 'sortBy', 'listRepositories_sortBy' - The criteria used to sort the results of a list repositories operation.
--
-- 'order', 'listRepositories_order' - The order in which to sort the results of a list repositories operation.
newListRepositories ::
  ListRepositories
newListRepositories =
  ListRepositories'
    { nextToken = Prelude.Nothing,
      sortBy = Prelude.Nothing,
      order = Prelude.Nothing
    }

-- | An enumeration token that allows the operation to batch the results of
-- the operation. Batch sizes are 1,000 for list repository operations.
-- When the client sends the token back to AWS CodeCommit, another page of
-- 1,000 records is retrieved.
listRepositories_nextToken :: Lens.Lens' ListRepositories (Prelude.Maybe Prelude.Text)
listRepositories_nextToken = Lens.lens (\ListRepositories' {nextToken} -> nextToken) (\s@ListRepositories' {} a -> s {nextToken = a} :: ListRepositories)

-- | The criteria used to sort the results of a list repositories operation.
listRepositories_sortBy :: Lens.Lens' ListRepositories (Prelude.Maybe SortByEnum)
listRepositories_sortBy = Lens.lens (\ListRepositories' {sortBy} -> sortBy) (\s@ListRepositories' {} a -> s {sortBy = a} :: ListRepositories)

-- | The order in which to sort the results of a list repositories operation.
listRepositories_order :: Lens.Lens' ListRepositories (Prelude.Maybe OrderEnum)
listRepositories_order = Lens.lens (\ListRepositories' {order} -> order) (\s@ListRepositories' {} a -> s {order = a} :: ListRepositories)

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
      Prelude.Just Prelude.$
        rq
          Prelude.& listRepositories_nextToken
          Lens..~ rs
          Lens.^? listRepositoriesResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListRepositories where
  type
    AWSResponse ListRepositories =
      ListRepositoriesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListRepositoriesResponse'
            Prelude.<$> (x Core..?> "nextToken")
            Prelude.<*> (x Core..?> "repositories" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListRepositories

instance Prelude.NFData ListRepositories

instance Core.ToHeaders ListRepositories where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "CodeCommit_20150413.ListRepositories" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListRepositories where
  toJSON ListRepositories' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("nextToken" Core..=) Prelude.<$> nextToken,
            ("sortBy" Core..=) Prelude.<$> sortBy,
            ("order" Core..=) Prelude.<$> order
          ]
      )

instance Core.ToPath ListRepositories where
  toPath = Prelude.const "/"

instance Core.ToQuery ListRepositories where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the output of a list repositories operation.
--
-- /See:/ 'newListRepositoriesResponse' smart constructor.
data ListRepositoriesResponse = ListRepositoriesResponse'
  { -- | An enumeration token that allows the operation to batch the results of
    -- the operation. Batch sizes are 1,000 for list repository operations.
    -- When the client sends the token back to AWS CodeCommit, another page of
    -- 1,000 records is retrieved.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Lists the repositories called by the list repositories operation.
    repositories :: Prelude.Maybe [RepositoryNameIdPair],
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
-- 'nextToken', 'listRepositoriesResponse_nextToken' - An enumeration token that allows the operation to batch the results of
-- the operation. Batch sizes are 1,000 for list repository operations.
-- When the client sends the token back to AWS CodeCommit, another page of
-- 1,000 records is retrieved.
--
-- 'repositories', 'listRepositoriesResponse_repositories' - Lists the repositories called by the list repositories operation.
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

-- | An enumeration token that allows the operation to batch the results of
-- the operation. Batch sizes are 1,000 for list repository operations.
-- When the client sends the token back to AWS CodeCommit, another page of
-- 1,000 records is retrieved.
listRepositoriesResponse_nextToken :: Lens.Lens' ListRepositoriesResponse (Prelude.Maybe Prelude.Text)
listRepositoriesResponse_nextToken = Lens.lens (\ListRepositoriesResponse' {nextToken} -> nextToken) (\s@ListRepositoriesResponse' {} a -> s {nextToken = a} :: ListRepositoriesResponse)

-- | Lists the repositories called by the list repositories operation.
listRepositoriesResponse_repositories :: Lens.Lens' ListRepositoriesResponse (Prelude.Maybe [RepositoryNameIdPair])
listRepositoriesResponse_repositories = Lens.lens (\ListRepositoriesResponse' {repositories} -> repositories) (\s@ListRepositoriesResponse' {} a -> s {repositories = a} :: ListRepositoriesResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listRepositoriesResponse_httpStatus :: Lens.Lens' ListRepositoriesResponse Prelude.Int
listRepositoriesResponse_httpStatus = Lens.lens (\ListRepositoriesResponse' {httpStatus} -> httpStatus) (\s@ListRepositoriesResponse' {} a -> s {httpStatus = a} :: ListRepositoriesResponse)

instance Prelude.NFData ListRepositoriesResponse
