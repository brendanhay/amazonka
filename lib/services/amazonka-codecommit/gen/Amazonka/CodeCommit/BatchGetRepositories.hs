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
-- Module      : Amazonka.CodeCommit.BatchGetRepositories
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about one or more repositories.
--
-- The description field for a repository accepts all HTML characters and
-- all valid Unicode characters. Applications that do not HTML-encode the
-- description and display it in a webpage can expose users to potentially
-- malicious code. Make sure that you HTML-encode the description field in
-- any application that uses this API to display the repository description
-- on a webpage.
module Amazonka.CodeCommit.BatchGetRepositories
  ( -- * Creating a Request
    BatchGetRepositories (..),
    newBatchGetRepositories,

    -- * Request Lenses
    batchGetRepositories_repositoryNames,

    -- * Destructuring the Response
    BatchGetRepositoriesResponse (..),
    newBatchGetRepositoriesResponse,

    -- * Response Lenses
    batchGetRepositoriesResponse_repositories,
    batchGetRepositoriesResponse_repositoriesNotFound,
    batchGetRepositoriesResponse_httpStatus,
  )
where

import Amazonka.CodeCommit.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Represents the input of a batch get repositories operation.
--
-- /See:/ 'newBatchGetRepositories' smart constructor.
data BatchGetRepositories = BatchGetRepositories'
  { -- | The names of the repositories to get information about.
    --
    -- The length constraint limit is for each string in the array. The array
    -- itself can be empty.
    repositoryNames :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchGetRepositories' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'repositoryNames', 'batchGetRepositories_repositoryNames' - The names of the repositories to get information about.
--
-- The length constraint limit is for each string in the array. The array
-- itself can be empty.
newBatchGetRepositories ::
  BatchGetRepositories
newBatchGetRepositories =
  BatchGetRepositories'
    { repositoryNames =
        Prelude.mempty
    }

-- | The names of the repositories to get information about.
--
-- The length constraint limit is for each string in the array. The array
-- itself can be empty.
batchGetRepositories_repositoryNames :: Lens.Lens' BatchGetRepositories [Prelude.Text]
batchGetRepositories_repositoryNames = Lens.lens (\BatchGetRepositories' {repositoryNames} -> repositoryNames) (\s@BatchGetRepositories' {} a -> s {repositoryNames = a} :: BatchGetRepositories) Prelude.. Lens.coerced

instance Core.AWSRequest BatchGetRepositories where
  type
    AWSResponse BatchGetRepositories =
      BatchGetRepositoriesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchGetRepositoriesResponse'
            Prelude.<$> (x Data..?> "repositories" Core..!@ Prelude.mempty)
            Prelude.<*> ( x Data..?> "repositoriesNotFound"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable BatchGetRepositories where
  hashWithSalt _salt BatchGetRepositories' {..} =
    _salt `Prelude.hashWithSalt` repositoryNames

instance Prelude.NFData BatchGetRepositories where
  rnf BatchGetRepositories' {..} =
    Prelude.rnf repositoryNames

instance Data.ToHeaders BatchGetRepositories where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "CodeCommit_20150413.BatchGetRepositories" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON BatchGetRepositories where
  toJSON BatchGetRepositories' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("repositoryNames" Data..= repositoryNames)
          ]
      )

instance Data.ToPath BatchGetRepositories where
  toPath = Prelude.const "/"

instance Data.ToQuery BatchGetRepositories where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the output of a batch get repositories operation.
--
-- /See:/ 'newBatchGetRepositoriesResponse' smart constructor.
data BatchGetRepositoriesResponse = BatchGetRepositoriesResponse'
  { -- | A list of repositories returned by the batch get repositories operation.
    repositories :: Prelude.Maybe [RepositoryMetadata],
    -- | Returns a list of repository names for which information could not be
    -- found.
    repositoriesNotFound :: Prelude.Maybe [Prelude.Text],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchGetRepositoriesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'repositories', 'batchGetRepositoriesResponse_repositories' - A list of repositories returned by the batch get repositories operation.
--
-- 'repositoriesNotFound', 'batchGetRepositoriesResponse_repositoriesNotFound' - Returns a list of repository names for which information could not be
-- found.
--
-- 'httpStatus', 'batchGetRepositoriesResponse_httpStatus' - The response's http status code.
newBatchGetRepositoriesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  BatchGetRepositoriesResponse
newBatchGetRepositoriesResponse pHttpStatus_ =
  BatchGetRepositoriesResponse'
    { repositories =
        Prelude.Nothing,
      repositoriesNotFound = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of repositories returned by the batch get repositories operation.
batchGetRepositoriesResponse_repositories :: Lens.Lens' BatchGetRepositoriesResponse (Prelude.Maybe [RepositoryMetadata])
batchGetRepositoriesResponse_repositories = Lens.lens (\BatchGetRepositoriesResponse' {repositories} -> repositories) (\s@BatchGetRepositoriesResponse' {} a -> s {repositories = a} :: BatchGetRepositoriesResponse) Prelude.. Lens.mapping Lens.coerced

-- | Returns a list of repository names for which information could not be
-- found.
batchGetRepositoriesResponse_repositoriesNotFound :: Lens.Lens' BatchGetRepositoriesResponse (Prelude.Maybe [Prelude.Text])
batchGetRepositoriesResponse_repositoriesNotFound = Lens.lens (\BatchGetRepositoriesResponse' {repositoriesNotFound} -> repositoriesNotFound) (\s@BatchGetRepositoriesResponse' {} a -> s {repositoriesNotFound = a} :: BatchGetRepositoriesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
batchGetRepositoriesResponse_httpStatus :: Lens.Lens' BatchGetRepositoriesResponse Prelude.Int
batchGetRepositoriesResponse_httpStatus = Lens.lens (\BatchGetRepositoriesResponse' {httpStatus} -> httpStatus) (\s@BatchGetRepositoriesResponse' {} a -> s {httpStatus = a} :: BatchGetRepositoriesResponse)

instance Prelude.NFData BatchGetRepositoriesResponse where
  rnf BatchGetRepositoriesResponse' {..} =
    Prelude.rnf repositories
      `Prelude.seq` Prelude.rnf repositoriesNotFound
      `Prelude.seq` Prelude.rnf httpStatus
