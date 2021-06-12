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
-- Module      : Network.AWS.CodeCommit.BatchGetRepositories
-- Copyright   : (c) 2013-2021 Brendan Hay
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
module Network.AWS.CodeCommit.BatchGetRepositories
  ( -- * Creating a Request
    BatchGetRepositories (..),
    newBatchGetRepositories,

    -- * Request Lenses
    batchGetRepositories_repositoryNames,

    -- * Destructuring the Response
    BatchGetRepositoriesResponse (..),
    newBatchGetRepositoriesResponse,

    -- * Response Lenses
    batchGetRepositoriesResponse_repositoriesNotFound,
    batchGetRepositoriesResponse_repositories,
    batchGetRepositoriesResponse_httpStatus,
  )
where

import Network.AWS.CodeCommit.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a batch get repositories operation.
--
-- /See:/ 'newBatchGetRepositories' smart constructor.
data BatchGetRepositories = BatchGetRepositories'
  { -- | The names of the repositories to get information about.
    --
    -- The length constraint limit is for each string in the array. The array
    -- itself can be empty.
    repositoryNames :: [Core.Text]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
        Core.mempty
    }

-- | The names of the repositories to get information about.
--
-- The length constraint limit is for each string in the array. The array
-- itself can be empty.
batchGetRepositories_repositoryNames :: Lens.Lens' BatchGetRepositories [Core.Text]
batchGetRepositories_repositoryNames = Lens.lens (\BatchGetRepositories' {repositoryNames} -> repositoryNames) (\s@BatchGetRepositories' {} a -> s {repositoryNames = a} :: BatchGetRepositories) Core.. Lens._Coerce

instance Core.AWSRequest BatchGetRepositories where
  type
    AWSResponse BatchGetRepositories =
      BatchGetRepositoriesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchGetRepositoriesResponse'
            Core.<$> ( x Core..?> "repositoriesNotFound"
                         Core..!@ Core.mempty
                     )
            Core.<*> (x Core..?> "repositories" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable BatchGetRepositories

instance Core.NFData BatchGetRepositories

instance Core.ToHeaders BatchGetRepositories where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "CodeCommit_20150413.BatchGetRepositories" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON BatchGetRepositories where
  toJSON BatchGetRepositories' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("repositoryNames" Core..= repositoryNames)
          ]
      )

instance Core.ToPath BatchGetRepositories where
  toPath = Core.const "/"

instance Core.ToQuery BatchGetRepositories where
  toQuery = Core.const Core.mempty

-- | Represents the output of a batch get repositories operation.
--
-- /See:/ 'newBatchGetRepositoriesResponse' smart constructor.
data BatchGetRepositoriesResponse = BatchGetRepositoriesResponse'
  { -- | Returns a list of repository names for which information could not be
    -- found.
    repositoriesNotFound :: Core.Maybe [Core.Text],
    -- | A list of repositories returned by the batch get repositories operation.
    repositories :: Core.Maybe [RepositoryMetadata],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'BatchGetRepositoriesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'repositoriesNotFound', 'batchGetRepositoriesResponse_repositoriesNotFound' - Returns a list of repository names for which information could not be
-- found.
--
-- 'repositories', 'batchGetRepositoriesResponse_repositories' - A list of repositories returned by the batch get repositories operation.
--
-- 'httpStatus', 'batchGetRepositoriesResponse_httpStatus' - The response's http status code.
newBatchGetRepositoriesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  BatchGetRepositoriesResponse
newBatchGetRepositoriesResponse pHttpStatus_ =
  BatchGetRepositoriesResponse'
    { repositoriesNotFound =
        Core.Nothing,
      repositories = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Returns a list of repository names for which information could not be
-- found.
batchGetRepositoriesResponse_repositoriesNotFound :: Lens.Lens' BatchGetRepositoriesResponse (Core.Maybe [Core.Text])
batchGetRepositoriesResponse_repositoriesNotFound = Lens.lens (\BatchGetRepositoriesResponse' {repositoriesNotFound} -> repositoriesNotFound) (\s@BatchGetRepositoriesResponse' {} a -> s {repositoriesNotFound = a} :: BatchGetRepositoriesResponse) Core.. Lens.mapping Lens._Coerce

-- | A list of repositories returned by the batch get repositories operation.
batchGetRepositoriesResponse_repositories :: Lens.Lens' BatchGetRepositoriesResponse (Core.Maybe [RepositoryMetadata])
batchGetRepositoriesResponse_repositories = Lens.lens (\BatchGetRepositoriesResponse' {repositories} -> repositories) (\s@BatchGetRepositoriesResponse' {} a -> s {repositories = a} :: BatchGetRepositoriesResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
batchGetRepositoriesResponse_httpStatus :: Lens.Lens' BatchGetRepositoriesResponse Core.Int
batchGetRepositoriesResponse_httpStatus = Lens.lens (\BatchGetRepositoriesResponse' {httpStatus} -> httpStatus) (\s@BatchGetRepositoriesResponse' {} a -> s {httpStatus = a} :: BatchGetRepositoriesResponse)

instance Core.NFData BatchGetRepositoriesResponse
