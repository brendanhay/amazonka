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
-- Module      : Network.AWS.CodeCommit.BatchGetCommits
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the contents of one or more commits in a
-- repository.
module Network.AWS.CodeCommit.BatchGetCommits
  ( -- * Creating a Request
    BatchGetCommits (..),
    newBatchGetCommits,

    -- * Request Lenses
    batchGetCommits_commitIds,
    batchGetCommits_repositoryName,

    -- * Destructuring the Response
    BatchGetCommitsResponse (..),
    newBatchGetCommitsResponse,

    -- * Response Lenses
    batchGetCommitsResponse_commits,
    batchGetCommitsResponse_errors,
    batchGetCommitsResponse_httpStatus,
  )
where

import Network.AWS.CodeCommit.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newBatchGetCommits' smart constructor.
data BatchGetCommits = BatchGetCommits'
  { -- | The full commit IDs of the commits to get information about.
    --
    -- You must supply the full SHA IDs of each commit. You cannot use
    -- shortened SHA IDs.
    commitIds :: [Core.Text],
    -- | The name of the repository that contains the commits.
    repositoryName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'BatchGetCommits' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'commitIds', 'batchGetCommits_commitIds' - The full commit IDs of the commits to get information about.
--
-- You must supply the full SHA IDs of each commit. You cannot use
-- shortened SHA IDs.
--
-- 'repositoryName', 'batchGetCommits_repositoryName' - The name of the repository that contains the commits.
newBatchGetCommits ::
  -- | 'repositoryName'
  Core.Text ->
  BatchGetCommits
newBatchGetCommits pRepositoryName_ =
  BatchGetCommits'
    { commitIds = Core.mempty,
      repositoryName = pRepositoryName_
    }

-- | The full commit IDs of the commits to get information about.
--
-- You must supply the full SHA IDs of each commit. You cannot use
-- shortened SHA IDs.
batchGetCommits_commitIds :: Lens.Lens' BatchGetCommits [Core.Text]
batchGetCommits_commitIds = Lens.lens (\BatchGetCommits' {commitIds} -> commitIds) (\s@BatchGetCommits' {} a -> s {commitIds = a} :: BatchGetCommits) Core.. Lens._Coerce

-- | The name of the repository that contains the commits.
batchGetCommits_repositoryName :: Lens.Lens' BatchGetCommits Core.Text
batchGetCommits_repositoryName = Lens.lens (\BatchGetCommits' {repositoryName} -> repositoryName) (\s@BatchGetCommits' {} a -> s {repositoryName = a} :: BatchGetCommits)

instance Core.AWSRequest BatchGetCommits where
  type
    AWSResponse BatchGetCommits =
      BatchGetCommitsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchGetCommitsResponse'
            Core.<$> (x Core..?> "commits" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "errors" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable BatchGetCommits

instance Core.NFData BatchGetCommits

instance Core.ToHeaders BatchGetCommits where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "CodeCommit_20150413.BatchGetCommits" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON BatchGetCommits where
  toJSON BatchGetCommits' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("commitIds" Core..= commitIds),
            Core.Just ("repositoryName" Core..= repositoryName)
          ]
      )

instance Core.ToPath BatchGetCommits where
  toPath = Core.const "/"

instance Core.ToQuery BatchGetCommits where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newBatchGetCommitsResponse' smart constructor.
data BatchGetCommitsResponse = BatchGetCommitsResponse'
  { -- | An array of commit data type objects, each of which contains information
    -- about a specified commit.
    commits :: Core.Maybe [Commit],
    -- | Returns any commit IDs for which information could not be found. For
    -- example, if one of the commit IDs was a shortened SHA ID or that commit
    -- was not found in the specified repository, the ID returns an error
    -- object with more information.
    errors :: Core.Maybe [BatchGetCommitsError],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'BatchGetCommitsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'commits', 'batchGetCommitsResponse_commits' - An array of commit data type objects, each of which contains information
-- about a specified commit.
--
-- 'errors', 'batchGetCommitsResponse_errors' - Returns any commit IDs for which information could not be found. For
-- example, if one of the commit IDs was a shortened SHA ID or that commit
-- was not found in the specified repository, the ID returns an error
-- object with more information.
--
-- 'httpStatus', 'batchGetCommitsResponse_httpStatus' - The response's http status code.
newBatchGetCommitsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  BatchGetCommitsResponse
newBatchGetCommitsResponse pHttpStatus_ =
  BatchGetCommitsResponse'
    { commits = Core.Nothing,
      errors = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of commit data type objects, each of which contains information
-- about a specified commit.
batchGetCommitsResponse_commits :: Lens.Lens' BatchGetCommitsResponse (Core.Maybe [Commit])
batchGetCommitsResponse_commits = Lens.lens (\BatchGetCommitsResponse' {commits} -> commits) (\s@BatchGetCommitsResponse' {} a -> s {commits = a} :: BatchGetCommitsResponse) Core.. Lens.mapping Lens._Coerce

-- | Returns any commit IDs for which information could not be found. For
-- example, if one of the commit IDs was a shortened SHA ID or that commit
-- was not found in the specified repository, the ID returns an error
-- object with more information.
batchGetCommitsResponse_errors :: Lens.Lens' BatchGetCommitsResponse (Core.Maybe [BatchGetCommitsError])
batchGetCommitsResponse_errors = Lens.lens (\BatchGetCommitsResponse' {errors} -> errors) (\s@BatchGetCommitsResponse' {} a -> s {errors = a} :: BatchGetCommitsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
batchGetCommitsResponse_httpStatus :: Lens.Lens' BatchGetCommitsResponse Core.Int
batchGetCommitsResponse_httpStatus = Lens.lens (\BatchGetCommitsResponse' {httpStatus} -> httpStatus) (\s@BatchGetCommitsResponse' {} a -> s {httpStatus = a} :: BatchGetCommitsResponse)

instance Core.NFData BatchGetCommitsResponse
