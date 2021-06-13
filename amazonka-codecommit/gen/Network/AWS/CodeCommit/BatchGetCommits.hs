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
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newBatchGetCommits' smart constructor.
data BatchGetCommits = BatchGetCommits'
  { -- | The full commit IDs of the commits to get information about.
    --
    -- You must supply the full SHA IDs of each commit. You cannot use
    -- shortened SHA IDs.
    commitIds :: [Prelude.Text],
    -- | The name of the repository that contains the commits.
    repositoryName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  BatchGetCommits
newBatchGetCommits pRepositoryName_ =
  BatchGetCommits'
    { commitIds = Prelude.mempty,
      repositoryName = pRepositoryName_
    }

-- | The full commit IDs of the commits to get information about.
--
-- You must supply the full SHA IDs of each commit. You cannot use
-- shortened SHA IDs.
batchGetCommits_commitIds :: Lens.Lens' BatchGetCommits [Prelude.Text]
batchGetCommits_commitIds = Lens.lens (\BatchGetCommits' {commitIds} -> commitIds) (\s@BatchGetCommits' {} a -> s {commitIds = a} :: BatchGetCommits) Prelude.. Lens._Coerce

-- | The name of the repository that contains the commits.
batchGetCommits_repositoryName :: Lens.Lens' BatchGetCommits Prelude.Text
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
            Prelude.<$> (x Core..?> "commits" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "errors" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable BatchGetCommits

instance Prelude.NFData BatchGetCommits

instance Core.ToHeaders BatchGetCommits where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "CodeCommit_20150413.BatchGetCommits" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON BatchGetCommits where
  toJSON BatchGetCommits' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("commitIds" Core..= commitIds),
            Prelude.Just
              ("repositoryName" Core..= repositoryName)
          ]
      )

instance Core.ToPath BatchGetCommits where
  toPath = Prelude.const "/"

instance Core.ToQuery BatchGetCommits where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newBatchGetCommitsResponse' smart constructor.
data BatchGetCommitsResponse = BatchGetCommitsResponse'
  { -- | An array of commit data type objects, each of which contains information
    -- about a specified commit.
    commits :: Prelude.Maybe [Commit],
    -- | Returns any commit IDs for which information could not be found. For
    -- example, if one of the commit IDs was a shortened SHA ID or that commit
    -- was not found in the specified repository, the ID returns an error
    -- object with more information.
    errors :: Prelude.Maybe [BatchGetCommitsError],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  BatchGetCommitsResponse
newBatchGetCommitsResponse pHttpStatus_ =
  BatchGetCommitsResponse'
    { commits = Prelude.Nothing,
      errors = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of commit data type objects, each of which contains information
-- about a specified commit.
batchGetCommitsResponse_commits :: Lens.Lens' BatchGetCommitsResponse (Prelude.Maybe [Commit])
batchGetCommitsResponse_commits = Lens.lens (\BatchGetCommitsResponse' {commits} -> commits) (\s@BatchGetCommitsResponse' {} a -> s {commits = a} :: BatchGetCommitsResponse) Prelude.. Lens.mapping Lens._Coerce

-- | Returns any commit IDs for which information could not be found. For
-- example, if one of the commit IDs was a shortened SHA ID or that commit
-- was not found in the specified repository, the ID returns an error
-- object with more information.
batchGetCommitsResponse_errors :: Lens.Lens' BatchGetCommitsResponse (Prelude.Maybe [BatchGetCommitsError])
batchGetCommitsResponse_errors = Lens.lens (\BatchGetCommitsResponse' {errors} -> errors) (\s@BatchGetCommitsResponse' {} a -> s {errors = a} :: BatchGetCommitsResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
batchGetCommitsResponse_httpStatus :: Lens.Lens' BatchGetCommitsResponse Prelude.Int
batchGetCommitsResponse_httpStatus = Lens.lens (\BatchGetCommitsResponse' {httpStatus} -> httpStatus) (\s@BatchGetCommitsResponse' {} a -> s {httpStatus = a} :: BatchGetCommitsResponse)

instance Prelude.NFData BatchGetCommitsResponse
