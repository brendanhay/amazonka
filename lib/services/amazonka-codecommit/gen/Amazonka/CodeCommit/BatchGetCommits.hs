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
-- Module      : Amazonka.CodeCommit.BatchGetCommits
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the contents of one or more commits in a
-- repository.
module Amazonka.CodeCommit.BatchGetCommits
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

import Amazonka.CodeCommit.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

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
batchGetCommits_commitIds = Lens.lens (\BatchGetCommits' {commitIds} -> commitIds) (\s@BatchGetCommits' {} a -> s {commitIds = a} :: BatchGetCommits) Prelude.. Lens.coerced

-- | The name of the repository that contains the commits.
batchGetCommits_repositoryName :: Lens.Lens' BatchGetCommits Prelude.Text
batchGetCommits_repositoryName = Lens.lens (\BatchGetCommits' {repositoryName} -> repositoryName) (\s@BatchGetCommits' {} a -> s {repositoryName = a} :: BatchGetCommits)

instance Core.AWSRequest BatchGetCommits where
  type
    AWSResponse BatchGetCommits =
      BatchGetCommitsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchGetCommitsResponse'
            Prelude.<$> (x Data..?> "commits" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "errors" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable BatchGetCommits where
  hashWithSalt _salt BatchGetCommits' {..} =
    _salt `Prelude.hashWithSalt` commitIds
      `Prelude.hashWithSalt` repositoryName

instance Prelude.NFData BatchGetCommits where
  rnf BatchGetCommits' {..} =
    Prelude.rnf commitIds
      `Prelude.seq` Prelude.rnf repositoryName

instance Data.ToHeaders BatchGetCommits where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "CodeCommit_20150413.BatchGetCommits" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON BatchGetCommits where
  toJSON BatchGetCommits' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("commitIds" Data..= commitIds),
            Prelude.Just
              ("repositoryName" Data..= repositoryName)
          ]
      )

instance Data.ToPath BatchGetCommits where
  toPath = Prelude.const "/"

instance Data.ToQuery BatchGetCommits where
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
batchGetCommitsResponse_commits = Lens.lens (\BatchGetCommitsResponse' {commits} -> commits) (\s@BatchGetCommitsResponse' {} a -> s {commits = a} :: BatchGetCommitsResponse) Prelude.. Lens.mapping Lens.coerced

-- | Returns any commit IDs for which information could not be found. For
-- example, if one of the commit IDs was a shortened SHA ID or that commit
-- was not found in the specified repository, the ID returns an error
-- object with more information.
batchGetCommitsResponse_errors :: Lens.Lens' BatchGetCommitsResponse (Prelude.Maybe [BatchGetCommitsError])
batchGetCommitsResponse_errors = Lens.lens (\BatchGetCommitsResponse' {errors} -> errors) (\s@BatchGetCommitsResponse' {} a -> s {errors = a} :: BatchGetCommitsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
batchGetCommitsResponse_httpStatus :: Lens.Lens' BatchGetCommitsResponse Prelude.Int
batchGetCommitsResponse_httpStatus = Lens.lens (\BatchGetCommitsResponse' {httpStatus} -> httpStatus) (\s@BatchGetCommitsResponse' {} a -> s {httpStatus = a} :: BatchGetCommitsResponse)

instance Prelude.NFData BatchGetCommitsResponse where
  rnf BatchGetCommitsResponse' {..} =
    Prelude.rnf commits
      `Prelude.seq` Prelude.rnf errors
      `Prelude.seq` Prelude.rnf httpStatus
