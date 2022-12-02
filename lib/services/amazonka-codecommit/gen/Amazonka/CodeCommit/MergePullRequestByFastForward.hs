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
-- Module      : Amazonka.CodeCommit.MergePullRequestByFastForward
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Attempts to merge the source commit of a pull request into the specified
-- destination branch for that pull request at the specified commit using
-- the fast-forward merge strategy. If the merge is successful, it closes
-- the pull request.
module Amazonka.CodeCommit.MergePullRequestByFastForward
  ( -- * Creating a Request
    MergePullRequestByFastForward (..),
    newMergePullRequestByFastForward,

    -- * Request Lenses
    mergePullRequestByFastForward_sourceCommitId,
    mergePullRequestByFastForward_pullRequestId,
    mergePullRequestByFastForward_repositoryName,

    -- * Destructuring the Response
    MergePullRequestByFastForwardResponse (..),
    newMergePullRequestByFastForwardResponse,

    -- * Response Lenses
    mergePullRequestByFastForwardResponse_pullRequest,
    mergePullRequestByFastForwardResponse_httpStatus,
  )
where

import Amazonka.CodeCommit.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newMergePullRequestByFastForward' smart constructor.
data MergePullRequestByFastForward = MergePullRequestByFastForward'
  { -- | The full commit ID of the original or updated commit in the pull request
    -- source branch. Pass this value if you want an exception thrown if the
    -- current commit ID of the tip of the source branch does not match this
    -- commit ID.
    sourceCommitId :: Prelude.Maybe Prelude.Text,
    -- | The system-generated ID of the pull request. To get this ID, use
    -- ListPullRequests.
    pullRequestId :: Prelude.Text,
    -- | The name of the repository where the pull request was created.
    repositoryName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MergePullRequestByFastForward' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sourceCommitId', 'mergePullRequestByFastForward_sourceCommitId' - The full commit ID of the original or updated commit in the pull request
-- source branch. Pass this value if you want an exception thrown if the
-- current commit ID of the tip of the source branch does not match this
-- commit ID.
--
-- 'pullRequestId', 'mergePullRequestByFastForward_pullRequestId' - The system-generated ID of the pull request. To get this ID, use
-- ListPullRequests.
--
-- 'repositoryName', 'mergePullRequestByFastForward_repositoryName' - The name of the repository where the pull request was created.
newMergePullRequestByFastForward ::
  -- | 'pullRequestId'
  Prelude.Text ->
  -- | 'repositoryName'
  Prelude.Text ->
  MergePullRequestByFastForward
newMergePullRequestByFastForward
  pPullRequestId_
  pRepositoryName_ =
    MergePullRequestByFastForward'
      { sourceCommitId =
          Prelude.Nothing,
        pullRequestId = pPullRequestId_,
        repositoryName = pRepositoryName_
      }

-- | The full commit ID of the original or updated commit in the pull request
-- source branch. Pass this value if you want an exception thrown if the
-- current commit ID of the tip of the source branch does not match this
-- commit ID.
mergePullRequestByFastForward_sourceCommitId :: Lens.Lens' MergePullRequestByFastForward (Prelude.Maybe Prelude.Text)
mergePullRequestByFastForward_sourceCommitId = Lens.lens (\MergePullRequestByFastForward' {sourceCommitId} -> sourceCommitId) (\s@MergePullRequestByFastForward' {} a -> s {sourceCommitId = a} :: MergePullRequestByFastForward)

-- | The system-generated ID of the pull request. To get this ID, use
-- ListPullRequests.
mergePullRequestByFastForward_pullRequestId :: Lens.Lens' MergePullRequestByFastForward Prelude.Text
mergePullRequestByFastForward_pullRequestId = Lens.lens (\MergePullRequestByFastForward' {pullRequestId} -> pullRequestId) (\s@MergePullRequestByFastForward' {} a -> s {pullRequestId = a} :: MergePullRequestByFastForward)

-- | The name of the repository where the pull request was created.
mergePullRequestByFastForward_repositoryName :: Lens.Lens' MergePullRequestByFastForward Prelude.Text
mergePullRequestByFastForward_repositoryName = Lens.lens (\MergePullRequestByFastForward' {repositoryName} -> repositoryName) (\s@MergePullRequestByFastForward' {} a -> s {repositoryName = a} :: MergePullRequestByFastForward)

instance
  Core.AWSRequest
    MergePullRequestByFastForward
  where
  type
    AWSResponse MergePullRequestByFastForward =
      MergePullRequestByFastForwardResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          MergePullRequestByFastForwardResponse'
            Prelude.<$> (x Data..?> "pullRequest")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    MergePullRequestByFastForward
  where
  hashWithSalt _salt MergePullRequestByFastForward' {..} =
    _salt `Prelude.hashWithSalt` sourceCommitId
      `Prelude.hashWithSalt` pullRequestId
      `Prelude.hashWithSalt` repositoryName

instance Prelude.NFData MergePullRequestByFastForward where
  rnf MergePullRequestByFastForward' {..} =
    Prelude.rnf sourceCommitId
      `Prelude.seq` Prelude.rnf pullRequestId
      `Prelude.seq` Prelude.rnf repositoryName

instance Data.ToHeaders MergePullRequestByFastForward where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "CodeCommit_20150413.MergePullRequestByFastForward" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON MergePullRequestByFastForward where
  toJSON MergePullRequestByFastForward' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("sourceCommitId" Data..=)
              Prelude.<$> sourceCommitId,
            Prelude.Just ("pullRequestId" Data..= pullRequestId),
            Prelude.Just
              ("repositoryName" Data..= repositoryName)
          ]
      )

instance Data.ToPath MergePullRequestByFastForward where
  toPath = Prelude.const "/"

instance Data.ToQuery MergePullRequestByFastForward where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newMergePullRequestByFastForwardResponse' smart constructor.
data MergePullRequestByFastForwardResponse = MergePullRequestByFastForwardResponse'
  { -- | Information about the specified pull request, including the merge.
    pullRequest :: Prelude.Maybe PullRequest,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MergePullRequestByFastForwardResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pullRequest', 'mergePullRequestByFastForwardResponse_pullRequest' - Information about the specified pull request, including the merge.
--
-- 'httpStatus', 'mergePullRequestByFastForwardResponse_httpStatus' - The response's http status code.
newMergePullRequestByFastForwardResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  MergePullRequestByFastForwardResponse
newMergePullRequestByFastForwardResponse pHttpStatus_ =
  MergePullRequestByFastForwardResponse'
    { pullRequest =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the specified pull request, including the merge.
mergePullRequestByFastForwardResponse_pullRequest :: Lens.Lens' MergePullRequestByFastForwardResponse (Prelude.Maybe PullRequest)
mergePullRequestByFastForwardResponse_pullRequest = Lens.lens (\MergePullRequestByFastForwardResponse' {pullRequest} -> pullRequest) (\s@MergePullRequestByFastForwardResponse' {} a -> s {pullRequest = a} :: MergePullRequestByFastForwardResponse)

-- | The response's http status code.
mergePullRequestByFastForwardResponse_httpStatus :: Lens.Lens' MergePullRequestByFastForwardResponse Prelude.Int
mergePullRequestByFastForwardResponse_httpStatus = Lens.lens (\MergePullRequestByFastForwardResponse' {httpStatus} -> httpStatus) (\s@MergePullRequestByFastForwardResponse' {} a -> s {httpStatus = a} :: MergePullRequestByFastForwardResponse)

instance
  Prelude.NFData
    MergePullRequestByFastForwardResponse
  where
  rnf MergePullRequestByFastForwardResponse' {..} =
    Prelude.rnf pullRequest
      `Prelude.seq` Prelude.rnf httpStatus
