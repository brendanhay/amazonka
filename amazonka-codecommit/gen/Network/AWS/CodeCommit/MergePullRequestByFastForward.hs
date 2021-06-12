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
-- Module      : Network.AWS.CodeCommit.MergePullRequestByFastForward
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Attempts to merge the source commit of a pull request into the specified
-- destination branch for that pull request at the specified commit using
-- the fast-forward merge strategy. If the merge is successful, it closes
-- the pull request.
module Network.AWS.CodeCommit.MergePullRequestByFastForward
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

import Network.AWS.CodeCommit.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newMergePullRequestByFastForward' smart constructor.
data MergePullRequestByFastForward = MergePullRequestByFastForward'
  { -- | The full commit ID of the original or updated commit in the pull request
    -- source branch. Pass this value if you want an exception thrown if the
    -- current commit ID of the tip of the source branch does not match this
    -- commit ID.
    sourceCommitId :: Core.Maybe Core.Text,
    -- | The system-generated ID of the pull request. To get this ID, use
    -- ListPullRequests.
    pullRequestId :: Core.Text,
    -- | The name of the repository where the pull request was created.
    repositoryName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  -- | 'repositoryName'
  Core.Text ->
  MergePullRequestByFastForward
newMergePullRequestByFastForward
  pPullRequestId_
  pRepositoryName_ =
    MergePullRequestByFastForward'
      { sourceCommitId =
          Core.Nothing,
        pullRequestId = pPullRequestId_,
        repositoryName = pRepositoryName_
      }

-- | The full commit ID of the original or updated commit in the pull request
-- source branch. Pass this value if you want an exception thrown if the
-- current commit ID of the tip of the source branch does not match this
-- commit ID.
mergePullRequestByFastForward_sourceCommitId :: Lens.Lens' MergePullRequestByFastForward (Core.Maybe Core.Text)
mergePullRequestByFastForward_sourceCommitId = Lens.lens (\MergePullRequestByFastForward' {sourceCommitId} -> sourceCommitId) (\s@MergePullRequestByFastForward' {} a -> s {sourceCommitId = a} :: MergePullRequestByFastForward)

-- | The system-generated ID of the pull request. To get this ID, use
-- ListPullRequests.
mergePullRequestByFastForward_pullRequestId :: Lens.Lens' MergePullRequestByFastForward Core.Text
mergePullRequestByFastForward_pullRequestId = Lens.lens (\MergePullRequestByFastForward' {pullRequestId} -> pullRequestId) (\s@MergePullRequestByFastForward' {} a -> s {pullRequestId = a} :: MergePullRequestByFastForward)

-- | The name of the repository where the pull request was created.
mergePullRequestByFastForward_repositoryName :: Lens.Lens' MergePullRequestByFastForward Core.Text
mergePullRequestByFastForward_repositoryName = Lens.lens (\MergePullRequestByFastForward' {repositoryName} -> repositoryName) (\s@MergePullRequestByFastForward' {} a -> s {repositoryName = a} :: MergePullRequestByFastForward)

instance
  Core.AWSRequest
    MergePullRequestByFastForward
  where
  type
    AWSResponse MergePullRequestByFastForward =
      MergePullRequestByFastForwardResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          MergePullRequestByFastForwardResponse'
            Core.<$> (x Core..?> "pullRequest")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable MergePullRequestByFastForward

instance Core.NFData MergePullRequestByFastForward

instance Core.ToHeaders MergePullRequestByFastForward where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "CodeCommit_20150413.MergePullRequestByFastForward" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON MergePullRequestByFastForward where
  toJSON MergePullRequestByFastForward' {..} =
    Core.object
      ( Core.catMaybes
          [ ("sourceCommitId" Core..=) Core.<$> sourceCommitId,
            Core.Just ("pullRequestId" Core..= pullRequestId),
            Core.Just ("repositoryName" Core..= repositoryName)
          ]
      )

instance Core.ToPath MergePullRequestByFastForward where
  toPath = Core.const "/"

instance Core.ToQuery MergePullRequestByFastForward where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newMergePullRequestByFastForwardResponse' smart constructor.
data MergePullRequestByFastForwardResponse = MergePullRequestByFastForwardResponse'
  { -- | Information about the specified pull request, including the merge.
    pullRequest :: Core.Maybe PullRequest,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  MergePullRequestByFastForwardResponse
newMergePullRequestByFastForwardResponse pHttpStatus_ =
  MergePullRequestByFastForwardResponse'
    { pullRequest =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the specified pull request, including the merge.
mergePullRequestByFastForwardResponse_pullRequest :: Lens.Lens' MergePullRequestByFastForwardResponse (Core.Maybe PullRequest)
mergePullRequestByFastForwardResponse_pullRequest = Lens.lens (\MergePullRequestByFastForwardResponse' {pullRequest} -> pullRequest) (\s@MergePullRequestByFastForwardResponse' {} a -> s {pullRequest = a} :: MergePullRequestByFastForwardResponse)

-- | The response's http status code.
mergePullRequestByFastForwardResponse_httpStatus :: Lens.Lens' MergePullRequestByFastForwardResponse Core.Int
mergePullRequestByFastForwardResponse_httpStatus = Lens.lens (\MergePullRequestByFastForwardResponse' {httpStatus} -> httpStatus) (\s@MergePullRequestByFastForwardResponse' {} a -> s {httpStatus = a} :: MergePullRequestByFastForwardResponse)

instance
  Core.NFData
    MergePullRequestByFastForwardResponse
