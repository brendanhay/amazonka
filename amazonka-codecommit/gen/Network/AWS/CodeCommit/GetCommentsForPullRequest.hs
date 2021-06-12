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
-- Module      : Network.AWS.CodeCommit.GetCommentsForPullRequest
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns comments made on a pull request.
--
-- Reaction counts might include numbers from user identities who were
-- deleted after the reaction was made. For a count of reactions from
-- active identities, use GetCommentReactions.
--
-- This operation returns paginated results.
module Network.AWS.CodeCommit.GetCommentsForPullRequest
  ( -- * Creating a Request
    GetCommentsForPullRequest (..),
    newGetCommentsForPullRequest,

    -- * Request Lenses
    getCommentsForPullRequest_nextToken,
    getCommentsForPullRequest_maxResults,
    getCommentsForPullRequest_repositoryName,
    getCommentsForPullRequest_beforeCommitId,
    getCommentsForPullRequest_afterCommitId,
    getCommentsForPullRequest_pullRequestId,

    -- * Destructuring the Response
    GetCommentsForPullRequestResponse (..),
    newGetCommentsForPullRequestResponse,

    -- * Response Lenses
    getCommentsForPullRequestResponse_commentsForPullRequestData,
    getCommentsForPullRequestResponse_nextToken,
    getCommentsForPullRequestResponse_httpStatus,
  )
where

import Network.AWS.CodeCommit.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetCommentsForPullRequest' smart constructor.
data GetCommentsForPullRequest = GetCommentsForPullRequest'
  { -- | An enumeration token that, when provided in a request, returns the next
    -- batch of the results.
    nextToken :: Core.Maybe Core.Text,
    -- | A non-zero, non-negative integer used to limit the number of returned
    -- results. The default is 100 comments. You can return up to 500 comments
    -- with a single request.
    maxResults :: Core.Maybe Core.Int,
    -- | The name of the repository that contains the pull request.
    repositoryName :: Core.Maybe Core.Text,
    -- | The full commit ID of the commit in the destination branch that was the
    -- tip of the branch at the time the pull request was created.
    beforeCommitId :: Core.Maybe Core.Text,
    -- | The full commit ID of the commit in the source branch that was the tip
    -- of the branch at the time the comment was made.
    afterCommitId :: Core.Maybe Core.Text,
    -- | The system-generated ID of the pull request. To get this ID, use
    -- ListPullRequests.
    pullRequestId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetCommentsForPullRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getCommentsForPullRequest_nextToken' - An enumeration token that, when provided in a request, returns the next
-- batch of the results.
--
-- 'maxResults', 'getCommentsForPullRequest_maxResults' - A non-zero, non-negative integer used to limit the number of returned
-- results. The default is 100 comments. You can return up to 500 comments
-- with a single request.
--
-- 'repositoryName', 'getCommentsForPullRequest_repositoryName' - The name of the repository that contains the pull request.
--
-- 'beforeCommitId', 'getCommentsForPullRequest_beforeCommitId' - The full commit ID of the commit in the destination branch that was the
-- tip of the branch at the time the pull request was created.
--
-- 'afterCommitId', 'getCommentsForPullRequest_afterCommitId' - The full commit ID of the commit in the source branch that was the tip
-- of the branch at the time the comment was made.
--
-- 'pullRequestId', 'getCommentsForPullRequest_pullRequestId' - The system-generated ID of the pull request. To get this ID, use
-- ListPullRequests.
newGetCommentsForPullRequest ::
  -- | 'pullRequestId'
  Core.Text ->
  GetCommentsForPullRequest
newGetCommentsForPullRequest pPullRequestId_ =
  GetCommentsForPullRequest'
    { nextToken =
        Core.Nothing,
      maxResults = Core.Nothing,
      repositoryName = Core.Nothing,
      beforeCommitId = Core.Nothing,
      afterCommitId = Core.Nothing,
      pullRequestId = pPullRequestId_
    }

-- | An enumeration token that, when provided in a request, returns the next
-- batch of the results.
getCommentsForPullRequest_nextToken :: Lens.Lens' GetCommentsForPullRequest (Core.Maybe Core.Text)
getCommentsForPullRequest_nextToken = Lens.lens (\GetCommentsForPullRequest' {nextToken} -> nextToken) (\s@GetCommentsForPullRequest' {} a -> s {nextToken = a} :: GetCommentsForPullRequest)

-- | A non-zero, non-negative integer used to limit the number of returned
-- results. The default is 100 comments. You can return up to 500 comments
-- with a single request.
getCommentsForPullRequest_maxResults :: Lens.Lens' GetCommentsForPullRequest (Core.Maybe Core.Int)
getCommentsForPullRequest_maxResults = Lens.lens (\GetCommentsForPullRequest' {maxResults} -> maxResults) (\s@GetCommentsForPullRequest' {} a -> s {maxResults = a} :: GetCommentsForPullRequest)

-- | The name of the repository that contains the pull request.
getCommentsForPullRequest_repositoryName :: Lens.Lens' GetCommentsForPullRequest (Core.Maybe Core.Text)
getCommentsForPullRequest_repositoryName = Lens.lens (\GetCommentsForPullRequest' {repositoryName} -> repositoryName) (\s@GetCommentsForPullRequest' {} a -> s {repositoryName = a} :: GetCommentsForPullRequest)

-- | The full commit ID of the commit in the destination branch that was the
-- tip of the branch at the time the pull request was created.
getCommentsForPullRequest_beforeCommitId :: Lens.Lens' GetCommentsForPullRequest (Core.Maybe Core.Text)
getCommentsForPullRequest_beforeCommitId = Lens.lens (\GetCommentsForPullRequest' {beforeCommitId} -> beforeCommitId) (\s@GetCommentsForPullRequest' {} a -> s {beforeCommitId = a} :: GetCommentsForPullRequest)

-- | The full commit ID of the commit in the source branch that was the tip
-- of the branch at the time the comment was made.
getCommentsForPullRequest_afterCommitId :: Lens.Lens' GetCommentsForPullRequest (Core.Maybe Core.Text)
getCommentsForPullRequest_afterCommitId = Lens.lens (\GetCommentsForPullRequest' {afterCommitId} -> afterCommitId) (\s@GetCommentsForPullRequest' {} a -> s {afterCommitId = a} :: GetCommentsForPullRequest)

-- | The system-generated ID of the pull request. To get this ID, use
-- ListPullRequests.
getCommentsForPullRequest_pullRequestId :: Lens.Lens' GetCommentsForPullRequest Core.Text
getCommentsForPullRequest_pullRequestId = Lens.lens (\GetCommentsForPullRequest' {pullRequestId} -> pullRequestId) (\s@GetCommentsForPullRequest' {} a -> s {pullRequestId = a} :: GetCommentsForPullRequest)

instance Core.AWSPager GetCommentsForPullRequest where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getCommentsForPullRequestResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? getCommentsForPullRequestResponse_commentsForPullRequestData
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& getCommentsForPullRequest_nextToken
          Lens..~ rs
          Lens.^? getCommentsForPullRequestResponse_nextToken
            Core.. Lens._Just

instance Core.AWSRequest GetCommentsForPullRequest where
  type
    AWSResponse GetCommentsForPullRequest =
      GetCommentsForPullRequestResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetCommentsForPullRequestResponse'
            Core.<$> ( x Core..?> "commentsForPullRequestData"
                         Core..!@ Core.mempty
                     )
            Core.<*> (x Core..?> "nextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetCommentsForPullRequest

instance Core.NFData GetCommentsForPullRequest

instance Core.ToHeaders GetCommentsForPullRequest where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "CodeCommit_20150413.GetCommentsForPullRequest" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GetCommentsForPullRequest where
  toJSON GetCommentsForPullRequest' {..} =
    Core.object
      ( Core.catMaybes
          [ ("nextToken" Core..=) Core.<$> nextToken,
            ("maxResults" Core..=) Core.<$> maxResults,
            ("repositoryName" Core..=) Core.<$> repositoryName,
            ("beforeCommitId" Core..=) Core.<$> beforeCommitId,
            ("afterCommitId" Core..=) Core.<$> afterCommitId,
            Core.Just ("pullRequestId" Core..= pullRequestId)
          ]
      )

instance Core.ToPath GetCommentsForPullRequest where
  toPath = Core.const "/"

instance Core.ToQuery GetCommentsForPullRequest where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetCommentsForPullRequestResponse' smart constructor.
data GetCommentsForPullRequestResponse = GetCommentsForPullRequestResponse'
  { -- | An array of comment objects on the pull request.
    commentsForPullRequestData :: Core.Maybe [CommentsForPullRequest],
    -- | An enumeration token that can be used in a request to return the next
    -- batch of the results.
    nextToken :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetCommentsForPullRequestResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'commentsForPullRequestData', 'getCommentsForPullRequestResponse_commentsForPullRequestData' - An array of comment objects on the pull request.
--
-- 'nextToken', 'getCommentsForPullRequestResponse_nextToken' - An enumeration token that can be used in a request to return the next
-- batch of the results.
--
-- 'httpStatus', 'getCommentsForPullRequestResponse_httpStatus' - The response's http status code.
newGetCommentsForPullRequestResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetCommentsForPullRequestResponse
newGetCommentsForPullRequestResponse pHttpStatus_ =
  GetCommentsForPullRequestResponse'
    { commentsForPullRequestData =
        Core.Nothing,
      nextToken = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of comment objects on the pull request.
getCommentsForPullRequestResponse_commentsForPullRequestData :: Lens.Lens' GetCommentsForPullRequestResponse (Core.Maybe [CommentsForPullRequest])
getCommentsForPullRequestResponse_commentsForPullRequestData = Lens.lens (\GetCommentsForPullRequestResponse' {commentsForPullRequestData} -> commentsForPullRequestData) (\s@GetCommentsForPullRequestResponse' {} a -> s {commentsForPullRequestData = a} :: GetCommentsForPullRequestResponse) Core.. Lens.mapping Lens._Coerce

-- | An enumeration token that can be used in a request to return the next
-- batch of the results.
getCommentsForPullRequestResponse_nextToken :: Lens.Lens' GetCommentsForPullRequestResponse (Core.Maybe Core.Text)
getCommentsForPullRequestResponse_nextToken = Lens.lens (\GetCommentsForPullRequestResponse' {nextToken} -> nextToken) (\s@GetCommentsForPullRequestResponse' {} a -> s {nextToken = a} :: GetCommentsForPullRequestResponse)

-- | The response's http status code.
getCommentsForPullRequestResponse_httpStatus :: Lens.Lens' GetCommentsForPullRequestResponse Core.Int
getCommentsForPullRequestResponse_httpStatus = Lens.lens (\GetCommentsForPullRequestResponse' {httpStatus} -> httpStatus) (\s@GetCommentsForPullRequestResponse' {} a -> s {httpStatus = a} :: GetCommentsForPullRequestResponse)

instance
  Core.NFData
    GetCommentsForPullRequestResponse
