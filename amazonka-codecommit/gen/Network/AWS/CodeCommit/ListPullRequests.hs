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
-- Module      : Network.AWS.CodeCommit.ListPullRequests
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of pull requests for a specified repository. The return
-- list can be refined by pull request status or pull request author ARN.
--
-- This operation returns paginated results.
module Network.AWS.CodeCommit.ListPullRequests
  ( -- * Creating a Request
    ListPullRequests (..),
    newListPullRequests,

    -- * Request Lenses
    listPullRequests_nextToken,
    listPullRequests_maxResults,
    listPullRequests_pullRequestStatus,
    listPullRequests_authorArn,
    listPullRequests_repositoryName,

    -- * Destructuring the Response
    ListPullRequestsResponse (..),
    newListPullRequestsResponse,

    -- * Response Lenses
    listPullRequestsResponse_nextToken,
    listPullRequestsResponse_httpStatus,
    listPullRequestsResponse_pullRequestIds,
  )
where

import Network.AWS.CodeCommit.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListPullRequests' smart constructor.
data ListPullRequests = ListPullRequests'
  { -- | An enumeration token that, when provided in a request, returns the next
    -- batch of the results.
    nextToken :: Core.Maybe Core.Text,
    -- | A non-zero, non-negative integer used to limit the number of returned
    -- results.
    maxResults :: Core.Maybe Core.Int,
    -- | Optional. The status of the pull request. If used, this refines the
    -- results to the pull requests that match the specified status.
    pullRequestStatus :: Core.Maybe PullRequestStatusEnum,
    -- | Optional. The Amazon Resource Name (ARN) of the user who created the
    -- pull request. If used, this filters the results to pull requests created
    -- by that user.
    authorArn :: Core.Maybe Core.Text,
    -- | The name of the repository for which you want to list pull requests.
    repositoryName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListPullRequests' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listPullRequests_nextToken' - An enumeration token that, when provided in a request, returns the next
-- batch of the results.
--
-- 'maxResults', 'listPullRequests_maxResults' - A non-zero, non-negative integer used to limit the number of returned
-- results.
--
-- 'pullRequestStatus', 'listPullRequests_pullRequestStatus' - Optional. The status of the pull request. If used, this refines the
-- results to the pull requests that match the specified status.
--
-- 'authorArn', 'listPullRequests_authorArn' - Optional. The Amazon Resource Name (ARN) of the user who created the
-- pull request. If used, this filters the results to pull requests created
-- by that user.
--
-- 'repositoryName', 'listPullRequests_repositoryName' - The name of the repository for which you want to list pull requests.
newListPullRequests ::
  -- | 'repositoryName'
  Core.Text ->
  ListPullRequests
newListPullRequests pRepositoryName_ =
  ListPullRequests'
    { nextToken = Core.Nothing,
      maxResults = Core.Nothing,
      pullRequestStatus = Core.Nothing,
      authorArn = Core.Nothing,
      repositoryName = pRepositoryName_
    }

-- | An enumeration token that, when provided in a request, returns the next
-- batch of the results.
listPullRequests_nextToken :: Lens.Lens' ListPullRequests (Core.Maybe Core.Text)
listPullRequests_nextToken = Lens.lens (\ListPullRequests' {nextToken} -> nextToken) (\s@ListPullRequests' {} a -> s {nextToken = a} :: ListPullRequests)

-- | A non-zero, non-negative integer used to limit the number of returned
-- results.
listPullRequests_maxResults :: Lens.Lens' ListPullRequests (Core.Maybe Core.Int)
listPullRequests_maxResults = Lens.lens (\ListPullRequests' {maxResults} -> maxResults) (\s@ListPullRequests' {} a -> s {maxResults = a} :: ListPullRequests)

-- | Optional. The status of the pull request. If used, this refines the
-- results to the pull requests that match the specified status.
listPullRequests_pullRequestStatus :: Lens.Lens' ListPullRequests (Core.Maybe PullRequestStatusEnum)
listPullRequests_pullRequestStatus = Lens.lens (\ListPullRequests' {pullRequestStatus} -> pullRequestStatus) (\s@ListPullRequests' {} a -> s {pullRequestStatus = a} :: ListPullRequests)

-- | Optional. The Amazon Resource Name (ARN) of the user who created the
-- pull request. If used, this filters the results to pull requests created
-- by that user.
listPullRequests_authorArn :: Lens.Lens' ListPullRequests (Core.Maybe Core.Text)
listPullRequests_authorArn = Lens.lens (\ListPullRequests' {authorArn} -> authorArn) (\s@ListPullRequests' {} a -> s {authorArn = a} :: ListPullRequests)

-- | The name of the repository for which you want to list pull requests.
listPullRequests_repositoryName :: Lens.Lens' ListPullRequests Core.Text
listPullRequests_repositoryName = Lens.lens (\ListPullRequests' {repositoryName} -> repositoryName) (\s@ListPullRequests' {} a -> s {repositoryName = a} :: ListPullRequests)

instance Core.AWSPager ListPullRequests where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listPullRequestsResponse_nextToken Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        (rs Lens.^. listPullRequestsResponse_pullRequestIds) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listPullRequests_nextToken
          Lens..~ rs
          Lens.^? listPullRequestsResponse_nextToken Core.. Lens._Just

instance Core.AWSRequest ListPullRequests where
  type
    AWSResponse ListPullRequests =
      ListPullRequestsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListPullRequestsResponse'
            Core.<$> (x Core..?> "nextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
            Core.<*> (x Core..?> "pullRequestIds" Core..!@ Core.mempty)
      )

instance Core.Hashable ListPullRequests

instance Core.NFData ListPullRequests

instance Core.ToHeaders ListPullRequests where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "CodeCommit_20150413.ListPullRequests" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListPullRequests where
  toJSON ListPullRequests' {..} =
    Core.object
      ( Core.catMaybes
          [ ("nextToken" Core..=) Core.<$> nextToken,
            ("maxResults" Core..=) Core.<$> maxResults,
            ("pullRequestStatus" Core..=)
              Core.<$> pullRequestStatus,
            ("authorArn" Core..=) Core.<$> authorArn,
            Core.Just ("repositoryName" Core..= repositoryName)
          ]
      )

instance Core.ToPath ListPullRequests where
  toPath = Core.const "/"

instance Core.ToQuery ListPullRequests where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListPullRequestsResponse' smart constructor.
data ListPullRequestsResponse = ListPullRequestsResponse'
  { -- | An enumeration token that allows the operation to batch the next results
    -- of the operation.
    nextToken :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int,
    -- | The system-generated IDs of the pull requests.
    pullRequestIds :: [Core.Text]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListPullRequestsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listPullRequestsResponse_nextToken' - An enumeration token that allows the operation to batch the next results
-- of the operation.
--
-- 'httpStatus', 'listPullRequestsResponse_httpStatus' - The response's http status code.
--
-- 'pullRequestIds', 'listPullRequestsResponse_pullRequestIds' - The system-generated IDs of the pull requests.
newListPullRequestsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListPullRequestsResponse
newListPullRequestsResponse pHttpStatus_ =
  ListPullRequestsResponse'
    { nextToken = Core.Nothing,
      httpStatus = pHttpStatus_,
      pullRequestIds = Core.mempty
    }

-- | An enumeration token that allows the operation to batch the next results
-- of the operation.
listPullRequestsResponse_nextToken :: Lens.Lens' ListPullRequestsResponse (Core.Maybe Core.Text)
listPullRequestsResponse_nextToken = Lens.lens (\ListPullRequestsResponse' {nextToken} -> nextToken) (\s@ListPullRequestsResponse' {} a -> s {nextToken = a} :: ListPullRequestsResponse)

-- | The response's http status code.
listPullRequestsResponse_httpStatus :: Lens.Lens' ListPullRequestsResponse Core.Int
listPullRequestsResponse_httpStatus = Lens.lens (\ListPullRequestsResponse' {httpStatus} -> httpStatus) (\s@ListPullRequestsResponse' {} a -> s {httpStatus = a} :: ListPullRequestsResponse)

-- | The system-generated IDs of the pull requests.
listPullRequestsResponse_pullRequestIds :: Lens.Lens' ListPullRequestsResponse [Core.Text]
listPullRequestsResponse_pullRequestIds = Lens.lens (\ListPullRequestsResponse' {pullRequestIds} -> pullRequestIds) (\s@ListPullRequestsResponse' {} a -> s {pullRequestIds = a} :: ListPullRequestsResponse) Core.. Lens._Coerce

instance Core.NFData ListPullRequestsResponse
