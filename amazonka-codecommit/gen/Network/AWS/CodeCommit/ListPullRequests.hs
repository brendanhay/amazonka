{-# LANGUAGE DeriveDataTypeable #-}
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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListPullRequests' smart constructor.
data ListPullRequests = ListPullRequests'
  { -- | An enumeration token that, when provided in a request, returns the next
    -- batch of the results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A non-zero, non-negative integer used to limit the number of returned
    -- results.
    maxResults :: Prelude.Maybe Prelude.Int,
    -- | Optional. The status of the pull request. If used, this refines the
    -- results to the pull requests that match the specified status.
    pullRequestStatus :: Prelude.Maybe PullRequestStatusEnum,
    -- | Optional. The Amazon Resource Name (ARN) of the user who created the
    -- pull request. If used, this filters the results to pull requests created
    -- by that user.
    authorArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the repository for which you want to list pull requests.
    repositoryName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  ListPullRequests
newListPullRequests pRepositoryName_ =
  ListPullRequests'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      pullRequestStatus = Prelude.Nothing,
      authorArn = Prelude.Nothing,
      repositoryName = pRepositoryName_
    }

-- | An enumeration token that, when provided in a request, returns the next
-- batch of the results.
listPullRequests_nextToken :: Lens.Lens' ListPullRequests (Prelude.Maybe Prelude.Text)
listPullRequests_nextToken = Lens.lens (\ListPullRequests' {nextToken} -> nextToken) (\s@ListPullRequests' {} a -> s {nextToken = a} :: ListPullRequests)

-- | A non-zero, non-negative integer used to limit the number of returned
-- results.
listPullRequests_maxResults :: Lens.Lens' ListPullRequests (Prelude.Maybe Prelude.Int)
listPullRequests_maxResults = Lens.lens (\ListPullRequests' {maxResults} -> maxResults) (\s@ListPullRequests' {} a -> s {maxResults = a} :: ListPullRequests)

-- | Optional. The status of the pull request. If used, this refines the
-- results to the pull requests that match the specified status.
listPullRequests_pullRequestStatus :: Lens.Lens' ListPullRequests (Prelude.Maybe PullRequestStatusEnum)
listPullRequests_pullRequestStatus = Lens.lens (\ListPullRequests' {pullRequestStatus} -> pullRequestStatus) (\s@ListPullRequests' {} a -> s {pullRequestStatus = a} :: ListPullRequests)

-- | Optional. The Amazon Resource Name (ARN) of the user who created the
-- pull request. If used, this filters the results to pull requests created
-- by that user.
listPullRequests_authorArn :: Lens.Lens' ListPullRequests (Prelude.Maybe Prelude.Text)
listPullRequests_authorArn = Lens.lens (\ListPullRequests' {authorArn} -> authorArn) (\s@ListPullRequests' {} a -> s {authorArn = a} :: ListPullRequests)

-- | The name of the repository for which you want to list pull requests.
listPullRequests_repositoryName :: Lens.Lens' ListPullRequests Prelude.Text
listPullRequests_repositoryName = Lens.lens (\ListPullRequests' {repositoryName} -> repositoryName) (\s@ListPullRequests' {} a -> s {repositoryName = a} :: ListPullRequests)

instance Pager.AWSPager ListPullRequests where
  page rq rs
    | Pager.stop
        ( rs
            Lens.^? listPullRequestsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Pager.stop
        (rs Lens.^. listPullRequestsResponse_pullRequestIds) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Lens.& listPullRequests_nextToken
          Lens..~ rs
          Lens.^? listPullRequestsResponse_nextToken
            Prelude.. Lens._Just

instance Prelude.AWSRequest ListPullRequests where
  type Rs ListPullRequests = ListPullRequestsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListPullRequestsResponse'
            Prelude.<$> (x Prelude..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x Prelude..?> "pullRequestIds"
                            Prelude..!@ Prelude.mempty
                        )
      )

instance Prelude.Hashable ListPullRequests

instance Prelude.NFData ListPullRequests

instance Prelude.ToHeaders ListPullRequests where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "CodeCommit_20150413.ListPullRequests" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON ListPullRequests where
  toJSON ListPullRequests' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("nextToken" Prelude..=) Prelude.<$> nextToken,
            ("maxResults" Prelude..=) Prelude.<$> maxResults,
            ("pullRequestStatus" Prelude..=)
              Prelude.<$> pullRequestStatus,
            ("authorArn" Prelude..=) Prelude.<$> authorArn,
            Prelude.Just
              ("repositoryName" Prelude..= repositoryName)
          ]
      )

instance Prelude.ToPath ListPullRequests where
  toPath = Prelude.const "/"

instance Prelude.ToQuery ListPullRequests where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListPullRequestsResponse' smart constructor.
data ListPullRequestsResponse = ListPullRequestsResponse'
  { -- | An enumeration token that allows the operation to batch the next results
    -- of the operation.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The system-generated IDs of the pull requests.
    pullRequestIds :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  ListPullRequestsResponse
newListPullRequestsResponse pHttpStatus_ =
  ListPullRequestsResponse'
    { nextToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      pullRequestIds = Prelude.mempty
    }

-- | An enumeration token that allows the operation to batch the next results
-- of the operation.
listPullRequestsResponse_nextToken :: Lens.Lens' ListPullRequestsResponse (Prelude.Maybe Prelude.Text)
listPullRequestsResponse_nextToken = Lens.lens (\ListPullRequestsResponse' {nextToken} -> nextToken) (\s@ListPullRequestsResponse' {} a -> s {nextToken = a} :: ListPullRequestsResponse)

-- | The response's http status code.
listPullRequestsResponse_httpStatus :: Lens.Lens' ListPullRequestsResponse Prelude.Int
listPullRequestsResponse_httpStatus = Lens.lens (\ListPullRequestsResponse' {httpStatus} -> httpStatus) (\s@ListPullRequestsResponse' {} a -> s {httpStatus = a} :: ListPullRequestsResponse)

-- | The system-generated IDs of the pull requests.
listPullRequestsResponse_pullRequestIds :: Lens.Lens' ListPullRequestsResponse [Prelude.Text]
listPullRequestsResponse_pullRequestIds = Lens.lens (\ListPullRequestsResponse' {pullRequestIds} -> pullRequestIds) (\s@ListPullRequestsResponse' {} a -> s {pullRequestIds = a} :: ListPullRequestsResponse) Prelude.. Prelude._Coerce

instance Prelude.NFData ListPullRequestsResponse
