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
-- Module      : Amazonka.CodeCommit.GetCommentsForPullRequest
-- Copyright   : (c) 2013-2023 Brendan Hay
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
module Amazonka.CodeCommit.GetCommentsForPullRequest
  ( -- * Creating a Request
    GetCommentsForPullRequest (..),
    newGetCommentsForPullRequest,

    -- * Request Lenses
    getCommentsForPullRequest_afterCommitId,
    getCommentsForPullRequest_beforeCommitId,
    getCommentsForPullRequest_maxResults,
    getCommentsForPullRequest_nextToken,
    getCommentsForPullRequest_repositoryName,
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

import Amazonka.CodeCommit.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetCommentsForPullRequest' smart constructor.
data GetCommentsForPullRequest = GetCommentsForPullRequest'
  { -- | The full commit ID of the commit in the source branch that was the tip
    -- of the branch at the time the comment was made.
    afterCommitId :: Prelude.Maybe Prelude.Text,
    -- | The full commit ID of the commit in the destination branch that was the
    -- tip of the branch at the time the pull request was created.
    beforeCommitId :: Prelude.Maybe Prelude.Text,
    -- | A non-zero, non-negative integer used to limit the number of returned
    -- results. The default is 100 comments. You can return up to 500 comments
    -- with a single request.
    maxResults :: Prelude.Maybe Prelude.Int,
    -- | An enumeration token that, when provided in a request, returns the next
    -- batch of the results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The name of the repository that contains the pull request.
    repositoryName :: Prelude.Maybe Prelude.Text,
    -- | The system-generated ID of the pull request. To get this ID, use
    -- ListPullRequests.
    pullRequestId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetCommentsForPullRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'afterCommitId', 'getCommentsForPullRequest_afterCommitId' - The full commit ID of the commit in the source branch that was the tip
-- of the branch at the time the comment was made.
--
-- 'beforeCommitId', 'getCommentsForPullRequest_beforeCommitId' - The full commit ID of the commit in the destination branch that was the
-- tip of the branch at the time the pull request was created.
--
-- 'maxResults', 'getCommentsForPullRequest_maxResults' - A non-zero, non-negative integer used to limit the number of returned
-- results. The default is 100 comments. You can return up to 500 comments
-- with a single request.
--
-- 'nextToken', 'getCommentsForPullRequest_nextToken' - An enumeration token that, when provided in a request, returns the next
-- batch of the results.
--
-- 'repositoryName', 'getCommentsForPullRequest_repositoryName' - The name of the repository that contains the pull request.
--
-- 'pullRequestId', 'getCommentsForPullRequest_pullRequestId' - The system-generated ID of the pull request. To get this ID, use
-- ListPullRequests.
newGetCommentsForPullRequest ::
  -- | 'pullRequestId'
  Prelude.Text ->
  GetCommentsForPullRequest
newGetCommentsForPullRequest pPullRequestId_ =
  GetCommentsForPullRequest'
    { afterCommitId =
        Prelude.Nothing,
      beforeCommitId = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      repositoryName = Prelude.Nothing,
      pullRequestId = pPullRequestId_
    }

-- | The full commit ID of the commit in the source branch that was the tip
-- of the branch at the time the comment was made.
getCommentsForPullRequest_afterCommitId :: Lens.Lens' GetCommentsForPullRequest (Prelude.Maybe Prelude.Text)
getCommentsForPullRequest_afterCommitId = Lens.lens (\GetCommentsForPullRequest' {afterCommitId} -> afterCommitId) (\s@GetCommentsForPullRequest' {} a -> s {afterCommitId = a} :: GetCommentsForPullRequest)

-- | The full commit ID of the commit in the destination branch that was the
-- tip of the branch at the time the pull request was created.
getCommentsForPullRequest_beforeCommitId :: Lens.Lens' GetCommentsForPullRequest (Prelude.Maybe Prelude.Text)
getCommentsForPullRequest_beforeCommitId = Lens.lens (\GetCommentsForPullRequest' {beforeCommitId} -> beforeCommitId) (\s@GetCommentsForPullRequest' {} a -> s {beforeCommitId = a} :: GetCommentsForPullRequest)

-- | A non-zero, non-negative integer used to limit the number of returned
-- results. The default is 100 comments. You can return up to 500 comments
-- with a single request.
getCommentsForPullRequest_maxResults :: Lens.Lens' GetCommentsForPullRequest (Prelude.Maybe Prelude.Int)
getCommentsForPullRequest_maxResults = Lens.lens (\GetCommentsForPullRequest' {maxResults} -> maxResults) (\s@GetCommentsForPullRequest' {} a -> s {maxResults = a} :: GetCommentsForPullRequest)

-- | An enumeration token that, when provided in a request, returns the next
-- batch of the results.
getCommentsForPullRequest_nextToken :: Lens.Lens' GetCommentsForPullRequest (Prelude.Maybe Prelude.Text)
getCommentsForPullRequest_nextToken = Lens.lens (\GetCommentsForPullRequest' {nextToken} -> nextToken) (\s@GetCommentsForPullRequest' {} a -> s {nextToken = a} :: GetCommentsForPullRequest)

-- | The name of the repository that contains the pull request.
getCommentsForPullRequest_repositoryName :: Lens.Lens' GetCommentsForPullRequest (Prelude.Maybe Prelude.Text)
getCommentsForPullRequest_repositoryName = Lens.lens (\GetCommentsForPullRequest' {repositoryName} -> repositoryName) (\s@GetCommentsForPullRequest' {} a -> s {repositoryName = a} :: GetCommentsForPullRequest)

-- | The system-generated ID of the pull request. To get this ID, use
-- ListPullRequests.
getCommentsForPullRequest_pullRequestId :: Lens.Lens' GetCommentsForPullRequest Prelude.Text
getCommentsForPullRequest_pullRequestId = Lens.lens (\GetCommentsForPullRequest' {pullRequestId} -> pullRequestId) (\s@GetCommentsForPullRequest' {} a -> s {pullRequestId = a} :: GetCommentsForPullRequest)

instance Core.AWSPager GetCommentsForPullRequest where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getCommentsForPullRequestResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? getCommentsForPullRequestResponse_commentsForPullRequestData
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& getCommentsForPullRequest_nextToken
          Lens..~ rs
          Lens.^? getCommentsForPullRequestResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest GetCommentsForPullRequest where
  type
    AWSResponse GetCommentsForPullRequest =
      GetCommentsForPullRequestResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetCommentsForPullRequestResponse'
            Prelude.<$> ( x
                            Data..?> "commentsForPullRequestData"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetCommentsForPullRequest where
  hashWithSalt _salt GetCommentsForPullRequest' {..} =
    _salt
      `Prelude.hashWithSalt` afterCommitId
      `Prelude.hashWithSalt` beforeCommitId
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` repositoryName
      `Prelude.hashWithSalt` pullRequestId

instance Prelude.NFData GetCommentsForPullRequest where
  rnf GetCommentsForPullRequest' {..} =
    Prelude.rnf afterCommitId
      `Prelude.seq` Prelude.rnf beforeCommitId
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf repositoryName
      `Prelude.seq` Prelude.rnf pullRequestId

instance Data.ToHeaders GetCommentsForPullRequest where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "CodeCommit_20150413.GetCommentsForPullRequest" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetCommentsForPullRequest where
  toJSON GetCommentsForPullRequest' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("afterCommitId" Data..=) Prelude.<$> afterCommitId,
            ("beforeCommitId" Data..=)
              Prelude.<$> beforeCommitId,
            ("maxResults" Data..=) Prelude.<$> maxResults,
            ("nextToken" Data..=) Prelude.<$> nextToken,
            ("repositoryName" Data..=)
              Prelude.<$> repositoryName,
            Prelude.Just
              ("pullRequestId" Data..= pullRequestId)
          ]
      )

instance Data.ToPath GetCommentsForPullRequest where
  toPath = Prelude.const "/"

instance Data.ToQuery GetCommentsForPullRequest where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetCommentsForPullRequestResponse' smart constructor.
data GetCommentsForPullRequestResponse = GetCommentsForPullRequestResponse'
  { -- | An array of comment objects on the pull request.
    commentsForPullRequestData :: Prelude.Maybe [CommentsForPullRequest],
    -- | An enumeration token that can be used in a request to return the next
    -- batch of the results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  GetCommentsForPullRequestResponse
newGetCommentsForPullRequestResponse pHttpStatus_ =
  GetCommentsForPullRequestResponse'
    { commentsForPullRequestData =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of comment objects on the pull request.
getCommentsForPullRequestResponse_commentsForPullRequestData :: Lens.Lens' GetCommentsForPullRequestResponse (Prelude.Maybe [CommentsForPullRequest])
getCommentsForPullRequestResponse_commentsForPullRequestData = Lens.lens (\GetCommentsForPullRequestResponse' {commentsForPullRequestData} -> commentsForPullRequestData) (\s@GetCommentsForPullRequestResponse' {} a -> s {commentsForPullRequestData = a} :: GetCommentsForPullRequestResponse) Prelude.. Lens.mapping Lens.coerced

-- | An enumeration token that can be used in a request to return the next
-- batch of the results.
getCommentsForPullRequestResponse_nextToken :: Lens.Lens' GetCommentsForPullRequestResponse (Prelude.Maybe Prelude.Text)
getCommentsForPullRequestResponse_nextToken = Lens.lens (\GetCommentsForPullRequestResponse' {nextToken} -> nextToken) (\s@GetCommentsForPullRequestResponse' {} a -> s {nextToken = a} :: GetCommentsForPullRequestResponse)

-- | The response's http status code.
getCommentsForPullRequestResponse_httpStatus :: Lens.Lens' GetCommentsForPullRequestResponse Prelude.Int
getCommentsForPullRequestResponse_httpStatus = Lens.lens (\GetCommentsForPullRequestResponse' {httpStatus} -> httpStatus) (\s@GetCommentsForPullRequestResponse' {} a -> s {httpStatus = a} :: GetCommentsForPullRequestResponse)

instance
  Prelude.NFData
    GetCommentsForPullRequestResponse
  where
  rnf GetCommentsForPullRequestResponse' {..} =
    Prelude.rnf commentsForPullRequestData
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
