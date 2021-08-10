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
-- Module      : Network.AWS.CodeCommit.GetCommentsForComparedCommit
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about comments made on the comparison between two
-- commits.
--
-- Reaction counts might include numbers from user identities who were
-- deleted after the reaction was made. For a count of reactions from
-- active identities, use GetCommentReactions.
--
-- This operation returns paginated results.
module Network.AWS.CodeCommit.GetCommentsForComparedCommit
  ( -- * Creating a Request
    GetCommentsForComparedCommit (..),
    newGetCommentsForComparedCommit,

    -- * Request Lenses
    getCommentsForComparedCommit_nextToken,
    getCommentsForComparedCommit_maxResults,
    getCommentsForComparedCommit_beforeCommitId,
    getCommentsForComparedCommit_repositoryName,
    getCommentsForComparedCommit_afterCommitId,

    -- * Destructuring the Response
    GetCommentsForComparedCommitResponse (..),
    newGetCommentsForComparedCommitResponse,

    -- * Response Lenses
    getCommentsForComparedCommitResponse_nextToken,
    getCommentsForComparedCommitResponse_commentsForComparedCommitData,
    getCommentsForComparedCommitResponse_httpStatus,
  )
where

import Network.AWS.CodeCommit.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetCommentsForComparedCommit' smart constructor.
data GetCommentsForComparedCommit = GetCommentsForComparedCommit'
  { -- | An enumeration token that when provided in a request, returns the next
    -- batch of the results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A non-zero, non-negative integer used to limit the number of returned
    -- results. The default is 100 comments, but you can configure up to 500.
    maxResults :: Prelude.Maybe Prelude.Int,
    -- | To establish the directionality of the comparison, the full commit ID of
    -- the before commit.
    beforeCommitId :: Prelude.Maybe Prelude.Text,
    -- | The name of the repository where you want to compare commits.
    repositoryName :: Prelude.Text,
    -- | To establish the directionality of the comparison, the full commit ID of
    -- the after commit.
    afterCommitId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetCommentsForComparedCommit' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getCommentsForComparedCommit_nextToken' - An enumeration token that when provided in a request, returns the next
-- batch of the results.
--
-- 'maxResults', 'getCommentsForComparedCommit_maxResults' - A non-zero, non-negative integer used to limit the number of returned
-- results. The default is 100 comments, but you can configure up to 500.
--
-- 'beforeCommitId', 'getCommentsForComparedCommit_beforeCommitId' - To establish the directionality of the comparison, the full commit ID of
-- the before commit.
--
-- 'repositoryName', 'getCommentsForComparedCommit_repositoryName' - The name of the repository where you want to compare commits.
--
-- 'afterCommitId', 'getCommentsForComparedCommit_afterCommitId' - To establish the directionality of the comparison, the full commit ID of
-- the after commit.
newGetCommentsForComparedCommit ::
  -- | 'repositoryName'
  Prelude.Text ->
  -- | 'afterCommitId'
  Prelude.Text ->
  GetCommentsForComparedCommit
newGetCommentsForComparedCommit
  pRepositoryName_
  pAfterCommitId_ =
    GetCommentsForComparedCommit'
      { nextToken =
          Prelude.Nothing,
        maxResults = Prelude.Nothing,
        beforeCommitId = Prelude.Nothing,
        repositoryName = pRepositoryName_,
        afterCommitId = pAfterCommitId_
      }

-- | An enumeration token that when provided in a request, returns the next
-- batch of the results.
getCommentsForComparedCommit_nextToken :: Lens.Lens' GetCommentsForComparedCommit (Prelude.Maybe Prelude.Text)
getCommentsForComparedCommit_nextToken = Lens.lens (\GetCommentsForComparedCommit' {nextToken} -> nextToken) (\s@GetCommentsForComparedCommit' {} a -> s {nextToken = a} :: GetCommentsForComparedCommit)

-- | A non-zero, non-negative integer used to limit the number of returned
-- results. The default is 100 comments, but you can configure up to 500.
getCommentsForComparedCommit_maxResults :: Lens.Lens' GetCommentsForComparedCommit (Prelude.Maybe Prelude.Int)
getCommentsForComparedCommit_maxResults = Lens.lens (\GetCommentsForComparedCommit' {maxResults} -> maxResults) (\s@GetCommentsForComparedCommit' {} a -> s {maxResults = a} :: GetCommentsForComparedCommit)

-- | To establish the directionality of the comparison, the full commit ID of
-- the before commit.
getCommentsForComparedCommit_beforeCommitId :: Lens.Lens' GetCommentsForComparedCommit (Prelude.Maybe Prelude.Text)
getCommentsForComparedCommit_beforeCommitId = Lens.lens (\GetCommentsForComparedCommit' {beforeCommitId} -> beforeCommitId) (\s@GetCommentsForComparedCommit' {} a -> s {beforeCommitId = a} :: GetCommentsForComparedCommit)

-- | The name of the repository where you want to compare commits.
getCommentsForComparedCommit_repositoryName :: Lens.Lens' GetCommentsForComparedCommit Prelude.Text
getCommentsForComparedCommit_repositoryName = Lens.lens (\GetCommentsForComparedCommit' {repositoryName} -> repositoryName) (\s@GetCommentsForComparedCommit' {} a -> s {repositoryName = a} :: GetCommentsForComparedCommit)

-- | To establish the directionality of the comparison, the full commit ID of
-- the after commit.
getCommentsForComparedCommit_afterCommitId :: Lens.Lens' GetCommentsForComparedCommit Prelude.Text
getCommentsForComparedCommit_afterCommitId = Lens.lens (\GetCommentsForComparedCommit' {afterCommitId} -> afterCommitId) (\s@GetCommentsForComparedCommit' {} a -> s {afterCommitId = a} :: GetCommentsForComparedCommit)

instance Core.AWSPager GetCommentsForComparedCommit where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getCommentsForComparedCommitResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? getCommentsForComparedCommitResponse_commentsForComparedCommitData
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& getCommentsForComparedCommit_nextToken
          Lens..~ rs
          Lens.^? getCommentsForComparedCommitResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest GetCommentsForComparedCommit where
  type
    AWSResponse GetCommentsForComparedCommit =
      GetCommentsForComparedCommitResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetCommentsForComparedCommitResponse'
            Prelude.<$> (x Core..?> "nextToken")
            Prelude.<*> ( x Core..?> "commentsForComparedCommitData"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    GetCommentsForComparedCommit

instance Prelude.NFData GetCommentsForComparedCommit

instance Core.ToHeaders GetCommentsForComparedCommit where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "CodeCommit_20150413.GetCommentsForComparedCommit" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GetCommentsForComparedCommit where
  toJSON GetCommentsForComparedCommit' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("nextToken" Core..=) Prelude.<$> nextToken,
            ("maxResults" Core..=) Prelude.<$> maxResults,
            ("beforeCommitId" Core..=)
              Prelude.<$> beforeCommitId,
            Prelude.Just
              ("repositoryName" Core..= repositoryName),
            Prelude.Just
              ("afterCommitId" Core..= afterCommitId)
          ]
      )

instance Core.ToPath GetCommentsForComparedCommit where
  toPath = Prelude.const "/"

instance Core.ToQuery GetCommentsForComparedCommit where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetCommentsForComparedCommitResponse' smart constructor.
data GetCommentsForComparedCommitResponse = GetCommentsForComparedCommitResponse'
  { -- | An enumeration token that can be used in a request to return the next
    -- batch of the results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of comment objects on the compared commit.
    commentsForComparedCommitData :: Prelude.Maybe [CommentsForComparedCommit],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetCommentsForComparedCommitResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getCommentsForComparedCommitResponse_nextToken' - An enumeration token that can be used in a request to return the next
-- batch of the results.
--
-- 'commentsForComparedCommitData', 'getCommentsForComparedCommitResponse_commentsForComparedCommitData' - A list of comment objects on the compared commit.
--
-- 'httpStatus', 'getCommentsForComparedCommitResponse_httpStatus' - The response's http status code.
newGetCommentsForComparedCommitResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetCommentsForComparedCommitResponse
newGetCommentsForComparedCommitResponse pHttpStatus_ =
  GetCommentsForComparedCommitResponse'
    { nextToken =
        Prelude.Nothing,
      commentsForComparedCommitData =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An enumeration token that can be used in a request to return the next
-- batch of the results.
getCommentsForComparedCommitResponse_nextToken :: Lens.Lens' GetCommentsForComparedCommitResponse (Prelude.Maybe Prelude.Text)
getCommentsForComparedCommitResponse_nextToken = Lens.lens (\GetCommentsForComparedCommitResponse' {nextToken} -> nextToken) (\s@GetCommentsForComparedCommitResponse' {} a -> s {nextToken = a} :: GetCommentsForComparedCommitResponse)

-- | A list of comment objects on the compared commit.
getCommentsForComparedCommitResponse_commentsForComparedCommitData :: Lens.Lens' GetCommentsForComparedCommitResponse (Prelude.Maybe [CommentsForComparedCommit])
getCommentsForComparedCommitResponse_commentsForComparedCommitData = Lens.lens (\GetCommentsForComparedCommitResponse' {commentsForComparedCommitData} -> commentsForComparedCommitData) (\s@GetCommentsForComparedCommitResponse' {} a -> s {commentsForComparedCommitData = a} :: GetCommentsForComparedCommitResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
getCommentsForComparedCommitResponse_httpStatus :: Lens.Lens' GetCommentsForComparedCommitResponse Prelude.Int
getCommentsForComparedCommitResponse_httpStatus = Lens.lens (\GetCommentsForComparedCommitResponse' {httpStatus} -> httpStatus) (\s@GetCommentsForComparedCommitResponse' {} a -> s {httpStatus = a} :: GetCommentsForComparedCommitResponse)

instance
  Prelude.NFData
    GetCommentsForComparedCommitResponse
