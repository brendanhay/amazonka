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
-- Module      : Amazonka.CodeCommit.GetCommentsForComparedCommit
-- Copyright   : (c) 2013-2022 Brendan Hay
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
module Amazonka.CodeCommit.GetCommentsForComparedCommit
  ( -- * Creating a Request
    GetCommentsForComparedCommit (..),
    newGetCommentsForComparedCommit,

    -- * Request Lenses
    getCommentsForComparedCommit_beforeCommitId,
    getCommentsForComparedCommit_maxResults,
    getCommentsForComparedCommit_nextToken,
    getCommentsForComparedCommit_repositoryName,
    getCommentsForComparedCommit_afterCommitId,

    -- * Destructuring the Response
    GetCommentsForComparedCommitResponse (..),
    newGetCommentsForComparedCommitResponse,

    -- * Response Lenses
    getCommentsForComparedCommitResponse_commentsForComparedCommitData,
    getCommentsForComparedCommitResponse_nextToken,
    getCommentsForComparedCommitResponse_httpStatus,
  )
where

import Amazonka.CodeCommit.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetCommentsForComparedCommit' smart constructor.
data GetCommentsForComparedCommit = GetCommentsForComparedCommit'
  { -- | To establish the directionality of the comparison, the full commit ID of
    -- the before commit.
    beforeCommitId :: Prelude.Maybe Prelude.Text,
    -- | A non-zero, non-negative integer used to limit the number of returned
    -- results. The default is 100 comments, but you can configure up to 500.
    maxResults :: Prelude.Maybe Prelude.Int,
    -- | An enumeration token that when provided in a request, returns the next
    -- batch of the results.
    nextToken :: Prelude.Maybe Prelude.Text,
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
-- 'beforeCommitId', 'getCommentsForComparedCommit_beforeCommitId' - To establish the directionality of the comparison, the full commit ID of
-- the before commit.
--
-- 'maxResults', 'getCommentsForComparedCommit_maxResults' - A non-zero, non-negative integer used to limit the number of returned
-- results. The default is 100 comments, but you can configure up to 500.
--
-- 'nextToken', 'getCommentsForComparedCommit_nextToken' - An enumeration token that when provided in a request, returns the next
-- batch of the results.
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
      { beforeCommitId =
          Prelude.Nothing,
        maxResults = Prelude.Nothing,
        nextToken = Prelude.Nothing,
        repositoryName = pRepositoryName_,
        afterCommitId = pAfterCommitId_
      }

-- | To establish the directionality of the comparison, the full commit ID of
-- the before commit.
getCommentsForComparedCommit_beforeCommitId :: Lens.Lens' GetCommentsForComparedCommit (Prelude.Maybe Prelude.Text)
getCommentsForComparedCommit_beforeCommitId = Lens.lens (\GetCommentsForComparedCommit' {beforeCommitId} -> beforeCommitId) (\s@GetCommentsForComparedCommit' {} a -> s {beforeCommitId = a} :: GetCommentsForComparedCommit)

-- | A non-zero, non-negative integer used to limit the number of returned
-- results. The default is 100 comments, but you can configure up to 500.
getCommentsForComparedCommit_maxResults :: Lens.Lens' GetCommentsForComparedCommit (Prelude.Maybe Prelude.Int)
getCommentsForComparedCommit_maxResults = Lens.lens (\GetCommentsForComparedCommit' {maxResults} -> maxResults) (\s@GetCommentsForComparedCommit' {} a -> s {maxResults = a} :: GetCommentsForComparedCommit)

-- | An enumeration token that when provided in a request, returns the next
-- batch of the results.
getCommentsForComparedCommit_nextToken :: Lens.Lens' GetCommentsForComparedCommit (Prelude.Maybe Prelude.Text)
getCommentsForComparedCommit_nextToken = Lens.lens (\GetCommentsForComparedCommit' {nextToken} -> nextToken) (\s@GetCommentsForComparedCommit' {} a -> s {nextToken = a} :: GetCommentsForComparedCommit)

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
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetCommentsForComparedCommitResponse'
            Prelude.<$> ( x Data..?> "commentsForComparedCommitData"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    GetCommentsForComparedCommit
  where
  hashWithSalt _salt GetCommentsForComparedCommit' {..} =
    _salt `Prelude.hashWithSalt` beforeCommitId
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` repositoryName
      `Prelude.hashWithSalt` afterCommitId

instance Prelude.NFData GetCommentsForComparedCommit where
  rnf GetCommentsForComparedCommit' {..} =
    Prelude.rnf beforeCommitId
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf repositoryName
      `Prelude.seq` Prelude.rnf afterCommitId

instance Data.ToHeaders GetCommentsForComparedCommit where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "CodeCommit_20150413.GetCommentsForComparedCommit" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetCommentsForComparedCommit where
  toJSON GetCommentsForComparedCommit' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("beforeCommitId" Data..=)
              Prelude.<$> beforeCommitId,
            ("maxResults" Data..=) Prelude.<$> maxResults,
            ("nextToken" Data..=) Prelude.<$> nextToken,
            Prelude.Just
              ("repositoryName" Data..= repositoryName),
            Prelude.Just
              ("afterCommitId" Data..= afterCommitId)
          ]
      )

instance Data.ToPath GetCommentsForComparedCommit where
  toPath = Prelude.const "/"

instance Data.ToQuery GetCommentsForComparedCommit where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetCommentsForComparedCommitResponse' smart constructor.
data GetCommentsForComparedCommitResponse = GetCommentsForComparedCommitResponse'
  { -- | A list of comment objects on the compared commit.
    commentsForComparedCommitData :: Prelude.Maybe [CommentsForComparedCommit],
    -- | An enumeration token that can be used in a request to return the next
    -- batch of the results.
    nextToken :: Prelude.Maybe Prelude.Text,
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
-- 'commentsForComparedCommitData', 'getCommentsForComparedCommitResponse_commentsForComparedCommitData' - A list of comment objects on the compared commit.
--
-- 'nextToken', 'getCommentsForComparedCommitResponse_nextToken' - An enumeration token that can be used in a request to return the next
-- batch of the results.
--
-- 'httpStatus', 'getCommentsForComparedCommitResponse_httpStatus' - The response's http status code.
newGetCommentsForComparedCommitResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetCommentsForComparedCommitResponse
newGetCommentsForComparedCommitResponse pHttpStatus_ =
  GetCommentsForComparedCommitResponse'
    { commentsForComparedCommitData =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of comment objects on the compared commit.
getCommentsForComparedCommitResponse_commentsForComparedCommitData :: Lens.Lens' GetCommentsForComparedCommitResponse (Prelude.Maybe [CommentsForComparedCommit])
getCommentsForComparedCommitResponse_commentsForComparedCommitData = Lens.lens (\GetCommentsForComparedCommitResponse' {commentsForComparedCommitData} -> commentsForComparedCommitData) (\s@GetCommentsForComparedCommitResponse' {} a -> s {commentsForComparedCommitData = a} :: GetCommentsForComparedCommitResponse) Prelude.. Lens.mapping Lens.coerced

-- | An enumeration token that can be used in a request to return the next
-- batch of the results.
getCommentsForComparedCommitResponse_nextToken :: Lens.Lens' GetCommentsForComparedCommitResponse (Prelude.Maybe Prelude.Text)
getCommentsForComparedCommitResponse_nextToken = Lens.lens (\GetCommentsForComparedCommitResponse' {nextToken} -> nextToken) (\s@GetCommentsForComparedCommitResponse' {} a -> s {nextToken = a} :: GetCommentsForComparedCommitResponse)

-- | The response's http status code.
getCommentsForComparedCommitResponse_httpStatus :: Lens.Lens' GetCommentsForComparedCommitResponse Prelude.Int
getCommentsForComparedCommitResponse_httpStatus = Lens.lens (\GetCommentsForComparedCommitResponse' {httpStatus} -> httpStatus) (\s@GetCommentsForComparedCommitResponse' {} a -> s {httpStatus = a} :: GetCommentsForComparedCommitResponse)

instance
  Prelude.NFData
    GetCommentsForComparedCommitResponse
  where
  rnf GetCommentsForComparedCommitResponse' {..} =
    Prelude.rnf commentsForComparedCommitData
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
