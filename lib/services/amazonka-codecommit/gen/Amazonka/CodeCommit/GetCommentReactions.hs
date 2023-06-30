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
-- Module      : Amazonka.CodeCommit.GetCommentReactions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about reactions to a specified comment ID. Reactions
-- from users who have been deleted will not be included in the count.
module Amazonka.CodeCommit.GetCommentReactions
  ( -- * Creating a Request
    GetCommentReactions (..),
    newGetCommentReactions,

    -- * Request Lenses
    getCommentReactions_maxResults,
    getCommentReactions_nextToken,
    getCommentReactions_reactionUserArn,
    getCommentReactions_commentId,

    -- * Destructuring the Response
    GetCommentReactionsResponse (..),
    newGetCommentReactionsResponse,

    -- * Response Lenses
    getCommentReactionsResponse_nextToken,
    getCommentReactionsResponse_httpStatus,
    getCommentReactionsResponse_reactionsForComment,
  )
where

import Amazonka.CodeCommit.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetCommentReactions' smart constructor.
data GetCommentReactions = GetCommentReactions'
  { -- | A non-zero, non-negative integer used to limit the number of returned
    -- results. The default is the same as the allowed maximum, 1,000.
    maxResults :: Prelude.Maybe Prelude.Int,
    -- | An enumeration token that, when provided in a request, returns the next
    -- batch of the results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Optional. The Amazon Resource Name (ARN) of the user or identity for
    -- which you want to get reaction information.
    reactionUserArn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the comment for which you want to get reactions information.
    commentId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetCommentReactions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'getCommentReactions_maxResults' - A non-zero, non-negative integer used to limit the number of returned
-- results. The default is the same as the allowed maximum, 1,000.
--
-- 'nextToken', 'getCommentReactions_nextToken' - An enumeration token that, when provided in a request, returns the next
-- batch of the results.
--
-- 'reactionUserArn', 'getCommentReactions_reactionUserArn' - Optional. The Amazon Resource Name (ARN) of the user or identity for
-- which you want to get reaction information.
--
-- 'commentId', 'getCommentReactions_commentId' - The ID of the comment for which you want to get reactions information.
newGetCommentReactions ::
  -- | 'commentId'
  Prelude.Text ->
  GetCommentReactions
newGetCommentReactions pCommentId_ =
  GetCommentReactions'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      reactionUserArn = Prelude.Nothing,
      commentId = pCommentId_
    }

-- | A non-zero, non-negative integer used to limit the number of returned
-- results. The default is the same as the allowed maximum, 1,000.
getCommentReactions_maxResults :: Lens.Lens' GetCommentReactions (Prelude.Maybe Prelude.Int)
getCommentReactions_maxResults = Lens.lens (\GetCommentReactions' {maxResults} -> maxResults) (\s@GetCommentReactions' {} a -> s {maxResults = a} :: GetCommentReactions)

-- | An enumeration token that, when provided in a request, returns the next
-- batch of the results.
getCommentReactions_nextToken :: Lens.Lens' GetCommentReactions (Prelude.Maybe Prelude.Text)
getCommentReactions_nextToken = Lens.lens (\GetCommentReactions' {nextToken} -> nextToken) (\s@GetCommentReactions' {} a -> s {nextToken = a} :: GetCommentReactions)

-- | Optional. The Amazon Resource Name (ARN) of the user or identity for
-- which you want to get reaction information.
getCommentReactions_reactionUserArn :: Lens.Lens' GetCommentReactions (Prelude.Maybe Prelude.Text)
getCommentReactions_reactionUserArn = Lens.lens (\GetCommentReactions' {reactionUserArn} -> reactionUserArn) (\s@GetCommentReactions' {} a -> s {reactionUserArn = a} :: GetCommentReactions)

-- | The ID of the comment for which you want to get reactions information.
getCommentReactions_commentId :: Lens.Lens' GetCommentReactions Prelude.Text
getCommentReactions_commentId = Lens.lens (\GetCommentReactions' {commentId} -> commentId) (\s@GetCommentReactions' {} a -> s {commentId = a} :: GetCommentReactions)

instance Core.AWSRequest GetCommentReactions where
  type
    AWSResponse GetCommentReactions =
      GetCommentReactionsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetCommentReactionsResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x
                            Data..?> "reactionsForComment"
                            Core..!@ Prelude.mempty
                        )
      )

instance Prelude.Hashable GetCommentReactions where
  hashWithSalt _salt GetCommentReactions' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` reactionUserArn
      `Prelude.hashWithSalt` commentId

instance Prelude.NFData GetCommentReactions where
  rnf GetCommentReactions' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf reactionUserArn
      `Prelude.seq` Prelude.rnf commentId

instance Data.ToHeaders GetCommentReactions where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "CodeCommit_20150413.GetCommentReactions" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetCommentReactions where
  toJSON GetCommentReactions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("maxResults" Data..=) Prelude.<$> maxResults,
            ("nextToken" Data..=) Prelude.<$> nextToken,
            ("reactionUserArn" Data..=)
              Prelude.<$> reactionUserArn,
            Prelude.Just ("commentId" Data..= commentId)
          ]
      )

instance Data.ToPath GetCommentReactions where
  toPath = Prelude.const "/"

instance Data.ToQuery GetCommentReactions where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetCommentReactionsResponse' smart constructor.
data GetCommentReactionsResponse = GetCommentReactionsResponse'
  { -- | An enumeration token that can be used in a request to return the next
    -- batch of the results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | An array of reactions to the specified comment.
    reactionsForComment :: [ReactionForComment]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetCommentReactionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getCommentReactionsResponse_nextToken' - An enumeration token that can be used in a request to return the next
-- batch of the results.
--
-- 'httpStatus', 'getCommentReactionsResponse_httpStatus' - The response's http status code.
--
-- 'reactionsForComment', 'getCommentReactionsResponse_reactionsForComment' - An array of reactions to the specified comment.
newGetCommentReactionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetCommentReactionsResponse
newGetCommentReactionsResponse pHttpStatus_ =
  GetCommentReactionsResponse'
    { nextToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      reactionsForComment = Prelude.mempty
    }

-- | An enumeration token that can be used in a request to return the next
-- batch of the results.
getCommentReactionsResponse_nextToken :: Lens.Lens' GetCommentReactionsResponse (Prelude.Maybe Prelude.Text)
getCommentReactionsResponse_nextToken = Lens.lens (\GetCommentReactionsResponse' {nextToken} -> nextToken) (\s@GetCommentReactionsResponse' {} a -> s {nextToken = a} :: GetCommentReactionsResponse)

-- | The response's http status code.
getCommentReactionsResponse_httpStatus :: Lens.Lens' GetCommentReactionsResponse Prelude.Int
getCommentReactionsResponse_httpStatus = Lens.lens (\GetCommentReactionsResponse' {httpStatus} -> httpStatus) (\s@GetCommentReactionsResponse' {} a -> s {httpStatus = a} :: GetCommentReactionsResponse)

-- | An array of reactions to the specified comment.
getCommentReactionsResponse_reactionsForComment :: Lens.Lens' GetCommentReactionsResponse [ReactionForComment]
getCommentReactionsResponse_reactionsForComment = Lens.lens (\GetCommentReactionsResponse' {reactionsForComment} -> reactionsForComment) (\s@GetCommentReactionsResponse' {} a -> s {reactionsForComment = a} :: GetCommentReactionsResponse) Prelude.. Lens.coerced

instance Prelude.NFData GetCommentReactionsResponse where
  rnf GetCommentReactionsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf reactionsForComment
