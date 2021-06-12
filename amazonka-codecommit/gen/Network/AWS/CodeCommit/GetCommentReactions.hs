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
-- Module      : Network.AWS.CodeCommit.GetCommentReactions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about reactions to a specified comment ID. Reactions
-- from users who have been deleted will not be included in the count.
module Network.AWS.CodeCommit.GetCommentReactions
  ( -- * Creating a Request
    GetCommentReactions (..),
    newGetCommentReactions,

    -- * Request Lenses
    getCommentReactions_nextToken,
    getCommentReactions_maxResults,
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

import Network.AWS.CodeCommit.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetCommentReactions' smart constructor.
data GetCommentReactions = GetCommentReactions'
  { -- | An enumeration token that, when provided in a request, returns the next
    -- batch of the results.
    nextToken :: Core.Maybe Core.Text,
    -- | A non-zero, non-negative integer used to limit the number of returned
    -- results. The default is the same as the allowed maximum, 1,000.
    maxResults :: Core.Maybe Core.Int,
    -- | Optional. The Amazon Resource Name (ARN) of the user or identity for
    -- which you want to get reaction information.
    reactionUserArn :: Core.Maybe Core.Text,
    -- | The ID of the comment for which you want to get reactions information.
    commentId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetCommentReactions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getCommentReactions_nextToken' - An enumeration token that, when provided in a request, returns the next
-- batch of the results.
--
-- 'maxResults', 'getCommentReactions_maxResults' - A non-zero, non-negative integer used to limit the number of returned
-- results. The default is the same as the allowed maximum, 1,000.
--
-- 'reactionUserArn', 'getCommentReactions_reactionUserArn' - Optional. The Amazon Resource Name (ARN) of the user or identity for
-- which you want to get reaction information.
--
-- 'commentId', 'getCommentReactions_commentId' - The ID of the comment for which you want to get reactions information.
newGetCommentReactions ::
  -- | 'commentId'
  Core.Text ->
  GetCommentReactions
newGetCommentReactions pCommentId_ =
  GetCommentReactions'
    { nextToken = Core.Nothing,
      maxResults = Core.Nothing,
      reactionUserArn = Core.Nothing,
      commentId = pCommentId_
    }

-- | An enumeration token that, when provided in a request, returns the next
-- batch of the results.
getCommentReactions_nextToken :: Lens.Lens' GetCommentReactions (Core.Maybe Core.Text)
getCommentReactions_nextToken = Lens.lens (\GetCommentReactions' {nextToken} -> nextToken) (\s@GetCommentReactions' {} a -> s {nextToken = a} :: GetCommentReactions)

-- | A non-zero, non-negative integer used to limit the number of returned
-- results. The default is the same as the allowed maximum, 1,000.
getCommentReactions_maxResults :: Lens.Lens' GetCommentReactions (Core.Maybe Core.Int)
getCommentReactions_maxResults = Lens.lens (\GetCommentReactions' {maxResults} -> maxResults) (\s@GetCommentReactions' {} a -> s {maxResults = a} :: GetCommentReactions)

-- | Optional. The Amazon Resource Name (ARN) of the user or identity for
-- which you want to get reaction information.
getCommentReactions_reactionUserArn :: Lens.Lens' GetCommentReactions (Core.Maybe Core.Text)
getCommentReactions_reactionUserArn = Lens.lens (\GetCommentReactions' {reactionUserArn} -> reactionUserArn) (\s@GetCommentReactions' {} a -> s {reactionUserArn = a} :: GetCommentReactions)

-- | The ID of the comment for which you want to get reactions information.
getCommentReactions_commentId :: Lens.Lens' GetCommentReactions Core.Text
getCommentReactions_commentId = Lens.lens (\GetCommentReactions' {commentId} -> commentId) (\s@GetCommentReactions' {} a -> s {commentId = a} :: GetCommentReactions)

instance Core.AWSRequest GetCommentReactions where
  type
    AWSResponse GetCommentReactions =
      GetCommentReactionsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetCommentReactionsResponse'
            Core.<$> (x Core..?> "nextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
            Core.<*> ( x Core..?> "reactionsForComment"
                         Core..!@ Core.mempty
                     )
      )

instance Core.Hashable GetCommentReactions

instance Core.NFData GetCommentReactions

instance Core.ToHeaders GetCommentReactions where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "CodeCommit_20150413.GetCommentReactions" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GetCommentReactions where
  toJSON GetCommentReactions' {..} =
    Core.object
      ( Core.catMaybes
          [ ("nextToken" Core..=) Core.<$> nextToken,
            ("maxResults" Core..=) Core.<$> maxResults,
            ("reactionUserArn" Core..=) Core.<$> reactionUserArn,
            Core.Just ("commentId" Core..= commentId)
          ]
      )

instance Core.ToPath GetCommentReactions where
  toPath = Core.const "/"

instance Core.ToQuery GetCommentReactions where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetCommentReactionsResponse' smart constructor.
data GetCommentReactionsResponse = GetCommentReactionsResponse'
  { -- | An enumeration token that can be used in a request to return the next
    -- batch of the results.
    nextToken :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int,
    -- | An array of reactions to the specified comment.
    reactionsForComment :: [ReactionForComment]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  GetCommentReactionsResponse
newGetCommentReactionsResponse pHttpStatus_ =
  GetCommentReactionsResponse'
    { nextToken =
        Core.Nothing,
      httpStatus = pHttpStatus_,
      reactionsForComment = Core.mempty
    }

-- | An enumeration token that can be used in a request to return the next
-- batch of the results.
getCommentReactionsResponse_nextToken :: Lens.Lens' GetCommentReactionsResponse (Core.Maybe Core.Text)
getCommentReactionsResponse_nextToken = Lens.lens (\GetCommentReactionsResponse' {nextToken} -> nextToken) (\s@GetCommentReactionsResponse' {} a -> s {nextToken = a} :: GetCommentReactionsResponse)

-- | The response's http status code.
getCommentReactionsResponse_httpStatus :: Lens.Lens' GetCommentReactionsResponse Core.Int
getCommentReactionsResponse_httpStatus = Lens.lens (\GetCommentReactionsResponse' {httpStatus} -> httpStatus) (\s@GetCommentReactionsResponse' {} a -> s {httpStatus = a} :: GetCommentReactionsResponse)

-- | An array of reactions to the specified comment.
getCommentReactionsResponse_reactionsForComment :: Lens.Lens' GetCommentReactionsResponse [ReactionForComment]
getCommentReactionsResponse_reactionsForComment = Lens.lens (\GetCommentReactionsResponse' {reactionsForComment} -> reactionsForComment) (\s@GetCommentReactionsResponse' {} a -> s {reactionsForComment = a} :: GetCommentReactionsResponse) Core.. Lens._Coerce

instance Core.NFData GetCommentReactionsResponse
