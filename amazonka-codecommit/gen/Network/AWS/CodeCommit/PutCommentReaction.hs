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
-- Module      : Network.AWS.CodeCommit.PutCommentReaction
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds or updates a reaction to a specified comment for the user whose
-- identity is used to make the request. You can only add or update a
-- reaction for yourself. You cannot add, modify, or delete a reaction for
-- another user.
module Network.AWS.CodeCommit.PutCommentReaction
  ( -- * Creating a Request
    PutCommentReaction (..),
    newPutCommentReaction,

    -- * Request Lenses
    putCommentReaction_commentId,
    putCommentReaction_reactionValue,

    -- * Destructuring the Response
    PutCommentReactionResponse (..),
    newPutCommentReactionResponse,
  )
where

import Network.AWS.CodeCommit.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newPutCommentReaction' smart constructor.
data PutCommentReaction = PutCommentReaction'
  { -- | The ID of the comment to which you want to add or update a reaction.
    commentId :: Core.Text,
    -- | The emoji reaction you want to add or update. To remove a reaction,
    -- provide a value of blank or null. You can also provide the value of
    -- none. For information about emoji reaction values supported in AWS
    -- CodeCommit, see the
    -- <https://docs.aws.amazon.com/codecommit/latest/userguide/how-to-commit-comment.html#emoji-reaction-table AWS CodeCommit User Guide>.
    reactionValue :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'PutCommentReaction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'commentId', 'putCommentReaction_commentId' - The ID of the comment to which you want to add or update a reaction.
--
-- 'reactionValue', 'putCommentReaction_reactionValue' - The emoji reaction you want to add or update. To remove a reaction,
-- provide a value of blank or null. You can also provide the value of
-- none. For information about emoji reaction values supported in AWS
-- CodeCommit, see the
-- <https://docs.aws.amazon.com/codecommit/latest/userguide/how-to-commit-comment.html#emoji-reaction-table AWS CodeCommit User Guide>.
newPutCommentReaction ::
  -- | 'commentId'
  Core.Text ->
  -- | 'reactionValue'
  Core.Text ->
  PutCommentReaction
newPutCommentReaction pCommentId_ pReactionValue_ =
  PutCommentReaction'
    { commentId = pCommentId_,
      reactionValue = pReactionValue_
    }

-- | The ID of the comment to which you want to add or update a reaction.
putCommentReaction_commentId :: Lens.Lens' PutCommentReaction Core.Text
putCommentReaction_commentId = Lens.lens (\PutCommentReaction' {commentId} -> commentId) (\s@PutCommentReaction' {} a -> s {commentId = a} :: PutCommentReaction)

-- | The emoji reaction you want to add or update. To remove a reaction,
-- provide a value of blank or null. You can also provide the value of
-- none. For information about emoji reaction values supported in AWS
-- CodeCommit, see the
-- <https://docs.aws.amazon.com/codecommit/latest/userguide/how-to-commit-comment.html#emoji-reaction-table AWS CodeCommit User Guide>.
putCommentReaction_reactionValue :: Lens.Lens' PutCommentReaction Core.Text
putCommentReaction_reactionValue = Lens.lens (\PutCommentReaction' {reactionValue} -> reactionValue) (\s@PutCommentReaction' {} a -> s {reactionValue = a} :: PutCommentReaction)

instance Core.AWSRequest PutCommentReaction where
  type
    AWSResponse PutCommentReaction =
      PutCommentReactionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull PutCommentReactionResponse'

instance Core.Hashable PutCommentReaction

instance Core.NFData PutCommentReaction

instance Core.ToHeaders PutCommentReaction where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "CodeCommit_20150413.PutCommentReaction" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON PutCommentReaction where
  toJSON PutCommentReaction' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("commentId" Core..= commentId),
            Core.Just ("reactionValue" Core..= reactionValue)
          ]
      )

instance Core.ToPath PutCommentReaction where
  toPath = Core.const "/"

instance Core.ToQuery PutCommentReaction where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newPutCommentReactionResponse' smart constructor.
data PutCommentReactionResponse = PutCommentReactionResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'PutCommentReactionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newPutCommentReactionResponse ::
  PutCommentReactionResponse
newPutCommentReactionResponse =
  PutCommentReactionResponse'

instance Core.NFData PutCommentReactionResponse
