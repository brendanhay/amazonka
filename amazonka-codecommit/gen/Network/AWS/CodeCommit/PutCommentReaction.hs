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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newPutCommentReaction' smart constructor.
data PutCommentReaction = PutCommentReaction'
  { -- | The ID of the comment to which you want to add or update a reaction.
    commentId :: Prelude.Text,
    -- | The emoji reaction you want to add or update. To remove a reaction,
    -- provide a value of blank or null. You can also provide the value of
    -- none. For information about emoji reaction values supported in AWS
    -- CodeCommit, see the
    -- <https://docs.aws.amazon.com/codecommit/latest/userguide/how-to-commit-comment.html#emoji-reaction-table AWS CodeCommit User Guide>.
    reactionValue :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'reactionValue'
  Prelude.Text ->
  PutCommentReaction
newPutCommentReaction pCommentId_ pReactionValue_ =
  PutCommentReaction'
    { commentId = pCommentId_,
      reactionValue = pReactionValue_
    }

-- | The ID of the comment to which you want to add or update a reaction.
putCommentReaction_commentId :: Lens.Lens' PutCommentReaction Prelude.Text
putCommentReaction_commentId = Lens.lens (\PutCommentReaction' {commentId} -> commentId) (\s@PutCommentReaction' {} a -> s {commentId = a} :: PutCommentReaction)

-- | The emoji reaction you want to add or update. To remove a reaction,
-- provide a value of blank or null. You can also provide the value of
-- none. For information about emoji reaction values supported in AWS
-- CodeCommit, see the
-- <https://docs.aws.amazon.com/codecommit/latest/userguide/how-to-commit-comment.html#emoji-reaction-table AWS CodeCommit User Guide>.
putCommentReaction_reactionValue :: Lens.Lens' PutCommentReaction Prelude.Text
putCommentReaction_reactionValue = Lens.lens (\PutCommentReaction' {reactionValue} -> reactionValue) (\s@PutCommentReaction' {} a -> s {reactionValue = a} :: PutCommentReaction)

instance Prelude.AWSRequest PutCommentReaction where
  type
    Rs PutCommentReaction =
      PutCommentReactionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull PutCommentReactionResponse'

instance Prelude.Hashable PutCommentReaction

instance Prelude.NFData PutCommentReaction

instance Prelude.ToHeaders PutCommentReaction where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "CodeCommit_20150413.PutCommentReaction" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON PutCommentReaction where
  toJSON PutCommentReaction' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("commentId" Prelude..= commentId),
            Prelude.Just
              ("reactionValue" Prelude..= reactionValue)
          ]
      )

instance Prelude.ToPath PutCommentReaction where
  toPath = Prelude.const "/"

instance Prelude.ToQuery PutCommentReaction where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutCommentReactionResponse' smart constructor.
data PutCommentReactionResponse = PutCommentReactionResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'PutCommentReactionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newPutCommentReactionResponse ::
  PutCommentReactionResponse
newPutCommentReactionResponse =
  PutCommentReactionResponse'

instance Prelude.NFData PutCommentReactionResponse
