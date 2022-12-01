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
-- Module      : Amazonka.CodeCommit.PutCommentReaction
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds or updates a reaction to a specified comment for the user whose
-- identity is used to make the request. You can only add or update a
-- reaction for yourself. You cannot add, modify, or delete a reaction for
-- another user.
module Amazonka.CodeCommit.PutCommentReaction
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

import Amazonka.CodeCommit.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Core.AWSRequest PutCommentReaction where
  type
    AWSResponse PutCommentReaction =
      PutCommentReactionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull PutCommentReactionResponse'

instance Prelude.Hashable PutCommentReaction where
  hashWithSalt _salt PutCommentReaction' {..} =
    _salt `Prelude.hashWithSalt` commentId
      `Prelude.hashWithSalt` reactionValue

instance Prelude.NFData PutCommentReaction where
  rnf PutCommentReaction' {..} =
    Prelude.rnf commentId
      `Prelude.seq` Prelude.rnf reactionValue

instance Core.ToHeaders PutCommentReaction where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "CodeCommit_20150413.PutCommentReaction" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON PutCommentReaction where
  toJSON PutCommentReaction' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("commentId" Core..= commentId),
            Prelude.Just
              ("reactionValue" Core..= reactionValue)
          ]
      )

instance Core.ToPath PutCommentReaction where
  toPath = Prelude.const "/"

instance Core.ToQuery PutCommentReaction where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutCommentReactionResponse' smart constructor.
data PutCommentReactionResponse = PutCommentReactionResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutCommentReactionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newPutCommentReactionResponse ::
  PutCommentReactionResponse
newPutCommentReactionResponse =
  PutCommentReactionResponse'

instance Prelude.NFData PutCommentReactionResponse where
  rnf _ = ()
