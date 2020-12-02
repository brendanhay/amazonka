{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.PutCommentReaction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds or updates a reaction to a specified comment for the user whose identity is used to make the request. You can only add or update a reaction for yourself. You cannot add, modify, or delete a reaction for another user.
module Network.AWS.CodeCommit.PutCommentReaction
  ( -- * Creating a Request
    putCommentReaction,
    PutCommentReaction,

    -- * Request Lenses
    pcrCommentId,
    pcrReactionValue,

    -- * Destructuring the Response
    putCommentReactionResponse,
    PutCommentReactionResponse,
  )
where

import Network.AWS.CodeCommit.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'putCommentReaction' smart constructor.
data PutCommentReaction = PutCommentReaction'
  { _pcrCommentId ::
      !Text,
    _pcrReactionValue :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PutCommentReaction' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pcrCommentId' - The ID of the comment to which you want to add or update a reaction.
--
-- * 'pcrReactionValue' - The emoji reaction you want to add or update. To remove a reaction, provide a value of blank or null. You can also provide the value of none. For information about emoji reaction values supported in AWS CodeCommit, see the <https://docs.aws.amazon.com/codecommit/latest/userguide/how-to-commit-comment.html#emoji-reaction-table AWS CodeCommit User Guide> .
putCommentReaction ::
  -- | 'pcrCommentId'
  Text ->
  -- | 'pcrReactionValue'
  Text ->
  PutCommentReaction
putCommentReaction pCommentId_ pReactionValue_ =
  PutCommentReaction'
    { _pcrCommentId = pCommentId_,
      _pcrReactionValue = pReactionValue_
    }

-- | The ID of the comment to which you want to add or update a reaction.
pcrCommentId :: Lens' PutCommentReaction Text
pcrCommentId = lens _pcrCommentId (\s a -> s {_pcrCommentId = a})

-- | The emoji reaction you want to add or update. To remove a reaction, provide a value of blank or null. You can also provide the value of none. For information about emoji reaction values supported in AWS CodeCommit, see the <https://docs.aws.amazon.com/codecommit/latest/userguide/how-to-commit-comment.html#emoji-reaction-table AWS CodeCommit User Guide> .
pcrReactionValue :: Lens' PutCommentReaction Text
pcrReactionValue = lens _pcrReactionValue (\s a -> s {_pcrReactionValue = a})

instance AWSRequest PutCommentReaction where
  type Rs PutCommentReaction = PutCommentReactionResponse
  request = postJSON codeCommit
  response = receiveNull PutCommentReactionResponse'

instance Hashable PutCommentReaction

instance NFData PutCommentReaction

instance ToHeaders PutCommentReaction where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("CodeCommit_20150413.PutCommentReaction" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON PutCommentReaction where
  toJSON PutCommentReaction' {..} =
    object
      ( catMaybes
          [ Just ("commentId" .= _pcrCommentId),
            Just ("reactionValue" .= _pcrReactionValue)
          ]
      )

instance ToPath PutCommentReaction where
  toPath = const "/"

instance ToQuery PutCommentReaction where
  toQuery = const mempty

-- | /See:/ 'putCommentReactionResponse' smart constructor.
data PutCommentReactionResponse = PutCommentReactionResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PutCommentReactionResponse' with the minimum fields required to make a request.
putCommentReactionResponse ::
  PutCommentReactionResponse
putCommentReactionResponse = PutCommentReactionResponse'

instance NFData PutCommentReactionResponse
