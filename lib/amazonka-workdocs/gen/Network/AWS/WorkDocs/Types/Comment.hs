{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkDocs.Types.Comment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkDocs.Types.Comment where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.WorkDocs.Types.CommentStatusType
import Network.AWS.WorkDocs.Types.CommentVisibilityType
import Network.AWS.WorkDocs.Types.User

-- | Describes a comment.
--
--
--
-- /See:/ 'comment' smart constructor.
data Comment = Comment'
  { _cStatus :: !(Maybe CommentStatusType),
    _cText :: !(Maybe (Sensitive Text)),
    _cVisibility :: !(Maybe CommentVisibilityType),
    _cThreadId :: !(Maybe Text),
    _cContributor :: !(Maybe User),
    _cCreatedTimestamp :: !(Maybe POSIX),
    _cRecipientId :: !(Maybe Text),
    _cParentId :: !(Maybe Text),
    _cCommentId :: !Text
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'Comment' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cStatus' - The status of the comment.
--
-- * 'cText' - The text of the comment.
--
-- * 'cVisibility' - The visibility of the comment. Options are either PRIVATE, where the comment is visible only to the comment author and document owner and co-owners, or PUBLIC, where the comment is visible to document owners, co-owners, and contributors.
--
-- * 'cThreadId' - The ID of the root comment in the thread.
--
-- * 'cContributor' - The details of the user who made the comment.
--
-- * 'cCreatedTimestamp' - The time that the comment was created.
--
-- * 'cRecipientId' - If the comment is a reply to another user's comment, this field contains the user ID of the user being replied to.
--
-- * 'cParentId' - The ID of the parent comment.
--
-- * 'cCommentId' - The ID of the comment.
comment ::
  -- | 'cCommentId'
  Text ->
  Comment
comment pCommentId_ =
  Comment'
    { _cStatus = Nothing,
      _cText = Nothing,
      _cVisibility = Nothing,
      _cThreadId = Nothing,
      _cContributor = Nothing,
      _cCreatedTimestamp = Nothing,
      _cRecipientId = Nothing,
      _cParentId = Nothing,
      _cCommentId = pCommentId_
    }

-- | The status of the comment.
cStatus :: Lens' Comment (Maybe CommentStatusType)
cStatus = lens _cStatus (\s a -> s {_cStatus = a})

-- | The text of the comment.
cText :: Lens' Comment (Maybe Text)
cText = lens _cText (\s a -> s {_cText = a}) . mapping _Sensitive

-- | The visibility of the comment. Options are either PRIVATE, where the comment is visible only to the comment author and document owner and co-owners, or PUBLIC, where the comment is visible to document owners, co-owners, and contributors.
cVisibility :: Lens' Comment (Maybe CommentVisibilityType)
cVisibility = lens _cVisibility (\s a -> s {_cVisibility = a})

-- | The ID of the root comment in the thread.
cThreadId :: Lens' Comment (Maybe Text)
cThreadId = lens _cThreadId (\s a -> s {_cThreadId = a})

-- | The details of the user who made the comment.
cContributor :: Lens' Comment (Maybe User)
cContributor = lens _cContributor (\s a -> s {_cContributor = a})

-- | The time that the comment was created.
cCreatedTimestamp :: Lens' Comment (Maybe UTCTime)
cCreatedTimestamp = lens _cCreatedTimestamp (\s a -> s {_cCreatedTimestamp = a}) . mapping _Time

-- | If the comment is a reply to another user's comment, this field contains the user ID of the user being replied to.
cRecipientId :: Lens' Comment (Maybe Text)
cRecipientId = lens _cRecipientId (\s a -> s {_cRecipientId = a})

-- | The ID of the parent comment.
cParentId :: Lens' Comment (Maybe Text)
cParentId = lens _cParentId (\s a -> s {_cParentId = a})

-- | The ID of the comment.
cCommentId :: Lens' Comment Text
cCommentId = lens _cCommentId (\s a -> s {_cCommentId = a})

instance FromJSON Comment where
  parseJSON =
    withObject
      "Comment"
      ( \x ->
          Comment'
            <$> (x .:? "Status")
            <*> (x .:? "Text")
            <*> (x .:? "Visibility")
            <*> (x .:? "ThreadId")
            <*> (x .:? "Contributor")
            <*> (x .:? "CreatedTimestamp")
            <*> (x .:? "RecipientId")
            <*> (x .:? "ParentId")
            <*> (x .: "CommentId")
      )

instance Hashable Comment

instance NFData Comment
