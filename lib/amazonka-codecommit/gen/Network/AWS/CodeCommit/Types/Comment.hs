{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.Types.Comment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeCommit.Types.Comment where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Returns information about a specific comment.
--
--
--
-- /See:/ 'comment' smart constructor.
data Comment = Comment'
  { _cLastModifiedDate :: !(Maybe POSIX),
    _cAuthorARN :: !(Maybe Text),
    _cContent :: !(Maybe Text),
    _cCallerReactions :: !(Maybe [Text]),
    _cCreationDate :: !(Maybe POSIX),
    _cDeleted :: !(Maybe Bool),
    _cClientRequestToken :: !(Maybe Text),
    _cCommentId :: !(Maybe Text),
    _cInReplyTo :: !(Maybe Text),
    _cReactionCounts :: !(Maybe (Map Text (Int)))
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Comment' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cLastModifiedDate' - The date and time the comment was most recently modified, in timestamp format.
--
-- * 'cAuthorARN' - The Amazon Resource Name (ARN) of the person who posted the comment.
--
-- * 'cContent' - The content of the comment.
--
-- * 'cCallerReactions' - The emoji reactions to a comment, if any, submitted by the user whose credentials are associated with the call to the API.
--
-- * 'cCreationDate' - The date and time the comment was created, in timestamp format.
--
-- * 'cDeleted' - A Boolean value indicating whether the comment has been deleted.
--
-- * 'cClientRequestToken' - A unique, client-generated idempotency token that, when provided in a request, ensures the request cannot be repeated with a changed parameter. If a request is received with the same parameters and a token is included, the request returns information about the initial request that used that token.
--
-- * 'cCommentId' - The system-generated comment ID.
--
-- * 'cInReplyTo' - The ID of the comment for which this comment is a reply, if any.
--
-- * 'cReactionCounts' - A string to integer map that represents the number of individual users who have responded to a comment with the specified reactions.
comment ::
  Comment
comment =
  Comment'
    { _cLastModifiedDate = Nothing,
      _cAuthorARN = Nothing,
      _cContent = Nothing,
      _cCallerReactions = Nothing,
      _cCreationDate = Nothing,
      _cDeleted = Nothing,
      _cClientRequestToken = Nothing,
      _cCommentId = Nothing,
      _cInReplyTo = Nothing,
      _cReactionCounts = Nothing
    }

-- | The date and time the comment was most recently modified, in timestamp format.
cLastModifiedDate :: Lens' Comment (Maybe UTCTime)
cLastModifiedDate = lens _cLastModifiedDate (\s a -> s {_cLastModifiedDate = a}) . mapping _Time

-- | The Amazon Resource Name (ARN) of the person who posted the comment.
cAuthorARN :: Lens' Comment (Maybe Text)
cAuthorARN = lens _cAuthorARN (\s a -> s {_cAuthorARN = a})

-- | The content of the comment.
cContent :: Lens' Comment (Maybe Text)
cContent = lens _cContent (\s a -> s {_cContent = a})

-- | The emoji reactions to a comment, if any, submitted by the user whose credentials are associated with the call to the API.
cCallerReactions :: Lens' Comment [Text]
cCallerReactions = lens _cCallerReactions (\s a -> s {_cCallerReactions = a}) . _Default . _Coerce

-- | The date and time the comment was created, in timestamp format.
cCreationDate :: Lens' Comment (Maybe UTCTime)
cCreationDate = lens _cCreationDate (\s a -> s {_cCreationDate = a}) . mapping _Time

-- | A Boolean value indicating whether the comment has been deleted.
cDeleted :: Lens' Comment (Maybe Bool)
cDeleted = lens _cDeleted (\s a -> s {_cDeleted = a})

-- | A unique, client-generated idempotency token that, when provided in a request, ensures the request cannot be repeated with a changed parameter. If a request is received with the same parameters and a token is included, the request returns information about the initial request that used that token.
cClientRequestToken :: Lens' Comment (Maybe Text)
cClientRequestToken = lens _cClientRequestToken (\s a -> s {_cClientRequestToken = a})

-- | The system-generated comment ID.
cCommentId :: Lens' Comment (Maybe Text)
cCommentId = lens _cCommentId (\s a -> s {_cCommentId = a})

-- | The ID of the comment for which this comment is a reply, if any.
cInReplyTo :: Lens' Comment (Maybe Text)
cInReplyTo = lens _cInReplyTo (\s a -> s {_cInReplyTo = a})

-- | A string to integer map that represents the number of individual users who have responded to a comment with the specified reactions.
cReactionCounts :: Lens' Comment (HashMap Text (Int))
cReactionCounts = lens _cReactionCounts (\s a -> s {_cReactionCounts = a}) . _Default . _Map

instance FromJSON Comment where
  parseJSON =
    withObject
      "Comment"
      ( \x ->
          Comment'
            <$> (x .:? "lastModifiedDate")
            <*> (x .:? "authorArn")
            <*> (x .:? "content")
            <*> (x .:? "callerReactions" .!= mempty)
            <*> (x .:? "creationDate")
            <*> (x .:? "deleted")
            <*> (x .:? "clientRequestToken")
            <*> (x .:? "commentId")
            <*> (x .:? "inReplyTo")
            <*> (x .:? "reactionCounts" .!= mempty)
      )

instance Hashable Comment

instance NFData Comment
