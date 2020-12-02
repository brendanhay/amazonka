{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.Types.CommentsForComparedCommit
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeCommit.Types.CommentsForComparedCommit where

import Network.AWS.CodeCommit.Types.Comment
import Network.AWS.CodeCommit.Types.Location
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Returns information about comments on the comparison between two commits.
--
--
--
-- /See:/ 'commentsForComparedCommit' smart constructor.
data CommentsForComparedCommit = CommentsForComparedCommit'
  { _cfccBeforeBlobId ::
      !(Maybe Text),
    _cfccLocation :: !(Maybe Location),
    _cfccAfterCommitId :: !(Maybe Text),
    _cfccAfterBlobId :: !(Maybe Text),
    _cfccBeforeCommitId :: !(Maybe Text),
    _cfccRepositoryName :: !(Maybe Text),
    _cfccComments :: !(Maybe [Comment])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CommentsForComparedCommit' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cfccBeforeBlobId' - The full blob ID of the commit used to establish the before of the comparison.
--
-- * 'cfccLocation' - Location information about the comment on the comparison, including the file name, line number, and whether the version of the file where the comment was made is BEFORE or AFTER.
--
-- * 'cfccAfterCommitId' - The full commit ID of the commit used to establish the after of the comparison.
--
-- * 'cfccAfterBlobId' - The full blob ID of the commit used to establish the after of the comparison.
--
-- * 'cfccBeforeCommitId' - The full commit ID of the commit used to establish the before of the comparison.
--
-- * 'cfccRepositoryName' - The name of the repository that contains the compared commits.
--
-- * 'cfccComments' - An array of comment objects. Each comment object contains information about a comment on the comparison between commits.
commentsForComparedCommit ::
  CommentsForComparedCommit
commentsForComparedCommit =
  CommentsForComparedCommit'
    { _cfccBeforeBlobId = Nothing,
      _cfccLocation = Nothing,
      _cfccAfterCommitId = Nothing,
      _cfccAfterBlobId = Nothing,
      _cfccBeforeCommitId = Nothing,
      _cfccRepositoryName = Nothing,
      _cfccComments = Nothing
    }

-- | The full blob ID of the commit used to establish the before of the comparison.
cfccBeforeBlobId :: Lens' CommentsForComparedCommit (Maybe Text)
cfccBeforeBlobId = lens _cfccBeforeBlobId (\s a -> s {_cfccBeforeBlobId = a})

-- | Location information about the comment on the comparison, including the file name, line number, and whether the version of the file where the comment was made is BEFORE or AFTER.
cfccLocation :: Lens' CommentsForComparedCommit (Maybe Location)
cfccLocation = lens _cfccLocation (\s a -> s {_cfccLocation = a})

-- | The full commit ID of the commit used to establish the after of the comparison.
cfccAfterCommitId :: Lens' CommentsForComparedCommit (Maybe Text)
cfccAfterCommitId = lens _cfccAfterCommitId (\s a -> s {_cfccAfterCommitId = a})

-- | The full blob ID of the commit used to establish the after of the comparison.
cfccAfterBlobId :: Lens' CommentsForComparedCommit (Maybe Text)
cfccAfterBlobId = lens _cfccAfterBlobId (\s a -> s {_cfccAfterBlobId = a})

-- | The full commit ID of the commit used to establish the before of the comparison.
cfccBeforeCommitId :: Lens' CommentsForComparedCommit (Maybe Text)
cfccBeforeCommitId = lens _cfccBeforeCommitId (\s a -> s {_cfccBeforeCommitId = a})

-- | The name of the repository that contains the compared commits.
cfccRepositoryName :: Lens' CommentsForComparedCommit (Maybe Text)
cfccRepositoryName = lens _cfccRepositoryName (\s a -> s {_cfccRepositoryName = a})

-- | An array of comment objects. Each comment object contains information about a comment on the comparison between commits.
cfccComments :: Lens' CommentsForComparedCommit [Comment]
cfccComments = lens _cfccComments (\s a -> s {_cfccComments = a}) . _Default . _Coerce

instance FromJSON CommentsForComparedCommit where
  parseJSON =
    withObject
      "CommentsForComparedCommit"
      ( \x ->
          CommentsForComparedCommit'
            <$> (x .:? "beforeBlobId")
            <*> (x .:? "location")
            <*> (x .:? "afterCommitId")
            <*> (x .:? "afterBlobId")
            <*> (x .:? "beforeCommitId")
            <*> (x .:? "repositoryName")
            <*> (x .:? "comments" .!= mempty)
      )

instance Hashable CommentsForComparedCommit

instance NFData CommentsForComparedCommit
