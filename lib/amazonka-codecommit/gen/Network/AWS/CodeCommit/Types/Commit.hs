{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.Types.Commit
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeCommit.Types.Commit where

import Network.AWS.CodeCommit.Types.UserInfo
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Returns information about a specific commit.
--
--
--
-- /See:/ 'commit' smart constructor.
data Commit = Commit'
  { _cCommitId :: !(Maybe Text),
    _cCommitter :: !(Maybe UserInfo),
    _cTreeId :: !(Maybe Text),
    _cAdditionalData :: !(Maybe Text),
    _cParents :: !(Maybe [Text]),
    _cAuthor :: !(Maybe UserInfo),
    _cMessage :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Commit' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cCommitId' - The full SHA ID of the specified commit.
--
-- * 'cCommitter' - Information about the person who committed the specified commit, also known as the committer. Information includes the date in timestamp format with GMT offset, the name of the committer, and the email address for the committer, as configured in Git. For more information about the difference between an author and a committer in Git, see <http://git-scm.com/book/ch2-3.html Viewing the Commit History> in Pro Git by Scott Chacon and Ben Straub.
--
-- * 'cTreeId' - Tree information for the specified commit.
--
-- * 'cAdditionalData' - Any other data associated with the specified commit.
--
-- * 'cParents' - A list of parent commits for the specified commit. Each parent commit ID is the full commit ID.
--
-- * 'cAuthor' - Information about the author of the specified commit. Information includes the date in timestamp format with GMT offset, the name of the author, and the email address for the author, as configured in Git.
--
-- * 'cMessage' - The commit message associated with the specified commit.
commit ::
  Commit
commit =
  Commit'
    { _cCommitId = Nothing,
      _cCommitter = Nothing,
      _cTreeId = Nothing,
      _cAdditionalData = Nothing,
      _cParents = Nothing,
      _cAuthor = Nothing,
      _cMessage = Nothing
    }

-- | The full SHA ID of the specified commit.
cCommitId :: Lens' Commit (Maybe Text)
cCommitId = lens _cCommitId (\s a -> s {_cCommitId = a})

-- | Information about the person who committed the specified commit, also known as the committer. Information includes the date in timestamp format with GMT offset, the name of the committer, and the email address for the committer, as configured in Git. For more information about the difference between an author and a committer in Git, see <http://git-scm.com/book/ch2-3.html Viewing the Commit History> in Pro Git by Scott Chacon and Ben Straub.
cCommitter :: Lens' Commit (Maybe UserInfo)
cCommitter = lens _cCommitter (\s a -> s {_cCommitter = a})

-- | Tree information for the specified commit.
cTreeId :: Lens' Commit (Maybe Text)
cTreeId = lens _cTreeId (\s a -> s {_cTreeId = a})

-- | Any other data associated with the specified commit.
cAdditionalData :: Lens' Commit (Maybe Text)
cAdditionalData = lens _cAdditionalData (\s a -> s {_cAdditionalData = a})

-- | A list of parent commits for the specified commit. Each parent commit ID is the full commit ID.
cParents :: Lens' Commit [Text]
cParents = lens _cParents (\s a -> s {_cParents = a}) . _Default . _Coerce

-- | Information about the author of the specified commit. Information includes the date in timestamp format with GMT offset, the name of the author, and the email address for the author, as configured in Git.
cAuthor :: Lens' Commit (Maybe UserInfo)
cAuthor = lens _cAuthor (\s a -> s {_cAuthor = a})

-- | The commit message associated with the specified commit.
cMessage :: Lens' Commit (Maybe Text)
cMessage = lens _cMessage (\s a -> s {_cMessage = a})

instance FromJSON Commit where
  parseJSON =
    withObject
      "Commit"
      ( \x ->
          Commit'
            <$> (x .:? "commitId")
            <*> (x .:? "committer")
            <*> (x .:? "treeId")
            <*> (x .:? "additionalData")
            <*> (x .:? "parents" .!= mempty)
            <*> (x .:? "author")
            <*> (x .:? "message")
      )

instance Hashable Commit

instance NFData Commit
