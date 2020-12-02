{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.Types.RepositoryMetadata
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeCommit.Types.RepositoryMetadata where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about a repository.
--
--
--
-- /See:/ 'repositoryMetadata' smart constructor.
data RepositoryMetadata = RepositoryMetadata'
  { _rmRepositoryDescription ::
      !(Maybe Text),
    _rmLastModifiedDate :: !(Maybe POSIX),
    _rmARN :: !(Maybe Text),
    _rmCloneURLHTTP :: !(Maybe Text),
    _rmAccountId :: !(Maybe Text),
    _rmDefaultBranch :: !(Maybe Text),
    _rmRepositoryId :: !(Maybe Text),
    _rmRepositoryName :: !(Maybe Text),
    _rmCreationDate :: !(Maybe POSIX),
    _rmCloneURLSSH :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RepositoryMetadata' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rmRepositoryDescription' - A comment or description about the repository.
--
-- * 'rmLastModifiedDate' - The date and time the repository was last modified, in timestamp format.
--
-- * 'rmARN' - The Amazon Resource Name (ARN) of the repository.
--
-- * 'rmCloneURLHTTP' - The URL to use for cloning the repository over HTTPS.
--
-- * 'rmAccountId' - The ID of the AWS account associated with the repository.
--
-- * 'rmDefaultBranch' - The repository's default branch name.
--
-- * 'rmRepositoryId' - The ID of the repository.
--
-- * 'rmRepositoryName' - The repository's name.
--
-- * 'rmCreationDate' - The date and time the repository was created, in timestamp format.
--
-- * 'rmCloneURLSSH' - The URL to use for cloning the repository over SSH.
repositoryMetadata ::
  RepositoryMetadata
repositoryMetadata =
  RepositoryMetadata'
    { _rmRepositoryDescription = Nothing,
      _rmLastModifiedDate = Nothing,
      _rmARN = Nothing,
      _rmCloneURLHTTP = Nothing,
      _rmAccountId = Nothing,
      _rmDefaultBranch = Nothing,
      _rmRepositoryId = Nothing,
      _rmRepositoryName = Nothing,
      _rmCreationDate = Nothing,
      _rmCloneURLSSH = Nothing
    }

-- | A comment or description about the repository.
rmRepositoryDescription :: Lens' RepositoryMetadata (Maybe Text)
rmRepositoryDescription = lens _rmRepositoryDescription (\s a -> s {_rmRepositoryDescription = a})

-- | The date and time the repository was last modified, in timestamp format.
rmLastModifiedDate :: Lens' RepositoryMetadata (Maybe UTCTime)
rmLastModifiedDate = lens _rmLastModifiedDate (\s a -> s {_rmLastModifiedDate = a}) . mapping _Time

-- | The Amazon Resource Name (ARN) of the repository.
rmARN :: Lens' RepositoryMetadata (Maybe Text)
rmARN = lens _rmARN (\s a -> s {_rmARN = a})

-- | The URL to use for cloning the repository over HTTPS.
rmCloneURLHTTP :: Lens' RepositoryMetadata (Maybe Text)
rmCloneURLHTTP = lens _rmCloneURLHTTP (\s a -> s {_rmCloneURLHTTP = a})

-- | The ID of the AWS account associated with the repository.
rmAccountId :: Lens' RepositoryMetadata (Maybe Text)
rmAccountId = lens _rmAccountId (\s a -> s {_rmAccountId = a})

-- | The repository's default branch name.
rmDefaultBranch :: Lens' RepositoryMetadata (Maybe Text)
rmDefaultBranch = lens _rmDefaultBranch (\s a -> s {_rmDefaultBranch = a})

-- | The ID of the repository.
rmRepositoryId :: Lens' RepositoryMetadata (Maybe Text)
rmRepositoryId = lens _rmRepositoryId (\s a -> s {_rmRepositoryId = a})

-- | The repository's name.
rmRepositoryName :: Lens' RepositoryMetadata (Maybe Text)
rmRepositoryName = lens _rmRepositoryName (\s a -> s {_rmRepositoryName = a})

-- | The date and time the repository was created, in timestamp format.
rmCreationDate :: Lens' RepositoryMetadata (Maybe UTCTime)
rmCreationDate = lens _rmCreationDate (\s a -> s {_rmCreationDate = a}) . mapping _Time

-- | The URL to use for cloning the repository over SSH.
rmCloneURLSSH :: Lens' RepositoryMetadata (Maybe Text)
rmCloneURLSSH = lens _rmCloneURLSSH (\s a -> s {_rmCloneURLSSH = a})

instance FromJSON RepositoryMetadata where
  parseJSON =
    withObject
      "RepositoryMetadata"
      ( \x ->
          RepositoryMetadata'
            <$> (x .:? "repositoryDescription")
            <*> (x .:? "lastModifiedDate")
            <*> (x .:? "Arn")
            <*> (x .:? "cloneUrlHttp")
            <*> (x .:? "accountId")
            <*> (x .:? "defaultBranch")
            <*> (x .:? "repositoryId")
            <*> (x .:? "repositoryName")
            <*> (x .:? "creationDate")
            <*> (x .:? "cloneUrlSsh")
      )

instance Hashable RepositoryMetadata

instance NFData RepositoryMetadata
