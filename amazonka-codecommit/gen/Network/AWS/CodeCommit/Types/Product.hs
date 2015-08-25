{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.Types.Product
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodeCommit.Types.Product where

import           Network.AWS.CodeCommit.Types.Sum
import           Network.AWS.Prelude

-- | Returns information about a branch.
--
-- /See:/ 'branchInfo' smart constructor.
data BranchInfo = BranchInfo'
    { _biCommitId   :: !(Maybe Text)
    , _biBranchName :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'BranchInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'biCommitId'
--
-- * 'biBranchName'
branchInfo
    :: BranchInfo
branchInfo =
    BranchInfo'
    { _biCommitId = Nothing
    , _biBranchName = Nothing
    }

-- | The ID of the last commit made to the branch.
biCommitId :: Lens' BranchInfo (Maybe Text)
biCommitId = lens _biCommitId (\ s a -> s{_biCommitId = a});

-- | The name of the branch.
biBranchName :: Lens' BranchInfo (Maybe Text)
biBranchName = lens _biBranchName (\ s a -> s{_biBranchName = a});

instance FromJSON BranchInfo where
        parseJSON
          = withObject "BranchInfo"
              (\ x ->
                 BranchInfo' <$>
                   (x .:? "commitId") <*> (x .:? "branchName"))

-- | Information about a repository.
--
-- /See:/ 'repositoryMetadata' smart constructor.
data RepositoryMetadata = RepositoryMetadata'
    { _rmRepositoryDescription :: !(Maybe Text)
    , _rmLastModifiedDate      :: !(Maybe POSIX)
    , _rmARN                   :: !(Maybe Text)
    , _rmCloneURLHTTP          :: !(Maybe Text)
    , _rmAccountId             :: !(Maybe Text)
    , _rmDefaultBranch         :: !(Maybe Text)
    , _rmRepositoryId          :: !(Maybe Text)
    , _rmCreationDate          :: !(Maybe POSIX)
    , _rmRepositoryName        :: !(Maybe Text)
    , _rmCloneURLSSH           :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'RepositoryMetadata' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rmRepositoryDescription'
--
-- * 'rmLastModifiedDate'
--
-- * 'rmARN'
--
-- * 'rmCloneURLHTTP'
--
-- * 'rmAccountId'
--
-- * 'rmDefaultBranch'
--
-- * 'rmRepositoryId'
--
-- * 'rmCreationDate'
--
-- * 'rmRepositoryName'
--
-- * 'rmCloneURLSSH'
repositoryMetadata
    :: RepositoryMetadata
repositoryMetadata =
    RepositoryMetadata'
    { _rmRepositoryDescription = Nothing
    , _rmLastModifiedDate = Nothing
    , _rmARN = Nothing
    , _rmCloneURLHTTP = Nothing
    , _rmAccountId = Nothing
    , _rmDefaultBranch = Nothing
    , _rmRepositoryId = Nothing
    , _rmCreationDate = Nothing
    , _rmRepositoryName = Nothing
    , _rmCloneURLSSH = Nothing
    }

-- | A comment or description about the repository.
rmRepositoryDescription :: Lens' RepositoryMetadata (Maybe Text)
rmRepositoryDescription = lens _rmRepositoryDescription (\ s a -> s{_rmRepositoryDescription = a});

-- | The date and time the repository was last modified, in timestamp format.
rmLastModifiedDate :: Lens' RepositoryMetadata (Maybe UTCTime)
rmLastModifiedDate = lens _rmLastModifiedDate (\ s a -> s{_rmLastModifiedDate = a}) . mapping _Time;

-- | The Amazon Resource Name (ARN) of the repository.
rmARN :: Lens' RepositoryMetadata (Maybe Text)
rmARN = lens _rmARN (\ s a -> s{_rmARN = a});

-- | The URL to use for cloning the repository over HTTPS.
rmCloneURLHTTP :: Lens' RepositoryMetadata (Maybe Text)
rmCloneURLHTTP = lens _rmCloneURLHTTP (\ s a -> s{_rmCloneURLHTTP = a});

-- | The ID of the AWS account associated with the repository.
rmAccountId :: Lens' RepositoryMetadata (Maybe Text)
rmAccountId = lens _rmAccountId (\ s a -> s{_rmAccountId = a});

-- | The repository\'s default branch name.
rmDefaultBranch :: Lens' RepositoryMetadata (Maybe Text)
rmDefaultBranch = lens _rmDefaultBranch (\ s a -> s{_rmDefaultBranch = a});

-- | The ID of the repository.
rmRepositoryId :: Lens' RepositoryMetadata (Maybe Text)
rmRepositoryId = lens _rmRepositoryId (\ s a -> s{_rmRepositoryId = a});

-- | The date and time the repository was created, in timestamp format.
rmCreationDate :: Lens' RepositoryMetadata (Maybe UTCTime)
rmCreationDate = lens _rmCreationDate (\ s a -> s{_rmCreationDate = a}) . mapping _Time;

-- | The repository\'s name.
rmRepositoryName :: Lens' RepositoryMetadata (Maybe Text)
rmRepositoryName = lens _rmRepositoryName (\ s a -> s{_rmRepositoryName = a});

-- | The URL to use for cloning the repository over SSH.
rmCloneURLSSH :: Lens' RepositoryMetadata (Maybe Text)
rmCloneURLSSH = lens _rmCloneURLSSH (\ s a -> s{_rmCloneURLSSH = a});

instance FromJSON RepositoryMetadata where
        parseJSON
          = withObject "RepositoryMetadata"
              (\ x ->
                 RepositoryMetadata' <$>
                   (x .:? "repositoryDescription") <*>
                     (x .:? "lastModifiedDate")
                     <*> (x .:? "Arn")
                     <*> (x .:? "cloneUrlHttp")
                     <*> (x .:? "accountId")
                     <*> (x .:? "defaultBranch")
                     <*> (x .:? "repositoryId")
                     <*> (x .:? "creationDate")
                     <*> (x .:? "repositoryName")
                     <*> (x .:? "cloneUrlSsh"))

-- | Information about a repository name and ID.
--
-- /See:/ 'repositoryNameIdPair' smart constructor.
data RepositoryNameIdPair = RepositoryNameIdPair'
    { _rnipRepositoryId   :: !(Maybe Text)
    , _rnipRepositoryName :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'RepositoryNameIdPair' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rnipRepositoryId'
--
-- * 'rnipRepositoryName'
repositoryNameIdPair
    :: RepositoryNameIdPair
repositoryNameIdPair =
    RepositoryNameIdPair'
    { _rnipRepositoryId = Nothing
    , _rnipRepositoryName = Nothing
    }

-- | The ID associated with the repository name.
rnipRepositoryId :: Lens' RepositoryNameIdPair (Maybe Text)
rnipRepositoryId = lens _rnipRepositoryId (\ s a -> s{_rnipRepositoryId = a});

-- | Undocumented member.
rnipRepositoryName :: Lens' RepositoryNameIdPair (Maybe Text)
rnipRepositoryName = lens _rnipRepositoryName (\ s a -> s{_rnipRepositoryName = a});

instance FromJSON RepositoryNameIdPair where
        parseJSON
          = withObject "RepositoryNameIdPair"
              (\ x ->
                 RepositoryNameIdPair' <$>
                   (x .:? "repositoryId") <*> (x .:? "repositoryName"))
