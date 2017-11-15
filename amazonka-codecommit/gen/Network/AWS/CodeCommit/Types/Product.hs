{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.Types.Product
-- Copyright   : (c) 2013-2017 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodeCommit.Types.Product where

import Network.AWS.CodeCommit.Types.Sum
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Returns information about a specific Git blob object.
--
--
--
-- /See:/ 'blobMetadata' smart constructor.
data BlobMetadata = BlobMetadata'
  { _bmPath   :: !(Maybe Text)
  , _bmMode   :: !(Maybe Text)
  , _bmBlobId :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'BlobMetadata' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bmPath' - The path to the blob and any associated file name, if any.
--
-- * 'bmMode' - The file mode permissions of the blob. File mode permission codes include:     * @100644@ indicates read/write     * @100755@ indicates read/write/execute     * @160000@ indicates a submodule     * @120000@ indicates a symlink
--
-- * 'bmBlobId' - The full ID of the blob.
blobMetadata
    :: BlobMetadata
blobMetadata =
  BlobMetadata' {_bmPath = Nothing, _bmMode = Nothing, _bmBlobId = Nothing}


-- | The path to the blob and any associated file name, if any.
bmPath :: Lens' BlobMetadata (Maybe Text)
bmPath = lens _bmPath (\ s a -> s{_bmPath = a});

-- | The file mode permissions of the blob. File mode permission codes include:     * @100644@ indicates read/write     * @100755@ indicates read/write/execute     * @160000@ indicates a submodule     * @120000@ indicates a symlink
bmMode :: Lens' BlobMetadata (Maybe Text)
bmMode = lens _bmMode (\ s a -> s{_bmMode = a});

-- | The full ID of the blob.
bmBlobId :: Lens' BlobMetadata (Maybe Text)
bmBlobId = lens _bmBlobId (\ s a -> s{_bmBlobId = a});

instance FromJSON BlobMetadata where
        parseJSON
          = withObject "BlobMetadata"
              (\ x ->
                 BlobMetadata' <$>
                   (x .:? "path") <*> (x .:? "mode") <*>
                     (x .:? "blobId"))

instance Hashable BlobMetadata where

instance NFData BlobMetadata where

-- | Returns information about a branch.
--
--
--
-- /See:/ 'branchInfo' smart constructor.
data BranchInfo = BranchInfo'
  { _biCommitId   :: !(Maybe Text)
  , _biBranchName :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'BranchInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'biCommitId' - The ID of the last commit made to the branch.
--
-- * 'biBranchName' - The name of the branch.
branchInfo
    :: BranchInfo
branchInfo = BranchInfo' {_biCommitId = Nothing, _biBranchName = Nothing}


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

instance Hashable BranchInfo where

instance NFData BranchInfo where

-- | Returns information about a specific commit.
--
--
--
-- /See:/ 'commit' smart constructor.
data Commit = Commit'
  { _cCommitId       :: !(Maybe Text)
  , _cCommitter      :: !(Maybe UserInfo)
  , _cTreeId         :: !(Maybe Text)
  , _cAdditionalData :: !(Maybe Text)
  , _cParents        :: !(Maybe [Text])
  , _cAuthor         :: !(Maybe UserInfo)
  , _cMessage        :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Commit' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cCommitId' - The full SHA of the specified commit.
--
-- * 'cCommitter' - Information about the person who committed the specified commit, also known as the committer. Information includes the date in timestamp format with GMT offset, the name of the committer, and the email address for the committer, as configured in Git. For more information about the difference between an author and a committer in Git, see <http://git-scm.com/book/ch2-3.html Viewing the Commit History> in Pro Git by Scott Chacon and Ben Straub.
--
-- * 'cTreeId' - Tree information for the specified commit.
--
-- * 'cAdditionalData' - Any additional data associated with the specified commit.
--
-- * 'cParents' - The parent list for the specified commit.
--
-- * 'cAuthor' - Information about the author of the specified commit. Information includes the date in timestamp format with GMT offset, the name of the author, and the email address for the author, as configured in Git.
--
-- * 'cMessage' - The commit message associated with the specified commit.
commit
    :: Commit
commit =
  Commit'
  { _cCommitId = Nothing
  , _cCommitter = Nothing
  , _cTreeId = Nothing
  , _cAdditionalData = Nothing
  , _cParents = Nothing
  , _cAuthor = Nothing
  , _cMessage = Nothing
  }


-- | The full SHA of the specified commit.
cCommitId :: Lens' Commit (Maybe Text)
cCommitId = lens _cCommitId (\ s a -> s{_cCommitId = a});

-- | Information about the person who committed the specified commit, also known as the committer. Information includes the date in timestamp format with GMT offset, the name of the committer, and the email address for the committer, as configured in Git. For more information about the difference between an author and a committer in Git, see <http://git-scm.com/book/ch2-3.html Viewing the Commit History> in Pro Git by Scott Chacon and Ben Straub.
cCommitter :: Lens' Commit (Maybe UserInfo)
cCommitter = lens _cCommitter (\ s a -> s{_cCommitter = a});

-- | Tree information for the specified commit.
cTreeId :: Lens' Commit (Maybe Text)
cTreeId = lens _cTreeId (\ s a -> s{_cTreeId = a});

-- | Any additional data associated with the specified commit.
cAdditionalData :: Lens' Commit (Maybe Text)
cAdditionalData = lens _cAdditionalData (\ s a -> s{_cAdditionalData = a});

-- | The parent list for the specified commit.
cParents :: Lens' Commit [Text]
cParents = lens _cParents (\ s a -> s{_cParents = a}) . _Default . _Coerce;

-- | Information about the author of the specified commit. Information includes the date in timestamp format with GMT offset, the name of the author, and the email address for the author, as configured in Git.
cAuthor :: Lens' Commit (Maybe UserInfo)
cAuthor = lens _cAuthor (\ s a -> s{_cAuthor = a});

-- | The commit message associated with the specified commit.
cMessage :: Lens' Commit (Maybe Text)
cMessage = lens _cMessage (\ s a -> s{_cMessage = a});

instance FromJSON Commit where
        parseJSON
          = withObject "Commit"
              (\ x ->
                 Commit' <$>
                   (x .:? "commitId") <*> (x .:? "committer") <*>
                     (x .:? "treeId")
                     <*> (x .:? "additionalData")
                     <*> (x .:? "parents" .!= mempty)
                     <*> (x .:? "author")
                     <*> (x .:? "message"))

instance Hashable Commit where

instance NFData Commit where

-- | Returns information about a set of differences for a commit specifier.
--
--
--
-- /See:/ 'difference' smart constructor.
data Difference = Difference'
  { _dAfterBlob  :: !(Maybe BlobMetadata)
  , _dBeforeBlob :: !(Maybe BlobMetadata)
  , _dChangeType :: !(Maybe ChangeTypeEnum)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Difference' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dAfterBlob' - Information about an @afterBlob@ data type object, including the ID, the file mode permission code, and the path.
--
-- * 'dBeforeBlob' - Information about a @beforeBlob@ data type object, including the ID, the file mode permission code, and the path.
--
-- * 'dChangeType' - Whether the change type of the difference is an addition (A), deletion (D), or modification (M).
difference
    :: Difference
difference =
  Difference'
  {_dAfterBlob = Nothing, _dBeforeBlob = Nothing, _dChangeType = Nothing}


-- | Information about an @afterBlob@ data type object, including the ID, the file mode permission code, and the path.
dAfterBlob :: Lens' Difference (Maybe BlobMetadata)
dAfterBlob = lens _dAfterBlob (\ s a -> s{_dAfterBlob = a});

-- | Information about a @beforeBlob@ data type object, including the ID, the file mode permission code, and the path.
dBeforeBlob :: Lens' Difference (Maybe BlobMetadata)
dBeforeBlob = lens _dBeforeBlob (\ s a -> s{_dBeforeBlob = a});

-- | Whether the change type of the difference is an addition (A), deletion (D), or modification (M).
dChangeType :: Lens' Difference (Maybe ChangeTypeEnum)
dChangeType = lens _dChangeType (\ s a -> s{_dChangeType = a});

instance FromJSON Difference where
        parseJSON
          = withObject "Difference"
              (\ x ->
                 Difference' <$>
                   (x .:? "afterBlob") <*> (x .:? "beforeBlob") <*>
                     (x .:? "changeType"))

instance Hashable Difference where

instance NFData Difference where

-- | Information about a repository.
--
--
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
  , _rmRepositoryName        :: !(Maybe Text)
  , _rmCreationDate          :: !(Maybe POSIX)
  , _rmCloneURLSSH           :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


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
  , _rmRepositoryName = Nothing
  , _rmCreationDate = Nothing
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

-- | The repository's default branch name.
rmDefaultBranch :: Lens' RepositoryMetadata (Maybe Text)
rmDefaultBranch = lens _rmDefaultBranch (\ s a -> s{_rmDefaultBranch = a});

-- | The ID of the repository.
rmRepositoryId :: Lens' RepositoryMetadata (Maybe Text)
rmRepositoryId = lens _rmRepositoryId (\ s a -> s{_rmRepositoryId = a});

-- | The repository's name.
rmRepositoryName :: Lens' RepositoryMetadata (Maybe Text)
rmRepositoryName = lens _rmRepositoryName (\ s a -> s{_rmRepositoryName = a});

-- | The date and time the repository was created, in timestamp format.
rmCreationDate :: Lens' RepositoryMetadata (Maybe UTCTime)
rmCreationDate = lens _rmCreationDate (\ s a -> s{_rmCreationDate = a}) . mapping _Time;

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
                     <*> (x .:? "repositoryName")
                     <*> (x .:? "creationDate")
                     <*> (x .:? "cloneUrlSsh"))

instance Hashable RepositoryMetadata where

instance NFData RepositoryMetadata where

-- | Information about a repository name and ID.
--
--
--
-- /See:/ 'repositoryNameIdPair' smart constructor.
data RepositoryNameIdPair = RepositoryNameIdPair'
  { _rnipRepositoryId   :: !(Maybe Text)
  , _rnipRepositoryName :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RepositoryNameIdPair' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rnipRepositoryId' - The ID associated with the repository.
--
-- * 'rnipRepositoryName' - The name associated with the repository.
repositoryNameIdPair
    :: RepositoryNameIdPair
repositoryNameIdPair =
  RepositoryNameIdPair'
  {_rnipRepositoryId = Nothing, _rnipRepositoryName = Nothing}


-- | The ID associated with the repository.
rnipRepositoryId :: Lens' RepositoryNameIdPair (Maybe Text)
rnipRepositoryId = lens _rnipRepositoryId (\ s a -> s{_rnipRepositoryId = a});

-- | The name associated with the repository.
rnipRepositoryName :: Lens' RepositoryNameIdPair (Maybe Text)
rnipRepositoryName = lens _rnipRepositoryName (\ s a -> s{_rnipRepositoryName = a});

instance FromJSON RepositoryNameIdPair where
        parseJSON
          = withObject "RepositoryNameIdPair"
              (\ x ->
                 RepositoryNameIdPair' <$>
                   (x .:? "repositoryId") <*> (x .:? "repositoryName"))

instance Hashable RepositoryNameIdPair where

instance NFData RepositoryNameIdPair where

-- | Information about a trigger for a repository.
--
--
--
-- /See:/ 'repositoryTrigger' smart constructor.
data RepositoryTrigger = RepositoryTrigger'
  { _rtBranches       :: !(Maybe [Text])
  , _rtCustomData     :: !(Maybe Text)
  , _rtName           :: !Text
  , _rtDestinationARN :: !Text
  , _rtEvents         :: ![RepositoryTriggerEventEnum]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RepositoryTrigger' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rtBranches' - The branches that will be included in the trigger configuration. If you specify an empty array, the trigger will apply to all branches.
--
-- * 'rtCustomData' - Any custom data associated with the trigger that will be included in the information sent to the target of the trigger.
--
-- * 'rtName' - The name of the trigger.
--
-- * 'rtDestinationARN' - The ARN of the resource that is the target for a trigger. For example, the ARN of a topic in Amazon Simple Notification Service (SNS).
--
-- * 'rtEvents' - The repository events that will cause the trigger to run actions in another service, such as sending a notification through Amazon Simple Notification Service (SNS).
repositoryTrigger
    :: Text -- ^ 'rtName'
    -> Text -- ^ 'rtDestinationARN'
    -> RepositoryTrigger
repositoryTrigger pName_ pDestinationARN_ =
  RepositoryTrigger'
  { _rtBranches = Nothing
  , _rtCustomData = Nothing
  , _rtName = pName_
  , _rtDestinationARN = pDestinationARN_
  , _rtEvents = mempty
  }


-- | The branches that will be included in the trigger configuration. If you specify an empty array, the trigger will apply to all branches.
rtBranches :: Lens' RepositoryTrigger [Text]
rtBranches = lens _rtBranches (\ s a -> s{_rtBranches = a}) . _Default . _Coerce;

-- | Any custom data associated with the trigger that will be included in the information sent to the target of the trigger.
rtCustomData :: Lens' RepositoryTrigger (Maybe Text)
rtCustomData = lens _rtCustomData (\ s a -> s{_rtCustomData = a});

-- | The name of the trigger.
rtName :: Lens' RepositoryTrigger Text
rtName = lens _rtName (\ s a -> s{_rtName = a});

-- | The ARN of the resource that is the target for a trigger. For example, the ARN of a topic in Amazon Simple Notification Service (SNS).
rtDestinationARN :: Lens' RepositoryTrigger Text
rtDestinationARN = lens _rtDestinationARN (\ s a -> s{_rtDestinationARN = a});

-- | The repository events that will cause the trigger to run actions in another service, such as sending a notification through Amazon Simple Notification Service (SNS).
rtEvents :: Lens' RepositoryTrigger [RepositoryTriggerEventEnum]
rtEvents = lens _rtEvents (\ s a -> s{_rtEvents = a}) . _Coerce;

instance FromJSON RepositoryTrigger where
        parseJSON
          = withObject "RepositoryTrigger"
              (\ x ->
                 RepositoryTrigger' <$>
                   (x .:? "branches" .!= mempty) <*>
                     (x .:? "customData")
                     <*> (x .: "name")
                     <*> (x .: "destinationArn")
                     <*> (x .:? "events" .!= mempty))

instance Hashable RepositoryTrigger where

instance NFData RepositoryTrigger where

instance ToJSON RepositoryTrigger where
        toJSON RepositoryTrigger'{..}
          = object
              (catMaybes
                 [("branches" .=) <$> _rtBranches,
                  ("customData" .=) <$> _rtCustomData,
                  Just ("name" .= _rtName),
                  Just ("destinationArn" .= _rtDestinationARN),
                  Just ("events" .= _rtEvents)])

-- | A trigger failed to run.
--
--
--
-- /See:/ 'repositoryTriggerExecutionFailure' smart constructor.
data RepositoryTriggerExecutionFailure = RepositoryTriggerExecutionFailure'
  { _rtefFailureMessage :: !(Maybe Text)
  , _rtefTrigger        :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RepositoryTriggerExecutionFailure' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rtefFailureMessage' - Additional message information about the trigger that did not run.
--
-- * 'rtefTrigger' - The name of the trigger that did not run.
repositoryTriggerExecutionFailure
    :: RepositoryTriggerExecutionFailure
repositoryTriggerExecutionFailure =
  RepositoryTriggerExecutionFailure'
  {_rtefFailureMessage = Nothing, _rtefTrigger = Nothing}


-- | Additional message information about the trigger that did not run.
rtefFailureMessage :: Lens' RepositoryTriggerExecutionFailure (Maybe Text)
rtefFailureMessage = lens _rtefFailureMessage (\ s a -> s{_rtefFailureMessage = a});

-- | The name of the trigger that did not run.
rtefTrigger :: Lens' RepositoryTriggerExecutionFailure (Maybe Text)
rtefTrigger = lens _rtefTrigger (\ s a -> s{_rtefTrigger = a});

instance FromJSON RepositoryTriggerExecutionFailure
         where
        parseJSON
          = withObject "RepositoryTriggerExecutionFailure"
              (\ x ->
                 RepositoryTriggerExecutionFailure' <$>
                   (x .:? "failureMessage") <*> (x .:? "trigger"))

instance Hashable RepositoryTriggerExecutionFailure
         where

instance NFData RepositoryTriggerExecutionFailure
         where

-- | Information about the user who made a specified commit.
--
--
--
-- /See:/ 'userInfo' smart constructor.
data UserInfo = UserInfo'
  { _uiEmail :: !(Maybe Text)
  , _uiDate  :: !(Maybe Text)
  , _uiName  :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UserInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uiEmail' - The email address associated with the user who made the commit, if any.
--
-- * 'uiDate' - The date when the specified commit was pushed to the repository.
--
-- * 'uiName' - The name of the user who made the specified commit.
userInfo
    :: UserInfo
userInfo = UserInfo' {_uiEmail = Nothing, _uiDate = Nothing, _uiName = Nothing}


-- | The email address associated with the user who made the commit, if any.
uiEmail :: Lens' UserInfo (Maybe Text)
uiEmail = lens _uiEmail (\ s a -> s{_uiEmail = a});

-- | The date when the specified commit was pushed to the repository.
uiDate :: Lens' UserInfo (Maybe Text)
uiDate = lens _uiDate (\ s a -> s{_uiDate = a});

-- | The name of the user who made the specified commit.
uiName :: Lens' UserInfo (Maybe Text)
uiName = lens _uiName (\ s a -> s{_uiName = a});

instance FromJSON UserInfo where
        parseJSON
          = withObject "UserInfo"
              (\ x ->
                 UserInfo' <$>
                   (x .:? "email") <*> (x .:? "date") <*>
                     (x .:? "name"))

instance Hashable UserInfo where

instance NFData UserInfo where
