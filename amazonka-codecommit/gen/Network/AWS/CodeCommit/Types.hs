{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.Types
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodeCommit.Types
    (
    -- * Service
      CodeCommit

    -- * Errors
    , _InvalidContinuationTokenException
    , _EncryptionKeyNotFoundException
    , _RepositoryNameExistsException
    , _MaximumRepositoryNamesExceededException
    , _InvalidRepositoryDescriptionException
    , _BranchNameRequiredException
    , _InvalidBranchNameException
    , _EncryptionKeyUnavailableException
    , _InvalidOrderException
    , _BranchDoesNotExistException
    , _RepositoryNamesRequiredException
    , _RepositoryDoesNotExistException
    , _EncryptionIntegrityChecksFailedException
    , _EncryptionKeyAccessDeniedException
    , _BranchNameExistsException
    , _EncryptionKeyDisabledException
    , _InvalidSortByException
    , _CommitIdRequiredException
    , _InvalidCommitIdException
    , _CommitDoesNotExistException
    , _RepositoryLimitExceededException
    , _InvalidRepositoryNameException
    , _RepositoryNameRequiredException

    -- * OrderEnum
    , OrderEnum (..)

    -- * SortByEnum
    , SortByEnum (..)

    -- * BranchInfo
    , BranchInfo
    , branchInfo
    , biCommitId
    , biBranchName

    -- * RepositoryMetadata
    , RepositoryMetadata
    , repositoryMetadata
    , rmRepositoryDescription
    , rmLastModifiedDate
    , rmARN
    , rmCloneURLHTTP
    , rmAccountId
    , rmDefaultBranch
    , rmRepositoryId
    , rmCreationDate
    , rmRepositoryName
    , rmCloneURLSSH

    -- * RepositoryNameIdPair
    , RepositoryNameIdPair
    , repositoryNameIdPair
    , rnipRepositoryId
    , rnipRepositoryName
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Sign.V4

-- | Version @2015-04-13@ of the Amazon CodeCommit SDK.
data CodeCommit

instance AWSService CodeCommit where
    type Sg CodeCommit = V4
    service = const svc
      where
        svc =
            Service
            { _svcAbbrev = "CodeCommit"
            , _svcPrefix = "codecommit"
            , _svcVersion = "2015-04-13"
            , _svcEndpoint = defaultEndpoint svc
            , _svcTimeout = Just 70000000
            , _svcStatus = statusSuccess
            , _svcError = parseJSONError
            , _svcRetry = retry
            }
        retry =
            Exponential
            { _retryBase = 5.0e-2
            , _retryGrowth = 2
            , _retryAttempts = 5
            , _retryCheck = check
            }
        check e
          | has (hasCode "ThrottlingException" . hasStatus 400) e =
              Just "throttling_exception"
          | has (hasCode "Throttling" . hasStatus 400) e = Just "throttling"
          | has (hasStatus 503) e = Just "service_unavailable"
          | has (hasStatus 500) e = Just "general_server_error"
          | has (hasStatus 509) e = Just "limit_exceeded"
          | otherwise = Nothing

-- | The specified continuation token is not valid.
_InvalidContinuationTokenException :: AWSError a => Getting (First ServiceError) a ServiceError
_InvalidContinuationTokenException =
    _ServiceError . hasCode "InvalidContinuationTokenException"

-- | No encryption key was found.
_EncryptionKeyNotFoundException :: AWSError a => Getting (First ServiceError) a ServiceError
_EncryptionKeyNotFoundException =
    _ServiceError . hasCode "EncryptionKeyNotFoundException"

-- | The specified repository name already exists.
_RepositoryNameExistsException :: AWSError a => Getting (First ServiceError) a ServiceError
_RepositoryNameExistsException =
    _ServiceError . hasCode "RepositoryNameExistsException"

-- | The maximum number of allowed repository names was exceeded. Currently,
-- this number is 25.
_MaximumRepositoryNamesExceededException :: AWSError a => Getting (First ServiceError) a ServiceError
_MaximumRepositoryNamesExceededException =
    _ServiceError . hasCode "MaximumRepositoryNamesExceededException"

-- | The specified repository description is not valid.
_InvalidRepositoryDescriptionException :: AWSError a => Getting (First ServiceError) a ServiceError
_InvalidRepositoryDescriptionException =
    _ServiceError . hasCode "InvalidRepositoryDescriptionException"

-- | A branch name is required but was not specified.
_BranchNameRequiredException :: AWSError a => Getting (First ServiceError) a ServiceError
_BranchNameRequiredException =
    _ServiceError . hasCode "BranchNameRequiredException"

-- | The specified branch name is not valid.
_InvalidBranchNameException :: AWSError a => Getting (First ServiceError) a ServiceError
_InvalidBranchNameException =
    _ServiceError . hasCode "InvalidBranchNameException"

-- | The encryption key is not available.
_EncryptionKeyUnavailableException :: AWSError a => Getting (First ServiceError) a ServiceError
_EncryptionKeyUnavailableException =
    _ServiceError . hasCode "EncryptionKeyUnavailableException"

-- | The specified sort order is not valid.
_InvalidOrderException :: AWSError a => Getting (First ServiceError) a ServiceError
_InvalidOrderException = _ServiceError . hasCode "InvalidOrderException"

-- | The specified branch does not exist.
_BranchDoesNotExistException :: AWSError a => Getting (First ServiceError) a ServiceError
_BranchDoesNotExistException =
    _ServiceError . hasCode "BranchDoesNotExistException"

-- | A repository names object is required but was not specified.
_RepositoryNamesRequiredException :: AWSError a => Getting (First ServiceError) a ServiceError
_RepositoryNamesRequiredException =
    _ServiceError . hasCode "RepositoryNamesRequiredException"

-- | The specified repository does not exist.
_RepositoryDoesNotExistException :: AWSError a => Getting (First ServiceError) a ServiceError
_RepositoryDoesNotExistException =
    _ServiceError . hasCode "RepositoryDoesNotExistException"

-- | An encryption integrity check failed.
_EncryptionIntegrityChecksFailedException :: AWSError a => Getting (First ServiceError) a ServiceError
_EncryptionIntegrityChecksFailedException =
    _ServiceError . hasCode "EncryptionIntegrityChecksFailedException"

-- | An encryption key could not be accessed.
_EncryptionKeyAccessDeniedException :: AWSError a => Getting (First ServiceError) a ServiceError
_EncryptionKeyAccessDeniedException =
    _ServiceError . hasCode "EncryptionKeyAccessDeniedException"

-- | The specified branch name already exists.
_BranchNameExistsException :: AWSError a => Getting (First ServiceError) a ServiceError
_BranchNameExistsException =
    _ServiceError . hasCode "BranchNameExistsException"

-- | The encryption key is disabled.
_EncryptionKeyDisabledException :: AWSError a => Getting (First ServiceError) a ServiceError
_EncryptionKeyDisabledException =
    _ServiceError . hasCode "EncryptionKeyDisabledException"

-- | The specified sort by value is not valid.
_InvalidSortByException :: AWSError a => Getting (First ServiceError) a ServiceError
_InvalidSortByException = _ServiceError . hasCode "InvalidSortByException"

-- | A commit ID was not specified.
_CommitIdRequiredException :: AWSError a => Getting (First ServiceError) a ServiceError
_CommitIdRequiredException =
    _ServiceError . hasCode "CommitIdRequiredException"

-- | The specified commit ID is not valid.
_InvalidCommitIdException :: AWSError a => Getting (First ServiceError) a ServiceError
_InvalidCommitIdException = _ServiceError . hasCode "InvalidCommitIdException"

-- | The specified commit does not exist or no commit was specified, and the
-- specified repository has no default branch.
_CommitDoesNotExistException :: AWSError a => Getting (First ServiceError) a ServiceError
_CommitDoesNotExistException =
    _ServiceError . hasCode "CommitDoesNotExistException"

-- | A repository resource limit was exceeded.
_RepositoryLimitExceededException :: AWSError a => Getting (First ServiceError) a ServiceError
_RepositoryLimitExceededException =
    _ServiceError . hasCode "RepositoryLimitExceededException"

-- | At least one specified repository name is not valid.
--
-- This exception only occurs when a specified repository name is not
-- valid. Other exceptions occur when a required repository parameter is
-- missing, or when a specified repository does not exist.
_InvalidRepositoryNameException :: AWSError a => Getting (First ServiceError) a ServiceError
_InvalidRepositoryNameException =
    _ServiceError . hasCode "InvalidRepositoryNameException"

-- | A repository name is required but was not specified.
_RepositoryNameRequiredException :: AWSError a => Getting (First ServiceError) a ServiceError
_RepositoryNameRequiredException =
    _ServiceError . hasCode "RepositoryNameRequiredException"

data OrderEnum
    = Ascending
    | Descending
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText OrderEnum where
    parser = takeLowerText >>= \case
        "ascending" -> pure Ascending
        "descending" -> pure Descending
        e -> fromTextError $ "Failure parsing OrderEnum from value: '" <> e
           <> "'. Accepted values: ascending, descending"

instance ToText OrderEnum where
    toText = \case
        Ascending -> "ascending"
        Descending -> "descending"

instance Hashable OrderEnum
instance ToQuery OrderEnum
instance ToHeader OrderEnum

instance ToJSON OrderEnum where
    toJSON = toJSONText

data SortByEnum
    = LastModifiedDate
    | RepositoryName
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText SortByEnum where
    parser = takeLowerText >>= \case
        "lastmodifieddate" -> pure LastModifiedDate
        "repositoryname" -> pure RepositoryName
        e -> fromTextError $ "Failure parsing SortByEnum from value: '" <> e
           <> "'. Accepted values: lastmodifieddate, repositoryname"

instance ToText SortByEnum where
    toText = \case
        LastModifiedDate -> "lastmodifieddate"
        RepositoryName -> "repositoryname"

instance Hashable SortByEnum
instance ToQuery SortByEnum
instance ToHeader SortByEnum

instance ToJSON SortByEnum where
    toJSON = toJSONText

-- | Returns information about a branch.
--
-- /See:/ 'branchInfo' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'biCommitId'
--
-- * 'biBranchName'
data BranchInfo = BranchInfo'
    { _biCommitId   :: !(Maybe Text)
    , _biBranchName :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'BranchInfo' smart constructor.
branchInfo :: BranchInfo
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
--
-- The fields accessible through corresponding lenses are:
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

-- | 'RepositoryMetadata' smart constructor.
repositoryMetadata :: RepositoryMetadata
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
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rnipRepositoryId'
--
-- * 'rnipRepositoryName'
data RepositoryNameIdPair = RepositoryNameIdPair'
    { _rnipRepositoryId   :: !(Maybe Text)
    , _rnipRepositoryName :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'RepositoryNameIdPair' smart constructor.
repositoryNameIdPair :: RepositoryNameIdPair
repositoryNameIdPair =
    RepositoryNameIdPair'
    { _rnipRepositoryId = Nothing
    , _rnipRepositoryName = Nothing
    }

-- | The ID associated with the repository name.
rnipRepositoryId :: Lens' RepositoryNameIdPair (Maybe Text)
rnipRepositoryId = lens _rnipRepositoryId (\ s a -> s{_rnipRepositoryId = a});

-- | FIXME: Undocumented member.
rnipRepositoryName :: Lens' RepositoryNameIdPair (Maybe Text)
rnipRepositoryName = lens _rnipRepositoryName (\ s a -> s{_rnipRepositoryName = a});

instance FromJSON RepositoryNameIdPair where
        parseJSON
          = withObject "RepositoryNameIdPair"
              (\ x ->
                 RepositoryNameIdPair' <$>
                   (x .:? "repositoryId") <*> (x .:? "repositoryName"))
