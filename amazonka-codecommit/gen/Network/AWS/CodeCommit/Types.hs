{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.Types
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodeCommit.Types
    (
    -- * Service Configuration
      codeCommit

    -- * Errors
    , _InvalidRepositoryTriggerRegionException
    , _InvalidContinuationTokenException
    , _EncryptionKeyNotFoundException
    , _InvalidRepositoryTriggerBranchNameException
    , _InvalidRepositoryTriggerCustomDataException
    , _BlobIdDoesNotExistException
    , _MaximumRepositoryNamesExceededException
    , _InvalidRepositoryDescriptionException
    , _RepositoryNameExistsException
    , _MaximumRepositoryTriggersExceededException
    , _InvalidBranchNameException
    , _BranchNameRequiredException
    , _RepositoryTriggersListRequiredException
    , _EncryptionKeyUnavailableException
    , _InvalidRepositoryTriggerDestinationARNException
    , _BlobIdRequiredException
    , _RepositoryNamesRequiredException
    , _InvalidBlobIdException
    , _InvalidOrderException
    , _BranchDoesNotExistException
    , _InvalidPathException
    , _RepositoryTriggerNameRequiredException
    , _RepositoryDoesNotExistException
    , _MaximumBranchesExceededException
    , _PathDoesNotExistException
    , _EncryptionIntegrityChecksFailedException
    , _RepositoryTriggerEventsListRequiredException
    , _EncryptionKeyAccessDeniedException
    , _BranchNameExistsException
    , _InvalidCommitException
    , _InvalidSortByException
    , _EncryptionKeyDisabledException
    , _CommitRequiredException
    , _CommitIdRequiredException
    , _InvalidCommitIdException
    , _RepositoryTriggerDestinationARNRequiredException
    , _CommitDoesNotExistException
    , _RepositoryTriggerBranchNameListRequiredException
    , _InvalidMaxResultsException
    , _FileTooLargeException
    , _CommitIdDoesNotExistException
    , _InvalidRepositoryTriggerNameException
    , _RepositoryNameRequiredException
    , _RepositoryLimitExceededException
    , _InvalidRepositoryTriggerEventsException
    , _InvalidRepositoryNameException

    -- * ChangeTypeEnum
    , ChangeTypeEnum (..)

    -- * OrderEnum
    , OrderEnum (..)

    -- * RepositoryTriggerEventEnum
    , RepositoryTriggerEventEnum (..)

    -- * SortByEnum
    , SortByEnum (..)

    -- * BlobMetadata
    , BlobMetadata
    , blobMetadata
    , bmPath
    , bmMode
    , bmBlobId

    -- * BranchInfo
    , BranchInfo
    , branchInfo
    , biCommitId
    , biBranchName

    -- * Commit
    , Commit
    , commit
    , cCommitter
    , cTreeId
    , cAdditionalData
    , cParents
    , cAuthor
    , cMessage

    -- * Difference
    , Difference
    , difference
    , dAfterBlob
    , dBeforeBlob
    , dChangeType

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
    , rmRepositoryName
    , rmCreationDate
    , rmCloneURLSSH

    -- * RepositoryNameIdPair
    , RepositoryNameIdPair
    , repositoryNameIdPair
    , rnipRepositoryId
    , rnipRepositoryName

    -- * RepositoryTrigger
    , RepositoryTrigger
    , repositoryTrigger
    , rtBranches
    , rtCustomData
    , rtName
    , rtDestinationARN
    , rtEvents

    -- * RepositoryTriggerExecutionFailure
    , RepositoryTriggerExecutionFailure
    , repositoryTriggerExecutionFailure
    , rtefFailureMessage
    , rtefTrigger

    -- * UserInfo
    , UserInfo
    , userInfo
    , uiEmail
    , uiDate
    , uiName
    ) where

import           Network.AWS.CodeCommit.Types.Product
import           Network.AWS.CodeCommit.Types.Sum
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Sign.V4

-- | API version @2015-04-13@ of the Amazon CodeCommit SDK configuration.
codeCommit :: Service
codeCommit =
    Service
    { _svcAbbrev = "CodeCommit"
    , _svcSigner = v4
    , _svcPrefix = "codecommit"
    , _svcVersion = "2015-04-13"
    , _svcEndpoint = defaultEndpoint codeCommit
    , _svcTimeout = Just 70
    , _svcCheck = statusSuccess
    , _svcError = parseJSONError "CodeCommit"
    , _svcRetry = retry
    }
  where
    retry =
        Exponential
        { _retryBase = 5.0e-2
        , _retryGrowth = 2
        , _retryAttempts = 5
        , _retryCheck = check
        }
    check e
      | has (hasCode "ThrottledException" . hasStatus 400) e =
          Just "throttled_exception"
      | has (hasStatus 429) e = Just "too_many_requests"
      | has (hasCode "ThrottlingException" . hasStatus 400) e =
          Just "throttling_exception"
      | has (hasCode "Throttling" . hasStatus 400) e = Just "throttling"
      | has (hasStatus 504) e = Just "gateway_timeout"
      | has (hasStatus 502) e = Just "bad_gateway"
      | has (hasStatus 503) e = Just "service_unavailable"
      | has (hasStatus 500) e = Just "general_server_error"
      | has (hasStatus 509) e = Just "limit_exceeded"
      | otherwise = Nothing

-- | The region for the trigger target does not match the region for the repository. Triggers must be created in the same region as the target for the trigger.
--
--
_InvalidRepositoryTriggerRegionException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidRepositoryTriggerRegionException =
    _MatchServiceError codeCommit "InvalidRepositoryTriggerRegionException"

-- | The specified continuation token is not valid.
--
--
_InvalidContinuationTokenException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidContinuationTokenException =
    _MatchServiceError codeCommit "InvalidContinuationTokenException"

-- | No encryption key was found.
--
--
_EncryptionKeyNotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_EncryptionKeyNotFoundException =
    _MatchServiceError codeCommit "EncryptionKeyNotFoundException"

-- | One or more branch names specified for the trigger is not valid.
--
--
_InvalidRepositoryTriggerBranchNameException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidRepositoryTriggerBranchNameException =
    _MatchServiceError codeCommit "InvalidRepositoryTriggerBranchNameException"

-- | The custom data provided for the trigger is not valid.
--
--
_InvalidRepositoryTriggerCustomDataException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidRepositoryTriggerCustomDataException =
    _MatchServiceError codeCommit "InvalidRepositoryTriggerCustomDataException"

-- | The specified blob does not exist.
--
--
_BlobIdDoesNotExistException :: AsError a => Getting (First ServiceError) a ServiceError
_BlobIdDoesNotExistException =
    _MatchServiceError codeCommit "BlobIdDoesNotExistException"

-- | The maximum number of allowed repository names was exceeded. Currently, this number is 25.
--
--
_MaximumRepositoryNamesExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_MaximumRepositoryNamesExceededException =
    _MatchServiceError codeCommit "MaximumRepositoryNamesExceededException"

-- | The specified repository description is not valid.
--
--
_InvalidRepositoryDescriptionException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidRepositoryDescriptionException =
    _MatchServiceError codeCommit "InvalidRepositoryDescriptionException"

-- | The specified repository name already exists.
--
--
_RepositoryNameExistsException :: AsError a => Getting (First ServiceError) a ServiceError
_RepositoryNameExistsException =
    _MatchServiceError codeCommit "RepositoryNameExistsException"

-- | The number of triggers allowed for the repository was exceeded.
--
--
_MaximumRepositoryTriggersExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_MaximumRepositoryTriggersExceededException =
    _MatchServiceError codeCommit "MaximumRepositoryTriggersExceededException"

-- | The specified branch name is not valid.
--
--
_InvalidBranchNameException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidBranchNameException =
    _MatchServiceError codeCommit "InvalidBranchNameException"

-- | A branch name is required but was not specified.
--
--
_BranchNameRequiredException :: AsError a => Getting (First ServiceError) a ServiceError
_BranchNameRequiredException =
    _MatchServiceError codeCommit "BranchNameRequiredException"

-- | The list of triggers for the repository is required but was not specified.
--
--
_RepositoryTriggersListRequiredException :: AsError a => Getting (First ServiceError) a ServiceError
_RepositoryTriggersListRequiredException =
    _MatchServiceError codeCommit "RepositoryTriggersListRequiredException"

-- | The encryption key is not available.
--
--
_EncryptionKeyUnavailableException :: AsError a => Getting (First ServiceError) a ServiceError
_EncryptionKeyUnavailableException =
    _MatchServiceError codeCommit "EncryptionKeyUnavailableException"

-- | The Amazon Resource Name (ARN) for the trigger is not valid for the specified destination. The most common reason for this error is that the ARN does not meet the requirements for the service type.
--
--
_InvalidRepositoryTriggerDestinationARNException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidRepositoryTriggerDestinationARNException =
    _MatchServiceError
        codeCommit
        "InvalidRepositoryTriggerDestinationArnException"

-- | A blob ID is required but was not specified.
--
--
_BlobIdRequiredException :: AsError a => Getting (First ServiceError) a ServiceError
_BlobIdRequiredException =
    _MatchServiceError codeCommit "BlobIdRequiredException"

-- | A repository names object is required but was not specified.
--
--
_RepositoryNamesRequiredException :: AsError a => Getting (First ServiceError) a ServiceError
_RepositoryNamesRequiredException =
    _MatchServiceError codeCommit "RepositoryNamesRequiredException"

-- | The specified blob is not valid.
--
--
_InvalidBlobIdException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidBlobIdException =
    _MatchServiceError codeCommit "InvalidBlobIdException"

-- | The specified sort order is not valid.
--
--
_InvalidOrderException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidOrderException = _MatchServiceError codeCommit "InvalidOrderException"

-- | The specified branch does not exist.
--
--
_BranchDoesNotExistException :: AsError a => Getting (First ServiceError) a ServiceError
_BranchDoesNotExistException =
    _MatchServiceError codeCommit "BranchDoesNotExistException"

-- | The specified path is not valid.
--
--
_InvalidPathException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidPathException = _MatchServiceError codeCommit "InvalidPathException"

-- | A name for the trigger is required but was not specified.
--
--
_RepositoryTriggerNameRequiredException :: AsError a => Getting (First ServiceError) a ServiceError
_RepositoryTriggerNameRequiredException =
    _MatchServiceError codeCommit "RepositoryTriggerNameRequiredException"

-- | The specified repository does not exist.
--
--
_RepositoryDoesNotExistException :: AsError a => Getting (First ServiceError) a ServiceError
_RepositoryDoesNotExistException =
    _MatchServiceError codeCommit "RepositoryDoesNotExistException"

-- | The number of branches for the trigger was exceeded.
--
--
_MaximumBranchesExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_MaximumBranchesExceededException =
    _MatchServiceError codeCommit "MaximumBranchesExceededException"

-- | The specified path does not exist.
--
--
_PathDoesNotExistException :: AsError a => Getting (First ServiceError) a ServiceError
_PathDoesNotExistException =
    _MatchServiceError codeCommit "PathDoesNotExistException"

-- | An encryption integrity check failed.
--
--
_EncryptionIntegrityChecksFailedException :: AsError a => Getting (First ServiceError) a ServiceError
_EncryptionIntegrityChecksFailedException =
    _MatchServiceError codeCommit "EncryptionIntegrityChecksFailedException"

-- | At least one event for the trigger is required but was not specified.
--
--
_RepositoryTriggerEventsListRequiredException :: AsError a => Getting (First ServiceError) a ServiceError
_RepositoryTriggerEventsListRequiredException =
    _MatchServiceError
        codeCommit
        "RepositoryTriggerEventsListRequiredException"

-- | An encryption key could not be accessed.
--
--
_EncryptionKeyAccessDeniedException :: AsError a => Getting (First ServiceError) a ServiceError
_EncryptionKeyAccessDeniedException =
    _MatchServiceError codeCommit "EncryptionKeyAccessDeniedException"

-- | The specified branch name already exists.
--
--
_BranchNameExistsException :: AsError a => Getting (First ServiceError) a ServiceError
_BranchNameExistsException =
    _MatchServiceError codeCommit "BranchNameExistsException"

-- | The specified commit is not valid.
--
--
_InvalidCommitException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidCommitException =
    _MatchServiceError codeCommit "InvalidCommitException"

-- | The specified sort by value is not valid.
--
--
_InvalidSortByException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidSortByException =
    _MatchServiceError codeCommit "InvalidSortByException"

-- | The encryption key is disabled.
--
--
_EncryptionKeyDisabledException :: AsError a => Getting (First ServiceError) a ServiceError
_EncryptionKeyDisabledException =
    _MatchServiceError codeCommit "EncryptionKeyDisabledException"

-- | A commit was not specified.
--
--
_CommitRequiredException :: AsError a => Getting (First ServiceError) a ServiceError
_CommitRequiredException =
    _MatchServiceError codeCommit "CommitRequiredException"

-- | A commit ID was not specified.
--
--
_CommitIdRequiredException :: AsError a => Getting (First ServiceError) a ServiceError
_CommitIdRequiredException =
    _MatchServiceError codeCommit "CommitIdRequiredException"

-- | The specified commit ID is not valid.
--
--
_InvalidCommitIdException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidCommitIdException =
    _MatchServiceError codeCommit "InvalidCommitIdException"

-- | A destination ARN for the target service for the trigger is required but was not specified.
--
--
_RepositoryTriggerDestinationARNRequiredException :: AsError a => Getting (First ServiceError) a ServiceError
_RepositoryTriggerDestinationARNRequiredException =
    _MatchServiceError
        codeCommit
        "RepositoryTriggerDestinationArnRequiredException"

-- | The specified commit does not exist or no commit was specified, and the specified repository has no default branch.
--
--
_CommitDoesNotExistException :: AsError a => Getting (First ServiceError) a ServiceError
_CommitDoesNotExistException =
    _MatchServiceError codeCommit "CommitDoesNotExistException"

-- | At least one branch name is required but was not specified in the trigger configuration.
--
--
_RepositoryTriggerBranchNameListRequiredException :: AsError a => Getting (First ServiceError) a ServiceError
_RepositoryTriggerBranchNameListRequiredException =
    _MatchServiceError
        codeCommit
        "RepositoryTriggerBranchNameListRequiredException"

-- | The specified number of maximum results is not valid.
--
--
_InvalidMaxResultsException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidMaxResultsException =
    _MatchServiceError codeCommit "InvalidMaxResultsException"

-- | The specified file exceeds the file size limit for AWS CodeCommit. For more information about limits in AWS CodeCommit, see <http://docs.aws.amazon.com/codecommit/latest/userguide/limits.html AWS CodeCommit User Guide> .
--
--
_FileTooLargeException :: AsError a => Getting (First ServiceError) a ServiceError
_FileTooLargeException = _MatchServiceError codeCommit "FileTooLargeException"

-- | The specified commit ID does not exist.
--
--
_CommitIdDoesNotExistException :: AsError a => Getting (First ServiceError) a ServiceError
_CommitIdDoesNotExistException =
    _MatchServiceError codeCommit "CommitIdDoesNotExistException"

-- | The name of the trigger is not valid.
--
--
_InvalidRepositoryTriggerNameException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidRepositoryTriggerNameException =
    _MatchServiceError codeCommit "InvalidRepositoryTriggerNameException"

-- | A repository name is required but was not specified.
--
--
_RepositoryNameRequiredException :: AsError a => Getting (First ServiceError) a ServiceError
_RepositoryNameRequiredException =
    _MatchServiceError codeCommit "RepositoryNameRequiredException"

-- | A repository resource limit was exceeded.
--
--
_RepositoryLimitExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_RepositoryLimitExceededException =
    _MatchServiceError codeCommit "RepositoryLimitExceededException"

-- | One or more events specified for the trigger is not valid. Check to make sure that all events specified match the requirements for allowed events.
--
--
_InvalidRepositoryTriggerEventsException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidRepositoryTriggerEventsException =
    _MatchServiceError codeCommit "InvalidRepositoryTriggerEventsException"

-- | At least one specified repository name is not valid.
--
--
_InvalidRepositoryNameException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidRepositoryNameException =
    _MatchServiceError codeCommit "InvalidRepositoryNameException"
