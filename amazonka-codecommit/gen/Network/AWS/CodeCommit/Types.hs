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
    , _MaximumRepositoryNamesExceededException
    , _InvalidRepositoryDescriptionException
    , _RepositoryNameExistsException
    , _MaximumRepositoryTriggersExceededException
    , _InvalidBranchNameException
    , _BranchNameRequiredException
    , _RepositoryTriggersListRequiredException
    , _EncryptionKeyUnavailableException
    , _InvalidRepositoryTriggerDestinationARNException
    , _RepositoryNamesRequiredException
    , _InvalidOrderException
    , _BranchDoesNotExistException
    , _RepositoryTriggerNameRequiredException
    , _RepositoryDoesNotExistException
    , _MaximumBranchesExceededException
    , _EncryptionIntegrityChecksFailedException
    , _RepositoryTriggerEventsListRequiredException
    , _EncryptionKeyAccessDeniedException
    , _BranchNameExistsException
    , _InvalidSortByException
    , _EncryptionKeyDisabledException
    , _CommitIdRequiredException
    , _InvalidCommitIdException
    , _RepositoryTriggerDestinationARNRequiredException
    , _CommitDoesNotExistException
    , _RepositoryTriggerBranchNameListRequiredException
    , _CommitIdDoesNotExistException
    , _InvalidRepositoryTriggerNameException
    , _RepositoryNameRequiredException
    , _RepositoryLimitExceededException
    , _InvalidRepositoryTriggerEventsException
    , _InvalidRepositoryNameException

    -- * OrderEnum
    , OrderEnum (..)

    -- * RepositoryTriggerEventEnum
    , RepositoryTriggerEventEnum (..)

    -- * SortByEnum
    , SortByEnum (..)

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
    , rtDestinationARN
    , rtName
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

-- | API version '2015-04-13' of the Amazon CodeCommit SDK configuration.
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
_InvalidRepositoryTriggerRegionException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidRepositoryTriggerRegionException =
    _ServiceError . hasCode "InvalidRepositoryTriggerRegionException"

-- | The specified continuation token is not valid.
_InvalidContinuationTokenException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidContinuationTokenException =
    _ServiceError . hasCode "InvalidContinuationTokenException"

-- | No encryption key was found.
_EncryptionKeyNotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_EncryptionKeyNotFoundException =
    _ServiceError . hasCode "EncryptionKeyNotFoundException"

-- | One or more branch names specified for the trigger is not valid.
_InvalidRepositoryTriggerBranchNameException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidRepositoryTriggerBranchNameException =
    _ServiceError . hasCode "InvalidRepositoryTriggerBranchNameException"

-- | The custom data provided for the trigger is not valid.
_InvalidRepositoryTriggerCustomDataException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidRepositoryTriggerCustomDataException =
    _ServiceError . hasCode "InvalidRepositoryTriggerCustomDataException"

-- | The maximum number of allowed repository names was exceeded. Currently, this number is 25.
_MaximumRepositoryNamesExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_MaximumRepositoryNamesExceededException =
    _ServiceError . hasCode "MaximumRepositoryNamesExceededException"

-- | The specified repository description is not valid.
_InvalidRepositoryDescriptionException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidRepositoryDescriptionException =
    _ServiceError . hasCode "InvalidRepositoryDescriptionException"

-- | The specified repository name already exists.
_RepositoryNameExistsException :: AsError a => Getting (First ServiceError) a ServiceError
_RepositoryNameExistsException =
    _ServiceError . hasCode "RepositoryNameExistsException"

-- | The number of triggers allowed for the repository was exceeded.
_MaximumRepositoryTriggersExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_MaximumRepositoryTriggersExceededException =
    _ServiceError . hasCode "MaximumRepositoryTriggersExceededException"

-- | The specified branch name is not valid.
_InvalidBranchNameException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidBranchNameException =
    _ServiceError . hasCode "InvalidBranchNameException"

-- | A branch name is required but was not specified.
_BranchNameRequiredException :: AsError a => Getting (First ServiceError) a ServiceError
_BranchNameRequiredException =
    _ServiceError . hasCode "BranchNameRequiredException"

-- | The list of triggers for the repository is required but was not specified.
_RepositoryTriggersListRequiredException :: AsError a => Getting (First ServiceError) a ServiceError
_RepositoryTriggersListRequiredException =
    _ServiceError . hasCode "RepositoryTriggersListRequiredException"

-- | The encryption key is not available.
_EncryptionKeyUnavailableException :: AsError a => Getting (First ServiceError) a ServiceError
_EncryptionKeyUnavailableException =
    _ServiceError . hasCode "EncryptionKeyUnavailableException"

-- | The Amazon Resource Name (ARN) for the trigger is not valid for the specified destination. The most common reason for this error is that the ARN does not meet the requirements for the service type.
_InvalidRepositoryTriggerDestinationARNException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidRepositoryTriggerDestinationARNException =
    _ServiceError . hasCode "InvalidRepositoryTriggerDestinationArnException"

-- | A repository names object is required but was not specified.
_RepositoryNamesRequiredException :: AsError a => Getting (First ServiceError) a ServiceError
_RepositoryNamesRequiredException =
    _ServiceError . hasCode "RepositoryNamesRequiredException"

-- | The specified sort order is not valid.
_InvalidOrderException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidOrderException = _ServiceError . hasCode "InvalidOrderException"

-- | The specified branch does not exist.
_BranchDoesNotExistException :: AsError a => Getting (First ServiceError) a ServiceError
_BranchDoesNotExistException =
    _ServiceError . hasCode "BranchDoesNotExistException"

-- | A name for the trigger is required but was not specified.
_RepositoryTriggerNameRequiredException :: AsError a => Getting (First ServiceError) a ServiceError
_RepositoryTriggerNameRequiredException =
    _ServiceError . hasCode "RepositoryTriggerNameRequiredException"

-- | The specified repository does not exist.
_RepositoryDoesNotExistException :: AsError a => Getting (First ServiceError) a ServiceError
_RepositoryDoesNotExistException =
    _ServiceError . hasCode "RepositoryDoesNotExistException"

-- | The number of branches for the trigger was exceeded.
_MaximumBranchesExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_MaximumBranchesExceededException =
    _ServiceError . hasCode "MaximumBranchesExceededException"

-- | An encryption integrity check failed.
_EncryptionIntegrityChecksFailedException :: AsError a => Getting (First ServiceError) a ServiceError
_EncryptionIntegrityChecksFailedException =
    _ServiceError . hasCode "EncryptionIntegrityChecksFailedException"

-- | At least one event for the trigger is required but was not specified.
_RepositoryTriggerEventsListRequiredException :: AsError a => Getting (First ServiceError) a ServiceError
_RepositoryTriggerEventsListRequiredException =
    _ServiceError . hasCode "RepositoryTriggerEventsListRequiredException"

-- | An encryption key could not be accessed.
_EncryptionKeyAccessDeniedException :: AsError a => Getting (First ServiceError) a ServiceError
_EncryptionKeyAccessDeniedException =
    _ServiceError . hasCode "EncryptionKeyAccessDeniedException"

-- | The specified branch name already exists.
_BranchNameExistsException :: AsError a => Getting (First ServiceError) a ServiceError
_BranchNameExistsException =
    _ServiceError . hasCode "BranchNameExistsException"

-- | The specified sort by value is not valid.
_InvalidSortByException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidSortByException = _ServiceError . hasCode "InvalidSortByException"

-- | The encryption key is disabled.
_EncryptionKeyDisabledException :: AsError a => Getting (First ServiceError) a ServiceError
_EncryptionKeyDisabledException =
    _ServiceError . hasCode "EncryptionKeyDisabledException"

-- | A commit ID was not specified.
_CommitIdRequiredException :: AsError a => Getting (First ServiceError) a ServiceError
_CommitIdRequiredException =
    _ServiceError . hasCode "CommitIdRequiredException"

-- | The specified commit ID is not valid.
_InvalidCommitIdException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidCommitIdException = _ServiceError . hasCode "InvalidCommitIdException"

-- | A destination ARN for the target service for the trigger is required but was not specified.
_RepositoryTriggerDestinationARNRequiredException :: AsError a => Getting (First ServiceError) a ServiceError
_RepositoryTriggerDestinationARNRequiredException =
    _ServiceError . hasCode "RepositoryTriggerDestinationArnRequiredException"

-- | The specified commit does not exist or no commit was specified, and the specified repository has no default branch.
_CommitDoesNotExistException :: AsError a => Getting (First ServiceError) a ServiceError
_CommitDoesNotExistException =
    _ServiceError . hasCode "CommitDoesNotExistException"

-- | At least one branch name is required but was not specified in the trigger configuration.
_RepositoryTriggerBranchNameListRequiredException :: AsError a => Getting (First ServiceError) a ServiceError
_RepositoryTriggerBranchNameListRequiredException =
    _ServiceError . hasCode "RepositoryTriggerBranchNameListRequiredException"

-- | The specified commit ID does not exist.
_CommitIdDoesNotExistException :: AsError a => Getting (First ServiceError) a ServiceError
_CommitIdDoesNotExistException =
    _ServiceError . hasCode "CommitIdDoesNotExistException"

-- | The name of the trigger is not valid.
_InvalidRepositoryTriggerNameException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidRepositoryTriggerNameException =
    _ServiceError . hasCode "InvalidRepositoryTriggerNameException"

-- | A repository name is required but was not specified.
_RepositoryNameRequiredException :: AsError a => Getting (First ServiceError) a ServiceError
_RepositoryNameRequiredException =
    _ServiceError . hasCode "RepositoryNameRequiredException"

-- | A repository resource limit was exceeded.
_RepositoryLimitExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_RepositoryLimitExceededException =
    _ServiceError . hasCode "RepositoryLimitExceededException"

-- | One or more events specified for the trigger is not valid. Check to make sure that all events specified match the requirements for allowed events.
_InvalidRepositoryTriggerEventsException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidRepositoryTriggerEventsException =
    _ServiceError . hasCode "InvalidRepositoryTriggerEventsException"

-- | At least one specified repository name is not valid.
--
-- This exception only occurs when a specified repository name is not valid. Other exceptions occur when a required repository parameter is missing, or when a specified repository does not exist.
_InvalidRepositoryNameException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidRepositoryNameException =
    _ServiceError . hasCode "InvalidRepositoryNameException"
