{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.Types
-- Copyright   : (c) 2013-2015 Brendan Hay
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

import           Network.AWS.CodeCommit.Types.Product
import           Network.AWS.CodeCommit.Types.Sum
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
    , _svcError = parseJSONError
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
      | has (hasCode "ThrottlingException" . hasStatus 400) e =
          Just "throttling_exception"
      | has (hasCode "Throttling" . hasStatus 400) e = Just "throttling"
      | has (hasStatus 503) e = Just "service_unavailable"
      | has (hasStatus 500) e = Just "general_server_error"
      | has (hasStatus 509) e = Just "limit_exceeded"
      | otherwise = Nothing

-- | The specified continuation token is not valid.
_InvalidContinuationTokenException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidContinuationTokenException =
    _ServiceError . hasCode "InvalidContinuationTokenException"

-- | No encryption key was found.
_EncryptionKeyNotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_EncryptionKeyNotFoundException =
    _ServiceError . hasCode "EncryptionKeyNotFoundException"

-- | The specified repository name already exists.
_RepositoryNameExistsException :: AsError a => Getting (First ServiceError) a ServiceError
_RepositoryNameExistsException =
    _ServiceError . hasCode "RepositoryNameExistsException"

-- | The maximum number of allowed repository names was exceeded. Currently,
-- this number is 25.
_MaximumRepositoryNamesExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_MaximumRepositoryNamesExceededException =
    _ServiceError . hasCode "MaximumRepositoryNamesExceededException"

-- | The specified repository description is not valid.
_InvalidRepositoryDescriptionException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidRepositoryDescriptionException =
    _ServiceError . hasCode "InvalidRepositoryDescriptionException"

-- | A branch name is required but was not specified.
_BranchNameRequiredException :: AsError a => Getting (First ServiceError) a ServiceError
_BranchNameRequiredException =
    _ServiceError . hasCode "BranchNameRequiredException"

-- | The specified branch name is not valid.
_InvalidBranchNameException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidBranchNameException =
    _ServiceError . hasCode "InvalidBranchNameException"

-- | The encryption key is not available.
_EncryptionKeyUnavailableException :: AsError a => Getting (First ServiceError) a ServiceError
_EncryptionKeyUnavailableException =
    _ServiceError . hasCode "EncryptionKeyUnavailableException"

-- | The specified sort order is not valid.
_InvalidOrderException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidOrderException = _ServiceError . hasCode "InvalidOrderException"

-- | The specified branch does not exist.
_BranchDoesNotExistException :: AsError a => Getting (First ServiceError) a ServiceError
_BranchDoesNotExistException =
    _ServiceError . hasCode "BranchDoesNotExistException"

-- | A repository names object is required but was not specified.
_RepositoryNamesRequiredException :: AsError a => Getting (First ServiceError) a ServiceError
_RepositoryNamesRequiredException =
    _ServiceError . hasCode "RepositoryNamesRequiredException"

-- | The specified repository does not exist.
_RepositoryDoesNotExistException :: AsError a => Getting (First ServiceError) a ServiceError
_RepositoryDoesNotExistException =
    _ServiceError . hasCode "RepositoryDoesNotExistException"

-- | An encryption integrity check failed.
_EncryptionIntegrityChecksFailedException :: AsError a => Getting (First ServiceError) a ServiceError
_EncryptionIntegrityChecksFailedException =
    _ServiceError . hasCode "EncryptionIntegrityChecksFailedException"

-- | An encryption key could not be accessed.
_EncryptionKeyAccessDeniedException :: AsError a => Getting (First ServiceError) a ServiceError
_EncryptionKeyAccessDeniedException =
    _ServiceError . hasCode "EncryptionKeyAccessDeniedException"

-- | The specified branch name already exists.
_BranchNameExistsException :: AsError a => Getting (First ServiceError) a ServiceError
_BranchNameExistsException =
    _ServiceError . hasCode "BranchNameExistsException"

-- | The encryption key is disabled.
_EncryptionKeyDisabledException :: AsError a => Getting (First ServiceError) a ServiceError
_EncryptionKeyDisabledException =
    _ServiceError . hasCode "EncryptionKeyDisabledException"

-- | The specified sort by value is not valid.
_InvalidSortByException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidSortByException = _ServiceError . hasCode "InvalidSortByException"

-- | A commit ID was not specified.
_CommitIdRequiredException :: AsError a => Getting (First ServiceError) a ServiceError
_CommitIdRequiredException =
    _ServiceError . hasCode "CommitIdRequiredException"

-- | The specified commit ID is not valid.
_InvalidCommitIdException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidCommitIdException = _ServiceError . hasCode "InvalidCommitIdException"

-- | The specified commit does not exist or no commit was specified, and the
-- specified repository has no default branch.
_CommitDoesNotExistException :: AsError a => Getting (First ServiceError) a ServiceError
_CommitDoesNotExistException =
    _ServiceError . hasCode "CommitDoesNotExistException"

-- | A repository resource limit was exceeded.
_RepositoryLimitExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_RepositoryLimitExceededException =
    _ServiceError . hasCode "RepositoryLimitExceededException"

-- | At least one specified repository name is not valid.
--
-- This exception only occurs when a specified repository name is not
-- valid. Other exceptions occur when a required repository parameter is
-- missing, or when a specified repository does not exist.
_InvalidRepositoryNameException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidRepositoryNameException =
    _ServiceError . hasCode "InvalidRepositoryNameException"

-- | A repository name is required but was not specified.
_RepositoryNameRequiredException :: AsError a => Getting (First ServiceError) a ServiceError
_RepositoryNameRequiredException =
    _ServiceError . hasCode "RepositoryNameRequiredException"
