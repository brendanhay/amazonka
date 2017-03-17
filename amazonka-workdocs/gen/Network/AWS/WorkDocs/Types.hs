{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkDocs.Types
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.WorkDocs.Types
    (
    -- * Service Configuration
      workDocs

    -- * Errors
    , _EntityAlreadyExistsException
    , _ProhibitedStateException
    , _InvalidArgumentException
    , _UnauthorizedResourceAccessException
    , _TooManySubscriptionsException
    , _FailedDependencyException
    , _EntityNotExistsException
    , _DeactivatingLastSystemUserException
    , _IllegalUserStateException
    , _StorageLimitWillExceedException
    , _ConcurrentModificationException
    , _StorageLimitExceededException
    , _ServiceUnavailableException
    , _InvalidOperationException
    , _UnauthorizedOperationException
    , _LimitExceededException

    -- * DocumentSourceType
    , DocumentSourceType (..)

    -- * DocumentStatusType
    , DocumentStatusType (..)

    -- * DocumentThumbnailType
    , DocumentThumbnailType (..)

    -- * DocumentVersionStatus
    , DocumentVersionStatus (..)

    -- * FolderContentType
    , FolderContentType (..)

    -- * LocaleType
    , LocaleType (..)

    -- * OrderType
    , OrderType (..)

    -- * PrincipalType
    , PrincipalType (..)

    -- * ResourceSortType
    , ResourceSortType (..)

    -- * ResourceStateType
    , ResourceStateType (..)

    -- * RolePermissionType
    , RolePermissionType (..)

    -- * RoleType
    , RoleType (..)

    -- * ShareStatusType
    , ShareStatusType (..)

    -- * StorageType
    , StorageType (..)

    -- * SubscriptionProtocolType
    , SubscriptionProtocolType (..)

    -- * SubscriptionType
    , SubscriptionType (..)

    -- * UserFilterType
    , UserFilterType (..)

    -- * UserSortType
    , UserSortType (..)

    -- * UserStatusType
    , UserStatusType (..)

    -- * UserType
    , UserType (..)

    -- * DocumentMetadata
    , DocumentMetadata
    , documentMetadata
    , dmLatestVersionMetadata
    , dmParentFolderId
    , dmModifiedTimestamp
    , dmId
    , dmResourceState
    , dmCreatedTimestamp
    , dmCreatorId

    -- * DocumentVersionMetadata
    , DocumentVersionMetadata
    , documentVersionMetadata
    , dvmThumbnail
    , dvmStatus
    , dvmSignature
    , dvmContentCreatedTimestamp
    , dvmSize
    , dvmName
    , dvmModifiedTimestamp
    , dvmSource
    , dvmId
    , dvmCreatedTimestamp
    , dvmContentModifiedTimestamp
    , dvmCreatorId
    , dvmContentType

    -- * FolderMetadata
    , FolderMetadata
    , folderMetadata
    , fmSignature
    , fmParentFolderId
    , fmName
    , fmModifiedTimestamp
    , fmId
    , fmResourceState
    , fmCreatedTimestamp
    , fmCreatorId

    -- * PermissionInfo
    , PermissionInfo
    , permissionInfo
    , piRole
    , piType

    -- * Principal
    , Principal
    , principal
    , pRoles
    , pId
    , pType

    -- * ResourcePath
    , ResourcePath
    , resourcePath
    , rpComponents

    -- * ResourcePathComponent
    , ResourcePathComponent
    , resourcePathComponent
    , rpcName
    , rpcId

    -- * SharePrincipal
    , SharePrincipal
    , sharePrincipal
    , spId
    , spType
    , spRole

    -- * ShareResult
    , ShareResult
    , shareResult
    , srStatus
    , srPrincipalId
    , srRole
    , srStatusMessage
    , srShareId

    -- * StorageRuleType
    , StorageRuleType
    , storageRuleType
    , srtStorageAllocatedInBytes
    , srtStorageType

    -- * Subscription
    , Subscription
    , subscription
    , sProtocol
    , sEndPoint
    , sSubscriptionId

    -- * UploadMetadata
    , UploadMetadata
    , uploadMetadata
    , umUploadURL
    , umSignedHeaders

    -- * User
    , User
    , user
    , uGivenName
    , uStatus
    , uLocale
    , uUsername
    , uStorage
    , uModifiedTimestamp
    , uEmailAddress
    , uId
    , uRootFolderId
    , uType
    , uSurname
    , uTimeZoneId
    , uCreatedTimestamp
    , uOrganizationId
    , uRecycleBinFolderId

    -- * UserStorageMetadata
    , UserStorageMetadata
    , userStorageMetadata
    , usmStorageUtilizedInBytes
    , usmStorageRule
    ) where

import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Sign.V4
import           Network.AWS.WorkDocs.Types.Product
import           Network.AWS.WorkDocs.Types.Sum

-- | API version @2016-05-01@ of the Amazon WorkDocs SDK configuration.
workDocs :: Service
workDocs =
    Service
    { _svcAbbrev = "WorkDocs"
    , _svcSigner = v4
    , _svcPrefix = "workdocs"
    , _svcVersion = "2016-05-01"
    , _svcEndpoint = defaultEndpoint workDocs
    , _svcTimeout = Just 70
    , _svcCheck = statusSuccess
    , _svcError = parseJSONError "WorkDocs"
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

-- | The resource already exists.
--
--
_EntityAlreadyExistsException :: AsError a => Getting (First ServiceError) a ServiceError
_EntityAlreadyExistsException =
    _MatchServiceError workDocs "EntityAlreadyExistsException" . hasStatus 409

-- | The specified document version is not in the INITIALIZED state.
--
--
_ProhibitedStateException :: AsError a => Getting (First ServiceError) a ServiceError
_ProhibitedStateException =
    _MatchServiceError workDocs "ProhibitedStateException" . hasStatus 409

-- | The pagination marker and/or limit fields are not valid.
--
--
_InvalidArgumentException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidArgumentException =
    _MatchServiceError workDocs "InvalidArgumentException" . hasStatus 400

-- | The caller does not have access to perform the action on the resource.
--
--
_UnauthorizedResourceAccessException :: AsError a => Getting (First ServiceError) a ServiceError
_UnauthorizedResourceAccessException =
    _MatchServiceError workDocs "UnauthorizedResourceAccessException" .
    hasStatus 404

-- | You've reached the limit on the number of subscriptions for the WorkDocs instance.
--
--
_TooManySubscriptionsException :: AsError a => Getting (First ServiceError) a ServiceError
_TooManySubscriptionsException =
    _MatchServiceError workDocs "TooManySubscriptionsException" . hasStatus 429

-- | The AWS Directory Service cannot reach an on-premises instance. Or a dependency under the control of the organization is failing, such as a connected active directory.
--
--
_FailedDependencyException :: AsError a => Getting (First ServiceError) a ServiceError
_FailedDependencyException =
    _MatchServiceError workDocs "FailedDependencyException" . hasStatus 424

-- | The resource does not exist.
--
--
_EntityNotExistsException :: AsError a => Getting (First ServiceError) a ServiceError
_EntityNotExistsException =
    _MatchServiceError workDocs "EntityNotExistsException" . hasStatus 404

-- | The last user in the organization is being deactivated.
--
--
_DeactivatingLastSystemUserException :: AsError a => Getting (First ServiceError) a ServiceError
_DeactivatingLastSystemUserException =
    _MatchServiceError workDocs "DeactivatingLastSystemUserException" .
    hasStatus 409

-- | The user is undergoing transfer of ownership.
--
--
_IllegalUserStateException :: AsError a => Getting (First ServiceError) a ServiceError
_IllegalUserStateException =
    _MatchServiceError workDocs "IllegalUserStateException" . hasStatus 409

-- | The storage limit will be exceeded.
--
--
_StorageLimitWillExceedException :: AsError a => Getting (First ServiceError) a ServiceError
_StorageLimitWillExceedException =
    _MatchServiceError workDocs "StorageLimitWillExceedException" .
    hasStatus 413

-- | The resource hierarchy is changing.
--
--
_ConcurrentModificationException :: AsError a => Getting (First ServiceError) a ServiceError
_ConcurrentModificationException =
    _MatchServiceError workDocs "ConcurrentModificationException" .
    hasStatus 409

-- | The storage limit has been exceeded.
--
--
_StorageLimitExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_StorageLimitExceededException =
    _MatchServiceError workDocs "StorageLimitExceededException" . hasStatus 409

-- | One or more of the dependencies is unavailable.
--
--
_ServiceUnavailableException :: AsError a => Getting (First ServiceError) a ServiceError
_ServiceUnavailableException =
    _MatchServiceError workDocs "ServiceUnavailableException" . hasStatus 503

-- | The operation is invalid.
--
--
_InvalidOperationException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidOperationException =
    _MatchServiceError workDocs "InvalidOperationException" . hasStatus 405

-- | The operation is not permitted.
--
--
_UnauthorizedOperationException :: AsError a => Getting (First ServiceError) a ServiceError
_UnauthorizedOperationException =
    _MatchServiceError workDocs "UnauthorizedOperationException" .
    hasStatus 403

-- | You've exceeded the maximum of 100,000 folders under the parent folder.
--
--
_LimitExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_LimitExceededException =
    _MatchServiceError workDocs "LimitExceededException" . hasStatus 409
