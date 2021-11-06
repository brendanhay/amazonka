{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.WorkDocs.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WorkDocs.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _CustomMetadataLimitExceededException,
    _EntityAlreadyExistsException,
    _ResourceAlreadyCheckedOutException,
    _ProhibitedStateException,
    _TooManyLabelsException,
    _InvalidArgumentException,
    _UnauthorizedResourceAccessException,
    _TooManySubscriptionsException,
    _FailedDependencyException,
    _DocumentLockedForCommentsException,
    _EntityNotExistsException,
    _ConflictingOperationException,
    _DeactivatingLastSystemUserException,
    _IllegalUserStateException,
    _StorageLimitWillExceedException,
    _ConcurrentModificationException,
    _RequestedEntityTooLargeException,
    _StorageLimitExceededException,
    _ServiceUnavailableException,
    _InvalidCommentOperationException,
    _InvalidOperationException,
    _UnauthorizedOperationException,
    _DraftUploadOutOfSyncException,
    _LimitExceededException,
    _InvalidPasswordException,

    -- * ActivityType
    ActivityType (..),

    -- * BooleanEnumType
    BooleanEnumType (..),

    -- * CommentStatusType
    CommentStatusType (..),

    -- * CommentVisibilityType
    CommentVisibilityType (..),

    -- * DocumentSourceType
    DocumentSourceType (..),

    -- * DocumentStatusType
    DocumentStatusType (..),

    -- * DocumentThumbnailType
    DocumentThumbnailType (..),

    -- * DocumentVersionStatus
    DocumentVersionStatus (..),

    -- * FolderContentType
    FolderContentType (..),

    -- * LocaleType
    LocaleType (..),

    -- * OrderType
    OrderType (..),

    -- * PrincipalType
    PrincipalType (..),

    -- * ResourceCollectionType
    ResourceCollectionType (..),

    -- * ResourceSortType
    ResourceSortType (..),

    -- * ResourceStateType
    ResourceStateType (..),

    -- * ResourceType
    ResourceType (..),

    -- * RolePermissionType
    RolePermissionType (..),

    -- * RoleType
    RoleType (..),

    -- * ShareStatusType
    ShareStatusType (..),

    -- * StorageType
    StorageType (..),

    -- * SubscriptionProtocolType
    SubscriptionProtocolType (..),

    -- * SubscriptionType
    SubscriptionType (..),

    -- * UserFilterType
    UserFilterType (..),

    -- * UserSortType
    UserSortType (..),

    -- * UserStatusType
    UserStatusType (..),

    -- * UserType
    UserType (..),

    -- * Activity
    Activity (..),
    newActivity,
    activity_resourceMetadata,
    activity_isIndirectActivity,
    activity_initiator,
    activity_participants,
    activity_originalParent,
    activity_type,
    activity_commentMetadata,
    activity_timeStamp,
    activity_organizationId,

    -- * Comment
    Comment (..),
    newComment,
    comment_status,
    comment_text,
    comment_visibility,
    comment_threadId,
    comment_contributor,
    comment_createdTimestamp,
    comment_recipientId,
    comment_parentId,
    comment_commentId,

    -- * CommentMetadata
    CommentMetadata (..),
    newCommentMetadata,
    commentMetadata_commentStatus,
    commentMetadata_contributor,
    commentMetadata_commentId,
    commentMetadata_createdTimestamp,
    commentMetadata_recipientId,

    -- * DocumentMetadata
    DocumentMetadata (..),
    newDocumentMetadata,
    documentMetadata_latestVersionMetadata,
    documentMetadata_parentFolderId,
    documentMetadata_modifiedTimestamp,
    documentMetadata_id,
    documentMetadata_labels,
    documentMetadata_resourceState,
    documentMetadata_createdTimestamp,
    documentMetadata_creatorId,

    -- * DocumentVersionMetadata
    DocumentVersionMetadata (..),
    newDocumentVersionMetadata,
    documentVersionMetadata_thumbnail,
    documentVersionMetadata_status,
    documentVersionMetadata_signature,
    documentVersionMetadata_contentCreatedTimestamp,
    documentVersionMetadata_size,
    documentVersionMetadata_name,
    documentVersionMetadata_modifiedTimestamp,
    documentVersionMetadata_source,
    documentVersionMetadata_id,
    documentVersionMetadata_createdTimestamp,
    documentVersionMetadata_contentModifiedTimestamp,
    documentVersionMetadata_creatorId,
    documentVersionMetadata_contentType,

    -- * FolderMetadata
    FolderMetadata (..),
    newFolderMetadata,
    folderMetadata_signature,
    folderMetadata_parentFolderId,
    folderMetadata_size,
    folderMetadata_latestVersionSize,
    folderMetadata_name,
    folderMetadata_modifiedTimestamp,
    folderMetadata_id,
    folderMetadata_labels,
    folderMetadata_resourceState,
    folderMetadata_createdTimestamp,
    folderMetadata_creatorId,

    -- * GroupMetadata
    GroupMetadata (..),
    newGroupMetadata,
    groupMetadata_name,
    groupMetadata_id,

    -- * NotificationOptions
    NotificationOptions (..),
    newNotificationOptions,
    notificationOptions_emailMessage,
    notificationOptions_sendEmail,

    -- * Participants
    Participants (..),
    newParticipants,
    participants_groups,
    participants_users,

    -- * PermissionInfo
    PermissionInfo (..),
    newPermissionInfo,
    permissionInfo_role,
    permissionInfo_type,

    -- * Principal
    Principal (..),
    newPrincipal,
    principal_roles,
    principal_id,
    principal_type,

    -- * ResourceMetadata
    ResourceMetadata (..),
    newResourceMetadata,
    resourceMetadata_versionId,
    resourceMetadata_owner,
    resourceMetadata_name,
    resourceMetadata_id,
    resourceMetadata_type,
    resourceMetadata_originalName,
    resourceMetadata_parentId,

    -- * ResourcePath
    ResourcePath (..),
    newResourcePath,
    resourcePath_components,

    -- * ResourcePathComponent
    ResourcePathComponent (..),
    newResourcePathComponent,
    resourcePathComponent_name,
    resourcePathComponent_id,

    -- * SharePrincipal
    SharePrincipal (..),
    newSharePrincipal,
    sharePrincipal_id,
    sharePrincipal_type,
    sharePrincipal_role,

    -- * ShareResult
    ShareResult (..),
    newShareResult,
    shareResult_status,
    shareResult_principalId,
    shareResult_inviteePrincipalId,
    shareResult_role,
    shareResult_statusMessage,
    shareResult_shareId,

    -- * StorageRuleType
    StorageRuleType (..),
    newStorageRuleType,
    storageRuleType_storageAllocatedInBytes,
    storageRuleType_storageType,

    -- * Subscription
    Subscription (..),
    newSubscription,
    subscription_protocol,
    subscription_endPoint,
    subscription_subscriptionId,

    -- * UploadMetadata
    UploadMetadata (..),
    newUploadMetadata,
    uploadMetadata_uploadUrl,
    uploadMetadata_signedHeaders,

    -- * User
    User (..),
    newUser,
    user_givenName,
    user_status,
    user_locale,
    user_username,
    user_storage,
    user_modifiedTimestamp,
    user_emailAddress,
    user_id,
    user_rootFolderId,
    user_type,
    user_surname,
    user_timeZoneId,
    user_createdTimestamp,
    user_organizationId,
    user_recycleBinFolderId,

    -- * UserMetadata
    UserMetadata (..),
    newUserMetadata,
    userMetadata_givenName,
    userMetadata_username,
    userMetadata_emailAddress,
    userMetadata_id,
    userMetadata_surname,

    -- * UserStorageMetadata
    UserStorageMetadata (..),
    newUserStorageMetadata,
    userStorageMetadata_storageUtilizedInBytes,
    userStorageMetadata_storageRule,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign
import Amazonka.WorkDocs.Types.Activity
import Amazonka.WorkDocs.Types.ActivityType
import Amazonka.WorkDocs.Types.BooleanEnumType
import Amazonka.WorkDocs.Types.Comment
import Amazonka.WorkDocs.Types.CommentMetadata
import Amazonka.WorkDocs.Types.CommentStatusType
import Amazonka.WorkDocs.Types.CommentVisibilityType
import Amazonka.WorkDocs.Types.DocumentMetadata
import Amazonka.WorkDocs.Types.DocumentSourceType
import Amazonka.WorkDocs.Types.DocumentStatusType
import Amazonka.WorkDocs.Types.DocumentThumbnailType
import Amazonka.WorkDocs.Types.DocumentVersionMetadata
import Amazonka.WorkDocs.Types.DocumentVersionStatus
import Amazonka.WorkDocs.Types.FolderContentType
import Amazonka.WorkDocs.Types.FolderMetadata
import Amazonka.WorkDocs.Types.GroupMetadata
import Amazonka.WorkDocs.Types.LocaleType
import Amazonka.WorkDocs.Types.NotificationOptions
import Amazonka.WorkDocs.Types.OrderType
import Amazonka.WorkDocs.Types.Participants
import Amazonka.WorkDocs.Types.PermissionInfo
import Amazonka.WorkDocs.Types.Principal
import Amazonka.WorkDocs.Types.PrincipalType
import Amazonka.WorkDocs.Types.ResourceCollectionType
import Amazonka.WorkDocs.Types.ResourceMetadata
import Amazonka.WorkDocs.Types.ResourcePath
import Amazonka.WorkDocs.Types.ResourcePathComponent
import Amazonka.WorkDocs.Types.ResourceSortType
import Amazonka.WorkDocs.Types.ResourceStateType
import Amazonka.WorkDocs.Types.ResourceType
import Amazonka.WorkDocs.Types.RolePermissionType
import Amazonka.WorkDocs.Types.RoleType
import Amazonka.WorkDocs.Types.SharePrincipal
import Amazonka.WorkDocs.Types.ShareResult
import Amazonka.WorkDocs.Types.ShareStatusType
import Amazonka.WorkDocs.Types.StorageRuleType
import Amazonka.WorkDocs.Types.StorageType
import Amazonka.WorkDocs.Types.Subscription
import Amazonka.WorkDocs.Types.SubscriptionProtocolType
import Amazonka.WorkDocs.Types.SubscriptionType
import Amazonka.WorkDocs.Types.UploadMetadata
import Amazonka.WorkDocs.Types.User
import Amazonka.WorkDocs.Types.UserFilterType
import Amazonka.WorkDocs.Types.UserMetadata
import Amazonka.WorkDocs.Types.UserSortType
import Amazonka.WorkDocs.Types.UserStatusType
import Amazonka.WorkDocs.Types.UserStorageMetadata
import Amazonka.WorkDocs.Types.UserType

-- | API version @2016-05-01@ of the Amazon WorkDocs SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core._serviceAbbrev = "WorkDocs",
      Core._serviceSigner = Sign.v4,
      Core._serviceEndpointPrefix = "workdocs",
      Core._serviceSigningName = "workdocs",
      Core._serviceVersion = "2016-05-01",
      Core._serviceEndpoint =
        Core.defaultEndpoint defaultService,
      Core._serviceTimeout = Prelude.Just 70,
      Core._serviceCheck = Core.statusSuccess,
      Core._serviceError = Core.parseJSONError "WorkDocs",
      Core._serviceRetry = retry
    }
  where
    retry =
      Core.Exponential
        { Core._retryBase = 5.0e-2,
          Core._retryGrowth = 2,
          Core._retryAttempts = 5,
          Core._retryCheck = check
        }
    check e
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Prelude.otherwise = Prelude.Nothing

-- | The limit has been reached on the number of custom properties for the
-- specified resource.
_CustomMetadataLimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_CustomMetadataLimitExceededException =
  Core._MatchServiceError
    defaultService
    "CustomMetadataLimitExceededException"
    Prelude.. Core.hasStatus 429

-- | The resource already exists.
_EntityAlreadyExistsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_EntityAlreadyExistsException =
  Core._MatchServiceError
    defaultService
    "EntityAlreadyExistsException"
    Prelude.. Core.hasStatus 409

-- | The resource is already checked out.
_ResourceAlreadyCheckedOutException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceAlreadyCheckedOutException =
  Core._MatchServiceError
    defaultService
    "ResourceAlreadyCheckedOutException"
    Prelude.. Core.hasStatus 409

-- | The specified document version is not in the INITIALIZED state.
_ProhibitedStateException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ProhibitedStateException =
  Core._MatchServiceError
    defaultService
    "ProhibitedStateException"
    Prelude.. Core.hasStatus 409

-- | The limit has been reached on the number of labels for the specified
-- resource.
_TooManyLabelsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TooManyLabelsException =
  Core._MatchServiceError
    defaultService
    "TooManyLabelsException"
    Prelude.. Core.hasStatus 429

-- | The pagination marker or limit fields are not valid.
_InvalidArgumentException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidArgumentException =
  Core._MatchServiceError
    defaultService
    "InvalidArgumentException"
    Prelude.. Core.hasStatus 400

-- | The caller does not have access to perform the action on the resource.
_UnauthorizedResourceAccessException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_UnauthorizedResourceAccessException =
  Core._MatchServiceError
    defaultService
    "UnauthorizedResourceAccessException"
    Prelude.. Core.hasStatus 404

-- | You\'ve reached the limit on the number of subscriptions for the
-- WorkDocs instance.
_TooManySubscriptionsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TooManySubscriptionsException =
  Core._MatchServiceError
    defaultService
    "TooManySubscriptionsException"
    Prelude.. Core.hasStatus 429

-- | The AWS Directory Service cannot reach an on-premises instance. Or a
-- dependency under the control of the organization is failing, such as a
-- connected Active Directory.
_FailedDependencyException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_FailedDependencyException =
  Core._MatchServiceError
    defaultService
    "FailedDependencyException"
    Prelude.. Core.hasStatus 424

-- | This exception is thrown when the document is locked for comments and
-- user tries to create or delete a comment on that document.
_DocumentLockedForCommentsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DocumentLockedForCommentsException =
  Core._MatchServiceError
    defaultService
    "DocumentLockedForCommentsException"
    Prelude.. Core.hasStatus 409

-- | The resource does not exist.
_EntityNotExistsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_EntityNotExistsException =
  Core._MatchServiceError
    defaultService
    "EntityNotExistsException"
    Prelude.. Core.hasStatus 404

-- | Another operation is in progress on the resource that conflicts with the
-- current operation.
_ConflictingOperationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ConflictingOperationException =
  Core._MatchServiceError
    defaultService
    "ConflictingOperationException"
    Prelude.. Core.hasStatus 409

-- | The last user in the organization is being deactivated.
_DeactivatingLastSystemUserException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DeactivatingLastSystemUserException =
  Core._MatchServiceError
    defaultService
    "DeactivatingLastSystemUserException"
    Prelude.. Core.hasStatus 409

-- | The user is undergoing transfer of ownership.
_IllegalUserStateException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_IllegalUserStateException =
  Core._MatchServiceError
    defaultService
    "IllegalUserStateException"
    Prelude.. Core.hasStatus 409

-- | The storage limit will be exceeded.
_StorageLimitWillExceedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_StorageLimitWillExceedException =
  Core._MatchServiceError
    defaultService
    "StorageLimitWillExceedException"
    Prelude.. Core.hasStatus 413

-- | The resource hierarchy is changing.
_ConcurrentModificationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ConcurrentModificationException =
  Core._MatchServiceError
    defaultService
    "ConcurrentModificationException"
    Prelude.. Core.hasStatus 409

-- | The response is too large to return. The request must include a filter
-- to reduce the size of the response.
_RequestedEntityTooLargeException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_RequestedEntityTooLargeException =
  Core._MatchServiceError
    defaultService
    "RequestedEntityTooLargeException"
    Prelude.. Core.hasStatus 413

-- | The storage limit has been exceeded.
_StorageLimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_StorageLimitExceededException =
  Core._MatchServiceError
    defaultService
    "StorageLimitExceededException"
    Prelude.. Core.hasStatus 409

-- | One or more of the dependencies is unavailable.
_ServiceUnavailableException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ServiceUnavailableException =
  Core._MatchServiceError
    defaultService
    "ServiceUnavailableException"
    Prelude.. Core.hasStatus 503

-- | The requested operation is not allowed on the specified comment object.
_InvalidCommentOperationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidCommentOperationException =
  Core._MatchServiceError
    defaultService
    "InvalidCommentOperationException"
    Prelude.. Core.hasStatus 409

-- | The operation is invalid.
_InvalidOperationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidOperationException =
  Core._MatchServiceError
    defaultService
    "InvalidOperationException"
    Prelude.. Core.hasStatus 405

-- | The operation is not permitted.
_UnauthorizedOperationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_UnauthorizedOperationException =
  Core._MatchServiceError
    defaultService
    "UnauthorizedOperationException"
    Prelude.. Core.hasStatus 403

-- | This exception is thrown when a valid checkout ID is not presented on
-- document version upload calls for a document that has been checked out
-- from Web client.
_DraftUploadOutOfSyncException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DraftUploadOutOfSyncException =
  Core._MatchServiceError
    defaultService
    "DraftUploadOutOfSyncException"
    Prelude.. Core.hasStatus 409

-- | The maximum of 100,000 folders under the parent folder has been
-- exceeded.
_LimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_LimitExceededException =
  Core._MatchServiceError
    defaultService
    "LimitExceededException"
    Prelude.. Core.hasStatus 409

-- | The password is invalid.
_InvalidPasswordException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidPasswordException =
  Core._MatchServiceError
    defaultService
    "InvalidPasswordException"
    Prelude.. Core.hasStatus 401
