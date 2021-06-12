{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkDocs.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkDocs.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _IllegalUserStateException,
    _StorageLimitWillExceedException,
    _CustomMetadataLimitExceededException,
    _ConflictingOperationException,
    _DeactivatingLastSystemUserException,
    _DraftUploadOutOfSyncException,
    _DocumentLockedForCommentsException,
    _EntityNotExistsException,
    _UnauthorizedOperationException,
    _InvalidOperationException,
    _StorageLimitExceededException,
    _ServiceUnavailableException,
    _UnauthorizedResourceAccessException,
    _RequestedEntityTooLargeException,
    _ProhibitedStateException,
    _EntityAlreadyExistsException,
    _ConcurrentModificationException,
    _ResourceAlreadyCheckedOutException,
    _LimitExceededException,
    _InvalidPasswordException,
    _FailedDependencyException,
    _InvalidCommentOperationException,
    _TooManySubscriptionsException,
    _TooManyLabelsException,
    _InvalidArgumentException,

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
    activity_organizationId,
    activity_originalParent,
    activity_participants,
    activity_commentMetadata,
    activity_timeStamp,
    activity_initiator,
    activity_type,
    activity_isIndirectActivity,

    -- * Comment
    Comment (..),
    newComment,
    comment_status,
    comment_createdTimestamp,
    comment_contributor,
    comment_parentId,
    comment_recipientId,
    comment_visibility,
    comment_threadId,
    comment_text,
    comment_commentId,

    -- * CommentMetadata
    CommentMetadata (..),
    newCommentMetadata,
    commentMetadata_commentStatus,
    commentMetadata_createdTimestamp,
    commentMetadata_contributor,
    commentMetadata_recipientId,
    commentMetadata_commentId,

    -- * DocumentMetadata
    DocumentMetadata (..),
    newDocumentMetadata,
    documentMetadata_modifiedTimestamp,
    documentMetadata_parentFolderId,
    documentMetadata_creatorId,
    documentMetadata_createdTimestamp,
    documentMetadata_id,
    documentMetadata_labels,
    documentMetadata_latestVersionMetadata,
    documentMetadata_resourceState,

    -- * DocumentVersionMetadata
    DocumentVersionMetadata (..),
    newDocumentVersionMetadata,
    documentVersionMetadata_modifiedTimestamp,
    documentVersionMetadata_status,
    documentVersionMetadata_creatorId,
    documentVersionMetadata_contentType,
    documentVersionMetadata_createdTimestamp,
    documentVersionMetadata_contentModifiedTimestamp,
    documentVersionMetadata_id,
    documentVersionMetadata_source,
    documentVersionMetadata_contentCreatedTimestamp,
    documentVersionMetadata_name,
    documentVersionMetadata_signature,
    documentVersionMetadata_thumbnail,
    documentVersionMetadata_size,

    -- * FolderMetadata
    FolderMetadata (..),
    newFolderMetadata,
    folderMetadata_modifiedTimestamp,
    folderMetadata_parentFolderId,
    folderMetadata_latestVersionSize,
    folderMetadata_creatorId,
    folderMetadata_createdTimestamp,
    folderMetadata_id,
    folderMetadata_labels,
    folderMetadata_name,
    folderMetadata_signature,
    folderMetadata_resourceState,
    folderMetadata_size,

    -- * GroupMetadata
    GroupMetadata (..),
    newGroupMetadata,
    groupMetadata_id,
    groupMetadata_name,

    -- * NotificationOptions
    NotificationOptions (..),
    newNotificationOptions,
    notificationOptions_sendEmail,
    notificationOptions_emailMessage,

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
    principal_id,
    principal_roles,
    principal_type,

    -- * ResourceMetadata
    ResourceMetadata (..),
    newResourceMetadata,
    resourceMetadata_originalName,
    resourceMetadata_id,
    resourceMetadata_versionId,
    resourceMetadata_name,
    resourceMetadata_parentId,
    resourceMetadata_owner,
    resourceMetadata_type,

    -- * ResourcePath
    ResourcePath (..),
    newResourcePath,
    resourcePath_components,

    -- * ResourcePathComponent
    ResourcePathComponent (..),
    newResourcePathComponent,
    resourcePathComponent_id,
    resourcePathComponent_name,

    -- * SharePrincipal
    SharePrincipal (..),
    newSharePrincipal,
    sharePrincipal_id,
    sharePrincipal_type,
    sharePrincipal_role,

    -- * ShareResult
    ShareResult (..),
    newShareResult,
    shareResult_statusMessage,
    shareResult_status,
    shareResult_inviteePrincipalId,
    shareResult_shareId,
    shareResult_principalId,
    shareResult_role,

    -- * StorageRuleType
    StorageRuleType (..),
    newStorageRuleType,
    storageRuleType_storageType,
    storageRuleType_storageAllocatedInBytes,

    -- * Subscription
    Subscription (..),
    newSubscription,
    subscription_subscriptionId,
    subscription_protocol,
    subscription_endPoint,

    -- * UploadMetadata
    UploadMetadata (..),
    newUploadMetadata,
    uploadMetadata_signedHeaders,
    uploadMetadata_uploadUrl,

    -- * User
    User (..),
    newUser,
    user_modifiedTimestamp,
    user_status,
    user_organizationId,
    user_createdTimestamp,
    user_timeZoneId,
    user_surname,
    user_locale,
    user_id,
    user_rootFolderId,
    user_givenName,
    user_recycleBinFolderId,
    user_storage,
    user_username,
    user_type,
    user_emailAddress,

    -- * UserMetadata
    UserMetadata (..),
    newUserMetadata,
    userMetadata_surname,
    userMetadata_id,
    userMetadata_givenName,
    userMetadata_username,
    userMetadata_emailAddress,

    -- * UserStorageMetadata
    UserStorageMetadata (..),
    newUserStorageMetadata,
    userStorageMetadata_storageRule,
    userStorageMetadata_storageUtilizedInBytes,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Sign.V4 as Sign
import Network.AWS.WorkDocs.Types.Activity
import Network.AWS.WorkDocs.Types.ActivityType
import Network.AWS.WorkDocs.Types.BooleanEnumType
import Network.AWS.WorkDocs.Types.Comment
import Network.AWS.WorkDocs.Types.CommentMetadata
import Network.AWS.WorkDocs.Types.CommentStatusType
import Network.AWS.WorkDocs.Types.CommentVisibilityType
import Network.AWS.WorkDocs.Types.DocumentMetadata
import Network.AWS.WorkDocs.Types.DocumentSourceType
import Network.AWS.WorkDocs.Types.DocumentStatusType
import Network.AWS.WorkDocs.Types.DocumentThumbnailType
import Network.AWS.WorkDocs.Types.DocumentVersionMetadata
import Network.AWS.WorkDocs.Types.DocumentVersionStatus
import Network.AWS.WorkDocs.Types.FolderContentType
import Network.AWS.WorkDocs.Types.FolderMetadata
import Network.AWS.WorkDocs.Types.GroupMetadata
import Network.AWS.WorkDocs.Types.LocaleType
import Network.AWS.WorkDocs.Types.NotificationOptions
import Network.AWS.WorkDocs.Types.OrderType
import Network.AWS.WorkDocs.Types.Participants
import Network.AWS.WorkDocs.Types.PermissionInfo
import Network.AWS.WorkDocs.Types.Principal
import Network.AWS.WorkDocs.Types.PrincipalType
import Network.AWS.WorkDocs.Types.ResourceCollectionType
import Network.AWS.WorkDocs.Types.ResourceMetadata
import Network.AWS.WorkDocs.Types.ResourcePath
import Network.AWS.WorkDocs.Types.ResourcePathComponent
import Network.AWS.WorkDocs.Types.ResourceSortType
import Network.AWS.WorkDocs.Types.ResourceStateType
import Network.AWS.WorkDocs.Types.ResourceType
import Network.AWS.WorkDocs.Types.RolePermissionType
import Network.AWS.WorkDocs.Types.RoleType
import Network.AWS.WorkDocs.Types.SharePrincipal
import Network.AWS.WorkDocs.Types.ShareResult
import Network.AWS.WorkDocs.Types.ShareStatusType
import Network.AWS.WorkDocs.Types.StorageRuleType
import Network.AWS.WorkDocs.Types.StorageType
import Network.AWS.WorkDocs.Types.Subscription
import Network.AWS.WorkDocs.Types.SubscriptionProtocolType
import Network.AWS.WorkDocs.Types.SubscriptionType
import Network.AWS.WorkDocs.Types.UploadMetadata
import Network.AWS.WorkDocs.Types.User
import Network.AWS.WorkDocs.Types.UserFilterType
import Network.AWS.WorkDocs.Types.UserMetadata
import Network.AWS.WorkDocs.Types.UserSortType
import Network.AWS.WorkDocs.Types.UserStatusType
import Network.AWS.WorkDocs.Types.UserStorageMetadata
import Network.AWS.WorkDocs.Types.UserType

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
      Core._serviceTimeout = Core.Just 70,
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
      | Lens.has (Core.hasStatus 504) e =
        Core.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 503) e =
        Core.Just "service_unavailable"
      | Lens.has (Core.hasStatus 502) e =
        Core.Just "bad_gateway"
      | Lens.has (Core.hasStatus 429) e =
        Core.Just "too_many_requests"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "request_throttled_exception"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "throttled_exception"
      | Lens.has (Core.hasStatus 509) e =
        Core.Just "limit_exceeded"
      | Lens.has (Core.hasStatus 500) e =
        Core.Just "general_server_error"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "throttling_exception"
      | Lens.has
          (Core.hasCode "Throttling" Core.. Core.hasStatus 400)
          e =
        Core.Just "throttling"
      | Core.otherwise = Core.Nothing

-- | The user is undergoing transfer of ownership.
_IllegalUserStateException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_IllegalUserStateException =
  Core._MatchServiceError
    defaultService
    "IllegalUserStateException"
    Core.. Core.hasStatus 409

-- | The storage limit will be exceeded.
_StorageLimitWillExceedException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_StorageLimitWillExceedException =
  Core._MatchServiceError
    defaultService
    "StorageLimitWillExceedException"
    Core.. Core.hasStatus 413

-- | The limit has been reached on the number of custom properties for the
-- specified resource.
_CustomMetadataLimitExceededException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_CustomMetadataLimitExceededException =
  Core._MatchServiceError
    defaultService
    "CustomMetadataLimitExceededException"
    Core.. Core.hasStatus 429

-- | Another operation is in progress on the resource that conflicts with the
-- current operation.
_ConflictingOperationException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ConflictingOperationException =
  Core._MatchServiceError
    defaultService
    "ConflictingOperationException"
    Core.. Core.hasStatus 409

-- | The last user in the organization is being deactivated.
_DeactivatingLastSystemUserException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DeactivatingLastSystemUserException =
  Core._MatchServiceError
    defaultService
    "DeactivatingLastSystemUserException"
    Core.. Core.hasStatus 409

-- | This exception is thrown when a valid checkout ID is not presented on
-- document version upload calls for a document that has been checked out
-- from Web client.
_DraftUploadOutOfSyncException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DraftUploadOutOfSyncException =
  Core._MatchServiceError
    defaultService
    "DraftUploadOutOfSyncException"
    Core.. Core.hasStatus 409

-- | This exception is thrown when the document is locked for comments and
-- user tries to create or delete a comment on that document.
_DocumentLockedForCommentsException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DocumentLockedForCommentsException =
  Core._MatchServiceError
    defaultService
    "DocumentLockedForCommentsException"
    Core.. Core.hasStatus 409

-- | The resource does not exist.
_EntityNotExistsException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_EntityNotExistsException =
  Core._MatchServiceError
    defaultService
    "EntityNotExistsException"
    Core.. Core.hasStatus 404

-- | The operation is not permitted.
_UnauthorizedOperationException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_UnauthorizedOperationException =
  Core._MatchServiceError
    defaultService
    "UnauthorizedOperationException"
    Core.. Core.hasStatus 403

-- | The operation is invalid.
_InvalidOperationException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidOperationException =
  Core._MatchServiceError
    defaultService
    "InvalidOperationException"
    Core.. Core.hasStatus 405

-- | The storage limit has been exceeded.
_StorageLimitExceededException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_StorageLimitExceededException =
  Core._MatchServiceError
    defaultService
    "StorageLimitExceededException"
    Core.. Core.hasStatus 409

-- | One or more of the dependencies is unavailable.
_ServiceUnavailableException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ServiceUnavailableException =
  Core._MatchServiceError
    defaultService
    "ServiceUnavailableException"
    Core.. Core.hasStatus 503

-- | The caller does not have access to perform the action on the resource.
_UnauthorizedResourceAccessException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_UnauthorizedResourceAccessException =
  Core._MatchServiceError
    defaultService
    "UnauthorizedResourceAccessException"
    Core.. Core.hasStatus 404

-- | The response is too large to return. The request must include a filter
-- to reduce the size of the response.
_RequestedEntityTooLargeException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_RequestedEntityTooLargeException =
  Core._MatchServiceError
    defaultService
    "RequestedEntityTooLargeException"
    Core.. Core.hasStatus 413

-- | The specified document version is not in the INITIALIZED state.
_ProhibitedStateException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ProhibitedStateException =
  Core._MatchServiceError
    defaultService
    "ProhibitedStateException"
    Core.. Core.hasStatus 409

-- | The resource already exists.
_EntityAlreadyExistsException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_EntityAlreadyExistsException =
  Core._MatchServiceError
    defaultService
    "EntityAlreadyExistsException"
    Core.. Core.hasStatus 409

-- | The resource hierarchy is changing.
_ConcurrentModificationException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ConcurrentModificationException =
  Core._MatchServiceError
    defaultService
    "ConcurrentModificationException"
    Core.. Core.hasStatus 409

-- | The resource is already checked out.
_ResourceAlreadyCheckedOutException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ResourceAlreadyCheckedOutException =
  Core._MatchServiceError
    defaultService
    "ResourceAlreadyCheckedOutException"
    Core.. Core.hasStatus 409

-- | The maximum of 100,000 folders under the parent folder has been
-- exceeded.
_LimitExceededException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_LimitExceededException =
  Core._MatchServiceError
    defaultService
    "LimitExceededException"
    Core.. Core.hasStatus 409

-- | The password is invalid.
_InvalidPasswordException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidPasswordException =
  Core._MatchServiceError
    defaultService
    "InvalidPasswordException"
    Core.. Core.hasStatus 401

-- | The AWS Directory Service cannot reach an on-premises instance. Or a
-- dependency under the control of the organization is failing, such as a
-- connected Active Directory.
_FailedDependencyException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_FailedDependencyException =
  Core._MatchServiceError
    defaultService
    "FailedDependencyException"
    Core.. Core.hasStatus 424

-- | The requested operation is not allowed on the specified comment object.
_InvalidCommentOperationException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidCommentOperationException =
  Core._MatchServiceError
    defaultService
    "InvalidCommentOperationException"
    Core.. Core.hasStatus 409

-- | You\'ve reached the limit on the number of subscriptions for the
-- WorkDocs instance.
_TooManySubscriptionsException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TooManySubscriptionsException =
  Core._MatchServiceError
    defaultService
    "TooManySubscriptionsException"
    Core.. Core.hasStatus 429

-- | The limit has been reached on the number of labels for the specified
-- resource.
_TooManyLabelsException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TooManyLabelsException =
  Core._MatchServiceError
    defaultService
    "TooManyLabelsException"
    Core.. Core.hasStatus 429

-- | The pagination marker or limit fields are not valid.
_InvalidArgumentException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidArgumentException =
  Core._MatchServiceError
    defaultService
    "InvalidArgumentException"
    Core.. Core.hasStatus 400
