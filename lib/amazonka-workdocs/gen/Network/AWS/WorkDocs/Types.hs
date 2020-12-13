-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkDocs.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkDocs.Types
  ( -- * Service configuration
    workDocsService,

    -- * Errors

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
    mkActivity,
    aResourceMetadata,
    aIsIndirectActivity,
    aInitiator,
    aParticipants,
    aOriginalParent,
    aType,
    aCommentMetadata,
    aTimeStamp,
    aOrganizationId,

    -- * Comment
    Comment (..),
    mkComment,
    cStatus,
    cText,
    cVisibility,
    cThreadId,
    cContributor,
    cCommentId,
    cCreatedTimestamp,
    cRecipientId,
    cParentId,

    -- * CommentMetadata
    CommentMetadata (..),
    mkCommentMetadata,
    cmCommentStatus,
    cmContributor,
    cmCommentId,
    cmCreatedTimestamp,
    cmRecipientId,

    -- * DocumentMetadata
    DocumentMetadata (..),
    mkDocumentMetadata,
    dmLatestVersionMetadata,
    dmParentFolderId,
    dmModifiedTimestamp,
    dmId,
    dmLabels,
    dmResourceState,
    dmCreatedTimestamp,
    dmCreatorId,

    -- * DocumentVersionMetadata
    DocumentVersionMetadata (..),
    mkDocumentVersionMetadata,
    dvmThumbnail,
    dvmStatus,
    dvmSignature,
    dvmContentCreatedTimestamp,
    dvmSize,
    dvmName,
    dvmModifiedTimestamp,
    dvmSource,
    dvmId,
    dvmCreatedTimestamp,
    dvmContentModifiedTimestamp,
    dvmCreatorId,
    dvmContentType,

    -- * FolderMetadata
    FolderMetadata (..),
    mkFolderMetadata,
    fmSignature,
    fmParentFolderId,
    fmSize,
    fmLatestVersionSize,
    fmName,
    fmModifiedTimestamp,
    fmId,
    fmLabels,
    fmResourceState,
    fmCreatedTimestamp,
    fmCreatorId,

    -- * GroupMetadata
    GroupMetadata (..),
    mkGroupMetadata,
    gmName,
    gmId,

    -- * NotificationOptions
    NotificationOptions (..),
    mkNotificationOptions,
    noEmailMessage,
    noSendEmail,

    -- * Participants
    Participants (..),
    mkParticipants,
    pGroups,
    pUsers,

    -- * PermissionInfo
    PermissionInfo (..),
    mkPermissionInfo,
    piRole,
    piType,

    -- * Principal
    Principal (..),
    mkPrincipal,
    pRoles,
    pId,
    pType,

    -- * ResourceMetadata
    ResourceMetadata (..),
    mkResourceMetadata,
    rmVersionId,
    rmOwner,
    rmName,
    rmId,
    rmType,
    rmOriginalName,
    rmParentId,

    -- * ResourcePath
    ResourcePath (..),
    mkResourcePath,
    rpComponents,

    -- * ResourcePathComponent
    ResourcePathComponent (..),
    mkResourcePathComponent,
    rpcName,
    rpcId,

    -- * SharePrincipal
    SharePrincipal (..),
    mkSharePrincipal,
    spRole,
    spId,
    spType,

    -- * ShareResult
    ShareResult (..),
    mkShareResult,
    srStatus,
    srPrincipalId,
    srInviteePrincipalId,
    srRole,
    srStatusMessage,
    srShareId,

    -- * StorageRuleType
    StorageRuleType (..),
    mkStorageRuleType,
    srtStorageAllocatedInBytes,
    srtStorageType,

    -- * Subscription
    Subscription (..),
    mkSubscription,
    sProtocol,
    sEndPoint,
    sSubscriptionId,

    -- * UploadMetadata
    UploadMetadata (..),
    mkUploadMetadata,
    umUploadURL,
    umSignedHeaders,

    -- * User
    User (..),
    mkUser,
    uGivenName,
    uStatus,
    uLocale,
    uUsername,
    uStorage,
    uModifiedTimestamp,
    uEmailAddress,
    uId,
    uRootFolderId,
    uType,
    uSurname,
    uTimeZoneId,
    uCreatedTimestamp,
    uOrganizationId,
    uRecycleBinFolderId,

    -- * UserMetadata
    UserMetadata (..),
    mkUserMetadata,
    umGivenName,
    umUsername,
    umEmailAddress,
    umId,
    umSurname,

    -- * UserStorageMetadata
    UserStorageMetadata (..),
    mkUserStorageMetadata,
    usmStorageUtilizedInBytes,
    usmStorageRule,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
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
workDocsService :: Lude.Service
workDocsService =
  Lude.Service
    { Lude._svcAbbrev = "WorkDocs",
      Lude._svcSigner = Sign.v4,
      Lude._svcPrefix = "workdocs",
      Lude._svcVersion = "2016-05-01",
      Lude._svcEndpoint = Lude.defaultEndpoint workDocsService,
      Lude._svcTimeout = Lude.Just 70,
      Lude._svcCheck = Lude.statusSuccess,
      Lude._svcError = Lude.parseJSONError "WorkDocs",
      Lude._svcRetry = retry
    }
  where
    retry =
      Lude.Exponential
        { Lude._retryBase = 5.0e-2,
          Lude._retryGrowth = 2,
          Lude._retryAttempts = 5,
          Lude._retryCheck = check
        }
    check e
      | Lens.has
          (Lude.hasCode "ThrottledException" Lude.. Lude.hasStatus 400)
          e =
        Lude.Just "throttled_exception"
      | Lens.has (Lude.hasStatus 429) e = Lude.Just "too_many_requests"
      | Lens.has
          (Lude.hasCode "ThrottlingException" Lude.. Lude.hasStatus 400)
          e =
        Lude.Just "throttling_exception"
      | Lens.has (Lude.hasCode "Throttling" Lude.. Lude.hasStatus 400) e =
        Lude.Just "throttling"
      | Lens.has
          ( Lude.hasCode "ProvisionedThroughputExceededException"
              Lude.. Lude.hasStatus 400
          )
          e =
        Lude.Just "throughput_exceeded"
      | Lens.has (Lude.hasStatus 504) e = Lude.Just "gateway_timeout"
      | Lens.has
          ( Lude.hasCode "RequestThrottledException"
              Lude.. Lude.hasStatus 400
          )
          e =
        Lude.Just "request_throttled_exception"
      | Lens.has (Lude.hasStatus 502) e = Lude.Just "bad_gateway"
      | Lens.has (Lude.hasStatus 503) e = Lude.Just "service_unavailable"
      | Lens.has (Lude.hasStatus 500) e =
        Lude.Just "general_server_error"
      | Lens.has (Lude.hasStatus 509) e = Lude.Just "limit_exceeded"
      | Lude.otherwise = Lude.Nothing
