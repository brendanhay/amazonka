{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkDocs.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkDocs.Types
  ( -- * Service Configuration
    workDocs,

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
    Activity,
    activity,
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
    Comment,
    comment,
    cStatus,
    cText,
    cVisibility,
    cThreadId,
    cContributor,
    cCreatedTimestamp,
    cRecipientId,
    cParentId,
    cCommentId,

    -- * CommentMetadata
    CommentMetadata,
    commentMetadata,
    cmCommentStatus,
    cmContributor,
    cmCommentId,
    cmCreatedTimestamp,
    cmRecipientId,

    -- * DocumentMetadata
    DocumentMetadata,
    documentMetadata,
    dmLatestVersionMetadata,
    dmParentFolderId,
    dmModifiedTimestamp,
    dmId,
    dmLabels,
    dmResourceState,
    dmCreatedTimestamp,
    dmCreatorId,

    -- * DocumentVersionMetadata
    DocumentVersionMetadata,
    documentVersionMetadata,
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
    FolderMetadata,
    folderMetadata,
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
    GroupMetadata,
    groupMetadata,
    gmName,
    gmId,

    -- * NotificationOptions
    NotificationOptions,
    notificationOptions,
    noEmailMessage,
    noSendEmail,

    -- * Participants
    Participants,
    participants,
    pGroups,
    pUsers,

    -- * PermissionInfo
    PermissionInfo,
    permissionInfo,
    piRole,
    piType,

    -- * Principal
    Principal,
    principal,
    pRoles,
    pId,
    pType,

    -- * ResourceMetadata
    ResourceMetadata,
    resourceMetadata,
    rmVersionId,
    rmOwner,
    rmName,
    rmId,
    rmType,
    rmOriginalName,
    rmParentId,

    -- * ResourcePath
    ResourcePath,
    resourcePath,
    rpComponents,

    -- * ResourcePathComponent
    ResourcePathComponent,
    resourcePathComponent,
    rpcName,
    rpcId,

    -- * SharePrincipal
    SharePrincipal,
    sharePrincipal,
    spId,
    spType,
    spRole,

    -- * ShareResult
    ShareResult,
    shareResult,
    srStatus,
    srPrincipalId,
    srInviteePrincipalId,
    srRole,
    srStatusMessage,
    srShareId,

    -- * StorageRuleType
    StorageRuleType,
    storageRuleType,
    srtStorageAllocatedInBytes,
    srtStorageType,

    -- * Subscription
    Subscription,
    subscription,
    sProtocol,
    sEndPoint,
    sSubscriptionId,

    -- * UploadMetadata
    UploadMetadata,
    uploadMetadata,
    umUploadURL,
    umSignedHeaders,

    -- * User
    User,
    user,
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
    UserMetadata,
    userMetadata,
    umGivenName,
    umUsername,
    umEmailAddress,
    umId,
    umSurname,

    -- * UserStorageMetadata
    UserStorageMetadata,
    userStorageMetadata,
    usmStorageUtilizedInBytes,
    usmStorageRule,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Sign.V4
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
workDocs :: Service
workDocs =
  Service
    { _svcAbbrev = "WorkDocs",
      _svcSigner = v4,
      _svcPrefix = "workdocs",
      _svcVersion = "2016-05-01",
      _svcEndpoint = defaultEndpoint workDocs,
      _svcTimeout = Just 70,
      _svcCheck = statusSuccess,
      _svcError = parseJSONError "WorkDocs",
      _svcRetry = retry
    }
  where
    retry =
      Exponential
        { _retryBase = 5.0e-2,
          _retryGrowth = 2,
          _retryAttempts = 5,
          _retryCheck = check
        }
    check e
      | has (hasCode "ThrottledException" . hasStatus 400) e =
        Just "throttled_exception"
      | has (hasStatus 429) e = Just "too_many_requests"
      | has (hasCode "ThrottlingException" . hasStatus 400) e =
        Just "throttling_exception"
      | has (hasCode "Throttling" . hasStatus 400) e = Just "throttling"
      | has
          (hasCode "ProvisionedThroughputExceededException" . hasStatus 400)
          e =
        Just "throughput_exceeded"
      | has (hasStatus 504) e = Just "gateway_timeout"
      | has (hasCode "RequestThrottledException" . hasStatus 400) e =
        Just "request_throttled_exception"
      | has (hasStatus 502) e = Just "bad_gateway"
      | has (hasStatus 503) e = Just "service_unavailable"
      | has (hasStatus 500) e = Just "general_server_error"
      | has (hasStatus 509) e = Just "limit_exceeded"
      | otherwise = Nothing
