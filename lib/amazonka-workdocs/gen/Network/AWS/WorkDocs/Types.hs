-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkDocs.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.WorkDocs.Types
    (
    -- * Service configuration
      mkServiceConfig

    -- * Errors
    , _CustomMetadataLimitExceededException
    , _EntityAlreadyExistsException
    , _ResourceAlreadyCheckedOutException
    , _ProhibitedStateException
    , _TooManyLabelsException
    , _InvalidArgumentException
    , _UnauthorizedResourceAccessException
    , _TooManySubscriptionsException
    , _FailedDependencyException
    , _DocumentLockedForCommentsException
    , _EntityNotExistsException
    , _ConflictingOperationException
    , _DeactivatingLastSystemUserException
    , _IllegalUserStateException
    , _StorageLimitWillExceedException
    , _ConcurrentModificationException
    , _RequestedEntityTooLargeException
    , _StorageLimitExceededException
    , _ServiceUnavailableException
    , _InvalidCommentOperationException
    , _InvalidOperationException
    , _UnauthorizedOperationException
    , _DraftUploadOutOfSyncException
    , _LimitExceededException
    , _InvalidPasswordException

    -- * RoleType
    , RoleType (..)

    -- * ShareResult
    , ShareResult (..)
    , mkShareResult
    , srInviteePrincipalId
    , srPrincipalId
    , srRole
    , srShareId
    , srStatus
    , srStatusMessage

    -- * SearchQueryType
    , SearchQueryType (..)

    -- * SharePrincipal
    , SharePrincipal (..)
    , mkSharePrincipal
    , spId
    , spType
    , spRole

    -- * FolderContentType
    , FolderContentType (..)

    -- * AuthenticationHeaderType
    , AuthenticationHeaderType (..)

    -- * ResourceMetadata
    , ResourceMetadata (..)
    , mkResourceMetadata
    , rmId
    , rmName
    , rmOriginalName
    , rmOwner
    , rmParentId
    , rmType
    , rmVersionId

    -- * CommentStatusType
    , CommentStatusType (..)

    -- * StorageRuleType
    , StorageRuleType (..)
    , mkStorageRuleType
    , srtStorageAllocatedInBytes
    , srtStorageType

    -- * PasswordType
    , PasswordType (..)

    -- * DocumentMetadata
    , DocumentMetadata (..)
    , mkDocumentMetadata
    , dmCreatedTimestamp
    , dmCreatorId
    , dmId
    , dmLabels
    , dmLatestVersionMetadata
    , dmModifiedTimestamp
    , dmParentFolderId
    , dmResourceState

    -- * EmailAddressType
    , EmailAddressType (..)

    -- * MarkerType
    , MarkerType (..)

    -- * ResourceType
    , ResourceType (..)

    -- * PrincipalType
    , PrincipalType (..)

    -- * UserStorageMetadata
    , UserStorageMetadata (..)
    , mkUserStorageMetadata
    , usmStorageRule
    , usmStorageUtilizedInBytes

    -- * CustomMetadataValueType
    , CustomMetadataValueType (..)

    -- * ActivityType
    , ActivityType (..)

    -- * ResourceStateType
    , ResourceStateType (..)

    -- * IdType
    , IdType (..)

    -- * NotificationOptions
    , NotificationOptions (..)
    , mkNotificationOptions
    , noEmailMessage
    , noSendEmail

    -- * ResourcePathComponent
    , ResourcePathComponent (..)
    , mkResourcePathComponent
    , rpcId
    , rpcName

    -- * GroupNameType
    , GroupNameType (..)

    -- * DocumentVersionMetadata
    , DocumentVersionMetadata (..)
    , mkDocumentVersionMetadata
    , dvmContentCreatedTimestamp
    , dvmContentModifiedTimestamp
    , dvmContentType
    , dvmCreatedTimestamp
    , dvmCreatorId
    , dvmId
    , dvmModifiedTimestamp
    , dvmName
    , dvmSignature
    , dvmSize
    , dvmSource
    , dvmStatus
    , dvmThumbnail

    -- * DocumentStatusType
    , DocumentStatusType (..)

    -- * SubscriptionType
    , SubscriptionType (..)

    -- * UserSortType
    , UserSortType (..)

    -- * FolderMetadata
    , FolderMetadata (..)
    , mkFolderMetadata
    , fmCreatedTimestamp
    , fmCreatorId
    , fmId
    , fmLabels
    , fmLatestVersionSize
    , fmModifiedTimestamp
    , fmName
    , fmParentFolderId
    , fmResourceState
    , fmSignature
    , fmSize

    -- * DocumentContentType
    , DocumentContentType (..)

    -- * UserAttributeValueType
    , UserAttributeValueType (..)

    -- * TimeZoneIdType
    , TimeZoneIdType (..)

    -- * CommentIdType
    , CommentIdType (..)

    -- * OrderType
    , OrderType (..)

    -- * PageMarkerType
    , PageMarkerType (..)

    -- * UserFilterType
    , UserFilterType (..)

    -- * User
    , User (..)
    , mkUser
    , uCreatedTimestamp
    , uEmailAddress
    , uGivenName
    , uId
    , uLocale
    , uModifiedTimestamp
    , uOrganizationId
    , uRecycleBinFolderId
    , uRootFolderId
    , uStatus
    , uStorage
    , uSurname
    , uTimeZoneId
    , uType
    , uUsername

    -- * Participants
    , Participants (..)
    , mkParticipants
    , pGroups
    , pUsers

    -- * UserMetadata
    , UserMetadata (..)
    , mkUserMetadata
    , umEmailAddress
    , umGivenName
    , umId
    , umSurname
    , umUsername

    -- * ResourceCollectionType
    , ResourceCollectionType (..)

    -- * HeaderValueType
    , HeaderValueType (..)

    -- * SharedLabel
    , SharedLabel (..)

    -- * RolePermissionType
    , RolePermissionType (..)

    -- * BooleanEnumType
    , BooleanEnumType (..)

    -- * ResourceIdType
    , ResourceIdType (..)

    -- * ResourcePath
    , ResourcePath (..)
    , mkResourcePath
    , rpComponents

    -- * UserIdsType
    , UserIdsType (..)

    -- * Principal
    , Principal (..)
    , mkPrincipal
    , pId
    , pRoles
    , pType

    -- * GroupMetadata
    , GroupMetadata (..)
    , mkGroupMetadata
    , gmId
    , gmName

    -- * DocumentSourceType
    , DocumentSourceType (..)

    -- * ResourceNameType
    , ResourceNameType (..)

    -- * PermissionInfo
    , PermissionInfo (..)
    , mkPermissionInfo
    , piRole
    , piType

    -- * SubscriptionProtocolType
    , SubscriptionProtocolType (..)

    -- * UserStatusType
    , UserStatusType (..)

    -- * Activity
    , Activity (..)
    , mkActivity
    , aCommentMetadata
    , aInitiator
    , aIsIndirectActivity
    , aOrganizationId
    , aOriginalParent
    , aParticipants
    , aResourceMetadata
    , aTimeStamp
    , aType

    -- * CommentVisibilityType
    , CommentVisibilityType (..)

    -- * ResourceSortType
    , ResourceSortType (..)

    -- * HeaderNameType
    , HeaderNameType (..)

    -- * DocumentVersionIdType
    , DocumentVersionIdType (..)

    -- * LocaleType
    , LocaleType (..)

    -- * SubscriptionEndPointType
    , SubscriptionEndPointType (..)

    -- * DocumentThumbnailType
    , DocumentThumbnailType (..)

    -- * Subscription
    , Subscription (..)
    , mkSubscription
    , sEndPoint
    , sProtocol
    , sSubscriptionId

    -- * UrlType
    , UrlType (..)

    -- * Comment
    , Comment (..)
    , mkComment
    , cCommentId
    , cContributor
    , cCreatedTimestamp
    , cParentId
    , cRecipientId
    , cStatus
    , cText
    , cThreadId
    , cVisibility

    -- * CommentMetadata
    , CommentMetadata (..)
    , mkCommentMetadata
    , cmCommentId
    , cmCommentStatus
    , cmContributor
    , cmCreatedTimestamp
    , cmRecipientId

    -- * CommentTextType
    , CommentTextType (..)

    -- * UploadMetadata
    , UploadMetadata (..)
    , mkUploadMetadata
    , umSignedHeaders
    , umUploadUrl

    -- * CustomMetadataKeyType
    , CustomMetadataKeyType (..)

    -- * DocumentVersionStatus
    , DocumentVersionStatus (..)

    -- * StorageType
    , StorageType (..)

    -- * UserType
    , UserType (..)

    -- * ShareStatusType
    , ShareStatusType (..)

    -- * FolderId
    , FolderId (..)

    -- * AuthenticationToken
    , AuthenticationToken (..)

    -- * Marker
    , Marker (..)

    -- * DocumentId
    , DocumentId (..)

    -- * VersionId
    , VersionId (..)

    -- * InviteePrincipalId
    , InviteePrincipalId (..)

    -- * PrincipalId
    , PrincipalId (..)

    -- * ShareId
    , ShareId (..)

    -- * StatusMessage
    , StatusMessage (..)

    -- * Fields
    , Fields (..)

    -- * ResourceId
    , ResourceId (..)

    -- * Id
    , Id (..)

    -- * Name
    , Name (..)

    -- * OriginalName
    , OriginalName (..)

    -- * ParentId
    , ParentId (..)

    -- * ActivityTypes
    , ActivityTypes (..)

    -- * OrganizationId
    , OrganizationId (..)

    -- * UserId
    , UserId (..)

    -- * CreatorId
    , CreatorId (..)

    -- * ParentFolderId
    , ParentFolderId (..)

    -- * Include
    , Include (..)

    -- * EmailMessage
    , EmailMessage (..)

    -- * ContentType
    , ContentType (..)

    -- * Signature
    , Signature (..)

    -- * CommentId
    , CommentId (..)

    -- * RecycleBinFolderId
    , RecycleBinFolderId (..)

    -- * RootFolderId
    , RootFolderId (..)

    -- * Username
    , Username (..)

    -- * Text
    , Text (..)
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Sign.V4 as Sign
import Network.AWS.WorkDocs.Types.RoleType
  
import Network.AWS.WorkDocs.Types.ShareResult
  
import Network.AWS.WorkDocs.Types.SearchQueryType
  
import Network.AWS.WorkDocs.Types.SharePrincipal
  
import Network.AWS.WorkDocs.Types.FolderContentType
  
import Network.AWS.WorkDocs.Types.AuthenticationHeaderType
  
import Network.AWS.WorkDocs.Types.ResourceMetadata
  
import Network.AWS.WorkDocs.Types.CommentStatusType
  
import Network.AWS.WorkDocs.Types.StorageRuleType
  
import Network.AWS.WorkDocs.Types.PasswordType
  
  
  
import Network.AWS.WorkDocs.Types.DocumentMetadata
  
import Network.AWS.WorkDocs.Types.EmailAddressType
  
import Network.AWS.WorkDocs.Types.MarkerType
  
  
  
import Network.AWS.WorkDocs.Types.ResourceType
  
import Network.AWS.WorkDocs.Types.PrincipalType
  
  
  
import Network.AWS.WorkDocs.Types.UserStorageMetadata
  
import Network.AWS.WorkDocs.Types.CustomMetadataValueType
  
import Network.AWS.WorkDocs.Types.ActivityType
  
import Network.AWS.WorkDocs.Types.ResourceStateType
  
import Network.AWS.WorkDocs.Types.IdType
  
  
import Network.AWS.WorkDocs.Types.NotificationOptions
  
import Network.AWS.WorkDocs.Types.ResourcePathComponent
  
  
import Network.AWS.WorkDocs.Types.GroupNameType
  
import Network.AWS.WorkDocs.Types.DocumentVersionMetadata
  
import Network.AWS.WorkDocs.Types.DocumentStatusType
  
import Network.AWS.WorkDocs.Types.SubscriptionType
  
import Network.AWS.WorkDocs.Types.UserSortType
  
  
import Network.AWS.WorkDocs.Types.FolderMetadata
  
import Network.AWS.WorkDocs.Types.DocumentContentType
  
import Network.AWS.WorkDocs.Types.UserAttributeValueType
  
import Network.AWS.WorkDocs.Types.TimeZoneIdType
  
import Network.AWS.WorkDocs.Types.CommentIdType
  
import Network.AWS.WorkDocs.Types.OrderType
  
import Network.AWS.WorkDocs.Types.PageMarkerType
  
  
import Network.AWS.WorkDocs.Types.UserFilterType
  
  
import Network.AWS.WorkDocs.Types.User
  
  
import Network.AWS.WorkDocs.Types.Participants
  
import Network.AWS.WorkDocs.Types.UserMetadata
  
import Network.AWS.WorkDocs.Types.ResourceCollectionType
  
import Network.AWS.WorkDocs.Types.HeaderValueType
  
  
import Network.AWS.WorkDocs.Types.SharedLabel
  
  
import Network.AWS.WorkDocs.Types.RolePermissionType
  
import Network.AWS.WorkDocs.Types.BooleanEnumType
  
import Network.AWS.WorkDocs.Types.ResourceIdType
  
import Network.AWS.WorkDocs.Types.ResourcePath
  
  
import Network.AWS.WorkDocs.Types.UserIdsType
  
import Network.AWS.WorkDocs.Types.Principal
  
import Network.AWS.WorkDocs.Types.GroupMetadata
  
  
import Network.AWS.WorkDocs.Types.DocumentSourceType
  
import Network.AWS.WorkDocs.Types.ResourceNameType
  
import Network.AWS.WorkDocs.Types.PermissionInfo
  
  
import Network.AWS.WorkDocs.Types.SubscriptionProtocolType
  
import Network.AWS.WorkDocs.Types.UserStatusType
  
import Network.AWS.WorkDocs.Types.Activity
  
import Network.AWS.WorkDocs.Types.CommentVisibilityType
  
  
  
import Network.AWS.WorkDocs.Types.ResourceSortType
  
  
import Network.AWS.WorkDocs.Types.HeaderNameType
  
import Network.AWS.WorkDocs.Types.DocumentVersionIdType
  
import Network.AWS.WorkDocs.Types.LocaleType
  
import Network.AWS.WorkDocs.Types.SubscriptionEndPointType
  
import Network.AWS.WorkDocs.Types.DocumentThumbnailType
  
import Network.AWS.WorkDocs.Types.Subscription
  
import Network.AWS.WorkDocs.Types.UrlType
  
  
import Network.AWS.WorkDocs.Types.Comment
  
import Network.AWS.WorkDocs.Types.CommentMetadata
  
import Network.AWS.WorkDocs.Types.CommentTextType
  
  
import Network.AWS.WorkDocs.Types.UploadMetadata
  
import Network.AWS.WorkDocs.Types.CustomMetadataKeyType
  
import Network.AWS.WorkDocs.Types.DocumentVersionStatus
  
  
import Network.AWS.WorkDocs.Types.StorageType
  
import Network.AWS.WorkDocs.Types.UserType
  
  
import Network.AWS.WorkDocs.Types.ShareStatusType
  
  
import Network.AWS.WorkDocs.Types.FolderId
  
import Network.AWS.WorkDocs.Types.AuthenticationToken
  
import Network.AWS.WorkDocs.Types.Marker
  
import Network.AWS.WorkDocs.Types.DocumentId
  
import Network.AWS.WorkDocs.Types.VersionId
  
import Network.AWS.WorkDocs.Types.InviteePrincipalId
  
import Network.AWS.WorkDocs.Types.PrincipalId
  
import Network.AWS.WorkDocs.Types.ShareId
  
import Network.AWS.WorkDocs.Types.StatusMessage
  
import Network.AWS.WorkDocs.Types.Fields
  
import Network.AWS.WorkDocs.Types.ResourceId
  
import Network.AWS.WorkDocs.Types.Id
  
import Network.AWS.WorkDocs.Types.Name
  
import Network.AWS.WorkDocs.Types.OriginalName
  
import Network.AWS.WorkDocs.Types.ParentId
  
import Network.AWS.WorkDocs.Types.ActivityTypes
  
import Network.AWS.WorkDocs.Types.OrganizationId
  
import Network.AWS.WorkDocs.Types.UserId
  
import Network.AWS.WorkDocs.Types.CreatorId
  
import Network.AWS.WorkDocs.Types.ParentFolderId
  
import Network.AWS.WorkDocs.Types.Include
  
import Network.AWS.WorkDocs.Types.EmailMessage
  
import Network.AWS.WorkDocs.Types.ContentType
  
import Network.AWS.WorkDocs.Types.Signature
  
import Network.AWS.WorkDocs.Types.CommentId
  
import Network.AWS.WorkDocs.Types.RecycleBinFolderId
  
import Network.AWS.WorkDocs.Types.RootFolderId
  
import Network.AWS.WorkDocs.Types.Username
  
import Network.AWS.WorkDocs.Types.Text
  

-- | API version @2016-05-01@ of the Amazon WorkDocs SDK configuration.
mkServiceConfig :: Core.Service
mkServiceConfig
  = Core.Service{Core._svcAbbrev = "WorkDocs",
                 Core._svcSigner = Sign.v4, Core._svcPrefix = "workdocs",
                 Core._svcVersion = "2016-05-01", Core._svcTimeout = Core.Just 70,
                 Core._svcCheck = Core.statusSuccess, Core._svcRetry = retry,
                 Core._svcError = Core.parseJSONError "WorkDocs",
                 Core._svcEndpoint = Core.defaultEndpoint mkServiceConfig}
  where retry
          = Core.Exponential{Core._retryBase = 5.0e-2, Core._retryGrowth = 2,
                             Core._retryAttempts = 5, Core._retryCheck = check}
        check e
          | Lens.has
              (Core.hasCode "ThrottledException" Core.. Core.hasStatus 400)
              e
            = Core.Just "throttled_exception"
          | Lens.has (Core.hasStatus 429) e = Core.Just "too_many_requests"
          | Lens.has
              (Core.hasCode "ThrottlingException" Core.. Core.hasStatus 400)
              e
            = Core.Just "throttling_exception"
          | Lens.has (Core.hasCode "Throttling" Core.. Core.hasStatus 400) e
            = Core.Just "throttling"
          | Lens.has
              (Core.hasCode "ProvisionedThroughputExceededException" Core..
                 Core.hasStatus 400)
              e
            = Core.Just "throughput_exceeded"
          | Lens.has (Core.hasStatus 504) e = Core.Just "gateway_timeout"
          | Lens.has
              (Core.hasCode "RequestThrottledException" Core..
                 Core.hasStatus 400)
              e
            = Core.Just "request_throttled_exception"
          | Lens.has (Core.hasStatus 502) e = Core.Just "bad_gateway"
          | Lens.has (Core.hasStatus 503) e = Core.Just "service_unavailable"
          | Lens.has (Core.hasStatus 500) e =
            Core.Just "general_server_error"
          | Lens.has (Core.hasStatus 509) e = Core.Just "limit_exceeded"
          | Core.otherwise = Core.Nothing

-- | The limit has been reached on the number of custom properties for the specified resource.
_CustomMetadataLimitExceededException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_CustomMetadataLimitExceededException
  = Core._MatchServiceError mkServiceConfig
      "CustomMetadataLimitExceededException"
      Core.. Core.hasStatues 429
{-# INLINEABLE _CustomMetadataLimitExceededException #-}
{-# DEPRECATED _CustomMetadataLimitExceededException "Use generic-lens or generic-optics instead"  #-}

-- | The resource already exists.
_EntityAlreadyExistsException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_EntityAlreadyExistsException
  = Core._MatchServiceError mkServiceConfig
      "EntityAlreadyExistsException"
      Core.. Core.hasStatues 409
{-# INLINEABLE _EntityAlreadyExistsException #-}
{-# DEPRECATED _EntityAlreadyExistsException "Use generic-lens or generic-optics instead"  #-}

-- | The resource is already checked out.
_ResourceAlreadyCheckedOutException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ResourceAlreadyCheckedOutException
  = Core._MatchServiceError mkServiceConfig
      "ResourceAlreadyCheckedOutException"
      Core.. Core.hasStatues 409
{-# INLINEABLE _ResourceAlreadyCheckedOutException #-}
{-# DEPRECATED _ResourceAlreadyCheckedOutException "Use generic-lens or generic-optics instead"  #-}

-- | The specified document version is not in the INITIALIZED state.
_ProhibitedStateException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ProhibitedStateException
  = Core._MatchServiceError mkServiceConfig
      "ProhibitedStateException"
      Core.. Core.hasStatues 409
{-# INLINEABLE _ProhibitedStateException #-}
{-# DEPRECATED _ProhibitedStateException "Use generic-lens or generic-optics instead"  #-}

-- | The limit has been reached on the number of labels for the specified resource.
_TooManyLabelsException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TooManyLabelsException
  = Core._MatchServiceError mkServiceConfig "TooManyLabelsException"
      Core.. Core.hasStatues 429
{-# INLINEABLE _TooManyLabelsException #-}
{-# DEPRECATED _TooManyLabelsException "Use generic-lens or generic-optics instead"  #-}

-- | The pagination marker or limit fields are not valid.
_InvalidArgumentException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidArgumentException
  = Core._MatchServiceError mkServiceConfig
      "InvalidArgumentException"
      Core.. Core.hasStatues 400
{-# INLINEABLE _InvalidArgumentException #-}
{-# DEPRECATED _InvalidArgumentException "Use generic-lens or generic-optics instead"  #-}

-- | The caller does not have access to perform the action on the resource.
_UnauthorizedResourceAccessException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_UnauthorizedResourceAccessException
  = Core._MatchServiceError mkServiceConfig
      "UnauthorizedResourceAccessException"
      Core.. Core.hasStatues 404
{-# INLINEABLE _UnauthorizedResourceAccessException #-}
{-# DEPRECATED _UnauthorizedResourceAccessException "Use generic-lens or generic-optics instead"  #-}

-- | You've reached the limit on the number of subscriptions for the WorkDocs instance.
_TooManySubscriptionsException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TooManySubscriptionsException
  = Core._MatchServiceError mkServiceConfig
      "TooManySubscriptionsException"
      Core.. Core.hasStatues 429
{-# INLINEABLE _TooManySubscriptionsException #-}
{-# DEPRECATED _TooManySubscriptionsException "Use generic-lens or generic-optics instead"  #-}

-- | The AWS Directory Service cannot reach an on-premises instance. Or a dependency under the control of the organization is failing, such as a connected Active Directory.
_FailedDependencyException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_FailedDependencyException
  = Core._MatchServiceError mkServiceConfig
      "FailedDependencyException"
      Core.. Core.hasStatues 424
{-# INLINEABLE _FailedDependencyException #-}
{-# DEPRECATED _FailedDependencyException "Use generic-lens or generic-optics instead"  #-}

-- | This exception is thrown when the document is locked for comments and user tries to create or delete a comment on that document.
_DocumentLockedForCommentsException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DocumentLockedForCommentsException
  = Core._MatchServiceError mkServiceConfig
      "DocumentLockedForCommentsException"
      Core.. Core.hasStatues 409
{-# INLINEABLE _DocumentLockedForCommentsException #-}
{-# DEPRECATED _DocumentLockedForCommentsException "Use generic-lens or generic-optics instead"  #-}

-- | The resource does not exist.
_EntityNotExistsException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_EntityNotExistsException
  = Core._MatchServiceError mkServiceConfig
      "EntityNotExistsException"
      Core.. Core.hasStatues 404
{-# INLINEABLE _EntityNotExistsException #-}
{-# DEPRECATED _EntityNotExistsException "Use generic-lens or generic-optics instead"  #-}

-- | Another operation is in progress on the resource that conflicts with the current operation.
_ConflictingOperationException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ConflictingOperationException
  = Core._MatchServiceError mkServiceConfig
      "ConflictingOperationException"
      Core.. Core.hasStatues 409
{-# INLINEABLE _ConflictingOperationException #-}
{-# DEPRECATED _ConflictingOperationException "Use generic-lens or generic-optics instead"  #-}

-- | The last user in the organization is being deactivated.
_DeactivatingLastSystemUserException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DeactivatingLastSystemUserException
  = Core._MatchServiceError mkServiceConfig
      "DeactivatingLastSystemUserException"
      Core.. Core.hasStatues 409
{-# INLINEABLE _DeactivatingLastSystemUserException #-}
{-# DEPRECATED _DeactivatingLastSystemUserException "Use generic-lens or generic-optics instead"  #-}

-- | The user is undergoing transfer of ownership.
_IllegalUserStateException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_IllegalUserStateException
  = Core._MatchServiceError mkServiceConfig
      "IllegalUserStateException"
      Core.. Core.hasStatues 409
{-# INLINEABLE _IllegalUserStateException #-}
{-# DEPRECATED _IllegalUserStateException "Use generic-lens or generic-optics instead"  #-}

-- | The storage limit will be exceeded.
_StorageLimitWillExceedException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_StorageLimitWillExceedException
  = Core._MatchServiceError mkServiceConfig
      "StorageLimitWillExceedException"
      Core.. Core.hasStatues 413
{-# INLINEABLE _StorageLimitWillExceedException #-}
{-# DEPRECATED _StorageLimitWillExceedException "Use generic-lens or generic-optics instead"  #-}

-- | The resource hierarchy is changing.
_ConcurrentModificationException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ConcurrentModificationException
  = Core._MatchServiceError mkServiceConfig
      "ConcurrentModificationException"
      Core.. Core.hasStatues 409
{-# INLINEABLE _ConcurrentModificationException #-}
{-# DEPRECATED _ConcurrentModificationException "Use generic-lens or generic-optics instead"  #-}

-- | The response is too large to return. The request must include a filter to reduce the size of the response.
_RequestedEntityTooLargeException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_RequestedEntityTooLargeException
  = Core._MatchServiceError mkServiceConfig
      "RequestedEntityTooLargeException"
      Core.. Core.hasStatues 413
{-# INLINEABLE _RequestedEntityTooLargeException #-}
{-# DEPRECATED _RequestedEntityTooLargeException "Use generic-lens or generic-optics instead"  #-}

-- | The storage limit has been exceeded.
_StorageLimitExceededException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_StorageLimitExceededException
  = Core._MatchServiceError mkServiceConfig
      "StorageLimitExceededException"
      Core.. Core.hasStatues 409
{-# INLINEABLE _StorageLimitExceededException #-}
{-# DEPRECATED _StorageLimitExceededException "Use generic-lens or generic-optics instead"  #-}

-- | One or more of the dependencies is unavailable.
_ServiceUnavailableException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ServiceUnavailableException
  = Core._MatchServiceError mkServiceConfig
      "ServiceUnavailableException"
      Core.. Core.hasStatues 503
{-# INLINEABLE _ServiceUnavailableException #-}
{-# DEPRECATED _ServiceUnavailableException "Use generic-lens or generic-optics instead"  #-}

-- | The requested operation is not allowed on the specified comment object.
_InvalidCommentOperationException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidCommentOperationException
  = Core._MatchServiceError mkServiceConfig
      "InvalidCommentOperationException"
      Core.. Core.hasStatues 409
{-# INLINEABLE _InvalidCommentOperationException #-}
{-# DEPRECATED _InvalidCommentOperationException "Use generic-lens or generic-optics instead"  #-}

-- | The operation is invalid.
_InvalidOperationException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidOperationException
  = Core._MatchServiceError mkServiceConfig
      "InvalidOperationException"
      Core.. Core.hasStatues 405
{-# INLINEABLE _InvalidOperationException #-}
{-# DEPRECATED _InvalidOperationException "Use generic-lens or generic-optics instead"  #-}

-- | The operation is not permitted.
_UnauthorizedOperationException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_UnauthorizedOperationException
  = Core._MatchServiceError mkServiceConfig
      "UnauthorizedOperationException"
      Core.. Core.hasStatues 403
{-# INLINEABLE _UnauthorizedOperationException #-}
{-# DEPRECATED _UnauthorizedOperationException "Use generic-lens or generic-optics instead"  #-}

-- | This exception is thrown when a valid checkout ID is not presented on document version upload calls for a document that has been checked out from Web client.
_DraftUploadOutOfSyncException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DraftUploadOutOfSyncException
  = Core._MatchServiceError mkServiceConfig
      "DraftUploadOutOfSyncException"
      Core.. Core.hasStatues 409
{-# INLINEABLE _DraftUploadOutOfSyncException #-}
{-# DEPRECATED _DraftUploadOutOfSyncException "Use generic-lens or generic-optics instead"  #-}

-- | The maximum of 100,000 folders under the parent folder has been exceeded.
_LimitExceededException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_LimitExceededException
  = Core._MatchServiceError mkServiceConfig "LimitExceededException"
      Core.. Core.hasStatues 409
{-# INLINEABLE _LimitExceededException #-}
{-# DEPRECATED _LimitExceededException "Use generic-lens or generic-optics instead"  #-}

-- | The password is invalid.
_InvalidPasswordException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidPasswordException
  = Core._MatchServiceError mkServiceConfig
      "InvalidPasswordException"
      Core.. Core.hasStatues 401
{-# INLINEABLE _InvalidPasswordException #-}
{-# DEPRECATED _InvalidPasswordException "Use generic-lens or generic-optics instead"  #-}
