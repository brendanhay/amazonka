{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkDocs
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The WorkDocs API is designed for the following use cases:
--
-- -   File Migration: File migration applications are supported for users
--     who want to migrate their files from an on-premises or off-premises
--     file system or service. Users can insert files into a user directory
--     structure, as well as allow for basic metadata changes, such as
--     modifications to the permissions of files.
--
-- -   Security: Support security applications are supported for users who
--     have additional security needs, such as antivirus or data loss
--     prevention. The API actions, along with AWS CloudTrail, allow these
--     applications to detect when changes occur in Amazon WorkDocs. Then,
--     the application can take the necessary actions and replace the
--     target file. If the target file violates the policy, the application
--     can also choose to email the user.
--
-- -   eDiscovery\/Analytics: General administrative applications are
--     supported, such as eDiscovery and analytics. These applications can
--     choose to mimic or record the actions in an Amazon WorkDocs site,
--     along with AWS CloudTrail, to replicate data for eDiscovery, backup,
--     or analytical applications.
--
-- All Amazon WorkDocs API actions are Amazon authenticated and
-- certificate-signed. They not only require the use of the AWS SDK, but
-- also allow for the exclusive use of IAM users and roles to help
-- facilitate access, trust, and permission policies. By creating a role
-- and allowing an IAM user to access the Amazon WorkDocs site, the IAM
-- user gains full administrative visibility into the entire Amazon
-- WorkDocs site (or as set in the IAM policy). This includes, but is not
-- limited to, the ability to modify file permissions and upload any file
-- to any user. This allows developers to perform the three use cases
-- above, as well as give users the ability to grant access on a selective
-- basis using the IAM model.
module Network.AWS.WorkDocs
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** IllegalUserStateException
    _IllegalUserStateException,

    -- ** StorageLimitWillExceedException
    _StorageLimitWillExceedException,

    -- ** CustomMetadataLimitExceededException
    _CustomMetadataLimitExceededException,

    -- ** ConflictingOperationException
    _ConflictingOperationException,

    -- ** DeactivatingLastSystemUserException
    _DeactivatingLastSystemUserException,

    -- ** DraftUploadOutOfSyncException
    _DraftUploadOutOfSyncException,

    -- ** DocumentLockedForCommentsException
    _DocumentLockedForCommentsException,

    -- ** EntityNotExistsException
    _EntityNotExistsException,

    -- ** UnauthorizedOperationException
    _UnauthorizedOperationException,

    -- ** InvalidOperationException
    _InvalidOperationException,

    -- ** StorageLimitExceededException
    _StorageLimitExceededException,

    -- ** ServiceUnavailableException
    _ServiceUnavailableException,

    -- ** UnauthorizedResourceAccessException
    _UnauthorizedResourceAccessException,

    -- ** RequestedEntityTooLargeException
    _RequestedEntityTooLargeException,

    -- ** ProhibitedStateException
    _ProhibitedStateException,

    -- ** EntityAlreadyExistsException
    _EntityAlreadyExistsException,

    -- ** ConcurrentModificationException
    _ConcurrentModificationException,

    -- ** ResourceAlreadyCheckedOutException
    _ResourceAlreadyCheckedOutException,

    -- ** LimitExceededException
    _LimitExceededException,

    -- ** InvalidPasswordException
    _InvalidPasswordException,

    -- ** FailedDependencyException
    _FailedDependencyException,

    -- ** InvalidCommentOperationException
    _InvalidCommentOperationException,

    -- ** TooManySubscriptionsException
    _TooManySubscriptionsException,

    -- ** TooManyLabelsException
    _TooManyLabelsException,

    -- ** InvalidArgumentException
    _InvalidArgumentException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** DeleteFolder
    DeleteFolder (DeleteFolder'),
    newDeleteFolder,
    DeleteFolderResponse (DeleteFolderResponse'),
    newDeleteFolderResponse,

    -- ** UpdateFolder
    UpdateFolder (UpdateFolder'),
    newUpdateFolder,
    UpdateFolderResponse (UpdateFolderResponse'),
    newUpdateFolderResponse,

    -- ** DeleteCustomMetadata
    DeleteCustomMetadata (DeleteCustomMetadata'),
    newDeleteCustomMetadata,
    DeleteCustomMetadataResponse (DeleteCustomMetadataResponse'),
    newDeleteCustomMetadataResponse,

    -- ** DeleteNotificationSubscription
    DeleteNotificationSubscription (DeleteNotificationSubscription'),
    newDeleteNotificationSubscription,
    DeleteNotificationSubscriptionResponse (DeleteNotificationSubscriptionResponse'),
    newDeleteNotificationSubscriptionResponse,

    -- ** UpdateDocumentVersion
    UpdateDocumentVersion (UpdateDocumentVersion'),
    newUpdateDocumentVersion,
    UpdateDocumentVersionResponse (UpdateDocumentVersionResponse'),
    newUpdateDocumentVersionResponse,

    -- ** DeleteLabels
    DeleteLabels (DeleteLabels'),
    newDeleteLabels,
    DeleteLabelsResponse (DeleteLabelsResponse'),
    newDeleteLabelsResponse,

    -- ** AbortDocumentVersionUpload
    AbortDocumentVersionUpload (AbortDocumentVersionUpload'),
    newAbortDocumentVersionUpload,
    AbortDocumentVersionUploadResponse (AbortDocumentVersionUploadResponse'),
    newAbortDocumentVersionUploadResponse,

    -- ** DescribeFolderContents (Paginated)
    DescribeFolderContents (DescribeFolderContents'),
    newDescribeFolderContents,
    DescribeFolderContentsResponse (DescribeFolderContentsResponse'),
    newDescribeFolderContentsResponse,

    -- ** CreateLabels
    CreateLabels (CreateLabels'),
    newCreateLabels,
    CreateLabelsResponse (CreateLabelsResponse'),
    newCreateLabelsResponse,

    -- ** DeactivateUser
    DeactivateUser (DeactivateUser'),
    newDeactivateUser,
    DeactivateUserResponse (DeactivateUserResponse'),
    newDeactivateUserResponse,

    -- ** DescribeRootFolders (Paginated)
    DescribeRootFolders (DescribeRootFolders'),
    newDescribeRootFolders,
    DescribeRootFoldersResponse (DescribeRootFoldersResponse'),
    newDescribeRootFoldersResponse,

    -- ** UpdateDocument
    UpdateDocument (UpdateDocument'),
    newUpdateDocument,
    UpdateDocumentResponse (UpdateDocumentResponse'),
    newUpdateDocumentResponse,

    -- ** DeleteDocument
    DeleteDocument (DeleteDocument'),
    newDeleteDocument,
    DeleteDocumentResponse (DeleteDocumentResponse'),
    newDeleteDocumentResponse,

    -- ** GetDocumentVersion
    GetDocumentVersion (GetDocumentVersion'),
    newGetDocumentVersion,
    GetDocumentVersionResponse (GetDocumentVersionResponse'),
    newGetDocumentVersionResponse,

    -- ** DescribeDocumentVersions (Paginated)
    DescribeDocumentVersions (DescribeDocumentVersions'),
    newDescribeDocumentVersions,
    DescribeDocumentVersionsResponse (DescribeDocumentVersionsResponse'),
    newDescribeDocumentVersionsResponse,

    -- ** ActivateUser
    ActivateUser (ActivateUser'),
    newActivateUser,
    ActivateUserResponse (ActivateUserResponse'),
    newActivateUserResponse,

    -- ** GetFolderPath
    GetFolderPath (GetFolderPath'),
    newGetFolderPath,
    GetFolderPathResponse (GetFolderPathResponse'),
    newGetFolderPathResponse,

    -- ** CreateUser
    CreateUser (CreateUser'),
    newCreateUser,
    CreateUserResponse (CreateUserResponse'),
    newCreateUserResponse,

    -- ** CreateCustomMetadata
    CreateCustomMetadata (CreateCustomMetadata'),
    newCreateCustomMetadata,
    CreateCustomMetadataResponse (CreateCustomMetadataResponse'),
    newCreateCustomMetadataResponse,

    -- ** DeleteComment
    DeleteComment (DeleteComment'),
    newDeleteComment,
    DeleteCommentResponse (DeleteCommentResponse'),
    newDeleteCommentResponse,

    -- ** CreateFolder
    CreateFolder (CreateFolder'),
    newCreateFolder,
    CreateFolderResponse (CreateFolderResponse'),
    newCreateFolderResponse,

    -- ** CreateNotificationSubscription
    CreateNotificationSubscription (CreateNotificationSubscription'),
    newCreateNotificationSubscription,
    CreateNotificationSubscriptionResponse (CreateNotificationSubscriptionResponse'),
    newCreateNotificationSubscriptionResponse,

    -- ** CreateComment
    CreateComment (CreateComment'),
    newCreateComment,
    CreateCommentResponse (CreateCommentResponse'),
    newCreateCommentResponse,

    -- ** DescribeResourcePermissions (Paginated)
    DescribeResourcePermissions (DescribeResourcePermissions'),
    newDescribeResourcePermissions,
    DescribeResourcePermissionsResponse (DescribeResourcePermissionsResponse'),
    newDescribeResourcePermissionsResponse,

    -- ** RemoveResourcePermission
    RemoveResourcePermission (RemoveResourcePermission'),
    newRemoveResourcePermission,
    RemoveResourcePermissionResponse (RemoveResourcePermissionResponse'),
    newRemoveResourcePermissionResponse,

    -- ** DescribeUsers (Paginated)
    DescribeUsers (DescribeUsers'),
    newDescribeUsers,
    DescribeUsersResponse (DescribeUsersResponse'),
    newDescribeUsersResponse,

    -- ** GetResources
    GetResources (GetResources'),
    newGetResources,
    GetResourcesResponse (GetResourcesResponse'),
    newGetResourcesResponse,

    -- ** GetDocumentPath
    GetDocumentPath (GetDocumentPath'),
    newGetDocumentPath,
    GetDocumentPathResponse (GetDocumentPathResponse'),
    newGetDocumentPathResponse,

    -- ** DescribeGroups (Paginated)
    DescribeGroups (DescribeGroups'),
    newDescribeGroups,
    DescribeGroupsResponse (DescribeGroupsResponse'),
    newDescribeGroupsResponse,

    -- ** GetDocument
    GetDocument (GetDocument'),
    newGetDocument,
    GetDocumentResponse (GetDocumentResponse'),
    newGetDocumentResponse,

    -- ** DescribeActivities (Paginated)
    DescribeActivities (DescribeActivities'),
    newDescribeActivities,
    DescribeActivitiesResponse (DescribeActivitiesResponse'),
    newDescribeActivitiesResponse,

    -- ** GetCurrentUser
    GetCurrentUser (GetCurrentUser'),
    newGetCurrentUser,
    GetCurrentUserResponse (GetCurrentUserResponse'),
    newGetCurrentUserResponse,

    -- ** AddResourcePermissions
    AddResourcePermissions (AddResourcePermissions'),
    newAddResourcePermissions,
    AddResourcePermissionsResponse (AddResourcePermissionsResponse'),
    newAddResourcePermissionsResponse,

    -- ** DeleteUser
    DeleteUser (DeleteUser'),
    newDeleteUser,
    DeleteUserResponse (DeleteUserResponse'),
    newDeleteUserResponse,

    -- ** GetFolder
    GetFolder (GetFolder'),
    newGetFolder,
    GetFolderResponse (GetFolderResponse'),
    newGetFolderResponse,

    -- ** UpdateUser
    UpdateUser (UpdateUser'),
    newUpdateUser,
    UpdateUserResponse (UpdateUserResponse'),
    newUpdateUserResponse,

    -- ** DescribeNotificationSubscriptions (Paginated)
    DescribeNotificationSubscriptions (DescribeNotificationSubscriptions'),
    newDescribeNotificationSubscriptions,
    DescribeNotificationSubscriptionsResponse (DescribeNotificationSubscriptionsResponse'),
    newDescribeNotificationSubscriptionsResponse,

    -- ** RemoveAllResourcePermissions
    RemoveAllResourcePermissions (RemoveAllResourcePermissions'),
    newRemoveAllResourcePermissions,
    RemoveAllResourcePermissionsResponse (RemoveAllResourcePermissionsResponse'),
    newRemoveAllResourcePermissionsResponse,

    -- ** DeleteFolderContents
    DeleteFolderContents (DeleteFolderContents'),
    newDeleteFolderContents,
    DeleteFolderContentsResponse (DeleteFolderContentsResponse'),
    newDeleteFolderContentsResponse,

    -- ** DescribeComments (Paginated)
    DescribeComments (DescribeComments'),
    newDescribeComments,
    DescribeCommentsResponse (DescribeCommentsResponse'),
    newDescribeCommentsResponse,

    -- ** InitiateDocumentVersionUpload
    InitiateDocumentVersionUpload (InitiateDocumentVersionUpload'),
    newInitiateDocumentVersionUpload,
    InitiateDocumentVersionUploadResponse (InitiateDocumentVersionUploadResponse'),
    newInitiateDocumentVersionUploadResponse,

    -- * Types

    -- ** ActivityType
    ActivityType (..),

    -- ** BooleanEnumType
    BooleanEnumType (..),

    -- ** CommentStatusType
    CommentStatusType (..),

    -- ** CommentVisibilityType
    CommentVisibilityType (..),

    -- ** DocumentSourceType
    DocumentSourceType (..),

    -- ** DocumentStatusType
    DocumentStatusType (..),

    -- ** DocumentThumbnailType
    DocumentThumbnailType (..),

    -- ** DocumentVersionStatus
    DocumentVersionStatus (..),

    -- ** FolderContentType
    FolderContentType (..),

    -- ** LocaleType
    LocaleType (..),

    -- ** OrderType
    OrderType (..),

    -- ** PrincipalType
    PrincipalType (..),

    -- ** ResourceCollectionType
    ResourceCollectionType (..),

    -- ** ResourceSortType
    ResourceSortType (..),

    -- ** ResourceStateType
    ResourceStateType (..),

    -- ** ResourceType
    ResourceType (..),

    -- ** RolePermissionType
    RolePermissionType (..),

    -- ** RoleType
    RoleType (..),

    -- ** ShareStatusType
    ShareStatusType (..),

    -- ** StorageType
    StorageType (..),

    -- ** SubscriptionProtocolType
    SubscriptionProtocolType (..),

    -- ** SubscriptionType
    SubscriptionType (..),

    -- ** UserFilterType
    UserFilterType (..),

    -- ** UserSortType
    UserSortType (..),

    -- ** UserStatusType
    UserStatusType (..),

    -- ** UserType
    UserType (..),

    -- ** Activity
    Activity (Activity'),
    newActivity,

    -- ** Comment
    Comment (Comment'),
    newComment,

    -- ** CommentMetadata
    CommentMetadata (CommentMetadata'),
    newCommentMetadata,

    -- ** DocumentMetadata
    DocumentMetadata (DocumentMetadata'),
    newDocumentMetadata,

    -- ** DocumentVersionMetadata
    DocumentVersionMetadata (DocumentVersionMetadata'),
    newDocumentVersionMetadata,

    -- ** FolderMetadata
    FolderMetadata (FolderMetadata'),
    newFolderMetadata,

    -- ** GroupMetadata
    GroupMetadata (GroupMetadata'),
    newGroupMetadata,

    -- ** NotificationOptions
    NotificationOptions (NotificationOptions'),
    newNotificationOptions,

    -- ** Participants
    Participants (Participants'),
    newParticipants,

    -- ** PermissionInfo
    PermissionInfo (PermissionInfo'),
    newPermissionInfo,

    -- ** Principal
    Principal (Principal'),
    newPrincipal,

    -- ** ResourceMetadata
    ResourceMetadata (ResourceMetadata'),
    newResourceMetadata,

    -- ** ResourcePath
    ResourcePath (ResourcePath'),
    newResourcePath,

    -- ** ResourcePathComponent
    ResourcePathComponent (ResourcePathComponent'),
    newResourcePathComponent,

    -- ** SharePrincipal
    SharePrincipal (SharePrincipal'),
    newSharePrincipal,

    -- ** ShareResult
    ShareResult (ShareResult'),
    newShareResult,

    -- ** StorageRuleType
    StorageRuleType (StorageRuleType'),
    newStorageRuleType,

    -- ** Subscription
    Subscription (Subscription'),
    newSubscription,

    -- ** UploadMetadata
    UploadMetadata (UploadMetadata'),
    newUploadMetadata,

    -- ** User
    User (User'),
    newUser,

    -- ** UserMetadata
    UserMetadata (UserMetadata'),
    newUserMetadata,

    -- ** UserStorageMetadata
    UserStorageMetadata (UserStorageMetadata'),
    newUserStorageMetadata,
  )
where

import Network.AWS.WorkDocs.AbortDocumentVersionUpload
import Network.AWS.WorkDocs.ActivateUser
import Network.AWS.WorkDocs.AddResourcePermissions
import Network.AWS.WorkDocs.CreateComment
import Network.AWS.WorkDocs.CreateCustomMetadata
import Network.AWS.WorkDocs.CreateFolder
import Network.AWS.WorkDocs.CreateLabels
import Network.AWS.WorkDocs.CreateNotificationSubscription
import Network.AWS.WorkDocs.CreateUser
import Network.AWS.WorkDocs.DeactivateUser
import Network.AWS.WorkDocs.DeleteComment
import Network.AWS.WorkDocs.DeleteCustomMetadata
import Network.AWS.WorkDocs.DeleteDocument
import Network.AWS.WorkDocs.DeleteFolder
import Network.AWS.WorkDocs.DeleteFolderContents
import Network.AWS.WorkDocs.DeleteLabels
import Network.AWS.WorkDocs.DeleteNotificationSubscription
import Network.AWS.WorkDocs.DeleteUser
import Network.AWS.WorkDocs.DescribeActivities
import Network.AWS.WorkDocs.DescribeComments
import Network.AWS.WorkDocs.DescribeDocumentVersions
import Network.AWS.WorkDocs.DescribeFolderContents
import Network.AWS.WorkDocs.DescribeGroups
import Network.AWS.WorkDocs.DescribeNotificationSubscriptions
import Network.AWS.WorkDocs.DescribeResourcePermissions
import Network.AWS.WorkDocs.DescribeRootFolders
import Network.AWS.WorkDocs.DescribeUsers
import Network.AWS.WorkDocs.GetCurrentUser
import Network.AWS.WorkDocs.GetDocument
import Network.AWS.WorkDocs.GetDocumentPath
import Network.AWS.WorkDocs.GetDocumentVersion
import Network.AWS.WorkDocs.GetFolder
import Network.AWS.WorkDocs.GetFolderPath
import Network.AWS.WorkDocs.GetResources
import Network.AWS.WorkDocs.InitiateDocumentVersionUpload
import Network.AWS.WorkDocs.Lens
import Network.AWS.WorkDocs.RemoveAllResourcePermissions
import Network.AWS.WorkDocs.RemoveResourcePermission
import Network.AWS.WorkDocs.Types
import Network.AWS.WorkDocs.UpdateDocument
import Network.AWS.WorkDocs.UpdateDocumentVersion
import Network.AWS.WorkDocs.UpdateFolder
import Network.AWS.WorkDocs.UpdateUser
import Network.AWS.WorkDocs.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'WorkDocs'.

-- $operations
-- Some AWS operations return results that are incomplete and require subsequent
-- requests in order to obtain the entire result set. The process of sending
-- subsequent requests to continue where a previous request left off is called
-- pagination. For example, the 'ListObjects' operation of Amazon S3 returns up to
-- 1000 objects at a time, and you must send subsequent requests with the
-- appropriate Marker in order to retrieve the next page of results.
--
-- Operations that have an 'AWSPager' instance can transparently perform subsequent
-- requests, correctly setting Markers and other request facets to iterate through
-- the entire result set of a truncated API operation. Operations which support
-- this have an additional note in the documentation.
--
-- Many operations have the ability to filter results on the server side. See the
-- individual operation parameters for details.

-- $waiters
-- Waiters poll by repeatedly sending a request until some remote success condition
-- configured by the 'Wait' specification is fulfilled. The 'Wait' specification
-- determines how many attempts should be made, in addition to delay and retry strategies.
