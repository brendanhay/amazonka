{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.WorkDocs
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2016-05-01@ of the AWS service descriptions, licensed under Apache 2.0.
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
--
-- The pricing for Amazon WorkDocs APIs varies depending on the API call
-- type for these actions:
--
-- -   @READ (Get*)@
--
-- -   @WRITE (Activate*, Add*, Create*, Deactivate*, Initiate*, Update*)@
--
-- -   @LIST (Describe*)@
--
-- -   @DELETE*, CANCEL@
--
-- For information about Amazon WorkDocs API pricing, see
-- <https://aws.amazon.com/workdocs/pricing/ Amazon WorkDocs Pricing>.
module Amazonka.WorkDocs
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** ConcurrentModificationException
    _ConcurrentModificationException,

    -- ** ConflictingOperationException
    _ConflictingOperationException,

    -- ** CustomMetadataLimitExceededException
    _CustomMetadataLimitExceededException,

    -- ** DeactivatingLastSystemUserException
    _DeactivatingLastSystemUserException,

    -- ** DocumentLockedForCommentsException
    _DocumentLockedForCommentsException,

    -- ** DraftUploadOutOfSyncException
    _DraftUploadOutOfSyncException,

    -- ** EntityAlreadyExistsException
    _EntityAlreadyExistsException,

    -- ** EntityNotExistsException
    _EntityNotExistsException,

    -- ** FailedDependencyException
    _FailedDependencyException,

    -- ** IllegalUserStateException
    _IllegalUserStateException,

    -- ** InvalidArgumentException
    _InvalidArgumentException,

    -- ** InvalidCommentOperationException
    _InvalidCommentOperationException,

    -- ** InvalidOperationException
    _InvalidOperationException,

    -- ** InvalidPasswordException
    _InvalidPasswordException,

    -- ** LimitExceededException
    _LimitExceededException,

    -- ** ProhibitedStateException
    _ProhibitedStateException,

    -- ** RequestedEntityTooLargeException
    _RequestedEntityTooLargeException,

    -- ** ResourceAlreadyCheckedOutException
    _ResourceAlreadyCheckedOutException,

    -- ** ServiceUnavailableException
    _ServiceUnavailableException,

    -- ** StorageLimitExceededException
    _StorageLimitExceededException,

    -- ** StorageLimitWillExceedException
    _StorageLimitWillExceedException,

    -- ** TooManyLabelsException
    _TooManyLabelsException,

    -- ** TooManySubscriptionsException
    _TooManySubscriptionsException,

    -- ** UnauthorizedOperationException
    _UnauthorizedOperationException,

    -- ** UnauthorizedResourceAccessException
    _UnauthorizedResourceAccessException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** AbortDocumentVersionUpload
    AbortDocumentVersionUpload (AbortDocumentVersionUpload'),
    newAbortDocumentVersionUpload,
    AbortDocumentVersionUploadResponse (AbortDocumentVersionUploadResponse'),
    newAbortDocumentVersionUploadResponse,

    -- ** ActivateUser
    ActivateUser (ActivateUser'),
    newActivateUser,
    ActivateUserResponse (ActivateUserResponse'),
    newActivateUserResponse,

    -- ** AddResourcePermissions
    AddResourcePermissions (AddResourcePermissions'),
    newAddResourcePermissions,
    AddResourcePermissionsResponse (AddResourcePermissionsResponse'),
    newAddResourcePermissionsResponse,

    -- ** CreateComment
    CreateComment (CreateComment'),
    newCreateComment,
    CreateCommentResponse (CreateCommentResponse'),
    newCreateCommentResponse,

    -- ** CreateCustomMetadata
    CreateCustomMetadata (CreateCustomMetadata'),
    newCreateCustomMetadata,
    CreateCustomMetadataResponse (CreateCustomMetadataResponse'),
    newCreateCustomMetadataResponse,

    -- ** CreateFolder
    CreateFolder (CreateFolder'),
    newCreateFolder,
    CreateFolderResponse (CreateFolderResponse'),
    newCreateFolderResponse,

    -- ** CreateLabels
    CreateLabels (CreateLabels'),
    newCreateLabels,
    CreateLabelsResponse (CreateLabelsResponse'),
    newCreateLabelsResponse,

    -- ** CreateNotificationSubscription
    CreateNotificationSubscription (CreateNotificationSubscription'),
    newCreateNotificationSubscription,
    CreateNotificationSubscriptionResponse (CreateNotificationSubscriptionResponse'),
    newCreateNotificationSubscriptionResponse,

    -- ** CreateUser
    CreateUser (CreateUser'),
    newCreateUser,
    CreateUserResponse (CreateUserResponse'),
    newCreateUserResponse,

    -- ** DeactivateUser
    DeactivateUser (DeactivateUser'),
    newDeactivateUser,
    DeactivateUserResponse (DeactivateUserResponse'),
    newDeactivateUserResponse,

    -- ** DeleteComment
    DeleteComment (DeleteComment'),
    newDeleteComment,
    DeleteCommentResponse (DeleteCommentResponse'),
    newDeleteCommentResponse,

    -- ** DeleteCustomMetadata
    DeleteCustomMetadata (DeleteCustomMetadata'),
    newDeleteCustomMetadata,
    DeleteCustomMetadataResponse (DeleteCustomMetadataResponse'),
    newDeleteCustomMetadataResponse,

    -- ** DeleteDocument
    DeleteDocument (DeleteDocument'),
    newDeleteDocument,
    DeleteDocumentResponse (DeleteDocumentResponse'),
    newDeleteDocumentResponse,

    -- ** DeleteDocumentVersion
    DeleteDocumentVersion (DeleteDocumentVersion'),
    newDeleteDocumentVersion,
    DeleteDocumentVersionResponse (DeleteDocumentVersionResponse'),
    newDeleteDocumentVersionResponse,

    -- ** DeleteFolder
    DeleteFolder (DeleteFolder'),
    newDeleteFolder,
    DeleteFolderResponse (DeleteFolderResponse'),
    newDeleteFolderResponse,

    -- ** DeleteFolderContents
    DeleteFolderContents (DeleteFolderContents'),
    newDeleteFolderContents,
    DeleteFolderContentsResponse (DeleteFolderContentsResponse'),
    newDeleteFolderContentsResponse,

    -- ** DeleteLabels
    DeleteLabels (DeleteLabels'),
    newDeleteLabels,
    DeleteLabelsResponse (DeleteLabelsResponse'),
    newDeleteLabelsResponse,

    -- ** DeleteNotificationSubscription
    DeleteNotificationSubscription (DeleteNotificationSubscription'),
    newDeleteNotificationSubscription,
    DeleteNotificationSubscriptionResponse (DeleteNotificationSubscriptionResponse'),
    newDeleteNotificationSubscriptionResponse,

    -- ** DeleteUser
    DeleteUser (DeleteUser'),
    newDeleteUser,
    DeleteUserResponse (DeleteUserResponse'),
    newDeleteUserResponse,

    -- ** DescribeActivities (Paginated)
    DescribeActivities (DescribeActivities'),
    newDescribeActivities,
    DescribeActivitiesResponse (DescribeActivitiesResponse'),
    newDescribeActivitiesResponse,

    -- ** DescribeComments (Paginated)
    DescribeComments (DescribeComments'),
    newDescribeComments,
    DescribeCommentsResponse (DescribeCommentsResponse'),
    newDescribeCommentsResponse,

    -- ** DescribeDocumentVersions (Paginated)
    DescribeDocumentVersions (DescribeDocumentVersions'),
    newDescribeDocumentVersions,
    DescribeDocumentVersionsResponse (DescribeDocumentVersionsResponse'),
    newDescribeDocumentVersionsResponse,

    -- ** DescribeFolderContents (Paginated)
    DescribeFolderContents (DescribeFolderContents'),
    newDescribeFolderContents,
    DescribeFolderContentsResponse (DescribeFolderContentsResponse'),
    newDescribeFolderContentsResponse,

    -- ** DescribeGroups (Paginated)
    DescribeGroups (DescribeGroups'),
    newDescribeGroups,
    DescribeGroupsResponse (DescribeGroupsResponse'),
    newDescribeGroupsResponse,

    -- ** DescribeNotificationSubscriptions (Paginated)
    DescribeNotificationSubscriptions (DescribeNotificationSubscriptions'),
    newDescribeNotificationSubscriptions,
    DescribeNotificationSubscriptionsResponse (DescribeNotificationSubscriptionsResponse'),
    newDescribeNotificationSubscriptionsResponse,

    -- ** DescribeResourcePermissions (Paginated)
    DescribeResourcePermissions (DescribeResourcePermissions'),
    newDescribeResourcePermissions,
    DescribeResourcePermissionsResponse (DescribeResourcePermissionsResponse'),
    newDescribeResourcePermissionsResponse,

    -- ** DescribeRootFolders (Paginated)
    DescribeRootFolders (DescribeRootFolders'),
    newDescribeRootFolders,
    DescribeRootFoldersResponse (DescribeRootFoldersResponse'),
    newDescribeRootFoldersResponse,

    -- ** DescribeUsers (Paginated)
    DescribeUsers (DescribeUsers'),
    newDescribeUsers,
    DescribeUsersResponse (DescribeUsersResponse'),
    newDescribeUsersResponse,

    -- ** GetCurrentUser
    GetCurrentUser (GetCurrentUser'),
    newGetCurrentUser,
    GetCurrentUserResponse (GetCurrentUserResponse'),
    newGetCurrentUserResponse,

    -- ** GetDocument
    GetDocument (GetDocument'),
    newGetDocument,
    GetDocumentResponse (GetDocumentResponse'),
    newGetDocumentResponse,

    -- ** GetDocumentPath
    GetDocumentPath (GetDocumentPath'),
    newGetDocumentPath,
    GetDocumentPathResponse (GetDocumentPathResponse'),
    newGetDocumentPathResponse,

    -- ** GetDocumentVersion
    GetDocumentVersion (GetDocumentVersion'),
    newGetDocumentVersion,
    GetDocumentVersionResponse (GetDocumentVersionResponse'),
    newGetDocumentVersionResponse,

    -- ** GetFolder
    GetFolder (GetFolder'),
    newGetFolder,
    GetFolderResponse (GetFolderResponse'),
    newGetFolderResponse,

    -- ** GetFolderPath
    GetFolderPath (GetFolderPath'),
    newGetFolderPath,
    GetFolderPathResponse (GetFolderPathResponse'),
    newGetFolderPathResponse,

    -- ** GetResources
    GetResources (GetResources'),
    newGetResources,
    GetResourcesResponse (GetResourcesResponse'),
    newGetResourcesResponse,

    -- ** InitiateDocumentVersionUpload
    InitiateDocumentVersionUpload (InitiateDocumentVersionUpload'),
    newInitiateDocumentVersionUpload,
    InitiateDocumentVersionUploadResponse (InitiateDocumentVersionUploadResponse'),
    newInitiateDocumentVersionUploadResponse,

    -- ** RemoveAllResourcePermissions
    RemoveAllResourcePermissions (RemoveAllResourcePermissions'),
    newRemoveAllResourcePermissions,
    RemoveAllResourcePermissionsResponse (RemoveAllResourcePermissionsResponse'),
    newRemoveAllResourcePermissionsResponse,

    -- ** RemoveResourcePermission
    RemoveResourcePermission (RemoveResourcePermission'),
    newRemoveResourcePermission,
    RemoveResourcePermissionResponse (RemoveResourcePermissionResponse'),
    newRemoveResourcePermissionResponse,

    -- ** RestoreDocumentVersions
    RestoreDocumentVersions (RestoreDocumentVersions'),
    newRestoreDocumentVersions,
    RestoreDocumentVersionsResponse (RestoreDocumentVersionsResponse'),
    newRestoreDocumentVersionsResponse,

    -- ** UpdateDocument
    UpdateDocument (UpdateDocument'),
    newUpdateDocument,
    UpdateDocumentResponse (UpdateDocumentResponse'),
    newUpdateDocumentResponse,

    -- ** UpdateDocumentVersion
    UpdateDocumentVersion (UpdateDocumentVersion'),
    newUpdateDocumentVersion,
    UpdateDocumentVersionResponse (UpdateDocumentVersionResponse'),
    newUpdateDocumentVersionResponse,

    -- ** UpdateFolder
    UpdateFolder (UpdateFolder'),
    newUpdateFolder,
    UpdateFolderResponse (UpdateFolderResponse'),
    newUpdateFolderResponse,

    -- ** UpdateUser
    UpdateUser (UpdateUser'),
    newUpdateUser,
    UpdateUserResponse (UpdateUserResponse'),
    newUpdateUserResponse,

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

import Amazonka.WorkDocs.AbortDocumentVersionUpload
import Amazonka.WorkDocs.ActivateUser
import Amazonka.WorkDocs.AddResourcePermissions
import Amazonka.WorkDocs.CreateComment
import Amazonka.WorkDocs.CreateCustomMetadata
import Amazonka.WorkDocs.CreateFolder
import Amazonka.WorkDocs.CreateLabels
import Amazonka.WorkDocs.CreateNotificationSubscription
import Amazonka.WorkDocs.CreateUser
import Amazonka.WorkDocs.DeactivateUser
import Amazonka.WorkDocs.DeleteComment
import Amazonka.WorkDocs.DeleteCustomMetadata
import Amazonka.WorkDocs.DeleteDocument
import Amazonka.WorkDocs.DeleteDocumentVersion
import Amazonka.WorkDocs.DeleteFolder
import Amazonka.WorkDocs.DeleteFolderContents
import Amazonka.WorkDocs.DeleteLabels
import Amazonka.WorkDocs.DeleteNotificationSubscription
import Amazonka.WorkDocs.DeleteUser
import Amazonka.WorkDocs.DescribeActivities
import Amazonka.WorkDocs.DescribeComments
import Amazonka.WorkDocs.DescribeDocumentVersions
import Amazonka.WorkDocs.DescribeFolderContents
import Amazonka.WorkDocs.DescribeGroups
import Amazonka.WorkDocs.DescribeNotificationSubscriptions
import Amazonka.WorkDocs.DescribeResourcePermissions
import Amazonka.WorkDocs.DescribeRootFolders
import Amazonka.WorkDocs.DescribeUsers
import Amazonka.WorkDocs.GetCurrentUser
import Amazonka.WorkDocs.GetDocument
import Amazonka.WorkDocs.GetDocumentPath
import Amazonka.WorkDocs.GetDocumentVersion
import Amazonka.WorkDocs.GetFolder
import Amazonka.WorkDocs.GetFolderPath
import Amazonka.WorkDocs.GetResources
import Amazonka.WorkDocs.InitiateDocumentVersionUpload
import Amazonka.WorkDocs.Lens
import Amazonka.WorkDocs.RemoveAllResourcePermissions
import Amazonka.WorkDocs.RemoveResourcePermission
import Amazonka.WorkDocs.RestoreDocumentVersions
import Amazonka.WorkDocs.Types
import Amazonka.WorkDocs.UpdateDocument
import Amazonka.WorkDocs.UpdateDocumentVersion
import Amazonka.WorkDocs.UpdateFolder
import Amazonka.WorkDocs.UpdateUser
import Amazonka.WorkDocs.Waiters

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
