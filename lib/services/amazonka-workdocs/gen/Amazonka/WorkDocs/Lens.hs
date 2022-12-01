{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.WorkDocs.Lens
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WorkDocs.Lens
  ( -- * Operations

    -- ** AbortDocumentVersionUpload
    abortDocumentVersionUpload_authenticationToken,
    abortDocumentVersionUpload_documentId,
    abortDocumentVersionUpload_versionId,

    -- ** ActivateUser
    activateUser_authenticationToken,
    activateUser_userId,
    activateUserResponse_user,
    activateUserResponse_httpStatus,

    -- ** AddResourcePermissions
    addResourcePermissions_notificationOptions,
    addResourcePermissions_authenticationToken,
    addResourcePermissions_resourceId,
    addResourcePermissions_principals,
    addResourcePermissionsResponse_shareResults,
    addResourcePermissionsResponse_httpStatus,

    -- ** CreateComment
    createComment_threadId,
    createComment_notifyCollaborators,
    createComment_visibility,
    createComment_parentId,
    createComment_authenticationToken,
    createComment_documentId,
    createComment_versionId,
    createComment_text,
    createCommentResponse_comment,
    createCommentResponse_httpStatus,

    -- ** CreateCustomMetadata
    createCustomMetadata_authenticationToken,
    createCustomMetadata_versionId,
    createCustomMetadata_resourceId,
    createCustomMetadata_customMetadata,
    createCustomMetadataResponse_httpStatus,

    -- ** CreateFolder
    createFolder_name,
    createFolder_authenticationToken,
    createFolder_parentFolderId,
    createFolderResponse_metadata,
    createFolderResponse_httpStatus,

    -- ** CreateLabels
    createLabels_authenticationToken,
    createLabels_resourceId,
    createLabels_labels,
    createLabelsResponse_httpStatus,

    -- ** CreateNotificationSubscription
    createNotificationSubscription_organizationId,
    createNotificationSubscription_endpoint,
    createNotificationSubscription_protocol,
    createNotificationSubscription_subscriptionType,
    createNotificationSubscriptionResponse_subscription,
    createNotificationSubscriptionResponse_httpStatus,

    -- ** CreateUser
    createUser_timeZoneId,
    createUser_storageRule,
    createUser_authenticationToken,
    createUser_organizationId,
    createUser_emailAddress,
    createUser_username,
    createUser_givenName,
    createUser_surname,
    createUser_password,
    createUserResponse_user,
    createUserResponse_httpStatus,

    -- ** DeactivateUser
    deactivateUser_authenticationToken,
    deactivateUser_userId,

    -- ** DeleteComment
    deleteComment_authenticationToken,
    deleteComment_documentId,
    deleteComment_versionId,
    deleteComment_commentId,

    -- ** DeleteCustomMetadata
    deleteCustomMetadata_deleteAll,
    deleteCustomMetadata_authenticationToken,
    deleteCustomMetadata_keys,
    deleteCustomMetadata_versionId,
    deleteCustomMetadata_resourceId,
    deleteCustomMetadataResponse_httpStatus,

    -- ** DeleteDocument
    deleteDocument_authenticationToken,
    deleteDocument_documentId,

    -- ** DeleteDocumentVersion
    deleteDocumentVersion_authenticationToken,
    deleteDocumentVersion_documentId,
    deleteDocumentVersion_versionId,
    deleteDocumentVersion_deletePriorVersions,

    -- ** DeleteFolder
    deleteFolder_authenticationToken,
    deleteFolder_folderId,

    -- ** DeleteFolderContents
    deleteFolderContents_authenticationToken,
    deleteFolderContents_folderId,

    -- ** DeleteLabels
    deleteLabels_deleteAll,
    deleteLabels_authenticationToken,
    deleteLabels_labels,
    deleteLabels_resourceId,
    deleteLabelsResponse_httpStatus,

    -- ** DeleteNotificationSubscription
    deleteNotificationSubscription_subscriptionId,
    deleteNotificationSubscription_organizationId,

    -- ** DeleteUser
    deleteUser_authenticationToken,
    deleteUser_userId,

    -- ** DescribeActivities
    describeActivities_resourceId,
    describeActivities_marker,
    describeActivities_authenticationToken,
    describeActivities_endTime,
    describeActivities_activityTypes,
    describeActivities_limit,
    describeActivities_userId,
    describeActivities_organizationId,
    describeActivities_includeIndirectActivities,
    describeActivities_startTime,
    describeActivitiesResponse_marker,
    describeActivitiesResponse_userActivities,
    describeActivitiesResponse_httpStatus,

    -- ** DescribeComments
    describeComments_marker,
    describeComments_authenticationToken,
    describeComments_limit,
    describeComments_documentId,
    describeComments_versionId,
    describeCommentsResponse_marker,
    describeCommentsResponse_comments,
    describeCommentsResponse_httpStatus,

    -- ** DescribeDocumentVersions
    describeDocumentVersions_marker,
    describeDocumentVersions_authenticationToken,
    describeDocumentVersions_fields,
    describeDocumentVersions_limit,
    describeDocumentVersions_include,
    describeDocumentVersions_documentId,
    describeDocumentVersionsResponse_marker,
    describeDocumentVersionsResponse_documentVersions,
    describeDocumentVersionsResponse_httpStatus,

    -- ** DescribeFolderContents
    describeFolderContents_type,
    describeFolderContents_marker,
    describeFolderContents_authenticationToken,
    describeFolderContents_sort,
    describeFolderContents_limit,
    describeFolderContents_order,
    describeFolderContents_include,
    describeFolderContents_folderId,
    describeFolderContentsResponse_marker,
    describeFolderContentsResponse_folders,
    describeFolderContentsResponse_documents,
    describeFolderContentsResponse_httpStatus,

    -- ** DescribeGroups
    describeGroups_marker,
    describeGroups_authenticationToken,
    describeGroups_limit,
    describeGroups_organizationId,
    describeGroups_searchQuery,
    describeGroupsResponse_marker,
    describeGroupsResponse_groups,
    describeGroupsResponse_httpStatus,

    -- ** DescribeNotificationSubscriptions
    describeNotificationSubscriptions_marker,
    describeNotificationSubscriptions_limit,
    describeNotificationSubscriptions_organizationId,
    describeNotificationSubscriptionsResponse_marker,
    describeNotificationSubscriptionsResponse_subscriptions,
    describeNotificationSubscriptionsResponse_httpStatus,

    -- ** DescribeResourcePermissions
    describeResourcePermissions_principalId,
    describeResourcePermissions_marker,
    describeResourcePermissions_authenticationToken,
    describeResourcePermissions_limit,
    describeResourcePermissions_resourceId,
    describeResourcePermissionsResponse_marker,
    describeResourcePermissionsResponse_principals,
    describeResourcePermissionsResponse_httpStatus,

    -- ** DescribeRootFolders
    describeRootFolders_marker,
    describeRootFolders_limit,
    describeRootFolders_authenticationToken,
    describeRootFoldersResponse_marker,
    describeRootFoldersResponse_folders,
    describeRootFoldersResponse_httpStatus,

    -- ** DescribeUsers
    describeUsers_marker,
    describeUsers_authenticationToken,
    describeUsers_fields,
    describeUsers_sort,
    describeUsers_limit,
    describeUsers_query,
    describeUsers_organizationId,
    describeUsers_order,
    describeUsers_include,
    describeUsers_userIds,
    describeUsersResponse_marker,
    describeUsersResponse_users,
    describeUsersResponse_totalNumberOfUsers,
    describeUsersResponse_httpStatus,

    -- ** GetCurrentUser
    getCurrentUser_authenticationToken,
    getCurrentUserResponse_user,
    getCurrentUserResponse_httpStatus,

    -- ** GetDocument
    getDocument_includeCustomMetadata,
    getDocument_authenticationToken,
    getDocument_documentId,
    getDocumentResponse_metadata,
    getDocumentResponse_customMetadata,
    getDocumentResponse_httpStatus,

    -- ** GetDocumentPath
    getDocumentPath_marker,
    getDocumentPath_authenticationToken,
    getDocumentPath_fields,
    getDocumentPath_limit,
    getDocumentPath_documentId,
    getDocumentPathResponse_path,
    getDocumentPathResponse_httpStatus,

    -- ** GetDocumentVersion
    getDocumentVersion_includeCustomMetadata,
    getDocumentVersion_authenticationToken,
    getDocumentVersion_fields,
    getDocumentVersion_documentId,
    getDocumentVersion_versionId,
    getDocumentVersionResponse_metadata,
    getDocumentVersionResponse_customMetadata,
    getDocumentVersionResponse_httpStatus,

    -- ** GetFolder
    getFolder_includeCustomMetadata,
    getFolder_authenticationToken,
    getFolder_folderId,
    getFolderResponse_metadata,
    getFolderResponse_customMetadata,
    getFolderResponse_httpStatus,

    -- ** GetFolderPath
    getFolderPath_marker,
    getFolderPath_authenticationToken,
    getFolderPath_fields,
    getFolderPath_limit,
    getFolderPath_folderId,
    getFolderPathResponse_path,
    getFolderPathResponse_httpStatus,

    -- ** GetResources
    getResources_marker,
    getResources_authenticationToken,
    getResources_limit,
    getResources_userId,
    getResources_collectionType,
    getResourcesResponse_marker,
    getResourcesResponse_folders,
    getResourcesResponse_documents,
    getResourcesResponse_httpStatus,

    -- ** InitiateDocumentVersionUpload
    initiateDocumentVersionUpload_name,
    initiateDocumentVersionUpload_parentFolderId,
    initiateDocumentVersionUpload_documentSizeInBytes,
    initiateDocumentVersionUpload_authenticationToken,
    initiateDocumentVersionUpload_contentCreatedTimestamp,
    initiateDocumentVersionUpload_id,
    initiateDocumentVersionUpload_contentModifiedTimestamp,
    initiateDocumentVersionUpload_contentType,
    initiateDocumentVersionUploadResponse_metadata,
    initiateDocumentVersionUploadResponse_uploadMetadata,
    initiateDocumentVersionUploadResponse_httpStatus,

    -- ** RemoveAllResourcePermissions
    removeAllResourcePermissions_authenticationToken,
    removeAllResourcePermissions_resourceId,

    -- ** RemoveResourcePermission
    removeResourcePermission_authenticationToken,
    removeResourcePermission_principalType,
    removeResourcePermission_resourceId,
    removeResourcePermission_principalId,

    -- ** RestoreDocumentVersions
    restoreDocumentVersions_authenticationToken,
    restoreDocumentVersions_documentId,

    -- ** UpdateDocument
    updateDocument_name,
    updateDocument_parentFolderId,
    updateDocument_authenticationToken,
    updateDocument_resourceState,
    updateDocument_documentId,

    -- ** UpdateDocumentVersion
    updateDocumentVersion_versionStatus,
    updateDocumentVersion_authenticationToken,
    updateDocumentVersion_documentId,
    updateDocumentVersion_versionId,

    -- ** UpdateFolder
    updateFolder_name,
    updateFolder_parentFolderId,
    updateFolder_authenticationToken,
    updateFolder_resourceState,
    updateFolder_folderId,

    -- ** UpdateUser
    updateUser_grantPoweruserPrivileges,
    updateUser_type,
    updateUser_timeZoneId,
    updateUser_storageRule,
    updateUser_locale,
    updateUser_givenName,
    updateUser_authenticationToken,
    updateUser_surname,
    updateUser_userId,
    updateUserResponse_user,
    updateUserResponse_httpStatus,

    -- * Types

    -- ** Activity
    activity_commentMetadata,
    activity_type,
    activity_resourceMetadata,
    activity_timeStamp,
    activity_isIndirectActivity,
    activity_organizationId,
    activity_originalParent,
    activity_initiator,
    activity_participants,

    -- ** Comment
    comment_createdTimestamp,
    comment_threadId,
    comment_visibility,
    comment_parentId,
    comment_status,
    comment_text,
    comment_contributor,
    comment_recipientId,
    comment_commentId,

    -- ** CommentMetadata
    commentMetadata_createdTimestamp,
    commentMetadata_commentStatus,
    commentMetadata_commentId,
    commentMetadata_contributor,
    commentMetadata_recipientId,

    -- ** DocumentMetadata
    documentMetadata_creatorId,
    documentMetadata_parentFolderId,
    documentMetadata_latestVersionMetadata,
    documentMetadata_createdTimestamp,
    documentMetadata_id,
    documentMetadata_labels,
    documentMetadata_resourceState,
    documentMetadata_modifiedTimestamp,

    -- ** DocumentVersionMetadata
    documentVersionMetadata_creatorId,
    documentVersionMetadata_name,
    documentVersionMetadata_createdTimestamp,
    documentVersionMetadata_thumbnail,
    documentVersionMetadata_size,
    documentVersionMetadata_contentCreatedTimestamp,
    documentVersionMetadata_status,
    documentVersionMetadata_id,
    documentVersionMetadata_contentModifiedTimestamp,
    documentVersionMetadata_source,
    documentVersionMetadata_signature,
    documentVersionMetadata_contentType,
    documentVersionMetadata_modifiedTimestamp,

    -- ** FolderMetadata
    folderMetadata_creatorId,
    folderMetadata_name,
    folderMetadata_parentFolderId,
    folderMetadata_createdTimestamp,
    folderMetadata_size,
    folderMetadata_id,
    folderMetadata_labels,
    folderMetadata_resourceState,
    folderMetadata_signature,
    folderMetadata_latestVersionSize,
    folderMetadata_modifiedTimestamp,

    -- ** GroupMetadata
    groupMetadata_name,
    groupMetadata_id,

    -- ** NotificationOptions
    notificationOptions_sendEmail,
    notificationOptions_emailMessage,

    -- ** Participants
    participants_users,
    participants_groups,

    -- ** PermissionInfo
    permissionInfo_type,
    permissionInfo_role,

    -- ** Principal
    principal_type,
    principal_id,
    principal_roles,

    -- ** ResourceMetadata
    resourceMetadata_name,
    resourceMetadata_type,
    resourceMetadata_originalName,
    resourceMetadata_parentId,
    resourceMetadata_owner,
    resourceMetadata_id,
    resourceMetadata_versionId,

    -- ** ResourcePath
    resourcePath_components,

    -- ** ResourcePathComponent
    resourcePathComponent_name,
    resourcePathComponent_id,

    -- ** SharePrincipal
    sharePrincipal_id,
    sharePrincipal_type,
    sharePrincipal_role,

    -- ** ShareResult
    shareResult_principalId,
    shareResult_status,
    shareResult_shareId,
    shareResult_role,
    shareResult_statusMessage,
    shareResult_inviteePrincipalId,

    -- ** StorageRuleType
    storageRuleType_storageAllocatedInBytes,
    storageRuleType_storageType,

    -- ** Subscription
    subscription_subscriptionId,
    subscription_protocol,
    subscription_endPoint,

    -- ** UploadMetadata
    uploadMetadata_signedHeaders,
    uploadMetadata_uploadUrl,

    -- ** User
    user_recycleBinFolderId,
    user_type,
    user_timeZoneId,
    user_createdTimestamp,
    user_username,
    user_locale,
    user_givenName,
    user_storage,
    user_status,
    user_id,
    user_surname,
    user_organizationId,
    user_emailAddress,
    user_rootFolderId,
    user_modifiedTimestamp,

    -- ** UserMetadata
    userMetadata_username,
    userMetadata_givenName,
    userMetadata_id,
    userMetadata_surname,
    userMetadata_emailAddress,

    -- ** UserStorageMetadata
    userStorageMetadata_storageUtilizedInBytes,
    userStorageMetadata_storageRule,
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
import Amazonka.WorkDocs.RemoveAllResourcePermissions
import Amazonka.WorkDocs.RemoveResourcePermission
import Amazonka.WorkDocs.RestoreDocumentVersions
import Amazonka.WorkDocs.Types.Activity
import Amazonka.WorkDocs.Types.Comment
import Amazonka.WorkDocs.Types.CommentMetadata
import Amazonka.WorkDocs.Types.DocumentMetadata
import Amazonka.WorkDocs.Types.DocumentVersionMetadata
import Amazonka.WorkDocs.Types.FolderMetadata
import Amazonka.WorkDocs.Types.GroupMetadata
import Amazonka.WorkDocs.Types.NotificationOptions
import Amazonka.WorkDocs.Types.Participants
import Amazonka.WorkDocs.Types.PermissionInfo
import Amazonka.WorkDocs.Types.Principal
import Amazonka.WorkDocs.Types.ResourceMetadata
import Amazonka.WorkDocs.Types.ResourcePath
import Amazonka.WorkDocs.Types.ResourcePathComponent
import Amazonka.WorkDocs.Types.SharePrincipal
import Amazonka.WorkDocs.Types.ShareResult
import Amazonka.WorkDocs.Types.StorageRuleType
import Amazonka.WorkDocs.Types.Subscription
import Amazonka.WorkDocs.Types.UploadMetadata
import Amazonka.WorkDocs.Types.User
import Amazonka.WorkDocs.Types.UserMetadata
import Amazonka.WorkDocs.Types.UserStorageMetadata
import Amazonka.WorkDocs.UpdateDocument
import Amazonka.WorkDocs.UpdateDocumentVersion
import Amazonka.WorkDocs.UpdateFolder
import Amazonka.WorkDocs.UpdateUser
