{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkDocs.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkDocs.Lens
  ( -- * Operations

    -- ** DeleteFolder
    deleteFolder_authenticationToken,
    deleteFolder_folderId,

    -- ** UpdateFolder
    updateFolder_parentFolderId,
    updateFolder_name,
    updateFolder_authenticationToken,
    updateFolder_resourceState,
    updateFolder_folderId,

    -- ** DeleteCustomMetadata
    deleteCustomMetadata_versionId,
    deleteCustomMetadata_keys,
    deleteCustomMetadata_authenticationToken,
    deleteCustomMetadata_deleteAll,
    deleteCustomMetadata_resourceId,
    deleteCustomMetadataResponse_httpStatus,

    -- ** DeleteNotificationSubscription
    deleteNotificationSubscription_subscriptionId,
    deleteNotificationSubscription_organizationId,

    -- ** UpdateDocumentVersion
    updateDocumentVersion_versionStatus,
    updateDocumentVersion_authenticationToken,
    updateDocumentVersion_documentId,
    updateDocumentVersion_versionId,

    -- ** DeleteLabels
    deleteLabels_labels,
    deleteLabels_authenticationToken,
    deleteLabels_deleteAll,
    deleteLabels_resourceId,
    deleteLabelsResponse_httpStatus,

    -- ** AbortDocumentVersionUpload
    abortDocumentVersionUpload_authenticationToken,
    abortDocumentVersionUpload_documentId,
    abortDocumentVersionUpload_versionId,

    -- ** DescribeFolderContents
    describeFolderContents_include,
    describeFolderContents_order,
    describeFolderContents_authenticationToken,
    describeFolderContents_type,
    describeFolderContents_limit,
    describeFolderContents_sort,
    describeFolderContents_marker,
    describeFolderContents_folderId,
    describeFolderContentsResponse_documents,
    describeFolderContentsResponse_folders,
    describeFolderContentsResponse_marker,
    describeFolderContentsResponse_httpStatus,

    -- ** CreateLabels
    createLabels_authenticationToken,
    createLabels_resourceId,
    createLabels_labels,
    createLabelsResponse_httpStatus,

    -- ** DeactivateUser
    deactivateUser_authenticationToken,
    deactivateUser_userId,

    -- ** DescribeRootFolders
    describeRootFolders_limit,
    describeRootFolders_marker,
    describeRootFolders_authenticationToken,
    describeRootFoldersResponse_folders,
    describeRootFoldersResponse_marker,
    describeRootFoldersResponse_httpStatus,

    -- ** UpdateDocument
    updateDocument_parentFolderId,
    updateDocument_name,
    updateDocument_authenticationToken,
    updateDocument_resourceState,
    updateDocument_documentId,

    -- ** DeleteDocument
    deleteDocument_authenticationToken,
    deleteDocument_documentId,

    -- ** GetDocumentVersion
    getDocumentVersion_includeCustomMetadata,
    getDocumentVersion_fields,
    getDocumentVersion_authenticationToken,
    getDocumentVersion_documentId,
    getDocumentVersion_versionId,
    getDocumentVersionResponse_metadata,
    getDocumentVersionResponse_customMetadata,
    getDocumentVersionResponse_httpStatus,

    -- ** DescribeDocumentVersions
    describeDocumentVersions_include,
    describeDocumentVersions_fields,
    describeDocumentVersions_authenticationToken,
    describeDocumentVersions_limit,
    describeDocumentVersions_marker,
    describeDocumentVersions_documentId,
    describeDocumentVersionsResponse_documentVersions,
    describeDocumentVersionsResponse_marker,
    describeDocumentVersionsResponse_httpStatus,

    -- ** ActivateUser
    activateUser_authenticationToken,
    activateUser_userId,
    activateUserResponse_user,
    activateUserResponse_httpStatus,

    -- ** GetFolderPath
    getFolderPath_fields,
    getFolderPath_authenticationToken,
    getFolderPath_limit,
    getFolderPath_marker,
    getFolderPath_folderId,
    getFolderPathResponse_path,
    getFolderPathResponse_httpStatus,

    -- ** CreateUser
    createUser_storageRule,
    createUser_organizationId,
    createUser_timeZoneId,
    createUser_authenticationToken,
    createUser_emailAddress,
    createUser_username,
    createUser_givenName,
    createUser_surname,
    createUser_password,
    createUserResponse_user,
    createUserResponse_httpStatus,

    -- ** CreateCustomMetadata
    createCustomMetadata_versionId,
    createCustomMetadata_authenticationToken,
    createCustomMetadata_resourceId,
    createCustomMetadata_customMetadata,
    createCustomMetadataResponse_httpStatus,

    -- ** DeleteComment
    deleteComment_authenticationToken,
    deleteComment_documentId,
    deleteComment_versionId,
    deleteComment_commentId,

    -- ** CreateFolder
    createFolder_name,
    createFolder_authenticationToken,
    createFolder_parentFolderId,
    createFolderResponse_metadata,
    createFolderResponse_httpStatus,

    -- ** CreateNotificationSubscription
    createNotificationSubscription_organizationId,
    createNotificationSubscription_endpoint,
    createNotificationSubscription_protocol,
    createNotificationSubscription_subscriptionType,
    createNotificationSubscriptionResponse_subscription,
    createNotificationSubscriptionResponse_httpStatus,

    -- ** CreateComment
    createComment_parentId,
    createComment_visibility,
    createComment_authenticationToken,
    createComment_threadId,
    createComment_notifyCollaborators,
    createComment_documentId,
    createComment_versionId,
    createComment_text,
    createCommentResponse_comment,
    createCommentResponse_httpStatus,

    -- ** DescribeResourcePermissions
    describeResourcePermissions_principalId,
    describeResourcePermissions_authenticationToken,
    describeResourcePermissions_limit,
    describeResourcePermissions_marker,
    describeResourcePermissions_resourceId,
    describeResourcePermissionsResponse_principals,
    describeResourcePermissionsResponse_marker,
    describeResourcePermissionsResponse_httpStatus,

    -- ** RemoveResourcePermission
    removeResourcePermission_authenticationToken,
    removeResourcePermission_principalType,
    removeResourcePermission_resourceId,
    removeResourcePermission_principalId,

    -- ** DescribeUsers
    describeUsers_organizationId,
    describeUsers_query,
    describeUsers_userIds,
    describeUsers_include,
    describeUsers_fields,
    describeUsers_order,
    describeUsers_authenticationToken,
    describeUsers_limit,
    describeUsers_sort,
    describeUsers_marker,
    describeUsersResponse_totalNumberOfUsers,
    describeUsersResponse_users,
    describeUsersResponse_marker,
    describeUsersResponse_httpStatus,

    -- ** GetResources
    getResources_collectionType,
    getResources_userId,
    getResources_authenticationToken,
    getResources_limit,
    getResources_marker,
    getResourcesResponse_documents,
    getResourcesResponse_folders,
    getResourcesResponse_marker,
    getResourcesResponse_httpStatus,

    -- ** GetDocumentPath
    getDocumentPath_fields,
    getDocumentPath_authenticationToken,
    getDocumentPath_limit,
    getDocumentPath_marker,
    getDocumentPath_documentId,
    getDocumentPathResponse_path,
    getDocumentPathResponse_httpStatus,

    -- ** DescribeGroups
    describeGroups_organizationId,
    describeGroups_authenticationToken,
    describeGroups_limit,
    describeGroups_marker,
    describeGroups_searchQuery,
    describeGroupsResponse_groups,
    describeGroupsResponse_marker,
    describeGroupsResponse_httpStatus,

    -- ** GetDocument
    getDocument_includeCustomMetadata,
    getDocument_authenticationToken,
    getDocument_documentId,
    getDocumentResponse_metadata,
    getDocumentResponse_customMetadata,
    getDocumentResponse_httpStatus,

    -- ** DescribeActivities
    describeActivities_resourceId,
    describeActivities_organizationId,
    describeActivities_startTime,
    describeActivities_includeIndirectActivities,
    describeActivities_endTime,
    describeActivities_userId,
    describeActivities_activityTypes,
    describeActivities_authenticationToken,
    describeActivities_limit,
    describeActivities_marker,
    describeActivitiesResponse_userActivities,
    describeActivitiesResponse_marker,
    describeActivitiesResponse_httpStatus,

    -- ** GetCurrentUser
    getCurrentUser_authenticationToken,
    getCurrentUserResponse_user,
    getCurrentUserResponse_httpStatus,

    -- ** AddResourcePermissions
    addResourcePermissions_notificationOptions,
    addResourcePermissions_authenticationToken,
    addResourcePermissions_resourceId,
    addResourcePermissions_principals,
    addResourcePermissionsResponse_shareResults,
    addResourcePermissionsResponse_httpStatus,

    -- ** DeleteUser
    deleteUser_authenticationToken,
    deleteUser_userId,

    -- ** GetFolder
    getFolder_includeCustomMetadata,
    getFolder_authenticationToken,
    getFolder_folderId,
    getFolderResponse_metadata,
    getFolderResponse_customMetadata,
    getFolderResponse_httpStatus,

    -- ** UpdateUser
    updateUser_storageRule,
    updateUser_grantPoweruserPrivileges,
    updateUser_timeZoneId,
    updateUser_surname,
    updateUser_locale,
    updateUser_givenName,
    updateUser_authenticationToken,
    updateUser_type,
    updateUser_userId,
    updateUserResponse_user,
    updateUserResponse_httpStatus,

    -- ** DescribeNotificationSubscriptions
    describeNotificationSubscriptions_limit,
    describeNotificationSubscriptions_marker,
    describeNotificationSubscriptions_organizationId,
    describeNotificationSubscriptionsResponse_subscriptions,
    describeNotificationSubscriptionsResponse_marker,
    describeNotificationSubscriptionsResponse_httpStatus,

    -- ** RemoveAllResourcePermissions
    removeAllResourcePermissions_authenticationToken,
    removeAllResourcePermissions_resourceId,

    -- ** DeleteFolderContents
    deleteFolderContents_authenticationToken,
    deleteFolderContents_folderId,

    -- ** DescribeComments
    describeComments_authenticationToken,
    describeComments_limit,
    describeComments_marker,
    describeComments_documentId,
    describeComments_versionId,
    describeCommentsResponse_comments,
    describeCommentsResponse_marker,
    describeCommentsResponse_httpStatus,

    -- ** InitiateDocumentVersionUpload
    initiateDocumentVersionUpload_contentType,
    initiateDocumentVersionUpload_contentModifiedTimestamp,
    initiateDocumentVersionUpload_id,
    initiateDocumentVersionUpload_contentCreatedTimestamp,
    initiateDocumentVersionUpload_name,
    initiateDocumentVersionUpload_documentSizeInBytes,
    initiateDocumentVersionUpload_authenticationToken,
    initiateDocumentVersionUpload_parentFolderId,
    initiateDocumentVersionUploadResponse_uploadMetadata,
    initiateDocumentVersionUploadResponse_metadata,
    initiateDocumentVersionUploadResponse_httpStatus,

    -- * Types

    -- ** Activity
    activity_resourceMetadata,
    activity_organizationId,
    activity_originalParent,
    activity_participants,
    activity_commentMetadata,
    activity_timeStamp,
    activity_initiator,
    activity_type,
    activity_isIndirectActivity,

    -- ** Comment
    comment_status,
    comment_createdTimestamp,
    comment_contributor,
    comment_parentId,
    comment_recipientId,
    comment_visibility,
    comment_threadId,
    comment_text,
    comment_commentId,

    -- ** CommentMetadata
    commentMetadata_commentStatus,
    commentMetadata_createdTimestamp,
    commentMetadata_contributor,
    commentMetadata_recipientId,
    commentMetadata_commentId,

    -- ** DocumentMetadata
    documentMetadata_modifiedTimestamp,
    documentMetadata_parentFolderId,
    documentMetadata_creatorId,
    documentMetadata_createdTimestamp,
    documentMetadata_id,
    documentMetadata_labels,
    documentMetadata_latestVersionMetadata,
    documentMetadata_resourceState,

    -- ** DocumentVersionMetadata
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

    -- ** FolderMetadata
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

    -- ** GroupMetadata
    groupMetadata_id,
    groupMetadata_name,

    -- ** NotificationOptions
    notificationOptions_sendEmail,
    notificationOptions_emailMessage,

    -- ** Participants
    participants_groups,
    participants_users,

    -- ** PermissionInfo
    permissionInfo_role,
    permissionInfo_type,

    -- ** Principal
    principal_id,
    principal_roles,
    principal_type,

    -- ** ResourceMetadata
    resourceMetadata_originalName,
    resourceMetadata_id,
    resourceMetadata_versionId,
    resourceMetadata_name,
    resourceMetadata_parentId,
    resourceMetadata_owner,
    resourceMetadata_type,

    -- ** ResourcePath
    resourcePath_components,

    -- ** ResourcePathComponent
    resourcePathComponent_id,
    resourcePathComponent_name,

    -- ** SharePrincipal
    sharePrincipal_id,
    sharePrincipal_type,
    sharePrincipal_role,

    -- ** ShareResult
    shareResult_statusMessage,
    shareResult_status,
    shareResult_inviteePrincipalId,
    shareResult_shareId,
    shareResult_principalId,
    shareResult_role,

    -- ** StorageRuleType
    storageRuleType_storageType,
    storageRuleType_storageAllocatedInBytes,

    -- ** Subscription
    subscription_subscriptionId,
    subscription_protocol,
    subscription_endPoint,

    -- ** UploadMetadata
    uploadMetadata_signedHeaders,
    uploadMetadata_uploadUrl,

    -- ** User
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

    -- ** UserMetadata
    userMetadata_surname,
    userMetadata_id,
    userMetadata_givenName,
    userMetadata_username,
    userMetadata_emailAddress,

    -- ** UserStorageMetadata
    userStorageMetadata_storageRule,
    userStorageMetadata_storageUtilizedInBytes,
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
import Network.AWS.WorkDocs.RemoveAllResourcePermissions
import Network.AWS.WorkDocs.RemoveResourcePermission
import Network.AWS.WorkDocs.Types.Activity
import Network.AWS.WorkDocs.Types.Comment
import Network.AWS.WorkDocs.Types.CommentMetadata
import Network.AWS.WorkDocs.Types.DocumentMetadata
import Network.AWS.WorkDocs.Types.DocumentVersionMetadata
import Network.AWS.WorkDocs.Types.FolderMetadata
import Network.AWS.WorkDocs.Types.GroupMetadata
import Network.AWS.WorkDocs.Types.NotificationOptions
import Network.AWS.WorkDocs.Types.Participants
import Network.AWS.WorkDocs.Types.PermissionInfo
import Network.AWS.WorkDocs.Types.Principal
import Network.AWS.WorkDocs.Types.ResourceMetadata
import Network.AWS.WorkDocs.Types.ResourcePath
import Network.AWS.WorkDocs.Types.ResourcePathComponent
import Network.AWS.WorkDocs.Types.SharePrincipal
import Network.AWS.WorkDocs.Types.ShareResult
import Network.AWS.WorkDocs.Types.StorageRuleType
import Network.AWS.WorkDocs.Types.Subscription
import Network.AWS.WorkDocs.Types.UploadMetadata
import Network.AWS.WorkDocs.Types.User
import Network.AWS.WorkDocs.Types.UserMetadata
import Network.AWS.WorkDocs.Types.UserStorageMetadata
import Network.AWS.WorkDocs.UpdateDocument
import Network.AWS.WorkDocs.UpdateDocumentVersion
import Network.AWS.WorkDocs.UpdateFolder
import Network.AWS.WorkDocs.UpdateUser
