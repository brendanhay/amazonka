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

    -- ** DeleteLabels
    deleteLabels_deleteAll,
    deleteLabels_authenticationToken,
    deleteLabels_labels,
    deleteLabels_resourceId,
    deleteLabelsResponse_httpStatus,

    -- ** AbortDocumentVersionUpload
    abortDocumentVersionUpload_authenticationToken,
    abortDocumentVersionUpload_documentId,
    abortDocumentVersionUpload_versionId,

    -- ** GetDocumentPath
    getDocumentPath_authenticationToken,
    getDocumentPath_marker,
    getDocumentPath_limit,
    getDocumentPath_fields,
    getDocumentPath_documentId,
    getDocumentPathResponse_path,
    getDocumentPathResponse_httpStatus,

    -- ** CreateComment
    createComment_notifyCollaborators,
    createComment_authenticationToken,
    createComment_visibility,
    createComment_threadId,
    createComment_parentId,
    createComment_documentId,
    createComment_versionId,
    createComment_text,
    createCommentResponse_comment,
    createCommentResponse_httpStatus,

    -- ** DescribeUsers
    describeUsers_include,
    describeUsers_userIds,
    describeUsers_authenticationToken,
    describeUsers_sort,
    describeUsers_marker,
    describeUsers_query,
    describeUsers_limit,
    describeUsers_order,
    describeUsers_organizationId,
    describeUsers_fields,
    describeUsersResponse_users,
    describeUsersResponse_totalNumberOfUsers,
    describeUsersResponse_marker,
    describeUsersResponse_httpStatus,

    -- ** DeleteFolder
    deleteFolder_authenticationToken,
    deleteFolder_folderId,

    -- ** UpdateFolder
    updateFolder_parentFolderId,
    updateFolder_authenticationToken,
    updateFolder_name,
    updateFolder_resourceState,
    updateFolder_folderId,

    -- ** DeleteCustomMetadata
    deleteCustomMetadata_versionId,
    deleteCustomMetadata_deleteAll,
    deleteCustomMetadata_authenticationToken,
    deleteCustomMetadata_keys,
    deleteCustomMetadata_resourceId,
    deleteCustomMetadataResponse_httpStatus,

    -- ** DescribeResourcePermissions
    describeResourcePermissions_principalId,
    describeResourcePermissions_authenticationToken,
    describeResourcePermissions_marker,
    describeResourcePermissions_limit,
    describeResourcePermissions_resourceId,
    describeResourcePermissionsResponse_principals,
    describeResourcePermissionsResponse_marker,
    describeResourcePermissionsResponse_httpStatus,

    -- ** DeleteNotificationSubscription
    deleteNotificationSubscription_subscriptionId,
    deleteNotificationSubscription_organizationId,

    -- ** CreateFolder
    createFolder_authenticationToken,
    createFolder_name,
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

    -- ** CreateCustomMetadata
    createCustomMetadata_versionId,
    createCustomMetadata_authenticationToken,
    createCustomMetadata_resourceId,
    createCustomMetadata_customMetadata,
    createCustomMetadataResponse_httpStatus,

    -- ** GetFolderPath
    getFolderPath_authenticationToken,
    getFolderPath_marker,
    getFolderPath_limit,
    getFolderPath_fields,
    getFolderPath_folderId,
    getFolderPathResponse_path,
    getFolderPathResponse_httpStatus,

    -- ** DescribeComments
    describeComments_authenticationToken,
    describeComments_marker,
    describeComments_limit,
    describeComments_documentId,
    describeComments_versionId,
    describeCommentsResponse_marker,
    describeCommentsResponse_comments,
    describeCommentsResponse_httpStatus,

    -- ** DeleteFolderContents
    deleteFolderContents_authenticationToken,
    deleteFolderContents_folderId,

    -- ** RemoveAllResourcePermissions
    removeAllResourcePermissions_authenticationToken,
    removeAllResourcePermissions_resourceId,

    -- ** GetFolder
    getFolder_authenticationToken,
    getFolder_includeCustomMetadata,
    getFolder_folderId,
    getFolderResponse_customMetadata,
    getFolderResponse_metadata,
    getFolderResponse_httpStatus,

    -- ** DescribeNotificationSubscriptions
    describeNotificationSubscriptions_marker,
    describeNotificationSubscriptions_limit,
    describeNotificationSubscriptions_organizationId,
    describeNotificationSubscriptionsResponse_marker,
    describeNotificationSubscriptionsResponse_subscriptions,
    describeNotificationSubscriptionsResponse_httpStatus,

    -- ** ActivateUser
    activateUser_authenticationToken,
    activateUser_userId,
    activateUserResponse_user,
    activateUserResponse_httpStatus,

    -- ** DescribeDocumentVersions
    describeDocumentVersions_include,
    describeDocumentVersions_authenticationToken,
    describeDocumentVersions_marker,
    describeDocumentVersions_limit,
    describeDocumentVersions_fields,
    describeDocumentVersions_documentId,
    describeDocumentVersionsResponse_documentVersions,
    describeDocumentVersionsResponse_marker,
    describeDocumentVersionsResponse_httpStatus,

    -- ** GetDocumentVersion
    getDocumentVersion_authenticationToken,
    getDocumentVersion_includeCustomMetadata,
    getDocumentVersion_fields,
    getDocumentVersion_documentId,
    getDocumentVersion_versionId,
    getDocumentVersionResponse_customMetadata,
    getDocumentVersionResponse_metadata,
    getDocumentVersionResponse_httpStatus,

    -- ** DescribeActivities
    describeActivities_resourceId,
    describeActivities_includeIndirectActivities,
    describeActivities_startTime,
    describeActivities_authenticationToken,
    describeActivities_userId,
    describeActivities_marker,
    describeActivities_endTime,
    describeActivities_limit,
    describeActivities_activityTypes,
    describeActivities_organizationId,
    describeActivitiesResponse_userActivities,
    describeActivitiesResponse_marker,
    describeActivitiesResponse_httpStatus,

    -- ** DescribeRootFolders
    describeRootFolders_marker,
    describeRootFolders_limit,
    describeRootFolders_authenticationToken,
    describeRootFoldersResponse_folders,
    describeRootFoldersResponse_marker,
    describeRootFoldersResponse_httpStatus,

    -- ** GetCurrentUser
    getCurrentUser_authenticationToken,
    getCurrentUserResponse_user,
    getCurrentUserResponse_httpStatus,

    -- ** DeactivateUser
    deactivateUser_authenticationToken,
    deactivateUser_userId,

    -- ** GetDocument
    getDocument_authenticationToken,
    getDocument_includeCustomMetadata,
    getDocument_documentId,
    getDocumentResponse_customMetadata,
    getDocumentResponse_metadata,
    getDocumentResponse_httpStatus,

    -- ** DescribeFolderContents
    describeFolderContents_include,
    describeFolderContents_authenticationToken,
    describeFolderContents_sort,
    describeFolderContents_marker,
    describeFolderContents_limit,
    describeFolderContents_type,
    describeFolderContents_order,
    describeFolderContents_folderId,
    describeFolderContentsResponse_folders,
    describeFolderContentsResponse_documents,
    describeFolderContentsResponse_marker,
    describeFolderContentsResponse_httpStatus,

    -- ** CreateLabels
    createLabels_authenticationToken,
    createLabels_resourceId,
    createLabels_labels,
    createLabelsResponse_httpStatus,

    -- ** UpdateDocumentVersion
    updateDocumentVersion_authenticationToken,
    updateDocumentVersion_versionStatus,
    updateDocumentVersion_documentId,
    updateDocumentVersion_versionId,

    -- ** RemoveResourcePermission
    removeResourcePermission_principalType,
    removeResourcePermission_authenticationToken,
    removeResourcePermission_resourceId,
    removeResourcePermission_principalId,

    -- ** GetResources
    getResources_authenticationToken,
    getResources_userId,
    getResources_marker,
    getResources_limit,
    getResources_collectionType,
    getResourcesResponse_folders,
    getResourcesResponse_documents,
    getResourcesResponse_marker,
    getResourcesResponse_httpStatus,

    -- ** DeleteComment
    deleteComment_authenticationToken,
    deleteComment_documentId,
    deleteComment_versionId,
    deleteComment_commentId,

    -- ** InitiateDocumentVersionUpload
    initiateDocumentVersionUpload_documentSizeInBytes,
    initiateDocumentVersionUpload_contentCreatedTimestamp,
    initiateDocumentVersionUpload_authenticationToken,
    initiateDocumentVersionUpload_name,
    initiateDocumentVersionUpload_id,
    initiateDocumentVersionUpload_contentModifiedTimestamp,
    initiateDocumentVersionUpload_contentType,
    initiateDocumentVersionUpload_parentFolderId,
    initiateDocumentVersionUploadResponse_metadata,
    initiateDocumentVersionUploadResponse_uploadMetadata,
    initiateDocumentVersionUploadResponse_httpStatus,

    -- ** CreateUser
    createUser_authenticationToken,
    createUser_storageRule,
    createUser_emailAddress,
    createUser_timeZoneId,
    createUser_organizationId,
    createUser_username,
    createUser_givenName,
    createUser_surname,
    createUser_password,
    createUserResponse_user,
    createUserResponse_httpStatus,

    -- ** UpdateUser
    updateUser_givenName,
    updateUser_grantPoweruserPrivileges,
    updateUser_locale,
    updateUser_authenticationToken,
    updateUser_storageRule,
    updateUser_type,
    updateUser_surname,
    updateUser_timeZoneId,
    updateUser_userId,
    updateUserResponse_user,
    updateUserResponse_httpStatus,

    -- ** DeleteUser
    deleteUser_authenticationToken,
    deleteUser_userId,

    -- ** AddResourcePermissions
    addResourcePermissions_notificationOptions,
    addResourcePermissions_authenticationToken,
    addResourcePermissions_resourceId,
    addResourcePermissions_principals,
    addResourcePermissionsResponse_shareResults,
    addResourcePermissionsResponse_httpStatus,

    -- ** UpdateDocument
    updateDocument_parentFolderId,
    updateDocument_authenticationToken,
    updateDocument_name,
    updateDocument_resourceState,
    updateDocument_documentId,

    -- ** DeleteDocument
    deleteDocument_authenticationToken,
    deleteDocument_documentId,

    -- ** DescribeGroups
    describeGroups_authenticationToken,
    describeGroups_marker,
    describeGroups_limit,
    describeGroups_organizationId,
    describeGroups_searchQuery,
    describeGroupsResponse_groups,
    describeGroupsResponse_marker,
    describeGroupsResponse_httpStatus,

    -- * Types

    -- ** Activity
    activity_resourceMetadata,
    activity_isIndirectActivity,
    activity_initiator,
    activity_participants,
    activity_originalParent,
    activity_type,
    activity_commentMetadata,
    activity_timeStamp,
    activity_organizationId,

    -- ** Comment
    comment_status,
    comment_text,
    comment_visibility,
    comment_threadId,
    comment_contributor,
    comment_createdTimestamp,
    comment_recipientId,
    comment_parentId,
    comment_commentId,

    -- ** CommentMetadata
    commentMetadata_commentStatus,
    commentMetadata_contributor,
    commentMetadata_commentId,
    commentMetadata_createdTimestamp,
    commentMetadata_recipientId,

    -- ** DocumentMetadata
    documentMetadata_latestVersionMetadata,
    documentMetadata_parentFolderId,
    documentMetadata_modifiedTimestamp,
    documentMetadata_id,
    documentMetadata_labels,
    documentMetadata_resourceState,
    documentMetadata_createdTimestamp,
    documentMetadata_creatorId,

    -- ** DocumentVersionMetadata
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

    -- ** FolderMetadata
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

    -- ** GroupMetadata
    groupMetadata_name,
    groupMetadata_id,

    -- ** NotificationOptions
    notificationOptions_emailMessage,
    notificationOptions_sendEmail,

    -- ** Participants
    participants_groups,
    participants_users,

    -- ** PermissionInfo
    permissionInfo_role,
    permissionInfo_type,

    -- ** Principal
    principal_roles,
    principal_id,
    principal_type,

    -- ** ResourceMetadata
    resourceMetadata_versionId,
    resourceMetadata_owner,
    resourceMetadata_name,
    resourceMetadata_id,
    resourceMetadata_type,
    resourceMetadata_originalName,
    resourceMetadata_parentId,

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
    shareResult_status,
    shareResult_principalId,
    shareResult_inviteePrincipalId,
    shareResult_role,
    shareResult_statusMessage,
    shareResult_shareId,

    -- ** StorageRuleType
    storageRuleType_storageAllocatedInBytes,
    storageRuleType_storageType,

    -- ** Subscription
    subscription_protocol,
    subscription_endPoint,
    subscription_subscriptionId,

    -- ** UploadMetadata
    uploadMetadata_uploadUrl,
    uploadMetadata_signedHeaders,

    -- ** User
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

    -- ** UserMetadata
    userMetadata_givenName,
    userMetadata_username,
    userMetadata_emailAddress,
    userMetadata_id,
    userMetadata_surname,

    -- ** UserStorageMetadata
    userStorageMetadata_storageUtilizedInBytes,
    userStorageMetadata_storageRule,
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
