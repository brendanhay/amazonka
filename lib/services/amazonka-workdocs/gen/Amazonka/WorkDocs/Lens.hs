{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.WorkDocs.Lens
-- Copyright   : (c) 2013-2023 Brendan Hay
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
    addResourcePermissions_authenticationToken,
    addResourcePermissions_notificationOptions,
    addResourcePermissions_resourceId,
    addResourcePermissions_principals,
    addResourcePermissionsResponse_shareResults,
    addResourcePermissionsResponse_httpStatus,

    -- ** CreateComment
    createComment_authenticationToken,
    createComment_notifyCollaborators,
    createComment_parentId,
    createComment_threadId,
    createComment_visibility,
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
    createFolder_authenticationToken,
    createFolder_name,
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
    createUser_authenticationToken,
    createUser_emailAddress,
    createUser_organizationId,
    createUser_storageRule,
    createUser_timeZoneId,
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
    deleteCustomMetadata_authenticationToken,
    deleteCustomMetadata_deleteAll,
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
    deleteLabels_authenticationToken,
    deleteLabels_deleteAll,
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
    describeActivities_activityTypes,
    describeActivities_authenticationToken,
    describeActivities_endTime,
    describeActivities_includeIndirectActivities,
    describeActivities_limit,
    describeActivities_marker,
    describeActivities_organizationId,
    describeActivities_resourceId,
    describeActivities_startTime,
    describeActivities_userId,
    describeActivitiesResponse_marker,
    describeActivitiesResponse_userActivities,
    describeActivitiesResponse_httpStatus,

    -- ** DescribeComments
    describeComments_authenticationToken,
    describeComments_limit,
    describeComments_marker,
    describeComments_documentId,
    describeComments_versionId,
    describeCommentsResponse_comments,
    describeCommentsResponse_marker,
    describeCommentsResponse_httpStatus,

    -- ** DescribeDocumentVersions
    describeDocumentVersions_authenticationToken,
    describeDocumentVersions_fields,
    describeDocumentVersions_include,
    describeDocumentVersions_limit,
    describeDocumentVersions_marker,
    describeDocumentVersions_documentId,
    describeDocumentVersionsResponse_documentVersions,
    describeDocumentVersionsResponse_marker,
    describeDocumentVersionsResponse_httpStatus,

    -- ** DescribeFolderContents
    describeFolderContents_authenticationToken,
    describeFolderContents_include,
    describeFolderContents_limit,
    describeFolderContents_marker,
    describeFolderContents_order,
    describeFolderContents_sort,
    describeFolderContents_type,
    describeFolderContents_folderId,
    describeFolderContentsResponse_documents,
    describeFolderContentsResponse_folders,
    describeFolderContentsResponse_marker,
    describeFolderContentsResponse_httpStatus,

    -- ** DescribeGroups
    describeGroups_authenticationToken,
    describeGroups_limit,
    describeGroups_marker,
    describeGroups_organizationId,
    describeGroups_searchQuery,
    describeGroupsResponse_groups,
    describeGroupsResponse_marker,
    describeGroupsResponse_httpStatus,

    -- ** DescribeNotificationSubscriptions
    describeNotificationSubscriptions_limit,
    describeNotificationSubscriptions_marker,
    describeNotificationSubscriptions_organizationId,
    describeNotificationSubscriptionsResponse_marker,
    describeNotificationSubscriptionsResponse_subscriptions,
    describeNotificationSubscriptionsResponse_httpStatus,

    -- ** DescribeResourcePermissions
    describeResourcePermissions_authenticationToken,
    describeResourcePermissions_limit,
    describeResourcePermissions_marker,
    describeResourcePermissions_principalId,
    describeResourcePermissions_resourceId,
    describeResourcePermissionsResponse_marker,
    describeResourcePermissionsResponse_principals,
    describeResourcePermissionsResponse_httpStatus,

    -- ** DescribeRootFolders
    describeRootFolders_limit,
    describeRootFolders_marker,
    describeRootFolders_authenticationToken,
    describeRootFoldersResponse_folders,
    describeRootFoldersResponse_marker,
    describeRootFoldersResponse_httpStatus,

    -- ** DescribeUsers
    describeUsers_authenticationToken,
    describeUsers_fields,
    describeUsers_include,
    describeUsers_limit,
    describeUsers_marker,
    describeUsers_order,
    describeUsers_organizationId,
    describeUsers_query,
    describeUsers_sort,
    describeUsers_userIds,
    describeUsersResponse_marker,
    describeUsersResponse_totalNumberOfUsers,
    describeUsersResponse_users,
    describeUsersResponse_httpStatus,

    -- ** GetCurrentUser
    getCurrentUser_authenticationToken,
    getCurrentUserResponse_user,
    getCurrentUserResponse_httpStatus,

    -- ** GetDocument
    getDocument_authenticationToken,
    getDocument_includeCustomMetadata,
    getDocument_documentId,
    getDocumentResponse_customMetadata,
    getDocumentResponse_metadata,
    getDocumentResponse_httpStatus,

    -- ** GetDocumentPath
    getDocumentPath_authenticationToken,
    getDocumentPath_fields,
    getDocumentPath_limit,
    getDocumentPath_marker,
    getDocumentPath_documentId,
    getDocumentPathResponse_path,
    getDocumentPathResponse_httpStatus,

    -- ** GetDocumentVersion
    getDocumentVersion_authenticationToken,
    getDocumentVersion_fields,
    getDocumentVersion_includeCustomMetadata,
    getDocumentVersion_documentId,
    getDocumentVersion_versionId,
    getDocumentVersionResponse_customMetadata,
    getDocumentVersionResponse_metadata,
    getDocumentVersionResponse_httpStatus,

    -- ** GetFolder
    getFolder_authenticationToken,
    getFolder_includeCustomMetadata,
    getFolder_folderId,
    getFolderResponse_customMetadata,
    getFolderResponse_metadata,
    getFolderResponse_httpStatus,

    -- ** GetFolderPath
    getFolderPath_authenticationToken,
    getFolderPath_fields,
    getFolderPath_limit,
    getFolderPath_marker,
    getFolderPath_folderId,
    getFolderPathResponse_path,
    getFolderPathResponse_httpStatus,

    -- ** GetResources
    getResources_authenticationToken,
    getResources_collectionType,
    getResources_limit,
    getResources_marker,
    getResources_userId,
    getResourcesResponse_documents,
    getResourcesResponse_folders,
    getResourcesResponse_marker,
    getResourcesResponse_httpStatus,

    -- ** InitiateDocumentVersionUpload
    initiateDocumentVersionUpload_authenticationToken,
    initiateDocumentVersionUpload_contentCreatedTimestamp,
    initiateDocumentVersionUpload_contentModifiedTimestamp,
    initiateDocumentVersionUpload_contentType,
    initiateDocumentVersionUpload_documentSizeInBytes,
    initiateDocumentVersionUpload_id,
    initiateDocumentVersionUpload_name,
    initiateDocumentVersionUpload_parentFolderId,
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

    -- ** SearchResources
    searchResources_additionalResponseFields,
    searchResources_authenticationToken,
    searchResources_filters,
    searchResources_limit,
    searchResources_marker,
    searchResources_orderBy,
    searchResources_organizationId,
    searchResources_queryScopes,
    searchResources_queryText,
    searchResourcesResponse_items,
    searchResourcesResponse_marker,
    searchResourcesResponse_httpStatus,

    -- ** UpdateDocument
    updateDocument_authenticationToken,
    updateDocument_name,
    updateDocument_parentFolderId,
    updateDocument_resourceState,
    updateDocument_documentId,

    -- ** UpdateDocumentVersion
    updateDocumentVersion_authenticationToken,
    updateDocumentVersion_versionStatus,
    updateDocumentVersion_documentId,
    updateDocumentVersion_versionId,

    -- ** UpdateFolder
    updateFolder_authenticationToken,
    updateFolder_name,
    updateFolder_parentFolderId,
    updateFolder_resourceState,
    updateFolder_folderId,

    -- ** UpdateUser
    updateUser_authenticationToken,
    updateUser_givenName,
    updateUser_grantPoweruserPrivileges,
    updateUser_locale,
    updateUser_storageRule,
    updateUser_surname,
    updateUser_timeZoneId,
    updateUser_type,
    updateUser_userId,
    updateUserResponse_user,
    updateUserResponse_httpStatus,

    -- * Types

    -- ** Activity
    activity_commentMetadata,
    activity_initiator,
    activity_isIndirectActivity,
    activity_organizationId,
    activity_originalParent,
    activity_participants,
    activity_resourceMetadata,
    activity_timeStamp,
    activity_type,

    -- ** Comment
    comment_contributor,
    comment_createdTimestamp,
    comment_parentId,
    comment_recipientId,
    comment_status,
    comment_text,
    comment_threadId,
    comment_visibility,
    comment_commentId,

    -- ** CommentMetadata
    commentMetadata_commentId,
    commentMetadata_commentStatus,
    commentMetadata_contributor,
    commentMetadata_contributorId,
    commentMetadata_createdTimestamp,
    commentMetadata_recipientId,

    -- ** DateRangeType
    dateRangeType_endValue,
    dateRangeType_startValue,

    -- ** DocumentMetadata
    documentMetadata_createdTimestamp,
    documentMetadata_creatorId,
    documentMetadata_id,
    documentMetadata_labels,
    documentMetadata_latestVersionMetadata,
    documentMetadata_modifiedTimestamp,
    documentMetadata_parentFolderId,
    documentMetadata_resourceState,

    -- ** DocumentVersionMetadata
    documentVersionMetadata_contentCreatedTimestamp,
    documentVersionMetadata_contentModifiedTimestamp,
    documentVersionMetadata_contentType,
    documentVersionMetadata_createdTimestamp,
    documentVersionMetadata_creatorId,
    documentVersionMetadata_id,
    documentVersionMetadata_modifiedTimestamp,
    documentVersionMetadata_name,
    documentVersionMetadata_signature,
    documentVersionMetadata_size,
    documentVersionMetadata_source,
    documentVersionMetadata_status,
    documentVersionMetadata_thumbnail,

    -- ** Filters
    filters_ancestorIds,
    filters_contentCategories,
    filters_createdRange,
    filters_labels,
    filters_modifiedRange,
    filters_principals,
    filters_resourceTypes,
    filters_searchCollectionTypes,
    filters_sizeRange,
    filters_textLocales,

    -- ** FolderMetadata
    folderMetadata_createdTimestamp,
    folderMetadata_creatorId,
    folderMetadata_id,
    folderMetadata_labels,
    folderMetadata_latestVersionSize,
    folderMetadata_modifiedTimestamp,
    folderMetadata_name,
    folderMetadata_parentFolderId,
    folderMetadata_resourceState,
    folderMetadata_signature,
    folderMetadata_size,

    -- ** GroupMetadata
    groupMetadata_id,
    groupMetadata_name,

    -- ** LongRangeType
    longRangeType_endValue,
    longRangeType_startValue,

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
    principal_id,
    principal_roles,
    principal_type,

    -- ** ResourceMetadata
    resourceMetadata_id,
    resourceMetadata_name,
    resourceMetadata_originalName,
    resourceMetadata_owner,
    resourceMetadata_parentId,
    resourceMetadata_type,
    resourceMetadata_versionId,

    -- ** ResourcePath
    resourcePath_components,

    -- ** ResourcePathComponent
    resourcePathComponent_id,
    resourcePathComponent_name,

    -- ** ResponseItem
    responseItem_commentMetadata,
    responseItem_documentMetadata,
    responseItem_documentVersionMetadata,
    responseItem_folderMetadata,
    responseItem_resourceType,
    responseItem_webUrl,

    -- ** SearchPrincipalType
    searchPrincipalType_roles,
    searchPrincipalType_id,

    -- ** SearchSortResult
    searchSortResult_field,
    searchSortResult_order,

    -- ** SharePrincipal
    sharePrincipal_id,
    sharePrincipal_type,
    sharePrincipal_role,

    -- ** ShareResult
    shareResult_inviteePrincipalId,
    shareResult_principalId,
    shareResult_role,
    shareResult_shareId,
    shareResult_status,
    shareResult_statusMessage,

    -- ** StorageRuleType
    storageRuleType_storageAllocatedInBytes,
    storageRuleType_storageType,

    -- ** Subscription
    subscription_endPoint,
    subscription_protocol,
    subscription_subscriptionId,

    -- ** UploadMetadata
    uploadMetadata_signedHeaders,
    uploadMetadata_uploadUrl,

    -- ** User
    user_createdTimestamp,
    user_emailAddress,
    user_givenName,
    user_id,
    user_locale,
    user_modifiedTimestamp,
    user_organizationId,
    user_recycleBinFolderId,
    user_rootFolderId,
    user_status,
    user_storage,
    user_surname,
    user_timeZoneId,
    user_type,
    user_username,

    -- ** UserMetadata
    userMetadata_emailAddress,
    userMetadata_givenName,
    userMetadata_id,
    userMetadata_surname,
    userMetadata_username,

    -- ** UserStorageMetadata
    userStorageMetadata_storageRule,
    userStorageMetadata_storageUtilizedInBytes,
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
import Amazonka.WorkDocs.SearchResources
import Amazonka.WorkDocs.Types.Activity
import Amazonka.WorkDocs.Types.Comment
import Amazonka.WorkDocs.Types.CommentMetadata
import Amazonka.WorkDocs.Types.DateRangeType
import Amazonka.WorkDocs.Types.DocumentMetadata
import Amazonka.WorkDocs.Types.DocumentVersionMetadata
import Amazonka.WorkDocs.Types.Filters
import Amazonka.WorkDocs.Types.FolderMetadata
import Amazonka.WorkDocs.Types.GroupMetadata
import Amazonka.WorkDocs.Types.LongRangeType
import Amazonka.WorkDocs.Types.NotificationOptions
import Amazonka.WorkDocs.Types.Participants
import Amazonka.WorkDocs.Types.PermissionInfo
import Amazonka.WorkDocs.Types.Principal
import Amazonka.WorkDocs.Types.ResourceMetadata
import Amazonka.WorkDocs.Types.ResourcePath
import Amazonka.WorkDocs.Types.ResourcePathComponent
import Amazonka.WorkDocs.Types.ResponseItem
import Amazonka.WorkDocs.Types.SearchPrincipalType
import Amazonka.WorkDocs.Types.SearchSortResult
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
