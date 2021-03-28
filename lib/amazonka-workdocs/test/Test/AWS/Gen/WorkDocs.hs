{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.WorkDocs
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Test.AWS.Gen.WorkDocs where

import Data.Proxy
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.Tasty
import Network.AWS.WorkDocs
import Test.AWS.WorkDocs.Internal

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestDeleteLabels $
--             mkDeleteLabels
--
--         , requestAbortDocumentVersionUpload $
--             mkAbortDocumentVersionUpload
--
--         , requestGetDocumentPath $
--             mkGetDocumentPath
--
--         , requestCreateComment $
--             mkCreateComment
--
--         , requestDescribeUsers $
--             mkDescribeUsers
--
--         , requestDeleteFolder $
--             mkDeleteFolder
--
--         , requestUpdateFolder $
--             mkUpdateFolder
--
--         , requestDeleteCustomMetadata $
--             mkDeleteCustomMetadata
--
--         , requestDescribeResourcePermissions $
--             mkDescribeResourcePermissions
--
--         , requestDeleteNotificationSubscription $
--             mkDeleteNotificationSubscription
--
--         , requestCreateFolder $
--             mkCreateFolder
--
--         , requestCreateNotificationSubscription $
--             mkCreateNotificationSubscription
--
--         , requestCreateCustomMetadata $
--             mkCreateCustomMetadata
--
--         , requestGetFolderPath $
--             mkGetFolderPath
--
--         , requestDescribeComments $
--             mkDescribeComments
--
--         , requestDeleteFolderContents $
--             mkDeleteFolderContents
--
--         , requestRemoveAllResourcePermissions $
--             mkRemoveAllResourcePermissions
--
--         , requestGetFolder $
--             mkGetFolder
--
--         , requestDescribeNotificationSubscriptions $
--             mkDescribeNotificationSubscriptions
--
--         , requestActivateUser $
--             mkActivateUser
--
--         , requestDescribeDocumentVersions $
--             mkDescribeDocumentVersions
--
--         , requestGetDocumentVersion $
--             mkGetDocumentVersion
--
--         , requestDescribeActivities $
--             mkDescribeActivities
--
--         , requestDescribeRootFolders $
--             mkDescribeRootFolders
--
--         , requestGetCurrentUser $
--             mkGetCurrentUser
--
--         , requestDeactivateUser $
--             mkDeactivateUser
--
--         , requestGetDocument $
--             mkGetDocument
--
--         , requestDescribeFolderContents $
--             mkDescribeFolderContents
--
--         , requestCreateLabels $
--             mkCreateLabels
--
--         , requestUpdateDocumentVersion $
--             mkUpdateDocumentVersion
--
--         , requestRemoveResourcePermission $
--             mkRemoveResourcePermission
--
--         , requestGetResources $
--             mkGetResources
--
--         , requestDeleteComment $
--             mkDeleteComment
--
--         , requestInitiateDocumentVersionUpload $
--             mkInitiateDocumentVersionUpload
--
--         , requestCreateUser $
--             mkCreateUser
--
--         , requestUpdateUser $
--             mkUpdateUser
--
--         , requestDeleteUser $
--             mkDeleteUser
--
--         , requestAddResourcePermissions $
--             mkAddResourcePermissions
--
--         , requestUpdateDocument $
--             mkUpdateDocument
--
--         , requestDeleteDocument $
--             mkDeleteDocument
--
--         , requestDescribeGroups $
--             mkDescribeGroups
--
--           ]

--     , testGroup "response"
--         [ responseDeleteLabels $
--             mkDeleteLabelsResponse
--
--         , responseAbortDocumentVersionUpload $
--             mkAbortDocumentVersionUploadResponse
--
--         , responseGetDocumentPath $
--             mkGetDocumentPathResponse
--
--         , responseCreateComment $
--             mkCreateCommentResponse
--
--         , responseDescribeUsers $
--             mkDescribeUsersResponse
--
--         , responseDeleteFolder $
--             mkDeleteFolderResponse
--
--         , responseUpdateFolder $
--             mkUpdateFolderResponse
--
--         , responseDeleteCustomMetadata $
--             mkDeleteCustomMetadataResponse
--
--         , responseDescribeResourcePermissions $
--             mkDescribeResourcePermissionsResponse
--
--         , responseDeleteNotificationSubscription $
--             mkDeleteNotificationSubscriptionResponse
--
--         , responseCreateFolder $
--             mkCreateFolderResponse
--
--         , responseCreateNotificationSubscription $
--             mkCreateNotificationSubscriptionResponse
--
--         , responseCreateCustomMetadata $
--             mkCreateCustomMetadataResponse
--
--         , responseGetFolderPath $
--             mkGetFolderPathResponse
--
--         , responseDescribeComments $
--             mkDescribeCommentsResponse
--
--         , responseDeleteFolderContents $
--             mkDeleteFolderContentsResponse
--
--         , responseRemoveAllResourcePermissions $
--             mkRemoveAllResourcePermissionsResponse
--
--         , responseGetFolder $
--             mkGetFolderResponse
--
--         , responseDescribeNotificationSubscriptions $
--             mkDescribeNotificationSubscriptionsResponse
--
--         , responseActivateUser $
--             mkActivateUserResponse
--
--         , responseDescribeDocumentVersions $
--             mkDescribeDocumentVersionsResponse
--
--         , responseGetDocumentVersion $
--             mkGetDocumentVersionResponse
--
--         , responseDescribeActivities $
--             mkDescribeActivitiesResponse
--
--         , responseDescribeRootFolders $
--             mkDescribeRootFoldersResponse
--
--         , responseGetCurrentUser $
--             mkGetCurrentUserResponse
--
--         , responseDeactivateUser $
--             mkDeactivateUserResponse
--
--         , responseGetDocument $
--             mkGetDocumentResponse
--
--         , responseDescribeFolderContents $
--             mkDescribeFolderContentsResponse
--
--         , responseCreateLabels $
--             mkCreateLabelsResponse
--
--         , responseUpdateDocumentVersion $
--             mkUpdateDocumentVersionResponse
--
--         , responseRemoveResourcePermission $
--             mkRemoveResourcePermissionResponse
--
--         , responseGetResources $
--             mkGetResourcesResponse
--
--         , responseDeleteComment $
--             mkDeleteCommentResponse
--
--         , responseInitiateDocumentVersionUpload $
--             mkInitiateDocumentVersionUploadResponse
--
--         , responseCreateUser $
--             mkCreateUserResponse
--
--         , responseUpdateUser $
--             mkUpdateUserResponse
--
--         , responseDeleteUser $
--             mkDeleteUserResponse
--
--         , responseAddResourcePermissions $
--             mkAddResourcePermissionsResponse
--
--         , responseUpdateDocument $
--             mkUpdateDocumentResponse
--
--         , responseDeleteDocument $
--             mkDeleteDocumentResponse
--
--         , responseDescribeGroups $
--             mkDescribeGroupsResponse
--
--           ]
--     ]

-- Requests

requestDeleteLabels :: DeleteLabels -> TestTree
requestDeleteLabels = req
    "DeleteLabels"
    "fixture/DeleteLabels.yaml"

requestAbortDocumentVersionUpload :: AbortDocumentVersionUpload -> TestTree
requestAbortDocumentVersionUpload = req
    "AbortDocumentVersionUpload"
    "fixture/AbortDocumentVersionUpload.yaml"

requestGetDocumentPath :: GetDocumentPath -> TestTree
requestGetDocumentPath = req
    "GetDocumentPath"
    "fixture/GetDocumentPath.yaml"

requestCreateComment :: CreateComment -> TestTree
requestCreateComment = req
    "CreateComment"
    "fixture/CreateComment.yaml"

requestDescribeUsers :: DescribeUsers -> TestTree
requestDescribeUsers = req
    "DescribeUsers"
    "fixture/DescribeUsers.yaml"

requestDeleteFolder :: DeleteFolder -> TestTree
requestDeleteFolder = req
    "DeleteFolder"
    "fixture/DeleteFolder.yaml"

requestUpdateFolder :: UpdateFolder -> TestTree
requestUpdateFolder = req
    "UpdateFolder"
    "fixture/UpdateFolder.yaml"

requestDeleteCustomMetadata :: DeleteCustomMetadata -> TestTree
requestDeleteCustomMetadata = req
    "DeleteCustomMetadata"
    "fixture/DeleteCustomMetadata.yaml"

requestDescribeResourcePermissions :: DescribeResourcePermissions -> TestTree
requestDescribeResourcePermissions = req
    "DescribeResourcePermissions"
    "fixture/DescribeResourcePermissions.yaml"

requestDeleteNotificationSubscription :: DeleteNotificationSubscription -> TestTree
requestDeleteNotificationSubscription = req
    "DeleteNotificationSubscription"
    "fixture/DeleteNotificationSubscription.yaml"

requestCreateFolder :: CreateFolder -> TestTree
requestCreateFolder = req
    "CreateFolder"
    "fixture/CreateFolder.yaml"

requestCreateNotificationSubscription :: CreateNotificationSubscription -> TestTree
requestCreateNotificationSubscription = req
    "CreateNotificationSubscription"
    "fixture/CreateNotificationSubscription.yaml"

requestCreateCustomMetadata :: CreateCustomMetadata -> TestTree
requestCreateCustomMetadata = req
    "CreateCustomMetadata"
    "fixture/CreateCustomMetadata.yaml"

requestGetFolderPath :: GetFolderPath -> TestTree
requestGetFolderPath = req
    "GetFolderPath"
    "fixture/GetFolderPath.yaml"

requestDescribeComments :: DescribeComments -> TestTree
requestDescribeComments = req
    "DescribeComments"
    "fixture/DescribeComments.yaml"

requestDeleteFolderContents :: DeleteFolderContents -> TestTree
requestDeleteFolderContents = req
    "DeleteFolderContents"
    "fixture/DeleteFolderContents.yaml"

requestRemoveAllResourcePermissions :: RemoveAllResourcePermissions -> TestTree
requestRemoveAllResourcePermissions = req
    "RemoveAllResourcePermissions"
    "fixture/RemoveAllResourcePermissions.yaml"

requestGetFolder :: GetFolder -> TestTree
requestGetFolder = req
    "GetFolder"
    "fixture/GetFolder.yaml"

requestDescribeNotificationSubscriptions :: DescribeNotificationSubscriptions -> TestTree
requestDescribeNotificationSubscriptions = req
    "DescribeNotificationSubscriptions"
    "fixture/DescribeNotificationSubscriptions.yaml"

requestActivateUser :: ActivateUser -> TestTree
requestActivateUser = req
    "ActivateUser"
    "fixture/ActivateUser.yaml"

requestDescribeDocumentVersions :: DescribeDocumentVersions -> TestTree
requestDescribeDocumentVersions = req
    "DescribeDocumentVersions"
    "fixture/DescribeDocumentVersions.yaml"

requestGetDocumentVersion :: GetDocumentVersion -> TestTree
requestGetDocumentVersion = req
    "GetDocumentVersion"
    "fixture/GetDocumentVersion.yaml"

requestDescribeActivities :: DescribeActivities -> TestTree
requestDescribeActivities = req
    "DescribeActivities"
    "fixture/DescribeActivities.yaml"

requestDescribeRootFolders :: DescribeRootFolders -> TestTree
requestDescribeRootFolders = req
    "DescribeRootFolders"
    "fixture/DescribeRootFolders.yaml"

requestGetCurrentUser :: GetCurrentUser -> TestTree
requestGetCurrentUser = req
    "GetCurrentUser"
    "fixture/GetCurrentUser.yaml"

requestDeactivateUser :: DeactivateUser -> TestTree
requestDeactivateUser = req
    "DeactivateUser"
    "fixture/DeactivateUser.yaml"

requestGetDocument :: GetDocument -> TestTree
requestGetDocument = req
    "GetDocument"
    "fixture/GetDocument.yaml"

requestDescribeFolderContents :: DescribeFolderContents -> TestTree
requestDescribeFolderContents = req
    "DescribeFolderContents"
    "fixture/DescribeFolderContents.yaml"

requestCreateLabels :: CreateLabels -> TestTree
requestCreateLabels = req
    "CreateLabels"
    "fixture/CreateLabels.yaml"

requestUpdateDocumentVersion :: UpdateDocumentVersion -> TestTree
requestUpdateDocumentVersion = req
    "UpdateDocumentVersion"
    "fixture/UpdateDocumentVersion.yaml"

requestRemoveResourcePermission :: RemoveResourcePermission -> TestTree
requestRemoveResourcePermission = req
    "RemoveResourcePermission"
    "fixture/RemoveResourcePermission.yaml"

requestGetResources :: GetResources -> TestTree
requestGetResources = req
    "GetResources"
    "fixture/GetResources.yaml"

requestDeleteComment :: DeleteComment -> TestTree
requestDeleteComment = req
    "DeleteComment"
    "fixture/DeleteComment.yaml"

requestInitiateDocumentVersionUpload :: InitiateDocumentVersionUpload -> TestTree
requestInitiateDocumentVersionUpload = req
    "InitiateDocumentVersionUpload"
    "fixture/InitiateDocumentVersionUpload.yaml"

requestCreateUser :: CreateUser -> TestTree
requestCreateUser = req
    "CreateUser"
    "fixture/CreateUser.yaml"

requestUpdateUser :: UpdateUser -> TestTree
requestUpdateUser = req
    "UpdateUser"
    "fixture/UpdateUser.yaml"

requestDeleteUser :: DeleteUser -> TestTree
requestDeleteUser = req
    "DeleteUser"
    "fixture/DeleteUser.yaml"

requestAddResourcePermissions :: AddResourcePermissions -> TestTree
requestAddResourcePermissions = req
    "AddResourcePermissions"
    "fixture/AddResourcePermissions.yaml"

requestUpdateDocument :: UpdateDocument -> TestTree
requestUpdateDocument = req
    "UpdateDocument"
    "fixture/UpdateDocument.yaml"

requestDeleteDocument :: DeleteDocument -> TestTree
requestDeleteDocument = req
    "DeleteDocument"
    "fixture/DeleteDocument.yaml"

requestDescribeGroups :: DescribeGroups -> TestTree
requestDescribeGroups = req
    "DescribeGroups"
    "fixture/DescribeGroups.yaml"

-- Responses

responseDeleteLabels :: DeleteLabelsResponse -> TestTree
responseDeleteLabels = res
    "DeleteLabelsResponse"
    "fixture/DeleteLabelsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteLabels)

responseAbortDocumentVersionUpload :: AbortDocumentVersionUploadResponse -> TestTree
responseAbortDocumentVersionUpload = res
    "AbortDocumentVersionUploadResponse"
    "fixture/AbortDocumentVersionUploadResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy AbortDocumentVersionUpload)

responseGetDocumentPath :: GetDocumentPathResponse -> TestTree
responseGetDocumentPath = res
    "GetDocumentPathResponse"
    "fixture/GetDocumentPathResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetDocumentPath)

responseCreateComment :: CreateCommentResponse -> TestTree
responseCreateComment = res
    "CreateCommentResponse"
    "fixture/CreateCommentResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateComment)

responseDescribeUsers :: DescribeUsersResponse -> TestTree
responseDescribeUsers = res
    "DescribeUsersResponse"
    "fixture/DescribeUsersResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeUsers)

responseDeleteFolder :: DeleteFolderResponse -> TestTree
responseDeleteFolder = res
    "DeleteFolderResponse"
    "fixture/DeleteFolderResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteFolder)

responseUpdateFolder :: UpdateFolderResponse -> TestTree
responseUpdateFolder = res
    "UpdateFolderResponse"
    "fixture/UpdateFolderResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateFolder)

responseDeleteCustomMetadata :: DeleteCustomMetadataResponse -> TestTree
responseDeleteCustomMetadata = res
    "DeleteCustomMetadataResponse"
    "fixture/DeleteCustomMetadataResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteCustomMetadata)

responseDescribeResourcePermissions :: DescribeResourcePermissionsResponse -> TestTree
responseDescribeResourcePermissions = res
    "DescribeResourcePermissionsResponse"
    "fixture/DescribeResourcePermissionsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeResourcePermissions)

responseDeleteNotificationSubscription :: DeleteNotificationSubscriptionResponse -> TestTree
responseDeleteNotificationSubscription = res
    "DeleteNotificationSubscriptionResponse"
    "fixture/DeleteNotificationSubscriptionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteNotificationSubscription)

responseCreateFolder :: CreateFolderResponse -> TestTree
responseCreateFolder = res
    "CreateFolderResponse"
    "fixture/CreateFolderResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateFolder)

responseCreateNotificationSubscription :: CreateNotificationSubscriptionResponse -> TestTree
responseCreateNotificationSubscription = res
    "CreateNotificationSubscriptionResponse"
    "fixture/CreateNotificationSubscriptionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateNotificationSubscription)

responseCreateCustomMetadata :: CreateCustomMetadataResponse -> TestTree
responseCreateCustomMetadata = res
    "CreateCustomMetadataResponse"
    "fixture/CreateCustomMetadataResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateCustomMetadata)

responseGetFolderPath :: GetFolderPathResponse -> TestTree
responseGetFolderPath = res
    "GetFolderPathResponse"
    "fixture/GetFolderPathResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetFolderPath)

responseDescribeComments :: DescribeCommentsResponse -> TestTree
responseDescribeComments = res
    "DescribeCommentsResponse"
    "fixture/DescribeCommentsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeComments)

responseDeleteFolderContents :: DeleteFolderContentsResponse -> TestTree
responseDeleteFolderContents = res
    "DeleteFolderContentsResponse"
    "fixture/DeleteFolderContentsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteFolderContents)

responseRemoveAllResourcePermissions :: RemoveAllResourcePermissionsResponse -> TestTree
responseRemoveAllResourcePermissions = res
    "RemoveAllResourcePermissionsResponse"
    "fixture/RemoveAllResourcePermissionsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy RemoveAllResourcePermissions)

responseGetFolder :: GetFolderResponse -> TestTree
responseGetFolder = res
    "GetFolderResponse"
    "fixture/GetFolderResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetFolder)

responseDescribeNotificationSubscriptions :: DescribeNotificationSubscriptionsResponse -> TestTree
responseDescribeNotificationSubscriptions = res
    "DescribeNotificationSubscriptionsResponse"
    "fixture/DescribeNotificationSubscriptionsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeNotificationSubscriptions)

responseActivateUser :: ActivateUserResponse -> TestTree
responseActivateUser = res
    "ActivateUserResponse"
    "fixture/ActivateUserResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ActivateUser)

responseDescribeDocumentVersions :: DescribeDocumentVersionsResponse -> TestTree
responseDescribeDocumentVersions = res
    "DescribeDocumentVersionsResponse"
    "fixture/DescribeDocumentVersionsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeDocumentVersions)

responseGetDocumentVersion :: GetDocumentVersionResponse -> TestTree
responseGetDocumentVersion = res
    "GetDocumentVersionResponse"
    "fixture/GetDocumentVersionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetDocumentVersion)

responseDescribeActivities :: DescribeActivitiesResponse -> TestTree
responseDescribeActivities = res
    "DescribeActivitiesResponse"
    "fixture/DescribeActivitiesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeActivities)

responseDescribeRootFolders :: DescribeRootFoldersResponse -> TestTree
responseDescribeRootFolders = res
    "DescribeRootFoldersResponse"
    "fixture/DescribeRootFoldersResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeRootFolders)

responseGetCurrentUser :: GetCurrentUserResponse -> TestTree
responseGetCurrentUser = res
    "GetCurrentUserResponse"
    "fixture/GetCurrentUserResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetCurrentUser)

responseDeactivateUser :: DeactivateUserResponse -> TestTree
responseDeactivateUser = res
    "DeactivateUserResponse"
    "fixture/DeactivateUserResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeactivateUser)

responseGetDocument :: GetDocumentResponse -> TestTree
responseGetDocument = res
    "GetDocumentResponse"
    "fixture/GetDocumentResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetDocument)

responseDescribeFolderContents :: DescribeFolderContentsResponse -> TestTree
responseDescribeFolderContents = res
    "DescribeFolderContentsResponse"
    "fixture/DescribeFolderContentsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeFolderContents)

responseCreateLabels :: CreateLabelsResponse -> TestTree
responseCreateLabels = res
    "CreateLabelsResponse"
    "fixture/CreateLabelsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateLabels)

responseUpdateDocumentVersion :: UpdateDocumentVersionResponse -> TestTree
responseUpdateDocumentVersion = res
    "UpdateDocumentVersionResponse"
    "fixture/UpdateDocumentVersionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateDocumentVersion)

responseRemoveResourcePermission :: RemoveResourcePermissionResponse -> TestTree
responseRemoveResourcePermission = res
    "RemoveResourcePermissionResponse"
    "fixture/RemoveResourcePermissionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy RemoveResourcePermission)

responseGetResources :: GetResourcesResponse -> TestTree
responseGetResources = res
    "GetResourcesResponse"
    "fixture/GetResourcesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetResources)

responseDeleteComment :: DeleteCommentResponse -> TestTree
responseDeleteComment = res
    "DeleteCommentResponse"
    "fixture/DeleteCommentResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteComment)

responseInitiateDocumentVersionUpload :: InitiateDocumentVersionUploadResponse -> TestTree
responseInitiateDocumentVersionUpload = res
    "InitiateDocumentVersionUploadResponse"
    "fixture/InitiateDocumentVersionUploadResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy InitiateDocumentVersionUpload)

responseCreateUser :: CreateUserResponse -> TestTree
responseCreateUser = res
    "CreateUserResponse"
    "fixture/CreateUserResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateUser)

responseUpdateUser :: UpdateUserResponse -> TestTree
responseUpdateUser = res
    "UpdateUserResponse"
    "fixture/UpdateUserResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateUser)

responseDeleteUser :: DeleteUserResponse -> TestTree
responseDeleteUser = res
    "DeleteUserResponse"
    "fixture/DeleteUserResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteUser)

responseAddResourcePermissions :: AddResourcePermissionsResponse -> TestTree
responseAddResourcePermissions = res
    "AddResourcePermissionsResponse"
    "fixture/AddResourcePermissionsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy AddResourcePermissions)

responseUpdateDocument :: UpdateDocumentResponse -> TestTree
responseUpdateDocument = res
    "UpdateDocumentResponse"
    "fixture/UpdateDocumentResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateDocument)

responseDeleteDocument :: DeleteDocumentResponse -> TestTree
responseDeleteDocument = res
    "DeleteDocumentResponse"
    "fixture/DeleteDocumentResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteDocument)

responseDescribeGroups :: DescribeGroupsResponse -> TestTree
responseDescribeGroups = res
    "DescribeGroupsResponse"
    "fixture/DescribeGroupsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeGroups)
