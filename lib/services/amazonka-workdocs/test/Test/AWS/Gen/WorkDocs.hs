{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.WorkDocs
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.AWS.Gen.WorkDocs where

import Data.Proxy
import Network.AWS.WorkDocs
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.AWS.WorkDocs.Internal
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestDeleteCustomMetadata $
--             newDeleteCustomMetadata
--
--         , requestUpdateFolder $
--             newUpdateFolder
--
--         , requestDeleteNotificationSubscription $
--             newDeleteNotificationSubscription
--
--         , requestDeleteFolder $
--             newDeleteFolder
--
--         , requestDeleteLabels $
--             newDeleteLabels
--
--         , requestAbortDocumentVersionUpload $
--             newAbortDocumentVersionUpload
--
--         , requestUpdateDocumentVersion $
--             newUpdateDocumentVersion
--
--         , requestDeactivateUser $
--             newDeactivateUser
--
--         , requestDescribeFolderContents $
--             newDescribeFolderContents
--
--         , requestCreateLabels $
--             newCreateLabels
--
--         , requestDescribeRootFolders $
--             newDescribeRootFolders
--
--         , requestUpdateDocument $
--             newUpdateDocument
--
--         , requestDeleteDocument $
--             newDeleteDocument
--
--         , requestDescribeDocumentVersions $
--             newDescribeDocumentVersions
--
--         , requestGetDocumentVersion $
--             newGetDocumentVersion
--
--         , requestActivateUser $
--             newActivateUser
--
--         , requestGetFolderPath $
--             newGetFolderPath
--
--         , requestCreateUser $
--             newCreateUser
--
--         , requestDeleteComment $
--             newDeleteComment
--
--         , requestCreateFolder $
--             newCreateFolder
--
--         , requestCreateCustomMetadata $
--             newCreateCustomMetadata
--
--         , requestCreateNotificationSubscription $
--             newCreateNotificationSubscription
--
--         , requestGetResources $
--             newGetResources
--
--         , requestRemoveResourcePermission $
--             newRemoveResourcePermission
--
--         , requestCreateComment $
--             newCreateComment
--
--         , requestDescribeResourcePermissions $
--             newDescribeResourcePermissions
--
--         , requestDescribeUsers $
--             newDescribeUsers
--
--         , requestGetDocumentPath $
--             newGetDocumentPath
--
--         , requestGetDocument $
--             newGetDocument
--
--         , requestDescribeGroups $
--             newDescribeGroups
--
--         , requestDescribeActivities $
--             newDescribeActivities
--
--         , requestGetCurrentUser $
--             newGetCurrentUser
--
--         , requestAddResourcePermissions $
--             newAddResourcePermissions
--
--         , requestUpdateUser $
--             newUpdateUser
--
--         , requestDescribeNotificationSubscriptions $
--             newDescribeNotificationSubscriptions
--
--         , requestGetFolder $
--             newGetFolder
--
--         , requestDeleteUser $
--             newDeleteUser
--
--         , requestDeleteFolderContents $
--             newDeleteFolderContents
--
--         , requestDescribeComments $
--             newDescribeComments
--
--         , requestInitiateDocumentVersionUpload $
--             newInitiateDocumentVersionUpload
--
--         , requestRemoveAllResourcePermissions $
--             newRemoveAllResourcePermissions
--
--           ]

--     , testGroup "response"
--         [ responseDeleteCustomMetadata $
--             newDeleteCustomMetadataResponse
--
--         , responseUpdateFolder $
--             newUpdateFolderResponse
--
--         , responseDeleteNotificationSubscription $
--             newDeleteNotificationSubscriptionResponse
--
--         , responseDeleteFolder $
--             newDeleteFolderResponse
--
--         , responseDeleteLabels $
--             newDeleteLabelsResponse
--
--         , responseAbortDocumentVersionUpload $
--             newAbortDocumentVersionUploadResponse
--
--         , responseUpdateDocumentVersion $
--             newUpdateDocumentVersionResponse
--
--         , responseDeactivateUser $
--             newDeactivateUserResponse
--
--         , responseDescribeFolderContents $
--             newDescribeFolderContentsResponse
--
--         , responseCreateLabels $
--             newCreateLabelsResponse
--
--         , responseDescribeRootFolders $
--             newDescribeRootFoldersResponse
--
--         , responseUpdateDocument $
--             newUpdateDocumentResponse
--
--         , responseDeleteDocument $
--             newDeleteDocumentResponse
--
--         , responseDescribeDocumentVersions $
--             newDescribeDocumentVersionsResponse
--
--         , responseGetDocumentVersion $
--             newGetDocumentVersionResponse
--
--         , responseActivateUser $
--             newActivateUserResponse
--
--         , responseGetFolderPath $
--             newGetFolderPathResponse
--
--         , responseCreateUser $
--             newCreateUserResponse
--
--         , responseDeleteComment $
--             newDeleteCommentResponse
--
--         , responseCreateFolder $
--             newCreateFolderResponse
--
--         , responseCreateCustomMetadata $
--             newCreateCustomMetadataResponse
--
--         , responseCreateNotificationSubscription $
--             newCreateNotificationSubscriptionResponse
--
--         , responseGetResources $
--             newGetResourcesResponse
--
--         , responseRemoveResourcePermission $
--             newRemoveResourcePermissionResponse
--
--         , responseCreateComment $
--             newCreateCommentResponse
--
--         , responseDescribeResourcePermissions $
--             newDescribeResourcePermissionsResponse
--
--         , responseDescribeUsers $
--             newDescribeUsersResponse
--
--         , responseGetDocumentPath $
--             newGetDocumentPathResponse
--
--         , responseGetDocument $
--             newGetDocumentResponse
--
--         , responseDescribeGroups $
--             newDescribeGroupsResponse
--
--         , responseDescribeActivities $
--             newDescribeActivitiesResponse
--
--         , responseGetCurrentUser $
--             newGetCurrentUserResponse
--
--         , responseAddResourcePermissions $
--             newAddResourcePermissionsResponse
--
--         , responseUpdateUser $
--             newUpdateUserResponse
--
--         , responseDescribeNotificationSubscriptions $
--             newDescribeNotificationSubscriptionsResponse
--
--         , responseGetFolder $
--             newGetFolderResponse
--
--         , responseDeleteUser $
--             newDeleteUserResponse
--
--         , responseDeleteFolderContents $
--             newDeleteFolderContentsResponse
--
--         , responseDescribeComments $
--             newDescribeCommentsResponse
--
--         , responseInitiateDocumentVersionUpload $
--             newInitiateDocumentVersionUploadResponse
--
--         , responseRemoveAllResourcePermissions $
--             newRemoveAllResourcePermissionsResponse
--
--           ]
--     ]

-- Requests

requestDeleteCustomMetadata :: DeleteCustomMetadata -> TestTree
requestDeleteCustomMetadata =
  req
    "DeleteCustomMetadata"
    "fixture/DeleteCustomMetadata.yaml"

requestUpdateFolder :: UpdateFolder -> TestTree
requestUpdateFolder =
  req
    "UpdateFolder"
    "fixture/UpdateFolder.yaml"

requestDeleteNotificationSubscription :: DeleteNotificationSubscription -> TestTree
requestDeleteNotificationSubscription =
  req
    "DeleteNotificationSubscription"
    "fixture/DeleteNotificationSubscription.yaml"

requestDeleteFolder :: DeleteFolder -> TestTree
requestDeleteFolder =
  req
    "DeleteFolder"
    "fixture/DeleteFolder.yaml"

requestDeleteLabels :: DeleteLabels -> TestTree
requestDeleteLabels =
  req
    "DeleteLabels"
    "fixture/DeleteLabels.yaml"

requestAbortDocumentVersionUpload :: AbortDocumentVersionUpload -> TestTree
requestAbortDocumentVersionUpload =
  req
    "AbortDocumentVersionUpload"
    "fixture/AbortDocumentVersionUpload.yaml"

requestUpdateDocumentVersion :: UpdateDocumentVersion -> TestTree
requestUpdateDocumentVersion =
  req
    "UpdateDocumentVersion"
    "fixture/UpdateDocumentVersion.yaml"

requestDeactivateUser :: DeactivateUser -> TestTree
requestDeactivateUser =
  req
    "DeactivateUser"
    "fixture/DeactivateUser.yaml"

requestDescribeFolderContents :: DescribeFolderContents -> TestTree
requestDescribeFolderContents =
  req
    "DescribeFolderContents"
    "fixture/DescribeFolderContents.yaml"

requestCreateLabels :: CreateLabels -> TestTree
requestCreateLabels =
  req
    "CreateLabels"
    "fixture/CreateLabels.yaml"

requestDescribeRootFolders :: DescribeRootFolders -> TestTree
requestDescribeRootFolders =
  req
    "DescribeRootFolders"
    "fixture/DescribeRootFolders.yaml"

requestUpdateDocument :: UpdateDocument -> TestTree
requestUpdateDocument =
  req
    "UpdateDocument"
    "fixture/UpdateDocument.yaml"

requestDeleteDocument :: DeleteDocument -> TestTree
requestDeleteDocument =
  req
    "DeleteDocument"
    "fixture/DeleteDocument.yaml"

requestDescribeDocumentVersions :: DescribeDocumentVersions -> TestTree
requestDescribeDocumentVersions =
  req
    "DescribeDocumentVersions"
    "fixture/DescribeDocumentVersions.yaml"

requestGetDocumentVersion :: GetDocumentVersion -> TestTree
requestGetDocumentVersion =
  req
    "GetDocumentVersion"
    "fixture/GetDocumentVersion.yaml"

requestActivateUser :: ActivateUser -> TestTree
requestActivateUser =
  req
    "ActivateUser"
    "fixture/ActivateUser.yaml"

requestGetFolderPath :: GetFolderPath -> TestTree
requestGetFolderPath =
  req
    "GetFolderPath"
    "fixture/GetFolderPath.yaml"

requestCreateUser :: CreateUser -> TestTree
requestCreateUser =
  req
    "CreateUser"
    "fixture/CreateUser.yaml"

requestDeleteComment :: DeleteComment -> TestTree
requestDeleteComment =
  req
    "DeleteComment"
    "fixture/DeleteComment.yaml"

requestCreateFolder :: CreateFolder -> TestTree
requestCreateFolder =
  req
    "CreateFolder"
    "fixture/CreateFolder.yaml"

requestCreateCustomMetadata :: CreateCustomMetadata -> TestTree
requestCreateCustomMetadata =
  req
    "CreateCustomMetadata"
    "fixture/CreateCustomMetadata.yaml"

requestCreateNotificationSubscription :: CreateNotificationSubscription -> TestTree
requestCreateNotificationSubscription =
  req
    "CreateNotificationSubscription"
    "fixture/CreateNotificationSubscription.yaml"

requestGetResources :: GetResources -> TestTree
requestGetResources =
  req
    "GetResources"
    "fixture/GetResources.yaml"

requestRemoveResourcePermission :: RemoveResourcePermission -> TestTree
requestRemoveResourcePermission =
  req
    "RemoveResourcePermission"
    "fixture/RemoveResourcePermission.yaml"

requestCreateComment :: CreateComment -> TestTree
requestCreateComment =
  req
    "CreateComment"
    "fixture/CreateComment.yaml"

requestDescribeResourcePermissions :: DescribeResourcePermissions -> TestTree
requestDescribeResourcePermissions =
  req
    "DescribeResourcePermissions"
    "fixture/DescribeResourcePermissions.yaml"

requestDescribeUsers :: DescribeUsers -> TestTree
requestDescribeUsers =
  req
    "DescribeUsers"
    "fixture/DescribeUsers.yaml"

requestGetDocumentPath :: GetDocumentPath -> TestTree
requestGetDocumentPath =
  req
    "GetDocumentPath"
    "fixture/GetDocumentPath.yaml"

requestGetDocument :: GetDocument -> TestTree
requestGetDocument =
  req
    "GetDocument"
    "fixture/GetDocument.yaml"

requestDescribeGroups :: DescribeGroups -> TestTree
requestDescribeGroups =
  req
    "DescribeGroups"
    "fixture/DescribeGroups.yaml"

requestDescribeActivities :: DescribeActivities -> TestTree
requestDescribeActivities =
  req
    "DescribeActivities"
    "fixture/DescribeActivities.yaml"

requestGetCurrentUser :: GetCurrentUser -> TestTree
requestGetCurrentUser =
  req
    "GetCurrentUser"
    "fixture/GetCurrentUser.yaml"

requestAddResourcePermissions :: AddResourcePermissions -> TestTree
requestAddResourcePermissions =
  req
    "AddResourcePermissions"
    "fixture/AddResourcePermissions.yaml"

requestUpdateUser :: UpdateUser -> TestTree
requestUpdateUser =
  req
    "UpdateUser"
    "fixture/UpdateUser.yaml"

requestDescribeNotificationSubscriptions :: DescribeNotificationSubscriptions -> TestTree
requestDescribeNotificationSubscriptions =
  req
    "DescribeNotificationSubscriptions"
    "fixture/DescribeNotificationSubscriptions.yaml"

requestGetFolder :: GetFolder -> TestTree
requestGetFolder =
  req
    "GetFolder"
    "fixture/GetFolder.yaml"

requestDeleteUser :: DeleteUser -> TestTree
requestDeleteUser =
  req
    "DeleteUser"
    "fixture/DeleteUser.yaml"

requestDeleteFolderContents :: DeleteFolderContents -> TestTree
requestDeleteFolderContents =
  req
    "DeleteFolderContents"
    "fixture/DeleteFolderContents.yaml"

requestDescribeComments :: DescribeComments -> TestTree
requestDescribeComments =
  req
    "DescribeComments"
    "fixture/DescribeComments.yaml"

requestInitiateDocumentVersionUpload :: InitiateDocumentVersionUpload -> TestTree
requestInitiateDocumentVersionUpload =
  req
    "InitiateDocumentVersionUpload"
    "fixture/InitiateDocumentVersionUpload.yaml"

requestRemoveAllResourcePermissions :: RemoveAllResourcePermissions -> TestTree
requestRemoveAllResourcePermissions =
  req
    "RemoveAllResourcePermissions"
    "fixture/RemoveAllResourcePermissions.yaml"

-- Responses

responseDeleteCustomMetadata :: DeleteCustomMetadataResponse -> TestTree
responseDeleteCustomMetadata =
  res
    "DeleteCustomMetadataResponse"
    "fixture/DeleteCustomMetadataResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteCustomMetadata)

responseUpdateFolder :: UpdateFolderResponse -> TestTree
responseUpdateFolder =
  res
    "UpdateFolderResponse"
    "fixture/UpdateFolderResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateFolder)

responseDeleteNotificationSubscription :: DeleteNotificationSubscriptionResponse -> TestTree
responseDeleteNotificationSubscription =
  res
    "DeleteNotificationSubscriptionResponse"
    "fixture/DeleteNotificationSubscriptionResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteNotificationSubscription)

responseDeleteFolder :: DeleteFolderResponse -> TestTree
responseDeleteFolder =
  res
    "DeleteFolderResponse"
    "fixture/DeleteFolderResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteFolder)

responseDeleteLabels :: DeleteLabelsResponse -> TestTree
responseDeleteLabels =
  res
    "DeleteLabelsResponse"
    "fixture/DeleteLabelsResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteLabels)

responseAbortDocumentVersionUpload :: AbortDocumentVersionUploadResponse -> TestTree
responseAbortDocumentVersionUpload =
  res
    "AbortDocumentVersionUploadResponse"
    "fixture/AbortDocumentVersionUploadResponse.proto"
    defaultService
    (Proxy :: Proxy AbortDocumentVersionUpload)

responseUpdateDocumentVersion :: UpdateDocumentVersionResponse -> TestTree
responseUpdateDocumentVersion =
  res
    "UpdateDocumentVersionResponse"
    "fixture/UpdateDocumentVersionResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateDocumentVersion)

responseDeactivateUser :: DeactivateUserResponse -> TestTree
responseDeactivateUser =
  res
    "DeactivateUserResponse"
    "fixture/DeactivateUserResponse.proto"
    defaultService
    (Proxy :: Proxy DeactivateUser)

responseDescribeFolderContents :: DescribeFolderContentsResponse -> TestTree
responseDescribeFolderContents =
  res
    "DescribeFolderContentsResponse"
    "fixture/DescribeFolderContentsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeFolderContents)

responseCreateLabels :: CreateLabelsResponse -> TestTree
responseCreateLabels =
  res
    "CreateLabelsResponse"
    "fixture/CreateLabelsResponse.proto"
    defaultService
    (Proxy :: Proxy CreateLabels)

responseDescribeRootFolders :: DescribeRootFoldersResponse -> TestTree
responseDescribeRootFolders =
  res
    "DescribeRootFoldersResponse"
    "fixture/DescribeRootFoldersResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeRootFolders)

responseUpdateDocument :: UpdateDocumentResponse -> TestTree
responseUpdateDocument =
  res
    "UpdateDocumentResponse"
    "fixture/UpdateDocumentResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateDocument)

responseDeleteDocument :: DeleteDocumentResponse -> TestTree
responseDeleteDocument =
  res
    "DeleteDocumentResponse"
    "fixture/DeleteDocumentResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteDocument)

responseDescribeDocumentVersions :: DescribeDocumentVersionsResponse -> TestTree
responseDescribeDocumentVersions =
  res
    "DescribeDocumentVersionsResponse"
    "fixture/DescribeDocumentVersionsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeDocumentVersions)

responseGetDocumentVersion :: GetDocumentVersionResponse -> TestTree
responseGetDocumentVersion =
  res
    "GetDocumentVersionResponse"
    "fixture/GetDocumentVersionResponse.proto"
    defaultService
    (Proxy :: Proxy GetDocumentVersion)

responseActivateUser :: ActivateUserResponse -> TestTree
responseActivateUser =
  res
    "ActivateUserResponse"
    "fixture/ActivateUserResponse.proto"
    defaultService
    (Proxy :: Proxy ActivateUser)

responseGetFolderPath :: GetFolderPathResponse -> TestTree
responseGetFolderPath =
  res
    "GetFolderPathResponse"
    "fixture/GetFolderPathResponse.proto"
    defaultService
    (Proxy :: Proxy GetFolderPath)

responseCreateUser :: CreateUserResponse -> TestTree
responseCreateUser =
  res
    "CreateUserResponse"
    "fixture/CreateUserResponse.proto"
    defaultService
    (Proxy :: Proxy CreateUser)

responseDeleteComment :: DeleteCommentResponse -> TestTree
responseDeleteComment =
  res
    "DeleteCommentResponse"
    "fixture/DeleteCommentResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteComment)

responseCreateFolder :: CreateFolderResponse -> TestTree
responseCreateFolder =
  res
    "CreateFolderResponse"
    "fixture/CreateFolderResponse.proto"
    defaultService
    (Proxy :: Proxy CreateFolder)

responseCreateCustomMetadata :: CreateCustomMetadataResponse -> TestTree
responseCreateCustomMetadata =
  res
    "CreateCustomMetadataResponse"
    "fixture/CreateCustomMetadataResponse.proto"
    defaultService
    (Proxy :: Proxy CreateCustomMetadata)

responseCreateNotificationSubscription :: CreateNotificationSubscriptionResponse -> TestTree
responseCreateNotificationSubscription =
  res
    "CreateNotificationSubscriptionResponse"
    "fixture/CreateNotificationSubscriptionResponse.proto"
    defaultService
    (Proxy :: Proxy CreateNotificationSubscription)

responseGetResources :: GetResourcesResponse -> TestTree
responseGetResources =
  res
    "GetResourcesResponse"
    "fixture/GetResourcesResponse.proto"
    defaultService
    (Proxy :: Proxy GetResources)

responseRemoveResourcePermission :: RemoveResourcePermissionResponse -> TestTree
responseRemoveResourcePermission =
  res
    "RemoveResourcePermissionResponse"
    "fixture/RemoveResourcePermissionResponse.proto"
    defaultService
    (Proxy :: Proxy RemoveResourcePermission)

responseCreateComment :: CreateCommentResponse -> TestTree
responseCreateComment =
  res
    "CreateCommentResponse"
    "fixture/CreateCommentResponse.proto"
    defaultService
    (Proxy :: Proxy CreateComment)

responseDescribeResourcePermissions :: DescribeResourcePermissionsResponse -> TestTree
responseDescribeResourcePermissions =
  res
    "DescribeResourcePermissionsResponse"
    "fixture/DescribeResourcePermissionsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeResourcePermissions)

responseDescribeUsers :: DescribeUsersResponse -> TestTree
responseDescribeUsers =
  res
    "DescribeUsersResponse"
    "fixture/DescribeUsersResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeUsers)

responseGetDocumentPath :: GetDocumentPathResponse -> TestTree
responseGetDocumentPath =
  res
    "GetDocumentPathResponse"
    "fixture/GetDocumentPathResponse.proto"
    defaultService
    (Proxy :: Proxy GetDocumentPath)

responseGetDocument :: GetDocumentResponse -> TestTree
responseGetDocument =
  res
    "GetDocumentResponse"
    "fixture/GetDocumentResponse.proto"
    defaultService
    (Proxy :: Proxy GetDocument)

responseDescribeGroups :: DescribeGroupsResponse -> TestTree
responseDescribeGroups =
  res
    "DescribeGroupsResponse"
    "fixture/DescribeGroupsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeGroups)

responseDescribeActivities :: DescribeActivitiesResponse -> TestTree
responseDescribeActivities =
  res
    "DescribeActivitiesResponse"
    "fixture/DescribeActivitiesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeActivities)

responseGetCurrentUser :: GetCurrentUserResponse -> TestTree
responseGetCurrentUser =
  res
    "GetCurrentUserResponse"
    "fixture/GetCurrentUserResponse.proto"
    defaultService
    (Proxy :: Proxy GetCurrentUser)

responseAddResourcePermissions :: AddResourcePermissionsResponse -> TestTree
responseAddResourcePermissions =
  res
    "AddResourcePermissionsResponse"
    "fixture/AddResourcePermissionsResponse.proto"
    defaultService
    (Proxy :: Proxy AddResourcePermissions)

responseUpdateUser :: UpdateUserResponse -> TestTree
responseUpdateUser =
  res
    "UpdateUserResponse"
    "fixture/UpdateUserResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateUser)

responseDescribeNotificationSubscriptions :: DescribeNotificationSubscriptionsResponse -> TestTree
responseDescribeNotificationSubscriptions =
  res
    "DescribeNotificationSubscriptionsResponse"
    "fixture/DescribeNotificationSubscriptionsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeNotificationSubscriptions)

responseGetFolder :: GetFolderResponse -> TestTree
responseGetFolder =
  res
    "GetFolderResponse"
    "fixture/GetFolderResponse.proto"
    defaultService
    (Proxy :: Proxy GetFolder)

responseDeleteUser :: DeleteUserResponse -> TestTree
responseDeleteUser =
  res
    "DeleteUserResponse"
    "fixture/DeleteUserResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteUser)

responseDeleteFolderContents :: DeleteFolderContentsResponse -> TestTree
responseDeleteFolderContents =
  res
    "DeleteFolderContentsResponse"
    "fixture/DeleteFolderContentsResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteFolderContents)

responseDescribeComments :: DescribeCommentsResponse -> TestTree
responseDescribeComments =
  res
    "DescribeCommentsResponse"
    "fixture/DescribeCommentsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeComments)

responseInitiateDocumentVersionUpload :: InitiateDocumentVersionUploadResponse -> TestTree
responseInitiateDocumentVersionUpload =
  res
    "InitiateDocumentVersionUploadResponse"
    "fixture/InitiateDocumentVersionUploadResponse.proto"
    defaultService
    (Proxy :: Proxy InitiateDocumentVersionUpload)

responseRemoveAllResourcePermissions :: RemoveAllResourcePermissionsResponse -> TestTree
responseRemoveAllResourcePermissions =
  res
    "RemoveAllResourcePermissionsResponse"
    "fixture/RemoveAllResourcePermissionsResponse.proto"
    defaultService
    (Proxy :: Proxy RemoveAllResourcePermissions)
