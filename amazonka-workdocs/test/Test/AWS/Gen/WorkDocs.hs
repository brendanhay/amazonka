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
--         [ requestDeleteFolder $
--             newDeleteFolder
--
--         , requestUpdateFolder $
--             newUpdateFolder
--
--         , requestDeleteCustomMetadata $
--             newDeleteCustomMetadata
--
--         , requestDeleteNotificationSubscription $
--             newDeleteNotificationSubscription
--
--         , requestUpdateDocumentVersion $
--             newUpdateDocumentVersion
--
--         , requestDeleteLabels $
--             newDeleteLabels
--
--         , requestAbortDocumentVersionUpload $
--             newAbortDocumentVersionUpload
--
--         , requestDescribeFolderContents $
--             newDescribeFolderContents
--
--         , requestCreateLabels $
--             newCreateLabels
--
--         , requestDeactivateUser $
--             newDeactivateUser
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
--         , requestGetDocumentVersion $
--             newGetDocumentVersion
--
--         , requestDescribeDocumentVersions $
--             newDescribeDocumentVersions
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
--         , requestCreateCustomMetadata $
--             newCreateCustomMetadata
--
--         , requestDeleteComment $
--             newDeleteComment
--
--         , requestCreateFolder $
--             newCreateFolder
--
--         , requestCreateNotificationSubscription $
--             newCreateNotificationSubscription
--
--         , requestCreateComment $
--             newCreateComment
--
--         , requestDescribeResourcePermissions $
--             newDescribeResourcePermissions
--
--         , requestRemoveResourcePermission $
--             newRemoveResourcePermission
--
--         , requestDescribeUsers $
--             newDescribeUsers
--
--         , requestGetResources $
--             newGetResources
--
--         , requestGetDocumentPath $
--             newGetDocumentPath
--
--         , requestDescribeGroups $
--             newDescribeGroups
--
--         , requestGetDocument $
--             newGetDocument
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
--         , requestDeleteUser $
--             newDeleteUser
--
--         , requestGetFolder $
--             newGetFolder
--
--         , requestUpdateUser $
--             newUpdateUser
--
--         , requestDescribeNotificationSubscriptions $
--             newDescribeNotificationSubscriptions
--
--         , requestRemoveAllResourcePermissions $
--             newRemoveAllResourcePermissions
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
--           ]

--     , testGroup "response"
--         [ responseDeleteFolder $
--             newDeleteFolderResponse
--
--         , responseUpdateFolder $
--             newUpdateFolderResponse
--
--         , responseDeleteCustomMetadata $
--             newDeleteCustomMetadataResponse
--
--         , responseDeleteNotificationSubscription $
--             newDeleteNotificationSubscriptionResponse
--
--         , responseUpdateDocumentVersion $
--             newUpdateDocumentVersionResponse
--
--         , responseDeleteLabels $
--             newDeleteLabelsResponse
--
--         , responseAbortDocumentVersionUpload $
--             newAbortDocumentVersionUploadResponse
--
--         , responseDescribeFolderContents $
--             newDescribeFolderContentsResponse
--
--         , responseCreateLabels $
--             newCreateLabelsResponse
--
--         , responseDeactivateUser $
--             newDeactivateUserResponse
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
--         , responseGetDocumentVersion $
--             newGetDocumentVersionResponse
--
--         , responseDescribeDocumentVersions $
--             newDescribeDocumentVersionsResponse
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
--         , responseCreateCustomMetadata $
--             newCreateCustomMetadataResponse
--
--         , responseDeleteComment $
--             newDeleteCommentResponse
--
--         , responseCreateFolder $
--             newCreateFolderResponse
--
--         , responseCreateNotificationSubscription $
--             newCreateNotificationSubscriptionResponse
--
--         , responseCreateComment $
--             newCreateCommentResponse
--
--         , responseDescribeResourcePermissions $
--             newDescribeResourcePermissionsResponse
--
--         , responseRemoveResourcePermission $
--             newRemoveResourcePermissionResponse
--
--         , responseDescribeUsers $
--             newDescribeUsersResponse
--
--         , responseGetResources $
--             newGetResourcesResponse
--
--         , responseGetDocumentPath $
--             newGetDocumentPathResponse
--
--         , responseDescribeGroups $
--             newDescribeGroupsResponse
--
--         , responseGetDocument $
--             newGetDocumentResponse
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
--         , responseDeleteUser $
--             newDeleteUserResponse
--
--         , responseGetFolder $
--             newGetFolderResponse
--
--         , responseUpdateUser $
--             newUpdateUserResponse
--
--         , responseDescribeNotificationSubscriptions $
--             newDescribeNotificationSubscriptionsResponse
--
--         , responseRemoveAllResourcePermissions $
--             newRemoveAllResourcePermissionsResponse
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
--           ]
--     ]

-- Requests

requestDeleteFolder :: DeleteFolder -> TestTree
requestDeleteFolder =
  req
    "DeleteFolder"
    "fixture/DeleteFolder.yaml"

requestUpdateFolder :: UpdateFolder -> TestTree
requestUpdateFolder =
  req
    "UpdateFolder"
    "fixture/UpdateFolder.yaml"

requestDeleteCustomMetadata :: DeleteCustomMetadata -> TestTree
requestDeleteCustomMetadata =
  req
    "DeleteCustomMetadata"
    "fixture/DeleteCustomMetadata.yaml"

requestDeleteNotificationSubscription :: DeleteNotificationSubscription -> TestTree
requestDeleteNotificationSubscription =
  req
    "DeleteNotificationSubscription"
    "fixture/DeleteNotificationSubscription.yaml"

requestUpdateDocumentVersion :: UpdateDocumentVersion -> TestTree
requestUpdateDocumentVersion =
  req
    "UpdateDocumentVersion"
    "fixture/UpdateDocumentVersion.yaml"

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

requestDeactivateUser :: DeactivateUser -> TestTree
requestDeactivateUser =
  req
    "DeactivateUser"
    "fixture/DeactivateUser.yaml"

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

requestGetDocumentVersion :: GetDocumentVersion -> TestTree
requestGetDocumentVersion =
  req
    "GetDocumentVersion"
    "fixture/GetDocumentVersion.yaml"

requestDescribeDocumentVersions :: DescribeDocumentVersions -> TestTree
requestDescribeDocumentVersions =
  req
    "DescribeDocumentVersions"
    "fixture/DescribeDocumentVersions.yaml"

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

requestCreateCustomMetadata :: CreateCustomMetadata -> TestTree
requestCreateCustomMetadata =
  req
    "CreateCustomMetadata"
    "fixture/CreateCustomMetadata.yaml"

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

requestCreateNotificationSubscription :: CreateNotificationSubscription -> TestTree
requestCreateNotificationSubscription =
  req
    "CreateNotificationSubscription"
    "fixture/CreateNotificationSubscription.yaml"

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

requestRemoveResourcePermission :: RemoveResourcePermission -> TestTree
requestRemoveResourcePermission =
  req
    "RemoveResourcePermission"
    "fixture/RemoveResourcePermission.yaml"

requestDescribeUsers :: DescribeUsers -> TestTree
requestDescribeUsers =
  req
    "DescribeUsers"
    "fixture/DescribeUsers.yaml"

requestGetResources :: GetResources -> TestTree
requestGetResources =
  req
    "GetResources"
    "fixture/GetResources.yaml"

requestGetDocumentPath :: GetDocumentPath -> TestTree
requestGetDocumentPath =
  req
    "GetDocumentPath"
    "fixture/GetDocumentPath.yaml"

requestDescribeGroups :: DescribeGroups -> TestTree
requestDescribeGroups =
  req
    "DescribeGroups"
    "fixture/DescribeGroups.yaml"

requestGetDocument :: GetDocument -> TestTree
requestGetDocument =
  req
    "GetDocument"
    "fixture/GetDocument.yaml"

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

requestDeleteUser :: DeleteUser -> TestTree
requestDeleteUser =
  req
    "DeleteUser"
    "fixture/DeleteUser.yaml"

requestGetFolder :: GetFolder -> TestTree
requestGetFolder =
  req
    "GetFolder"
    "fixture/GetFolder.yaml"

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

requestRemoveAllResourcePermissions :: RemoveAllResourcePermissions -> TestTree
requestRemoveAllResourcePermissions =
  req
    "RemoveAllResourcePermissions"
    "fixture/RemoveAllResourcePermissions.yaml"

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

-- Responses

responseDeleteFolder :: DeleteFolderResponse -> TestTree
responseDeleteFolder =
  res
    "DeleteFolderResponse"
    "fixture/DeleteFolderResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteFolder)

responseUpdateFolder :: UpdateFolderResponse -> TestTree
responseUpdateFolder =
  res
    "UpdateFolderResponse"
    "fixture/UpdateFolderResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateFolder)

responseDeleteCustomMetadata :: DeleteCustomMetadataResponse -> TestTree
responseDeleteCustomMetadata =
  res
    "DeleteCustomMetadataResponse"
    "fixture/DeleteCustomMetadataResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteCustomMetadata)

responseDeleteNotificationSubscription :: DeleteNotificationSubscriptionResponse -> TestTree
responseDeleteNotificationSubscription =
  res
    "DeleteNotificationSubscriptionResponse"
    "fixture/DeleteNotificationSubscriptionResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteNotificationSubscription)

responseUpdateDocumentVersion :: UpdateDocumentVersionResponse -> TestTree
responseUpdateDocumentVersion =
  res
    "UpdateDocumentVersionResponse"
    "fixture/UpdateDocumentVersionResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateDocumentVersion)

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

responseDeactivateUser :: DeactivateUserResponse -> TestTree
responseDeactivateUser =
  res
    "DeactivateUserResponse"
    "fixture/DeactivateUserResponse.proto"
    defaultService
    (Proxy :: Proxy DeactivateUser)

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

responseGetDocumentVersion :: GetDocumentVersionResponse -> TestTree
responseGetDocumentVersion =
  res
    "GetDocumentVersionResponse"
    "fixture/GetDocumentVersionResponse.proto"
    defaultService
    (Proxy :: Proxy GetDocumentVersion)

responseDescribeDocumentVersions :: DescribeDocumentVersionsResponse -> TestTree
responseDescribeDocumentVersions =
  res
    "DescribeDocumentVersionsResponse"
    "fixture/DescribeDocumentVersionsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeDocumentVersions)

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

responseCreateCustomMetadata :: CreateCustomMetadataResponse -> TestTree
responseCreateCustomMetadata =
  res
    "CreateCustomMetadataResponse"
    "fixture/CreateCustomMetadataResponse.proto"
    defaultService
    (Proxy :: Proxy CreateCustomMetadata)

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

responseCreateNotificationSubscription :: CreateNotificationSubscriptionResponse -> TestTree
responseCreateNotificationSubscription =
  res
    "CreateNotificationSubscriptionResponse"
    "fixture/CreateNotificationSubscriptionResponse.proto"
    defaultService
    (Proxy :: Proxy CreateNotificationSubscription)

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

responseRemoveResourcePermission :: RemoveResourcePermissionResponse -> TestTree
responseRemoveResourcePermission =
  res
    "RemoveResourcePermissionResponse"
    "fixture/RemoveResourcePermissionResponse.proto"
    defaultService
    (Proxy :: Proxy RemoveResourcePermission)

responseDescribeUsers :: DescribeUsersResponse -> TestTree
responseDescribeUsers =
  res
    "DescribeUsersResponse"
    "fixture/DescribeUsersResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeUsers)

responseGetResources :: GetResourcesResponse -> TestTree
responseGetResources =
  res
    "GetResourcesResponse"
    "fixture/GetResourcesResponse.proto"
    defaultService
    (Proxy :: Proxy GetResources)

responseGetDocumentPath :: GetDocumentPathResponse -> TestTree
responseGetDocumentPath =
  res
    "GetDocumentPathResponse"
    "fixture/GetDocumentPathResponse.proto"
    defaultService
    (Proxy :: Proxy GetDocumentPath)

responseDescribeGroups :: DescribeGroupsResponse -> TestTree
responseDescribeGroups =
  res
    "DescribeGroupsResponse"
    "fixture/DescribeGroupsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeGroups)

responseGetDocument :: GetDocumentResponse -> TestTree
responseGetDocument =
  res
    "GetDocumentResponse"
    "fixture/GetDocumentResponse.proto"
    defaultService
    (Proxy :: Proxy GetDocument)

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

responseDeleteUser :: DeleteUserResponse -> TestTree
responseDeleteUser =
  res
    "DeleteUserResponse"
    "fixture/DeleteUserResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteUser)

responseGetFolder :: GetFolderResponse -> TestTree
responseGetFolder =
  res
    "GetFolderResponse"
    "fixture/GetFolderResponse.proto"
    defaultService
    (Proxy :: Proxy GetFolder)

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

responseRemoveAllResourcePermissions :: RemoveAllResourcePermissionsResponse -> TestTree
responseRemoveAllResourcePermissions =
  res
    "RemoveAllResourcePermissionsResponse"
    "fixture/RemoveAllResourcePermissionsResponse.proto"
    defaultService
    (Proxy :: Proxy RemoveAllResourcePermissions)

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
