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

import Amazonka.WorkDocs
import qualified Data.Proxy as Proxy
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
--         [ requestDeleteLabels $
--             newDeleteLabels
--
--         , requestAbortDocumentVersionUpload $
--             newAbortDocumentVersionUpload
--
--         , requestGetDocumentPath $
--             newGetDocumentPath
--
--         , requestCreateComment $
--             newCreateComment
--
--         , requestDescribeUsers $
--             newDescribeUsers
--
--         , requestDeleteFolder $
--             newDeleteFolder
--
--         , requestUpdateFolder $
--             newUpdateFolder
--
--         , requestDeleteCustomMetadata $
--             newDeleteCustomMetadata
--
--         , requestDescribeResourcePermissions $
--             newDescribeResourcePermissions
--
--         , requestDeleteNotificationSubscription $
--             newDeleteNotificationSubscription
--
--         , requestCreateFolder $
--             newCreateFolder
--
--         , requestCreateNotificationSubscription $
--             newCreateNotificationSubscription
--
--         , requestCreateCustomMetadata $
--             newCreateCustomMetadata
--
--         , requestGetFolderPath $
--             newGetFolderPath
--
--         , requestDescribeComments $
--             newDescribeComments
--
--         , requestDeleteFolderContents $
--             newDeleteFolderContents
--
--         , requestRemoveAllResourcePermissions $
--             newRemoveAllResourcePermissions
--
--         , requestGetFolder $
--             newGetFolder
--
--         , requestDescribeNotificationSubscriptions $
--             newDescribeNotificationSubscriptions
--
--         , requestActivateUser $
--             newActivateUser
--
--         , requestDescribeDocumentVersions $
--             newDescribeDocumentVersions
--
--         , requestGetDocumentVersion $
--             newGetDocumentVersion
--
--         , requestDescribeActivities $
--             newDescribeActivities
--
--         , requestDescribeRootFolders $
--             newDescribeRootFolders
--
--         , requestGetCurrentUser $
--             newGetCurrentUser
--
--         , requestDeactivateUser $
--             newDeactivateUser
--
--         , requestGetDocument $
--             newGetDocument
--
--         , requestDescribeFolderContents $
--             newDescribeFolderContents
--
--         , requestCreateLabels $
--             newCreateLabels
--
--         , requestUpdateDocumentVersion $
--             newUpdateDocumentVersion
--
--         , requestRemoveResourcePermission $
--             newRemoveResourcePermission
--
--         , requestGetResources $
--             newGetResources
--
--         , requestDeleteComment $
--             newDeleteComment
--
--         , requestInitiateDocumentVersionUpload $
--             newInitiateDocumentVersionUpload
--
--         , requestCreateUser $
--             newCreateUser
--
--         , requestUpdateUser $
--             newUpdateUser
--
--         , requestDeleteUser $
--             newDeleteUser
--
--         , requestAddResourcePermissions $
--             newAddResourcePermissions
--
--         , requestUpdateDocument $
--             newUpdateDocument
--
--         , requestDeleteDocument $
--             newDeleteDocument
--
--         , requestDescribeGroups $
--             newDescribeGroups
--
--           ]

--     , testGroup "response"
--         [ responseDeleteLabels $
--             newDeleteLabelsResponse
--
--         , responseAbortDocumentVersionUpload $
--             newAbortDocumentVersionUploadResponse
--
--         , responseGetDocumentPath $
--             newGetDocumentPathResponse
--
--         , responseCreateComment $
--             newCreateCommentResponse
--
--         , responseDescribeUsers $
--             newDescribeUsersResponse
--
--         , responseDeleteFolder $
--             newDeleteFolderResponse
--
--         , responseUpdateFolder $
--             newUpdateFolderResponse
--
--         , responseDeleteCustomMetadata $
--             newDeleteCustomMetadataResponse
--
--         , responseDescribeResourcePermissions $
--             newDescribeResourcePermissionsResponse
--
--         , responseDeleteNotificationSubscription $
--             newDeleteNotificationSubscriptionResponse
--
--         , responseCreateFolder $
--             newCreateFolderResponse
--
--         , responseCreateNotificationSubscription $
--             newCreateNotificationSubscriptionResponse
--
--         , responseCreateCustomMetadata $
--             newCreateCustomMetadataResponse
--
--         , responseGetFolderPath $
--             newGetFolderPathResponse
--
--         , responseDescribeComments $
--             newDescribeCommentsResponse
--
--         , responseDeleteFolderContents $
--             newDeleteFolderContentsResponse
--
--         , responseRemoveAllResourcePermissions $
--             newRemoveAllResourcePermissionsResponse
--
--         , responseGetFolder $
--             newGetFolderResponse
--
--         , responseDescribeNotificationSubscriptions $
--             newDescribeNotificationSubscriptionsResponse
--
--         , responseActivateUser $
--             newActivateUserResponse
--
--         , responseDescribeDocumentVersions $
--             newDescribeDocumentVersionsResponse
--
--         , responseGetDocumentVersion $
--             newGetDocumentVersionResponse
--
--         , responseDescribeActivities $
--             newDescribeActivitiesResponse
--
--         , responseDescribeRootFolders $
--             newDescribeRootFoldersResponse
--
--         , responseGetCurrentUser $
--             newGetCurrentUserResponse
--
--         , responseDeactivateUser $
--             newDeactivateUserResponse
--
--         , responseGetDocument $
--             newGetDocumentResponse
--
--         , responseDescribeFolderContents $
--             newDescribeFolderContentsResponse
--
--         , responseCreateLabels $
--             newCreateLabelsResponse
--
--         , responseUpdateDocumentVersion $
--             newUpdateDocumentVersionResponse
--
--         , responseRemoveResourcePermission $
--             newRemoveResourcePermissionResponse
--
--         , responseGetResources $
--             newGetResourcesResponse
--
--         , responseDeleteComment $
--             newDeleteCommentResponse
--
--         , responseInitiateDocumentVersionUpload $
--             newInitiateDocumentVersionUploadResponse
--
--         , responseCreateUser $
--             newCreateUserResponse
--
--         , responseUpdateUser $
--             newUpdateUserResponse
--
--         , responseDeleteUser $
--             newDeleteUserResponse
--
--         , responseAddResourcePermissions $
--             newAddResourcePermissionsResponse
--
--         , responseUpdateDocument $
--             newUpdateDocumentResponse
--
--         , responseDeleteDocument $
--             newDeleteDocumentResponse
--
--         , responseDescribeGroups $
--             newDescribeGroupsResponse
--
--           ]
--     ]

-- Requests

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

requestGetDocumentPath :: GetDocumentPath -> TestTree
requestGetDocumentPath =
  req
    "GetDocumentPath"
    "fixture/GetDocumentPath.yaml"

requestCreateComment :: CreateComment -> TestTree
requestCreateComment =
  req
    "CreateComment"
    "fixture/CreateComment.yaml"

requestDescribeUsers :: DescribeUsers -> TestTree
requestDescribeUsers =
  req
    "DescribeUsers"
    "fixture/DescribeUsers.yaml"

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

requestDescribeResourcePermissions :: DescribeResourcePermissions -> TestTree
requestDescribeResourcePermissions =
  req
    "DescribeResourcePermissions"
    "fixture/DescribeResourcePermissions.yaml"

requestDeleteNotificationSubscription :: DeleteNotificationSubscription -> TestTree
requestDeleteNotificationSubscription =
  req
    "DeleteNotificationSubscription"
    "fixture/DeleteNotificationSubscription.yaml"

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

requestCreateCustomMetadata :: CreateCustomMetadata -> TestTree
requestCreateCustomMetadata =
  req
    "CreateCustomMetadata"
    "fixture/CreateCustomMetadata.yaml"

requestGetFolderPath :: GetFolderPath -> TestTree
requestGetFolderPath =
  req
    "GetFolderPath"
    "fixture/GetFolderPath.yaml"

requestDescribeComments :: DescribeComments -> TestTree
requestDescribeComments =
  req
    "DescribeComments"
    "fixture/DescribeComments.yaml"

requestDeleteFolderContents :: DeleteFolderContents -> TestTree
requestDeleteFolderContents =
  req
    "DeleteFolderContents"
    "fixture/DeleteFolderContents.yaml"

requestRemoveAllResourcePermissions :: RemoveAllResourcePermissions -> TestTree
requestRemoveAllResourcePermissions =
  req
    "RemoveAllResourcePermissions"
    "fixture/RemoveAllResourcePermissions.yaml"

requestGetFolder :: GetFolder -> TestTree
requestGetFolder =
  req
    "GetFolder"
    "fixture/GetFolder.yaml"

requestDescribeNotificationSubscriptions :: DescribeNotificationSubscriptions -> TestTree
requestDescribeNotificationSubscriptions =
  req
    "DescribeNotificationSubscriptions"
    "fixture/DescribeNotificationSubscriptions.yaml"

requestActivateUser :: ActivateUser -> TestTree
requestActivateUser =
  req
    "ActivateUser"
    "fixture/ActivateUser.yaml"

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

requestDescribeActivities :: DescribeActivities -> TestTree
requestDescribeActivities =
  req
    "DescribeActivities"
    "fixture/DescribeActivities.yaml"

requestDescribeRootFolders :: DescribeRootFolders -> TestTree
requestDescribeRootFolders =
  req
    "DescribeRootFolders"
    "fixture/DescribeRootFolders.yaml"

requestGetCurrentUser :: GetCurrentUser -> TestTree
requestGetCurrentUser =
  req
    "GetCurrentUser"
    "fixture/GetCurrentUser.yaml"

requestDeactivateUser :: DeactivateUser -> TestTree
requestDeactivateUser =
  req
    "DeactivateUser"
    "fixture/DeactivateUser.yaml"

requestGetDocument :: GetDocument -> TestTree
requestGetDocument =
  req
    "GetDocument"
    "fixture/GetDocument.yaml"

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

requestUpdateDocumentVersion :: UpdateDocumentVersion -> TestTree
requestUpdateDocumentVersion =
  req
    "UpdateDocumentVersion"
    "fixture/UpdateDocumentVersion.yaml"

requestRemoveResourcePermission :: RemoveResourcePermission -> TestTree
requestRemoveResourcePermission =
  req
    "RemoveResourcePermission"
    "fixture/RemoveResourcePermission.yaml"

requestGetResources :: GetResources -> TestTree
requestGetResources =
  req
    "GetResources"
    "fixture/GetResources.yaml"

requestDeleteComment :: DeleteComment -> TestTree
requestDeleteComment =
  req
    "DeleteComment"
    "fixture/DeleteComment.yaml"

requestInitiateDocumentVersionUpload :: InitiateDocumentVersionUpload -> TestTree
requestInitiateDocumentVersionUpload =
  req
    "InitiateDocumentVersionUpload"
    "fixture/InitiateDocumentVersionUpload.yaml"

requestCreateUser :: CreateUser -> TestTree
requestCreateUser =
  req
    "CreateUser"
    "fixture/CreateUser.yaml"

requestUpdateUser :: UpdateUser -> TestTree
requestUpdateUser =
  req
    "UpdateUser"
    "fixture/UpdateUser.yaml"

requestDeleteUser :: DeleteUser -> TestTree
requestDeleteUser =
  req
    "DeleteUser"
    "fixture/DeleteUser.yaml"

requestAddResourcePermissions :: AddResourcePermissions -> TestTree
requestAddResourcePermissions =
  req
    "AddResourcePermissions"
    "fixture/AddResourcePermissions.yaml"

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

requestDescribeGroups :: DescribeGroups -> TestTree
requestDescribeGroups =
  req
    "DescribeGroups"
    "fixture/DescribeGroups.yaml"

-- Responses

responseDeleteLabels :: DeleteLabelsResponse -> TestTree
responseDeleteLabels =
  res
    "DeleteLabelsResponse"
    "fixture/DeleteLabelsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteLabels)

responseAbortDocumentVersionUpload :: AbortDocumentVersionUploadResponse -> TestTree
responseAbortDocumentVersionUpload =
  res
    "AbortDocumentVersionUploadResponse"
    "fixture/AbortDocumentVersionUploadResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AbortDocumentVersionUpload)

responseGetDocumentPath :: GetDocumentPathResponse -> TestTree
responseGetDocumentPath =
  res
    "GetDocumentPathResponse"
    "fixture/GetDocumentPathResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetDocumentPath)

responseCreateComment :: CreateCommentResponse -> TestTree
responseCreateComment =
  res
    "CreateCommentResponse"
    "fixture/CreateCommentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateComment)

responseDescribeUsers :: DescribeUsersResponse -> TestTree
responseDescribeUsers =
  res
    "DescribeUsersResponse"
    "fixture/DescribeUsersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeUsers)

responseDeleteFolder :: DeleteFolderResponse -> TestTree
responseDeleteFolder =
  res
    "DeleteFolderResponse"
    "fixture/DeleteFolderResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteFolder)

responseUpdateFolder :: UpdateFolderResponse -> TestTree
responseUpdateFolder =
  res
    "UpdateFolderResponse"
    "fixture/UpdateFolderResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateFolder)

responseDeleteCustomMetadata :: DeleteCustomMetadataResponse -> TestTree
responseDeleteCustomMetadata =
  res
    "DeleteCustomMetadataResponse"
    "fixture/DeleteCustomMetadataResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteCustomMetadata)

responseDescribeResourcePermissions :: DescribeResourcePermissionsResponse -> TestTree
responseDescribeResourcePermissions =
  res
    "DescribeResourcePermissionsResponse"
    "fixture/DescribeResourcePermissionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeResourcePermissions)

responseDeleteNotificationSubscription :: DeleteNotificationSubscriptionResponse -> TestTree
responseDeleteNotificationSubscription =
  res
    "DeleteNotificationSubscriptionResponse"
    "fixture/DeleteNotificationSubscriptionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteNotificationSubscription)

responseCreateFolder :: CreateFolderResponse -> TestTree
responseCreateFolder =
  res
    "CreateFolderResponse"
    "fixture/CreateFolderResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateFolder)

responseCreateNotificationSubscription :: CreateNotificationSubscriptionResponse -> TestTree
responseCreateNotificationSubscription =
  res
    "CreateNotificationSubscriptionResponse"
    "fixture/CreateNotificationSubscriptionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateNotificationSubscription)

responseCreateCustomMetadata :: CreateCustomMetadataResponse -> TestTree
responseCreateCustomMetadata =
  res
    "CreateCustomMetadataResponse"
    "fixture/CreateCustomMetadataResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateCustomMetadata)

responseGetFolderPath :: GetFolderPathResponse -> TestTree
responseGetFolderPath =
  res
    "GetFolderPathResponse"
    "fixture/GetFolderPathResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetFolderPath)

responseDescribeComments :: DescribeCommentsResponse -> TestTree
responseDescribeComments =
  res
    "DescribeCommentsResponse"
    "fixture/DescribeCommentsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeComments)

responseDeleteFolderContents :: DeleteFolderContentsResponse -> TestTree
responseDeleteFolderContents =
  res
    "DeleteFolderContentsResponse"
    "fixture/DeleteFolderContentsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteFolderContents)

responseRemoveAllResourcePermissions :: RemoveAllResourcePermissionsResponse -> TestTree
responseRemoveAllResourcePermissions =
  res
    "RemoveAllResourcePermissionsResponse"
    "fixture/RemoveAllResourcePermissionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RemoveAllResourcePermissions)

responseGetFolder :: GetFolderResponse -> TestTree
responseGetFolder =
  res
    "GetFolderResponse"
    "fixture/GetFolderResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetFolder)

responseDescribeNotificationSubscriptions :: DescribeNotificationSubscriptionsResponse -> TestTree
responseDescribeNotificationSubscriptions =
  res
    "DescribeNotificationSubscriptionsResponse"
    "fixture/DescribeNotificationSubscriptionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeNotificationSubscriptions)

responseActivateUser :: ActivateUserResponse -> TestTree
responseActivateUser =
  res
    "ActivateUserResponse"
    "fixture/ActivateUserResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ActivateUser)

responseDescribeDocumentVersions :: DescribeDocumentVersionsResponse -> TestTree
responseDescribeDocumentVersions =
  res
    "DescribeDocumentVersionsResponse"
    "fixture/DescribeDocumentVersionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeDocumentVersions)

responseGetDocumentVersion :: GetDocumentVersionResponse -> TestTree
responseGetDocumentVersion =
  res
    "GetDocumentVersionResponse"
    "fixture/GetDocumentVersionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetDocumentVersion)

responseDescribeActivities :: DescribeActivitiesResponse -> TestTree
responseDescribeActivities =
  res
    "DescribeActivitiesResponse"
    "fixture/DescribeActivitiesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeActivities)

responseDescribeRootFolders :: DescribeRootFoldersResponse -> TestTree
responseDescribeRootFolders =
  res
    "DescribeRootFoldersResponse"
    "fixture/DescribeRootFoldersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeRootFolders)

responseGetCurrentUser :: GetCurrentUserResponse -> TestTree
responseGetCurrentUser =
  res
    "GetCurrentUserResponse"
    "fixture/GetCurrentUserResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetCurrentUser)

responseDeactivateUser :: DeactivateUserResponse -> TestTree
responseDeactivateUser =
  res
    "DeactivateUserResponse"
    "fixture/DeactivateUserResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeactivateUser)

responseGetDocument :: GetDocumentResponse -> TestTree
responseGetDocument =
  res
    "GetDocumentResponse"
    "fixture/GetDocumentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetDocument)

responseDescribeFolderContents :: DescribeFolderContentsResponse -> TestTree
responseDescribeFolderContents =
  res
    "DescribeFolderContentsResponse"
    "fixture/DescribeFolderContentsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeFolderContents)

responseCreateLabels :: CreateLabelsResponse -> TestTree
responseCreateLabels =
  res
    "CreateLabelsResponse"
    "fixture/CreateLabelsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateLabels)

responseUpdateDocumentVersion :: UpdateDocumentVersionResponse -> TestTree
responseUpdateDocumentVersion =
  res
    "UpdateDocumentVersionResponse"
    "fixture/UpdateDocumentVersionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateDocumentVersion)

responseRemoveResourcePermission :: RemoveResourcePermissionResponse -> TestTree
responseRemoveResourcePermission =
  res
    "RemoveResourcePermissionResponse"
    "fixture/RemoveResourcePermissionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RemoveResourcePermission)

responseGetResources :: GetResourcesResponse -> TestTree
responseGetResources =
  res
    "GetResourcesResponse"
    "fixture/GetResourcesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetResources)

responseDeleteComment :: DeleteCommentResponse -> TestTree
responseDeleteComment =
  res
    "DeleteCommentResponse"
    "fixture/DeleteCommentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteComment)

responseInitiateDocumentVersionUpload :: InitiateDocumentVersionUploadResponse -> TestTree
responseInitiateDocumentVersionUpload =
  res
    "InitiateDocumentVersionUploadResponse"
    "fixture/InitiateDocumentVersionUploadResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy InitiateDocumentVersionUpload)

responseCreateUser :: CreateUserResponse -> TestTree
responseCreateUser =
  res
    "CreateUserResponse"
    "fixture/CreateUserResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateUser)

responseUpdateUser :: UpdateUserResponse -> TestTree
responseUpdateUser =
  res
    "UpdateUserResponse"
    "fixture/UpdateUserResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateUser)

responseDeleteUser :: DeleteUserResponse -> TestTree
responseDeleteUser =
  res
    "DeleteUserResponse"
    "fixture/DeleteUserResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteUser)

responseAddResourcePermissions :: AddResourcePermissionsResponse -> TestTree
responseAddResourcePermissions =
  res
    "AddResourcePermissionsResponse"
    "fixture/AddResourcePermissionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AddResourcePermissions)

responseUpdateDocument :: UpdateDocumentResponse -> TestTree
responseUpdateDocument =
  res
    "UpdateDocumentResponse"
    "fixture/UpdateDocumentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateDocument)

responseDeleteDocument :: DeleteDocumentResponse -> TestTree
responseDeleteDocument =
  res
    "DeleteDocumentResponse"
    "fixture/DeleteDocumentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteDocument)

responseDescribeGroups :: DescribeGroupsResponse -> TestTree
responseDescribeGroups =
  res
    "DescribeGroupsResponse"
    "fixture/DescribeGroupsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeGroups)
