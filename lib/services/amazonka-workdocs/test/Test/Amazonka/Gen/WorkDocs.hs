{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.WorkDocs
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.WorkDocs where

import Amazonka.WorkDocs
import qualified Data.Proxy as Proxy
import Test.Amazonka.Fixture
import Test.Amazonka.Prelude
import Test.Amazonka.WorkDocs.Internal
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestAbortDocumentVersionUpload $
--             newAbortDocumentVersionUpload
--
--         , requestActivateUser $
--             newActivateUser
--
--         , requestAddResourcePermissions $
--             newAddResourcePermissions
--
--         , requestCreateComment $
--             newCreateComment
--
--         , requestCreateCustomMetadata $
--             newCreateCustomMetadata
--
--         , requestCreateFolder $
--             newCreateFolder
--
--         , requestCreateLabels $
--             newCreateLabels
--
--         , requestCreateNotificationSubscription $
--             newCreateNotificationSubscription
--
--         , requestCreateUser $
--             newCreateUser
--
--         , requestDeactivateUser $
--             newDeactivateUser
--
--         , requestDeleteComment $
--             newDeleteComment
--
--         , requestDeleteCustomMetadata $
--             newDeleteCustomMetadata
--
--         , requestDeleteDocument $
--             newDeleteDocument
--
--         , requestDeleteDocumentVersion $
--             newDeleteDocumentVersion
--
--         , requestDeleteFolder $
--             newDeleteFolder
--
--         , requestDeleteFolderContents $
--             newDeleteFolderContents
--
--         , requestDeleteLabels $
--             newDeleteLabels
--
--         , requestDeleteNotificationSubscription $
--             newDeleteNotificationSubscription
--
--         , requestDeleteUser $
--             newDeleteUser
--
--         , requestDescribeActivities $
--             newDescribeActivities
--
--         , requestDescribeComments $
--             newDescribeComments
--
--         , requestDescribeDocumentVersions $
--             newDescribeDocumentVersions
--
--         , requestDescribeFolderContents $
--             newDescribeFolderContents
--
--         , requestDescribeGroups $
--             newDescribeGroups
--
--         , requestDescribeNotificationSubscriptions $
--             newDescribeNotificationSubscriptions
--
--         , requestDescribeResourcePermissions $
--             newDescribeResourcePermissions
--
--         , requestDescribeRootFolders $
--             newDescribeRootFolders
--
--         , requestDescribeUsers $
--             newDescribeUsers
--
--         , requestGetCurrentUser $
--             newGetCurrentUser
--
--         , requestGetDocument $
--             newGetDocument
--
--         , requestGetDocumentPath $
--             newGetDocumentPath
--
--         , requestGetDocumentVersion $
--             newGetDocumentVersion
--
--         , requestGetFolder $
--             newGetFolder
--
--         , requestGetFolderPath $
--             newGetFolderPath
--
--         , requestGetResources $
--             newGetResources
--
--         , requestInitiateDocumentVersionUpload $
--             newInitiateDocumentVersionUpload
--
--         , requestRemoveAllResourcePermissions $
--             newRemoveAllResourcePermissions
--
--         , requestRemoveResourcePermission $
--             newRemoveResourcePermission
--
--         , requestRestoreDocumentVersions $
--             newRestoreDocumentVersions
--
--         , requestUpdateDocument $
--             newUpdateDocument
--
--         , requestUpdateDocumentVersion $
--             newUpdateDocumentVersion
--
--         , requestUpdateFolder $
--             newUpdateFolder
--
--         , requestUpdateUser $
--             newUpdateUser
--
--           ]

--     , testGroup "response"
--         [ responseAbortDocumentVersionUpload $
--             newAbortDocumentVersionUploadResponse
--
--         , responseActivateUser $
--             newActivateUserResponse
--
--         , responseAddResourcePermissions $
--             newAddResourcePermissionsResponse
--
--         , responseCreateComment $
--             newCreateCommentResponse
--
--         , responseCreateCustomMetadata $
--             newCreateCustomMetadataResponse
--
--         , responseCreateFolder $
--             newCreateFolderResponse
--
--         , responseCreateLabels $
--             newCreateLabelsResponse
--
--         , responseCreateNotificationSubscription $
--             newCreateNotificationSubscriptionResponse
--
--         , responseCreateUser $
--             newCreateUserResponse
--
--         , responseDeactivateUser $
--             newDeactivateUserResponse
--
--         , responseDeleteComment $
--             newDeleteCommentResponse
--
--         , responseDeleteCustomMetadata $
--             newDeleteCustomMetadataResponse
--
--         , responseDeleteDocument $
--             newDeleteDocumentResponse
--
--         , responseDeleteDocumentVersion $
--             newDeleteDocumentVersionResponse
--
--         , responseDeleteFolder $
--             newDeleteFolderResponse
--
--         , responseDeleteFolderContents $
--             newDeleteFolderContentsResponse
--
--         , responseDeleteLabels $
--             newDeleteLabelsResponse
--
--         , responseDeleteNotificationSubscription $
--             newDeleteNotificationSubscriptionResponse
--
--         , responseDeleteUser $
--             newDeleteUserResponse
--
--         , responseDescribeActivities $
--             newDescribeActivitiesResponse
--
--         , responseDescribeComments $
--             newDescribeCommentsResponse
--
--         , responseDescribeDocumentVersions $
--             newDescribeDocumentVersionsResponse
--
--         , responseDescribeFolderContents $
--             newDescribeFolderContentsResponse
--
--         , responseDescribeGroups $
--             newDescribeGroupsResponse
--
--         , responseDescribeNotificationSubscriptions $
--             newDescribeNotificationSubscriptionsResponse
--
--         , responseDescribeResourcePermissions $
--             newDescribeResourcePermissionsResponse
--
--         , responseDescribeRootFolders $
--             newDescribeRootFoldersResponse
--
--         , responseDescribeUsers $
--             newDescribeUsersResponse
--
--         , responseGetCurrentUser $
--             newGetCurrentUserResponse
--
--         , responseGetDocument $
--             newGetDocumentResponse
--
--         , responseGetDocumentPath $
--             newGetDocumentPathResponse
--
--         , responseGetDocumentVersion $
--             newGetDocumentVersionResponse
--
--         , responseGetFolder $
--             newGetFolderResponse
--
--         , responseGetFolderPath $
--             newGetFolderPathResponse
--
--         , responseGetResources $
--             newGetResourcesResponse
--
--         , responseInitiateDocumentVersionUpload $
--             newInitiateDocumentVersionUploadResponse
--
--         , responseRemoveAllResourcePermissions $
--             newRemoveAllResourcePermissionsResponse
--
--         , responseRemoveResourcePermission $
--             newRemoveResourcePermissionResponse
--
--         , responseRestoreDocumentVersions $
--             newRestoreDocumentVersionsResponse
--
--         , responseUpdateDocument $
--             newUpdateDocumentResponse
--
--         , responseUpdateDocumentVersion $
--             newUpdateDocumentVersionResponse
--
--         , responseUpdateFolder $
--             newUpdateFolderResponse
--
--         , responseUpdateUser $
--             newUpdateUserResponse
--
--           ]
--     ]

-- Requests

requestAbortDocumentVersionUpload :: AbortDocumentVersionUpload -> TestTree
requestAbortDocumentVersionUpload =
  req
    "AbortDocumentVersionUpload"
    "fixture/AbortDocumentVersionUpload.yaml"

requestActivateUser :: ActivateUser -> TestTree
requestActivateUser =
  req
    "ActivateUser"
    "fixture/ActivateUser.yaml"

requestAddResourcePermissions :: AddResourcePermissions -> TestTree
requestAddResourcePermissions =
  req
    "AddResourcePermissions"
    "fixture/AddResourcePermissions.yaml"

requestCreateComment :: CreateComment -> TestTree
requestCreateComment =
  req
    "CreateComment"
    "fixture/CreateComment.yaml"

requestCreateCustomMetadata :: CreateCustomMetadata -> TestTree
requestCreateCustomMetadata =
  req
    "CreateCustomMetadata"
    "fixture/CreateCustomMetadata.yaml"

requestCreateFolder :: CreateFolder -> TestTree
requestCreateFolder =
  req
    "CreateFolder"
    "fixture/CreateFolder.yaml"

requestCreateLabels :: CreateLabels -> TestTree
requestCreateLabels =
  req
    "CreateLabels"
    "fixture/CreateLabels.yaml"

requestCreateNotificationSubscription :: CreateNotificationSubscription -> TestTree
requestCreateNotificationSubscription =
  req
    "CreateNotificationSubscription"
    "fixture/CreateNotificationSubscription.yaml"

requestCreateUser :: CreateUser -> TestTree
requestCreateUser =
  req
    "CreateUser"
    "fixture/CreateUser.yaml"

requestDeactivateUser :: DeactivateUser -> TestTree
requestDeactivateUser =
  req
    "DeactivateUser"
    "fixture/DeactivateUser.yaml"

requestDeleteComment :: DeleteComment -> TestTree
requestDeleteComment =
  req
    "DeleteComment"
    "fixture/DeleteComment.yaml"

requestDeleteCustomMetadata :: DeleteCustomMetadata -> TestTree
requestDeleteCustomMetadata =
  req
    "DeleteCustomMetadata"
    "fixture/DeleteCustomMetadata.yaml"

requestDeleteDocument :: DeleteDocument -> TestTree
requestDeleteDocument =
  req
    "DeleteDocument"
    "fixture/DeleteDocument.yaml"

requestDeleteDocumentVersion :: DeleteDocumentVersion -> TestTree
requestDeleteDocumentVersion =
  req
    "DeleteDocumentVersion"
    "fixture/DeleteDocumentVersion.yaml"

requestDeleteFolder :: DeleteFolder -> TestTree
requestDeleteFolder =
  req
    "DeleteFolder"
    "fixture/DeleteFolder.yaml"

requestDeleteFolderContents :: DeleteFolderContents -> TestTree
requestDeleteFolderContents =
  req
    "DeleteFolderContents"
    "fixture/DeleteFolderContents.yaml"

requestDeleteLabels :: DeleteLabels -> TestTree
requestDeleteLabels =
  req
    "DeleteLabels"
    "fixture/DeleteLabels.yaml"

requestDeleteNotificationSubscription :: DeleteNotificationSubscription -> TestTree
requestDeleteNotificationSubscription =
  req
    "DeleteNotificationSubscription"
    "fixture/DeleteNotificationSubscription.yaml"

requestDeleteUser :: DeleteUser -> TestTree
requestDeleteUser =
  req
    "DeleteUser"
    "fixture/DeleteUser.yaml"

requestDescribeActivities :: DescribeActivities -> TestTree
requestDescribeActivities =
  req
    "DescribeActivities"
    "fixture/DescribeActivities.yaml"

requestDescribeComments :: DescribeComments -> TestTree
requestDescribeComments =
  req
    "DescribeComments"
    "fixture/DescribeComments.yaml"

requestDescribeDocumentVersions :: DescribeDocumentVersions -> TestTree
requestDescribeDocumentVersions =
  req
    "DescribeDocumentVersions"
    "fixture/DescribeDocumentVersions.yaml"

requestDescribeFolderContents :: DescribeFolderContents -> TestTree
requestDescribeFolderContents =
  req
    "DescribeFolderContents"
    "fixture/DescribeFolderContents.yaml"

requestDescribeGroups :: DescribeGroups -> TestTree
requestDescribeGroups =
  req
    "DescribeGroups"
    "fixture/DescribeGroups.yaml"

requestDescribeNotificationSubscriptions :: DescribeNotificationSubscriptions -> TestTree
requestDescribeNotificationSubscriptions =
  req
    "DescribeNotificationSubscriptions"
    "fixture/DescribeNotificationSubscriptions.yaml"

requestDescribeResourcePermissions :: DescribeResourcePermissions -> TestTree
requestDescribeResourcePermissions =
  req
    "DescribeResourcePermissions"
    "fixture/DescribeResourcePermissions.yaml"

requestDescribeRootFolders :: DescribeRootFolders -> TestTree
requestDescribeRootFolders =
  req
    "DescribeRootFolders"
    "fixture/DescribeRootFolders.yaml"

requestDescribeUsers :: DescribeUsers -> TestTree
requestDescribeUsers =
  req
    "DescribeUsers"
    "fixture/DescribeUsers.yaml"

requestGetCurrentUser :: GetCurrentUser -> TestTree
requestGetCurrentUser =
  req
    "GetCurrentUser"
    "fixture/GetCurrentUser.yaml"

requestGetDocument :: GetDocument -> TestTree
requestGetDocument =
  req
    "GetDocument"
    "fixture/GetDocument.yaml"

requestGetDocumentPath :: GetDocumentPath -> TestTree
requestGetDocumentPath =
  req
    "GetDocumentPath"
    "fixture/GetDocumentPath.yaml"

requestGetDocumentVersion :: GetDocumentVersion -> TestTree
requestGetDocumentVersion =
  req
    "GetDocumentVersion"
    "fixture/GetDocumentVersion.yaml"

requestGetFolder :: GetFolder -> TestTree
requestGetFolder =
  req
    "GetFolder"
    "fixture/GetFolder.yaml"

requestGetFolderPath :: GetFolderPath -> TestTree
requestGetFolderPath =
  req
    "GetFolderPath"
    "fixture/GetFolderPath.yaml"

requestGetResources :: GetResources -> TestTree
requestGetResources =
  req
    "GetResources"
    "fixture/GetResources.yaml"

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

requestRemoveResourcePermission :: RemoveResourcePermission -> TestTree
requestRemoveResourcePermission =
  req
    "RemoveResourcePermission"
    "fixture/RemoveResourcePermission.yaml"

requestRestoreDocumentVersions :: RestoreDocumentVersions -> TestTree
requestRestoreDocumentVersions =
  req
    "RestoreDocumentVersions"
    "fixture/RestoreDocumentVersions.yaml"

requestUpdateDocument :: UpdateDocument -> TestTree
requestUpdateDocument =
  req
    "UpdateDocument"
    "fixture/UpdateDocument.yaml"

requestUpdateDocumentVersion :: UpdateDocumentVersion -> TestTree
requestUpdateDocumentVersion =
  req
    "UpdateDocumentVersion"
    "fixture/UpdateDocumentVersion.yaml"

requestUpdateFolder :: UpdateFolder -> TestTree
requestUpdateFolder =
  req
    "UpdateFolder"
    "fixture/UpdateFolder.yaml"

requestUpdateUser :: UpdateUser -> TestTree
requestUpdateUser =
  req
    "UpdateUser"
    "fixture/UpdateUser.yaml"

-- Responses

responseAbortDocumentVersionUpload :: AbortDocumentVersionUploadResponse -> TestTree
responseAbortDocumentVersionUpload =
  res
    "AbortDocumentVersionUploadResponse"
    "fixture/AbortDocumentVersionUploadResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AbortDocumentVersionUpload)

responseActivateUser :: ActivateUserResponse -> TestTree
responseActivateUser =
  res
    "ActivateUserResponse"
    "fixture/ActivateUserResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ActivateUser)

responseAddResourcePermissions :: AddResourcePermissionsResponse -> TestTree
responseAddResourcePermissions =
  res
    "AddResourcePermissionsResponse"
    "fixture/AddResourcePermissionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AddResourcePermissions)

responseCreateComment :: CreateCommentResponse -> TestTree
responseCreateComment =
  res
    "CreateCommentResponse"
    "fixture/CreateCommentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateComment)

responseCreateCustomMetadata :: CreateCustomMetadataResponse -> TestTree
responseCreateCustomMetadata =
  res
    "CreateCustomMetadataResponse"
    "fixture/CreateCustomMetadataResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateCustomMetadata)

responseCreateFolder :: CreateFolderResponse -> TestTree
responseCreateFolder =
  res
    "CreateFolderResponse"
    "fixture/CreateFolderResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateFolder)

responseCreateLabels :: CreateLabelsResponse -> TestTree
responseCreateLabels =
  res
    "CreateLabelsResponse"
    "fixture/CreateLabelsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateLabels)

responseCreateNotificationSubscription :: CreateNotificationSubscriptionResponse -> TestTree
responseCreateNotificationSubscription =
  res
    "CreateNotificationSubscriptionResponse"
    "fixture/CreateNotificationSubscriptionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateNotificationSubscription)

responseCreateUser :: CreateUserResponse -> TestTree
responseCreateUser =
  res
    "CreateUserResponse"
    "fixture/CreateUserResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateUser)

responseDeactivateUser :: DeactivateUserResponse -> TestTree
responseDeactivateUser =
  res
    "DeactivateUserResponse"
    "fixture/DeactivateUserResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeactivateUser)

responseDeleteComment :: DeleteCommentResponse -> TestTree
responseDeleteComment =
  res
    "DeleteCommentResponse"
    "fixture/DeleteCommentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteComment)

responseDeleteCustomMetadata :: DeleteCustomMetadataResponse -> TestTree
responseDeleteCustomMetadata =
  res
    "DeleteCustomMetadataResponse"
    "fixture/DeleteCustomMetadataResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteCustomMetadata)

responseDeleteDocument :: DeleteDocumentResponse -> TestTree
responseDeleteDocument =
  res
    "DeleteDocumentResponse"
    "fixture/DeleteDocumentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteDocument)

responseDeleteDocumentVersion :: DeleteDocumentVersionResponse -> TestTree
responseDeleteDocumentVersion =
  res
    "DeleteDocumentVersionResponse"
    "fixture/DeleteDocumentVersionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteDocumentVersion)

responseDeleteFolder :: DeleteFolderResponse -> TestTree
responseDeleteFolder =
  res
    "DeleteFolderResponse"
    "fixture/DeleteFolderResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteFolder)

responseDeleteFolderContents :: DeleteFolderContentsResponse -> TestTree
responseDeleteFolderContents =
  res
    "DeleteFolderContentsResponse"
    "fixture/DeleteFolderContentsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteFolderContents)

responseDeleteLabels :: DeleteLabelsResponse -> TestTree
responseDeleteLabels =
  res
    "DeleteLabelsResponse"
    "fixture/DeleteLabelsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteLabels)

responseDeleteNotificationSubscription :: DeleteNotificationSubscriptionResponse -> TestTree
responseDeleteNotificationSubscription =
  res
    "DeleteNotificationSubscriptionResponse"
    "fixture/DeleteNotificationSubscriptionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteNotificationSubscription)

responseDeleteUser :: DeleteUserResponse -> TestTree
responseDeleteUser =
  res
    "DeleteUserResponse"
    "fixture/DeleteUserResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteUser)

responseDescribeActivities :: DescribeActivitiesResponse -> TestTree
responseDescribeActivities =
  res
    "DescribeActivitiesResponse"
    "fixture/DescribeActivitiesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeActivities)

responseDescribeComments :: DescribeCommentsResponse -> TestTree
responseDescribeComments =
  res
    "DescribeCommentsResponse"
    "fixture/DescribeCommentsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeComments)

responseDescribeDocumentVersions :: DescribeDocumentVersionsResponse -> TestTree
responseDescribeDocumentVersions =
  res
    "DescribeDocumentVersionsResponse"
    "fixture/DescribeDocumentVersionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeDocumentVersions)

responseDescribeFolderContents :: DescribeFolderContentsResponse -> TestTree
responseDescribeFolderContents =
  res
    "DescribeFolderContentsResponse"
    "fixture/DescribeFolderContentsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeFolderContents)

responseDescribeGroups :: DescribeGroupsResponse -> TestTree
responseDescribeGroups =
  res
    "DescribeGroupsResponse"
    "fixture/DescribeGroupsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeGroups)

responseDescribeNotificationSubscriptions :: DescribeNotificationSubscriptionsResponse -> TestTree
responseDescribeNotificationSubscriptions =
  res
    "DescribeNotificationSubscriptionsResponse"
    "fixture/DescribeNotificationSubscriptionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeNotificationSubscriptions)

responseDescribeResourcePermissions :: DescribeResourcePermissionsResponse -> TestTree
responseDescribeResourcePermissions =
  res
    "DescribeResourcePermissionsResponse"
    "fixture/DescribeResourcePermissionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeResourcePermissions)

responseDescribeRootFolders :: DescribeRootFoldersResponse -> TestTree
responseDescribeRootFolders =
  res
    "DescribeRootFoldersResponse"
    "fixture/DescribeRootFoldersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeRootFolders)

responseDescribeUsers :: DescribeUsersResponse -> TestTree
responseDescribeUsers =
  res
    "DescribeUsersResponse"
    "fixture/DescribeUsersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeUsers)

responseGetCurrentUser :: GetCurrentUserResponse -> TestTree
responseGetCurrentUser =
  res
    "GetCurrentUserResponse"
    "fixture/GetCurrentUserResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetCurrentUser)

responseGetDocument :: GetDocumentResponse -> TestTree
responseGetDocument =
  res
    "GetDocumentResponse"
    "fixture/GetDocumentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetDocument)

responseGetDocumentPath :: GetDocumentPathResponse -> TestTree
responseGetDocumentPath =
  res
    "GetDocumentPathResponse"
    "fixture/GetDocumentPathResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetDocumentPath)

responseGetDocumentVersion :: GetDocumentVersionResponse -> TestTree
responseGetDocumentVersion =
  res
    "GetDocumentVersionResponse"
    "fixture/GetDocumentVersionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetDocumentVersion)

responseGetFolder :: GetFolderResponse -> TestTree
responseGetFolder =
  res
    "GetFolderResponse"
    "fixture/GetFolderResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetFolder)

responseGetFolderPath :: GetFolderPathResponse -> TestTree
responseGetFolderPath =
  res
    "GetFolderPathResponse"
    "fixture/GetFolderPathResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetFolderPath)

responseGetResources :: GetResourcesResponse -> TestTree
responseGetResources =
  res
    "GetResourcesResponse"
    "fixture/GetResourcesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetResources)

responseInitiateDocumentVersionUpload :: InitiateDocumentVersionUploadResponse -> TestTree
responseInitiateDocumentVersionUpload =
  res
    "InitiateDocumentVersionUploadResponse"
    "fixture/InitiateDocumentVersionUploadResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy InitiateDocumentVersionUpload)

responseRemoveAllResourcePermissions :: RemoveAllResourcePermissionsResponse -> TestTree
responseRemoveAllResourcePermissions =
  res
    "RemoveAllResourcePermissionsResponse"
    "fixture/RemoveAllResourcePermissionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RemoveAllResourcePermissions)

responseRemoveResourcePermission :: RemoveResourcePermissionResponse -> TestTree
responseRemoveResourcePermission =
  res
    "RemoveResourcePermissionResponse"
    "fixture/RemoveResourcePermissionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RemoveResourcePermission)

responseRestoreDocumentVersions :: RestoreDocumentVersionsResponse -> TestTree
responseRestoreDocumentVersions =
  res
    "RestoreDocumentVersionsResponse"
    "fixture/RestoreDocumentVersionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RestoreDocumentVersions)

responseUpdateDocument :: UpdateDocumentResponse -> TestTree
responseUpdateDocument =
  res
    "UpdateDocumentResponse"
    "fixture/UpdateDocumentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateDocument)

responseUpdateDocumentVersion :: UpdateDocumentVersionResponse -> TestTree
responseUpdateDocumentVersion =
  res
    "UpdateDocumentVersionResponse"
    "fixture/UpdateDocumentVersionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateDocumentVersion)

responseUpdateFolder :: UpdateFolderResponse -> TestTree
responseUpdateFolder =
  res
    "UpdateFolderResponse"
    "fixture/UpdateFolderResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateFolder)

responseUpdateUser :: UpdateUserResponse -> TestTree
responseUpdateUser =
  res
    "UpdateUserResponse"
    "fixture/UpdateUserResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateUser)
