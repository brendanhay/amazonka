{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.WorkDocs
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
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
--         [ requestDeleteLabels $
--             deleteLabels
--
--         , requestAbortDocumentVersionUpload $
--             abortDocumentVersionUpload
--
--         , requestGetDocumentPath $
--             getDocumentPath
--
--         , requestCreateComment $
--             createComment
--
--         , requestDescribeUsers $
--             describeUsers
--
--         , requestDeleteFolder $
--             deleteFolder
--
--         , requestUpdateFolder $
--             updateFolder
--
--         , requestDeleteCustomMetadata $
--             deleteCustomMetadata
--
--         , requestDescribeResourcePermissions $
--             describeResourcePermissions
--
--         , requestDeleteNotificationSubscription $
--             deleteNotificationSubscription
--
--         , requestCreateFolder $
--             createFolder
--
--         , requestCreateNotificationSubscription $
--             createNotificationSubscription
--
--         , requestCreateCustomMetadata $
--             createCustomMetadata
--
--         , requestGetFolderPath $
--             getFolderPath
--
--         , requestDescribeComments $
--             describeComments
--
--         , requestDeleteFolderContents $
--             deleteFolderContents
--
--         , requestRemoveAllResourcePermissions $
--             removeAllResourcePermissions
--
--         , requestGetFolder $
--             getFolder
--
--         , requestDescribeNotificationSubscriptions $
--             describeNotificationSubscriptions
--
--         , requestActivateUser $
--             activateUser
--
--         , requestDescribeDocumentVersions $
--             describeDocumentVersions
--
--         , requestGetDocumentVersion $
--             getDocumentVersion
--
--         , requestDescribeActivities $
--             describeActivities
--
--         , requestDescribeRootFolders $
--             describeRootFolders
--
--         , requestGetCurrentUser $
--             getCurrentUser
--
--         , requestDeactivateUser $
--             deactivateUser
--
--         , requestGetDocument $
--             getDocument
--
--         , requestDescribeFolderContents $
--             describeFolderContents
--
--         , requestCreateLabels $
--             createLabels
--
--         , requestUpdateDocumentVersion $
--             updateDocumentVersion
--
--         , requestRemoveResourcePermission $
--             removeResourcePermission
--
--         , requestDeleteComment $
--             deleteComment
--
--         , requestInitiateDocumentVersionUpload $
--             initiateDocumentVersionUpload
--
--         , requestCreateUser $
--             createUser
--
--         , requestUpdateUser $
--             updateUser
--
--         , requestDeleteUser $
--             deleteUser
--
--         , requestAddResourcePermissions $
--             addResourcePermissions
--
--         , requestUpdateDocument $
--             updateDocument
--
--         , requestDeleteDocument $
--             deleteDocument
--
--         , requestDescribeGroups $
--             describeGroups
--
--           ]

--     , testGroup "response"
--         [ responseDeleteLabels $
--             deleteLabelsResponse
--
--         , responseAbortDocumentVersionUpload $
--             abortDocumentVersionUploadResponse
--
--         , responseGetDocumentPath $
--             getDocumentPathResponse
--
--         , responseCreateComment $
--             createCommentResponse
--
--         , responseDescribeUsers $
--             describeUsersResponse
--
--         , responseDeleteFolder $
--             deleteFolderResponse
--
--         , responseUpdateFolder $
--             updateFolderResponse
--
--         , responseDeleteCustomMetadata $
--             deleteCustomMetadataResponse
--
--         , responseDescribeResourcePermissions $
--             describeResourcePermissionsResponse
--
--         , responseDeleteNotificationSubscription $
--             deleteNotificationSubscriptionResponse
--
--         , responseCreateFolder $
--             createFolderResponse
--
--         , responseCreateNotificationSubscription $
--             createNotificationSubscriptionResponse
--
--         , responseCreateCustomMetadata $
--             createCustomMetadataResponse
--
--         , responseGetFolderPath $
--             getFolderPathResponse
--
--         , responseDescribeComments $
--             describeCommentsResponse
--
--         , responseDeleteFolderContents $
--             deleteFolderContentsResponse
--
--         , responseRemoveAllResourcePermissions $
--             removeAllResourcePermissionsResponse
--
--         , responseGetFolder $
--             getFolderResponse
--
--         , responseDescribeNotificationSubscriptions $
--             describeNotificationSubscriptionsResponse
--
--         , responseActivateUser $
--             activateUserResponse
--
--         , responseDescribeDocumentVersions $
--             describeDocumentVersionsResponse
--
--         , responseGetDocumentVersion $
--             getDocumentVersionResponse
--
--         , responseDescribeActivities $
--             describeActivitiesResponse
--
--         , responseDescribeRootFolders $
--             describeRootFoldersResponse
--
--         , responseGetCurrentUser $
--             getCurrentUserResponse
--
--         , responseDeactivateUser $
--             deactivateUserResponse
--
--         , responseGetDocument $
--             getDocumentResponse
--
--         , responseDescribeFolderContents $
--             describeFolderContentsResponse
--
--         , responseCreateLabels $
--             createLabelsResponse
--
--         , responseUpdateDocumentVersion $
--             updateDocumentVersionResponse
--
--         , responseRemoveResourcePermission $
--             removeResourcePermissionResponse
--
--         , responseDeleteComment $
--             deleteCommentResponse
--
--         , responseInitiateDocumentVersionUpload $
--             initiateDocumentVersionUploadResponse
--
--         , responseCreateUser $
--             createUserResponse
--
--         , responseUpdateUser $
--             updateUserResponse
--
--         , responseDeleteUser $
--             deleteUserResponse
--
--         , responseAddResourcePermissions $
--             addResourcePermissionsResponse
--
--         , responseUpdateDocument $
--             updateDocumentResponse
--
--         , responseDeleteDocument $
--             deleteDocumentResponse
--
--         , responseDescribeGroups $
--             describeGroupsResponse
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
    workDocs
    (Proxy :: Proxy DeleteLabels)

responseAbortDocumentVersionUpload :: AbortDocumentVersionUploadResponse -> TestTree
responseAbortDocumentVersionUpload = res
    "AbortDocumentVersionUploadResponse"
    "fixture/AbortDocumentVersionUploadResponse.proto"
    workDocs
    (Proxy :: Proxy AbortDocumentVersionUpload)

responseGetDocumentPath :: GetDocumentPathResponse -> TestTree
responseGetDocumentPath = res
    "GetDocumentPathResponse"
    "fixture/GetDocumentPathResponse.proto"
    workDocs
    (Proxy :: Proxy GetDocumentPath)

responseCreateComment :: CreateCommentResponse -> TestTree
responseCreateComment = res
    "CreateCommentResponse"
    "fixture/CreateCommentResponse.proto"
    workDocs
    (Proxy :: Proxy CreateComment)

responseDescribeUsers :: DescribeUsersResponse -> TestTree
responseDescribeUsers = res
    "DescribeUsersResponse"
    "fixture/DescribeUsersResponse.proto"
    workDocs
    (Proxy :: Proxy DescribeUsers)

responseDeleteFolder :: DeleteFolderResponse -> TestTree
responseDeleteFolder = res
    "DeleteFolderResponse"
    "fixture/DeleteFolderResponse.proto"
    workDocs
    (Proxy :: Proxy DeleteFolder)

responseUpdateFolder :: UpdateFolderResponse -> TestTree
responseUpdateFolder = res
    "UpdateFolderResponse"
    "fixture/UpdateFolderResponse.proto"
    workDocs
    (Proxy :: Proxy UpdateFolder)

responseDeleteCustomMetadata :: DeleteCustomMetadataResponse -> TestTree
responseDeleteCustomMetadata = res
    "DeleteCustomMetadataResponse"
    "fixture/DeleteCustomMetadataResponse.proto"
    workDocs
    (Proxy :: Proxy DeleteCustomMetadata)

responseDescribeResourcePermissions :: DescribeResourcePermissionsResponse -> TestTree
responseDescribeResourcePermissions = res
    "DescribeResourcePermissionsResponse"
    "fixture/DescribeResourcePermissionsResponse.proto"
    workDocs
    (Proxy :: Proxy DescribeResourcePermissions)

responseDeleteNotificationSubscription :: DeleteNotificationSubscriptionResponse -> TestTree
responseDeleteNotificationSubscription = res
    "DeleteNotificationSubscriptionResponse"
    "fixture/DeleteNotificationSubscriptionResponse.proto"
    workDocs
    (Proxy :: Proxy DeleteNotificationSubscription)

responseCreateFolder :: CreateFolderResponse -> TestTree
responseCreateFolder = res
    "CreateFolderResponse"
    "fixture/CreateFolderResponse.proto"
    workDocs
    (Proxy :: Proxy CreateFolder)

responseCreateNotificationSubscription :: CreateNotificationSubscriptionResponse -> TestTree
responseCreateNotificationSubscription = res
    "CreateNotificationSubscriptionResponse"
    "fixture/CreateNotificationSubscriptionResponse.proto"
    workDocs
    (Proxy :: Proxy CreateNotificationSubscription)

responseCreateCustomMetadata :: CreateCustomMetadataResponse -> TestTree
responseCreateCustomMetadata = res
    "CreateCustomMetadataResponse"
    "fixture/CreateCustomMetadataResponse.proto"
    workDocs
    (Proxy :: Proxy CreateCustomMetadata)

responseGetFolderPath :: GetFolderPathResponse -> TestTree
responseGetFolderPath = res
    "GetFolderPathResponse"
    "fixture/GetFolderPathResponse.proto"
    workDocs
    (Proxy :: Proxy GetFolderPath)

responseDescribeComments :: DescribeCommentsResponse -> TestTree
responseDescribeComments = res
    "DescribeCommentsResponse"
    "fixture/DescribeCommentsResponse.proto"
    workDocs
    (Proxy :: Proxy DescribeComments)

responseDeleteFolderContents :: DeleteFolderContentsResponse -> TestTree
responseDeleteFolderContents = res
    "DeleteFolderContentsResponse"
    "fixture/DeleteFolderContentsResponse.proto"
    workDocs
    (Proxy :: Proxy DeleteFolderContents)

responseRemoveAllResourcePermissions :: RemoveAllResourcePermissionsResponse -> TestTree
responseRemoveAllResourcePermissions = res
    "RemoveAllResourcePermissionsResponse"
    "fixture/RemoveAllResourcePermissionsResponse.proto"
    workDocs
    (Proxy :: Proxy RemoveAllResourcePermissions)

responseGetFolder :: GetFolderResponse -> TestTree
responseGetFolder = res
    "GetFolderResponse"
    "fixture/GetFolderResponse.proto"
    workDocs
    (Proxy :: Proxy GetFolder)

responseDescribeNotificationSubscriptions :: DescribeNotificationSubscriptionsResponse -> TestTree
responseDescribeNotificationSubscriptions = res
    "DescribeNotificationSubscriptionsResponse"
    "fixture/DescribeNotificationSubscriptionsResponse.proto"
    workDocs
    (Proxy :: Proxy DescribeNotificationSubscriptions)

responseActivateUser :: ActivateUserResponse -> TestTree
responseActivateUser = res
    "ActivateUserResponse"
    "fixture/ActivateUserResponse.proto"
    workDocs
    (Proxy :: Proxy ActivateUser)

responseDescribeDocumentVersions :: DescribeDocumentVersionsResponse -> TestTree
responseDescribeDocumentVersions = res
    "DescribeDocumentVersionsResponse"
    "fixture/DescribeDocumentVersionsResponse.proto"
    workDocs
    (Proxy :: Proxy DescribeDocumentVersions)

responseGetDocumentVersion :: GetDocumentVersionResponse -> TestTree
responseGetDocumentVersion = res
    "GetDocumentVersionResponse"
    "fixture/GetDocumentVersionResponse.proto"
    workDocs
    (Proxy :: Proxy GetDocumentVersion)

responseDescribeActivities :: DescribeActivitiesResponse -> TestTree
responseDescribeActivities = res
    "DescribeActivitiesResponse"
    "fixture/DescribeActivitiesResponse.proto"
    workDocs
    (Proxy :: Proxy DescribeActivities)

responseDescribeRootFolders :: DescribeRootFoldersResponse -> TestTree
responseDescribeRootFolders = res
    "DescribeRootFoldersResponse"
    "fixture/DescribeRootFoldersResponse.proto"
    workDocs
    (Proxy :: Proxy DescribeRootFolders)

responseGetCurrentUser :: GetCurrentUserResponse -> TestTree
responseGetCurrentUser = res
    "GetCurrentUserResponse"
    "fixture/GetCurrentUserResponse.proto"
    workDocs
    (Proxy :: Proxy GetCurrentUser)

responseDeactivateUser :: DeactivateUserResponse -> TestTree
responseDeactivateUser = res
    "DeactivateUserResponse"
    "fixture/DeactivateUserResponse.proto"
    workDocs
    (Proxy :: Proxy DeactivateUser)

responseGetDocument :: GetDocumentResponse -> TestTree
responseGetDocument = res
    "GetDocumentResponse"
    "fixture/GetDocumentResponse.proto"
    workDocs
    (Proxy :: Proxy GetDocument)

responseDescribeFolderContents :: DescribeFolderContentsResponse -> TestTree
responseDescribeFolderContents = res
    "DescribeFolderContentsResponse"
    "fixture/DescribeFolderContentsResponse.proto"
    workDocs
    (Proxy :: Proxy DescribeFolderContents)

responseCreateLabels :: CreateLabelsResponse -> TestTree
responseCreateLabels = res
    "CreateLabelsResponse"
    "fixture/CreateLabelsResponse.proto"
    workDocs
    (Proxy :: Proxy CreateLabels)

responseUpdateDocumentVersion :: UpdateDocumentVersionResponse -> TestTree
responseUpdateDocumentVersion = res
    "UpdateDocumentVersionResponse"
    "fixture/UpdateDocumentVersionResponse.proto"
    workDocs
    (Proxy :: Proxy UpdateDocumentVersion)

responseRemoveResourcePermission :: RemoveResourcePermissionResponse -> TestTree
responseRemoveResourcePermission = res
    "RemoveResourcePermissionResponse"
    "fixture/RemoveResourcePermissionResponse.proto"
    workDocs
    (Proxy :: Proxy RemoveResourcePermission)

responseDeleteComment :: DeleteCommentResponse -> TestTree
responseDeleteComment = res
    "DeleteCommentResponse"
    "fixture/DeleteCommentResponse.proto"
    workDocs
    (Proxy :: Proxy DeleteComment)

responseInitiateDocumentVersionUpload :: InitiateDocumentVersionUploadResponse -> TestTree
responseInitiateDocumentVersionUpload = res
    "InitiateDocumentVersionUploadResponse"
    "fixture/InitiateDocumentVersionUploadResponse.proto"
    workDocs
    (Proxy :: Proxy InitiateDocumentVersionUpload)

responseCreateUser :: CreateUserResponse -> TestTree
responseCreateUser = res
    "CreateUserResponse"
    "fixture/CreateUserResponse.proto"
    workDocs
    (Proxy :: Proxy CreateUser)

responseUpdateUser :: UpdateUserResponse -> TestTree
responseUpdateUser = res
    "UpdateUserResponse"
    "fixture/UpdateUserResponse.proto"
    workDocs
    (Proxy :: Proxy UpdateUser)

responseDeleteUser :: DeleteUserResponse -> TestTree
responseDeleteUser = res
    "DeleteUserResponse"
    "fixture/DeleteUserResponse.proto"
    workDocs
    (Proxy :: Proxy DeleteUser)

responseAddResourcePermissions :: AddResourcePermissionsResponse -> TestTree
responseAddResourcePermissions = res
    "AddResourcePermissionsResponse"
    "fixture/AddResourcePermissionsResponse.proto"
    workDocs
    (Proxy :: Proxy AddResourcePermissions)

responseUpdateDocument :: UpdateDocumentResponse -> TestTree
responseUpdateDocument = res
    "UpdateDocumentResponse"
    "fixture/UpdateDocumentResponse.proto"
    workDocs
    (Proxy :: Proxy UpdateDocument)

responseDeleteDocument :: DeleteDocumentResponse -> TestTree
responseDeleteDocument = res
    "DeleteDocumentResponse"
    "fixture/DeleteDocumentResponse.proto"
    workDocs
    (Proxy :: Proxy DeleteDocument)

responseDescribeGroups :: DescribeGroupsResponse -> TestTree
responseDescribeGroups = res
    "DescribeGroupsResponse"
    "fixture/DescribeGroupsResponse.proto"
    workDocs
    (Proxy :: Proxy DescribeGroups)
