{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.WorkDocs
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
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
--         [ requestAbortDocumentVersionUpload $
--             abortDocumentVersionUpload
--
--         , requestGetDocumentPath $
--             getDocumentPath
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
--         , requestGetFolderPath $
--             getFolderPath
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
--         , requestDeactivateUser $
--             deactivateUser
--
--         , requestGetDocument $
--             getDocument
--
--         , requestDescribeFolderContents $
--             describeFolderContents
--
--         , requestUpdateDocumentVersion $
--             updateDocumentVersion
--
--         , requestRemoveResourcePermission $
--             removeResourcePermission
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
--           ]

--     , testGroup "response"
--         [ responseAbortDocumentVersionUpload $
--             abortDocumentVersionUploadResponse
--
--         , responseGetDocumentPath $
--             getDocumentPathResponse
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
--         , responseGetFolderPath $
--             getFolderPathResponse
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
--         , responseDeactivateUser $
--             deactivateUserResponse
--
--         , responseGetDocument $
--             getDocumentResponse
--
--         , responseDescribeFolderContents $
--             describeFolderContentsResponse
--
--         , responseUpdateDocumentVersion $
--             updateDocumentVersionResponse
--
--         , responseRemoveResourcePermission $
--             removeResourcePermissionResponse
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
--           ]
--     ]

-- Requests

requestAbortDocumentVersionUpload :: AbortDocumentVersionUpload -> TestTree
requestAbortDocumentVersionUpload = req
    "AbortDocumentVersionUpload"
    "fixture/AbortDocumentVersionUpload.yaml"

requestGetDocumentPath :: GetDocumentPath -> TestTree
requestGetDocumentPath = req
    "GetDocumentPath"
    "fixture/GetDocumentPath.yaml"

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

requestGetFolderPath :: GetFolderPath -> TestTree
requestGetFolderPath = req
    "GetFolderPath"
    "fixture/GetFolderPath.yaml"

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

requestUpdateDocumentVersion :: UpdateDocumentVersion -> TestTree
requestUpdateDocumentVersion = req
    "UpdateDocumentVersion"
    "fixture/UpdateDocumentVersion.yaml"

requestRemoveResourcePermission :: RemoveResourcePermission -> TestTree
requestRemoveResourcePermission = req
    "RemoveResourcePermission"
    "fixture/RemoveResourcePermission.yaml"

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

-- Responses

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

responseGetFolderPath :: GetFolderPathResponse -> TestTree
responseGetFolderPath = res
    "GetFolderPathResponse"
    "fixture/GetFolderPathResponse.proto"
    workDocs
    (Proxy :: Proxy GetFolderPath)

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
