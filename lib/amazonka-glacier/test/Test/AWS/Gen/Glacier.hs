{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.Glacier
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Test.AWS.Gen.Glacier where

import Data.Proxy
import Network.AWS.Glacier
import Test.AWS.Fixture
import Test.AWS.Glacier.Internal
import Test.AWS.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestSetVaultAccessPolicy $
--             setVaultAccessPolicy
--
--         , requestInitiateJob $
--             initiateJob
--
--         , requestSetDataRetrievalPolicy $
--             setDataRetrievalPolicy
--
--         , requestDeleteVaultAccessPolicy $
--             deleteVaultAccessPolicy
--
--         , requestGetVaultNotifications $
--             getVaultNotifications
--
--         , requestListTagsForVault $
--             listTagsForVault
--
--         , requestUploadMultipartPart $
--             uploadMultipartPart
--
--         , requestDeleteVaultNotifications $
--             deleteVaultNotifications
--
--         , requestCompleteVaultLock $
--             completeVaultLock
--
--         , requestAbortVaultLock $
--             abortVaultLock
--
--         , requestListVaults $
--             listVaults
--
--         , requestListProvisionedCapacity $
--             listProvisionedCapacity
--
--         , requestListJobs $
--             listJobs
--
--         , requestSetVaultNotifications $
--             setVaultNotifications
--
--         , requestGetJobOutput $
--             getJobOutput
--
--         , requestCompleteMultipartUpload $
--             completeMultipartUpload
--
--         , requestListMultipartUploads $
--             listMultipartUploads
--
--         , requestAbortMultipartUpload $
--             abortMultipartUpload
--
--         , requestPurchaseProvisionedCapacity $
--             purchaseProvisionedCapacity
--
--         , requestDescribeVault $
--             describeVault
--
--         , requestGetVaultLock $
--             getVaultLock
--
--         , requestDescribeJob $
--             describeJob
--
--         , requestInitiateVaultLock $
--             initiateVaultLock
--
--         , requestGetVaultAccessPolicy $
--             getVaultAccessPolicy
--
--         , requestGetDataRetrievalPolicy $
--             getDataRetrievalPolicy
--
--         , requestRemoveTagsFromVault $
--             removeTagsFromVault
--
--         , requestDeleteVault $
--             deleteVault
--
--         , requestDeleteArchive $
--             deleteArchive
--
--         , requestCreateVault $
--             createVault
--
--         , requestInitiateMultipartUpload $
--             initiateMultipartUpload
--
--         , requestListParts $
--             listParts
--
--         , requestAddTagsToVault $
--             addTagsToVault
--
--         , requestUploadArchive $
--             uploadArchive
--
--           ]

--     , testGroup "response"
--         [ responseSetVaultAccessPolicy $
--             setVaultAccessPolicyResponse
--
--         , responseInitiateJob $
--             initiateJobResponse
--
--         , responseSetDataRetrievalPolicy $
--             setDataRetrievalPolicyResponse
--
--         , responseDeleteVaultAccessPolicy $
--             deleteVaultAccessPolicyResponse
--
--         , responseGetVaultNotifications $
--             getVaultNotificationsResponse
--
--         , responseListTagsForVault $
--             listTagsForVaultResponse
--
--         , responseUploadMultipartPart $
--             uploadMultipartPartResponse
--
--         , responseDeleteVaultNotifications $
--             deleteVaultNotificationsResponse
--
--         , responseCompleteVaultLock $
--             completeVaultLockResponse
--
--         , responseAbortVaultLock $
--             abortVaultLockResponse
--
--         , responseListVaults $
--             listVaultsResponse
--
--         , responseListProvisionedCapacity $
--             listProvisionedCapacityResponse
--
--         , responseListJobs $
--             listJobsResponse
--
--         , responseSetVaultNotifications $
--             setVaultNotificationsResponse
--
--         , responseGetJobOutput $
--             getJobOutputResponse
--
--         , responseCompleteMultipartUpload $
--             archiveCreationOutput
--
--         , responseListMultipartUploads $
--             listMultipartUploadsResponse
--
--         , responseAbortMultipartUpload $
--             abortMultipartUploadResponse
--
--         , responsePurchaseProvisionedCapacity $
--             purchaseProvisionedCapacityResponse
--
--         , responseDescribeVault $
--             describeVaultOutput
--
--         , responseGetVaultLock $
--             getVaultLockResponse
--
--         , responseDescribeJob $
--             glacierJobDescription
--
--         , responseInitiateVaultLock $
--             initiateVaultLockResponse
--
--         , responseGetVaultAccessPolicy $
--             getVaultAccessPolicyResponse
--
--         , responseGetDataRetrievalPolicy $
--             getDataRetrievalPolicyResponse
--
--         , responseRemoveTagsFromVault $
--             removeTagsFromVaultResponse
--
--         , responseDeleteVault $
--             deleteVaultResponse
--
--         , responseDeleteArchive $
--             deleteArchiveResponse
--
--         , responseCreateVault $
--             createVaultResponse
--
--         , responseInitiateMultipartUpload $
--             initiateMultipartUploadResponse
--
--         , responseListParts $
--             listPartsResponse
--
--         , responseAddTagsToVault $
--             addTagsToVaultResponse
--
--         , responseUploadArchive $
--             archiveCreationOutput
--
--           ]
--     ]

-- Requests

requestSetVaultAccessPolicy :: SetVaultAccessPolicy -> TestTree
requestSetVaultAccessPolicy = req
    "SetVaultAccessPolicy"
    "fixture/SetVaultAccessPolicy.yaml"

requestInitiateJob :: InitiateJob -> TestTree
requestInitiateJob = req
    "InitiateJob"
    "fixture/InitiateJob.yaml"

requestSetDataRetrievalPolicy :: SetDataRetrievalPolicy -> TestTree
requestSetDataRetrievalPolicy = req
    "SetDataRetrievalPolicy"
    "fixture/SetDataRetrievalPolicy.yaml"

requestDeleteVaultAccessPolicy :: DeleteVaultAccessPolicy -> TestTree
requestDeleteVaultAccessPolicy = req
    "DeleteVaultAccessPolicy"
    "fixture/DeleteVaultAccessPolicy.yaml"

requestGetVaultNotifications :: GetVaultNotifications -> TestTree
requestGetVaultNotifications = req
    "GetVaultNotifications"
    "fixture/GetVaultNotifications.yaml"

requestListTagsForVault :: ListTagsForVault -> TestTree
requestListTagsForVault = req
    "ListTagsForVault"
    "fixture/ListTagsForVault.yaml"

requestDeleteVaultNotifications :: DeleteVaultNotifications -> TestTree
requestDeleteVaultNotifications = req
    "DeleteVaultNotifications"
    "fixture/DeleteVaultNotifications.yaml"

requestCompleteVaultLock :: CompleteVaultLock -> TestTree
requestCompleteVaultLock = req
    "CompleteVaultLock"
    "fixture/CompleteVaultLock.yaml"

requestAbortVaultLock :: AbortVaultLock -> TestTree
requestAbortVaultLock = req
    "AbortVaultLock"
    "fixture/AbortVaultLock.yaml"

requestListVaults :: ListVaults -> TestTree
requestListVaults = req
    "ListVaults"
    "fixture/ListVaults.yaml"

requestListProvisionedCapacity :: ListProvisionedCapacity -> TestTree
requestListProvisionedCapacity = req
    "ListProvisionedCapacity"
    "fixture/ListProvisionedCapacity.yaml"

requestListJobs :: ListJobs -> TestTree
requestListJobs = req
    "ListJobs"
    "fixture/ListJobs.yaml"

requestSetVaultNotifications :: SetVaultNotifications -> TestTree
requestSetVaultNotifications = req
    "SetVaultNotifications"
    "fixture/SetVaultNotifications.yaml"

requestGetJobOutput :: GetJobOutput -> TestTree
requestGetJobOutput = req
    "GetJobOutput"
    "fixture/GetJobOutput.yaml"

requestCompleteMultipartUpload :: CompleteMultipartUpload -> TestTree
requestCompleteMultipartUpload = req
    "CompleteMultipartUpload"
    "fixture/CompleteMultipartUpload.yaml"

requestListMultipartUploads :: ListMultipartUploads -> TestTree
requestListMultipartUploads = req
    "ListMultipartUploads"
    "fixture/ListMultipartUploads.yaml"

requestAbortMultipartUpload :: AbortMultipartUpload -> TestTree
requestAbortMultipartUpload = req
    "AbortMultipartUpload"
    "fixture/AbortMultipartUpload.yaml"

requestPurchaseProvisionedCapacity :: PurchaseProvisionedCapacity -> TestTree
requestPurchaseProvisionedCapacity = req
    "PurchaseProvisionedCapacity"
    "fixture/PurchaseProvisionedCapacity.yaml"

requestDescribeVault :: DescribeVault -> TestTree
requestDescribeVault = req
    "DescribeVault"
    "fixture/DescribeVault.yaml"

requestGetVaultLock :: GetVaultLock -> TestTree
requestGetVaultLock = req
    "GetVaultLock"
    "fixture/GetVaultLock.yaml"

requestDescribeJob :: DescribeJob -> TestTree
requestDescribeJob = req
    "DescribeJob"
    "fixture/DescribeJob.yaml"

requestInitiateVaultLock :: InitiateVaultLock -> TestTree
requestInitiateVaultLock = req
    "InitiateVaultLock"
    "fixture/InitiateVaultLock.yaml"

requestGetVaultAccessPolicy :: GetVaultAccessPolicy -> TestTree
requestGetVaultAccessPolicy = req
    "GetVaultAccessPolicy"
    "fixture/GetVaultAccessPolicy.yaml"

requestGetDataRetrievalPolicy :: GetDataRetrievalPolicy -> TestTree
requestGetDataRetrievalPolicy = req
    "GetDataRetrievalPolicy"
    "fixture/GetDataRetrievalPolicy.yaml"

requestRemoveTagsFromVault :: RemoveTagsFromVault -> TestTree
requestRemoveTagsFromVault = req
    "RemoveTagsFromVault"
    "fixture/RemoveTagsFromVault.yaml"

requestDeleteVault :: DeleteVault -> TestTree
requestDeleteVault = req
    "DeleteVault"
    "fixture/DeleteVault.yaml"

requestDeleteArchive :: DeleteArchive -> TestTree
requestDeleteArchive = req
    "DeleteArchive"
    "fixture/DeleteArchive.yaml"

requestCreateVault :: CreateVault -> TestTree
requestCreateVault = req
    "CreateVault"
    "fixture/CreateVault.yaml"

requestInitiateMultipartUpload :: InitiateMultipartUpload -> TestTree
requestInitiateMultipartUpload = req
    "InitiateMultipartUpload"
    "fixture/InitiateMultipartUpload.yaml"

requestListParts :: ListParts -> TestTree
requestListParts = req
    "ListParts"
    "fixture/ListParts.yaml"

requestAddTagsToVault :: AddTagsToVault -> TestTree
requestAddTagsToVault = req
    "AddTagsToVault"
    "fixture/AddTagsToVault.yaml"

-- Responses

responseSetVaultAccessPolicy :: SetVaultAccessPolicyResponse -> TestTree
responseSetVaultAccessPolicy = res
    "SetVaultAccessPolicyResponse"
    "fixture/SetVaultAccessPolicyResponse.proto"
    glacier
    (Proxy :: Proxy SetVaultAccessPolicy)

responseInitiateJob :: InitiateJobResponse -> TestTree
responseInitiateJob = res
    "InitiateJobResponse"
    "fixture/InitiateJobResponse.proto"
    glacier
    (Proxy :: Proxy InitiateJob)

responseSetDataRetrievalPolicy :: SetDataRetrievalPolicyResponse -> TestTree
responseSetDataRetrievalPolicy = res
    "SetDataRetrievalPolicyResponse"
    "fixture/SetDataRetrievalPolicyResponse.proto"
    glacier
    (Proxy :: Proxy SetDataRetrievalPolicy)

responseDeleteVaultAccessPolicy :: DeleteVaultAccessPolicyResponse -> TestTree
responseDeleteVaultAccessPolicy = res
    "DeleteVaultAccessPolicyResponse"
    "fixture/DeleteVaultAccessPolicyResponse.proto"
    glacier
    (Proxy :: Proxy DeleteVaultAccessPolicy)

responseGetVaultNotifications :: GetVaultNotificationsResponse -> TestTree
responseGetVaultNotifications = res
    "GetVaultNotificationsResponse"
    "fixture/GetVaultNotificationsResponse.proto"
    glacier
    (Proxy :: Proxy GetVaultNotifications)

responseListTagsForVault :: ListTagsForVaultResponse -> TestTree
responseListTagsForVault = res
    "ListTagsForVaultResponse"
    "fixture/ListTagsForVaultResponse.proto"
    glacier
    (Proxy :: Proxy ListTagsForVault)

responseUploadMultipartPart :: UploadMultipartPartResponse -> TestTree
responseUploadMultipartPart = res
    "UploadMultipartPartResponse"
    "fixture/UploadMultipartPartResponse.proto"
    glacier
    (Proxy :: Proxy UploadMultipartPart)

responseDeleteVaultNotifications :: DeleteVaultNotificationsResponse -> TestTree
responseDeleteVaultNotifications = res
    "DeleteVaultNotificationsResponse"
    "fixture/DeleteVaultNotificationsResponse.proto"
    glacier
    (Proxy :: Proxy DeleteVaultNotifications)

responseCompleteVaultLock :: CompleteVaultLockResponse -> TestTree
responseCompleteVaultLock = res
    "CompleteVaultLockResponse"
    "fixture/CompleteVaultLockResponse.proto"
    glacier
    (Proxy :: Proxy CompleteVaultLock)

responseAbortVaultLock :: AbortVaultLockResponse -> TestTree
responseAbortVaultLock = res
    "AbortVaultLockResponse"
    "fixture/AbortVaultLockResponse.proto"
    glacier
    (Proxy :: Proxy AbortVaultLock)

responseListVaults :: ListVaultsResponse -> TestTree
responseListVaults = res
    "ListVaultsResponse"
    "fixture/ListVaultsResponse.proto"
    glacier
    (Proxy :: Proxy ListVaults)

responseListProvisionedCapacity :: ListProvisionedCapacityResponse -> TestTree
responseListProvisionedCapacity = res
    "ListProvisionedCapacityResponse"
    "fixture/ListProvisionedCapacityResponse.proto"
    glacier
    (Proxy :: Proxy ListProvisionedCapacity)

responseListJobs :: ListJobsResponse -> TestTree
responseListJobs = res
    "ListJobsResponse"
    "fixture/ListJobsResponse.proto"
    glacier
    (Proxy :: Proxy ListJobs)

responseSetVaultNotifications :: SetVaultNotificationsResponse -> TestTree
responseSetVaultNotifications = res
    "SetVaultNotificationsResponse"
    "fixture/SetVaultNotificationsResponse.proto"
    glacier
    (Proxy :: Proxy SetVaultNotifications)

responseCompleteMultipartUpload :: ArchiveCreationOutput -> TestTree
responseCompleteMultipartUpload = res
    "CompleteMultipartUploadResponse"
    "fixture/CompleteMultipartUploadResponse.proto"
    glacier
    (Proxy :: Proxy CompleteMultipartUpload)

responseListMultipartUploads :: ListMultipartUploadsResponse -> TestTree
responseListMultipartUploads = res
    "ListMultipartUploadsResponse"
    "fixture/ListMultipartUploadsResponse.proto"
    glacier
    (Proxy :: Proxy ListMultipartUploads)

responseAbortMultipartUpload :: AbortMultipartUploadResponse -> TestTree
responseAbortMultipartUpload = res
    "AbortMultipartUploadResponse"
    "fixture/AbortMultipartUploadResponse.proto"
    glacier
    (Proxy :: Proxy AbortMultipartUpload)

responsePurchaseProvisionedCapacity :: PurchaseProvisionedCapacityResponse -> TestTree
responsePurchaseProvisionedCapacity = res
    "PurchaseProvisionedCapacityResponse"
    "fixture/PurchaseProvisionedCapacityResponse.proto"
    glacier
    (Proxy :: Proxy PurchaseProvisionedCapacity)

responseDescribeVault :: DescribeVaultOutput -> TestTree
responseDescribeVault = res
    "DescribeVaultResponse"
    "fixture/DescribeVaultResponse.proto"
    glacier
    (Proxy :: Proxy DescribeVault)

responseGetVaultLock :: GetVaultLockResponse -> TestTree
responseGetVaultLock = res
    "GetVaultLockResponse"
    "fixture/GetVaultLockResponse.proto"
    glacier
    (Proxy :: Proxy GetVaultLock)

responseDescribeJob :: GlacierJobDescription -> TestTree
responseDescribeJob = res
    "DescribeJobResponse"
    "fixture/DescribeJobResponse.proto"
    glacier
    (Proxy :: Proxy DescribeJob)

responseInitiateVaultLock :: InitiateVaultLockResponse -> TestTree
responseInitiateVaultLock = res
    "InitiateVaultLockResponse"
    "fixture/InitiateVaultLockResponse.proto"
    glacier
    (Proxy :: Proxy InitiateVaultLock)

responseGetVaultAccessPolicy :: GetVaultAccessPolicyResponse -> TestTree
responseGetVaultAccessPolicy = res
    "GetVaultAccessPolicyResponse"
    "fixture/GetVaultAccessPolicyResponse.proto"
    glacier
    (Proxy :: Proxy GetVaultAccessPolicy)

responseGetDataRetrievalPolicy :: GetDataRetrievalPolicyResponse -> TestTree
responseGetDataRetrievalPolicy = res
    "GetDataRetrievalPolicyResponse"
    "fixture/GetDataRetrievalPolicyResponse.proto"
    glacier
    (Proxy :: Proxy GetDataRetrievalPolicy)

responseRemoveTagsFromVault :: RemoveTagsFromVaultResponse -> TestTree
responseRemoveTagsFromVault = res
    "RemoveTagsFromVaultResponse"
    "fixture/RemoveTagsFromVaultResponse.proto"
    glacier
    (Proxy :: Proxy RemoveTagsFromVault)

responseDeleteVault :: DeleteVaultResponse -> TestTree
responseDeleteVault = res
    "DeleteVaultResponse"
    "fixture/DeleteVaultResponse.proto"
    glacier
    (Proxy :: Proxy DeleteVault)

responseDeleteArchive :: DeleteArchiveResponse -> TestTree
responseDeleteArchive = res
    "DeleteArchiveResponse"
    "fixture/DeleteArchiveResponse.proto"
    glacier
    (Proxy :: Proxy DeleteArchive)

responseCreateVault :: CreateVaultResponse -> TestTree
responseCreateVault = res
    "CreateVaultResponse"
    "fixture/CreateVaultResponse.proto"
    glacier
    (Proxy :: Proxy CreateVault)

responseInitiateMultipartUpload :: InitiateMultipartUploadResponse -> TestTree
responseInitiateMultipartUpload = res
    "InitiateMultipartUploadResponse"
    "fixture/InitiateMultipartUploadResponse.proto"
    glacier
    (Proxy :: Proxy InitiateMultipartUpload)

responseListParts :: ListPartsResponse -> TestTree
responseListParts = res
    "ListPartsResponse"
    "fixture/ListPartsResponse.proto"
    glacier
    (Proxy :: Proxy ListParts)

responseAddTagsToVault :: AddTagsToVaultResponse -> TestTree
responseAddTagsToVault = res
    "AddTagsToVaultResponse"
    "fixture/AddTagsToVaultResponse.proto"
    glacier
    (Proxy :: Proxy AddTagsToVault)

responseUploadArchive :: ArchiveCreationOutput -> TestTree
responseUploadArchive = res
    "UploadArchiveResponse"
    "fixture/UploadArchiveResponse.proto"
    glacier
    (Proxy :: Proxy UploadArchive)
