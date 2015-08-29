{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.Glacier
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Test.AWS.Gen.Glacier where

import Data.Proxy
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.Tasty
import Network.AWS.Glacier
import Test.AWS.Glacier.Internal

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ testSetVaultAccessPolicy $
--             setVaultAccessPolicy
--
--         , testInitiateJob $
--             initiateJob
--
--         , testSetDataRetrievalPolicy $
--             setDataRetrievalPolicy
--
--         , testDeleteVaultAccessPolicy $
--             deleteVaultAccessPolicy
--
--         , testListTagsForVault $
--             listTagsForVault
--
--         , testGetVaultNotifications $
--             getVaultNotifications
--
--         , testUploadMultipartPart $
--             uploadMultipartPart
--
--         , testDeleteVaultNotifications $
--             deleteVaultNotifications
--
--         , testCompleteVaultLock $
--             completeVaultLock
--
--         , testAbortVaultLock $
--             abortVaultLock
--
--         , testListVaults $
--             listVaults
--
--         , testGetJobOutput $
--             getJobOutput
--
--         , testListJobs $
--             listJobs
--
--         , testSetVaultNotifications $
--             setVaultNotifications
--
--         , testCompleteMultipartUpload $
--             completeMultipartUpload
--
--         , testListMultipartUploads $
--             listMultipartUploads
--
--         , testDescribeVault $
--             describeVault
--
--         , testAbortMultipartUpload $
--             abortMultipartUpload
--
--         , testGetVaultLock $
--             getVaultLock
--
--         , testDescribeJob $
--             describeJob
--
--         , testInitiateVaultLock $
--             initiateVaultLock
--
--         , testGetVaultAccessPolicy $
--             getVaultAccessPolicy
--
--         , testGetDataRetrievalPolicy $
--             getDataRetrievalPolicy
--
--         , testRemoveTagsFromVault $
--             removeTagsFromVault
--
--         , testDeleteArchive $
--             deleteArchive
--
--         , testDeleteVault $
--             deleteVault
--
--         , testInitiateMultipartUpload $
--             initiateMultipartUpload
--
--         , testListParts $
--             listParts
--
--         , testCreateVault $
--             createVault
--
--         , testAddTagsToVault $
--             addTagsToVault
--
--         , testUploadArchive $
--             uploadArchive
--
--           ]

--     , testGroup "response"
--         [ testSetVaultAccessPolicyResponse $
--             setVaultAccessPolicyResponse
--
--         , testInitiateJobResponse $
--             initiateJobResponse
--
--         , testSetDataRetrievalPolicyResponse $
--             setDataRetrievalPolicyResponse
--
--         , testDeleteVaultAccessPolicyResponse $
--             deleteVaultAccessPolicyResponse
--
--         , testListTagsForVaultResponse $
--             listTagsForVaultResponse
--
--         , testGetVaultNotificationsResponse $
--             getVaultNotificationsResponse
--
--         , testUploadMultipartPartResponse $
--             uploadMultipartPartResponse
--
--         , testDeleteVaultNotificationsResponse $
--             deleteVaultNotificationsResponse
--
--         , testCompleteVaultLockResponse $
--             completeVaultLockResponse
--
--         , testAbortVaultLockResponse $
--             abortVaultLockResponse
--
--         , testListVaultsResponse $
--             listVaultsResponse
--
--         , testGetJobOutputResponse $
--             getJobOutputResponse
--
--         , testListJobsResponse $
--             listJobsResponse
--
--         , testSetVaultNotificationsResponse $
--             setVaultNotificationsResponse
--
--         , testCompleteMultipartUploadResponse $
--             archiveCreationOutput
--
--         , testListMultipartUploadsResponse $
--             listMultipartUploadsResponse
--
--         , testDescribeVaultResponse $
--             describeVaultOutput
--
--         , testAbortMultipartUploadResponse $
--             abortMultipartUploadResponse
--
--         , testGetVaultLockResponse $
--             getVaultLockResponse
--
--         , testDescribeJobResponse $
--             glacierJobDescription
--
--         , testInitiateVaultLockResponse $
--             initiateVaultLockResponse
--
--         , testGetVaultAccessPolicyResponse $
--             getVaultAccessPolicyResponse
--
--         , testGetDataRetrievalPolicyResponse $
--             getDataRetrievalPolicyResponse
--
--         , testRemoveTagsFromVaultResponse $
--             removeTagsFromVaultResponse
--
--         , testDeleteArchiveResponse $
--             deleteArchiveResponse
--
--         , testDeleteVaultResponse $
--             deleteVaultResponse
--
--         , testInitiateMultipartUploadResponse $
--             initiateMultipartUploadResponse
--
--         , testListPartsResponse $
--             listPartsResponse
--
--         , testCreateVaultResponse $
--             createVaultResponse
--
--         , testAddTagsToVaultResponse $
--             addTagsToVaultResponse
--
--         , testUploadArchiveResponse $
--             archiveCreationOutput
--
--           ]
--     ]

-- Requests

testSetVaultAccessPolicy :: SetVaultAccessPolicy -> TestTree
testSetVaultAccessPolicy = req
    "SetVaultAccessPolicy"
    "fixture/SetVaultAccessPolicy.yaml"

testInitiateJob :: InitiateJob -> TestTree
testInitiateJob = req
    "InitiateJob"
    "fixture/InitiateJob.yaml"

testSetDataRetrievalPolicy :: SetDataRetrievalPolicy -> TestTree
testSetDataRetrievalPolicy = req
    "SetDataRetrievalPolicy"
    "fixture/SetDataRetrievalPolicy.yaml"

testDeleteVaultAccessPolicy :: DeleteVaultAccessPolicy -> TestTree
testDeleteVaultAccessPolicy = req
    "DeleteVaultAccessPolicy"
    "fixture/DeleteVaultAccessPolicy.yaml"

testListTagsForVault :: ListTagsForVault -> TestTree
testListTagsForVault = req
    "ListTagsForVault"
    "fixture/ListTagsForVault.yaml"

testGetVaultNotifications :: GetVaultNotifications -> TestTree
testGetVaultNotifications = req
    "GetVaultNotifications"
    "fixture/GetVaultNotifications.yaml"

testDeleteVaultNotifications :: DeleteVaultNotifications -> TestTree
testDeleteVaultNotifications = req
    "DeleteVaultNotifications"
    "fixture/DeleteVaultNotifications.yaml"

testCompleteVaultLock :: CompleteVaultLock -> TestTree
testCompleteVaultLock = req
    "CompleteVaultLock"
    "fixture/CompleteVaultLock.yaml"

testAbortVaultLock :: AbortVaultLock -> TestTree
testAbortVaultLock = req
    "AbortVaultLock"
    "fixture/AbortVaultLock.yaml"

testListVaults :: ListVaults -> TestTree
testListVaults = req
    "ListVaults"
    "fixture/ListVaults.yaml"

testGetJobOutput :: GetJobOutput -> TestTree
testGetJobOutput = req
    "GetJobOutput"
    "fixture/GetJobOutput.yaml"

testListJobs :: ListJobs -> TestTree
testListJobs = req
    "ListJobs"
    "fixture/ListJobs.yaml"

testSetVaultNotifications :: SetVaultNotifications -> TestTree
testSetVaultNotifications = req
    "SetVaultNotifications"
    "fixture/SetVaultNotifications.yaml"

testCompleteMultipartUpload :: CompleteMultipartUpload -> TestTree
testCompleteMultipartUpload = req
    "CompleteMultipartUpload"
    "fixture/CompleteMultipartUpload.yaml"

testListMultipartUploads :: ListMultipartUploads -> TestTree
testListMultipartUploads = req
    "ListMultipartUploads"
    "fixture/ListMultipartUploads.yaml"

testDescribeVault :: DescribeVault -> TestTree
testDescribeVault = req
    "DescribeVault"
    "fixture/DescribeVault.yaml"

testAbortMultipartUpload :: AbortMultipartUpload -> TestTree
testAbortMultipartUpload = req
    "AbortMultipartUpload"
    "fixture/AbortMultipartUpload.yaml"

testGetVaultLock :: GetVaultLock -> TestTree
testGetVaultLock = req
    "GetVaultLock"
    "fixture/GetVaultLock.yaml"

testDescribeJob :: DescribeJob -> TestTree
testDescribeJob = req
    "DescribeJob"
    "fixture/DescribeJob.yaml"

testInitiateVaultLock :: InitiateVaultLock -> TestTree
testInitiateVaultLock = req
    "InitiateVaultLock"
    "fixture/InitiateVaultLock.yaml"

testGetVaultAccessPolicy :: GetVaultAccessPolicy -> TestTree
testGetVaultAccessPolicy = req
    "GetVaultAccessPolicy"
    "fixture/GetVaultAccessPolicy.yaml"

testGetDataRetrievalPolicy :: GetDataRetrievalPolicy -> TestTree
testGetDataRetrievalPolicy = req
    "GetDataRetrievalPolicy"
    "fixture/GetDataRetrievalPolicy.yaml"

testRemoveTagsFromVault :: RemoveTagsFromVault -> TestTree
testRemoveTagsFromVault = req
    "RemoveTagsFromVault"
    "fixture/RemoveTagsFromVault.yaml"

testDeleteArchive :: DeleteArchive -> TestTree
testDeleteArchive = req
    "DeleteArchive"
    "fixture/DeleteArchive.yaml"

testDeleteVault :: DeleteVault -> TestTree
testDeleteVault = req
    "DeleteVault"
    "fixture/DeleteVault.yaml"

testInitiateMultipartUpload :: InitiateMultipartUpload -> TestTree
testInitiateMultipartUpload = req
    "InitiateMultipartUpload"
    "fixture/InitiateMultipartUpload.yaml"

testListParts :: ListParts -> TestTree
testListParts = req
    "ListParts"
    "fixture/ListParts.yaml"

testCreateVault :: CreateVault -> TestTree
testCreateVault = req
    "CreateVault"
    "fixture/CreateVault.yaml"

testAddTagsToVault :: AddTagsToVault -> TestTree
testAddTagsToVault = req
    "AddTagsToVault"
    "fixture/AddTagsToVault.yaml"

-- Responses

testSetVaultAccessPolicyResponse :: SetVaultAccessPolicyResponse -> TestTree
testSetVaultAccessPolicyResponse = res
    "SetVaultAccessPolicyResponse"
    "fixture/SetVaultAccessPolicyResponse.proto"
    glacier
    (Proxy :: Proxy SetVaultAccessPolicy)

testInitiateJobResponse :: InitiateJobResponse -> TestTree
testInitiateJobResponse = res
    "InitiateJobResponse"
    "fixture/InitiateJobResponse.proto"
    glacier
    (Proxy :: Proxy InitiateJob)

testSetDataRetrievalPolicyResponse :: SetDataRetrievalPolicyResponse -> TestTree
testSetDataRetrievalPolicyResponse = res
    "SetDataRetrievalPolicyResponse"
    "fixture/SetDataRetrievalPolicyResponse.proto"
    glacier
    (Proxy :: Proxy SetDataRetrievalPolicy)

testDeleteVaultAccessPolicyResponse :: DeleteVaultAccessPolicyResponse -> TestTree
testDeleteVaultAccessPolicyResponse = res
    "DeleteVaultAccessPolicyResponse"
    "fixture/DeleteVaultAccessPolicyResponse.proto"
    glacier
    (Proxy :: Proxy DeleteVaultAccessPolicy)

testListTagsForVaultResponse :: ListTagsForVaultResponse -> TestTree
testListTagsForVaultResponse = res
    "ListTagsForVaultResponse"
    "fixture/ListTagsForVaultResponse.proto"
    glacier
    (Proxy :: Proxy ListTagsForVault)

testGetVaultNotificationsResponse :: GetVaultNotificationsResponse -> TestTree
testGetVaultNotificationsResponse = res
    "GetVaultNotificationsResponse"
    "fixture/GetVaultNotificationsResponse.proto"
    glacier
    (Proxy :: Proxy GetVaultNotifications)

testUploadMultipartPartResponse :: UploadMultipartPartResponse -> TestTree
testUploadMultipartPartResponse = res
    "UploadMultipartPartResponse"
    "fixture/UploadMultipartPartResponse.proto"
    glacier
    (Proxy :: Proxy UploadMultipartPart)

testDeleteVaultNotificationsResponse :: DeleteVaultNotificationsResponse -> TestTree
testDeleteVaultNotificationsResponse = res
    "DeleteVaultNotificationsResponse"
    "fixture/DeleteVaultNotificationsResponse.proto"
    glacier
    (Proxy :: Proxy DeleteVaultNotifications)

testCompleteVaultLockResponse :: CompleteVaultLockResponse -> TestTree
testCompleteVaultLockResponse = res
    "CompleteVaultLockResponse"
    "fixture/CompleteVaultLockResponse.proto"
    glacier
    (Proxy :: Proxy CompleteVaultLock)

testAbortVaultLockResponse :: AbortVaultLockResponse -> TestTree
testAbortVaultLockResponse = res
    "AbortVaultLockResponse"
    "fixture/AbortVaultLockResponse.proto"
    glacier
    (Proxy :: Proxy AbortVaultLock)

testListVaultsResponse :: ListVaultsResponse -> TestTree
testListVaultsResponse = res
    "ListVaultsResponse"
    "fixture/ListVaultsResponse.proto"
    glacier
    (Proxy :: Proxy ListVaults)

testListJobsResponse :: ListJobsResponse -> TestTree
testListJobsResponse = res
    "ListJobsResponse"
    "fixture/ListJobsResponse.proto"
    glacier
    (Proxy :: Proxy ListJobs)

testSetVaultNotificationsResponse :: SetVaultNotificationsResponse -> TestTree
testSetVaultNotificationsResponse = res
    "SetVaultNotificationsResponse"
    "fixture/SetVaultNotificationsResponse.proto"
    glacier
    (Proxy :: Proxy SetVaultNotifications)

testCompleteMultipartUploadResponse :: ArchiveCreationOutput -> TestTree
testCompleteMultipartUploadResponse = res
    "CompleteMultipartUploadResponse"
    "fixture/CompleteMultipartUploadResponse.proto"
    glacier
    (Proxy :: Proxy CompleteMultipartUpload)

testListMultipartUploadsResponse :: ListMultipartUploadsResponse -> TestTree
testListMultipartUploadsResponse = res
    "ListMultipartUploadsResponse"
    "fixture/ListMultipartUploadsResponse.proto"
    glacier
    (Proxy :: Proxy ListMultipartUploads)

testDescribeVaultResponse :: DescribeVaultOutput -> TestTree
testDescribeVaultResponse = res
    "DescribeVaultResponse"
    "fixture/DescribeVaultResponse.proto"
    glacier
    (Proxy :: Proxy DescribeVault)

testAbortMultipartUploadResponse :: AbortMultipartUploadResponse -> TestTree
testAbortMultipartUploadResponse = res
    "AbortMultipartUploadResponse"
    "fixture/AbortMultipartUploadResponse.proto"
    glacier
    (Proxy :: Proxy AbortMultipartUpload)

testGetVaultLockResponse :: GetVaultLockResponse -> TestTree
testGetVaultLockResponse = res
    "GetVaultLockResponse"
    "fixture/GetVaultLockResponse.proto"
    glacier
    (Proxy :: Proxy GetVaultLock)

testDescribeJobResponse :: GlacierJobDescription -> TestTree
testDescribeJobResponse = res
    "DescribeJobResponse"
    "fixture/DescribeJobResponse.proto"
    glacier
    (Proxy :: Proxy DescribeJob)

testInitiateVaultLockResponse :: InitiateVaultLockResponse -> TestTree
testInitiateVaultLockResponse = res
    "InitiateVaultLockResponse"
    "fixture/InitiateVaultLockResponse.proto"
    glacier
    (Proxy :: Proxy InitiateVaultLock)

testGetVaultAccessPolicyResponse :: GetVaultAccessPolicyResponse -> TestTree
testGetVaultAccessPolicyResponse = res
    "GetVaultAccessPolicyResponse"
    "fixture/GetVaultAccessPolicyResponse.proto"
    glacier
    (Proxy :: Proxy GetVaultAccessPolicy)

testGetDataRetrievalPolicyResponse :: GetDataRetrievalPolicyResponse -> TestTree
testGetDataRetrievalPolicyResponse = res
    "GetDataRetrievalPolicyResponse"
    "fixture/GetDataRetrievalPolicyResponse.proto"
    glacier
    (Proxy :: Proxy GetDataRetrievalPolicy)

testRemoveTagsFromVaultResponse :: RemoveTagsFromVaultResponse -> TestTree
testRemoveTagsFromVaultResponse = res
    "RemoveTagsFromVaultResponse"
    "fixture/RemoveTagsFromVaultResponse.proto"
    glacier
    (Proxy :: Proxy RemoveTagsFromVault)

testDeleteArchiveResponse :: DeleteArchiveResponse -> TestTree
testDeleteArchiveResponse = res
    "DeleteArchiveResponse"
    "fixture/DeleteArchiveResponse.proto"
    glacier
    (Proxy :: Proxy DeleteArchive)

testDeleteVaultResponse :: DeleteVaultResponse -> TestTree
testDeleteVaultResponse = res
    "DeleteVaultResponse"
    "fixture/DeleteVaultResponse.proto"
    glacier
    (Proxy :: Proxy DeleteVault)

testInitiateMultipartUploadResponse :: InitiateMultipartUploadResponse -> TestTree
testInitiateMultipartUploadResponse = res
    "InitiateMultipartUploadResponse"
    "fixture/InitiateMultipartUploadResponse.proto"
    glacier
    (Proxy :: Proxy InitiateMultipartUpload)

testListPartsResponse :: ListPartsResponse -> TestTree
testListPartsResponse = res
    "ListPartsResponse"
    "fixture/ListPartsResponse.proto"
    glacier
    (Proxy :: Proxy ListParts)

testCreateVaultResponse :: CreateVaultResponse -> TestTree
testCreateVaultResponse = res
    "CreateVaultResponse"
    "fixture/CreateVaultResponse.proto"
    glacier
    (Proxy :: Proxy CreateVault)

testAddTagsToVaultResponse :: AddTagsToVaultResponse -> TestTree
testAddTagsToVaultResponse = res
    "AddTagsToVaultResponse"
    "fixture/AddTagsToVaultResponse.proto"
    glacier
    (Proxy :: Proxy AddTagsToVault)

testUploadArchiveResponse :: ArchiveCreationOutput -> TestTree
testUploadArchiveResponse = res
    "UploadArchiveResponse"
    "fixture/UploadArchiveResponse.proto"
    glacier
    (Proxy :: Proxy UploadArchive)
