{-# OPTIONS_GHC -fno-warn-orphans #-}

-- Module      : Test.AWS.Gen.Glacier
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

module Test.AWS.Gen.Glacier where

import Data.Proxy
import Test.AWS.Fixture
import Test.Tasty
import Network.AWS.Glacier

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
--         , testDescribeJob $
--             describeJob
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
--         , testDescribeJobResponse $
--             glacierJobDescription
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
testSetVaultAccessPolicy = undefined

testInitiateJob :: InitiateJob -> TestTree
testInitiateJob = undefined

testSetDataRetrievalPolicy :: SetDataRetrievalPolicy -> TestTree
testSetDataRetrievalPolicy = undefined

testDeleteVaultAccessPolicy :: DeleteVaultAccessPolicy -> TestTree
testDeleteVaultAccessPolicy = undefined

testListTagsForVault :: ListTagsForVault -> TestTree
testListTagsForVault = undefined

testGetVaultNotifications :: GetVaultNotifications -> TestTree
testGetVaultNotifications = undefined

testDeleteVaultNotifications :: DeleteVaultNotifications -> TestTree
testDeleteVaultNotifications = undefined

testListVaults :: ListVaults -> TestTree
testListVaults = undefined

testGetJobOutput :: GetJobOutput -> TestTree
testGetJobOutput = undefined

testListJobs :: ListJobs -> TestTree
testListJobs = undefined

testSetVaultNotifications :: SetVaultNotifications -> TestTree
testSetVaultNotifications = undefined

testCompleteMultipartUpload :: CompleteMultipartUpload -> TestTree
testCompleteMultipartUpload = undefined

testListMultipartUploads :: ListMultipartUploads -> TestTree
testListMultipartUploads = undefined

testDescribeVault :: DescribeVault -> TestTree
testDescribeVault = undefined

testAbortMultipartUpload :: AbortMultipartUpload -> TestTree
testAbortMultipartUpload = undefined

testDescribeJob :: DescribeJob -> TestTree
testDescribeJob = undefined

testGetVaultAccessPolicy :: GetVaultAccessPolicy -> TestTree
testGetVaultAccessPolicy = undefined

testGetDataRetrievalPolicy :: GetDataRetrievalPolicy -> TestTree
testGetDataRetrievalPolicy = undefined

testRemoveTagsFromVault :: RemoveTagsFromVault -> TestTree
testRemoveTagsFromVault = undefined

testDeleteArchive :: DeleteArchive -> TestTree
testDeleteArchive = undefined

testDeleteVault :: DeleteVault -> TestTree
testDeleteVault = undefined

testInitiateMultipartUpload :: InitiateMultipartUpload -> TestTree
testInitiateMultipartUpload = undefined

testListParts :: ListParts -> TestTree
testListParts = undefined

testCreateVault :: CreateVault -> TestTree
testCreateVault = undefined

testAddTagsToVault :: AddTagsToVault -> TestTree
testAddTagsToVault = undefined

-- Responses

testSetVaultAccessPolicyResponse :: SetVaultAccessPolicyResponse -> TestTree
testSetVaultAccessPolicyResponse = resp
    "SetVaultAccessPolicyResponse"
    "fixture/SetVaultAccessPolicyResponse"
    (Proxy :: Proxy SetVaultAccessPolicy)

testInitiateJobResponse :: InitiateJobResponse -> TestTree
testInitiateJobResponse = resp
    "InitiateJobResponse"
    "fixture/InitiateJobResponse"
    (Proxy :: Proxy InitiateJob)

testSetDataRetrievalPolicyResponse :: SetDataRetrievalPolicyResponse -> TestTree
testSetDataRetrievalPolicyResponse = resp
    "SetDataRetrievalPolicyResponse"
    "fixture/SetDataRetrievalPolicyResponse"
    (Proxy :: Proxy SetDataRetrievalPolicy)

testDeleteVaultAccessPolicyResponse :: DeleteVaultAccessPolicyResponse -> TestTree
testDeleteVaultAccessPolicyResponse = resp
    "DeleteVaultAccessPolicyResponse"
    "fixture/DeleteVaultAccessPolicyResponse"
    (Proxy :: Proxy DeleteVaultAccessPolicy)

testListTagsForVaultResponse :: ListTagsForVaultResponse -> TestTree
testListTagsForVaultResponse = resp
    "ListTagsForVaultResponse"
    "fixture/ListTagsForVaultResponse"
    (Proxy :: Proxy ListTagsForVault)

testGetVaultNotificationsResponse :: GetVaultNotificationsResponse -> TestTree
testGetVaultNotificationsResponse = resp
    "GetVaultNotificationsResponse"
    "fixture/GetVaultNotificationsResponse"
    (Proxy :: Proxy GetVaultNotifications)

testUploadMultipartPartResponse :: UploadMultipartPartResponse -> TestTree
testUploadMultipartPartResponse = resp
    "UploadMultipartPartResponse"
    "fixture/UploadMultipartPartResponse"
    (Proxy :: Proxy UploadMultipartPart)

testDeleteVaultNotificationsResponse :: DeleteVaultNotificationsResponse -> TestTree
testDeleteVaultNotificationsResponse = resp
    "DeleteVaultNotificationsResponse"
    "fixture/DeleteVaultNotificationsResponse"
    (Proxy :: Proxy DeleteVaultNotifications)

testListVaultsResponse :: ListVaultsResponse -> TestTree
testListVaultsResponse = resp
    "ListVaultsResponse"
    "fixture/ListVaultsResponse"
    (Proxy :: Proxy ListVaults)

testListJobsResponse :: ListJobsResponse -> TestTree
testListJobsResponse = resp
    "ListJobsResponse"
    "fixture/ListJobsResponse"
    (Proxy :: Proxy ListJobs)

testSetVaultNotificationsResponse :: SetVaultNotificationsResponse -> TestTree
testSetVaultNotificationsResponse = resp
    "SetVaultNotificationsResponse"
    "fixture/SetVaultNotificationsResponse"
    (Proxy :: Proxy SetVaultNotifications)

testCompleteMultipartUploadResponse :: ArchiveCreationOutput -> TestTree
testCompleteMultipartUploadResponse = resp
    "CompleteMultipartUploadResponse"
    "fixture/CompleteMultipartUploadResponse"
    (Proxy :: Proxy CompleteMultipartUpload)

testListMultipartUploadsResponse :: ListMultipartUploadsResponse -> TestTree
testListMultipartUploadsResponse = resp
    "ListMultipartUploadsResponse"
    "fixture/ListMultipartUploadsResponse"
    (Proxy :: Proxy ListMultipartUploads)

testDescribeVaultResponse :: DescribeVaultOutput -> TestTree
testDescribeVaultResponse = resp
    "DescribeVaultResponse"
    "fixture/DescribeVaultResponse"
    (Proxy :: Proxy DescribeVault)

testAbortMultipartUploadResponse :: AbortMultipartUploadResponse -> TestTree
testAbortMultipartUploadResponse = resp
    "AbortMultipartUploadResponse"
    "fixture/AbortMultipartUploadResponse"
    (Proxy :: Proxy AbortMultipartUpload)

testDescribeJobResponse :: GlacierJobDescription -> TestTree
testDescribeJobResponse = resp
    "DescribeJobResponse"
    "fixture/DescribeJobResponse"
    (Proxy :: Proxy DescribeJob)

testGetVaultAccessPolicyResponse :: GetVaultAccessPolicyResponse -> TestTree
testGetVaultAccessPolicyResponse = resp
    "GetVaultAccessPolicyResponse"
    "fixture/GetVaultAccessPolicyResponse"
    (Proxy :: Proxy GetVaultAccessPolicy)

testGetDataRetrievalPolicyResponse :: GetDataRetrievalPolicyResponse -> TestTree
testGetDataRetrievalPolicyResponse = resp
    "GetDataRetrievalPolicyResponse"
    "fixture/GetDataRetrievalPolicyResponse"
    (Proxy :: Proxy GetDataRetrievalPolicy)

testRemoveTagsFromVaultResponse :: RemoveTagsFromVaultResponse -> TestTree
testRemoveTagsFromVaultResponse = resp
    "RemoveTagsFromVaultResponse"
    "fixture/RemoveTagsFromVaultResponse"
    (Proxy :: Proxy RemoveTagsFromVault)

testDeleteArchiveResponse :: DeleteArchiveResponse -> TestTree
testDeleteArchiveResponse = resp
    "DeleteArchiveResponse"
    "fixture/DeleteArchiveResponse"
    (Proxy :: Proxy DeleteArchive)

testDeleteVaultResponse :: DeleteVaultResponse -> TestTree
testDeleteVaultResponse = resp
    "DeleteVaultResponse"
    "fixture/DeleteVaultResponse"
    (Proxy :: Proxy DeleteVault)

testInitiateMultipartUploadResponse :: InitiateMultipartUploadResponse -> TestTree
testInitiateMultipartUploadResponse = resp
    "InitiateMultipartUploadResponse"
    "fixture/InitiateMultipartUploadResponse"
    (Proxy :: Proxy InitiateMultipartUpload)

testListPartsResponse :: ListPartsResponse -> TestTree
testListPartsResponse = resp
    "ListPartsResponse"
    "fixture/ListPartsResponse"
    (Proxy :: Proxy ListParts)

testCreateVaultResponse :: CreateVaultResponse -> TestTree
testCreateVaultResponse = resp
    "CreateVaultResponse"
    "fixture/CreateVaultResponse"
    (Proxy :: Proxy CreateVault)

testAddTagsToVaultResponse :: AddTagsToVaultResponse -> TestTree
testAddTagsToVaultResponse = resp
    "AddTagsToVaultResponse"
    "fixture/AddTagsToVaultResponse"
    (Proxy :: Proxy AddTagsToVault)

testUploadArchiveResponse :: ArchiveCreationOutput -> TestTree
testUploadArchiveResponse = resp
    "UploadArchiveResponse"
    "fixture/UploadArchiveResponse"
    (Proxy :: Proxy UploadArchive)

instance Out AbortMultipartUpload
instance Out AbortMultipartUploadResponse
instance Out ActionCode
instance Out AddTagsToVault
instance Out AddTagsToVaultResponse
instance Out ArchiveCreationOutput
instance Out CompleteMultipartUpload
instance Out CreateVault
instance Out CreateVaultResponse
instance Out DataRetrievalPolicy
instance Out DataRetrievalRule
instance Out DeleteArchive
instance Out DeleteArchiveResponse
instance Out DeleteVault
instance Out DeleteVaultAccessPolicy
instance Out DeleteVaultAccessPolicyResponse
instance Out DeleteVaultNotifications
instance Out DeleteVaultNotificationsResponse
instance Out DeleteVaultResponse
instance Out DescribeJob
instance Out DescribeVault
instance Out DescribeVaultOutput
instance Out GetDataRetrievalPolicy
instance Out GetDataRetrievalPolicyResponse
instance Out GetJobOutput
instance Out GetJobOutputResponse
instance Out GetVaultAccessPolicy
instance Out GetVaultAccessPolicyResponse
instance Out GetVaultNotifications
instance Out GetVaultNotificationsResponse
instance Out GlacierJobDescription
instance Out InitiateJob
instance Out InitiateJobResponse
instance Out InitiateMultipartUpload
instance Out InitiateMultipartUploadResponse
instance Out InventoryRetrievalJobDescription
instance Out InventoryRetrievalJobInput
instance Out JobParameters
instance Out ListJobs
instance Out ListJobsResponse
instance Out ListMultipartUploads
instance Out ListMultipartUploadsResponse
instance Out ListParts
instance Out ListPartsResponse
instance Out ListTagsForVault
instance Out ListTagsForVaultResponse
instance Out ListVaults
instance Out ListVaultsResponse
instance Out PartListElement
instance Out RemoveTagsFromVault
instance Out RemoveTagsFromVaultResponse
instance Out SetDataRetrievalPolicy
instance Out SetDataRetrievalPolicyResponse
instance Out SetVaultAccessPolicy
instance Out SetVaultAccessPolicyResponse
instance Out SetVaultNotifications
instance Out SetVaultNotificationsResponse
instance Out StatusCode
instance Out UploadArchive
instance Out UploadListElement
instance Out UploadMultipartPart
instance Out UploadMultipartPartResponse
instance Out VaultAccessPolicy
instance Out VaultNotificationConfig
