-- Module      : Test.AWS.Gen.Glacier
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
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
-- fixtures = testGroup "SQS"
--     [ testGroup "request"
--         [ abortMultipartUploadTest $
--             abortMultipartUpload
--
--         , addTagsToVaultTest $
--             addTagsToVault
--
--         , completeMultipartUploadTest $
--             completeMultipartUpload
--
--         , createVaultTest $
--             createVault
--
--         , deleteArchiveTest $
--             deleteArchive
--
--         , deleteVaultTest $
--             deleteVault
--
--         , deleteVaultAccessPolicyTest $
--             deleteVaultAccessPolicy
--
--         , deleteVaultNotificationsTest $
--             deleteVaultNotifications
--
--         , describeJobTest $
--             describeJob
--
--         , describeVaultTest $
--             describeVault
--
--         , getDataRetrievalPolicyTest $
--             getDataRetrievalPolicy
--
--         , getJobOutputTest $
--             getJobOutput
--
--         , getVaultAccessPolicyTest $
--             getVaultAccessPolicy
--
--         , getVaultNotificationsTest $
--             getVaultNotifications
--
--         , initiateJobTest $
--             initiateJob
--
--         , initiateMultipartUploadTest $
--             initiateMultipartUpload
--
--         , listJobsTest $
--             listJobs
--
--         , listMultipartUploadsTest $
--             listMultipartUploads
--
--         , listPartsTest $
--             listParts
--
--         , listTagsForVaultTest $
--             listTagsForVault
--
--         , listVaultsTest $
--             listVaults
--
--         , removeTagsFromVaultTest $
--             removeTagsFromVault
--
--         , setDataRetrievalPolicyTest $
--             setDataRetrievalPolicy
--
--         , setVaultAccessPolicyTest $
--             setVaultAccessPolicy
--
--         , setVaultNotificationsTest $
--             setVaultNotifications
--
--         , uploadArchiveTest $
--             uploadArchive
--
--         , uploadMultipartPartTest $
--             uploadMultipartPart
--
--           ]

--     , testGroup "response"
--         [ abortMultipartUploadResponseTest $
--             abortMultipartUploadResponse
--
--         , addTagsToVaultResponseTest $
--             addTagsToVaultResponse
--
--         , completeMultipartUploadResponseTest $
--             archiveCreationOutput
--
--         , createVaultResponseTest $
--             createVaultResponse
--
--         , deleteArchiveResponseTest $
--             deleteArchiveResponse
--
--         , deleteVaultResponseTest $
--             deleteVaultResponse
--
--         , deleteVaultAccessPolicyResponseTest $
--             deleteVaultAccessPolicyResponse
--
--         , deleteVaultNotificationsResponseTest $
--             deleteVaultNotificationsResponse
--
--         , describeJobResponseTest $
--             glacierJobDescription
--
--         , describeVaultResponseTest $
--             describeVaultOutput
--
--         , getDataRetrievalPolicyResponseTest $
--             getDataRetrievalPolicyResponse
--
--         , getJobOutputResponseTest $
--             getJobOutputResponse
--
--         , getVaultAccessPolicyResponseTest $
--             getVaultAccessPolicyResponse
--
--         , getVaultNotificationsResponseTest $
--             getVaultNotificationsResponse
--
--         , initiateJobResponseTest $
--             initiateJobResponse
--
--         , initiateMultipartUploadResponseTest $
--             initiateMultipartUploadResponse
--
--         , listJobsResponseTest $
--             listJobsResponse
--
--         , listMultipartUploadsResponseTest $
--             listMultipartUploadsResponse
--
--         , listPartsResponseTest $
--             listPartsResponse
--
--         , listTagsForVaultResponseTest $
--             listTagsForVaultResponse
--
--         , listVaultsResponseTest $
--             listVaultsResponse
--
--         , removeTagsFromVaultResponseTest $
--             removeTagsFromVaultResponse
--
--         , setDataRetrievalPolicyResponseTest $
--             setDataRetrievalPolicyResponse
--
--         , setVaultAccessPolicyResponseTest $
--             setVaultAccessPolicyResponse
--
--         , setVaultNotificationsResponseTest $
--             setVaultNotificationsResponse
--
--         , uploadArchiveResponseTest $
--             archiveCreationOutput
--
--         , uploadMultipartPartResponseTest $
--             uploadMultipartPartResponse
--
--           ]
--     ]

-- Requests

abortMultipartUploadTest :: AbortMultipartUpload -> TestTree
abortMultipartUploadTest = undefined

addTagsToVaultTest :: AddTagsToVault -> TestTree
addTagsToVaultTest = undefined

completeMultipartUploadTest :: CompleteMultipartUpload -> TestTree
completeMultipartUploadTest = undefined

createVaultTest :: CreateVault -> TestTree
createVaultTest = undefined

deleteArchiveTest :: DeleteArchive -> TestTree
deleteArchiveTest = undefined

deleteVaultTest :: DeleteVault -> TestTree
deleteVaultTest = undefined

deleteVaultAccessPolicyTest :: DeleteVaultAccessPolicy -> TestTree
deleteVaultAccessPolicyTest = undefined

deleteVaultNotificationsTest :: DeleteVaultNotifications -> TestTree
deleteVaultNotificationsTest = undefined

describeJobTest :: DescribeJob -> TestTree
describeJobTest = undefined

describeVaultTest :: DescribeVault -> TestTree
describeVaultTest = undefined

getDataRetrievalPolicyTest :: GetDataRetrievalPolicy -> TestTree
getDataRetrievalPolicyTest = undefined

getJobOutputTest :: GetJobOutput -> TestTree
getJobOutputTest = undefined

getVaultAccessPolicyTest :: GetVaultAccessPolicy -> TestTree
getVaultAccessPolicyTest = undefined

getVaultNotificationsTest :: GetVaultNotifications -> TestTree
getVaultNotificationsTest = undefined

initiateJobTest :: InitiateJob -> TestTree
initiateJobTest = undefined

initiateMultipartUploadTest :: InitiateMultipartUpload -> TestTree
initiateMultipartUploadTest = undefined

listJobsTest :: ListJobs -> TestTree
listJobsTest = undefined

listMultipartUploadsTest :: ListMultipartUploads -> TestTree
listMultipartUploadsTest = undefined

listPartsTest :: ListParts -> TestTree
listPartsTest = undefined

listTagsForVaultTest :: ListTagsForVault -> TestTree
listTagsForVaultTest = undefined

listVaultsTest :: ListVaults -> TestTree
listVaultsTest = undefined

removeTagsFromVaultTest :: RemoveTagsFromVault -> TestTree
removeTagsFromVaultTest = undefined

setDataRetrievalPolicyTest :: SetDataRetrievalPolicy -> TestTree
setDataRetrievalPolicyTest = undefined

setVaultAccessPolicyTest :: SetVaultAccessPolicy -> TestTree
setVaultAccessPolicyTest = undefined

setVaultNotificationsTest :: SetVaultNotifications -> TestTree
setVaultNotificationsTest = undefined

uploadArchiveTest :: UploadArchive -> TestTree
uploadArchiveTest = undefined

uploadMultipartPartTest :: UploadMultipartPart -> TestTree
uploadMultipartPartTest = undefined

-- Responses

abortMultipartUploadResponseTest :: AbortMultipartUploadResponse -> TestTree
abortMultipartUploadResponseTest = resp
    "abortMultipartUploadResponse"
    "fixture/AbortMultipartUploadResponse"
    (Proxy :: Proxy AbortMultipartUpload)

addTagsToVaultResponseTest :: AddTagsToVaultResponse -> TestTree
addTagsToVaultResponseTest = resp
    "addTagsToVaultResponse"
    "fixture/AddTagsToVaultResponse"
    (Proxy :: Proxy AddTagsToVault)

completeMultipartUploadResponseTest :: ArchiveCreationOutput -> TestTree
completeMultipartUploadResponseTest = resp
    "completeMultipartUploadResponse"
    "fixture/ArchiveCreationOutput"
    (Proxy :: Proxy CompleteMultipartUpload)

createVaultResponseTest :: CreateVaultResponse -> TestTree
createVaultResponseTest = resp
    "createVaultResponse"
    "fixture/CreateVaultResponse"
    (Proxy :: Proxy CreateVault)

deleteArchiveResponseTest :: DeleteArchiveResponse -> TestTree
deleteArchiveResponseTest = resp
    "deleteArchiveResponse"
    "fixture/DeleteArchiveResponse"
    (Proxy :: Proxy DeleteArchive)

deleteVaultResponseTest :: DeleteVaultResponse -> TestTree
deleteVaultResponseTest = resp
    "deleteVaultResponse"
    "fixture/DeleteVaultResponse"
    (Proxy :: Proxy DeleteVault)

deleteVaultAccessPolicyResponseTest :: DeleteVaultAccessPolicyResponse -> TestTree
deleteVaultAccessPolicyResponseTest = resp
    "deleteVaultAccessPolicyResponse"
    "fixture/DeleteVaultAccessPolicyResponse"
    (Proxy :: Proxy DeleteVaultAccessPolicy)

deleteVaultNotificationsResponseTest :: DeleteVaultNotificationsResponse -> TestTree
deleteVaultNotificationsResponseTest = resp
    "deleteVaultNotificationsResponse"
    "fixture/DeleteVaultNotificationsResponse"
    (Proxy :: Proxy DeleteVaultNotifications)

describeJobResponseTest :: GlacierJobDescription -> TestTree
describeJobResponseTest = resp
    "describeJobResponse"
    "fixture/GlacierJobDescription"
    (Proxy :: Proxy DescribeJob)

describeVaultResponseTest :: DescribeVaultOutput -> TestTree
describeVaultResponseTest = resp
    "describeVaultResponse"
    "fixture/DescribeVaultOutput"
    (Proxy :: Proxy DescribeVault)

getDataRetrievalPolicyResponseTest :: GetDataRetrievalPolicyResponse -> TestTree
getDataRetrievalPolicyResponseTest = resp
    "getDataRetrievalPolicyResponse"
    "fixture/GetDataRetrievalPolicyResponse"
    (Proxy :: Proxy GetDataRetrievalPolicy)

getJobOutputResponseTest :: GetJobOutputResponse -> TestTree
getJobOutputResponseTest = resp
    "getJobOutputResponse"
    "fixture/GetJobOutputResponse"
    (Proxy :: Proxy GetJobOutput)

getVaultAccessPolicyResponseTest :: GetVaultAccessPolicyResponse -> TestTree
getVaultAccessPolicyResponseTest = resp
    "getVaultAccessPolicyResponse"
    "fixture/GetVaultAccessPolicyResponse"
    (Proxy :: Proxy GetVaultAccessPolicy)

getVaultNotificationsResponseTest :: GetVaultNotificationsResponse -> TestTree
getVaultNotificationsResponseTest = resp
    "getVaultNotificationsResponse"
    "fixture/GetVaultNotificationsResponse"
    (Proxy :: Proxy GetVaultNotifications)

initiateJobResponseTest :: InitiateJobResponse -> TestTree
initiateJobResponseTest = resp
    "initiateJobResponse"
    "fixture/InitiateJobResponse"
    (Proxy :: Proxy InitiateJob)

initiateMultipartUploadResponseTest :: InitiateMultipartUploadResponse -> TestTree
initiateMultipartUploadResponseTest = resp
    "initiateMultipartUploadResponse"
    "fixture/InitiateMultipartUploadResponse"
    (Proxy :: Proxy InitiateMultipartUpload)

listJobsResponseTest :: ListJobsResponse -> TestTree
listJobsResponseTest = resp
    "listJobsResponse"
    "fixture/ListJobsResponse"
    (Proxy :: Proxy ListJobs)

listMultipartUploadsResponseTest :: ListMultipartUploadsResponse -> TestTree
listMultipartUploadsResponseTest = resp
    "listMultipartUploadsResponse"
    "fixture/ListMultipartUploadsResponse"
    (Proxy :: Proxy ListMultipartUploads)

listPartsResponseTest :: ListPartsResponse -> TestTree
listPartsResponseTest = resp
    "listPartsResponse"
    "fixture/ListPartsResponse"
    (Proxy :: Proxy ListParts)

listTagsForVaultResponseTest :: ListTagsForVaultResponse -> TestTree
listTagsForVaultResponseTest = resp
    "listTagsForVaultResponse"
    "fixture/ListTagsForVaultResponse"
    (Proxy :: Proxy ListTagsForVault)

listVaultsResponseTest :: ListVaultsResponse -> TestTree
listVaultsResponseTest = resp
    "listVaultsResponse"
    "fixture/ListVaultsResponse"
    (Proxy :: Proxy ListVaults)

removeTagsFromVaultResponseTest :: RemoveTagsFromVaultResponse -> TestTree
removeTagsFromVaultResponseTest = resp
    "removeTagsFromVaultResponse"
    "fixture/RemoveTagsFromVaultResponse"
    (Proxy :: Proxy RemoveTagsFromVault)

setDataRetrievalPolicyResponseTest :: SetDataRetrievalPolicyResponse -> TestTree
setDataRetrievalPolicyResponseTest = resp
    "setDataRetrievalPolicyResponse"
    "fixture/SetDataRetrievalPolicyResponse"
    (Proxy :: Proxy SetDataRetrievalPolicy)

setVaultAccessPolicyResponseTest :: SetVaultAccessPolicyResponse -> TestTree
setVaultAccessPolicyResponseTest = resp
    "setVaultAccessPolicyResponse"
    "fixture/SetVaultAccessPolicyResponse"
    (Proxy :: Proxy SetVaultAccessPolicy)

setVaultNotificationsResponseTest :: SetVaultNotificationsResponse -> TestTree
setVaultNotificationsResponseTest = resp
    "setVaultNotificationsResponse"
    "fixture/SetVaultNotificationsResponse"
    (Proxy :: Proxy SetVaultNotifications)

uploadArchiveResponseTest :: ArchiveCreationOutput -> TestTree
uploadArchiveResponseTest = resp
    "uploadArchiveResponse"
    "fixture/ArchiveCreationOutput"
    (Proxy :: Proxy UploadArchive)

uploadMultipartPartResponseTest :: UploadMultipartPartResponse -> TestTree
uploadMultipartPartResponseTest = resp
    "uploadMultipartPartResponse"
    "fixture/UploadMultipartPartResponse"
    (Proxy :: Proxy UploadMultipartPart)
