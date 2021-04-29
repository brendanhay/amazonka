{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.Glacier
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
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
--         [ requestPurchaseProvisionedCapacity $
--             newPurchaseProvisionedCapacity
--
--         , requestDescribeVault $
--             newDescribeVault
--
--         , requestSetVaultAccessPolicy $
--             newSetVaultAccessPolicy
--
--         , requestCompleteMultipartUpload $
--             newCompleteMultipartUpload
--
--         , requestSetDataRetrievalPolicy $
--             newSetDataRetrievalPolicy
--
--         , requestUploadArchive $
--             newUploadArchive
--
--         , requestListProvisionedCapacity $
--             newListProvisionedCapacity
--
--         , requestSetVaultNotifications $
--             newSetVaultNotifications
--
--         , requestDeleteVault $
--             newDeleteVault
--
--         , requestAbortVaultLock $
--             newAbortVaultLock
--
--         , requestDeleteArchive $
--             newDeleteArchive
--
--         , requestRemoveTagsFromVault $
--             newRemoveTagsFromVault
--
--         , requestListVaults $
--             newListVaults
--
--         , requestInitiateVaultLock $
--             newInitiateVaultLock
--
--         , requestDescribeJob $
--             newDescribeJob
--
--         , requestListTagsForVault $
--             newListTagsForVault
--
--         , requestGetVaultLock $
--             newGetVaultLock
--
--         , requestAbortMultipartUpload $
--             newAbortMultipartUpload
--
--         , requestDeleteVaultAccessPolicy $
--             newDeleteVaultAccessPolicy
--
--         , requestInitiateJob $
--             newInitiateJob
--
--         , requestListMultipartUploads $
--             newListMultipartUploads
--
--         , requestAddTagsToVault $
--             newAddTagsToVault
--
--         , requestInitiateMultipartUpload $
--             newInitiateMultipartUpload
--
--         , requestCreateVault $
--             newCreateVault
--
--         , requestListJobs $
--             newListJobs
--
--         , requestListParts $
--             newListParts
--
--         , requestGetJobOutput $
--             newGetJobOutput
--
--         , requestCompleteVaultLock $
--             newCompleteVaultLock
--
--         , requestGetVaultAccessPolicy $
--             newGetVaultAccessPolicy
--
--         , requestGetDataRetrievalPolicy $
--             newGetDataRetrievalPolicy
--
--         , requestDeleteVaultNotifications $
--             newDeleteVaultNotifications
--
--         , requestUploadMultipartPart $
--             newUploadMultipartPart
--
--         , requestGetVaultNotifications $
--             newGetVaultNotifications
--
--           ]

--     , testGroup "response"
--         [ responsePurchaseProvisionedCapacity $
--             newPurchaseProvisionedCapacityResponse
--
--         , responseDescribeVault $
--             newDescribeVaultOutput
--
--         , responseSetVaultAccessPolicy $
--             newSetVaultAccessPolicyResponse
--
--         , responseCompleteMultipartUpload $
--             newArchiveCreationOutput
--
--         , responseSetDataRetrievalPolicy $
--             newSetDataRetrievalPolicyResponse
--
--         , responseUploadArchive $
--             newArchiveCreationOutput
--
--         , responseListProvisionedCapacity $
--             newListProvisionedCapacityResponse
--
--         , responseSetVaultNotifications $
--             newSetVaultNotificationsResponse
--
--         , responseDeleteVault $
--             newDeleteVaultResponse
--
--         , responseAbortVaultLock $
--             newAbortVaultLockResponse
--
--         , responseDeleteArchive $
--             newDeleteArchiveResponse
--
--         , responseRemoveTagsFromVault $
--             newRemoveTagsFromVaultResponse
--
--         , responseListVaults $
--             newListVaultsResponse
--
--         , responseInitiateVaultLock $
--             newInitiateVaultLockResponse
--
--         , responseDescribeJob $
--             newGlacierJobDescription
--
--         , responseListTagsForVault $
--             newListTagsForVaultResponse
--
--         , responseGetVaultLock $
--             newGetVaultLockResponse
--
--         , responseAbortMultipartUpload $
--             newAbortMultipartUploadResponse
--
--         , responseDeleteVaultAccessPolicy $
--             newDeleteVaultAccessPolicyResponse
--
--         , responseInitiateJob $
--             newInitiateJobResponse
--
--         , responseListMultipartUploads $
--             newListMultipartUploadsResponse
--
--         , responseAddTagsToVault $
--             newAddTagsToVaultResponse
--
--         , responseInitiateMultipartUpload $
--             newInitiateMultipartUploadResponse
--
--         , responseCreateVault $
--             newCreateVaultResponse
--
--         , responseListJobs $
--             newListJobsResponse
--
--         , responseListParts $
--             newListPartsResponse
--
--         , responseGetJobOutput $
--             newGetJobOutputResponse
--
--         , responseCompleteVaultLock $
--             newCompleteVaultLockResponse
--
--         , responseGetVaultAccessPolicy $
--             newGetVaultAccessPolicyResponse
--
--         , responseGetDataRetrievalPolicy $
--             newGetDataRetrievalPolicyResponse
--
--         , responseDeleteVaultNotifications $
--             newDeleteVaultNotificationsResponse
--
--         , responseUploadMultipartPart $
--             newUploadMultipartPartResponse
--
--         , responseGetVaultNotifications $
--             newGetVaultNotificationsResponse
--
--           ]
--     ]

-- Requests

requestPurchaseProvisionedCapacity :: PurchaseProvisionedCapacity -> TestTree
requestPurchaseProvisionedCapacity =
  req
    "PurchaseProvisionedCapacity"
    "fixture/PurchaseProvisionedCapacity.yaml"

requestDescribeVault :: DescribeVault -> TestTree
requestDescribeVault =
  req
    "DescribeVault"
    "fixture/DescribeVault.yaml"

requestSetVaultAccessPolicy :: SetVaultAccessPolicy -> TestTree
requestSetVaultAccessPolicy =
  req
    "SetVaultAccessPolicy"
    "fixture/SetVaultAccessPolicy.yaml"

requestCompleteMultipartUpload :: CompleteMultipartUpload -> TestTree
requestCompleteMultipartUpload =
  req
    "CompleteMultipartUpload"
    "fixture/CompleteMultipartUpload.yaml"

requestSetDataRetrievalPolicy :: SetDataRetrievalPolicy -> TestTree
requestSetDataRetrievalPolicy =
  req
    "SetDataRetrievalPolicy"
    "fixture/SetDataRetrievalPolicy.yaml"

requestListProvisionedCapacity :: ListProvisionedCapacity -> TestTree
requestListProvisionedCapacity =
  req
    "ListProvisionedCapacity"
    "fixture/ListProvisionedCapacity.yaml"

requestSetVaultNotifications :: SetVaultNotifications -> TestTree
requestSetVaultNotifications =
  req
    "SetVaultNotifications"
    "fixture/SetVaultNotifications.yaml"

requestDeleteVault :: DeleteVault -> TestTree
requestDeleteVault =
  req
    "DeleteVault"
    "fixture/DeleteVault.yaml"

requestAbortVaultLock :: AbortVaultLock -> TestTree
requestAbortVaultLock =
  req
    "AbortVaultLock"
    "fixture/AbortVaultLock.yaml"

requestDeleteArchive :: DeleteArchive -> TestTree
requestDeleteArchive =
  req
    "DeleteArchive"
    "fixture/DeleteArchive.yaml"

requestRemoveTagsFromVault :: RemoveTagsFromVault -> TestTree
requestRemoveTagsFromVault =
  req
    "RemoveTagsFromVault"
    "fixture/RemoveTagsFromVault.yaml"

requestListVaults :: ListVaults -> TestTree
requestListVaults =
  req
    "ListVaults"
    "fixture/ListVaults.yaml"

requestInitiateVaultLock :: InitiateVaultLock -> TestTree
requestInitiateVaultLock =
  req
    "InitiateVaultLock"
    "fixture/InitiateVaultLock.yaml"

requestDescribeJob :: DescribeJob -> TestTree
requestDescribeJob =
  req
    "DescribeJob"
    "fixture/DescribeJob.yaml"

requestListTagsForVault :: ListTagsForVault -> TestTree
requestListTagsForVault =
  req
    "ListTagsForVault"
    "fixture/ListTagsForVault.yaml"

requestGetVaultLock :: GetVaultLock -> TestTree
requestGetVaultLock =
  req
    "GetVaultLock"
    "fixture/GetVaultLock.yaml"

requestAbortMultipartUpload :: AbortMultipartUpload -> TestTree
requestAbortMultipartUpload =
  req
    "AbortMultipartUpload"
    "fixture/AbortMultipartUpload.yaml"

requestDeleteVaultAccessPolicy :: DeleteVaultAccessPolicy -> TestTree
requestDeleteVaultAccessPolicy =
  req
    "DeleteVaultAccessPolicy"
    "fixture/DeleteVaultAccessPolicy.yaml"

requestInitiateJob :: InitiateJob -> TestTree
requestInitiateJob =
  req
    "InitiateJob"
    "fixture/InitiateJob.yaml"

requestListMultipartUploads :: ListMultipartUploads -> TestTree
requestListMultipartUploads =
  req
    "ListMultipartUploads"
    "fixture/ListMultipartUploads.yaml"

requestAddTagsToVault :: AddTagsToVault -> TestTree
requestAddTagsToVault =
  req
    "AddTagsToVault"
    "fixture/AddTagsToVault.yaml"

requestInitiateMultipartUpload :: InitiateMultipartUpload -> TestTree
requestInitiateMultipartUpload =
  req
    "InitiateMultipartUpload"
    "fixture/InitiateMultipartUpload.yaml"

requestCreateVault :: CreateVault -> TestTree
requestCreateVault =
  req
    "CreateVault"
    "fixture/CreateVault.yaml"

requestListJobs :: ListJobs -> TestTree
requestListJobs =
  req
    "ListJobs"
    "fixture/ListJobs.yaml"

requestListParts :: ListParts -> TestTree
requestListParts =
  req
    "ListParts"
    "fixture/ListParts.yaml"

requestGetJobOutput :: GetJobOutput -> TestTree
requestGetJobOutput =
  req
    "GetJobOutput"
    "fixture/GetJobOutput.yaml"

requestCompleteVaultLock :: CompleteVaultLock -> TestTree
requestCompleteVaultLock =
  req
    "CompleteVaultLock"
    "fixture/CompleteVaultLock.yaml"

requestGetVaultAccessPolicy :: GetVaultAccessPolicy -> TestTree
requestGetVaultAccessPolicy =
  req
    "GetVaultAccessPolicy"
    "fixture/GetVaultAccessPolicy.yaml"

requestGetDataRetrievalPolicy :: GetDataRetrievalPolicy -> TestTree
requestGetDataRetrievalPolicy =
  req
    "GetDataRetrievalPolicy"
    "fixture/GetDataRetrievalPolicy.yaml"

requestDeleteVaultNotifications :: DeleteVaultNotifications -> TestTree
requestDeleteVaultNotifications =
  req
    "DeleteVaultNotifications"
    "fixture/DeleteVaultNotifications.yaml"

requestGetVaultNotifications :: GetVaultNotifications -> TestTree
requestGetVaultNotifications =
  req
    "GetVaultNotifications"
    "fixture/GetVaultNotifications.yaml"

-- Responses

responsePurchaseProvisionedCapacity :: PurchaseProvisionedCapacityResponse -> TestTree
responsePurchaseProvisionedCapacity =
  res
    "PurchaseProvisionedCapacityResponse"
    "fixture/PurchaseProvisionedCapacityResponse.proto"
    defaultService
    (Proxy :: Proxy PurchaseProvisionedCapacity)

responseDescribeVault :: DescribeVaultOutput -> TestTree
responseDescribeVault =
  res
    "DescribeVaultResponse"
    "fixture/DescribeVaultResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeVault)

responseSetVaultAccessPolicy :: SetVaultAccessPolicyResponse -> TestTree
responseSetVaultAccessPolicy =
  res
    "SetVaultAccessPolicyResponse"
    "fixture/SetVaultAccessPolicyResponse.proto"
    defaultService
    (Proxy :: Proxy SetVaultAccessPolicy)

responseCompleteMultipartUpload :: ArchiveCreationOutput -> TestTree
responseCompleteMultipartUpload =
  res
    "CompleteMultipartUploadResponse"
    "fixture/CompleteMultipartUploadResponse.proto"
    defaultService
    (Proxy :: Proxy CompleteMultipartUpload)

responseSetDataRetrievalPolicy :: SetDataRetrievalPolicyResponse -> TestTree
responseSetDataRetrievalPolicy =
  res
    "SetDataRetrievalPolicyResponse"
    "fixture/SetDataRetrievalPolicyResponse.proto"
    defaultService
    (Proxy :: Proxy SetDataRetrievalPolicy)

responseUploadArchive :: ArchiveCreationOutput -> TestTree
responseUploadArchive =
  res
    "UploadArchiveResponse"
    "fixture/UploadArchiveResponse.proto"
    defaultService
    (Proxy :: Proxy UploadArchive)

responseListProvisionedCapacity :: ListProvisionedCapacityResponse -> TestTree
responseListProvisionedCapacity =
  res
    "ListProvisionedCapacityResponse"
    "fixture/ListProvisionedCapacityResponse.proto"
    defaultService
    (Proxy :: Proxy ListProvisionedCapacity)

responseSetVaultNotifications :: SetVaultNotificationsResponse -> TestTree
responseSetVaultNotifications =
  res
    "SetVaultNotificationsResponse"
    "fixture/SetVaultNotificationsResponse.proto"
    defaultService
    (Proxy :: Proxy SetVaultNotifications)

responseDeleteVault :: DeleteVaultResponse -> TestTree
responseDeleteVault =
  res
    "DeleteVaultResponse"
    "fixture/DeleteVaultResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteVault)

responseAbortVaultLock :: AbortVaultLockResponse -> TestTree
responseAbortVaultLock =
  res
    "AbortVaultLockResponse"
    "fixture/AbortVaultLockResponse.proto"
    defaultService
    (Proxy :: Proxy AbortVaultLock)

responseDeleteArchive :: DeleteArchiveResponse -> TestTree
responseDeleteArchive =
  res
    "DeleteArchiveResponse"
    "fixture/DeleteArchiveResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteArchive)

responseRemoveTagsFromVault :: RemoveTagsFromVaultResponse -> TestTree
responseRemoveTagsFromVault =
  res
    "RemoveTagsFromVaultResponse"
    "fixture/RemoveTagsFromVaultResponse.proto"
    defaultService
    (Proxy :: Proxy RemoveTagsFromVault)

responseListVaults :: ListVaultsResponse -> TestTree
responseListVaults =
  res
    "ListVaultsResponse"
    "fixture/ListVaultsResponse.proto"
    defaultService
    (Proxy :: Proxy ListVaults)

responseInitiateVaultLock :: InitiateVaultLockResponse -> TestTree
responseInitiateVaultLock =
  res
    "InitiateVaultLockResponse"
    "fixture/InitiateVaultLockResponse.proto"
    defaultService
    (Proxy :: Proxy InitiateVaultLock)

responseDescribeJob :: GlacierJobDescription -> TestTree
responseDescribeJob =
  res
    "DescribeJobResponse"
    "fixture/DescribeJobResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeJob)

responseListTagsForVault :: ListTagsForVaultResponse -> TestTree
responseListTagsForVault =
  res
    "ListTagsForVaultResponse"
    "fixture/ListTagsForVaultResponse.proto"
    defaultService
    (Proxy :: Proxy ListTagsForVault)

responseGetVaultLock :: GetVaultLockResponse -> TestTree
responseGetVaultLock =
  res
    "GetVaultLockResponse"
    "fixture/GetVaultLockResponse.proto"
    defaultService
    (Proxy :: Proxy GetVaultLock)

responseAbortMultipartUpload :: AbortMultipartUploadResponse -> TestTree
responseAbortMultipartUpload =
  res
    "AbortMultipartUploadResponse"
    "fixture/AbortMultipartUploadResponse.proto"
    defaultService
    (Proxy :: Proxy AbortMultipartUpload)

responseDeleteVaultAccessPolicy :: DeleteVaultAccessPolicyResponse -> TestTree
responseDeleteVaultAccessPolicy =
  res
    "DeleteVaultAccessPolicyResponse"
    "fixture/DeleteVaultAccessPolicyResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteVaultAccessPolicy)

responseInitiateJob :: InitiateJobResponse -> TestTree
responseInitiateJob =
  res
    "InitiateJobResponse"
    "fixture/InitiateJobResponse.proto"
    defaultService
    (Proxy :: Proxy InitiateJob)

responseListMultipartUploads :: ListMultipartUploadsResponse -> TestTree
responseListMultipartUploads =
  res
    "ListMultipartUploadsResponse"
    "fixture/ListMultipartUploadsResponse.proto"
    defaultService
    (Proxy :: Proxy ListMultipartUploads)

responseAddTagsToVault :: AddTagsToVaultResponse -> TestTree
responseAddTagsToVault =
  res
    "AddTagsToVaultResponse"
    "fixture/AddTagsToVaultResponse.proto"
    defaultService
    (Proxy :: Proxy AddTagsToVault)

responseInitiateMultipartUpload :: InitiateMultipartUploadResponse -> TestTree
responseInitiateMultipartUpload =
  res
    "InitiateMultipartUploadResponse"
    "fixture/InitiateMultipartUploadResponse.proto"
    defaultService
    (Proxy :: Proxy InitiateMultipartUpload)

responseCreateVault :: CreateVaultResponse -> TestTree
responseCreateVault =
  res
    "CreateVaultResponse"
    "fixture/CreateVaultResponse.proto"
    defaultService
    (Proxy :: Proxy CreateVault)

responseListJobs :: ListJobsResponse -> TestTree
responseListJobs =
  res
    "ListJobsResponse"
    "fixture/ListJobsResponse.proto"
    defaultService
    (Proxy :: Proxy ListJobs)

responseListParts :: ListPartsResponse -> TestTree
responseListParts =
  res
    "ListPartsResponse"
    "fixture/ListPartsResponse.proto"
    defaultService
    (Proxy :: Proxy ListParts)

responseCompleteVaultLock :: CompleteVaultLockResponse -> TestTree
responseCompleteVaultLock =
  res
    "CompleteVaultLockResponse"
    "fixture/CompleteVaultLockResponse.proto"
    defaultService
    (Proxy :: Proxy CompleteVaultLock)

responseGetVaultAccessPolicy :: GetVaultAccessPolicyResponse -> TestTree
responseGetVaultAccessPolicy =
  res
    "GetVaultAccessPolicyResponse"
    "fixture/GetVaultAccessPolicyResponse.proto"
    defaultService
    (Proxy :: Proxy GetVaultAccessPolicy)

responseGetDataRetrievalPolicy :: GetDataRetrievalPolicyResponse -> TestTree
responseGetDataRetrievalPolicy =
  res
    "GetDataRetrievalPolicyResponse"
    "fixture/GetDataRetrievalPolicyResponse.proto"
    defaultService
    (Proxy :: Proxy GetDataRetrievalPolicy)

responseDeleteVaultNotifications :: DeleteVaultNotificationsResponse -> TestTree
responseDeleteVaultNotifications =
  res
    "DeleteVaultNotificationsResponse"
    "fixture/DeleteVaultNotificationsResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteVaultNotifications)

responseUploadMultipartPart :: UploadMultipartPartResponse -> TestTree
responseUploadMultipartPart =
  res
    "UploadMultipartPartResponse"
    "fixture/UploadMultipartPartResponse.proto"
    defaultService
    (Proxy :: Proxy UploadMultipartPart)

responseGetVaultNotifications :: GetVaultNotificationsResponse -> TestTree
responseGetVaultNotifications =
  res
    "GetVaultNotificationsResponse"
    "fixture/GetVaultNotificationsResponse.proto"
    defaultService
    (Proxy :: Proxy GetVaultNotifications)
