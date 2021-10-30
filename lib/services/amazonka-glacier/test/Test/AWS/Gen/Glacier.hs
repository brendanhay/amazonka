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

import qualified Data.Proxy as Proxy
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
--             newSetVaultAccessPolicy
--
--         , requestInitiateJob $
--             newInitiateJob
--
--         , requestSetDataRetrievalPolicy $
--             newSetDataRetrievalPolicy
--
--         , requestDeleteVaultAccessPolicy $
--             newDeleteVaultAccessPolicy
--
--         , requestGetVaultNotifications $
--             newGetVaultNotifications
--
--         , requestListTagsForVault $
--             newListTagsForVault
--
--         , requestUploadMultipartPart $
--             newUploadMultipartPart
--
--         , requestDeleteVaultNotifications $
--             newDeleteVaultNotifications
--
--         , requestCompleteVaultLock $
--             newCompleteVaultLock
--
--         , requestAbortVaultLock $
--             newAbortVaultLock
--
--         , requestListVaults $
--             newListVaults
--
--         , requestListProvisionedCapacity $
--             newListProvisionedCapacity
--
--         , requestListJobs $
--             newListJobs
--
--         , requestSetVaultNotifications $
--             newSetVaultNotifications
--
--         , requestGetJobOutput $
--             newGetJobOutput
--
--         , requestCompleteMultipartUpload $
--             newCompleteMultipartUpload
--
--         , requestListMultipartUploads $
--             newListMultipartUploads
--
--         , requestAbortMultipartUpload $
--             newAbortMultipartUpload
--
--         , requestPurchaseProvisionedCapacity $
--             newPurchaseProvisionedCapacity
--
--         , requestDescribeVault $
--             newDescribeVault
--
--         , requestGetVaultLock $
--             newGetVaultLock
--
--         , requestDescribeJob $
--             newDescribeJob
--
--         , requestInitiateVaultLock $
--             newInitiateVaultLock
--
--         , requestGetVaultAccessPolicy $
--             newGetVaultAccessPolicy
--
--         , requestGetDataRetrievalPolicy $
--             newGetDataRetrievalPolicy
--
--         , requestRemoveTagsFromVault $
--             newRemoveTagsFromVault
--
--         , requestDeleteVault $
--             newDeleteVault
--
--         , requestDeleteArchive $
--             newDeleteArchive
--
--         , requestCreateVault $
--             newCreateVault
--
--         , requestInitiateMultipartUpload $
--             newInitiateMultipartUpload
--
--         , requestListParts $
--             newListParts
--
--         , requestAddTagsToVault $
--             newAddTagsToVault
--
--         , requestUploadArchive $
--             newUploadArchive
--
--           ]

--     , testGroup "response"
--         [ responseSetVaultAccessPolicy $
--             newSetVaultAccessPolicyResponse
--
--         , responseInitiateJob $
--             newInitiateJobResponse
--
--         , responseSetDataRetrievalPolicy $
--             newSetDataRetrievalPolicyResponse
--
--         , responseDeleteVaultAccessPolicy $
--             newDeleteVaultAccessPolicyResponse
--
--         , responseGetVaultNotifications $
--             newGetVaultNotificationsResponse
--
--         , responseListTagsForVault $
--             newListTagsForVaultResponse
--
--         , responseUploadMultipartPart $
--             newUploadMultipartPartResponse
--
--         , responseDeleteVaultNotifications $
--             newDeleteVaultNotificationsResponse
--
--         , responseCompleteVaultLock $
--             newCompleteVaultLockResponse
--
--         , responseAbortVaultLock $
--             newAbortVaultLockResponse
--
--         , responseListVaults $
--             newListVaultsResponse
--
--         , responseListProvisionedCapacity $
--             newListProvisionedCapacityResponse
--
--         , responseListJobs $
--             newListJobsResponse
--
--         , responseSetVaultNotifications $
--             newSetVaultNotificationsResponse
--
--         , responseGetJobOutput $
--             newGetJobOutputResponse
--
--         , responseCompleteMultipartUpload $
--             newArchiveCreationOutput
--
--         , responseListMultipartUploads $
--             newListMultipartUploadsResponse
--
--         , responseAbortMultipartUpload $
--             newAbortMultipartUploadResponse
--
--         , responsePurchaseProvisionedCapacity $
--             newPurchaseProvisionedCapacityResponse
--
--         , responseDescribeVault $
--             newDescribeVaultOutput
--
--         , responseGetVaultLock $
--             newGetVaultLockResponse
--
--         , responseDescribeJob $
--             newGlacierJobDescription
--
--         , responseInitiateVaultLock $
--             newInitiateVaultLockResponse
--
--         , responseGetVaultAccessPolicy $
--             newGetVaultAccessPolicyResponse
--
--         , responseGetDataRetrievalPolicy $
--             newGetDataRetrievalPolicyResponse
--
--         , responseRemoveTagsFromVault $
--             newRemoveTagsFromVaultResponse
--
--         , responseDeleteVault $
--             newDeleteVaultResponse
--
--         , responseDeleteArchive $
--             newDeleteArchiveResponse
--
--         , responseCreateVault $
--             newCreateVaultResponse
--
--         , responseInitiateMultipartUpload $
--             newInitiateMultipartUploadResponse
--
--         , responseListParts $
--             newListPartsResponse
--
--         , responseAddTagsToVault $
--             newAddTagsToVaultResponse
--
--         , responseUploadArchive $
--             newArchiveCreationOutput
--
--           ]
--     ]

-- Requests

requestSetVaultAccessPolicy :: SetVaultAccessPolicy -> TestTree
requestSetVaultAccessPolicy =
  req
    "SetVaultAccessPolicy"
    "fixture/SetVaultAccessPolicy.yaml"

requestInitiateJob :: InitiateJob -> TestTree
requestInitiateJob =
  req
    "InitiateJob"
    "fixture/InitiateJob.yaml"

requestSetDataRetrievalPolicy :: SetDataRetrievalPolicy -> TestTree
requestSetDataRetrievalPolicy =
  req
    "SetDataRetrievalPolicy"
    "fixture/SetDataRetrievalPolicy.yaml"

requestDeleteVaultAccessPolicy :: DeleteVaultAccessPolicy -> TestTree
requestDeleteVaultAccessPolicy =
  req
    "DeleteVaultAccessPolicy"
    "fixture/DeleteVaultAccessPolicy.yaml"

requestGetVaultNotifications :: GetVaultNotifications -> TestTree
requestGetVaultNotifications =
  req
    "GetVaultNotifications"
    "fixture/GetVaultNotifications.yaml"

requestListTagsForVault :: ListTagsForVault -> TestTree
requestListTagsForVault =
  req
    "ListTagsForVault"
    "fixture/ListTagsForVault.yaml"

requestDeleteVaultNotifications :: DeleteVaultNotifications -> TestTree
requestDeleteVaultNotifications =
  req
    "DeleteVaultNotifications"
    "fixture/DeleteVaultNotifications.yaml"

requestCompleteVaultLock :: CompleteVaultLock -> TestTree
requestCompleteVaultLock =
  req
    "CompleteVaultLock"
    "fixture/CompleteVaultLock.yaml"

requestAbortVaultLock :: AbortVaultLock -> TestTree
requestAbortVaultLock =
  req
    "AbortVaultLock"
    "fixture/AbortVaultLock.yaml"

requestListVaults :: ListVaults -> TestTree
requestListVaults =
  req
    "ListVaults"
    "fixture/ListVaults.yaml"

requestListProvisionedCapacity :: ListProvisionedCapacity -> TestTree
requestListProvisionedCapacity =
  req
    "ListProvisionedCapacity"
    "fixture/ListProvisionedCapacity.yaml"

requestListJobs :: ListJobs -> TestTree
requestListJobs =
  req
    "ListJobs"
    "fixture/ListJobs.yaml"

requestSetVaultNotifications :: SetVaultNotifications -> TestTree
requestSetVaultNotifications =
  req
    "SetVaultNotifications"
    "fixture/SetVaultNotifications.yaml"

requestGetJobOutput :: GetJobOutput -> TestTree
requestGetJobOutput =
  req
    "GetJobOutput"
    "fixture/GetJobOutput.yaml"

requestCompleteMultipartUpload :: CompleteMultipartUpload -> TestTree
requestCompleteMultipartUpload =
  req
    "CompleteMultipartUpload"
    "fixture/CompleteMultipartUpload.yaml"

requestListMultipartUploads :: ListMultipartUploads -> TestTree
requestListMultipartUploads =
  req
    "ListMultipartUploads"
    "fixture/ListMultipartUploads.yaml"

requestAbortMultipartUpload :: AbortMultipartUpload -> TestTree
requestAbortMultipartUpload =
  req
    "AbortMultipartUpload"
    "fixture/AbortMultipartUpload.yaml"

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

requestGetVaultLock :: GetVaultLock -> TestTree
requestGetVaultLock =
  req
    "GetVaultLock"
    "fixture/GetVaultLock.yaml"

requestDescribeJob :: DescribeJob -> TestTree
requestDescribeJob =
  req
    "DescribeJob"
    "fixture/DescribeJob.yaml"

requestInitiateVaultLock :: InitiateVaultLock -> TestTree
requestInitiateVaultLock =
  req
    "InitiateVaultLock"
    "fixture/InitiateVaultLock.yaml"

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

requestRemoveTagsFromVault :: RemoveTagsFromVault -> TestTree
requestRemoveTagsFromVault =
  req
    "RemoveTagsFromVault"
    "fixture/RemoveTagsFromVault.yaml"

requestDeleteVault :: DeleteVault -> TestTree
requestDeleteVault =
  req
    "DeleteVault"
    "fixture/DeleteVault.yaml"

requestDeleteArchive :: DeleteArchive -> TestTree
requestDeleteArchive =
  req
    "DeleteArchive"
    "fixture/DeleteArchive.yaml"

requestCreateVault :: CreateVault -> TestTree
requestCreateVault =
  req
    "CreateVault"
    "fixture/CreateVault.yaml"

requestInitiateMultipartUpload :: InitiateMultipartUpload -> TestTree
requestInitiateMultipartUpload =
  req
    "InitiateMultipartUpload"
    "fixture/InitiateMultipartUpload.yaml"

requestListParts :: ListParts -> TestTree
requestListParts =
  req
    "ListParts"
    "fixture/ListParts.yaml"

requestAddTagsToVault :: AddTagsToVault -> TestTree
requestAddTagsToVault =
  req
    "AddTagsToVault"
    "fixture/AddTagsToVault.yaml"

-- Responses

responseSetVaultAccessPolicy :: SetVaultAccessPolicyResponse -> TestTree
responseSetVaultAccessPolicy =
  res
    "SetVaultAccessPolicyResponse"
    "fixture/SetVaultAccessPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SetVaultAccessPolicy)

responseInitiateJob :: InitiateJobResponse -> TestTree
responseInitiateJob =
  res
    "InitiateJobResponse"
    "fixture/InitiateJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy InitiateJob)

responseSetDataRetrievalPolicy :: SetDataRetrievalPolicyResponse -> TestTree
responseSetDataRetrievalPolicy =
  res
    "SetDataRetrievalPolicyResponse"
    "fixture/SetDataRetrievalPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SetDataRetrievalPolicy)

responseDeleteVaultAccessPolicy :: DeleteVaultAccessPolicyResponse -> TestTree
responseDeleteVaultAccessPolicy =
  res
    "DeleteVaultAccessPolicyResponse"
    "fixture/DeleteVaultAccessPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteVaultAccessPolicy)

responseGetVaultNotifications :: GetVaultNotificationsResponse -> TestTree
responseGetVaultNotifications =
  res
    "GetVaultNotificationsResponse"
    "fixture/GetVaultNotificationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetVaultNotifications)

responseListTagsForVault :: ListTagsForVaultResponse -> TestTree
responseListTagsForVault =
  res
    "ListTagsForVaultResponse"
    "fixture/ListTagsForVaultResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForVault)

responseUploadMultipartPart :: UploadMultipartPartResponse -> TestTree
responseUploadMultipartPart =
  res
    "UploadMultipartPartResponse"
    "fixture/UploadMultipartPartResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UploadMultipartPart)

responseDeleteVaultNotifications :: DeleteVaultNotificationsResponse -> TestTree
responseDeleteVaultNotifications =
  res
    "DeleteVaultNotificationsResponse"
    "fixture/DeleteVaultNotificationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteVaultNotifications)

responseCompleteVaultLock :: CompleteVaultLockResponse -> TestTree
responseCompleteVaultLock =
  res
    "CompleteVaultLockResponse"
    "fixture/CompleteVaultLockResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CompleteVaultLock)

responseAbortVaultLock :: AbortVaultLockResponse -> TestTree
responseAbortVaultLock =
  res
    "AbortVaultLockResponse"
    "fixture/AbortVaultLockResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AbortVaultLock)

responseListVaults :: ListVaultsResponse -> TestTree
responseListVaults =
  res
    "ListVaultsResponse"
    "fixture/ListVaultsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListVaults)

responseListProvisionedCapacity :: ListProvisionedCapacityResponse -> TestTree
responseListProvisionedCapacity =
  res
    "ListProvisionedCapacityResponse"
    "fixture/ListProvisionedCapacityResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListProvisionedCapacity)

responseListJobs :: ListJobsResponse -> TestTree
responseListJobs =
  res
    "ListJobsResponse"
    "fixture/ListJobsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListJobs)

responseSetVaultNotifications :: SetVaultNotificationsResponse -> TestTree
responseSetVaultNotifications =
  res
    "SetVaultNotificationsResponse"
    "fixture/SetVaultNotificationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SetVaultNotifications)

responseCompleteMultipartUpload :: ArchiveCreationOutput -> TestTree
responseCompleteMultipartUpload =
  res
    "CompleteMultipartUploadResponse"
    "fixture/CompleteMultipartUploadResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CompleteMultipartUpload)

responseListMultipartUploads :: ListMultipartUploadsResponse -> TestTree
responseListMultipartUploads =
  res
    "ListMultipartUploadsResponse"
    "fixture/ListMultipartUploadsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListMultipartUploads)

responseAbortMultipartUpload :: AbortMultipartUploadResponse -> TestTree
responseAbortMultipartUpload =
  res
    "AbortMultipartUploadResponse"
    "fixture/AbortMultipartUploadResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AbortMultipartUpload)

responsePurchaseProvisionedCapacity :: PurchaseProvisionedCapacityResponse -> TestTree
responsePurchaseProvisionedCapacity =
  res
    "PurchaseProvisionedCapacityResponse"
    "fixture/PurchaseProvisionedCapacityResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PurchaseProvisionedCapacity)

responseDescribeVault :: DescribeVaultOutput -> TestTree
responseDescribeVault =
  res
    "DescribeVaultResponse"
    "fixture/DescribeVaultResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeVault)

responseGetVaultLock :: GetVaultLockResponse -> TestTree
responseGetVaultLock =
  res
    "GetVaultLockResponse"
    "fixture/GetVaultLockResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetVaultLock)

responseDescribeJob :: GlacierJobDescription -> TestTree
responseDescribeJob =
  res
    "DescribeJobResponse"
    "fixture/DescribeJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeJob)

responseInitiateVaultLock :: InitiateVaultLockResponse -> TestTree
responseInitiateVaultLock =
  res
    "InitiateVaultLockResponse"
    "fixture/InitiateVaultLockResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy InitiateVaultLock)

responseGetVaultAccessPolicy :: GetVaultAccessPolicyResponse -> TestTree
responseGetVaultAccessPolicy =
  res
    "GetVaultAccessPolicyResponse"
    "fixture/GetVaultAccessPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetVaultAccessPolicy)

responseGetDataRetrievalPolicy :: GetDataRetrievalPolicyResponse -> TestTree
responseGetDataRetrievalPolicy =
  res
    "GetDataRetrievalPolicyResponse"
    "fixture/GetDataRetrievalPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetDataRetrievalPolicy)

responseRemoveTagsFromVault :: RemoveTagsFromVaultResponse -> TestTree
responseRemoveTagsFromVault =
  res
    "RemoveTagsFromVaultResponse"
    "fixture/RemoveTagsFromVaultResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RemoveTagsFromVault)

responseDeleteVault :: DeleteVaultResponse -> TestTree
responseDeleteVault =
  res
    "DeleteVaultResponse"
    "fixture/DeleteVaultResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteVault)

responseDeleteArchive :: DeleteArchiveResponse -> TestTree
responseDeleteArchive =
  res
    "DeleteArchiveResponse"
    "fixture/DeleteArchiveResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteArchive)

responseCreateVault :: CreateVaultResponse -> TestTree
responseCreateVault =
  res
    "CreateVaultResponse"
    "fixture/CreateVaultResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateVault)

responseInitiateMultipartUpload :: InitiateMultipartUploadResponse -> TestTree
responseInitiateMultipartUpload =
  res
    "InitiateMultipartUploadResponse"
    "fixture/InitiateMultipartUploadResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy InitiateMultipartUpload)

responseListParts :: ListPartsResponse -> TestTree
responseListParts =
  res
    "ListPartsResponse"
    "fixture/ListPartsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListParts)

responseAddTagsToVault :: AddTagsToVaultResponse -> TestTree
responseAddTagsToVault =
  res
    "AddTagsToVaultResponse"
    "fixture/AddTagsToVaultResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AddTagsToVault)

responseUploadArchive :: ArchiveCreationOutput -> TestTree
responseUploadArchive =
  res
    "UploadArchiveResponse"
    "fixture/UploadArchiveResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UploadArchive)
