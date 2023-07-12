{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.Glacier
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.Glacier where

import Amazonka.Glacier
import qualified Data.Proxy as Proxy
import Test.Amazonka.Fixture
import Test.Amazonka.Glacier.Internal
import Test.Amazonka.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestAbortMultipartUpload $
--             newAbortMultipartUpload
--
--         , requestAbortVaultLock $
--             newAbortVaultLock
--
--         , requestAddTagsToVault $
--             newAddTagsToVault
--
--         , requestCompleteMultipartUpload $
--             newCompleteMultipartUpload
--
--         , requestCompleteVaultLock $
--             newCompleteVaultLock
--
--         , requestCreateVault $
--             newCreateVault
--
--         , requestDeleteArchive $
--             newDeleteArchive
--
--         , requestDeleteVault $
--             newDeleteVault
--
--         , requestDeleteVaultAccessPolicy $
--             newDeleteVaultAccessPolicy
--
--         , requestDeleteVaultNotifications $
--             newDeleteVaultNotifications
--
--         , requestDescribeJob $
--             newDescribeJob
--
--         , requestDescribeVault $
--             newDescribeVault
--
--         , requestGetDataRetrievalPolicy $
--             newGetDataRetrievalPolicy
--
--         , requestGetJobOutput $
--             newGetJobOutput
--
--         , requestGetVaultAccessPolicy $
--             newGetVaultAccessPolicy
--
--         , requestGetVaultLock $
--             newGetVaultLock
--
--         , requestGetVaultNotifications $
--             newGetVaultNotifications
--
--         , requestInitiateJob $
--             newInitiateJob
--
--         , requestInitiateMultipartUpload $
--             newInitiateMultipartUpload
--
--         , requestInitiateVaultLock $
--             newInitiateVaultLock
--
--         , requestListJobs $
--             newListJobs
--
--         , requestListMultipartUploads $
--             newListMultipartUploads
--
--         , requestListParts $
--             newListParts
--
--         , requestListProvisionedCapacity $
--             newListProvisionedCapacity
--
--         , requestListTagsForVault $
--             newListTagsForVault
--
--         , requestListVaults $
--             newListVaults
--
--         , requestPurchaseProvisionedCapacity $
--             newPurchaseProvisionedCapacity
--
--         , requestRemoveTagsFromVault $
--             newRemoveTagsFromVault
--
--         , requestSetDataRetrievalPolicy $
--             newSetDataRetrievalPolicy
--
--         , requestSetVaultAccessPolicy $
--             newSetVaultAccessPolicy
--
--         , requestSetVaultNotifications $
--             newSetVaultNotifications
--
--         , requestUploadArchive $
--             newUploadArchive
--
--         , requestUploadMultipartPart $
--             newUploadMultipartPart
--
--           ]

--     , testGroup "response"
--         [ responseAbortMultipartUpload $
--             newAbortMultipartUploadResponse
--
--         , responseAbortVaultLock $
--             newAbortVaultLockResponse
--
--         , responseAddTagsToVault $
--             newAddTagsToVaultResponse
--
--         , responseCompleteMultipartUpload $
--             newArchiveCreationOutput
--
--         , responseCompleteVaultLock $
--             newCompleteVaultLockResponse
--
--         , responseCreateVault $
--             newCreateVaultResponse
--
--         , responseDeleteArchive $
--             newDeleteArchiveResponse
--
--         , responseDeleteVault $
--             newDeleteVaultResponse
--
--         , responseDeleteVaultAccessPolicy $
--             newDeleteVaultAccessPolicyResponse
--
--         , responseDeleteVaultNotifications $
--             newDeleteVaultNotificationsResponse
--
--         , responseDescribeJob $
--             newGlacierJobDescription
--
--         , responseDescribeVault $
--             newDescribeVaultOutput
--
--         , responseGetDataRetrievalPolicy $
--             newGetDataRetrievalPolicyResponse
--
--         , responseGetJobOutput $
--             newGetJobOutputResponse
--
--         , responseGetVaultAccessPolicy $
--             newGetVaultAccessPolicyResponse
--
--         , responseGetVaultLock $
--             newGetVaultLockResponse
--
--         , responseGetVaultNotifications $
--             newGetVaultNotificationsResponse
--
--         , responseInitiateJob $
--             newInitiateJobResponse
--
--         , responseInitiateMultipartUpload $
--             newInitiateMultipartUploadResponse
--
--         , responseInitiateVaultLock $
--             newInitiateVaultLockResponse
--
--         , responseListJobs $
--             newListJobsResponse
--
--         , responseListMultipartUploads $
--             newListMultipartUploadsResponse
--
--         , responseListParts $
--             newListPartsResponse
--
--         , responseListProvisionedCapacity $
--             newListProvisionedCapacityResponse
--
--         , responseListTagsForVault $
--             newListTagsForVaultResponse
--
--         , responseListVaults $
--             newListVaultsResponse
--
--         , responsePurchaseProvisionedCapacity $
--             newPurchaseProvisionedCapacityResponse
--
--         , responseRemoveTagsFromVault $
--             newRemoveTagsFromVaultResponse
--
--         , responseSetDataRetrievalPolicy $
--             newSetDataRetrievalPolicyResponse
--
--         , responseSetVaultAccessPolicy $
--             newSetVaultAccessPolicyResponse
--
--         , responseSetVaultNotifications $
--             newSetVaultNotificationsResponse
--
--         , responseUploadArchive $
--             newArchiveCreationOutput
--
--         , responseUploadMultipartPart $
--             newUploadMultipartPartResponse
--
--           ]
--     ]

-- Requests

requestAbortMultipartUpload :: AbortMultipartUpload -> TestTree
requestAbortMultipartUpload =
  req
    "AbortMultipartUpload"
    "fixture/AbortMultipartUpload.yaml"

requestAbortVaultLock :: AbortVaultLock -> TestTree
requestAbortVaultLock =
  req
    "AbortVaultLock"
    "fixture/AbortVaultLock.yaml"

requestAddTagsToVault :: AddTagsToVault -> TestTree
requestAddTagsToVault =
  req
    "AddTagsToVault"
    "fixture/AddTagsToVault.yaml"

requestCompleteMultipartUpload :: CompleteMultipartUpload -> TestTree
requestCompleteMultipartUpload =
  req
    "CompleteMultipartUpload"
    "fixture/CompleteMultipartUpload.yaml"

requestCompleteVaultLock :: CompleteVaultLock -> TestTree
requestCompleteVaultLock =
  req
    "CompleteVaultLock"
    "fixture/CompleteVaultLock.yaml"

requestCreateVault :: CreateVault -> TestTree
requestCreateVault =
  req
    "CreateVault"
    "fixture/CreateVault.yaml"

requestDeleteArchive :: DeleteArchive -> TestTree
requestDeleteArchive =
  req
    "DeleteArchive"
    "fixture/DeleteArchive.yaml"

requestDeleteVault :: DeleteVault -> TestTree
requestDeleteVault =
  req
    "DeleteVault"
    "fixture/DeleteVault.yaml"

requestDeleteVaultAccessPolicy :: DeleteVaultAccessPolicy -> TestTree
requestDeleteVaultAccessPolicy =
  req
    "DeleteVaultAccessPolicy"
    "fixture/DeleteVaultAccessPolicy.yaml"

requestDeleteVaultNotifications :: DeleteVaultNotifications -> TestTree
requestDeleteVaultNotifications =
  req
    "DeleteVaultNotifications"
    "fixture/DeleteVaultNotifications.yaml"

requestDescribeJob :: DescribeJob -> TestTree
requestDescribeJob =
  req
    "DescribeJob"
    "fixture/DescribeJob.yaml"

requestDescribeVault :: DescribeVault -> TestTree
requestDescribeVault =
  req
    "DescribeVault"
    "fixture/DescribeVault.yaml"

requestGetDataRetrievalPolicy :: GetDataRetrievalPolicy -> TestTree
requestGetDataRetrievalPolicy =
  req
    "GetDataRetrievalPolicy"
    "fixture/GetDataRetrievalPolicy.yaml"

requestGetJobOutput :: GetJobOutput -> TestTree
requestGetJobOutput =
  req
    "GetJobOutput"
    "fixture/GetJobOutput.yaml"

requestGetVaultAccessPolicy :: GetVaultAccessPolicy -> TestTree
requestGetVaultAccessPolicy =
  req
    "GetVaultAccessPolicy"
    "fixture/GetVaultAccessPolicy.yaml"

requestGetVaultLock :: GetVaultLock -> TestTree
requestGetVaultLock =
  req
    "GetVaultLock"
    "fixture/GetVaultLock.yaml"

requestGetVaultNotifications :: GetVaultNotifications -> TestTree
requestGetVaultNotifications =
  req
    "GetVaultNotifications"
    "fixture/GetVaultNotifications.yaml"

requestInitiateJob :: InitiateJob -> TestTree
requestInitiateJob =
  req
    "InitiateJob"
    "fixture/InitiateJob.yaml"

requestInitiateMultipartUpload :: InitiateMultipartUpload -> TestTree
requestInitiateMultipartUpload =
  req
    "InitiateMultipartUpload"
    "fixture/InitiateMultipartUpload.yaml"

requestInitiateVaultLock :: InitiateVaultLock -> TestTree
requestInitiateVaultLock =
  req
    "InitiateVaultLock"
    "fixture/InitiateVaultLock.yaml"

requestListJobs :: ListJobs -> TestTree
requestListJobs =
  req
    "ListJobs"
    "fixture/ListJobs.yaml"

requestListMultipartUploads :: ListMultipartUploads -> TestTree
requestListMultipartUploads =
  req
    "ListMultipartUploads"
    "fixture/ListMultipartUploads.yaml"

requestListParts :: ListParts -> TestTree
requestListParts =
  req
    "ListParts"
    "fixture/ListParts.yaml"

requestListProvisionedCapacity :: ListProvisionedCapacity -> TestTree
requestListProvisionedCapacity =
  req
    "ListProvisionedCapacity"
    "fixture/ListProvisionedCapacity.yaml"

requestListTagsForVault :: ListTagsForVault -> TestTree
requestListTagsForVault =
  req
    "ListTagsForVault"
    "fixture/ListTagsForVault.yaml"

requestListVaults :: ListVaults -> TestTree
requestListVaults =
  req
    "ListVaults"
    "fixture/ListVaults.yaml"

requestPurchaseProvisionedCapacity :: PurchaseProvisionedCapacity -> TestTree
requestPurchaseProvisionedCapacity =
  req
    "PurchaseProvisionedCapacity"
    "fixture/PurchaseProvisionedCapacity.yaml"

requestRemoveTagsFromVault :: RemoveTagsFromVault -> TestTree
requestRemoveTagsFromVault =
  req
    "RemoveTagsFromVault"
    "fixture/RemoveTagsFromVault.yaml"

requestSetDataRetrievalPolicy :: SetDataRetrievalPolicy -> TestTree
requestSetDataRetrievalPolicy =
  req
    "SetDataRetrievalPolicy"
    "fixture/SetDataRetrievalPolicy.yaml"

requestSetVaultAccessPolicy :: SetVaultAccessPolicy -> TestTree
requestSetVaultAccessPolicy =
  req
    "SetVaultAccessPolicy"
    "fixture/SetVaultAccessPolicy.yaml"

requestSetVaultNotifications :: SetVaultNotifications -> TestTree
requestSetVaultNotifications =
  req
    "SetVaultNotifications"
    "fixture/SetVaultNotifications.yaml"

-- Responses

responseAbortMultipartUpload :: AbortMultipartUploadResponse -> TestTree
responseAbortMultipartUpload =
  res
    "AbortMultipartUploadResponse"
    "fixture/AbortMultipartUploadResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AbortMultipartUpload)

responseAbortVaultLock :: AbortVaultLockResponse -> TestTree
responseAbortVaultLock =
  res
    "AbortVaultLockResponse"
    "fixture/AbortVaultLockResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AbortVaultLock)

responseAddTagsToVault :: AddTagsToVaultResponse -> TestTree
responseAddTagsToVault =
  res
    "AddTagsToVaultResponse"
    "fixture/AddTagsToVaultResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AddTagsToVault)

responseCompleteMultipartUpload :: ArchiveCreationOutput -> TestTree
responseCompleteMultipartUpload =
  res
    "CompleteMultipartUploadResponse"
    "fixture/CompleteMultipartUploadResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CompleteMultipartUpload)

responseCompleteVaultLock :: CompleteVaultLockResponse -> TestTree
responseCompleteVaultLock =
  res
    "CompleteVaultLockResponse"
    "fixture/CompleteVaultLockResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CompleteVaultLock)

responseCreateVault :: CreateVaultResponse -> TestTree
responseCreateVault =
  res
    "CreateVaultResponse"
    "fixture/CreateVaultResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateVault)

responseDeleteArchive :: DeleteArchiveResponse -> TestTree
responseDeleteArchive =
  res
    "DeleteArchiveResponse"
    "fixture/DeleteArchiveResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteArchive)

responseDeleteVault :: DeleteVaultResponse -> TestTree
responseDeleteVault =
  res
    "DeleteVaultResponse"
    "fixture/DeleteVaultResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteVault)

responseDeleteVaultAccessPolicy :: DeleteVaultAccessPolicyResponse -> TestTree
responseDeleteVaultAccessPolicy =
  res
    "DeleteVaultAccessPolicyResponse"
    "fixture/DeleteVaultAccessPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteVaultAccessPolicy)

responseDeleteVaultNotifications :: DeleteVaultNotificationsResponse -> TestTree
responseDeleteVaultNotifications =
  res
    "DeleteVaultNotificationsResponse"
    "fixture/DeleteVaultNotificationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteVaultNotifications)

responseDescribeJob :: GlacierJobDescription -> TestTree
responseDescribeJob =
  res
    "DescribeJobResponse"
    "fixture/DescribeJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeJob)

responseDescribeVault :: DescribeVaultOutput -> TestTree
responseDescribeVault =
  res
    "DescribeVaultResponse"
    "fixture/DescribeVaultResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeVault)

responseGetDataRetrievalPolicy :: GetDataRetrievalPolicyResponse -> TestTree
responseGetDataRetrievalPolicy =
  res
    "GetDataRetrievalPolicyResponse"
    "fixture/GetDataRetrievalPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetDataRetrievalPolicy)

responseGetVaultAccessPolicy :: GetVaultAccessPolicyResponse -> TestTree
responseGetVaultAccessPolicy =
  res
    "GetVaultAccessPolicyResponse"
    "fixture/GetVaultAccessPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetVaultAccessPolicy)

responseGetVaultLock :: GetVaultLockResponse -> TestTree
responseGetVaultLock =
  res
    "GetVaultLockResponse"
    "fixture/GetVaultLockResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetVaultLock)

responseGetVaultNotifications :: GetVaultNotificationsResponse -> TestTree
responseGetVaultNotifications =
  res
    "GetVaultNotificationsResponse"
    "fixture/GetVaultNotificationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetVaultNotifications)

responseInitiateJob :: InitiateJobResponse -> TestTree
responseInitiateJob =
  res
    "InitiateJobResponse"
    "fixture/InitiateJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy InitiateJob)

responseInitiateMultipartUpload :: InitiateMultipartUploadResponse -> TestTree
responseInitiateMultipartUpload =
  res
    "InitiateMultipartUploadResponse"
    "fixture/InitiateMultipartUploadResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy InitiateMultipartUpload)

responseInitiateVaultLock :: InitiateVaultLockResponse -> TestTree
responseInitiateVaultLock =
  res
    "InitiateVaultLockResponse"
    "fixture/InitiateVaultLockResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy InitiateVaultLock)

responseListJobs :: ListJobsResponse -> TestTree
responseListJobs =
  res
    "ListJobsResponse"
    "fixture/ListJobsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListJobs)

responseListMultipartUploads :: ListMultipartUploadsResponse -> TestTree
responseListMultipartUploads =
  res
    "ListMultipartUploadsResponse"
    "fixture/ListMultipartUploadsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListMultipartUploads)

responseListParts :: ListPartsResponse -> TestTree
responseListParts =
  res
    "ListPartsResponse"
    "fixture/ListPartsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListParts)

responseListProvisionedCapacity :: ListProvisionedCapacityResponse -> TestTree
responseListProvisionedCapacity =
  res
    "ListProvisionedCapacityResponse"
    "fixture/ListProvisionedCapacityResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListProvisionedCapacity)

responseListTagsForVault :: ListTagsForVaultResponse -> TestTree
responseListTagsForVault =
  res
    "ListTagsForVaultResponse"
    "fixture/ListTagsForVaultResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForVault)

responseListVaults :: ListVaultsResponse -> TestTree
responseListVaults =
  res
    "ListVaultsResponse"
    "fixture/ListVaultsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListVaults)

responsePurchaseProvisionedCapacity :: PurchaseProvisionedCapacityResponse -> TestTree
responsePurchaseProvisionedCapacity =
  res
    "PurchaseProvisionedCapacityResponse"
    "fixture/PurchaseProvisionedCapacityResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PurchaseProvisionedCapacity)

responseRemoveTagsFromVault :: RemoveTagsFromVaultResponse -> TestTree
responseRemoveTagsFromVault =
  res
    "RemoveTagsFromVaultResponse"
    "fixture/RemoveTagsFromVaultResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RemoveTagsFromVault)

responseSetDataRetrievalPolicy :: SetDataRetrievalPolicyResponse -> TestTree
responseSetDataRetrievalPolicy =
  res
    "SetDataRetrievalPolicyResponse"
    "fixture/SetDataRetrievalPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SetDataRetrievalPolicy)

responseSetVaultAccessPolicy :: SetVaultAccessPolicyResponse -> TestTree
responseSetVaultAccessPolicy =
  res
    "SetVaultAccessPolicyResponse"
    "fixture/SetVaultAccessPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SetVaultAccessPolicy)

responseSetVaultNotifications :: SetVaultNotificationsResponse -> TestTree
responseSetVaultNotifications =
  res
    "SetVaultNotificationsResponse"
    "fixture/SetVaultNotificationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SetVaultNotifications)

responseUploadArchive :: ArchiveCreationOutput -> TestTree
responseUploadArchive =
  res
    "UploadArchiveResponse"
    "fixture/UploadArchiveResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UploadArchive)

responseUploadMultipartPart :: UploadMultipartPartResponse -> TestTree
responseUploadMultipartPart =
  res
    "UploadMultipartPartResponse"
    "fixture/UploadMultipartPartResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UploadMultipartPart)
