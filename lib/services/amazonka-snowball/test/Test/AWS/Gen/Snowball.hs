{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.Snowball
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.AWS.Gen.Snowball where

import Data.Proxy
import Network.AWS.Snowball
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.AWS.Snowball.Internal
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestListClusterJobs $
--             newListClusterJobs
--
--         , requestUpdateJobShipmentState $
--             newUpdateJobShipmentState
--
--         , requestCancelJob $
--             newCancelJob
--
--         , requestUpdateJob $
--             newUpdateJob
--
--         , requestCreateCluster $
--             newCreateCluster
--
--         , requestListLongTermPricing $
--             newListLongTermPricing
--
--         , requestGetSoftwareUpdates $
--             newGetSoftwareUpdates
--
--         , requestDescribeReturnShippingLabel $
--             newDescribeReturnShippingLabel
--
--         , requestDescribeAddress $
--             newDescribeAddress
--
--         , requestListCompatibleImages $
--             newListCompatibleImages
--
--         , requestDescribeAddresses $
--             newDescribeAddresses
--
--         , requestDescribeJob $
--             newDescribeJob
--
--         , requestDescribeCluster $
--             newDescribeCluster
--
--         , requestGetJobUnlockCode $
--             newGetJobUnlockCode
--
--         , requestCancelCluster $
--             newCancelCluster
--
--         , requestGetJobManifest $
--             newGetJobManifest
--
--         , requestListJobs $
--             newListJobs
--
--         , requestCreateJob $
--             newCreateJob
--
--         , requestListClusters $
--             newListClusters
--
--         , requestUpdateCluster $
--             newUpdateCluster
--
--         , requestCreateLongTermPricing $
--             newCreateLongTermPricing
--
--         , requestUpdateLongTermPricing $
--             newUpdateLongTermPricing
--
--         , requestGetSnowballUsage $
--             newGetSnowballUsage
--
--         , requestCreateAddress $
--             newCreateAddress
--
--         , requestCreateReturnShippingLabel $
--             newCreateReturnShippingLabel
--
--           ]

--     , testGroup "response"
--         [ responseListClusterJobs $
--             newListClusterJobsResponse
--
--         , responseUpdateJobShipmentState $
--             newUpdateJobShipmentStateResponse
--
--         , responseCancelJob $
--             newCancelJobResponse
--
--         , responseUpdateJob $
--             newUpdateJobResponse
--
--         , responseCreateCluster $
--             newCreateClusterResponse
--
--         , responseListLongTermPricing $
--             newListLongTermPricingResponse
--
--         , responseGetSoftwareUpdates $
--             newGetSoftwareUpdatesResponse
--
--         , responseDescribeReturnShippingLabel $
--             newDescribeReturnShippingLabelResponse
--
--         , responseDescribeAddress $
--             newDescribeAddressResponse
--
--         , responseListCompatibleImages $
--             newListCompatibleImagesResponse
--
--         , responseDescribeAddresses $
--             newDescribeAddressesResponse
--
--         , responseDescribeJob $
--             newDescribeJobResponse
--
--         , responseDescribeCluster $
--             newDescribeClusterResponse
--
--         , responseGetJobUnlockCode $
--             newGetJobUnlockCodeResponse
--
--         , responseCancelCluster $
--             newCancelClusterResponse
--
--         , responseGetJobManifest $
--             newGetJobManifestResponse
--
--         , responseListJobs $
--             newListJobsResponse
--
--         , responseCreateJob $
--             newCreateJobResponse
--
--         , responseListClusters $
--             newListClustersResponse
--
--         , responseUpdateCluster $
--             newUpdateClusterResponse
--
--         , responseCreateLongTermPricing $
--             newCreateLongTermPricingResponse
--
--         , responseUpdateLongTermPricing $
--             newUpdateLongTermPricingResponse
--
--         , responseGetSnowballUsage $
--             newGetSnowballUsageResponse
--
--         , responseCreateAddress $
--             newCreateAddressResponse
--
--         , responseCreateReturnShippingLabel $
--             newCreateReturnShippingLabelResponse
--
--           ]
--     ]

-- Requests

requestListClusterJobs :: ListClusterJobs -> TestTree
requestListClusterJobs =
  req
    "ListClusterJobs"
    "fixture/ListClusterJobs.yaml"

requestUpdateJobShipmentState :: UpdateJobShipmentState -> TestTree
requestUpdateJobShipmentState =
  req
    "UpdateJobShipmentState"
    "fixture/UpdateJobShipmentState.yaml"

requestCancelJob :: CancelJob -> TestTree
requestCancelJob =
  req
    "CancelJob"
    "fixture/CancelJob.yaml"

requestUpdateJob :: UpdateJob -> TestTree
requestUpdateJob =
  req
    "UpdateJob"
    "fixture/UpdateJob.yaml"

requestCreateCluster :: CreateCluster -> TestTree
requestCreateCluster =
  req
    "CreateCluster"
    "fixture/CreateCluster.yaml"

requestListLongTermPricing :: ListLongTermPricing -> TestTree
requestListLongTermPricing =
  req
    "ListLongTermPricing"
    "fixture/ListLongTermPricing.yaml"

requestGetSoftwareUpdates :: GetSoftwareUpdates -> TestTree
requestGetSoftwareUpdates =
  req
    "GetSoftwareUpdates"
    "fixture/GetSoftwareUpdates.yaml"

requestDescribeReturnShippingLabel :: DescribeReturnShippingLabel -> TestTree
requestDescribeReturnShippingLabel =
  req
    "DescribeReturnShippingLabel"
    "fixture/DescribeReturnShippingLabel.yaml"

requestDescribeAddress :: DescribeAddress -> TestTree
requestDescribeAddress =
  req
    "DescribeAddress"
    "fixture/DescribeAddress.yaml"

requestListCompatibleImages :: ListCompatibleImages -> TestTree
requestListCompatibleImages =
  req
    "ListCompatibleImages"
    "fixture/ListCompatibleImages.yaml"

requestDescribeAddresses :: DescribeAddresses -> TestTree
requestDescribeAddresses =
  req
    "DescribeAddresses"
    "fixture/DescribeAddresses.yaml"

requestDescribeJob :: DescribeJob -> TestTree
requestDescribeJob =
  req
    "DescribeJob"
    "fixture/DescribeJob.yaml"

requestDescribeCluster :: DescribeCluster -> TestTree
requestDescribeCluster =
  req
    "DescribeCluster"
    "fixture/DescribeCluster.yaml"

requestGetJobUnlockCode :: GetJobUnlockCode -> TestTree
requestGetJobUnlockCode =
  req
    "GetJobUnlockCode"
    "fixture/GetJobUnlockCode.yaml"

requestCancelCluster :: CancelCluster -> TestTree
requestCancelCluster =
  req
    "CancelCluster"
    "fixture/CancelCluster.yaml"

requestGetJobManifest :: GetJobManifest -> TestTree
requestGetJobManifest =
  req
    "GetJobManifest"
    "fixture/GetJobManifest.yaml"

requestListJobs :: ListJobs -> TestTree
requestListJobs =
  req
    "ListJobs"
    "fixture/ListJobs.yaml"

requestCreateJob :: CreateJob -> TestTree
requestCreateJob =
  req
    "CreateJob"
    "fixture/CreateJob.yaml"

requestListClusters :: ListClusters -> TestTree
requestListClusters =
  req
    "ListClusters"
    "fixture/ListClusters.yaml"

requestUpdateCluster :: UpdateCluster -> TestTree
requestUpdateCluster =
  req
    "UpdateCluster"
    "fixture/UpdateCluster.yaml"

requestCreateLongTermPricing :: CreateLongTermPricing -> TestTree
requestCreateLongTermPricing =
  req
    "CreateLongTermPricing"
    "fixture/CreateLongTermPricing.yaml"

requestUpdateLongTermPricing :: UpdateLongTermPricing -> TestTree
requestUpdateLongTermPricing =
  req
    "UpdateLongTermPricing"
    "fixture/UpdateLongTermPricing.yaml"

requestGetSnowballUsage :: GetSnowballUsage -> TestTree
requestGetSnowballUsage =
  req
    "GetSnowballUsage"
    "fixture/GetSnowballUsage.yaml"

requestCreateAddress :: CreateAddress -> TestTree
requestCreateAddress =
  req
    "CreateAddress"
    "fixture/CreateAddress.yaml"

requestCreateReturnShippingLabel :: CreateReturnShippingLabel -> TestTree
requestCreateReturnShippingLabel =
  req
    "CreateReturnShippingLabel"
    "fixture/CreateReturnShippingLabel.yaml"

-- Responses

responseListClusterJobs :: ListClusterJobsResponse -> TestTree
responseListClusterJobs =
  res
    "ListClusterJobsResponse"
    "fixture/ListClusterJobsResponse.proto"
    defaultService
    (Proxy :: Proxy ListClusterJobs)

responseUpdateJobShipmentState :: UpdateJobShipmentStateResponse -> TestTree
responseUpdateJobShipmentState =
  res
    "UpdateJobShipmentStateResponse"
    "fixture/UpdateJobShipmentStateResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateJobShipmentState)

responseCancelJob :: CancelJobResponse -> TestTree
responseCancelJob =
  res
    "CancelJobResponse"
    "fixture/CancelJobResponse.proto"
    defaultService
    (Proxy :: Proxy CancelJob)

responseUpdateJob :: UpdateJobResponse -> TestTree
responseUpdateJob =
  res
    "UpdateJobResponse"
    "fixture/UpdateJobResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateJob)

responseCreateCluster :: CreateClusterResponse -> TestTree
responseCreateCluster =
  res
    "CreateClusterResponse"
    "fixture/CreateClusterResponse.proto"
    defaultService
    (Proxy :: Proxy CreateCluster)

responseListLongTermPricing :: ListLongTermPricingResponse -> TestTree
responseListLongTermPricing =
  res
    "ListLongTermPricingResponse"
    "fixture/ListLongTermPricingResponse.proto"
    defaultService
    (Proxy :: Proxy ListLongTermPricing)

responseGetSoftwareUpdates :: GetSoftwareUpdatesResponse -> TestTree
responseGetSoftwareUpdates =
  res
    "GetSoftwareUpdatesResponse"
    "fixture/GetSoftwareUpdatesResponse.proto"
    defaultService
    (Proxy :: Proxy GetSoftwareUpdates)

responseDescribeReturnShippingLabel :: DescribeReturnShippingLabelResponse -> TestTree
responseDescribeReturnShippingLabel =
  res
    "DescribeReturnShippingLabelResponse"
    "fixture/DescribeReturnShippingLabelResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeReturnShippingLabel)

responseDescribeAddress :: DescribeAddressResponse -> TestTree
responseDescribeAddress =
  res
    "DescribeAddressResponse"
    "fixture/DescribeAddressResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeAddress)

responseListCompatibleImages :: ListCompatibleImagesResponse -> TestTree
responseListCompatibleImages =
  res
    "ListCompatibleImagesResponse"
    "fixture/ListCompatibleImagesResponse.proto"
    defaultService
    (Proxy :: Proxy ListCompatibleImages)

responseDescribeAddresses :: DescribeAddressesResponse -> TestTree
responseDescribeAddresses =
  res
    "DescribeAddressesResponse"
    "fixture/DescribeAddressesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeAddresses)

responseDescribeJob :: DescribeJobResponse -> TestTree
responseDescribeJob =
  res
    "DescribeJobResponse"
    "fixture/DescribeJobResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeJob)

responseDescribeCluster :: DescribeClusterResponse -> TestTree
responseDescribeCluster =
  res
    "DescribeClusterResponse"
    "fixture/DescribeClusterResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeCluster)

responseGetJobUnlockCode :: GetJobUnlockCodeResponse -> TestTree
responseGetJobUnlockCode =
  res
    "GetJobUnlockCodeResponse"
    "fixture/GetJobUnlockCodeResponse.proto"
    defaultService
    (Proxy :: Proxy GetJobUnlockCode)

responseCancelCluster :: CancelClusterResponse -> TestTree
responseCancelCluster =
  res
    "CancelClusterResponse"
    "fixture/CancelClusterResponse.proto"
    defaultService
    (Proxy :: Proxy CancelCluster)

responseGetJobManifest :: GetJobManifestResponse -> TestTree
responseGetJobManifest =
  res
    "GetJobManifestResponse"
    "fixture/GetJobManifestResponse.proto"
    defaultService
    (Proxy :: Proxy GetJobManifest)

responseListJobs :: ListJobsResponse -> TestTree
responseListJobs =
  res
    "ListJobsResponse"
    "fixture/ListJobsResponse.proto"
    defaultService
    (Proxy :: Proxy ListJobs)

responseCreateJob :: CreateJobResponse -> TestTree
responseCreateJob =
  res
    "CreateJobResponse"
    "fixture/CreateJobResponse.proto"
    defaultService
    (Proxy :: Proxy CreateJob)

responseListClusters :: ListClustersResponse -> TestTree
responseListClusters =
  res
    "ListClustersResponse"
    "fixture/ListClustersResponse.proto"
    defaultService
    (Proxy :: Proxy ListClusters)

responseUpdateCluster :: UpdateClusterResponse -> TestTree
responseUpdateCluster =
  res
    "UpdateClusterResponse"
    "fixture/UpdateClusterResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateCluster)

responseCreateLongTermPricing :: CreateLongTermPricingResponse -> TestTree
responseCreateLongTermPricing =
  res
    "CreateLongTermPricingResponse"
    "fixture/CreateLongTermPricingResponse.proto"
    defaultService
    (Proxy :: Proxy CreateLongTermPricing)

responseUpdateLongTermPricing :: UpdateLongTermPricingResponse -> TestTree
responseUpdateLongTermPricing =
  res
    "UpdateLongTermPricingResponse"
    "fixture/UpdateLongTermPricingResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateLongTermPricing)

responseGetSnowballUsage :: GetSnowballUsageResponse -> TestTree
responseGetSnowballUsage =
  res
    "GetSnowballUsageResponse"
    "fixture/GetSnowballUsageResponse.proto"
    defaultService
    (Proxy :: Proxy GetSnowballUsage)

responseCreateAddress :: CreateAddressResponse -> TestTree
responseCreateAddress =
  res
    "CreateAddressResponse"
    "fixture/CreateAddressResponse.proto"
    defaultService
    (Proxy :: Proxy CreateAddress)

responseCreateReturnShippingLabel :: CreateReturnShippingLabelResponse -> TestTree
responseCreateReturnShippingLabel =
  res
    "CreateReturnShippingLabelResponse"
    "fixture/CreateReturnShippingLabelResponse.proto"
    defaultService
    (Proxy :: Proxy CreateReturnShippingLabel)
