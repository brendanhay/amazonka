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

import Amazonka.Snowball
import qualified Data.Proxy as Proxy
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
--         [ requestCancelCluster $
--             newCancelCluster
--
--         , requestDescribeCluster $
--             newDescribeCluster
--
--         , requestCreateAddress $
--             newCreateAddress
--
--         , requestCreateReturnShippingLabel $
--             newCreateReturnShippingLabel
--
--         , requestGetSnowballUsage $
--             newGetSnowballUsage
--
--         , requestDescribeAddresses $
--             newDescribeAddresses
--
--         , requestListCompatibleImages $
--             newListCompatibleImages
--
--         , requestCreateLongTermPricing $
--             newCreateLongTermPricing
--
--         , requestUpdateCluster $
--             newUpdateCluster
--
--         , requestGetSoftwareUpdates $
--             newGetSoftwareUpdates
--
--         , requestCreateJob $
--             newCreateJob
--
--         , requestListLongTermPricing $
--             newListLongTermPricing
--
--         , requestGetJobManifest $
--             newGetJobManifest
--
--         , requestCreateCluster $
--             newCreateCluster
--
--         , requestListJobs $
--             newListJobs
--
--         , requestUpdateJob $
--             newUpdateJob
--
--         , requestUpdateJobShipmentState $
--             newUpdateJobShipmentState
--
--         , requestGetJobUnlockCode $
--             newGetJobUnlockCode
--
--         , requestListClusterJobs $
--             newListClusterJobs
--
--         , requestDescribeJob $
--             newDescribeJob
--
--         , requestUpdateLongTermPricing $
--             newUpdateLongTermPricing
--
--         , requestListClusters $
--             newListClusters
--
--         , requestDescribeAddress $
--             newDescribeAddress
--
--         , requestDescribeReturnShippingLabel $
--             newDescribeReturnShippingLabel
--
--         , requestCancelJob $
--             newCancelJob
--
--           ]

--     , testGroup "response"
--         [ responseCancelCluster $
--             newCancelClusterResponse
--
--         , responseDescribeCluster $
--             newDescribeClusterResponse
--
--         , responseCreateAddress $
--             newCreateAddressResponse
--
--         , responseCreateReturnShippingLabel $
--             newCreateReturnShippingLabelResponse
--
--         , responseGetSnowballUsage $
--             newGetSnowballUsageResponse
--
--         , responseDescribeAddresses $
--             newDescribeAddressesResponse
--
--         , responseListCompatibleImages $
--             newListCompatibleImagesResponse
--
--         , responseCreateLongTermPricing $
--             newCreateLongTermPricingResponse
--
--         , responseUpdateCluster $
--             newUpdateClusterResponse
--
--         , responseGetSoftwareUpdates $
--             newGetSoftwareUpdatesResponse
--
--         , responseCreateJob $
--             newCreateJobResponse
--
--         , responseListLongTermPricing $
--             newListLongTermPricingResponse
--
--         , responseGetJobManifest $
--             newGetJobManifestResponse
--
--         , responseCreateCluster $
--             newCreateClusterResponse
--
--         , responseListJobs $
--             newListJobsResponse
--
--         , responseUpdateJob $
--             newUpdateJobResponse
--
--         , responseUpdateJobShipmentState $
--             newUpdateJobShipmentStateResponse
--
--         , responseGetJobUnlockCode $
--             newGetJobUnlockCodeResponse
--
--         , responseListClusterJobs $
--             newListClusterJobsResponse
--
--         , responseDescribeJob $
--             newDescribeJobResponse
--
--         , responseUpdateLongTermPricing $
--             newUpdateLongTermPricingResponse
--
--         , responseListClusters $
--             newListClustersResponse
--
--         , responseDescribeAddress $
--             newDescribeAddressResponse
--
--         , responseDescribeReturnShippingLabel $
--             newDescribeReturnShippingLabelResponse
--
--         , responseCancelJob $
--             newCancelJobResponse
--
--           ]
--     ]

-- Requests

requestCancelCluster :: CancelCluster -> TestTree
requestCancelCluster =
  req
    "CancelCluster"
    "fixture/CancelCluster.yaml"

requestDescribeCluster :: DescribeCluster -> TestTree
requestDescribeCluster =
  req
    "DescribeCluster"
    "fixture/DescribeCluster.yaml"

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

requestGetSnowballUsage :: GetSnowballUsage -> TestTree
requestGetSnowballUsage =
  req
    "GetSnowballUsage"
    "fixture/GetSnowballUsage.yaml"

requestDescribeAddresses :: DescribeAddresses -> TestTree
requestDescribeAddresses =
  req
    "DescribeAddresses"
    "fixture/DescribeAddresses.yaml"

requestListCompatibleImages :: ListCompatibleImages -> TestTree
requestListCompatibleImages =
  req
    "ListCompatibleImages"
    "fixture/ListCompatibleImages.yaml"

requestCreateLongTermPricing :: CreateLongTermPricing -> TestTree
requestCreateLongTermPricing =
  req
    "CreateLongTermPricing"
    "fixture/CreateLongTermPricing.yaml"

requestUpdateCluster :: UpdateCluster -> TestTree
requestUpdateCluster =
  req
    "UpdateCluster"
    "fixture/UpdateCluster.yaml"

requestGetSoftwareUpdates :: GetSoftwareUpdates -> TestTree
requestGetSoftwareUpdates =
  req
    "GetSoftwareUpdates"
    "fixture/GetSoftwareUpdates.yaml"

requestCreateJob :: CreateJob -> TestTree
requestCreateJob =
  req
    "CreateJob"
    "fixture/CreateJob.yaml"

requestListLongTermPricing :: ListLongTermPricing -> TestTree
requestListLongTermPricing =
  req
    "ListLongTermPricing"
    "fixture/ListLongTermPricing.yaml"

requestGetJobManifest :: GetJobManifest -> TestTree
requestGetJobManifest =
  req
    "GetJobManifest"
    "fixture/GetJobManifest.yaml"

requestCreateCluster :: CreateCluster -> TestTree
requestCreateCluster =
  req
    "CreateCluster"
    "fixture/CreateCluster.yaml"

requestListJobs :: ListJobs -> TestTree
requestListJobs =
  req
    "ListJobs"
    "fixture/ListJobs.yaml"

requestUpdateJob :: UpdateJob -> TestTree
requestUpdateJob =
  req
    "UpdateJob"
    "fixture/UpdateJob.yaml"

requestUpdateJobShipmentState :: UpdateJobShipmentState -> TestTree
requestUpdateJobShipmentState =
  req
    "UpdateJobShipmentState"
    "fixture/UpdateJobShipmentState.yaml"

requestGetJobUnlockCode :: GetJobUnlockCode -> TestTree
requestGetJobUnlockCode =
  req
    "GetJobUnlockCode"
    "fixture/GetJobUnlockCode.yaml"

requestListClusterJobs :: ListClusterJobs -> TestTree
requestListClusterJobs =
  req
    "ListClusterJobs"
    "fixture/ListClusterJobs.yaml"

requestDescribeJob :: DescribeJob -> TestTree
requestDescribeJob =
  req
    "DescribeJob"
    "fixture/DescribeJob.yaml"

requestUpdateLongTermPricing :: UpdateLongTermPricing -> TestTree
requestUpdateLongTermPricing =
  req
    "UpdateLongTermPricing"
    "fixture/UpdateLongTermPricing.yaml"

requestListClusters :: ListClusters -> TestTree
requestListClusters =
  req
    "ListClusters"
    "fixture/ListClusters.yaml"

requestDescribeAddress :: DescribeAddress -> TestTree
requestDescribeAddress =
  req
    "DescribeAddress"
    "fixture/DescribeAddress.yaml"

requestDescribeReturnShippingLabel :: DescribeReturnShippingLabel -> TestTree
requestDescribeReturnShippingLabel =
  req
    "DescribeReturnShippingLabel"
    "fixture/DescribeReturnShippingLabel.yaml"

requestCancelJob :: CancelJob -> TestTree
requestCancelJob =
  req
    "CancelJob"
    "fixture/CancelJob.yaml"

-- Responses

responseCancelCluster :: CancelClusterResponse -> TestTree
responseCancelCluster =
  res
    "CancelClusterResponse"
    "fixture/CancelClusterResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CancelCluster)

responseDescribeCluster :: DescribeClusterResponse -> TestTree
responseDescribeCluster =
  res
    "DescribeClusterResponse"
    "fixture/DescribeClusterResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeCluster)

responseCreateAddress :: CreateAddressResponse -> TestTree
responseCreateAddress =
  res
    "CreateAddressResponse"
    "fixture/CreateAddressResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateAddress)

responseCreateReturnShippingLabel :: CreateReturnShippingLabelResponse -> TestTree
responseCreateReturnShippingLabel =
  res
    "CreateReturnShippingLabelResponse"
    "fixture/CreateReturnShippingLabelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateReturnShippingLabel)

responseGetSnowballUsage :: GetSnowballUsageResponse -> TestTree
responseGetSnowballUsage =
  res
    "GetSnowballUsageResponse"
    "fixture/GetSnowballUsageResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetSnowballUsage)

responseDescribeAddresses :: DescribeAddressesResponse -> TestTree
responseDescribeAddresses =
  res
    "DescribeAddressesResponse"
    "fixture/DescribeAddressesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeAddresses)

responseListCompatibleImages :: ListCompatibleImagesResponse -> TestTree
responseListCompatibleImages =
  res
    "ListCompatibleImagesResponse"
    "fixture/ListCompatibleImagesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListCompatibleImages)

responseCreateLongTermPricing :: CreateLongTermPricingResponse -> TestTree
responseCreateLongTermPricing =
  res
    "CreateLongTermPricingResponse"
    "fixture/CreateLongTermPricingResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateLongTermPricing)

responseUpdateCluster :: UpdateClusterResponse -> TestTree
responseUpdateCluster =
  res
    "UpdateClusterResponse"
    "fixture/UpdateClusterResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateCluster)

responseGetSoftwareUpdates :: GetSoftwareUpdatesResponse -> TestTree
responseGetSoftwareUpdates =
  res
    "GetSoftwareUpdatesResponse"
    "fixture/GetSoftwareUpdatesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetSoftwareUpdates)

responseCreateJob :: CreateJobResponse -> TestTree
responseCreateJob =
  res
    "CreateJobResponse"
    "fixture/CreateJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateJob)

responseListLongTermPricing :: ListLongTermPricingResponse -> TestTree
responseListLongTermPricing =
  res
    "ListLongTermPricingResponse"
    "fixture/ListLongTermPricingResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListLongTermPricing)

responseGetJobManifest :: GetJobManifestResponse -> TestTree
responseGetJobManifest =
  res
    "GetJobManifestResponse"
    "fixture/GetJobManifestResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetJobManifest)

responseCreateCluster :: CreateClusterResponse -> TestTree
responseCreateCluster =
  res
    "CreateClusterResponse"
    "fixture/CreateClusterResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateCluster)

responseListJobs :: ListJobsResponse -> TestTree
responseListJobs =
  res
    "ListJobsResponse"
    "fixture/ListJobsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListJobs)

responseUpdateJob :: UpdateJobResponse -> TestTree
responseUpdateJob =
  res
    "UpdateJobResponse"
    "fixture/UpdateJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateJob)

responseUpdateJobShipmentState :: UpdateJobShipmentStateResponse -> TestTree
responseUpdateJobShipmentState =
  res
    "UpdateJobShipmentStateResponse"
    "fixture/UpdateJobShipmentStateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateJobShipmentState)

responseGetJobUnlockCode :: GetJobUnlockCodeResponse -> TestTree
responseGetJobUnlockCode =
  res
    "GetJobUnlockCodeResponse"
    "fixture/GetJobUnlockCodeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetJobUnlockCode)

responseListClusterJobs :: ListClusterJobsResponse -> TestTree
responseListClusterJobs =
  res
    "ListClusterJobsResponse"
    "fixture/ListClusterJobsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListClusterJobs)

responseDescribeJob :: DescribeJobResponse -> TestTree
responseDescribeJob =
  res
    "DescribeJobResponse"
    "fixture/DescribeJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeJob)

responseUpdateLongTermPricing :: UpdateLongTermPricingResponse -> TestTree
responseUpdateLongTermPricing =
  res
    "UpdateLongTermPricingResponse"
    "fixture/UpdateLongTermPricingResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateLongTermPricing)

responseListClusters :: ListClustersResponse -> TestTree
responseListClusters =
  res
    "ListClustersResponse"
    "fixture/ListClustersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListClusters)

responseDescribeAddress :: DescribeAddressResponse -> TestTree
responseDescribeAddress =
  res
    "DescribeAddressResponse"
    "fixture/DescribeAddressResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeAddress)

responseDescribeReturnShippingLabel :: DescribeReturnShippingLabelResponse -> TestTree
responseDescribeReturnShippingLabel =
  res
    "DescribeReturnShippingLabelResponse"
    "fixture/DescribeReturnShippingLabelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeReturnShippingLabel)

responseCancelJob :: CancelJobResponse -> TestTree
responseCancelJob =
  res
    "CancelJobResponse"
    "fixture/CancelJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CancelJob)
