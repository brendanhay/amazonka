{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.Snowball
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.Snowball where

import Amazonka.Snowball
import qualified Data.Proxy as Proxy
import Test.Amazonka.Fixture
import Test.Amazonka.Prelude
import Test.Amazonka.Snowball.Internal
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
--         , requestCancelJob $
--             newCancelJob
--
--         , requestCreateAddress $
--             newCreateAddress
--
--         , requestCreateCluster $
--             newCreateCluster
--
--         , requestCreateJob $
--             newCreateJob
--
--         , requestCreateLongTermPricing $
--             newCreateLongTermPricing
--
--         , requestCreateReturnShippingLabel $
--             newCreateReturnShippingLabel
--
--         , requestDescribeAddress $
--             newDescribeAddress
--
--         , requestDescribeAddresses $
--             newDescribeAddresses
--
--         , requestDescribeCluster $
--             newDescribeCluster
--
--         , requestDescribeJob $
--             newDescribeJob
--
--         , requestDescribeReturnShippingLabel $
--             newDescribeReturnShippingLabel
--
--         , requestGetJobManifest $
--             newGetJobManifest
--
--         , requestGetJobUnlockCode $
--             newGetJobUnlockCode
--
--         , requestGetSnowballUsage $
--             newGetSnowballUsage
--
--         , requestGetSoftwareUpdates $
--             newGetSoftwareUpdates
--
--         , requestListClusterJobs $
--             newListClusterJobs
--
--         , requestListClusters $
--             newListClusters
--
--         , requestListCompatibleImages $
--             newListCompatibleImages
--
--         , requestListJobs $
--             newListJobs
--
--         , requestListLongTermPricing $
--             newListLongTermPricing
--
--         , requestListServiceVersions $
--             newListServiceVersions
--
--         , requestUpdateCluster $
--             newUpdateCluster
--
--         , requestUpdateJob $
--             newUpdateJob
--
--         , requestUpdateJobShipmentState $
--             newUpdateJobShipmentState
--
--         , requestUpdateLongTermPricing $
--             newUpdateLongTermPricing
--
--           ]

--     , testGroup "response"
--         [ responseCancelCluster $
--             newCancelClusterResponse
--
--         , responseCancelJob $
--             newCancelJobResponse
--
--         , responseCreateAddress $
--             newCreateAddressResponse
--
--         , responseCreateCluster $
--             newCreateClusterResponse
--
--         , responseCreateJob $
--             newCreateJobResponse
--
--         , responseCreateLongTermPricing $
--             newCreateLongTermPricingResponse
--
--         , responseCreateReturnShippingLabel $
--             newCreateReturnShippingLabelResponse
--
--         , responseDescribeAddress $
--             newDescribeAddressResponse
--
--         , responseDescribeAddresses $
--             newDescribeAddressesResponse
--
--         , responseDescribeCluster $
--             newDescribeClusterResponse
--
--         , responseDescribeJob $
--             newDescribeJobResponse
--
--         , responseDescribeReturnShippingLabel $
--             newDescribeReturnShippingLabelResponse
--
--         , responseGetJobManifest $
--             newGetJobManifestResponse
--
--         , responseGetJobUnlockCode $
--             newGetJobUnlockCodeResponse
--
--         , responseGetSnowballUsage $
--             newGetSnowballUsageResponse
--
--         , responseGetSoftwareUpdates $
--             newGetSoftwareUpdatesResponse
--
--         , responseListClusterJobs $
--             newListClusterJobsResponse
--
--         , responseListClusters $
--             newListClustersResponse
--
--         , responseListCompatibleImages $
--             newListCompatibleImagesResponse
--
--         , responseListJobs $
--             newListJobsResponse
--
--         , responseListLongTermPricing $
--             newListLongTermPricingResponse
--
--         , responseListServiceVersions $
--             newListServiceVersionsResponse
--
--         , responseUpdateCluster $
--             newUpdateClusterResponse
--
--         , responseUpdateJob $
--             newUpdateJobResponse
--
--         , responseUpdateJobShipmentState $
--             newUpdateJobShipmentStateResponse
--
--         , responseUpdateLongTermPricing $
--             newUpdateLongTermPricingResponse
--
--           ]
--     ]

-- Requests

requestCancelCluster :: CancelCluster -> TestTree
requestCancelCluster =
  req
    "CancelCluster"
    "fixture/CancelCluster.yaml"

requestCancelJob :: CancelJob -> TestTree
requestCancelJob =
  req
    "CancelJob"
    "fixture/CancelJob.yaml"

requestCreateAddress :: CreateAddress -> TestTree
requestCreateAddress =
  req
    "CreateAddress"
    "fixture/CreateAddress.yaml"

requestCreateCluster :: CreateCluster -> TestTree
requestCreateCluster =
  req
    "CreateCluster"
    "fixture/CreateCluster.yaml"

requestCreateJob :: CreateJob -> TestTree
requestCreateJob =
  req
    "CreateJob"
    "fixture/CreateJob.yaml"

requestCreateLongTermPricing :: CreateLongTermPricing -> TestTree
requestCreateLongTermPricing =
  req
    "CreateLongTermPricing"
    "fixture/CreateLongTermPricing.yaml"

requestCreateReturnShippingLabel :: CreateReturnShippingLabel -> TestTree
requestCreateReturnShippingLabel =
  req
    "CreateReturnShippingLabel"
    "fixture/CreateReturnShippingLabel.yaml"

requestDescribeAddress :: DescribeAddress -> TestTree
requestDescribeAddress =
  req
    "DescribeAddress"
    "fixture/DescribeAddress.yaml"

requestDescribeAddresses :: DescribeAddresses -> TestTree
requestDescribeAddresses =
  req
    "DescribeAddresses"
    "fixture/DescribeAddresses.yaml"

requestDescribeCluster :: DescribeCluster -> TestTree
requestDescribeCluster =
  req
    "DescribeCluster"
    "fixture/DescribeCluster.yaml"

requestDescribeJob :: DescribeJob -> TestTree
requestDescribeJob =
  req
    "DescribeJob"
    "fixture/DescribeJob.yaml"

requestDescribeReturnShippingLabel :: DescribeReturnShippingLabel -> TestTree
requestDescribeReturnShippingLabel =
  req
    "DescribeReturnShippingLabel"
    "fixture/DescribeReturnShippingLabel.yaml"

requestGetJobManifest :: GetJobManifest -> TestTree
requestGetJobManifest =
  req
    "GetJobManifest"
    "fixture/GetJobManifest.yaml"

requestGetJobUnlockCode :: GetJobUnlockCode -> TestTree
requestGetJobUnlockCode =
  req
    "GetJobUnlockCode"
    "fixture/GetJobUnlockCode.yaml"

requestGetSnowballUsage :: GetSnowballUsage -> TestTree
requestGetSnowballUsage =
  req
    "GetSnowballUsage"
    "fixture/GetSnowballUsage.yaml"

requestGetSoftwareUpdates :: GetSoftwareUpdates -> TestTree
requestGetSoftwareUpdates =
  req
    "GetSoftwareUpdates"
    "fixture/GetSoftwareUpdates.yaml"

requestListClusterJobs :: ListClusterJobs -> TestTree
requestListClusterJobs =
  req
    "ListClusterJobs"
    "fixture/ListClusterJobs.yaml"

requestListClusters :: ListClusters -> TestTree
requestListClusters =
  req
    "ListClusters"
    "fixture/ListClusters.yaml"

requestListCompatibleImages :: ListCompatibleImages -> TestTree
requestListCompatibleImages =
  req
    "ListCompatibleImages"
    "fixture/ListCompatibleImages.yaml"

requestListJobs :: ListJobs -> TestTree
requestListJobs =
  req
    "ListJobs"
    "fixture/ListJobs.yaml"

requestListLongTermPricing :: ListLongTermPricing -> TestTree
requestListLongTermPricing =
  req
    "ListLongTermPricing"
    "fixture/ListLongTermPricing.yaml"

requestListServiceVersions :: ListServiceVersions -> TestTree
requestListServiceVersions =
  req
    "ListServiceVersions"
    "fixture/ListServiceVersions.yaml"

requestUpdateCluster :: UpdateCluster -> TestTree
requestUpdateCluster =
  req
    "UpdateCluster"
    "fixture/UpdateCluster.yaml"

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

requestUpdateLongTermPricing :: UpdateLongTermPricing -> TestTree
requestUpdateLongTermPricing =
  req
    "UpdateLongTermPricing"
    "fixture/UpdateLongTermPricing.yaml"

-- Responses

responseCancelCluster :: CancelClusterResponse -> TestTree
responseCancelCluster =
  res
    "CancelClusterResponse"
    "fixture/CancelClusterResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CancelCluster)

responseCancelJob :: CancelJobResponse -> TestTree
responseCancelJob =
  res
    "CancelJobResponse"
    "fixture/CancelJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CancelJob)

responseCreateAddress :: CreateAddressResponse -> TestTree
responseCreateAddress =
  res
    "CreateAddressResponse"
    "fixture/CreateAddressResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateAddress)

responseCreateCluster :: CreateClusterResponse -> TestTree
responseCreateCluster =
  res
    "CreateClusterResponse"
    "fixture/CreateClusterResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateCluster)

responseCreateJob :: CreateJobResponse -> TestTree
responseCreateJob =
  res
    "CreateJobResponse"
    "fixture/CreateJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateJob)

responseCreateLongTermPricing :: CreateLongTermPricingResponse -> TestTree
responseCreateLongTermPricing =
  res
    "CreateLongTermPricingResponse"
    "fixture/CreateLongTermPricingResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateLongTermPricing)

responseCreateReturnShippingLabel :: CreateReturnShippingLabelResponse -> TestTree
responseCreateReturnShippingLabel =
  res
    "CreateReturnShippingLabelResponse"
    "fixture/CreateReturnShippingLabelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateReturnShippingLabel)

responseDescribeAddress :: DescribeAddressResponse -> TestTree
responseDescribeAddress =
  res
    "DescribeAddressResponse"
    "fixture/DescribeAddressResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeAddress)

responseDescribeAddresses :: DescribeAddressesResponse -> TestTree
responseDescribeAddresses =
  res
    "DescribeAddressesResponse"
    "fixture/DescribeAddressesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeAddresses)

responseDescribeCluster :: DescribeClusterResponse -> TestTree
responseDescribeCluster =
  res
    "DescribeClusterResponse"
    "fixture/DescribeClusterResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeCluster)

responseDescribeJob :: DescribeJobResponse -> TestTree
responseDescribeJob =
  res
    "DescribeJobResponse"
    "fixture/DescribeJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeJob)

responseDescribeReturnShippingLabel :: DescribeReturnShippingLabelResponse -> TestTree
responseDescribeReturnShippingLabel =
  res
    "DescribeReturnShippingLabelResponse"
    "fixture/DescribeReturnShippingLabelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeReturnShippingLabel)

responseGetJobManifest :: GetJobManifestResponse -> TestTree
responseGetJobManifest =
  res
    "GetJobManifestResponse"
    "fixture/GetJobManifestResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetJobManifest)

responseGetJobUnlockCode :: GetJobUnlockCodeResponse -> TestTree
responseGetJobUnlockCode =
  res
    "GetJobUnlockCodeResponse"
    "fixture/GetJobUnlockCodeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetJobUnlockCode)

responseGetSnowballUsage :: GetSnowballUsageResponse -> TestTree
responseGetSnowballUsage =
  res
    "GetSnowballUsageResponse"
    "fixture/GetSnowballUsageResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetSnowballUsage)

responseGetSoftwareUpdates :: GetSoftwareUpdatesResponse -> TestTree
responseGetSoftwareUpdates =
  res
    "GetSoftwareUpdatesResponse"
    "fixture/GetSoftwareUpdatesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetSoftwareUpdates)

responseListClusterJobs :: ListClusterJobsResponse -> TestTree
responseListClusterJobs =
  res
    "ListClusterJobsResponse"
    "fixture/ListClusterJobsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListClusterJobs)

responseListClusters :: ListClustersResponse -> TestTree
responseListClusters =
  res
    "ListClustersResponse"
    "fixture/ListClustersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListClusters)

responseListCompatibleImages :: ListCompatibleImagesResponse -> TestTree
responseListCompatibleImages =
  res
    "ListCompatibleImagesResponse"
    "fixture/ListCompatibleImagesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListCompatibleImages)

responseListJobs :: ListJobsResponse -> TestTree
responseListJobs =
  res
    "ListJobsResponse"
    "fixture/ListJobsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListJobs)

responseListLongTermPricing :: ListLongTermPricingResponse -> TestTree
responseListLongTermPricing =
  res
    "ListLongTermPricingResponse"
    "fixture/ListLongTermPricingResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListLongTermPricing)

responseListServiceVersions :: ListServiceVersionsResponse -> TestTree
responseListServiceVersions =
  res
    "ListServiceVersionsResponse"
    "fixture/ListServiceVersionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListServiceVersions)

responseUpdateCluster :: UpdateClusterResponse -> TestTree
responseUpdateCluster =
  res
    "UpdateClusterResponse"
    "fixture/UpdateClusterResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateCluster)

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

responseUpdateLongTermPricing :: UpdateLongTermPricingResponse -> TestTree
responseUpdateLongTermPricing =
  res
    "UpdateLongTermPricingResponse"
    "fixture/UpdateLongTermPricingResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateLongTermPricing)
