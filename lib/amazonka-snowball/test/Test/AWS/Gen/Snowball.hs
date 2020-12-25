{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.Snowball
-- Copyright   : (c) 2013-2020 Brendan Hay
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
--         [ requestCancelCluster $
--             mkCancelCluster
--
--         , requestDescribeCluster $
--             mkDescribeCluster
--
--         , requestCreateAddress $
--             mkCreateAddress
--
--         , requestCreateReturnShippingLabel $
--             mkCreateReturnShippingLabel
--
--         , requestGetSnowballUsage $
--             mkGetSnowballUsage
--
--         , requestDescribeAddresses $
--             mkDescribeAddresses
--
--         , requestListCompatibleImages $
--             mkListCompatibleImages
--
--         , requestUpdateCluster $
--             mkUpdateCluster
--
--         , requestGetSoftwareUpdates $
--             mkGetSoftwareUpdates
--
--         , requestCreateJob $
--             mkCreateJob
--
--         , requestGetJobManifest $
--             mkGetJobManifest
--
--         , requestCreateCluster $
--             mkCreateCluster
--
--         , requestListJobs $
--             mkListJobs
--
--         , requestUpdateJob $
--             mkUpdateJob
--
--         , requestUpdateJobShipmentState $
--             mkUpdateJobShipmentState
--
--         , requestGetJobUnlockCode $
--             mkGetJobUnlockCode
--
--         , requestListClusterJobs $
--             mkListClusterJobs
--
--         , requestDescribeJob $
--             mkDescribeJob
--
--         , requestListClusters $
--             mkListClusters
--
--         , requestDescribeAddress $
--             mkDescribeAddress
--
--         , requestDescribeReturnShippingLabel $
--             mkDescribeReturnShippingLabel
--
--         , requestCancelJob $
--             mkCancelJob
--
--           ]

--     , testGroup "response"
--         [ responseCancelCluster $
--             mkCancelClusterResponse
--
--         , responseDescribeCluster $
--             mkDescribeClusterResponse
--
--         , responseCreateAddress $
--             mkCreateAddressResponse
--
--         , responseCreateReturnShippingLabel $
--             mkCreateReturnShippingLabelResponse
--
--         , responseGetSnowballUsage $
--             mkGetSnowballUsageResponse
--
--         , responseDescribeAddresses $
--             mkDescribeAddressesResponse
--
--         , responseListCompatibleImages $
--             mkListCompatibleImagesResponse
--
--         , responseUpdateCluster $
--             mkUpdateClusterResponse
--
--         , responseGetSoftwareUpdates $
--             mkGetSoftwareUpdatesResponse
--
--         , responseCreateJob $
--             mkCreateJobResponse
--
--         , responseGetJobManifest $
--             mkGetJobManifestResponse
--
--         , responseCreateCluster $
--             mkCreateClusterResponse
--
--         , responseListJobs $
--             mkListJobsResponse
--
--         , responseUpdateJob $
--             mkUpdateJobResponse
--
--         , responseUpdateJobShipmentState $
--             mkUpdateJobShipmentStateResponse
--
--         , responseGetJobUnlockCode $
--             mkGetJobUnlockCodeResponse
--
--         , responseListClusterJobs $
--             mkListClusterJobsResponse
--
--         , responseDescribeJob $
--             mkDescribeJobResponse
--
--         , responseListClusters $
--             mkListClustersResponse
--
--         , responseDescribeAddress $
--             mkDescribeAddressResponse
--
--         , responseDescribeReturnShippingLabel $
--             mkDescribeReturnShippingLabelResponse
--
--         , responseCancelJob $
--             mkCancelJobResponse
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
    mkServiceConfig
    (Proxy :: Proxy CancelCluster)

responseDescribeCluster :: DescribeClusterResponse -> TestTree
responseDescribeCluster =
  res
    "DescribeClusterResponse"
    "fixture/DescribeClusterResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeCluster)

responseCreateAddress :: CreateAddressResponse -> TestTree
responseCreateAddress =
  res
    "CreateAddressResponse"
    "fixture/CreateAddressResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateAddress)

responseCreateReturnShippingLabel :: CreateReturnShippingLabelResponse -> TestTree
responseCreateReturnShippingLabel =
  res
    "CreateReturnShippingLabelResponse"
    "fixture/CreateReturnShippingLabelResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateReturnShippingLabel)

responseGetSnowballUsage :: GetSnowballUsageResponse -> TestTree
responseGetSnowballUsage =
  res
    "GetSnowballUsageResponse"
    "fixture/GetSnowballUsageResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetSnowballUsage)

responseDescribeAddresses :: DescribeAddressesResponse -> TestTree
responseDescribeAddresses =
  res
    "DescribeAddressesResponse"
    "fixture/DescribeAddressesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeAddresses)

responseListCompatibleImages :: ListCompatibleImagesResponse -> TestTree
responseListCompatibleImages =
  res
    "ListCompatibleImagesResponse"
    "fixture/ListCompatibleImagesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListCompatibleImages)

responseUpdateCluster :: UpdateClusterResponse -> TestTree
responseUpdateCluster =
  res
    "UpdateClusterResponse"
    "fixture/UpdateClusterResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateCluster)

responseGetSoftwareUpdates :: GetSoftwareUpdatesResponse -> TestTree
responseGetSoftwareUpdates =
  res
    "GetSoftwareUpdatesResponse"
    "fixture/GetSoftwareUpdatesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetSoftwareUpdates)

responseCreateJob :: CreateJobResponse -> TestTree
responseCreateJob =
  res
    "CreateJobResponse"
    "fixture/CreateJobResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateJob)

responseGetJobManifest :: GetJobManifestResponse -> TestTree
responseGetJobManifest =
  res
    "GetJobManifestResponse"
    "fixture/GetJobManifestResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetJobManifest)

responseCreateCluster :: CreateClusterResponse -> TestTree
responseCreateCluster =
  res
    "CreateClusterResponse"
    "fixture/CreateClusterResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateCluster)

responseListJobs :: ListJobsResponse -> TestTree
responseListJobs =
  res
    "ListJobsResponse"
    "fixture/ListJobsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListJobs)

responseUpdateJob :: UpdateJobResponse -> TestTree
responseUpdateJob =
  res
    "UpdateJobResponse"
    "fixture/UpdateJobResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateJob)

responseUpdateJobShipmentState :: UpdateJobShipmentStateResponse -> TestTree
responseUpdateJobShipmentState =
  res
    "UpdateJobShipmentStateResponse"
    "fixture/UpdateJobShipmentStateResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateJobShipmentState)

responseGetJobUnlockCode :: GetJobUnlockCodeResponse -> TestTree
responseGetJobUnlockCode =
  res
    "GetJobUnlockCodeResponse"
    "fixture/GetJobUnlockCodeResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetJobUnlockCode)

responseListClusterJobs :: ListClusterJobsResponse -> TestTree
responseListClusterJobs =
  res
    "ListClusterJobsResponse"
    "fixture/ListClusterJobsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListClusterJobs)

responseDescribeJob :: DescribeJobResponse -> TestTree
responseDescribeJob =
  res
    "DescribeJobResponse"
    "fixture/DescribeJobResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeJob)

responseListClusters :: ListClustersResponse -> TestTree
responseListClusters =
  res
    "ListClustersResponse"
    "fixture/ListClustersResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListClusters)

responseDescribeAddress :: DescribeAddressResponse -> TestTree
responseDescribeAddress =
  res
    "DescribeAddressResponse"
    "fixture/DescribeAddressResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeAddress)

responseDescribeReturnShippingLabel :: DescribeReturnShippingLabelResponse -> TestTree
responseDescribeReturnShippingLabel =
  res
    "DescribeReturnShippingLabelResponse"
    "fixture/DescribeReturnShippingLabelResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeReturnShippingLabel)

responseCancelJob :: CancelJobResponse -> TestTree
responseCancelJob =
  res
    "CancelJobResponse"
    "fixture/CancelJobResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CancelJob)
