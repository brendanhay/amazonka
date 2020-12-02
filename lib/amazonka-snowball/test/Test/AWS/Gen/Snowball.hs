{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.Snowball
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
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
--             cancelCluster
--
--         , requestDescribeCluster $
--             describeCluster
--
--         , requestCreateAddress $
--             createAddress
--
--         , requestGetSnowballUsage $
--             getSnowballUsage
--
--         , requestDescribeAddresses $
--             describeAddresses
--
--         , requestUpdateCluster $
--             updateCluster
--
--         , requestCreateJob $
--             createJob
--
--         , requestGetJobManifest $
--             getJobManifest
--
--         , requestCreateCluster $
--             createCluster
--
--         , requestListJobs $
--             listJobs
--
--         , requestUpdateJob $
--             updateJob
--
--         , requestGetJobUnlockCode $
--             getJobUnlockCode
--
--         , requestListClusterJobs $
--             listClusterJobs
--
--         , requestDescribeJob $
--             describeJob
--
--         , requestListClusters $
--             listClusters
--
--         , requestDescribeAddress $
--             describeAddress
--
--         , requestCancelJob $
--             cancelJob
--
--           ]

--     , testGroup "response"
--         [ responseCancelCluster $
--             cancelClusterResponse
--
--         , responseDescribeCluster $
--             describeClusterResponse
--
--         , responseCreateAddress $
--             createAddressResponse
--
--         , responseGetSnowballUsage $
--             getSnowballUsageResponse
--
--         , responseDescribeAddresses $
--             describeAddressesResponse
--
--         , responseUpdateCluster $
--             updateClusterResponse
--
--         , responseCreateJob $
--             createJobResponse
--
--         , responseGetJobManifest $
--             getJobManifestResponse
--
--         , responseCreateCluster $
--             createClusterResponse
--
--         , responseListJobs $
--             listJobsResponse
--
--         , responseUpdateJob $
--             updateJobResponse
--
--         , responseGetJobUnlockCode $
--             getJobUnlockCodeResponse
--
--         , responseListClusterJobs $
--             listClusterJobsResponse
--
--         , responseDescribeJob $
--             describeJobResponse
--
--         , responseListClusters $
--             listClustersResponse
--
--         , responseDescribeAddress $
--             describeAddressResponse
--
--         , responseCancelJob $
--             cancelJobResponse
--
--           ]
--     ]

-- Requests

requestCancelCluster :: CancelCluster -> TestTree
requestCancelCluster = req
    "CancelCluster"
    "fixture/CancelCluster.yaml"

requestDescribeCluster :: DescribeCluster -> TestTree
requestDescribeCluster = req
    "DescribeCluster"
    "fixture/DescribeCluster.yaml"

requestCreateAddress :: CreateAddress -> TestTree
requestCreateAddress = req
    "CreateAddress"
    "fixture/CreateAddress.yaml"

requestGetSnowballUsage :: GetSnowballUsage -> TestTree
requestGetSnowballUsage = req
    "GetSnowballUsage"
    "fixture/GetSnowballUsage.yaml"

requestDescribeAddresses :: DescribeAddresses -> TestTree
requestDescribeAddresses = req
    "DescribeAddresses"
    "fixture/DescribeAddresses.yaml"

requestUpdateCluster :: UpdateCluster -> TestTree
requestUpdateCluster = req
    "UpdateCluster"
    "fixture/UpdateCluster.yaml"

requestCreateJob :: CreateJob -> TestTree
requestCreateJob = req
    "CreateJob"
    "fixture/CreateJob.yaml"

requestGetJobManifest :: GetJobManifest -> TestTree
requestGetJobManifest = req
    "GetJobManifest"
    "fixture/GetJobManifest.yaml"

requestCreateCluster :: CreateCluster -> TestTree
requestCreateCluster = req
    "CreateCluster"
    "fixture/CreateCluster.yaml"

requestListJobs :: ListJobs -> TestTree
requestListJobs = req
    "ListJobs"
    "fixture/ListJobs.yaml"

requestUpdateJob :: UpdateJob -> TestTree
requestUpdateJob = req
    "UpdateJob"
    "fixture/UpdateJob.yaml"

requestGetJobUnlockCode :: GetJobUnlockCode -> TestTree
requestGetJobUnlockCode = req
    "GetJobUnlockCode"
    "fixture/GetJobUnlockCode.yaml"

requestListClusterJobs :: ListClusterJobs -> TestTree
requestListClusterJobs = req
    "ListClusterJobs"
    "fixture/ListClusterJobs.yaml"

requestDescribeJob :: DescribeJob -> TestTree
requestDescribeJob = req
    "DescribeJob"
    "fixture/DescribeJob.yaml"

requestListClusters :: ListClusters -> TestTree
requestListClusters = req
    "ListClusters"
    "fixture/ListClusters.yaml"

requestDescribeAddress :: DescribeAddress -> TestTree
requestDescribeAddress = req
    "DescribeAddress"
    "fixture/DescribeAddress.yaml"

requestCancelJob :: CancelJob -> TestTree
requestCancelJob = req
    "CancelJob"
    "fixture/CancelJob.yaml"

-- Responses

responseCancelCluster :: CancelClusterResponse -> TestTree
responseCancelCluster = res
    "CancelClusterResponse"
    "fixture/CancelClusterResponse.proto"
    snowball
    (Proxy :: Proxy CancelCluster)

responseDescribeCluster :: DescribeClusterResponse -> TestTree
responseDescribeCluster = res
    "DescribeClusterResponse"
    "fixture/DescribeClusterResponse.proto"
    snowball
    (Proxy :: Proxy DescribeCluster)

responseCreateAddress :: CreateAddressResponse -> TestTree
responseCreateAddress = res
    "CreateAddressResponse"
    "fixture/CreateAddressResponse.proto"
    snowball
    (Proxy :: Proxy CreateAddress)

responseGetSnowballUsage :: GetSnowballUsageResponse -> TestTree
responseGetSnowballUsage = res
    "GetSnowballUsageResponse"
    "fixture/GetSnowballUsageResponse.proto"
    snowball
    (Proxy :: Proxy GetSnowballUsage)

responseDescribeAddresses :: DescribeAddressesResponse -> TestTree
responseDescribeAddresses = res
    "DescribeAddressesResponse"
    "fixture/DescribeAddressesResponse.proto"
    snowball
    (Proxy :: Proxy DescribeAddresses)

responseUpdateCluster :: UpdateClusterResponse -> TestTree
responseUpdateCluster = res
    "UpdateClusterResponse"
    "fixture/UpdateClusterResponse.proto"
    snowball
    (Proxy :: Proxy UpdateCluster)

responseCreateJob :: CreateJobResponse -> TestTree
responseCreateJob = res
    "CreateJobResponse"
    "fixture/CreateJobResponse.proto"
    snowball
    (Proxy :: Proxy CreateJob)

responseGetJobManifest :: GetJobManifestResponse -> TestTree
responseGetJobManifest = res
    "GetJobManifestResponse"
    "fixture/GetJobManifestResponse.proto"
    snowball
    (Proxy :: Proxy GetJobManifest)

responseCreateCluster :: CreateClusterResponse -> TestTree
responseCreateCluster = res
    "CreateClusterResponse"
    "fixture/CreateClusterResponse.proto"
    snowball
    (Proxy :: Proxy CreateCluster)

responseListJobs :: ListJobsResponse -> TestTree
responseListJobs = res
    "ListJobsResponse"
    "fixture/ListJobsResponse.proto"
    snowball
    (Proxy :: Proxy ListJobs)

responseUpdateJob :: UpdateJobResponse -> TestTree
responseUpdateJob = res
    "UpdateJobResponse"
    "fixture/UpdateJobResponse.proto"
    snowball
    (Proxy :: Proxy UpdateJob)

responseGetJobUnlockCode :: GetJobUnlockCodeResponse -> TestTree
responseGetJobUnlockCode = res
    "GetJobUnlockCodeResponse"
    "fixture/GetJobUnlockCodeResponse.proto"
    snowball
    (Proxy :: Proxy GetJobUnlockCode)

responseListClusterJobs :: ListClusterJobsResponse -> TestTree
responseListClusterJobs = res
    "ListClusterJobsResponse"
    "fixture/ListClusterJobsResponse.proto"
    snowball
    (Proxy :: Proxy ListClusterJobs)

responseDescribeJob :: DescribeJobResponse -> TestTree
responseDescribeJob = res
    "DescribeJobResponse"
    "fixture/DescribeJobResponse.proto"
    snowball
    (Proxy :: Proxy DescribeJob)

responseListClusters :: ListClustersResponse -> TestTree
responseListClusters = res
    "ListClustersResponse"
    "fixture/ListClustersResponse.proto"
    snowball
    (Proxy :: Proxy ListClusters)

responseDescribeAddress :: DescribeAddressResponse -> TestTree
responseDescribeAddress = res
    "DescribeAddressResponse"
    "fixture/DescribeAddressResponse.proto"
    snowball
    (Proxy :: Proxy DescribeAddress)

responseCancelJob :: CancelJobResponse -> TestTree
responseCancelJob = res
    "CancelJobResponse"
    "fixture/CancelJobResponse.proto"
    snowball
    (Proxy :: Proxy CancelJob)
