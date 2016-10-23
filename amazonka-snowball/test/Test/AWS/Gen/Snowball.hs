{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.Snowball
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Test.AWS.Gen.Snowball where

import Data.Proxy
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.Tasty
import Network.AWS.Snowball
import Test.AWS.Snowball.Internal

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestCreateAddress $
--             createAddress
--
--         , requestGetSnowballUsage $
--             getSnowballUsage
--
--         , requestDescribeAddresses $
--             describeAddresses
--
--         , requestCreateJob $
--             createJob
--
--         , requestGetJobManifest $
--             getJobManifest
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
--         , requestDescribeJob $
--             describeJob
--
--         , requestDescribeAddress $
--             describeAddress
--
--         , requestCancelJob $
--             cancelJob
--
--           ]

--     , testGroup "response"
--         [ responseCreateAddress $
--             createAddressResponse
--
--         , responseGetSnowballUsage $
--             getSnowballUsageResponse
--
--         , responseDescribeAddresses $
--             describeAddressesResponse
--
--         , responseCreateJob $
--             createJobResponse
--
--         , responseGetJobManifest $
--             getJobManifestResponse
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
--         , responseDescribeJob $
--             describeJobResponse
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

requestCreateJob :: CreateJob -> TestTree
requestCreateJob = req
    "CreateJob"
    "fixture/CreateJob.yaml"

requestGetJobManifest :: GetJobManifest -> TestTree
requestGetJobManifest = req
    "GetJobManifest"
    "fixture/GetJobManifest.yaml"

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

requestDescribeJob :: DescribeJob -> TestTree
requestDescribeJob = req
    "DescribeJob"
    "fixture/DescribeJob.yaml"

requestDescribeAddress :: DescribeAddress -> TestTree
requestDescribeAddress = req
    "DescribeAddress"
    "fixture/DescribeAddress.yaml"

requestCancelJob :: CancelJob -> TestTree
requestCancelJob = req
    "CancelJob"
    "fixture/CancelJob.yaml"

-- Responses

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

responseDescribeJob :: DescribeJobResponse -> TestTree
responseDescribeJob = res
    "DescribeJobResponse"
    "fixture/DescribeJobResponse.proto"
    snowball
    (Proxy :: Proxy DescribeJob)

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
