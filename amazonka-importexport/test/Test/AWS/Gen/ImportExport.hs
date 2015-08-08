{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.ImportExport
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Test.AWS.Gen.ImportExport where

import Data.Proxy
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.Tasty
import Network.AWS.ImportExport
import Test.AWS.ImportExport.Internal

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ testGetShippingLabel $
--             getShippingLabel
--
--         , testCreateJob $
--             createJob
--
--         , testListJobs $
--             listJobs
--
--         , testUpdateJob $
--             updateJob
--
--         , testGetStatus $
--             getStatus
--
--         , testCancelJob $
--             cancelJob
--
--           ]

--     , testGroup "response"
--         [ testGetShippingLabelResponse $
--             getShippingLabelResponse
--
--         , testCreateJobResponse $
--             createJobResponse
--
--         , testListJobsResponse $
--             listJobsResponse
--
--         , testUpdateJobResponse $
--             updateJobResponse
--
--         , testGetStatusResponse $
--             getStatusResponse
--
--         , testCancelJobResponse $
--             cancelJobResponse
--
--           ]
--     ]

-- Requests

testGetShippingLabel :: GetShippingLabel -> TestTree
testGetShippingLabel = req
    "GetShippingLabel"
    "fixture/GetShippingLabel"

testCreateJob :: CreateJob -> TestTree
testCreateJob = req
    "CreateJob"
    "fixture/CreateJob"

testListJobs :: ListJobs -> TestTree
testListJobs = req
    "ListJobs"
    "fixture/ListJobs"

testUpdateJob :: UpdateJob -> TestTree
testUpdateJob = req
    "UpdateJob"
    "fixture/UpdateJob"

testGetStatus :: GetStatus -> TestTree
testGetStatus = req
    "GetStatus"
    "fixture/GetStatus"

testCancelJob :: CancelJob -> TestTree
testCancelJob = req
    "CancelJob"
    "fixture/CancelJob"

-- Responses

testGetShippingLabelResponse :: GetShippingLabelResponse -> TestTree
testGetShippingLabelResponse = res
    "GetShippingLabelResponse"
    "fixture/GetShippingLabelResponse"
    (Proxy :: Proxy GetShippingLabel)

testCreateJobResponse :: CreateJobResponse -> TestTree
testCreateJobResponse = res
    "CreateJobResponse"
    "fixture/CreateJobResponse"
    (Proxy :: Proxy CreateJob)

testListJobsResponse :: ListJobsResponse -> TestTree
testListJobsResponse = res
    "ListJobsResponse"
    "fixture/ListJobsResponse"
    (Proxy :: Proxy ListJobs)

testUpdateJobResponse :: UpdateJobResponse -> TestTree
testUpdateJobResponse = res
    "UpdateJobResponse"
    "fixture/UpdateJobResponse"
    (Proxy :: Proxy UpdateJob)

testGetStatusResponse :: GetStatusResponse -> TestTree
testGetStatusResponse = res
    "GetStatusResponse"
    "fixture/GetStatusResponse"
    (Proxy :: Proxy GetStatus)

testCancelJobResponse :: CancelJobResponse -> TestTree
testCancelJobResponse = res
    "CancelJobResponse"
    "fixture/CancelJobResponse"
    (Proxy :: Proxy CancelJob)
