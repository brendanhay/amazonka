{-# OPTIONS_GHC -fno-warn-orphans #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.ImportExport
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
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
testGetShippingLabel = undefined

testCreateJob :: CreateJob -> TestTree
testCreateJob = undefined

testListJobs :: ListJobs -> TestTree
testListJobs = undefined

testUpdateJob :: UpdateJob -> TestTree
testUpdateJob = undefined

testGetStatus :: GetStatus -> TestTree
testGetStatus = undefined

testCancelJob :: CancelJob -> TestTree
testCancelJob = undefined

-- Responses

testGetShippingLabelResponse :: GetShippingLabelResponse -> TestTree
testGetShippingLabelResponse = resp
    "GetShippingLabelResponse"
    "fixture/GetShippingLabelResponse"
    (Proxy :: Proxy GetShippingLabel)

testCreateJobResponse :: CreateJobResponse -> TestTree
testCreateJobResponse = resp
    "CreateJobResponse"
    "fixture/CreateJobResponse"
    (Proxy :: Proxy CreateJob)

testListJobsResponse :: ListJobsResponse -> TestTree
testListJobsResponse = resp
    "ListJobsResponse"
    "fixture/ListJobsResponse"
    (Proxy :: Proxy ListJobs)

testUpdateJobResponse :: UpdateJobResponse -> TestTree
testUpdateJobResponse = resp
    "UpdateJobResponse"
    "fixture/UpdateJobResponse"
    (Proxy :: Proxy UpdateJob)

testGetStatusResponse :: GetStatusResponse -> TestTree
testGetStatusResponse = resp
    "GetStatusResponse"
    "fixture/GetStatusResponse"
    (Proxy :: Proxy GetStatus)

testCancelJobResponse :: CancelJobResponse -> TestTree
testCancelJobResponse = resp
    "CancelJobResponse"
    "fixture/CancelJobResponse"
    (Proxy :: Proxy CancelJob)

instance Out Artifact
instance Out CancelJob
instance Out CancelJobResponse
instance Out CreateJob
instance Out CreateJobResponse
instance Out GetShippingLabel
instance Out GetShippingLabelResponse
instance Out GetStatus
instance Out GetStatusResponse
instance Out Job
instance Out JobType
instance Out ListJobs
instance Out ListJobsResponse
instance Out UpdateJob
instance Out UpdateJobResponse
