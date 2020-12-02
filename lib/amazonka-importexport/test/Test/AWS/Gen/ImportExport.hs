{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.ImportExport
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Test.AWS.Gen.ImportExport where

import Data.Proxy
import Network.AWS.ImportExport
import Test.AWS.Fixture
import Test.AWS.ImportExport.Internal
import Test.AWS.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestGetShippingLabel $
--             getShippingLabel
--
--         , requestCreateJob $
--             createJob
--
--         , requestListJobs $
--             listJobs
--
--         , requestUpdateJob $
--             updateJob
--
--         , requestGetStatus $
--             getStatus
--
--         , requestCancelJob $
--             cancelJob
--
--           ]

--     , testGroup "response"
--         [ responseGetShippingLabel $
--             getShippingLabelResponse
--
--         , responseCreateJob $
--             createJobResponse
--
--         , responseListJobs $
--             listJobsResponse
--
--         , responseUpdateJob $
--             updateJobResponse
--
--         , responseGetStatus $
--             getStatusResponse
--
--         , responseCancelJob $
--             cancelJobResponse
--
--           ]
--     ]

-- Requests

requestGetShippingLabel :: GetShippingLabel -> TestTree
requestGetShippingLabel = req
    "GetShippingLabel"
    "fixture/GetShippingLabel.yaml"

requestCreateJob :: CreateJob -> TestTree
requestCreateJob = req
    "CreateJob"
    "fixture/CreateJob.yaml"

requestListJobs :: ListJobs -> TestTree
requestListJobs = req
    "ListJobs"
    "fixture/ListJobs.yaml"

requestUpdateJob :: UpdateJob -> TestTree
requestUpdateJob = req
    "UpdateJob"
    "fixture/UpdateJob.yaml"

requestGetStatus :: GetStatus -> TestTree
requestGetStatus = req
    "GetStatus"
    "fixture/GetStatus.yaml"

requestCancelJob :: CancelJob -> TestTree
requestCancelJob = req
    "CancelJob"
    "fixture/CancelJob.yaml"

-- Responses

responseGetShippingLabel :: GetShippingLabelResponse -> TestTree
responseGetShippingLabel = res
    "GetShippingLabelResponse"
    "fixture/GetShippingLabelResponse.proto"
    importExport
    (Proxy :: Proxy GetShippingLabel)

responseCreateJob :: CreateJobResponse -> TestTree
responseCreateJob = res
    "CreateJobResponse"
    "fixture/CreateJobResponse.proto"
    importExport
    (Proxy :: Proxy CreateJob)

responseListJobs :: ListJobsResponse -> TestTree
responseListJobs = res
    "ListJobsResponse"
    "fixture/ListJobsResponse.proto"
    importExport
    (Proxy :: Proxy ListJobs)

responseUpdateJob :: UpdateJobResponse -> TestTree
responseUpdateJob = res
    "UpdateJobResponse"
    "fixture/UpdateJobResponse.proto"
    importExport
    (Proxy :: Proxy UpdateJob)

responseGetStatus :: GetStatusResponse -> TestTree
responseGetStatus = res
    "GetStatusResponse"
    "fixture/GetStatusResponse.proto"
    importExport
    (Proxy :: Proxy GetStatus)

responseCancelJob :: CancelJobResponse -> TestTree
responseCancelJob = res
    "CancelJobResponse"
    "fixture/CancelJobResponse.proto"
    importExport
    (Proxy :: Proxy CancelJob)
