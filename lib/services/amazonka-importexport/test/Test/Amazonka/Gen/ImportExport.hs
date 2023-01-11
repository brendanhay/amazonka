{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.ImportExport
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.ImportExport where

import Amazonka.ImportExport
import qualified Data.Proxy as Proxy
import Test.Amazonka.Fixture
import Test.Amazonka.ImportExport.Internal
import Test.Amazonka.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestCancelJob $
--             newCancelJob
--
--         , requestCreateJob $
--             newCreateJob
--
--         , requestGetShippingLabel $
--             newGetShippingLabel
--
--         , requestGetStatus $
--             newGetStatus
--
--         , requestListJobs $
--             newListJobs
--
--         , requestUpdateJob $
--             newUpdateJob
--
--           ]

--     , testGroup "response"
--         [ responseCancelJob $
--             newCancelJobResponse
--
--         , responseCreateJob $
--             newCreateJobResponse
--
--         , responseGetShippingLabel $
--             newGetShippingLabelResponse
--
--         , responseGetStatus $
--             newGetStatusResponse
--
--         , responseListJobs $
--             newListJobsResponse
--
--         , responseUpdateJob $
--             newUpdateJobResponse
--
--           ]
--     ]

-- Requests

requestCancelJob :: CancelJob -> TestTree
requestCancelJob =
  req
    "CancelJob"
    "fixture/CancelJob.yaml"

requestCreateJob :: CreateJob -> TestTree
requestCreateJob =
  req
    "CreateJob"
    "fixture/CreateJob.yaml"

requestGetShippingLabel :: GetShippingLabel -> TestTree
requestGetShippingLabel =
  req
    "GetShippingLabel"
    "fixture/GetShippingLabel.yaml"

requestGetStatus :: GetStatus -> TestTree
requestGetStatus =
  req
    "GetStatus"
    "fixture/GetStatus.yaml"

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

-- Responses

responseCancelJob :: CancelJobResponse -> TestTree
responseCancelJob =
  res
    "CancelJobResponse"
    "fixture/CancelJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CancelJob)

responseCreateJob :: CreateJobResponse -> TestTree
responseCreateJob =
  res
    "CreateJobResponse"
    "fixture/CreateJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateJob)

responseGetShippingLabel :: GetShippingLabelResponse -> TestTree
responseGetShippingLabel =
  res
    "GetShippingLabelResponse"
    "fixture/GetShippingLabelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetShippingLabel)

responseGetStatus :: GetStatusResponse -> TestTree
responseGetStatus =
  res
    "GetStatusResponse"
    "fixture/GetStatusResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetStatus)

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
