{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.IoTJobsData
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.AWS.Gen.IoTJobsData where

import Amazonka.IoTJobsData
import qualified Data.Proxy as Proxy
import Test.AWS.Fixture
import Test.AWS.IoTJobsData.Internal
import Test.AWS.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestUpdateJobExecution $
--             newUpdateJobExecution
--
--         , requestStartNextPendingJobExecution $
--             newStartNextPendingJobExecution
--
--         , requestDescribeJobExecution $
--             newDescribeJobExecution
--
--         , requestGetPendingJobExecutions $
--             newGetPendingJobExecutions
--
--           ]

--     , testGroup "response"
--         [ responseUpdateJobExecution $
--             newUpdateJobExecutionResponse
--
--         , responseStartNextPendingJobExecution $
--             newStartNextPendingJobExecutionResponse
--
--         , responseDescribeJobExecution $
--             newDescribeJobExecutionResponse
--
--         , responseGetPendingJobExecutions $
--             newGetPendingJobExecutionsResponse
--
--           ]
--     ]

-- Requests

requestUpdateJobExecution :: UpdateJobExecution -> TestTree
requestUpdateJobExecution =
  req
    "UpdateJobExecution"
    "fixture/UpdateJobExecution.yaml"

requestStartNextPendingJobExecution :: StartNextPendingJobExecution -> TestTree
requestStartNextPendingJobExecution =
  req
    "StartNextPendingJobExecution"
    "fixture/StartNextPendingJobExecution.yaml"

requestDescribeJobExecution :: DescribeJobExecution -> TestTree
requestDescribeJobExecution =
  req
    "DescribeJobExecution"
    "fixture/DescribeJobExecution.yaml"

requestGetPendingJobExecutions :: GetPendingJobExecutions -> TestTree
requestGetPendingJobExecutions =
  req
    "GetPendingJobExecutions"
    "fixture/GetPendingJobExecutions.yaml"

-- Responses

responseUpdateJobExecution :: UpdateJobExecutionResponse -> TestTree
responseUpdateJobExecution =
  res
    "UpdateJobExecutionResponse"
    "fixture/UpdateJobExecutionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateJobExecution)

responseStartNextPendingJobExecution :: StartNextPendingJobExecutionResponse -> TestTree
responseStartNextPendingJobExecution =
  res
    "StartNextPendingJobExecutionResponse"
    "fixture/StartNextPendingJobExecutionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartNextPendingJobExecution)

responseDescribeJobExecution :: DescribeJobExecutionResponse -> TestTree
responseDescribeJobExecution =
  res
    "DescribeJobExecutionResponse"
    "fixture/DescribeJobExecutionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeJobExecution)

responseGetPendingJobExecutions :: GetPendingJobExecutionsResponse -> TestTree
responseGetPendingJobExecutions =
  res
    "GetPendingJobExecutionsResponse"
    "fixture/GetPendingJobExecutionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetPendingJobExecutions)
