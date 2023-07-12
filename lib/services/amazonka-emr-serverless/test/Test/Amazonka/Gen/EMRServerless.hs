{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.EMRServerless
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.EMRServerless where

import Amazonka.EMRServerless
import qualified Data.Proxy as Proxy
import Test.Amazonka.EMRServerless.Internal
import Test.Amazonka.Fixture
import Test.Amazonka.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestCancelJobRun $
--             newCancelJobRun
--
--         , requestCreateApplication $
--             newCreateApplication
--
--         , requestDeleteApplication $
--             newDeleteApplication
--
--         , requestGetApplication $
--             newGetApplication
--
--         , requestGetDashboardForJobRun $
--             newGetDashboardForJobRun
--
--         , requestGetJobRun $
--             newGetJobRun
--
--         , requestListApplications $
--             newListApplications
--
--         , requestListJobRuns $
--             newListJobRuns
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestStartApplication $
--             newStartApplication
--
--         , requestStartJobRun $
--             newStartJobRun
--
--         , requestStopApplication $
--             newStopApplication
--
--         , requestTagResource $
--             newTagResource
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestUpdateApplication $
--             newUpdateApplication
--
--           ]

--     , testGroup "response"
--         [ responseCancelJobRun $
--             newCancelJobRunResponse
--
--         , responseCreateApplication $
--             newCreateApplicationResponse
--
--         , responseDeleteApplication $
--             newDeleteApplicationResponse
--
--         , responseGetApplication $
--             newGetApplicationResponse
--
--         , responseGetDashboardForJobRun $
--             newGetDashboardForJobRunResponse
--
--         , responseGetJobRun $
--             newGetJobRunResponse
--
--         , responseListApplications $
--             newListApplicationsResponse
--
--         , responseListJobRuns $
--             newListJobRunsResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseStartApplication $
--             newStartApplicationResponse
--
--         , responseStartJobRun $
--             newStartJobRunResponse
--
--         , responseStopApplication $
--             newStopApplicationResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseUpdateApplication $
--             newUpdateApplicationResponse
--
--           ]
--     ]

-- Requests

requestCancelJobRun :: CancelJobRun -> TestTree
requestCancelJobRun =
  req
    "CancelJobRun"
    "fixture/CancelJobRun.yaml"

requestCreateApplication :: CreateApplication -> TestTree
requestCreateApplication =
  req
    "CreateApplication"
    "fixture/CreateApplication.yaml"

requestDeleteApplication :: DeleteApplication -> TestTree
requestDeleteApplication =
  req
    "DeleteApplication"
    "fixture/DeleteApplication.yaml"

requestGetApplication :: GetApplication -> TestTree
requestGetApplication =
  req
    "GetApplication"
    "fixture/GetApplication.yaml"

requestGetDashboardForJobRun :: GetDashboardForJobRun -> TestTree
requestGetDashboardForJobRun =
  req
    "GetDashboardForJobRun"
    "fixture/GetDashboardForJobRun.yaml"

requestGetJobRun :: GetJobRun -> TestTree
requestGetJobRun =
  req
    "GetJobRun"
    "fixture/GetJobRun.yaml"

requestListApplications :: ListApplications -> TestTree
requestListApplications =
  req
    "ListApplications"
    "fixture/ListApplications.yaml"

requestListJobRuns :: ListJobRuns -> TestTree
requestListJobRuns =
  req
    "ListJobRuns"
    "fixture/ListJobRuns.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestStartApplication :: StartApplication -> TestTree
requestStartApplication =
  req
    "StartApplication"
    "fixture/StartApplication.yaml"

requestStartJobRun :: StartJobRun -> TestTree
requestStartJobRun =
  req
    "StartJobRun"
    "fixture/StartJobRun.yaml"

requestStopApplication :: StopApplication -> TestTree
requestStopApplication =
  req
    "StopApplication"
    "fixture/StopApplication.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestUpdateApplication :: UpdateApplication -> TestTree
requestUpdateApplication =
  req
    "UpdateApplication"
    "fixture/UpdateApplication.yaml"

-- Responses

responseCancelJobRun :: CancelJobRunResponse -> TestTree
responseCancelJobRun =
  res
    "CancelJobRunResponse"
    "fixture/CancelJobRunResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CancelJobRun)

responseCreateApplication :: CreateApplicationResponse -> TestTree
responseCreateApplication =
  res
    "CreateApplicationResponse"
    "fixture/CreateApplicationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateApplication)

responseDeleteApplication :: DeleteApplicationResponse -> TestTree
responseDeleteApplication =
  res
    "DeleteApplicationResponse"
    "fixture/DeleteApplicationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteApplication)

responseGetApplication :: GetApplicationResponse -> TestTree
responseGetApplication =
  res
    "GetApplicationResponse"
    "fixture/GetApplicationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetApplication)

responseGetDashboardForJobRun :: GetDashboardForJobRunResponse -> TestTree
responseGetDashboardForJobRun =
  res
    "GetDashboardForJobRunResponse"
    "fixture/GetDashboardForJobRunResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetDashboardForJobRun)

responseGetJobRun :: GetJobRunResponse -> TestTree
responseGetJobRun =
  res
    "GetJobRunResponse"
    "fixture/GetJobRunResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetJobRun)

responseListApplications :: ListApplicationsResponse -> TestTree
responseListApplications =
  res
    "ListApplicationsResponse"
    "fixture/ListApplicationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListApplications)

responseListJobRuns :: ListJobRunsResponse -> TestTree
responseListJobRuns =
  res
    "ListJobRunsResponse"
    "fixture/ListJobRunsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListJobRuns)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseStartApplication :: StartApplicationResponse -> TestTree
responseStartApplication =
  res
    "StartApplicationResponse"
    "fixture/StartApplicationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartApplication)

responseStartJobRun :: StartJobRunResponse -> TestTree
responseStartJobRun =
  res
    "StartJobRunResponse"
    "fixture/StartJobRunResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartJobRun)

responseStopApplication :: StopApplicationResponse -> TestTree
responseStopApplication =
  res
    "StopApplicationResponse"
    "fixture/StopApplicationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StopApplication)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TagResource)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UntagResource)

responseUpdateApplication :: UpdateApplicationResponse -> TestTree
responseUpdateApplication =
  res
    "UpdateApplicationResponse"
    "fixture/UpdateApplicationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateApplication)
