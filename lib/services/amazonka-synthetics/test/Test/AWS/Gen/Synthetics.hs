{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.Synthetics
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.AWS.Gen.Synthetics where

import qualified Data.Proxy as Proxy
import Network.AWS.Synthetics
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.AWS.Synthetics.Internal
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestUpdateCanary $
--             newUpdateCanary
--
--         , requestDeleteCanary $
--             newDeleteCanary
--
--         , requestCreateCanary $
--             newCreateCanary
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestGetCanaryRuns $
--             newGetCanaryRuns
--
--         , requestGetCanary $
--             newGetCanary
--
--         , requestDescribeRuntimeVersions $
--             newDescribeRuntimeVersions
--
--         , requestDescribeCanariesLastRun $
--             newDescribeCanariesLastRun
--
--         , requestStartCanary $
--             newStartCanary
--
--         , requestDescribeCanaries $
--             newDescribeCanaries
--
--         , requestTagResource $
--             newTagResource
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestStopCanary $
--             newStopCanary
--
--           ]

--     , testGroup "response"
--         [ responseUpdateCanary $
--             newUpdateCanaryResponse
--
--         , responseDeleteCanary $
--             newDeleteCanaryResponse
--
--         , responseCreateCanary $
--             newCreateCanaryResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseGetCanaryRuns $
--             newGetCanaryRunsResponse
--
--         , responseGetCanary $
--             newGetCanaryResponse
--
--         , responseDescribeRuntimeVersions $
--             newDescribeRuntimeVersionsResponse
--
--         , responseDescribeCanariesLastRun $
--             newDescribeCanariesLastRunResponse
--
--         , responseStartCanary $
--             newStartCanaryResponse
--
--         , responseDescribeCanaries $
--             newDescribeCanariesResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseStopCanary $
--             newStopCanaryResponse
--
--           ]
--     ]

-- Requests

requestUpdateCanary :: UpdateCanary -> TestTree
requestUpdateCanary =
  req
    "UpdateCanary"
    "fixture/UpdateCanary.yaml"

requestDeleteCanary :: DeleteCanary -> TestTree
requestDeleteCanary =
  req
    "DeleteCanary"
    "fixture/DeleteCanary.yaml"

requestCreateCanary :: CreateCanary -> TestTree
requestCreateCanary =
  req
    "CreateCanary"
    "fixture/CreateCanary.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestGetCanaryRuns :: GetCanaryRuns -> TestTree
requestGetCanaryRuns =
  req
    "GetCanaryRuns"
    "fixture/GetCanaryRuns.yaml"

requestGetCanary :: GetCanary -> TestTree
requestGetCanary =
  req
    "GetCanary"
    "fixture/GetCanary.yaml"

requestDescribeRuntimeVersions :: DescribeRuntimeVersions -> TestTree
requestDescribeRuntimeVersions =
  req
    "DescribeRuntimeVersions"
    "fixture/DescribeRuntimeVersions.yaml"

requestDescribeCanariesLastRun :: DescribeCanariesLastRun -> TestTree
requestDescribeCanariesLastRun =
  req
    "DescribeCanariesLastRun"
    "fixture/DescribeCanariesLastRun.yaml"

requestStartCanary :: StartCanary -> TestTree
requestStartCanary =
  req
    "StartCanary"
    "fixture/StartCanary.yaml"

requestDescribeCanaries :: DescribeCanaries -> TestTree
requestDescribeCanaries =
  req
    "DescribeCanaries"
    "fixture/DescribeCanaries.yaml"

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

requestStopCanary :: StopCanary -> TestTree
requestStopCanary =
  req
    "StopCanary"
    "fixture/StopCanary.yaml"

-- Responses

responseUpdateCanary :: UpdateCanaryResponse -> TestTree
responseUpdateCanary =
  res
    "UpdateCanaryResponse"
    "fixture/UpdateCanaryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateCanary)

responseDeleteCanary :: DeleteCanaryResponse -> TestTree
responseDeleteCanary =
  res
    "DeleteCanaryResponse"
    "fixture/DeleteCanaryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteCanary)

responseCreateCanary :: CreateCanaryResponse -> TestTree
responseCreateCanary =
  res
    "CreateCanaryResponse"
    "fixture/CreateCanaryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateCanary)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseGetCanaryRuns :: GetCanaryRunsResponse -> TestTree
responseGetCanaryRuns =
  res
    "GetCanaryRunsResponse"
    "fixture/GetCanaryRunsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetCanaryRuns)

responseGetCanary :: GetCanaryResponse -> TestTree
responseGetCanary =
  res
    "GetCanaryResponse"
    "fixture/GetCanaryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetCanary)

responseDescribeRuntimeVersions :: DescribeRuntimeVersionsResponse -> TestTree
responseDescribeRuntimeVersions =
  res
    "DescribeRuntimeVersionsResponse"
    "fixture/DescribeRuntimeVersionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeRuntimeVersions)

responseDescribeCanariesLastRun :: DescribeCanariesLastRunResponse -> TestTree
responseDescribeCanariesLastRun =
  res
    "DescribeCanariesLastRunResponse"
    "fixture/DescribeCanariesLastRunResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeCanariesLastRun)

responseStartCanary :: StartCanaryResponse -> TestTree
responseStartCanary =
  res
    "StartCanaryResponse"
    "fixture/StartCanaryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartCanary)

responseDescribeCanaries :: DescribeCanariesResponse -> TestTree
responseDescribeCanaries =
  res
    "DescribeCanariesResponse"
    "fixture/DescribeCanariesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeCanaries)

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

responseStopCanary :: StopCanaryResponse -> TestTree
responseStopCanary =
  res
    "StopCanaryResponse"
    "fixture/StopCanaryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StopCanary)
