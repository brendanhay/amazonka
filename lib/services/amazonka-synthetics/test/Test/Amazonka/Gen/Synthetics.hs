{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.Synthetics
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.Synthetics where

import Amazonka.Synthetics
import qualified Data.Proxy as Proxy
import Test.Amazonka.Fixture
import Test.Amazonka.Prelude
import Test.Amazonka.Synthetics.Internal
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestCreateCanary $
--             newCreateCanary
--
--         , requestDeleteCanary $
--             newDeleteCanary
--
--         , requestDescribeCanaries $
--             newDescribeCanaries
--
--         , requestDescribeCanariesLastRun $
--             newDescribeCanariesLastRun
--
--         , requestDescribeRuntimeVersions $
--             newDescribeRuntimeVersions
--
--         , requestGetCanary $
--             newGetCanary
--
--         , requestGetCanaryRuns $
--             newGetCanaryRuns
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestStartCanary $
--             newStartCanary
--
--         , requestStopCanary $
--             newStopCanary
--
--         , requestTagResource $
--             newTagResource
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestUpdateCanary $
--             newUpdateCanary
--
--           ]

--     , testGroup "response"
--         [ responseCreateCanary $
--             newCreateCanaryResponse
--
--         , responseDeleteCanary $
--             newDeleteCanaryResponse
--
--         , responseDescribeCanaries $
--             newDescribeCanariesResponse
--
--         , responseDescribeCanariesLastRun $
--             newDescribeCanariesLastRunResponse
--
--         , responseDescribeRuntimeVersions $
--             newDescribeRuntimeVersionsResponse
--
--         , responseGetCanary $
--             newGetCanaryResponse
--
--         , responseGetCanaryRuns $
--             newGetCanaryRunsResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseStartCanary $
--             newStartCanaryResponse
--
--         , responseStopCanary $
--             newStopCanaryResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseUpdateCanary $
--             newUpdateCanaryResponse
--
--           ]
--     ]

-- Requests

requestCreateCanary :: CreateCanary -> TestTree
requestCreateCanary =
  req
    "CreateCanary"
    "fixture/CreateCanary.yaml"

requestDeleteCanary :: DeleteCanary -> TestTree
requestDeleteCanary =
  req
    "DeleteCanary"
    "fixture/DeleteCanary.yaml"

requestDescribeCanaries :: DescribeCanaries -> TestTree
requestDescribeCanaries =
  req
    "DescribeCanaries"
    "fixture/DescribeCanaries.yaml"

requestDescribeCanariesLastRun :: DescribeCanariesLastRun -> TestTree
requestDescribeCanariesLastRun =
  req
    "DescribeCanariesLastRun"
    "fixture/DescribeCanariesLastRun.yaml"

requestDescribeRuntimeVersions :: DescribeRuntimeVersions -> TestTree
requestDescribeRuntimeVersions =
  req
    "DescribeRuntimeVersions"
    "fixture/DescribeRuntimeVersions.yaml"

requestGetCanary :: GetCanary -> TestTree
requestGetCanary =
  req
    "GetCanary"
    "fixture/GetCanary.yaml"

requestGetCanaryRuns :: GetCanaryRuns -> TestTree
requestGetCanaryRuns =
  req
    "GetCanaryRuns"
    "fixture/GetCanaryRuns.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestStartCanary :: StartCanary -> TestTree
requestStartCanary =
  req
    "StartCanary"
    "fixture/StartCanary.yaml"

requestStopCanary :: StopCanary -> TestTree
requestStopCanary =
  req
    "StopCanary"
    "fixture/StopCanary.yaml"

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

requestUpdateCanary :: UpdateCanary -> TestTree
requestUpdateCanary =
  req
    "UpdateCanary"
    "fixture/UpdateCanary.yaml"

-- Responses

responseCreateCanary :: CreateCanaryResponse -> TestTree
responseCreateCanary =
  res
    "CreateCanaryResponse"
    "fixture/CreateCanaryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateCanary)

responseDeleteCanary :: DeleteCanaryResponse -> TestTree
responseDeleteCanary =
  res
    "DeleteCanaryResponse"
    "fixture/DeleteCanaryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteCanary)

responseDescribeCanaries :: DescribeCanariesResponse -> TestTree
responseDescribeCanaries =
  res
    "DescribeCanariesResponse"
    "fixture/DescribeCanariesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeCanaries)

responseDescribeCanariesLastRun :: DescribeCanariesLastRunResponse -> TestTree
responseDescribeCanariesLastRun =
  res
    "DescribeCanariesLastRunResponse"
    "fixture/DescribeCanariesLastRunResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeCanariesLastRun)

responseDescribeRuntimeVersions :: DescribeRuntimeVersionsResponse -> TestTree
responseDescribeRuntimeVersions =
  res
    "DescribeRuntimeVersionsResponse"
    "fixture/DescribeRuntimeVersionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeRuntimeVersions)

responseGetCanary :: GetCanaryResponse -> TestTree
responseGetCanary =
  res
    "GetCanaryResponse"
    "fixture/GetCanaryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetCanary)

responseGetCanaryRuns :: GetCanaryRunsResponse -> TestTree
responseGetCanaryRuns =
  res
    "GetCanaryRunsResponse"
    "fixture/GetCanaryRunsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetCanaryRuns)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseStartCanary :: StartCanaryResponse -> TestTree
responseStartCanary =
  res
    "StartCanaryResponse"
    "fixture/StartCanaryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartCanary)

responseStopCanary :: StopCanaryResponse -> TestTree
responseStopCanary =
  res
    "StopCanaryResponse"
    "fixture/StopCanaryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StopCanary)

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

responseUpdateCanary :: UpdateCanaryResponse -> TestTree
responseUpdateCanary =
  res
    "UpdateCanaryResponse"
    "fixture/UpdateCanaryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateCanary)
