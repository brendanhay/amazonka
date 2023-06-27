{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.KendraRanking
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.KendraRanking where

import Amazonka.KendraRanking
import qualified Data.Proxy as Proxy
import Test.Amazonka.Fixture
import Test.Amazonka.KendraRanking.Internal
import Test.Amazonka.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestCreateRescoreExecutionPlan $
--             newCreateRescoreExecutionPlan
--
--         , requestDeleteRescoreExecutionPlan $
--             newDeleteRescoreExecutionPlan
--
--         , requestDescribeRescoreExecutionPlan $
--             newDescribeRescoreExecutionPlan
--
--         , requestListRescoreExecutionPlans $
--             newListRescoreExecutionPlans
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestRescore $
--             newRescore
--
--         , requestTagResource $
--             newTagResource
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestUpdateRescoreExecutionPlan $
--             newUpdateRescoreExecutionPlan
--
--           ]

--     , testGroup "response"
--         [ responseCreateRescoreExecutionPlan $
--             newCreateRescoreExecutionPlanResponse
--
--         , responseDeleteRescoreExecutionPlan $
--             newDeleteRescoreExecutionPlanResponse
--
--         , responseDescribeRescoreExecutionPlan $
--             newDescribeRescoreExecutionPlanResponse
--
--         , responseListRescoreExecutionPlans $
--             newListRescoreExecutionPlansResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseRescore $
--             newRescoreResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseUpdateRescoreExecutionPlan $
--             newUpdateRescoreExecutionPlanResponse
--
--           ]
--     ]

-- Requests

requestCreateRescoreExecutionPlan :: CreateRescoreExecutionPlan -> TestTree
requestCreateRescoreExecutionPlan =
  req
    "CreateRescoreExecutionPlan"
    "fixture/CreateRescoreExecutionPlan.yaml"

requestDeleteRescoreExecutionPlan :: DeleteRescoreExecutionPlan -> TestTree
requestDeleteRescoreExecutionPlan =
  req
    "DeleteRescoreExecutionPlan"
    "fixture/DeleteRescoreExecutionPlan.yaml"

requestDescribeRescoreExecutionPlan :: DescribeRescoreExecutionPlan -> TestTree
requestDescribeRescoreExecutionPlan =
  req
    "DescribeRescoreExecutionPlan"
    "fixture/DescribeRescoreExecutionPlan.yaml"

requestListRescoreExecutionPlans :: ListRescoreExecutionPlans -> TestTree
requestListRescoreExecutionPlans =
  req
    "ListRescoreExecutionPlans"
    "fixture/ListRescoreExecutionPlans.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestRescore :: Rescore -> TestTree
requestRescore =
  req
    "Rescore"
    "fixture/Rescore.yaml"

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

requestUpdateRescoreExecutionPlan :: UpdateRescoreExecutionPlan -> TestTree
requestUpdateRescoreExecutionPlan =
  req
    "UpdateRescoreExecutionPlan"
    "fixture/UpdateRescoreExecutionPlan.yaml"

-- Responses

responseCreateRescoreExecutionPlan :: CreateRescoreExecutionPlanResponse -> TestTree
responseCreateRescoreExecutionPlan =
  res
    "CreateRescoreExecutionPlanResponse"
    "fixture/CreateRescoreExecutionPlanResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateRescoreExecutionPlan)

responseDeleteRescoreExecutionPlan :: DeleteRescoreExecutionPlanResponse -> TestTree
responseDeleteRescoreExecutionPlan =
  res
    "DeleteRescoreExecutionPlanResponse"
    "fixture/DeleteRescoreExecutionPlanResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteRescoreExecutionPlan)

responseDescribeRescoreExecutionPlan :: DescribeRescoreExecutionPlanResponse -> TestTree
responseDescribeRescoreExecutionPlan =
  res
    "DescribeRescoreExecutionPlanResponse"
    "fixture/DescribeRescoreExecutionPlanResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeRescoreExecutionPlan)

responseListRescoreExecutionPlans :: ListRescoreExecutionPlansResponse -> TestTree
responseListRescoreExecutionPlans =
  res
    "ListRescoreExecutionPlansResponse"
    "fixture/ListRescoreExecutionPlansResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListRescoreExecutionPlans)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseRescore :: RescoreResponse -> TestTree
responseRescore =
  res
    "RescoreResponse"
    "fixture/RescoreResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy Rescore)

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

responseUpdateRescoreExecutionPlan :: UpdateRescoreExecutionPlanResponse -> TestTree
responseUpdateRescoreExecutionPlan =
  res
    "UpdateRescoreExecutionPlanResponse"
    "fixture/UpdateRescoreExecutionPlanResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateRescoreExecutionPlan)
