{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.Scheduler
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.Scheduler where

import Amazonka.Scheduler
import qualified Data.Proxy as Proxy
import Test.Amazonka.Fixture
import Test.Amazonka.Prelude
import Test.Amazonka.Scheduler.Internal
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestCreateSchedule $
--             newCreateSchedule
--
--         , requestCreateScheduleGroup $
--             newCreateScheduleGroup
--
--         , requestDeleteSchedule $
--             newDeleteSchedule
--
--         , requestDeleteScheduleGroup $
--             newDeleteScheduleGroup
--
--         , requestGetSchedule $
--             newGetSchedule
--
--         , requestGetScheduleGroup $
--             newGetScheduleGroup
--
--         , requestListScheduleGroups $
--             newListScheduleGroups
--
--         , requestListSchedules $
--             newListSchedules
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestTagResource $
--             newTagResource
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestUpdateSchedule $
--             newUpdateSchedule
--
--           ]

--     , testGroup "response"
--         [ responseCreateSchedule $
--             newCreateScheduleResponse
--
--         , responseCreateScheduleGroup $
--             newCreateScheduleGroupResponse
--
--         , responseDeleteSchedule $
--             newDeleteScheduleResponse
--
--         , responseDeleteScheduleGroup $
--             newDeleteScheduleGroupResponse
--
--         , responseGetSchedule $
--             newGetScheduleResponse
--
--         , responseGetScheduleGroup $
--             newGetScheduleGroupResponse
--
--         , responseListScheduleGroups $
--             newListScheduleGroupsResponse
--
--         , responseListSchedules $
--             newListSchedulesResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseUpdateSchedule $
--             newUpdateScheduleResponse
--
--           ]
--     ]

-- Requests

requestCreateSchedule :: CreateSchedule -> TestTree
requestCreateSchedule =
  req
    "CreateSchedule"
    "fixture/CreateSchedule.yaml"

requestCreateScheduleGroup :: CreateScheduleGroup -> TestTree
requestCreateScheduleGroup =
  req
    "CreateScheduleGroup"
    "fixture/CreateScheduleGroup.yaml"

requestDeleteSchedule :: DeleteSchedule -> TestTree
requestDeleteSchedule =
  req
    "DeleteSchedule"
    "fixture/DeleteSchedule.yaml"

requestDeleteScheduleGroup :: DeleteScheduleGroup -> TestTree
requestDeleteScheduleGroup =
  req
    "DeleteScheduleGroup"
    "fixture/DeleteScheduleGroup.yaml"

requestGetSchedule :: GetSchedule -> TestTree
requestGetSchedule =
  req
    "GetSchedule"
    "fixture/GetSchedule.yaml"

requestGetScheduleGroup :: GetScheduleGroup -> TestTree
requestGetScheduleGroup =
  req
    "GetScheduleGroup"
    "fixture/GetScheduleGroup.yaml"

requestListScheduleGroups :: ListScheduleGroups -> TestTree
requestListScheduleGroups =
  req
    "ListScheduleGroups"
    "fixture/ListScheduleGroups.yaml"

requestListSchedules :: ListSchedules -> TestTree
requestListSchedules =
  req
    "ListSchedules"
    "fixture/ListSchedules.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

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

requestUpdateSchedule :: UpdateSchedule -> TestTree
requestUpdateSchedule =
  req
    "UpdateSchedule"
    "fixture/UpdateSchedule.yaml"

-- Responses

responseCreateSchedule :: CreateScheduleResponse -> TestTree
responseCreateSchedule =
  res
    "CreateScheduleResponse"
    "fixture/CreateScheduleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateSchedule)

responseCreateScheduleGroup :: CreateScheduleGroupResponse -> TestTree
responseCreateScheduleGroup =
  res
    "CreateScheduleGroupResponse"
    "fixture/CreateScheduleGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateScheduleGroup)

responseDeleteSchedule :: DeleteScheduleResponse -> TestTree
responseDeleteSchedule =
  res
    "DeleteScheduleResponse"
    "fixture/DeleteScheduleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteSchedule)

responseDeleteScheduleGroup :: DeleteScheduleGroupResponse -> TestTree
responseDeleteScheduleGroup =
  res
    "DeleteScheduleGroupResponse"
    "fixture/DeleteScheduleGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteScheduleGroup)

responseGetSchedule :: GetScheduleResponse -> TestTree
responseGetSchedule =
  res
    "GetScheduleResponse"
    "fixture/GetScheduleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetSchedule)

responseGetScheduleGroup :: GetScheduleGroupResponse -> TestTree
responseGetScheduleGroup =
  res
    "GetScheduleGroupResponse"
    "fixture/GetScheduleGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetScheduleGroup)

responseListScheduleGroups :: ListScheduleGroupsResponse -> TestTree
responseListScheduleGroups =
  res
    "ListScheduleGroupsResponse"
    "fixture/ListScheduleGroupsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListScheduleGroups)

responseListSchedules :: ListSchedulesResponse -> TestTree
responseListSchedules =
  res
    "ListSchedulesResponse"
    "fixture/ListSchedulesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListSchedules)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

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

responseUpdateSchedule :: UpdateScheduleResponse -> TestTree
responseUpdateSchedule =
  res
    "UpdateScheduleResponse"
    "fixture/UpdateScheduleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateSchedule)
