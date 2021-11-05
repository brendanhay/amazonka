{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.SSMIncidents
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.AWS.Gen.SSMIncidents where

import Amazonka.SSMIncidents
import qualified Data.Proxy as Proxy
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.AWS.SSMIncidents.Internal
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestDeleteReplicationSet $
--             newDeleteReplicationSet
--
--         , requestUpdateReplicationSet $
--             newUpdateReplicationSet
--
--         , requestListReplicationSets $
--             newListReplicationSets
--
--         , requestUpdateIncidentRecord $
--             newUpdateIncidentRecord
--
--         , requestDeleteIncidentRecord $
--             newDeleteIncidentRecord
--
--         , requestCreateReplicationSet $
--             newCreateReplicationSet
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestGetResourcePolicies $
--             newGetResourcePolicies
--
--         , requestGetIncidentRecord $
--             newGetIncidentRecord
--
--         , requestGetReplicationSet $
--             newGetReplicationSet
--
--         , requestListRelatedItems $
--             newListRelatedItems
--
--         , requestUpdateDeletionProtection $
--             newUpdateDeletionProtection
--
--         , requestGetResponsePlan $
--             newGetResponsePlan
--
--         , requestCreateResponsePlan $
--             newCreateResponsePlan
--
--         , requestListIncidentRecords $
--             newListIncidentRecords
--
--         , requestUpdateRelatedItems $
--             newUpdateRelatedItems
--
--         , requestTagResource $
--             newTagResource
--
--         , requestPutResourcePolicy $
--             newPutResourcePolicy
--
--         , requestDeleteResourcePolicy $
--             newDeleteResourcePolicy
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestCreateTimelineEvent $
--             newCreateTimelineEvent
--
--         , requestListTimelineEvents $
--             newListTimelineEvents
--
--         , requestStartIncident $
--             newStartIncident
--
--         , requestDeleteTimelineEvent $
--             newDeleteTimelineEvent
--
--         , requestUpdateTimelineEvent $
--             newUpdateTimelineEvent
--
--         , requestListResponsePlans $
--             newListResponsePlans
--
--         , requestGetTimelineEvent $
--             newGetTimelineEvent
--
--         , requestUpdateResponsePlan $
--             newUpdateResponsePlan
--
--         , requestDeleteResponsePlan $
--             newDeleteResponsePlan
--
--           ]

--     , testGroup "response"
--         [ responseDeleteReplicationSet $
--             newDeleteReplicationSetResponse
--
--         , responseUpdateReplicationSet $
--             newUpdateReplicationSetResponse
--
--         , responseListReplicationSets $
--             newListReplicationSetsResponse
--
--         , responseUpdateIncidentRecord $
--             newUpdateIncidentRecordResponse
--
--         , responseDeleteIncidentRecord $
--             newDeleteIncidentRecordResponse
--
--         , responseCreateReplicationSet $
--             newCreateReplicationSetResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseGetResourcePolicies $
--             newGetResourcePoliciesResponse
--
--         , responseGetIncidentRecord $
--             newGetIncidentRecordResponse
--
--         , responseGetReplicationSet $
--             newGetReplicationSetResponse
--
--         , responseListRelatedItems $
--             newListRelatedItemsResponse
--
--         , responseUpdateDeletionProtection $
--             newUpdateDeletionProtectionResponse
--
--         , responseGetResponsePlan $
--             newGetResponsePlanResponse
--
--         , responseCreateResponsePlan $
--             newCreateResponsePlanResponse
--
--         , responseListIncidentRecords $
--             newListIncidentRecordsResponse
--
--         , responseUpdateRelatedItems $
--             newUpdateRelatedItemsResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responsePutResourcePolicy $
--             newPutResourcePolicyResponse
--
--         , responseDeleteResourcePolicy $
--             newDeleteResourcePolicyResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseCreateTimelineEvent $
--             newCreateTimelineEventResponse
--
--         , responseListTimelineEvents $
--             newListTimelineEventsResponse
--
--         , responseStartIncident $
--             newStartIncidentResponse
--
--         , responseDeleteTimelineEvent $
--             newDeleteTimelineEventResponse
--
--         , responseUpdateTimelineEvent $
--             newUpdateTimelineEventResponse
--
--         , responseListResponsePlans $
--             newListResponsePlansResponse
--
--         , responseGetTimelineEvent $
--             newGetTimelineEventResponse
--
--         , responseUpdateResponsePlan $
--             newUpdateResponsePlanResponse
--
--         , responseDeleteResponsePlan $
--             newDeleteResponsePlanResponse
--
--           ]
--     ]

-- Requests

requestDeleteReplicationSet :: DeleteReplicationSet -> TestTree
requestDeleteReplicationSet =
  req
    "DeleteReplicationSet"
    "fixture/DeleteReplicationSet.yaml"

requestUpdateReplicationSet :: UpdateReplicationSet -> TestTree
requestUpdateReplicationSet =
  req
    "UpdateReplicationSet"
    "fixture/UpdateReplicationSet.yaml"

requestListReplicationSets :: ListReplicationSets -> TestTree
requestListReplicationSets =
  req
    "ListReplicationSets"
    "fixture/ListReplicationSets.yaml"

requestUpdateIncidentRecord :: UpdateIncidentRecord -> TestTree
requestUpdateIncidentRecord =
  req
    "UpdateIncidentRecord"
    "fixture/UpdateIncidentRecord.yaml"

requestDeleteIncidentRecord :: DeleteIncidentRecord -> TestTree
requestDeleteIncidentRecord =
  req
    "DeleteIncidentRecord"
    "fixture/DeleteIncidentRecord.yaml"

requestCreateReplicationSet :: CreateReplicationSet -> TestTree
requestCreateReplicationSet =
  req
    "CreateReplicationSet"
    "fixture/CreateReplicationSet.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestGetResourcePolicies :: GetResourcePolicies -> TestTree
requestGetResourcePolicies =
  req
    "GetResourcePolicies"
    "fixture/GetResourcePolicies.yaml"

requestGetIncidentRecord :: GetIncidentRecord -> TestTree
requestGetIncidentRecord =
  req
    "GetIncidentRecord"
    "fixture/GetIncidentRecord.yaml"

requestGetReplicationSet :: GetReplicationSet -> TestTree
requestGetReplicationSet =
  req
    "GetReplicationSet"
    "fixture/GetReplicationSet.yaml"

requestListRelatedItems :: ListRelatedItems -> TestTree
requestListRelatedItems =
  req
    "ListRelatedItems"
    "fixture/ListRelatedItems.yaml"

requestUpdateDeletionProtection :: UpdateDeletionProtection -> TestTree
requestUpdateDeletionProtection =
  req
    "UpdateDeletionProtection"
    "fixture/UpdateDeletionProtection.yaml"

requestGetResponsePlan :: GetResponsePlan -> TestTree
requestGetResponsePlan =
  req
    "GetResponsePlan"
    "fixture/GetResponsePlan.yaml"

requestCreateResponsePlan :: CreateResponsePlan -> TestTree
requestCreateResponsePlan =
  req
    "CreateResponsePlan"
    "fixture/CreateResponsePlan.yaml"

requestListIncidentRecords :: ListIncidentRecords -> TestTree
requestListIncidentRecords =
  req
    "ListIncidentRecords"
    "fixture/ListIncidentRecords.yaml"

requestUpdateRelatedItems :: UpdateRelatedItems -> TestTree
requestUpdateRelatedItems =
  req
    "UpdateRelatedItems"
    "fixture/UpdateRelatedItems.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestPutResourcePolicy :: PutResourcePolicy -> TestTree
requestPutResourcePolicy =
  req
    "PutResourcePolicy"
    "fixture/PutResourcePolicy.yaml"

requestDeleteResourcePolicy :: DeleteResourcePolicy -> TestTree
requestDeleteResourcePolicy =
  req
    "DeleteResourcePolicy"
    "fixture/DeleteResourcePolicy.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestCreateTimelineEvent :: CreateTimelineEvent -> TestTree
requestCreateTimelineEvent =
  req
    "CreateTimelineEvent"
    "fixture/CreateTimelineEvent.yaml"

requestListTimelineEvents :: ListTimelineEvents -> TestTree
requestListTimelineEvents =
  req
    "ListTimelineEvents"
    "fixture/ListTimelineEvents.yaml"

requestStartIncident :: StartIncident -> TestTree
requestStartIncident =
  req
    "StartIncident"
    "fixture/StartIncident.yaml"

requestDeleteTimelineEvent :: DeleteTimelineEvent -> TestTree
requestDeleteTimelineEvent =
  req
    "DeleteTimelineEvent"
    "fixture/DeleteTimelineEvent.yaml"

requestUpdateTimelineEvent :: UpdateTimelineEvent -> TestTree
requestUpdateTimelineEvent =
  req
    "UpdateTimelineEvent"
    "fixture/UpdateTimelineEvent.yaml"

requestListResponsePlans :: ListResponsePlans -> TestTree
requestListResponsePlans =
  req
    "ListResponsePlans"
    "fixture/ListResponsePlans.yaml"

requestGetTimelineEvent :: GetTimelineEvent -> TestTree
requestGetTimelineEvent =
  req
    "GetTimelineEvent"
    "fixture/GetTimelineEvent.yaml"

requestUpdateResponsePlan :: UpdateResponsePlan -> TestTree
requestUpdateResponsePlan =
  req
    "UpdateResponsePlan"
    "fixture/UpdateResponsePlan.yaml"

requestDeleteResponsePlan :: DeleteResponsePlan -> TestTree
requestDeleteResponsePlan =
  req
    "DeleteResponsePlan"
    "fixture/DeleteResponsePlan.yaml"

-- Responses

responseDeleteReplicationSet :: DeleteReplicationSetResponse -> TestTree
responseDeleteReplicationSet =
  res
    "DeleteReplicationSetResponse"
    "fixture/DeleteReplicationSetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteReplicationSet)

responseUpdateReplicationSet :: UpdateReplicationSetResponse -> TestTree
responseUpdateReplicationSet =
  res
    "UpdateReplicationSetResponse"
    "fixture/UpdateReplicationSetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateReplicationSet)

responseListReplicationSets :: ListReplicationSetsResponse -> TestTree
responseListReplicationSets =
  res
    "ListReplicationSetsResponse"
    "fixture/ListReplicationSetsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListReplicationSets)

responseUpdateIncidentRecord :: UpdateIncidentRecordResponse -> TestTree
responseUpdateIncidentRecord =
  res
    "UpdateIncidentRecordResponse"
    "fixture/UpdateIncidentRecordResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateIncidentRecord)

responseDeleteIncidentRecord :: DeleteIncidentRecordResponse -> TestTree
responseDeleteIncidentRecord =
  res
    "DeleteIncidentRecordResponse"
    "fixture/DeleteIncidentRecordResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteIncidentRecord)

responseCreateReplicationSet :: CreateReplicationSetResponse -> TestTree
responseCreateReplicationSet =
  res
    "CreateReplicationSetResponse"
    "fixture/CreateReplicationSetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateReplicationSet)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseGetResourcePolicies :: GetResourcePoliciesResponse -> TestTree
responseGetResourcePolicies =
  res
    "GetResourcePoliciesResponse"
    "fixture/GetResourcePoliciesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetResourcePolicies)

responseGetIncidentRecord :: GetIncidentRecordResponse -> TestTree
responseGetIncidentRecord =
  res
    "GetIncidentRecordResponse"
    "fixture/GetIncidentRecordResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetIncidentRecord)

responseGetReplicationSet :: GetReplicationSetResponse -> TestTree
responseGetReplicationSet =
  res
    "GetReplicationSetResponse"
    "fixture/GetReplicationSetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetReplicationSet)

responseListRelatedItems :: ListRelatedItemsResponse -> TestTree
responseListRelatedItems =
  res
    "ListRelatedItemsResponse"
    "fixture/ListRelatedItemsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListRelatedItems)

responseUpdateDeletionProtection :: UpdateDeletionProtectionResponse -> TestTree
responseUpdateDeletionProtection =
  res
    "UpdateDeletionProtectionResponse"
    "fixture/UpdateDeletionProtectionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateDeletionProtection)

responseGetResponsePlan :: GetResponsePlanResponse -> TestTree
responseGetResponsePlan =
  res
    "GetResponsePlanResponse"
    "fixture/GetResponsePlanResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetResponsePlan)

responseCreateResponsePlan :: CreateResponsePlanResponse -> TestTree
responseCreateResponsePlan =
  res
    "CreateResponsePlanResponse"
    "fixture/CreateResponsePlanResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateResponsePlan)

responseListIncidentRecords :: ListIncidentRecordsResponse -> TestTree
responseListIncidentRecords =
  res
    "ListIncidentRecordsResponse"
    "fixture/ListIncidentRecordsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListIncidentRecords)

responseUpdateRelatedItems :: UpdateRelatedItemsResponse -> TestTree
responseUpdateRelatedItems =
  res
    "UpdateRelatedItemsResponse"
    "fixture/UpdateRelatedItemsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateRelatedItems)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TagResource)

responsePutResourcePolicy :: PutResourcePolicyResponse -> TestTree
responsePutResourcePolicy =
  res
    "PutResourcePolicyResponse"
    "fixture/PutResourcePolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutResourcePolicy)

responseDeleteResourcePolicy :: DeleteResourcePolicyResponse -> TestTree
responseDeleteResourcePolicy =
  res
    "DeleteResourcePolicyResponse"
    "fixture/DeleteResourcePolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteResourcePolicy)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UntagResource)

responseCreateTimelineEvent :: CreateTimelineEventResponse -> TestTree
responseCreateTimelineEvent =
  res
    "CreateTimelineEventResponse"
    "fixture/CreateTimelineEventResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateTimelineEvent)

responseListTimelineEvents :: ListTimelineEventsResponse -> TestTree
responseListTimelineEvents =
  res
    "ListTimelineEventsResponse"
    "fixture/ListTimelineEventsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTimelineEvents)

responseStartIncident :: StartIncidentResponse -> TestTree
responseStartIncident =
  res
    "StartIncidentResponse"
    "fixture/StartIncidentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartIncident)

responseDeleteTimelineEvent :: DeleteTimelineEventResponse -> TestTree
responseDeleteTimelineEvent =
  res
    "DeleteTimelineEventResponse"
    "fixture/DeleteTimelineEventResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteTimelineEvent)

responseUpdateTimelineEvent :: UpdateTimelineEventResponse -> TestTree
responseUpdateTimelineEvent =
  res
    "UpdateTimelineEventResponse"
    "fixture/UpdateTimelineEventResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateTimelineEvent)

responseListResponsePlans :: ListResponsePlansResponse -> TestTree
responseListResponsePlans =
  res
    "ListResponsePlansResponse"
    "fixture/ListResponsePlansResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListResponsePlans)

responseGetTimelineEvent :: GetTimelineEventResponse -> TestTree
responseGetTimelineEvent =
  res
    "GetTimelineEventResponse"
    "fixture/GetTimelineEventResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetTimelineEvent)

responseUpdateResponsePlan :: UpdateResponsePlanResponse -> TestTree
responseUpdateResponsePlan =
  res
    "UpdateResponsePlanResponse"
    "fixture/UpdateResponsePlanResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateResponsePlan)

responseDeleteResponsePlan :: DeleteResponsePlanResponse -> TestTree
responseDeleteResponsePlan =
  res
    "DeleteResponsePlanResponse"
    "fixture/DeleteResponsePlanResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteResponsePlan)
