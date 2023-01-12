{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.SSMIncidents
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.SSMIncidents where

import Amazonka.SSMIncidents
import qualified Data.Proxy as Proxy
import Test.Amazonka.Fixture
import Test.Amazonka.Prelude
import Test.Amazonka.SSMIncidents.Internal
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestCreateReplicationSet $
--             newCreateReplicationSet
--
--         , requestCreateResponsePlan $
--             newCreateResponsePlan
--
--         , requestCreateTimelineEvent $
--             newCreateTimelineEvent
--
--         , requestDeleteIncidentRecord $
--             newDeleteIncidentRecord
--
--         , requestDeleteReplicationSet $
--             newDeleteReplicationSet
--
--         , requestDeleteResourcePolicy $
--             newDeleteResourcePolicy
--
--         , requestDeleteResponsePlan $
--             newDeleteResponsePlan
--
--         , requestDeleteTimelineEvent $
--             newDeleteTimelineEvent
--
--         , requestGetIncidentRecord $
--             newGetIncidentRecord
--
--         , requestGetReplicationSet $
--             newGetReplicationSet
--
--         , requestGetResourcePolicies $
--             newGetResourcePolicies
--
--         , requestGetResponsePlan $
--             newGetResponsePlan
--
--         , requestGetTimelineEvent $
--             newGetTimelineEvent
--
--         , requestListIncidentRecords $
--             newListIncidentRecords
--
--         , requestListRelatedItems $
--             newListRelatedItems
--
--         , requestListReplicationSets $
--             newListReplicationSets
--
--         , requestListResponsePlans $
--             newListResponsePlans
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestListTimelineEvents $
--             newListTimelineEvents
--
--         , requestPutResourcePolicy $
--             newPutResourcePolicy
--
--         , requestStartIncident $
--             newStartIncident
--
--         , requestTagResource $
--             newTagResource
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestUpdateDeletionProtection $
--             newUpdateDeletionProtection
--
--         , requestUpdateIncidentRecord $
--             newUpdateIncidentRecord
--
--         , requestUpdateRelatedItems $
--             newUpdateRelatedItems
--
--         , requestUpdateReplicationSet $
--             newUpdateReplicationSet
--
--         , requestUpdateResponsePlan $
--             newUpdateResponsePlan
--
--         , requestUpdateTimelineEvent $
--             newUpdateTimelineEvent
--
--           ]

--     , testGroup "response"
--         [ responseCreateReplicationSet $
--             newCreateReplicationSetResponse
--
--         , responseCreateResponsePlan $
--             newCreateResponsePlanResponse
--
--         , responseCreateTimelineEvent $
--             newCreateTimelineEventResponse
--
--         , responseDeleteIncidentRecord $
--             newDeleteIncidentRecordResponse
--
--         , responseDeleteReplicationSet $
--             newDeleteReplicationSetResponse
--
--         , responseDeleteResourcePolicy $
--             newDeleteResourcePolicyResponse
--
--         , responseDeleteResponsePlan $
--             newDeleteResponsePlanResponse
--
--         , responseDeleteTimelineEvent $
--             newDeleteTimelineEventResponse
--
--         , responseGetIncidentRecord $
--             newGetIncidentRecordResponse
--
--         , responseGetReplicationSet $
--             newGetReplicationSetResponse
--
--         , responseGetResourcePolicies $
--             newGetResourcePoliciesResponse
--
--         , responseGetResponsePlan $
--             newGetResponsePlanResponse
--
--         , responseGetTimelineEvent $
--             newGetTimelineEventResponse
--
--         , responseListIncidentRecords $
--             newListIncidentRecordsResponse
--
--         , responseListRelatedItems $
--             newListRelatedItemsResponse
--
--         , responseListReplicationSets $
--             newListReplicationSetsResponse
--
--         , responseListResponsePlans $
--             newListResponsePlansResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseListTimelineEvents $
--             newListTimelineEventsResponse
--
--         , responsePutResourcePolicy $
--             newPutResourcePolicyResponse
--
--         , responseStartIncident $
--             newStartIncidentResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseUpdateDeletionProtection $
--             newUpdateDeletionProtectionResponse
--
--         , responseUpdateIncidentRecord $
--             newUpdateIncidentRecordResponse
--
--         , responseUpdateRelatedItems $
--             newUpdateRelatedItemsResponse
--
--         , responseUpdateReplicationSet $
--             newUpdateReplicationSetResponse
--
--         , responseUpdateResponsePlan $
--             newUpdateResponsePlanResponse
--
--         , responseUpdateTimelineEvent $
--             newUpdateTimelineEventResponse
--
--           ]
--     ]

-- Requests

requestCreateReplicationSet :: CreateReplicationSet -> TestTree
requestCreateReplicationSet =
  req
    "CreateReplicationSet"
    "fixture/CreateReplicationSet.yaml"

requestCreateResponsePlan :: CreateResponsePlan -> TestTree
requestCreateResponsePlan =
  req
    "CreateResponsePlan"
    "fixture/CreateResponsePlan.yaml"

requestCreateTimelineEvent :: CreateTimelineEvent -> TestTree
requestCreateTimelineEvent =
  req
    "CreateTimelineEvent"
    "fixture/CreateTimelineEvent.yaml"

requestDeleteIncidentRecord :: DeleteIncidentRecord -> TestTree
requestDeleteIncidentRecord =
  req
    "DeleteIncidentRecord"
    "fixture/DeleteIncidentRecord.yaml"

requestDeleteReplicationSet :: DeleteReplicationSet -> TestTree
requestDeleteReplicationSet =
  req
    "DeleteReplicationSet"
    "fixture/DeleteReplicationSet.yaml"

requestDeleteResourcePolicy :: DeleteResourcePolicy -> TestTree
requestDeleteResourcePolicy =
  req
    "DeleteResourcePolicy"
    "fixture/DeleteResourcePolicy.yaml"

requestDeleteResponsePlan :: DeleteResponsePlan -> TestTree
requestDeleteResponsePlan =
  req
    "DeleteResponsePlan"
    "fixture/DeleteResponsePlan.yaml"

requestDeleteTimelineEvent :: DeleteTimelineEvent -> TestTree
requestDeleteTimelineEvent =
  req
    "DeleteTimelineEvent"
    "fixture/DeleteTimelineEvent.yaml"

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

requestGetResourcePolicies :: GetResourcePolicies -> TestTree
requestGetResourcePolicies =
  req
    "GetResourcePolicies"
    "fixture/GetResourcePolicies.yaml"

requestGetResponsePlan :: GetResponsePlan -> TestTree
requestGetResponsePlan =
  req
    "GetResponsePlan"
    "fixture/GetResponsePlan.yaml"

requestGetTimelineEvent :: GetTimelineEvent -> TestTree
requestGetTimelineEvent =
  req
    "GetTimelineEvent"
    "fixture/GetTimelineEvent.yaml"

requestListIncidentRecords :: ListIncidentRecords -> TestTree
requestListIncidentRecords =
  req
    "ListIncidentRecords"
    "fixture/ListIncidentRecords.yaml"

requestListRelatedItems :: ListRelatedItems -> TestTree
requestListRelatedItems =
  req
    "ListRelatedItems"
    "fixture/ListRelatedItems.yaml"

requestListReplicationSets :: ListReplicationSets -> TestTree
requestListReplicationSets =
  req
    "ListReplicationSets"
    "fixture/ListReplicationSets.yaml"

requestListResponsePlans :: ListResponsePlans -> TestTree
requestListResponsePlans =
  req
    "ListResponsePlans"
    "fixture/ListResponsePlans.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestListTimelineEvents :: ListTimelineEvents -> TestTree
requestListTimelineEvents =
  req
    "ListTimelineEvents"
    "fixture/ListTimelineEvents.yaml"

requestPutResourcePolicy :: PutResourcePolicy -> TestTree
requestPutResourcePolicy =
  req
    "PutResourcePolicy"
    "fixture/PutResourcePolicy.yaml"

requestStartIncident :: StartIncident -> TestTree
requestStartIncident =
  req
    "StartIncident"
    "fixture/StartIncident.yaml"

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

requestUpdateDeletionProtection :: UpdateDeletionProtection -> TestTree
requestUpdateDeletionProtection =
  req
    "UpdateDeletionProtection"
    "fixture/UpdateDeletionProtection.yaml"

requestUpdateIncidentRecord :: UpdateIncidentRecord -> TestTree
requestUpdateIncidentRecord =
  req
    "UpdateIncidentRecord"
    "fixture/UpdateIncidentRecord.yaml"

requestUpdateRelatedItems :: UpdateRelatedItems -> TestTree
requestUpdateRelatedItems =
  req
    "UpdateRelatedItems"
    "fixture/UpdateRelatedItems.yaml"

requestUpdateReplicationSet :: UpdateReplicationSet -> TestTree
requestUpdateReplicationSet =
  req
    "UpdateReplicationSet"
    "fixture/UpdateReplicationSet.yaml"

requestUpdateResponsePlan :: UpdateResponsePlan -> TestTree
requestUpdateResponsePlan =
  req
    "UpdateResponsePlan"
    "fixture/UpdateResponsePlan.yaml"

requestUpdateTimelineEvent :: UpdateTimelineEvent -> TestTree
requestUpdateTimelineEvent =
  req
    "UpdateTimelineEvent"
    "fixture/UpdateTimelineEvent.yaml"

-- Responses

responseCreateReplicationSet :: CreateReplicationSetResponse -> TestTree
responseCreateReplicationSet =
  res
    "CreateReplicationSetResponse"
    "fixture/CreateReplicationSetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateReplicationSet)

responseCreateResponsePlan :: CreateResponsePlanResponse -> TestTree
responseCreateResponsePlan =
  res
    "CreateResponsePlanResponse"
    "fixture/CreateResponsePlanResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateResponsePlan)

responseCreateTimelineEvent :: CreateTimelineEventResponse -> TestTree
responseCreateTimelineEvent =
  res
    "CreateTimelineEventResponse"
    "fixture/CreateTimelineEventResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateTimelineEvent)

responseDeleteIncidentRecord :: DeleteIncidentRecordResponse -> TestTree
responseDeleteIncidentRecord =
  res
    "DeleteIncidentRecordResponse"
    "fixture/DeleteIncidentRecordResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteIncidentRecord)

responseDeleteReplicationSet :: DeleteReplicationSetResponse -> TestTree
responseDeleteReplicationSet =
  res
    "DeleteReplicationSetResponse"
    "fixture/DeleteReplicationSetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteReplicationSet)

responseDeleteResourcePolicy :: DeleteResourcePolicyResponse -> TestTree
responseDeleteResourcePolicy =
  res
    "DeleteResourcePolicyResponse"
    "fixture/DeleteResourcePolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteResourcePolicy)

responseDeleteResponsePlan :: DeleteResponsePlanResponse -> TestTree
responseDeleteResponsePlan =
  res
    "DeleteResponsePlanResponse"
    "fixture/DeleteResponsePlanResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteResponsePlan)

responseDeleteTimelineEvent :: DeleteTimelineEventResponse -> TestTree
responseDeleteTimelineEvent =
  res
    "DeleteTimelineEventResponse"
    "fixture/DeleteTimelineEventResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteTimelineEvent)

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

responseGetResourcePolicies :: GetResourcePoliciesResponse -> TestTree
responseGetResourcePolicies =
  res
    "GetResourcePoliciesResponse"
    "fixture/GetResourcePoliciesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetResourcePolicies)

responseGetResponsePlan :: GetResponsePlanResponse -> TestTree
responseGetResponsePlan =
  res
    "GetResponsePlanResponse"
    "fixture/GetResponsePlanResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetResponsePlan)

responseGetTimelineEvent :: GetTimelineEventResponse -> TestTree
responseGetTimelineEvent =
  res
    "GetTimelineEventResponse"
    "fixture/GetTimelineEventResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetTimelineEvent)

responseListIncidentRecords :: ListIncidentRecordsResponse -> TestTree
responseListIncidentRecords =
  res
    "ListIncidentRecordsResponse"
    "fixture/ListIncidentRecordsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListIncidentRecords)

responseListRelatedItems :: ListRelatedItemsResponse -> TestTree
responseListRelatedItems =
  res
    "ListRelatedItemsResponse"
    "fixture/ListRelatedItemsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListRelatedItems)

responseListReplicationSets :: ListReplicationSetsResponse -> TestTree
responseListReplicationSets =
  res
    "ListReplicationSetsResponse"
    "fixture/ListReplicationSetsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListReplicationSets)

responseListResponsePlans :: ListResponsePlansResponse -> TestTree
responseListResponsePlans =
  res
    "ListResponsePlansResponse"
    "fixture/ListResponsePlansResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListResponsePlans)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseListTimelineEvents :: ListTimelineEventsResponse -> TestTree
responseListTimelineEvents =
  res
    "ListTimelineEventsResponse"
    "fixture/ListTimelineEventsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTimelineEvents)

responsePutResourcePolicy :: PutResourcePolicyResponse -> TestTree
responsePutResourcePolicy =
  res
    "PutResourcePolicyResponse"
    "fixture/PutResourcePolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutResourcePolicy)

responseStartIncident :: StartIncidentResponse -> TestTree
responseStartIncident =
  res
    "StartIncidentResponse"
    "fixture/StartIncidentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartIncident)

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

responseUpdateDeletionProtection :: UpdateDeletionProtectionResponse -> TestTree
responseUpdateDeletionProtection =
  res
    "UpdateDeletionProtectionResponse"
    "fixture/UpdateDeletionProtectionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateDeletionProtection)

responseUpdateIncidentRecord :: UpdateIncidentRecordResponse -> TestTree
responseUpdateIncidentRecord =
  res
    "UpdateIncidentRecordResponse"
    "fixture/UpdateIncidentRecordResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateIncidentRecord)

responseUpdateRelatedItems :: UpdateRelatedItemsResponse -> TestTree
responseUpdateRelatedItems =
  res
    "UpdateRelatedItemsResponse"
    "fixture/UpdateRelatedItemsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateRelatedItems)

responseUpdateReplicationSet :: UpdateReplicationSetResponse -> TestTree
responseUpdateReplicationSet =
  res
    "UpdateReplicationSetResponse"
    "fixture/UpdateReplicationSetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateReplicationSet)

responseUpdateResponsePlan :: UpdateResponsePlanResponse -> TestTree
responseUpdateResponsePlan =
  res
    "UpdateResponsePlanResponse"
    "fixture/UpdateResponsePlanResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateResponsePlan)

responseUpdateTimelineEvent :: UpdateTimelineEventResponse -> TestTree
responseUpdateTimelineEvent =
  res
    "UpdateTimelineEventResponse"
    "fixture/UpdateTimelineEventResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateTimelineEvent)
