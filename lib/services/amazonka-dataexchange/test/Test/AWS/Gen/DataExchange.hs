{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.DataExchange
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.AWS.Gen.DataExchange where

import Amazonka.DataExchange
import qualified Data.Proxy as Proxy
import Test.AWS.DataExchange.Internal
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestGetEventAction $
--             newGetEventAction
--
--         , requestCreateRevision $
--             newCreateRevision
--
--         , requestListRevisionAssets $
--             newListRevisionAssets
--
--         , requestDeleteRevision $
--             newDeleteRevision
--
--         , requestUpdateRevision $
--             newUpdateRevision
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestGetRevision $
--             newGetRevision
--
--         , requestDeleteDataSet $
--             newDeleteDataSet
--
--         , requestUpdateDataSet $
--             newUpdateDataSet
--
--         , requestCreateJob $
--             newCreateJob
--
--         , requestListEventActions $
--             newListEventActions
--
--         , requestGetAsset $
--             newGetAsset
--
--         , requestListJobs $
--             newListJobs
--
--         , requestCreateDataSet $
--             newCreateDataSet
--
--         , requestDeleteAsset $
--             newDeleteAsset
--
--         , requestUpdateAsset $
--             newUpdateAsset
--
--         , requestGetJob $
--             newGetJob
--
--         , requestGetDataSet $
--             newGetDataSet
--
--         , requestStartJob $
--             newStartJob
--
--         , requestTagResource $
--             newTagResource
--
--         , requestListDataSetRevisions $
--             newListDataSetRevisions
--
--         , requestDeleteEventAction $
--             newDeleteEventAction
--
--         , requestUpdateEventAction $
--             newUpdateEventAction
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestListDataSets $
--             newListDataSets
--
--         , requestCreateEventAction $
--             newCreateEventAction
--
--         , requestCancelJob $
--             newCancelJob
--
--           ]

--     , testGroup "response"
--         [ responseGetEventAction $
--             newGetEventActionResponse
--
--         , responseCreateRevision $
--             newCreateRevisionResponse
--
--         , responseListRevisionAssets $
--             newListRevisionAssetsResponse
--
--         , responseDeleteRevision $
--             newDeleteRevisionResponse
--
--         , responseUpdateRevision $
--             newUpdateRevisionResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseGetRevision $
--             newGetRevisionResponse
--
--         , responseDeleteDataSet $
--             newDeleteDataSetResponse
--
--         , responseUpdateDataSet $
--             newUpdateDataSetResponse
--
--         , responseCreateJob $
--             newCreateJobResponse
--
--         , responseListEventActions $
--             newListEventActionsResponse
--
--         , responseGetAsset $
--             newGetAssetResponse
--
--         , responseListJobs $
--             newListJobsResponse
--
--         , responseCreateDataSet $
--             newCreateDataSetResponse
--
--         , responseDeleteAsset $
--             newDeleteAssetResponse
--
--         , responseUpdateAsset $
--             newUpdateAssetResponse
--
--         , responseGetJob $
--             newGetJobResponse
--
--         , responseGetDataSet $
--             newGetDataSetResponse
--
--         , responseStartJob $
--             newStartJobResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseListDataSetRevisions $
--             newListDataSetRevisionsResponse
--
--         , responseDeleteEventAction $
--             newDeleteEventActionResponse
--
--         , responseUpdateEventAction $
--             newUpdateEventActionResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseListDataSets $
--             newListDataSetsResponse
--
--         , responseCreateEventAction $
--             newCreateEventActionResponse
--
--         , responseCancelJob $
--             newCancelJobResponse
--
--           ]
--     ]

-- Requests

requestGetEventAction :: GetEventAction -> TestTree
requestGetEventAction =
  req
    "GetEventAction"
    "fixture/GetEventAction.yaml"

requestCreateRevision :: CreateRevision -> TestTree
requestCreateRevision =
  req
    "CreateRevision"
    "fixture/CreateRevision.yaml"

requestListRevisionAssets :: ListRevisionAssets -> TestTree
requestListRevisionAssets =
  req
    "ListRevisionAssets"
    "fixture/ListRevisionAssets.yaml"

requestDeleteRevision :: DeleteRevision -> TestTree
requestDeleteRevision =
  req
    "DeleteRevision"
    "fixture/DeleteRevision.yaml"

requestUpdateRevision :: UpdateRevision -> TestTree
requestUpdateRevision =
  req
    "UpdateRevision"
    "fixture/UpdateRevision.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestGetRevision :: GetRevision -> TestTree
requestGetRevision =
  req
    "GetRevision"
    "fixture/GetRevision.yaml"

requestDeleteDataSet :: DeleteDataSet -> TestTree
requestDeleteDataSet =
  req
    "DeleteDataSet"
    "fixture/DeleteDataSet.yaml"

requestUpdateDataSet :: UpdateDataSet -> TestTree
requestUpdateDataSet =
  req
    "UpdateDataSet"
    "fixture/UpdateDataSet.yaml"

requestCreateJob :: CreateJob -> TestTree
requestCreateJob =
  req
    "CreateJob"
    "fixture/CreateJob.yaml"

requestListEventActions :: ListEventActions -> TestTree
requestListEventActions =
  req
    "ListEventActions"
    "fixture/ListEventActions.yaml"

requestGetAsset :: GetAsset -> TestTree
requestGetAsset =
  req
    "GetAsset"
    "fixture/GetAsset.yaml"

requestListJobs :: ListJobs -> TestTree
requestListJobs =
  req
    "ListJobs"
    "fixture/ListJobs.yaml"

requestCreateDataSet :: CreateDataSet -> TestTree
requestCreateDataSet =
  req
    "CreateDataSet"
    "fixture/CreateDataSet.yaml"

requestDeleteAsset :: DeleteAsset -> TestTree
requestDeleteAsset =
  req
    "DeleteAsset"
    "fixture/DeleteAsset.yaml"

requestUpdateAsset :: UpdateAsset -> TestTree
requestUpdateAsset =
  req
    "UpdateAsset"
    "fixture/UpdateAsset.yaml"

requestGetJob :: GetJob -> TestTree
requestGetJob =
  req
    "GetJob"
    "fixture/GetJob.yaml"

requestGetDataSet :: GetDataSet -> TestTree
requestGetDataSet =
  req
    "GetDataSet"
    "fixture/GetDataSet.yaml"

requestStartJob :: StartJob -> TestTree
requestStartJob =
  req
    "StartJob"
    "fixture/StartJob.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestListDataSetRevisions :: ListDataSetRevisions -> TestTree
requestListDataSetRevisions =
  req
    "ListDataSetRevisions"
    "fixture/ListDataSetRevisions.yaml"

requestDeleteEventAction :: DeleteEventAction -> TestTree
requestDeleteEventAction =
  req
    "DeleteEventAction"
    "fixture/DeleteEventAction.yaml"

requestUpdateEventAction :: UpdateEventAction -> TestTree
requestUpdateEventAction =
  req
    "UpdateEventAction"
    "fixture/UpdateEventAction.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestListDataSets :: ListDataSets -> TestTree
requestListDataSets =
  req
    "ListDataSets"
    "fixture/ListDataSets.yaml"

requestCreateEventAction :: CreateEventAction -> TestTree
requestCreateEventAction =
  req
    "CreateEventAction"
    "fixture/CreateEventAction.yaml"

requestCancelJob :: CancelJob -> TestTree
requestCancelJob =
  req
    "CancelJob"
    "fixture/CancelJob.yaml"

-- Responses

responseGetEventAction :: GetEventActionResponse -> TestTree
responseGetEventAction =
  res
    "GetEventActionResponse"
    "fixture/GetEventActionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetEventAction)

responseCreateRevision :: CreateRevisionResponse -> TestTree
responseCreateRevision =
  res
    "CreateRevisionResponse"
    "fixture/CreateRevisionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateRevision)

responseListRevisionAssets :: ListRevisionAssetsResponse -> TestTree
responseListRevisionAssets =
  res
    "ListRevisionAssetsResponse"
    "fixture/ListRevisionAssetsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListRevisionAssets)

responseDeleteRevision :: DeleteRevisionResponse -> TestTree
responseDeleteRevision =
  res
    "DeleteRevisionResponse"
    "fixture/DeleteRevisionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteRevision)

responseUpdateRevision :: UpdateRevisionResponse -> TestTree
responseUpdateRevision =
  res
    "UpdateRevisionResponse"
    "fixture/UpdateRevisionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateRevision)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseGetRevision :: GetRevisionResponse -> TestTree
responseGetRevision =
  res
    "GetRevisionResponse"
    "fixture/GetRevisionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetRevision)

responseDeleteDataSet :: DeleteDataSetResponse -> TestTree
responseDeleteDataSet =
  res
    "DeleteDataSetResponse"
    "fixture/DeleteDataSetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteDataSet)

responseUpdateDataSet :: UpdateDataSetResponse -> TestTree
responseUpdateDataSet =
  res
    "UpdateDataSetResponse"
    "fixture/UpdateDataSetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateDataSet)

responseCreateJob :: CreateJobResponse -> TestTree
responseCreateJob =
  res
    "CreateJobResponse"
    "fixture/CreateJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateJob)

responseListEventActions :: ListEventActionsResponse -> TestTree
responseListEventActions =
  res
    "ListEventActionsResponse"
    "fixture/ListEventActionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListEventActions)

responseGetAsset :: GetAssetResponse -> TestTree
responseGetAsset =
  res
    "GetAssetResponse"
    "fixture/GetAssetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetAsset)

responseListJobs :: ListJobsResponse -> TestTree
responseListJobs =
  res
    "ListJobsResponse"
    "fixture/ListJobsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListJobs)

responseCreateDataSet :: CreateDataSetResponse -> TestTree
responseCreateDataSet =
  res
    "CreateDataSetResponse"
    "fixture/CreateDataSetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateDataSet)

responseDeleteAsset :: DeleteAssetResponse -> TestTree
responseDeleteAsset =
  res
    "DeleteAssetResponse"
    "fixture/DeleteAssetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteAsset)

responseUpdateAsset :: UpdateAssetResponse -> TestTree
responseUpdateAsset =
  res
    "UpdateAssetResponse"
    "fixture/UpdateAssetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateAsset)

responseGetJob :: GetJobResponse -> TestTree
responseGetJob =
  res
    "GetJobResponse"
    "fixture/GetJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetJob)

responseGetDataSet :: GetDataSetResponse -> TestTree
responseGetDataSet =
  res
    "GetDataSetResponse"
    "fixture/GetDataSetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetDataSet)

responseStartJob :: StartJobResponse -> TestTree
responseStartJob =
  res
    "StartJobResponse"
    "fixture/StartJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartJob)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TagResource)

responseListDataSetRevisions :: ListDataSetRevisionsResponse -> TestTree
responseListDataSetRevisions =
  res
    "ListDataSetRevisionsResponse"
    "fixture/ListDataSetRevisionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListDataSetRevisions)

responseDeleteEventAction :: DeleteEventActionResponse -> TestTree
responseDeleteEventAction =
  res
    "DeleteEventActionResponse"
    "fixture/DeleteEventActionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteEventAction)

responseUpdateEventAction :: UpdateEventActionResponse -> TestTree
responseUpdateEventAction =
  res
    "UpdateEventActionResponse"
    "fixture/UpdateEventActionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateEventAction)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UntagResource)

responseListDataSets :: ListDataSetsResponse -> TestTree
responseListDataSets =
  res
    "ListDataSetsResponse"
    "fixture/ListDataSetsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListDataSets)

responseCreateEventAction :: CreateEventActionResponse -> TestTree
responseCreateEventAction =
  res
    "CreateEventActionResponse"
    "fixture/CreateEventActionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateEventAction)

responseCancelJob :: CancelJobResponse -> TestTree
responseCancelJob =
  res
    "CancelJobResponse"
    "fixture/CancelJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CancelJob)
