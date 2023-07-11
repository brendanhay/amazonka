{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.DataExchange
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.DataExchange where

import Amazonka.DataExchange
import qualified Data.Proxy as Proxy
import Test.Amazonka.DataExchange.Internal
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
--         [ requestCancelJob $
--             newCancelJob
--
--         , requestCreateDataSet $
--             newCreateDataSet
--
--         , requestCreateEventAction $
--             newCreateEventAction
--
--         , requestCreateJob $
--             newCreateJob
--
--         , requestCreateRevision $
--             newCreateRevision
--
--         , requestDeleteAsset $
--             newDeleteAsset
--
--         , requestDeleteDataSet $
--             newDeleteDataSet
--
--         , requestDeleteEventAction $
--             newDeleteEventAction
--
--         , requestDeleteRevision $
--             newDeleteRevision
--
--         , requestGetAsset $
--             newGetAsset
--
--         , requestGetDataSet $
--             newGetDataSet
--
--         , requestGetEventAction $
--             newGetEventAction
--
--         , requestGetJob $
--             newGetJob
--
--         , requestGetRevision $
--             newGetRevision
--
--         , requestListDataSetRevisions $
--             newListDataSetRevisions
--
--         , requestListDataSets $
--             newListDataSets
--
--         , requestListEventActions $
--             newListEventActions
--
--         , requestListJobs $
--             newListJobs
--
--         , requestListRevisionAssets $
--             newListRevisionAssets
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestRevokeRevision $
--             newRevokeRevision
--
--         , requestSendApiAsset $
--             newSendApiAsset
--
--         , requestStartJob $
--             newStartJob
--
--         , requestTagResource $
--             newTagResource
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestUpdateAsset $
--             newUpdateAsset
--
--         , requestUpdateDataSet $
--             newUpdateDataSet
--
--         , requestUpdateEventAction $
--             newUpdateEventAction
--
--         , requestUpdateRevision $
--             newUpdateRevision
--
--           ]

--     , testGroup "response"
--         [ responseCancelJob $
--             newCancelJobResponse
--
--         , responseCreateDataSet $
--             newCreateDataSetResponse
--
--         , responseCreateEventAction $
--             newCreateEventActionResponse
--
--         , responseCreateJob $
--             newCreateJobResponse
--
--         , responseCreateRevision $
--             newCreateRevisionResponse
--
--         , responseDeleteAsset $
--             newDeleteAssetResponse
--
--         , responseDeleteDataSet $
--             newDeleteDataSetResponse
--
--         , responseDeleteEventAction $
--             newDeleteEventActionResponse
--
--         , responseDeleteRevision $
--             newDeleteRevisionResponse
--
--         , responseGetAsset $
--             newGetAssetResponse
--
--         , responseGetDataSet $
--             newGetDataSetResponse
--
--         , responseGetEventAction $
--             newGetEventActionResponse
--
--         , responseGetJob $
--             newGetJobResponse
--
--         , responseGetRevision $
--             newGetRevisionResponse
--
--         , responseListDataSetRevisions $
--             newListDataSetRevisionsResponse
--
--         , responseListDataSets $
--             newListDataSetsResponse
--
--         , responseListEventActions $
--             newListEventActionsResponse
--
--         , responseListJobs $
--             newListJobsResponse
--
--         , responseListRevisionAssets $
--             newListRevisionAssetsResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseRevokeRevision $
--             newRevokeRevisionResponse
--
--         , responseSendApiAsset $
--             newSendApiAssetResponse
--
--         , responseStartJob $
--             newStartJobResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseUpdateAsset $
--             newUpdateAssetResponse
--
--         , responseUpdateDataSet $
--             newUpdateDataSetResponse
--
--         , responseUpdateEventAction $
--             newUpdateEventActionResponse
--
--         , responseUpdateRevision $
--             newUpdateRevisionResponse
--
--           ]
--     ]

-- Requests

requestCancelJob :: CancelJob -> TestTree
requestCancelJob =
  req
    "CancelJob"
    "fixture/CancelJob.yaml"

requestCreateDataSet :: CreateDataSet -> TestTree
requestCreateDataSet =
  req
    "CreateDataSet"
    "fixture/CreateDataSet.yaml"

requestCreateEventAction :: CreateEventAction -> TestTree
requestCreateEventAction =
  req
    "CreateEventAction"
    "fixture/CreateEventAction.yaml"

requestCreateJob :: CreateJob -> TestTree
requestCreateJob =
  req
    "CreateJob"
    "fixture/CreateJob.yaml"

requestCreateRevision :: CreateRevision -> TestTree
requestCreateRevision =
  req
    "CreateRevision"
    "fixture/CreateRevision.yaml"

requestDeleteAsset :: DeleteAsset -> TestTree
requestDeleteAsset =
  req
    "DeleteAsset"
    "fixture/DeleteAsset.yaml"

requestDeleteDataSet :: DeleteDataSet -> TestTree
requestDeleteDataSet =
  req
    "DeleteDataSet"
    "fixture/DeleteDataSet.yaml"

requestDeleteEventAction :: DeleteEventAction -> TestTree
requestDeleteEventAction =
  req
    "DeleteEventAction"
    "fixture/DeleteEventAction.yaml"

requestDeleteRevision :: DeleteRevision -> TestTree
requestDeleteRevision =
  req
    "DeleteRevision"
    "fixture/DeleteRevision.yaml"

requestGetAsset :: GetAsset -> TestTree
requestGetAsset =
  req
    "GetAsset"
    "fixture/GetAsset.yaml"

requestGetDataSet :: GetDataSet -> TestTree
requestGetDataSet =
  req
    "GetDataSet"
    "fixture/GetDataSet.yaml"

requestGetEventAction :: GetEventAction -> TestTree
requestGetEventAction =
  req
    "GetEventAction"
    "fixture/GetEventAction.yaml"

requestGetJob :: GetJob -> TestTree
requestGetJob =
  req
    "GetJob"
    "fixture/GetJob.yaml"

requestGetRevision :: GetRevision -> TestTree
requestGetRevision =
  req
    "GetRevision"
    "fixture/GetRevision.yaml"

requestListDataSetRevisions :: ListDataSetRevisions -> TestTree
requestListDataSetRevisions =
  req
    "ListDataSetRevisions"
    "fixture/ListDataSetRevisions.yaml"

requestListDataSets :: ListDataSets -> TestTree
requestListDataSets =
  req
    "ListDataSets"
    "fixture/ListDataSets.yaml"

requestListEventActions :: ListEventActions -> TestTree
requestListEventActions =
  req
    "ListEventActions"
    "fixture/ListEventActions.yaml"

requestListJobs :: ListJobs -> TestTree
requestListJobs =
  req
    "ListJobs"
    "fixture/ListJobs.yaml"

requestListRevisionAssets :: ListRevisionAssets -> TestTree
requestListRevisionAssets =
  req
    "ListRevisionAssets"
    "fixture/ListRevisionAssets.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestRevokeRevision :: RevokeRevision -> TestTree
requestRevokeRevision =
  req
    "RevokeRevision"
    "fixture/RevokeRevision.yaml"

requestSendApiAsset :: SendApiAsset -> TestTree
requestSendApiAsset =
  req
    "SendApiAsset"
    "fixture/SendApiAsset.yaml"

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

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestUpdateAsset :: UpdateAsset -> TestTree
requestUpdateAsset =
  req
    "UpdateAsset"
    "fixture/UpdateAsset.yaml"

requestUpdateDataSet :: UpdateDataSet -> TestTree
requestUpdateDataSet =
  req
    "UpdateDataSet"
    "fixture/UpdateDataSet.yaml"

requestUpdateEventAction :: UpdateEventAction -> TestTree
requestUpdateEventAction =
  req
    "UpdateEventAction"
    "fixture/UpdateEventAction.yaml"

requestUpdateRevision :: UpdateRevision -> TestTree
requestUpdateRevision =
  req
    "UpdateRevision"
    "fixture/UpdateRevision.yaml"

-- Responses

responseCancelJob :: CancelJobResponse -> TestTree
responseCancelJob =
  res
    "CancelJobResponse"
    "fixture/CancelJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CancelJob)

responseCreateDataSet :: CreateDataSetResponse -> TestTree
responseCreateDataSet =
  res
    "CreateDataSetResponse"
    "fixture/CreateDataSetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateDataSet)

responseCreateEventAction :: CreateEventActionResponse -> TestTree
responseCreateEventAction =
  res
    "CreateEventActionResponse"
    "fixture/CreateEventActionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateEventAction)

responseCreateJob :: CreateJobResponse -> TestTree
responseCreateJob =
  res
    "CreateJobResponse"
    "fixture/CreateJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateJob)

responseCreateRevision :: CreateRevisionResponse -> TestTree
responseCreateRevision =
  res
    "CreateRevisionResponse"
    "fixture/CreateRevisionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateRevision)

responseDeleteAsset :: DeleteAssetResponse -> TestTree
responseDeleteAsset =
  res
    "DeleteAssetResponse"
    "fixture/DeleteAssetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteAsset)

responseDeleteDataSet :: DeleteDataSetResponse -> TestTree
responseDeleteDataSet =
  res
    "DeleteDataSetResponse"
    "fixture/DeleteDataSetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteDataSet)

responseDeleteEventAction :: DeleteEventActionResponse -> TestTree
responseDeleteEventAction =
  res
    "DeleteEventActionResponse"
    "fixture/DeleteEventActionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteEventAction)

responseDeleteRevision :: DeleteRevisionResponse -> TestTree
responseDeleteRevision =
  res
    "DeleteRevisionResponse"
    "fixture/DeleteRevisionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteRevision)

responseGetAsset :: GetAssetResponse -> TestTree
responseGetAsset =
  res
    "GetAssetResponse"
    "fixture/GetAssetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetAsset)

responseGetDataSet :: GetDataSetResponse -> TestTree
responseGetDataSet =
  res
    "GetDataSetResponse"
    "fixture/GetDataSetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetDataSet)

responseGetEventAction :: GetEventActionResponse -> TestTree
responseGetEventAction =
  res
    "GetEventActionResponse"
    "fixture/GetEventActionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetEventAction)

responseGetJob :: GetJobResponse -> TestTree
responseGetJob =
  res
    "GetJobResponse"
    "fixture/GetJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetJob)

responseGetRevision :: GetRevisionResponse -> TestTree
responseGetRevision =
  res
    "GetRevisionResponse"
    "fixture/GetRevisionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetRevision)

responseListDataSetRevisions :: ListDataSetRevisionsResponse -> TestTree
responseListDataSetRevisions =
  res
    "ListDataSetRevisionsResponse"
    "fixture/ListDataSetRevisionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListDataSetRevisions)

responseListDataSets :: ListDataSetsResponse -> TestTree
responseListDataSets =
  res
    "ListDataSetsResponse"
    "fixture/ListDataSetsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListDataSets)

responseListEventActions :: ListEventActionsResponse -> TestTree
responseListEventActions =
  res
    "ListEventActionsResponse"
    "fixture/ListEventActionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListEventActions)

responseListJobs :: ListJobsResponse -> TestTree
responseListJobs =
  res
    "ListJobsResponse"
    "fixture/ListJobsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListJobs)

responseListRevisionAssets :: ListRevisionAssetsResponse -> TestTree
responseListRevisionAssets =
  res
    "ListRevisionAssetsResponse"
    "fixture/ListRevisionAssetsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListRevisionAssets)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseRevokeRevision :: RevokeRevisionResponse -> TestTree
responseRevokeRevision =
  res
    "RevokeRevisionResponse"
    "fixture/RevokeRevisionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RevokeRevision)

responseSendApiAsset :: SendApiAssetResponse -> TestTree
responseSendApiAsset =
  res
    "SendApiAssetResponse"
    "fixture/SendApiAssetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SendApiAsset)

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

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UntagResource)

responseUpdateAsset :: UpdateAssetResponse -> TestTree
responseUpdateAsset =
  res
    "UpdateAssetResponse"
    "fixture/UpdateAssetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateAsset)

responseUpdateDataSet :: UpdateDataSetResponse -> TestTree
responseUpdateDataSet =
  res
    "UpdateDataSetResponse"
    "fixture/UpdateDataSetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateDataSet)

responseUpdateEventAction :: UpdateEventActionResponse -> TestTree
responseUpdateEventAction =
  res
    "UpdateEventActionResponse"
    "fixture/UpdateEventActionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateEventAction)

responseUpdateRevision :: UpdateRevisionResponse -> TestTree
responseUpdateRevision =
  res
    "UpdateRevisionResponse"
    "fixture/UpdateRevisionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateRevision)
