{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.Omics
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.Omics where

import Amazonka.Omics
import qualified Data.Proxy as Proxy
import Test.Amazonka.Fixture
import Test.Amazonka.Omics.Internal
import Test.Amazonka.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestBatchDeleteReadSet $
--             newBatchDeleteReadSet
--
--         , requestCancelAnnotationImportJob $
--             newCancelAnnotationImportJob
--
--         , requestCancelRun $
--             newCancelRun
--
--         , requestCancelVariantImportJob $
--             newCancelVariantImportJob
--
--         , requestCreateAnnotationStore $
--             newCreateAnnotationStore
--
--         , requestCreateReferenceStore $
--             newCreateReferenceStore
--
--         , requestCreateRunGroup $
--             newCreateRunGroup
--
--         , requestCreateSequenceStore $
--             newCreateSequenceStore
--
--         , requestCreateVariantStore $
--             newCreateVariantStore
--
--         , requestCreateWorkflow $
--             newCreateWorkflow
--
--         , requestDeleteAnnotationStore $
--             newDeleteAnnotationStore
--
--         , requestDeleteReference $
--             newDeleteReference
--
--         , requestDeleteReferenceStore $
--             newDeleteReferenceStore
--
--         , requestDeleteRun $
--             newDeleteRun
--
--         , requestDeleteRunGroup $
--             newDeleteRunGroup
--
--         , requestDeleteSequenceStore $
--             newDeleteSequenceStore
--
--         , requestDeleteVariantStore $
--             newDeleteVariantStore
--
--         , requestDeleteWorkflow $
--             newDeleteWorkflow
--
--         , requestGetAnnotationImportJob $
--             newGetAnnotationImportJob
--
--         , requestGetAnnotationStore $
--             newGetAnnotationStore
--
--         , requestGetReadSet $
--             newGetReadSet
--
--         , requestGetReadSetActivationJob $
--             newGetReadSetActivationJob
--
--         , requestGetReadSetExportJob $
--             newGetReadSetExportJob
--
--         , requestGetReadSetImportJob $
--             newGetReadSetImportJob
--
--         , requestGetReadSetMetadata $
--             newGetReadSetMetadata
--
--         , requestGetReference $
--             newGetReference
--
--         , requestGetReferenceImportJob $
--             newGetReferenceImportJob
--
--         , requestGetReferenceMetadata $
--             newGetReferenceMetadata
--
--         , requestGetReferenceStore $
--             newGetReferenceStore
--
--         , requestGetRun $
--             newGetRun
--
--         , requestGetRunGroup $
--             newGetRunGroup
--
--         , requestGetRunTask $
--             newGetRunTask
--
--         , requestGetSequenceStore $
--             newGetSequenceStore
--
--         , requestGetVariantImportJob $
--             newGetVariantImportJob
--
--         , requestGetVariantStore $
--             newGetVariantStore
--
--         , requestGetWorkflow $
--             newGetWorkflow
--
--         , requestListAnnotationImportJobs $
--             newListAnnotationImportJobs
--
--         , requestListAnnotationStores $
--             newListAnnotationStores
--
--         , requestListReadSetActivationJobs $
--             newListReadSetActivationJobs
--
--         , requestListReadSetExportJobs $
--             newListReadSetExportJobs
--
--         , requestListReadSetImportJobs $
--             newListReadSetImportJobs
--
--         , requestListReadSets $
--             newListReadSets
--
--         , requestListReferenceImportJobs $
--             newListReferenceImportJobs
--
--         , requestListReferenceStores $
--             newListReferenceStores
--
--         , requestListReferences $
--             newListReferences
--
--         , requestListRunGroups $
--             newListRunGroups
--
--         , requestListRunTasks $
--             newListRunTasks
--
--         , requestListRuns $
--             newListRuns
--
--         , requestListSequenceStores $
--             newListSequenceStores
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestListVariantImportJobs $
--             newListVariantImportJobs
--
--         , requestListVariantStores $
--             newListVariantStores
--
--         , requestListWorkflows $
--             newListWorkflows
--
--         , requestStartAnnotationImportJob $
--             newStartAnnotationImportJob
--
--         , requestStartReadSetActivationJob $
--             newStartReadSetActivationJob
--
--         , requestStartReadSetExportJob $
--             newStartReadSetExportJob
--
--         , requestStartReadSetImportJob $
--             newStartReadSetImportJob
--
--         , requestStartReferenceImportJob $
--             newStartReferenceImportJob
--
--         , requestStartRun $
--             newStartRun
--
--         , requestStartVariantImportJob $
--             newStartVariantImportJob
--
--         , requestTagResource $
--             newTagResource
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestUpdateAnnotationStore $
--             newUpdateAnnotationStore
--
--         , requestUpdateRunGroup $
--             newUpdateRunGroup
--
--         , requestUpdateVariantStore $
--             newUpdateVariantStore
--
--         , requestUpdateWorkflow $
--             newUpdateWorkflow
--
--           ]

--     , testGroup "response"
--         [ responseBatchDeleteReadSet $
--             newBatchDeleteReadSetResponse
--
--         , responseCancelAnnotationImportJob $
--             newCancelAnnotationImportJobResponse
--
--         , responseCancelRun $
--             newCancelRunResponse
--
--         , responseCancelVariantImportJob $
--             newCancelVariantImportJobResponse
--
--         , responseCreateAnnotationStore $
--             newCreateAnnotationStoreResponse
--
--         , responseCreateReferenceStore $
--             newCreateReferenceStoreResponse
--
--         , responseCreateRunGroup $
--             newCreateRunGroupResponse
--
--         , responseCreateSequenceStore $
--             newCreateSequenceStoreResponse
--
--         , responseCreateVariantStore $
--             newCreateVariantStoreResponse
--
--         , responseCreateWorkflow $
--             newCreateWorkflowResponse
--
--         , responseDeleteAnnotationStore $
--             newDeleteAnnotationStoreResponse
--
--         , responseDeleteReference $
--             newDeleteReferenceResponse
--
--         , responseDeleteReferenceStore $
--             newDeleteReferenceStoreResponse
--
--         , responseDeleteRun $
--             newDeleteRunResponse
--
--         , responseDeleteRunGroup $
--             newDeleteRunGroupResponse
--
--         , responseDeleteSequenceStore $
--             newDeleteSequenceStoreResponse
--
--         , responseDeleteVariantStore $
--             newDeleteVariantStoreResponse
--
--         , responseDeleteWorkflow $
--             newDeleteWorkflowResponse
--
--         , responseGetAnnotationImportJob $
--             newGetAnnotationImportJobResponse
--
--         , responseGetAnnotationStore $
--             newGetAnnotationStoreResponse
--
--         , responseGetReadSet $
--             newGetReadSetResponse
--
--         , responseGetReadSetActivationJob $
--             newGetReadSetActivationJobResponse
--
--         , responseGetReadSetExportJob $
--             newGetReadSetExportJobResponse
--
--         , responseGetReadSetImportJob $
--             newGetReadSetImportJobResponse
--
--         , responseGetReadSetMetadata $
--             newGetReadSetMetadataResponse
--
--         , responseGetReference $
--             newGetReferenceResponse
--
--         , responseGetReferenceImportJob $
--             newGetReferenceImportJobResponse
--
--         , responseGetReferenceMetadata $
--             newGetReferenceMetadataResponse
--
--         , responseGetReferenceStore $
--             newGetReferenceStoreResponse
--
--         , responseGetRun $
--             newGetRunResponse
--
--         , responseGetRunGroup $
--             newGetRunGroupResponse
--
--         , responseGetRunTask $
--             newGetRunTaskResponse
--
--         , responseGetSequenceStore $
--             newGetSequenceStoreResponse
--
--         , responseGetVariantImportJob $
--             newGetVariantImportJobResponse
--
--         , responseGetVariantStore $
--             newGetVariantStoreResponse
--
--         , responseGetWorkflow $
--             newGetWorkflowResponse
--
--         , responseListAnnotationImportJobs $
--             newListAnnotationImportJobsResponse
--
--         , responseListAnnotationStores $
--             newListAnnotationStoresResponse
--
--         , responseListReadSetActivationJobs $
--             newListReadSetActivationJobsResponse
--
--         , responseListReadSetExportJobs $
--             newListReadSetExportJobsResponse
--
--         , responseListReadSetImportJobs $
--             newListReadSetImportJobsResponse
--
--         , responseListReadSets $
--             newListReadSetsResponse
--
--         , responseListReferenceImportJobs $
--             newListReferenceImportJobsResponse
--
--         , responseListReferenceStores $
--             newListReferenceStoresResponse
--
--         , responseListReferences $
--             newListReferencesResponse
--
--         , responseListRunGroups $
--             newListRunGroupsResponse
--
--         , responseListRunTasks $
--             newListRunTasksResponse
--
--         , responseListRuns $
--             newListRunsResponse
--
--         , responseListSequenceStores $
--             newListSequenceStoresResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseListVariantImportJobs $
--             newListVariantImportJobsResponse
--
--         , responseListVariantStores $
--             newListVariantStoresResponse
--
--         , responseListWorkflows $
--             newListWorkflowsResponse
--
--         , responseStartAnnotationImportJob $
--             newStartAnnotationImportJobResponse
--
--         , responseStartReadSetActivationJob $
--             newStartReadSetActivationJobResponse
--
--         , responseStartReadSetExportJob $
--             newStartReadSetExportJobResponse
--
--         , responseStartReadSetImportJob $
--             newStartReadSetImportJobResponse
--
--         , responseStartReferenceImportJob $
--             newStartReferenceImportJobResponse
--
--         , responseStartRun $
--             newStartRunResponse
--
--         , responseStartVariantImportJob $
--             newStartVariantImportJobResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseUpdateAnnotationStore $
--             newUpdateAnnotationStoreResponse
--
--         , responseUpdateRunGroup $
--             newUpdateRunGroupResponse
--
--         , responseUpdateVariantStore $
--             newUpdateVariantStoreResponse
--
--         , responseUpdateWorkflow $
--             newUpdateWorkflowResponse
--
--           ]
--     ]

-- Requests

requestBatchDeleteReadSet :: BatchDeleteReadSet -> TestTree
requestBatchDeleteReadSet =
  req
    "BatchDeleteReadSet"
    "fixture/BatchDeleteReadSet.yaml"

requestCancelAnnotationImportJob :: CancelAnnotationImportJob -> TestTree
requestCancelAnnotationImportJob =
  req
    "CancelAnnotationImportJob"
    "fixture/CancelAnnotationImportJob.yaml"

requestCancelRun :: CancelRun -> TestTree
requestCancelRun =
  req
    "CancelRun"
    "fixture/CancelRun.yaml"

requestCancelVariantImportJob :: CancelVariantImportJob -> TestTree
requestCancelVariantImportJob =
  req
    "CancelVariantImportJob"
    "fixture/CancelVariantImportJob.yaml"

requestCreateAnnotationStore :: CreateAnnotationStore -> TestTree
requestCreateAnnotationStore =
  req
    "CreateAnnotationStore"
    "fixture/CreateAnnotationStore.yaml"

requestCreateReferenceStore :: CreateReferenceStore -> TestTree
requestCreateReferenceStore =
  req
    "CreateReferenceStore"
    "fixture/CreateReferenceStore.yaml"

requestCreateRunGroup :: CreateRunGroup -> TestTree
requestCreateRunGroup =
  req
    "CreateRunGroup"
    "fixture/CreateRunGroup.yaml"

requestCreateSequenceStore :: CreateSequenceStore -> TestTree
requestCreateSequenceStore =
  req
    "CreateSequenceStore"
    "fixture/CreateSequenceStore.yaml"

requestCreateVariantStore :: CreateVariantStore -> TestTree
requestCreateVariantStore =
  req
    "CreateVariantStore"
    "fixture/CreateVariantStore.yaml"

requestCreateWorkflow :: CreateWorkflow -> TestTree
requestCreateWorkflow =
  req
    "CreateWorkflow"
    "fixture/CreateWorkflow.yaml"

requestDeleteAnnotationStore :: DeleteAnnotationStore -> TestTree
requestDeleteAnnotationStore =
  req
    "DeleteAnnotationStore"
    "fixture/DeleteAnnotationStore.yaml"

requestDeleteReference :: DeleteReference -> TestTree
requestDeleteReference =
  req
    "DeleteReference"
    "fixture/DeleteReference.yaml"

requestDeleteReferenceStore :: DeleteReferenceStore -> TestTree
requestDeleteReferenceStore =
  req
    "DeleteReferenceStore"
    "fixture/DeleteReferenceStore.yaml"

requestDeleteRun :: DeleteRun -> TestTree
requestDeleteRun =
  req
    "DeleteRun"
    "fixture/DeleteRun.yaml"

requestDeleteRunGroup :: DeleteRunGroup -> TestTree
requestDeleteRunGroup =
  req
    "DeleteRunGroup"
    "fixture/DeleteRunGroup.yaml"

requestDeleteSequenceStore :: DeleteSequenceStore -> TestTree
requestDeleteSequenceStore =
  req
    "DeleteSequenceStore"
    "fixture/DeleteSequenceStore.yaml"

requestDeleteVariantStore :: DeleteVariantStore -> TestTree
requestDeleteVariantStore =
  req
    "DeleteVariantStore"
    "fixture/DeleteVariantStore.yaml"

requestDeleteWorkflow :: DeleteWorkflow -> TestTree
requestDeleteWorkflow =
  req
    "DeleteWorkflow"
    "fixture/DeleteWorkflow.yaml"

requestGetAnnotationImportJob :: GetAnnotationImportJob -> TestTree
requestGetAnnotationImportJob =
  req
    "GetAnnotationImportJob"
    "fixture/GetAnnotationImportJob.yaml"

requestGetAnnotationStore :: GetAnnotationStore -> TestTree
requestGetAnnotationStore =
  req
    "GetAnnotationStore"
    "fixture/GetAnnotationStore.yaml"

requestGetReadSet :: GetReadSet -> TestTree
requestGetReadSet =
  req
    "GetReadSet"
    "fixture/GetReadSet.yaml"

requestGetReadSetActivationJob :: GetReadSetActivationJob -> TestTree
requestGetReadSetActivationJob =
  req
    "GetReadSetActivationJob"
    "fixture/GetReadSetActivationJob.yaml"

requestGetReadSetExportJob :: GetReadSetExportJob -> TestTree
requestGetReadSetExportJob =
  req
    "GetReadSetExportJob"
    "fixture/GetReadSetExportJob.yaml"

requestGetReadSetImportJob :: GetReadSetImportJob -> TestTree
requestGetReadSetImportJob =
  req
    "GetReadSetImportJob"
    "fixture/GetReadSetImportJob.yaml"

requestGetReadSetMetadata :: GetReadSetMetadata -> TestTree
requestGetReadSetMetadata =
  req
    "GetReadSetMetadata"
    "fixture/GetReadSetMetadata.yaml"

requestGetReference :: GetReference -> TestTree
requestGetReference =
  req
    "GetReference"
    "fixture/GetReference.yaml"

requestGetReferenceImportJob :: GetReferenceImportJob -> TestTree
requestGetReferenceImportJob =
  req
    "GetReferenceImportJob"
    "fixture/GetReferenceImportJob.yaml"

requestGetReferenceMetadata :: GetReferenceMetadata -> TestTree
requestGetReferenceMetadata =
  req
    "GetReferenceMetadata"
    "fixture/GetReferenceMetadata.yaml"

requestGetReferenceStore :: GetReferenceStore -> TestTree
requestGetReferenceStore =
  req
    "GetReferenceStore"
    "fixture/GetReferenceStore.yaml"

requestGetRun :: GetRun -> TestTree
requestGetRun =
  req
    "GetRun"
    "fixture/GetRun.yaml"

requestGetRunGroup :: GetRunGroup -> TestTree
requestGetRunGroup =
  req
    "GetRunGroup"
    "fixture/GetRunGroup.yaml"

requestGetRunTask :: GetRunTask -> TestTree
requestGetRunTask =
  req
    "GetRunTask"
    "fixture/GetRunTask.yaml"

requestGetSequenceStore :: GetSequenceStore -> TestTree
requestGetSequenceStore =
  req
    "GetSequenceStore"
    "fixture/GetSequenceStore.yaml"

requestGetVariantImportJob :: GetVariantImportJob -> TestTree
requestGetVariantImportJob =
  req
    "GetVariantImportJob"
    "fixture/GetVariantImportJob.yaml"

requestGetVariantStore :: GetVariantStore -> TestTree
requestGetVariantStore =
  req
    "GetVariantStore"
    "fixture/GetVariantStore.yaml"

requestGetWorkflow :: GetWorkflow -> TestTree
requestGetWorkflow =
  req
    "GetWorkflow"
    "fixture/GetWorkflow.yaml"

requestListAnnotationImportJobs :: ListAnnotationImportJobs -> TestTree
requestListAnnotationImportJobs =
  req
    "ListAnnotationImportJobs"
    "fixture/ListAnnotationImportJobs.yaml"

requestListAnnotationStores :: ListAnnotationStores -> TestTree
requestListAnnotationStores =
  req
    "ListAnnotationStores"
    "fixture/ListAnnotationStores.yaml"

requestListReadSetActivationJobs :: ListReadSetActivationJobs -> TestTree
requestListReadSetActivationJobs =
  req
    "ListReadSetActivationJobs"
    "fixture/ListReadSetActivationJobs.yaml"

requestListReadSetExportJobs :: ListReadSetExportJobs -> TestTree
requestListReadSetExportJobs =
  req
    "ListReadSetExportJobs"
    "fixture/ListReadSetExportJobs.yaml"

requestListReadSetImportJobs :: ListReadSetImportJobs -> TestTree
requestListReadSetImportJobs =
  req
    "ListReadSetImportJobs"
    "fixture/ListReadSetImportJobs.yaml"

requestListReadSets :: ListReadSets -> TestTree
requestListReadSets =
  req
    "ListReadSets"
    "fixture/ListReadSets.yaml"

requestListReferenceImportJobs :: ListReferenceImportJobs -> TestTree
requestListReferenceImportJobs =
  req
    "ListReferenceImportJobs"
    "fixture/ListReferenceImportJobs.yaml"

requestListReferenceStores :: ListReferenceStores -> TestTree
requestListReferenceStores =
  req
    "ListReferenceStores"
    "fixture/ListReferenceStores.yaml"

requestListReferences :: ListReferences -> TestTree
requestListReferences =
  req
    "ListReferences"
    "fixture/ListReferences.yaml"

requestListRunGroups :: ListRunGroups -> TestTree
requestListRunGroups =
  req
    "ListRunGroups"
    "fixture/ListRunGroups.yaml"

requestListRunTasks :: ListRunTasks -> TestTree
requestListRunTasks =
  req
    "ListRunTasks"
    "fixture/ListRunTasks.yaml"

requestListRuns :: ListRuns -> TestTree
requestListRuns =
  req
    "ListRuns"
    "fixture/ListRuns.yaml"

requestListSequenceStores :: ListSequenceStores -> TestTree
requestListSequenceStores =
  req
    "ListSequenceStores"
    "fixture/ListSequenceStores.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestListVariantImportJobs :: ListVariantImportJobs -> TestTree
requestListVariantImportJobs =
  req
    "ListVariantImportJobs"
    "fixture/ListVariantImportJobs.yaml"

requestListVariantStores :: ListVariantStores -> TestTree
requestListVariantStores =
  req
    "ListVariantStores"
    "fixture/ListVariantStores.yaml"

requestListWorkflows :: ListWorkflows -> TestTree
requestListWorkflows =
  req
    "ListWorkflows"
    "fixture/ListWorkflows.yaml"

requestStartAnnotationImportJob :: StartAnnotationImportJob -> TestTree
requestStartAnnotationImportJob =
  req
    "StartAnnotationImportJob"
    "fixture/StartAnnotationImportJob.yaml"

requestStartReadSetActivationJob :: StartReadSetActivationJob -> TestTree
requestStartReadSetActivationJob =
  req
    "StartReadSetActivationJob"
    "fixture/StartReadSetActivationJob.yaml"

requestStartReadSetExportJob :: StartReadSetExportJob -> TestTree
requestStartReadSetExportJob =
  req
    "StartReadSetExportJob"
    "fixture/StartReadSetExportJob.yaml"

requestStartReadSetImportJob :: StartReadSetImportJob -> TestTree
requestStartReadSetImportJob =
  req
    "StartReadSetImportJob"
    "fixture/StartReadSetImportJob.yaml"

requestStartReferenceImportJob :: StartReferenceImportJob -> TestTree
requestStartReferenceImportJob =
  req
    "StartReferenceImportJob"
    "fixture/StartReferenceImportJob.yaml"

requestStartRun :: StartRun -> TestTree
requestStartRun =
  req
    "StartRun"
    "fixture/StartRun.yaml"

requestStartVariantImportJob :: StartVariantImportJob -> TestTree
requestStartVariantImportJob =
  req
    "StartVariantImportJob"
    "fixture/StartVariantImportJob.yaml"

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

requestUpdateAnnotationStore :: UpdateAnnotationStore -> TestTree
requestUpdateAnnotationStore =
  req
    "UpdateAnnotationStore"
    "fixture/UpdateAnnotationStore.yaml"

requestUpdateRunGroup :: UpdateRunGroup -> TestTree
requestUpdateRunGroup =
  req
    "UpdateRunGroup"
    "fixture/UpdateRunGroup.yaml"

requestUpdateVariantStore :: UpdateVariantStore -> TestTree
requestUpdateVariantStore =
  req
    "UpdateVariantStore"
    "fixture/UpdateVariantStore.yaml"

requestUpdateWorkflow :: UpdateWorkflow -> TestTree
requestUpdateWorkflow =
  req
    "UpdateWorkflow"
    "fixture/UpdateWorkflow.yaml"

-- Responses

responseBatchDeleteReadSet :: BatchDeleteReadSetResponse -> TestTree
responseBatchDeleteReadSet =
  res
    "BatchDeleteReadSetResponse"
    "fixture/BatchDeleteReadSetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchDeleteReadSet)

responseCancelAnnotationImportJob :: CancelAnnotationImportJobResponse -> TestTree
responseCancelAnnotationImportJob =
  res
    "CancelAnnotationImportJobResponse"
    "fixture/CancelAnnotationImportJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CancelAnnotationImportJob)

responseCancelRun :: CancelRunResponse -> TestTree
responseCancelRun =
  res
    "CancelRunResponse"
    "fixture/CancelRunResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CancelRun)

responseCancelVariantImportJob :: CancelVariantImportJobResponse -> TestTree
responseCancelVariantImportJob =
  res
    "CancelVariantImportJobResponse"
    "fixture/CancelVariantImportJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CancelVariantImportJob)

responseCreateAnnotationStore :: CreateAnnotationStoreResponse -> TestTree
responseCreateAnnotationStore =
  res
    "CreateAnnotationStoreResponse"
    "fixture/CreateAnnotationStoreResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateAnnotationStore)

responseCreateReferenceStore :: CreateReferenceStoreResponse -> TestTree
responseCreateReferenceStore =
  res
    "CreateReferenceStoreResponse"
    "fixture/CreateReferenceStoreResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateReferenceStore)

responseCreateRunGroup :: CreateRunGroupResponse -> TestTree
responseCreateRunGroup =
  res
    "CreateRunGroupResponse"
    "fixture/CreateRunGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateRunGroup)

responseCreateSequenceStore :: CreateSequenceStoreResponse -> TestTree
responseCreateSequenceStore =
  res
    "CreateSequenceStoreResponse"
    "fixture/CreateSequenceStoreResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateSequenceStore)

responseCreateVariantStore :: CreateVariantStoreResponse -> TestTree
responseCreateVariantStore =
  res
    "CreateVariantStoreResponse"
    "fixture/CreateVariantStoreResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateVariantStore)

responseCreateWorkflow :: CreateWorkflowResponse -> TestTree
responseCreateWorkflow =
  res
    "CreateWorkflowResponse"
    "fixture/CreateWorkflowResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateWorkflow)

responseDeleteAnnotationStore :: DeleteAnnotationStoreResponse -> TestTree
responseDeleteAnnotationStore =
  res
    "DeleteAnnotationStoreResponse"
    "fixture/DeleteAnnotationStoreResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteAnnotationStore)

responseDeleteReference :: DeleteReferenceResponse -> TestTree
responseDeleteReference =
  res
    "DeleteReferenceResponse"
    "fixture/DeleteReferenceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteReference)

responseDeleteReferenceStore :: DeleteReferenceStoreResponse -> TestTree
responseDeleteReferenceStore =
  res
    "DeleteReferenceStoreResponse"
    "fixture/DeleteReferenceStoreResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteReferenceStore)

responseDeleteRun :: DeleteRunResponse -> TestTree
responseDeleteRun =
  res
    "DeleteRunResponse"
    "fixture/DeleteRunResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteRun)

responseDeleteRunGroup :: DeleteRunGroupResponse -> TestTree
responseDeleteRunGroup =
  res
    "DeleteRunGroupResponse"
    "fixture/DeleteRunGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteRunGroup)

responseDeleteSequenceStore :: DeleteSequenceStoreResponse -> TestTree
responseDeleteSequenceStore =
  res
    "DeleteSequenceStoreResponse"
    "fixture/DeleteSequenceStoreResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteSequenceStore)

responseDeleteVariantStore :: DeleteVariantStoreResponse -> TestTree
responseDeleteVariantStore =
  res
    "DeleteVariantStoreResponse"
    "fixture/DeleteVariantStoreResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteVariantStore)

responseDeleteWorkflow :: DeleteWorkflowResponse -> TestTree
responseDeleteWorkflow =
  res
    "DeleteWorkflowResponse"
    "fixture/DeleteWorkflowResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteWorkflow)

responseGetAnnotationImportJob :: GetAnnotationImportJobResponse -> TestTree
responseGetAnnotationImportJob =
  res
    "GetAnnotationImportJobResponse"
    "fixture/GetAnnotationImportJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetAnnotationImportJob)

responseGetAnnotationStore :: GetAnnotationStoreResponse -> TestTree
responseGetAnnotationStore =
  res
    "GetAnnotationStoreResponse"
    "fixture/GetAnnotationStoreResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetAnnotationStore)

responseGetReadSetActivationJob :: GetReadSetActivationJobResponse -> TestTree
responseGetReadSetActivationJob =
  res
    "GetReadSetActivationJobResponse"
    "fixture/GetReadSetActivationJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetReadSetActivationJob)

responseGetReadSetExportJob :: GetReadSetExportJobResponse -> TestTree
responseGetReadSetExportJob =
  res
    "GetReadSetExportJobResponse"
    "fixture/GetReadSetExportJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetReadSetExportJob)

responseGetReadSetImportJob :: GetReadSetImportJobResponse -> TestTree
responseGetReadSetImportJob =
  res
    "GetReadSetImportJobResponse"
    "fixture/GetReadSetImportJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetReadSetImportJob)

responseGetReadSetMetadata :: GetReadSetMetadataResponse -> TestTree
responseGetReadSetMetadata =
  res
    "GetReadSetMetadataResponse"
    "fixture/GetReadSetMetadataResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetReadSetMetadata)

responseGetReferenceImportJob :: GetReferenceImportJobResponse -> TestTree
responseGetReferenceImportJob =
  res
    "GetReferenceImportJobResponse"
    "fixture/GetReferenceImportJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetReferenceImportJob)

responseGetReferenceMetadata :: GetReferenceMetadataResponse -> TestTree
responseGetReferenceMetadata =
  res
    "GetReferenceMetadataResponse"
    "fixture/GetReferenceMetadataResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetReferenceMetadata)

responseGetReferenceStore :: GetReferenceStoreResponse -> TestTree
responseGetReferenceStore =
  res
    "GetReferenceStoreResponse"
    "fixture/GetReferenceStoreResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetReferenceStore)

responseGetRun :: GetRunResponse -> TestTree
responseGetRun =
  res
    "GetRunResponse"
    "fixture/GetRunResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetRun)

responseGetRunGroup :: GetRunGroupResponse -> TestTree
responseGetRunGroup =
  res
    "GetRunGroupResponse"
    "fixture/GetRunGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetRunGroup)

responseGetRunTask :: GetRunTaskResponse -> TestTree
responseGetRunTask =
  res
    "GetRunTaskResponse"
    "fixture/GetRunTaskResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetRunTask)

responseGetSequenceStore :: GetSequenceStoreResponse -> TestTree
responseGetSequenceStore =
  res
    "GetSequenceStoreResponse"
    "fixture/GetSequenceStoreResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetSequenceStore)

responseGetVariantImportJob :: GetVariantImportJobResponse -> TestTree
responseGetVariantImportJob =
  res
    "GetVariantImportJobResponse"
    "fixture/GetVariantImportJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetVariantImportJob)

responseGetVariantStore :: GetVariantStoreResponse -> TestTree
responseGetVariantStore =
  res
    "GetVariantStoreResponse"
    "fixture/GetVariantStoreResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetVariantStore)

responseGetWorkflow :: GetWorkflowResponse -> TestTree
responseGetWorkflow =
  res
    "GetWorkflowResponse"
    "fixture/GetWorkflowResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetWorkflow)

responseListAnnotationImportJobs :: ListAnnotationImportJobsResponse -> TestTree
responseListAnnotationImportJobs =
  res
    "ListAnnotationImportJobsResponse"
    "fixture/ListAnnotationImportJobsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListAnnotationImportJobs)

responseListAnnotationStores :: ListAnnotationStoresResponse -> TestTree
responseListAnnotationStores =
  res
    "ListAnnotationStoresResponse"
    "fixture/ListAnnotationStoresResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListAnnotationStores)

responseListReadSetActivationJobs :: ListReadSetActivationJobsResponse -> TestTree
responseListReadSetActivationJobs =
  res
    "ListReadSetActivationJobsResponse"
    "fixture/ListReadSetActivationJobsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListReadSetActivationJobs)

responseListReadSetExportJobs :: ListReadSetExportJobsResponse -> TestTree
responseListReadSetExportJobs =
  res
    "ListReadSetExportJobsResponse"
    "fixture/ListReadSetExportJobsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListReadSetExportJobs)

responseListReadSetImportJobs :: ListReadSetImportJobsResponse -> TestTree
responseListReadSetImportJobs =
  res
    "ListReadSetImportJobsResponse"
    "fixture/ListReadSetImportJobsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListReadSetImportJobs)

responseListReadSets :: ListReadSetsResponse -> TestTree
responseListReadSets =
  res
    "ListReadSetsResponse"
    "fixture/ListReadSetsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListReadSets)

responseListReferenceImportJobs :: ListReferenceImportJobsResponse -> TestTree
responseListReferenceImportJobs =
  res
    "ListReferenceImportJobsResponse"
    "fixture/ListReferenceImportJobsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListReferenceImportJobs)

responseListReferenceStores :: ListReferenceStoresResponse -> TestTree
responseListReferenceStores =
  res
    "ListReferenceStoresResponse"
    "fixture/ListReferenceStoresResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListReferenceStores)

responseListReferences :: ListReferencesResponse -> TestTree
responseListReferences =
  res
    "ListReferencesResponse"
    "fixture/ListReferencesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListReferences)

responseListRunGroups :: ListRunGroupsResponse -> TestTree
responseListRunGroups =
  res
    "ListRunGroupsResponse"
    "fixture/ListRunGroupsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListRunGroups)

responseListRunTasks :: ListRunTasksResponse -> TestTree
responseListRunTasks =
  res
    "ListRunTasksResponse"
    "fixture/ListRunTasksResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListRunTasks)

responseListRuns :: ListRunsResponse -> TestTree
responseListRuns =
  res
    "ListRunsResponse"
    "fixture/ListRunsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListRuns)

responseListSequenceStores :: ListSequenceStoresResponse -> TestTree
responseListSequenceStores =
  res
    "ListSequenceStoresResponse"
    "fixture/ListSequenceStoresResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListSequenceStores)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseListVariantImportJobs :: ListVariantImportJobsResponse -> TestTree
responseListVariantImportJobs =
  res
    "ListVariantImportJobsResponse"
    "fixture/ListVariantImportJobsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListVariantImportJobs)

responseListVariantStores :: ListVariantStoresResponse -> TestTree
responseListVariantStores =
  res
    "ListVariantStoresResponse"
    "fixture/ListVariantStoresResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListVariantStores)

responseListWorkflows :: ListWorkflowsResponse -> TestTree
responseListWorkflows =
  res
    "ListWorkflowsResponse"
    "fixture/ListWorkflowsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListWorkflows)

responseStartAnnotationImportJob :: StartAnnotationImportJobResponse -> TestTree
responseStartAnnotationImportJob =
  res
    "StartAnnotationImportJobResponse"
    "fixture/StartAnnotationImportJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartAnnotationImportJob)

responseStartReadSetActivationJob :: StartReadSetActivationJobResponse -> TestTree
responseStartReadSetActivationJob =
  res
    "StartReadSetActivationJobResponse"
    "fixture/StartReadSetActivationJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartReadSetActivationJob)

responseStartReadSetExportJob :: StartReadSetExportJobResponse -> TestTree
responseStartReadSetExportJob =
  res
    "StartReadSetExportJobResponse"
    "fixture/StartReadSetExportJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartReadSetExportJob)

responseStartReadSetImportJob :: StartReadSetImportJobResponse -> TestTree
responseStartReadSetImportJob =
  res
    "StartReadSetImportJobResponse"
    "fixture/StartReadSetImportJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartReadSetImportJob)

responseStartReferenceImportJob :: StartReferenceImportJobResponse -> TestTree
responseStartReferenceImportJob =
  res
    "StartReferenceImportJobResponse"
    "fixture/StartReferenceImportJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartReferenceImportJob)

responseStartRun :: StartRunResponse -> TestTree
responseStartRun =
  res
    "StartRunResponse"
    "fixture/StartRunResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartRun)

responseStartVariantImportJob :: StartVariantImportJobResponse -> TestTree
responseStartVariantImportJob =
  res
    "StartVariantImportJobResponse"
    "fixture/StartVariantImportJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartVariantImportJob)

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

responseUpdateAnnotationStore :: UpdateAnnotationStoreResponse -> TestTree
responseUpdateAnnotationStore =
  res
    "UpdateAnnotationStoreResponse"
    "fixture/UpdateAnnotationStoreResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateAnnotationStore)

responseUpdateRunGroup :: UpdateRunGroupResponse -> TestTree
responseUpdateRunGroup =
  res
    "UpdateRunGroupResponse"
    "fixture/UpdateRunGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateRunGroup)

responseUpdateVariantStore :: UpdateVariantStoreResponse -> TestTree
responseUpdateVariantStore =
  res
    "UpdateVariantStoreResponse"
    "fixture/UpdateVariantStoreResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateVariantStore)

responseUpdateWorkflow :: UpdateWorkflowResponse -> TestTree
responseUpdateWorkflow =
  res
    "UpdateWorkflowResponse"
    "fixture/UpdateWorkflowResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateWorkflow)
