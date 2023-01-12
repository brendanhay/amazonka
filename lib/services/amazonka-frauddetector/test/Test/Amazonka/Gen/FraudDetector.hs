{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.FraudDetector
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.FraudDetector where

import Amazonka.FraudDetector
import qualified Data.Proxy as Proxy
import Test.Amazonka.Fixture
import Test.Amazonka.FraudDetector.Internal
import Test.Amazonka.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestBatchCreateVariable $
--             newBatchCreateVariable
--
--         , requestBatchGetVariable $
--             newBatchGetVariable
--
--         , requestCancelBatchImportJob $
--             newCancelBatchImportJob
--
--         , requestCancelBatchPredictionJob $
--             newCancelBatchPredictionJob
--
--         , requestCreateBatchImportJob $
--             newCreateBatchImportJob
--
--         , requestCreateBatchPredictionJob $
--             newCreateBatchPredictionJob
--
--         , requestCreateDetectorVersion $
--             newCreateDetectorVersion
--
--         , requestCreateModel $
--             newCreateModel
--
--         , requestCreateModelVersion $
--             newCreateModelVersion
--
--         , requestCreateRule $
--             newCreateRule
--
--         , requestCreateVariable $
--             newCreateVariable
--
--         , requestDeleteBatchImportJob $
--             newDeleteBatchImportJob
--
--         , requestDeleteBatchPredictionJob $
--             newDeleteBatchPredictionJob
--
--         , requestDeleteDetector $
--             newDeleteDetector
--
--         , requestDeleteDetectorVersion $
--             newDeleteDetectorVersion
--
--         , requestDeleteEntityType $
--             newDeleteEntityType
--
--         , requestDeleteEvent $
--             newDeleteEvent
--
--         , requestDeleteEventType $
--             newDeleteEventType
--
--         , requestDeleteEventsByEventType $
--             newDeleteEventsByEventType
--
--         , requestDeleteExternalModel $
--             newDeleteExternalModel
--
--         , requestDeleteLabel $
--             newDeleteLabel
--
--         , requestDeleteModel $
--             newDeleteModel
--
--         , requestDeleteModelVersion $
--             newDeleteModelVersion
--
--         , requestDeleteOutcome $
--             newDeleteOutcome
--
--         , requestDeleteRule $
--             newDeleteRule
--
--         , requestDeleteVariable $
--             newDeleteVariable
--
--         , requestDescribeDetector $
--             newDescribeDetector
--
--         , requestDescribeModelVersions $
--             newDescribeModelVersions
--
--         , requestGetBatchImportJobs $
--             newGetBatchImportJobs
--
--         , requestGetBatchPredictionJobs $
--             newGetBatchPredictionJobs
--
--         , requestGetDeleteEventsByEventTypeStatus $
--             newGetDeleteEventsByEventTypeStatus
--
--         , requestGetDetectorVersion $
--             newGetDetectorVersion
--
--         , requestGetDetectors $
--             newGetDetectors
--
--         , requestGetEntityTypes $
--             newGetEntityTypes
--
--         , requestGetEvent $
--             newGetEvent
--
--         , requestGetEventPrediction $
--             newGetEventPrediction
--
--         , requestGetEventPredictionMetadata $
--             newGetEventPredictionMetadata
--
--         , requestGetEventTypes $
--             newGetEventTypes
--
--         , requestGetExternalModels $
--             newGetExternalModels
--
--         , requestGetKMSEncryptionKey $
--             newGetKMSEncryptionKey
--
--         , requestGetLabels $
--             newGetLabels
--
--         , requestGetModelVersion $
--             newGetModelVersion
--
--         , requestGetModels $
--             newGetModels
--
--         , requestGetOutcomes $
--             newGetOutcomes
--
--         , requestGetRules $
--             newGetRules
--
--         , requestGetVariables $
--             newGetVariables
--
--         , requestListEventPredictions $
--             newListEventPredictions
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestPutDetector $
--             newPutDetector
--
--         , requestPutEntityType $
--             newPutEntityType
--
--         , requestPutEventType $
--             newPutEventType
--
--         , requestPutExternalModel $
--             newPutExternalModel
--
--         , requestPutKMSEncryptionKey $
--             newPutKMSEncryptionKey
--
--         , requestPutLabel $
--             newPutLabel
--
--         , requestPutOutcome $
--             newPutOutcome
--
--         , requestSendEvent $
--             newSendEvent
--
--         , requestTagResource $
--             newTagResource
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestUpdateDetectorVersion $
--             newUpdateDetectorVersion
--
--         , requestUpdateDetectorVersionMetadata $
--             newUpdateDetectorVersionMetadata
--
--         , requestUpdateDetectorVersionStatus $
--             newUpdateDetectorVersionStatus
--
--         , requestUpdateEventLabel $
--             newUpdateEventLabel
--
--         , requestUpdateModel $
--             newUpdateModel
--
--         , requestUpdateModelVersion $
--             newUpdateModelVersion
--
--         , requestUpdateModelVersionStatus $
--             newUpdateModelVersionStatus
--
--         , requestUpdateRuleMetadata $
--             newUpdateRuleMetadata
--
--         , requestUpdateRuleVersion $
--             newUpdateRuleVersion
--
--         , requestUpdateVariable $
--             newUpdateVariable
--
--           ]

--     , testGroup "response"
--         [ responseBatchCreateVariable $
--             newBatchCreateVariableResponse
--
--         , responseBatchGetVariable $
--             newBatchGetVariableResponse
--
--         , responseCancelBatchImportJob $
--             newCancelBatchImportJobResponse
--
--         , responseCancelBatchPredictionJob $
--             newCancelBatchPredictionJobResponse
--
--         , responseCreateBatchImportJob $
--             newCreateBatchImportJobResponse
--
--         , responseCreateBatchPredictionJob $
--             newCreateBatchPredictionJobResponse
--
--         , responseCreateDetectorVersion $
--             newCreateDetectorVersionResponse
--
--         , responseCreateModel $
--             newCreateModelResponse
--
--         , responseCreateModelVersion $
--             newCreateModelVersionResponse
--
--         , responseCreateRule $
--             newCreateRuleResponse
--
--         , responseCreateVariable $
--             newCreateVariableResponse
--
--         , responseDeleteBatchImportJob $
--             newDeleteBatchImportJobResponse
--
--         , responseDeleteBatchPredictionJob $
--             newDeleteBatchPredictionJobResponse
--
--         , responseDeleteDetector $
--             newDeleteDetectorResponse
--
--         , responseDeleteDetectorVersion $
--             newDeleteDetectorVersionResponse
--
--         , responseDeleteEntityType $
--             newDeleteEntityTypeResponse
--
--         , responseDeleteEvent $
--             newDeleteEventResponse
--
--         , responseDeleteEventType $
--             newDeleteEventTypeResponse
--
--         , responseDeleteEventsByEventType $
--             newDeleteEventsByEventTypeResponse
--
--         , responseDeleteExternalModel $
--             newDeleteExternalModelResponse
--
--         , responseDeleteLabel $
--             newDeleteLabelResponse
--
--         , responseDeleteModel $
--             newDeleteModelResponse
--
--         , responseDeleteModelVersion $
--             newDeleteModelVersionResponse
--
--         , responseDeleteOutcome $
--             newDeleteOutcomeResponse
--
--         , responseDeleteRule $
--             newDeleteRuleResponse
--
--         , responseDeleteVariable $
--             newDeleteVariableResponse
--
--         , responseDescribeDetector $
--             newDescribeDetectorResponse
--
--         , responseDescribeModelVersions $
--             newDescribeModelVersionsResponse
--
--         , responseGetBatchImportJobs $
--             newGetBatchImportJobsResponse
--
--         , responseGetBatchPredictionJobs $
--             newGetBatchPredictionJobsResponse
--
--         , responseGetDeleteEventsByEventTypeStatus $
--             newGetDeleteEventsByEventTypeStatusResponse
--
--         , responseGetDetectorVersion $
--             newGetDetectorVersionResponse
--
--         , responseGetDetectors $
--             newGetDetectorsResponse
--
--         , responseGetEntityTypes $
--             newGetEntityTypesResponse
--
--         , responseGetEvent $
--             newGetEventResponse
--
--         , responseGetEventPrediction $
--             newGetEventPredictionResponse
--
--         , responseGetEventPredictionMetadata $
--             newGetEventPredictionMetadataResponse
--
--         , responseGetEventTypes $
--             newGetEventTypesResponse
--
--         , responseGetExternalModels $
--             newGetExternalModelsResponse
--
--         , responseGetKMSEncryptionKey $
--             newGetKMSEncryptionKeyResponse
--
--         , responseGetLabels $
--             newGetLabelsResponse
--
--         , responseGetModelVersion $
--             newGetModelVersionResponse
--
--         , responseGetModels $
--             newGetModelsResponse
--
--         , responseGetOutcomes $
--             newGetOutcomesResponse
--
--         , responseGetRules $
--             newGetRulesResponse
--
--         , responseGetVariables $
--             newGetVariablesResponse
--
--         , responseListEventPredictions $
--             newListEventPredictionsResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responsePutDetector $
--             newPutDetectorResponse
--
--         , responsePutEntityType $
--             newPutEntityTypeResponse
--
--         , responsePutEventType $
--             newPutEventTypeResponse
--
--         , responsePutExternalModel $
--             newPutExternalModelResponse
--
--         , responsePutKMSEncryptionKey $
--             newPutKMSEncryptionKeyResponse
--
--         , responsePutLabel $
--             newPutLabelResponse
--
--         , responsePutOutcome $
--             newPutOutcomeResponse
--
--         , responseSendEvent $
--             newSendEventResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseUpdateDetectorVersion $
--             newUpdateDetectorVersionResponse
--
--         , responseUpdateDetectorVersionMetadata $
--             newUpdateDetectorVersionMetadataResponse
--
--         , responseUpdateDetectorVersionStatus $
--             newUpdateDetectorVersionStatusResponse
--
--         , responseUpdateEventLabel $
--             newUpdateEventLabelResponse
--
--         , responseUpdateModel $
--             newUpdateModelResponse
--
--         , responseUpdateModelVersion $
--             newUpdateModelVersionResponse
--
--         , responseUpdateModelVersionStatus $
--             newUpdateModelVersionStatusResponse
--
--         , responseUpdateRuleMetadata $
--             newUpdateRuleMetadataResponse
--
--         , responseUpdateRuleVersion $
--             newUpdateRuleVersionResponse
--
--         , responseUpdateVariable $
--             newUpdateVariableResponse
--
--           ]
--     ]

-- Requests

requestBatchCreateVariable :: BatchCreateVariable -> TestTree
requestBatchCreateVariable =
  req
    "BatchCreateVariable"
    "fixture/BatchCreateVariable.yaml"

requestBatchGetVariable :: BatchGetVariable -> TestTree
requestBatchGetVariable =
  req
    "BatchGetVariable"
    "fixture/BatchGetVariable.yaml"

requestCancelBatchImportJob :: CancelBatchImportJob -> TestTree
requestCancelBatchImportJob =
  req
    "CancelBatchImportJob"
    "fixture/CancelBatchImportJob.yaml"

requestCancelBatchPredictionJob :: CancelBatchPredictionJob -> TestTree
requestCancelBatchPredictionJob =
  req
    "CancelBatchPredictionJob"
    "fixture/CancelBatchPredictionJob.yaml"

requestCreateBatchImportJob :: CreateBatchImportJob -> TestTree
requestCreateBatchImportJob =
  req
    "CreateBatchImportJob"
    "fixture/CreateBatchImportJob.yaml"

requestCreateBatchPredictionJob :: CreateBatchPredictionJob -> TestTree
requestCreateBatchPredictionJob =
  req
    "CreateBatchPredictionJob"
    "fixture/CreateBatchPredictionJob.yaml"

requestCreateDetectorVersion :: CreateDetectorVersion -> TestTree
requestCreateDetectorVersion =
  req
    "CreateDetectorVersion"
    "fixture/CreateDetectorVersion.yaml"

requestCreateModel :: CreateModel -> TestTree
requestCreateModel =
  req
    "CreateModel"
    "fixture/CreateModel.yaml"

requestCreateModelVersion :: CreateModelVersion -> TestTree
requestCreateModelVersion =
  req
    "CreateModelVersion"
    "fixture/CreateModelVersion.yaml"

requestCreateRule :: CreateRule -> TestTree
requestCreateRule =
  req
    "CreateRule"
    "fixture/CreateRule.yaml"

requestCreateVariable :: CreateVariable -> TestTree
requestCreateVariable =
  req
    "CreateVariable"
    "fixture/CreateVariable.yaml"

requestDeleteBatchImportJob :: DeleteBatchImportJob -> TestTree
requestDeleteBatchImportJob =
  req
    "DeleteBatchImportJob"
    "fixture/DeleteBatchImportJob.yaml"

requestDeleteBatchPredictionJob :: DeleteBatchPredictionJob -> TestTree
requestDeleteBatchPredictionJob =
  req
    "DeleteBatchPredictionJob"
    "fixture/DeleteBatchPredictionJob.yaml"

requestDeleteDetector :: DeleteDetector -> TestTree
requestDeleteDetector =
  req
    "DeleteDetector"
    "fixture/DeleteDetector.yaml"

requestDeleteDetectorVersion :: DeleteDetectorVersion -> TestTree
requestDeleteDetectorVersion =
  req
    "DeleteDetectorVersion"
    "fixture/DeleteDetectorVersion.yaml"

requestDeleteEntityType :: DeleteEntityType -> TestTree
requestDeleteEntityType =
  req
    "DeleteEntityType"
    "fixture/DeleteEntityType.yaml"

requestDeleteEvent :: DeleteEvent -> TestTree
requestDeleteEvent =
  req
    "DeleteEvent"
    "fixture/DeleteEvent.yaml"

requestDeleteEventType :: DeleteEventType -> TestTree
requestDeleteEventType =
  req
    "DeleteEventType"
    "fixture/DeleteEventType.yaml"

requestDeleteEventsByEventType :: DeleteEventsByEventType -> TestTree
requestDeleteEventsByEventType =
  req
    "DeleteEventsByEventType"
    "fixture/DeleteEventsByEventType.yaml"

requestDeleteExternalModel :: DeleteExternalModel -> TestTree
requestDeleteExternalModel =
  req
    "DeleteExternalModel"
    "fixture/DeleteExternalModel.yaml"

requestDeleteLabel :: DeleteLabel -> TestTree
requestDeleteLabel =
  req
    "DeleteLabel"
    "fixture/DeleteLabel.yaml"

requestDeleteModel :: DeleteModel -> TestTree
requestDeleteModel =
  req
    "DeleteModel"
    "fixture/DeleteModel.yaml"

requestDeleteModelVersion :: DeleteModelVersion -> TestTree
requestDeleteModelVersion =
  req
    "DeleteModelVersion"
    "fixture/DeleteModelVersion.yaml"

requestDeleteOutcome :: DeleteOutcome -> TestTree
requestDeleteOutcome =
  req
    "DeleteOutcome"
    "fixture/DeleteOutcome.yaml"

requestDeleteRule :: DeleteRule -> TestTree
requestDeleteRule =
  req
    "DeleteRule"
    "fixture/DeleteRule.yaml"

requestDeleteVariable :: DeleteVariable -> TestTree
requestDeleteVariable =
  req
    "DeleteVariable"
    "fixture/DeleteVariable.yaml"

requestDescribeDetector :: DescribeDetector -> TestTree
requestDescribeDetector =
  req
    "DescribeDetector"
    "fixture/DescribeDetector.yaml"

requestDescribeModelVersions :: DescribeModelVersions -> TestTree
requestDescribeModelVersions =
  req
    "DescribeModelVersions"
    "fixture/DescribeModelVersions.yaml"

requestGetBatchImportJobs :: GetBatchImportJobs -> TestTree
requestGetBatchImportJobs =
  req
    "GetBatchImportJobs"
    "fixture/GetBatchImportJobs.yaml"

requestGetBatchPredictionJobs :: GetBatchPredictionJobs -> TestTree
requestGetBatchPredictionJobs =
  req
    "GetBatchPredictionJobs"
    "fixture/GetBatchPredictionJobs.yaml"

requestGetDeleteEventsByEventTypeStatus :: GetDeleteEventsByEventTypeStatus -> TestTree
requestGetDeleteEventsByEventTypeStatus =
  req
    "GetDeleteEventsByEventTypeStatus"
    "fixture/GetDeleteEventsByEventTypeStatus.yaml"

requestGetDetectorVersion :: GetDetectorVersion -> TestTree
requestGetDetectorVersion =
  req
    "GetDetectorVersion"
    "fixture/GetDetectorVersion.yaml"

requestGetDetectors :: GetDetectors -> TestTree
requestGetDetectors =
  req
    "GetDetectors"
    "fixture/GetDetectors.yaml"

requestGetEntityTypes :: GetEntityTypes -> TestTree
requestGetEntityTypes =
  req
    "GetEntityTypes"
    "fixture/GetEntityTypes.yaml"

requestGetEvent :: GetEvent -> TestTree
requestGetEvent =
  req
    "GetEvent"
    "fixture/GetEvent.yaml"

requestGetEventPrediction :: GetEventPrediction -> TestTree
requestGetEventPrediction =
  req
    "GetEventPrediction"
    "fixture/GetEventPrediction.yaml"

requestGetEventPredictionMetadata :: GetEventPredictionMetadata -> TestTree
requestGetEventPredictionMetadata =
  req
    "GetEventPredictionMetadata"
    "fixture/GetEventPredictionMetadata.yaml"

requestGetEventTypes :: GetEventTypes -> TestTree
requestGetEventTypes =
  req
    "GetEventTypes"
    "fixture/GetEventTypes.yaml"

requestGetExternalModels :: GetExternalModels -> TestTree
requestGetExternalModels =
  req
    "GetExternalModels"
    "fixture/GetExternalModels.yaml"

requestGetKMSEncryptionKey :: GetKMSEncryptionKey -> TestTree
requestGetKMSEncryptionKey =
  req
    "GetKMSEncryptionKey"
    "fixture/GetKMSEncryptionKey.yaml"

requestGetLabels :: GetLabels -> TestTree
requestGetLabels =
  req
    "GetLabels"
    "fixture/GetLabels.yaml"

requestGetModelVersion :: GetModelVersion -> TestTree
requestGetModelVersion =
  req
    "GetModelVersion"
    "fixture/GetModelVersion.yaml"

requestGetModels :: GetModels -> TestTree
requestGetModels =
  req
    "GetModels"
    "fixture/GetModels.yaml"

requestGetOutcomes :: GetOutcomes -> TestTree
requestGetOutcomes =
  req
    "GetOutcomes"
    "fixture/GetOutcomes.yaml"

requestGetRules :: GetRules -> TestTree
requestGetRules =
  req
    "GetRules"
    "fixture/GetRules.yaml"

requestGetVariables :: GetVariables -> TestTree
requestGetVariables =
  req
    "GetVariables"
    "fixture/GetVariables.yaml"

requestListEventPredictions :: ListEventPredictions -> TestTree
requestListEventPredictions =
  req
    "ListEventPredictions"
    "fixture/ListEventPredictions.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestPutDetector :: PutDetector -> TestTree
requestPutDetector =
  req
    "PutDetector"
    "fixture/PutDetector.yaml"

requestPutEntityType :: PutEntityType -> TestTree
requestPutEntityType =
  req
    "PutEntityType"
    "fixture/PutEntityType.yaml"

requestPutEventType :: PutEventType -> TestTree
requestPutEventType =
  req
    "PutEventType"
    "fixture/PutEventType.yaml"

requestPutExternalModel :: PutExternalModel -> TestTree
requestPutExternalModel =
  req
    "PutExternalModel"
    "fixture/PutExternalModel.yaml"

requestPutKMSEncryptionKey :: PutKMSEncryptionKey -> TestTree
requestPutKMSEncryptionKey =
  req
    "PutKMSEncryptionKey"
    "fixture/PutKMSEncryptionKey.yaml"

requestPutLabel :: PutLabel -> TestTree
requestPutLabel =
  req
    "PutLabel"
    "fixture/PutLabel.yaml"

requestPutOutcome :: PutOutcome -> TestTree
requestPutOutcome =
  req
    "PutOutcome"
    "fixture/PutOutcome.yaml"

requestSendEvent :: SendEvent -> TestTree
requestSendEvent =
  req
    "SendEvent"
    "fixture/SendEvent.yaml"

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

requestUpdateDetectorVersion :: UpdateDetectorVersion -> TestTree
requestUpdateDetectorVersion =
  req
    "UpdateDetectorVersion"
    "fixture/UpdateDetectorVersion.yaml"

requestUpdateDetectorVersionMetadata :: UpdateDetectorVersionMetadata -> TestTree
requestUpdateDetectorVersionMetadata =
  req
    "UpdateDetectorVersionMetadata"
    "fixture/UpdateDetectorVersionMetadata.yaml"

requestUpdateDetectorVersionStatus :: UpdateDetectorVersionStatus -> TestTree
requestUpdateDetectorVersionStatus =
  req
    "UpdateDetectorVersionStatus"
    "fixture/UpdateDetectorVersionStatus.yaml"

requestUpdateEventLabel :: UpdateEventLabel -> TestTree
requestUpdateEventLabel =
  req
    "UpdateEventLabel"
    "fixture/UpdateEventLabel.yaml"

requestUpdateModel :: UpdateModel -> TestTree
requestUpdateModel =
  req
    "UpdateModel"
    "fixture/UpdateModel.yaml"

requestUpdateModelVersion :: UpdateModelVersion -> TestTree
requestUpdateModelVersion =
  req
    "UpdateModelVersion"
    "fixture/UpdateModelVersion.yaml"

requestUpdateModelVersionStatus :: UpdateModelVersionStatus -> TestTree
requestUpdateModelVersionStatus =
  req
    "UpdateModelVersionStatus"
    "fixture/UpdateModelVersionStatus.yaml"

requestUpdateRuleMetadata :: UpdateRuleMetadata -> TestTree
requestUpdateRuleMetadata =
  req
    "UpdateRuleMetadata"
    "fixture/UpdateRuleMetadata.yaml"

requestUpdateRuleVersion :: UpdateRuleVersion -> TestTree
requestUpdateRuleVersion =
  req
    "UpdateRuleVersion"
    "fixture/UpdateRuleVersion.yaml"

requestUpdateVariable :: UpdateVariable -> TestTree
requestUpdateVariable =
  req
    "UpdateVariable"
    "fixture/UpdateVariable.yaml"

-- Responses

responseBatchCreateVariable :: BatchCreateVariableResponse -> TestTree
responseBatchCreateVariable =
  res
    "BatchCreateVariableResponse"
    "fixture/BatchCreateVariableResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchCreateVariable)

responseBatchGetVariable :: BatchGetVariableResponse -> TestTree
responseBatchGetVariable =
  res
    "BatchGetVariableResponse"
    "fixture/BatchGetVariableResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchGetVariable)

responseCancelBatchImportJob :: CancelBatchImportJobResponse -> TestTree
responseCancelBatchImportJob =
  res
    "CancelBatchImportJobResponse"
    "fixture/CancelBatchImportJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CancelBatchImportJob)

responseCancelBatchPredictionJob :: CancelBatchPredictionJobResponse -> TestTree
responseCancelBatchPredictionJob =
  res
    "CancelBatchPredictionJobResponse"
    "fixture/CancelBatchPredictionJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CancelBatchPredictionJob)

responseCreateBatchImportJob :: CreateBatchImportJobResponse -> TestTree
responseCreateBatchImportJob =
  res
    "CreateBatchImportJobResponse"
    "fixture/CreateBatchImportJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateBatchImportJob)

responseCreateBatchPredictionJob :: CreateBatchPredictionJobResponse -> TestTree
responseCreateBatchPredictionJob =
  res
    "CreateBatchPredictionJobResponse"
    "fixture/CreateBatchPredictionJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateBatchPredictionJob)

responseCreateDetectorVersion :: CreateDetectorVersionResponse -> TestTree
responseCreateDetectorVersion =
  res
    "CreateDetectorVersionResponse"
    "fixture/CreateDetectorVersionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateDetectorVersion)

responseCreateModel :: CreateModelResponse -> TestTree
responseCreateModel =
  res
    "CreateModelResponse"
    "fixture/CreateModelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateModel)

responseCreateModelVersion :: CreateModelVersionResponse -> TestTree
responseCreateModelVersion =
  res
    "CreateModelVersionResponse"
    "fixture/CreateModelVersionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateModelVersion)

responseCreateRule :: CreateRuleResponse -> TestTree
responseCreateRule =
  res
    "CreateRuleResponse"
    "fixture/CreateRuleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateRule)

responseCreateVariable :: CreateVariableResponse -> TestTree
responseCreateVariable =
  res
    "CreateVariableResponse"
    "fixture/CreateVariableResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateVariable)

responseDeleteBatchImportJob :: DeleteBatchImportJobResponse -> TestTree
responseDeleteBatchImportJob =
  res
    "DeleteBatchImportJobResponse"
    "fixture/DeleteBatchImportJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteBatchImportJob)

responseDeleteBatchPredictionJob :: DeleteBatchPredictionJobResponse -> TestTree
responseDeleteBatchPredictionJob =
  res
    "DeleteBatchPredictionJobResponse"
    "fixture/DeleteBatchPredictionJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteBatchPredictionJob)

responseDeleteDetector :: DeleteDetectorResponse -> TestTree
responseDeleteDetector =
  res
    "DeleteDetectorResponse"
    "fixture/DeleteDetectorResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteDetector)

responseDeleteDetectorVersion :: DeleteDetectorVersionResponse -> TestTree
responseDeleteDetectorVersion =
  res
    "DeleteDetectorVersionResponse"
    "fixture/DeleteDetectorVersionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteDetectorVersion)

responseDeleteEntityType :: DeleteEntityTypeResponse -> TestTree
responseDeleteEntityType =
  res
    "DeleteEntityTypeResponse"
    "fixture/DeleteEntityTypeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteEntityType)

responseDeleteEvent :: DeleteEventResponse -> TestTree
responseDeleteEvent =
  res
    "DeleteEventResponse"
    "fixture/DeleteEventResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteEvent)

responseDeleteEventType :: DeleteEventTypeResponse -> TestTree
responseDeleteEventType =
  res
    "DeleteEventTypeResponse"
    "fixture/DeleteEventTypeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteEventType)

responseDeleteEventsByEventType :: DeleteEventsByEventTypeResponse -> TestTree
responseDeleteEventsByEventType =
  res
    "DeleteEventsByEventTypeResponse"
    "fixture/DeleteEventsByEventTypeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteEventsByEventType)

responseDeleteExternalModel :: DeleteExternalModelResponse -> TestTree
responseDeleteExternalModel =
  res
    "DeleteExternalModelResponse"
    "fixture/DeleteExternalModelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteExternalModel)

responseDeleteLabel :: DeleteLabelResponse -> TestTree
responseDeleteLabel =
  res
    "DeleteLabelResponse"
    "fixture/DeleteLabelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteLabel)

responseDeleteModel :: DeleteModelResponse -> TestTree
responseDeleteModel =
  res
    "DeleteModelResponse"
    "fixture/DeleteModelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteModel)

responseDeleteModelVersion :: DeleteModelVersionResponse -> TestTree
responseDeleteModelVersion =
  res
    "DeleteModelVersionResponse"
    "fixture/DeleteModelVersionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteModelVersion)

responseDeleteOutcome :: DeleteOutcomeResponse -> TestTree
responseDeleteOutcome =
  res
    "DeleteOutcomeResponse"
    "fixture/DeleteOutcomeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteOutcome)

responseDeleteRule :: DeleteRuleResponse -> TestTree
responseDeleteRule =
  res
    "DeleteRuleResponse"
    "fixture/DeleteRuleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteRule)

responseDeleteVariable :: DeleteVariableResponse -> TestTree
responseDeleteVariable =
  res
    "DeleteVariableResponse"
    "fixture/DeleteVariableResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteVariable)

responseDescribeDetector :: DescribeDetectorResponse -> TestTree
responseDescribeDetector =
  res
    "DescribeDetectorResponse"
    "fixture/DescribeDetectorResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeDetector)

responseDescribeModelVersions :: DescribeModelVersionsResponse -> TestTree
responseDescribeModelVersions =
  res
    "DescribeModelVersionsResponse"
    "fixture/DescribeModelVersionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeModelVersions)

responseGetBatchImportJobs :: GetBatchImportJobsResponse -> TestTree
responseGetBatchImportJobs =
  res
    "GetBatchImportJobsResponse"
    "fixture/GetBatchImportJobsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetBatchImportJobs)

responseGetBatchPredictionJobs :: GetBatchPredictionJobsResponse -> TestTree
responseGetBatchPredictionJobs =
  res
    "GetBatchPredictionJobsResponse"
    "fixture/GetBatchPredictionJobsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetBatchPredictionJobs)

responseGetDeleteEventsByEventTypeStatus :: GetDeleteEventsByEventTypeStatusResponse -> TestTree
responseGetDeleteEventsByEventTypeStatus =
  res
    "GetDeleteEventsByEventTypeStatusResponse"
    "fixture/GetDeleteEventsByEventTypeStatusResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetDeleteEventsByEventTypeStatus)

responseGetDetectorVersion :: GetDetectorVersionResponse -> TestTree
responseGetDetectorVersion =
  res
    "GetDetectorVersionResponse"
    "fixture/GetDetectorVersionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetDetectorVersion)

responseGetDetectors :: GetDetectorsResponse -> TestTree
responseGetDetectors =
  res
    "GetDetectorsResponse"
    "fixture/GetDetectorsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetDetectors)

responseGetEntityTypes :: GetEntityTypesResponse -> TestTree
responseGetEntityTypes =
  res
    "GetEntityTypesResponse"
    "fixture/GetEntityTypesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetEntityTypes)

responseGetEvent :: GetEventResponse -> TestTree
responseGetEvent =
  res
    "GetEventResponse"
    "fixture/GetEventResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetEvent)

responseGetEventPrediction :: GetEventPredictionResponse -> TestTree
responseGetEventPrediction =
  res
    "GetEventPredictionResponse"
    "fixture/GetEventPredictionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetEventPrediction)

responseGetEventPredictionMetadata :: GetEventPredictionMetadataResponse -> TestTree
responseGetEventPredictionMetadata =
  res
    "GetEventPredictionMetadataResponse"
    "fixture/GetEventPredictionMetadataResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetEventPredictionMetadata)

responseGetEventTypes :: GetEventTypesResponse -> TestTree
responseGetEventTypes =
  res
    "GetEventTypesResponse"
    "fixture/GetEventTypesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetEventTypes)

responseGetExternalModels :: GetExternalModelsResponse -> TestTree
responseGetExternalModels =
  res
    "GetExternalModelsResponse"
    "fixture/GetExternalModelsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetExternalModels)

responseGetKMSEncryptionKey :: GetKMSEncryptionKeyResponse -> TestTree
responseGetKMSEncryptionKey =
  res
    "GetKMSEncryptionKeyResponse"
    "fixture/GetKMSEncryptionKeyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetKMSEncryptionKey)

responseGetLabels :: GetLabelsResponse -> TestTree
responseGetLabels =
  res
    "GetLabelsResponse"
    "fixture/GetLabelsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetLabels)

responseGetModelVersion :: GetModelVersionResponse -> TestTree
responseGetModelVersion =
  res
    "GetModelVersionResponse"
    "fixture/GetModelVersionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetModelVersion)

responseGetModels :: GetModelsResponse -> TestTree
responseGetModels =
  res
    "GetModelsResponse"
    "fixture/GetModelsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetModels)

responseGetOutcomes :: GetOutcomesResponse -> TestTree
responseGetOutcomes =
  res
    "GetOutcomesResponse"
    "fixture/GetOutcomesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetOutcomes)

responseGetRules :: GetRulesResponse -> TestTree
responseGetRules =
  res
    "GetRulesResponse"
    "fixture/GetRulesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetRules)

responseGetVariables :: GetVariablesResponse -> TestTree
responseGetVariables =
  res
    "GetVariablesResponse"
    "fixture/GetVariablesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetVariables)

responseListEventPredictions :: ListEventPredictionsResponse -> TestTree
responseListEventPredictions =
  res
    "ListEventPredictionsResponse"
    "fixture/ListEventPredictionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListEventPredictions)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responsePutDetector :: PutDetectorResponse -> TestTree
responsePutDetector =
  res
    "PutDetectorResponse"
    "fixture/PutDetectorResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutDetector)

responsePutEntityType :: PutEntityTypeResponse -> TestTree
responsePutEntityType =
  res
    "PutEntityTypeResponse"
    "fixture/PutEntityTypeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutEntityType)

responsePutEventType :: PutEventTypeResponse -> TestTree
responsePutEventType =
  res
    "PutEventTypeResponse"
    "fixture/PutEventTypeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutEventType)

responsePutExternalModel :: PutExternalModelResponse -> TestTree
responsePutExternalModel =
  res
    "PutExternalModelResponse"
    "fixture/PutExternalModelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutExternalModel)

responsePutKMSEncryptionKey :: PutKMSEncryptionKeyResponse -> TestTree
responsePutKMSEncryptionKey =
  res
    "PutKMSEncryptionKeyResponse"
    "fixture/PutKMSEncryptionKeyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutKMSEncryptionKey)

responsePutLabel :: PutLabelResponse -> TestTree
responsePutLabel =
  res
    "PutLabelResponse"
    "fixture/PutLabelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutLabel)

responsePutOutcome :: PutOutcomeResponse -> TestTree
responsePutOutcome =
  res
    "PutOutcomeResponse"
    "fixture/PutOutcomeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutOutcome)

responseSendEvent :: SendEventResponse -> TestTree
responseSendEvent =
  res
    "SendEventResponse"
    "fixture/SendEventResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SendEvent)

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

responseUpdateDetectorVersion :: UpdateDetectorVersionResponse -> TestTree
responseUpdateDetectorVersion =
  res
    "UpdateDetectorVersionResponse"
    "fixture/UpdateDetectorVersionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateDetectorVersion)

responseUpdateDetectorVersionMetadata :: UpdateDetectorVersionMetadataResponse -> TestTree
responseUpdateDetectorVersionMetadata =
  res
    "UpdateDetectorVersionMetadataResponse"
    "fixture/UpdateDetectorVersionMetadataResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateDetectorVersionMetadata)

responseUpdateDetectorVersionStatus :: UpdateDetectorVersionStatusResponse -> TestTree
responseUpdateDetectorVersionStatus =
  res
    "UpdateDetectorVersionStatusResponse"
    "fixture/UpdateDetectorVersionStatusResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateDetectorVersionStatus)

responseUpdateEventLabel :: UpdateEventLabelResponse -> TestTree
responseUpdateEventLabel =
  res
    "UpdateEventLabelResponse"
    "fixture/UpdateEventLabelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateEventLabel)

responseUpdateModel :: UpdateModelResponse -> TestTree
responseUpdateModel =
  res
    "UpdateModelResponse"
    "fixture/UpdateModelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateModel)

responseUpdateModelVersion :: UpdateModelVersionResponse -> TestTree
responseUpdateModelVersion =
  res
    "UpdateModelVersionResponse"
    "fixture/UpdateModelVersionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateModelVersion)

responseUpdateModelVersionStatus :: UpdateModelVersionStatusResponse -> TestTree
responseUpdateModelVersionStatus =
  res
    "UpdateModelVersionStatusResponse"
    "fixture/UpdateModelVersionStatusResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateModelVersionStatus)

responseUpdateRuleMetadata :: UpdateRuleMetadataResponse -> TestTree
responseUpdateRuleMetadata =
  res
    "UpdateRuleMetadataResponse"
    "fixture/UpdateRuleMetadataResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateRuleMetadata)

responseUpdateRuleVersion :: UpdateRuleVersionResponse -> TestTree
responseUpdateRuleVersion =
  res
    "UpdateRuleVersionResponse"
    "fixture/UpdateRuleVersionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateRuleVersion)

responseUpdateVariable :: UpdateVariableResponse -> TestTree
responseUpdateVariable =
  res
    "UpdateVariableResponse"
    "fixture/UpdateVariableResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateVariable)
