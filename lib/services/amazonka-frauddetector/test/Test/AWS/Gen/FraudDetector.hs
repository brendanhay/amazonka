{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.FraudDetector
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.AWS.Gen.FraudDetector where

import Amazonka.FraudDetector
import qualified Data.Proxy as Proxy
import Test.AWS.Fixture
import Test.AWS.FraudDetector.Internal
import Test.AWS.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestCreateDetectorVersion $
--             newCreateDetectorVersion
--
--         , requestBatchGetVariable $
--             newBatchGetVariable
--
--         , requestUpdateModelVersion $
--             newUpdateModelVersion
--
--         , requestDeleteModelVersion $
--             newDeleteModelVersion
--
--         , requestUpdateDetectorVersionMetadata $
--             newUpdateDetectorVersionMetadata
--
--         , requestDeleteBatchImportJob $
--             newDeleteBatchImportJob
--
--         , requestDeleteRule $
--             newDeleteRule
--
--         , requestPutLabel $
--             newPutLabel
--
--         , requestGetExternalModels $
--             newGetExternalModels
--
--         , requestGetDetectors $
--             newGetDetectors
--
--         , requestDeleteLabel $
--             newDeleteLabel
--
--         , requestDeleteVariable $
--             newDeleteVariable
--
--         , requestUpdateVariable $
--             newUpdateVariable
--
--         , requestCreateVariable $
--             newCreateVariable
--
--         , requestCreateBatchImportJob $
--             newCreateBatchImportJob
--
--         , requestCreateRule $
--             newCreateRule
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestGetModels $
--             newGetModels
--
--         , requestUpdateRuleVersion $
--             newUpdateRuleVersion
--
--         , requestDeleteEvent $
--             newDeleteEvent
--
--         , requestCancelBatchPredictionJob $
--             newCancelBatchPredictionJob
--
--         , requestUpdateModelVersionStatus $
--             newUpdateModelVersionStatus
--
--         , requestGetBatchPredictionJobs $
--             newGetBatchPredictionJobs
--
--         , requestCreateModel $
--             newCreateModel
--
--         , requestGetLabels $
--             newGetLabels
--
--         , requestGetModelVersion $
--             newGetModelVersion
--
--         , requestPutExternalModel $
--             newPutExternalModel
--
--         , requestDeleteExternalModel $
--             newDeleteExternalModel
--
--         , requestGetEntityTypes $
--             newGetEntityTypes
--
--         , requestDeleteModel $
--             newDeleteModel
--
--         , requestUpdateModel $
--             newUpdateModel
--
--         , requestCreateModelVersion $
--             newCreateModelVersion
--
--         , requestDeleteEventsByEventType $
--             newDeleteEventsByEventType
--
--         , requestPutKMSEncryptionKey $
--             newPutKMSEncryptionKey
--
--         , requestDescribeDetector $
--             newDescribeDetector
--
--         , requestGetOutcomes $
--             newGetOutcomes
--
--         , requestGetEventPrediction $
--             newGetEventPrediction
--
--         , requestDeleteBatchPredictionJob $
--             newDeleteBatchPredictionJob
--
--         , requestGetEvent $
--             newGetEvent
--
--         , requestUpdateRuleMetadata $
--             newUpdateRuleMetadata
--
--         , requestPutEntityType $
--             newPutEntityType
--
--         , requestCreateBatchPredictionJob $
--             newCreateBatchPredictionJob
--
--         , requestDeleteEntityType $
--             newDeleteEntityType
--
--         , requestDeleteEventType $
--             newDeleteEventType
--
--         , requestPutEventType $
--             newPutEventType
--
--         , requestUpdateDetectorVersionStatus $
--             newUpdateDetectorVersionStatus
--
--         , requestCancelBatchImportJob $
--             newCancelBatchImportJob
--
--         , requestSendEvent $
--             newSendEvent
--
--         , requestTagResource $
--             newTagResource
--
--         , requestGetKMSEncryptionKey $
--             newGetKMSEncryptionKey
--
--         , requestUpdateEventLabel $
--             newUpdateEventLabel
--
--         , requestGetBatchImportJobs $
--             newGetBatchImportJobs
--
--         , requestGetDeleteEventsByEventTypeStatus $
--             newGetDeleteEventsByEventTypeStatus
--
--         , requestDeleteOutcome $
--             newDeleteOutcome
--
--         , requestGetRules $
--             newGetRules
--
--         , requestGetVariables $
--             newGetVariables
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestPutDetector $
--             newPutDetector
--
--         , requestPutOutcome $
--             newPutOutcome
--
--         , requestDeleteDetector $
--             newDeleteDetector
--
--         , requestDescribeModelVersions $
--             newDescribeModelVersions
--
--         , requestBatchCreateVariable $
--             newBatchCreateVariable
--
--         , requestGetDetectorVersion $
--             newGetDetectorVersion
--
--         , requestGetEventTypes $
--             newGetEventTypes
--
--         , requestDeleteDetectorVersion $
--             newDeleteDetectorVersion
--
--         , requestUpdateDetectorVersion $
--             newUpdateDetectorVersion
--
--           ]

--     , testGroup "response"
--         [ responseCreateDetectorVersion $
--             newCreateDetectorVersionResponse
--
--         , responseBatchGetVariable $
--             newBatchGetVariableResponse
--
--         , responseUpdateModelVersion $
--             newUpdateModelVersionResponse
--
--         , responseDeleteModelVersion $
--             newDeleteModelVersionResponse
--
--         , responseUpdateDetectorVersionMetadata $
--             newUpdateDetectorVersionMetadataResponse
--
--         , responseDeleteBatchImportJob $
--             newDeleteBatchImportJobResponse
--
--         , responseDeleteRule $
--             newDeleteRuleResponse
--
--         , responsePutLabel $
--             newPutLabelResponse
--
--         , responseGetExternalModels $
--             newGetExternalModelsResponse
--
--         , responseGetDetectors $
--             newGetDetectorsResponse
--
--         , responseDeleteLabel $
--             newDeleteLabelResponse
--
--         , responseDeleteVariable $
--             newDeleteVariableResponse
--
--         , responseUpdateVariable $
--             newUpdateVariableResponse
--
--         , responseCreateVariable $
--             newCreateVariableResponse
--
--         , responseCreateBatchImportJob $
--             newCreateBatchImportJobResponse
--
--         , responseCreateRule $
--             newCreateRuleResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseGetModels $
--             newGetModelsResponse
--
--         , responseUpdateRuleVersion $
--             newUpdateRuleVersionResponse
--
--         , responseDeleteEvent $
--             newDeleteEventResponse
--
--         , responseCancelBatchPredictionJob $
--             newCancelBatchPredictionJobResponse
--
--         , responseUpdateModelVersionStatus $
--             newUpdateModelVersionStatusResponse
--
--         , responseGetBatchPredictionJobs $
--             newGetBatchPredictionJobsResponse
--
--         , responseCreateModel $
--             newCreateModelResponse
--
--         , responseGetLabels $
--             newGetLabelsResponse
--
--         , responseGetModelVersion $
--             newGetModelVersionResponse
--
--         , responsePutExternalModel $
--             newPutExternalModelResponse
--
--         , responseDeleteExternalModel $
--             newDeleteExternalModelResponse
--
--         , responseGetEntityTypes $
--             newGetEntityTypesResponse
--
--         , responseDeleteModel $
--             newDeleteModelResponse
--
--         , responseUpdateModel $
--             newUpdateModelResponse
--
--         , responseCreateModelVersion $
--             newCreateModelVersionResponse
--
--         , responseDeleteEventsByEventType $
--             newDeleteEventsByEventTypeResponse
--
--         , responsePutKMSEncryptionKey $
--             newPutKMSEncryptionKeyResponse
--
--         , responseDescribeDetector $
--             newDescribeDetectorResponse
--
--         , responseGetOutcomes $
--             newGetOutcomesResponse
--
--         , responseGetEventPrediction $
--             newGetEventPredictionResponse
--
--         , responseDeleteBatchPredictionJob $
--             newDeleteBatchPredictionJobResponse
--
--         , responseGetEvent $
--             newGetEventResponse
--
--         , responseUpdateRuleMetadata $
--             newUpdateRuleMetadataResponse
--
--         , responsePutEntityType $
--             newPutEntityTypeResponse
--
--         , responseCreateBatchPredictionJob $
--             newCreateBatchPredictionJobResponse
--
--         , responseDeleteEntityType $
--             newDeleteEntityTypeResponse
--
--         , responseDeleteEventType $
--             newDeleteEventTypeResponse
--
--         , responsePutEventType $
--             newPutEventTypeResponse
--
--         , responseUpdateDetectorVersionStatus $
--             newUpdateDetectorVersionStatusResponse
--
--         , responseCancelBatchImportJob $
--             newCancelBatchImportJobResponse
--
--         , responseSendEvent $
--             newSendEventResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseGetKMSEncryptionKey $
--             newGetKMSEncryptionKeyResponse
--
--         , responseUpdateEventLabel $
--             newUpdateEventLabelResponse
--
--         , responseGetBatchImportJobs $
--             newGetBatchImportJobsResponse
--
--         , responseGetDeleteEventsByEventTypeStatus $
--             newGetDeleteEventsByEventTypeStatusResponse
--
--         , responseDeleteOutcome $
--             newDeleteOutcomeResponse
--
--         , responseGetRules $
--             newGetRulesResponse
--
--         , responseGetVariables $
--             newGetVariablesResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responsePutDetector $
--             newPutDetectorResponse
--
--         , responsePutOutcome $
--             newPutOutcomeResponse
--
--         , responseDeleteDetector $
--             newDeleteDetectorResponse
--
--         , responseDescribeModelVersions $
--             newDescribeModelVersionsResponse
--
--         , responseBatchCreateVariable $
--             newBatchCreateVariableResponse
--
--         , responseGetDetectorVersion $
--             newGetDetectorVersionResponse
--
--         , responseGetEventTypes $
--             newGetEventTypesResponse
--
--         , responseDeleteDetectorVersion $
--             newDeleteDetectorVersionResponse
--
--         , responseUpdateDetectorVersion $
--             newUpdateDetectorVersionResponse
--
--           ]
--     ]

-- Requests

requestCreateDetectorVersion :: CreateDetectorVersion -> TestTree
requestCreateDetectorVersion =
  req
    "CreateDetectorVersion"
    "fixture/CreateDetectorVersion.yaml"

requestBatchGetVariable :: BatchGetVariable -> TestTree
requestBatchGetVariable =
  req
    "BatchGetVariable"
    "fixture/BatchGetVariable.yaml"

requestUpdateModelVersion :: UpdateModelVersion -> TestTree
requestUpdateModelVersion =
  req
    "UpdateModelVersion"
    "fixture/UpdateModelVersion.yaml"

requestDeleteModelVersion :: DeleteModelVersion -> TestTree
requestDeleteModelVersion =
  req
    "DeleteModelVersion"
    "fixture/DeleteModelVersion.yaml"

requestUpdateDetectorVersionMetadata :: UpdateDetectorVersionMetadata -> TestTree
requestUpdateDetectorVersionMetadata =
  req
    "UpdateDetectorVersionMetadata"
    "fixture/UpdateDetectorVersionMetadata.yaml"

requestDeleteBatchImportJob :: DeleteBatchImportJob -> TestTree
requestDeleteBatchImportJob =
  req
    "DeleteBatchImportJob"
    "fixture/DeleteBatchImportJob.yaml"

requestDeleteRule :: DeleteRule -> TestTree
requestDeleteRule =
  req
    "DeleteRule"
    "fixture/DeleteRule.yaml"

requestPutLabel :: PutLabel -> TestTree
requestPutLabel =
  req
    "PutLabel"
    "fixture/PutLabel.yaml"

requestGetExternalModels :: GetExternalModels -> TestTree
requestGetExternalModels =
  req
    "GetExternalModels"
    "fixture/GetExternalModels.yaml"

requestGetDetectors :: GetDetectors -> TestTree
requestGetDetectors =
  req
    "GetDetectors"
    "fixture/GetDetectors.yaml"

requestDeleteLabel :: DeleteLabel -> TestTree
requestDeleteLabel =
  req
    "DeleteLabel"
    "fixture/DeleteLabel.yaml"

requestDeleteVariable :: DeleteVariable -> TestTree
requestDeleteVariable =
  req
    "DeleteVariable"
    "fixture/DeleteVariable.yaml"

requestUpdateVariable :: UpdateVariable -> TestTree
requestUpdateVariable =
  req
    "UpdateVariable"
    "fixture/UpdateVariable.yaml"

requestCreateVariable :: CreateVariable -> TestTree
requestCreateVariable =
  req
    "CreateVariable"
    "fixture/CreateVariable.yaml"

requestCreateBatchImportJob :: CreateBatchImportJob -> TestTree
requestCreateBatchImportJob =
  req
    "CreateBatchImportJob"
    "fixture/CreateBatchImportJob.yaml"

requestCreateRule :: CreateRule -> TestTree
requestCreateRule =
  req
    "CreateRule"
    "fixture/CreateRule.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestGetModels :: GetModels -> TestTree
requestGetModels =
  req
    "GetModels"
    "fixture/GetModels.yaml"

requestUpdateRuleVersion :: UpdateRuleVersion -> TestTree
requestUpdateRuleVersion =
  req
    "UpdateRuleVersion"
    "fixture/UpdateRuleVersion.yaml"

requestDeleteEvent :: DeleteEvent -> TestTree
requestDeleteEvent =
  req
    "DeleteEvent"
    "fixture/DeleteEvent.yaml"

requestCancelBatchPredictionJob :: CancelBatchPredictionJob -> TestTree
requestCancelBatchPredictionJob =
  req
    "CancelBatchPredictionJob"
    "fixture/CancelBatchPredictionJob.yaml"

requestUpdateModelVersionStatus :: UpdateModelVersionStatus -> TestTree
requestUpdateModelVersionStatus =
  req
    "UpdateModelVersionStatus"
    "fixture/UpdateModelVersionStatus.yaml"

requestGetBatchPredictionJobs :: GetBatchPredictionJobs -> TestTree
requestGetBatchPredictionJobs =
  req
    "GetBatchPredictionJobs"
    "fixture/GetBatchPredictionJobs.yaml"

requestCreateModel :: CreateModel -> TestTree
requestCreateModel =
  req
    "CreateModel"
    "fixture/CreateModel.yaml"

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

requestPutExternalModel :: PutExternalModel -> TestTree
requestPutExternalModel =
  req
    "PutExternalModel"
    "fixture/PutExternalModel.yaml"

requestDeleteExternalModel :: DeleteExternalModel -> TestTree
requestDeleteExternalModel =
  req
    "DeleteExternalModel"
    "fixture/DeleteExternalModel.yaml"

requestGetEntityTypes :: GetEntityTypes -> TestTree
requestGetEntityTypes =
  req
    "GetEntityTypes"
    "fixture/GetEntityTypes.yaml"

requestDeleteModel :: DeleteModel -> TestTree
requestDeleteModel =
  req
    "DeleteModel"
    "fixture/DeleteModel.yaml"

requestUpdateModel :: UpdateModel -> TestTree
requestUpdateModel =
  req
    "UpdateModel"
    "fixture/UpdateModel.yaml"

requestCreateModelVersion :: CreateModelVersion -> TestTree
requestCreateModelVersion =
  req
    "CreateModelVersion"
    "fixture/CreateModelVersion.yaml"

requestDeleteEventsByEventType :: DeleteEventsByEventType -> TestTree
requestDeleteEventsByEventType =
  req
    "DeleteEventsByEventType"
    "fixture/DeleteEventsByEventType.yaml"

requestPutKMSEncryptionKey :: PutKMSEncryptionKey -> TestTree
requestPutKMSEncryptionKey =
  req
    "PutKMSEncryptionKey"
    "fixture/PutKMSEncryptionKey.yaml"

requestDescribeDetector :: DescribeDetector -> TestTree
requestDescribeDetector =
  req
    "DescribeDetector"
    "fixture/DescribeDetector.yaml"

requestGetOutcomes :: GetOutcomes -> TestTree
requestGetOutcomes =
  req
    "GetOutcomes"
    "fixture/GetOutcomes.yaml"

requestGetEventPrediction :: GetEventPrediction -> TestTree
requestGetEventPrediction =
  req
    "GetEventPrediction"
    "fixture/GetEventPrediction.yaml"

requestDeleteBatchPredictionJob :: DeleteBatchPredictionJob -> TestTree
requestDeleteBatchPredictionJob =
  req
    "DeleteBatchPredictionJob"
    "fixture/DeleteBatchPredictionJob.yaml"

requestGetEvent :: GetEvent -> TestTree
requestGetEvent =
  req
    "GetEvent"
    "fixture/GetEvent.yaml"

requestUpdateRuleMetadata :: UpdateRuleMetadata -> TestTree
requestUpdateRuleMetadata =
  req
    "UpdateRuleMetadata"
    "fixture/UpdateRuleMetadata.yaml"

requestPutEntityType :: PutEntityType -> TestTree
requestPutEntityType =
  req
    "PutEntityType"
    "fixture/PutEntityType.yaml"

requestCreateBatchPredictionJob :: CreateBatchPredictionJob -> TestTree
requestCreateBatchPredictionJob =
  req
    "CreateBatchPredictionJob"
    "fixture/CreateBatchPredictionJob.yaml"

requestDeleteEntityType :: DeleteEntityType -> TestTree
requestDeleteEntityType =
  req
    "DeleteEntityType"
    "fixture/DeleteEntityType.yaml"

requestDeleteEventType :: DeleteEventType -> TestTree
requestDeleteEventType =
  req
    "DeleteEventType"
    "fixture/DeleteEventType.yaml"

requestPutEventType :: PutEventType -> TestTree
requestPutEventType =
  req
    "PutEventType"
    "fixture/PutEventType.yaml"

requestUpdateDetectorVersionStatus :: UpdateDetectorVersionStatus -> TestTree
requestUpdateDetectorVersionStatus =
  req
    "UpdateDetectorVersionStatus"
    "fixture/UpdateDetectorVersionStatus.yaml"

requestCancelBatchImportJob :: CancelBatchImportJob -> TestTree
requestCancelBatchImportJob =
  req
    "CancelBatchImportJob"
    "fixture/CancelBatchImportJob.yaml"

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

requestGetKMSEncryptionKey :: GetKMSEncryptionKey -> TestTree
requestGetKMSEncryptionKey =
  req
    "GetKMSEncryptionKey"
    "fixture/GetKMSEncryptionKey.yaml"

requestUpdateEventLabel :: UpdateEventLabel -> TestTree
requestUpdateEventLabel =
  req
    "UpdateEventLabel"
    "fixture/UpdateEventLabel.yaml"

requestGetBatchImportJobs :: GetBatchImportJobs -> TestTree
requestGetBatchImportJobs =
  req
    "GetBatchImportJobs"
    "fixture/GetBatchImportJobs.yaml"

requestGetDeleteEventsByEventTypeStatus :: GetDeleteEventsByEventTypeStatus -> TestTree
requestGetDeleteEventsByEventTypeStatus =
  req
    "GetDeleteEventsByEventTypeStatus"
    "fixture/GetDeleteEventsByEventTypeStatus.yaml"

requestDeleteOutcome :: DeleteOutcome -> TestTree
requestDeleteOutcome =
  req
    "DeleteOutcome"
    "fixture/DeleteOutcome.yaml"

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

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestPutDetector :: PutDetector -> TestTree
requestPutDetector =
  req
    "PutDetector"
    "fixture/PutDetector.yaml"

requestPutOutcome :: PutOutcome -> TestTree
requestPutOutcome =
  req
    "PutOutcome"
    "fixture/PutOutcome.yaml"

requestDeleteDetector :: DeleteDetector -> TestTree
requestDeleteDetector =
  req
    "DeleteDetector"
    "fixture/DeleteDetector.yaml"

requestDescribeModelVersions :: DescribeModelVersions -> TestTree
requestDescribeModelVersions =
  req
    "DescribeModelVersions"
    "fixture/DescribeModelVersions.yaml"

requestBatchCreateVariable :: BatchCreateVariable -> TestTree
requestBatchCreateVariable =
  req
    "BatchCreateVariable"
    "fixture/BatchCreateVariable.yaml"

requestGetDetectorVersion :: GetDetectorVersion -> TestTree
requestGetDetectorVersion =
  req
    "GetDetectorVersion"
    "fixture/GetDetectorVersion.yaml"

requestGetEventTypes :: GetEventTypes -> TestTree
requestGetEventTypes =
  req
    "GetEventTypes"
    "fixture/GetEventTypes.yaml"

requestDeleteDetectorVersion :: DeleteDetectorVersion -> TestTree
requestDeleteDetectorVersion =
  req
    "DeleteDetectorVersion"
    "fixture/DeleteDetectorVersion.yaml"

requestUpdateDetectorVersion :: UpdateDetectorVersion -> TestTree
requestUpdateDetectorVersion =
  req
    "UpdateDetectorVersion"
    "fixture/UpdateDetectorVersion.yaml"

-- Responses

responseCreateDetectorVersion :: CreateDetectorVersionResponse -> TestTree
responseCreateDetectorVersion =
  res
    "CreateDetectorVersionResponse"
    "fixture/CreateDetectorVersionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateDetectorVersion)

responseBatchGetVariable :: BatchGetVariableResponse -> TestTree
responseBatchGetVariable =
  res
    "BatchGetVariableResponse"
    "fixture/BatchGetVariableResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchGetVariable)

responseUpdateModelVersion :: UpdateModelVersionResponse -> TestTree
responseUpdateModelVersion =
  res
    "UpdateModelVersionResponse"
    "fixture/UpdateModelVersionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateModelVersion)

responseDeleteModelVersion :: DeleteModelVersionResponse -> TestTree
responseDeleteModelVersion =
  res
    "DeleteModelVersionResponse"
    "fixture/DeleteModelVersionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteModelVersion)

responseUpdateDetectorVersionMetadata :: UpdateDetectorVersionMetadataResponse -> TestTree
responseUpdateDetectorVersionMetadata =
  res
    "UpdateDetectorVersionMetadataResponse"
    "fixture/UpdateDetectorVersionMetadataResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateDetectorVersionMetadata)

responseDeleteBatchImportJob :: DeleteBatchImportJobResponse -> TestTree
responseDeleteBatchImportJob =
  res
    "DeleteBatchImportJobResponse"
    "fixture/DeleteBatchImportJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteBatchImportJob)

responseDeleteRule :: DeleteRuleResponse -> TestTree
responseDeleteRule =
  res
    "DeleteRuleResponse"
    "fixture/DeleteRuleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteRule)

responsePutLabel :: PutLabelResponse -> TestTree
responsePutLabel =
  res
    "PutLabelResponse"
    "fixture/PutLabelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutLabel)

responseGetExternalModels :: GetExternalModelsResponse -> TestTree
responseGetExternalModels =
  res
    "GetExternalModelsResponse"
    "fixture/GetExternalModelsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetExternalModels)

responseGetDetectors :: GetDetectorsResponse -> TestTree
responseGetDetectors =
  res
    "GetDetectorsResponse"
    "fixture/GetDetectorsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetDetectors)

responseDeleteLabel :: DeleteLabelResponse -> TestTree
responseDeleteLabel =
  res
    "DeleteLabelResponse"
    "fixture/DeleteLabelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteLabel)

responseDeleteVariable :: DeleteVariableResponse -> TestTree
responseDeleteVariable =
  res
    "DeleteVariableResponse"
    "fixture/DeleteVariableResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteVariable)

responseUpdateVariable :: UpdateVariableResponse -> TestTree
responseUpdateVariable =
  res
    "UpdateVariableResponse"
    "fixture/UpdateVariableResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateVariable)

responseCreateVariable :: CreateVariableResponse -> TestTree
responseCreateVariable =
  res
    "CreateVariableResponse"
    "fixture/CreateVariableResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateVariable)

responseCreateBatchImportJob :: CreateBatchImportJobResponse -> TestTree
responseCreateBatchImportJob =
  res
    "CreateBatchImportJobResponse"
    "fixture/CreateBatchImportJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateBatchImportJob)

responseCreateRule :: CreateRuleResponse -> TestTree
responseCreateRule =
  res
    "CreateRuleResponse"
    "fixture/CreateRuleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateRule)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseGetModels :: GetModelsResponse -> TestTree
responseGetModels =
  res
    "GetModelsResponse"
    "fixture/GetModelsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetModels)

responseUpdateRuleVersion :: UpdateRuleVersionResponse -> TestTree
responseUpdateRuleVersion =
  res
    "UpdateRuleVersionResponse"
    "fixture/UpdateRuleVersionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateRuleVersion)

responseDeleteEvent :: DeleteEventResponse -> TestTree
responseDeleteEvent =
  res
    "DeleteEventResponse"
    "fixture/DeleteEventResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteEvent)

responseCancelBatchPredictionJob :: CancelBatchPredictionJobResponse -> TestTree
responseCancelBatchPredictionJob =
  res
    "CancelBatchPredictionJobResponse"
    "fixture/CancelBatchPredictionJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CancelBatchPredictionJob)

responseUpdateModelVersionStatus :: UpdateModelVersionStatusResponse -> TestTree
responseUpdateModelVersionStatus =
  res
    "UpdateModelVersionStatusResponse"
    "fixture/UpdateModelVersionStatusResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateModelVersionStatus)

responseGetBatchPredictionJobs :: GetBatchPredictionJobsResponse -> TestTree
responseGetBatchPredictionJobs =
  res
    "GetBatchPredictionJobsResponse"
    "fixture/GetBatchPredictionJobsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetBatchPredictionJobs)

responseCreateModel :: CreateModelResponse -> TestTree
responseCreateModel =
  res
    "CreateModelResponse"
    "fixture/CreateModelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateModel)

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

responsePutExternalModel :: PutExternalModelResponse -> TestTree
responsePutExternalModel =
  res
    "PutExternalModelResponse"
    "fixture/PutExternalModelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutExternalModel)

responseDeleteExternalModel :: DeleteExternalModelResponse -> TestTree
responseDeleteExternalModel =
  res
    "DeleteExternalModelResponse"
    "fixture/DeleteExternalModelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteExternalModel)

responseGetEntityTypes :: GetEntityTypesResponse -> TestTree
responseGetEntityTypes =
  res
    "GetEntityTypesResponse"
    "fixture/GetEntityTypesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetEntityTypes)

responseDeleteModel :: DeleteModelResponse -> TestTree
responseDeleteModel =
  res
    "DeleteModelResponse"
    "fixture/DeleteModelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteModel)

responseUpdateModel :: UpdateModelResponse -> TestTree
responseUpdateModel =
  res
    "UpdateModelResponse"
    "fixture/UpdateModelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateModel)

responseCreateModelVersion :: CreateModelVersionResponse -> TestTree
responseCreateModelVersion =
  res
    "CreateModelVersionResponse"
    "fixture/CreateModelVersionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateModelVersion)

responseDeleteEventsByEventType :: DeleteEventsByEventTypeResponse -> TestTree
responseDeleteEventsByEventType =
  res
    "DeleteEventsByEventTypeResponse"
    "fixture/DeleteEventsByEventTypeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteEventsByEventType)

responsePutKMSEncryptionKey :: PutKMSEncryptionKeyResponse -> TestTree
responsePutKMSEncryptionKey =
  res
    "PutKMSEncryptionKeyResponse"
    "fixture/PutKMSEncryptionKeyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutKMSEncryptionKey)

responseDescribeDetector :: DescribeDetectorResponse -> TestTree
responseDescribeDetector =
  res
    "DescribeDetectorResponse"
    "fixture/DescribeDetectorResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeDetector)

responseGetOutcomes :: GetOutcomesResponse -> TestTree
responseGetOutcomes =
  res
    "GetOutcomesResponse"
    "fixture/GetOutcomesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetOutcomes)

responseGetEventPrediction :: GetEventPredictionResponse -> TestTree
responseGetEventPrediction =
  res
    "GetEventPredictionResponse"
    "fixture/GetEventPredictionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetEventPrediction)

responseDeleteBatchPredictionJob :: DeleteBatchPredictionJobResponse -> TestTree
responseDeleteBatchPredictionJob =
  res
    "DeleteBatchPredictionJobResponse"
    "fixture/DeleteBatchPredictionJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteBatchPredictionJob)

responseGetEvent :: GetEventResponse -> TestTree
responseGetEvent =
  res
    "GetEventResponse"
    "fixture/GetEventResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetEvent)

responseUpdateRuleMetadata :: UpdateRuleMetadataResponse -> TestTree
responseUpdateRuleMetadata =
  res
    "UpdateRuleMetadataResponse"
    "fixture/UpdateRuleMetadataResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateRuleMetadata)

responsePutEntityType :: PutEntityTypeResponse -> TestTree
responsePutEntityType =
  res
    "PutEntityTypeResponse"
    "fixture/PutEntityTypeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutEntityType)

responseCreateBatchPredictionJob :: CreateBatchPredictionJobResponse -> TestTree
responseCreateBatchPredictionJob =
  res
    "CreateBatchPredictionJobResponse"
    "fixture/CreateBatchPredictionJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateBatchPredictionJob)

responseDeleteEntityType :: DeleteEntityTypeResponse -> TestTree
responseDeleteEntityType =
  res
    "DeleteEntityTypeResponse"
    "fixture/DeleteEntityTypeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteEntityType)

responseDeleteEventType :: DeleteEventTypeResponse -> TestTree
responseDeleteEventType =
  res
    "DeleteEventTypeResponse"
    "fixture/DeleteEventTypeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteEventType)

responsePutEventType :: PutEventTypeResponse -> TestTree
responsePutEventType =
  res
    "PutEventTypeResponse"
    "fixture/PutEventTypeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutEventType)

responseUpdateDetectorVersionStatus :: UpdateDetectorVersionStatusResponse -> TestTree
responseUpdateDetectorVersionStatus =
  res
    "UpdateDetectorVersionStatusResponse"
    "fixture/UpdateDetectorVersionStatusResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateDetectorVersionStatus)

responseCancelBatchImportJob :: CancelBatchImportJobResponse -> TestTree
responseCancelBatchImportJob =
  res
    "CancelBatchImportJobResponse"
    "fixture/CancelBatchImportJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CancelBatchImportJob)

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

responseGetKMSEncryptionKey :: GetKMSEncryptionKeyResponse -> TestTree
responseGetKMSEncryptionKey =
  res
    "GetKMSEncryptionKeyResponse"
    "fixture/GetKMSEncryptionKeyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetKMSEncryptionKey)

responseUpdateEventLabel :: UpdateEventLabelResponse -> TestTree
responseUpdateEventLabel =
  res
    "UpdateEventLabelResponse"
    "fixture/UpdateEventLabelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateEventLabel)

responseGetBatchImportJobs :: GetBatchImportJobsResponse -> TestTree
responseGetBatchImportJobs =
  res
    "GetBatchImportJobsResponse"
    "fixture/GetBatchImportJobsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetBatchImportJobs)

responseGetDeleteEventsByEventTypeStatus :: GetDeleteEventsByEventTypeStatusResponse -> TestTree
responseGetDeleteEventsByEventTypeStatus =
  res
    "GetDeleteEventsByEventTypeStatusResponse"
    "fixture/GetDeleteEventsByEventTypeStatusResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetDeleteEventsByEventTypeStatus)

responseDeleteOutcome :: DeleteOutcomeResponse -> TestTree
responseDeleteOutcome =
  res
    "DeleteOutcomeResponse"
    "fixture/DeleteOutcomeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteOutcome)

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

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UntagResource)

responsePutDetector :: PutDetectorResponse -> TestTree
responsePutDetector =
  res
    "PutDetectorResponse"
    "fixture/PutDetectorResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutDetector)

responsePutOutcome :: PutOutcomeResponse -> TestTree
responsePutOutcome =
  res
    "PutOutcomeResponse"
    "fixture/PutOutcomeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutOutcome)

responseDeleteDetector :: DeleteDetectorResponse -> TestTree
responseDeleteDetector =
  res
    "DeleteDetectorResponse"
    "fixture/DeleteDetectorResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteDetector)

responseDescribeModelVersions :: DescribeModelVersionsResponse -> TestTree
responseDescribeModelVersions =
  res
    "DescribeModelVersionsResponse"
    "fixture/DescribeModelVersionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeModelVersions)

responseBatchCreateVariable :: BatchCreateVariableResponse -> TestTree
responseBatchCreateVariable =
  res
    "BatchCreateVariableResponse"
    "fixture/BatchCreateVariableResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchCreateVariable)

responseGetDetectorVersion :: GetDetectorVersionResponse -> TestTree
responseGetDetectorVersion =
  res
    "GetDetectorVersionResponse"
    "fixture/GetDetectorVersionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetDetectorVersion)

responseGetEventTypes :: GetEventTypesResponse -> TestTree
responseGetEventTypes =
  res
    "GetEventTypesResponse"
    "fixture/GetEventTypesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetEventTypes)

responseDeleteDetectorVersion :: DeleteDetectorVersionResponse -> TestTree
responseDeleteDetectorVersion =
  res
    "DeleteDetectorVersionResponse"
    "fixture/DeleteDetectorVersionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteDetectorVersion)

responseUpdateDetectorVersion :: UpdateDetectorVersionResponse -> TestTree
responseUpdateDetectorVersion =
  res
    "UpdateDetectorVersionResponse"
    "fixture/UpdateDetectorVersionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateDetectorVersion)
