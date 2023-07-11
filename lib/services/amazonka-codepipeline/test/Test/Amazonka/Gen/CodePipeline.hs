{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.CodePipeline
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.CodePipeline where

import Amazonka.CodePipeline
import qualified Data.Proxy as Proxy
import Test.Amazonka.CodePipeline.Internal
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
--         [ requestAcknowledgeJob $
--             newAcknowledgeJob
--
--         , requestAcknowledgeThirdPartyJob $
--             newAcknowledgeThirdPartyJob
--
--         , requestCreateCustomActionType $
--             newCreateCustomActionType
--
--         , requestCreatePipeline $
--             newCreatePipeline
--
--         , requestDeleteCustomActionType $
--             newDeleteCustomActionType
--
--         , requestDeletePipeline $
--             newDeletePipeline
--
--         , requestDeleteWebhook $
--             newDeleteWebhook
--
--         , requestDeregisterWebhookWithThirdParty $
--             newDeregisterWebhookWithThirdParty
--
--         , requestDisableStageTransition $
--             newDisableStageTransition
--
--         , requestEnableStageTransition $
--             newEnableStageTransition
--
--         , requestGetActionType $
--             newGetActionType
--
--         , requestGetJobDetails $
--             newGetJobDetails
--
--         , requestGetPipeline $
--             newGetPipeline
--
--         , requestGetPipelineExecution $
--             newGetPipelineExecution
--
--         , requestGetPipelineState $
--             newGetPipelineState
--
--         , requestGetThirdPartyJobDetails $
--             newGetThirdPartyJobDetails
--
--         , requestListActionExecutions $
--             newListActionExecutions
--
--         , requestListActionTypes $
--             newListActionTypes
--
--         , requestListPipelineExecutions $
--             newListPipelineExecutions
--
--         , requestListPipelines $
--             newListPipelines
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestListWebhooks $
--             newListWebhooks
--
--         , requestPollForJobs $
--             newPollForJobs
--
--         , requestPollForThirdPartyJobs $
--             newPollForThirdPartyJobs
--
--         , requestPutActionRevision $
--             newPutActionRevision
--
--         , requestPutApprovalResult $
--             newPutApprovalResult
--
--         , requestPutJobFailureResult $
--             newPutJobFailureResult
--
--         , requestPutJobSuccessResult $
--             newPutJobSuccessResult
--
--         , requestPutThirdPartyJobFailureResult $
--             newPutThirdPartyJobFailureResult
--
--         , requestPutThirdPartyJobSuccessResult $
--             newPutThirdPartyJobSuccessResult
--
--         , requestPutWebhook $
--             newPutWebhook
--
--         , requestRegisterWebhookWithThirdParty $
--             newRegisterWebhookWithThirdParty
--
--         , requestRetryStageExecution $
--             newRetryStageExecution
--
--         , requestStartPipelineExecution $
--             newStartPipelineExecution
--
--         , requestStopPipelineExecution $
--             newStopPipelineExecution
--
--         , requestTagResource $
--             newTagResource
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestUpdateActionType $
--             newUpdateActionType
--
--         , requestUpdatePipeline $
--             newUpdatePipeline
--
--           ]

--     , testGroup "response"
--         [ responseAcknowledgeJob $
--             newAcknowledgeJobResponse
--
--         , responseAcknowledgeThirdPartyJob $
--             newAcknowledgeThirdPartyJobResponse
--
--         , responseCreateCustomActionType $
--             newCreateCustomActionTypeResponse
--
--         , responseCreatePipeline $
--             newCreatePipelineResponse
--
--         , responseDeleteCustomActionType $
--             newDeleteCustomActionTypeResponse
--
--         , responseDeletePipeline $
--             newDeletePipelineResponse
--
--         , responseDeleteWebhook $
--             newDeleteWebhookResponse
--
--         , responseDeregisterWebhookWithThirdParty $
--             newDeregisterWebhookWithThirdPartyResponse
--
--         , responseDisableStageTransition $
--             newDisableStageTransitionResponse
--
--         , responseEnableStageTransition $
--             newEnableStageTransitionResponse
--
--         , responseGetActionType $
--             newGetActionTypeResponse
--
--         , responseGetJobDetails $
--             newGetJobDetailsResponse
--
--         , responseGetPipeline $
--             newGetPipelineResponse
--
--         , responseGetPipelineExecution $
--             newGetPipelineExecutionResponse
--
--         , responseGetPipelineState $
--             newGetPipelineStateResponse
--
--         , responseGetThirdPartyJobDetails $
--             newGetThirdPartyJobDetailsResponse
--
--         , responseListActionExecutions $
--             newListActionExecutionsResponse
--
--         , responseListActionTypes $
--             newListActionTypesResponse
--
--         , responseListPipelineExecutions $
--             newListPipelineExecutionsResponse
--
--         , responseListPipelines $
--             newListPipelinesResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseListWebhooks $
--             newListWebhooksResponse
--
--         , responsePollForJobs $
--             newPollForJobsResponse
--
--         , responsePollForThirdPartyJobs $
--             newPollForThirdPartyJobsResponse
--
--         , responsePutActionRevision $
--             newPutActionRevisionResponse
--
--         , responsePutApprovalResult $
--             newPutApprovalResultResponse
--
--         , responsePutJobFailureResult $
--             newPutJobFailureResultResponse
--
--         , responsePutJobSuccessResult $
--             newPutJobSuccessResultResponse
--
--         , responsePutThirdPartyJobFailureResult $
--             newPutThirdPartyJobFailureResultResponse
--
--         , responsePutThirdPartyJobSuccessResult $
--             newPutThirdPartyJobSuccessResultResponse
--
--         , responsePutWebhook $
--             newPutWebhookResponse
--
--         , responseRegisterWebhookWithThirdParty $
--             newRegisterWebhookWithThirdPartyResponse
--
--         , responseRetryStageExecution $
--             newRetryStageExecutionResponse
--
--         , responseStartPipelineExecution $
--             newStartPipelineExecutionResponse
--
--         , responseStopPipelineExecution $
--             newStopPipelineExecutionResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseUpdateActionType $
--             newUpdateActionTypeResponse
--
--         , responseUpdatePipeline $
--             newUpdatePipelineResponse
--
--           ]
--     ]

-- Requests

requestAcknowledgeJob :: AcknowledgeJob -> TestTree
requestAcknowledgeJob =
  req
    "AcknowledgeJob"
    "fixture/AcknowledgeJob.yaml"

requestAcknowledgeThirdPartyJob :: AcknowledgeThirdPartyJob -> TestTree
requestAcknowledgeThirdPartyJob =
  req
    "AcknowledgeThirdPartyJob"
    "fixture/AcknowledgeThirdPartyJob.yaml"

requestCreateCustomActionType :: CreateCustomActionType -> TestTree
requestCreateCustomActionType =
  req
    "CreateCustomActionType"
    "fixture/CreateCustomActionType.yaml"

requestCreatePipeline :: CreatePipeline -> TestTree
requestCreatePipeline =
  req
    "CreatePipeline"
    "fixture/CreatePipeline.yaml"

requestDeleteCustomActionType :: DeleteCustomActionType -> TestTree
requestDeleteCustomActionType =
  req
    "DeleteCustomActionType"
    "fixture/DeleteCustomActionType.yaml"

requestDeletePipeline :: DeletePipeline -> TestTree
requestDeletePipeline =
  req
    "DeletePipeline"
    "fixture/DeletePipeline.yaml"

requestDeleteWebhook :: DeleteWebhook -> TestTree
requestDeleteWebhook =
  req
    "DeleteWebhook"
    "fixture/DeleteWebhook.yaml"

requestDeregisterWebhookWithThirdParty :: DeregisterWebhookWithThirdParty -> TestTree
requestDeregisterWebhookWithThirdParty =
  req
    "DeregisterWebhookWithThirdParty"
    "fixture/DeregisterWebhookWithThirdParty.yaml"

requestDisableStageTransition :: DisableStageTransition -> TestTree
requestDisableStageTransition =
  req
    "DisableStageTransition"
    "fixture/DisableStageTransition.yaml"

requestEnableStageTransition :: EnableStageTransition -> TestTree
requestEnableStageTransition =
  req
    "EnableStageTransition"
    "fixture/EnableStageTransition.yaml"

requestGetActionType :: GetActionType -> TestTree
requestGetActionType =
  req
    "GetActionType"
    "fixture/GetActionType.yaml"

requestGetJobDetails :: GetJobDetails -> TestTree
requestGetJobDetails =
  req
    "GetJobDetails"
    "fixture/GetJobDetails.yaml"

requestGetPipeline :: GetPipeline -> TestTree
requestGetPipeline =
  req
    "GetPipeline"
    "fixture/GetPipeline.yaml"

requestGetPipelineExecution :: GetPipelineExecution -> TestTree
requestGetPipelineExecution =
  req
    "GetPipelineExecution"
    "fixture/GetPipelineExecution.yaml"

requestGetPipelineState :: GetPipelineState -> TestTree
requestGetPipelineState =
  req
    "GetPipelineState"
    "fixture/GetPipelineState.yaml"

requestGetThirdPartyJobDetails :: GetThirdPartyJobDetails -> TestTree
requestGetThirdPartyJobDetails =
  req
    "GetThirdPartyJobDetails"
    "fixture/GetThirdPartyJobDetails.yaml"

requestListActionExecutions :: ListActionExecutions -> TestTree
requestListActionExecutions =
  req
    "ListActionExecutions"
    "fixture/ListActionExecutions.yaml"

requestListActionTypes :: ListActionTypes -> TestTree
requestListActionTypes =
  req
    "ListActionTypes"
    "fixture/ListActionTypes.yaml"

requestListPipelineExecutions :: ListPipelineExecutions -> TestTree
requestListPipelineExecutions =
  req
    "ListPipelineExecutions"
    "fixture/ListPipelineExecutions.yaml"

requestListPipelines :: ListPipelines -> TestTree
requestListPipelines =
  req
    "ListPipelines"
    "fixture/ListPipelines.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestListWebhooks :: ListWebhooks -> TestTree
requestListWebhooks =
  req
    "ListWebhooks"
    "fixture/ListWebhooks.yaml"

requestPollForJobs :: PollForJobs -> TestTree
requestPollForJobs =
  req
    "PollForJobs"
    "fixture/PollForJobs.yaml"

requestPollForThirdPartyJobs :: PollForThirdPartyJobs -> TestTree
requestPollForThirdPartyJobs =
  req
    "PollForThirdPartyJobs"
    "fixture/PollForThirdPartyJobs.yaml"

requestPutActionRevision :: PutActionRevision -> TestTree
requestPutActionRevision =
  req
    "PutActionRevision"
    "fixture/PutActionRevision.yaml"

requestPutApprovalResult :: PutApprovalResult -> TestTree
requestPutApprovalResult =
  req
    "PutApprovalResult"
    "fixture/PutApprovalResult.yaml"

requestPutJobFailureResult :: PutJobFailureResult -> TestTree
requestPutJobFailureResult =
  req
    "PutJobFailureResult"
    "fixture/PutJobFailureResult.yaml"

requestPutJobSuccessResult :: PutJobSuccessResult -> TestTree
requestPutJobSuccessResult =
  req
    "PutJobSuccessResult"
    "fixture/PutJobSuccessResult.yaml"

requestPutThirdPartyJobFailureResult :: PutThirdPartyJobFailureResult -> TestTree
requestPutThirdPartyJobFailureResult =
  req
    "PutThirdPartyJobFailureResult"
    "fixture/PutThirdPartyJobFailureResult.yaml"

requestPutThirdPartyJobSuccessResult :: PutThirdPartyJobSuccessResult -> TestTree
requestPutThirdPartyJobSuccessResult =
  req
    "PutThirdPartyJobSuccessResult"
    "fixture/PutThirdPartyJobSuccessResult.yaml"

requestPutWebhook :: PutWebhook -> TestTree
requestPutWebhook =
  req
    "PutWebhook"
    "fixture/PutWebhook.yaml"

requestRegisterWebhookWithThirdParty :: RegisterWebhookWithThirdParty -> TestTree
requestRegisterWebhookWithThirdParty =
  req
    "RegisterWebhookWithThirdParty"
    "fixture/RegisterWebhookWithThirdParty.yaml"

requestRetryStageExecution :: RetryStageExecution -> TestTree
requestRetryStageExecution =
  req
    "RetryStageExecution"
    "fixture/RetryStageExecution.yaml"

requestStartPipelineExecution :: StartPipelineExecution -> TestTree
requestStartPipelineExecution =
  req
    "StartPipelineExecution"
    "fixture/StartPipelineExecution.yaml"

requestStopPipelineExecution :: StopPipelineExecution -> TestTree
requestStopPipelineExecution =
  req
    "StopPipelineExecution"
    "fixture/StopPipelineExecution.yaml"

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

requestUpdateActionType :: UpdateActionType -> TestTree
requestUpdateActionType =
  req
    "UpdateActionType"
    "fixture/UpdateActionType.yaml"

requestUpdatePipeline :: UpdatePipeline -> TestTree
requestUpdatePipeline =
  req
    "UpdatePipeline"
    "fixture/UpdatePipeline.yaml"

-- Responses

responseAcknowledgeJob :: AcknowledgeJobResponse -> TestTree
responseAcknowledgeJob =
  res
    "AcknowledgeJobResponse"
    "fixture/AcknowledgeJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AcknowledgeJob)

responseAcknowledgeThirdPartyJob :: AcknowledgeThirdPartyJobResponse -> TestTree
responseAcknowledgeThirdPartyJob =
  res
    "AcknowledgeThirdPartyJobResponse"
    "fixture/AcknowledgeThirdPartyJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AcknowledgeThirdPartyJob)

responseCreateCustomActionType :: CreateCustomActionTypeResponse -> TestTree
responseCreateCustomActionType =
  res
    "CreateCustomActionTypeResponse"
    "fixture/CreateCustomActionTypeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateCustomActionType)

responseCreatePipeline :: CreatePipelineResponse -> TestTree
responseCreatePipeline =
  res
    "CreatePipelineResponse"
    "fixture/CreatePipelineResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreatePipeline)

responseDeleteCustomActionType :: DeleteCustomActionTypeResponse -> TestTree
responseDeleteCustomActionType =
  res
    "DeleteCustomActionTypeResponse"
    "fixture/DeleteCustomActionTypeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteCustomActionType)

responseDeletePipeline :: DeletePipelineResponse -> TestTree
responseDeletePipeline =
  res
    "DeletePipelineResponse"
    "fixture/DeletePipelineResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeletePipeline)

responseDeleteWebhook :: DeleteWebhookResponse -> TestTree
responseDeleteWebhook =
  res
    "DeleteWebhookResponse"
    "fixture/DeleteWebhookResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteWebhook)

responseDeregisterWebhookWithThirdParty :: DeregisterWebhookWithThirdPartyResponse -> TestTree
responseDeregisterWebhookWithThirdParty =
  res
    "DeregisterWebhookWithThirdPartyResponse"
    "fixture/DeregisterWebhookWithThirdPartyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeregisterWebhookWithThirdParty)

responseDisableStageTransition :: DisableStageTransitionResponse -> TestTree
responseDisableStageTransition =
  res
    "DisableStageTransitionResponse"
    "fixture/DisableStageTransitionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisableStageTransition)

responseEnableStageTransition :: EnableStageTransitionResponse -> TestTree
responseEnableStageTransition =
  res
    "EnableStageTransitionResponse"
    "fixture/EnableStageTransitionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy EnableStageTransition)

responseGetActionType :: GetActionTypeResponse -> TestTree
responseGetActionType =
  res
    "GetActionTypeResponse"
    "fixture/GetActionTypeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetActionType)

responseGetJobDetails :: GetJobDetailsResponse -> TestTree
responseGetJobDetails =
  res
    "GetJobDetailsResponse"
    "fixture/GetJobDetailsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetJobDetails)

responseGetPipeline :: GetPipelineResponse -> TestTree
responseGetPipeline =
  res
    "GetPipelineResponse"
    "fixture/GetPipelineResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetPipeline)

responseGetPipelineExecution :: GetPipelineExecutionResponse -> TestTree
responseGetPipelineExecution =
  res
    "GetPipelineExecutionResponse"
    "fixture/GetPipelineExecutionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetPipelineExecution)

responseGetPipelineState :: GetPipelineStateResponse -> TestTree
responseGetPipelineState =
  res
    "GetPipelineStateResponse"
    "fixture/GetPipelineStateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetPipelineState)

responseGetThirdPartyJobDetails :: GetThirdPartyJobDetailsResponse -> TestTree
responseGetThirdPartyJobDetails =
  res
    "GetThirdPartyJobDetailsResponse"
    "fixture/GetThirdPartyJobDetailsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetThirdPartyJobDetails)

responseListActionExecutions :: ListActionExecutionsResponse -> TestTree
responseListActionExecutions =
  res
    "ListActionExecutionsResponse"
    "fixture/ListActionExecutionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListActionExecutions)

responseListActionTypes :: ListActionTypesResponse -> TestTree
responseListActionTypes =
  res
    "ListActionTypesResponse"
    "fixture/ListActionTypesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListActionTypes)

responseListPipelineExecutions :: ListPipelineExecutionsResponse -> TestTree
responseListPipelineExecutions =
  res
    "ListPipelineExecutionsResponse"
    "fixture/ListPipelineExecutionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListPipelineExecutions)

responseListPipelines :: ListPipelinesResponse -> TestTree
responseListPipelines =
  res
    "ListPipelinesResponse"
    "fixture/ListPipelinesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListPipelines)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseListWebhooks :: ListWebhooksResponse -> TestTree
responseListWebhooks =
  res
    "ListWebhooksResponse"
    "fixture/ListWebhooksResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListWebhooks)

responsePollForJobs :: PollForJobsResponse -> TestTree
responsePollForJobs =
  res
    "PollForJobsResponse"
    "fixture/PollForJobsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PollForJobs)

responsePollForThirdPartyJobs :: PollForThirdPartyJobsResponse -> TestTree
responsePollForThirdPartyJobs =
  res
    "PollForThirdPartyJobsResponse"
    "fixture/PollForThirdPartyJobsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PollForThirdPartyJobs)

responsePutActionRevision :: PutActionRevisionResponse -> TestTree
responsePutActionRevision =
  res
    "PutActionRevisionResponse"
    "fixture/PutActionRevisionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutActionRevision)

responsePutApprovalResult :: PutApprovalResultResponse -> TestTree
responsePutApprovalResult =
  res
    "PutApprovalResultResponse"
    "fixture/PutApprovalResultResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutApprovalResult)

responsePutJobFailureResult :: PutJobFailureResultResponse -> TestTree
responsePutJobFailureResult =
  res
    "PutJobFailureResultResponse"
    "fixture/PutJobFailureResultResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutJobFailureResult)

responsePutJobSuccessResult :: PutJobSuccessResultResponse -> TestTree
responsePutJobSuccessResult =
  res
    "PutJobSuccessResultResponse"
    "fixture/PutJobSuccessResultResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutJobSuccessResult)

responsePutThirdPartyJobFailureResult :: PutThirdPartyJobFailureResultResponse -> TestTree
responsePutThirdPartyJobFailureResult =
  res
    "PutThirdPartyJobFailureResultResponse"
    "fixture/PutThirdPartyJobFailureResultResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutThirdPartyJobFailureResult)

responsePutThirdPartyJobSuccessResult :: PutThirdPartyJobSuccessResultResponse -> TestTree
responsePutThirdPartyJobSuccessResult =
  res
    "PutThirdPartyJobSuccessResultResponse"
    "fixture/PutThirdPartyJobSuccessResultResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutThirdPartyJobSuccessResult)

responsePutWebhook :: PutWebhookResponse -> TestTree
responsePutWebhook =
  res
    "PutWebhookResponse"
    "fixture/PutWebhookResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutWebhook)

responseRegisterWebhookWithThirdParty :: RegisterWebhookWithThirdPartyResponse -> TestTree
responseRegisterWebhookWithThirdParty =
  res
    "RegisterWebhookWithThirdPartyResponse"
    "fixture/RegisterWebhookWithThirdPartyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RegisterWebhookWithThirdParty)

responseRetryStageExecution :: RetryStageExecutionResponse -> TestTree
responseRetryStageExecution =
  res
    "RetryStageExecutionResponse"
    "fixture/RetryStageExecutionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RetryStageExecution)

responseStartPipelineExecution :: StartPipelineExecutionResponse -> TestTree
responseStartPipelineExecution =
  res
    "StartPipelineExecutionResponse"
    "fixture/StartPipelineExecutionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartPipelineExecution)

responseStopPipelineExecution :: StopPipelineExecutionResponse -> TestTree
responseStopPipelineExecution =
  res
    "StopPipelineExecutionResponse"
    "fixture/StopPipelineExecutionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StopPipelineExecution)

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

responseUpdateActionType :: UpdateActionTypeResponse -> TestTree
responseUpdateActionType =
  res
    "UpdateActionTypeResponse"
    "fixture/UpdateActionTypeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateActionType)

responseUpdatePipeline :: UpdatePipelineResponse -> TestTree
responseUpdatePipeline =
  res
    "UpdatePipelineResponse"
    "fixture/UpdatePipelineResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdatePipeline)
