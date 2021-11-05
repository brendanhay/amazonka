{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.CodePipeline
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.AWS.Gen.CodePipeline where

import Amazonka.CodePipeline
import qualified Data.Proxy as Proxy
import Test.AWS.CodePipeline.Internal
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
--         [ requestGetPipeline $
--             newGetPipeline
--
--         , requestPutJobFailureResult $
--             newPutJobFailureResult
--
--         , requestPutApprovalResult $
--             newPutApprovalResult
--
--         , requestAcknowledgeThirdPartyJob $
--             newAcknowledgeThirdPartyJob
--
--         , requestPutThirdPartyJobFailureResult $
--             newPutThirdPartyJobFailureResult
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestRegisterWebhookWithThirdParty $
--             newRegisterWebhookWithThirdParty
--
--         , requestPollForThirdPartyJobs $
--             newPollForThirdPartyJobs
--
--         , requestPollForJobs $
--             newPollForJobs
--
--         , requestStartPipelineExecution $
--             newStartPipelineExecution
--
--         , requestUpdatePipeline $
--             newUpdatePipeline
--
--         , requestDeletePipeline $
--             newDeletePipeline
--
--         , requestGetPipelineState $
--             newGetPipelineState
--
--         , requestGetJobDetails $
--             newGetJobDetails
--
--         , requestListPipelines $
--             newListPipelines
--
--         , requestRetryStageExecution $
--             newRetryStageExecution
--
--         , requestGetPipelineExecution $
--             newGetPipelineExecution
--
--         , requestPutJobSuccessResult $
--             newPutJobSuccessResult
--
--         , requestDeregisterWebhookWithThirdParty $
--             newDeregisterWebhookWithThirdParty
--
--         , requestDeleteCustomActionType $
--             newDeleteCustomActionType
--
--         , requestPutActionRevision $
--             newPutActionRevision
--
--         , requestDisableStageTransition $
--             newDisableStageTransition
--
--         , requestUpdateActionType $
--             newUpdateActionType
--
--         , requestListActionTypes $
--             newListActionTypes
--
--         , requestAcknowledgeJob $
--             newAcknowledgeJob
--
--         , requestEnableStageTransition $
--             newEnableStageTransition
--
--         , requestDeleteWebhook $
--             newDeleteWebhook
--
--         , requestPutWebhook $
--             newPutWebhook
--
--         , requestListWebhooks $
--             newListWebhooks
--
--         , requestListActionExecutions $
--             newListActionExecutions
--
--         , requestGetActionType $
--             newGetActionType
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
--         , requestCreatePipeline $
--             newCreatePipeline
--
--         , requestGetThirdPartyJobDetails $
--             newGetThirdPartyJobDetails
--
--         , requestPutThirdPartyJobSuccessResult $
--             newPutThirdPartyJobSuccessResult
--
--         , requestCreateCustomActionType $
--             newCreateCustomActionType
--
--         , requestListPipelineExecutions $
--             newListPipelineExecutions
--
--           ]

--     , testGroup "response"
--         [ responseGetPipeline $
--             newGetPipelineResponse
--
--         , responsePutJobFailureResult $
--             newPutJobFailureResultResponse
--
--         , responsePutApprovalResult $
--             newPutApprovalResultResponse
--
--         , responseAcknowledgeThirdPartyJob $
--             newAcknowledgeThirdPartyJobResponse
--
--         , responsePutThirdPartyJobFailureResult $
--             newPutThirdPartyJobFailureResultResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseRegisterWebhookWithThirdParty $
--             newRegisterWebhookWithThirdPartyResponse
--
--         , responsePollForThirdPartyJobs $
--             newPollForThirdPartyJobsResponse
--
--         , responsePollForJobs $
--             newPollForJobsResponse
--
--         , responseStartPipelineExecution $
--             newStartPipelineExecutionResponse
--
--         , responseUpdatePipeline $
--             newUpdatePipelineResponse
--
--         , responseDeletePipeline $
--             newDeletePipelineResponse
--
--         , responseGetPipelineState $
--             newGetPipelineStateResponse
--
--         , responseGetJobDetails $
--             newGetJobDetailsResponse
--
--         , responseListPipelines $
--             newListPipelinesResponse
--
--         , responseRetryStageExecution $
--             newRetryStageExecutionResponse
--
--         , responseGetPipelineExecution $
--             newGetPipelineExecutionResponse
--
--         , responsePutJobSuccessResult $
--             newPutJobSuccessResultResponse
--
--         , responseDeregisterWebhookWithThirdParty $
--             newDeregisterWebhookWithThirdPartyResponse
--
--         , responseDeleteCustomActionType $
--             newDeleteCustomActionTypeResponse
--
--         , responsePutActionRevision $
--             newPutActionRevisionResponse
--
--         , responseDisableStageTransition $
--             newDisableStageTransitionResponse
--
--         , responseUpdateActionType $
--             newUpdateActionTypeResponse
--
--         , responseListActionTypes $
--             newListActionTypesResponse
--
--         , responseAcknowledgeJob $
--             newAcknowledgeJobResponse
--
--         , responseEnableStageTransition $
--             newEnableStageTransitionResponse
--
--         , responseDeleteWebhook $
--             newDeleteWebhookResponse
--
--         , responsePutWebhook $
--             newPutWebhookResponse
--
--         , responseListWebhooks $
--             newListWebhooksResponse
--
--         , responseListActionExecutions $
--             newListActionExecutionsResponse
--
--         , responseGetActionType $
--             newGetActionTypeResponse
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
--         , responseCreatePipeline $
--             newCreatePipelineResponse
--
--         , responseGetThirdPartyJobDetails $
--             newGetThirdPartyJobDetailsResponse
--
--         , responsePutThirdPartyJobSuccessResult $
--             newPutThirdPartyJobSuccessResultResponse
--
--         , responseCreateCustomActionType $
--             newCreateCustomActionTypeResponse
--
--         , responseListPipelineExecutions $
--             newListPipelineExecutionsResponse
--
--           ]
--     ]

-- Requests

requestGetPipeline :: GetPipeline -> TestTree
requestGetPipeline =
  req
    "GetPipeline"
    "fixture/GetPipeline.yaml"

requestPutJobFailureResult :: PutJobFailureResult -> TestTree
requestPutJobFailureResult =
  req
    "PutJobFailureResult"
    "fixture/PutJobFailureResult.yaml"

requestPutApprovalResult :: PutApprovalResult -> TestTree
requestPutApprovalResult =
  req
    "PutApprovalResult"
    "fixture/PutApprovalResult.yaml"

requestAcknowledgeThirdPartyJob :: AcknowledgeThirdPartyJob -> TestTree
requestAcknowledgeThirdPartyJob =
  req
    "AcknowledgeThirdPartyJob"
    "fixture/AcknowledgeThirdPartyJob.yaml"

requestPutThirdPartyJobFailureResult :: PutThirdPartyJobFailureResult -> TestTree
requestPutThirdPartyJobFailureResult =
  req
    "PutThirdPartyJobFailureResult"
    "fixture/PutThirdPartyJobFailureResult.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestRegisterWebhookWithThirdParty :: RegisterWebhookWithThirdParty -> TestTree
requestRegisterWebhookWithThirdParty =
  req
    "RegisterWebhookWithThirdParty"
    "fixture/RegisterWebhookWithThirdParty.yaml"

requestPollForThirdPartyJobs :: PollForThirdPartyJobs -> TestTree
requestPollForThirdPartyJobs =
  req
    "PollForThirdPartyJobs"
    "fixture/PollForThirdPartyJobs.yaml"

requestPollForJobs :: PollForJobs -> TestTree
requestPollForJobs =
  req
    "PollForJobs"
    "fixture/PollForJobs.yaml"

requestStartPipelineExecution :: StartPipelineExecution -> TestTree
requestStartPipelineExecution =
  req
    "StartPipelineExecution"
    "fixture/StartPipelineExecution.yaml"

requestUpdatePipeline :: UpdatePipeline -> TestTree
requestUpdatePipeline =
  req
    "UpdatePipeline"
    "fixture/UpdatePipeline.yaml"

requestDeletePipeline :: DeletePipeline -> TestTree
requestDeletePipeline =
  req
    "DeletePipeline"
    "fixture/DeletePipeline.yaml"

requestGetPipelineState :: GetPipelineState -> TestTree
requestGetPipelineState =
  req
    "GetPipelineState"
    "fixture/GetPipelineState.yaml"

requestGetJobDetails :: GetJobDetails -> TestTree
requestGetJobDetails =
  req
    "GetJobDetails"
    "fixture/GetJobDetails.yaml"

requestListPipelines :: ListPipelines -> TestTree
requestListPipelines =
  req
    "ListPipelines"
    "fixture/ListPipelines.yaml"

requestRetryStageExecution :: RetryStageExecution -> TestTree
requestRetryStageExecution =
  req
    "RetryStageExecution"
    "fixture/RetryStageExecution.yaml"

requestGetPipelineExecution :: GetPipelineExecution -> TestTree
requestGetPipelineExecution =
  req
    "GetPipelineExecution"
    "fixture/GetPipelineExecution.yaml"

requestPutJobSuccessResult :: PutJobSuccessResult -> TestTree
requestPutJobSuccessResult =
  req
    "PutJobSuccessResult"
    "fixture/PutJobSuccessResult.yaml"

requestDeregisterWebhookWithThirdParty :: DeregisterWebhookWithThirdParty -> TestTree
requestDeregisterWebhookWithThirdParty =
  req
    "DeregisterWebhookWithThirdParty"
    "fixture/DeregisterWebhookWithThirdParty.yaml"

requestDeleteCustomActionType :: DeleteCustomActionType -> TestTree
requestDeleteCustomActionType =
  req
    "DeleteCustomActionType"
    "fixture/DeleteCustomActionType.yaml"

requestPutActionRevision :: PutActionRevision -> TestTree
requestPutActionRevision =
  req
    "PutActionRevision"
    "fixture/PutActionRevision.yaml"

requestDisableStageTransition :: DisableStageTransition -> TestTree
requestDisableStageTransition =
  req
    "DisableStageTransition"
    "fixture/DisableStageTransition.yaml"

requestUpdateActionType :: UpdateActionType -> TestTree
requestUpdateActionType =
  req
    "UpdateActionType"
    "fixture/UpdateActionType.yaml"

requestListActionTypes :: ListActionTypes -> TestTree
requestListActionTypes =
  req
    "ListActionTypes"
    "fixture/ListActionTypes.yaml"

requestAcknowledgeJob :: AcknowledgeJob -> TestTree
requestAcknowledgeJob =
  req
    "AcknowledgeJob"
    "fixture/AcknowledgeJob.yaml"

requestEnableStageTransition :: EnableStageTransition -> TestTree
requestEnableStageTransition =
  req
    "EnableStageTransition"
    "fixture/EnableStageTransition.yaml"

requestDeleteWebhook :: DeleteWebhook -> TestTree
requestDeleteWebhook =
  req
    "DeleteWebhook"
    "fixture/DeleteWebhook.yaml"

requestPutWebhook :: PutWebhook -> TestTree
requestPutWebhook =
  req
    "PutWebhook"
    "fixture/PutWebhook.yaml"

requestListWebhooks :: ListWebhooks -> TestTree
requestListWebhooks =
  req
    "ListWebhooks"
    "fixture/ListWebhooks.yaml"

requestListActionExecutions :: ListActionExecutions -> TestTree
requestListActionExecutions =
  req
    "ListActionExecutions"
    "fixture/ListActionExecutions.yaml"

requestGetActionType :: GetActionType -> TestTree
requestGetActionType =
  req
    "GetActionType"
    "fixture/GetActionType.yaml"

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

requestCreatePipeline :: CreatePipeline -> TestTree
requestCreatePipeline =
  req
    "CreatePipeline"
    "fixture/CreatePipeline.yaml"

requestGetThirdPartyJobDetails :: GetThirdPartyJobDetails -> TestTree
requestGetThirdPartyJobDetails =
  req
    "GetThirdPartyJobDetails"
    "fixture/GetThirdPartyJobDetails.yaml"

requestPutThirdPartyJobSuccessResult :: PutThirdPartyJobSuccessResult -> TestTree
requestPutThirdPartyJobSuccessResult =
  req
    "PutThirdPartyJobSuccessResult"
    "fixture/PutThirdPartyJobSuccessResult.yaml"

requestCreateCustomActionType :: CreateCustomActionType -> TestTree
requestCreateCustomActionType =
  req
    "CreateCustomActionType"
    "fixture/CreateCustomActionType.yaml"

requestListPipelineExecutions :: ListPipelineExecutions -> TestTree
requestListPipelineExecutions =
  req
    "ListPipelineExecutions"
    "fixture/ListPipelineExecutions.yaml"

-- Responses

responseGetPipeline :: GetPipelineResponse -> TestTree
responseGetPipeline =
  res
    "GetPipelineResponse"
    "fixture/GetPipelineResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetPipeline)

responsePutJobFailureResult :: PutJobFailureResultResponse -> TestTree
responsePutJobFailureResult =
  res
    "PutJobFailureResultResponse"
    "fixture/PutJobFailureResultResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutJobFailureResult)

responsePutApprovalResult :: PutApprovalResultResponse -> TestTree
responsePutApprovalResult =
  res
    "PutApprovalResultResponse"
    "fixture/PutApprovalResultResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutApprovalResult)

responseAcknowledgeThirdPartyJob :: AcknowledgeThirdPartyJobResponse -> TestTree
responseAcknowledgeThirdPartyJob =
  res
    "AcknowledgeThirdPartyJobResponse"
    "fixture/AcknowledgeThirdPartyJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AcknowledgeThirdPartyJob)

responsePutThirdPartyJobFailureResult :: PutThirdPartyJobFailureResultResponse -> TestTree
responsePutThirdPartyJobFailureResult =
  res
    "PutThirdPartyJobFailureResultResponse"
    "fixture/PutThirdPartyJobFailureResultResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutThirdPartyJobFailureResult)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseRegisterWebhookWithThirdParty :: RegisterWebhookWithThirdPartyResponse -> TestTree
responseRegisterWebhookWithThirdParty =
  res
    "RegisterWebhookWithThirdPartyResponse"
    "fixture/RegisterWebhookWithThirdPartyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RegisterWebhookWithThirdParty)

responsePollForThirdPartyJobs :: PollForThirdPartyJobsResponse -> TestTree
responsePollForThirdPartyJobs =
  res
    "PollForThirdPartyJobsResponse"
    "fixture/PollForThirdPartyJobsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PollForThirdPartyJobs)

responsePollForJobs :: PollForJobsResponse -> TestTree
responsePollForJobs =
  res
    "PollForJobsResponse"
    "fixture/PollForJobsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PollForJobs)

responseStartPipelineExecution :: StartPipelineExecutionResponse -> TestTree
responseStartPipelineExecution =
  res
    "StartPipelineExecutionResponse"
    "fixture/StartPipelineExecutionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartPipelineExecution)

responseUpdatePipeline :: UpdatePipelineResponse -> TestTree
responseUpdatePipeline =
  res
    "UpdatePipelineResponse"
    "fixture/UpdatePipelineResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdatePipeline)

responseDeletePipeline :: DeletePipelineResponse -> TestTree
responseDeletePipeline =
  res
    "DeletePipelineResponse"
    "fixture/DeletePipelineResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeletePipeline)

responseGetPipelineState :: GetPipelineStateResponse -> TestTree
responseGetPipelineState =
  res
    "GetPipelineStateResponse"
    "fixture/GetPipelineStateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetPipelineState)

responseGetJobDetails :: GetJobDetailsResponse -> TestTree
responseGetJobDetails =
  res
    "GetJobDetailsResponse"
    "fixture/GetJobDetailsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetJobDetails)

responseListPipelines :: ListPipelinesResponse -> TestTree
responseListPipelines =
  res
    "ListPipelinesResponse"
    "fixture/ListPipelinesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListPipelines)

responseRetryStageExecution :: RetryStageExecutionResponse -> TestTree
responseRetryStageExecution =
  res
    "RetryStageExecutionResponse"
    "fixture/RetryStageExecutionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RetryStageExecution)

responseGetPipelineExecution :: GetPipelineExecutionResponse -> TestTree
responseGetPipelineExecution =
  res
    "GetPipelineExecutionResponse"
    "fixture/GetPipelineExecutionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetPipelineExecution)

responsePutJobSuccessResult :: PutJobSuccessResultResponse -> TestTree
responsePutJobSuccessResult =
  res
    "PutJobSuccessResultResponse"
    "fixture/PutJobSuccessResultResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutJobSuccessResult)

responseDeregisterWebhookWithThirdParty :: DeregisterWebhookWithThirdPartyResponse -> TestTree
responseDeregisterWebhookWithThirdParty =
  res
    "DeregisterWebhookWithThirdPartyResponse"
    "fixture/DeregisterWebhookWithThirdPartyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeregisterWebhookWithThirdParty)

responseDeleteCustomActionType :: DeleteCustomActionTypeResponse -> TestTree
responseDeleteCustomActionType =
  res
    "DeleteCustomActionTypeResponse"
    "fixture/DeleteCustomActionTypeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteCustomActionType)

responsePutActionRevision :: PutActionRevisionResponse -> TestTree
responsePutActionRevision =
  res
    "PutActionRevisionResponse"
    "fixture/PutActionRevisionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutActionRevision)

responseDisableStageTransition :: DisableStageTransitionResponse -> TestTree
responseDisableStageTransition =
  res
    "DisableStageTransitionResponse"
    "fixture/DisableStageTransitionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisableStageTransition)

responseUpdateActionType :: UpdateActionTypeResponse -> TestTree
responseUpdateActionType =
  res
    "UpdateActionTypeResponse"
    "fixture/UpdateActionTypeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateActionType)

responseListActionTypes :: ListActionTypesResponse -> TestTree
responseListActionTypes =
  res
    "ListActionTypesResponse"
    "fixture/ListActionTypesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListActionTypes)

responseAcknowledgeJob :: AcknowledgeJobResponse -> TestTree
responseAcknowledgeJob =
  res
    "AcknowledgeJobResponse"
    "fixture/AcknowledgeJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AcknowledgeJob)

responseEnableStageTransition :: EnableStageTransitionResponse -> TestTree
responseEnableStageTransition =
  res
    "EnableStageTransitionResponse"
    "fixture/EnableStageTransitionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy EnableStageTransition)

responseDeleteWebhook :: DeleteWebhookResponse -> TestTree
responseDeleteWebhook =
  res
    "DeleteWebhookResponse"
    "fixture/DeleteWebhookResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteWebhook)

responsePutWebhook :: PutWebhookResponse -> TestTree
responsePutWebhook =
  res
    "PutWebhookResponse"
    "fixture/PutWebhookResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutWebhook)

responseListWebhooks :: ListWebhooksResponse -> TestTree
responseListWebhooks =
  res
    "ListWebhooksResponse"
    "fixture/ListWebhooksResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListWebhooks)

responseListActionExecutions :: ListActionExecutionsResponse -> TestTree
responseListActionExecutions =
  res
    "ListActionExecutionsResponse"
    "fixture/ListActionExecutionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListActionExecutions)

responseGetActionType :: GetActionTypeResponse -> TestTree
responseGetActionType =
  res
    "GetActionTypeResponse"
    "fixture/GetActionTypeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetActionType)

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

responseCreatePipeline :: CreatePipelineResponse -> TestTree
responseCreatePipeline =
  res
    "CreatePipelineResponse"
    "fixture/CreatePipelineResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreatePipeline)

responseGetThirdPartyJobDetails :: GetThirdPartyJobDetailsResponse -> TestTree
responseGetThirdPartyJobDetails =
  res
    "GetThirdPartyJobDetailsResponse"
    "fixture/GetThirdPartyJobDetailsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetThirdPartyJobDetails)

responsePutThirdPartyJobSuccessResult :: PutThirdPartyJobSuccessResultResponse -> TestTree
responsePutThirdPartyJobSuccessResult =
  res
    "PutThirdPartyJobSuccessResultResponse"
    "fixture/PutThirdPartyJobSuccessResultResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutThirdPartyJobSuccessResult)

responseCreateCustomActionType :: CreateCustomActionTypeResponse -> TestTree
responseCreateCustomActionType =
  res
    "CreateCustomActionTypeResponse"
    "fixture/CreateCustomActionTypeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateCustomActionType)

responseListPipelineExecutions :: ListPipelineExecutionsResponse -> TestTree
responseListPipelineExecutions =
  res
    "ListPipelineExecutionsResponse"
    "fixture/ListPipelineExecutionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListPipelineExecutions)
