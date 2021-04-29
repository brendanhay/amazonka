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

import Data.Proxy
import Network.AWS.CodePipeline
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
--         [ requestListActionTypes $
--             newListActionTypes
--
--         , requestDeregisterWebhookWithThirdParty $
--             newDeregisterWebhookWithThirdParty
--
--         , requestPutActionRevision $
--             newPutActionRevision
--
--         , requestPutJobSuccessResult $
--             newPutJobSuccessResult
--
--         , requestPutThirdPartyJobSuccessResult $
--             newPutThirdPartyJobSuccessResult
--
--         , requestCreatePipeline $
--             newCreatePipeline
--
--         , requestRetryStageExecution $
--             newRetryStageExecution
--
--         , requestUpdatePipeline $
--             newUpdatePipeline
--
--         , requestGetPipelineState $
--             newGetPipelineState
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestDeletePipeline $
--             newDeletePipeline
--
--         , requestStartPipelineExecution $
--             newStartPipelineExecution
--
--         , requestGetActionType $
--             newGetActionType
--
--         , requestTagResource $
--             newTagResource
--
--         , requestStopPipelineExecution $
--             newStopPipelineExecution
--
--         , requestRegisterWebhookWithThirdParty $
--             newRegisterWebhookWithThirdParty
--
--         , requestListActionExecutions $
--             newListActionExecutions
--
--         , requestPollForThirdPartyJobs $
--             newPollForThirdPartyJobs
--
--         , requestEnableStageTransition $
--             newEnableStageTransition
--
--         , requestDeleteWebhook $
--             newDeleteWebhook
--
--         , requestAcknowledgeThirdPartyJob $
--             newAcknowledgeThirdPartyJob
--
--         , requestAcknowledgeJob $
--             newAcknowledgeJob
--
--         , requestDisableStageTransition $
--             newDisableStageTransition
--
--         , requestUpdateActionType $
--             newUpdateActionType
--
--         , requestPutApprovalResult $
--             newPutApprovalResult
--
--         , requestPutJobFailureResult $
--             newPutJobFailureResult
--
--         , requestDeleteCustomActionType $
--             newDeleteCustomActionType
--
--         , requestGetPipeline $
--             newGetPipeline
--
--         , requestCreateCustomActionType $
--             newCreateCustomActionType
--
--         , requestListPipelineExecutions $
--             newListPipelineExecutions
--
--         , requestGetThirdPartyJobDetails $
--             newGetThirdPartyJobDetails
--
--         , requestGetPipelineExecution $
--             newGetPipelineExecution
--
--         , requestGetJobDetails $
--             newGetJobDetails
--
--         , requestListPipelines $
--             newListPipelines
--
--         , requestPollForJobs $
--             newPollForJobs
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestPutWebhook $
--             newPutWebhook
--
--         , requestPutThirdPartyJobFailureResult $
--             newPutThirdPartyJobFailureResult
--
--         , requestListWebhooks $
--             newListWebhooks
--
--           ]

--     , testGroup "response"
--         [ responseListActionTypes $
--             newListActionTypesResponse
--
--         , responseDeregisterWebhookWithThirdParty $
--             newDeregisterWebhookWithThirdPartyResponse
--
--         , responsePutActionRevision $
--             newPutActionRevisionResponse
--
--         , responsePutJobSuccessResult $
--             newPutJobSuccessResultResponse
--
--         , responsePutThirdPartyJobSuccessResult $
--             newPutThirdPartyJobSuccessResultResponse
--
--         , responseCreatePipeline $
--             newCreatePipelineResponse
--
--         , responseRetryStageExecution $
--             newRetryStageExecutionResponse
--
--         , responseUpdatePipeline $
--             newUpdatePipelineResponse
--
--         , responseGetPipelineState $
--             newGetPipelineStateResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseDeletePipeline $
--             newDeletePipelineResponse
--
--         , responseStartPipelineExecution $
--             newStartPipelineExecutionResponse
--
--         , responseGetActionType $
--             newGetActionTypeResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseStopPipelineExecution $
--             newStopPipelineExecutionResponse
--
--         , responseRegisterWebhookWithThirdParty $
--             newRegisterWebhookWithThirdPartyResponse
--
--         , responseListActionExecutions $
--             newListActionExecutionsResponse
--
--         , responsePollForThirdPartyJobs $
--             newPollForThirdPartyJobsResponse
--
--         , responseEnableStageTransition $
--             newEnableStageTransitionResponse
--
--         , responseDeleteWebhook $
--             newDeleteWebhookResponse
--
--         , responseAcknowledgeThirdPartyJob $
--             newAcknowledgeThirdPartyJobResponse
--
--         , responseAcknowledgeJob $
--             newAcknowledgeJobResponse
--
--         , responseDisableStageTransition $
--             newDisableStageTransitionResponse
--
--         , responseUpdateActionType $
--             newUpdateActionTypeResponse
--
--         , responsePutApprovalResult $
--             newPutApprovalResultResponse
--
--         , responsePutJobFailureResult $
--             newPutJobFailureResultResponse
--
--         , responseDeleteCustomActionType $
--             newDeleteCustomActionTypeResponse
--
--         , responseGetPipeline $
--             newGetPipelineResponse
--
--         , responseCreateCustomActionType $
--             newCreateCustomActionTypeResponse
--
--         , responseListPipelineExecutions $
--             newListPipelineExecutionsResponse
--
--         , responseGetThirdPartyJobDetails $
--             newGetThirdPartyJobDetailsResponse
--
--         , responseGetPipelineExecution $
--             newGetPipelineExecutionResponse
--
--         , responseGetJobDetails $
--             newGetJobDetailsResponse
--
--         , responseListPipelines $
--             newListPipelinesResponse
--
--         , responsePollForJobs $
--             newPollForJobsResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responsePutWebhook $
--             newPutWebhookResponse
--
--         , responsePutThirdPartyJobFailureResult $
--             newPutThirdPartyJobFailureResultResponse
--
--         , responseListWebhooks $
--             newListWebhooksResponse
--
--           ]
--     ]

-- Requests

requestListActionTypes :: ListActionTypes -> TestTree
requestListActionTypes =
  req
    "ListActionTypes"
    "fixture/ListActionTypes.yaml"

requestDeregisterWebhookWithThirdParty :: DeregisterWebhookWithThirdParty -> TestTree
requestDeregisterWebhookWithThirdParty =
  req
    "DeregisterWebhookWithThirdParty"
    "fixture/DeregisterWebhookWithThirdParty.yaml"

requestPutActionRevision :: PutActionRevision -> TestTree
requestPutActionRevision =
  req
    "PutActionRevision"
    "fixture/PutActionRevision.yaml"

requestPutJobSuccessResult :: PutJobSuccessResult -> TestTree
requestPutJobSuccessResult =
  req
    "PutJobSuccessResult"
    "fixture/PutJobSuccessResult.yaml"

requestPutThirdPartyJobSuccessResult :: PutThirdPartyJobSuccessResult -> TestTree
requestPutThirdPartyJobSuccessResult =
  req
    "PutThirdPartyJobSuccessResult"
    "fixture/PutThirdPartyJobSuccessResult.yaml"

requestCreatePipeline :: CreatePipeline -> TestTree
requestCreatePipeline =
  req
    "CreatePipeline"
    "fixture/CreatePipeline.yaml"

requestRetryStageExecution :: RetryStageExecution -> TestTree
requestRetryStageExecution =
  req
    "RetryStageExecution"
    "fixture/RetryStageExecution.yaml"

requestUpdatePipeline :: UpdatePipeline -> TestTree
requestUpdatePipeline =
  req
    "UpdatePipeline"
    "fixture/UpdatePipeline.yaml"

requestGetPipelineState :: GetPipelineState -> TestTree
requestGetPipelineState =
  req
    "GetPipelineState"
    "fixture/GetPipelineState.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestDeletePipeline :: DeletePipeline -> TestTree
requestDeletePipeline =
  req
    "DeletePipeline"
    "fixture/DeletePipeline.yaml"

requestStartPipelineExecution :: StartPipelineExecution -> TestTree
requestStartPipelineExecution =
  req
    "StartPipelineExecution"
    "fixture/StartPipelineExecution.yaml"

requestGetActionType :: GetActionType -> TestTree
requestGetActionType =
  req
    "GetActionType"
    "fixture/GetActionType.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestStopPipelineExecution :: StopPipelineExecution -> TestTree
requestStopPipelineExecution =
  req
    "StopPipelineExecution"
    "fixture/StopPipelineExecution.yaml"

requestRegisterWebhookWithThirdParty :: RegisterWebhookWithThirdParty -> TestTree
requestRegisterWebhookWithThirdParty =
  req
    "RegisterWebhookWithThirdParty"
    "fixture/RegisterWebhookWithThirdParty.yaml"

requestListActionExecutions :: ListActionExecutions -> TestTree
requestListActionExecutions =
  req
    "ListActionExecutions"
    "fixture/ListActionExecutions.yaml"

requestPollForThirdPartyJobs :: PollForThirdPartyJobs -> TestTree
requestPollForThirdPartyJobs =
  req
    "PollForThirdPartyJobs"
    "fixture/PollForThirdPartyJobs.yaml"

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

requestAcknowledgeThirdPartyJob :: AcknowledgeThirdPartyJob -> TestTree
requestAcknowledgeThirdPartyJob =
  req
    "AcknowledgeThirdPartyJob"
    "fixture/AcknowledgeThirdPartyJob.yaml"

requestAcknowledgeJob :: AcknowledgeJob -> TestTree
requestAcknowledgeJob =
  req
    "AcknowledgeJob"
    "fixture/AcknowledgeJob.yaml"

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

requestDeleteCustomActionType :: DeleteCustomActionType -> TestTree
requestDeleteCustomActionType =
  req
    "DeleteCustomActionType"
    "fixture/DeleteCustomActionType.yaml"

requestGetPipeline :: GetPipeline -> TestTree
requestGetPipeline =
  req
    "GetPipeline"
    "fixture/GetPipeline.yaml"

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

requestGetThirdPartyJobDetails :: GetThirdPartyJobDetails -> TestTree
requestGetThirdPartyJobDetails =
  req
    "GetThirdPartyJobDetails"
    "fixture/GetThirdPartyJobDetails.yaml"

requestGetPipelineExecution :: GetPipelineExecution -> TestTree
requestGetPipelineExecution =
  req
    "GetPipelineExecution"
    "fixture/GetPipelineExecution.yaml"

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

requestPollForJobs :: PollForJobs -> TestTree
requestPollForJobs =
  req
    "PollForJobs"
    "fixture/PollForJobs.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestPutWebhook :: PutWebhook -> TestTree
requestPutWebhook =
  req
    "PutWebhook"
    "fixture/PutWebhook.yaml"

requestPutThirdPartyJobFailureResult :: PutThirdPartyJobFailureResult -> TestTree
requestPutThirdPartyJobFailureResult =
  req
    "PutThirdPartyJobFailureResult"
    "fixture/PutThirdPartyJobFailureResult.yaml"

requestListWebhooks :: ListWebhooks -> TestTree
requestListWebhooks =
  req
    "ListWebhooks"
    "fixture/ListWebhooks.yaml"

-- Responses

responseListActionTypes :: ListActionTypesResponse -> TestTree
responseListActionTypes =
  res
    "ListActionTypesResponse"
    "fixture/ListActionTypesResponse.proto"
    defaultService
    (Proxy :: Proxy ListActionTypes)

responseDeregisterWebhookWithThirdParty :: DeregisterWebhookWithThirdPartyResponse -> TestTree
responseDeregisterWebhookWithThirdParty =
  res
    "DeregisterWebhookWithThirdPartyResponse"
    "fixture/DeregisterWebhookWithThirdPartyResponse.proto"
    defaultService
    (Proxy :: Proxy DeregisterWebhookWithThirdParty)

responsePutActionRevision :: PutActionRevisionResponse -> TestTree
responsePutActionRevision =
  res
    "PutActionRevisionResponse"
    "fixture/PutActionRevisionResponse.proto"
    defaultService
    (Proxy :: Proxy PutActionRevision)

responsePutJobSuccessResult :: PutJobSuccessResultResponse -> TestTree
responsePutJobSuccessResult =
  res
    "PutJobSuccessResultResponse"
    "fixture/PutJobSuccessResultResponse.proto"
    defaultService
    (Proxy :: Proxy PutJobSuccessResult)

responsePutThirdPartyJobSuccessResult :: PutThirdPartyJobSuccessResultResponse -> TestTree
responsePutThirdPartyJobSuccessResult =
  res
    "PutThirdPartyJobSuccessResultResponse"
    "fixture/PutThirdPartyJobSuccessResultResponse.proto"
    defaultService
    (Proxy :: Proxy PutThirdPartyJobSuccessResult)

responseCreatePipeline :: CreatePipelineResponse -> TestTree
responseCreatePipeline =
  res
    "CreatePipelineResponse"
    "fixture/CreatePipelineResponse.proto"
    defaultService
    (Proxy :: Proxy CreatePipeline)

responseRetryStageExecution :: RetryStageExecutionResponse -> TestTree
responseRetryStageExecution =
  res
    "RetryStageExecutionResponse"
    "fixture/RetryStageExecutionResponse.proto"
    defaultService
    (Proxy :: Proxy RetryStageExecution)

responseUpdatePipeline :: UpdatePipelineResponse -> TestTree
responseUpdatePipeline =
  res
    "UpdatePipelineResponse"
    "fixture/UpdatePipelineResponse.proto"
    defaultService
    (Proxy :: Proxy UpdatePipeline)

responseGetPipelineState :: GetPipelineStateResponse -> TestTree
responseGetPipelineState =
  res
    "GetPipelineStateResponse"
    "fixture/GetPipelineStateResponse.proto"
    defaultService
    (Proxy :: Proxy GetPipelineState)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy :: Proxy UntagResource)

responseDeletePipeline :: DeletePipelineResponse -> TestTree
responseDeletePipeline =
  res
    "DeletePipelineResponse"
    "fixture/DeletePipelineResponse.proto"
    defaultService
    (Proxy :: Proxy DeletePipeline)

responseStartPipelineExecution :: StartPipelineExecutionResponse -> TestTree
responseStartPipelineExecution =
  res
    "StartPipelineExecutionResponse"
    "fixture/StartPipelineExecutionResponse.proto"
    defaultService
    (Proxy :: Proxy StartPipelineExecution)

responseGetActionType :: GetActionTypeResponse -> TestTree
responseGetActionType =
  res
    "GetActionTypeResponse"
    "fixture/GetActionTypeResponse.proto"
    defaultService
    (Proxy :: Proxy GetActionType)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy :: Proxy TagResource)

responseStopPipelineExecution :: StopPipelineExecutionResponse -> TestTree
responseStopPipelineExecution =
  res
    "StopPipelineExecutionResponse"
    "fixture/StopPipelineExecutionResponse.proto"
    defaultService
    (Proxy :: Proxy StopPipelineExecution)

responseRegisterWebhookWithThirdParty :: RegisterWebhookWithThirdPartyResponse -> TestTree
responseRegisterWebhookWithThirdParty =
  res
    "RegisterWebhookWithThirdPartyResponse"
    "fixture/RegisterWebhookWithThirdPartyResponse.proto"
    defaultService
    (Proxy :: Proxy RegisterWebhookWithThirdParty)

responseListActionExecutions :: ListActionExecutionsResponse -> TestTree
responseListActionExecutions =
  res
    "ListActionExecutionsResponse"
    "fixture/ListActionExecutionsResponse.proto"
    defaultService
    (Proxy :: Proxy ListActionExecutions)

responsePollForThirdPartyJobs :: PollForThirdPartyJobsResponse -> TestTree
responsePollForThirdPartyJobs =
  res
    "PollForThirdPartyJobsResponse"
    "fixture/PollForThirdPartyJobsResponse.proto"
    defaultService
    (Proxy :: Proxy PollForThirdPartyJobs)

responseEnableStageTransition :: EnableStageTransitionResponse -> TestTree
responseEnableStageTransition =
  res
    "EnableStageTransitionResponse"
    "fixture/EnableStageTransitionResponse.proto"
    defaultService
    (Proxy :: Proxy EnableStageTransition)

responseDeleteWebhook :: DeleteWebhookResponse -> TestTree
responseDeleteWebhook =
  res
    "DeleteWebhookResponse"
    "fixture/DeleteWebhookResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteWebhook)

responseAcknowledgeThirdPartyJob :: AcknowledgeThirdPartyJobResponse -> TestTree
responseAcknowledgeThirdPartyJob =
  res
    "AcknowledgeThirdPartyJobResponse"
    "fixture/AcknowledgeThirdPartyJobResponse.proto"
    defaultService
    (Proxy :: Proxy AcknowledgeThirdPartyJob)

responseAcknowledgeJob :: AcknowledgeJobResponse -> TestTree
responseAcknowledgeJob =
  res
    "AcknowledgeJobResponse"
    "fixture/AcknowledgeJobResponse.proto"
    defaultService
    (Proxy :: Proxy AcknowledgeJob)

responseDisableStageTransition :: DisableStageTransitionResponse -> TestTree
responseDisableStageTransition =
  res
    "DisableStageTransitionResponse"
    "fixture/DisableStageTransitionResponse.proto"
    defaultService
    (Proxy :: Proxy DisableStageTransition)

responseUpdateActionType :: UpdateActionTypeResponse -> TestTree
responseUpdateActionType =
  res
    "UpdateActionTypeResponse"
    "fixture/UpdateActionTypeResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateActionType)

responsePutApprovalResult :: PutApprovalResultResponse -> TestTree
responsePutApprovalResult =
  res
    "PutApprovalResultResponse"
    "fixture/PutApprovalResultResponse.proto"
    defaultService
    (Proxy :: Proxy PutApprovalResult)

responsePutJobFailureResult :: PutJobFailureResultResponse -> TestTree
responsePutJobFailureResult =
  res
    "PutJobFailureResultResponse"
    "fixture/PutJobFailureResultResponse.proto"
    defaultService
    (Proxy :: Proxy PutJobFailureResult)

responseDeleteCustomActionType :: DeleteCustomActionTypeResponse -> TestTree
responseDeleteCustomActionType =
  res
    "DeleteCustomActionTypeResponse"
    "fixture/DeleteCustomActionTypeResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteCustomActionType)

responseGetPipeline :: GetPipelineResponse -> TestTree
responseGetPipeline =
  res
    "GetPipelineResponse"
    "fixture/GetPipelineResponse.proto"
    defaultService
    (Proxy :: Proxy GetPipeline)

responseCreateCustomActionType :: CreateCustomActionTypeResponse -> TestTree
responseCreateCustomActionType =
  res
    "CreateCustomActionTypeResponse"
    "fixture/CreateCustomActionTypeResponse.proto"
    defaultService
    (Proxy :: Proxy CreateCustomActionType)

responseListPipelineExecutions :: ListPipelineExecutionsResponse -> TestTree
responseListPipelineExecutions =
  res
    "ListPipelineExecutionsResponse"
    "fixture/ListPipelineExecutionsResponse.proto"
    defaultService
    (Proxy :: Proxy ListPipelineExecutions)

responseGetThirdPartyJobDetails :: GetThirdPartyJobDetailsResponse -> TestTree
responseGetThirdPartyJobDetails =
  res
    "GetThirdPartyJobDetailsResponse"
    "fixture/GetThirdPartyJobDetailsResponse.proto"
    defaultService
    (Proxy :: Proxy GetThirdPartyJobDetails)

responseGetPipelineExecution :: GetPipelineExecutionResponse -> TestTree
responseGetPipelineExecution =
  res
    "GetPipelineExecutionResponse"
    "fixture/GetPipelineExecutionResponse.proto"
    defaultService
    (Proxy :: Proxy GetPipelineExecution)

responseGetJobDetails :: GetJobDetailsResponse -> TestTree
responseGetJobDetails =
  res
    "GetJobDetailsResponse"
    "fixture/GetJobDetailsResponse.proto"
    defaultService
    (Proxy :: Proxy GetJobDetails)

responseListPipelines :: ListPipelinesResponse -> TestTree
responseListPipelines =
  res
    "ListPipelinesResponse"
    "fixture/ListPipelinesResponse.proto"
    defaultService
    (Proxy :: Proxy ListPipelines)

responsePollForJobs :: PollForJobsResponse -> TestTree
responsePollForJobs =
  res
    "PollForJobsResponse"
    "fixture/PollForJobsResponse.proto"
    defaultService
    (Proxy :: Proxy PollForJobs)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy :: Proxy ListTagsForResource)

responsePutWebhook :: PutWebhookResponse -> TestTree
responsePutWebhook =
  res
    "PutWebhookResponse"
    "fixture/PutWebhookResponse.proto"
    defaultService
    (Proxy :: Proxy PutWebhook)

responsePutThirdPartyJobFailureResult :: PutThirdPartyJobFailureResultResponse -> TestTree
responsePutThirdPartyJobFailureResult =
  res
    "PutThirdPartyJobFailureResultResponse"
    "fixture/PutThirdPartyJobFailureResultResponse.proto"
    defaultService
    (Proxy :: Proxy PutThirdPartyJobFailureResult)

responseListWebhooks :: ListWebhooksResponse -> TestTree
responseListWebhooks =
  res
    "ListWebhooksResponse"
    "fixture/ListWebhooksResponse.proto"
    defaultService
    (Proxy :: Proxy ListWebhooks)
