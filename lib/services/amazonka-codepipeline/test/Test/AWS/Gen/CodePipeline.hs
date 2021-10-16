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
--         , requestPutJobSuccessResult $
--             newPutJobSuccessResult
--
--         , requestDeregisterWebhookWithThirdParty $
--             newDeregisterWebhookWithThirdParty
--
--         , requestPutActionRevision $
--             newPutActionRevision
--
--         , requestPutThirdPartyJobSuccessResult $
--             newPutThirdPartyJobSuccessResult
--
--         , requestRetryStageExecution $
--             newRetryStageExecution
--
--         , requestCreatePipeline $
--             newCreatePipeline
--
--         , requestDeletePipeline $
--             newDeletePipeline
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
--         , requestStopPipelineExecution $
--             newStopPipelineExecution
--
--         , requestGetActionType $
--             newGetActionType
--
--         , requestTagResource $
--             newTagResource
--
--         , requestStartPipelineExecution $
--             newStartPipelineExecution
--
--         , requestPollForThirdPartyJobs $
--             newPollForThirdPartyJobs
--
--         , requestListActionExecutions $
--             newListActionExecutions
--
--         , requestRegisterWebhookWithThirdParty $
--             newRegisterWebhookWithThirdParty
--
--         , requestEnableStageTransition $
--             newEnableStageTransition
--
--         , requestAcknowledgeThirdPartyJob $
--             newAcknowledgeThirdPartyJob
--
--         , requestDeleteWebhook $
--             newDeleteWebhook
--
--         , requestPutJobFailureResult $
--             newPutJobFailureResult
--
--         , requestPutApprovalResult $
--             newPutApprovalResult
--
--         , requestAcknowledgeJob $
--             newAcknowledgeJob
--
--         , requestUpdateActionType $
--             newUpdateActionType
--
--         , requestDisableStageTransition $
--             newDisableStageTransition
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
--         , requestGetPipelineExecution $
--             newGetPipelineExecution
--
--         , requestGetThirdPartyJobDetails $
--             newGetThirdPartyJobDetails
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
--         , requestPutThirdPartyJobFailureResult $
--             newPutThirdPartyJobFailureResult
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestListWebhooks $
--             newListWebhooks
--
--         , requestPutWebhook $
--             newPutWebhook
--
--           ]

--     , testGroup "response"
--         [ responseListActionTypes $
--             newListActionTypesResponse
--
--         , responsePutJobSuccessResult $
--             newPutJobSuccessResultResponse
--
--         , responseDeregisterWebhookWithThirdParty $
--             newDeregisterWebhookWithThirdPartyResponse
--
--         , responsePutActionRevision $
--             newPutActionRevisionResponse
--
--         , responsePutThirdPartyJobSuccessResult $
--             newPutThirdPartyJobSuccessResultResponse
--
--         , responseRetryStageExecution $
--             newRetryStageExecutionResponse
--
--         , responseCreatePipeline $
--             newCreatePipelineResponse
--
--         , responseDeletePipeline $
--             newDeletePipelineResponse
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
--         , responseStopPipelineExecution $
--             newStopPipelineExecutionResponse
--
--         , responseGetActionType $
--             newGetActionTypeResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseStartPipelineExecution $
--             newStartPipelineExecutionResponse
--
--         , responsePollForThirdPartyJobs $
--             newPollForThirdPartyJobsResponse
--
--         , responseListActionExecutions $
--             newListActionExecutionsResponse
--
--         , responseRegisterWebhookWithThirdParty $
--             newRegisterWebhookWithThirdPartyResponse
--
--         , responseEnableStageTransition $
--             newEnableStageTransitionResponse
--
--         , responseAcknowledgeThirdPartyJob $
--             newAcknowledgeThirdPartyJobResponse
--
--         , responseDeleteWebhook $
--             newDeleteWebhookResponse
--
--         , responsePutJobFailureResult $
--             newPutJobFailureResultResponse
--
--         , responsePutApprovalResult $
--             newPutApprovalResultResponse
--
--         , responseAcknowledgeJob $
--             newAcknowledgeJobResponse
--
--         , responseUpdateActionType $
--             newUpdateActionTypeResponse
--
--         , responseDisableStageTransition $
--             newDisableStageTransitionResponse
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
--         , responseGetPipelineExecution $
--             newGetPipelineExecutionResponse
--
--         , responseGetThirdPartyJobDetails $
--             newGetThirdPartyJobDetailsResponse
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
--         , responsePutThirdPartyJobFailureResult $
--             newPutThirdPartyJobFailureResultResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseListWebhooks $
--             newListWebhooksResponse
--
--         , responsePutWebhook $
--             newPutWebhookResponse
--
--           ]
--     ]

-- Requests

requestListActionTypes :: ListActionTypes -> TestTree
requestListActionTypes =
  req
    "ListActionTypes"
    "fixture/ListActionTypes.yaml"

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

requestPutActionRevision :: PutActionRevision -> TestTree
requestPutActionRevision =
  req
    "PutActionRevision"
    "fixture/PutActionRevision.yaml"

requestPutThirdPartyJobSuccessResult :: PutThirdPartyJobSuccessResult -> TestTree
requestPutThirdPartyJobSuccessResult =
  req
    "PutThirdPartyJobSuccessResult"
    "fixture/PutThirdPartyJobSuccessResult.yaml"

requestRetryStageExecution :: RetryStageExecution -> TestTree
requestRetryStageExecution =
  req
    "RetryStageExecution"
    "fixture/RetryStageExecution.yaml"

requestCreatePipeline :: CreatePipeline -> TestTree
requestCreatePipeline =
  req
    "CreatePipeline"
    "fixture/CreatePipeline.yaml"

requestDeletePipeline :: DeletePipeline -> TestTree
requestDeletePipeline =
  req
    "DeletePipeline"
    "fixture/DeletePipeline.yaml"

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

requestStopPipelineExecution :: StopPipelineExecution -> TestTree
requestStopPipelineExecution =
  req
    "StopPipelineExecution"
    "fixture/StopPipelineExecution.yaml"

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

requestStartPipelineExecution :: StartPipelineExecution -> TestTree
requestStartPipelineExecution =
  req
    "StartPipelineExecution"
    "fixture/StartPipelineExecution.yaml"

requestPollForThirdPartyJobs :: PollForThirdPartyJobs -> TestTree
requestPollForThirdPartyJobs =
  req
    "PollForThirdPartyJobs"
    "fixture/PollForThirdPartyJobs.yaml"

requestListActionExecutions :: ListActionExecutions -> TestTree
requestListActionExecutions =
  req
    "ListActionExecutions"
    "fixture/ListActionExecutions.yaml"

requestRegisterWebhookWithThirdParty :: RegisterWebhookWithThirdParty -> TestTree
requestRegisterWebhookWithThirdParty =
  req
    "RegisterWebhookWithThirdParty"
    "fixture/RegisterWebhookWithThirdParty.yaml"

requestEnableStageTransition :: EnableStageTransition -> TestTree
requestEnableStageTransition =
  req
    "EnableStageTransition"
    "fixture/EnableStageTransition.yaml"

requestAcknowledgeThirdPartyJob :: AcknowledgeThirdPartyJob -> TestTree
requestAcknowledgeThirdPartyJob =
  req
    "AcknowledgeThirdPartyJob"
    "fixture/AcknowledgeThirdPartyJob.yaml"

requestDeleteWebhook :: DeleteWebhook -> TestTree
requestDeleteWebhook =
  req
    "DeleteWebhook"
    "fixture/DeleteWebhook.yaml"

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

requestAcknowledgeJob :: AcknowledgeJob -> TestTree
requestAcknowledgeJob =
  req
    "AcknowledgeJob"
    "fixture/AcknowledgeJob.yaml"

requestUpdateActionType :: UpdateActionType -> TestTree
requestUpdateActionType =
  req
    "UpdateActionType"
    "fixture/UpdateActionType.yaml"

requestDisableStageTransition :: DisableStageTransition -> TestTree
requestDisableStageTransition =
  req
    "DisableStageTransition"
    "fixture/DisableStageTransition.yaml"

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

requestGetPipelineExecution :: GetPipelineExecution -> TestTree
requestGetPipelineExecution =
  req
    "GetPipelineExecution"
    "fixture/GetPipelineExecution.yaml"

requestGetThirdPartyJobDetails :: GetThirdPartyJobDetails -> TestTree
requestGetThirdPartyJobDetails =
  req
    "GetThirdPartyJobDetails"
    "fixture/GetThirdPartyJobDetails.yaml"

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

requestListWebhooks :: ListWebhooks -> TestTree
requestListWebhooks =
  req
    "ListWebhooks"
    "fixture/ListWebhooks.yaml"

requestPutWebhook :: PutWebhook -> TestTree
requestPutWebhook =
  req
    "PutWebhook"
    "fixture/PutWebhook.yaml"

-- Responses

responseListActionTypes :: ListActionTypesResponse -> TestTree
responseListActionTypes =
  res
    "ListActionTypesResponse"
    "fixture/ListActionTypesResponse.proto"
    defaultService
    (Proxy :: Proxy ListActionTypes)

responsePutJobSuccessResult :: PutJobSuccessResultResponse -> TestTree
responsePutJobSuccessResult =
  res
    "PutJobSuccessResultResponse"
    "fixture/PutJobSuccessResultResponse.proto"
    defaultService
    (Proxy :: Proxy PutJobSuccessResult)

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

responsePutThirdPartyJobSuccessResult :: PutThirdPartyJobSuccessResultResponse -> TestTree
responsePutThirdPartyJobSuccessResult =
  res
    "PutThirdPartyJobSuccessResultResponse"
    "fixture/PutThirdPartyJobSuccessResultResponse.proto"
    defaultService
    (Proxy :: Proxy PutThirdPartyJobSuccessResult)

responseRetryStageExecution :: RetryStageExecutionResponse -> TestTree
responseRetryStageExecution =
  res
    "RetryStageExecutionResponse"
    "fixture/RetryStageExecutionResponse.proto"
    defaultService
    (Proxy :: Proxy RetryStageExecution)

responseCreatePipeline :: CreatePipelineResponse -> TestTree
responseCreatePipeline =
  res
    "CreatePipelineResponse"
    "fixture/CreatePipelineResponse.proto"
    defaultService
    (Proxy :: Proxy CreatePipeline)

responseDeletePipeline :: DeletePipelineResponse -> TestTree
responseDeletePipeline =
  res
    "DeletePipelineResponse"
    "fixture/DeletePipelineResponse.proto"
    defaultService
    (Proxy :: Proxy DeletePipeline)

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

responseStopPipelineExecution :: StopPipelineExecutionResponse -> TestTree
responseStopPipelineExecution =
  res
    "StopPipelineExecutionResponse"
    "fixture/StopPipelineExecutionResponse.proto"
    defaultService
    (Proxy :: Proxy StopPipelineExecution)

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

responseStartPipelineExecution :: StartPipelineExecutionResponse -> TestTree
responseStartPipelineExecution =
  res
    "StartPipelineExecutionResponse"
    "fixture/StartPipelineExecutionResponse.proto"
    defaultService
    (Proxy :: Proxy StartPipelineExecution)

responsePollForThirdPartyJobs :: PollForThirdPartyJobsResponse -> TestTree
responsePollForThirdPartyJobs =
  res
    "PollForThirdPartyJobsResponse"
    "fixture/PollForThirdPartyJobsResponse.proto"
    defaultService
    (Proxy :: Proxy PollForThirdPartyJobs)

responseListActionExecutions :: ListActionExecutionsResponse -> TestTree
responseListActionExecutions =
  res
    "ListActionExecutionsResponse"
    "fixture/ListActionExecutionsResponse.proto"
    defaultService
    (Proxy :: Proxy ListActionExecutions)

responseRegisterWebhookWithThirdParty :: RegisterWebhookWithThirdPartyResponse -> TestTree
responseRegisterWebhookWithThirdParty =
  res
    "RegisterWebhookWithThirdPartyResponse"
    "fixture/RegisterWebhookWithThirdPartyResponse.proto"
    defaultService
    (Proxy :: Proxy RegisterWebhookWithThirdParty)

responseEnableStageTransition :: EnableStageTransitionResponse -> TestTree
responseEnableStageTransition =
  res
    "EnableStageTransitionResponse"
    "fixture/EnableStageTransitionResponse.proto"
    defaultService
    (Proxy :: Proxy EnableStageTransition)

responseAcknowledgeThirdPartyJob :: AcknowledgeThirdPartyJobResponse -> TestTree
responseAcknowledgeThirdPartyJob =
  res
    "AcknowledgeThirdPartyJobResponse"
    "fixture/AcknowledgeThirdPartyJobResponse.proto"
    defaultService
    (Proxy :: Proxy AcknowledgeThirdPartyJob)

responseDeleteWebhook :: DeleteWebhookResponse -> TestTree
responseDeleteWebhook =
  res
    "DeleteWebhookResponse"
    "fixture/DeleteWebhookResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteWebhook)

responsePutJobFailureResult :: PutJobFailureResultResponse -> TestTree
responsePutJobFailureResult =
  res
    "PutJobFailureResultResponse"
    "fixture/PutJobFailureResultResponse.proto"
    defaultService
    (Proxy :: Proxy PutJobFailureResult)

responsePutApprovalResult :: PutApprovalResultResponse -> TestTree
responsePutApprovalResult =
  res
    "PutApprovalResultResponse"
    "fixture/PutApprovalResultResponse.proto"
    defaultService
    (Proxy :: Proxy PutApprovalResult)

responseAcknowledgeJob :: AcknowledgeJobResponse -> TestTree
responseAcknowledgeJob =
  res
    "AcknowledgeJobResponse"
    "fixture/AcknowledgeJobResponse.proto"
    defaultService
    (Proxy :: Proxy AcknowledgeJob)

responseUpdateActionType :: UpdateActionTypeResponse -> TestTree
responseUpdateActionType =
  res
    "UpdateActionTypeResponse"
    "fixture/UpdateActionTypeResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateActionType)

responseDisableStageTransition :: DisableStageTransitionResponse -> TestTree
responseDisableStageTransition =
  res
    "DisableStageTransitionResponse"
    "fixture/DisableStageTransitionResponse.proto"
    defaultService
    (Proxy :: Proxy DisableStageTransition)

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

responseGetPipelineExecution :: GetPipelineExecutionResponse -> TestTree
responseGetPipelineExecution =
  res
    "GetPipelineExecutionResponse"
    "fixture/GetPipelineExecutionResponse.proto"
    defaultService
    (Proxy :: Proxy GetPipelineExecution)

responseGetThirdPartyJobDetails :: GetThirdPartyJobDetailsResponse -> TestTree
responseGetThirdPartyJobDetails =
  res
    "GetThirdPartyJobDetailsResponse"
    "fixture/GetThirdPartyJobDetailsResponse.proto"
    defaultService
    (Proxy :: Proxy GetThirdPartyJobDetails)

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

responsePutThirdPartyJobFailureResult :: PutThirdPartyJobFailureResultResponse -> TestTree
responsePutThirdPartyJobFailureResult =
  res
    "PutThirdPartyJobFailureResultResponse"
    "fixture/PutThirdPartyJobFailureResultResponse.proto"
    defaultService
    (Proxy :: Proxy PutThirdPartyJobFailureResult)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy :: Proxy ListTagsForResource)

responseListWebhooks :: ListWebhooksResponse -> TestTree
responseListWebhooks =
  res
    "ListWebhooksResponse"
    "fixture/ListWebhooksResponse.proto"
    defaultService
    (Proxy :: Proxy ListWebhooks)

responsePutWebhook :: PutWebhookResponse -> TestTree
responsePutWebhook =
  res
    "PutWebhookResponse"
    "fixture/PutWebhookResponse.proto"
    defaultService
    (Proxy :: Proxy PutWebhook)
