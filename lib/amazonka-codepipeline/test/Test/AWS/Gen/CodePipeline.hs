{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.CodePipeline
-- Copyright   : (c) 2013-2020 Brendan Hay
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
--         [ requestGetPipeline $
--             mkGetPipeline
--
--         , requestPutJobFailureResult $
--             mkPutJobFailureResult
--
--         , requestPutApprovalResult $
--             mkPutApprovalResult
--
--         , requestAcknowledgeThirdPartyJob $
--             mkAcknowledgeThirdPartyJob
--
--         , requestPutThirdPartyJobFailureResult $
--             mkPutThirdPartyJobFailureResult
--
--         , requestListTagsForResource $
--             mkListTagsForResource
--
--         , requestRegisterWebhookWithThirdParty $
--             mkRegisterWebhookWithThirdParty
--
--         , requestPollForThirdPartyJobs $
--             mkPollForThirdPartyJobs
--
--         , requestPollForJobs $
--             mkPollForJobs
--
--         , requestStartPipelineExecution $
--             mkStartPipelineExecution
--
--         , requestUpdatePipeline $
--             mkUpdatePipeline
--
--         , requestDeletePipeline $
--             mkDeletePipeline
--
--         , requestGetPipelineState $
--             mkGetPipelineState
--
--         , requestGetJobDetails $
--             mkGetJobDetails
--
--         , requestListPipelines $
--             mkListPipelines
--
--         , requestRetryStageExecution $
--             mkRetryStageExecution
--
--         , requestGetPipelineExecution $
--             mkGetPipelineExecution
--
--         , requestPutJobSuccessResult $
--             mkPutJobSuccessResult
--
--         , requestDeregisterWebhookWithThirdParty $
--             mkDeregisterWebhookWithThirdParty
--
--         , requestDeleteCustomActionType $
--             mkDeleteCustomActionType
--
--         , requestPutActionRevision $
--             mkPutActionRevision
--
--         , requestDisableStageTransition $
--             mkDisableStageTransition
--
--         , requestListActionTypes $
--             mkListActionTypes
--
--         , requestAcknowledgeJob $
--             mkAcknowledgeJob
--
--         , requestEnableStageTransition $
--             mkEnableStageTransition
--
--         , requestDeleteWebhook $
--             mkDeleteWebhook
--
--         , requestPutWebhook $
--             mkPutWebhook
--
--         , requestListWebhooks $
--             mkListWebhooks
--
--         , requestListActionExecutions $
--             mkListActionExecutions
--
--         , requestStopPipelineExecution $
--             mkStopPipelineExecution
--
--         , requestTagResource $
--             mkTagResource
--
--         , requestUntagResource $
--             mkUntagResource
--
--         , requestCreatePipeline $
--             mkCreatePipeline
--
--         , requestGetThirdPartyJobDetails $
--             mkGetThirdPartyJobDetails
--
--         , requestPutThirdPartyJobSuccessResult $
--             mkPutThirdPartyJobSuccessResult
--
--         , requestCreateCustomActionType $
--             mkCreateCustomActionType
--
--         , requestListPipelineExecutions $
--             mkListPipelineExecutions
--
--           ]

--     , testGroup "response"
--         [ responseGetPipeline $
--             mkGetPipelineResponse
--
--         , responsePutJobFailureResult $
--             mkPutJobFailureResultResponse
--
--         , responsePutApprovalResult $
--             mkPutApprovalResultResponse
--
--         , responseAcknowledgeThirdPartyJob $
--             mkAcknowledgeThirdPartyJobResponse
--
--         , responsePutThirdPartyJobFailureResult $
--             mkPutThirdPartyJobFailureResultResponse
--
--         , responseListTagsForResource $
--             mkListTagsForResourceResponse
--
--         , responseRegisterWebhookWithThirdParty $
--             mkRegisterWebhookWithThirdPartyResponse
--
--         , responsePollForThirdPartyJobs $
--             mkPollForThirdPartyJobsResponse
--
--         , responsePollForJobs $
--             mkPollForJobsResponse
--
--         , responseStartPipelineExecution $
--             mkStartPipelineExecutionResponse
--
--         , responseUpdatePipeline $
--             mkUpdatePipelineResponse
--
--         , responseDeletePipeline $
--             mkDeletePipelineResponse
--
--         , responseGetPipelineState $
--             mkGetPipelineStateResponse
--
--         , responseGetJobDetails $
--             mkGetJobDetailsResponse
--
--         , responseListPipelines $
--             mkListPipelinesResponse
--
--         , responseRetryStageExecution $
--             mkRetryStageExecutionResponse
--
--         , responseGetPipelineExecution $
--             mkGetPipelineExecutionResponse
--
--         , responsePutJobSuccessResult $
--             mkPutJobSuccessResultResponse
--
--         , responseDeregisterWebhookWithThirdParty $
--             mkDeregisterWebhookWithThirdPartyResponse
--
--         , responseDeleteCustomActionType $
--             mkDeleteCustomActionTypeResponse
--
--         , responsePutActionRevision $
--             mkPutActionRevisionResponse
--
--         , responseDisableStageTransition $
--             mkDisableStageTransitionResponse
--
--         , responseListActionTypes $
--             mkListActionTypesResponse
--
--         , responseAcknowledgeJob $
--             mkAcknowledgeJobResponse
--
--         , responseEnableStageTransition $
--             mkEnableStageTransitionResponse
--
--         , responseDeleteWebhook $
--             mkDeleteWebhookResponse
--
--         , responsePutWebhook $
--             mkPutWebhookResponse
--
--         , responseListWebhooks $
--             mkListWebhooksResponse
--
--         , responseListActionExecutions $
--             mkListActionExecutionsResponse
--
--         , responseStopPipelineExecution $
--             mkStopPipelineExecutionResponse
--
--         , responseTagResource $
--             mkTagResourceResponse
--
--         , responseUntagResource $
--             mkUntagResourceResponse
--
--         , responseCreatePipeline $
--             mkCreatePipelineResponse
--
--         , responseGetThirdPartyJobDetails $
--             mkGetThirdPartyJobDetailsResponse
--
--         , responsePutThirdPartyJobSuccessResult $
--             mkPutThirdPartyJobSuccessResultResponse
--
--         , responseCreateCustomActionType $
--             mkCreateCustomActionTypeResponse
--
--         , responseListPipelineExecutions $
--             mkListPipelineExecutionsResponse
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
    codePipelineService
    (Proxy :: Proxy GetPipeline)

responsePutJobFailureResult :: PutJobFailureResultResponse -> TestTree
responsePutJobFailureResult =
  res
    "PutJobFailureResultResponse"
    "fixture/PutJobFailureResultResponse.proto"
    codePipelineService
    (Proxy :: Proxy PutJobFailureResult)

responsePutApprovalResult :: PutApprovalResultResponse -> TestTree
responsePutApprovalResult =
  res
    "PutApprovalResultResponse"
    "fixture/PutApprovalResultResponse.proto"
    codePipelineService
    (Proxy :: Proxy PutApprovalResult)

responseAcknowledgeThirdPartyJob :: AcknowledgeThirdPartyJobResponse -> TestTree
responseAcknowledgeThirdPartyJob =
  res
    "AcknowledgeThirdPartyJobResponse"
    "fixture/AcknowledgeThirdPartyJobResponse.proto"
    codePipelineService
    (Proxy :: Proxy AcknowledgeThirdPartyJob)

responsePutThirdPartyJobFailureResult :: PutThirdPartyJobFailureResultResponse -> TestTree
responsePutThirdPartyJobFailureResult =
  res
    "PutThirdPartyJobFailureResultResponse"
    "fixture/PutThirdPartyJobFailureResultResponse.proto"
    codePipelineService
    (Proxy :: Proxy PutThirdPartyJobFailureResult)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    codePipelineService
    (Proxy :: Proxy ListTagsForResource)

responseRegisterWebhookWithThirdParty :: RegisterWebhookWithThirdPartyResponse -> TestTree
responseRegisterWebhookWithThirdParty =
  res
    "RegisterWebhookWithThirdPartyResponse"
    "fixture/RegisterWebhookWithThirdPartyResponse.proto"
    codePipelineService
    (Proxy :: Proxy RegisterWebhookWithThirdParty)

responsePollForThirdPartyJobs :: PollForThirdPartyJobsResponse -> TestTree
responsePollForThirdPartyJobs =
  res
    "PollForThirdPartyJobsResponse"
    "fixture/PollForThirdPartyJobsResponse.proto"
    codePipelineService
    (Proxy :: Proxy PollForThirdPartyJobs)

responsePollForJobs :: PollForJobsResponse -> TestTree
responsePollForJobs =
  res
    "PollForJobsResponse"
    "fixture/PollForJobsResponse.proto"
    codePipelineService
    (Proxy :: Proxy PollForJobs)

responseStartPipelineExecution :: StartPipelineExecutionResponse -> TestTree
responseStartPipelineExecution =
  res
    "StartPipelineExecutionResponse"
    "fixture/StartPipelineExecutionResponse.proto"
    codePipelineService
    (Proxy :: Proxy StartPipelineExecution)

responseUpdatePipeline :: UpdatePipelineResponse -> TestTree
responseUpdatePipeline =
  res
    "UpdatePipelineResponse"
    "fixture/UpdatePipelineResponse.proto"
    codePipelineService
    (Proxy :: Proxy UpdatePipeline)

responseDeletePipeline :: DeletePipelineResponse -> TestTree
responseDeletePipeline =
  res
    "DeletePipelineResponse"
    "fixture/DeletePipelineResponse.proto"
    codePipelineService
    (Proxy :: Proxy DeletePipeline)

responseGetPipelineState :: GetPipelineStateResponse -> TestTree
responseGetPipelineState =
  res
    "GetPipelineStateResponse"
    "fixture/GetPipelineStateResponse.proto"
    codePipelineService
    (Proxy :: Proxy GetPipelineState)

responseGetJobDetails :: GetJobDetailsResponse -> TestTree
responseGetJobDetails =
  res
    "GetJobDetailsResponse"
    "fixture/GetJobDetailsResponse.proto"
    codePipelineService
    (Proxy :: Proxy GetJobDetails)

responseListPipelines :: ListPipelinesResponse -> TestTree
responseListPipelines =
  res
    "ListPipelinesResponse"
    "fixture/ListPipelinesResponse.proto"
    codePipelineService
    (Proxy :: Proxy ListPipelines)

responseRetryStageExecution :: RetryStageExecutionResponse -> TestTree
responseRetryStageExecution =
  res
    "RetryStageExecutionResponse"
    "fixture/RetryStageExecutionResponse.proto"
    codePipelineService
    (Proxy :: Proxy RetryStageExecution)

responseGetPipelineExecution :: GetPipelineExecutionResponse -> TestTree
responseGetPipelineExecution =
  res
    "GetPipelineExecutionResponse"
    "fixture/GetPipelineExecutionResponse.proto"
    codePipelineService
    (Proxy :: Proxy GetPipelineExecution)

responsePutJobSuccessResult :: PutJobSuccessResultResponse -> TestTree
responsePutJobSuccessResult =
  res
    "PutJobSuccessResultResponse"
    "fixture/PutJobSuccessResultResponse.proto"
    codePipelineService
    (Proxy :: Proxy PutJobSuccessResult)

responseDeregisterWebhookWithThirdParty :: DeregisterWebhookWithThirdPartyResponse -> TestTree
responseDeregisterWebhookWithThirdParty =
  res
    "DeregisterWebhookWithThirdPartyResponse"
    "fixture/DeregisterWebhookWithThirdPartyResponse.proto"
    codePipelineService
    (Proxy :: Proxy DeregisterWebhookWithThirdParty)

responseDeleteCustomActionType :: DeleteCustomActionTypeResponse -> TestTree
responseDeleteCustomActionType =
  res
    "DeleteCustomActionTypeResponse"
    "fixture/DeleteCustomActionTypeResponse.proto"
    codePipelineService
    (Proxy :: Proxy DeleteCustomActionType)

responsePutActionRevision :: PutActionRevisionResponse -> TestTree
responsePutActionRevision =
  res
    "PutActionRevisionResponse"
    "fixture/PutActionRevisionResponse.proto"
    codePipelineService
    (Proxy :: Proxy PutActionRevision)

responseDisableStageTransition :: DisableStageTransitionResponse -> TestTree
responseDisableStageTransition =
  res
    "DisableStageTransitionResponse"
    "fixture/DisableStageTransitionResponse.proto"
    codePipelineService
    (Proxy :: Proxy DisableStageTransition)

responseListActionTypes :: ListActionTypesResponse -> TestTree
responseListActionTypes =
  res
    "ListActionTypesResponse"
    "fixture/ListActionTypesResponse.proto"
    codePipelineService
    (Proxy :: Proxy ListActionTypes)

responseAcknowledgeJob :: AcknowledgeJobResponse -> TestTree
responseAcknowledgeJob =
  res
    "AcknowledgeJobResponse"
    "fixture/AcknowledgeJobResponse.proto"
    codePipelineService
    (Proxy :: Proxy AcknowledgeJob)

responseEnableStageTransition :: EnableStageTransitionResponse -> TestTree
responseEnableStageTransition =
  res
    "EnableStageTransitionResponse"
    "fixture/EnableStageTransitionResponse.proto"
    codePipelineService
    (Proxy :: Proxy EnableStageTransition)

responseDeleteWebhook :: DeleteWebhookResponse -> TestTree
responseDeleteWebhook =
  res
    "DeleteWebhookResponse"
    "fixture/DeleteWebhookResponse.proto"
    codePipelineService
    (Proxy :: Proxy DeleteWebhook)

responsePutWebhook :: PutWebhookResponse -> TestTree
responsePutWebhook =
  res
    "PutWebhookResponse"
    "fixture/PutWebhookResponse.proto"
    codePipelineService
    (Proxy :: Proxy PutWebhook)

responseListWebhooks :: ListWebhooksResponse -> TestTree
responseListWebhooks =
  res
    "ListWebhooksResponse"
    "fixture/ListWebhooksResponse.proto"
    codePipelineService
    (Proxy :: Proxy ListWebhooks)

responseListActionExecutions :: ListActionExecutionsResponse -> TestTree
responseListActionExecutions =
  res
    "ListActionExecutionsResponse"
    "fixture/ListActionExecutionsResponse.proto"
    codePipelineService
    (Proxy :: Proxy ListActionExecutions)

responseStopPipelineExecution :: StopPipelineExecutionResponse -> TestTree
responseStopPipelineExecution =
  res
    "StopPipelineExecutionResponse"
    "fixture/StopPipelineExecutionResponse.proto"
    codePipelineService
    (Proxy :: Proxy StopPipelineExecution)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    codePipelineService
    (Proxy :: Proxy TagResource)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    codePipelineService
    (Proxy :: Proxy UntagResource)

responseCreatePipeline :: CreatePipelineResponse -> TestTree
responseCreatePipeline =
  res
    "CreatePipelineResponse"
    "fixture/CreatePipelineResponse.proto"
    codePipelineService
    (Proxy :: Proxy CreatePipeline)

responseGetThirdPartyJobDetails :: GetThirdPartyJobDetailsResponse -> TestTree
responseGetThirdPartyJobDetails =
  res
    "GetThirdPartyJobDetailsResponse"
    "fixture/GetThirdPartyJobDetailsResponse.proto"
    codePipelineService
    (Proxy :: Proxy GetThirdPartyJobDetails)

responsePutThirdPartyJobSuccessResult :: PutThirdPartyJobSuccessResultResponse -> TestTree
responsePutThirdPartyJobSuccessResult =
  res
    "PutThirdPartyJobSuccessResultResponse"
    "fixture/PutThirdPartyJobSuccessResultResponse.proto"
    codePipelineService
    (Proxy :: Proxy PutThirdPartyJobSuccessResult)

responseCreateCustomActionType :: CreateCustomActionTypeResponse -> TestTree
responseCreateCustomActionType =
  res
    "CreateCustomActionTypeResponse"
    "fixture/CreateCustomActionTypeResponse.proto"
    codePipelineService
    (Proxy :: Proxy CreateCustomActionType)

responseListPipelineExecutions :: ListPipelineExecutionsResponse -> TestTree
responseListPipelineExecutions =
  res
    "ListPipelineExecutionsResponse"
    "fixture/ListPipelineExecutionsResponse.proto"
    codePipelineService
    (Proxy :: Proxy ListPipelineExecutions)
