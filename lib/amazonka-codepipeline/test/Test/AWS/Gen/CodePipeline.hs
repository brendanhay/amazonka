{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.CodePipeline
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
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
--             getPipeline
--
--         , requestPutJobFailureResult $
--             putJobFailureResult
--
--         , requestPutApprovalResult $
--             putApprovalResult
--
--         , requestAcknowledgeThirdPartyJob $
--             acknowledgeThirdPartyJob
--
--         , requestPutThirdPartyJobFailureResult $
--             putThirdPartyJobFailureResult
--
--         , requestRegisterWebhookWithThirdParty $
--             registerWebhookWithThirdParty
--
--         , requestPollForThirdPartyJobs $
--             pollForThirdPartyJobs
--
--         , requestPollForJobs $
--             pollForJobs
--
--         , requestStartPipelineExecution $
--             startPipelineExecution
--
--         , requestUpdatePipeline $
--             updatePipeline
--
--         , requestDeletePipeline $
--             deletePipeline
--
--         , requestGetPipelineState $
--             getPipelineState
--
--         , requestGetJobDetails $
--             getJobDetails
--
--         , requestListPipelines $
--             listPipelines
--
--         , requestRetryStageExecution $
--             retryStageExecution
--
--         , requestGetPipelineExecution $
--             getPipelineExecution
--
--         , requestPutJobSuccessResult $
--             putJobSuccessResult
--
--         , requestDeregisterWebhookWithThirdParty $
--             deregisterWebhookWithThirdParty
--
--         , requestDeleteCustomActionType $
--             deleteCustomActionType
--
--         , requestPutActionRevision $
--             putActionRevision
--
--         , requestDisableStageTransition $
--             disableStageTransition
--
--         , requestListActionTypes $
--             listActionTypes
--
--         , requestAcknowledgeJob $
--             acknowledgeJob
--
--         , requestEnableStageTransition $
--             enableStageTransition
--
--         , requestDeleteWebhook $
--             deleteWebhook
--
--         , requestPutWebhook $
--             putWebhook
--
--         , requestListWebhooks $
--             listWebhooks
--
--         , requestCreatePipeline $
--             createPipeline
--
--         , requestGetThirdPartyJobDetails $
--             getThirdPartyJobDetails
--
--         , requestPutThirdPartyJobSuccessResult $
--             putThirdPartyJobSuccessResult
--
--         , requestCreateCustomActionType $
--             createCustomActionType
--
--         , requestListPipelineExecutions $
--             listPipelineExecutions
--
--           ]

--     , testGroup "response"
--         [ responseGetPipeline $
--             getPipelineResponse
--
--         , responsePutJobFailureResult $
--             putJobFailureResultResponse
--
--         , responsePutApprovalResult $
--             putApprovalResultResponse
--
--         , responseAcknowledgeThirdPartyJob $
--             acknowledgeThirdPartyJobResponse
--
--         , responsePutThirdPartyJobFailureResult $
--             putThirdPartyJobFailureResultResponse
--
--         , responseRegisterWebhookWithThirdParty $
--             registerWebhookWithThirdPartyResponse
--
--         , responsePollForThirdPartyJobs $
--             pollForThirdPartyJobsResponse
--
--         , responsePollForJobs $
--             pollForJobsResponse
--
--         , responseStartPipelineExecution $
--             startPipelineExecutionResponse
--
--         , responseUpdatePipeline $
--             updatePipelineResponse
--
--         , responseDeletePipeline $
--             deletePipelineResponse
--
--         , responseGetPipelineState $
--             getPipelineStateResponse
--
--         , responseGetJobDetails $
--             getJobDetailsResponse
--
--         , responseListPipelines $
--             listPipelinesResponse
--
--         , responseRetryStageExecution $
--             retryStageExecutionResponse
--
--         , responseGetPipelineExecution $
--             getPipelineExecutionResponse
--
--         , responsePutJobSuccessResult $
--             putJobSuccessResultResponse
--
--         , responseDeregisterWebhookWithThirdParty $
--             deregisterWebhookWithThirdPartyResponse
--
--         , responseDeleteCustomActionType $
--             deleteCustomActionTypeResponse
--
--         , responsePutActionRevision $
--             putActionRevisionResponse
--
--         , responseDisableStageTransition $
--             disableStageTransitionResponse
--
--         , responseListActionTypes $
--             listActionTypesResponse
--
--         , responseAcknowledgeJob $
--             acknowledgeJobResponse
--
--         , responseEnableStageTransition $
--             enableStageTransitionResponse
--
--         , responseDeleteWebhook $
--             deleteWebhookResponse
--
--         , responsePutWebhook $
--             putWebhookResponse
--
--         , responseListWebhooks $
--             listWebhooksResponse
--
--         , responseCreatePipeline $
--             createPipelineResponse
--
--         , responseGetThirdPartyJobDetails $
--             getThirdPartyJobDetailsResponse
--
--         , responsePutThirdPartyJobSuccessResult $
--             putThirdPartyJobSuccessResultResponse
--
--         , responseCreateCustomActionType $
--             createCustomActionTypeResponse
--
--         , responseListPipelineExecutions $
--             listPipelineExecutionsResponse
--
--           ]
--     ]

-- Requests

requestGetPipeline :: GetPipeline -> TestTree
requestGetPipeline = req
    "GetPipeline"
    "fixture/GetPipeline.yaml"

requestPutJobFailureResult :: PutJobFailureResult -> TestTree
requestPutJobFailureResult = req
    "PutJobFailureResult"
    "fixture/PutJobFailureResult.yaml"

requestPutApprovalResult :: PutApprovalResult -> TestTree
requestPutApprovalResult = req
    "PutApprovalResult"
    "fixture/PutApprovalResult.yaml"

requestAcknowledgeThirdPartyJob :: AcknowledgeThirdPartyJob -> TestTree
requestAcknowledgeThirdPartyJob = req
    "AcknowledgeThirdPartyJob"
    "fixture/AcknowledgeThirdPartyJob.yaml"

requestPutThirdPartyJobFailureResult :: PutThirdPartyJobFailureResult -> TestTree
requestPutThirdPartyJobFailureResult = req
    "PutThirdPartyJobFailureResult"
    "fixture/PutThirdPartyJobFailureResult.yaml"

requestRegisterWebhookWithThirdParty :: RegisterWebhookWithThirdParty -> TestTree
requestRegisterWebhookWithThirdParty = req
    "RegisterWebhookWithThirdParty"
    "fixture/RegisterWebhookWithThirdParty.yaml"

requestPollForThirdPartyJobs :: PollForThirdPartyJobs -> TestTree
requestPollForThirdPartyJobs = req
    "PollForThirdPartyJobs"
    "fixture/PollForThirdPartyJobs.yaml"

requestPollForJobs :: PollForJobs -> TestTree
requestPollForJobs = req
    "PollForJobs"
    "fixture/PollForJobs.yaml"

requestStartPipelineExecution :: StartPipelineExecution -> TestTree
requestStartPipelineExecution = req
    "StartPipelineExecution"
    "fixture/StartPipelineExecution.yaml"

requestUpdatePipeline :: UpdatePipeline -> TestTree
requestUpdatePipeline = req
    "UpdatePipeline"
    "fixture/UpdatePipeline.yaml"

requestDeletePipeline :: DeletePipeline -> TestTree
requestDeletePipeline = req
    "DeletePipeline"
    "fixture/DeletePipeline.yaml"

requestGetPipelineState :: GetPipelineState -> TestTree
requestGetPipelineState = req
    "GetPipelineState"
    "fixture/GetPipelineState.yaml"

requestGetJobDetails :: GetJobDetails -> TestTree
requestGetJobDetails = req
    "GetJobDetails"
    "fixture/GetJobDetails.yaml"

requestListPipelines :: ListPipelines -> TestTree
requestListPipelines = req
    "ListPipelines"
    "fixture/ListPipelines.yaml"

requestRetryStageExecution :: RetryStageExecution -> TestTree
requestRetryStageExecution = req
    "RetryStageExecution"
    "fixture/RetryStageExecution.yaml"

requestGetPipelineExecution :: GetPipelineExecution -> TestTree
requestGetPipelineExecution = req
    "GetPipelineExecution"
    "fixture/GetPipelineExecution.yaml"

requestPutJobSuccessResult :: PutJobSuccessResult -> TestTree
requestPutJobSuccessResult = req
    "PutJobSuccessResult"
    "fixture/PutJobSuccessResult.yaml"

requestDeregisterWebhookWithThirdParty :: DeregisterWebhookWithThirdParty -> TestTree
requestDeregisterWebhookWithThirdParty = req
    "DeregisterWebhookWithThirdParty"
    "fixture/DeregisterWebhookWithThirdParty.yaml"

requestDeleteCustomActionType :: DeleteCustomActionType -> TestTree
requestDeleteCustomActionType = req
    "DeleteCustomActionType"
    "fixture/DeleteCustomActionType.yaml"

requestPutActionRevision :: PutActionRevision -> TestTree
requestPutActionRevision = req
    "PutActionRevision"
    "fixture/PutActionRevision.yaml"

requestDisableStageTransition :: DisableStageTransition -> TestTree
requestDisableStageTransition = req
    "DisableStageTransition"
    "fixture/DisableStageTransition.yaml"

requestListActionTypes :: ListActionTypes -> TestTree
requestListActionTypes = req
    "ListActionTypes"
    "fixture/ListActionTypes.yaml"

requestAcknowledgeJob :: AcknowledgeJob -> TestTree
requestAcknowledgeJob = req
    "AcknowledgeJob"
    "fixture/AcknowledgeJob.yaml"

requestEnableStageTransition :: EnableStageTransition -> TestTree
requestEnableStageTransition = req
    "EnableStageTransition"
    "fixture/EnableStageTransition.yaml"

requestDeleteWebhook :: DeleteWebhook -> TestTree
requestDeleteWebhook = req
    "DeleteWebhook"
    "fixture/DeleteWebhook.yaml"

requestPutWebhook :: PutWebhook -> TestTree
requestPutWebhook = req
    "PutWebhook"
    "fixture/PutWebhook.yaml"

requestListWebhooks :: ListWebhooks -> TestTree
requestListWebhooks = req
    "ListWebhooks"
    "fixture/ListWebhooks.yaml"

requestCreatePipeline :: CreatePipeline -> TestTree
requestCreatePipeline = req
    "CreatePipeline"
    "fixture/CreatePipeline.yaml"

requestGetThirdPartyJobDetails :: GetThirdPartyJobDetails -> TestTree
requestGetThirdPartyJobDetails = req
    "GetThirdPartyJobDetails"
    "fixture/GetThirdPartyJobDetails.yaml"

requestPutThirdPartyJobSuccessResult :: PutThirdPartyJobSuccessResult -> TestTree
requestPutThirdPartyJobSuccessResult = req
    "PutThirdPartyJobSuccessResult"
    "fixture/PutThirdPartyJobSuccessResult.yaml"

requestCreateCustomActionType :: CreateCustomActionType -> TestTree
requestCreateCustomActionType = req
    "CreateCustomActionType"
    "fixture/CreateCustomActionType.yaml"

requestListPipelineExecutions :: ListPipelineExecutions -> TestTree
requestListPipelineExecutions = req
    "ListPipelineExecutions"
    "fixture/ListPipelineExecutions.yaml"

-- Responses

responseGetPipeline :: GetPipelineResponse -> TestTree
responseGetPipeline = res
    "GetPipelineResponse"
    "fixture/GetPipelineResponse.proto"
    codePipeline
    (Proxy :: Proxy GetPipeline)

responsePutJobFailureResult :: PutJobFailureResultResponse -> TestTree
responsePutJobFailureResult = res
    "PutJobFailureResultResponse"
    "fixture/PutJobFailureResultResponse.proto"
    codePipeline
    (Proxy :: Proxy PutJobFailureResult)

responsePutApprovalResult :: PutApprovalResultResponse -> TestTree
responsePutApprovalResult = res
    "PutApprovalResultResponse"
    "fixture/PutApprovalResultResponse.proto"
    codePipeline
    (Proxy :: Proxy PutApprovalResult)

responseAcknowledgeThirdPartyJob :: AcknowledgeThirdPartyJobResponse -> TestTree
responseAcknowledgeThirdPartyJob = res
    "AcknowledgeThirdPartyJobResponse"
    "fixture/AcknowledgeThirdPartyJobResponse.proto"
    codePipeline
    (Proxy :: Proxy AcknowledgeThirdPartyJob)

responsePutThirdPartyJobFailureResult :: PutThirdPartyJobFailureResultResponse -> TestTree
responsePutThirdPartyJobFailureResult = res
    "PutThirdPartyJobFailureResultResponse"
    "fixture/PutThirdPartyJobFailureResultResponse.proto"
    codePipeline
    (Proxy :: Proxy PutThirdPartyJobFailureResult)

responseRegisterWebhookWithThirdParty :: RegisterWebhookWithThirdPartyResponse -> TestTree
responseRegisterWebhookWithThirdParty = res
    "RegisterWebhookWithThirdPartyResponse"
    "fixture/RegisterWebhookWithThirdPartyResponse.proto"
    codePipeline
    (Proxy :: Proxy RegisterWebhookWithThirdParty)

responsePollForThirdPartyJobs :: PollForThirdPartyJobsResponse -> TestTree
responsePollForThirdPartyJobs = res
    "PollForThirdPartyJobsResponse"
    "fixture/PollForThirdPartyJobsResponse.proto"
    codePipeline
    (Proxy :: Proxy PollForThirdPartyJobs)

responsePollForJobs :: PollForJobsResponse -> TestTree
responsePollForJobs = res
    "PollForJobsResponse"
    "fixture/PollForJobsResponse.proto"
    codePipeline
    (Proxy :: Proxy PollForJobs)

responseStartPipelineExecution :: StartPipelineExecutionResponse -> TestTree
responseStartPipelineExecution = res
    "StartPipelineExecutionResponse"
    "fixture/StartPipelineExecutionResponse.proto"
    codePipeline
    (Proxy :: Proxy StartPipelineExecution)

responseUpdatePipeline :: UpdatePipelineResponse -> TestTree
responseUpdatePipeline = res
    "UpdatePipelineResponse"
    "fixture/UpdatePipelineResponse.proto"
    codePipeline
    (Proxy :: Proxy UpdatePipeline)

responseDeletePipeline :: DeletePipelineResponse -> TestTree
responseDeletePipeline = res
    "DeletePipelineResponse"
    "fixture/DeletePipelineResponse.proto"
    codePipeline
    (Proxy :: Proxy DeletePipeline)

responseGetPipelineState :: GetPipelineStateResponse -> TestTree
responseGetPipelineState = res
    "GetPipelineStateResponse"
    "fixture/GetPipelineStateResponse.proto"
    codePipeline
    (Proxy :: Proxy GetPipelineState)

responseGetJobDetails :: GetJobDetailsResponse -> TestTree
responseGetJobDetails = res
    "GetJobDetailsResponse"
    "fixture/GetJobDetailsResponse.proto"
    codePipeline
    (Proxy :: Proxy GetJobDetails)

responseListPipelines :: ListPipelinesResponse -> TestTree
responseListPipelines = res
    "ListPipelinesResponse"
    "fixture/ListPipelinesResponse.proto"
    codePipeline
    (Proxy :: Proxy ListPipelines)

responseRetryStageExecution :: RetryStageExecutionResponse -> TestTree
responseRetryStageExecution = res
    "RetryStageExecutionResponse"
    "fixture/RetryStageExecutionResponse.proto"
    codePipeline
    (Proxy :: Proxy RetryStageExecution)

responseGetPipelineExecution :: GetPipelineExecutionResponse -> TestTree
responseGetPipelineExecution = res
    "GetPipelineExecutionResponse"
    "fixture/GetPipelineExecutionResponse.proto"
    codePipeline
    (Proxy :: Proxy GetPipelineExecution)

responsePutJobSuccessResult :: PutJobSuccessResultResponse -> TestTree
responsePutJobSuccessResult = res
    "PutJobSuccessResultResponse"
    "fixture/PutJobSuccessResultResponse.proto"
    codePipeline
    (Proxy :: Proxy PutJobSuccessResult)

responseDeregisterWebhookWithThirdParty :: DeregisterWebhookWithThirdPartyResponse -> TestTree
responseDeregisterWebhookWithThirdParty = res
    "DeregisterWebhookWithThirdPartyResponse"
    "fixture/DeregisterWebhookWithThirdPartyResponse.proto"
    codePipeline
    (Proxy :: Proxy DeregisterWebhookWithThirdParty)

responseDeleteCustomActionType :: DeleteCustomActionTypeResponse -> TestTree
responseDeleteCustomActionType = res
    "DeleteCustomActionTypeResponse"
    "fixture/DeleteCustomActionTypeResponse.proto"
    codePipeline
    (Proxy :: Proxy DeleteCustomActionType)

responsePutActionRevision :: PutActionRevisionResponse -> TestTree
responsePutActionRevision = res
    "PutActionRevisionResponse"
    "fixture/PutActionRevisionResponse.proto"
    codePipeline
    (Proxy :: Proxy PutActionRevision)

responseDisableStageTransition :: DisableStageTransitionResponse -> TestTree
responseDisableStageTransition = res
    "DisableStageTransitionResponse"
    "fixture/DisableStageTransitionResponse.proto"
    codePipeline
    (Proxy :: Proxy DisableStageTransition)

responseListActionTypes :: ListActionTypesResponse -> TestTree
responseListActionTypes = res
    "ListActionTypesResponse"
    "fixture/ListActionTypesResponse.proto"
    codePipeline
    (Proxy :: Proxy ListActionTypes)

responseAcknowledgeJob :: AcknowledgeJobResponse -> TestTree
responseAcknowledgeJob = res
    "AcknowledgeJobResponse"
    "fixture/AcknowledgeJobResponse.proto"
    codePipeline
    (Proxy :: Proxy AcknowledgeJob)

responseEnableStageTransition :: EnableStageTransitionResponse -> TestTree
responseEnableStageTransition = res
    "EnableStageTransitionResponse"
    "fixture/EnableStageTransitionResponse.proto"
    codePipeline
    (Proxy :: Proxy EnableStageTransition)

responseDeleteWebhook :: DeleteWebhookResponse -> TestTree
responseDeleteWebhook = res
    "DeleteWebhookResponse"
    "fixture/DeleteWebhookResponse.proto"
    codePipeline
    (Proxy :: Proxy DeleteWebhook)

responsePutWebhook :: PutWebhookResponse -> TestTree
responsePutWebhook = res
    "PutWebhookResponse"
    "fixture/PutWebhookResponse.proto"
    codePipeline
    (Proxy :: Proxy PutWebhook)

responseListWebhooks :: ListWebhooksResponse -> TestTree
responseListWebhooks = res
    "ListWebhooksResponse"
    "fixture/ListWebhooksResponse.proto"
    codePipeline
    (Proxy :: Proxy ListWebhooks)

responseCreatePipeline :: CreatePipelineResponse -> TestTree
responseCreatePipeline = res
    "CreatePipelineResponse"
    "fixture/CreatePipelineResponse.proto"
    codePipeline
    (Proxy :: Proxy CreatePipeline)

responseGetThirdPartyJobDetails :: GetThirdPartyJobDetailsResponse -> TestTree
responseGetThirdPartyJobDetails = res
    "GetThirdPartyJobDetailsResponse"
    "fixture/GetThirdPartyJobDetailsResponse.proto"
    codePipeline
    (Proxy :: Proxy GetThirdPartyJobDetails)

responsePutThirdPartyJobSuccessResult :: PutThirdPartyJobSuccessResultResponse -> TestTree
responsePutThirdPartyJobSuccessResult = res
    "PutThirdPartyJobSuccessResultResponse"
    "fixture/PutThirdPartyJobSuccessResultResponse.proto"
    codePipeline
    (Proxy :: Proxy PutThirdPartyJobSuccessResult)

responseCreateCustomActionType :: CreateCustomActionTypeResponse -> TestTree
responseCreateCustomActionType = res
    "CreateCustomActionTypeResponse"
    "fixture/CreateCustomActionTypeResponse.proto"
    codePipeline
    (Proxy :: Proxy CreateCustomActionType)

responseListPipelineExecutions :: ListPipelineExecutionsResponse -> TestTree
responseListPipelineExecutions = res
    "ListPipelineExecutionsResponse"
    "fixture/ListPipelineExecutionsResponse.proto"
    codePipeline
    (Proxy :: Proxy ListPipelineExecutions)
