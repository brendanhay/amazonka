{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.CodePipeline
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Test.AWS.Gen.CodePipeline where

import Data.Proxy
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.Tasty
import Network.AWS.CodePipeline
import Test.AWS.CodePipeline.Internal

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ testGetPipeline $
--             getPipeline
--
--         , testPutJobFailureResult $
--             putJobFailureResult
--
--         , testAcknowledgeThirdPartyJob $
--             acknowledgeThirdPartyJob
--
--         , testPutThirdPartyJobFailureResult $
--             putThirdPartyJobFailureResult
--
--         , testPollForThirdPartyJobs $
--             pollForThirdPartyJobs
--
--         , testPollForJobs $
--             pollForJobs
--
--         , testStartPipelineExecution $
--             startPipelineExecution
--
--         , testUpdatePipeline $
--             updatePipeline
--
--         , testDeletePipeline $
--             deletePipeline
--
--         , testGetPipelineState $
--             getPipelineState
--
--         , testGetJobDetails $
--             getJobDetails
--
--         , testListPipelines $
--             listPipelines
--
--         , testPutJobSuccessResult $
--             putJobSuccessResult
--
--         , testDeleteCustomActionType $
--             deleteCustomActionType
--
--         , testPutActionRevision $
--             putActionRevision
--
--         , testDisableStageTransition $
--             disableStageTransition
--
--         , testListActionTypes $
--             listActionTypes
--
--         , testAcknowledgeJob $
--             acknowledgeJob
--
--         , testEnableStageTransition $
--             enableStageTransition
--
--         , testCreatePipeline $
--             createPipeline
--
--         , testGetThirdPartyJobDetails $
--             getThirdPartyJobDetails
--
--         , testPutThirdPartyJobSuccessResult $
--             putThirdPartyJobSuccessResult
--
--         , testCreateCustomActionType $
--             createCustomActionType
--
--           ]

--     , testGroup "response"
--         [ testGetPipelineResponse $
--             getPipelineResponse
--
--         , testPutJobFailureResultResponse $
--             putJobFailureResultResponse
--
--         , testAcknowledgeThirdPartyJobResponse $
--             acknowledgeThirdPartyJobResponse
--
--         , testPutThirdPartyJobFailureResultResponse $
--             putThirdPartyJobFailureResultResponse
--
--         , testPollForThirdPartyJobsResponse $
--             pollForThirdPartyJobsResponse
--
--         , testPollForJobsResponse $
--             pollForJobsResponse
--
--         , testStartPipelineExecutionResponse $
--             startPipelineExecutionResponse
--
--         , testUpdatePipelineResponse $
--             updatePipelineResponse
--
--         , testDeletePipelineResponse $
--             deletePipelineResponse
--
--         , testGetPipelineStateResponse $
--             getPipelineStateResponse
--
--         , testGetJobDetailsResponse $
--             getJobDetailsResponse
--
--         , testListPipelinesResponse $
--             listPipelinesResponse
--
--         , testPutJobSuccessResultResponse $
--             putJobSuccessResultResponse
--
--         , testDeleteCustomActionTypeResponse $
--             deleteCustomActionTypeResponse
--
--         , testPutActionRevisionResponse $
--             putActionRevisionResponse
--
--         , testDisableStageTransitionResponse $
--             disableStageTransitionResponse
--
--         , testListActionTypesResponse $
--             listActionTypesResponse
--
--         , testAcknowledgeJobResponse $
--             acknowledgeJobResponse
--
--         , testEnableStageTransitionResponse $
--             enableStageTransitionResponse
--
--         , testCreatePipelineResponse $
--             createPipelineResponse
--
--         , testGetThirdPartyJobDetailsResponse $
--             getThirdPartyJobDetailsResponse
--
--         , testPutThirdPartyJobSuccessResultResponse $
--             putThirdPartyJobSuccessResultResponse
--
--         , testCreateCustomActionTypeResponse $
--             createCustomActionTypeResponse
--
--           ]
--     ]

-- Requests

testGetPipeline :: GetPipeline -> TestTree
testGetPipeline = req
    "GetPipeline"
    "fixture/GetPipeline.yaml"

testPutJobFailureResult :: PutJobFailureResult -> TestTree
testPutJobFailureResult = req
    "PutJobFailureResult"
    "fixture/PutJobFailureResult.yaml"

testAcknowledgeThirdPartyJob :: AcknowledgeThirdPartyJob -> TestTree
testAcknowledgeThirdPartyJob = req
    "AcknowledgeThirdPartyJob"
    "fixture/AcknowledgeThirdPartyJob.yaml"

testPutThirdPartyJobFailureResult :: PutThirdPartyJobFailureResult -> TestTree
testPutThirdPartyJobFailureResult = req
    "PutThirdPartyJobFailureResult"
    "fixture/PutThirdPartyJobFailureResult.yaml"

testPollForThirdPartyJobs :: PollForThirdPartyJobs -> TestTree
testPollForThirdPartyJobs = req
    "PollForThirdPartyJobs"
    "fixture/PollForThirdPartyJobs.yaml"

testPollForJobs :: PollForJobs -> TestTree
testPollForJobs = req
    "PollForJobs"
    "fixture/PollForJobs.yaml"

testStartPipelineExecution :: StartPipelineExecution -> TestTree
testStartPipelineExecution = req
    "StartPipelineExecution"
    "fixture/StartPipelineExecution.yaml"

testUpdatePipeline :: UpdatePipeline -> TestTree
testUpdatePipeline = req
    "UpdatePipeline"
    "fixture/UpdatePipeline.yaml"

testDeletePipeline :: DeletePipeline -> TestTree
testDeletePipeline = req
    "DeletePipeline"
    "fixture/DeletePipeline.yaml"

testGetPipelineState :: GetPipelineState -> TestTree
testGetPipelineState = req
    "GetPipelineState"
    "fixture/GetPipelineState.yaml"

testGetJobDetails :: GetJobDetails -> TestTree
testGetJobDetails = req
    "GetJobDetails"
    "fixture/GetJobDetails.yaml"

testListPipelines :: ListPipelines -> TestTree
testListPipelines = req
    "ListPipelines"
    "fixture/ListPipelines.yaml"

testPutJobSuccessResult :: PutJobSuccessResult -> TestTree
testPutJobSuccessResult = req
    "PutJobSuccessResult"
    "fixture/PutJobSuccessResult.yaml"

testDeleteCustomActionType :: DeleteCustomActionType -> TestTree
testDeleteCustomActionType = req
    "DeleteCustomActionType"
    "fixture/DeleteCustomActionType.yaml"

testPutActionRevision :: PutActionRevision -> TestTree
testPutActionRevision = req
    "PutActionRevision"
    "fixture/PutActionRevision.yaml"

testDisableStageTransition :: DisableStageTransition -> TestTree
testDisableStageTransition = req
    "DisableStageTransition"
    "fixture/DisableStageTransition.yaml"

testListActionTypes :: ListActionTypes -> TestTree
testListActionTypes = req
    "ListActionTypes"
    "fixture/ListActionTypes.yaml"

testAcknowledgeJob :: AcknowledgeJob -> TestTree
testAcknowledgeJob = req
    "AcknowledgeJob"
    "fixture/AcknowledgeJob.yaml"

testEnableStageTransition :: EnableStageTransition -> TestTree
testEnableStageTransition = req
    "EnableStageTransition"
    "fixture/EnableStageTransition.yaml"

testCreatePipeline :: CreatePipeline -> TestTree
testCreatePipeline = req
    "CreatePipeline"
    "fixture/CreatePipeline.yaml"

testGetThirdPartyJobDetails :: GetThirdPartyJobDetails -> TestTree
testGetThirdPartyJobDetails = req
    "GetThirdPartyJobDetails"
    "fixture/GetThirdPartyJobDetails.yaml"

testPutThirdPartyJobSuccessResult :: PutThirdPartyJobSuccessResult -> TestTree
testPutThirdPartyJobSuccessResult = req
    "PutThirdPartyJobSuccessResult"
    "fixture/PutThirdPartyJobSuccessResult.yaml"

testCreateCustomActionType :: CreateCustomActionType -> TestTree
testCreateCustomActionType = req
    "CreateCustomActionType"
    "fixture/CreateCustomActionType.yaml"

-- Responses

testGetPipelineResponse :: GetPipelineResponse -> TestTree
testGetPipelineResponse = res
    "GetPipelineResponse"
    "fixture/GetPipelineResponse.proto"
    codePipeline
    (Proxy :: Proxy GetPipeline)

testPutJobFailureResultResponse :: PutJobFailureResultResponse -> TestTree
testPutJobFailureResultResponse = res
    "PutJobFailureResultResponse"
    "fixture/PutJobFailureResultResponse.proto"
    codePipeline
    (Proxy :: Proxy PutJobFailureResult)

testAcknowledgeThirdPartyJobResponse :: AcknowledgeThirdPartyJobResponse -> TestTree
testAcknowledgeThirdPartyJobResponse = res
    "AcknowledgeThirdPartyJobResponse"
    "fixture/AcknowledgeThirdPartyJobResponse.proto"
    codePipeline
    (Proxy :: Proxy AcknowledgeThirdPartyJob)

testPutThirdPartyJobFailureResultResponse :: PutThirdPartyJobFailureResultResponse -> TestTree
testPutThirdPartyJobFailureResultResponse = res
    "PutThirdPartyJobFailureResultResponse"
    "fixture/PutThirdPartyJobFailureResultResponse.proto"
    codePipeline
    (Proxy :: Proxy PutThirdPartyJobFailureResult)

testPollForThirdPartyJobsResponse :: PollForThirdPartyJobsResponse -> TestTree
testPollForThirdPartyJobsResponse = res
    "PollForThirdPartyJobsResponse"
    "fixture/PollForThirdPartyJobsResponse.proto"
    codePipeline
    (Proxy :: Proxy PollForThirdPartyJobs)

testPollForJobsResponse :: PollForJobsResponse -> TestTree
testPollForJobsResponse = res
    "PollForJobsResponse"
    "fixture/PollForJobsResponse.proto"
    codePipeline
    (Proxy :: Proxy PollForJobs)

testStartPipelineExecutionResponse :: StartPipelineExecutionResponse -> TestTree
testStartPipelineExecutionResponse = res
    "StartPipelineExecutionResponse"
    "fixture/StartPipelineExecutionResponse.proto"
    codePipeline
    (Proxy :: Proxy StartPipelineExecution)

testUpdatePipelineResponse :: UpdatePipelineResponse -> TestTree
testUpdatePipelineResponse = res
    "UpdatePipelineResponse"
    "fixture/UpdatePipelineResponse.proto"
    codePipeline
    (Proxy :: Proxy UpdatePipeline)

testDeletePipelineResponse :: DeletePipelineResponse -> TestTree
testDeletePipelineResponse = res
    "DeletePipelineResponse"
    "fixture/DeletePipelineResponse.proto"
    codePipeline
    (Proxy :: Proxy DeletePipeline)

testGetPipelineStateResponse :: GetPipelineStateResponse -> TestTree
testGetPipelineStateResponse = res
    "GetPipelineStateResponse"
    "fixture/GetPipelineStateResponse.proto"
    codePipeline
    (Proxy :: Proxy GetPipelineState)

testGetJobDetailsResponse :: GetJobDetailsResponse -> TestTree
testGetJobDetailsResponse = res
    "GetJobDetailsResponse"
    "fixture/GetJobDetailsResponse.proto"
    codePipeline
    (Proxy :: Proxy GetJobDetails)

testListPipelinesResponse :: ListPipelinesResponse -> TestTree
testListPipelinesResponse = res
    "ListPipelinesResponse"
    "fixture/ListPipelinesResponse.proto"
    codePipeline
    (Proxy :: Proxy ListPipelines)

testPutJobSuccessResultResponse :: PutJobSuccessResultResponse -> TestTree
testPutJobSuccessResultResponse = res
    "PutJobSuccessResultResponse"
    "fixture/PutJobSuccessResultResponse.proto"
    codePipeline
    (Proxy :: Proxy PutJobSuccessResult)

testDeleteCustomActionTypeResponse :: DeleteCustomActionTypeResponse -> TestTree
testDeleteCustomActionTypeResponse = res
    "DeleteCustomActionTypeResponse"
    "fixture/DeleteCustomActionTypeResponse.proto"
    codePipeline
    (Proxy :: Proxy DeleteCustomActionType)

testPutActionRevisionResponse :: PutActionRevisionResponse -> TestTree
testPutActionRevisionResponse = res
    "PutActionRevisionResponse"
    "fixture/PutActionRevisionResponse.proto"
    codePipeline
    (Proxy :: Proxy PutActionRevision)

testDisableStageTransitionResponse :: DisableStageTransitionResponse -> TestTree
testDisableStageTransitionResponse = res
    "DisableStageTransitionResponse"
    "fixture/DisableStageTransitionResponse.proto"
    codePipeline
    (Proxy :: Proxy DisableStageTransition)

testListActionTypesResponse :: ListActionTypesResponse -> TestTree
testListActionTypesResponse = res
    "ListActionTypesResponse"
    "fixture/ListActionTypesResponse.proto"
    codePipeline
    (Proxy :: Proxy ListActionTypes)

testAcknowledgeJobResponse :: AcknowledgeJobResponse -> TestTree
testAcknowledgeJobResponse = res
    "AcknowledgeJobResponse"
    "fixture/AcknowledgeJobResponse.proto"
    codePipeline
    (Proxy :: Proxy AcknowledgeJob)

testEnableStageTransitionResponse :: EnableStageTransitionResponse -> TestTree
testEnableStageTransitionResponse = res
    "EnableStageTransitionResponse"
    "fixture/EnableStageTransitionResponse.proto"
    codePipeline
    (Proxy :: Proxy EnableStageTransition)

testCreatePipelineResponse :: CreatePipelineResponse -> TestTree
testCreatePipelineResponse = res
    "CreatePipelineResponse"
    "fixture/CreatePipelineResponse.proto"
    codePipeline
    (Proxy :: Proxy CreatePipeline)

testGetThirdPartyJobDetailsResponse :: GetThirdPartyJobDetailsResponse -> TestTree
testGetThirdPartyJobDetailsResponse = res
    "GetThirdPartyJobDetailsResponse"
    "fixture/GetThirdPartyJobDetailsResponse.proto"
    codePipeline
    (Proxy :: Proxy GetThirdPartyJobDetails)

testPutThirdPartyJobSuccessResultResponse :: PutThirdPartyJobSuccessResultResponse -> TestTree
testPutThirdPartyJobSuccessResultResponse = res
    "PutThirdPartyJobSuccessResultResponse"
    "fixture/PutThirdPartyJobSuccessResultResponse.proto"
    codePipeline
    (Proxy :: Proxy PutThirdPartyJobSuccessResult)

testCreateCustomActionTypeResponse :: CreateCustomActionTypeResponse -> TestTree
testCreateCustomActionTypeResponse = res
    "CreateCustomActionTypeResponse"
    "fixture/CreateCustomActionTypeResponse.proto"
    codePipeline
    (Proxy :: Proxy CreateCustomActionType)
