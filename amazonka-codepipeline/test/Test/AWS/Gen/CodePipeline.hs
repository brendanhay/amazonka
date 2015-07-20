{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.CodePipeline
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
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
--         , testDeleteCustomActionType $
--             deleteCustomActionType
--
--         , testPutActionRevision $
--             putActionRevision
--
--         , testPutJobSuccessResult $
--             putJobSuccessResult
--
--         , testListActionTypes $
--             listActionTypes
--
--         , testDisableStageTransition $
--             disableStageTransition
--
--         , testAcknowledgeJob $
--             acknowledgeJob
--
--         , testEnableStageTransition $
--             enableStageTransition
--
--         , testGetThirdPartyJobDetails $
--             getThirdPartyJobDetails
--
--         , testCreatePipeline $
--             createPipeline
--
--         , testCreateCustomActionType $
--             createCustomActionType
--
--         , testPutThirdPartyJobSuccessResult $
--             putThirdPartyJobSuccessResult
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
--         , testDeleteCustomActionTypeResponse $
--             deleteCustomActionTypeResponse
--
--         , testPutActionRevisionResponse $
--             putActionRevisionResponse
--
--         , testPutJobSuccessResultResponse $
--             putJobSuccessResultResponse
--
--         , testListActionTypesResponse $
--             listActionTypesResponse
--
--         , testDisableStageTransitionResponse $
--             disableStageTransitionResponse
--
--         , testAcknowledgeJobResponse $
--             acknowledgeJobResponse
--
--         , testEnableStageTransitionResponse $
--             enableStageTransitionResponse
--
--         , testGetThirdPartyJobDetailsResponse $
--             getThirdPartyJobDetailsResponse
--
--         , testCreatePipelineResponse $
--             createPipelineResponse
--
--         , testCreateCustomActionTypeResponse $
--             createCustomActionTypeResponse
--
--         , testPutThirdPartyJobSuccessResultResponse $
--             putThirdPartyJobSuccessResultResponse
--
--           ]
--     ]

-- Requests

testGetPipeline :: GetPipeline -> TestTree
testGetPipeline = req
    "GetPipeline"
    "fixture/GetPipeline"

testPutJobFailureResult :: PutJobFailureResult -> TestTree
testPutJobFailureResult = req
    "PutJobFailureResult"
    "fixture/PutJobFailureResult"

testAcknowledgeThirdPartyJob :: AcknowledgeThirdPartyJob -> TestTree
testAcknowledgeThirdPartyJob = req
    "AcknowledgeThirdPartyJob"
    "fixture/AcknowledgeThirdPartyJob"

testPutThirdPartyJobFailureResult :: PutThirdPartyJobFailureResult -> TestTree
testPutThirdPartyJobFailureResult = req
    "PutThirdPartyJobFailureResult"
    "fixture/PutThirdPartyJobFailureResult"

testPollForThirdPartyJobs :: PollForThirdPartyJobs -> TestTree
testPollForThirdPartyJobs = req
    "PollForThirdPartyJobs"
    "fixture/PollForThirdPartyJobs"

testPollForJobs :: PollForJobs -> TestTree
testPollForJobs = req
    "PollForJobs"
    "fixture/PollForJobs"

testStartPipelineExecution :: StartPipelineExecution -> TestTree
testStartPipelineExecution = req
    "StartPipelineExecution"
    "fixture/StartPipelineExecution"

testUpdatePipeline :: UpdatePipeline -> TestTree
testUpdatePipeline = req
    "UpdatePipeline"
    "fixture/UpdatePipeline"

testDeletePipeline :: DeletePipeline -> TestTree
testDeletePipeline = req
    "DeletePipeline"
    "fixture/DeletePipeline"

testGetPipelineState :: GetPipelineState -> TestTree
testGetPipelineState = req
    "GetPipelineState"
    "fixture/GetPipelineState"

testGetJobDetails :: GetJobDetails -> TestTree
testGetJobDetails = req
    "GetJobDetails"
    "fixture/GetJobDetails"

testListPipelines :: ListPipelines -> TestTree
testListPipelines = req
    "ListPipelines"
    "fixture/ListPipelines"

testDeleteCustomActionType :: DeleteCustomActionType -> TestTree
testDeleteCustomActionType = req
    "DeleteCustomActionType"
    "fixture/DeleteCustomActionType"

testPutActionRevision :: PutActionRevision -> TestTree
testPutActionRevision = req
    "PutActionRevision"
    "fixture/PutActionRevision"

testPutJobSuccessResult :: PutJobSuccessResult -> TestTree
testPutJobSuccessResult = req
    "PutJobSuccessResult"
    "fixture/PutJobSuccessResult"

testListActionTypes :: ListActionTypes -> TestTree
testListActionTypes = req
    "ListActionTypes"
    "fixture/ListActionTypes"

testDisableStageTransition :: DisableStageTransition -> TestTree
testDisableStageTransition = req
    "DisableStageTransition"
    "fixture/DisableStageTransition"

testAcknowledgeJob :: AcknowledgeJob -> TestTree
testAcknowledgeJob = req
    "AcknowledgeJob"
    "fixture/AcknowledgeJob"

testEnableStageTransition :: EnableStageTransition -> TestTree
testEnableStageTransition = req
    "EnableStageTransition"
    "fixture/EnableStageTransition"

testGetThirdPartyJobDetails :: GetThirdPartyJobDetails -> TestTree
testGetThirdPartyJobDetails = req
    "GetThirdPartyJobDetails"
    "fixture/GetThirdPartyJobDetails"

testCreatePipeline :: CreatePipeline -> TestTree
testCreatePipeline = req
    "CreatePipeline"
    "fixture/CreatePipeline"

testCreateCustomActionType :: CreateCustomActionType -> TestTree
testCreateCustomActionType = req
    "CreateCustomActionType"
    "fixture/CreateCustomActionType"

testPutThirdPartyJobSuccessResult :: PutThirdPartyJobSuccessResult -> TestTree
testPutThirdPartyJobSuccessResult = req
    "PutThirdPartyJobSuccessResult"
    "fixture/PutThirdPartyJobSuccessResult"

-- Responses

testGetPipelineResponse :: GetPipelineResponse -> TestTree
testGetPipelineResponse = res
    "GetPipelineResponse"
    "fixture/GetPipelineResponse"
    (Proxy :: Proxy GetPipeline)

testPutJobFailureResultResponse :: PutJobFailureResultResponse -> TestTree
testPutJobFailureResultResponse = res
    "PutJobFailureResultResponse"
    "fixture/PutJobFailureResultResponse"
    (Proxy :: Proxy PutJobFailureResult)

testAcknowledgeThirdPartyJobResponse :: AcknowledgeThirdPartyJobResponse -> TestTree
testAcknowledgeThirdPartyJobResponse = res
    "AcknowledgeThirdPartyJobResponse"
    "fixture/AcknowledgeThirdPartyJobResponse"
    (Proxy :: Proxy AcknowledgeThirdPartyJob)

testPutThirdPartyJobFailureResultResponse :: PutThirdPartyJobFailureResultResponse -> TestTree
testPutThirdPartyJobFailureResultResponse = res
    "PutThirdPartyJobFailureResultResponse"
    "fixture/PutThirdPartyJobFailureResultResponse"
    (Proxy :: Proxy PutThirdPartyJobFailureResult)

testPollForThirdPartyJobsResponse :: PollForThirdPartyJobsResponse -> TestTree
testPollForThirdPartyJobsResponse = res
    "PollForThirdPartyJobsResponse"
    "fixture/PollForThirdPartyJobsResponse"
    (Proxy :: Proxy PollForThirdPartyJobs)

testPollForJobsResponse :: PollForJobsResponse -> TestTree
testPollForJobsResponse = res
    "PollForJobsResponse"
    "fixture/PollForJobsResponse"
    (Proxy :: Proxy PollForJobs)

testStartPipelineExecutionResponse :: StartPipelineExecutionResponse -> TestTree
testStartPipelineExecutionResponse = res
    "StartPipelineExecutionResponse"
    "fixture/StartPipelineExecutionResponse"
    (Proxy :: Proxy StartPipelineExecution)

testUpdatePipelineResponse :: UpdatePipelineResponse -> TestTree
testUpdatePipelineResponse = res
    "UpdatePipelineResponse"
    "fixture/UpdatePipelineResponse"
    (Proxy :: Proxy UpdatePipeline)

testDeletePipelineResponse :: DeletePipelineResponse -> TestTree
testDeletePipelineResponse = res
    "DeletePipelineResponse"
    "fixture/DeletePipelineResponse"
    (Proxy :: Proxy DeletePipeline)

testGetPipelineStateResponse :: GetPipelineStateResponse -> TestTree
testGetPipelineStateResponse = res
    "GetPipelineStateResponse"
    "fixture/GetPipelineStateResponse"
    (Proxy :: Proxy GetPipelineState)

testGetJobDetailsResponse :: GetJobDetailsResponse -> TestTree
testGetJobDetailsResponse = res
    "GetJobDetailsResponse"
    "fixture/GetJobDetailsResponse"
    (Proxy :: Proxy GetJobDetails)

testListPipelinesResponse :: ListPipelinesResponse -> TestTree
testListPipelinesResponse = res
    "ListPipelinesResponse"
    "fixture/ListPipelinesResponse"
    (Proxy :: Proxy ListPipelines)

testDeleteCustomActionTypeResponse :: DeleteCustomActionTypeResponse -> TestTree
testDeleteCustomActionTypeResponse = res
    "DeleteCustomActionTypeResponse"
    "fixture/DeleteCustomActionTypeResponse"
    (Proxy :: Proxy DeleteCustomActionType)

testPutActionRevisionResponse :: PutActionRevisionResponse -> TestTree
testPutActionRevisionResponse = res
    "PutActionRevisionResponse"
    "fixture/PutActionRevisionResponse"
    (Proxy :: Proxy PutActionRevision)

testPutJobSuccessResultResponse :: PutJobSuccessResultResponse -> TestTree
testPutJobSuccessResultResponse = res
    "PutJobSuccessResultResponse"
    "fixture/PutJobSuccessResultResponse"
    (Proxy :: Proxy PutJobSuccessResult)

testListActionTypesResponse :: ListActionTypesResponse -> TestTree
testListActionTypesResponse = res
    "ListActionTypesResponse"
    "fixture/ListActionTypesResponse"
    (Proxy :: Proxy ListActionTypes)

testDisableStageTransitionResponse :: DisableStageTransitionResponse -> TestTree
testDisableStageTransitionResponse = res
    "DisableStageTransitionResponse"
    "fixture/DisableStageTransitionResponse"
    (Proxy :: Proxy DisableStageTransition)

testAcknowledgeJobResponse :: AcknowledgeJobResponse -> TestTree
testAcknowledgeJobResponse = res
    "AcknowledgeJobResponse"
    "fixture/AcknowledgeJobResponse"
    (Proxy :: Proxy AcknowledgeJob)

testEnableStageTransitionResponse :: EnableStageTransitionResponse -> TestTree
testEnableStageTransitionResponse = res
    "EnableStageTransitionResponse"
    "fixture/EnableStageTransitionResponse"
    (Proxy :: Proxy EnableStageTransition)

testGetThirdPartyJobDetailsResponse :: GetThirdPartyJobDetailsResponse -> TestTree
testGetThirdPartyJobDetailsResponse = res
    "GetThirdPartyJobDetailsResponse"
    "fixture/GetThirdPartyJobDetailsResponse"
    (Proxy :: Proxy GetThirdPartyJobDetails)

testCreatePipelineResponse :: CreatePipelineResponse -> TestTree
testCreatePipelineResponse = res
    "CreatePipelineResponse"
    "fixture/CreatePipelineResponse"
    (Proxy :: Proxy CreatePipeline)

testCreateCustomActionTypeResponse :: CreateCustomActionTypeResponse -> TestTree
testCreateCustomActionTypeResponse = res
    "CreateCustomActionTypeResponse"
    "fixture/CreateCustomActionTypeResponse"
    (Proxy :: Proxy CreateCustomActionType)

testPutThirdPartyJobSuccessResultResponse :: PutThirdPartyJobSuccessResultResponse -> TestTree
testPutThirdPartyJobSuccessResultResponse = res
    "PutThirdPartyJobSuccessResultResponse"
    "fixture/PutThirdPartyJobSuccessResultResponse"
    (Proxy :: Proxy PutThirdPartyJobSuccessResult)
