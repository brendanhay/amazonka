{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.CloudFormation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Test.AWS.Gen.CloudFormation where

import Data.Proxy
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.Tasty
import Network.AWS.CloudFormation
import Test.AWS.CloudFormation.Internal

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestDescribeStackSetOperation $
--             mkDescribeStackSetOperation
--
--         , requestDeleteStack $
--             mkDeleteStack
--
--         , requestUpdateStack $
--             mkUpdateStack
--
--         , requestGetTemplateSummary $
--             mkGetTemplateSummary
--
--         , requestListChangeSets $
--             mkListChangeSets
--
--         , requestListStackResources $
--             mkListStackResources
--
--         , requestUpdateStackInstances $
--             mkUpdateStackInstances
--
--         , requestDeleteStackInstances $
--             mkDeleteStackInstances
--
--         , requestDescribeType $
--             mkDescribeType
--
--         , requestCreateStackInstances $
--             mkCreateStackInstances
--
--         , requestListTypeRegistrations $
--             mkListTypeRegistrations
--
--         , requestGetStackPolicy $
--             mkGetStackPolicy
--
--         , requestDescribeStacks $
--             mkDescribeStacks
--
--         , requestCreateChangeSet $
--             mkCreateChangeSet
--
--         , requestListStackSetOperations $
--             mkListStackSetOperations
--
--         , requestExecuteChangeSet $
--             mkExecuteChangeSet
--
--         , requestListStackInstances $
--             mkListStackInstances
--
--         , requestContinueUpdateRollback $
--             mkContinueUpdateRollback
--
--         , requestValidateTemplate $
--             mkValidateTemplate
--
--         , requestCancelUpdateStack $
--             mkCancelUpdateStack
--
--         , requestListTypes $
--             mkListTypes
--
--         , requestDescribeTypeRegistration $
--             mkDescribeTypeRegistration
--
--         , requestDetectStackDrift $
--             mkDetectStackDrift
--
--         , requestDescribeStackEvents $
--             mkDescribeStackEvents
--
--         , requestSignalResource $
--             mkSignalResource
--
--         , requestSetStackPolicy $
--             mkSetStackPolicy
--
--         , requestListImports $
--             mkListImports
--
--         , requestDescribeStackResourceDrifts $
--             mkDescribeStackResourceDrifts
--
--         , requestListStacks $
--             mkListStacks
--
--         , requestDescribeAccountLimits $
--             mkDescribeAccountLimits
--
--         , requestDescribeStackResources $
--             mkDescribeStackResources
--
--         , requestDescribeStackInstance $
--             mkDescribeStackInstance
--
--         , requestCreateStack $
--             mkCreateStack
--
--         , requestUpdateStackSet $
--             mkUpdateStackSet
--
--         , requestDeleteStackSet $
--             mkDeleteStackSet
--
--         , requestEstimateTemplateCost $
--             mkEstimateTemplateCost
--
--         , requestDeleteChangeSet $
--             mkDeleteChangeSet
--
--         , requestListStackSets $
--             mkListStackSets
--
--         , requestListExports $
--             mkListExports
--
--         , requestDescribeStackDriftDetectionStatus $
--             mkDescribeStackDriftDetectionStatus
--
--         , requestCreateStackSet $
--             mkCreateStackSet
--
--         , requestDeregisterType $
--             mkDeregisterType
--
--         , requestRecordHandlerProgress $
--             mkRecordHandlerProgress
--
--         , requestListTypeVersions $
--             mkListTypeVersions
--
--         , requestSetTypeDefaultVersion $
--             mkSetTypeDefaultVersion
--
--         , requestUpdateTerminationProtection $
--             mkUpdateTerminationProtection
--
--         , requestGetTemplate $
--             mkGetTemplate
--
--         , requestDetectStackSetDrift $
--             mkDetectStackSetDrift
--
--         , requestDetectStackResourceDrift $
--             mkDetectStackResourceDrift
--
--         , requestDescribeChangeSet $
--             mkDescribeChangeSet
--
--         , requestDescribeStackSet $
--             mkDescribeStackSet
--
--         , requestListStackSetOperationResults $
--             mkListStackSetOperationResults
--
--         , requestRegisterType $
--             mkRegisterType
--
--         , requestStopStackSetOperation $
--             mkStopStackSetOperation
--
--         , requestDescribeStackResource $
--             mkDescribeStackResource
--
--           ]

--     , testGroup "response"
--         [ responseDescribeStackSetOperation $
--             mkDescribeStackSetOperationResponse
--
--         , responseDeleteStack $
--             mkDeleteStackResponse
--
--         , responseUpdateStack $
--             mkUpdateStackResponse
--
--         , responseGetTemplateSummary $
--             mkGetTemplateSummaryResponse
--
--         , responseListChangeSets $
--             mkListChangeSetsResponse
--
--         , responseListStackResources $
--             mkListStackResourcesResponse
--
--         , responseUpdateStackInstances $
--             mkUpdateStackInstancesResponse
--
--         , responseDeleteStackInstances $
--             mkDeleteStackInstancesResponse
--
--         , responseDescribeType $
--             mkDescribeTypeResponse
--
--         , responseCreateStackInstances $
--             mkCreateStackInstancesResponse
--
--         , responseListTypeRegistrations $
--             mkListTypeRegistrationsResponse
--
--         , responseGetStackPolicy $
--             mkGetStackPolicyResponse
--
--         , responseDescribeStacks $
--             mkDescribeStacksResponse
--
--         , responseCreateChangeSet $
--             mkCreateChangeSetResponse
--
--         , responseListStackSetOperations $
--             mkListStackSetOperationsResponse
--
--         , responseExecuteChangeSet $
--             mkExecuteChangeSetResponse
--
--         , responseListStackInstances $
--             mkListStackInstancesResponse
--
--         , responseContinueUpdateRollback $
--             mkContinueUpdateRollbackResponse
--
--         , responseValidateTemplate $
--             mkValidateTemplateResponse
--
--         , responseCancelUpdateStack $
--             mkCancelUpdateStackResponse
--
--         , responseListTypes $
--             mkListTypesResponse
--
--         , responseDescribeTypeRegistration $
--             mkDescribeTypeRegistrationResponse
--
--         , responseDetectStackDrift $
--             mkDetectStackDriftResponse
--
--         , responseDescribeStackEvents $
--             mkDescribeStackEventsResponse
--
--         , responseSignalResource $
--             mkSignalResourceResponse
--
--         , responseSetStackPolicy $
--             mkSetStackPolicyResponse
--
--         , responseListImports $
--             mkListImportsResponse
--
--         , responseDescribeStackResourceDrifts $
--             mkDescribeStackResourceDriftsResponse
--
--         , responseListStacks $
--             mkListStacksResponse
--
--         , responseDescribeAccountLimits $
--             mkDescribeAccountLimitsResponse
--
--         , responseDescribeStackResources $
--             mkDescribeStackResourcesResponse
--
--         , responseDescribeStackInstance $
--             mkDescribeStackInstanceResponse
--
--         , responseCreateStack $
--             mkCreateStackResponse
--
--         , responseUpdateStackSet $
--             mkUpdateStackSetResponse
--
--         , responseDeleteStackSet $
--             mkDeleteStackSetResponse
--
--         , responseEstimateTemplateCost $
--             mkEstimateTemplateCostResponse
--
--         , responseDeleteChangeSet $
--             mkDeleteChangeSetResponse
--
--         , responseListStackSets $
--             mkListStackSetsResponse
--
--         , responseListExports $
--             mkListExportsResponse
--
--         , responseDescribeStackDriftDetectionStatus $
--             mkDescribeStackDriftDetectionStatusResponse
--
--         , responseCreateStackSet $
--             mkCreateStackSetResponse
--
--         , responseDeregisterType $
--             mkDeregisterTypeResponse
--
--         , responseRecordHandlerProgress $
--             mkRecordHandlerProgressResponse
--
--         , responseListTypeVersions $
--             mkListTypeVersionsResponse
--
--         , responseSetTypeDefaultVersion $
--             mkSetTypeDefaultVersionResponse
--
--         , responseUpdateTerminationProtection $
--             mkUpdateTerminationProtectionResponse
--
--         , responseGetTemplate $
--             mkGetTemplateResponse
--
--         , responseDetectStackSetDrift $
--             mkDetectStackSetDriftResponse
--
--         , responseDetectStackResourceDrift $
--             mkDetectStackResourceDriftResponse
--
--         , responseDescribeChangeSet $
--             mkDescribeChangeSetResponse
--
--         , responseDescribeStackSet $
--             mkDescribeStackSetResponse
--
--         , responseListStackSetOperationResults $
--             mkListStackSetOperationResultsResponse
--
--         , responseRegisterType $
--             mkRegisterTypeResponse
--
--         , responseStopStackSetOperation $
--             mkStopStackSetOperationResponse
--
--         , responseDescribeStackResource $
--             mkDescribeStackResourceResponse
--
--           ]
--     ]

-- Requests

requestDescribeStackSetOperation :: DescribeStackSetOperation -> TestTree
requestDescribeStackSetOperation = req
    "DescribeStackSetOperation"
    "fixture/DescribeStackSetOperation.yaml"

requestDeleteStack :: DeleteStack -> TestTree
requestDeleteStack = req
    "DeleteStack"
    "fixture/DeleteStack.yaml"

requestUpdateStack :: UpdateStack -> TestTree
requestUpdateStack = req
    "UpdateStack"
    "fixture/UpdateStack.yaml"

requestGetTemplateSummary :: GetTemplateSummary -> TestTree
requestGetTemplateSummary = req
    "GetTemplateSummary"
    "fixture/GetTemplateSummary.yaml"

requestListChangeSets :: ListChangeSets -> TestTree
requestListChangeSets = req
    "ListChangeSets"
    "fixture/ListChangeSets.yaml"

requestListStackResources :: ListStackResources -> TestTree
requestListStackResources = req
    "ListStackResources"
    "fixture/ListStackResources.yaml"

requestUpdateStackInstances :: UpdateStackInstances -> TestTree
requestUpdateStackInstances = req
    "UpdateStackInstances"
    "fixture/UpdateStackInstances.yaml"

requestDeleteStackInstances :: DeleteStackInstances -> TestTree
requestDeleteStackInstances = req
    "DeleteStackInstances"
    "fixture/DeleteStackInstances.yaml"

requestDescribeType :: DescribeType -> TestTree
requestDescribeType = req
    "DescribeType"
    "fixture/DescribeType.yaml"

requestCreateStackInstances :: CreateStackInstances -> TestTree
requestCreateStackInstances = req
    "CreateStackInstances"
    "fixture/CreateStackInstances.yaml"

requestListTypeRegistrations :: ListTypeRegistrations -> TestTree
requestListTypeRegistrations = req
    "ListTypeRegistrations"
    "fixture/ListTypeRegistrations.yaml"

requestGetStackPolicy :: GetStackPolicy -> TestTree
requestGetStackPolicy = req
    "GetStackPolicy"
    "fixture/GetStackPolicy.yaml"

requestDescribeStacks :: DescribeStacks -> TestTree
requestDescribeStacks = req
    "DescribeStacks"
    "fixture/DescribeStacks.yaml"

requestCreateChangeSet :: CreateChangeSet -> TestTree
requestCreateChangeSet = req
    "CreateChangeSet"
    "fixture/CreateChangeSet.yaml"

requestListStackSetOperations :: ListStackSetOperations -> TestTree
requestListStackSetOperations = req
    "ListStackSetOperations"
    "fixture/ListStackSetOperations.yaml"

requestExecuteChangeSet :: ExecuteChangeSet -> TestTree
requestExecuteChangeSet = req
    "ExecuteChangeSet"
    "fixture/ExecuteChangeSet.yaml"

requestListStackInstances :: ListStackInstances -> TestTree
requestListStackInstances = req
    "ListStackInstances"
    "fixture/ListStackInstances.yaml"

requestContinueUpdateRollback :: ContinueUpdateRollback -> TestTree
requestContinueUpdateRollback = req
    "ContinueUpdateRollback"
    "fixture/ContinueUpdateRollback.yaml"

requestValidateTemplate :: ValidateTemplate -> TestTree
requestValidateTemplate = req
    "ValidateTemplate"
    "fixture/ValidateTemplate.yaml"

requestCancelUpdateStack :: CancelUpdateStack -> TestTree
requestCancelUpdateStack = req
    "CancelUpdateStack"
    "fixture/CancelUpdateStack.yaml"

requestListTypes :: ListTypes -> TestTree
requestListTypes = req
    "ListTypes"
    "fixture/ListTypes.yaml"

requestDescribeTypeRegistration :: DescribeTypeRegistration -> TestTree
requestDescribeTypeRegistration = req
    "DescribeTypeRegistration"
    "fixture/DescribeTypeRegistration.yaml"

requestDetectStackDrift :: DetectStackDrift -> TestTree
requestDetectStackDrift = req
    "DetectStackDrift"
    "fixture/DetectStackDrift.yaml"

requestDescribeStackEvents :: DescribeStackEvents -> TestTree
requestDescribeStackEvents = req
    "DescribeStackEvents"
    "fixture/DescribeStackEvents.yaml"

requestSignalResource :: SignalResource -> TestTree
requestSignalResource = req
    "SignalResource"
    "fixture/SignalResource.yaml"

requestSetStackPolicy :: SetStackPolicy -> TestTree
requestSetStackPolicy = req
    "SetStackPolicy"
    "fixture/SetStackPolicy.yaml"

requestListImports :: ListImports -> TestTree
requestListImports = req
    "ListImports"
    "fixture/ListImports.yaml"

requestDescribeStackResourceDrifts :: DescribeStackResourceDrifts -> TestTree
requestDescribeStackResourceDrifts = req
    "DescribeStackResourceDrifts"
    "fixture/DescribeStackResourceDrifts.yaml"

requestListStacks :: ListStacks -> TestTree
requestListStacks = req
    "ListStacks"
    "fixture/ListStacks.yaml"

requestDescribeAccountLimits :: DescribeAccountLimits -> TestTree
requestDescribeAccountLimits = req
    "DescribeAccountLimits"
    "fixture/DescribeAccountLimits.yaml"

requestDescribeStackResources :: DescribeStackResources -> TestTree
requestDescribeStackResources = req
    "DescribeStackResources"
    "fixture/DescribeStackResources.yaml"

requestDescribeStackInstance :: DescribeStackInstance -> TestTree
requestDescribeStackInstance = req
    "DescribeStackInstance"
    "fixture/DescribeStackInstance.yaml"

requestCreateStack :: CreateStack -> TestTree
requestCreateStack = req
    "CreateStack"
    "fixture/CreateStack.yaml"

requestUpdateStackSet :: UpdateStackSet -> TestTree
requestUpdateStackSet = req
    "UpdateStackSet"
    "fixture/UpdateStackSet.yaml"

requestDeleteStackSet :: DeleteStackSet -> TestTree
requestDeleteStackSet = req
    "DeleteStackSet"
    "fixture/DeleteStackSet.yaml"

requestEstimateTemplateCost :: EstimateTemplateCost -> TestTree
requestEstimateTemplateCost = req
    "EstimateTemplateCost"
    "fixture/EstimateTemplateCost.yaml"

requestDeleteChangeSet :: DeleteChangeSet -> TestTree
requestDeleteChangeSet = req
    "DeleteChangeSet"
    "fixture/DeleteChangeSet.yaml"

requestListStackSets :: ListStackSets -> TestTree
requestListStackSets = req
    "ListStackSets"
    "fixture/ListStackSets.yaml"

requestListExports :: ListExports -> TestTree
requestListExports = req
    "ListExports"
    "fixture/ListExports.yaml"

requestDescribeStackDriftDetectionStatus :: DescribeStackDriftDetectionStatus -> TestTree
requestDescribeStackDriftDetectionStatus = req
    "DescribeStackDriftDetectionStatus"
    "fixture/DescribeStackDriftDetectionStatus.yaml"

requestCreateStackSet :: CreateStackSet -> TestTree
requestCreateStackSet = req
    "CreateStackSet"
    "fixture/CreateStackSet.yaml"

requestDeregisterType :: DeregisterType -> TestTree
requestDeregisterType = req
    "DeregisterType"
    "fixture/DeregisterType.yaml"

requestRecordHandlerProgress :: RecordHandlerProgress -> TestTree
requestRecordHandlerProgress = req
    "RecordHandlerProgress"
    "fixture/RecordHandlerProgress.yaml"

requestListTypeVersions :: ListTypeVersions -> TestTree
requestListTypeVersions = req
    "ListTypeVersions"
    "fixture/ListTypeVersions.yaml"

requestSetTypeDefaultVersion :: SetTypeDefaultVersion -> TestTree
requestSetTypeDefaultVersion = req
    "SetTypeDefaultVersion"
    "fixture/SetTypeDefaultVersion.yaml"

requestUpdateTerminationProtection :: UpdateTerminationProtection -> TestTree
requestUpdateTerminationProtection = req
    "UpdateTerminationProtection"
    "fixture/UpdateTerminationProtection.yaml"

requestGetTemplate :: GetTemplate -> TestTree
requestGetTemplate = req
    "GetTemplate"
    "fixture/GetTemplate.yaml"

requestDetectStackSetDrift :: DetectStackSetDrift -> TestTree
requestDetectStackSetDrift = req
    "DetectStackSetDrift"
    "fixture/DetectStackSetDrift.yaml"

requestDetectStackResourceDrift :: DetectStackResourceDrift -> TestTree
requestDetectStackResourceDrift = req
    "DetectStackResourceDrift"
    "fixture/DetectStackResourceDrift.yaml"

requestDescribeChangeSet :: DescribeChangeSet -> TestTree
requestDescribeChangeSet = req
    "DescribeChangeSet"
    "fixture/DescribeChangeSet.yaml"

requestDescribeStackSet :: DescribeStackSet -> TestTree
requestDescribeStackSet = req
    "DescribeStackSet"
    "fixture/DescribeStackSet.yaml"

requestListStackSetOperationResults :: ListStackSetOperationResults -> TestTree
requestListStackSetOperationResults = req
    "ListStackSetOperationResults"
    "fixture/ListStackSetOperationResults.yaml"

requestRegisterType :: RegisterType -> TestTree
requestRegisterType = req
    "RegisterType"
    "fixture/RegisterType.yaml"

requestStopStackSetOperation :: StopStackSetOperation -> TestTree
requestStopStackSetOperation = req
    "StopStackSetOperation"
    "fixture/StopStackSetOperation.yaml"

requestDescribeStackResource :: DescribeStackResource -> TestTree
requestDescribeStackResource = req
    "DescribeStackResource"
    "fixture/DescribeStackResource.yaml"

-- Responses

responseDescribeStackSetOperation :: DescribeStackSetOperationResponse -> TestTree
responseDescribeStackSetOperation = res
    "DescribeStackSetOperationResponse"
    "fixture/DescribeStackSetOperationResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeStackSetOperation)

responseDeleteStack :: DeleteStackResponse -> TestTree
responseDeleteStack = res
    "DeleteStackResponse"
    "fixture/DeleteStackResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteStack)

responseUpdateStack :: UpdateStackResponse -> TestTree
responseUpdateStack = res
    "UpdateStackResponse"
    "fixture/UpdateStackResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateStack)

responseGetTemplateSummary :: GetTemplateSummaryResponse -> TestTree
responseGetTemplateSummary = res
    "GetTemplateSummaryResponse"
    "fixture/GetTemplateSummaryResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetTemplateSummary)

responseListChangeSets :: ListChangeSetsResponse -> TestTree
responseListChangeSets = res
    "ListChangeSetsResponse"
    "fixture/ListChangeSetsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListChangeSets)

responseListStackResources :: ListStackResourcesResponse -> TestTree
responseListStackResources = res
    "ListStackResourcesResponse"
    "fixture/ListStackResourcesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListStackResources)

responseUpdateStackInstances :: UpdateStackInstancesResponse -> TestTree
responseUpdateStackInstances = res
    "UpdateStackInstancesResponse"
    "fixture/UpdateStackInstancesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateStackInstances)

responseDeleteStackInstances :: DeleteStackInstancesResponse -> TestTree
responseDeleteStackInstances = res
    "DeleteStackInstancesResponse"
    "fixture/DeleteStackInstancesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteStackInstances)

responseDescribeType :: DescribeTypeResponse -> TestTree
responseDescribeType = res
    "DescribeTypeResponse"
    "fixture/DescribeTypeResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeType)

responseCreateStackInstances :: CreateStackInstancesResponse -> TestTree
responseCreateStackInstances = res
    "CreateStackInstancesResponse"
    "fixture/CreateStackInstancesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateStackInstances)

responseListTypeRegistrations :: ListTypeRegistrationsResponse -> TestTree
responseListTypeRegistrations = res
    "ListTypeRegistrationsResponse"
    "fixture/ListTypeRegistrationsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListTypeRegistrations)

responseGetStackPolicy :: GetStackPolicyResponse -> TestTree
responseGetStackPolicy = res
    "GetStackPolicyResponse"
    "fixture/GetStackPolicyResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetStackPolicy)

responseDescribeStacks :: DescribeStacksResponse -> TestTree
responseDescribeStacks = res
    "DescribeStacksResponse"
    "fixture/DescribeStacksResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeStacks)

responseCreateChangeSet :: CreateChangeSetResponse -> TestTree
responseCreateChangeSet = res
    "CreateChangeSetResponse"
    "fixture/CreateChangeSetResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateChangeSet)

responseListStackSetOperations :: ListStackSetOperationsResponse -> TestTree
responseListStackSetOperations = res
    "ListStackSetOperationsResponse"
    "fixture/ListStackSetOperationsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListStackSetOperations)

responseExecuteChangeSet :: ExecuteChangeSetResponse -> TestTree
responseExecuteChangeSet = res
    "ExecuteChangeSetResponse"
    "fixture/ExecuteChangeSetResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ExecuteChangeSet)

responseListStackInstances :: ListStackInstancesResponse -> TestTree
responseListStackInstances = res
    "ListStackInstancesResponse"
    "fixture/ListStackInstancesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListStackInstances)

responseContinueUpdateRollback :: ContinueUpdateRollbackResponse -> TestTree
responseContinueUpdateRollback = res
    "ContinueUpdateRollbackResponse"
    "fixture/ContinueUpdateRollbackResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ContinueUpdateRollback)

responseValidateTemplate :: ValidateTemplateResponse -> TestTree
responseValidateTemplate = res
    "ValidateTemplateResponse"
    "fixture/ValidateTemplateResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ValidateTemplate)

responseCancelUpdateStack :: CancelUpdateStackResponse -> TestTree
responseCancelUpdateStack = res
    "CancelUpdateStackResponse"
    "fixture/CancelUpdateStackResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CancelUpdateStack)

responseListTypes :: ListTypesResponse -> TestTree
responseListTypes = res
    "ListTypesResponse"
    "fixture/ListTypesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListTypes)

responseDescribeTypeRegistration :: DescribeTypeRegistrationResponse -> TestTree
responseDescribeTypeRegistration = res
    "DescribeTypeRegistrationResponse"
    "fixture/DescribeTypeRegistrationResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeTypeRegistration)

responseDetectStackDrift :: DetectStackDriftResponse -> TestTree
responseDetectStackDrift = res
    "DetectStackDriftResponse"
    "fixture/DetectStackDriftResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DetectStackDrift)

responseDescribeStackEvents :: DescribeStackEventsResponse -> TestTree
responseDescribeStackEvents = res
    "DescribeStackEventsResponse"
    "fixture/DescribeStackEventsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeStackEvents)

responseSignalResource :: SignalResourceResponse -> TestTree
responseSignalResource = res
    "SignalResourceResponse"
    "fixture/SignalResourceResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy SignalResource)

responseSetStackPolicy :: SetStackPolicyResponse -> TestTree
responseSetStackPolicy = res
    "SetStackPolicyResponse"
    "fixture/SetStackPolicyResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy SetStackPolicy)

responseListImports :: ListImportsResponse -> TestTree
responseListImports = res
    "ListImportsResponse"
    "fixture/ListImportsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListImports)

responseDescribeStackResourceDrifts :: DescribeStackResourceDriftsResponse -> TestTree
responseDescribeStackResourceDrifts = res
    "DescribeStackResourceDriftsResponse"
    "fixture/DescribeStackResourceDriftsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeStackResourceDrifts)

responseListStacks :: ListStacksResponse -> TestTree
responseListStacks = res
    "ListStacksResponse"
    "fixture/ListStacksResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListStacks)

responseDescribeAccountLimits :: DescribeAccountLimitsResponse -> TestTree
responseDescribeAccountLimits = res
    "DescribeAccountLimitsResponse"
    "fixture/DescribeAccountLimitsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeAccountLimits)

responseDescribeStackResources :: DescribeStackResourcesResponse -> TestTree
responseDescribeStackResources = res
    "DescribeStackResourcesResponse"
    "fixture/DescribeStackResourcesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeStackResources)

responseDescribeStackInstance :: DescribeStackInstanceResponse -> TestTree
responseDescribeStackInstance = res
    "DescribeStackInstanceResponse"
    "fixture/DescribeStackInstanceResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeStackInstance)

responseCreateStack :: CreateStackResponse -> TestTree
responseCreateStack = res
    "CreateStackResponse"
    "fixture/CreateStackResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateStack)

responseUpdateStackSet :: UpdateStackSetResponse -> TestTree
responseUpdateStackSet = res
    "UpdateStackSetResponse"
    "fixture/UpdateStackSetResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateStackSet)

responseDeleteStackSet :: DeleteStackSetResponse -> TestTree
responseDeleteStackSet = res
    "DeleteStackSetResponse"
    "fixture/DeleteStackSetResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteStackSet)

responseEstimateTemplateCost :: EstimateTemplateCostResponse -> TestTree
responseEstimateTemplateCost = res
    "EstimateTemplateCostResponse"
    "fixture/EstimateTemplateCostResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy EstimateTemplateCost)

responseDeleteChangeSet :: DeleteChangeSetResponse -> TestTree
responseDeleteChangeSet = res
    "DeleteChangeSetResponse"
    "fixture/DeleteChangeSetResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteChangeSet)

responseListStackSets :: ListStackSetsResponse -> TestTree
responseListStackSets = res
    "ListStackSetsResponse"
    "fixture/ListStackSetsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListStackSets)

responseListExports :: ListExportsResponse -> TestTree
responseListExports = res
    "ListExportsResponse"
    "fixture/ListExportsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListExports)

responseDescribeStackDriftDetectionStatus :: DescribeStackDriftDetectionStatusResponse -> TestTree
responseDescribeStackDriftDetectionStatus = res
    "DescribeStackDriftDetectionStatusResponse"
    "fixture/DescribeStackDriftDetectionStatusResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeStackDriftDetectionStatus)

responseCreateStackSet :: CreateStackSetResponse -> TestTree
responseCreateStackSet = res
    "CreateStackSetResponse"
    "fixture/CreateStackSetResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateStackSet)

responseDeregisterType :: DeregisterTypeResponse -> TestTree
responseDeregisterType = res
    "DeregisterTypeResponse"
    "fixture/DeregisterTypeResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeregisterType)

responseRecordHandlerProgress :: RecordHandlerProgressResponse -> TestTree
responseRecordHandlerProgress = res
    "RecordHandlerProgressResponse"
    "fixture/RecordHandlerProgressResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy RecordHandlerProgress)

responseListTypeVersions :: ListTypeVersionsResponse -> TestTree
responseListTypeVersions = res
    "ListTypeVersionsResponse"
    "fixture/ListTypeVersionsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListTypeVersions)

responseSetTypeDefaultVersion :: SetTypeDefaultVersionResponse -> TestTree
responseSetTypeDefaultVersion = res
    "SetTypeDefaultVersionResponse"
    "fixture/SetTypeDefaultVersionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy SetTypeDefaultVersion)

responseUpdateTerminationProtection :: UpdateTerminationProtectionResponse -> TestTree
responseUpdateTerminationProtection = res
    "UpdateTerminationProtectionResponse"
    "fixture/UpdateTerminationProtectionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateTerminationProtection)

responseGetTemplate :: GetTemplateResponse -> TestTree
responseGetTemplate = res
    "GetTemplateResponse"
    "fixture/GetTemplateResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetTemplate)

responseDetectStackSetDrift :: DetectStackSetDriftResponse -> TestTree
responseDetectStackSetDrift = res
    "DetectStackSetDriftResponse"
    "fixture/DetectStackSetDriftResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DetectStackSetDrift)

responseDetectStackResourceDrift :: DetectStackResourceDriftResponse -> TestTree
responseDetectStackResourceDrift = res
    "DetectStackResourceDriftResponse"
    "fixture/DetectStackResourceDriftResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DetectStackResourceDrift)

responseDescribeChangeSet :: DescribeChangeSetResponse -> TestTree
responseDescribeChangeSet = res
    "DescribeChangeSetResponse"
    "fixture/DescribeChangeSetResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeChangeSet)

responseDescribeStackSet :: DescribeStackSetResponse -> TestTree
responseDescribeStackSet = res
    "DescribeStackSetResponse"
    "fixture/DescribeStackSetResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeStackSet)

responseListStackSetOperationResults :: ListStackSetOperationResultsResponse -> TestTree
responseListStackSetOperationResults = res
    "ListStackSetOperationResultsResponse"
    "fixture/ListStackSetOperationResultsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListStackSetOperationResults)

responseRegisterType :: RegisterTypeResponse -> TestTree
responseRegisterType = res
    "RegisterTypeResponse"
    "fixture/RegisterTypeResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy RegisterType)

responseStopStackSetOperation :: StopStackSetOperationResponse -> TestTree
responseStopStackSetOperation = res
    "StopStackSetOperationResponse"
    "fixture/StopStackSetOperationResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy StopStackSetOperation)

responseDescribeStackResource :: DescribeStackResourceResponse -> TestTree
responseDescribeStackResource = res
    "DescribeStackResourceResponse"
    "fixture/DescribeStackResourceResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeStackResource)
