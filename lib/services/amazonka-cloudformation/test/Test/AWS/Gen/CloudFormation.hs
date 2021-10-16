{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.CloudFormation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.AWS.Gen.CloudFormation where

import Data.Proxy
import Network.AWS.CloudFormation
import Test.AWS.CloudFormation.Internal
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
--         [ requestRegisterPublisher $
--             newRegisterPublisher
--
--         , requestImportStacksToStackSet $
--             newImportStacksToStackSet
--
--         , requestDescribeStackResourceDrifts $
--             newDescribeStackResourceDrifts
--
--         , requestDescribeStackEvents $
--             newDescribeStackEvents
--
--         , requestListImports $
--             newListImports
--
--         , requestDescribeChangeSet $
--             newDescribeChangeSet
--
--         , requestStopStackSetOperation $
--             newStopStackSetOperation
--
--         , requestDescribeStackResource $
--             newDescribeStackResource
--
--         , requestTestType $
--             newTestType
--
--         , requestDetectStackResourceDrift $
--             newDetectStackResourceDrift
--
--         , requestSetTypeDefaultVersion $
--             newSetTypeDefaultVersion
--
--         , requestExecuteChangeSet $
--             newExecuteChangeSet
--
--         , requestRollbackStack $
--             newRollbackStack
--
--         , requestGetStackPolicy $
--             newGetStackPolicy
--
--         , requestCreateStackInstances $
--             newCreateStackInstances
--
--         , requestDescribeStacks $
--             newDescribeStacks
--
--         , requestRecordHandlerProgress $
--             newRecordHandlerProgress
--
--         , requestListStackSetOperations $
--             newListStackSetOperations
--
--         , requestUpdateStackSet $
--             newUpdateStackSet
--
--         , requestEstimateTemplateCost $
--             newEstimateTemplateCost
--
--         , requestDeleteStackSet $
--             newDeleteStackSet
--
--         , requestCreateStack $
--             newCreateStack
--
--         , requestDescribeAccountLimits $
--             newDescribeAccountLimits
--
--         , requestGetTemplateSummary $
--             newGetTemplateSummary
--
--         , requestSetTypeConfiguration $
--             newSetTypeConfiguration
--
--         , requestDescribeStackInstance $
--             newDescribeStackInstance
--
--         , requestDeleteStack $
--             newDeleteStack
--
--         , requestUpdateStack $
--             newUpdateStack
--
--         , requestListStacks $
--             newListStacks
--
--         , requestSignalResource $
--             newSignalResource
--
--         , requestDetectStackDrift $
--             newDetectStackDrift
--
--         , requestSetStackPolicy $
--             newSetStackPolicy
--
--         , requestDescribeStackSetOperation $
--             newDescribeStackSetOperation
--
--         , requestDescribeTypeRegistration $
--             newDescribeTypeRegistration
--
--         , requestListTypes $
--             newListTypes
--
--         , requestDescribeStackSet $
--             newDescribeStackSet
--
--         , requestListStackSetOperationResults $
--             newListStackSetOperationResults
--
--         , requestRegisterType $
--             newRegisterType
--
--         , requestPublishType $
--             newPublishType
--
--         , requestCancelUpdateStack $
--             newCancelUpdateStack
--
--         , requestActivateType $
--             newActivateType
--
--         , requestValidateTemplate $
--             newValidateTemplate
--
--         , requestDetectStackSetDrift $
--             newDetectStackSetDrift
--
--         , requestGetTemplate $
--             newGetTemplate
--
--         , requestListStackInstances $
--             newListStackInstances
--
--         , requestContinueUpdateRollback $
--             newContinueUpdateRollback
--
--         , requestUpdateTerminationProtection $
--             newUpdateTerminationProtection
--
--         , requestListTypeVersions $
--             newListTypeVersions
--
--         , requestDescribePublisher $
--             newDescribePublisher
--
--         , requestDeactivateType $
--             newDeactivateType
--
--         , requestListTypeRegistrations $
--             newListTypeRegistrations
--
--         , requestCreateStackSet $
--             newCreateStackSet
--
--         , requestCreateChangeSet $
--             newCreateChangeSet
--
--         , requestDeregisterType $
--             newDeregisterType
--
--         , requestDescribeType $
--             newDescribeType
--
--         , requestListChangeSets $
--             newListChangeSets
--
--         , requestDeleteChangeSet $
--             newDeleteChangeSet
--
--         , requestDeleteStackInstances $
--             newDeleteStackInstances
--
--         , requestListStackResources $
--             newListStackResources
--
--         , requestUpdateStackInstances $
--             newUpdateStackInstances
--
--         , requestDescribeStackDriftDetectionStatus $
--             newDescribeStackDriftDetectionStatus
--
--         , requestListStackSets $
--             newListStackSets
--
--         , requestListExports $
--             newListExports
--
--         , requestDescribeStackResources $
--             newDescribeStackResources
--
--         , requestBatchDescribeTypeConfigurations $
--             newBatchDescribeTypeConfigurations
--
--           ]

--     , testGroup "response"
--         [ responseRegisterPublisher $
--             newRegisterPublisherResponse
--
--         , responseImportStacksToStackSet $
--             newImportStacksToStackSetResponse
--
--         , responseDescribeStackResourceDrifts $
--             newDescribeStackResourceDriftsResponse
--
--         , responseDescribeStackEvents $
--             newDescribeStackEventsResponse
--
--         , responseListImports $
--             newListImportsResponse
--
--         , responseDescribeChangeSet $
--             newDescribeChangeSetResponse
--
--         , responseStopStackSetOperation $
--             newStopStackSetOperationResponse
--
--         , responseDescribeStackResource $
--             newDescribeStackResourceResponse
--
--         , responseTestType $
--             newTestTypeResponse
--
--         , responseDetectStackResourceDrift $
--             newDetectStackResourceDriftResponse
--
--         , responseSetTypeDefaultVersion $
--             newSetTypeDefaultVersionResponse
--
--         , responseExecuteChangeSet $
--             newExecuteChangeSetResponse
--
--         , responseRollbackStack $
--             newRollbackStackResponse
--
--         , responseGetStackPolicy $
--             newGetStackPolicyResponse
--
--         , responseCreateStackInstances $
--             newCreateStackInstancesResponse
--
--         , responseDescribeStacks $
--             newDescribeStacksResponse
--
--         , responseRecordHandlerProgress $
--             newRecordHandlerProgressResponse
--
--         , responseListStackSetOperations $
--             newListStackSetOperationsResponse
--
--         , responseUpdateStackSet $
--             newUpdateStackSetResponse
--
--         , responseEstimateTemplateCost $
--             newEstimateTemplateCostResponse
--
--         , responseDeleteStackSet $
--             newDeleteStackSetResponse
--
--         , responseCreateStack $
--             newCreateStackResponse
--
--         , responseDescribeAccountLimits $
--             newDescribeAccountLimitsResponse
--
--         , responseGetTemplateSummary $
--             newGetTemplateSummaryResponse
--
--         , responseSetTypeConfiguration $
--             newSetTypeConfigurationResponse
--
--         , responseDescribeStackInstance $
--             newDescribeStackInstanceResponse
--
--         , responseDeleteStack $
--             newDeleteStackResponse
--
--         , responseUpdateStack $
--             newUpdateStackResponse
--
--         , responseListStacks $
--             newListStacksResponse
--
--         , responseSignalResource $
--             newSignalResourceResponse
--
--         , responseDetectStackDrift $
--             newDetectStackDriftResponse
--
--         , responseSetStackPolicy $
--             newSetStackPolicyResponse
--
--         , responseDescribeStackSetOperation $
--             newDescribeStackSetOperationResponse
--
--         , responseDescribeTypeRegistration $
--             newDescribeTypeRegistrationResponse
--
--         , responseListTypes $
--             newListTypesResponse
--
--         , responseDescribeStackSet $
--             newDescribeStackSetResponse
--
--         , responseListStackSetOperationResults $
--             newListStackSetOperationResultsResponse
--
--         , responseRegisterType $
--             newRegisterTypeResponse
--
--         , responsePublishType $
--             newPublishTypeResponse
--
--         , responseCancelUpdateStack $
--             newCancelUpdateStackResponse
--
--         , responseActivateType $
--             newActivateTypeResponse
--
--         , responseValidateTemplate $
--             newValidateTemplateResponse
--
--         , responseDetectStackSetDrift $
--             newDetectStackSetDriftResponse
--
--         , responseGetTemplate $
--             newGetTemplateResponse
--
--         , responseListStackInstances $
--             newListStackInstancesResponse
--
--         , responseContinueUpdateRollback $
--             newContinueUpdateRollbackResponse
--
--         , responseUpdateTerminationProtection $
--             newUpdateTerminationProtectionResponse
--
--         , responseListTypeVersions $
--             newListTypeVersionsResponse
--
--         , responseDescribePublisher $
--             newDescribePublisherResponse
--
--         , responseDeactivateType $
--             newDeactivateTypeResponse
--
--         , responseListTypeRegistrations $
--             newListTypeRegistrationsResponse
--
--         , responseCreateStackSet $
--             newCreateStackSetResponse
--
--         , responseCreateChangeSet $
--             newCreateChangeSetResponse
--
--         , responseDeregisterType $
--             newDeregisterTypeResponse
--
--         , responseDescribeType $
--             newDescribeTypeResponse
--
--         , responseListChangeSets $
--             newListChangeSetsResponse
--
--         , responseDeleteChangeSet $
--             newDeleteChangeSetResponse
--
--         , responseDeleteStackInstances $
--             newDeleteStackInstancesResponse
--
--         , responseListStackResources $
--             newListStackResourcesResponse
--
--         , responseUpdateStackInstances $
--             newUpdateStackInstancesResponse
--
--         , responseDescribeStackDriftDetectionStatus $
--             newDescribeStackDriftDetectionStatusResponse
--
--         , responseListStackSets $
--             newListStackSetsResponse
--
--         , responseListExports $
--             newListExportsResponse
--
--         , responseDescribeStackResources $
--             newDescribeStackResourcesResponse
--
--         , responseBatchDescribeTypeConfigurations $
--             newBatchDescribeTypeConfigurationsResponse
--
--           ]
--     ]

-- Requests

requestRegisterPublisher :: RegisterPublisher -> TestTree
requestRegisterPublisher =
  req
    "RegisterPublisher"
    "fixture/RegisterPublisher.yaml"

requestImportStacksToStackSet :: ImportStacksToStackSet -> TestTree
requestImportStacksToStackSet =
  req
    "ImportStacksToStackSet"
    "fixture/ImportStacksToStackSet.yaml"

requestDescribeStackResourceDrifts :: DescribeStackResourceDrifts -> TestTree
requestDescribeStackResourceDrifts =
  req
    "DescribeStackResourceDrifts"
    "fixture/DescribeStackResourceDrifts.yaml"

requestDescribeStackEvents :: DescribeStackEvents -> TestTree
requestDescribeStackEvents =
  req
    "DescribeStackEvents"
    "fixture/DescribeStackEvents.yaml"

requestListImports :: ListImports -> TestTree
requestListImports =
  req
    "ListImports"
    "fixture/ListImports.yaml"

requestDescribeChangeSet :: DescribeChangeSet -> TestTree
requestDescribeChangeSet =
  req
    "DescribeChangeSet"
    "fixture/DescribeChangeSet.yaml"

requestStopStackSetOperation :: StopStackSetOperation -> TestTree
requestStopStackSetOperation =
  req
    "StopStackSetOperation"
    "fixture/StopStackSetOperation.yaml"

requestDescribeStackResource :: DescribeStackResource -> TestTree
requestDescribeStackResource =
  req
    "DescribeStackResource"
    "fixture/DescribeStackResource.yaml"

requestTestType :: TestType -> TestTree
requestTestType =
  req
    "TestType"
    "fixture/TestType.yaml"

requestDetectStackResourceDrift :: DetectStackResourceDrift -> TestTree
requestDetectStackResourceDrift =
  req
    "DetectStackResourceDrift"
    "fixture/DetectStackResourceDrift.yaml"

requestSetTypeDefaultVersion :: SetTypeDefaultVersion -> TestTree
requestSetTypeDefaultVersion =
  req
    "SetTypeDefaultVersion"
    "fixture/SetTypeDefaultVersion.yaml"

requestExecuteChangeSet :: ExecuteChangeSet -> TestTree
requestExecuteChangeSet =
  req
    "ExecuteChangeSet"
    "fixture/ExecuteChangeSet.yaml"

requestRollbackStack :: RollbackStack -> TestTree
requestRollbackStack =
  req
    "RollbackStack"
    "fixture/RollbackStack.yaml"

requestGetStackPolicy :: GetStackPolicy -> TestTree
requestGetStackPolicy =
  req
    "GetStackPolicy"
    "fixture/GetStackPolicy.yaml"

requestCreateStackInstances :: CreateStackInstances -> TestTree
requestCreateStackInstances =
  req
    "CreateStackInstances"
    "fixture/CreateStackInstances.yaml"

requestDescribeStacks :: DescribeStacks -> TestTree
requestDescribeStacks =
  req
    "DescribeStacks"
    "fixture/DescribeStacks.yaml"

requestRecordHandlerProgress :: RecordHandlerProgress -> TestTree
requestRecordHandlerProgress =
  req
    "RecordHandlerProgress"
    "fixture/RecordHandlerProgress.yaml"

requestListStackSetOperations :: ListStackSetOperations -> TestTree
requestListStackSetOperations =
  req
    "ListStackSetOperations"
    "fixture/ListStackSetOperations.yaml"

requestUpdateStackSet :: UpdateStackSet -> TestTree
requestUpdateStackSet =
  req
    "UpdateStackSet"
    "fixture/UpdateStackSet.yaml"

requestEstimateTemplateCost :: EstimateTemplateCost -> TestTree
requestEstimateTemplateCost =
  req
    "EstimateTemplateCost"
    "fixture/EstimateTemplateCost.yaml"

requestDeleteStackSet :: DeleteStackSet -> TestTree
requestDeleteStackSet =
  req
    "DeleteStackSet"
    "fixture/DeleteStackSet.yaml"

requestCreateStack :: CreateStack -> TestTree
requestCreateStack =
  req
    "CreateStack"
    "fixture/CreateStack.yaml"

requestDescribeAccountLimits :: DescribeAccountLimits -> TestTree
requestDescribeAccountLimits =
  req
    "DescribeAccountLimits"
    "fixture/DescribeAccountLimits.yaml"

requestGetTemplateSummary :: GetTemplateSummary -> TestTree
requestGetTemplateSummary =
  req
    "GetTemplateSummary"
    "fixture/GetTemplateSummary.yaml"

requestSetTypeConfiguration :: SetTypeConfiguration -> TestTree
requestSetTypeConfiguration =
  req
    "SetTypeConfiguration"
    "fixture/SetTypeConfiguration.yaml"

requestDescribeStackInstance :: DescribeStackInstance -> TestTree
requestDescribeStackInstance =
  req
    "DescribeStackInstance"
    "fixture/DescribeStackInstance.yaml"

requestDeleteStack :: DeleteStack -> TestTree
requestDeleteStack =
  req
    "DeleteStack"
    "fixture/DeleteStack.yaml"

requestUpdateStack :: UpdateStack -> TestTree
requestUpdateStack =
  req
    "UpdateStack"
    "fixture/UpdateStack.yaml"

requestListStacks :: ListStacks -> TestTree
requestListStacks =
  req
    "ListStacks"
    "fixture/ListStacks.yaml"

requestSignalResource :: SignalResource -> TestTree
requestSignalResource =
  req
    "SignalResource"
    "fixture/SignalResource.yaml"

requestDetectStackDrift :: DetectStackDrift -> TestTree
requestDetectStackDrift =
  req
    "DetectStackDrift"
    "fixture/DetectStackDrift.yaml"

requestSetStackPolicy :: SetStackPolicy -> TestTree
requestSetStackPolicy =
  req
    "SetStackPolicy"
    "fixture/SetStackPolicy.yaml"

requestDescribeStackSetOperation :: DescribeStackSetOperation -> TestTree
requestDescribeStackSetOperation =
  req
    "DescribeStackSetOperation"
    "fixture/DescribeStackSetOperation.yaml"

requestDescribeTypeRegistration :: DescribeTypeRegistration -> TestTree
requestDescribeTypeRegistration =
  req
    "DescribeTypeRegistration"
    "fixture/DescribeTypeRegistration.yaml"

requestListTypes :: ListTypes -> TestTree
requestListTypes =
  req
    "ListTypes"
    "fixture/ListTypes.yaml"

requestDescribeStackSet :: DescribeStackSet -> TestTree
requestDescribeStackSet =
  req
    "DescribeStackSet"
    "fixture/DescribeStackSet.yaml"

requestListStackSetOperationResults :: ListStackSetOperationResults -> TestTree
requestListStackSetOperationResults =
  req
    "ListStackSetOperationResults"
    "fixture/ListStackSetOperationResults.yaml"

requestRegisterType :: RegisterType -> TestTree
requestRegisterType =
  req
    "RegisterType"
    "fixture/RegisterType.yaml"

requestPublishType :: PublishType -> TestTree
requestPublishType =
  req
    "PublishType"
    "fixture/PublishType.yaml"

requestCancelUpdateStack :: CancelUpdateStack -> TestTree
requestCancelUpdateStack =
  req
    "CancelUpdateStack"
    "fixture/CancelUpdateStack.yaml"

requestActivateType :: ActivateType -> TestTree
requestActivateType =
  req
    "ActivateType"
    "fixture/ActivateType.yaml"

requestValidateTemplate :: ValidateTemplate -> TestTree
requestValidateTemplate =
  req
    "ValidateTemplate"
    "fixture/ValidateTemplate.yaml"

requestDetectStackSetDrift :: DetectStackSetDrift -> TestTree
requestDetectStackSetDrift =
  req
    "DetectStackSetDrift"
    "fixture/DetectStackSetDrift.yaml"

requestGetTemplate :: GetTemplate -> TestTree
requestGetTemplate =
  req
    "GetTemplate"
    "fixture/GetTemplate.yaml"

requestListStackInstances :: ListStackInstances -> TestTree
requestListStackInstances =
  req
    "ListStackInstances"
    "fixture/ListStackInstances.yaml"

requestContinueUpdateRollback :: ContinueUpdateRollback -> TestTree
requestContinueUpdateRollback =
  req
    "ContinueUpdateRollback"
    "fixture/ContinueUpdateRollback.yaml"

requestUpdateTerminationProtection :: UpdateTerminationProtection -> TestTree
requestUpdateTerminationProtection =
  req
    "UpdateTerminationProtection"
    "fixture/UpdateTerminationProtection.yaml"

requestListTypeVersions :: ListTypeVersions -> TestTree
requestListTypeVersions =
  req
    "ListTypeVersions"
    "fixture/ListTypeVersions.yaml"

requestDescribePublisher :: DescribePublisher -> TestTree
requestDescribePublisher =
  req
    "DescribePublisher"
    "fixture/DescribePublisher.yaml"

requestDeactivateType :: DeactivateType -> TestTree
requestDeactivateType =
  req
    "DeactivateType"
    "fixture/DeactivateType.yaml"

requestListTypeRegistrations :: ListTypeRegistrations -> TestTree
requestListTypeRegistrations =
  req
    "ListTypeRegistrations"
    "fixture/ListTypeRegistrations.yaml"

requestCreateStackSet :: CreateStackSet -> TestTree
requestCreateStackSet =
  req
    "CreateStackSet"
    "fixture/CreateStackSet.yaml"

requestCreateChangeSet :: CreateChangeSet -> TestTree
requestCreateChangeSet =
  req
    "CreateChangeSet"
    "fixture/CreateChangeSet.yaml"

requestDeregisterType :: DeregisterType -> TestTree
requestDeregisterType =
  req
    "DeregisterType"
    "fixture/DeregisterType.yaml"

requestDescribeType :: DescribeType -> TestTree
requestDescribeType =
  req
    "DescribeType"
    "fixture/DescribeType.yaml"

requestListChangeSets :: ListChangeSets -> TestTree
requestListChangeSets =
  req
    "ListChangeSets"
    "fixture/ListChangeSets.yaml"

requestDeleteChangeSet :: DeleteChangeSet -> TestTree
requestDeleteChangeSet =
  req
    "DeleteChangeSet"
    "fixture/DeleteChangeSet.yaml"

requestDeleteStackInstances :: DeleteStackInstances -> TestTree
requestDeleteStackInstances =
  req
    "DeleteStackInstances"
    "fixture/DeleteStackInstances.yaml"

requestListStackResources :: ListStackResources -> TestTree
requestListStackResources =
  req
    "ListStackResources"
    "fixture/ListStackResources.yaml"

requestUpdateStackInstances :: UpdateStackInstances -> TestTree
requestUpdateStackInstances =
  req
    "UpdateStackInstances"
    "fixture/UpdateStackInstances.yaml"

requestDescribeStackDriftDetectionStatus :: DescribeStackDriftDetectionStatus -> TestTree
requestDescribeStackDriftDetectionStatus =
  req
    "DescribeStackDriftDetectionStatus"
    "fixture/DescribeStackDriftDetectionStatus.yaml"

requestListStackSets :: ListStackSets -> TestTree
requestListStackSets =
  req
    "ListStackSets"
    "fixture/ListStackSets.yaml"

requestListExports :: ListExports -> TestTree
requestListExports =
  req
    "ListExports"
    "fixture/ListExports.yaml"

requestDescribeStackResources :: DescribeStackResources -> TestTree
requestDescribeStackResources =
  req
    "DescribeStackResources"
    "fixture/DescribeStackResources.yaml"

requestBatchDescribeTypeConfigurations :: BatchDescribeTypeConfigurations -> TestTree
requestBatchDescribeTypeConfigurations =
  req
    "BatchDescribeTypeConfigurations"
    "fixture/BatchDescribeTypeConfigurations.yaml"

-- Responses

responseRegisterPublisher :: RegisterPublisherResponse -> TestTree
responseRegisterPublisher =
  res
    "RegisterPublisherResponse"
    "fixture/RegisterPublisherResponse.proto"
    defaultService
    (Proxy :: Proxy RegisterPublisher)

responseImportStacksToStackSet :: ImportStacksToStackSetResponse -> TestTree
responseImportStacksToStackSet =
  res
    "ImportStacksToStackSetResponse"
    "fixture/ImportStacksToStackSetResponse.proto"
    defaultService
    (Proxy :: Proxy ImportStacksToStackSet)

responseDescribeStackResourceDrifts :: DescribeStackResourceDriftsResponse -> TestTree
responseDescribeStackResourceDrifts =
  res
    "DescribeStackResourceDriftsResponse"
    "fixture/DescribeStackResourceDriftsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeStackResourceDrifts)

responseDescribeStackEvents :: DescribeStackEventsResponse -> TestTree
responseDescribeStackEvents =
  res
    "DescribeStackEventsResponse"
    "fixture/DescribeStackEventsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeStackEvents)

responseListImports :: ListImportsResponse -> TestTree
responseListImports =
  res
    "ListImportsResponse"
    "fixture/ListImportsResponse.proto"
    defaultService
    (Proxy :: Proxy ListImports)

responseDescribeChangeSet :: DescribeChangeSetResponse -> TestTree
responseDescribeChangeSet =
  res
    "DescribeChangeSetResponse"
    "fixture/DescribeChangeSetResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeChangeSet)

responseStopStackSetOperation :: StopStackSetOperationResponse -> TestTree
responseStopStackSetOperation =
  res
    "StopStackSetOperationResponse"
    "fixture/StopStackSetOperationResponse.proto"
    defaultService
    (Proxy :: Proxy StopStackSetOperation)

responseDescribeStackResource :: DescribeStackResourceResponse -> TestTree
responseDescribeStackResource =
  res
    "DescribeStackResourceResponse"
    "fixture/DescribeStackResourceResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeStackResource)

responseTestType :: TestTypeResponse -> TestTree
responseTestType =
  res
    "TestTypeResponse"
    "fixture/TestTypeResponse.proto"
    defaultService
    (Proxy :: Proxy TestType)

responseDetectStackResourceDrift :: DetectStackResourceDriftResponse -> TestTree
responseDetectStackResourceDrift =
  res
    "DetectStackResourceDriftResponse"
    "fixture/DetectStackResourceDriftResponse.proto"
    defaultService
    (Proxy :: Proxy DetectStackResourceDrift)

responseSetTypeDefaultVersion :: SetTypeDefaultVersionResponse -> TestTree
responseSetTypeDefaultVersion =
  res
    "SetTypeDefaultVersionResponse"
    "fixture/SetTypeDefaultVersionResponse.proto"
    defaultService
    (Proxy :: Proxy SetTypeDefaultVersion)

responseExecuteChangeSet :: ExecuteChangeSetResponse -> TestTree
responseExecuteChangeSet =
  res
    "ExecuteChangeSetResponse"
    "fixture/ExecuteChangeSetResponse.proto"
    defaultService
    (Proxy :: Proxy ExecuteChangeSet)

responseRollbackStack :: RollbackStackResponse -> TestTree
responseRollbackStack =
  res
    "RollbackStackResponse"
    "fixture/RollbackStackResponse.proto"
    defaultService
    (Proxy :: Proxy RollbackStack)

responseGetStackPolicy :: GetStackPolicyResponse -> TestTree
responseGetStackPolicy =
  res
    "GetStackPolicyResponse"
    "fixture/GetStackPolicyResponse.proto"
    defaultService
    (Proxy :: Proxy GetStackPolicy)

responseCreateStackInstances :: CreateStackInstancesResponse -> TestTree
responseCreateStackInstances =
  res
    "CreateStackInstancesResponse"
    "fixture/CreateStackInstancesResponse.proto"
    defaultService
    (Proxy :: Proxy CreateStackInstances)

responseDescribeStacks :: DescribeStacksResponse -> TestTree
responseDescribeStacks =
  res
    "DescribeStacksResponse"
    "fixture/DescribeStacksResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeStacks)

responseRecordHandlerProgress :: RecordHandlerProgressResponse -> TestTree
responseRecordHandlerProgress =
  res
    "RecordHandlerProgressResponse"
    "fixture/RecordHandlerProgressResponse.proto"
    defaultService
    (Proxy :: Proxy RecordHandlerProgress)

responseListStackSetOperations :: ListStackSetOperationsResponse -> TestTree
responseListStackSetOperations =
  res
    "ListStackSetOperationsResponse"
    "fixture/ListStackSetOperationsResponse.proto"
    defaultService
    (Proxy :: Proxy ListStackSetOperations)

responseUpdateStackSet :: UpdateStackSetResponse -> TestTree
responseUpdateStackSet =
  res
    "UpdateStackSetResponse"
    "fixture/UpdateStackSetResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateStackSet)

responseEstimateTemplateCost :: EstimateTemplateCostResponse -> TestTree
responseEstimateTemplateCost =
  res
    "EstimateTemplateCostResponse"
    "fixture/EstimateTemplateCostResponse.proto"
    defaultService
    (Proxy :: Proxy EstimateTemplateCost)

responseDeleteStackSet :: DeleteStackSetResponse -> TestTree
responseDeleteStackSet =
  res
    "DeleteStackSetResponse"
    "fixture/DeleteStackSetResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteStackSet)

responseCreateStack :: CreateStackResponse -> TestTree
responseCreateStack =
  res
    "CreateStackResponse"
    "fixture/CreateStackResponse.proto"
    defaultService
    (Proxy :: Proxy CreateStack)

responseDescribeAccountLimits :: DescribeAccountLimitsResponse -> TestTree
responseDescribeAccountLimits =
  res
    "DescribeAccountLimitsResponse"
    "fixture/DescribeAccountLimitsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeAccountLimits)

responseGetTemplateSummary :: GetTemplateSummaryResponse -> TestTree
responseGetTemplateSummary =
  res
    "GetTemplateSummaryResponse"
    "fixture/GetTemplateSummaryResponse.proto"
    defaultService
    (Proxy :: Proxy GetTemplateSummary)

responseSetTypeConfiguration :: SetTypeConfigurationResponse -> TestTree
responseSetTypeConfiguration =
  res
    "SetTypeConfigurationResponse"
    "fixture/SetTypeConfigurationResponse.proto"
    defaultService
    (Proxy :: Proxy SetTypeConfiguration)

responseDescribeStackInstance :: DescribeStackInstanceResponse -> TestTree
responseDescribeStackInstance =
  res
    "DescribeStackInstanceResponse"
    "fixture/DescribeStackInstanceResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeStackInstance)

responseDeleteStack :: DeleteStackResponse -> TestTree
responseDeleteStack =
  res
    "DeleteStackResponse"
    "fixture/DeleteStackResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteStack)

responseUpdateStack :: UpdateStackResponse -> TestTree
responseUpdateStack =
  res
    "UpdateStackResponse"
    "fixture/UpdateStackResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateStack)

responseListStacks :: ListStacksResponse -> TestTree
responseListStacks =
  res
    "ListStacksResponse"
    "fixture/ListStacksResponse.proto"
    defaultService
    (Proxy :: Proxy ListStacks)

responseSignalResource :: SignalResourceResponse -> TestTree
responseSignalResource =
  res
    "SignalResourceResponse"
    "fixture/SignalResourceResponse.proto"
    defaultService
    (Proxy :: Proxy SignalResource)

responseDetectStackDrift :: DetectStackDriftResponse -> TestTree
responseDetectStackDrift =
  res
    "DetectStackDriftResponse"
    "fixture/DetectStackDriftResponse.proto"
    defaultService
    (Proxy :: Proxy DetectStackDrift)

responseSetStackPolicy :: SetStackPolicyResponse -> TestTree
responseSetStackPolicy =
  res
    "SetStackPolicyResponse"
    "fixture/SetStackPolicyResponse.proto"
    defaultService
    (Proxy :: Proxy SetStackPolicy)

responseDescribeStackSetOperation :: DescribeStackSetOperationResponse -> TestTree
responseDescribeStackSetOperation =
  res
    "DescribeStackSetOperationResponse"
    "fixture/DescribeStackSetOperationResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeStackSetOperation)

responseDescribeTypeRegistration :: DescribeTypeRegistrationResponse -> TestTree
responseDescribeTypeRegistration =
  res
    "DescribeTypeRegistrationResponse"
    "fixture/DescribeTypeRegistrationResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeTypeRegistration)

responseListTypes :: ListTypesResponse -> TestTree
responseListTypes =
  res
    "ListTypesResponse"
    "fixture/ListTypesResponse.proto"
    defaultService
    (Proxy :: Proxy ListTypes)

responseDescribeStackSet :: DescribeStackSetResponse -> TestTree
responseDescribeStackSet =
  res
    "DescribeStackSetResponse"
    "fixture/DescribeStackSetResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeStackSet)

responseListStackSetOperationResults :: ListStackSetOperationResultsResponse -> TestTree
responseListStackSetOperationResults =
  res
    "ListStackSetOperationResultsResponse"
    "fixture/ListStackSetOperationResultsResponse.proto"
    defaultService
    (Proxy :: Proxy ListStackSetOperationResults)

responseRegisterType :: RegisterTypeResponse -> TestTree
responseRegisterType =
  res
    "RegisterTypeResponse"
    "fixture/RegisterTypeResponse.proto"
    defaultService
    (Proxy :: Proxy RegisterType)

responsePublishType :: PublishTypeResponse -> TestTree
responsePublishType =
  res
    "PublishTypeResponse"
    "fixture/PublishTypeResponse.proto"
    defaultService
    (Proxy :: Proxy PublishType)

responseCancelUpdateStack :: CancelUpdateStackResponse -> TestTree
responseCancelUpdateStack =
  res
    "CancelUpdateStackResponse"
    "fixture/CancelUpdateStackResponse.proto"
    defaultService
    (Proxy :: Proxy CancelUpdateStack)

responseActivateType :: ActivateTypeResponse -> TestTree
responseActivateType =
  res
    "ActivateTypeResponse"
    "fixture/ActivateTypeResponse.proto"
    defaultService
    (Proxy :: Proxy ActivateType)

responseValidateTemplate :: ValidateTemplateResponse -> TestTree
responseValidateTemplate =
  res
    "ValidateTemplateResponse"
    "fixture/ValidateTemplateResponse.proto"
    defaultService
    (Proxy :: Proxy ValidateTemplate)

responseDetectStackSetDrift :: DetectStackSetDriftResponse -> TestTree
responseDetectStackSetDrift =
  res
    "DetectStackSetDriftResponse"
    "fixture/DetectStackSetDriftResponse.proto"
    defaultService
    (Proxy :: Proxy DetectStackSetDrift)

responseGetTemplate :: GetTemplateResponse -> TestTree
responseGetTemplate =
  res
    "GetTemplateResponse"
    "fixture/GetTemplateResponse.proto"
    defaultService
    (Proxy :: Proxy GetTemplate)

responseListStackInstances :: ListStackInstancesResponse -> TestTree
responseListStackInstances =
  res
    "ListStackInstancesResponse"
    "fixture/ListStackInstancesResponse.proto"
    defaultService
    (Proxy :: Proxy ListStackInstances)

responseContinueUpdateRollback :: ContinueUpdateRollbackResponse -> TestTree
responseContinueUpdateRollback =
  res
    "ContinueUpdateRollbackResponse"
    "fixture/ContinueUpdateRollbackResponse.proto"
    defaultService
    (Proxy :: Proxy ContinueUpdateRollback)

responseUpdateTerminationProtection :: UpdateTerminationProtectionResponse -> TestTree
responseUpdateTerminationProtection =
  res
    "UpdateTerminationProtectionResponse"
    "fixture/UpdateTerminationProtectionResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateTerminationProtection)

responseListTypeVersions :: ListTypeVersionsResponse -> TestTree
responseListTypeVersions =
  res
    "ListTypeVersionsResponse"
    "fixture/ListTypeVersionsResponse.proto"
    defaultService
    (Proxy :: Proxy ListTypeVersions)

responseDescribePublisher :: DescribePublisherResponse -> TestTree
responseDescribePublisher =
  res
    "DescribePublisherResponse"
    "fixture/DescribePublisherResponse.proto"
    defaultService
    (Proxy :: Proxy DescribePublisher)

responseDeactivateType :: DeactivateTypeResponse -> TestTree
responseDeactivateType =
  res
    "DeactivateTypeResponse"
    "fixture/DeactivateTypeResponse.proto"
    defaultService
    (Proxy :: Proxy DeactivateType)

responseListTypeRegistrations :: ListTypeRegistrationsResponse -> TestTree
responseListTypeRegistrations =
  res
    "ListTypeRegistrationsResponse"
    "fixture/ListTypeRegistrationsResponse.proto"
    defaultService
    (Proxy :: Proxy ListTypeRegistrations)

responseCreateStackSet :: CreateStackSetResponse -> TestTree
responseCreateStackSet =
  res
    "CreateStackSetResponse"
    "fixture/CreateStackSetResponse.proto"
    defaultService
    (Proxy :: Proxy CreateStackSet)

responseCreateChangeSet :: CreateChangeSetResponse -> TestTree
responseCreateChangeSet =
  res
    "CreateChangeSetResponse"
    "fixture/CreateChangeSetResponse.proto"
    defaultService
    (Proxy :: Proxy CreateChangeSet)

responseDeregisterType :: DeregisterTypeResponse -> TestTree
responseDeregisterType =
  res
    "DeregisterTypeResponse"
    "fixture/DeregisterTypeResponse.proto"
    defaultService
    (Proxy :: Proxy DeregisterType)

responseDescribeType :: DescribeTypeResponse -> TestTree
responseDescribeType =
  res
    "DescribeTypeResponse"
    "fixture/DescribeTypeResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeType)

responseListChangeSets :: ListChangeSetsResponse -> TestTree
responseListChangeSets =
  res
    "ListChangeSetsResponse"
    "fixture/ListChangeSetsResponse.proto"
    defaultService
    (Proxy :: Proxy ListChangeSets)

responseDeleteChangeSet :: DeleteChangeSetResponse -> TestTree
responseDeleteChangeSet =
  res
    "DeleteChangeSetResponse"
    "fixture/DeleteChangeSetResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteChangeSet)

responseDeleteStackInstances :: DeleteStackInstancesResponse -> TestTree
responseDeleteStackInstances =
  res
    "DeleteStackInstancesResponse"
    "fixture/DeleteStackInstancesResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteStackInstances)

responseListStackResources :: ListStackResourcesResponse -> TestTree
responseListStackResources =
  res
    "ListStackResourcesResponse"
    "fixture/ListStackResourcesResponse.proto"
    defaultService
    (Proxy :: Proxy ListStackResources)

responseUpdateStackInstances :: UpdateStackInstancesResponse -> TestTree
responseUpdateStackInstances =
  res
    "UpdateStackInstancesResponse"
    "fixture/UpdateStackInstancesResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateStackInstances)

responseDescribeStackDriftDetectionStatus :: DescribeStackDriftDetectionStatusResponse -> TestTree
responseDescribeStackDriftDetectionStatus =
  res
    "DescribeStackDriftDetectionStatusResponse"
    "fixture/DescribeStackDriftDetectionStatusResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeStackDriftDetectionStatus)

responseListStackSets :: ListStackSetsResponse -> TestTree
responseListStackSets =
  res
    "ListStackSetsResponse"
    "fixture/ListStackSetsResponse.proto"
    defaultService
    (Proxy :: Proxy ListStackSets)

responseListExports :: ListExportsResponse -> TestTree
responseListExports =
  res
    "ListExportsResponse"
    "fixture/ListExportsResponse.proto"
    defaultService
    (Proxy :: Proxy ListExports)

responseDescribeStackResources :: DescribeStackResourcesResponse -> TestTree
responseDescribeStackResources =
  res
    "DescribeStackResourcesResponse"
    "fixture/DescribeStackResourcesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeStackResources)

responseBatchDescribeTypeConfigurations :: BatchDescribeTypeConfigurationsResponse -> TestTree
responseBatchDescribeTypeConfigurations =
  res
    "BatchDescribeTypeConfigurationsResponse"
    "fixture/BatchDescribeTypeConfigurationsResponse.proto"
    defaultService
    (Proxy :: Proxy BatchDescribeTypeConfigurations)
