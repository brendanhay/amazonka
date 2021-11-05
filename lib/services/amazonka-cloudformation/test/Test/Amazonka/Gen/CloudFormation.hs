{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.CloudFormation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.CloudFormation where

import Amazonka.CloudFormation
import qualified Data.Proxy as Proxy
import Test.Amazonka.CloudFormation.Internal
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
--         [ requestDescribeStackSetOperation $
--             newDescribeStackSetOperation
--
--         , requestImportStacksToStackSet $
--             newImportStacksToStackSet
--
--         , requestDeleteStack $
--             newDeleteStack
--
--         , requestUpdateStack $
--             newUpdateStack
--
--         , requestBatchDescribeTypeConfigurations $
--             newBatchDescribeTypeConfigurations
--
--         , requestSetTypeConfiguration $
--             newSetTypeConfiguration
--
--         , requestGetTemplateSummary $
--             newGetTemplateSummary
--
--         , requestListChangeSets $
--             newListChangeSets
--
--         , requestListStackResources $
--             newListStackResources
--
--         , requestUpdateStackInstances $
--             newUpdateStackInstances
--
--         , requestDeleteStackInstances $
--             newDeleteStackInstances
--
--         , requestDescribeType $
--             newDescribeType
--
--         , requestCreateStackInstances $
--             newCreateStackInstances
--
--         , requestListTypeRegistrations $
--             newListTypeRegistrations
--
--         , requestGetStackPolicy $
--             newGetStackPolicy
--
--         , requestDescribeStacks $
--             newDescribeStacks
--
--         , requestCreateChangeSet $
--             newCreateChangeSet
--
--         , requestListStackSetOperations $
--             newListStackSetOperations
--
--         , requestExecuteChangeSet $
--             newExecuteChangeSet
--
--         , requestDescribePublisher $
--             newDescribePublisher
--
--         , requestListStackInstances $
--             newListStackInstances
--
--         , requestContinueUpdateRollback $
--             newContinueUpdateRollback
--
--         , requestValidateTemplate $
--             newValidateTemplate
--
--         , requestCancelUpdateStack $
--             newCancelUpdateStack
--
--         , requestPublishType $
--             newPublishType
--
--         , requestListTypes $
--             newListTypes
--
--         , requestDescribeTypeRegistration $
--             newDescribeTypeRegistration
--
--         , requestDetectStackDrift $
--             newDetectStackDrift
--
--         , requestDescribeStackEvents $
--             newDescribeStackEvents
--
--         , requestSignalResource $
--             newSignalResource
--
--         , requestSetStackPolicy $
--             newSetStackPolicy
--
--         , requestListImports $
--             newListImports
--
--         , requestDescribeStackResourceDrifts $
--             newDescribeStackResourceDrifts
--
--         , requestListStacks $
--             newListStacks
--
--         , requestRegisterPublisher $
--             newRegisterPublisher
--
--         , requestDescribeAccountLimits $
--             newDescribeAccountLimits
--
--         , requestDescribeStackResources $
--             newDescribeStackResources
--
--         , requestDescribeStackInstance $
--             newDescribeStackInstance
--
--         , requestCreateStack $
--             newCreateStack
--
--         , requestUpdateStackSet $
--             newUpdateStackSet
--
--         , requestDeleteStackSet $
--             newDeleteStackSet
--
--         , requestEstimateTemplateCost $
--             newEstimateTemplateCost
--
--         , requestDeleteChangeSet $
--             newDeleteChangeSet
--
--         , requestListStackSets $
--             newListStackSets
--
--         , requestListExports $
--             newListExports
--
--         , requestDescribeStackDriftDetectionStatus $
--             newDescribeStackDriftDetectionStatus
--
--         , requestRollbackStack $
--             newRollbackStack
--
--         , requestCreateStackSet $
--             newCreateStackSet
--
--         , requestDeregisterType $
--             newDeregisterType
--
--         , requestDeactivateType $
--             newDeactivateType
--
--         , requestRecordHandlerProgress $
--             newRecordHandlerProgress
--
--         , requestListTypeVersions $
--             newListTypeVersions
--
--         , requestSetTypeDefaultVersion $
--             newSetTypeDefaultVersion
--
--         , requestUpdateTerminationProtection $
--             newUpdateTerminationProtection
--
--         , requestTestType $
--             newTestType
--
--         , requestGetTemplate $
--             newGetTemplate
--
--         , requestDetectStackSetDrift $
--             newDetectStackSetDrift
--
--         , requestDetectStackResourceDrift $
--             newDetectStackResourceDrift
--
--         , requestDescribeChangeSet $
--             newDescribeChangeSet
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
--         , requestActivateType $
--             newActivateType
--
--         , requestStopStackSetOperation $
--             newStopStackSetOperation
--
--         , requestDescribeStackResource $
--             newDescribeStackResource
--
--           ]

--     , testGroup "response"
--         [ responseDescribeStackSetOperation $
--             newDescribeStackSetOperationResponse
--
--         , responseImportStacksToStackSet $
--             newImportStacksToStackSetResponse
--
--         , responseDeleteStack $
--             newDeleteStackResponse
--
--         , responseUpdateStack $
--             newUpdateStackResponse
--
--         , responseBatchDescribeTypeConfigurations $
--             newBatchDescribeTypeConfigurationsResponse
--
--         , responseSetTypeConfiguration $
--             newSetTypeConfigurationResponse
--
--         , responseGetTemplateSummary $
--             newGetTemplateSummaryResponse
--
--         , responseListChangeSets $
--             newListChangeSetsResponse
--
--         , responseListStackResources $
--             newListStackResourcesResponse
--
--         , responseUpdateStackInstances $
--             newUpdateStackInstancesResponse
--
--         , responseDeleteStackInstances $
--             newDeleteStackInstancesResponse
--
--         , responseDescribeType $
--             newDescribeTypeResponse
--
--         , responseCreateStackInstances $
--             newCreateStackInstancesResponse
--
--         , responseListTypeRegistrations $
--             newListTypeRegistrationsResponse
--
--         , responseGetStackPolicy $
--             newGetStackPolicyResponse
--
--         , responseDescribeStacks $
--             newDescribeStacksResponse
--
--         , responseCreateChangeSet $
--             newCreateChangeSetResponse
--
--         , responseListStackSetOperations $
--             newListStackSetOperationsResponse
--
--         , responseExecuteChangeSet $
--             newExecuteChangeSetResponse
--
--         , responseDescribePublisher $
--             newDescribePublisherResponse
--
--         , responseListStackInstances $
--             newListStackInstancesResponse
--
--         , responseContinueUpdateRollback $
--             newContinueUpdateRollbackResponse
--
--         , responseValidateTemplate $
--             newValidateTemplateResponse
--
--         , responseCancelUpdateStack $
--             newCancelUpdateStackResponse
--
--         , responsePublishType $
--             newPublishTypeResponse
--
--         , responseListTypes $
--             newListTypesResponse
--
--         , responseDescribeTypeRegistration $
--             newDescribeTypeRegistrationResponse
--
--         , responseDetectStackDrift $
--             newDetectStackDriftResponse
--
--         , responseDescribeStackEvents $
--             newDescribeStackEventsResponse
--
--         , responseSignalResource $
--             newSignalResourceResponse
--
--         , responseSetStackPolicy $
--             newSetStackPolicyResponse
--
--         , responseListImports $
--             newListImportsResponse
--
--         , responseDescribeStackResourceDrifts $
--             newDescribeStackResourceDriftsResponse
--
--         , responseListStacks $
--             newListStacksResponse
--
--         , responseRegisterPublisher $
--             newRegisterPublisherResponse
--
--         , responseDescribeAccountLimits $
--             newDescribeAccountLimitsResponse
--
--         , responseDescribeStackResources $
--             newDescribeStackResourcesResponse
--
--         , responseDescribeStackInstance $
--             newDescribeStackInstanceResponse
--
--         , responseCreateStack $
--             newCreateStackResponse
--
--         , responseUpdateStackSet $
--             newUpdateStackSetResponse
--
--         , responseDeleteStackSet $
--             newDeleteStackSetResponse
--
--         , responseEstimateTemplateCost $
--             newEstimateTemplateCostResponse
--
--         , responseDeleteChangeSet $
--             newDeleteChangeSetResponse
--
--         , responseListStackSets $
--             newListStackSetsResponse
--
--         , responseListExports $
--             newListExportsResponse
--
--         , responseDescribeStackDriftDetectionStatus $
--             newDescribeStackDriftDetectionStatusResponse
--
--         , responseRollbackStack $
--             newRollbackStackResponse
--
--         , responseCreateStackSet $
--             newCreateStackSetResponse
--
--         , responseDeregisterType $
--             newDeregisterTypeResponse
--
--         , responseDeactivateType $
--             newDeactivateTypeResponse
--
--         , responseRecordHandlerProgress $
--             newRecordHandlerProgressResponse
--
--         , responseListTypeVersions $
--             newListTypeVersionsResponse
--
--         , responseSetTypeDefaultVersion $
--             newSetTypeDefaultVersionResponse
--
--         , responseUpdateTerminationProtection $
--             newUpdateTerminationProtectionResponse
--
--         , responseTestType $
--             newTestTypeResponse
--
--         , responseGetTemplate $
--             newGetTemplateResponse
--
--         , responseDetectStackSetDrift $
--             newDetectStackSetDriftResponse
--
--         , responseDetectStackResourceDrift $
--             newDetectStackResourceDriftResponse
--
--         , responseDescribeChangeSet $
--             newDescribeChangeSetResponse
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
--         , responseActivateType $
--             newActivateTypeResponse
--
--         , responseStopStackSetOperation $
--             newStopStackSetOperationResponse
--
--         , responseDescribeStackResource $
--             newDescribeStackResourceResponse
--
--           ]
--     ]

-- Requests

requestDescribeStackSetOperation :: DescribeStackSetOperation -> TestTree
requestDescribeStackSetOperation =
  req
    "DescribeStackSetOperation"
    "fixture/DescribeStackSetOperation.yaml"

requestImportStacksToStackSet :: ImportStacksToStackSet -> TestTree
requestImportStacksToStackSet =
  req
    "ImportStacksToStackSet"
    "fixture/ImportStacksToStackSet.yaml"

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

requestBatchDescribeTypeConfigurations :: BatchDescribeTypeConfigurations -> TestTree
requestBatchDescribeTypeConfigurations =
  req
    "BatchDescribeTypeConfigurations"
    "fixture/BatchDescribeTypeConfigurations.yaml"

requestSetTypeConfiguration :: SetTypeConfiguration -> TestTree
requestSetTypeConfiguration =
  req
    "SetTypeConfiguration"
    "fixture/SetTypeConfiguration.yaml"

requestGetTemplateSummary :: GetTemplateSummary -> TestTree
requestGetTemplateSummary =
  req
    "GetTemplateSummary"
    "fixture/GetTemplateSummary.yaml"

requestListChangeSets :: ListChangeSets -> TestTree
requestListChangeSets =
  req
    "ListChangeSets"
    "fixture/ListChangeSets.yaml"

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

requestDeleteStackInstances :: DeleteStackInstances -> TestTree
requestDeleteStackInstances =
  req
    "DeleteStackInstances"
    "fixture/DeleteStackInstances.yaml"

requestDescribeType :: DescribeType -> TestTree
requestDescribeType =
  req
    "DescribeType"
    "fixture/DescribeType.yaml"

requestCreateStackInstances :: CreateStackInstances -> TestTree
requestCreateStackInstances =
  req
    "CreateStackInstances"
    "fixture/CreateStackInstances.yaml"

requestListTypeRegistrations :: ListTypeRegistrations -> TestTree
requestListTypeRegistrations =
  req
    "ListTypeRegistrations"
    "fixture/ListTypeRegistrations.yaml"

requestGetStackPolicy :: GetStackPolicy -> TestTree
requestGetStackPolicy =
  req
    "GetStackPolicy"
    "fixture/GetStackPolicy.yaml"

requestDescribeStacks :: DescribeStacks -> TestTree
requestDescribeStacks =
  req
    "DescribeStacks"
    "fixture/DescribeStacks.yaml"

requestCreateChangeSet :: CreateChangeSet -> TestTree
requestCreateChangeSet =
  req
    "CreateChangeSet"
    "fixture/CreateChangeSet.yaml"

requestListStackSetOperations :: ListStackSetOperations -> TestTree
requestListStackSetOperations =
  req
    "ListStackSetOperations"
    "fixture/ListStackSetOperations.yaml"

requestExecuteChangeSet :: ExecuteChangeSet -> TestTree
requestExecuteChangeSet =
  req
    "ExecuteChangeSet"
    "fixture/ExecuteChangeSet.yaml"

requestDescribePublisher :: DescribePublisher -> TestTree
requestDescribePublisher =
  req
    "DescribePublisher"
    "fixture/DescribePublisher.yaml"

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

requestValidateTemplate :: ValidateTemplate -> TestTree
requestValidateTemplate =
  req
    "ValidateTemplate"
    "fixture/ValidateTemplate.yaml"

requestCancelUpdateStack :: CancelUpdateStack -> TestTree
requestCancelUpdateStack =
  req
    "CancelUpdateStack"
    "fixture/CancelUpdateStack.yaml"

requestPublishType :: PublishType -> TestTree
requestPublishType =
  req
    "PublishType"
    "fixture/PublishType.yaml"

requestListTypes :: ListTypes -> TestTree
requestListTypes =
  req
    "ListTypes"
    "fixture/ListTypes.yaml"

requestDescribeTypeRegistration :: DescribeTypeRegistration -> TestTree
requestDescribeTypeRegistration =
  req
    "DescribeTypeRegistration"
    "fixture/DescribeTypeRegistration.yaml"

requestDetectStackDrift :: DetectStackDrift -> TestTree
requestDetectStackDrift =
  req
    "DetectStackDrift"
    "fixture/DetectStackDrift.yaml"

requestDescribeStackEvents :: DescribeStackEvents -> TestTree
requestDescribeStackEvents =
  req
    "DescribeStackEvents"
    "fixture/DescribeStackEvents.yaml"

requestSignalResource :: SignalResource -> TestTree
requestSignalResource =
  req
    "SignalResource"
    "fixture/SignalResource.yaml"

requestSetStackPolicy :: SetStackPolicy -> TestTree
requestSetStackPolicy =
  req
    "SetStackPolicy"
    "fixture/SetStackPolicy.yaml"

requestListImports :: ListImports -> TestTree
requestListImports =
  req
    "ListImports"
    "fixture/ListImports.yaml"

requestDescribeStackResourceDrifts :: DescribeStackResourceDrifts -> TestTree
requestDescribeStackResourceDrifts =
  req
    "DescribeStackResourceDrifts"
    "fixture/DescribeStackResourceDrifts.yaml"

requestListStacks :: ListStacks -> TestTree
requestListStacks =
  req
    "ListStacks"
    "fixture/ListStacks.yaml"

requestRegisterPublisher :: RegisterPublisher -> TestTree
requestRegisterPublisher =
  req
    "RegisterPublisher"
    "fixture/RegisterPublisher.yaml"

requestDescribeAccountLimits :: DescribeAccountLimits -> TestTree
requestDescribeAccountLimits =
  req
    "DescribeAccountLimits"
    "fixture/DescribeAccountLimits.yaml"

requestDescribeStackResources :: DescribeStackResources -> TestTree
requestDescribeStackResources =
  req
    "DescribeStackResources"
    "fixture/DescribeStackResources.yaml"

requestDescribeStackInstance :: DescribeStackInstance -> TestTree
requestDescribeStackInstance =
  req
    "DescribeStackInstance"
    "fixture/DescribeStackInstance.yaml"

requestCreateStack :: CreateStack -> TestTree
requestCreateStack =
  req
    "CreateStack"
    "fixture/CreateStack.yaml"

requestUpdateStackSet :: UpdateStackSet -> TestTree
requestUpdateStackSet =
  req
    "UpdateStackSet"
    "fixture/UpdateStackSet.yaml"

requestDeleteStackSet :: DeleteStackSet -> TestTree
requestDeleteStackSet =
  req
    "DeleteStackSet"
    "fixture/DeleteStackSet.yaml"

requestEstimateTemplateCost :: EstimateTemplateCost -> TestTree
requestEstimateTemplateCost =
  req
    "EstimateTemplateCost"
    "fixture/EstimateTemplateCost.yaml"

requestDeleteChangeSet :: DeleteChangeSet -> TestTree
requestDeleteChangeSet =
  req
    "DeleteChangeSet"
    "fixture/DeleteChangeSet.yaml"

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

requestDescribeStackDriftDetectionStatus :: DescribeStackDriftDetectionStatus -> TestTree
requestDescribeStackDriftDetectionStatus =
  req
    "DescribeStackDriftDetectionStatus"
    "fixture/DescribeStackDriftDetectionStatus.yaml"

requestRollbackStack :: RollbackStack -> TestTree
requestRollbackStack =
  req
    "RollbackStack"
    "fixture/RollbackStack.yaml"

requestCreateStackSet :: CreateStackSet -> TestTree
requestCreateStackSet =
  req
    "CreateStackSet"
    "fixture/CreateStackSet.yaml"

requestDeregisterType :: DeregisterType -> TestTree
requestDeregisterType =
  req
    "DeregisterType"
    "fixture/DeregisterType.yaml"

requestDeactivateType :: DeactivateType -> TestTree
requestDeactivateType =
  req
    "DeactivateType"
    "fixture/DeactivateType.yaml"

requestRecordHandlerProgress :: RecordHandlerProgress -> TestTree
requestRecordHandlerProgress =
  req
    "RecordHandlerProgress"
    "fixture/RecordHandlerProgress.yaml"

requestListTypeVersions :: ListTypeVersions -> TestTree
requestListTypeVersions =
  req
    "ListTypeVersions"
    "fixture/ListTypeVersions.yaml"

requestSetTypeDefaultVersion :: SetTypeDefaultVersion -> TestTree
requestSetTypeDefaultVersion =
  req
    "SetTypeDefaultVersion"
    "fixture/SetTypeDefaultVersion.yaml"

requestUpdateTerminationProtection :: UpdateTerminationProtection -> TestTree
requestUpdateTerminationProtection =
  req
    "UpdateTerminationProtection"
    "fixture/UpdateTerminationProtection.yaml"

requestTestType :: TestType -> TestTree
requestTestType =
  req
    "TestType"
    "fixture/TestType.yaml"

requestGetTemplate :: GetTemplate -> TestTree
requestGetTemplate =
  req
    "GetTemplate"
    "fixture/GetTemplate.yaml"

requestDetectStackSetDrift :: DetectStackSetDrift -> TestTree
requestDetectStackSetDrift =
  req
    "DetectStackSetDrift"
    "fixture/DetectStackSetDrift.yaml"

requestDetectStackResourceDrift :: DetectStackResourceDrift -> TestTree
requestDetectStackResourceDrift =
  req
    "DetectStackResourceDrift"
    "fixture/DetectStackResourceDrift.yaml"

requestDescribeChangeSet :: DescribeChangeSet -> TestTree
requestDescribeChangeSet =
  req
    "DescribeChangeSet"
    "fixture/DescribeChangeSet.yaml"

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

requestActivateType :: ActivateType -> TestTree
requestActivateType =
  req
    "ActivateType"
    "fixture/ActivateType.yaml"

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

-- Responses

responseDescribeStackSetOperation :: DescribeStackSetOperationResponse -> TestTree
responseDescribeStackSetOperation =
  res
    "DescribeStackSetOperationResponse"
    "fixture/DescribeStackSetOperationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeStackSetOperation)

responseImportStacksToStackSet :: ImportStacksToStackSetResponse -> TestTree
responseImportStacksToStackSet =
  res
    "ImportStacksToStackSetResponse"
    "fixture/ImportStacksToStackSetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ImportStacksToStackSet)

responseDeleteStack :: DeleteStackResponse -> TestTree
responseDeleteStack =
  res
    "DeleteStackResponse"
    "fixture/DeleteStackResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteStack)

responseUpdateStack :: UpdateStackResponse -> TestTree
responseUpdateStack =
  res
    "UpdateStackResponse"
    "fixture/UpdateStackResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateStack)

responseBatchDescribeTypeConfigurations :: BatchDescribeTypeConfigurationsResponse -> TestTree
responseBatchDescribeTypeConfigurations =
  res
    "BatchDescribeTypeConfigurationsResponse"
    "fixture/BatchDescribeTypeConfigurationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchDescribeTypeConfigurations)

responseSetTypeConfiguration :: SetTypeConfigurationResponse -> TestTree
responseSetTypeConfiguration =
  res
    "SetTypeConfigurationResponse"
    "fixture/SetTypeConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SetTypeConfiguration)

responseGetTemplateSummary :: GetTemplateSummaryResponse -> TestTree
responseGetTemplateSummary =
  res
    "GetTemplateSummaryResponse"
    "fixture/GetTemplateSummaryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetTemplateSummary)

responseListChangeSets :: ListChangeSetsResponse -> TestTree
responseListChangeSets =
  res
    "ListChangeSetsResponse"
    "fixture/ListChangeSetsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListChangeSets)

responseListStackResources :: ListStackResourcesResponse -> TestTree
responseListStackResources =
  res
    "ListStackResourcesResponse"
    "fixture/ListStackResourcesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListStackResources)

responseUpdateStackInstances :: UpdateStackInstancesResponse -> TestTree
responseUpdateStackInstances =
  res
    "UpdateStackInstancesResponse"
    "fixture/UpdateStackInstancesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateStackInstances)

responseDeleteStackInstances :: DeleteStackInstancesResponse -> TestTree
responseDeleteStackInstances =
  res
    "DeleteStackInstancesResponse"
    "fixture/DeleteStackInstancesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteStackInstances)

responseDescribeType :: DescribeTypeResponse -> TestTree
responseDescribeType =
  res
    "DescribeTypeResponse"
    "fixture/DescribeTypeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeType)

responseCreateStackInstances :: CreateStackInstancesResponse -> TestTree
responseCreateStackInstances =
  res
    "CreateStackInstancesResponse"
    "fixture/CreateStackInstancesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateStackInstances)

responseListTypeRegistrations :: ListTypeRegistrationsResponse -> TestTree
responseListTypeRegistrations =
  res
    "ListTypeRegistrationsResponse"
    "fixture/ListTypeRegistrationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTypeRegistrations)

responseGetStackPolicy :: GetStackPolicyResponse -> TestTree
responseGetStackPolicy =
  res
    "GetStackPolicyResponse"
    "fixture/GetStackPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetStackPolicy)

responseDescribeStacks :: DescribeStacksResponse -> TestTree
responseDescribeStacks =
  res
    "DescribeStacksResponse"
    "fixture/DescribeStacksResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeStacks)

responseCreateChangeSet :: CreateChangeSetResponse -> TestTree
responseCreateChangeSet =
  res
    "CreateChangeSetResponse"
    "fixture/CreateChangeSetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateChangeSet)

responseListStackSetOperations :: ListStackSetOperationsResponse -> TestTree
responseListStackSetOperations =
  res
    "ListStackSetOperationsResponse"
    "fixture/ListStackSetOperationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListStackSetOperations)

responseExecuteChangeSet :: ExecuteChangeSetResponse -> TestTree
responseExecuteChangeSet =
  res
    "ExecuteChangeSetResponse"
    "fixture/ExecuteChangeSetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ExecuteChangeSet)

responseDescribePublisher :: DescribePublisherResponse -> TestTree
responseDescribePublisher =
  res
    "DescribePublisherResponse"
    "fixture/DescribePublisherResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribePublisher)

responseListStackInstances :: ListStackInstancesResponse -> TestTree
responseListStackInstances =
  res
    "ListStackInstancesResponse"
    "fixture/ListStackInstancesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListStackInstances)

responseContinueUpdateRollback :: ContinueUpdateRollbackResponse -> TestTree
responseContinueUpdateRollback =
  res
    "ContinueUpdateRollbackResponse"
    "fixture/ContinueUpdateRollbackResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ContinueUpdateRollback)

responseValidateTemplate :: ValidateTemplateResponse -> TestTree
responseValidateTemplate =
  res
    "ValidateTemplateResponse"
    "fixture/ValidateTemplateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ValidateTemplate)

responseCancelUpdateStack :: CancelUpdateStackResponse -> TestTree
responseCancelUpdateStack =
  res
    "CancelUpdateStackResponse"
    "fixture/CancelUpdateStackResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CancelUpdateStack)

responsePublishType :: PublishTypeResponse -> TestTree
responsePublishType =
  res
    "PublishTypeResponse"
    "fixture/PublishTypeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PublishType)

responseListTypes :: ListTypesResponse -> TestTree
responseListTypes =
  res
    "ListTypesResponse"
    "fixture/ListTypesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTypes)

responseDescribeTypeRegistration :: DescribeTypeRegistrationResponse -> TestTree
responseDescribeTypeRegistration =
  res
    "DescribeTypeRegistrationResponse"
    "fixture/DescribeTypeRegistrationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeTypeRegistration)

responseDetectStackDrift :: DetectStackDriftResponse -> TestTree
responseDetectStackDrift =
  res
    "DetectStackDriftResponse"
    "fixture/DetectStackDriftResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DetectStackDrift)

responseDescribeStackEvents :: DescribeStackEventsResponse -> TestTree
responseDescribeStackEvents =
  res
    "DescribeStackEventsResponse"
    "fixture/DescribeStackEventsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeStackEvents)

responseSignalResource :: SignalResourceResponse -> TestTree
responseSignalResource =
  res
    "SignalResourceResponse"
    "fixture/SignalResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SignalResource)

responseSetStackPolicy :: SetStackPolicyResponse -> TestTree
responseSetStackPolicy =
  res
    "SetStackPolicyResponse"
    "fixture/SetStackPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SetStackPolicy)

responseListImports :: ListImportsResponse -> TestTree
responseListImports =
  res
    "ListImportsResponse"
    "fixture/ListImportsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListImports)

responseDescribeStackResourceDrifts :: DescribeStackResourceDriftsResponse -> TestTree
responseDescribeStackResourceDrifts =
  res
    "DescribeStackResourceDriftsResponse"
    "fixture/DescribeStackResourceDriftsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeStackResourceDrifts)

responseListStacks :: ListStacksResponse -> TestTree
responseListStacks =
  res
    "ListStacksResponse"
    "fixture/ListStacksResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListStacks)

responseRegisterPublisher :: RegisterPublisherResponse -> TestTree
responseRegisterPublisher =
  res
    "RegisterPublisherResponse"
    "fixture/RegisterPublisherResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RegisterPublisher)

responseDescribeAccountLimits :: DescribeAccountLimitsResponse -> TestTree
responseDescribeAccountLimits =
  res
    "DescribeAccountLimitsResponse"
    "fixture/DescribeAccountLimitsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeAccountLimits)

responseDescribeStackResources :: DescribeStackResourcesResponse -> TestTree
responseDescribeStackResources =
  res
    "DescribeStackResourcesResponse"
    "fixture/DescribeStackResourcesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeStackResources)

responseDescribeStackInstance :: DescribeStackInstanceResponse -> TestTree
responseDescribeStackInstance =
  res
    "DescribeStackInstanceResponse"
    "fixture/DescribeStackInstanceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeStackInstance)

responseCreateStack :: CreateStackResponse -> TestTree
responseCreateStack =
  res
    "CreateStackResponse"
    "fixture/CreateStackResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateStack)

responseUpdateStackSet :: UpdateStackSetResponse -> TestTree
responseUpdateStackSet =
  res
    "UpdateStackSetResponse"
    "fixture/UpdateStackSetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateStackSet)

responseDeleteStackSet :: DeleteStackSetResponse -> TestTree
responseDeleteStackSet =
  res
    "DeleteStackSetResponse"
    "fixture/DeleteStackSetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteStackSet)

responseEstimateTemplateCost :: EstimateTemplateCostResponse -> TestTree
responseEstimateTemplateCost =
  res
    "EstimateTemplateCostResponse"
    "fixture/EstimateTemplateCostResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy EstimateTemplateCost)

responseDeleteChangeSet :: DeleteChangeSetResponse -> TestTree
responseDeleteChangeSet =
  res
    "DeleteChangeSetResponse"
    "fixture/DeleteChangeSetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteChangeSet)

responseListStackSets :: ListStackSetsResponse -> TestTree
responseListStackSets =
  res
    "ListStackSetsResponse"
    "fixture/ListStackSetsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListStackSets)

responseListExports :: ListExportsResponse -> TestTree
responseListExports =
  res
    "ListExportsResponse"
    "fixture/ListExportsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListExports)

responseDescribeStackDriftDetectionStatus :: DescribeStackDriftDetectionStatusResponse -> TestTree
responseDescribeStackDriftDetectionStatus =
  res
    "DescribeStackDriftDetectionStatusResponse"
    "fixture/DescribeStackDriftDetectionStatusResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeStackDriftDetectionStatus)

responseRollbackStack :: RollbackStackResponse -> TestTree
responseRollbackStack =
  res
    "RollbackStackResponse"
    "fixture/RollbackStackResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RollbackStack)

responseCreateStackSet :: CreateStackSetResponse -> TestTree
responseCreateStackSet =
  res
    "CreateStackSetResponse"
    "fixture/CreateStackSetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateStackSet)

responseDeregisterType :: DeregisterTypeResponse -> TestTree
responseDeregisterType =
  res
    "DeregisterTypeResponse"
    "fixture/DeregisterTypeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeregisterType)

responseDeactivateType :: DeactivateTypeResponse -> TestTree
responseDeactivateType =
  res
    "DeactivateTypeResponse"
    "fixture/DeactivateTypeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeactivateType)

responseRecordHandlerProgress :: RecordHandlerProgressResponse -> TestTree
responseRecordHandlerProgress =
  res
    "RecordHandlerProgressResponse"
    "fixture/RecordHandlerProgressResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RecordHandlerProgress)

responseListTypeVersions :: ListTypeVersionsResponse -> TestTree
responseListTypeVersions =
  res
    "ListTypeVersionsResponse"
    "fixture/ListTypeVersionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTypeVersions)

responseSetTypeDefaultVersion :: SetTypeDefaultVersionResponse -> TestTree
responseSetTypeDefaultVersion =
  res
    "SetTypeDefaultVersionResponse"
    "fixture/SetTypeDefaultVersionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SetTypeDefaultVersion)

responseUpdateTerminationProtection :: UpdateTerminationProtectionResponse -> TestTree
responseUpdateTerminationProtection =
  res
    "UpdateTerminationProtectionResponse"
    "fixture/UpdateTerminationProtectionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateTerminationProtection)

responseTestType :: TestTypeResponse -> TestTree
responseTestType =
  res
    "TestTypeResponse"
    "fixture/TestTypeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TestType)

responseGetTemplate :: GetTemplateResponse -> TestTree
responseGetTemplate =
  res
    "GetTemplateResponse"
    "fixture/GetTemplateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetTemplate)

responseDetectStackSetDrift :: DetectStackSetDriftResponse -> TestTree
responseDetectStackSetDrift =
  res
    "DetectStackSetDriftResponse"
    "fixture/DetectStackSetDriftResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DetectStackSetDrift)

responseDetectStackResourceDrift :: DetectStackResourceDriftResponse -> TestTree
responseDetectStackResourceDrift =
  res
    "DetectStackResourceDriftResponse"
    "fixture/DetectStackResourceDriftResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DetectStackResourceDrift)

responseDescribeChangeSet :: DescribeChangeSetResponse -> TestTree
responseDescribeChangeSet =
  res
    "DescribeChangeSetResponse"
    "fixture/DescribeChangeSetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeChangeSet)

responseDescribeStackSet :: DescribeStackSetResponse -> TestTree
responseDescribeStackSet =
  res
    "DescribeStackSetResponse"
    "fixture/DescribeStackSetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeStackSet)

responseListStackSetOperationResults :: ListStackSetOperationResultsResponse -> TestTree
responseListStackSetOperationResults =
  res
    "ListStackSetOperationResultsResponse"
    "fixture/ListStackSetOperationResultsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListStackSetOperationResults)

responseRegisterType :: RegisterTypeResponse -> TestTree
responseRegisterType =
  res
    "RegisterTypeResponse"
    "fixture/RegisterTypeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RegisterType)

responseActivateType :: ActivateTypeResponse -> TestTree
responseActivateType =
  res
    "ActivateTypeResponse"
    "fixture/ActivateTypeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ActivateType)

responseStopStackSetOperation :: StopStackSetOperationResponse -> TestTree
responseStopStackSetOperation =
  res
    "StopStackSetOperationResponse"
    "fixture/StopStackSetOperationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StopStackSetOperation)

responseDescribeStackResource :: DescribeStackResourceResponse -> TestTree
responseDescribeStackResource =
  res
    "DescribeStackResourceResponse"
    "fixture/DescribeStackResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeStackResource)
