{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.CloudFormation
-- Copyright   : (c) 2013-2023 Brendan Hay
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
--         [ requestActivateType $
--             newActivateType
--
--         , requestBatchDescribeTypeConfigurations $
--             newBatchDescribeTypeConfigurations
--
--         , requestCancelUpdateStack $
--             newCancelUpdateStack
--
--         , requestContinueUpdateRollback $
--             newContinueUpdateRollback
--
--         , requestCreateChangeSet $
--             newCreateChangeSet
--
--         , requestCreateStack $
--             newCreateStack
--
--         , requestCreateStackInstances $
--             newCreateStackInstances
--
--         , requestCreateStackSet $
--             newCreateStackSet
--
--         , requestDeactivateType $
--             newDeactivateType
--
--         , requestDeleteChangeSet $
--             newDeleteChangeSet
--
--         , requestDeleteStack $
--             newDeleteStack
--
--         , requestDeleteStackInstances $
--             newDeleteStackInstances
--
--         , requestDeleteStackSet $
--             newDeleteStackSet
--
--         , requestDeregisterType $
--             newDeregisterType
--
--         , requestDescribeAccountLimits $
--             newDescribeAccountLimits
--
--         , requestDescribeChangeSet $
--             newDescribeChangeSet
--
--         , requestDescribeChangeSetHooks $
--             newDescribeChangeSetHooks
--
--         , requestDescribePublisher $
--             newDescribePublisher
--
--         , requestDescribeStackDriftDetectionStatus $
--             newDescribeStackDriftDetectionStatus
--
--         , requestDescribeStackEvents $
--             newDescribeStackEvents
--
--         , requestDescribeStackInstance $
--             newDescribeStackInstance
--
--         , requestDescribeStackResource $
--             newDescribeStackResource
--
--         , requestDescribeStackResourceDrifts $
--             newDescribeStackResourceDrifts
--
--         , requestDescribeStackResources $
--             newDescribeStackResources
--
--         , requestDescribeStackSet $
--             newDescribeStackSet
--
--         , requestDescribeStackSetOperation $
--             newDescribeStackSetOperation
--
--         , requestDescribeStacks $
--             newDescribeStacks
--
--         , requestDescribeType $
--             newDescribeType
--
--         , requestDescribeTypeRegistration $
--             newDescribeTypeRegistration
--
--         , requestDetectStackDrift $
--             newDetectStackDrift
--
--         , requestDetectStackResourceDrift $
--             newDetectStackResourceDrift
--
--         , requestDetectStackSetDrift $
--             newDetectStackSetDrift
--
--         , requestEstimateTemplateCost $
--             newEstimateTemplateCost
--
--         , requestExecuteChangeSet $
--             newExecuteChangeSet
--
--         , requestGetStackPolicy $
--             newGetStackPolicy
--
--         , requestGetTemplate $
--             newGetTemplate
--
--         , requestGetTemplateSummary $
--             newGetTemplateSummary
--
--         , requestImportStacksToStackSet $
--             newImportStacksToStackSet
--
--         , requestListChangeSets $
--             newListChangeSets
--
--         , requestListExports $
--             newListExports
--
--         , requestListImports $
--             newListImports
--
--         , requestListStackInstances $
--             newListStackInstances
--
--         , requestListStackResources $
--             newListStackResources
--
--         , requestListStackSetOperationResults $
--             newListStackSetOperationResults
--
--         , requestListStackSetOperations $
--             newListStackSetOperations
--
--         , requestListStackSets $
--             newListStackSets
--
--         , requestListStacks $
--             newListStacks
--
--         , requestListTypeRegistrations $
--             newListTypeRegistrations
--
--         , requestListTypeVersions $
--             newListTypeVersions
--
--         , requestListTypes $
--             newListTypes
--
--         , requestPublishType $
--             newPublishType
--
--         , requestRecordHandlerProgress $
--             newRecordHandlerProgress
--
--         , requestRegisterPublisher $
--             newRegisterPublisher
--
--         , requestRegisterType $
--             newRegisterType
--
--         , requestRollbackStack $
--             newRollbackStack
--
--         , requestSetStackPolicy $
--             newSetStackPolicy
--
--         , requestSetTypeConfiguration $
--             newSetTypeConfiguration
--
--         , requestSetTypeDefaultVersion $
--             newSetTypeDefaultVersion
--
--         , requestSignalResource $
--             newSignalResource
--
--         , requestStopStackSetOperation $
--             newStopStackSetOperation
--
--         , requestTestType $
--             newTestType
--
--         , requestUpdateStack $
--             newUpdateStack
--
--         , requestUpdateStackInstances $
--             newUpdateStackInstances
--
--         , requestUpdateStackSet $
--             newUpdateStackSet
--
--         , requestUpdateTerminationProtection $
--             newUpdateTerminationProtection
--
--         , requestValidateTemplate $
--             newValidateTemplate
--
--           ]

--     , testGroup "response"
--         [ responseActivateType $
--             newActivateTypeResponse
--
--         , responseBatchDescribeTypeConfigurations $
--             newBatchDescribeTypeConfigurationsResponse
--
--         , responseCancelUpdateStack $
--             newCancelUpdateStackResponse
--
--         , responseContinueUpdateRollback $
--             newContinueUpdateRollbackResponse
--
--         , responseCreateChangeSet $
--             newCreateChangeSetResponse
--
--         , responseCreateStack $
--             newCreateStackResponse
--
--         , responseCreateStackInstances $
--             newCreateStackInstancesResponse
--
--         , responseCreateStackSet $
--             newCreateStackSetResponse
--
--         , responseDeactivateType $
--             newDeactivateTypeResponse
--
--         , responseDeleteChangeSet $
--             newDeleteChangeSetResponse
--
--         , responseDeleteStack $
--             newDeleteStackResponse
--
--         , responseDeleteStackInstances $
--             newDeleteStackInstancesResponse
--
--         , responseDeleteStackSet $
--             newDeleteStackSetResponse
--
--         , responseDeregisterType $
--             newDeregisterTypeResponse
--
--         , responseDescribeAccountLimits $
--             newDescribeAccountLimitsResponse
--
--         , responseDescribeChangeSet $
--             newDescribeChangeSetResponse
--
--         , responseDescribeChangeSetHooks $
--             newDescribeChangeSetHooksResponse
--
--         , responseDescribePublisher $
--             newDescribePublisherResponse
--
--         , responseDescribeStackDriftDetectionStatus $
--             newDescribeStackDriftDetectionStatusResponse
--
--         , responseDescribeStackEvents $
--             newDescribeStackEventsResponse
--
--         , responseDescribeStackInstance $
--             newDescribeStackInstanceResponse
--
--         , responseDescribeStackResource $
--             newDescribeStackResourceResponse
--
--         , responseDescribeStackResourceDrifts $
--             newDescribeStackResourceDriftsResponse
--
--         , responseDescribeStackResources $
--             newDescribeStackResourcesResponse
--
--         , responseDescribeStackSet $
--             newDescribeStackSetResponse
--
--         , responseDescribeStackSetOperation $
--             newDescribeStackSetOperationResponse
--
--         , responseDescribeStacks $
--             newDescribeStacksResponse
--
--         , responseDescribeType $
--             newDescribeTypeResponse
--
--         , responseDescribeTypeRegistration $
--             newDescribeTypeRegistrationResponse
--
--         , responseDetectStackDrift $
--             newDetectStackDriftResponse
--
--         , responseDetectStackResourceDrift $
--             newDetectStackResourceDriftResponse
--
--         , responseDetectStackSetDrift $
--             newDetectStackSetDriftResponse
--
--         , responseEstimateTemplateCost $
--             newEstimateTemplateCostResponse
--
--         , responseExecuteChangeSet $
--             newExecuteChangeSetResponse
--
--         , responseGetStackPolicy $
--             newGetStackPolicyResponse
--
--         , responseGetTemplate $
--             newGetTemplateResponse
--
--         , responseGetTemplateSummary $
--             newGetTemplateSummaryResponse
--
--         , responseImportStacksToStackSet $
--             newImportStacksToStackSetResponse
--
--         , responseListChangeSets $
--             newListChangeSetsResponse
--
--         , responseListExports $
--             newListExportsResponse
--
--         , responseListImports $
--             newListImportsResponse
--
--         , responseListStackInstances $
--             newListStackInstancesResponse
--
--         , responseListStackResources $
--             newListStackResourcesResponse
--
--         , responseListStackSetOperationResults $
--             newListStackSetOperationResultsResponse
--
--         , responseListStackSetOperations $
--             newListStackSetOperationsResponse
--
--         , responseListStackSets $
--             newListStackSetsResponse
--
--         , responseListStacks $
--             newListStacksResponse
--
--         , responseListTypeRegistrations $
--             newListTypeRegistrationsResponse
--
--         , responseListTypeVersions $
--             newListTypeVersionsResponse
--
--         , responseListTypes $
--             newListTypesResponse
--
--         , responsePublishType $
--             newPublishTypeResponse
--
--         , responseRecordHandlerProgress $
--             newRecordHandlerProgressResponse
--
--         , responseRegisterPublisher $
--             newRegisterPublisherResponse
--
--         , responseRegisterType $
--             newRegisterTypeResponse
--
--         , responseRollbackStack $
--             newRollbackStackResponse
--
--         , responseSetStackPolicy $
--             newSetStackPolicyResponse
--
--         , responseSetTypeConfiguration $
--             newSetTypeConfigurationResponse
--
--         , responseSetTypeDefaultVersion $
--             newSetTypeDefaultVersionResponse
--
--         , responseSignalResource $
--             newSignalResourceResponse
--
--         , responseStopStackSetOperation $
--             newStopStackSetOperationResponse
--
--         , responseTestType $
--             newTestTypeResponse
--
--         , responseUpdateStack $
--             newUpdateStackResponse
--
--         , responseUpdateStackInstances $
--             newUpdateStackInstancesResponse
--
--         , responseUpdateStackSet $
--             newUpdateStackSetResponse
--
--         , responseUpdateTerminationProtection $
--             newUpdateTerminationProtectionResponse
--
--         , responseValidateTemplate $
--             newValidateTemplateResponse
--
--           ]
--     ]

-- Requests

requestActivateType :: ActivateType -> TestTree
requestActivateType =
  req
    "ActivateType"
    "fixture/ActivateType.yaml"

requestBatchDescribeTypeConfigurations :: BatchDescribeTypeConfigurations -> TestTree
requestBatchDescribeTypeConfigurations =
  req
    "BatchDescribeTypeConfigurations"
    "fixture/BatchDescribeTypeConfigurations.yaml"

requestCancelUpdateStack :: CancelUpdateStack -> TestTree
requestCancelUpdateStack =
  req
    "CancelUpdateStack"
    "fixture/CancelUpdateStack.yaml"

requestContinueUpdateRollback :: ContinueUpdateRollback -> TestTree
requestContinueUpdateRollback =
  req
    "ContinueUpdateRollback"
    "fixture/ContinueUpdateRollback.yaml"

requestCreateChangeSet :: CreateChangeSet -> TestTree
requestCreateChangeSet =
  req
    "CreateChangeSet"
    "fixture/CreateChangeSet.yaml"

requestCreateStack :: CreateStack -> TestTree
requestCreateStack =
  req
    "CreateStack"
    "fixture/CreateStack.yaml"

requestCreateStackInstances :: CreateStackInstances -> TestTree
requestCreateStackInstances =
  req
    "CreateStackInstances"
    "fixture/CreateStackInstances.yaml"

requestCreateStackSet :: CreateStackSet -> TestTree
requestCreateStackSet =
  req
    "CreateStackSet"
    "fixture/CreateStackSet.yaml"

requestDeactivateType :: DeactivateType -> TestTree
requestDeactivateType =
  req
    "DeactivateType"
    "fixture/DeactivateType.yaml"

requestDeleteChangeSet :: DeleteChangeSet -> TestTree
requestDeleteChangeSet =
  req
    "DeleteChangeSet"
    "fixture/DeleteChangeSet.yaml"

requestDeleteStack :: DeleteStack -> TestTree
requestDeleteStack =
  req
    "DeleteStack"
    "fixture/DeleteStack.yaml"

requestDeleteStackInstances :: DeleteStackInstances -> TestTree
requestDeleteStackInstances =
  req
    "DeleteStackInstances"
    "fixture/DeleteStackInstances.yaml"

requestDeleteStackSet :: DeleteStackSet -> TestTree
requestDeleteStackSet =
  req
    "DeleteStackSet"
    "fixture/DeleteStackSet.yaml"

requestDeregisterType :: DeregisterType -> TestTree
requestDeregisterType =
  req
    "DeregisterType"
    "fixture/DeregisterType.yaml"

requestDescribeAccountLimits :: DescribeAccountLimits -> TestTree
requestDescribeAccountLimits =
  req
    "DescribeAccountLimits"
    "fixture/DescribeAccountLimits.yaml"

requestDescribeChangeSet :: DescribeChangeSet -> TestTree
requestDescribeChangeSet =
  req
    "DescribeChangeSet"
    "fixture/DescribeChangeSet.yaml"

requestDescribeChangeSetHooks :: DescribeChangeSetHooks -> TestTree
requestDescribeChangeSetHooks =
  req
    "DescribeChangeSetHooks"
    "fixture/DescribeChangeSetHooks.yaml"

requestDescribePublisher :: DescribePublisher -> TestTree
requestDescribePublisher =
  req
    "DescribePublisher"
    "fixture/DescribePublisher.yaml"

requestDescribeStackDriftDetectionStatus :: DescribeStackDriftDetectionStatus -> TestTree
requestDescribeStackDriftDetectionStatus =
  req
    "DescribeStackDriftDetectionStatus"
    "fixture/DescribeStackDriftDetectionStatus.yaml"

requestDescribeStackEvents :: DescribeStackEvents -> TestTree
requestDescribeStackEvents =
  req
    "DescribeStackEvents"
    "fixture/DescribeStackEvents.yaml"

requestDescribeStackInstance :: DescribeStackInstance -> TestTree
requestDescribeStackInstance =
  req
    "DescribeStackInstance"
    "fixture/DescribeStackInstance.yaml"

requestDescribeStackResource :: DescribeStackResource -> TestTree
requestDescribeStackResource =
  req
    "DescribeStackResource"
    "fixture/DescribeStackResource.yaml"

requestDescribeStackResourceDrifts :: DescribeStackResourceDrifts -> TestTree
requestDescribeStackResourceDrifts =
  req
    "DescribeStackResourceDrifts"
    "fixture/DescribeStackResourceDrifts.yaml"

requestDescribeStackResources :: DescribeStackResources -> TestTree
requestDescribeStackResources =
  req
    "DescribeStackResources"
    "fixture/DescribeStackResources.yaml"

requestDescribeStackSet :: DescribeStackSet -> TestTree
requestDescribeStackSet =
  req
    "DescribeStackSet"
    "fixture/DescribeStackSet.yaml"

requestDescribeStackSetOperation :: DescribeStackSetOperation -> TestTree
requestDescribeStackSetOperation =
  req
    "DescribeStackSetOperation"
    "fixture/DescribeStackSetOperation.yaml"

requestDescribeStacks :: DescribeStacks -> TestTree
requestDescribeStacks =
  req
    "DescribeStacks"
    "fixture/DescribeStacks.yaml"

requestDescribeType :: DescribeType -> TestTree
requestDescribeType =
  req
    "DescribeType"
    "fixture/DescribeType.yaml"

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

requestDetectStackResourceDrift :: DetectStackResourceDrift -> TestTree
requestDetectStackResourceDrift =
  req
    "DetectStackResourceDrift"
    "fixture/DetectStackResourceDrift.yaml"

requestDetectStackSetDrift :: DetectStackSetDrift -> TestTree
requestDetectStackSetDrift =
  req
    "DetectStackSetDrift"
    "fixture/DetectStackSetDrift.yaml"

requestEstimateTemplateCost :: EstimateTemplateCost -> TestTree
requestEstimateTemplateCost =
  req
    "EstimateTemplateCost"
    "fixture/EstimateTemplateCost.yaml"

requestExecuteChangeSet :: ExecuteChangeSet -> TestTree
requestExecuteChangeSet =
  req
    "ExecuteChangeSet"
    "fixture/ExecuteChangeSet.yaml"

requestGetStackPolicy :: GetStackPolicy -> TestTree
requestGetStackPolicy =
  req
    "GetStackPolicy"
    "fixture/GetStackPolicy.yaml"

requestGetTemplate :: GetTemplate -> TestTree
requestGetTemplate =
  req
    "GetTemplate"
    "fixture/GetTemplate.yaml"

requestGetTemplateSummary :: GetTemplateSummary -> TestTree
requestGetTemplateSummary =
  req
    "GetTemplateSummary"
    "fixture/GetTemplateSummary.yaml"

requestImportStacksToStackSet :: ImportStacksToStackSet -> TestTree
requestImportStacksToStackSet =
  req
    "ImportStacksToStackSet"
    "fixture/ImportStacksToStackSet.yaml"

requestListChangeSets :: ListChangeSets -> TestTree
requestListChangeSets =
  req
    "ListChangeSets"
    "fixture/ListChangeSets.yaml"

requestListExports :: ListExports -> TestTree
requestListExports =
  req
    "ListExports"
    "fixture/ListExports.yaml"

requestListImports :: ListImports -> TestTree
requestListImports =
  req
    "ListImports"
    "fixture/ListImports.yaml"

requestListStackInstances :: ListStackInstances -> TestTree
requestListStackInstances =
  req
    "ListStackInstances"
    "fixture/ListStackInstances.yaml"

requestListStackResources :: ListStackResources -> TestTree
requestListStackResources =
  req
    "ListStackResources"
    "fixture/ListStackResources.yaml"

requestListStackSetOperationResults :: ListStackSetOperationResults -> TestTree
requestListStackSetOperationResults =
  req
    "ListStackSetOperationResults"
    "fixture/ListStackSetOperationResults.yaml"

requestListStackSetOperations :: ListStackSetOperations -> TestTree
requestListStackSetOperations =
  req
    "ListStackSetOperations"
    "fixture/ListStackSetOperations.yaml"

requestListStackSets :: ListStackSets -> TestTree
requestListStackSets =
  req
    "ListStackSets"
    "fixture/ListStackSets.yaml"

requestListStacks :: ListStacks -> TestTree
requestListStacks =
  req
    "ListStacks"
    "fixture/ListStacks.yaml"

requestListTypeRegistrations :: ListTypeRegistrations -> TestTree
requestListTypeRegistrations =
  req
    "ListTypeRegistrations"
    "fixture/ListTypeRegistrations.yaml"

requestListTypeVersions :: ListTypeVersions -> TestTree
requestListTypeVersions =
  req
    "ListTypeVersions"
    "fixture/ListTypeVersions.yaml"

requestListTypes :: ListTypes -> TestTree
requestListTypes =
  req
    "ListTypes"
    "fixture/ListTypes.yaml"

requestPublishType :: PublishType -> TestTree
requestPublishType =
  req
    "PublishType"
    "fixture/PublishType.yaml"

requestRecordHandlerProgress :: RecordHandlerProgress -> TestTree
requestRecordHandlerProgress =
  req
    "RecordHandlerProgress"
    "fixture/RecordHandlerProgress.yaml"

requestRegisterPublisher :: RegisterPublisher -> TestTree
requestRegisterPublisher =
  req
    "RegisterPublisher"
    "fixture/RegisterPublisher.yaml"

requestRegisterType :: RegisterType -> TestTree
requestRegisterType =
  req
    "RegisterType"
    "fixture/RegisterType.yaml"

requestRollbackStack :: RollbackStack -> TestTree
requestRollbackStack =
  req
    "RollbackStack"
    "fixture/RollbackStack.yaml"

requestSetStackPolicy :: SetStackPolicy -> TestTree
requestSetStackPolicy =
  req
    "SetStackPolicy"
    "fixture/SetStackPolicy.yaml"

requestSetTypeConfiguration :: SetTypeConfiguration -> TestTree
requestSetTypeConfiguration =
  req
    "SetTypeConfiguration"
    "fixture/SetTypeConfiguration.yaml"

requestSetTypeDefaultVersion :: SetTypeDefaultVersion -> TestTree
requestSetTypeDefaultVersion =
  req
    "SetTypeDefaultVersion"
    "fixture/SetTypeDefaultVersion.yaml"

requestSignalResource :: SignalResource -> TestTree
requestSignalResource =
  req
    "SignalResource"
    "fixture/SignalResource.yaml"

requestStopStackSetOperation :: StopStackSetOperation -> TestTree
requestStopStackSetOperation =
  req
    "StopStackSetOperation"
    "fixture/StopStackSetOperation.yaml"

requestTestType :: TestType -> TestTree
requestTestType =
  req
    "TestType"
    "fixture/TestType.yaml"

requestUpdateStack :: UpdateStack -> TestTree
requestUpdateStack =
  req
    "UpdateStack"
    "fixture/UpdateStack.yaml"

requestUpdateStackInstances :: UpdateStackInstances -> TestTree
requestUpdateStackInstances =
  req
    "UpdateStackInstances"
    "fixture/UpdateStackInstances.yaml"

requestUpdateStackSet :: UpdateStackSet -> TestTree
requestUpdateStackSet =
  req
    "UpdateStackSet"
    "fixture/UpdateStackSet.yaml"

requestUpdateTerminationProtection :: UpdateTerminationProtection -> TestTree
requestUpdateTerminationProtection =
  req
    "UpdateTerminationProtection"
    "fixture/UpdateTerminationProtection.yaml"

requestValidateTemplate :: ValidateTemplate -> TestTree
requestValidateTemplate =
  req
    "ValidateTemplate"
    "fixture/ValidateTemplate.yaml"

-- Responses

responseActivateType :: ActivateTypeResponse -> TestTree
responseActivateType =
  res
    "ActivateTypeResponse"
    "fixture/ActivateTypeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ActivateType)

responseBatchDescribeTypeConfigurations :: BatchDescribeTypeConfigurationsResponse -> TestTree
responseBatchDescribeTypeConfigurations =
  res
    "BatchDescribeTypeConfigurationsResponse"
    "fixture/BatchDescribeTypeConfigurationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchDescribeTypeConfigurations)

responseCancelUpdateStack :: CancelUpdateStackResponse -> TestTree
responseCancelUpdateStack =
  res
    "CancelUpdateStackResponse"
    "fixture/CancelUpdateStackResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CancelUpdateStack)

responseContinueUpdateRollback :: ContinueUpdateRollbackResponse -> TestTree
responseContinueUpdateRollback =
  res
    "ContinueUpdateRollbackResponse"
    "fixture/ContinueUpdateRollbackResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ContinueUpdateRollback)

responseCreateChangeSet :: CreateChangeSetResponse -> TestTree
responseCreateChangeSet =
  res
    "CreateChangeSetResponse"
    "fixture/CreateChangeSetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateChangeSet)

responseCreateStack :: CreateStackResponse -> TestTree
responseCreateStack =
  res
    "CreateStackResponse"
    "fixture/CreateStackResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateStack)

responseCreateStackInstances :: CreateStackInstancesResponse -> TestTree
responseCreateStackInstances =
  res
    "CreateStackInstancesResponse"
    "fixture/CreateStackInstancesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateStackInstances)

responseCreateStackSet :: CreateStackSetResponse -> TestTree
responseCreateStackSet =
  res
    "CreateStackSetResponse"
    "fixture/CreateStackSetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateStackSet)

responseDeactivateType :: DeactivateTypeResponse -> TestTree
responseDeactivateType =
  res
    "DeactivateTypeResponse"
    "fixture/DeactivateTypeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeactivateType)

responseDeleteChangeSet :: DeleteChangeSetResponse -> TestTree
responseDeleteChangeSet =
  res
    "DeleteChangeSetResponse"
    "fixture/DeleteChangeSetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteChangeSet)

responseDeleteStack :: DeleteStackResponse -> TestTree
responseDeleteStack =
  res
    "DeleteStackResponse"
    "fixture/DeleteStackResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteStack)

responseDeleteStackInstances :: DeleteStackInstancesResponse -> TestTree
responseDeleteStackInstances =
  res
    "DeleteStackInstancesResponse"
    "fixture/DeleteStackInstancesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteStackInstances)

responseDeleteStackSet :: DeleteStackSetResponse -> TestTree
responseDeleteStackSet =
  res
    "DeleteStackSetResponse"
    "fixture/DeleteStackSetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteStackSet)

responseDeregisterType :: DeregisterTypeResponse -> TestTree
responseDeregisterType =
  res
    "DeregisterTypeResponse"
    "fixture/DeregisterTypeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeregisterType)

responseDescribeAccountLimits :: DescribeAccountLimitsResponse -> TestTree
responseDescribeAccountLimits =
  res
    "DescribeAccountLimitsResponse"
    "fixture/DescribeAccountLimitsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeAccountLimits)

responseDescribeChangeSet :: DescribeChangeSetResponse -> TestTree
responseDescribeChangeSet =
  res
    "DescribeChangeSetResponse"
    "fixture/DescribeChangeSetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeChangeSet)

responseDescribeChangeSetHooks :: DescribeChangeSetHooksResponse -> TestTree
responseDescribeChangeSetHooks =
  res
    "DescribeChangeSetHooksResponse"
    "fixture/DescribeChangeSetHooksResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeChangeSetHooks)

responseDescribePublisher :: DescribePublisherResponse -> TestTree
responseDescribePublisher =
  res
    "DescribePublisherResponse"
    "fixture/DescribePublisherResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribePublisher)

responseDescribeStackDriftDetectionStatus :: DescribeStackDriftDetectionStatusResponse -> TestTree
responseDescribeStackDriftDetectionStatus =
  res
    "DescribeStackDriftDetectionStatusResponse"
    "fixture/DescribeStackDriftDetectionStatusResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeStackDriftDetectionStatus)

responseDescribeStackEvents :: DescribeStackEventsResponse -> TestTree
responseDescribeStackEvents =
  res
    "DescribeStackEventsResponse"
    "fixture/DescribeStackEventsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeStackEvents)

responseDescribeStackInstance :: DescribeStackInstanceResponse -> TestTree
responseDescribeStackInstance =
  res
    "DescribeStackInstanceResponse"
    "fixture/DescribeStackInstanceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeStackInstance)

responseDescribeStackResource :: DescribeStackResourceResponse -> TestTree
responseDescribeStackResource =
  res
    "DescribeStackResourceResponse"
    "fixture/DescribeStackResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeStackResource)

responseDescribeStackResourceDrifts :: DescribeStackResourceDriftsResponse -> TestTree
responseDescribeStackResourceDrifts =
  res
    "DescribeStackResourceDriftsResponse"
    "fixture/DescribeStackResourceDriftsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeStackResourceDrifts)

responseDescribeStackResources :: DescribeStackResourcesResponse -> TestTree
responseDescribeStackResources =
  res
    "DescribeStackResourcesResponse"
    "fixture/DescribeStackResourcesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeStackResources)

responseDescribeStackSet :: DescribeStackSetResponse -> TestTree
responseDescribeStackSet =
  res
    "DescribeStackSetResponse"
    "fixture/DescribeStackSetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeStackSet)

responseDescribeStackSetOperation :: DescribeStackSetOperationResponse -> TestTree
responseDescribeStackSetOperation =
  res
    "DescribeStackSetOperationResponse"
    "fixture/DescribeStackSetOperationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeStackSetOperation)

responseDescribeStacks :: DescribeStacksResponse -> TestTree
responseDescribeStacks =
  res
    "DescribeStacksResponse"
    "fixture/DescribeStacksResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeStacks)

responseDescribeType :: DescribeTypeResponse -> TestTree
responseDescribeType =
  res
    "DescribeTypeResponse"
    "fixture/DescribeTypeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeType)

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

responseDetectStackResourceDrift :: DetectStackResourceDriftResponse -> TestTree
responseDetectStackResourceDrift =
  res
    "DetectStackResourceDriftResponse"
    "fixture/DetectStackResourceDriftResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DetectStackResourceDrift)

responseDetectStackSetDrift :: DetectStackSetDriftResponse -> TestTree
responseDetectStackSetDrift =
  res
    "DetectStackSetDriftResponse"
    "fixture/DetectStackSetDriftResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DetectStackSetDrift)

responseEstimateTemplateCost :: EstimateTemplateCostResponse -> TestTree
responseEstimateTemplateCost =
  res
    "EstimateTemplateCostResponse"
    "fixture/EstimateTemplateCostResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy EstimateTemplateCost)

responseExecuteChangeSet :: ExecuteChangeSetResponse -> TestTree
responseExecuteChangeSet =
  res
    "ExecuteChangeSetResponse"
    "fixture/ExecuteChangeSetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ExecuteChangeSet)

responseGetStackPolicy :: GetStackPolicyResponse -> TestTree
responseGetStackPolicy =
  res
    "GetStackPolicyResponse"
    "fixture/GetStackPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetStackPolicy)

responseGetTemplate :: GetTemplateResponse -> TestTree
responseGetTemplate =
  res
    "GetTemplateResponse"
    "fixture/GetTemplateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetTemplate)

responseGetTemplateSummary :: GetTemplateSummaryResponse -> TestTree
responseGetTemplateSummary =
  res
    "GetTemplateSummaryResponse"
    "fixture/GetTemplateSummaryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetTemplateSummary)

responseImportStacksToStackSet :: ImportStacksToStackSetResponse -> TestTree
responseImportStacksToStackSet =
  res
    "ImportStacksToStackSetResponse"
    "fixture/ImportStacksToStackSetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ImportStacksToStackSet)

responseListChangeSets :: ListChangeSetsResponse -> TestTree
responseListChangeSets =
  res
    "ListChangeSetsResponse"
    "fixture/ListChangeSetsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListChangeSets)

responseListExports :: ListExportsResponse -> TestTree
responseListExports =
  res
    "ListExportsResponse"
    "fixture/ListExportsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListExports)

responseListImports :: ListImportsResponse -> TestTree
responseListImports =
  res
    "ListImportsResponse"
    "fixture/ListImportsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListImports)

responseListStackInstances :: ListStackInstancesResponse -> TestTree
responseListStackInstances =
  res
    "ListStackInstancesResponse"
    "fixture/ListStackInstancesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListStackInstances)

responseListStackResources :: ListStackResourcesResponse -> TestTree
responseListStackResources =
  res
    "ListStackResourcesResponse"
    "fixture/ListStackResourcesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListStackResources)

responseListStackSetOperationResults :: ListStackSetOperationResultsResponse -> TestTree
responseListStackSetOperationResults =
  res
    "ListStackSetOperationResultsResponse"
    "fixture/ListStackSetOperationResultsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListStackSetOperationResults)

responseListStackSetOperations :: ListStackSetOperationsResponse -> TestTree
responseListStackSetOperations =
  res
    "ListStackSetOperationsResponse"
    "fixture/ListStackSetOperationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListStackSetOperations)

responseListStackSets :: ListStackSetsResponse -> TestTree
responseListStackSets =
  res
    "ListStackSetsResponse"
    "fixture/ListStackSetsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListStackSets)

responseListStacks :: ListStacksResponse -> TestTree
responseListStacks =
  res
    "ListStacksResponse"
    "fixture/ListStacksResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListStacks)

responseListTypeRegistrations :: ListTypeRegistrationsResponse -> TestTree
responseListTypeRegistrations =
  res
    "ListTypeRegistrationsResponse"
    "fixture/ListTypeRegistrationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTypeRegistrations)

responseListTypeVersions :: ListTypeVersionsResponse -> TestTree
responseListTypeVersions =
  res
    "ListTypeVersionsResponse"
    "fixture/ListTypeVersionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTypeVersions)

responseListTypes :: ListTypesResponse -> TestTree
responseListTypes =
  res
    "ListTypesResponse"
    "fixture/ListTypesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTypes)

responsePublishType :: PublishTypeResponse -> TestTree
responsePublishType =
  res
    "PublishTypeResponse"
    "fixture/PublishTypeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PublishType)

responseRecordHandlerProgress :: RecordHandlerProgressResponse -> TestTree
responseRecordHandlerProgress =
  res
    "RecordHandlerProgressResponse"
    "fixture/RecordHandlerProgressResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RecordHandlerProgress)

responseRegisterPublisher :: RegisterPublisherResponse -> TestTree
responseRegisterPublisher =
  res
    "RegisterPublisherResponse"
    "fixture/RegisterPublisherResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RegisterPublisher)

responseRegisterType :: RegisterTypeResponse -> TestTree
responseRegisterType =
  res
    "RegisterTypeResponse"
    "fixture/RegisterTypeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RegisterType)

responseRollbackStack :: RollbackStackResponse -> TestTree
responseRollbackStack =
  res
    "RollbackStackResponse"
    "fixture/RollbackStackResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RollbackStack)

responseSetStackPolicy :: SetStackPolicyResponse -> TestTree
responseSetStackPolicy =
  res
    "SetStackPolicyResponse"
    "fixture/SetStackPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SetStackPolicy)

responseSetTypeConfiguration :: SetTypeConfigurationResponse -> TestTree
responseSetTypeConfiguration =
  res
    "SetTypeConfigurationResponse"
    "fixture/SetTypeConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SetTypeConfiguration)

responseSetTypeDefaultVersion :: SetTypeDefaultVersionResponse -> TestTree
responseSetTypeDefaultVersion =
  res
    "SetTypeDefaultVersionResponse"
    "fixture/SetTypeDefaultVersionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SetTypeDefaultVersion)

responseSignalResource :: SignalResourceResponse -> TestTree
responseSignalResource =
  res
    "SignalResourceResponse"
    "fixture/SignalResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SignalResource)

responseStopStackSetOperation :: StopStackSetOperationResponse -> TestTree
responseStopStackSetOperation =
  res
    "StopStackSetOperationResponse"
    "fixture/StopStackSetOperationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StopStackSetOperation)

responseTestType :: TestTypeResponse -> TestTree
responseTestType =
  res
    "TestTypeResponse"
    "fixture/TestTypeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TestType)

responseUpdateStack :: UpdateStackResponse -> TestTree
responseUpdateStack =
  res
    "UpdateStackResponse"
    "fixture/UpdateStackResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateStack)

responseUpdateStackInstances :: UpdateStackInstancesResponse -> TestTree
responseUpdateStackInstances =
  res
    "UpdateStackInstancesResponse"
    "fixture/UpdateStackInstancesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateStackInstances)

responseUpdateStackSet :: UpdateStackSetResponse -> TestTree
responseUpdateStackSet =
  res
    "UpdateStackSetResponse"
    "fixture/UpdateStackSetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateStackSet)

responseUpdateTerminationProtection :: UpdateTerminationProtectionResponse -> TestTree
responseUpdateTerminationProtection =
  res
    "UpdateTerminationProtectionResponse"
    "fixture/UpdateTerminationProtectionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateTerminationProtection)

responseValidateTemplate :: ValidateTemplateResponse -> TestTree
responseValidateTemplate =
  res
    "ValidateTemplateResponse"
    "fixture/ValidateTemplateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ValidateTemplate)
