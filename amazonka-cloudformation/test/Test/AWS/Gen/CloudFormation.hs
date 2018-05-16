{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.CloudFormation
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
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
--         [ requestDescribeStackSetOperation $
--             describeStackSetOperation
--
--         , requestDeleteStack $
--             deleteStack
--
--         , requestUpdateStack $
--             updateStack
--
--         , requestGetTemplateSummary $
--             getTemplateSummary
--
--         , requestListChangeSets $
--             listChangeSets
--
--         , requestListStackResources $
--             listStackResources
--
--         , requestUpdateStackInstances $
--             updateStackInstances
--
--         , requestDeleteStackInstances $
--             deleteStackInstances
--
--         , requestCreateStackInstances $
--             createStackInstances
--
--         , requestGetStackPolicy $
--             getStackPolicy
--
--         , requestDescribeStacks $
--             describeStacks
--
--         , requestCreateChangeSet $
--             createChangeSet
--
--         , requestListStackSetOperations $
--             listStackSetOperations
--
--         , requestExecuteChangeSet $
--             executeChangeSet
--
--         , requestListStackInstances $
--             listStackInstances
--
--         , requestContinueUpdateRollback $
--             continueUpdateRollback
--
--         , requestValidateTemplate $
--             validateTemplate
--
--         , requestCancelUpdateStack $
--             cancelUpdateStack
--
--         , requestDescribeStackEvents $
--             describeStackEvents
--
--         , requestSignalResource $
--             signalResource
--
--         , requestSetStackPolicy $
--             setStackPolicy
--
--         , requestListImports $
--             listImports
--
--         , requestListStacks $
--             listStacks
--
--         , requestDescribeAccountLimits $
--             describeAccountLimits
--
--         , requestDescribeStackResources $
--             describeStackResources
--
--         , requestDescribeStackInstance $
--             describeStackInstance
--
--         , requestCreateStack $
--             createStack
--
--         , requestUpdateStackSet $
--             updateStackSet
--
--         , requestDeleteStackSet $
--             deleteStackSet
--
--         , requestEstimateTemplateCost $
--             estimateTemplateCost
--
--         , requestDeleteChangeSet $
--             deleteChangeSet
--
--         , requestListStackSets $
--             listStackSets
--
--         , requestListExports $
--             listExports
--
--         , requestCreateStackSet $
--             createStackSet
--
--         , requestUpdateTerminationProtection $
--             updateTerminationProtection
--
--         , requestGetTemplate $
--             getTemplate
--
--         , requestDescribeChangeSet $
--             describeChangeSet
--
--         , requestDescribeStackSet $
--             describeStackSet
--
--         , requestListStackSetOperationResults $
--             listStackSetOperationResults
--
--         , requestStopStackSetOperation $
--             stopStackSetOperation
--
--         , requestDescribeStackResource $
--             describeStackResource
--
--           ]

--     , testGroup "response"
--         [ responseDescribeStackSetOperation $
--             describeStackSetOperationResponse
--
--         , responseDeleteStack $
--             deleteStackResponse
--
--         , responseUpdateStack $
--             updateStackResponse
--
--         , responseGetTemplateSummary $
--             getTemplateSummaryResponse
--
--         , responseListChangeSets $
--             listChangeSetsResponse
--
--         , responseListStackResources $
--             listStackResourcesResponse
--
--         , responseUpdateStackInstances $
--             updateStackInstancesResponse
--
--         , responseDeleteStackInstances $
--             deleteStackInstancesResponse
--
--         , responseCreateStackInstances $
--             createStackInstancesResponse
--
--         , responseGetStackPolicy $
--             getStackPolicyResponse
--
--         , responseDescribeStacks $
--             describeStacksResponse
--
--         , responseCreateChangeSet $
--             createChangeSetResponse
--
--         , responseListStackSetOperations $
--             listStackSetOperationsResponse
--
--         , responseExecuteChangeSet $
--             executeChangeSetResponse
--
--         , responseListStackInstances $
--             listStackInstancesResponse
--
--         , responseContinueUpdateRollback $
--             continueUpdateRollbackResponse
--
--         , responseValidateTemplate $
--             validateTemplateResponse
--
--         , responseCancelUpdateStack $
--             cancelUpdateStackResponse
--
--         , responseDescribeStackEvents $
--             describeStackEventsResponse
--
--         , responseSignalResource $
--             signalResourceResponse
--
--         , responseSetStackPolicy $
--             setStackPolicyResponse
--
--         , responseListImports $
--             listImportsResponse
--
--         , responseListStacks $
--             listStacksResponse
--
--         , responseDescribeAccountLimits $
--             describeAccountLimitsResponse
--
--         , responseDescribeStackResources $
--             describeStackResourcesResponse
--
--         , responseDescribeStackInstance $
--             describeStackInstanceResponse
--
--         , responseCreateStack $
--             createStackResponse
--
--         , responseUpdateStackSet $
--             updateStackSetResponse
--
--         , responseDeleteStackSet $
--             deleteStackSetResponse
--
--         , responseEstimateTemplateCost $
--             estimateTemplateCostResponse
--
--         , responseDeleteChangeSet $
--             deleteChangeSetResponse
--
--         , responseListStackSets $
--             listStackSetsResponse
--
--         , responseListExports $
--             listExportsResponse
--
--         , responseCreateStackSet $
--             createStackSetResponse
--
--         , responseUpdateTerminationProtection $
--             updateTerminationProtectionResponse
--
--         , responseGetTemplate $
--             getTemplateResponse
--
--         , responseDescribeChangeSet $
--             describeChangeSetResponse
--
--         , responseDescribeStackSet $
--             describeStackSetResponse
--
--         , responseListStackSetOperationResults $
--             listStackSetOperationResultsResponse
--
--         , responseStopStackSetOperation $
--             stopStackSetOperationResponse
--
--         , responseDescribeStackResource $
--             describeStackResourceResponse
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

requestCreateStackInstances :: CreateStackInstances -> TestTree
requestCreateStackInstances = req
    "CreateStackInstances"
    "fixture/CreateStackInstances.yaml"

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

requestCreateStackSet :: CreateStackSet -> TestTree
requestCreateStackSet = req
    "CreateStackSet"
    "fixture/CreateStackSet.yaml"

requestUpdateTerminationProtection :: UpdateTerminationProtection -> TestTree
requestUpdateTerminationProtection = req
    "UpdateTerminationProtection"
    "fixture/UpdateTerminationProtection.yaml"

requestGetTemplate :: GetTemplate -> TestTree
requestGetTemplate = req
    "GetTemplate"
    "fixture/GetTemplate.yaml"

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
    cloudFormation
    (Proxy :: Proxy DescribeStackSetOperation)

responseDeleteStack :: DeleteStackResponse -> TestTree
responseDeleteStack = res
    "DeleteStackResponse"
    "fixture/DeleteStackResponse.proto"
    cloudFormation
    (Proxy :: Proxy DeleteStack)

responseUpdateStack :: UpdateStackResponse -> TestTree
responseUpdateStack = res
    "UpdateStackResponse"
    "fixture/UpdateStackResponse.proto"
    cloudFormation
    (Proxy :: Proxy UpdateStack)

responseGetTemplateSummary :: GetTemplateSummaryResponse -> TestTree
responseGetTemplateSummary = res
    "GetTemplateSummaryResponse"
    "fixture/GetTemplateSummaryResponse.proto"
    cloudFormation
    (Proxy :: Proxy GetTemplateSummary)

responseListChangeSets :: ListChangeSetsResponse -> TestTree
responseListChangeSets = res
    "ListChangeSetsResponse"
    "fixture/ListChangeSetsResponse.proto"
    cloudFormation
    (Proxy :: Proxy ListChangeSets)

responseListStackResources :: ListStackResourcesResponse -> TestTree
responseListStackResources = res
    "ListStackResourcesResponse"
    "fixture/ListStackResourcesResponse.proto"
    cloudFormation
    (Proxy :: Proxy ListStackResources)

responseUpdateStackInstances :: UpdateStackInstancesResponse -> TestTree
responseUpdateStackInstances = res
    "UpdateStackInstancesResponse"
    "fixture/UpdateStackInstancesResponse.proto"
    cloudFormation
    (Proxy :: Proxy UpdateStackInstances)

responseDeleteStackInstances :: DeleteStackInstancesResponse -> TestTree
responseDeleteStackInstances = res
    "DeleteStackInstancesResponse"
    "fixture/DeleteStackInstancesResponse.proto"
    cloudFormation
    (Proxy :: Proxy DeleteStackInstances)

responseCreateStackInstances :: CreateStackInstancesResponse -> TestTree
responseCreateStackInstances = res
    "CreateStackInstancesResponse"
    "fixture/CreateStackInstancesResponse.proto"
    cloudFormation
    (Proxy :: Proxy CreateStackInstances)

responseGetStackPolicy :: GetStackPolicyResponse -> TestTree
responseGetStackPolicy = res
    "GetStackPolicyResponse"
    "fixture/GetStackPolicyResponse.proto"
    cloudFormation
    (Proxy :: Proxy GetStackPolicy)

responseDescribeStacks :: DescribeStacksResponse -> TestTree
responseDescribeStacks = res
    "DescribeStacksResponse"
    "fixture/DescribeStacksResponse.proto"
    cloudFormation
    (Proxy :: Proxy DescribeStacks)

responseCreateChangeSet :: CreateChangeSetResponse -> TestTree
responseCreateChangeSet = res
    "CreateChangeSetResponse"
    "fixture/CreateChangeSetResponse.proto"
    cloudFormation
    (Proxy :: Proxy CreateChangeSet)

responseListStackSetOperations :: ListStackSetOperationsResponse -> TestTree
responseListStackSetOperations = res
    "ListStackSetOperationsResponse"
    "fixture/ListStackSetOperationsResponse.proto"
    cloudFormation
    (Proxy :: Proxy ListStackSetOperations)

responseExecuteChangeSet :: ExecuteChangeSetResponse -> TestTree
responseExecuteChangeSet = res
    "ExecuteChangeSetResponse"
    "fixture/ExecuteChangeSetResponse.proto"
    cloudFormation
    (Proxy :: Proxy ExecuteChangeSet)

responseListStackInstances :: ListStackInstancesResponse -> TestTree
responseListStackInstances = res
    "ListStackInstancesResponse"
    "fixture/ListStackInstancesResponse.proto"
    cloudFormation
    (Proxy :: Proxy ListStackInstances)

responseContinueUpdateRollback :: ContinueUpdateRollbackResponse -> TestTree
responseContinueUpdateRollback = res
    "ContinueUpdateRollbackResponse"
    "fixture/ContinueUpdateRollbackResponse.proto"
    cloudFormation
    (Proxy :: Proxy ContinueUpdateRollback)

responseValidateTemplate :: ValidateTemplateResponse -> TestTree
responseValidateTemplate = res
    "ValidateTemplateResponse"
    "fixture/ValidateTemplateResponse.proto"
    cloudFormation
    (Proxy :: Proxy ValidateTemplate)

responseCancelUpdateStack :: CancelUpdateStackResponse -> TestTree
responseCancelUpdateStack = res
    "CancelUpdateStackResponse"
    "fixture/CancelUpdateStackResponse.proto"
    cloudFormation
    (Proxy :: Proxy CancelUpdateStack)

responseDescribeStackEvents :: DescribeStackEventsResponse -> TestTree
responseDescribeStackEvents = res
    "DescribeStackEventsResponse"
    "fixture/DescribeStackEventsResponse.proto"
    cloudFormation
    (Proxy :: Proxy DescribeStackEvents)

responseSignalResource :: SignalResourceResponse -> TestTree
responseSignalResource = res
    "SignalResourceResponse"
    "fixture/SignalResourceResponse.proto"
    cloudFormation
    (Proxy :: Proxy SignalResource)

responseSetStackPolicy :: SetStackPolicyResponse -> TestTree
responseSetStackPolicy = res
    "SetStackPolicyResponse"
    "fixture/SetStackPolicyResponse.proto"
    cloudFormation
    (Proxy :: Proxy SetStackPolicy)

responseListImports :: ListImportsResponse -> TestTree
responseListImports = res
    "ListImportsResponse"
    "fixture/ListImportsResponse.proto"
    cloudFormation
    (Proxy :: Proxy ListImports)

responseListStacks :: ListStacksResponse -> TestTree
responseListStacks = res
    "ListStacksResponse"
    "fixture/ListStacksResponse.proto"
    cloudFormation
    (Proxy :: Proxy ListStacks)

responseDescribeAccountLimits :: DescribeAccountLimitsResponse -> TestTree
responseDescribeAccountLimits = res
    "DescribeAccountLimitsResponse"
    "fixture/DescribeAccountLimitsResponse.proto"
    cloudFormation
    (Proxy :: Proxy DescribeAccountLimits)

responseDescribeStackResources :: DescribeStackResourcesResponse -> TestTree
responseDescribeStackResources = res
    "DescribeStackResourcesResponse"
    "fixture/DescribeStackResourcesResponse.proto"
    cloudFormation
    (Proxy :: Proxy DescribeStackResources)

responseDescribeStackInstance :: DescribeStackInstanceResponse -> TestTree
responseDescribeStackInstance = res
    "DescribeStackInstanceResponse"
    "fixture/DescribeStackInstanceResponse.proto"
    cloudFormation
    (Proxy :: Proxy DescribeStackInstance)

responseCreateStack :: CreateStackResponse -> TestTree
responseCreateStack = res
    "CreateStackResponse"
    "fixture/CreateStackResponse.proto"
    cloudFormation
    (Proxy :: Proxy CreateStack)

responseUpdateStackSet :: UpdateStackSetResponse -> TestTree
responseUpdateStackSet = res
    "UpdateStackSetResponse"
    "fixture/UpdateStackSetResponse.proto"
    cloudFormation
    (Proxy :: Proxy UpdateStackSet)

responseDeleteStackSet :: DeleteStackSetResponse -> TestTree
responseDeleteStackSet = res
    "DeleteStackSetResponse"
    "fixture/DeleteStackSetResponse.proto"
    cloudFormation
    (Proxy :: Proxy DeleteStackSet)

responseEstimateTemplateCost :: EstimateTemplateCostResponse -> TestTree
responseEstimateTemplateCost = res
    "EstimateTemplateCostResponse"
    "fixture/EstimateTemplateCostResponse.proto"
    cloudFormation
    (Proxy :: Proxy EstimateTemplateCost)

responseDeleteChangeSet :: DeleteChangeSetResponse -> TestTree
responseDeleteChangeSet = res
    "DeleteChangeSetResponse"
    "fixture/DeleteChangeSetResponse.proto"
    cloudFormation
    (Proxy :: Proxy DeleteChangeSet)

responseListStackSets :: ListStackSetsResponse -> TestTree
responseListStackSets = res
    "ListStackSetsResponse"
    "fixture/ListStackSetsResponse.proto"
    cloudFormation
    (Proxy :: Proxy ListStackSets)

responseListExports :: ListExportsResponse -> TestTree
responseListExports = res
    "ListExportsResponse"
    "fixture/ListExportsResponse.proto"
    cloudFormation
    (Proxy :: Proxy ListExports)

responseCreateStackSet :: CreateStackSetResponse -> TestTree
responseCreateStackSet = res
    "CreateStackSetResponse"
    "fixture/CreateStackSetResponse.proto"
    cloudFormation
    (Proxy :: Proxy CreateStackSet)

responseUpdateTerminationProtection :: UpdateTerminationProtectionResponse -> TestTree
responseUpdateTerminationProtection = res
    "UpdateTerminationProtectionResponse"
    "fixture/UpdateTerminationProtectionResponse.proto"
    cloudFormation
    (Proxy :: Proxy UpdateTerminationProtection)

responseGetTemplate :: GetTemplateResponse -> TestTree
responseGetTemplate = res
    "GetTemplateResponse"
    "fixture/GetTemplateResponse.proto"
    cloudFormation
    (Proxy :: Proxy GetTemplate)

responseDescribeChangeSet :: DescribeChangeSetResponse -> TestTree
responseDescribeChangeSet = res
    "DescribeChangeSetResponse"
    "fixture/DescribeChangeSetResponse.proto"
    cloudFormation
    (Proxy :: Proxy DescribeChangeSet)

responseDescribeStackSet :: DescribeStackSetResponse -> TestTree
responseDescribeStackSet = res
    "DescribeStackSetResponse"
    "fixture/DescribeStackSetResponse.proto"
    cloudFormation
    (Proxy :: Proxy DescribeStackSet)

responseListStackSetOperationResults :: ListStackSetOperationResultsResponse -> TestTree
responseListStackSetOperationResults = res
    "ListStackSetOperationResultsResponse"
    "fixture/ListStackSetOperationResultsResponse.proto"
    cloudFormation
    (Proxy :: Proxy ListStackSetOperationResults)

responseStopStackSetOperation :: StopStackSetOperationResponse -> TestTree
responseStopStackSetOperation = res
    "StopStackSetOperationResponse"
    "fixture/StopStackSetOperationResponse.proto"
    cloudFormation
    (Proxy :: Proxy StopStackSetOperation)

responseDescribeStackResource :: DescribeStackResourceResponse -> TestTree
responseDescribeStackResource = res
    "DescribeStackResourceResponse"
    "fixture/DescribeStackResourceResponse.proto"
    cloudFormation
    (Proxy :: Proxy DescribeStackResource)
