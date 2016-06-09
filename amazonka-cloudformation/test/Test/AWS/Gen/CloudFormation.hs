{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.CloudFormation
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
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
--         [ requestDeleteStack $
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
--         , requestGetStackPolicy $
--             getStackPolicy
--
--         , requestDescribeStacks $
--             describeStacks
--
--         , requestCreateChangeSet $
--             createChangeSet
--
--         , requestExecuteChangeSet $
--             executeChangeSet
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
--         , requestListStacks $
--             listStacks
--
--         , requestDescribeAccountLimits $
--             describeAccountLimits
--
--         , requestDescribeStackResources $
--             describeStackResources
--
--         , requestCreateStack $
--             createStack
--
--         , requestEstimateTemplateCost $
--             estimateTemplateCost
--
--         , requestDeleteChangeSet $
--             deleteChangeSet
--
--         , requestGetTemplate $
--             getTemplate
--
--         , requestDescribeChangeSet $
--             describeChangeSet
--
--         , requestDescribeStackResource $
--             describeStackResource
--
--           ]

--     , testGroup "response"
--         [ responseDeleteStack $
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
--         , responseGetStackPolicy $
--             getStackPolicyResponse
--
--         , responseDescribeStacks $
--             describeStacksResponse
--
--         , responseCreateChangeSet $
--             createChangeSetResponse
--
--         , responseExecuteChangeSet $
--             executeChangeSetResponse
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
--         , responseListStacks $
--             listStacksResponse
--
--         , responseDescribeAccountLimits $
--             describeAccountLimitsResponse
--
--         , responseDescribeStackResources $
--             describeStackResourcesResponse
--
--         , responseCreateStack $
--             createStackResponse
--
--         , responseEstimateTemplateCost $
--             estimateTemplateCostResponse
--
--         , responseDeleteChangeSet $
--             deleteChangeSetResponse
--
--         , responseGetTemplate $
--             getTemplateResponse
--
--         , responseDescribeChangeSet $
--             describeChangeSetResponse
--
--         , responseDescribeStackResource $
--             describeStackResourceResponse
--
--           ]
--     ]

-- Requests

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

requestExecuteChangeSet :: ExecuteChangeSet -> TestTree
requestExecuteChangeSet = req
    "ExecuteChangeSet"
    "fixture/ExecuteChangeSet.yaml"

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

requestCreateStack :: CreateStack -> TestTree
requestCreateStack = req
    "CreateStack"
    "fixture/CreateStack.yaml"

requestEstimateTemplateCost :: EstimateTemplateCost -> TestTree
requestEstimateTemplateCost = req
    "EstimateTemplateCost"
    "fixture/EstimateTemplateCost.yaml"

requestDeleteChangeSet :: DeleteChangeSet -> TestTree
requestDeleteChangeSet = req
    "DeleteChangeSet"
    "fixture/DeleteChangeSet.yaml"

requestGetTemplate :: GetTemplate -> TestTree
requestGetTemplate = req
    "GetTemplate"
    "fixture/GetTemplate.yaml"

requestDescribeChangeSet :: DescribeChangeSet -> TestTree
requestDescribeChangeSet = req
    "DescribeChangeSet"
    "fixture/DescribeChangeSet.yaml"

requestDescribeStackResource :: DescribeStackResource -> TestTree
requestDescribeStackResource = req
    "DescribeStackResource"
    "fixture/DescribeStackResource.yaml"

-- Responses

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

responseExecuteChangeSet :: ExecuteChangeSetResponse -> TestTree
responseExecuteChangeSet = res
    "ExecuteChangeSetResponse"
    "fixture/ExecuteChangeSetResponse.proto"
    cloudFormation
    (Proxy :: Proxy ExecuteChangeSet)

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

responseCreateStack :: CreateStackResponse -> TestTree
responseCreateStack = res
    "CreateStackResponse"
    "fixture/CreateStackResponse.proto"
    cloudFormation
    (Proxy :: Proxy CreateStack)

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

responseDescribeStackResource :: DescribeStackResourceResponse -> TestTree
responseDescribeStackResource = res
    "DescribeStackResourceResponse"
    "fixture/DescribeStackResourceResponse.proto"
    cloudFormation
    (Proxy :: Proxy DescribeStackResource)
