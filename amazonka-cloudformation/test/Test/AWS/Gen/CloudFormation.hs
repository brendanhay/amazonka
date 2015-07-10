{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.CloudFormation
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
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
--         [ testDeleteStack $
--             deleteStack
--
--         , testUpdateStack $
--             updateStack
--
--         , testGetTemplateSummary $
--             getTemplateSummary
--
--         , testListStackResources $
--             listStackResources
--
--         , testDescribeStacks $
--             describeStacks
--
--         , testGetStackPolicy $
--             getStackPolicy
--
--         , testValidateTemplate $
--             validateTemplate
--
--         , testCancelUpdateStack $
--             cancelUpdateStack
--
--         , testSetStackPolicy $
--             setStackPolicy
--
--         , testDescribeStackEvents $
--             describeStackEvents
--
--         , testSignalResource $
--             signalResource
--
--         , testListStacks $
--             listStacks
--
--         , testCreateStack $
--             createStack
--
--         , testDescribeStackResources $
--             describeStackResources
--
--         , testEstimateTemplateCost $
--             estimateTemplateCost
--
--         , testGetTemplate $
--             getTemplate
--
--         , testDescribeStackResource $
--             describeStackResource
--
--           ]

--     , testGroup "response"
--         [ testDeleteStackResponse $
--             deleteStackResponse
--
--         , testUpdateStackResponse $
--             updateStackResponse
--
--         , testGetTemplateSummaryResponse $
--             getTemplateSummaryResponse
--
--         , testListStackResourcesResponse $
--             listStackResourcesResponse
--
--         , testDescribeStacksResponse $
--             describeStacksResponse
--
--         , testGetStackPolicyResponse $
--             getStackPolicyResponse
--
--         , testValidateTemplateResponse $
--             validateTemplateResponse
--
--         , testCancelUpdateStackResponse $
--             cancelUpdateStackResponse
--
--         , testSetStackPolicyResponse $
--             setStackPolicyResponse
--
--         , testDescribeStackEventsResponse $
--             describeStackEventsResponse
--
--         , testSignalResourceResponse $
--             signalResourceResponse
--
--         , testListStacksResponse $
--             listStacksResponse
--
--         , testCreateStackResponse $
--             createStackResponse
--
--         , testDescribeStackResourcesResponse $
--             describeStackResourcesResponse
--
--         , testEstimateTemplateCostResponse $
--             estimateTemplateCostResponse
--
--         , testGetTemplateResponse $
--             getTemplateResponse
--
--         , testDescribeStackResourceResponse $
--             describeStackResourceResponse
--
--           ]
--     ]

-- Requests

testDeleteStack :: DeleteStack -> TestTree
testDeleteStack = req
    "DeleteStack"
    "fixture/DeleteStack"

testUpdateStack :: UpdateStack -> TestTree
testUpdateStack = req
    "UpdateStack"
    "fixture/UpdateStack"

testGetTemplateSummary :: GetTemplateSummary -> TestTree
testGetTemplateSummary = req
    "GetTemplateSummary"
    "fixture/GetTemplateSummary"

testListStackResources :: ListStackResources -> TestTree
testListStackResources = req
    "ListStackResources"
    "fixture/ListStackResources"

testDescribeStacks :: DescribeStacks -> TestTree
testDescribeStacks = req
    "DescribeStacks"
    "fixture/DescribeStacks"

testGetStackPolicy :: GetStackPolicy -> TestTree
testGetStackPolicy = req
    "GetStackPolicy"
    "fixture/GetStackPolicy"

testValidateTemplate :: ValidateTemplate -> TestTree
testValidateTemplate = req
    "ValidateTemplate"
    "fixture/ValidateTemplate"

testCancelUpdateStack :: CancelUpdateStack -> TestTree
testCancelUpdateStack = req
    "CancelUpdateStack"
    "fixture/CancelUpdateStack"

testSetStackPolicy :: SetStackPolicy -> TestTree
testSetStackPolicy = req
    "SetStackPolicy"
    "fixture/SetStackPolicy"

testDescribeStackEvents :: DescribeStackEvents -> TestTree
testDescribeStackEvents = req
    "DescribeStackEvents"
    "fixture/DescribeStackEvents"

testSignalResource :: SignalResource -> TestTree
testSignalResource = req
    "SignalResource"
    "fixture/SignalResource"

testListStacks :: ListStacks -> TestTree
testListStacks = req
    "ListStacks"
    "fixture/ListStacks"

testCreateStack :: CreateStack -> TestTree
testCreateStack = req
    "CreateStack"
    "fixture/CreateStack"

testDescribeStackResources :: DescribeStackResources -> TestTree
testDescribeStackResources = req
    "DescribeStackResources"
    "fixture/DescribeStackResources"

testEstimateTemplateCost :: EstimateTemplateCost -> TestTree
testEstimateTemplateCost = req
    "EstimateTemplateCost"
    "fixture/EstimateTemplateCost"

testGetTemplate :: GetTemplate -> TestTree
testGetTemplate = req
    "GetTemplate"
    "fixture/GetTemplate"

testDescribeStackResource :: DescribeStackResource -> TestTree
testDescribeStackResource = req
    "DescribeStackResource"
    "fixture/DescribeStackResource"

-- Responses

testDeleteStackResponse :: DeleteStackResponse -> TestTree
testDeleteStackResponse = res
    "DeleteStackResponse"
    "fixture/DeleteStackResponse"
    (Proxy :: Proxy DeleteStack)

testUpdateStackResponse :: UpdateStackResponse -> TestTree
testUpdateStackResponse = res
    "UpdateStackResponse"
    "fixture/UpdateStackResponse"
    (Proxy :: Proxy UpdateStack)

testGetTemplateSummaryResponse :: GetTemplateSummaryResponse -> TestTree
testGetTemplateSummaryResponse = res
    "GetTemplateSummaryResponse"
    "fixture/GetTemplateSummaryResponse"
    (Proxy :: Proxy GetTemplateSummary)

testListStackResourcesResponse :: ListStackResourcesResponse -> TestTree
testListStackResourcesResponse = res
    "ListStackResourcesResponse"
    "fixture/ListStackResourcesResponse"
    (Proxy :: Proxy ListStackResources)

testDescribeStacksResponse :: DescribeStacksResponse -> TestTree
testDescribeStacksResponse = res
    "DescribeStacksResponse"
    "fixture/DescribeStacksResponse"
    (Proxy :: Proxy DescribeStacks)

testGetStackPolicyResponse :: GetStackPolicyResponse -> TestTree
testGetStackPolicyResponse = res
    "GetStackPolicyResponse"
    "fixture/GetStackPolicyResponse"
    (Proxy :: Proxy GetStackPolicy)

testValidateTemplateResponse :: ValidateTemplateResponse -> TestTree
testValidateTemplateResponse = res
    "ValidateTemplateResponse"
    "fixture/ValidateTemplateResponse"
    (Proxy :: Proxy ValidateTemplate)

testCancelUpdateStackResponse :: CancelUpdateStackResponse -> TestTree
testCancelUpdateStackResponse = res
    "CancelUpdateStackResponse"
    "fixture/CancelUpdateStackResponse"
    (Proxy :: Proxy CancelUpdateStack)

testSetStackPolicyResponse :: SetStackPolicyResponse -> TestTree
testSetStackPolicyResponse = res
    "SetStackPolicyResponse"
    "fixture/SetStackPolicyResponse"
    (Proxy :: Proxy SetStackPolicy)

testDescribeStackEventsResponse :: DescribeStackEventsResponse -> TestTree
testDescribeStackEventsResponse = res
    "DescribeStackEventsResponse"
    "fixture/DescribeStackEventsResponse"
    (Proxy :: Proxy DescribeStackEvents)

testSignalResourceResponse :: SignalResourceResponse -> TestTree
testSignalResourceResponse = res
    "SignalResourceResponse"
    "fixture/SignalResourceResponse"
    (Proxy :: Proxy SignalResource)

testListStacksResponse :: ListStacksResponse -> TestTree
testListStacksResponse = res
    "ListStacksResponse"
    "fixture/ListStacksResponse"
    (Proxy :: Proxy ListStacks)

testCreateStackResponse :: CreateStackResponse -> TestTree
testCreateStackResponse = res
    "CreateStackResponse"
    "fixture/CreateStackResponse"
    (Proxy :: Proxy CreateStack)

testDescribeStackResourcesResponse :: DescribeStackResourcesResponse -> TestTree
testDescribeStackResourcesResponse = res
    "DescribeStackResourcesResponse"
    "fixture/DescribeStackResourcesResponse"
    (Proxy :: Proxy DescribeStackResources)

testEstimateTemplateCostResponse :: EstimateTemplateCostResponse -> TestTree
testEstimateTemplateCostResponse = res
    "EstimateTemplateCostResponse"
    "fixture/EstimateTemplateCostResponse"
    (Proxy :: Proxy EstimateTemplateCost)

testGetTemplateResponse :: GetTemplateResponse -> TestTree
testGetTemplateResponse = res
    "GetTemplateResponse"
    "fixture/GetTemplateResponse"
    (Proxy :: Proxy GetTemplate)

testDescribeStackResourceResponse :: DescribeStackResourceResponse -> TestTree
testDescribeStackResourceResponse = res
    "DescribeStackResourceResponse"
    "fixture/DescribeStackResourceResponse"
    (Proxy :: Proxy DescribeStackResource)

instance Out CancelUpdateStack
instance Out CancelUpdateStackResponse
instance Out Capability
instance Out CreateStack
instance Out CreateStackResponse
instance Out DeleteStack
instance Out DeleteStackResponse
instance Out DescribeStackEvents
instance Out DescribeStackEventsResponse
instance Out DescribeStackResource
instance Out DescribeStackResourceResponse
instance Out DescribeStackResources
instance Out DescribeStackResourcesResponse
instance Out DescribeStacks
instance Out DescribeStacksResponse
instance Out EstimateTemplateCost
instance Out EstimateTemplateCostResponse
instance Out GetStackPolicy
instance Out GetStackPolicyResponse
instance Out GetTemplate
instance Out GetTemplateResponse
instance Out GetTemplateSummary
instance Out GetTemplateSummaryResponse
instance Out ListStackResources
instance Out ListStackResourcesResponse
instance Out ListStacks
instance Out ListStacksResponse
instance Out OnFailure
instance Out Output
instance Out Parameter
instance Out ParameterConstraints
instance Out ParameterDeclaration
instance Out ResourceSignalStatus
instance Out ResourceStatus
instance Out SetStackPolicy
instance Out SetStackPolicyResponse
instance Out SignalResource
instance Out SignalResourceResponse
instance Out Stack
instance Out StackEvent
instance Out StackResource
instance Out StackResourceDetail
instance Out StackResourceSummary
instance Out StackStatus
instance Out StackSummary
instance Out Tag
instance Out TemplateParameter
instance Out UpdateStack
instance Out UpdateStackResponse
instance Out ValidateTemplate
instance Out ValidateTemplateResponse
