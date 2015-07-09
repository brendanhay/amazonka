{-# OPTIONS_GHC -fno-warn-orphans #-}

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
testDeleteStack = undefined

testUpdateStack :: UpdateStack -> TestTree
testUpdateStack = undefined

testGetTemplateSummary :: GetTemplateSummary -> TestTree
testGetTemplateSummary = undefined

testListStackResources :: ListStackResources -> TestTree
testListStackResources = undefined

testDescribeStacks :: DescribeStacks -> TestTree
testDescribeStacks = undefined

testGetStackPolicy :: GetStackPolicy -> TestTree
testGetStackPolicy = undefined

testValidateTemplate :: ValidateTemplate -> TestTree
testValidateTemplate = undefined

testCancelUpdateStack :: CancelUpdateStack -> TestTree
testCancelUpdateStack = undefined

testSetStackPolicy :: SetStackPolicy -> TestTree
testSetStackPolicy = undefined

testDescribeStackEvents :: DescribeStackEvents -> TestTree
testDescribeStackEvents = undefined

testSignalResource :: SignalResource -> TestTree
testSignalResource = undefined

testListStacks :: ListStacks -> TestTree
testListStacks = undefined

testCreateStack :: CreateStack -> TestTree
testCreateStack = undefined

testDescribeStackResources :: DescribeStackResources -> TestTree
testDescribeStackResources = undefined

testEstimateTemplateCost :: EstimateTemplateCost -> TestTree
testEstimateTemplateCost = undefined

testGetTemplate :: GetTemplate -> TestTree
testGetTemplate = undefined

testDescribeStackResource :: DescribeStackResource -> TestTree
testDescribeStackResource = undefined

-- Responses

testDeleteStackResponse :: DeleteStackResponse -> TestTree
testDeleteStackResponse = resp
    "DeleteStackResponse"
    "fixture/DeleteStackResponse"
    (Proxy :: Proxy DeleteStack)

testUpdateStackResponse :: UpdateStackResponse -> TestTree
testUpdateStackResponse = resp
    "UpdateStackResponse"
    "fixture/UpdateStackResponse"
    (Proxy :: Proxy UpdateStack)

testGetTemplateSummaryResponse :: GetTemplateSummaryResponse -> TestTree
testGetTemplateSummaryResponse = resp
    "GetTemplateSummaryResponse"
    "fixture/GetTemplateSummaryResponse"
    (Proxy :: Proxy GetTemplateSummary)

testListStackResourcesResponse :: ListStackResourcesResponse -> TestTree
testListStackResourcesResponse = resp
    "ListStackResourcesResponse"
    "fixture/ListStackResourcesResponse"
    (Proxy :: Proxy ListStackResources)

testDescribeStacksResponse :: DescribeStacksResponse -> TestTree
testDescribeStacksResponse = resp
    "DescribeStacksResponse"
    "fixture/DescribeStacksResponse"
    (Proxy :: Proxy DescribeStacks)

testGetStackPolicyResponse :: GetStackPolicyResponse -> TestTree
testGetStackPolicyResponse = resp
    "GetStackPolicyResponse"
    "fixture/GetStackPolicyResponse"
    (Proxy :: Proxy GetStackPolicy)

testValidateTemplateResponse :: ValidateTemplateResponse -> TestTree
testValidateTemplateResponse = resp
    "ValidateTemplateResponse"
    "fixture/ValidateTemplateResponse"
    (Proxy :: Proxy ValidateTemplate)

testCancelUpdateStackResponse :: CancelUpdateStackResponse -> TestTree
testCancelUpdateStackResponse = resp
    "CancelUpdateStackResponse"
    "fixture/CancelUpdateStackResponse"
    (Proxy :: Proxy CancelUpdateStack)

testSetStackPolicyResponse :: SetStackPolicyResponse -> TestTree
testSetStackPolicyResponse = resp
    "SetStackPolicyResponse"
    "fixture/SetStackPolicyResponse"
    (Proxy :: Proxy SetStackPolicy)

testDescribeStackEventsResponse :: DescribeStackEventsResponse -> TestTree
testDescribeStackEventsResponse = resp
    "DescribeStackEventsResponse"
    "fixture/DescribeStackEventsResponse"
    (Proxy :: Proxy DescribeStackEvents)

testSignalResourceResponse :: SignalResourceResponse -> TestTree
testSignalResourceResponse = resp
    "SignalResourceResponse"
    "fixture/SignalResourceResponse"
    (Proxy :: Proxy SignalResource)

testListStacksResponse :: ListStacksResponse -> TestTree
testListStacksResponse = resp
    "ListStacksResponse"
    "fixture/ListStacksResponse"
    (Proxy :: Proxy ListStacks)

testCreateStackResponse :: CreateStackResponse -> TestTree
testCreateStackResponse = resp
    "CreateStackResponse"
    "fixture/CreateStackResponse"
    (Proxy :: Proxy CreateStack)

testDescribeStackResourcesResponse :: DescribeStackResourcesResponse -> TestTree
testDescribeStackResourcesResponse = resp
    "DescribeStackResourcesResponse"
    "fixture/DescribeStackResourcesResponse"
    (Proxy :: Proxy DescribeStackResources)

testEstimateTemplateCostResponse :: EstimateTemplateCostResponse -> TestTree
testEstimateTemplateCostResponse = resp
    "EstimateTemplateCostResponse"
    "fixture/EstimateTemplateCostResponse"
    (Proxy :: Proxy EstimateTemplateCost)

testGetTemplateResponse :: GetTemplateResponse -> TestTree
testGetTemplateResponse = resp
    "GetTemplateResponse"
    "fixture/GetTemplateResponse"
    (Proxy :: Proxy GetTemplate)

testDescribeStackResourceResponse :: DescribeStackResourceResponse -> TestTree
testDescribeStackResourceResponse = resp
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
