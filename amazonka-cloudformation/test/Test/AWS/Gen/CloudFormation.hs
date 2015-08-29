{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.CloudFormation
-- Copyright   : (c) 2013-2015 Brendan Hay
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
    "fixture/DeleteStack.yaml"

testUpdateStack :: UpdateStack -> TestTree
testUpdateStack = req
    "UpdateStack"
    "fixture/UpdateStack.yaml"

testGetTemplateSummary :: GetTemplateSummary -> TestTree
testGetTemplateSummary = req
    "GetTemplateSummary"
    "fixture/GetTemplateSummary.yaml"

testListStackResources :: ListStackResources -> TestTree
testListStackResources = req
    "ListStackResources"
    "fixture/ListStackResources.yaml"

testDescribeStacks :: DescribeStacks -> TestTree
testDescribeStacks = req
    "DescribeStacks"
    "fixture/DescribeStacks.yaml"

testGetStackPolicy :: GetStackPolicy -> TestTree
testGetStackPolicy = req
    "GetStackPolicy"
    "fixture/GetStackPolicy.yaml"

testValidateTemplate :: ValidateTemplate -> TestTree
testValidateTemplate = req
    "ValidateTemplate"
    "fixture/ValidateTemplate.yaml"

testCancelUpdateStack :: CancelUpdateStack -> TestTree
testCancelUpdateStack = req
    "CancelUpdateStack"
    "fixture/CancelUpdateStack.yaml"

testSetStackPolicy :: SetStackPolicy -> TestTree
testSetStackPolicy = req
    "SetStackPolicy"
    "fixture/SetStackPolicy.yaml"

testDescribeStackEvents :: DescribeStackEvents -> TestTree
testDescribeStackEvents = req
    "DescribeStackEvents"
    "fixture/DescribeStackEvents.yaml"

testSignalResource :: SignalResource -> TestTree
testSignalResource = req
    "SignalResource"
    "fixture/SignalResource.yaml"

testListStacks :: ListStacks -> TestTree
testListStacks = req
    "ListStacks"
    "fixture/ListStacks.yaml"

testCreateStack :: CreateStack -> TestTree
testCreateStack = req
    "CreateStack"
    "fixture/CreateStack.yaml"

testDescribeStackResources :: DescribeStackResources -> TestTree
testDescribeStackResources = req
    "DescribeStackResources"
    "fixture/DescribeStackResources.yaml"

testEstimateTemplateCost :: EstimateTemplateCost -> TestTree
testEstimateTemplateCost = req
    "EstimateTemplateCost"
    "fixture/EstimateTemplateCost.yaml"

testGetTemplate :: GetTemplate -> TestTree
testGetTemplate = req
    "GetTemplate"
    "fixture/GetTemplate.yaml"

testDescribeStackResource :: DescribeStackResource -> TestTree
testDescribeStackResource = req
    "DescribeStackResource"
    "fixture/DescribeStackResource.yaml"

-- Responses

testDeleteStackResponse :: DeleteStackResponse -> TestTree
testDeleteStackResponse = res
    "DeleteStackResponse"
    "fixture/DeleteStackResponse.proto"
    cloudFormation
    (Proxy :: Proxy DeleteStack)

testUpdateStackResponse :: UpdateStackResponse -> TestTree
testUpdateStackResponse = res
    "UpdateStackResponse"
    "fixture/UpdateStackResponse.proto"
    cloudFormation
    (Proxy :: Proxy UpdateStack)

testGetTemplateSummaryResponse :: GetTemplateSummaryResponse -> TestTree
testGetTemplateSummaryResponse = res
    "GetTemplateSummaryResponse"
    "fixture/GetTemplateSummaryResponse.proto"
    cloudFormation
    (Proxy :: Proxy GetTemplateSummary)

testListStackResourcesResponse :: ListStackResourcesResponse -> TestTree
testListStackResourcesResponse = res
    "ListStackResourcesResponse"
    "fixture/ListStackResourcesResponse.proto"
    cloudFormation
    (Proxy :: Proxy ListStackResources)

testDescribeStacksResponse :: DescribeStacksResponse -> TestTree
testDescribeStacksResponse = res
    "DescribeStacksResponse"
    "fixture/DescribeStacksResponse.proto"
    cloudFormation
    (Proxy :: Proxy DescribeStacks)

testGetStackPolicyResponse :: GetStackPolicyResponse -> TestTree
testGetStackPolicyResponse = res
    "GetStackPolicyResponse"
    "fixture/GetStackPolicyResponse.proto"
    cloudFormation
    (Proxy :: Proxy GetStackPolicy)

testValidateTemplateResponse :: ValidateTemplateResponse -> TestTree
testValidateTemplateResponse = res
    "ValidateTemplateResponse"
    "fixture/ValidateTemplateResponse.proto"
    cloudFormation
    (Proxy :: Proxy ValidateTemplate)

testCancelUpdateStackResponse :: CancelUpdateStackResponse -> TestTree
testCancelUpdateStackResponse = res
    "CancelUpdateStackResponse"
    "fixture/CancelUpdateStackResponse.proto"
    cloudFormation
    (Proxy :: Proxy CancelUpdateStack)

testSetStackPolicyResponse :: SetStackPolicyResponse -> TestTree
testSetStackPolicyResponse = res
    "SetStackPolicyResponse"
    "fixture/SetStackPolicyResponse.proto"
    cloudFormation
    (Proxy :: Proxy SetStackPolicy)

testDescribeStackEventsResponse :: DescribeStackEventsResponse -> TestTree
testDescribeStackEventsResponse = res
    "DescribeStackEventsResponse"
    "fixture/DescribeStackEventsResponse.proto"
    cloudFormation
    (Proxy :: Proxy DescribeStackEvents)

testSignalResourceResponse :: SignalResourceResponse -> TestTree
testSignalResourceResponse = res
    "SignalResourceResponse"
    "fixture/SignalResourceResponse.proto"
    cloudFormation
    (Proxy :: Proxy SignalResource)

testListStacksResponse :: ListStacksResponse -> TestTree
testListStacksResponse = res
    "ListStacksResponse"
    "fixture/ListStacksResponse.proto"
    cloudFormation
    (Proxy :: Proxy ListStacks)

testCreateStackResponse :: CreateStackResponse -> TestTree
testCreateStackResponse = res
    "CreateStackResponse"
    "fixture/CreateStackResponse.proto"
    cloudFormation
    (Proxy :: Proxy CreateStack)

testDescribeStackResourcesResponse :: DescribeStackResourcesResponse -> TestTree
testDescribeStackResourcesResponse = res
    "DescribeStackResourcesResponse"
    "fixture/DescribeStackResourcesResponse.proto"
    cloudFormation
    (Proxy :: Proxy DescribeStackResources)

testEstimateTemplateCostResponse :: EstimateTemplateCostResponse -> TestTree
testEstimateTemplateCostResponse = res
    "EstimateTemplateCostResponse"
    "fixture/EstimateTemplateCostResponse.proto"
    cloudFormation
    (Proxy :: Proxy EstimateTemplateCost)

testGetTemplateResponse :: GetTemplateResponse -> TestTree
testGetTemplateResponse = res
    "GetTemplateResponse"
    "fixture/GetTemplateResponse.proto"
    cloudFormation
    (Proxy :: Proxy GetTemplate)

testDescribeStackResourceResponse :: DescribeStackResourceResponse -> TestTree
testDescribeStackResourceResponse = res
    "DescribeStackResourceResponse"
    "fixture/DescribeStackResourceResponse.proto"
    cloudFormation
    (Proxy :: Proxy DescribeStackResource)
