-- Module      : Test.AWS.Gen.CloudFormation
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

module Test.AWS.Gen.CloudFormation where

import           Data.Proxy
import           Network.AWS.CloudFormation
import           Test.AWS.Fixture
import           Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures = testGroup "SQS"
--     [ testGroup "request"
--         [ deleteStackTest $
--             deleteStack
--
--         , updateStackTest $
--             updateStack
--
--         , getTemplateSummaryTest $
--             getTemplateSummary
--
--         , listStackResourcesTest $
--             listStackResources
--
--         , describeStacksTest $
--             describeStacks
--
--         , getStackPolicyTest $
--             getStackPolicy
--
--         , validateTemplateTest $
--             validateTemplate
--
--         , cancelUpdateStackTest $
--             cancelUpdateStack
--
--         , setStackPolicyTest $
--             setStackPolicy
--
--         , describeStackEventsTest $
--             describeStackEvents
--
--         , signalResourceTest $
--             signalResource
--
--         , listStacksTest $
--             listStacks
--
--         , createStackTest $
--             createStack
--
--         , describeStackResourcesTest $
--             describeStackResources
--
--         , estimateTemplateCostTest $
--             estimateTemplateCost
--
--         , getTemplateTest $
--             getTemplate
--
--         , describeStackResourceTest $
--             describeStackResource
--
--           ]

--     , testGroup "response"
--         [ deleteStackResponseTest $
--             deleteStackResponse
--
--         , updateStackResponseTest $
--             updateStackResponse
--
--         , getTemplateSummaryResponseTest $
--             getTemplateSummaryResponse
--
--         , listStackResourcesResponseTest $
--             listStackResourcesResponse
--
--         , describeStacksResponseTest $
--             describeStacksResponse
--
--         , getStackPolicyResponseTest $
--             getStackPolicyResponse
--
--         , validateTemplateResponseTest $
--             validateTemplateResponse
--
--         , cancelUpdateStackResponseTest $
--             cancelUpdateStackResponse
--
--         , setStackPolicyResponseTest $
--             setStackPolicyResponse
--
--         , describeStackEventsResponseTest $
--             describeStackEventsResponse
--
--         , signalResourceResponseTest $
--             signalResourceResponse
--
--         , listStacksResponseTest $
--             listStacksResponse
--
--         , createStackResponseTest $
--             createStackResponse
--
--         , describeStackResourcesResponseTest $
--             describeStackResourcesResponse
--
--         , estimateTemplateCostResponseTest $
--             estimateTemplateCostResponse
--
--         , getTemplateResponseTest $
--             getTemplateResponse
--
--         , describeStackResourceResponseTest $
--             describeStackResourceResponse
--
--           ]
--     ]

-- Requests

deleteStackTest :: DeleteStack -> TestTree
deleteStackTest = undefined

updateStackTest :: UpdateStack -> TestTree
updateStackTest = undefined

getTemplateSummaryTest :: GetTemplateSummary -> TestTree
getTemplateSummaryTest = undefined

listStackResourcesTest :: ListStackResources -> TestTree
listStackResourcesTest = undefined

describeStacksTest :: DescribeStacks -> TestTree
describeStacksTest = undefined

getStackPolicyTest :: GetStackPolicy -> TestTree
getStackPolicyTest = undefined

validateTemplateTest :: ValidateTemplate -> TestTree
validateTemplateTest = undefined

cancelUpdateStackTest :: CancelUpdateStack -> TestTree
cancelUpdateStackTest = undefined

setStackPolicyTest :: SetStackPolicy -> TestTree
setStackPolicyTest = undefined

describeStackEventsTest :: DescribeStackEvents -> TestTree
describeStackEventsTest = undefined

signalResourceTest :: SignalResource -> TestTree
signalResourceTest = undefined

listStacksTest :: ListStacks -> TestTree
listStacksTest = undefined

createStackTest :: CreateStack -> TestTree
createStackTest = undefined

describeStackResourcesTest :: DescribeStackResources -> TestTree
describeStackResourcesTest = undefined

estimateTemplateCostTest :: EstimateTemplateCost -> TestTree
estimateTemplateCostTest = undefined

getTemplateTest :: GetTemplate -> TestTree
getTemplateTest = undefined

describeStackResourceTest :: DescribeStackResource -> TestTree
describeStackResourceTest = undefined

-- Responses

deleteStackResponseTest :: DeleteStackResponse -> TestTree
deleteStackResponseTest = resp
    "DeleteStackResponse"
    "fixture/CloudFormation/DeleteStackResponse"
    (Proxy :: Proxy DeleteStack)

updateStackResponseTest :: UpdateStackResponse -> TestTree
updateStackResponseTest = resp
    "UpdateStackResponse"
    "fixture/CloudFormation/UpdateStackResponse"
    (Proxy :: Proxy UpdateStack)

getTemplateSummaryResponseTest :: GetTemplateSummaryResponse -> TestTree
getTemplateSummaryResponseTest = resp
    "GetTemplateSummaryResponse"
    "fixture/CloudFormation/GetTemplateSummaryResponse"
    (Proxy :: Proxy GetTemplateSummary)

listStackResourcesResponseTest :: ListStackResourcesResponse -> TestTree
listStackResourcesResponseTest = resp
    "ListStackResourcesResponse"
    "fixture/CloudFormation/ListStackResourcesResponse"
    (Proxy :: Proxy ListStackResources)

describeStacksResponseTest :: DescribeStacksResponse -> TestTree
describeStacksResponseTest = resp
    "DescribeStacksResponse"
    "fixture/CloudFormation/DescribeStacksResponse"
    (Proxy :: Proxy DescribeStacks)

getStackPolicyResponseTest :: GetStackPolicyResponse -> TestTree
getStackPolicyResponseTest = resp
    "GetStackPolicyResponse"
    "fixture/CloudFormation/GetStackPolicyResponse"
    (Proxy :: Proxy GetStackPolicy)

validateTemplateResponseTest :: ValidateTemplateResponse -> TestTree
validateTemplateResponseTest = resp
    "ValidateTemplateResponse"
    "fixture/CloudFormation/ValidateTemplateResponse"
    (Proxy :: Proxy ValidateTemplate)

cancelUpdateStackResponseTest :: CancelUpdateStackResponse -> TestTree
cancelUpdateStackResponseTest = resp
    "CancelUpdateStackResponse"
    "fixture/CloudFormation/CancelUpdateStackResponse"
    (Proxy :: Proxy CancelUpdateStack)

setStackPolicyResponseTest :: SetStackPolicyResponse -> TestTree
setStackPolicyResponseTest = resp
    "SetStackPolicyResponse"
    "fixture/CloudFormation/SetStackPolicyResponse"
    (Proxy :: Proxy SetStackPolicy)

describeStackEventsResponseTest :: DescribeStackEventsResponse -> TestTree
describeStackEventsResponseTest = resp
    "DescribeStackEventsResponse"
    "fixture/CloudFormation/DescribeStackEventsResponse"
    (Proxy :: Proxy DescribeStackEvents)

signalResourceResponseTest :: SignalResourceResponse -> TestTree
signalResourceResponseTest = resp
    "SignalResourceResponse"
    "fixture/CloudFormation/SignalResourceResponse"
    (Proxy :: Proxy SignalResource)

listStacksResponseTest :: ListStacksResponse -> TestTree
listStacksResponseTest = resp
    "ListStacksResponse"
    "fixture/CloudFormation/ListStacksResponse"
    (Proxy :: Proxy ListStacks)

createStackResponseTest :: CreateStackResponse -> TestTree
createStackResponseTest = resp
    "CreateStackResponse"
    "fixture/CloudFormation/CreateStackResponse"
    (Proxy :: Proxy CreateStack)

describeStackResourcesResponseTest :: DescribeStackResourcesResponse -> TestTree
describeStackResourcesResponseTest = resp
    "DescribeStackResourcesResponse"
    "fixture/CloudFormation/DescribeStackResourcesResponse"
    (Proxy :: Proxy DescribeStackResources)

estimateTemplateCostResponseTest :: EstimateTemplateCostResponse -> TestTree
estimateTemplateCostResponseTest = resp
    "EstimateTemplateCostResponse"
    "fixture/CloudFormation/EstimateTemplateCostResponse"
    (Proxy :: Proxy EstimateTemplateCost)

getTemplateResponseTest :: GetTemplateResponse -> TestTree
getTemplateResponseTest = resp
    "GetTemplateResponse"
    "fixture/CloudFormation/GetTemplateResponse"
    (Proxy :: Proxy GetTemplate)

describeStackResourceResponseTest :: DescribeStackResourceResponse -> TestTree
describeStackResourceResponseTest = resp
    "DescribeStackResourceResponse"
    "fixture/CloudFormation/DescribeStackResourceResponse"
    (Proxy :: Proxy DescribeStackResource)
