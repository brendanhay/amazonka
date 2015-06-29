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

import Data.Proxy
import Test.AWS.Fixture
import Test.Tasty
import Network.AWS.CloudFormation

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures = testGroup "SQS"
--     [ testGroup "request"
--         [ cancelUpdateStackTest $
--             cancelUpdateStack
--
--         , createStackTest $
--             createStack
--
--         , deleteStackTest $
--             deleteStack
--
--         , describeStackEventsTest $
--             describeStackEvents
--
--         , describeStackResourceTest $
--             describeStackResource
--
--         , describeStackResourcesTest $
--             describeStackResources
--
--         , describeStacksTest $
--             describeStacks
--
--         , estimateTemplateCostTest $
--             estimateTemplateCost
--
--         , getStackPolicyTest $
--             getStackPolicy
--
--         , getTemplateTest $
--             getTemplate
--
--         , getTemplateSummaryTest $
--             getTemplateSummary
--
--         , listStackResourcesTest $
--             listStackResources
--
--         , listStacksTest $
--             listStacks
--
--         , setStackPolicyTest $
--             setStackPolicy
--
--         , signalResourceTest $
--             signalResource
--
--         , updateStackTest $
--             updateStack
--
--         , validateTemplateTest $
--             validateTemplate
--
--           ]

--     , testGroup "response"
--         [ cancelUpdateStackResponseTest $
--             cancelUpdateStackResponse
--
--         , createStackResponseTest $
--             createStackResponse
--
--         , deleteStackResponseTest $
--             deleteStackResponse
--
--         , describeStackEventsResponseTest $
--             describeStackEventsResponse
--
--         , describeStackResourceResponseTest $
--             describeStackResourceResponse
--
--         , describeStackResourcesResponseTest $
--             describeStackResourcesResponse
--
--         , describeStacksResponseTest $
--             describeStacksResponse
--
--         , estimateTemplateCostResponseTest $
--             estimateTemplateCostResponse
--
--         , getStackPolicyResponseTest $
--             getStackPolicyResponse
--
--         , getTemplateResponseTest $
--             getTemplateResponse
--
--         , getTemplateSummaryResponseTest $
--             getTemplateSummaryResponse
--
--         , listStackResourcesResponseTest $
--             listStackResourcesResponse
--
--         , listStacksResponseTest $
--             listStacksResponse
--
--         , setStackPolicyResponseTest $
--             setStackPolicyResponse
--
--         , signalResourceResponseTest $
--             signalResourceResponse
--
--         , updateStackResponseTest $
--             updateStackResponse
--
--         , validateTemplateResponseTest $
--             validateTemplateResponse
--
--           ]
--     ]

-- Requests

cancelUpdateStackTest :: CancelUpdateStack -> TestTree
cancelUpdateStackTest = undefined

createStackTest :: CreateStack -> TestTree
createStackTest = undefined

deleteStackTest :: DeleteStack -> TestTree
deleteStackTest = undefined

describeStackEventsTest :: DescribeStackEvents -> TestTree
describeStackEventsTest = undefined

describeStackResourceTest :: DescribeStackResource -> TestTree
describeStackResourceTest = undefined

describeStackResourcesTest :: DescribeStackResources -> TestTree
describeStackResourcesTest = undefined

describeStacksTest :: DescribeStacks -> TestTree
describeStacksTest = undefined

estimateTemplateCostTest :: EstimateTemplateCost -> TestTree
estimateTemplateCostTest = undefined

getStackPolicyTest :: GetStackPolicy -> TestTree
getStackPolicyTest = undefined

getTemplateTest :: GetTemplate -> TestTree
getTemplateTest = undefined

getTemplateSummaryTest :: GetTemplateSummary -> TestTree
getTemplateSummaryTest = undefined

listStackResourcesTest :: ListStackResources -> TestTree
listStackResourcesTest = undefined

listStacksTest :: ListStacks -> TestTree
listStacksTest = undefined

setStackPolicyTest :: SetStackPolicy -> TestTree
setStackPolicyTest = undefined

signalResourceTest :: SignalResource -> TestTree
signalResourceTest = undefined

updateStackTest :: UpdateStack -> TestTree
updateStackTest = undefined

validateTemplateTest :: ValidateTemplate -> TestTree
validateTemplateTest = undefined

-- Responses

cancelUpdateStackResponseTest :: CancelUpdateStackResponse -> TestTree
cancelUpdateStackResponseTest = resp
    "cancelUpdateStackResponse"
    "fixture/CancelUpdateStackResponse"
    (Proxy :: Proxy CancelUpdateStack)

createStackResponseTest :: CreateStackResponse -> TestTree
createStackResponseTest = resp
    "createStackResponse"
    "fixture/CreateStackResponse"
    (Proxy :: Proxy CreateStack)

deleteStackResponseTest :: DeleteStackResponse -> TestTree
deleteStackResponseTest = resp
    "deleteStackResponse"
    "fixture/DeleteStackResponse"
    (Proxy :: Proxy DeleteStack)

describeStackEventsResponseTest :: DescribeStackEventsResponse -> TestTree
describeStackEventsResponseTest = resp
    "describeStackEventsResponse"
    "fixture/DescribeStackEventsResponse"
    (Proxy :: Proxy DescribeStackEvents)

describeStackResourceResponseTest :: DescribeStackResourceResponse -> TestTree
describeStackResourceResponseTest = resp
    "describeStackResourceResponse"
    "fixture/DescribeStackResourceResponse"
    (Proxy :: Proxy DescribeStackResource)

describeStackResourcesResponseTest :: DescribeStackResourcesResponse -> TestTree
describeStackResourcesResponseTest = resp
    "describeStackResourcesResponse"
    "fixture/DescribeStackResourcesResponse"
    (Proxy :: Proxy DescribeStackResources)

describeStacksResponseTest :: DescribeStacksResponse -> TestTree
describeStacksResponseTest = resp
    "describeStacksResponse"
    "fixture/DescribeStacksResponse"
    (Proxy :: Proxy DescribeStacks)

estimateTemplateCostResponseTest :: EstimateTemplateCostResponse -> TestTree
estimateTemplateCostResponseTest = resp
    "estimateTemplateCostResponse"
    "fixture/EstimateTemplateCostResponse"
    (Proxy :: Proxy EstimateTemplateCost)

getStackPolicyResponseTest :: GetStackPolicyResponse -> TestTree
getStackPolicyResponseTest = resp
    "getStackPolicyResponse"
    "fixture/GetStackPolicyResponse"
    (Proxy :: Proxy GetStackPolicy)

getTemplateResponseTest :: GetTemplateResponse -> TestTree
getTemplateResponseTest = resp
    "getTemplateResponse"
    "fixture/GetTemplateResponse"
    (Proxy :: Proxy GetTemplate)

getTemplateSummaryResponseTest :: GetTemplateSummaryResponse -> TestTree
getTemplateSummaryResponseTest = resp
    "getTemplateSummaryResponse"
    "fixture/GetTemplateSummaryResponse"
    (Proxy :: Proxy GetTemplateSummary)

listStackResourcesResponseTest :: ListStackResourcesResponse -> TestTree
listStackResourcesResponseTest = resp
    "listStackResourcesResponse"
    "fixture/ListStackResourcesResponse"
    (Proxy :: Proxy ListStackResources)

listStacksResponseTest :: ListStacksResponse -> TestTree
listStacksResponseTest = resp
    "listStacksResponse"
    "fixture/ListStacksResponse"
    (Proxy :: Proxy ListStacks)

setStackPolicyResponseTest :: SetStackPolicyResponse -> TestTree
setStackPolicyResponseTest = resp
    "setStackPolicyResponse"
    "fixture/SetStackPolicyResponse"
    (Proxy :: Proxy SetStackPolicy)

signalResourceResponseTest :: SignalResourceResponse -> TestTree
signalResourceResponseTest = resp
    "signalResourceResponse"
    "fixture/SignalResourceResponse"
    (Proxy :: Proxy SignalResource)

updateStackResponseTest :: UpdateStackResponse -> TestTree
updateStackResponseTest = resp
    "updateStackResponse"
    "fixture/UpdateStackResponse"
    (Proxy :: Proxy UpdateStack)

validateTemplateResponseTest :: ValidateTemplateResponse -> TestTree
validateTemplateResponseTest = resp
    "validateTemplateResponse"
    "fixture/ValidateTemplateResponse"
    (Proxy :: Proxy ValidateTemplate)
