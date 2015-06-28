-- Module      : Test.AWS.Gen.Lambda
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

module Test.AWS.Gen.Lambda where

import           Data.Proxy
import           Network.AWS.Lambda
import           Test.AWS.Fixture
import           Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures = testGroup "SQS"
--     [ testGroup "request"
--         [ getFunctionConfigurationTest $
--             getFunctionConfiguration
--
--         , updateEventSourceMappingTest $
--             updateEventSourceMapping
--
--         , deleteEventSourceMappingTest $
--             deleteEventSourceMapping
--
--         , removePermissionTest $
--             removePermission
--
--         , invokeTest $
--             invoke
--
--         , getEventSourceMappingTest $
--             getEventSourceMapping
--
--         , createFunctionTest $
--             createFunction
--
--         , createEventSourceMappingTest $
--             createEventSourceMapping
--
--         , getFunctionTest $
--             getFunction
--
--         , listEventSourceMappingsTest $
--             listEventSourceMappings
--
--         , addPermissionTest $
--             addPermission
--
--         , deleteFunctionTest $
--             deleteFunction
--
--         , updateFunctionConfigurationTest $
--             updateFunctionConfiguration
--
--         , listFunctionsTest $
--             listFunctions
--
--         , updateFunctionCodeTest $
--             updateFunctionCode
--
--         , getPolicyTest $
--             getPolicy
--
--           ]

--     , testGroup "response"
--         [ functionConfigurationTest $
--             functionConfiguration
--
--         , eventSourceMappingConfigurationTest $
--             eventSourceMappingConfiguration
--
--         , eventSourceMappingConfigurationTest $
--             eventSourceMappingConfiguration
--
--         , removePermissionResponseTest $
--             removePermissionResponse
--
--         , invokeResponseTest $
--             invokeResponse
--
--         , eventSourceMappingConfigurationTest $
--             eventSourceMappingConfiguration
--
--         , functionConfigurationTest $
--             functionConfiguration
--
--         , eventSourceMappingConfigurationTest $
--             eventSourceMappingConfiguration
--
--         , getFunctionResponseTest $
--             getFunctionResponse
--
--         , listEventSourceMappingsResponseTest $
--             listEventSourceMappingsResponse
--
--         , addPermissionResponseTest $
--             addPermissionResponse
--
--         , deleteFunctionResponseTest $
--             deleteFunctionResponse
--
--         , functionConfigurationTest $
--             functionConfiguration
--
--         , listFunctionsResponseTest $
--             listFunctionsResponse
--
--         , functionConfigurationTest $
--             functionConfiguration
--
--         , getPolicyResponseTest $
--             getPolicyResponse
--
--           ]
--     ]

-- Requests

getFunctionConfigurationTest :: GetFunctionConfiguration -> TestTree
getFunctionConfigurationTest = undefined

updateEventSourceMappingTest :: UpdateEventSourceMapping -> TestTree
updateEventSourceMappingTest = undefined

deleteEventSourceMappingTest :: DeleteEventSourceMapping -> TestTree
deleteEventSourceMappingTest = undefined

removePermissionTest :: RemovePermission -> TestTree
removePermissionTest = undefined

invokeTest :: Invoke -> TestTree
invokeTest = undefined

getEventSourceMappingTest :: GetEventSourceMapping -> TestTree
getEventSourceMappingTest = undefined

createFunctionTest :: CreateFunction -> TestTree
createFunctionTest = undefined

createEventSourceMappingTest :: CreateEventSourceMapping -> TestTree
createEventSourceMappingTest = undefined

getFunctionTest :: GetFunction -> TestTree
getFunctionTest = undefined

listEventSourceMappingsTest :: ListEventSourceMappings -> TestTree
listEventSourceMappingsTest = undefined

addPermissionTest :: AddPermission -> TestTree
addPermissionTest = undefined

deleteFunctionTest :: DeleteFunction -> TestTree
deleteFunctionTest = undefined

updateFunctionConfigurationTest :: UpdateFunctionConfiguration -> TestTree
updateFunctionConfigurationTest = undefined

listFunctionsTest :: ListFunctions -> TestTree
listFunctionsTest = undefined

updateFunctionCodeTest :: UpdateFunctionCode -> TestTree
updateFunctionCodeTest = undefined

getPolicyTest :: GetPolicy -> TestTree
getPolicyTest = undefined

-- Responses

functionConfigurationTest :: FunctionConfiguration -> TestTree
functionConfigurationTest = resp
    "GetFunctionConfiguration"
    "fixture/Lambda/FunctionConfiguration"
    (Proxy :: Proxy GetFunctionConfiguration)

eventSourceMappingConfigurationTest :: EventSourceMappingConfiguration -> TestTree
eventSourceMappingConfigurationTest = resp
    "UpdateEventSourceMapping"
    "fixture/Lambda/EventSourceMappingConfiguration"
    (Proxy :: Proxy UpdateEventSourceMapping)

eventSourceMappingConfigurationTest :: EventSourceMappingConfiguration -> TestTree
eventSourceMappingConfigurationTest = resp
    "DeleteEventSourceMapping"
    "fixture/Lambda/EventSourceMappingConfiguration"
    (Proxy :: Proxy DeleteEventSourceMapping)

removePermissionResponseTest :: RemovePermissionResponse -> TestTree
removePermissionResponseTest = resp
    "RemovePermission"
    "fixture/Lambda/RemovePermissionResponse"
    (Proxy :: Proxy RemovePermission)

invokeResponseTest :: InvokeResponse -> TestTree
invokeResponseTest = resp
    "Invoke"
    "fixture/Lambda/InvokeResponse"
    (Proxy :: Proxy Invoke)

eventSourceMappingConfigurationTest :: EventSourceMappingConfiguration -> TestTree
eventSourceMappingConfigurationTest = resp
    "GetEventSourceMapping"
    "fixture/Lambda/EventSourceMappingConfiguration"
    (Proxy :: Proxy GetEventSourceMapping)

functionConfigurationTest :: FunctionConfiguration -> TestTree
functionConfigurationTest = resp
    "CreateFunction"
    "fixture/Lambda/FunctionConfiguration"
    (Proxy :: Proxy CreateFunction)

eventSourceMappingConfigurationTest :: EventSourceMappingConfiguration -> TestTree
eventSourceMappingConfigurationTest = resp
    "CreateEventSourceMapping"
    "fixture/Lambda/EventSourceMappingConfiguration"
    (Proxy :: Proxy CreateEventSourceMapping)

getFunctionResponseTest :: GetFunctionResponse -> TestTree
getFunctionResponseTest = resp
    "GetFunction"
    "fixture/Lambda/GetFunctionResponse"
    (Proxy :: Proxy GetFunction)

listEventSourceMappingsResponseTest :: ListEventSourceMappingsResponse -> TestTree
listEventSourceMappingsResponseTest = resp
    "ListEventSourceMappings"
    "fixture/Lambda/ListEventSourceMappingsResponse"
    (Proxy :: Proxy ListEventSourceMappings)

addPermissionResponseTest :: AddPermissionResponse -> TestTree
addPermissionResponseTest = resp
    "AddPermission"
    "fixture/Lambda/AddPermissionResponse"
    (Proxy :: Proxy AddPermission)

deleteFunctionResponseTest :: DeleteFunctionResponse -> TestTree
deleteFunctionResponseTest = resp
    "DeleteFunction"
    "fixture/Lambda/DeleteFunctionResponse"
    (Proxy :: Proxy DeleteFunction)

functionConfigurationTest :: FunctionConfiguration -> TestTree
functionConfigurationTest = resp
    "UpdateFunctionConfiguration"
    "fixture/Lambda/FunctionConfiguration"
    (Proxy :: Proxy UpdateFunctionConfiguration)

listFunctionsResponseTest :: ListFunctionsResponse -> TestTree
listFunctionsResponseTest = resp
    "ListFunctions"
    "fixture/Lambda/ListFunctionsResponse"
    (Proxy :: Proxy ListFunctions)

functionConfigurationTest :: FunctionConfiguration -> TestTree
functionConfigurationTest = resp
    "UpdateFunctionCode"
    "fixture/Lambda/FunctionConfiguration"
    (Proxy :: Proxy UpdateFunctionCode)

getPolicyResponseTest :: GetPolicyResponse -> TestTree
getPolicyResponseTest = resp
    "GetPolicy"
    "fixture/Lambda/GetPolicyResponse"
    (Proxy :: Proxy GetPolicy)
