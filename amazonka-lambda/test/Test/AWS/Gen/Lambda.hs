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

import Data.Proxy
import Test.AWS.Fixture
import Test.Tasty
import Network.AWS.Lambda

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures = testGroup "SQS"
--     [ testGroup "request"
--         [ addPermissionTest $
--             addPermission
--
--         , createEventSourceMappingTest $
--             createEventSourceMapping
--
--         , createFunctionTest $
--             createFunction
--
--         , deleteEventSourceMappingTest $
--             deleteEventSourceMapping
--
--         , deleteFunctionTest $
--             deleteFunction
--
--         , getEventSourceMappingTest $
--             getEventSourceMapping
--
--         , getFunctionTest $
--             getFunction
--
--         , getFunctionConfigurationTest $
--             getFunctionConfiguration
--
--         , getPolicyTest $
--             getPolicy
--
--         , invokeTest $
--             invoke
--
--         , listEventSourceMappingsTest $
--             listEventSourceMappings
--
--         , listFunctionsTest $
--             listFunctions
--
--         , removePermissionTest $
--             removePermission
--
--         , updateEventSourceMappingTest $
--             updateEventSourceMapping
--
--         , updateFunctionCodeTest $
--             updateFunctionCode
--
--         , updateFunctionConfigurationTest $
--             updateFunctionConfiguration
--
--           ]

--     , testGroup "response"
--         [ addPermissionResponseTest $
--             addPermissionResponse
--
--         , createEventSourceMappingResponseTest $
--             eventSourceMappingConfiguration
--
--         , createFunctionResponseTest $
--             functionConfiguration
--
--         , deleteEventSourceMappingResponseTest $
--             eventSourceMappingConfiguration
--
--         , deleteFunctionResponseTest $
--             deleteFunctionResponse
--
--         , getEventSourceMappingResponseTest $
--             eventSourceMappingConfiguration
--
--         , getFunctionResponseTest $
--             getFunctionResponse
--
--         , getFunctionConfigurationResponseTest $
--             functionConfiguration
--
--         , getPolicyResponseTest $
--             getPolicyResponse
--
--         , invokeResponseTest $
--             invokeResponse
--
--         , listEventSourceMappingsResponseTest $
--             listEventSourceMappingsResponse
--
--         , listFunctionsResponseTest $
--             listFunctionsResponse
--
--         , removePermissionResponseTest $
--             removePermissionResponse
--
--         , updateEventSourceMappingResponseTest $
--             eventSourceMappingConfiguration
--
--         , updateFunctionCodeResponseTest $
--             functionConfiguration
--
--         , updateFunctionConfigurationResponseTest $
--             functionConfiguration
--
--           ]
--     ]

-- Requests

addPermissionTest :: AddPermission -> TestTree
addPermissionTest = undefined

createEventSourceMappingTest :: CreateEventSourceMapping -> TestTree
createEventSourceMappingTest = undefined

createFunctionTest :: CreateFunction -> TestTree
createFunctionTest = undefined

deleteEventSourceMappingTest :: DeleteEventSourceMapping -> TestTree
deleteEventSourceMappingTest = undefined

deleteFunctionTest :: DeleteFunction -> TestTree
deleteFunctionTest = undefined

getEventSourceMappingTest :: GetEventSourceMapping -> TestTree
getEventSourceMappingTest = undefined

getFunctionTest :: GetFunction -> TestTree
getFunctionTest = undefined

getFunctionConfigurationTest :: GetFunctionConfiguration -> TestTree
getFunctionConfigurationTest = undefined

getPolicyTest :: GetPolicy -> TestTree
getPolicyTest = undefined

invokeTest :: Invoke -> TestTree
invokeTest = undefined

listEventSourceMappingsTest :: ListEventSourceMappings -> TestTree
listEventSourceMappingsTest = undefined

listFunctionsTest :: ListFunctions -> TestTree
listFunctionsTest = undefined

removePermissionTest :: RemovePermission -> TestTree
removePermissionTest = undefined

updateEventSourceMappingTest :: UpdateEventSourceMapping -> TestTree
updateEventSourceMappingTest = undefined

updateFunctionCodeTest :: UpdateFunctionCode -> TestTree
updateFunctionCodeTest = undefined

updateFunctionConfigurationTest :: UpdateFunctionConfiguration -> TestTree
updateFunctionConfigurationTest = undefined

-- Responses

addPermissionResponseTest :: AddPermissionResponse -> TestTree
addPermissionResponseTest = resp
    "addPermissionResponse"
    "fixture/AddPermissionResponse"
    (Proxy :: Proxy AddPermission)

createEventSourceMappingResponseTest :: EventSourceMappingConfiguration -> TestTree
createEventSourceMappingResponseTest = resp
    "createEventSourceMappingResponse"
    "fixture/EventSourceMappingConfiguration"
    (Proxy :: Proxy CreateEventSourceMapping)

createFunctionResponseTest :: FunctionConfiguration -> TestTree
createFunctionResponseTest = resp
    "createFunctionResponse"
    "fixture/FunctionConfiguration"
    (Proxy :: Proxy CreateFunction)

deleteEventSourceMappingResponseTest :: EventSourceMappingConfiguration -> TestTree
deleteEventSourceMappingResponseTest = resp
    "deleteEventSourceMappingResponse"
    "fixture/EventSourceMappingConfiguration"
    (Proxy :: Proxy DeleteEventSourceMapping)

deleteFunctionResponseTest :: DeleteFunctionResponse -> TestTree
deleteFunctionResponseTest = resp
    "deleteFunctionResponse"
    "fixture/DeleteFunctionResponse"
    (Proxy :: Proxy DeleteFunction)

getEventSourceMappingResponseTest :: EventSourceMappingConfiguration -> TestTree
getEventSourceMappingResponseTest = resp
    "getEventSourceMappingResponse"
    "fixture/EventSourceMappingConfiguration"
    (Proxy :: Proxy GetEventSourceMapping)

getFunctionResponseTest :: GetFunctionResponse -> TestTree
getFunctionResponseTest = resp
    "getFunctionResponse"
    "fixture/GetFunctionResponse"
    (Proxy :: Proxy GetFunction)

getFunctionConfigurationResponseTest :: FunctionConfiguration -> TestTree
getFunctionConfigurationResponseTest = resp
    "getFunctionConfigurationResponse"
    "fixture/FunctionConfiguration"
    (Proxy :: Proxy GetFunctionConfiguration)

getPolicyResponseTest :: GetPolicyResponse -> TestTree
getPolicyResponseTest = resp
    "getPolicyResponse"
    "fixture/GetPolicyResponse"
    (Proxy :: Proxy GetPolicy)

invokeResponseTest :: InvokeResponse -> TestTree
invokeResponseTest = resp
    "invokeResponse"
    "fixture/InvokeResponse"
    (Proxy :: Proxy Invoke)

listEventSourceMappingsResponseTest :: ListEventSourceMappingsResponse -> TestTree
listEventSourceMappingsResponseTest = resp
    "listEventSourceMappingsResponse"
    "fixture/ListEventSourceMappingsResponse"
    (Proxy :: Proxy ListEventSourceMappings)

listFunctionsResponseTest :: ListFunctionsResponse -> TestTree
listFunctionsResponseTest = resp
    "listFunctionsResponse"
    "fixture/ListFunctionsResponse"
    (Proxy :: Proxy ListFunctions)

removePermissionResponseTest :: RemovePermissionResponse -> TestTree
removePermissionResponseTest = resp
    "removePermissionResponse"
    "fixture/RemovePermissionResponse"
    (Proxy :: Proxy RemovePermission)

updateEventSourceMappingResponseTest :: EventSourceMappingConfiguration -> TestTree
updateEventSourceMappingResponseTest = resp
    "updateEventSourceMappingResponse"
    "fixture/EventSourceMappingConfiguration"
    (Proxy :: Proxy UpdateEventSourceMapping)

updateFunctionCodeResponseTest :: FunctionConfiguration -> TestTree
updateFunctionCodeResponseTest = resp
    "updateFunctionCodeResponse"
    "fixture/FunctionConfiguration"
    (Proxy :: Proxy UpdateFunctionCode)

updateFunctionConfigurationResponseTest :: FunctionConfiguration -> TestTree
updateFunctionConfigurationResponseTest = resp
    "updateFunctionConfigurationResponse"
    "fixture/FunctionConfiguration"
    (Proxy :: Proxy UpdateFunctionConfiguration)
