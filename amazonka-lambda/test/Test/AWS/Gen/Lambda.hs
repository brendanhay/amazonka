{-# OPTIONS_GHC -fno-warn-orphans #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.Lambda
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
module Test.AWS.Gen.Lambda where

import Data.Proxy
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.Tasty
import Network.AWS.Lambda
import Test.AWS.Lambda.Internal

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ testGetFunctionConfiguration $
--             getFunctionConfiguration
--
--         , testUpdateEventSourceMapping $
--             updateEventSourceMapping
--
--         , testDeleteEventSourceMapping $
--             deleteEventSourceMapping
--
--         , testRemovePermission $
--             removePermission
--
--         , testInvoke $
--             invoke
--
--         , testGetEventSourceMapping $
--             getEventSourceMapping
--
--         , testCreateFunction $
--             createFunction
--
--         , testCreateEventSourceMapping $
--             createEventSourceMapping
--
--         , testGetFunction $
--             getFunction
--
--         , testListEventSourceMappings $
--             listEventSourceMappings
--
--         , testAddPermission $
--             addPermission
--
--         , testDeleteFunction $
--             deleteFunction
--
--         , testUpdateFunctionConfiguration $
--             updateFunctionConfiguration
--
--         , testListFunctions $
--             listFunctions
--
--         , testUpdateFunctionCode $
--             updateFunctionCode
--
--         , testGetPolicy $
--             getPolicy
--
--           ]

--     , testGroup "response"
--         [ testGetFunctionConfigurationResponse $
--             functionConfiguration
--
--         , testUpdateEventSourceMappingResponse $
--             eventSourceMappingConfiguration
--
--         , testDeleteEventSourceMappingResponse $
--             eventSourceMappingConfiguration
--
--         , testRemovePermissionResponse $
--             removePermissionResponse
--
--         , testInvokeResponse $
--             invokeResponse
--
--         , testGetEventSourceMappingResponse $
--             eventSourceMappingConfiguration
--
--         , testCreateFunctionResponse $
--             functionConfiguration
--
--         , testCreateEventSourceMappingResponse $
--             eventSourceMappingConfiguration
--
--         , testGetFunctionResponse $
--             getFunctionResponse
--
--         , testListEventSourceMappingsResponse $
--             listEventSourceMappingsResponse
--
--         , testAddPermissionResponse $
--             addPermissionResponse
--
--         , testDeleteFunctionResponse $
--             deleteFunctionResponse
--
--         , testUpdateFunctionConfigurationResponse $
--             functionConfiguration
--
--         , testListFunctionsResponse $
--             listFunctionsResponse
--
--         , testUpdateFunctionCodeResponse $
--             functionConfiguration
--
--         , testGetPolicyResponse $
--             getPolicyResponse
--
--           ]
--     ]

-- Requests

testGetFunctionConfiguration :: GetFunctionConfiguration -> TestTree
testGetFunctionConfiguration = undefined

testUpdateEventSourceMapping :: UpdateEventSourceMapping -> TestTree
testUpdateEventSourceMapping = undefined

testDeleteEventSourceMapping :: DeleteEventSourceMapping -> TestTree
testDeleteEventSourceMapping = undefined

testRemovePermission :: RemovePermission -> TestTree
testRemovePermission = undefined

testInvoke :: Invoke -> TestTree
testInvoke = undefined

testGetEventSourceMapping :: GetEventSourceMapping -> TestTree
testGetEventSourceMapping = undefined

testCreateFunction :: CreateFunction -> TestTree
testCreateFunction = undefined

testCreateEventSourceMapping :: CreateEventSourceMapping -> TestTree
testCreateEventSourceMapping = undefined

testGetFunction :: GetFunction -> TestTree
testGetFunction = undefined

testListEventSourceMappings :: ListEventSourceMappings -> TestTree
testListEventSourceMappings = undefined

testAddPermission :: AddPermission -> TestTree
testAddPermission = undefined

testDeleteFunction :: DeleteFunction -> TestTree
testDeleteFunction = undefined

testUpdateFunctionConfiguration :: UpdateFunctionConfiguration -> TestTree
testUpdateFunctionConfiguration = undefined

testListFunctions :: ListFunctions -> TestTree
testListFunctions = undefined

testUpdateFunctionCode :: UpdateFunctionCode -> TestTree
testUpdateFunctionCode = undefined

testGetPolicy :: GetPolicy -> TestTree
testGetPolicy = undefined

-- Responses

testGetFunctionConfigurationResponse :: FunctionConfiguration -> TestTree
testGetFunctionConfigurationResponse = resp
    "GetFunctionConfigurationResponse"
    "fixture/GetFunctionConfigurationResponse"
    (Proxy :: Proxy GetFunctionConfiguration)

testUpdateEventSourceMappingResponse :: EventSourceMappingConfiguration -> TestTree
testUpdateEventSourceMappingResponse = resp
    "UpdateEventSourceMappingResponse"
    "fixture/UpdateEventSourceMappingResponse"
    (Proxy :: Proxy UpdateEventSourceMapping)

testDeleteEventSourceMappingResponse :: EventSourceMappingConfiguration -> TestTree
testDeleteEventSourceMappingResponse = resp
    "DeleteEventSourceMappingResponse"
    "fixture/DeleteEventSourceMappingResponse"
    (Proxy :: Proxy DeleteEventSourceMapping)

testRemovePermissionResponse :: RemovePermissionResponse -> TestTree
testRemovePermissionResponse = resp
    "RemovePermissionResponse"
    "fixture/RemovePermissionResponse"
    (Proxy :: Proxy RemovePermission)

testInvokeResponse :: InvokeResponse -> TestTree
testInvokeResponse = resp
    "InvokeResponse"
    "fixture/InvokeResponse"
    (Proxy :: Proxy Invoke)

testGetEventSourceMappingResponse :: EventSourceMappingConfiguration -> TestTree
testGetEventSourceMappingResponse = resp
    "GetEventSourceMappingResponse"
    "fixture/GetEventSourceMappingResponse"
    (Proxy :: Proxy GetEventSourceMapping)

testCreateFunctionResponse :: FunctionConfiguration -> TestTree
testCreateFunctionResponse = resp
    "CreateFunctionResponse"
    "fixture/CreateFunctionResponse"
    (Proxy :: Proxy CreateFunction)

testCreateEventSourceMappingResponse :: EventSourceMappingConfiguration -> TestTree
testCreateEventSourceMappingResponse = resp
    "CreateEventSourceMappingResponse"
    "fixture/CreateEventSourceMappingResponse"
    (Proxy :: Proxy CreateEventSourceMapping)

testGetFunctionResponse :: GetFunctionResponse -> TestTree
testGetFunctionResponse = resp
    "GetFunctionResponse"
    "fixture/GetFunctionResponse"
    (Proxy :: Proxy GetFunction)

testListEventSourceMappingsResponse :: ListEventSourceMappingsResponse -> TestTree
testListEventSourceMappingsResponse = resp
    "ListEventSourceMappingsResponse"
    "fixture/ListEventSourceMappingsResponse"
    (Proxy :: Proxy ListEventSourceMappings)

testAddPermissionResponse :: AddPermissionResponse -> TestTree
testAddPermissionResponse = resp
    "AddPermissionResponse"
    "fixture/AddPermissionResponse"
    (Proxy :: Proxy AddPermission)

testDeleteFunctionResponse :: DeleteFunctionResponse -> TestTree
testDeleteFunctionResponse = resp
    "DeleteFunctionResponse"
    "fixture/DeleteFunctionResponse"
    (Proxy :: Proxy DeleteFunction)

testUpdateFunctionConfigurationResponse :: FunctionConfiguration -> TestTree
testUpdateFunctionConfigurationResponse = resp
    "UpdateFunctionConfigurationResponse"
    "fixture/UpdateFunctionConfigurationResponse"
    (Proxy :: Proxy UpdateFunctionConfiguration)

testListFunctionsResponse :: ListFunctionsResponse -> TestTree
testListFunctionsResponse = resp
    "ListFunctionsResponse"
    "fixture/ListFunctionsResponse"
    (Proxy :: Proxy ListFunctions)

testUpdateFunctionCodeResponse :: FunctionConfiguration -> TestTree
testUpdateFunctionCodeResponse = resp
    "UpdateFunctionCodeResponse"
    "fixture/UpdateFunctionCodeResponse"
    (Proxy :: Proxy UpdateFunctionCode)

testGetPolicyResponse :: GetPolicyResponse -> TestTree
testGetPolicyResponse = resp
    "GetPolicyResponse"
    "fixture/GetPolicyResponse"
    (Proxy :: Proxy GetPolicy)

instance Out AddPermission
instance Out AddPermissionResponse
instance Out CreateEventSourceMapping
instance Out CreateFunction
instance Out DeleteEventSourceMapping
instance Out DeleteFunction
instance Out DeleteFunctionResponse
instance Out EventSourceMappingConfiguration
instance Out EventSourcePosition
instance Out FunctionCode
instance Out FunctionCodeLocation
instance Out FunctionConfiguration
instance Out GetEventSourceMapping
instance Out GetFunction
instance Out GetFunctionConfiguration
instance Out GetFunctionResponse
instance Out GetPolicy
instance Out GetPolicyResponse
instance Out InvocationType
instance Out Invoke
instance Out InvokeResponse
instance Out ListEventSourceMappings
instance Out ListEventSourceMappingsResponse
instance Out ListFunctions
instance Out ListFunctionsResponse
instance Out LogType
instance Out RemovePermission
instance Out RemovePermissionResponse
instance Out Runtime
instance Out UpdateEventSourceMapping
instance Out UpdateFunctionCode
instance Out UpdateFunctionConfiguration
