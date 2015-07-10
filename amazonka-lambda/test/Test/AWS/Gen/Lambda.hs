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
testGetFunctionConfiguration = req
    "GetFunctionConfiguration"
    "fixture/GetFunctionConfiguration"

testUpdateEventSourceMapping :: UpdateEventSourceMapping -> TestTree
testUpdateEventSourceMapping = req
    "UpdateEventSourceMapping"
    "fixture/UpdateEventSourceMapping"

testDeleteEventSourceMapping :: DeleteEventSourceMapping -> TestTree
testDeleteEventSourceMapping = req
    "DeleteEventSourceMapping"
    "fixture/DeleteEventSourceMapping"

testRemovePermission :: RemovePermission -> TestTree
testRemovePermission = req
    "RemovePermission"
    "fixture/RemovePermission"

testInvoke :: Invoke -> TestTree
testInvoke = req
    "Invoke"
    "fixture/Invoke"

testGetEventSourceMapping :: GetEventSourceMapping -> TestTree
testGetEventSourceMapping = req
    "GetEventSourceMapping"
    "fixture/GetEventSourceMapping"

testCreateFunction :: CreateFunction -> TestTree
testCreateFunction = req
    "CreateFunction"
    "fixture/CreateFunction"

testCreateEventSourceMapping :: CreateEventSourceMapping -> TestTree
testCreateEventSourceMapping = req
    "CreateEventSourceMapping"
    "fixture/CreateEventSourceMapping"

testGetFunction :: GetFunction -> TestTree
testGetFunction = req
    "GetFunction"
    "fixture/GetFunction"

testListEventSourceMappings :: ListEventSourceMappings -> TestTree
testListEventSourceMappings = req
    "ListEventSourceMappings"
    "fixture/ListEventSourceMappings"

testAddPermission :: AddPermission -> TestTree
testAddPermission = req
    "AddPermission"
    "fixture/AddPermission"

testDeleteFunction :: DeleteFunction -> TestTree
testDeleteFunction = req
    "DeleteFunction"
    "fixture/DeleteFunction"

testUpdateFunctionConfiguration :: UpdateFunctionConfiguration -> TestTree
testUpdateFunctionConfiguration = req
    "UpdateFunctionConfiguration"
    "fixture/UpdateFunctionConfiguration"

testListFunctions :: ListFunctions -> TestTree
testListFunctions = req
    "ListFunctions"
    "fixture/ListFunctions"

testUpdateFunctionCode :: UpdateFunctionCode -> TestTree
testUpdateFunctionCode = req
    "UpdateFunctionCode"
    "fixture/UpdateFunctionCode"

testGetPolicy :: GetPolicy -> TestTree
testGetPolicy = req
    "GetPolicy"
    "fixture/GetPolicy"

-- Responses

testGetFunctionConfigurationResponse :: FunctionConfiguration -> TestTree
testGetFunctionConfigurationResponse = res
    "GetFunctionConfigurationResponse"
    "fixture/GetFunctionConfigurationResponse"
    (Proxy :: Proxy GetFunctionConfiguration)

testUpdateEventSourceMappingResponse :: EventSourceMappingConfiguration -> TestTree
testUpdateEventSourceMappingResponse = res
    "UpdateEventSourceMappingResponse"
    "fixture/UpdateEventSourceMappingResponse"
    (Proxy :: Proxy UpdateEventSourceMapping)

testDeleteEventSourceMappingResponse :: EventSourceMappingConfiguration -> TestTree
testDeleteEventSourceMappingResponse = res
    "DeleteEventSourceMappingResponse"
    "fixture/DeleteEventSourceMappingResponse"
    (Proxy :: Proxy DeleteEventSourceMapping)

testRemovePermissionResponse :: RemovePermissionResponse -> TestTree
testRemovePermissionResponse = res
    "RemovePermissionResponse"
    "fixture/RemovePermissionResponse"
    (Proxy :: Proxy RemovePermission)

testInvokeResponse :: InvokeResponse -> TestTree
testInvokeResponse = res
    "InvokeResponse"
    "fixture/InvokeResponse"
    (Proxy :: Proxy Invoke)

testGetEventSourceMappingResponse :: EventSourceMappingConfiguration -> TestTree
testGetEventSourceMappingResponse = res
    "GetEventSourceMappingResponse"
    "fixture/GetEventSourceMappingResponse"
    (Proxy :: Proxy GetEventSourceMapping)

testCreateFunctionResponse :: FunctionConfiguration -> TestTree
testCreateFunctionResponse = res
    "CreateFunctionResponse"
    "fixture/CreateFunctionResponse"
    (Proxy :: Proxy CreateFunction)

testCreateEventSourceMappingResponse :: EventSourceMappingConfiguration -> TestTree
testCreateEventSourceMappingResponse = res
    "CreateEventSourceMappingResponse"
    "fixture/CreateEventSourceMappingResponse"
    (Proxy :: Proxy CreateEventSourceMapping)

testGetFunctionResponse :: GetFunctionResponse -> TestTree
testGetFunctionResponse = res
    "GetFunctionResponse"
    "fixture/GetFunctionResponse"
    (Proxy :: Proxy GetFunction)

testListEventSourceMappingsResponse :: ListEventSourceMappingsResponse -> TestTree
testListEventSourceMappingsResponse = res
    "ListEventSourceMappingsResponse"
    "fixture/ListEventSourceMappingsResponse"
    (Proxy :: Proxy ListEventSourceMappings)

testAddPermissionResponse :: AddPermissionResponse -> TestTree
testAddPermissionResponse = res
    "AddPermissionResponse"
    "fixture/AddPermissionResponse"
    (Proxy :: Proxy AddPermission)

testDeleteFunctionResponse :: DeleteFunctionResponse -> TestTree
testDeleteFunctionResponse = res
    "DeleteFunctionResponse"
    "fixture/DeleteFunctionResponse"
    (Proxy :: Proxy DeleteFunction)

testUpdateFunctionConfigurationResponse :: FunctionConfiguration -> TestTree
testUpdateFunctionConfigurationResponse = res
    "UpdateFunctionConfigurationResponse"
    "fixture/UpdateFunctionConfigurationResponse"
    (Proxy :: Proxy UpdateFunctionConfiguration)

testListFunctionsResponse :: ListFunctionsResponse -> TestTree
testListFunctionsResponse = res
    "ListFunctionsResponse"
    "fixture/ListFunctionsResponse"
    (Proxy :: Proxy ListFunctions)

testUpdateFunctionCodeResponse :: FunctionConfiguration -> TestTree
testUpdateFunctionCodeResponse = res
    "UpdateFunctionCodeResponse"
    "fixture/UpdateFunctionCodeResponse"
    (Proxy :: Proxy UpdateFunctionCode)

testGetPolicyResponse :: GetPolicyResponse -> TestTree
testGetPolicyResponse = res
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
