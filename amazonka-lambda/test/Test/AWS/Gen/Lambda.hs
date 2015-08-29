{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.Lambda
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
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
    "fixture/GetFunctionConfiguration.yaml"

testUpdateEventSourceMapping :: UpdateEventSourceMapping -> TestTree
testUpdateEventSourceMapping = req
    "UpdateEventSourceMapping"
    "fixture/UpdateEventSourceMapping.yaml"

testDeleteEventSourceMapping :: DeleteEventSourceMapping -> TestTree
testDeleteEventSourceMapping = req
    "DeleteEventSourceMapping"
    "fixture/DeleteEventSourceMapping.yaml"

testRemovePermission :: RemovePermission -> TestTree
testRemovePermission = req
    "RemovePermission"
    "fixture/RemovePermission.yaml"

testInvoke :: Invoke -> TestTree
testInvoke = req
    "Invoke"
    "fixture/Invoke.yaml"

testGetEventSourceMapping :: GetEventSourceMapping -> TestTree
testGetEventSourceMapping = req
    "GetEventSourceMapping"
    "fixture/GetEventSourceMapping.yaml"

testCreateFunction :: CreateFunction -> TestTree
testCreateFunction = req
    "CreateFunction"
    "fixture/CreateFunction.yaml"

testCreateEventSourceMapping :: CreateEventSourceMapping -> TestTree
testCreateEventSourceMapping = req
    "CreateEventSourceMapping"
    "fixture/CreateEventSourceMapping.yaml"

testGetFunction :: GetFunction -> TestTree
testGetFunction = req
    "GetFunction"
    "fixture/GetFunction.yaml"

testListEventSourceMappings :: ListEventSourceMappings -> TestTree
testListEventSourceMappings = req
    "ListEventSourceMappings"
    "fixture/ListEventSourceMappings.yaml"

testAddPermission :: AddPermission -> TestTree
testAddPermission = req
    "AddPermission"
    "fixture/AddPermission.yaml"

testDeleteFunction :: DeleteFunction -> TestTree
testDeleteFunction = req
    "DeleteFunction"
    "fixture/DeleteFunction.yaml"

testUpdateFunctionConfiguration :: UpdateFunctionConfiguration -> TestTree
testUpdateFunctionConfiguration = req
    "UpdateFunctionConfiguration"
    "fixture/UpdateFunctionConfiguration.yaml"

testListFunctions :: ListFunctions -> TestTree
testListFunctions = req
    "ListFunctions"
    "fixture/ListFunctions.yaml"

testUpdateFunctionCode :: UpdateFunctionCode -> TestTree
testUpdateFunctionCode = req
    "UpdateFunctionCode"
    "fixture/UpdateFunctionCode.yaml"

testGetPolicy :: GetPolicy -> TestTree
testGetPolicy = req
    "GetPolicy"
    "fixture/GetPolicy.yaml"

-- Responses

testGetFunctionConfigurationResponse :: FunctionConfiguration -> TestTree
testGetFunctionConfigurationResponse = res
    "GetFunctionConfigurationResponse"
    "fixture/GetFunctionConfigurationResponse.proto"
    lambda
    (Proxy :: Proxy GetFunctionConfiguration)

testUpdateEventSourceMappingResponse :: EventSourceMappingConfiguration -> TestTree
testUpdateEventSourceMappingResponse = res
    "UpdateEventSourceMappingResponse"
    "fixture/UpdateEventSourceMappingResponse.proto"
    lambda
    (Proxy :: Proxy UpdateEventSourceMapping)

testDeleteEventSourceMappingResponse :: EventSourceMappingConfiguration -> TestTree
testDeleteEventSourceMappingResponse = res
    "DeleteEventSourceMappingResponse"
    "fixture/DeleteEventSourceMappingResponse.proto"
    lambda
    (Proxy :: Proxy DeleteEventSourceMapping)

testRemovePermissionResponse :: RemovePermissionResponse -> TestTree
testRemovePermissionResponse = res
    "RemovePermissionResponse"
    "fixture/RemovePermissionResponse.proto"
    lambda
    (Proxy :: Proxy RemovePermission)

testInvokeResponse :: InvokeResponse -> TestTree
testInvokeResponse = res
    "InvokeResponse"
    "fixture/InvokeResponse.proto"
    lambda
    (Proxy :: Proxy Invoke)

testGetEventSourceMappingResponse :: EventSourceMappingConfiguration -> TestTree
testGetEventSourceMappingResponse = res
    "GetEventSourceMappingResponse"
    "fixture/GetEventSourceMappingResponse.proto"
    lambda
    (Proxy :: Proxy GetEventSourceMapping)

testCreateFunctionResponse :: FunctionConfiguration -> TestTree
testCreateFunctionResponse = res
    "CreateFunctionResponse"
    "fixture/CreateFunctionResponse.proto"
    lambda
    (Proxy :: Proxy CreateFunction)

testCreateEventSourceMappingResponse :: EventSourceMappingConfiguration -> TestTree
testCreateEventSourceMappingResponse = res
    "CreateEventSourceMappingResponse"
    "fixture/CreateEventSourceMappingResponse.proto"
    lambda
    (Proxy :: Proxy CreateEventSourceMapping)

testGetFunctionResponse :: GetFunctionResponse -> TestTree
testGetFunctionResponse = res
    "GetFunctionResponse"
    "fixture/GetFunctionResponse.proto"
    lambda
    (Proxy :: Proxy GetFunction)

testListEventSourceMappingsResponse :: ListEventSourceMappingsResponse -> TestTree
testListEventSourceMappingsResponse = res
    "ListEventSourceMappingsResponse"
    "fixture/ListEventSourceMappingsResponse.proto"
    lambda
    (Proxy :: Proxy ListEventSourceMappings)

testAddPermissionResponse :: AddPermissionResponse -> TestTree
testAddPermissionResponse = res
    "AddPermissionResponse"
    "fixture/AddPermissionResponse.proto"
    lambda
    (Proxy :: Proxy AddPermission)

testDeleteFunctionResponse :: DeleteFunctionResponse -> TestTree
testDeleteFunctionResponse = res
    "DeleteFunctionResponse"
    "fixture/DeleteFunctionResponse.proto"
    lambda
    (Proxy :: Proxy DeleteFunction)

testUpdateFunctionConfigurationResponse :: FunctionConfiguration -> TestTree
testUpdateFunctionConfigurationResponse = res
    "UpdateFunctionConfigurationResponse"
    "fixture/UpdateFunctionConfigurationResponse.proto"
    lambda
    (Proxy :: Proxy UpdateFunctionConfiguration)

testListFunctionsResponse :: ListFunctionsResponse -> TestTree
testListFunctionsResponse = res
    "ListFunctionsResponse"
    "fixture/ListFunctionsResponse.proto"
    lambda
    (Proxy :: Proxy ListFunctions)

testUpdateFunctionCodeResponse :: FunctionConfiguration -> TestTree
testUpdateFunctionCodeResponse = res
    "UpdateFunctionCodeResponse"
    "fixture/UpdateFunctionCodeResponse.proto"
    lambda
    (Proxy :: Proxy UpdateFunctionCode)

testGetPolicyResponse :: GetPolicyResponse -> TestTree
testGetPolicyResponse = res
    "GetPolicyResponse"
    "fixture/GetPolicyResponse.proto"
    lambda
    (Proxy :: Proxy GetPolicy)
