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
--         , testDeleteEventSourceMapping $
--             deleteEventSourceMapping
--
--         , testUpdateEventSourceMapping $
--             updateEventSourceMapping
--
--         , testCreateAlias $
--             createAlias
--
--         , testListVersionsByFunction $
--             listVersionsByFunction
--
--         , testListAliases $
--             listAliases
--
--         , testRemovePermission $
--             removePermission
--
--         , testInvoke $
--             invoke
--
--         , testGetAlias $
--             getAlias
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
--         , testDeleteAlias $
--             deleteAlias
--
--         , testUpdateAlias $
--             updateAlias
--
--         , testAddPermission $
--             addPermission
--
--         , testPublishVersion $
--             publishVersion
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
--         , testDeleteEventSourceMappingResponse $
--             eventSourceMappingConfiguration
--
--         , testUpdateEventSourceMappingResponse $
--             eventSourceMappingConfiguration
--
--         , testCreateAliasResponse $
--             aliasConfiguration
--
--         , testListVersionsByFunctionResponse $
--             listVersionsByFunctionResponse
--
--         , testListAliasesResponse $
--             listAliasesResponse
--
--         , testRemovePermissionResponse $
--             removePermissionResponse
--
--         , testInvokeResponse $
--             invokeResponse
--
--         , testGetAliasResponse $
--             aliasConfiguration
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
--         , testDeleteAliasResponse $
--             deleteAliasResponse
--
--         , testUpdateAliasResponse $
--             aliasConfiguration
--
--         , testAddPermissionResponse $
--             addPermissionResponse
--
--         , testPublishVersionResponse $
--             functionConfiguration
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

testDeleteEventSourceMapping :: DeleteEventSourceMapping -> TestTree
testDeleteEventSourceMapping = req
    "DeleteEventSourceMapping"
    "fixture/DeleteEventSourceMapping.yaml"

testUpdateEventSourceMapping :: UpdateEventSourceMapping -> TestTree
testUpdateEventSourceMapping = req
    "UpdateEventSourceMapping"
    "fixture/UpdateEventSourceMapping.yaml"

testCreateAlias :: CreateAlias -> TestTree
testCreateAlias = req
    "CreateAlias"
    "fixture/CreateAlias.yaml"

testListVersionsByFunction :: ListVersionsByFunction -> TestTree
testListVersionsByFunction = req
    "ListVersionsByFunction"
    "fixture/ListVersionsByFunction.yaml"

testListAliases :: ListAliases -> TestTree
testListAliases = req
    "ListAliases"
    "fixture/ListAliases.yaml"

testRemovePermission :: RemovePermission -> TestTree
testRemovePermission = req
    "RemovePermission"
    "fixture/RemovePermission.yaml"

testInvoke :: Invoke -> TestTree
testInvoke = req
    "Invoke"
    "fixture/Invoke.yaml"

testGetAlias :: GetAlias -> TestTree
testGetAlias = req
    "GetAlias"
    "fixture/GetAlias.yaml"

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

testDeleteAlias :: DeleteAlias -> TestTree
testDeleteAlias = req
    "DeleteAlias"
    "fixture/DeleteAlias.yaml"

testUpdateAlias :: UpdateAlias -> TestTree
testUpdateAlias = req
    "UpdateAlias"
    "fixture/UpdateAlias.yaml"

testAddPermission :: AddPermission -> TestTree
testAddPermission = req
    "AddPermission"
    "fixture/AddPermission.yaml"

testPublishVersion :: PublishVersion -> TestTree
testPublishVersion = req
    "PublishVersion"
    "fixture/PublishVersion.yaml"

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

testDeleteEventSourceMappingResponse :: EventSourceMappingConfiguration -> TestTree
testDeleteEventSourceMappingResponse = res
    "DeleteEventSourceMappingResponse"
    "fixture/DeleteEventSourceMappingResponse.proto"
    lambda
    (Proxy :: Proxy DeleteEventSourceMapping)

testUpdateEventSourceMappingResponse :: EventSourceMappingConfiguration -> TestTree
testUpdateEventSourceMappingResponse = res
    "UpdateEventSourceMappingResponse"
    "fixture/UpdateEventSourceMappingResponse.proto"
    lambda
    (Proxy :: Proxy UpdateEventSourceMapping)

testCreateAliasResponse :: AliasConfiguration -> TestTree
testCreateAliasResponse = res
    "CreateAliasResponse"
    "fixture/CreateAliasResponse.proto"
    lambda
    (Proxy :: Proxy CreateAlias)

testListVersionsByFunctionResponse :: ListVersionsByFunctionResponse -> TestTree
testListVersionsByFunctionResponse = res
    "ListVersionsByFunctionResponse"
    "fixture/ListVersionsByFunctionResponse.proto"
    lambda
    (Proxy :: Proxy ListVersionsByFunction)

testListAliasesResponse :: ListAliasesResponse -> TestTree
testListAliasesResponse = res
    "ListAliasesResponse"
    "fixture/ListAliasesResponse.proto"
    lambda
    (Proxy :: Proxy ListAliases)

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

testGetAliasResponse :: AliasConfiguration -> TestTree
testGetAliasResponse = res
    "GetAliasResponse"
    "fixture/GetAliasResponse.proto"
    lambda
    (Proxy :: Proxy GetAlias)

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

testDeleteAliasResponse :: DeleteAliasResponse -> TestTree
testDeleteAliasResponse = res
    "DeleteAliasResponse"
    "fixture/DeleteAliasResponse.proto"
    lambda
    (Proxy :: Proxy DeleteAlias)

testUpdateAliasResponse :: AliasConfiguration -> TestTree
testUpdateAliasResponse = res
    "UpdateAliasResponse"
    "fixture/UpdateAliasResponse.proto"
    lambda
    (Proxy :: Proxy UpdateAlias)

testAddPermissionResponse :: AddPermissionResponse -> TestTree
testAddPermissionResponse = res
    "AddPermissionResponse"
    "fixture/AddPermissionResponse.proto"
    lambda
    (Proxy :: Proxy AddPermission)

testPublishVersionResponse :: FunctionConfiguration -> TestTree
testPublishVersionResponse = res
    "PublishVersionResponse"
    "fixture/PublishVersionResponse.proto"
    lambda
    (Proxy :: Proxy PublishVersion)

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
