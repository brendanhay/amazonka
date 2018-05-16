{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.Lambda
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Test.AWS.Gen.Lambda where

import Data.Proxy
import Network.AWS.Lambda
import Test.AWS.Fixture
import Test.AWS.Lambda.Internal
import Test.AWS.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestGetFunctionConfiguration $
--             getFunctionConfiguration
--
--         , requestDeleteEventSourceMapping $
--             deleteEventSourceMapping
--
--         , requestUpdateEventSourceMapping $
--             updateEventSourceMapping
--
--         , requestCreateAlias $
--             createAlias
--
--         , requestListVersionsByFunction $
--             listVersionsByFunction
--
--         , requestListAliases $
--             listAliases
--
--         , requestRemovePermission $
--             removePermission
--
--         , requestInvoke $
--             invoke
--
--         , requestGetAlias $
--             getAlias
--
--         , requestGetEventSourceMapping $
--             getEventSourceMapping
--
--         , requestPutFunctionConcurrency $
--             putFunctionConcurrency
--
--         , requestCreateFunction $
--             createFunction
--
--         , requestDeleteFunctionConcurrency $
--             deleteFunctionConcurrency
--
--         , requestCreateEventSourceMapping $
--             createEventSourceMapping
--
--         , requestGetFunction $
--             getFunction
--
--         , requestListEventSourceMappings $
--             listEventSourceMappings
--
--         , requestDeleteAlias $
--             deleteAlias
--
--         , requestUpdateAlias $
--             updateAlias
--
--         , requestGetAccountSettings $
--             getAccountSettings
--
--         , requestAddPermission $
--             addPermission
--
--         , requestTagResource $
--             tagResource
--
--         , requestPublishVersion $
--             publishVersion
--
--         , requestListTags $
--             listTags
--
--         , requestDeleteFunction $
--             deleteFunction
--
--         , requestUntagResource $
--             untagResource
--
--         , requestUpdateFunctionConfiguration $
--             updateFunctionConfiguration
--
--         , requestListFunctions $
--             listFunctions
--
--         , requestUpdateFunctionCode $
--             updateFunctionCode
--
--         , requestGetPolicy $
--             getPolicy
--
--           ]

--     , testGroup "response"
--         [ responseGetFunctionConfiguration $
--             functionConfiguration
--
--         , responseDeleteEventSourceMapping $
--             eventSourceMappingConfiguration
--
--         , responseUpdateEventSourceMapping $
--             eventSourceMappingConfiguration
--
--         , responseCreateAlias $
--             aliasConfiguration
--
--         , responseListVersionsByFunction $
--             listVersionsByFunctionResponse
--
--         , responseListAliases $
--             listAliasesResponse
--
--         , responseRemovePermission $
--             removePermissionResponse
--
--         , responseInvoke $
--             invokeResponse
--
--         , responseGetAlias $
--             aliasConfiguration
--
--         , responseGetEventSourceMapping $
--             eventSourceMappingConfiguration
--
--         , responsePutFunctionConcurrency $
--             concurrency
--
--         , responseCreateFunction $
--             functionConfiguration
--
--         , responseDeleteFunctionConcurrency $
--             deleteFunctionConcurrencyResponse
--
--         , responseCreateEventSourceMapping $
--             eventSourceMappingConfiguration
--
--         , responseGetFunction $
--             getFunctionResponse
--
--         , responseListEventSourceMappings $
--             listEventSourceMappingsResponse
--
--         , responseDeleteAlias $
--             deleteAliasResponse
--
--         , responseUpdateAlias $
--             aliasConfiguration
--
--         , responseGetAccountSettings $
--             getAccountSettingsResponse
--
--         , responseAddPermission $
--             addPermissionResponse
--
--         , responseTagResource $
--             tagResourceResponse
--
--         , responsePublishVersion $
--             functionConfiguration
--
--         , responseListTags $
--             listTagsResponse
--
--         , responseDeleteFunction $
--             deleteFunctionResponse
--
--         , responseUntagResource $
--             untagResourceResponse
--
--         , responseUpdateFunctionConfiguration $
--             functionConfiguration
--
--         , responseListFunctions $
--             listFunctionsResponse
--
--         , responseUpdateFunctionCode $
--             functionConfiguration
--
--         , responseGetPolicy $
--             getPolicyResponse
--
--           ]
--     ]

-- Requests

requestGetFunctionConfiguration :: GetFunctionConfiguration -> TestTree
requestGetFunctionConfiguration = req
    "GetFunctionConfiguration"
    "fixture/GetFunctionConfiguration.yaml"

requestDeleteEventSourceMapping :: DeleteEventSourceMapping -> TestTree
requestDeleteEventSourceMapping = req
    "DeleteEventSourceMapping"
    "fixture/DeleteEventSourceMapping.yaml"

requestUpdateEventSourceMapping :: UpdateEventSourceMapping -> TestTree
requestUpdateEventSourceMapping = req
    "UpdateEventSourceMapping"
    "fixture/UpdateEventSourceMapping.yaml"

requestCreateAlias :: CreateAlias -> TestTree
requestCreateAlias = req
    "CreateAlias"
    "fixture/CreateAlias.yaml"

requestListVersionsByFunction :: ListVersionsByFunction -> TestTree
requestListVersionsByFunction = req
    "ListVersionsByFunction"
    "fixture/ListVersionsByFunction.yaml"

requestListAliases :: ListAliases -> TestTree
requestListAliases = req
    "ListAliases"
    "fixture/ListAliases.yaml"

requestRemovePermission :: RemovePermission -> TestTree
requestRemovePermission = req
    "RemovePermission"
    "fixture/RemovePermission.yaml"

requestInvoke :: Invoke -> TestTree
requestInvoke = req
    "Invoke"
    "fixture/Invoke.yaml"

requestGetAlias :: GetAlias -> TestTree
requestGetAlias = req
    "GetAlias"
    "fixture/GetAlias.yaml"

requestGetEventSourceMapping :: GetEventSourceMapping -> TestTree
requestGetEventSourceMapping = req
    "GetEventSourceMapping"
    "fixture/GetEventSourceMapping.yaml"

requestPutFunctionConcurrency :: PutFunctionConcurrency -> TestTree
requestPutFunctionConcurrency = req
    "PutFunctionConcurrency"
    "fixture/PutFunctionConcurrency.yaml"

requestCreateFunction :: CreateFunction -> TestTree
requestCreateFunction = req
    "CreateFunction"
    "fixture/CreateFunction.yaml"

requestDeleteFunctionConcurrency :: DeleteFunctionConcurrency -> TestTree
requestDeleteFunctionConcurrency = req
    "DeleteFunctionConcurrency"
    "fixture/DeleteFunctionConcurrency.yaml"

requestCreateEventSourceMapping :: CreateEventSourceMapping -> TestTree
requestCreateEventSourceMapping = req
    "CreateEventSourceMapping"
    "fixture/CreateEventSourceMapping.yaml"

requestGetFunction :: GetFunction -> TestTree
requestGetFunction = req
    "GetFunction"
    "fixture/GetFunction.yaml"

requestListEventSourceMappings :: ListEventSourceMappings -> TestTree
requestListEventSourceMappings = req
    "ListEventSourceMappings"
    "fixture/ListEventSourceMappings.yaml"

requestDeleteAlias :: DeleteAlias -> TestTree
requestDeleteAlias = req
    "DeleteAlias"
    "fixture/DeleteAlias.yaml"

requestUpdateAlias :: UpdateAlias -> TestTree
requestUpdateAlias = req
    "UpdateAlias"
    "fixture/UpdateAlias.yaml"

requestGetAccountSettings :: GetAccountSettings -> TestTree
requestGetAccountSettings = req
    "GetAccountSettings"
    "fixture/GetAccountSettings.yaml"

requestAddPermission :: AddPermission -> TestTree
requestAddPermission = req
    "AddPermission"
    "fixture/AddPermission.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource = req
    "TagResource"
    "fixture/TagResource.yaml"

requestPublishVersion :: PublishVersion -> TestTree
requestPublishVersion = req
    "PublishVersion"
    "fixture/PublishVersion.yaml"

requestListTags :: ListTags -> TestTree
requestListTags = req
    "ListTags"
    "fixture/ListTags.yaml"

requestDeleteFunction :: DeleteFunction -> TestTree
requestDeleteFunction = req
    "DeleteFunction"
    "fixture/DeleteFunction.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource = req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestUpdateFunctionConfiguration :: UpdateFunctionConfiguration -> TestTree
requestUpdateFunctionConfiguration = req
    "UpdateFunctionConfiguration"
    "fixture/UpdateFunctionConfiguration.yaml"

requestListFunctions :: ListFunctions -> TestTree
requestListFunctions = req
    "ListFunctions"
    "fixture/ListFunctions.yaml"

requestUpdateFunctionCode :: UpdateFunctionCode -> TestTree
requestUpdateFunctionCode = req
    "UpdateFunctionCode"
    "fixture/UpdateFunctionCode.yaml"

requestGetPolicy :: GetPolicy -> TestTree
requestGetPolicy = req
    "GetPolicy"
    "fixture/GetPolicy.yaml"

-- Responses

responseGetFunctionConfiguration :: FunctionConfiguration -> TestTree
responseGetFunctionConfiguration = res
    "GetFunctionConfigurationResponse"
    "fixture/GetFunctionConfigurationResponse.proto"
    lambda
    (Proxy :: Proxy GetFunctionConfiguration)

responseDeleteEventSourceMapping :: EventSourceMappingConfiguration -> TestTree
responseDeleteEventSourceMapping = res
    "DeleteEventSourceMappingResponse"
    "fixture/DeleteEventSourceMappingResponse.proto"
    lambda
    (Proxy :: Proxy DeleteEventSourceMapping)

responseUpdateEventSourceMapping :: EventSourceMappingConfiguration -> TestTree
responseUpdateEventSourceMapping = res
    "UpdateEventSourceMappingResponse"
    "fixture/UpdateEventSourceMappingResponse.proto"
    lambda
    (Proxy :: Proxy UpdateEventSourceMapping)

responseCreateAlias :: AliasConfiguration -> TestTree
responseCreateAlias = res
    "CreateAliasResponse"
    "fixture/CreateAliasResponse.proto"
    lambda
    (Proxy :: Proxy CreateAlias)

responseListVersionsByFunction :: ListVersionsByFunctionResponse -> TestTree
responseListVersionsByFunction = res
    "ListVersionsByFunctionResponse"
    "fixture/ListVersionsByFunctionResponse.proto"
    lambda
    (Proxy :: Proxy ListVersionsByFunction)

responseListAliases :: ListAliasesResponse -> TestTree
responseListAliases = res
    "ListAliasesResponse"
    "fixture/ListAliasesResponse.proto"
    lambda
    (Proxy :: Proxy ListAliases)

responseRemovePermission :: RemovePermissionResponse -> TestTree
responseRemovePermission = res
    "RemovePermissionResponse"
    "fixture/RemovePermissionResponse.proto"
    lambda
    (Proxy :: Proxy RemovePermission)

responseInvoke :: InvokeResponse -> TestTree
responseInvoke = res
    "InvokeResponse"
    "fixture/InvokeResponse.proto"
    lambda
    (Proxy :: Proxy Invoke)

responseGetAlias :: AliasConfiguration -> TestTree
responseGetAlias = res
    "GetAliasResponse"
    "fixture/GetAliasResponse.proto"
    lambda
    (Proxy :: Proxy GetAlias)

responseGetEventSourceMapping :: EventSourceMappingConfiguration -> TestTree
responseGetEventSourceMapping = res
    "GetEventSourceMappingResponse"
    "fixture/GetEventSourceMappingResponse.proto"
    lambda
    (Proxy :: Proxy GetEventSourceMapping)

responsePutFunctionConcurrency :: Concurrency -> TestTree
responsePutFunctionConcurrency = res
    "PutFunctionConcurrencyResponse"
    "fixture/PutFunctionConcurrencyResponse.proto"
    lambda
    (Proxy :: Proxy PutFunctionConcurrency)

responseCreateFunction :: FunctionConfiguration -> TestTree
responseCreateFunction = res
    "CreateFunctionResponse"
    "fixture/CreateFunctionResponse.proto"
    lambda
    (Proxy :: Proxy CreateFunction)

responseDeleteFunctionConcurrency :: DeleteFunctionConcurrencyResponse -> TestTree
responseDeleteFunctionConcurrency = res
    "DeleteFunctionConcurrencyResponse"
    "fixture/DeleteFunctionConcurrencyResponse.proto"
    lambda
    (Proxy :: Proxy DeleteFunctionConcurrency)

responseCreateEventSourceMapping :: EventSourceMappingConfiguration -> TestTree
responseCreateEventSourceMapping = res
    "CreateEventSourceMappingResponse"
    "fixture/CreateEventSourceMappingResponse.proto"
    lambda
    (Proxy :: Proxy CreateEventSourceMapping)

responseGetFunction :: GetFunctionResponse -> TestTree
responseGetFunction = res
    "GetFunctionResponse"
    "fixture/GetFunctionResponse.proto"
    lambda
    (Proxy :: Proxy GetFunction)

responseListEventSourceMappings :: ListEventSourceMappingsResponse -> TestTree
responseListEventSourceMappings = res
    "ListEventSourceMappingsResponse"
    "fixture/ListEventSourceMappingsResponse.proto"
    lambda
    (Proxy :: Proxy ListEventSourceMappings)

responseDeleteAlias :: DeleteAliasResponse -> TestTree
responseDeleteAlias = res
    "DeleteAliasResponse"
    "fixture/DeleteAliasResponse.proto"
    lambda
    (Proxy :: Proxy DeleteAlias)

responseUpdateAlias :: AliasConfiguration -> TestTree
responseUpdateAlias = res
    "UpdateAliasResponse"
    "fixture/UpdateAliasResponse.proto"
    lambda
    (Proxy :: Proxy UpdateAlias)

responseGetAccountSettings :: GetAccountSettingsResponse -> TestTree
responseGetAccountSettings = res
    "GetAccountSettingsResponse"
    "fixture/GetAccountSettingsResponse.proto"
    lambda
    (Proxy :: Proxy GetAccountSettings)

responseAddPermission :: AddPermissionResponse -> TestTree
responseAddPermission = res
    "AddPermissionResponse"
    "fixture/AddPermissionResponse.proto"
    lambda
    (Proxy :: Proxy AddPermission)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource = res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    lambda
    (Proxy :: Proxy TagResource)

responsePublishVersion :: FunctionConfiguration -> TestTree
responsePublishVersion = res
    "PublishVersionResponse"
    "fixture/PublishVersionResponse.proto"
    lambda
    (Proxy :: Proxy PublishVersion)

responseListTags :: ListTagsResponse -> TestTree
responseListTags = res
    "ListTagsResponse"
    "fixture/ListTagsResponse.proto"
    lambda
    (Proxy :: Proxy ListTags)

responseDeleteFunction :: DeleteFunctionResponse -> TestTree
responseDeleteFunction = res
    "DeleteFunctionResponse"
    "fixture/DeleteFunctionResponse.proto"
    lambda
    (Proxy :: Proxy DeleteFunction)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource = res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    lambda
    (Proxy :: Proxy UntagResource)

responseUpdateFunctionConfiguration :: FunctionConfiguration -> TestTree
responseUpdateFunctionConfiguration = res
    "UpdateFunctionConfigurationResponse"
    "fixture/UpdateFunctionConfigurationResponse.proto"
    lambda
    (Proxy :: Proxy UpdateFunctionConfiguration)

responseListFunctions :: ListFunctionsResponse -> TestTree
responseListFunctions = res
    "ListFunctionsResponse"
    "fixture/ListFunctionsResponse.proto"
    lambda
    (Proxy :: Proxy ListFunctions)

responseUpdateFunctionCode :: FunctionConfiguration -> TestTree
responseUpdateFunctionCode = res
    "UpdateFunctionCodeResponse"
    "fixture/UpdateFunctionCodeResponse.proto"
    lambda
    (Proxy :: Proxy UpdateFunctionCode)

responseGetPolicy :: GetPolicyResponse -> TestTree
responseGetPolicy = res
    "GetPolicyResponse"
    "fixture/GetPolicyResponse.proto"
    lambda
    (Proxy :: Proxy GetPolicy)
