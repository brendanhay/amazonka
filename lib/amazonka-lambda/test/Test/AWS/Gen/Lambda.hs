{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.Lambda
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
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
--         , requestGetLayerVersion $
--             getLayerVersion
--
--         , requestDeleteFunctionCodeSigningConfig $
--             deleteFunctionCodeSigningConfig
--
--         , requestPutFunctionCodeSigningConfig $
--             putFunctionCodeSigningConfig
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
--         , requestDeleteCodeSigningConfig $
--             deleteCodeSigningConfig
--
--         , requestUpdateCodeSigningConfig $
--             updateCodeSigningConfig
--
--         , requestRemovePermission $
--             removePermission
--
--         , requestDeleteFunctionEventInvokeConfig $
--             deleteFunctionEventInvokeConfig
--
--         , requestUpdateFunctionEventInvokeConfig $
--             updateFunctionEventInvokeConfig
--
--         , requestPutFunctionEventInvokeConfig $
--             putFunctionEventInvokeConfig
--
--         , requestInvoke $
--             invoke
--
--         , requestDeleteLayerVersion $
--             deleteLayerVersion
--
--         , requestGetAlias $
--             getAlias
--
--         , requestPublishLayerVersion $
--             publishLayerVersion
--
--         , requestGetEventSourceMapping $
--             getEventSourceMapping
--
--         , requestAddLayerVersionPermission $
--             addLayerVersionPermission
--
--         , requestListProvisionedConcurrencyConfigs $
--             listProvisionedConcurrencyConfigs
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
--         , requestGetLayerVersionByARN $
--             getLayerVersionByARN
--
--         , requestGetFunctionConcurrency $
--             getFunctionConcurrency
--
--         , requestCreateEventSourceMapping $
--             createEventSourceMapping
--
--         , requestGetProvisionedConcurrencyConfig $
--             getProvisionedConcurrencyConfig
--
--         , requestRemoveLayerVersionPermission $
--             removeLayerVersionPermission
--
--         , requestListFunctionsByCodeSigningConfig $
--             listFunctionsByCodeSigningConfig
--
--         , requestGetFunction $
--             getFunction
--
--         , requestListEventSourceMappings $
--             listEventSourceMappings
--
--         , requestGetLayerVersionPolicy $
--             getLayerVersionPolicy
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
--         , requestGetFunctionEventInvokeConfig $
--             getFunctionEventInvokeConfig
--
--         , requestGetCodeSigningConfig $
--             getCodeSigningConfig
--
--         , requestAddPermission $
--             addPermission
--
--         , requestListLayers $
--             listLayers
--
--         , requestListFunctionEventInvokeConfigs $
--             listFunctionEventInvokeConfigs
--
--         , requestListCodeSigningConfigs $
--             listCodeSigningConfigs
--
--         , requestGetFunctionCodeSigningConfig $
--             getFunctionCodeSigningConfig
--
--         , requestCreateCodeSigningConfig $
--             createCodeSigningConfig
--
--         , requestListLayerVersions $
--             listLayerVersions
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
--         , requestDeleteProvisionedConcurrencyConfig $
--             deleteProvisionedConcurrencyConfig
--
--         , requestGetPolicy $
--             getPolicy
--
--         , requestPutProvisionedConcurrencyConfig $
--             putProvisionedConcurrencyConfig
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
--         , responseGetLayerVersion $
--             getLayerVersionResponse
--
--         , responseDeleteFunctionCodeSigningConfig $
--             deleteFunctionCodeSigningConfigResponse
--
--         , responsePutFunctionCodeSigningConfig $
--             putFunctionCodeSigningConfigResponse
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
--         , responseDeleteCodeSigningConfig $
--             deleteCodeSigningConfigResponse
--
--         , responseUpdateCodeSigningConfig $
--             updateCodeSigningConfigResponse
--
--         , responseRemovePermission $
--             removePermissionResponse
--
--         , responseDeleteFunctionEventInvokeConfig $
--             deleteFunctionEventInvokeConfigResponse
--
--         , responseUpdateFunctionEventInvokeConfig $
--             functionEventInvokeConfig
--
--         , responsePutFunctionEventInvokeConfig $
--             functionEventInvokeConfig
--
--         , responseInvoke $
--             invokeResponse
--
--         , responseDeleteLayerVersion $
--             deleteLayerVersionResponse
--
--         , responseGetAlias $
--             aliasConfiguration
--
--         , responsePublishLayerVersion $
--             publishLayerVersionResponse
--
--         , responseGetEventSourceMapping $
--             eventSourceMappingConfiguration
--
--         , responseAddLayerVersionPermission $
--             addLayerVersionPermissionResponse
--
--         , responseListProvisionedConcurrencyConfigs $
--             listProvisionedConcurrencyConfigsResponse
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
--         , responseGetLayerVersionByARN $
--             getLayerVersionResponse
--
--         , responseGetFunctionConcurrency $
--             getFunctionConcurrencyResponse
--
--         , responseCreateEventSourceMapping $
--             eventSourceMappingConfiguration
--
--         , responseGetProvisionedConcurrencyConfig $
--             getProvisionedConcurrencyConfigResponse
--
--         , responseRemoveLayerVersionPermission $
--             removeLayerVersionPermissionResponse
--
--         , responseListFunctionsByCodeSigningConfig $
--             listFunctionsByCodeSigningConfigResponse
--
--         , responseGetFunction $
--             getFunctionResponse
--
--         , responseListEventSourceMappings $
--             listEventSourceMappingsResponse
--
--         , responseGetLayerVersionPolicy $
--             getLayerVersionPolicyResponse
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
--         , responseGetFunctionEventInvokeConfig $
--             functionEventInvokeConfig
--
--         , responseGetCodeSigningConfig $
--             getCodeSigningConfigResponse
--
--         , responseAddPermission $
--             addPermissionResponse
--
--         , responseListLayers $
--             listLayersResponse
--
--         , responseListFunctionEventInvokeConfigs $
--             listFunctionEventInvokeConfigsResponse
--
--         , responseListCodeSigningConfigs $
--             listCodeSigningConfigsResponse
--
--         , responseGetFunctionCodeSigningConfig $
--             getFunctionCodeSigningConfigResponse
--
--         , responseCreateCodeSigningConfig $
--             createCodeSigningConfigResponse
--
--         , responseListLayerVersions $
--             listLayerVersionsResponse
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
--         , responseDeleteProvisionedConcurrencyConfig $
--             deleteProvisionedConcurrencyConfigResponse
--
--         , responseGetPolicy $
--             getPolicyResponse
--
--         , responsePutProvisionedConcurrencyConfig $
--             putProvisionedConcurrencyConfigResponse
--
--           ]
--     ]

-- Requests

requestGetFunctionConfiguration :: GetFunctionConfiguration -> TestTree
requestGetFunctionConfiguration =
  req
    "GetFunctionConfiguration"
    "fixture/GetFunctionConfiguration.yaml"

requestDeleteEventSourceMapping :: DeleteEventSourceMapping -> TestTree
requestDeleteEventSourceMapping =
  req
    "DeleteEventSourceMapping"
    "fixture/DeleteEventSourceMapping.yaml"

requestUpdateEventSourceMapping :: UpdateEventSourceMapping -> TestTree
requestUpdateEventSourceMapping =
  req
    "UpdateEventSourceMapping"
    "fixture/UpdateEventSourceMapping.yaml"

requestGetLayerVersion :: GetLayerVersion -> TestTree
requestGetLayerVersion =
  req
    "GetLayerVersion"
    "fixture/GetLayerVersion.yaml"

requestDeleteFunctionCodeSigningConfig :: DeleteFunctionCodeSigningConfig -> TestTree
requestDeleteFunctionCodeSigningConfig =
  req
    "DeleteFunctionCodeSigningConfig"
    "fixture/DeleteFunctionCodeSigningConfig.yaml"

requestPutFunctionCodeSigningConfig :: PutFunctionCodeSigningConfig -> TestTree
requestPutFunctionCodeSigningConfig =
  req
    "PutFunctionCodeSigningConfig"
    "fixture/PutFunctionCodeSigningConfig.yaml"

requestCreateAlias :: CreateAlias -> TestTree
requestCreateAlias =
  req
    "CreateAlias"
    "fixture/CreateAlias.yaml"

requestListVersionsByFunction :: ListVersionsByFunction -> TestTree
requestListVersionsByFunction =
  req
    "ListVersionsByFunction"
    "fixture/ListVersionsByFunction.yaml"

requestListAliases :: ListAliases -> TestTree
requestListAliases =
  req
    "ListAliases"
    "fixture/ListAliases.yaml"

requestDeleteCodeSigningConfig :: DeleteCodeSigningConfig -> TestTree
requestDeleteCodeSigningConfig =
  req
    "DeleteCodeSigningConfig"
    "fixture/DeleteCodeSigningConfig.yaml"

requestUpdateCodeSigningConfig :: UpdateCodeSigningConfig -> TestTree
requestUpdateCodeSigningConfig =
  req
    "UpdateCodeSigningConfig"
    "fixture/UpdateCodeSigningConfig.yaml"

requestRemovePermission :: RemovePermission -> TestTree
requestRemovePermission =
  req
    "RemovePermission"
    "fixture/RemovePermission.yaml"

requestDeleteFunctionEventInvokeConfig :: DeleteFunctionEventInvokeConfig -> TestTree
requestDeleteFunctionEventInvokeConfig =
  req
    "DeleteFunctionEventInvokeConfig"
    "fixture/DeleteFunctionEventInvokeConfig.yaml"

requestUpdateFunctionEventInvokeConfig :: UpdateFunctionEventInvokeConfig -> TestTree
requestUpdateFunctionEventInvokeConfig =
  req
    "UpdateFunctionEventInvokeConfig"
    "fixture/UpdateFunctionEventInvokeConfig.yaml"

requestPutFunctionEventInvokeConfig :: PutFunctionEventInvokeConfig -> TestTree
requestPutFunctionEventInvokeConfig =
  req
    "PutFunctionEventInvokeConfig"
    "fixture/PutFunctionEventInvokeConfig.yaml"

requestInvoke :: Invoke -> TestTree
requestInvoke =
  req
    "Invoke"
    "fixture/Invoke.yaml"

requestDeleteLayerVersion :: DeleteLayerVersion -> TestTree
requestDeleteLayerVersion =
  req
    "DeleteLayerVersion"
    "fixture/DeleteLayerVersion.yaml"

requestGetAlias :: GetAlias -> TestTree
requestGetAlias =
  req
    "GetAlias"
    "fixture/GetAlias.yaml"

requestPublishLayerVersion :: PublishLayerVersion -> TestTree
requestPublishLayerVersion =
  req
    "PublishLayerVersion"
    "fixture/PublishLayerVersion.yaml"

requestGetEventSourceMapping :: GetEventSourceMapping -> TestTree
requestGetEventSourceMapping =
  req
    "GetEventSourceMapping"
    "fixture/GetEventSourceMapping.yaml"

requestAddLayerVersionPermission :: AddLayerVersionPermission -> TestTree
requestAddLayerVersionPermission =
  req
    "AddLayerVersionPermission"
    "fixture/AddLayerVersionPermission.yaml"

requestListProvisionedConcurrencyConfigs :: ListProvisionedConcurrencyConfigs -> TestTree
requestListProvisionedConcurrencyConfigs =
  req
    "ListProvisionedConcurrencyConfigs"
    "fixture/ListProvisionedConcurrencyConfigs.yaml"

requestPutFunctionConcurrency :: PutFunctionConcurrency -> TestTree
requestPutFunctionConcurrency =
  req
    "PutFunctionConcurrency"
    "fixture/PutFunctionConcurrency.yaml"

requestCreateFunction :: CreateFunction -> TestTree
requestCreateFunction =
  req
    "CreateFunction"
    "fixture/CreateFunction.yaml"

requestDeleteFunctionConcurrency :: DeleteFunctionConcurrency -> TestTree
requestDeleteFunctionConcurrency =
  req
    "DeleteFunctionConcurrency"
    "fixture/DeleteFunctionConcurrency.yaml"

requestGetLayerVersionByARN :: GetLayerVersionByARN -> TestTree
requestGetLayerVersionByARN =
  req
    "GetLayerVersionByARN"
    "fixture/GetLayerVersionByARN.yaml"

requestGetFunctionConcurrency :: GetFunctionConcurrency -> TestTree
requestGetFunctionConcurrency =
  req
    "GetFunctionConcurrency"
    "fixture/GetFunctionConcurrency.yaml"

requestCreateEventSourceMapping :: CreateEventSourceMapping -> TestTree
requestCreateEventSourceMapping =
  req
    "CreateEventSourceMapping"
    "fixture/CreateEventSourceMapping.yaml"

requestGetProvisionedConcurrencyConfig :: GetProvisionedConcurrencyConfig -> TestTree
requestGetProvisionedConcurrencyConfig =
  req
    "GetProvisionedConcurrencyConfig"
    "fixture/GetProvisionedConcurrencyConfig.yaml"

requestRemoveLayerVersionPermission :: RemoveLayerVersionPermission -> TestTree
requestRemoveLayerVersionPermission =
  req
    "RemoveLayerVersionPermission"
    "fixture/RemoveLayerVersionPermission.yaml"

requestListFunctionsByCodeSigningConfig :: ListFunctionsByCodeSigningConfig -> TestTree
requestListFunctionsByCodeSigningConfig =
  req
    "ListFunctionsByCodeSigningConfig"
    "fixture/ListFunctionsByCodeSigningConfig.yaml"

requestGetFunction :: GetFunction -> TestTree
requestGetFunction =
  req
    "GetFunction"
    "fixture/GetFunction.yaml"

requestListEventSourceMappings :: ListEventSourceMappings -> TestTree
requestListEventSourceMappings =
  req
    "ListEventSourceMappings"
    "fixture/ListEventSourceMappings.yaml"

requestGetLayerVersionPolicy :: GetLayerVersionPolicy -> TestTree
requestGetLayerVersionPolicy =
  req
    "GetLayerVersionPolicy"
    "fixture/GetLayerVersionPolicy.yaml"

requestDeleteAlias :: DeleteAlias -> TestTree
requestDeleteAlias =
  req
    "DeleteAlias"
    "fixture/DeleteAlias.yaml"

requestUpdateAlias :: UpdateAlias -> TestTree
requestUpdateAlias =
  req
    "UpdateAlias"
    "fixture/UpdateAlias.yaml"

requestGetAccountSettings :: GetAccountSettings -> TestTree
requestGetAccountSettings =
  req
    "GetAccountSettings"
    "fixture/GetAccountSettings.yaml"

requestGetFunctionEventInvokeConfig :: GetFunctionEventInvokeConfig -> TestTree
requestGetFunctionEventInvokeConfig =
  req
    "GetFunctionEventInvokeConfig"
    "fixture/GetFunctionEventInvokeConfig.yaml"

requestGetCodeSigningConfig :: GetCodeSigningConfig -> TestTree
requestGetCodeSigningConfig =
  req
    "GetCodeSigningConfig"
    "fixture/GetCodeSigningConfig.yaml"

requestAddPermission :: AddPermission -> TestTree
requestAddPermission =
  req
    "AddPermission"
    "fixture/AddPermission.yaml"

requestListLayers :: ListLayers -> TestTree
requestListLayers =
  req
    "ListLayers"
    "fixture/ListLayers.yaml"

requestListFunctionEventInvokeConfigs :: ListFunctionEventInvokeConfigs -> TestTree
requestListFunctionEventInvokeConfigs =
  req
    "ListFunctionEventInvokeConfigs"
    "fixture/ListFunctionEventInvokeConfigs.yaml"

requestListCodeSigningConfigs :: ListCodeSigningConfigs -> TestTree
requestListCodeSigningConfigs =
  req
    "ListCodeSigningConfigs"
    "fixture/ListCodeSigningConfigs.yaml"

requestGetFunctionCodeSigningConfig :: GetFunctionCodeSigningConfig -> TestTree
requestGetFunctionCodeSigningConfig =
  req
    "GetFunctionCodeSigningConfig"
    "fixture/GetFunctionCodeSigningConfig.yaml"

requestCreateCodeSigningConfig :: CreateCodeSigningConfig -> TestTree
requestCreateCodeSigningConfig =
  req
    "CreateCodeSigningConfig"
    "fixture/CreateCodeSigningConfig.yaml"

requestListLayerVersions :: ListLayerVersions -> TestTree
requestListLayerVersions =
  req
    "ListLayerVersions"
    "fixture/ListLayerVersions.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestPublishVersion :: PublishVersion -> TestTree
requestPublishVersion =
  req
    "PublishVersion"
    "fixture/PublishVersion.yaml"

requestListTags :: ListTags -> TestTree
requestListTags =
  req
    "ListTags"
    "fixture/ListTags.yaml"

requestDeleteFunction :: DeleteFunction -> TestTree
requestDeleteFunction =
  req
    "DeleteFunction"
    "fixture/DeleteFunction.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestUpdateFunctionConfiguration :: UpdateFunctionConfiguration -> TestTree
requestUpdateFunctionConfiguration =
  req
    "UpdateFunctionConfiguration"
    "fixture/UpdateFunctionConfiguration.yaml"

requestListFunctions :: ListFunctions -> TestTree
requestListFunctions =
  req
    "ListFunctions"
    "fixture/ListFunctions.yaml"

requestUpdateFunctionCode :: UpdateFunctionCode -> TestTree
requestUpdateFunctionCode =
  req
    "UpdateFunctionCode"
    "fixture/UpdateFunctionCode.yaml"

requestDeleteProvisionedConcurrencyConfig :: DeleteProvisionedConcurrencyConfig -> TestTree
requestDeleteProvisionedConcurrencyConfig =
  req
    "DeleteProvisionedConcurrencyConfig"
    "fixture/DeleteProvisionedConcurrencyConfig.yaml"

requestGetPolicy :: GetPolicy -> TestTree
requestGetPolicy =
  req
    "GetPolicy"
    "fixture/GetPolicy.yaml"

requestPutProvisionedConcurrencyConfig :: PutProvisionedConcurrencyConfig -> TestTree
requestPutProvisionedConcurrencyConfig =
  req
    "PutProvisionedConcurrencyConfig"
    "fixture/PutProvisionedConcurrencyConfig.yaml"

-- Responses

responseGetFunctionConfiguration :: FunctionConfiguration -> TestTree
responseGetFunctionConfiguration =
  res
    "GetFunctionConfigurationResponse"
    "fixture/GetFunctionConfigurationResponse.proto"
    lambda
    (Proxy :: Proxy GetFunctionConfiguration)

responseDeleteEventSourceMapping :: EventSourceMappingConfiguration -> TestTree
responseDeleteEventSourceMapping =
  res
    "DeleteEventSourceMappingResponse"
    "fixture/DeleteEventSourceMappingResponse.proto"
    lambda
    (Proxy :: Proxy DeleteEventSourceMapping)

responseUpdateEventSourceMapping :: EventSourceMappingConfiguration -> TestTree
responseUpdateEventSourceMapping =
  res
    "UpdateEventSourceMappingResponse"
    "fixture/UpdateEventSourceMappingResponse.proto"
    lambda
    (Proxy :: Proxy UpdateEventSourceMapping)

responseGetLayerVersion :: GetLayerVersionResponse -> TestTree
responseGetLayerVersion =
  res
    "GetLayerVersionResponse"
    "fixture/GetLayerVersionResponse.proto"
    lambda
    (Proxy :: Proxy GetLayerVersion)

responseDeleteFunctionCodeSigningConfig :: DeleteFunctionCodeSigningConfigResponse -> TestTree
responseDeleteFunctionCodeSigningConfig =
  res
    "DeleteFunctionCodeSigningConfigResponse"
    "fixture/DeleteFunctionCodeSigningConfigResponse.proto"
    lambda
    (Proxy :: Proxy DeleteFunctionCodeSigningConfig)

responsePutFunctionCodeSigningConfig :: PutFunctionCodeSigningConfigResponse -> TestTree
responsePutFunctionCodeSigningConfig =
  res
    "PutFunctionCodeSigningConfigResponse"
    "fixture/PutFunctionCodeSigningConfigResponse.proto"
    lambda
    (Proxy :: Proxy PutFunctionCodeSigningConfig)

responseCreateAlias :: AliasConfiguration -> TestTree
responseCreateAlias =
  res
    "CreateAliasResponse"
    "fixture/CreateAliasResponse.proto"
    lambda
    (Proxy :: Proxy CreateAlias)

responseListVersionsByFunction :: ListVersionsByFunctionResponse -> TestTree
responseListVersionsByFunction =
  res
    "ListVersionsByFunctionResponse"
    "fixture/ListVersionsByFunctionResponse.proto"
    lambda
    (Proxy :: Proxy ListVersionsByFunction)

responseListAliases :: ListAliasesResponse -> TestTree
responseListAliases =
  res
    "ListAliasesResponse"
    "fixture/ListAliasesResponse.proto"
    lambda
    (Proxy :: Proxy ListAliases)

responseDeleteCodeSigningConfig :: DeleteCodeSigningConfigResponse -> TestTree
responseDeleteCodeSigningConfig =
  res
    "DeleteCodeSigningConfigResponse"
    "fixture/DeleteCodeSigningConfigResponse.proto"
    lambda
    (Proxy :: Proxy DeleteCodeSigningConfig)

responseUpdateCodeSigningConfig :: UpdateCodeSigningConfigResponse -> TestTree
responseUpdateCodeSigningConfig =
  res
    "UpdateCodeSigningConfigResponse"
    "fixture/UpdateCodeSigningConfigResponse.proto"
    lambda
    (Proxy :: Proxy UpdateCodeSigningConfig)

responseRemovePermission :: RemovePermissionResponse -> TestTree
responseRemovePermission =
  res
    "RemovePermissionResponse"
    "fixture/RemovePermissionResponse.proto"
    lambda
    (Proxy :: Proxy RemovePermission)

responseDeleteFunctionEventInvokeConfig :: DeleteFunctionEventInvokeConfigResponse -> TestTree
responseDeleteFunctionEventInvokeConfig =
  res
    "DeleteFunctionEventInvokeConfigResponse"
    "fixture/DeleteFunctionEventInvokeConfigResponse.proto"
    lambda
    (Proxy :: Proxy DeleteFunctionEventInvokeConfig)

responseUpdateFunctionEventInvokeConfig :: FunctionEventInvokeConfig -> TestTree
responseUpdateFunctionEventInvokeConfig =
  res
    "UpdateFunctionEventInvokeConfigResponse"
    "fixture/UpdateFunctionEventInvokeConfigResponse.proto"
    lambda
    (Proxy :: Proxy UpdateFunctionEventInvokeConfig)

responsePutFunctionEventInvokeConfig :: FunctionEventInvokeConfig -> TestTree
responsePutFunctionEventInvokeConfig =
  res
    "PutFunctionEventInvokeConfigResponse"
    "fixture/PutFunctionEventInvokeConfigResponse.proto"
    lambda
    (Proxy :: Proxy PutFunctionEventInvokeConfig)

responseInvoke :: InvokeResponse -> TestTree
responseInvoke =
  res
    "InvokeResponse"
    "fixture/InvokeResponse.proto"
    lambda
    (Proxy :: Proxy Invoke)

responseDeleteLayerVersion :: DeleteLayerVersionResponse -> TestTree
responseDeleteLayerVersion =
  res
    "DeleteLayerVersionResponse"
    "fixture/DeleteLayerVersionResponse.proto"
    lambda
    (Proxy :: Proxy DeleteLayerVersion)

responseGetAlias :: AliasConfiguration -> TestTree
responseGetAlias =
  res
    "GetAliasResponse"
    "fixture/GetAliasResponse.proto"
    lambda
    (Proxy :: Proxy GetAlias)

responsePublishLayerVersion :: PublishLayerVersionResponse -> TestTree
responsePublishLayerVersion =
  res
    "PublishLayerVersionResponse"
    "fixture/PublishLayerVersionResponse.proto"
    lambda
    (Proxy :: Proxy PublishLayerVersion)

responseGetEventSourceMapping :: EventSourceMappingConfiguration -> TestTree
responseGetEventSourceMapping =
  res
    "GetEventSourceMappingResponse"
    "fixture/GetEventSourceMappingResponse.proto"
    lambda
    (Proxy :: Proxy GetEventSourceMapping)

responseAddLayerVersionPermission :: AddLayerVersionPermissionResponse -> TestTree
responseAddLayerVersionPermission =
  res
    "AddLayerVersionPermissionResponse"
    "fixture/AddLayerVersionPermissionResponse.proto"
    lambda
    (Proxy :: Proxy AddLayerVersionPermission)

responseListProvisionedConcurrencyConfigs :: ListProvisionedConcurrencyConfigsResponse -> TestTree
responseListProvisionedConcurrencyConfigs =
  res
    "ListProvisionedConcurrencyConfigsResponse"
    "fixture/ListProvisionedConcurrencyConfigsResponse.proto"
    lambda
    (Proxy :: Proxy ListProvisionedConcurrencyConfigs)

responsePutFunctionConcurrency :: Concurrency -> TestTree
responsePutFunctionConcurrency =
  res
    "PutFunctionConcurrencyResponse"
    "fixture/PutFunctionConcurrencyResponse.proto"
    lambda
    (Proxy :: Proxy PutFunctionConcurrency)

responseCreateFunction :: FunctionConfiguration -> TestTree
responseCreateFunction =
  res
    "CreateFunctionResponse"
    "fixture/CreateFunctionResponse.proto"
    lambda
    (Proxy :: Proxy CreateFunction)

responseDeleteFunctionConcurrency :: DeleteFunctionConcurrencyResponse -> TestTree
responseDeleteFunctionConcurrency =
  res
    "DeleteFunctionConcurrencyResponse"
    "fixture/DeleteFunctionConcurrencyResponse.proto"
    lambda
    (Proxy :: Proxy DeleteFunctionConcurrency)

responseGetLayerVersionByARN :: GetLayerVersionResponse -> TestTree
responseGetLayerVersionByARN =
  res
    "GetLayerVersionByARNResponse"
    "fixture/GetLayerVersionByARNResponse.proto"
    lambda
    (Proxy :: Proxy GetLayerVersionByARN)

responseGetFunctionConcurrency :: GetFunctionConcurrencyResponse -> TestTree
responseGetFunctionConcurrency =
  res
    "GetFunctionConcurrencyResponse"
    "fixture/GetFunctionConcurrencyResponse.proto"
    lambda
    (Proxy :: Proxy GetFunctionConcurrency)

responseCreateEventSourceMapping :: EventSourceMappingConfiguration -> TestTree
responseCreateEventSourceMapping =
  res
    "CreateEventSourceMappingResponse"
    "fixture/CreateEventSourceMappingResponse.proto"
    lambda
    (Proxy :: Proxy CreateEventSourceMapping)

responseGetProvisionedConcurrencyConfig :: GetProvisionedConcurrencyConfigResponse -> TestTree
responseGetProvisionedConcurrencyConfig =
  res
    "GetProvisionedConcurrencyConfigResponse"
    "fixture/GetProvisionedConcurrencyConfigResponse.proto"
    lambda
    (Proxy :: Proxy GetProvisionedConcurrencyConfig)

responseRemoveLayerVersionPermission :: RemoveLayerVersionPermissionResponse -> TestTree
responseRemoveLayerVersionPermission =
  res
    "RemoveLayerVersionPermissionResponse"
    "fixture/RemoveLayerVersionPermissionResponse.proto"
    lambda
    (Proxy :: Proxy RemoveLayerVersionPermission)

responseListFunctionsByCodeSigningConfig :: ListFunctionsByCodeSigningConfigResponse -> TestTree
responseListFunctionsByCodeSigningConfig =
  res
    "ListFunctionsByCodeSigningConfigResponse"
    "fixture/ListFunctionsByCodeSigningConfigResponse.proto"
    lambda
    (Proxy :: Proxy ListFunctionsByCodeSigningConfig)

responseGetFunction :: GetFunctionResponse -> TestTree
responseGetFunction =
  res
    "GetFunctionResponse"
    "fixture/GetFunctionResponse.proto"
    lambda
    (Proxy :: Proxy GetFunction)

responseListEventSourceMappings :: ListEventSourceMappingsResponse -> TestTree
responseListEventSourceMappings =
  res
    "ListEventSourceMappingsResponse"
    "fixture/ListEventSourceMappingsResponse.proto"
    lambda
    (Proxy :: Proxy ListEventSourceMappings)

responseGetLayerVersionPolicy :: GetLayerVersionPolicyResponse -> TestTree
responseGetLayerVersionPolicy =
  res
    "GetLayerVersionPolicyResponse"
    "fixture/GetLayerVersionPolicyResponse.proto"
    lambda
    (Proxy :: Proxy GetLayerVersionPolicy)

responseDeleteAlias :: DeleteAliasResponse -> TestTree
responseDeleteAlias =
  res
    "DeleteAliasResponse"
    "fixture/DeleteAliasResponse.proto"
    lambda
    (Proxy :: Proxy DeleteAlias)

responseUpdateAlias :: AliasConfiguration -> TestTree
responseUpdateAlias =
  res
    "UpdateAliasResponse"
    "fixture/UpdateAliasResponse.proto"
    lambda
    (Proxy :: Proxy UpdateAlias)

responseGetAccountSettings :: GetAccountSettingsResponse -> TestTree
responseGetAccountSettings =
  res
    "GetAccountSettingsResponse"
    "fixture/GetAccountSettingsResponse.proto"
    lambda
    (Proxy :: Proxy GetAccountSettings)

responseGetFunctionEventInvokeConfig :: FunctionEventInvokeConfig -> TestTree
responseGetFunctionEventInvokeConfig =
  res
    "GetFunctionEventInvokeConfigResponse"
    "fixture/GetFunctionEventInvokeConfigResponse.proto"
    lambda
    (Proxy :: Proxy GetFunctionEventInvokeConfig)

responseGetCodeSigningConfig :: GetCodeSigningConfigResponse -> TestTree
responseGetCodeSigningConfig =
  res
    "GetCodeSigningConfigResponse"
    "fixture/GetCodeSigningConfigResponse.proto"
    lambda
    (Proxy :: Proxy GetCodeSigningConfig)

responseAddPermission :: AddPermissionResponse -> TestTree
responseAddPermission =
  res
    "AddPermissionResponse"
    "fixture/AddPermissionResponse.proto"
    lambda
    (Proxy :: Proxy AddPermission)

responseListLayers :: ListLayersResponse -> TestTree
responseListLayers =
  res
    "ListLayersResponse"
    "fixture/ListLayersResponse.proto"
    lambda
    (Proxy :: Proxy ListLayers)

responseListFunctionEventInvokeConfigs :: ListFunctionEventInvokeConfigsResponse -> TestTree
responseListFunctionEventInvokeConfigs =
  res
    "ListFunctionEventInvokeConfigsResponse"
    "fixture/ListFunctionEventInvokeConfigsResponse.proto"
    lambda
    (Proxy :: Proxy ListFunctionEventInvokeConfigs)

responseListCodeSigningConfigs :: ListCodeSigningConfigsResponse -> TestTree
responseListCodeSigningConfigs =
  res
    "ListCodeSigningConfigsResponse"
    "fixture/ListCodeSigningConfigsResponse.proto"
    lambda
    (Proxy :: Proxy ListCodeSigningConfigs)

responseGetFunctionCodeSigningConfig :: GetFunctionCodeSigningConfigResponse -> TestTree
responseGetFunctionCodeSigningConfig =
  res
    "GetFunctionCodeSigningConfigResponse"
    "fixture/GetFunctionCodeSigningConfigResponse.proto"
    lambda
    (Proxy :: Proxy GetFunctionCodeSigningConfig)

responseCreateCodeSigningConfig :: CreateCodeSigningConfigResponse -> TestTree
responseCreateCodeSigningConfig =
  res
    "CreateCodeSigningConfigResponse"
    "fixture/CreateCodeSigningConfigResponse.proto"
    lambda
    (Proxy :: Proxy CreateCodeSigningConfig)

responseListLayerVersions :: ListLayerVersionsResponse -> TestTree
responseListLayerVersions =
  res
    "ListLayerVersionsResponse"
    "fixture/ListLayerVersionsResponse.proto"
    lambda
    (Proxy :: Proxy ListLayerVersions)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    lambda
    (Proxy :: Proxy TagResource)

responsePublishVersion :: FunctionConfiguration -> TestTree
responsePublishVersion =
  res
    "PublishVersionResponse"
    "fixture/PublishVersionResponse.proto"
    lambda
    (Proxy :: Proxy PublishVersion)

responseListTags :: ListTagsResponse -> TestTree
responseListTags =
  res
    "ListTagsResponse"
    "fixture/ListTagsResponse.proto"
    lambda
    (Proxy :: Proxy ListTags)

responseDeleteFunction :: DeleteFunctionResponse -> TestTree
responseDeleteFunction =
  res
    "DeleteFunctionResponse"
    "fixture/DeleteFunctionResponse.proto"
    lambda
    (Proxy :: Proxy DeleteFunction)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    lambda
    (Proxy :: Proxy UntagResource)

responseUpdateFunctionConfiguration :: FunctionConfiguration -> TestTree
responseUpdateFunctionConfiguration =
  res
    "UpdateFunctionConfigurationResponse"
    "fixture/UpdateFunctionConfigurationResponse.proto"
    lambda
    (Proxy :: Proxy UpdateFunctionConfiguration)

responseListFunctions :: ListFunctionsResponse -> TestTree
responseListFunctions =
  res
    "ListFunctionsResponse"
    "fixture/ListFunctionsResponse.proto"
    lambda
    (Proxy :: Proxy ListFunctions)

responseUpdateFunctionCode :: FunctionConfiguration -> TestTree
responseUpdateFunctionCode =
  res
    "UpdateFunctionCodeResponse"
    "fixture/UpdateFunctionCodeResponse.proto"
    lambda
    (Proxy :: Proxy UpdateFunctionCode)

responseDeleteProvisionedConcurrencyConfig :: DeleteProvisionedConcurrencyConfigResponse -> TestTree
responseDeleteProvisionedConcurrencyConfig =
  res
    "DeleteProvisionedConcurrencyConfigResponse"
    "fixture/DeleteProvisionedConcurrencyConfigResponse.proto"
    lambda
    (Proxy :: Proxy DeleteProvisionedConcurrencyConfig)

responseGetPolicy :: GetPolicyResponse -> TestTree
responseGetPolicy =
  res
    "GetPolicyResponse"
    "fixture/GetPolicyResponse.proto"
    lambda
    (Proxy :: Proxy GetPolicy)

responsePutProvisionedConcurrencyConfig :: PutProvisionedConcurrencyConfigResponse -> TestTree
responsePutProvisionedConcurrencyConfig =
  res
    "PutProvisionedConcurrencyConfigResponse"
    "fixture/PutProvisionedConcurrencyConfigResponse.proto"
    lambda
    (Proxy :: Proxy PutProvisionedConcurrencyConfig)
