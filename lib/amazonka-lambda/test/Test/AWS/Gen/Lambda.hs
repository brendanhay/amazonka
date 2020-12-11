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
--             mkGetFunctionConfiguration
--
--         , requestDeleteEventSourceMapping $
--             mkDeleteEventSourceMapping
--
--         , requestUpdateEventSourceMapping $
--             mkUpdateEventSourceMapping
--
--         , requestGetLayerVersion $
--             mkGetLayerVersion
--
--         , requestDeleteFunctionCodeSigningConfig $
--             mkDeleteFunctionCodeSigningConfig
--
--         , requestPutFunctionCodeSigningConfig $
--             mkPutFunctionCodeSigningConfig
--
--         , requestCreateAlias $
--             mkCreateAlias
--
--         , requestListVersionsByFunction $
--             mkListVersionsByFunction
--
--         , requestListAliases $
--             mkListAliases
--
--         , requestDeleteCodeSigningConfig $
--             mkDeleteCodeSigningConfig
--
--         , requestUpdateCodeSigningConfig $
--             mkUpdateCodeSigningConfig
--
--         , requestRemovePermission $
--             mkRemovePermission
--
--         , requestDeleteFunctionEventInvokeConfig $
--             mkDeleteFunctionEventInvokeConfig
--
--         , requestUpdateFunctionEventInvokeConfig $
--             mkUpdateFunctionEventInvokeConfig
--
--         , requestPutFunctionEventInvokeConfig $
--             mkPutFunctionEventInvokeConfig
--
--         , requestInvoke $
--             mkInvoke
--
--         , requestDeleteLayerVersion $
--             mkDeleteLayerVersion
--
--         , requestGetAlias $
--             mkGetAlias
--
--         , requestPublishLayerVersion $
--             mkPublishLayerVersion
--
--         , requestGetEventSourceMapping $
--             mkGetEventSourceMapping
--
--         , requestAddLayerVersionPermission $
--             mkAddLayerVersionPermission
--
--         , requestListProvisionedConcurrencyConfigs $
--             mkListProvisionedConcurrencyConfigs
--
--         , requestPutFunctionConcurrency $
--             mkPutFunctionConcurrency
--
--         , requestCreateFunction $
--             mkCreateFunction
--
--         , requestDeleteFunctionConcurrency $
--             mkDeleteFunctionConcurrency
--
--         , requestGetLayerVersionByARN $
--             mkGetLayerVersionByARN
--
--         , requestGetFunctionConcurrency $
--             mkGetFunctionConcurrency
--
--         , requestCreateEventSourceMapping $
--             mkCreateEventSourceMapping
--
--         , requestGetProvisionedConcurrencyConfig $
--             mkGetProvisionedConcurrencyConfig
--
--         , requestRemoveLayerVersionPermission $
--             mkRemoveLayerVersionPermission
--
--         , requestListFunctionsByCodeSigningConfig $
--             mkListFunctionsByCodeSigningConfig
--
--         , requestGetFunction $
--             mkGetFunction
--
--         , requestListEventSourceMappings $
--             mkListEventSourceMappings
--
--         , requestGetLayerVersionPolicy $
--             mkGetLayerVersionPolicy
--
--         , requestDeleteAlias $
--             mkDeleteAlias
--
--         , requestUpdateAlias $
--             mkUpdateAlias
--
--         , requestGetAccountSettings $
--             mkGetAccountSettings
--
--         , requestGetFunctionEventInvokeConfig $
--             mkGetFunctionEventInvokeConfig
--
--         , requestGetCodeSigningConfig $
--             mkGetCodeSigningConfig
--
--         , requestAddPermission $
--             mkAddPermission
--
--         , requestListLayers $
--             mkListLayers
--
--         , requestListFunctionEventInvokeConfigs $
--             mkListFunctionEventInvokeConfigs
--
--         , requestListCodeSigningConfigs $
--             mkListCodeSigningConfigs
--
--         , requestGetFunctionCodeSigningConfig $
--             mkGetFunctionCodeSigningConfig
--
--         , requestCreateCodeSigningConfig $
--             mkCreateCodeSigningConfig
--
--         , requestListLayerVersions $
--             mkListLayerVersions
--
--         , requestTagResource $
--             mkTagResource
--
--         , requestPublishVersion $
--             mkPublishVersion
--
--         , requestListTags $
--             mkListTags
--
--         , requestDeleteFunction $
--             mkDeleteFunction
--
--         , requestUntagResource $
--             mkUntagResource
--
--         , requestUpdateFunctionConfiguration $
--             mkUpdateFunctionConfiguration
--
--         , requestListFunctions $
--             mkListFunctions
--
--         , requestUpdateFunctionCode $
--             mkUpdateFunctionCode
--
--         , requestDeleteProvisionedConcurrencyConfig $
--             mkDeleteProvisionedConcurrencyConfig
--
--         , requestGetPolicy $
--             mkGetPolicy
--
--         , requestPutProvisionedConcurrencyConfig $
--             mkPutProvisionedConcurrencyConfig
--
--           ]

--     , testGroup "response"
--         [ responseGetFunctionConfiguration $
--             mkFunctionConfiguration
--
--         , responseDeleteEventSourceMapping $
--             mkEventSourceMappingConfiguration
--
--         , responseUpdateEventSourceMapping $
--             mkEventSourceMappingConfiguration
--
--         , responseGetLayerVersion $
--             mkGetLayerVersionResponse
--
--         , responseDeleteFunctionCodeSigningConfig $
--             mkDeleteFunctionCodeSigningConfigResponse
--
--         , responsePutFunctionCodeSigningConfig $
--             mkPutFunctionCodeSigningConfigResponse
--
--         , responseCreateAlias $
--             mkAliasConfiguration
--
--         , responseListVersionsByFunction $
--             mkListVersionsByFunctionResponse
--
--         , responseListAliases $
--             mkListAliasesResponse
--
--         , responseDeleteCodeSigningConfig $
--             mkDeleteCodeSigningConfigResponse
--
--         , responseUpdateCodeSigningConfig $
--             mkUpdateCodeSigningConfigResponse
--
--         , responseRemovePermission $
--             mkRemovePermissionResponse
--
--         , responseDeleteFunctionEventInvokeConfig $
--             mkDeleteFunctionEventInvokeConfigResponse
--
--         , responseUpdateFunctionEventInvokeConfig $
--             mkFunctionEventInvokeConfig
--
--         , responsePutFunctionEventInvokeConfig $
--             mkFunctionEventInvokeConfig
--
--         , responseInvoke $
--             mkInvokeResponse
--
--         , responseDeleteLayerVersion $
--             mkDeleteLayerVersionResponse
--
--         , responseGetAlias $
--             mkAliasConfiguration
--
--         , responsePublishLayerVersion $
--             mkPublishLayerVersionResponse
--
--         , responseGetEventSourceMapping $
--             mkEventSourceMappingConfiguration
--
--         , responseAddLayerVersionPermission $
--             mkAddLayerVersionPermissionResponse
--
--         , responseListProvisionedConcurrencyConfigs $
--             mkListProvisionedConcurrencyConfigsResponse
--
--         , responsePutFunctionConcurrency $
--             mkConcurrency
--
--         , responseCreateFunction $
--             mkFunctionConfiguration
--
--         , responseDeleteFunctionConcurrency $
--             mkDeleteFunctionConcurrencyResponse
--
--         , responseGetLayerVersionByARN $
--             mkGetLayerVersionResponse
--
--         , responseGetFunctionConcurrency $
--             mkGetFunctionConcurrencyResponse
--
--         , responseCreateEventSourceMapping $
--             mkEventSourceMappingConfiguration
--
--         , responseGetProvisionedConcurrencyConfig $
--             mkGetProvisionedConcurrencyConfigResponse
--
--         , responseRemoveLayerVersionPermission $
--             mkRemoveLayerVersionPermissionResponse
--
--         , responseListFunctionsByCodeSigningConfig $
--             mkListFunctionsByCodeSigningConfigResponse
--
--         , responseGetFunction $
--             mkGetFunctionResponse
--
--         , responseListEventSourceMappings $
--             mkListEventSourceMappingsResponse
--
--         , responseGetLayerVersionPolicy $
--             mkGetLayerVersionPolicyResponse
--
--         , responseDeleteAlias $
--             mkDeleteAliasResponse
--
--         , responseUpdateAlias $
--             mkAliasConfiguration
--
--         , responseGetAccountSettings $
--             mkGetAccountSettingsResponse
--
--         , responseGetFunctionEventInvokeConfig $
--             mkFunctionEventInvokeConfig
--
--         , responseGetCodeSigningConfig $
--             mkGetCodeSigningConfigResponse
--
--         , responseAddPermission $
--             mkAddPermissionResponse
--
--         , responseListLayers $
--             mkListLayersResponse
--
--         , responseListFunctionEventInvokeConfigs $
--             mkListFunctionEventInvokeConfigsResponse
--
--         , responseListCodeSigningConfigs $
--             mkListCodeSigningConfigsResponse
--
--         , responseGetFunctionCodeSigningConfig $
--             mkGetFunctionCodeSigningConfigResponse
--
--         , responseCreateCodeSigningConfig $
--             mkCreateCodeSigningConfigResponse
--
--         , responseListLayerVersions $
--             mkListLayerVersionsResponse
--
--         , responseTagResource $
--             mkTagResourceResponse
--
--         , responsePublishVersion $
--             mkFunctionConfiguration
--
--         , responseListTags $
--             mkListTagsResponse
--
--         , responseDeleteFunction $
--             mkDeleteFunctionResponse
--
--         , responseUntagResource $
--             mkUntagResourceResponse
--
--         , responseUpdateFunctionConfiguration $
--             mkFunctionConfiguration
--
--         , responseListFunctions $
--             mkListFunctionsResponse
--
--         , responseUpdateFunctionCode $
--             mkFunctionConfiguration
--
--         , responseDeleteProvisionedConcurrencyConfig $
--             mkDeleteProvisionedConcurrencyConfigResponse
--
--         , responseGetPolicy $
--             mkGetPolicyResponse
--
--         , responsePutProvisionedConcurrencyConfig $
--             mkPutProvisionedConcurrencyConfigResponse
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
    lambdaService
    (Proxy :: Proxy GetFunctionConfiguration)

responseDeleteEventSourceMapping :: EventSourceMappingConfiguration -> TestTree
responseDeleteEventSourceMapping =
  res
    "DeleteEventSourceMappingResponse"
    "fixture/DeleteEventSourceMappingResponse.proto"
    lambdaService
    (Proxy :: Proxy DeleteEventSourceMapping)

responseUpdateEventSourceMapping :: EventSourceMappingConfiguration -> TestTree
responseUpdateEventSourceMapping =
  res
    "UpdateEventSourceMappingResponse"
    "fixture/UpdateEventSourceMappingResponse.proto"
    lambdaService
    (Proxy :: Proxy UpdateEventSourceMapping)

responseGetLayerVersion :: GetLayerVersionResponse -> TestTree
responseGetLayerVersion =
  res
    "GetLayerVersionResponse"
    "fixture/GetLayerVersionResponse.proto"
    lambdaService
    (Proxy :: Proxy GetLayerVersion)

responseDeleteFunctionCodeSigningConfig :: DeleteFunctionCodeSigningConfigResponse -> TestTree
responseDeleteFunctionCodeSigningConfig =
  res
    "DeleteFunctionCodeSigningConfigResponse"
    "fixture/DeleteFunctionCodeSigningConfigResponse.proto"
    lambdaService
    (Proxy :: Proxy DeleteFunctionCodeSigningConfig)

responsePutFunctionCodeSigningConfig :: PutFunctionCodeSigningConfigResponse -> TestTree
responsePutFunctionCodeSigningConfig =
  res
    "PutFunctionCodeSigningConfigResponse"
    "fixture/PutFunctionCodeSigningConfigResponse.proto"
    lambdaService
    (Proxy :: Proxy PutFunctionCodeSigningConfig)

responseCreateAlias :: AliasConfiguration -> TestTree
responseCreateAlias =
  res
    "CreateAliasResponse"
    "fixture/CreateAliasResponse.proto"
    lambdaService
    (Proxy :: Proxy CreateAlias)

responseListVersionsByFunction :: ListVersionsByFunctionResponse -> TestTree
responseListVersionsByFunction =
  res
    "ListVersionsByFunctionResponse"
    "fixture/ListVersionsByFunctionResponse.proto"
    lambdaService
    (Proxy :: Proxy ListVersionsByFunction)

responseListAliases :: ListAliasesResponse -> TestTree
responseListAliases =
  res
    "ListAliasesResponse"
    "fixture/ListAliasesResponse.proto"
    lambdaService
    (Proxy :: Proxy ListAliases)

responseDeleteCodeSigningConfig :: DeleteCodeSigningConfigResponse -> TestTree
responseDeleteCodeSigningConfig =
  res
    "DeleteCodeSigningConfigResponse"
    "fixture/DeleteCodeSigningConfigResponse.proto"
    lambdaService
    (Proxy :: Proxy DeleteCodeSigningConfig)

responseUpdateCodeSigningConfig :: UpdateCodeSigningConfigResponse -> TestTree
responseUpdateCodeSigningConfig =
  res
    "UpdateCodeSigningConfigResponse"
    "fixture/UpdateCodeSigningConfigResponse.proto"
    lambdaService
    (Proxy :: Proxy UpdateCodeSigningConfig)

responseRemovePermission :: RemovePermissionResponse -> TestTree
responseRemovePermission =
  res
    "RemovePermissionResponse"
    "fixture/RemovePermissionResponse.proto"
    lambdaService
    (Proxy :: Proxy RemovePermission)

responseDeleteFunctionEventInvokeConfig :: DeleteFunctionEventInvokeConfigResponse -> TestTree
responseDeleteFunctionEventInvokeConfig =
  res
    "DeleteFunctionEventInvokeConfigResponse"
    "fixture/DeleteFunctionEventInvokeConfigResponse.proto"
    lambdaService
    (Proxy :: Proxy DeleteFunctionEventInvokeConfig)

responseUpdateFunctionEventInvokeConfig :: FunctionEventInvokeConfig -> TestTree
responseUpdateFunctionEventInvokeConfig =
  res
    "UpdateFunctionEventInvokeConfigResponse"
    "fixture/UpdateFunctionEventInvokeConfigResponse.proto"
    lambdaService
    (Proxy :: Proxy UpdateFunctionEventInvokeConfig)

responsePutFunctionEventInvokeConfig :: FunctionEventInvokeConfig -> TestTree
responsePutFunctionEventInvokeConfig =
  res
    "PutFunctionEventInvokeConfigResponse"
    "fixture/PutFunctionEventInvokeConfigResponse.proto"
    lambdaService
    (Proxy :: Proxy PutFunctionEventInvokeConfig)

responseInvoke :: InvokeResponse -> TestTree
responseInvoke =
  res
    "InvokeResponse"
    "fixture/InvokeResponse.proto"
    lambdaService
    (Proxy :: Proxy Invoke)

responseDeleteLayerVersion :: DeleteLayerVersionResponse -> TestTree
responseDeleteLayerVersion =
  res
    "DeleteLayerVersionResponse"
    "fixture/DeleteLayerVersionResponse.proto"
    lambdaService
    (Proxy :: Proxy DeleteLayerVersion)

responseGetAlias :: AliasConfiguration -> TestTree
responseGetAlias =
  res
    "GetAliasResponse"
    "fixture/GetAliasResponse.proto"
    lambdaService
    (Proxy :: Proxy GetAlias)

responsePublishLayerVersion :: PublishLayerVersionResponse -> TestTree
responsePublishLayerVersion =
  res
    "PublishLayerVersionResponse"
    "fixture/PublishLayerVersionResponse.proto"
    lambdaService
    (Proxy :: Proxy PublishLayerVersion)

responseGetEventSourceMapping :: EventSourceMappingConfiguration -> TestTree
responseGetEventSourceMapping =
  res
    "GetEventSourceMappingResponse"
    "fixture/GetEventSourceMappingResponse.proto"
    lambdaService
    (Proxy :: Proxy GetEventSourceMapping)

responseAddLayerVersionPermission :: AddLayerVersionPermissionResponse -> TestTree
responseAddLayerVersionPermission =
  res
    "AddLayerVersionPermissionResponse"
    "fixture/AddLayerVersionPermissionResponse.proto"
    lambdaService
    (Proxy :: Proxy AddLayerVersionPermission)

responseListProvisionedConcurrencyConfigs :: ListProvisionedConcurrencyConfigsResponse -> TestTree
responseListProvisionedConcurrencyConfigs =
  res
    "ListProvisionedConcurrencyConfigsResponse"
    "fixture/ListProvisionedConcurrencyConfigsResponse.proto"
    lambdaService
    (Proxy :: Proxy ListProvisionedConcurrencyConfigs)

responsePutFunctionConcurrency :: Concurrency -> TestTree
responsePutFunctionConcurrency =
  res
    "PutFunctionConcurrencyResponse"
    "fixture/PutFunctionConcurrencyResponse.proto"
    lambdaService
    (Proxy :: Proxy PutFunctionConcurrency)

responseCreateFunction :: FunctionConfiguration -> TestTree
responseCreateFunction =
  res
    "CreateFunctionResponse"
    "fixture/CreateFunctionResponse.proto"
    lambdaService
    (Proxy :: Proxy CreateFunction)

responseDeleteFunctionConcurrency :: DeleteFunctionConcurrencyResponse -> TestTree
responseDeleteFunctionConcurrency =
  res
    "DeleteFunctionConcurrencyResponse"
    "fixture/DeleteFunctionConcurrencyResponse.proto"
    lambdaService
    (Proxy :: Proxy DeleteFunctionConcurrency)

responseGetLayerVersionByARN :: GetLayerVersionResponse -> TestTree
responseGetLayerVersionByARN =
  res
    "GetLayerVersionByARNResponse"
    "fixture/GetLayerVersionByARNResponse.proto"
    lambdaService
    (Proxy :: Proxy GetLayerVersionByARN)

responseGetFunctionConcurrency :: GetFunctionConcurrencyResponse -> TestTree
responseGetFunctionConcurrency =
  res
    "GetFunctionConcurrencyResponse"
    "fixture/GetFunctionConcurrencyResponse.proto"
    lambdaService
    (Proxy :: Proxy GetFunctionConcurrency)

responseCreateEventSourceMapping :: EventSourceMappingConfiguration -> TestTree
responseCreateEventSourceMapping =
  res
    "CreateEventSourceMappingResponse"
    "fixture/CreateEventSourceMappingResponse.proto"
    lambdaService
    (Proxy :: Proxy CreateEventSourceMapping)

responseGetProvisionedConcurrencyConfig :: GetProvisionedConcurrencyConfigResponse -> TestTree
responseGetProvisionedConcurrencyConfig =
  res
    "GetProvisionedConcurrencyConfigResponse"
    "fixture/GetProvisionedConcurrencyConfigResponse.proto"
    lambdaService
    (Proxy :: Proxy GetProvisionedConcurrencyConfig)

responseRemoveLayerVersionPermission :: RemoveLayerVersionPermissionResponse -> TestTree
responseRemoveLayerVersionPermission =
  res
    "RemoveLayerVersionPermissionResponse"
    "fixture/RemoveLayerVersionPermissionResponse.proto"
    lambdaService
    (Proxy :: Proxy RemoveLayerVersionPermission)

responseListFunctionsByCodeSigningConfig :: ListFunctionsByCodeSigningConfigResponse -> TestTree
responseListFunctionsByCodeSigningConfig =
  res
    "ListFunctionsByCodeSigningConfigResponse"
    "fixture/ListFunctionsByCodeSigningConfigResponse.proto"
    lambdaService
    (Proxy :: Proxy ListFunctionsByCodeSigningConfig)

responseGetFunction :: GetFunctionResponse -> TestTree
responseGetFunction =
  res
    "GetFunctionResponse"
    "fixture/GetFunctionResponse.proto"
    lambdaService
    (Proxy :: Proxy GetFunction)

responseListEventSourceMappings :: ListEventSourceMappingsResponse -> TestTree
responseListEventSourceMappings =
  res
    "ListEventSourceMappingsResponse"
    "fixture/ListEventSourceMappingsResponse.proto"
    lambdaService
    (Proxy :: Proxy ListEventSourceMappings)

responseGetLayerVersionPolicy :: GetLayerVersionPolicyResponse -> TestTree
responseGetLayerVersionPolicy =
  res
    "GetLayerVersionPolicyResponse"
    "fixture/GetLayerVersionPolicyResponse.proto"
    lambdaService
    (Proxy :: Proxy GetLayerVersionPolicy)

responseDeleteAlias :: DeleteAliasResponse -> TestTree
responseDeleteAlias =
  res
    "DeleteAliasResponse"
    "fixture/DeleteAliasResponse.proto"
    lambdaService
    (Proxy :: Proxy DeleteAlias)

responseUpdateAlias :: AliasConfiguration -> TestTree
responseUpdateAlias =
  res
    "UpdateAliasResponse"
    "fixture/UpdateAliasResponse.proto"
    lambdaService
    (Proxy :: Proxy UpdateAlias)

responseGetAccountSettings :: GetAccountSettingsResponse -> TestTree
responseGetAccountSettings =
  res
    "GetAccountSettingsResponse"
    "fixture/GetAccountSettingsResponse.proto"
    lambdaService
    (Proxy :: Proxy GetAccountSettings)

responseGetFunctionEventInvokeConfig :: FunctionEventInvokeConfig -> TestTree
responseGetFunctionEventInvokeConfig =
  res
    "GetFunctionEventInvokeConfigResponse"
    "fixture/GetFunctionEventInvokeConfigResponse.proto"
    lambdaService
    (Proxy :: Proxy GetFunctionEventInvokeConfig)

responseGetCodeSigningConfig :: GetCodeSigningConfigResponse -> TestTree
responseGetCodeSigningConfig =
  res
    "GetCodeSigningConfigResponse"
    "fixture/GetCodeSigningConfigResponse.proto"
    lambdaService
    (Proxy :: Proxy GetCodeSigningConfig)

responseAddPermission :: AddPermissionResponse -> TestTree
responseAddPermission =
  res
    "AddPermissionResponse"
    "fixture/AddPermissionResponse.proto"
    lambdaService
    (Proxy :: Proxy AddPermission)

responseListLayers :: ListLayersResponse -> TestTree
responseListLayers =
  res
    "ListLayersResponse"
    "fixture/ListLayersResponse.proto"
    lambdaService
    (Proxy :: Proxy ListLayers)

responseListFunctionEventInvokeConfigs :: ListFunctionEventInvokeConfigsResponse -> TestTree
responseListFunctionEventInvokeConfigs =
  res
    "ListFunctionEventInvokeConfigsResponse"
    "fixture/ListFunctionEventInvokeConfigsResponse.proto"
    lambdaService
    (Proxy :: Proxy ListFunctionEventInvokeConfigs)

responseListCodeSigningConfigs :: ListCodeSigningConfigsResponse -> TestTree
responseListCodeSigningConfigs =
  res
    "ListCodeSigningConfigsResponse"
    "fixture/ListCodeSigningConfigsResponse.proto"
    lambdaService
    (Proxy :: Proxy ListCodeSigningConfigs)

responseGetFunctionCodeSigningConfig :: GetFunctionCodeSigningConfigResponse -> TestTree
responseGetFunctionCodeSigningConfig =
  res
    "GetFunctionCodeSigningConfigResponse"
    "fixture/GetFunctionCodeSigningConfigResponse.proto"
    lambdaService
    (Proxy :: Proxy GetFunctionCodeSigningConfig)

responseCreateCodeSigningConfig :: CreateCodeSigningConfigResponse -> TestTree
responseCreateCodeSigningConfig =
  res
    "CreateCodeSigningConfigResponse"
    "fixture/CreateCodeSigningConfigResponse.proto"
    lambdaService
    (Proxy :: Proxy CreateCodeSigningConfig)

responseListLayerVersions :: ListLayerVersionsResponse -> TestTree
responseListLayerVersions =
  res
    "ListLayerVersionsResponse"
    "fixture/ListLayerVersionsResponse.proto"
    lambdaService
    (Proxy :: Proxy ListLayerVersions)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    lambdaService
    (Proxy :: Proxy TagResource)

responsePublishVersion :: FunctionConfiguration -> TestTree
responsePublishVersion =
  res
    "PublishVersionResponse"
    "fixture/PublishVersionResponse.proto"
    lambdaService
    (Proxy :: Proxy PublishVersion)

responseListTags :: ListTagsResponse -> TestTree
responseListTags =
  res
    "ListTagsResponse"
    "fixture/ListTagsResponse.proto"
    lambdaService
    (Proxy :: Proxy ListTags)

responseDeleteFunction :: DeleteFunctionResponse -> TestTree
responseDeleteFunction =
  res
    "DeleteFunctionResponse"
    "fixture/DeleteFunctionResponse.proto"
    lambdaService
    (Proxy :: Proxy DeleteFunction)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    lambdaService
    (Proxy :: Proxy UntagResource)

responseUpdateFunctionConfiguration :: FunctionConfiguration -> TestTree
responseUpdateFunctionConfiguration =
  res
    "UpdateFunctionConfigurationResponse"
    "fixture/UpdateFunctionConfigurationResponse.proto"
    lambdaService
    (Proxy :: Proxy UpdateFunctionConfiguration)

responseListFunctions :: ListFunctionsResponse -> TestTree
responseListFunctions =
  res
    "ListFunctionsResponse"
    "fixture/ListFunctionsResponse.proto"
    lambdaService
    (Proxy :: Proxy ListFunctions)

responseUpdateFunctionCode :: FunctionConfiguration -> TestTree
responseUpdateFunctionCode =
  res
    "UpdateFunctionCodeResponse"
    "fixture/UpdateFunctionCodeResponse.proto"
    lambdaService
    (Proxy :: Proxy UpdateFunctionCode)

responseDeleteProvisionedConcurrencyConfig :: DeleteProvisionedConcurrencyConfigResponse -> TestTree
responseDeleteProvisionedConcurrencyConfig =
  res
    "DeleteProvisionedConcurrencyConfigResponse"
    "fixture/DeleteProvisionedConcurrencyConfigResponse.proto"
    lambdaService
    (Proxy :: Proxy DeleteProvisionedConcurrencyConfig)

responseGetPolicy :: GetPolicyResponse -> TestTree
responseGetPolicy =
  res
    "GetPolicyResponse"
    "fixture/GetPolicyResponse.proto"
    lambdaService
    (Proxy :: Proxy GetPolicy)

responsePutProvisionedConcurrencyConfig :: PutProvisionedConcurrencyConfigResponse -> TestTree
responsePutProvisionedConcurrencyConfig =
  res
    "PutProvisionedConcurrencyConfigResponse"
    "fixture/PutProvisionedConcurrencyConfigResponse.proto"
    lambdaService
    (Proxy :: Proxy PutProvisionedConcurrencyConfig)
