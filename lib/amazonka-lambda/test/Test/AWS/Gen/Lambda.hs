{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.Lambda
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
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
--         , requestGetLayerVersionByArn $
--             mkGetLayerVersionByArn
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
--         , responseGetLayerVersionByArn $
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

requestGetLayerVersion :: GetLayerVersion -> TestTree
requestGetLayerVersion = req
    "GetLayerVersion"
    "fixture/GetLayerVersion.yaml"

requestDeleteFunctionCodeSigningConfig :: DeleteFunctionCodeSigningConfig -> TestTree
requestDeleteFunctionCodeSigningConfig = req
    "DeleteFunctionCodeSigningConfig"
    "fixture/DeleteFunctionCodeSigningConfig.yaml"

requestPutFunctionCodeSigningConfig :: PutFunctionCodeSigningConfig -> TestTree
requestPutFunctionCodeSigningConfig = req
    "PutFunctionCodeSigningConfig"
    "fixture/PutFunctionCodeSigningConfig.yaml"

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

requestDeleteCodeSigningConfig :: DeleteCodeSigningConfig -> TestTree
requestDeleteCodeSigningConfig = req
    "DeleteCodeSigningConfig"
    "fixture/DeleteCodeSigningConfig.yaml"

requestUpdateCodeSigningConfig :: UpdateCodeSigningConfig -> TestTree
requestUpdateCodeSigningConfig = req
    "UpdateCodeSigningConfig"
    "fixture/UpdateCodeSigningConfig.yaml"

requestRemovePermission :: RemovePermission -> TestTree
requestRemovePermission = req
    "RemovePermission"
    "fixture/RemovePermission.yaml"

requestDeleteFunctionEventInvokeConfig :: DeleteFunctionEventInvokeConfig -> TestTree
requestDeleteFunctionEventInvokeConfig = req
    "DeleteFunctionEventInvokeConfig"
    "fixture/DeleteFunctionEventInvokeConfig.yaml"

requestUpdateFunctionEventInvokeConfig :: UpdateFunctionEventInvokeConfig -> TestTree
requestUpdateFunctionEventInvokeConfig = req
    "UpdateFunctionEventInvokeConfig"
    "fixture/UpdateFunctionEventInvokeConfig.yaml"

requestPutFunctionEventInvokeConfig :: PutFunctionEventInvokeConfig -> TestTree
requestPutFunctionEventInvokeConfig = req
    "PutFunctionEventInvokeConfig"
    "fixture/PutFunctionEventInvokeConfig.yaml"

requestInvoke :: Invoke -> TestTree
requestInvoke = req
    "Invoke"
    "fixture/Invoke.yaml"

requestDeleteLayerVersion :: DeleteLayerVersion -> TestTree
requestDeleteLayerVersion = req
    "DeleteLayerVersion"
    "fixture/DeleteLayerVersion.yaml"

requestGetAlias :: GetAlias -> TestTree
requestGetAlias = req
    "GetAlias"
    "fixture/GetAlias.yaml"

requestPublishLayerVersion :: PublishLayerVersion -> TestTree
requestPublishLayerVersion = req
    "PublishLayerVersion"
    "fixture/PublishLayerVersion.yaml"

requestGetEventSourceMapping :: GetEventSourceMapping -> TestTree
requestGetEventSourceMapping = req
    "GetEventSourceMapping"
    "fixture/GetEventSourceMapping.yaml"

requestAddLayerVersionPermission :: AddLayerVersionPermission -> TestTree
requestAddLayerVersionPermission = req
    "AddLayerVersionPermission"
    "fixture/AddLayerVersionPermission.yaml"

requestListProvisionedConcurrencyConfigs :: ListProvisionedConcurrencyConfigs -> TestTree
requestListProvisionedConcurrencyConfigs = req
    "ListProvisionedConcurrencyConfigs"
    "fixture/ListProvisionedConcurrencyConfigs.yaml"

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

requestGetLayerVersionByArn :: GetLayerVersionByArn -> TestTree
requestGetLayerVersionByArn = req
    "GetLayerVersionByArn"
    "fixture/GetLayerVersionByArn.yaml"

requestGetFunctionConcurrency :: GetFunctionConcurrency -> TestTree
requestGetFunctionConcurrency = req
    "GetFunctionConcurrency"
    "fixture/GetFunctionConcurrency.yaml"

requestCreateEventSourceMapping :: CreateEventSourceMapping -> TestTree
requestCreateEventSourceMapping = req
    "CreateEventSourceMapping"
    "fixture/CreateEventSourceMapping.yaml"

requestGetProvisionedConcurrencyConfig :: GetProvisionedConcurrencyConfig -> TestTree
requestGetProvisionedConcurrencyConfig = req
    "GetProvisionedConcurrencyConfig"
    "fixture/GetProvisionedConcurrencyConfig.yaml"

requestRemoveLayerVersionPermission :: RemoveLayerVersionPermission -> TestTree
requestRemoveLayerVersionPermission = req
    "RemoveLayerVersionPermission"
    "fixture/RemoveLayerVersionPermission.yaml"

requestListFunctionsByCodeSigningConfig :: ListFunctionsByCodeSigningConfig -> TestTree
requestListFunctionsByCodeSigningConfig = req
    "ListFunctionsByCodeSigningConfig"
    "fixture/ListFunctionsByCodeSigningConfig.yaml"

requestGetFunction :: GetFunction -> TestTree
requestGetFunction = req
    "GetFunction"
    "fixture/GetFunction.yaml"

requestListEventSourceMappings :: ListEventSourceMappings -> TestTree
requestListEventSourceMappings = req
    "ListEventSourceMappings"
    "fixture/ListEventSourceMappings.yaml"

requestGetLayerVersionPolicy :: GetLayerVersionPolicy -> TestTree
requestGetLayerVersionPolicy = req
    "GetLayerVersionPolicy"
    "fixture/GetLayerVersionPolicy.yaml"

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

requestGetFunctionEventInvokeConfig :: GetFunctionEventInvokeConfig -> TestTree
requestGetFunctionEventInvokeConfig = req
    "GetFunctionEventInvokeConfig"
    "fixture/GetFunctionEventInvokeConfig.yaml"

requestGetCodeSigningConfig :: GetCodeSigningConfig -> TestTree
requestGetCodeSigningConfig = req
    "GetCodeSigningConfig"
    "fixture/GetCodeSigningConfig.yaml"

requestAddPermission :: AddPermission -> TestTree
requestAddPermission = req
    "AddPermission"
    "fixture/AddPermission.yaml"

requestListLayers :: ListLayers -> TestTree
requestListLayers = req
    "ListLayers"
    "fixture/ListLayers.yaml"

requestListFunctionEventInvokeConfigs :: ListFunctionEventInvokeConfigs -> TestTree
requestListFunctionEventInvokeConfigs = req
    "ListFunctionEventInvokeConfigs"
    "fixture/ListFunctionEventInvokeConfigs.yaml"

requestListCodeSigningConfigs :: ListCodeSigningConfigs -> TestTree
requestListCodeSigningConfigs = req
    "ListCodeSigningConfigs"
    "fixture/ListCodeSigningConfigs.yaml"

requestGetFunctionCodeSigningConfig :: GetFunctionCodeSigningConfig -> TestTree
requestGetFunctionCodeSigningConfig = req
    "GetFunctionCodeSigningConfig"
    "fixture/GetFunctionCodeSigningConfig.yaml"

requestCreateCodeSigningConfig :: CreateCodeSigningConfig -> TestTree
requestCreateCodeSigningConfig = req
    "CreateCodeSigningConfig"
    "fixture/CreateCodeSigningConfig.yaml"

requestListLayerVersions :: ListLayerVersions -> TestTree
requestListLayerVersions = req
    "ListLayerVersions"
    "fixture/ListLayerVersions.yaml"

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

requestDeleteProvisionedConcurrencyConfig :: DeleteProvisionedConcurrencyConfig -> TestTree
requestDeleteProvisionedConcurrencyConfig = req
    "DeleteProvisionedConcurrencyConfig"
    "fixture/DeleteProvisionedConcurrencyConfig.yaml"

requestGetPolicy :: GetPolicy -> TestTree
requestGetPolicy = req
    "GetPolicy"
    "fixture/GetPolicy.yaml"

requestPutProvisionedConcurrencyConfig :: PutProvisionedConcurrencyConfig -> TestTree
requestPutProvisionedConcurrencyConfig = req
    "PutProvisionedConcurrencyConfig"
    "fixture/PutProvisionedConcurrencyConfig.yaml"

-- Responses

responseGetFunctionConfiguration :: FunctionConfiguration -> TestTree
responseGetFunctionConfiguration = res
    "GetFunctionConfigurationResponse"
    "fixture/GetFunctionConfigurationResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetFunctionConfiguration)

responseDeleteEventSourceMapping :: EventSourceMappingConfiguration -> TestTree
responseDeleteEventSourceMapping = res
    "DeleteEventSourceMappingResponse"
    "fixture/DeleteEventSourceMappingResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteEventSourceMapping)

responseUpdateEventSourceMapping :: EventSourceMappingConfiguration -> TestTree
responseUpdateEventSourceMapping = res
    "UpdateEventSourceMappingResponse"
    "fixture/UpdateEventSourceMappingResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateEventSourceMapping)

responseGetLayerVersion :: GetLayerVersionResponse -> TestTree
responseGetLayerVersion = res
    "GetLayerVersionResponse"
    "fixture/GetLayerVersionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetLayerVersion)

responseDeleteFunctionCodeSigningConfig :: DeleteFunctionCodeSigningConfigResponse -> TestTree
responseDeleteFunctionCodeSigningConfig = res
    "DeleteFunctionCodeSigningConfigResponse"
    "fixture/DeleteFunctionCodeSigningConfigResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteFunctionCodeSigningConfig)

responsePutFunctionCodeSigningConfig :: PutFunctionCodeSigningConfigResponse -> TestTree
responsePutFunctionCodeSigningConfig = res
    "PutFunctionCodeSigningConfigResponse"
    "fixture/PutFunctionCodeSigningConfigResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy PutFunctionCodeSigningConfig)

responseCreateAlias :: AliasConfiguration -> TestTree
responseCreateAlias = res
    "CreateAliasResponse"
    "fixture/CreateAliasResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateAlias)

responseListVersionsByFunction :: ListVersionsByFunctionResponse -> TestTree
responseListVersionsByFunction = res
    "ListVersionsByFunctionResponse"
    "fixture/ListVersionsByFunctionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListVersionsByFunction)

responseListAliases :: ListAliasesResponse -> TestTree
responseListAliases = res
    "ListAliasesResponse"
    "fixture/ListAliasesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListAliases)

responseDeleteCodeSigningConfig :: DeleteCodeSigningConfigResponse -> TestTree
responseDeleteCodeSigningConfig = res
    "DeleteCodeSigningConfigResponse"
    "fixture/DeleteCodeSigningConfigResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteCodeSigningConfig)

responseUpdateCodeSigningConfig :: UpdateCodeSigningConfigResponse -> TestTree
responseUpdateCodeSigningConfig = res
    "UpdateCodeSigningConfigResponse"
    "fixture/UpdateCodeSigningConfigResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateCodeSigningConfig)

responseRemovePermission :: RemovePermissionResponse -> TestTree
responseRemovePermission = res
    "RemovePermissionResponse"
    "fixture/RemovePermissionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy RemovePermission)

responseDeleteFunctionEventInvokeConfig :: DeleteFunctionEventInvokeConfigResponse -> TestTree
responseDeleteFunctionEventInvokeConfig = res
    "DeleteFunctionEventInvokeConfigResponse"
    "fixture/DeleteFunctionEventInvokeConfigResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteFunctionEventInvokeConfig)

responseUpdateFunctionEventInvokeConfig :: FunctionEventInvokeConfig -> TestTree
responseUpdateFunctionEventInvokeConfig = res
    "UpdateFunctionEventInvokeConfigResponse"
    "fixture/UpdateFunctionEventInvokeConfigResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateFunctionEventInvokeConfig)

responsePutFunctionEventInvokeConfig :: FunctionEventInvokeConfig -> TestTree
responsePutFunctionEventInvokeConfig = res
    "PutFunctionEventInvokeConfigResponse"
    "fixture/PutFunctionEventInvokeConfigResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy PutFunctionEventInvokeConfig)

responseInvoke :: InvokeResponse -> TestTree
responseInvoke = res
    "InvokeResponse"
    "fixture/InvokeResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy Invoke)

responseDeleteLayerVersion :: DeleteLayerVersionResponse -> TestTree
responseDeleteLayerVersion = res
    "DeleteLayerVersionResponse"
    "fixture/DeleteLayerVersionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteLayerVersion)

responseGetAlias :: AliasConfiguration -> TestTree
responseGetAlias = res
    "GetAliasResponse"
    "fixture/GetAliasResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetAlias)

responsePublishLayerVersion :: PublishLayerVersionResponse -> TestTree
responsePublishLayerVersion = res
    "PublishLayerVersionResponse"
    "fixture/PublishLayerVersionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy PublishLayerVersion)

responseGetEventSourceMapping :: EventSourceMappingConfiguration -> TestTree
responseGetEventSourceMapping = res
    "GetEventSourceMappingResponse"
    "fixture/GetEventSourceMappingResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetEventSourceMapping)

responseAddLayerVersionPermission :: AddLayerVersionPermissionResponse -> TestTree
responseAddLayerVersionPermission = res
    "AddLayerVersionPermissionResponse"
    "fixture/AddLayerVersionPermissionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy AddLayerVersionPermission)

responseListProvisionedConcurrencyConfigs :: ListProvisionedConcurrencyConfigsResponse -> TestTree
responseListProvisionedConcurrencyConfigs = res
    "ListProvisionedConcurrencyConfigsResponse"
    "fixture/ListProvisionedConcurrencyConfigsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListProvisionedConcurrencyConfigs)

responsePutFunctionConcurrency :: Concurrency -> TestTree
responsePutFunctionConcurrency = res
    "PutFunctionConcurrencyResponse"
    "fixture/PutFunctionConcurrencyResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy PutFunctionConcurrency)

responseCreateFunction :: FunctionConfiguration -> TestTree
responseCreateFunction = res
    "CreateFunctionResponse"
    "fixture/CreateFunctionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateFunction)

responseDeleteFunctionConcurrency :: DeleteFunctionConcurrencyResponse -> TestTree
responseDeleteFunctionConcurrency = res
    "DeleteFunctionConcurrencyResponse"
    "fixture/DeleteFunctionConcurrencyResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteFunctionConcurrency)

responseGetLayerVersionByArn :: GetLayerVersionResponse -> TestTree
responseGetLayerVersionByArn = res
    "GetLayerVersionByArnResponse"
    "fixture/GetLayerVersionByArnResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetLayerVersionByArn)

responseGetFunctionConcurrency :: GetFunctionConcurrencyResponse -> TestTree
responseGetFunctionConcurrency = res
    "GetFunctionConcurrencyResponse"
    "fixture/GetFunctionConcurrencyResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetFunctionConcurrency)

responseCreateEventSourceMapping :: EventSourceMappingConfiguration -> TestTree
responseCreateEventSourceMapping = res
    "CreateEventSourceMappingResponse"
    "fixture/CreateEventSourceMappingResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateEventSourceMapping)

responseGetProvisionedConcurrencyConfig :: GetProvisionedConcurrencyConfigResponse -> TestTree
responseGetProvisionedConcurrencyConfig = res
    "GetProvisionedConcurrencyConfigResponse"
    "fixture/GetProvisionedConcurrencyConfigResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetProvisionedConcurrencyConfig)

responseRemoveLayerVersionPermission :: RemoveLayerVersionPermissionResponse -> TestTree
responseRemoveLayerVersionPermission = res
    "RemoveLayerVersionPermissionResponse"
    "fixture/RemoveLayerVersionPermissionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy RemoveLayerVersionPermission)

responseListFunctionsByCodeSigningConfig :: ListFunctionsByCodeSigningConfigResponse -> TestTree
responseListFunctionsByCodeSigningConfig = res
    "ListFunctionsByCodeSigningConfigResponse"
    "fixture/ListFunctionsByCodeSigningConfigResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListFunctionsByCodeSigningConfig)

responseGetFunction :: GetFunctionResponse -> TestTree
responseGetFunction = res
    "GetFunctionResponse"
    "fixture/GetFunctionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetFunction)

responseListEventSourceMappings :: ListEventSourceMappingsResponse -> TestTree
responseListEventSourceMappings = res
    "ListEventSourceMappingsResponse"
    "fixture/ListEventSourceMappingsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListEventSourceMappings)

responseGetLayerVersionPolicy :: GetLayerVersionPolicyResponse -> TestTree
responseGetLayerVersionPolicy = res
    "GetLayerVersionPolicyResponse"
    "fixture/GetLayerVersionPolicyResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetLayerVersionPolicy)

responseDeleteAlias :: DeleteAliasResponse -> TestTree
responseDeleteAlias = res
    "DeleteAliasResponse"
    "fixture/DeleteAliasResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteAlias)

responseUpdateAlias :: AliasConfiguration -> TestTree
responseUpdateAlias = res
    "UpdateAliasResponse"
    "fixture/UpdateAliasResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateAlias)

responseGetAccountSettings :: GetAccountSettingsResponse -> TestTree
responseGetAccountSettings = res
    "GetAccountSettingsResponse"
    "fixture/GetAccountSettingsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetAccountSettings)

responseGetFunctionEventInvokeConfig :: FunctionEventInvokeConfig -> TestTree
responseGetFunctionEventInvokeConfig = res
    "GetFunctionEventInvokeConfigResponse"
    "fixture/GetFunctionEventInvokeConfigResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetFunctionEventInvokeConfig)

responseGetCodeSigningConfig :: GetCodeSigningConfigResponse -> TestTree
responseGetCodeSigningConfig = res
    "GetCodeSigningConfigResponse"
    "fixture/GetCodeSigningConfigResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetCodeSigningConfig)

responseAddPermission :: AddPermissionResponse -> TestTree
responseAddPermission = res
    "AddPermissionResponse"
    "fixture/AddPermissionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy AddPermission)

responseListLayers :: ListLayersResponse -> TestTree
responseListLayers = res
    "ListLayersResponse"
    "fixture/ListLayersResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListLayers)

responseListFunctionEventInvokeConfigs :: ListFunctionEventInvokeConfigsResponse -> TestTree
responseListFunctionEventInvokeConfigs = res
    "ListFunctionEventInvokeConfigsResponse"
    "fixture/ListFunctionEventInvokeConfigsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListFunctionEventInvokeConfigs)

responseListCodeSigningConfigs :: ListCodeSigningConfigsResponse -> TestTree
responseListCodeSigningConfigs = res
    "ListCodeSigningConfigsResponse"
    "fixture/ListCodeSigningConfigsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListCodeSigningConfigs)

responseGetFunctionCodeSigningConfig :: GetFunctionCodeSigningConfigResponse -> TestTree
responseGetFunctionCodeSigningConfig = res
    "GetFunctionCodeSigningConfigResponse"
    "fixture/GetFunctionCodeSigningConfigResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetFunctionCodeSigningConfig)

responseCreateCodeSigningConfig :: CreateCodeSigningConfigResponse -> TestTree
responseCreateCodeSigningConfig = res
    "CreateCodeSigningConfigResponse"
    "fixture/CreateCodeSigningConfigResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateCodeSigningConfig)

responseListLayerVersions :: ListLayerVersionsResponse -> TestTree
responseListLayerVersions = res
    "ListLayerVersionsResponse"
    "fixture/ListLayerVersionsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListLayerVersions)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource = res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy TagResource)

responsePublishVersion :: FunctionConfiguration -> TestTree
responsePublishVersion = res
    "PublishVersionResponse"
    "fixture/PublishVersionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy PublishVersion)

responseListTags :: ListTagsResponse -> TestTree
responseListTags = res
    "ListTagsResponse"
    "fixture/ListTagsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListTags)

responseDeleteFunction :: DeleteFunctionResponse -> TestTree
responseDeleteFunction = res
    "DeleteFunctionResponse"
    "fixture/DeleteFunctionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteFunction)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource = res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UntagResource)

responseUpdateFunctionConfiguration :: FunctionConfiguration -> TestTree
responseUpdateFunctionConfiguration = res
    "UpdateFunctionConfigurationResponse"
    "fixture/UpdateFunctionConfigurationResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateFunctionConfiguration)

responseListFunctions :: ListFunctionsResponse -> TestTree
responseListFunctions = res
    "ListFunctionsResponse"
    "fixture/ListFunctionsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListFunctions)

responseUpdateFunctionCode :: FunctionConfiguration -> TestTree
responseUpdateFunctionCode = res
    "UpdateFunctionCodeResponse"
    "fixture/UpdateFunctionCodeResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateFunctionCode)

responseDeleteProvisionedConcurrencyConfig :: DeleteProvisionedConcurrencyConfigResponse -> TestTree
responseDeleteProvisionedConcurrencyConfig = res
    "DeleteProvisionedConcurrencyConfigResponse"
    "fixture/DeleteProvisionedConcurrencyConfigResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteProvisionedConcurrencyConfig)

responseGetPolicy :: GetPolicyResponse -> TestTree
responseGetPolicy = res
    "GetPolicyResponse"
    "fixture/GetPolicyResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetPolicy)

responsePutProvisionedConcurrencyConfig :: PutProvisionedConcurrencyConfigResponse -> TestTree
responsePutProvisionedConcurrencyConfig = res
    "PutProvisionedConcurrencyConfigResponse"
    "fixture/PutProvisionedConcurrencyConfigResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy PutProvisionedConcurrencyConfig)
