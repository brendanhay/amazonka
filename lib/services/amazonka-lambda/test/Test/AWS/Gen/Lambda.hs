{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.Lambda
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.AWS.Gen.Lambda where

import Amazonka.Lambda
import qualified Data.Proxy as Proxy
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
--             newGetFunctionConfiguration
--
--         , requestDeleteEventSourceMapping $
--             newDeleteEventSourceMapping
--
--         , requestUpdateEventSourceMapping $
--             newUpdateEventSourceMapping
--
--         , requestGetLayerVersion $
--             newGetLayerVersion
--
--         , requestDeleteFunctionCodeSigningConfig $
--             newDeleteFunctionCodeSigningConfig
--
--         , requestPutFunctionCodeSigningConfig $
--             newPutFunctionCodeSigningConfig
--
--         , requestCreateAlias $
--             newCreateAlias
--
--         , requestListVersionsByFunction $
--             newListVersionsByFunction
--
--         , requestListAliases $
--             newListAliases
--
--         , requestDeleteCodeSigningConfig $
--             newDeleteCodeSigningConfig
--
--         , requestUpdateCodeSigningConfig $
--             newUpdateCodeSigningConfig
--
--         , requestRemovePermission $
--             newRemovePermission
--
--         , requestDeleteFunctionEventInvokeConfig $
--             newDeleteFunctionEventInvokeConfig
--
--         , requestUpdateFunctionEventInvokeConfig $
--             newUpdateFunctionEventInvokeConfig
--
--         , requestPutFunctionEventInvokeConfig $
--             newPutFunctionEventInvokeConfig
--
--         , requestInvoke $
--             newInvoke
--
--         , requestDeleteLayerVersion $
--             newDeleteLayerVersion
--
--         , requestGetAlias $
--             newGetAlias
--
--         , requestPublishLayerVersion $
--             newPublishLayerVersion
--
--         , requestGetEventSourceMapping $
--             newGetEventSourceMapping
--
--         , requestAddLayerVersionPermission $
--             newAddLayerVersionPermission
--
--         , requestListProvisionedConcurrencyConfigs $
--             newListProvisionedConcurrencyConfigs
--
--         , requestPutFunctionConcurrency $
--             newPutFunctionConcurrency
--
--         , requestCreateFunction $
--             newCreateFunction
--
--         , requestDeleteFunctionConcurrency $
--             newDeleteFunctionConcurrency
--
--         , requestGetLayerVersionByArn $
--             newGetLayerVersionByArn
--
--         , requestGetFunctionConcurrency $
--             newGetFunctionConcurrency
--
--         , requestCreateEventSourceMapping $
--             newCreateEventSourceMapping
--
--         , requestGetProvisionedConcurrencyConfig $
--             newGetProvisionedConcurrencyConfig
--
--         , requestRemoveLayerVersionPermission $
--             newRemoveLayerVersionPermission
--
--         , requestListFunctionsByCodeSigningConfig $
--             newListFunctionsByCodeSigningConfig
--
--         , requestGetFunction $
--             newGetFunction
--
--         , requestListEventSourceMappings $
--             newListEventSourceMappings
--
--         , requestGetLayerVersionPolicy $
--             newGetLayerVersionPolicy
--
--         , requestDeleteAlias $
--             newDeleteAlias
--
--         , requestUpdateAlias $
--             newUpdateAlias
--
--         , requestGetAccountSettings $
--             newGetAccountSettings
--
--         , requestGetFunctionEventInvokeConfig $
--             newGetFunctionEventInvokeConfig
--
--         , requestGetCodeSigningConfig $
--             newGetCodeSigningConfig
--
--         , requestAddPermission $
--             newAddPermission
--
--         , requestListLayers $
--             newListLayers
--
--         , requestListFunctionEventInvokeConfigs $
--             newListFunctionEventInvokeConfigs
--
--         , requestListCodeSigningConfigs $
--             newListCodeSigningConfigs
--
--         , requestGetFunctionCodeSigningConfig $
--             newGetFunctionCodeSigningConfig
--
--         , requestCreateCodeSigningConfig $
--             newCreateCodeSigningConfig
--
--         , requestListLayerVersions $
--             newListLayerVersions
--
--         , requestTagResource $
--             newTagResource
--
--         , requestPublishVersion $
--             newPublishVersion
--
--         , requestListTags $
--             newListTags
--
--         , requestDeleteFunction $
--             newDeleteFunction
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestUpdateFunctionConfiguration $
--             newUpdateFunctionConfiguration
--
--         , requestListFunctions $
--             newListFunctions
--
--         , requestUpdateFunctionCode $
--             newUpdateFunctionCode
--
--         , requestDeleteProvisionedConcurrencyConfig $
--             newDeleteProvisionedConcurrencyConfig
--
--         , requestGetPolicy $
--             newGetPolicy
--
--         , requestPutProvisionedConcurrencyConfig $
--             newPutProvisionedConcurrencyConfig
--
--           ]

--     , testGroup "response"
--         [ responseGetFunctionConfiguration $
--             newFunctionConfiguration
--
--         , responseDeleteEventSourceMapping $
--             newEventSourceMappingConfiguration
--
--         , responseUpdateEventSourceMapping $
--             newEventSourceMappingConfiguration
--
--         , responseGetLayerVersion $
--             newGetLayerVersionResponse
--
--         , responseDeleteFunctionCodeSigningConfig $
--             newDeleteFunctionCodeSigningConfigResponse
--
--         , responsePutFunctionCodeSigningConfig $
--             newPutFunctionCodeSigningConfigResponse
--
--         , responseCreateAlias $
--             newAliasConfiguration
--
--         , responseListVersionsByFunction $
--             newListVersionsByFunctionResponse
--
--         , responseListAliases $
--             newListAliasesResponse
--
--         , responseDeleteCodeSigningConfig $
--             newDeleteCodeSigningConfigResponse
--
--         , responseUpdateCodeSigningConfig $
--             newUpdateCodeSigningConfigResponse
--
--         , responseRemovePermission $
--             newRemovePermissionResponse
--
--         , responseDeleteFunctionEventInvokeConfig $
--             newDeleteFunctionEventInvokeConfigResponse
--
--         , responseUpdateFunctionEventInvokeConfig $
--             newFunctionEventInvokeConfig
--
--         , responsePutFunctionEventInvokeConfig $
--             newFunctionEventInvokeConfig
--
--         , responseInvoke $
--             newInvokeResponse
--
--         , responseDeleteLayerVersion $
--             newDeleteLayerVersionResponse
--
--         , responseGetAlias $
--             newAliasConfiguration
--
--         , responsePublishLayerVersion $
--             newPublishLayerVersionResponse
--
--         , responseGetEventSourceMapping $
--             newEventSourceMappingConfiguration
--
--         , responseAddLayerVersionPermission $
--             newAddLayerVersionPermissionResponse
--
--         , responseListProvisionedConcurrencyConfigs $
--             newListProvisionedConcurrencyConfigsResponse
--
--         , responsePutFunctionConcurrency $
--             newConcurrency
--
--         , responseCreateFunction $
--             newFunctionConfiguration
--
--         , responseDeleteFunctionConcurrency $
--             newDeleteFunctionConcurrencyResponse
--
--         , responseGetLayerVersionByArn $
--             newGetLayerVersionResponse
--
--         , responseGetFunctionConcurrency $
--             newGetFunctionConcurrencyResponse
--
--         , responseCreateEventSourceMapping $
--             newEventSourceMappingConfiguration
--
--         , responseGetProvisionedConcurrencyConfig $
--             newGetProvisionedConcurrencyConfigResponse
--
--         , responseRemoveLayerVersionPermission $
--             newRemoveLayerVersionPermissionResponse
--
--         , responseListFunctionsByCodeSigningConfig $
--             newListFunctionsByCodeSigningConfigResponse
--
--         , responseGetFunction $
--             newGetFunctionResponse
--
--         , responseListEventSourceMappings $
--             newListEventSourceMappingsResponse
--
--         , responseGetLayerVersionPolicy $
--             newGetLayerVersionPolicyResponse
--
--         , responseDeleteAlias $
--             newDeleteAliasResponse
--
--         , responseUpdateAlias $
--             newAliasConfiguration
--
--         , responseGetAccountSettings $
--             newGetAccountSettingsResponse
--
--         , responseGetFunctionEventInvokeConfig $
--             newFunctionEventInvokeConfig
--
--         , responseGetCodeSigningConfig $
--             newGetCodeSigningConfigResponse
--
--         , responseAddPermission $
--             newAddPermissionResponse
--
--         , responseListLayers $
--             newListLayersResponse
--
--         , responseListFunctionEventInvokeConfigs $
--             newListFunctionEventInvokeConfigsResponse
--
--         , responseListCodeSigningConfigs $
--             newListCodeSigningConfigsResponse
--
--         , responseGetFunctionCodeSigningConfig $
--             newGetFunctionCodeSigningConfigResponse
--
--         , responseCreateCodeSigningConfig $
--             newCreateCodeSigningConfigResponse
--
--         , responseListLayerVersions $
--             newListLayerVersionsResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responsePublishVersion $
--             newFunctionConfiguration
--
--         , responseListTags $
--             newListTagsResponse
--
--         , responseDeleteFunction $
--             newDeleteFunctionResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseUpdateFunctionConfiguration $
--             newFunctionConfiguration
--
--         , responseListFunctions $
--             newListFunctionsResponse
--
--         , responseUpdateFunctionCode $
--             newFunctionConfiguration
--
--         , responseDeleteProvisionedConcurrencyConfig $
--             newDeleteProvisionedConcurrencyConfigResponse
--
--         , responseGetPolicy $
--             newGetPolicyResponse
--
--         , responsePutProvisionedConcurrencyConfig $
--             newPutProvisionedConcurrencyConfigResponse
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

requestGetLayerVersionByArn :: GetLayerVersionByArn -> TestTree
requestGetLayerVersionByArn =
  req
    "GetLayerVersionByArn"
    "fixture/GetLayerVersionByArn.yaml"

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
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetFunctionConfiguration)

responseDeleteEventSourceMapping :: EventSourceMappingConfiguration -> TestTree
responseDeleteEventSourceMapping =
  res
    "DeleteEventSourceMappingResponse"
    "fixture/DeleteEventSourceMappingResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteEventSourceMapping)

responseUpdateEventSourceMapping :: EventSourceMappingConfiguration -> TestTree
responseUpdateEventSourceMapping =
  res
    "UpdateEventSourceMappingResponse"
    "fixture/UpdateEventSourceMappingResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateEventSourceMapping)

responseGetLayerVersion :: GetLayerVersionResponse -> TestTree
responseGetLayerVersion =
  res
    "GetLayerVersionResponse"
    "fixture/GetLayerVersionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetLayerVersion)

responseDeleteFunctionCodeSigningConfig :: DeleteFunctionCodeSigningConfigResponse -> TestTree
responseDeleteFunctionCodeSigningConfig =
  res
    "DeleteFunctionCodeSigningConfigResponse"
    "fixture/DeleteFunctionCodeSigningConfigResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteFunctionCodeSigningConfig)

responsePutFunctionCodeSigningConfig :: PutFunctionCodeSigningConfigResponse -> TestTree
responsePutFunctionCodeSigningConfig =
  res
    "PutFunctionCodeSigningConfigResponse"
    "fixture/PutFunctionCodeSigningConfigResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutFunctionCodeSigningConfig)

responseCreateAlias :: AliasConfiguration -> TestTree
responseCreateAlias =
  res
    "CreateAliasResponse"
    "fixture/CreateAliasResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateAlias)

responseListVersionsByFunction :: ListVersionsByFunctionResponse -> TestTree
responseListVersionsByFunction =
  res
    "ListVersionsByFunctionResponse"
    "fixture/ListVersionsByFunctionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListVersionsByFunction)

responseListAliases :: ListAliasesResponse -> TestTree
responseListAliases =
  res
    "ListAliasesResponse"
    "fixture/ListAliasesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListAliases)

responseDeleteCodeSigningConfig :: DeleteCodeSigningConfigResponse -> TestTree
responseDeleteCodeSigningConfig =
  res
    "DeleteCodeSigningConfigResponse"
    "fixture/DeleteCodeSigningConfigResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteCodeSigningConfig)

responseUpdateCodeSigningConfig :: UpdateCodeSigningConfigResponse -> TestTree
responseUpdateCodeSigningConfig =
  res
    "UpdateCodeSigningConfigResponse"
    "fixture/UpdateCodeSigningConfigResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateCodeSigningConfig)

responseRemovePermission :: RemovePermissionResponse -> TestTree
responseRemovePermission =
  res
    "RemovePermissionResponse"
    "fixture/RemovePermissionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RemovePermission)

responseDeleteFunctionEventInvokeConfig :: DeleteFunctionEventInvokeConfigResponse -> TestTree
responseDeleteFunctionEventInvokeConfig =
  res
    "DeleteFunctionEventInvokeConfigResponse"
    "fixture/DeleteFunctionEventInvokeConfigResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteFunctionEventInvokeConfig)

responseUpdateFunctionEventInvokeConfig :: FunctionEventInvokeConfig -> TestTree
responseUpdateFunctionEventInvokeConfig =
  res
    "UpdateFunctionEventInvokeConfigResponse"
    "fixture/UpdateFunctionEventInvokeConfigResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateFunctionEventInvokeConfig)

responsePutFunctionEventInvokeConfig :: FunctionEventInvokeConfig -> TestTree
responsePutFunctionEventInvokeConfig =
  res
    "PutFunctionEventInvokeConfigResponse"
    "fixture/PutFunctionEventInvokeConfigResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutFunctionEventInvokeConfig)

responseInvoke :: InvokeResponse -> TestTree
responseInvoke =
  res
    "InvokeResponse"
    "fixture/InvokeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy Invoke)

responseDeleteLayerVersion :: DeleteLayerVersionResponse -> TestTree
responseDeleteLayerVersion =
  res
    "DeleteLayerVersionResponse"
    "fixture/DeleteLayerVersionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteLayerVersion)

responseGetAlias :: AliasConfiguration -> TestTree
responseGetAlias =
  res
    "GetAliasResponse"
    "fixture/GetAliasResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetAlias)

responsePublishLayerVersion :: PublishLayerVersionResponse -> TestTree
responsePublishLayerVersion =
  res
    "PublishLayerVersionResponse"
    "fixture/PublishLayerVersionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PublishLayerVersion)

responseGetEventSourceMapping :: EventSourceMappingConfiguration -> TestTree
responseGetEventSourceMapping =
  res
    "GetEventSourceMappingResponse"
    "fixture/GetEventSourceMappingResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetEventSourceMapping)

responseAddLayerVersionPermission :: AddLayerVersionPermissionResponse -> TestTree
responseAddLayerVersionPermission =
  res
    "AddLayerVersionPermissionResponse"
    "fixture/AddLayerVersionPermissionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AddLayerVersionPermission)

responseListProvisionedConcurrencyConfigs :: ListProvisionedConcurrencyConfigsResponse -> TestTree
responseListProvisionedConcurrencyConfigs =
  res
    "ListProvisionedConcurrencyConfigsResponse"
    "fixture/ListProvisionedConcurrencyConfigsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListProvisionedConcurrencyConfigs)

responsePutFunctionConcurrency :: Concurrency -> TestTree
responsePutFunctionConcurrency =
  res
    "PutFunctionConcurrencyResponse"
    "fixture/PutFunctionConcurrencyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutFunctionConcurrency)

responseCreateFunction :: FunctionConfiguration -> TestTree
responseCreateFunction =
  res
    "CreateFunctionResponse"
    "fixture/CreateFunctionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateFunction)

responseDeleteFunctionConcurrency :: DeleteFunctionConcurrencyResponse -> TestTree
responseDeleteFunctionConcurrency =
  res
    "DeleteFunctionConcurrencyResponse"
    "fixture/DeleteFunctionConcurrencyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteFunctionConcurrency)

responseGetLayerVersionByArn :: GetLayerVersionResponse -> TestTree
responseGetLayerVersionByArn =
  res
    "GetLayerVersionByArnResponse"
    "fixture/GetLayerVersionByArnResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetLayerVersionByArn)

responseGetFunctionConcurrency :: GetFunctionConcurrencyResponse -> TestTree
responseGetFunctionConcurrency =
  res
    "GetFunctionConcurrencyResponse"
    "fixture/GetFunctionConcurrencyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetFunctionConcurrency)

responseCreateEventSourceMapping :: EventSourceMappingConfiguration -> TestTree
responseCreateEventSourceMapping =
  res
    "CreateEventSourceMappingResponse"
    "fixture/CreateEventSourceMappingResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateEventSourceMapping)

responseGetProvisionedConcurrencyConfig :: GetProvisionedConcurrencyConfigResponse -> TestTree
responseGetProvisionedConcurrencyConfig =
  res
    "GetProvisionedConcurrencyConfigResponse"
    "fixture/GetProvisionedConcurrencyConfigResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetProvisionedConcurrencyConfig)

responseRemoveLayerVersionPermission :: RemoveLayerVersionPermissionResponse -> TestTree
responseRemoveLayerVersionPermission =
  res
    "RemoveLayerVersionPermissionResponse"
    "fixture/RemoveLayerVersionPermissionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RemoveLayerVersionPermission)

responseListFunctionsByCodeSigningConfig :: ListFunctionsByCodeSigningConfigResponse -> TestTree
responseListFunctionsByCodeSigningConfig =
  res
    "ListFunctionsByCodeSigningConfigResponse"
    "fixture/ListFunctionsByCodeSigningConfigResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListFunctionsByCodeSigningConfig)

responseGetFunction :: GetFunctionResponse -> TestTree
responseGetFunction =
  res
    "GetFunctionResponse"
    "fixture/GetFunctionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetFunction)

responseListEventSourceMappings :: ListEventSourceMappingsResponse -> TestTree
responseListEventSourceMappings =
  res
    "ListEventSourceMappingsResponse"
    "fixture/ListEventSourceMappingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListEventSourceMappings)

responseGetLayerVersionPolicy :: GetLayerVersionPolicyResponse -> TestTree
responseGetLayerVersionPolicy =
  res
    "GetLayerVersionPolicyResponse"
    "fixture/GetLayerVersionPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetLayerVersionPolicy)

responseDeleteAlias :: DeleteAliasResponse -> TestTree
responseDeleteAlias =
  res
    "DeleteAliasResponse"
    "fixture/DeleteAliasResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteAlias)

responseUpdateAlias :: AliasConfiguration -> TestTree
responseUpdateAlias =
  res
    "UpdateAliasResponse"
    "fixture/UpdateAliasResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateAlias)

responseGetAccountSettings :: GetAccountSettingsResponse -> TestTree
responseGetAccountSettings =
  res
    "GetAccountSettingsResponse"
    "fixture/GetAccountSettingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetAccountSettings)

responseGetFunctionEventInvokeConfig :: FunctionEventInvokeConfig -> TestTree
responseGetFunctionEventInvokeConfig =
  res
    "GetFunctionEventInvokeConfigResponse"
    "fixture/GetFunctionEventInvokeConfigResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetFunctionEventInvokeConfig)

responseGetCodeSigningConfig :: GetCodeSigningConfigResponse -> TestTree
responseGetCodeSigningConfig =
  res
    "GetCodeSigningConfigResponse"
    "fixture/GetCodeSigningConfigResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetCodeSigningConfig)

responseAddPermission :: AddPermissionResponse -> TestTree
responseAddPermission =
  res
    "AddPermissionResponse"
    "fixture/AddPermissionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AddPermission)

responseListLayers :: ListLayersResponse -> TestTree
responseListLayers =
  res
    "ListLayersResponse"
    "fixture/ListLayersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListLayers)

responseListFunctionEventInvokeConfigs :: ListFunctionEventInvokeConfigsResponse -> TestTree
responseListFunctionEventInvokeConfigs =
  res
    "ListFunctionEventInvokeConfigsResponse"
    "fixture/ListFunctionEventInvokeConfigsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListFunctionEventInvokeConfigs)

responseListCodeSigningConfigs :: ListCodeSigningConfigsResponse -> TestTree
responseListCodeSigningConfigs =
  res
    "ListCodeSigningConfigsResponse"
    "fixture/ListCodeSigningConfigsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListCodeSigningConfigs)

responseGetFunctionCodeSigningConfig :: GetFunctionCodeSigningConfigResponse -> TestTree
responseGetFunctionCodeSigningConfig =
  res
    "GetFunctionCodeSigningConfigResponse"
    "fixture/GetFunctionCodeSigningConfigResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetFunctionCodeSigningConfig)

responseCreateCodeSigningConfig :: CreateCodeSigningConfigResponse -> TestTree
responseCreateCodeSigningConfig =
  res
    "CreateCodeSigningConfigResponse"
    "fixture/CreateCodeSigningConfigResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateCodeSigningConfig)

responseListLayerVersions :: ListLayerVersionsResponse -> TestTree
responseListLayerVersions =
  res
    "ListLayerVersionsResponse"
    "fixture/ListLayerVersionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListLayerVersions)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TagResource)

responsePublishVersion :: FunctionConfiguration -> TestTree
responsePublishVersion =
  res
    "PublishVersionResponse"
    "fixture/PublishVersionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PublishVersion)

responseListTags :: ListTagsResponse -> TestTree
responseListTags =
  res
    "ListTagsResponse"
    "fixture/ListTagsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTags)

responseDeleteFunction :: DeleteFunctionResponse -> TestTree
responseDeleteFunction =
  res
    "DeleteFunctionResponse"
    "fixture/DeleteFunctionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteFunction)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UntagResource)

responseUpdateFunctionConfiguration :: FunctionConfiguration -> TestTree
responseUpdateFunctionConfiguration =
  res
    "UpdateFunctionConfigurationResponse"
    "fixture/UpdateFunctionConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateFunctionConfiguration)

responseListFunctions :: ListFunctionsResponse -> TestTree
responseListFunctions =
  res
    "ListFunctionsResponse"
    "fixture/ListFunctionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListFunctions)

responseUpdateFunctionCode :: FunctionConfiguration -> TestTree
responseUpdateFunctionCode =
  res
    "UpdateFunctionCodeResponse"
    "fixture/UpdateFunctionCodeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateFunctionCode)

responseDeleteProvisionedConcurrencyConfig :: DeleteProvisionedConcurrencyConfigResponse -> TestTree
responseDeleteProvisionedConcurrencyConfig =
  res
    "DeleteProvisionedConcurrencyConfigResponse"
    "fixture/DeleteProvisionedConcurrencyConfigResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteProvisionedConcurrencyConfig)

responseGetPolicy :: GetPolicyResponse -> TestTree
responseGetPolicy =
  res
    "GetPolicyResponse"
    "fixture/GetPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetPolicy)

responsePutProvisionedConcurrencyConfig :: PutProvisionedConcurrencyConfigResponse -> TestTree
responsePutProvisionedConcurrencyConfig =
  res
    "PutProvisionedConcurrencyConfigResponse"
    "fixture/PutProvisionedConcurrencyConfigResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutProvisionedConcurrencyConfig)
