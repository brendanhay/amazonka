{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.Lambda
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.Lambda where

import Amazonka.Lambda
import qualified Data.Proxy as Proxy
import Test.Amazonka.Fixture
import Test.Amazonka.Lambda.Internal
import Test.Amazonka.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestAddLayerVersionPermission $
--             newAddLayerVersionPermission
--
--         , requestAddPermission $
--             newAddPermission
--
--         , requestCreateAlias $
--             newCreateAlias
--
--         , requestCreateCodeSigningConfig $
--             newCreateCodeSigningConfig
--
--         , requestCreateEventSourceMapping $
--             newCreateEventSourceMapping
--
--         , requestCreateFunction $
--             newCreateFunction
--
--         , requestCreateFunctionUrlConfig $
--             newCreateFunctionUrlConfig
--
--         , requestDeleteAlias $
--             newDeleteAlias
--
--         , requestDeleteCodeSigningConfig $
--             newDeleteCodeSigningConfig
--
--         , requestDeleteEventSourceMapping $
--             newDeleteEventSourceMapping
--
--         , requestDeleteFunction $
--             newDeleteFunction
--
--         , requestDeleteFunctionCodeSigningConfig $
--             newDeleteFunctionCodeSigningConfig
--
--         , requestDeleteFunctionConcurrency $
--             newDeleteFunctionConcurrency
--
--         , requestDeleteFunctionEventInvokeConfig $
--             newDeleteFunctionEventInvokeConfig
--
--         , requestDeleteFunctionUrlConfig $
--             newDeleteFunctionUrlConfig
--
--         , requestDeleteLayerVersion $
--             newDeleteLayerVersion
--
--         , requestDeleteProvisionedConcurrencyConfig $
--             newDeleteProvisionedConcurrencyConfig
--
--         , requestGetAccountSettings $
--             newGetAccountSettings
--
--         , requestGetAlias $
--             newGetAlias
--
--         , requestGetCodeSigningConfig $
--             newGetCodeSigningConfig
--
--         , requestGetEventSourceMapping $
--             newGetEventSourceMapping
--
--         , requestGetFunction $
--             newGetFunction
--
--         , requestGetFunctionCodeSigningConfig $
--             newGetFunctionCodeSigningConfig
--
--         , requestGetFunctionConcurrency $
--             newGetFunctionConcurrency
--
--         , requestGetFunctionConfiguration $
--             newGetFunctionConfiguration
--
--         , requestGetFunctionEventInvokeConfig $
--             newGetFunctionEventInvokeConfig
--
--         , requestGetFunctionUrlConfig $
--             newGetFunctionUrlConfig
--
--         , requestGetLayerVersion $
--             newGetLayerVersion
--
--         , requestGetLayerVersionByArn $
--             newGetLayerVersionByArn
--
--         , requestGetLayerVersionPolicy $
--             newGetLayerVersionPolicy
--
--         , requestGetPolicy $
--             newGetPolicy
--
--         , requestGetProvisionedConcurrencyConfig $
--             newGetProvisionedConcurrencyConfig
--
--         , requestInvoke $
--             newInvoke
--
--         , requestListAliases $
--             newListAliases
--
--         , requestListCodeSigningConfigs $
--             newListCodeSigningConfigs
--
--         , requestListEventSourceMappings $
--             newListEventSourceMappings
--
--         , requestListFunctionEventInvokeConfigs $
--             newListFunctionEventInvokeConfigs
--
--         , requestListFunctionUrlConfigs $
--             newListFunctionUrlConfigs
--
--         , requestListFunctions $
--             newListFunctions
--
--         , requestListFunctionsByCodeSigningConfig $
--             newListFunctionsByCodeSigningConfig
--
--         , requestListLayerVersions $
--             newListLayerVersions
--
--         , requestListLayers $
--             newListLayers
--
--         , requestListProvisionedConcurrencyConfigs $
--             newListProvisionedConcurrencyConfigs
--
--         , requestListTags $
--             newListTags
--
--         , requestListVersionsByFunction $
--             newListVersionsByFunction
--
--         , requestPublishLayerVersion $
--             newPublishLayerVersion
--
--         , requestPublishVersion $
--             newPublishVersion
--
--         , requestPutFunctionCodeSigningConfig $
--             newPutFunctionCodeSigningConfig
--
--         , requestPutFunctionConcurrency $
--             newPutFunctionConcurrency
--
--         , requestPutFunctionEventInvokeConfig $
--             newPutFunctionEventInvokeConfig
--
--         , requestPutProvisionedConcurrencyConfig $
--             newPutProvisionedConcurrencyConfig
--
--         , requestRemoveLayerVersionPermission $
--             newRemoveLayerVersionPermission
--
--         , requestRemovePermission $
--             newRemovePermission
--
--         , requestTagResource $
--             newTagResource
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestUpdateAlias $
--             newUpdateAlias
--
--         , requestUpdateCodeSigningConfig $
--             newUpdateCodeSigningConfig
--
--         , requestUpdateEventSourceMapping $
--             newUpdateEventSourceMapping
--
--         , requestUpdateFunctionCode $
--             newUpdateFunctionCode
--
--         , requestUpdateFunctionConfiguration $
--             newUpdateFunctionConfiguration
--
--         , requestUpdateFunctionEventInvokeConfig $
--             newUpdateFunctionEventInvokeConfig
--
--         , requestUpdateFunctionUrlConfig $
--             newUpdateFunctionUrlConfig
--
--           ]

--     , testGroup "response"
--         [ responseAddLayerVersionPermission $
--             newAddLayerVersionPermissionResponse
--
--         , responseAddPermission $
--             newAddPermissionResponse
--
--         , responseCreateAlias $
--             newAliasConfiguration
--
--         , responseCreateCodeSigningConfig $
--             newCreateCodeSigningConfigResponse
--
--         , responseCreateEventSourceMapping $
--             newEventSourceMappingConfiguration
--
--         , responseCreateFunction $
--             newFunctionConfiguration
--
--         , responseCreateFunctionUrlConfig $
--             newCreateFunctionUrlConfigResponse
--
--         , responseDeleteAlias $
--             newDeleteAliasResponse
--
--         , responseDeleteCodeSigningConfig $
--             newDeleteCodeSigningConfigResponse
--
--         , responseDeleteEventSourceMapping $
--             newEventSourceMappingConfiguration
--
--         , responseDeleteFunction $
--             newDeleteFunctionResponse
--
--         , responseDeleteFunctionCodeSigningConfig $
--             newDeleteFunctionCodeSigningConfigResponse
--
--         , responseDeleteFunctionConcurrency $
--             newDeleteFunctionConcurrencyResponse
--
--         , responseDeleteFunctionEventInvokeConfig $
--             newDeleteFunctionEventInvokeConfigResponse
--
--         , responseDeleteFunctionUrlConfig $
--             newDeleteFunctionUrlConfigResponse
--
--         , responseDeleteLayerVersion $
--             newDeleteLayerVersionResponse
--
--         , responseDeleteProvisionedConcurrencyConfig $
--             newDeleteProvisionedConcurrencyConfigResponse
--
--         , responseGetAccountSettings $
--             newGetAccountSettingsResponse
--
--         , responseGetAlias $
--             newAliasConfiguration
--
--         , responseGetCodeSigningConfig $
--             newGetCodeSigningConfigResponse
--
--         , responseGetEventSourceMapping $
--             newEventSourceMappingConfiguration
--
--         , responseGetFunction $
--             newGetFunctionResponse
--
--         , responseGetFunctionCodeSigningConfig $
--             newGetFunctionCodeSigningConfigResponse
--
--         , responseGetFunctionConcurrency $
--             newGetFunctionConcurrencyResponse
--
--         , responseGetFunctionConfiguration $
--             newFunctionConfiguration
--
--         , responseGetFunctionEventInvokeConfig $
--             newFunctionEventInvokeConfig
--
--         , responseGetFunctionUrlConfig $
--             newGetFunctionUrlConfigResponse
--
--         , responseGetLayerVersion $
--             newGetLayerVersionResponse
--
--         , responseGetLayerVersionByArn $
--             newGetLayerVersionResponse
--
--         , responseGetLayerVersionPolicy $
--             newGetLayerVersionPolicyResponse
--
--         , responseGetPolicy $
--             newGetPolicyResponse
--
--         , responseGetProvisionedConcurrencyConfig $
--             newGetProvisionedConcurrencyConfigResponse
--
--         , responseInvoke $
--             newInvokeResponse
--
--         , responseListAliases $
--             newListAliasesResponse
--
--         , responseListCodeSigningConfigs $
--             newListCodeSigningConfigsResponse
--
--         , responseListEventSourceMappings $
--             newListEventSourceMappingsResponse
--
--         , responseListFunctionEventInvokeConfigs $
--             newListFunctionEventInvokeConfigsResponse
--
--         , responseListFunctionUrlConfigs $
--             newListFunctionUrlConfigsResponse
--
--         , responseListFunctions $
--             newListFunctionsResponse
--
--         , responseListFunctionsByCodeSigningConfig $
--             newListFunctionsByCodeSigningConfigResponse
--
--         , responseListLayerVersions $
--             newListLayerVersionsResponse
--
--         , responseListLayers $
--             newListLayersResponse
--
--         , responseListProvisionedConcurrencyConfigs $
--             newListProvisionedConcurrencyConfigsResponse
--
--         , responseListTags $
--             newListTagsResponse
--
--         , responseListVersionsByFunction $
--             newListVersionsByFunctionResponse
--
--         , responsePublishLayerVersion $
--             newPublishLayerVersionResponse
--
--         , responsePublishVersion $
--             newFunctionConfiguration
--
--         , responsePutFunctionCodeSigningConfig $
--             newPutFunctionCodeSigningConfigResponse
--
--         , responsePutFunctionConcurrency $
--             newConcurrency
--
--         , responsePutFunctionEventInvokeConfig $
--             newFunctionEventInvokeConfig
--
--         , responsePutProvisionedConcurrencyConfig $
--             newPutProvisionedConcurrencyConfigResponse
--
--         , responseRemoveLayerVersionPermission $
--             newRemoveLayerVersionPermissionResponse
--
--         , responseRemovePermission $
--             newRemovePermissionResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseUpdateAlias $
--             newAliasConfiguration
--
--         , responseUpdateCodeSigningConfig $
--             newUpdateCodeSigningConfigResponse
--
--         , responseUpdateEventSourceMapping $
--             newEventSourceMappingConfiguration
--
--         , responseUpdateFunctionCode $
--             newFunctionConfiguration
--
--         , responseUpdateFunctionConfiguration $
--             newFunctionConfiguration
--
--         , responseUpdateFunctionEventInvokeConfig $
--             newFunctionEventInvokeConfig
--
--         , responseUpdateFunctionUrlConfig $
--             newUpdateFunctionUrlConfigResponse
--
--           ]
--     ]

-- Requests

requestAddLayerVersionPermission :: AddLayerVersionPermission -> TestTree
requestAddLayerVersionPermission =
  req
    "AddLayerVersionPermission"
    "fixture/AddLayerVersionPermission.yaml"

requestAddPermission :: AddPermission -> TestTree
requestAddPermission =
  req
    "AddPermission"
    "fixture/AddPermission.yaml"

requestCreateAlias :: CreateAlias -> TestTree
requestCreateAlias =
  req
    "CreateAlias"
    "fixture/CreateAlias.yaml"

requestCreateCodeSigningConfig :: CreateCodeSigningConfig -> TestTree
requestCreateCodeSigningConfig =
  req
    "CreateCodeSigningConfig"
    "fixture/CreateCodeSigningConfig.yaml"

requestCreateEventSourceMapping :: CreateEventSourceMapping -> TestTree
requestCreateEventSourceMapping =
  req
    "CreateEventSourceMapping"
    "fixture/CreateEventSourceMapping.yaml"

requestCreateFunction :: CreateFunction -> TestTree
requestCreateFunction =
  req
    "CreateFunction"
    "fixture/CreateFunction.yaml"

requestCreateFunctionUrlConfig :: CreateFunctionUrlConfig -> TestTree
requestCreateFunctionUrlConfig =
  req
    "CreateFunctionUrlConfig"
    "fixture/CreateFunctionUrlConfig.yaml"

requestDeleteAlias :: DeleteAlias -> TestTree
requestDeleteAlias =
  req
    "DeleteAlias"
    "fixture/DeleteAlias.yaml"

requestDeleteCodeSigningConfig :: DeleteCodeSigningConfig -> TestTree
requestDeleteCodeSigningConfig =
  req
    "DeleteCodeSigningConfig"
    "fixture/DeleteCodeSigningConfig.yaml"

requestDeleteEventSourceMapping :: DeleteEventSourceMapping -> TestTree
requestDeleteEventSourceMapping =
  req
    "DeleteEventSourceMapping"
    "fixture/DeleteEventSourceMapping.yaml"

requestDeleteFunction :: DeleteFunction -> TestTree
requestDeleteFunction =
  req
    "DeleteFunction"
    "fixture/DeleteFunction.yaml"

requestDeleteFunctionCodeSigningConfig :: DeleteFunctionCodeSigningConfig -> TestTree
requestDeleteFunctionCodeSigningConfig =
  req
    "DeleteFunctionCodeSigningConfig"
    "fixture/DeleteFunctionCodeSigningConfig.yaml"

requestDeleteFunctionConcurrency :: DeleteFunctionConcurrency -> TestTree
requestDeleteFunctionConcurrency =
  req
    "DeleteFunctionConcurrency"
    "fixture/DeleteFunctionConcurrency.yaml"

requestDeleteFunctionEventInvokeConfig :: DeleteFunctionEventInvokeConfig -> TestTree
requestDeleteFunctionEventInvokeConfig =
  req
    "DeleteFunctionEventInvokeConfig"
    "fixture/DeleteFunctionEventInvokeConfig.yaml"

requestDeleteFunctionUrlConfig :: DeleteFunctionUrlConfig -> TestTree
requestDeleteFunctionUrlConfig =
  req
    "DeleteFunctionUrlConfig"
    "fixture/DeleteFunctionUrlConfig.yaml"

requestDeleteLayerVersion :: DeleteLayerVersion -> TestTree
requestDeleteLayerVersion =
  req
    "DeleteLayerVersion"
    "fixture/DeleteLayerVersion.yaml"

requestDeleteProvisionedConcurrencyConfig :: DeleteProvisionedConcurrencyConfig -> TestTree
requestDeleteProvisionedConcurrencyConfig =
  req
    "DeleteProvisionedConcurrencyConfig"
    "fixture/DeleteProvisionedConcurrencyConfig.yaml"

requestGetAccountSettings :: GetAccountSettings -> TestTree
requestGetAccountSettings =
  req
    "GetAccountSettings"
    "fixture/GetAccountSettings.yaml"

requestGetAlias :: GetAlias -> TestTree
requestGetAlias =
  req
    "GetAlias"
    "fixture/GetAlias.yaml"

requestGetCodeSigningConfig :: GetCodeSigningConfig -> TestTree
requestGetCodeSigningConfig =
  req
    "GetCodeSigningConfig"
    "fixture/GetCodeSigningConfig.yaml"

requestGetEventSourceMapping :: GetEventSourceMapping -> TestTree
requestGetEventSourceMapping =
  req
    "GetEventSourceMapping"
    "fixture/GetEventSourceMapping.yaml"

requestGetFunction :: GetFunction -> TestTree
requestGetFunction =
  req
    "GetFunction"
    "fixture/GetFunction.yaml"

requestGetFunctionCodeSigningConfig :: GetFunctionCodeSigningConfig -> TestTree
requestGetFunctionCodeSigningConfig =
  req
    "GetFunctionCodeSigningConfig"
    "fixture/GetFunctionCodeSigningConfig.yaml"

requestGetFunctionConcurrency :: GetFunctionConcurrency -> TestTree
requestGetFunctionConcurrency =
  req
    "GetFunctionConcurrency"
    "fixture/GetFunctionConcurrency.yaml"

requestGetFunctionConfiguration :: GetFunctionConfiguration -> TestTree
requestGetFunctionConfiguration =
  req
    "GetFunctionConfiguration"
    "fixture/GetFunctionConfiguration.yaml"

requestGetFunctionEventInvokeConfig :: GetFunctionEventInvokeConfig -> TestTree
requestGetFunctionEventInvokeConfig =
  req
    "GetFunctionEventInvokeConfig"
    "fixture/GetFunctionEventInvokeConfig.yaml"

requestGetFunctionUrlConfig :: GetFunctionUrlConfig -> TestTree
requestGetFunctionUrlConfig =
  req
    "GetFunctionUrlConfig"
    "fixture/GetFunctionUrlConfig.yaml"

requestGetLayerVersion :: GetLayerVersion -> TestTree
requestGetLayerVersion =
  req
    "GetLayerVersion"
    "fixture/GetLayerVersion.yaml"

requestGetLayerVersionByArn :: GetLayerVersionByArn -> TestTree
requestGetLayerVersionByArn =
  req
    "GetLayerVersionByArn"
    "fixture/GetLayerVersionByArn.yaml"

requestGetLayerVersionPolicy :: GetLayerVersionPolicy -> TestTree
requestGetLayerVersionPolicy =
  req
    "GetLayerVersionPolicy"
    "fixture/GetLayerVersionPolicy.yaml"

requestGetPolicy :: GetPolicy -> TestTree
requestGetPolicy =
  req
    "GetPolicy"
    "fixture/GetPolicy.yaml"

requestGetProvisionedConcurrencyConfig :: GetProvisionedConcurrencyConfig -> TestTree
requestGetProvisionedConcurrencyConfig =
  req
    "GetProvisionedConcurrencyConfig"
    "fixture/GetProvisionedConcurrencyConfig.yaml"

requestInvoke :: Invoke -> TestTree
requestInvoke =
  req
    "Invoke"
    "fixture/Invoke.yaml"

requestListAliases :: ListAliases -> TestTree
requestListAliases =
  req
    "ListAliases"
    "fixture/ListAliases.yaml"

requestListCodeSigningConfigs :: ListCodeSigningConfigs -> TestTree
requestListCodeSigningConfigs =
  req
    "ListCodeSigningConfigs"
    "fixture/ListCodeSigningConfigs.yaml"

requestListEventSourceMappings :: ListEventSourceMappings -> TestTree
requestListEventSourceMappings =
  req
    "ListEventSourceMappings"
    "fixture/ListEventSourceMappings.yaml"

requestListFunctionEventInvokeConfigs :: ListFunctionEventInvokeConfigs -> TestTree
requestListFunctionEventInvokeConfigs =
  req
    "ListFunctionEventInvokeConfigs"
    "fixture/ListFunctionEventInvokeConfigs.yaml"

requestListFunctionUrlConfigs :: ListFunctionUrlConfigs -> TestTree
requestListFunctionUrlConfigs =
  req
    "ListFunctionUrlConfigs"
    "fixture/ListFunctionUrlConfigs.yaml"

requestListFunctions :: ListFunctions -> TestTree
requestListFunctions =
  req
    "ListFunctions"
    "fixture/ListFunctions.yaml"

requestListFunctionsByCodeSigningConfig :: ListFunctionsByCodeSigningConfig -> TestTree
requestListFunctionsByCodeSigningConfig =
  req
    "ListFunctionsByCodeSigningConfig"
    "fixture/ListFunctionsByCodeSigningConfig.yaml"

requestListLayerVersions :: ListLayerVersions -> TestTree
requestListLayerVersions =
  req
    "ListLayerVersions"
    "fixture/ListLayerVersions.yaml"

requestListLayers :: ListLayers -> TestTree
requestListLayers =
  req
    "ListLayers"
    "fixture/ListLayers.yaml"

requestListProvisionedConcurrencyConfigs :: ListProvisionedConcurrencyConfigs -> TestTree
requestListProvisionedConcurrencyConfigs =
  req
    "ListProvisionedConcurrencyConfigs"
    "fixture/ListProvisionedConcurrencyConfigs.yaml"

requestListTags :: ListTags -> TestTree
requestListTags =
  req
    "ListTags"
    "fixture/ListTags.yaml"

requestListVersionsByFunction :: ListVersionsByFunction -> TestTree
requestListVersionsByFunction =
  req
    "ListVersionsByFunction"
    "fixture/ListVersionsByFunction.yaml"

requestPublishLayerVersion :: PublishLayerVersion -> TestTree
requestPublishLayerVersion =
  req
    "PublishLayerVersion"
    "fixture/PublishLayerVersion.yaml"

requestPublishVersion :: PublishVersion -> TestTree
requestPublishVersion =
  req
    "PublishVersion"
    "fixture/PublishVersion.yaml"

requestPutFunctionCodeSigningConfig :: PutFunctionCodeSigningConfig -> TestTree
requestPutFunctionCodeSigningConfig =
  req
    "PutFunctionCodeSigningConfig"
    "fixture/PutFunctionCodeSigningConfig.yaml"

requestPutFunctionConcurrency :: PutFunctionConcurrency -> TestTree
requestPutFunctionConcurrency =
  req
    "PutFunctionConcurrency"
    "fixture/PutFunctionConcurrency.yaml"

requestPutFunctionEventInvokeConfig :: PutFunctionEventInvokeConfig -> TestTree
requestPutFunctionEventInvokeConfig =
  req
    "PutFunctionEventInvokeConfig"
    "fixture/PutFunctionEventInvokeConfig.yaml"

requestPutProvisionedConcurrencyConfig :: PutProvisionedConcurrencyConfig -> TestTree
requestPutProvisionedConcurrencyConfig =
  req
    "PutProvisionedConcurrencyConfig"
    "fixture/PutProvisionedConcurrencyConfig.yaml"

requestRemoveLayerVersionPermission :: RemoveLayerVersionPermission -> TestTree
requestRemoveLayerVersionPermission =
  req
    "RemoveLayerVersionPermission"
    "fixture/RemoveLayerVersionPermission.yaml"

requestRemovePermission :: RemovePermission -> TestTree
requestRemovePermission =
  req
    "RemovePermission"
    "fixture/RemovePermission.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestUpdateAlias :: UpdateAlias -> TestTree
requestUpdateAlias =
  req
    "UpdateAlias"
    "fixture/UpdateAlias.yaml"

requestUpdateCodeSigningConfig :: UpdateCodeSigningConfig -> TestTree
requestUpdateCodeSigningConfig =
  req
    "UpdateCodeSigningConfig"
    "fixture/UpdateCodeSigningConfig.yaml"

requestUpdateEventSourceMapping :: UpdateEventSourceMapping -> TestTree
requestUpdateEventSourceMapping =
  req
    "UpdateEventSourceMapping"
    "fixture/UpdateEventSourceMapping.yaml"

requestUpdateFunctionCode :: UpdateFunctionCode -> TestTree
requestUpdateFunctionCode =
  req
    "UpdateFunctionCode"
    "fixture/UpdateFunctionCode.yaml"

requestUpdateFunctionConfiguration :: UpdateFunctionConfiguration -> TestTree
requestUpdateFunctionConfiguration =
  req
    "UpdateFunctionConfiguration"
    "fixture/UpdateFunctionConfiguration.yaml"

requestUpdateFunctionEventInvokeConfig :: UpdateFunctionEventInvokeConfig -> TestTree
requestUpdateFunctionEventInvokeConfig =
  req
    "UpdateFunctionEventInvokeConfig"
    "fixture/UpdateFunctionEventInvokeConfig.yaml"

requestUpdateFunctionUrlConfig :: UpdateFunctionUrlConfig -> TestTree
requestUpdateFunctionUrlConfig =
  req
    "UpdateFunctionUrlConfig"
    "fixture/UpdateFunctionUrlConfig.yaml"

-- Responses

responseAddLayerVersionPermission :: AddLayerVersionPermissionResponse -> TestTree
responseAddLayerVersionPermission =
  res
    "AddLayerVersionPermissionResponse"
    "fixture/AddLayerVersionPermissionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AddLayerVersionPermission)

responseAddPermission :: AddPermissionResponse -> TestTree
responseAddPermission =
  res
    "AddPermissionResponse"
    "fixture/AddPermissionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AddPermission)

responseCreateAlias :: AliasConfiguration -> TestTree
responseCreateAlias =
  res
    "CreateAliasResponse"
    "fixture/CreateAliasResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateAlias)

responseCreateCodeSigningConfig :: CreateCodeSigningConfigResponse -> TestTree
responseCreateCodeSigningConfig =
  res
    "CreateCodeSigningConfigResponse"
    "fixture/CreateCodeSigningConfigResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateCodeSigningConfig)

responseCreateEventSourceMapping :: EventSourceMappingConfiguration -> TestTree
responseCreateEventSourceMapping =
  res
    "CreateEventSourceMappingResponse"
    "fixture/CreateEventSourceMappingResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateEventSourceMapping)

responseCreateFunction :: FunctionConfiguration -> TestTree
responseCreateFunction =
  res
    "CreateFunctionResponse"
    "fixture/CreateFunctionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateFunction)

responseCreateFunctionUrlConfig :: CreateFunctionUrlConfigResponse -> TestTree
responseCreateFunctionUrlConfig =
  res
    "CreateFunctionUrlConfigResponse"
    "fixture/CreateFunctionUrlConfigResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateFunctionUrlConfig)

responseDeleteAlias :: DeleteAliasResponse -> TestTree
responseDeleteAlias =
  res
    "DeleteAliasResponse"
    "fixture/DeleteAliasResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteAlias)

responseDeleteCodeSigningConfig :: DeleteCodeSigningConfigResponse -> TestTree
responseDeleteCodeSigningConfig =
  res
    "DeleteCodeSigningConfigResponse"
    "fixture/DeleteCodeSigningConfigResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteCodeSigningConfig)

responseDeleteEventSourceMapping :: EventSourceMappingConfiguration -> TestTree
responseDeleteEventSourceMapping =
  res
    "DeleteEventSourceMappingResponse"
    "fixture/DeleteEventSourceMappingResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteEventSourceMapping)

responseDeleteFunction :: DeleteFunctionResponse -> TestTree
responseDeleteFunction =
  res
    "DeleteFunctionResponse"
    "fixture/DeleteFunctionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteFunction)

responseDeleteFunctionCodeSigningConfig :: DeleteFunctionCodeSigningConfigResponse -> TestTree
responseDeleteFunctionCodeSigningConfig =
  res
    "DeleteFunctionCodeSigningConfigResponse"
    "fixture/DeleteFunctionCodeSigningConfigResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteFunctionCodeSigningConfig)

responseDeleteFunctionConcurrency :: DeleteFunctionConcurrencyResponse -> TestTree
responseDeleteFunctionConcurrency =
  res
    "DeleteFunctionConcurrencyResponse"
    "fixture/DeleteFunctionConcurrencyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteFunctionConcurrency)

responseDeleteFunctionEventInvokeConfig :: DeleteFunctionEventInvokeConfigResponse -> TestTree
responseDeleteFunctionEventInvokeConfig =
  res
    "DeleteFunctionEventInvokeConfigResponse"
    "fixture/DeleteFunctionEventInvokeConfigResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteFunctionEventInvokeConfig)

responseDeleteFunctionUrlConfig :: DeleteFunctionUrlConfigResponse -> TestTree
responseDeleteFunctionUrlConfig =
  res
    "DeleteFunctionUrlConfigResponse"
    "fixture/DeleteFunctionUrlConfigResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteFunctionUrlConfig)

responseDeleteLayerVersion :: DeleteLayerVersionResponse -> TestTree
responseDeleteLayerVersion =
  res
    "DeleteLayerVersionResponse"
    "fixture/DeleteLayerVersionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteLayerVersion)

responseDeleteProvisionedConcurrencyConfig :: DeleteProvisionedConcurrencyConfigResponse -> TestTree
responseDeleteProvisionedConcurrencyConfig =
  res
    "DeleteProvisionedConcurrencyConfigResponse"
    "fixture/DeleteProvisionedConcurrencyConfigResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteProvisionedConcurrencyConfig)

responseGetAccountSettings :: GetAccountSettingsResponse -> TestTree
responseGetAccountSettings =
  res
    "GetAccountSettingsResponse"
    "fixture/GetAccountSettingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetAccountSettings)

responseGetAlias :: AliasConfiguration -> TestTree
responseGetAlias =
  res
    "GetAliasResponse"
    "fixture/GetAliasResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetAlias)

responseGetCodeSigningConfig :: GetCodeSigningConfigResponse -> TestTree
responseGetCodeSigningConfig =
  res
    "GetCodeSigningConfigResponse"
    "fixture/GetCodeSigningConfigResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetCodeSigningConfig)

responseGetEventSourceMapping :: EventSourceMappingConfiguration -> TestTree
responseGetEventSourceMapping =
  res
    "GetEventSourceMappingResponse"
    "fixture/GetEventSourceMappingResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetEventSourceMapping)

responseGetFunction :: GetFunctionResponse -> TestTree
responseGetFunction =
  res
    "GetFunctionResponse"
    "fixture/GetFunctionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetFunction)

responseGetFunctionCodeSigningConfig :: GetFunctionCodeSigningConfigResponse -> TestTree
responseGetFunctionCodeSigningConfig =
  res
    "GetFunctionCodeSigningConfigResponse"
    "fixture/GetFunctionCodeSigningConfigResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetFunctionCodeSigningConfig)

responseGetFunctionConcurrency :: GetFunctionConcurrencyResponse -> TestTree
responseGetFunctionConcurrency =
  res
    "GetFunctionConcurrencyResponse"
    "fixture/GetFunctionConcurrencyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetFunctionConcurrency)

responseGetFunctionConfiguration :: FunctionConfiguration -> TestTree
responseGetFunctionConfiguration =
  res
    "GetFunctionConfigurationResponse"
    "fixture/GetFunctionConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetFunctionConfiguration)

responseGetFunctionEventInvokeConfig :: FunctionEventInvokeConfig -> TestTree
responseGetFunctionEventInvokeConfig =
  res
    "GetFunctionEventInvokeConfigResponse"
    "fixture/GetFunctionEventInvokeConfigResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetFunctionEventInvokeConfig)

responseGetFunctionUrlConfig :: GetFunctionUrlConfigResponse -> TestTree
responseGetFunctionUrlConfig =
  res
    "GetFunctionUrlConfigResponse"
    "fixture/GetFunctionUrlConfigResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetFunctionUrlConfig)

responseGetLayerVersion :: GetLayerVersionResponse -> TestTree
responseGetLayerVersion =
  res
    "GetLayerVersionResponse"
    "fixture/GetLayerVersionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetLayerVersion)

responseGetLayerVersionByArn :: GetLayerVersionResponse -> TestTree
responseGetLayerVersionByArn =
  res
    "GetLayerVersionByArnResponse"
    "fixture/GetLayerVersionByArnResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetLayerVersionByArn)

responseGetLayerVersionPolicy :: GetLayerVersionPolicyResponse -> TestTree
responseGetLayerVersionPolicy =
  res
    "GetLayerVersionPolicyResponse"
    "fixture/GetLayerVersionPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetLayerVersionPolicy)

responseGetPolicy :: GetPolicyResponse -> TestTree
responseGetPolicy =
  res
    "GetPolicyResponse"
    "fixture/GetPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetPolicy)

responseGetProvisionedConcurrencyConfig :: GetProvisionedConcurrencyConfigResponse -> TestTree
responseGetProvisionedConcurrencyConfig =
  res
    "GetProvisionedConcurrencyConfigResponse"
    "fixture/GetProvisionedConcurrencyConfigResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetProvisionedConcurrencyConfig)

responseInvoke :: InvokeResponse -> TestTree
responseInvoke =
  res
    "InvokeResponse"
    "fixture/InvokeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy Invoke)

responseListAliases :: ListAliasesResponse -> TestTree
responseListAliases =
  res
    "ListAliasesResponse"
    "fixture/ListAliasesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListAliases)

responseListCodeSigningConfigs :: ListCodeSigningConfigsResponse -> TestTree
responseListCodeSigningConfigs =
  res
    "ListCodeSigningConfigsResponse"
    "fixture/ListCodeSigningConfigsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListCodeSigningConfigs)

responseListEventSourceMappings :: ListEventSourceMappingsResponse -> TestTree
responseListEventSourceMappings =
  res
    "ListEventSourceMappingsResponse"
    "fixture/ListEventSourceMappingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListEventSourceMappings)

responseListFunctionEventInvokeConfigs :: ListFunctionEventInvokeConfigsResponse -> TestTree
responseListFunctionEventInvokeConfigs =
  res
    "ListFunctionEventInvokeConfigsResponse"
    "fixture/ListFunctionEventInvokeConfigsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListFunctionEventInvokeConfigs)

responseListFunctionUrlConfigs :: ListFunctionUrlConfigsResponse -> TestTree
responseListFunctionUrlConfigs =
  res
    "ListFunctionUrlConfigsResponse"
    "fixture/ListFunctionUrlConfigsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListFunctionUrlConfigs)

responseListFunctions :: ListFunctionsResponse -> TestTree
responseListFunctions =
  res
    "ListFunctionsResponse"
    "fixture/ListFunctionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListFunctions)

responseListFunctionsByCodeSigningConfig :: ListFunctionsByCodeSigningConfigResponse -> TestTree
responseListFunctionsByCodeSigningConfig =
  res
    "ListFunctionsByCodeSigningConfigResponse"
    "fixture/ListFunctionsByCodeSigningConfigResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListFunctionsByCodeSigningConfig)

responseListLayerVersions :: ListLayerVersionsResponse -> TestTree
responseListLayerVersions =
  res
    "ListLayerVersionsResponse"
    "fixture/ListLayerVersionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListLayerVersions)

responseListLayers :: ListLayersResponse -> TestTree
responseListLayers =
  res
    "ListLayersResponse"
    "fixture/ListLayersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListLayers)

responseListProvisionedConcurrencyConfigs :: ListProvisionedConcurrencyConfigsResponse -> TestTree
responseListProvisionedConcurrencyConfigs =
  res
    "ListProvisionedConcurrencyConfigsResponse"
    "fixture/ListProvisionedConcurrencyConfigsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListProvisionedConcurrencyConfigs)

responseListTags :: ListTagsResponse -> TestTree
responseListTags =
  res
    "ListTagsResponse"
    "fixture/ListTagsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTags)

responseListVersionsByFunction :: ListVersionsByFunctionResponse -> TestTree
responseListVersionsByFunction =
  res
    "ListVersionsByFunctionResponse"
    "fixture/ListVersionsByFunctionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListVersionsByFunction)

responsePublishLayerVersion :: PublishLayerVersionResponse -> TestTree
responsePublishLayerVersion =
  res
    "PublishLayerVersionResponse"
    "fixture/PublishLayerVersionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PublishLayerVersion)

responsePublishVersion :: FunctionConfiguration -> TestTree
responsePublishVersion =
  res
    "PublishVersionResponse"
    "fixture/PublishVersionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PublishVersion)

responsePutFunctionCodeSigningConfig :: PutFunctionCodeSigningConfigResponse -> TestTree
responsePutFunctionCodeSigningConfig =
  res
    "PutFunctionCodeSigningConfigResponse"
    "fixture/PutFunctionCodeSigningConfigResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutFunctionCodeSigningConfig)

responsePutFunctionConcurrency :: Concurrency -> TestTree
responsePutFunctionConcurrency =
  res
    "PutFunctionConcurrencyResponse"
    "fixture/PutFunctionConcurrencyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutFunctionConcurrency)

responsePutFunctionEventInvokeConfig :: FunctionEventInvokeConfig -> TestTree
responsePutFunctionEventInvokeConfig =
  res
    "PutFunctionEventInvokeConfigResponse"
    "fixture/PutFunctionEventInvokeConfigResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutFunctionEventInvokeConfig)

responsePutProvisionedConcurrencyConfig :: PutProvisionedConcurrencyConfigResponse -> TestTree
responsePutProvisionedConcurrencyConfig =
  res
    "PutProvisionedConcurrencyConfigResponse"
    "fixture/PutProvisionedConcurrencyConfigResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutProvisionedConcurrencyConfig)

responseRemoveLayerVersionPermission :: RemoveLayerVersionPermissionResponse -> TestTree
responseRemoveLayerVersionPermission =
  res
    "RemoveLayerVersionPermissionResponse"
    "fixture/RemoveLayerVersionPermissionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RemoveLayerVersionPermission)

responseRemovePermission :: RemovePermissionResponse -> TestTree
responseRemovePermission =
  res
    "RemovePermissionResponse"
    "fixture/RemovePermissionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RemovePermission)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TagResource)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UntagResource)

responseUpdateAlias :: AliasConfiguration -> TestTree
responseUpdateAlias =
  res
    "UpdateAliasResponse"
    "fixture/UpdateAliasResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateAlias)

responseUpdateCodeSigningConfig :: UpdateCodeSigningConfigResponse -> TestTree
responseUpdateCodeSigningConfig =
  res
    "UpdateCodeSigningConfigResponse"
    "fixture/UpdateCodeSigningConfigResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateCodeSigningConfig)

responseUpdateEventSourceMapping :: EventSourceMappingConfiguration -> TestTree
responseUpdateEventSourceMapping =
  res
    "UpdateEventSourceMappingResponse"
    "fixture/UpdateEventSourceMappingResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateEventSourceMapping)

responseUpdateFunctionCode :: FunctionConfiguration -> TestTree
responseUpdateFunctionCode =
  res
    "UpdateFunctionCodeResponse"
    "fixture/UpdateFunctionCodeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateFunctionCode)

responseUpdateFunctionConfiguration :: FunctionConfiguration -> TestTree
responseUpdateFunctionConfiguration =
  res
    "UpdateFunctionConfigurationResponse"
    "fixture/UpdateFunctionConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateFunctionConfiguration)

responseUpdateFunctionEventInvokeConfig :: FunctionEventInvokeConfig -> TestTree
responseUpdateFunctionEventInvokeConfig =
  res
    "UpdateFunctionEventInvokeConfigResponse"
    "fixture/UpdateFunctionEventInvokeConfigResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateFunctionEventInvokeConfig)

responseUpdateFunctionUrlConfig :: UpdateFunctionUrlConfigResponse -> TestTree
responseUpdateFunctionUrlConfig =
  res
    "UpdateFunctionUrlConfigResponse"
    "fixture/UpdateFunctionUrlConfigResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateFunctionUrlConfig)
