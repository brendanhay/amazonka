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
--         [ requestDeleteAlias $
--             newDeleteAlias
--
--         , requestPutFunctionCodeSigningConfig $
--             newPutFunctionCodeSigningConfig
--
--         , requestGetLayerVersionPolicy $
--             newGetLayerVersionPolicy
--
--         , requestUpdateAlias $
--             newUpdateAlias
--
--         , requestGetFunctionConfiguration $
--             newGetFunctionConfiguration
--
--         , requestUpdateEventSourceMapping $
--             newUpdateEventSourceMapping
--
--         , requestGetFunction $
--             newGetFunction
--
--         , requestListEventSourceMappings $
--             newListEventSourceMappings
--
--         , requestDeleteEventSourceMapping $
--             newDeleteEventSourceMapping
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
--         , requestDeleteFunctionConcurrency $
--             newDeleteFunctionConcurrency
--
--         , requestListProvisionedConcurrencyConfigs $
--             newListProvisionedConcurrencyConfigs
--
--         , requestDeleteProvisionedConcurrencyConfig $
--             newDeleteProvisionedConcurrencyConfig
--
--         , requestUpdateFunctionCode $
--             newUpdateFunctionCode
--
--         , requestUpdateFunctionConfiguration $
--             newUpdateFunctionConfiguration
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestListFunctions $
--             newListFunctions
--
--         , requestListLayerVersions $
--             newListLayerVersions
--
--         , requestInvoke $
--             newInvoke
--
--         , requestDeleteLayerVersion $
--             newDeleteLayerVersion
--
--         , requestCreateCodeSigningConfig $
--             newCreateCodeSigningConfig
--
--         , requestTagResource $
--             newTagResource
--
--         , requestGetAlias $
--             newGetAlias
--
--         , requestDeleteCodeSigningConfig $
--             newDeleteCodeSigningConfig
--
--         , requestUpdateCodeSigningConfig $
--             newUpdateCodeSigningConfig
--
--         , requestListCodeSigningConfigs $
--             newListCodeSigningConfigs
--
--         , requestDeleteFunctionEventInvokeConfig $
--             newDeleteFunctionEventInvokeConfig
--
--         , requestUpdateFunctionEventInvokeConfig $
--             newUpdateFunctionEventInvokeConfig
--
--         , requestListFunctionEventInvokeConfigs $
--             newListFunctionEventInvokeConfigs
--
--         , requestListVersionsByFunction $
--             newListVersionsByFunction
--
--         , requestAddPermission $
--             newAddPermission
--
--         , requestGetLayerVersion $
--             newGetLayerVersion
--
--         , requestDeleteFunctionCodeSigningConfig $
--             newDeleteFunctionCodeSigningConfig
--
--         , requestListFunctionsByCodeSigningConfig $
--             newListFunctionsByCodeSigningConfig
--
--         , requestRemoveLayerVersionPermission $
--             newRemoveLayerVersionPermission
--
--         , requestGetProvisionedConcurrencyConfig $
--             newGetProvisionedConcurrencyConfig
--
--         , requestCreateFunction $
--             newCreateFunction
--
--         , requestPutFunctionConcurrency $
--             newPutFunctionConcurrency
--
--         , requestGetPolicy $
--             newGetPolicy
--
--         , requestPutProvisionedConcurrencyConfig $
--             newPutProvisionedConcurrencyConfig
--
--         , requestAddLayerVersionPermission $
--             newAddLayerVersionPermission
--
--         , requestPublishVersion $
--             newPublishVersion
--
--         , requestDeleteFunction $
--             newDeleteFunction
--
--         , requestGetEventSourceMapping $
--             newGetEventSourceMapping
--
--         , requestListTags $
--             newListTags
--
--         , requestPublishLayerVersion $
--             newPublishLayerVersion
--
--         , requestGetFunctionCodeSigningConfig $
--             newGetFunctionCodeSigningConfig
--
--         , requestPutFunctionEventInvokeConfig $
--             newPutFunctionEventInvokeConfig
--
--         , requestRemovePermission $
--             newRemovePermission
--
--         , requestCreateAlias $
--             newCreateAlias
--
--         , requestGetCodeSigningConfig $
--             newGetCodeSigningConfig
--
--         , requestGetFunctionEventInvokeConfig $
--             newGetFunctionEventInvokeConfig
--
--         , requestListLayers $
--             newListLayers
--
--         , requestListAliases $
--             newListAliases
--
--         , requestGetAccountSettings $
--             newGetAccountSettings
--
--           ]

--     , testGroup "response"
--         [ responseDeleteAlias $
--             newDeleteAliasResponse
--
--         , responsePutFunctionCodeSigningConfig $
--             newPutFunctionCodeSigningConfigResponse
--
--         , responseGetLayerVersionPolicy $
--             newGetLayerVersionPolicyResponse
--
--         , responseUpdateAlias $
--             newAliasConfiguration
--
--         , responseGetFunctionConfiguration $
--             newFunctionConfiguration
--
--         , responseUpdateEventSourceMapping $
--             newEventSourceMappingConfiguration
--
--         , responseGetFunction $
--             newGetFunctionResponse
--
--         , responseListEventSourceMappings $
--             newListEventSourceMappingsResponse
--
--         , responseDeleteEventSourceMapping $
--             newEventSourceMappingConfiguration
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
--         , responseDeleteFunctionConcurrency $
--             newDeleteFunctionConcurrencyResponse
--
--         , responseListProvisionedConcurrencyConfigs $
--             newListProvisionedConcurrencyConfigsResponse
--
--         , responseDeleteProvisionedConcurrencyConfig $
--             newDeleteProvisionedConcurrencyConfigResponse
--
--         , responseUpdateFunctionCode $
--             newFunctionConfiguration
--
--         , responseUpdateFunctionConfiguration $
--             newFunctionConfiguration
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseListFunctions $
--             newListFunctionsResponse
--
--         , responseListLayerVersions $
--             newListLayerVersionsResponse
--
--         , responseInvoke $
--             newInvokeResponse
--
--         , responseDeleteLayerVersion $
--             newDeleteLayerVersionResponse
--
--         , responseCreateCodeSigningConfig $
--             newCreateCodeSigningConfigResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseGetAlias $
--             newAliasConfiguration
--
--         , responseDeleteCodeSigningConfig $
--             newDeleteCodeSigningConfigResponse
--
--         , responseUpdateCodeSigningConfig $
--             newUpdateCodeSigningConfigResponse
--
--         , responseListCodeSigningConfigs $
--             newListCodeSigningConfigsResponse
--
--         , responseDeleteFunctionEventInvokeConfig $
--             newDeleteFunctionEventInvokeConfigResponse
--
--         , responseUpdateFunctionEventInvokeConfig $
--             newFunctionEventInvokeConfig
--
--         , responseListFunctionEventInvokeConfigs $
--             newListFunctionEventInvokeConfigsResponse
--
--         , responseListVersionsByFunction $
--             newListVersionsByFunctionResponse
--
--         , responseAddPermission $
--             newAddPermissionResponse
--
--         , responseGetLayerVersion $
--             newGetLayerVersionResponse
--
--         , responseDeleteFunctionCodeSigningConfig $
--             newDeleteFunctionCodeSigningConfigResponse
--
--         , responseListFunctionsByCodeSigningConfig $
--             newListFunctionsByCodeSigningConfigResponse
--
--         , responseRemoveLayerVersionPermission $
--             newRemoveLayerVersionPermissionResponse
--
--         , responseGetProvisionedConcurrencyConfig $
--             newGetProvisionedConcurrencyConfigResponse
--
--         , responseCreateFunction $
--             newFunctionConfiguration
--
--         , responsePutFunctionConcurrency $
--             newConcurrency
--
--         , responseGetPolicy $
--             newGetPolicyResponse
--
--         , responsePutProvisionedConcurrencyConfig $
--             newPutProvisionedConcurrencyConfigResponse
--
--         , responseAddLayerVersionPermission $
--             newAddLayerVersionPermissionResponse
--
--         , responsePublishVersion $
--             newFunctionConfiguration
--
--         , responseDeleteFunction $
--             newDeleteFunctionResponse
--
--         , responseGetEventSourceMapping $
--             newEventSourceMappingConfiguration
--
--         , responseListTags $
--             newListTagsResponse
--
--         , responsePublishLayerVersion $
--             newPublishLayerVersionResponse
--
--         , responseGetFunctionCodeSigningConfig $
--             newGetFunctionCodeSigningConfigResponse
--
--         , responsePutFunctionEventInvokeConfig $
--             newFunctionEventInvokeConfig
--
--         , responseRemovePermission $
--             newRemovePermissionResponse
--
--         , responseCreateAlias $
--             newAliasConfiguration
--
--         , responseGetCodeSigningConfig $
--             newGetCodeSigningConfigResponse
--
--         , responseGetFunctionEventInvokeConfig $
--             newFunctionEventInvokeConfig
--
--         , responseListLayers $
--             newListLayersResponse
--
--         , responseListAliases $
--             newListAliasesResponse
--
--         , responseGetAccountSettings $
--             newGetAccountSettingsResponse
--
--           ]
--     ]

-- Requests

requestDeleteAlias :: DeleteAlias -> TestTree
requestDeleteAlias =
  req
    "DeleteAlias"
    "fixture/DeleteAlias.yaml"

requestPutFunctionCodeSigningConfig :: PutFunctionCodeSigningConfig -> TestTree
requestPutFunctionCodeSigningConfig =
  req
    "PutFunctionCodeSigningConfig"
    "fixture/PutFunctionCodeSigningConfig.yaml"

requestGetLayerVersionPolicy :: GetLayerVersionPolicy -> TestTree
requestGetLayerVersionPolicy =
  req
    "GetLayerVersionPolicy"
    "fixture/GetLayerVersionPolicy.yaml"

requestUpdateAlias :: UpdateAlias -> TestTree
requestUpdateAlias =
  req
    "UpdateAlias"
    "fixture/UpdateAlias.yaml"

requestGetFunctionConfiguration :: GetFunctionConfiguration -> TestTree
requestGetFunctionConfiguration =
  req
    "GetFunctionConfiguration"
    "fixture/GetFunctionConfiguration.yaml"

requestUpdateEventSourceMapping :: UpdateEventSourceMapping -> TestTree
requestUpdateEventSourceMapping =
  req
    "UpdateEventSourceMapping"
    "fixture/UpdateEventSourceMapping.yaml"

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

requestDeleteEventSourceMapping :: DeleteEventSourceMapping -> TestTree
requestDeleteEventSourceMapping =
  req
    "DeleteEventSourceMapping"
    "fixture/DeleteEventSourceMapping.yaml"

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

requestDeleteFunctionConcurrency :: DeleteFunctionConcurrency -> TestTree
requestDeleteFunctionConcurrency =
  req
    "DeleteFunctionConcurrency"
    "fixture/DeleteFunctionConcurrency.yaml"

requestListProvisionedConcurrencyConfigs :: ListProvisionedConcurrencyConfigs -> TestTree
requestListProvisionedConcurrencyConfigs =
  req
    "ListProvisionedConcurrencyConfigs"
    "fixture/ListProvisionedConcurrencyConfigs.yaml"

requestDeleteProvisionedConcurrencyConfig :: DeleteProvisionedConcurrencyConfig -> TestTree
requestDeleteProvisionedConcurrencyConfig =
  req
    "DeleteProvisionedConcurrencyConfig"
    "fixture/DeleteProvisionedConcurrencyConfig.yaml"

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

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestListFunctions :: ListFunctions -> TestTree
requestListFunctions =
  req
    "ListFunctions"
    "fixture/ListFunctions.yaml"

requestListLayerVersions :: ListLayerVersions -> TestTree
requestListLayerVersions =
  req
    "ListLayerVersions"
    "fixture/ListLayerVersions.yaml"

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

requestCreateCodeSigningConfig :: CreateCodeSigningConfig -> TestTree
requestCreateCodeSigningConfig =
  req
    "CreateCodeSigningConfig"
    "fixture/CreateCodeSigningConfig.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestGetAlias :: GetAlias -> TestTree
requestGetAlias =
  req
    "GetAlias"
    "fixture/GetAlias.yaml"

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

requestListCodeSigningConfigs :: ListCodeSigningConfigs -> TestTree
requestListCodeSigningConfigs =
  req
    "ListCodeSigningConfigs"
    "fixture/ListCodeSigningConfigs.yaml"

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

requestListFunctionEventInvokeConfigs :: ListFunctionEventInvokeConfigs -> TestTree
requestListFunctionEventInvokeConfigs =
  req
    "ListFunctionEventInvokeConfigs"
    "fixture/ListFunctionEventInvokeConfigs.yaml"

requestListVersionsByFunction :: ListVersionsByFunction -> TestTree
requestListVersionsByFunction =
  req
    "ListVersionsByFunction"
    "fixture/ListVersionsByFunction.yaml"

requestAddPermission :: AddPermission -> TestTree
requestAddPermission =
  req
    "AddPermission"
    "fixture/AddPermission.yaml"

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

requestListFunctionsByCodeSigningConfig :: ListFunctionsByCodeSigningConfig -> TestTree
requestListFunctionsByCodeSigningConfig =
  req
    "ListFunctionsByCodeSigningConfig"
    "fixture/ListFunctionsByCodeSigningConfig.yaml"

requestRemoveLayerVersionPermission :: RemoveLayerVersionPermission -> TestTree
requestRemoveLayerVersionPermission =
  req
    "RemoveLayerVersionPermission"
    "fixture/RemoveLayerVersionPermission.yaml"

requestGetProvisionedConcurrencyConfig :: GetProvisionedConcurrencyConfig -> TestTree
requestGetProvisionedConcurrencyConfig =
  req
    "GetProvisionedConcurrencyConfig"
    "fixture/GetProvisionedConcurrencyConfig.yaml"

requestCreateFunction :: CreateFunction -> TestTree
requestCreateFunction =
  req
    "CreateFunction"
    "fixture/CreateFunction.yaml"

requestPutFunctionConcurrency :: PutFunctionConcurrency -> TestTree
requestPutFunctionConcurrency =
  req
    "PutFunctionConcurrency"
    "fixture/PutFunctionConcurrency.yaml"

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

requestAddLayerVersionPermission :: AddLayerVersionPermission -> TestTree
requestAddLayerVersionPermission =
  req
    "AddLayerVersionPermission"
    "fixture/AddLayerVersionPermission.yaml"

requestPublishVersion :: PublishVersion -> TestTree
requestPublishVersion =
  req
    "PublishVersion"
    "fixture/PublishVersion.yaml"

requestDeleteFunction :: DeleteFunction -> TestTree
requestDeleteFunction =
  req
    "DeleteFunction"
    "fixture/DeleteFunction.yaml"

requestGetEventSourceMapping :: GetEventSourceMapping -> TestTree
requestGetEventSourceMapping =
  req
    "GetEventSourceMapping"
    "fixture/GetEventSourceMapping.yaml"

requestListTags :: ListTags -> TestTree
requestListTags =
  req
    "ListTags"
    "fixture/ListTags.yaml"

requestPublishLayerVersion :: PublishLayerVersion -> TestTree
requestPublishLayerVersion =
  req
    "PublishLayerVersion"
    "fixture/PublishLayerVersion.yaml"

requestGetFunctionCodeSigningConfig :: GetFunctionCodeSigningConfig -> TestTree
requestGetFunctionCodeSigningConfig =
  req
    "GetFunctionCodeSigningConfig"
    "fixture/GetFunctionCodeSigningConfig.yaml"

requestPutFunctionEventInvokeConfig :: PutFunctionEventInvokeConfig -> TestTree
requestPutFunctionEventInvokeConfig =
  req
    "PutFunctionEventInvokeConfig"
    "fixture/PutFunctionEventInvokeConfig.yaml"

requestRemovePermission :: RemovePermission -> TestTree
requestRemovePermission =
  req
    "RemovePermission"
    "fixture/RemovePermission.yaml"

requestCreateAlias :: CreateAlias -> TestTree
requestCreateAlias =
  req
    "CreateAlias"
    "fixture/CreateAlias.yaml"

requestGetCodeSigningConfig :: GetCodeSigningConfig -> TestTree
requestGetCodeSigningConfig =
  req
    "GetCodeSigningConfig"
    "fixture/GetCodeSigningConfig.yaml"

requestGetFunctionEventInvokeConfig :: GetFunctionEventInvokeConfig -> TestTree
requestGetFunctionEventInvokeConfig =
  req
    "GetFunctionEventInvokeConfig"
    "fixture/GetFunctionEventInvokeConfig.yaml"

requestListLayers :: ListLayers -> TestTree
requestListLayers =
  req
    "ListLayers"
    "fixture/ListLayers.yaml"

requestListAliases :: ListAliases -> TestTree
requestListAliases =
  req
    "ListAliases"
    "fixture/ListAliases.yaml"

requestGetAccountSettings :: GetAccountSettings -> TestTree
requestGetAccountSettings =
  req
    "GetAccountSettings"
    "fixture/GetAccountSettings.yaml"

-- Responses

responseDeleteAlias :: DeleteAliasResponse -> TestTree
responseDeleteAlias =
  res
    "DeleteAliasResponse"
    "fixture/DeleteAliasResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteAlias)

responsePutFunctionCodeSigningConfig :: PutFunctionCodeSigningConfigResponse -> TestTree
responsePutFunctionCodeSigningConfig =
  res
    "PutFunctionCodeSigningConfigResponse"
    "fixture/PutFunctionCodeSigningConfigResponse.proto"
    defaultService
    (Proxy :: Proxy PutFunctionCodeSigningConfig)

responseGetLayerVersionPolicy :: GetLayerVersionPolicyResponse -> TestTree
responseGetLayerVersionPolicy =
  res
    "GetLayerVersionPolicyResponse"
    "fixture/GetLayerVersionPolicyResponse.proto"
    defaultService
    (Proxy :: Proxy GetLayerVersionPolicy)

responseUpdateAlias :: AliasConfiguration -> TestTree
responseUpdateAlias =
  res
    "UpdateAliasResponse"
    "fixture/UpdateAliasResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateAlias)

responseGetFunctionConfiguration :: FunctionConfiguration -> TestTree
responseGetFunctionConfiguration =
  res
    "GetFunctionConfigurationResponse"
    "fixture/GetFunctionConfigurationResponse.proto"
    defaultService
    (Proxy :: Proxy GetFunctionConfiguration)

responseUpdateEventSourceMapping :: EventSourceMappingConfiguration -> TestTree
responseUpdateEventSourceMapping =
  res
    "UpdateEventSourceMappingResponse"
    "fixture/UpdateEventSourceMappingResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateEventSourceMapping)

responseGetFunction :: GetFunctionResponse -> TestTree
responseGetFunction =
  res
    "GetFunctionResponse"
    "fixture/GetFunctionResponse.proto"
    defaultService
    (Proxy :: Proxy GetFunction)

responseListEventSourceMappings :: ListEventSourceMappingsResponse -> TestTree
responseListEventSourceMappings =
  res
    "ListEventSourceMappingsResponse"
    "fixture/ListEventSourceMappingsResponse.proto"
    defaultService
    (Proxy :: Proxy ListEventSourceMappings)

responseDeleteEventSourceMapping :: EventSourceMappingConfiguration -> TestTree
responseDeleteEventSourceMapping =
  res
    "DeleteEventSourceMappingResponse"
    "fixture/DeleteEventSourceMappingResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteEventSourceMapping)

responseGetLayerVersionByArn :: GetLayerVersionResponse -> TestTree
responseGetLayerVersionByArn =
  res
    "GetLayerVersionByArnResponse"
    "fixture/GetLayerVersionByArnResponse.proto"
    defaultService
    (Proxy :: Proxy GetLayerVersionByArn)

responseGetFunctionConcurrency :: GetFunctionConcurrencyResponse -> TestTree
responseGetFunctionConcurrency =
  res
    "GetFunctionConcurrencyResponse"
    "fixture/GetFunctionConcurrencyResponse.proto"
    defaultService
    (Proxy :: Proxy GetFunctionConcurrency)

responseCreateEventSourceMapping :: EventSourceMappingConfiguration -> TestTree
responseCreateEventSourceMapping =
  res
    "CreateEventSourceMappingResponse"
    "fixture/CreateEventSourceMappingResponse.proto"
    defaultService
    (Proxy :: Proxy CreateEventSourceMapping)

responseDeleteFunctionConcurrency :: DeleteFunctionConcurrencyResponse -> TestTree
responseDeleteFunctionConcurrency =
  res
    "DeleteFunctionConcurrencyResponse"
    "fixture/DeleteFunctionConcurrencyResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteFunctionConcurrency)

responseListProvisionedConcurrencyConfigs :: ListProvisionedConcurrencyConfigsResponse -> TestTree
responseListProvisionedConcurrencyConfigs =
  res
    "ListProvisionedConcurrencyConfigsResponse"
    "fixture/ListProvisionedConcurrencyConfigsResponse.proto"
    defaultService
    (Proxy :: Proxy ListProvisionedConcurrencyConfigs)

responseDeleteProvisionedConcurrencyConfig :: DeleteProvisionedConcurrencyConfigResponse -> TestTree
responseDeleteProvisionedConcurrencyConfig =
  res
    "DeleteProvisionedConcurrencyConfigResponse"
    "fixture/DeleteProvisionedConcurrencyConfigResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteProvisionedConcurrencyConfig)

responseUpdateFunctionCode :: FunctionConfiguration -> TestTree
responseUpdateFunctionCode =
  res
    "UpdateFunctionCodeResponse"
    "fixture/UpdateFunctionCodeResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateFunctionCode)

responseUpdateFunctionConfiguration :: FunctionConfiguration -> TestTree
responseUpdateFunctionConfiguration =
  res
    "UpdateFunctionConfigurationResponse"
    "fixture/UpdateFunctionConfigurationResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateFunctionConfiguration)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy :: Proxy UntagResource)

responseListFunctions :: ListFunctionsResponse -> TestTree
responseListFunctions =
  res
    "ListFunctionsResponse"
    "fixture/ListFunctionsResponse.proto"
    defaultService
    (Proxy :: Proxy ListFunctions)

responseListLayerVersions :: ListLayerVersionsResponse -> TestTree
responseListLayerVersions =
  res
    "ListLayerVersionsResponse"
    "fixture/ListLayerVersionsResponse.proto"
    defaultService
    (Proxy :: Proxy ListLayerVersions)

responseInvoke :: InvokeResponse -> TestTree
responseInvoke =
  res
    "InvokeResponse"
    "fixture/InvokeResponse.proto"
    defaultService
    (Proxy :: Proxy Invoke)

responseDeleteLayerVersion :: DeleteLayerVersionResponse -> TestTree
responseDeleteLayerVersion =
  res
    "DeleteLayerVersionResponse"
    "fixture/DeleteLayerVersionResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteLayerVersion)

responseCreateCodeSigningConfig :: CreateCodeSigningConfigResponse -> TestTree
responseCreateCodeSigningConfig =
  res
    "CreateCodeSigningConfigResponse"
    "fixture/CreateCodeSigningConfigResponse.proto"
    defaultService
    (Proxy :: Proxy CreateCodeSigningConfig)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy :: Proxy TagResource)

responseGetAlias :: AliasConfiguration -> TestTree
responseGetAlias =
  res
    "GetAliasResponse"
    "fixture/GetAliasResponse.proto"
    defaultService
    (Proxy :: Proxy GetAlias)

responseDeleteCodeSigningConfig :: DeleteCodeSigningConfigResponse -> TestTree
responseDeleteCodeSigningConfig =
  res
    "DeleteCodeSigningConfigResponse"
    "fixture/DeleteCodeSigningConfigResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteCodeSigningConfig)

responseUpdateCodeSigningConfig :: UpdateCodeSigningConfigResponse -> TestTree
responseUpdateCodeSigningConfig =
  res
    "UpdateCodeSigningConfigResponse"
    "fixture/UpdateCodeSigningConfigResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateCodeSigningConfig)

responseListCodeSigningConfigs :: ListCodeSigningConfigsResponse -> TestTree
responseListCodeSigningConfigs =
  res
    "ListCodeSigningConfigsResponse"
    "fixture/ListCodeSigningConfigsResponse.proto"
    defaultService
    (Proxy :: Proxy ListCodeSigningConfigs)

responseDeleteFunctionEventInvokeConfig :: DeleteFunctionEventInvokeConfigResponse -> TestTree
responseDeleteFunctionEventInvokeConfig =
  res
    "DeleteFunctionEventInvokeConfigResponse"
    "fixture/DeleteFunctionEventInvokeConfigResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteFunctionEventInvokeConfig)

responseUpdateFunctionEventInvokeConfig :: FunctionEventInvokeConfig -> TestTree
responseUpdateFunctionEventInvokeConfig =
  res
    "UpdateFunctionEventInvokeConfigResponse"
    "fixture/UpdateFunctionEventInvokeConfigResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateFunctionEventInvokeConfig)

responseListFunctionEventInvokeConfigs :: ListFunctionEventInvokeConfigsResponse -> TestTree
responseListFunctionEventInvokeConfigs =
  res
    "ListFunctionEventInvokeConfigsResponse"
    "fixture/ListFunctionEventInvokeConfigsResponse.proto"
    defaultService
    (Proxy :: Proxy ListFunctionEventInvokeConfigs)

responseListVersionsByFunction :: ListVersionsByFunctionResponse -> TestTree
responseListVersionsByFunction =
  res
    "ListVersionsByFunctionResponse"
    "fixture/ListVersionsByFunctionResponse.proto"
    defaultService
    (Proxy :: Proxy ListVersionsByFunction)

responseAddPermission :: AddPermissionResponse -> TestTree
responseAddPermission =
  res
    "AddPermissionResponse"
    "fixture/AddPermissionResponse.proto"
    defaultService
    (Proxy :: Proxy AddPermission)

responseGetLayerVersion :: GetLayerVersionResponse -> TestTree
responseGetLayerVersion =
  res
    "GetLayerVersionResponse"
    "fixture/GetLayerVersionResponse.proto"
    defaultService
    (Proxy :: Proxy GetLayerVersion)

responseDeleteFunctionCodeSigningConfig :: DeleteFunctionCodeSigningConfigResponse -> TestTree
responseDeleteFunctionCodeSigningConfig =
  res
    "DeleteFunctionCodeSigningConfigResponse"
    "fixture/DeleteFunctionCodeSigningConfigResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteFunctionCodeSigningConfig)

responseListFunctionsByCodeSigningConfig :: ListFunctionsByCodeSigningConfigResponse -> TestTree
responseListFunctionsByCodeSigningConfig =
  res
    "ListFunctionsByCodeSigningConfigResponse"
    "fixture/ListFunctionsByCodeSigningConfigResponse.proto"
    defaultService
    (Proxy :: Proxy ListFunctionsByCodeSigningConfig)

responseRemoveLayerVersionPermission :: RemoveLayerVersionPermissionResponse -> TestTree
responseRemoveLayerVersionPermission =
  res
    "RemoveLayerVersionPermissionResponse"
    "fixture/RemoveLayerVersionPermissionResponse.proto"
    defaultService
    (Proxy :: Proxy RemoveLayerVersionPermission)

responseGetProvisionedConcurrencyConfig :: GetProvisionedConcurrencyConfigResponse -> TestTree
responseGetProvisionedConcurrencyConfig =
  res
    "GetProvisionedConcurrencyConfigResponse"
    "fixture/GetProvisionedConcurrencyConfigResponse.proto"
    defaultService
    (Proxy :: Proxy GetProvisionedConcurrencyConfig)

responseCreateFunction :: FunctionConfiguration -> TestTree
responseCreateFunction =
  res
    "CreateFunctionResponse"
    "fixture/CreateFunctionResponse.proto"
    defaultService
    (Proxy :: Proxy CreateFunction)

responsePutFunctionConcurrency :: Concurrency -> TestTree
responsePutFunctionConcurrency =
  res
    "PutFunctionConcurrencyResponse"
    "fixture/PutFunctionConcurrencyResponse.proto"
    defaultService
    (Proxy :: Proxy PutFunctionConcurrency)

responseGetPolicy :: GetPolicyResponse -> TestTree
responseGetPolicy =
  res
    "GetPolicyResponse"
    "fixture/GetPolicyResponse.proto"
    defaultService
    (Proxy :: Proxy GetPolicy)

responsePutProvisionedConcurrencyConfig :: PutProvisionedConcurrencyConfigResponse -> TestTree
responsePutProvisionedConcurrencyConfig =
  res
    "PutProvisionedConcurrencyConfigResponse"
    "fixture/PutProvisionedConcurrencyConfigResponse.proto"
    defaultService
    (Proxy :: Proxy PutProvisionedConcurrencyConfig)

responseAddLayerVersionPermission :: AddLayerVersionPermissionResponse -> TestTree
responseAddLayerVersionPermission =
  res
    "AddLayerVersionPermissionResponse"
    "fixture/AddLayerVersionPermissionResponse.proto"
    defaultService
    (Proxy :: Proxy AddLayerVersionPermission)

responsePublishVersion :: FunctionConfiguration -> TestTree
responsePublishVersion =
  res
    "PublishVersionResponse"
    "fixture/PublishVersionResponse.proto"
    defaultService
    (Proxy :: Proxy PublishVersion)

responseDeleteFunction :: DeleteFunctionResponse -> TestTree
responseDeleteFunction =
  res
    "DeleteFunctionResponse"
    "fixture/DeleteFunctionResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteFunction)

responseGetEventSourceMapping :: EventSourceMappingConfiguration -> TestTree
responseGetEventSourceMapping =
  res
    "GetEventSourceMappingResponse"
    "fixture/GetEventSourceMappingResponse.proto"
    defaultService
    (Proxy :: Proxy GetEventSourceMapping)

responseListTags :: ListTagsResponse -> TestTree
responseListTags =
  res
    "ListTagsResponse"
    "fixture/ListTagsResponse.proto"
    defaultService
    (Proxy :: Proxy ListTags)

responsePublishLayerVersion :: PublishLayerVersionResponse -> TestTree
responsePublishLayerVersion =
  res
    "PublishLayerVersionResponse"
    "fixture/PublishLayerVersionResponse.proto"
    defaultService
    (Proxy :: Proxy PublishLayerVersion)

responseGetFunctionCodeSigningConfig :: GetFunctionCodeSigningConfigResponse -> TestTree
responseGetFunctionCodeSigningConfig =
  res
    "GetFunctionCodeSigningConfigResponse"
    "fixture/GetFunctionCodeSigningConfigResponse.proto"
    defaultService
    (Proxy :: Proxy GetFunctionCodeSigningConfig)

responsePutFunctionEventInvokeConfig :: FunctionEventInvokeConfig -> TestTree
responsePutFunctionEventInvokeConfig =
  res
    "PutFunctionEventInvokeConfigResponse"
    "fixture/PutFunctionEventInvokeConfigResponse.proto"
    defaultService
    (Proxy :: Proxy PutFunctionEventInvokeConfig)

responseRemovePermission :: RemovePermissionResponse -> TestTree
responseRemovePermission =
  res
    "RemovePermissionResponse"
    "fixture/RemovePermissionResponse.proto"
    defaultService
    (Proxy :: Proxy RemovePermission)

responseCreateAlias :: AliasConfiguration -> TestTree
responseCreateAlias =
  res
    "CreateAliasResponse"
    "fixture/CreateAliasResponse.proto"
    defaultService
    (Proxy :: Proxy CreateAlias)

responseGetCodeSigningConfig :: GetCodeSigningConfigResponse -> TestTree
responseGetCodeSigningConfig =
  res
    "GetCodeSigningConfigResponse"
    "fixture/GetCodeSigningConfigResponse.proto"
    defaultService
    (Proxy :: Proxy GetCodeSigningConfig)

responseGetFunctionEventInvokeConfig :: FunctionEventInvokeConfig -> TestTree
responseGetFunctionEventInvokeConfig =
  res
    "GetFunctionEventInvokeConfigResponse"
    "fixture/GetFunctionEventInvokeConfigResponse.proto"
    defaultService
    (Proxy :: Proxy GetFunctionEventInvokeConfig)

responseListLayers :: ListLayersResponse -> TestTree
responseListLayers =
  res
    "ListLayersResponse"
    "fixture/ListLayersResponse.proto"
    defaultService
    (Proxy :: Proxy ListLayers)

responseListAliases :: ListAliasesResponse -> TestTree
responseListAliases =
  res
    "ListAliasesResponse"
    "fixture/ListAliasesResponse.proto"
    defaultService
    (Proxy :: Proxy ListAliases)

responseGetAccountSettings :: GetAccountSettingsResponse -> TestTree
responseGetAccountSettings =
  res
    "GetAccountSettingsResponse"
    "fixture/GetAccountSettingsResponse.proto"
    defaultService
    (Proxy :: Proxy GetAccountSettings)
