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
--         , requestGetLayerVersionPolicy $
--             newGetLayerVersionPolicy
--
--         , requestPutFunctionCodeSigningConfig $
--             newPutFunctionCodeSigningConfig
--
--         , requestUpdateAlias $
--             newUpdateAlias
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
--         , requestGetFunctionConfiguration $
--             newGetFunctionConfiguration
--
--         , requestDeleteEventSourceMapping $
--             newDeleteEventSourceMapping
--
--         , requestGetLayerVersionByArn $
--             newGetLayerVersionByArn
--
--         , requestCreateEventSourceMapping $
--             newCreateEventSourceMapping
--
--         , requestGetFunctionConcurrency $
--             newGetFunctionConcurrency
--
--         , requestUpdateFunctionCode $
--             newUpdateFunctionCode
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
--         , requestListFunctions $
--             newListFunctions
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestUpdateFunctionConfiguration $
--             newUpdateFunctionConfiguration
--
--         , requestTagResource $
--             newTagResource
--
--         , requestInvoke $
--             newInvoke
--
--         , requestListLayerVersions $
--             newListLayerVersions
--
--         , requestCreateCodeSigningConfig $
--             newCreateCodeSigningConfig
--
--         , requestDeleteLayerVersion $
--             newDeleteLayerVersion
--
--         , requestGetAlias $
--             newGetAlias
--
--         , requestListFunctionEventInvokeConfigs $
--             newListFunctionEventInvokeConfigs
--
--         , requestDeleteFunctionEventInvokeConfig $
--             newDeleteFunctionEventInvokeConfig
--
--         , requestUpdateFunctionEventInvokeConfig $
--             newUpdateFunctionEventInvokeConfig
--
--         , requestDeleteCodeSigningConfig $
--             newDeleteCodeSigningConfig
--
--         , requestListCodeSigningConfigs $
--             newListCodeSigningConfigs
--
--         , requestUpdateCodeSigningConfig $
--             newUpdateCodeSigningConfig
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
--         , requestRemoveLayerVersionPermission $
--             newRemoveLayerVersionPermission
--
--         , requestListFunctionsByCodeSigningConfig $
--             newListFunctionsByCodeSigningConfig
--
--         , requestGetProvisionedConcurrencyConfig $
--             newGetProvisionedConcurrencyConfig
--
--         , requestGetPolicy $
--             newGetPolicy
--
--         , requestCreateFunction $
--             newCreateFunction
--
--         , requestPutProvisionedConcurrencyConfig $
--             newPutProvisionedConcurrencyConfig
--
--         , requestPutFunctionConcurrency $
--             newPutFunctionConcurrency
--
--         , requestListTags $
--             newListTags
--
--         , requestDeleteFunction $
--             newDeleteFunction
--
--         , requestGetEventSourceMapping $
--             newGetEventSourceMapping
--
--         , requestPublishVersion $
--             newPublishVersion
--
--         , requestAddLayerVersionPermission $
--             newAddLayerVersionPermission
--
--         , requestGetFunctionCodeSigningConfig $
--             newGetFunctionCodeSigningConfig
--
--         , requestPublishLayerVersion $
--             newPublishLayerVersion
--
--         , requestPutFunctionEventInvokeConfig $
--             newPutFunctionEventInvokeConfig
--
--         , requestRemovePermission $
--             newRemovePermission
--
--         , requestListAliases $
--             newListAliases
--
--         , requestCreateAlias $
--             newCreateAlias
--
--         , requestGetFunctionEventInvokeConfig $
--             newGetFunctionEventInvokeConfig
--
--         , requestGetAccountSettings $
--             newGetAccountSettings
--
--         , requestGetCodeSigningConfig $
--             newGetCodeSigningConfig
--
--         , requestListLayers $
--             newListLayers
--
--           ]

--     , testGroup "response"
--         [ responseDeleteAlias $
--             newDeleteAliasResponse
--
--         , responseGetLayerVersionPolicy $
--             newGetLayerVersionPolicyResponse
--
--         , responsePutFunctionCodeSigningConfig $
--             newPutFunctionCodeSigningConfigResponse
--
--         , responseUpdateAlias $
--             newAliasConfiguration
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
--         , responseGetFunctionConfiguration $
--             newFunctionConfiguration
--
--         , responseDeleteEventSourceMapping $
--             newEventSourceMappingConfiguration
--
--         , responseGetLayerVersionByArn $
--             newGetLayerVersionResponse
--
--         , responseCreateEventSourceMapping $
--             newEventSourceMappingConfiguration
--
--         , responseGetFunctionConcurrency $
--             newGetFunctionConcurrencyResponse
--
--         , responseUpdateFunctionCode $
--             newFunctionConfiguration
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
--         , responseListFunctions $
--             newListFunctionsResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseUpdateFunctionConfiguration $
--             newFunctionConfiguration
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseInvoke $
--             newInvokeResponse
--
--         , responseListLayerVersions $
--             newListLayerVersionsResponse
--
--         , responseCreateCodeSigningConfig $
--             newCreateCodeSigningConfigResponse
--
--         , responseDeleteLayerVersion $
--             newDeleteLayerVersionResponse
--
--         , responseGetAlias $
--             newAliasConfiguration
--
--         , responseListFunctionEventInvokeConfigs $
--             newListFunctionEventInvokeConfigsResponse
--
--         , responseDeleteFunctionEventInvokeConfig $
--             newDeleteFunctionEventInvokeConfigResponse
--
--         , responseUpdateFunctionEventInvokeConfig $
--             newFunctionEventInvokeConfig
--
--         , responseDeleteCodeSigningConfig $
--             newDeleteCodeSigningConfigResponse
--
--         , responseListCodeSigningConfigs $
--             newListCodeSigningConfigsResponse
--
--         , responseUpdateCodeSigningConfig $
--             newUpdateCodeSigningConfigResponse
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
--         , responseRemoveLayerVersionPermission $
--             newRemoveLayerVersionPermissionResponse
--
--         , responseListFunctionsByCodeSigningConfig $
--             newListFunctionsByCodeSigningConfigResponse
--
--         , responseGetProvisionedConcurrencyConfig $
--             newGetProvisionedConcurrencyConfigResponse
--
--         , responseGetPolicy $
--             newGetPolicyResponse
--
--         , responseCreateFunction $
--             newFunctionConfiguration
--
--         , responsePutProvisionedConcurrencyConfig $
--             newPutProvisionedConcurrencyConfigResponse
--
--         , responsePutFunctionConcurrency $
--             newConcurrency
--
--         , responseListTags $
--             newListTagsResponse
--
--         , responseDeleteFunction $
--             newDeleteFunctionResponse
--
--         , responseGetEventSourceMapping $
--             newEventSourceMappingConfiguration
--
--         , responsePublishVersion $
--             newFunctionConfiguration
--
--         , responseAddLayerVersionPermission $
--             newAddLayerVersionPermissionResponse
--
--         , responseGetFunctionCodeSigningConfig $
--             newGetFunctionCodeSigningConfigResponse
--
--         , responsePublishLayerVersion $
--             newPublishLayerVersionResponse
--
--         , responsePutFunctionEventInvokeConfig $
--             newFunctionEventInvokeConfig
--
--         , responseRemovePermission $
--             newRemovePermissionResponse
--
--         , responseListAliases $
--             newListAliasesResponse
--
--         , responseCreateAlias $
--             newAliasConfiguration
--
--         , responseGetFunctionEventInvokeConfig $
--             newFunctionEventInvokeConfig
--
--         , responseGetAccountSettings $
--             newGetAccountSettingsResponse
--
--         , responseGetCodeSigningConfig $
--             newGetCodeSigningConfigResponse
--
--         , responseListLayers $
--             newListLayersResponse
--
--           ]
--     ]

-- Requests

requestDeleteAlias :: DeleteAlias -> TestTree
requestDeleteAlias =
  req
    "DeleteAlias"
    "fixture/DeleteAlias.yaml"

requestGetLayerVersionPolicy :: GetLayerVersionPolicy -> TestTree
requestGetLayerVersionPolicy =
  req
    "GetLayerVersionPolicy"
    "fixture/GetLayerVersionPolicy.yaml"

requestPutFunctionCodeSigningConfig :: PutFunctionCodeSigningConfig -> TestTree
requestPutFunctionCodeSigningConfig =
  req
    "PutFunctionCodeSigningConfig"
    "fixture/PutFunctionCodeSigningConfig.yaml"

requestUpdateAlias :: UpdateAlias -> TestTree
requestUpdateAlias =
  req
    "UpdateAlias"
    "fixture/UpdateAlias.yaml"

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

requestGetLayerVersionByArn :: GetLayerVersionByArn -> TestTree
requestGetLayerVersionByArn =
  req
    "GetLayerVersionByArn"
    "fixture/GetLayerVersionByArn.yaml"

requestCreateEventSourceMapping :: CreateEventSourceMapping -> TestTree
requestCreateEventSourceMapping =
  req
    "CreateEventSourceMapping"
    "fixture/CreateEventSourceMapping.yaml"

requestGetFunctionConcurrency :: GetFunctionConcurrency -> TestTree
requestGetFunctionConcurrency =
  req
    "GetFunctionConcurrency"
    "fixture/GetFunctionConcurrency.yaml"

requestUpdateFunctionCode :: UpdateFunctionCode -> TestTree
requestUpdateFunctionCode =
  req
    "UpdateFunctionCode"
    "fixture/UpdateFunctionCode.yaml"

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

requestListFunctions :: ListFunctions -> TestTree
requestListFunctions =
  req
    "ListFunctions"
    "fixture/ListFunctions.yaml"

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

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestInvoke :: Invoke -> TestTree
requestInvoke =
  req
    "Invoke"
    "fixture/Invoke.yaml"

requestListLayerVersions :: ListLayerVersions -> TestTree
requestListLayerVersions =
  req
    "ListLayerVersions"
    "fixture/ListLayerVersions.yaml"

requestCreateCodeSigningConfig :: CreateCodeSigningConfig -> TestTree
requestCreateCodeSigningConfig =
  req
    "CreateCodeSigningConfig"
    "fixture/CreateCodeSigningConfig.yaml"

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

requestListFunctionEventInvokeConfigs :: ListFunctionEventInvokeConfigs -> TestTree
requestListFunctionEventInvokeConfigs =
  req
    "ListFunctionEventInvokeConfigs"
    "fixture/ListFunctionEventInvokeConfigs.yaml"

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

requestDeleteCodeSigningConfig :: DeleteCodeSigningConfig -> TestTree
requestDeleteCodeSigningConfig =
  req
    "DeleteCodeSigningConfig"
    "fixture/DeleteCodeSigningConfig.yaml"

requestListCodeSigningConfigs :: ListCodeSigningConfigs -> TestTree
requestListCodeSigningConfigs =
  req
    "ListCodeSigningConfigs"
    "fixture/ListCodeSigningConfigs.yaml"

requestUpdateCodeSigningConfig :: UpdateCodeSigningConfig -> TestTree
requestUpdateCodeSigningConfig =
  req
    "UpdateCodeSigningConfig"
    "fixture/UpdateCodeSigningConfig.yaml"

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

requestGetProvisionedConcurrencyConfig :: GetProvisionedConcurrencyConfig -> TestTree
requestGetProvisionedConcurrencyConfig =
  req
    "GetProvisionedConcurrencyConfig"
    "fixture/GetProvisionedConcurrencyConfig.yaml"

requestGetPolicy :: GetPolicy -> TestTree
requestGetPolicy =
  req
    "GetPolicy"
    "fixture/GetPolicy.yaml"

requestCreateFunction :: CreateFunction -> TestTree
requestCreateFunction =
  req
    "CreateFunction"
    "fixture/CreateFunction.yaml"

requestPutProvisionedConcurrencyConfig :: PutProvisionedConcurrencyConfig -> TestTree
requestPutProvisionedConcurrencyConfig =
  req
    "PutProvisionedConcurrencyConfig"
    "fixture/PutProvisionedConcurrencyConfig.yaml"

requestPutFunctionConcurrency :: PutFunctionConcurrency -> TestTree
requestPutFunctionConcurrency =
  req
    "PutFunctionConcurrency"
    "fixture/PutFunctionConcurrency.yaml"

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

requestGetEventSourceMapping :: GetEventSourceMapping -> TestTree
requestGetEventSourceMapping =
  req
    "GetEventSourceMapping"
    "fixture/GetEventSourceMapping.yaml"

requestPublishVersion :: PublishVersion -> TestTree
requestPublishVersion =
  req
    "PublishVersion"
    "fixture/PublishVersion.yaml"

requestAddLayerVersionPermission :: AddLayerVersionPermission -> TestTree
requestAddLayerVersionPermission =
  req
    "AddLayerVersionPermission"
    "fixture/AddLayerVersionPermission.yaml"

requestGetFunctionCodeSigningConfig :: GetFunctionCodeSigningConfig -> TestTree
requestGetFunctionCodeSigningConfig =
  req
    "GetFunctionCodeSigningConfig"
    "fixture/GetFunctionCodeSigningConfig.yaml"

requestPublishLayerVersion :: PublishLayerVersion -> TestTree
requestPublishLayerVersion =
  req
    "PublishLayerVersion"
    "fixture/PublishLayerVersion.yaml"

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

requestListAliases :: ListAliases -> TestTree
requestListAliases =
  req
    "ListAliases"
    "fixture/ListAliases.yaml"

requestCreateAlias :: CreateAlias -> TestTree
requestCreateAlias =
  req
    "CreateAlias"
    "fixture/CreateAlias.yaml"

requestGetFunctionEventInvokeConfig :: GetFunctionEventInvokeConfig -> TestTree
requestGetFunctionEventInvokeConfig =
  req
    "GetFunctionEventInvokeConfig"
    "fixture/GetFunctionEventInvokeConfig.yaml"

requestGetAccountSettings :: GetAccountSettings -> TestTree
requestGetAccountSettings =
  req
    "GetAccountSettings"
    "fixture/GetAccountSettings.yaml"

requestGetCodeSigningConfig :: GetCodeSigningConfig -> TestTree
requestGetCodeSigningConfig =
  req
    "GetCodeSigningConfig"
    "fixture/GetCodeSigningConfig.yaml"

requestListLayers :: ListLayers -> TestTree
requestListLayers =
  req
    "ListLayers"
    "fixture/ListLayers.yaml"

-- Responses

responseDeleteAlias :: DeleteAliasResponse -> TestTree
responseDeleteAlias =
  res
    "DeleteAliasResponse"
    "fixture/DeleteAliasResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteAlias)

responseGetLayerVersionPolicy :: GetLayerVersionPolicyResponse -> TestTree
responseGetLayerVersionPolicy =
  res
    "GetLayerVersionPolicyResponse"
    "fixture/GetLayerVersionPolicyResponse.proto"
    defaultService
    (Proxy :: Proxy GetLayerVersionPolicy)

responsePutFunctionCodeSigningConfig :: PutFunctionCodeSigningConfigResponse -> TestTree
responsePutFunctionCodeSigningConfig =
  res
    "PutFunctionCodeSigningConfigResponse"
    "fixture/PutFunctionCodeSigningConfigResponse.proto"
    defaultService
    (Proxy :: Proxy PutFunctionCodeSigningConfig)

responseUpdateAlias :: AliasConfiguration -> TestTree
responseUpdateAlias =
  res
    "UpdateAliasResponse"
    "fixture/UpdateAliasResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateAlias)

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

responseGetFunctionConfiguration :: FunctionConfiguration -> TestTree
responseGetFunctionConfiguration =
  res
    "GetFunctionConfigurationResponse"
    "fixture/GetFunctionConfigurationResponse.proto"
    defaultService
    (Proxy :: Proxy GetFunctionConfiguration)

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

responseCreateEventSourceMapping :: EventSourceMappingConfiguration -> TestTree
responseCreateEventSourceMapping =
  res
    "CreateEventSourceMappingResponse"
    "fixture/CreateEventSourceMappingResponse.proto"
    defaultService
    (Proxy :: Proxy CreateEventSourceMapping)

responseGetFunctionConcurrency :: GetFunctionConcurrencyResponse -> TestTree
responseGetFunctionConcurrency =
  res
    "GetFunctionConcurrencyResponse"
    "fixture/GetFunctionConcurrencyResponse.proto"
    defaultService
    (Proxy :: Proxy GetFunctionConcurrency)

responseUpdateFunctionCode :: FunctionConfiguration -> TestTree
responseUpdateFunctionCode =
  res
    "UpdateFunctionCodeResponse"
    "fixture/UpdateFunctionCodeResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateFunctionCode)

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

responseListFunctions :: ListFunctionsResponse -> TestTree
responseListFunctions =
  res
    "ListFunctionsResponse"
    "fixture/ListFunctionsResponse.proto"
    defaultService
    (Proxy :: Proxy ListFunctions)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy :: Proxy UntagResource)

responseUpdateFunctionConfiguration :: FunctionConfiguration -> TestTree
responseUpdateFunctionConfiguration =
  res
    "UpdateFunctionConfigurationResponse"
    "fixture/UpdateFunctionConfigurationResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateFunctionConfiguration)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy :: Proxy TagResource)

responseInvoke :: InvokeResponse -> TestTree
responseInvoke =
  res
    "InvokeResponse"
    "fixture/InvokeResponse.proto"
    defaultService
    (Proxy :: Proxy Invoke)

responseListLayerVersions :: ListLayerVersionsResponse -> TestTree
responseListLayerVersions =
  res
    "ListLayerVersionsResponse"
    "fixture/ListLayerVersionsResponse.proto"
    defaultService
    (Proxy :: Proxy ListLayerVersions)

responseCreateCodeSigningConfig :: CreateCodeSigningConfigResponse -> TestTree
responseCreateCodeSigningConfig =
  res
    "CreateCodeSigningConfigResponse"
    "fixture/CreateCodeSigningConfigResponse.proto"
    defaultService
    (Proxy :: Proxy CreateCodeSigningConfig)

responseDeleteLayerVersion :: DeleteLayerVersionResponse -> TestTree
responseDeleteLayerVersion =
  res
    "DeleteLayerVersionResponse"
    "fixture/DeleteLayerVersionResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteLayerVersion)

responseGetAlias :: AliasConfiguration -> TestTree
responseGetAlias =
  res
    "GetAliasResponse"
    "fixture/GetAliasResponse.proto"
    defaultService
    (Proxy :: Proxy GetAlias)

responseListFunctionEventInvokeConfigs :: ListFunctionEventInvokeConfigsResponse -> TestTree
responseListFunctionEventInvokeConfigs =
  res
    "ListFunctionEventInvokeConfigsResponse"
    "fixture/ListFunctionEventInvokeConfigsResponse.proto"
    defaultService
    (Proxy :: Proxy ListFunctionEventInvokeConfigs)

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

responseDeleteCodeSigningConfig :: DeleteCodeSigningConfigResponse -> TestTree
responseDeleteCodeSigningConfig =
  res
    "DeleteCodeSigningConfigResponse"
    "fixture/DeleteCodeSigningConfigResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteCodeSigningConfig)

responseListCodeSigningConfigs :: ListCodeSigningConfigsResponse -> TestTree
responseListCodeSigningConfigs =
  res
    "ListCodeSigningConfigsResponse"
    "fixture/ListCodeSigningConfigsResponse.proto"
    defaultService
    (Proxy :: Proxy ListCodeSigningConfigs)

responseUpdateCodeSigningConfig :: UpdateCodeSigningConfigResponse -> TestTree
responseUpdateCodeSigningConfig =
  res
    "UpdateCodeSigningConfigResponse"
    "fixture/UpdateCodeSigningConfigResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateCodeSigningConfig)

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

responseRemoveLayerVersionPermission :: RemoveLayerVersionPermissionResponse -> TestTree
responseRemoveLayerVersionPermission =
  res
    "RemoveLayerVersionPermissionResponse"
    "fixture/RemoveLayerVersionPermissionResponse.proto"
    defaultService
    (Proxy :: Proxy RemoveLayerVersionPermission)

responseListFunctionsByCodeSigningConfig :: ListFunctionsByCodeSigningConfigResponse -> TestTree
responseListFunctionsByCodeSigningConfig =
  res
    "ListFunctionsByCodeSigningConfigResponse"
    "fixture/ListFunctionsByCodeSigningConfigResponse.proto"
    defaultService
    (Proxy :: Proxy ListFunctionsByCodeSigningConfig)

responseGetProvisionedConcurrencyConfig :: GetProvisionedConcurrencyConfigResponse -> TestTree
responseGetProvisionedConcurrencyConfig =
  res
    "GetProvisionedConcurrencyConfigResponse"
    "fixture/GetProvisionedConcurrencyConfigResponse.proto"
    defaultService
    (Proxy :: Proxy GetProvisionedConcurrencyConfig)

responseGetPolicy :: GetPolicyResponse -> TestTree
responseGetPolicy =
  res
    "GetPolicyResponse"
    "fixture/GetPolicyResponse.proto"
    defaultService
    (Proxy :: Proxy GetPolicy)

responseCreateFunction :: FunctionConfiguration -> TestTree
responseCreateFunction =
  res
    "CreateFunctionResponse"
    "fixture/CreateFunctionResponse.proto"
    defaultService
    (Proxy :: Proxy CreateFunction)

responsePutProvisionedConcurrencyConfig :: PutProvisionedConcurrencyConfigResponse -> TestTree
responsePutProvisionedConcurrencyConfig =
  res
    "PutProvisionedConcurrencyConfigResponse"
    "fixture/PutProvisionedConcurrencyConfigResponse.proto"
    defaultService
    (Proxy :: Proxy PutProvisionedConcurrencyConfig)

responsePutFunctionConcurrency :: Concurrency -> TestTree
responsePutFunctionConcurrency =
  res
    "PutFunctionConcurrencyResponse"
    "fixture/PutFunctionConcurrencyResponse.proto"
    defaultService
    (Proxy :: Proxy PutFunctionConcurrency)

responseListTags :: ListTagsResponse -> TestTree
responseListTags =
  res
    "ListTagsResponse"
    "fixture/ListTagsResponse.proto"
    defaultService
    (Proxy :: Proxy ListTags)

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

responsePublishVersion :: FunctionConfiguration -> TestTree
responsePublishVersion =
  res
    "PublishVersionResponse"
    "fixture/PublishVersionResponse.proto"
    defaultService
    (Proxy :: Proxy PublishVersion)

responseAddLayerVersionPermission :: AddLayerVersionPermissionResponse -> TestTree
responseAddLayerVersionPermission =
  res
    "AddLayerVersionPermissionResponse"
    "fixture/AddLayerVersionPermissionResponse.proto"
    defaultService
    (Proxy :: Proxy AddLayerVersionPermission)

responseGetFunctionCodeSigningConfig :: GetFunctionCodeSigningConfigResponse -> TestTree
responseGetFunctionCodeSigningConfig =
  res
    "GetFunctionCodeSigningConfigResponse"
    "fixture/GetFunctionCodeSigningConfigResponse.proto"
    defaultService
    (Proxy :: Proxy GetFunctionCodeSigningConfig)

responsePublishLayerVersion :: PublishLayerVersionResponse -> TestTree
responsePublishLayerVersion =
  res
    "PublishLayerVersionResponse"
    "fixture/PublishLayerVersionResponse.proto"
    defaultService
    (Proxy :: Proxy PublishLayerVersion)

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

responseListAliases :: ListAliasesResponse -> TestTree
responseListAliases =
  res
    "ListAliasesResponse"
    "fixture/ListAliasesResponse.proto"
    defaultService
    (Proxy :: Proxy ListAliases)

responseCreateAlias :: AliasConfiguration -> TestTree
responseCreateAlias =
  res
    "CreateAliasResponse"
    "fixture/CreateAliasResponse.proto"
    defaultService
    (Proxy :: Proxy CreateAlias)

responseGetFunctionEventInvokeConfig :: FunctionEventInvokeConfig -> TestTree
responseGetFunctionEventInvokeConfig =
  res
    "GetFunctionEventInvokeConfigResponse"
    "fixture/GetFunctionEventInvokeConfigResponse.proto"
    defaultService
    (Proxy :: Proxy GetFunctionEventInvokeConfig)

responseGetAccountSettings :: GetAccountSettingsResponse -> TestTree
responseGetAccountSettings =
  res
    "GetAccountSettingsResponse"
    "fixture/GetAccountSettingsResponse.proto"
    defaultService
    (Proxy :: Proxy GetAccountSettings)

responseGetCodeSigningConfig :: GetCodeSigningConfigResponse -> TestTree
responseGetCodeSigningConfig =
  res
    "GetCodeSigningConfigResponse"
    "fixture/GetCodeSigningConfigResponse.proto"
    defaultService
    (Proxy :: Proxy GetCodeSigningConfig)

responseListLayers :: ListLayersResponse -> TestTree
responseListLayers =
  res
    "ListLayersResponse"
    "fixture/ListLayersResponse.proto"
    defaultService
    (Proxy :: Proxy ListLayers)
