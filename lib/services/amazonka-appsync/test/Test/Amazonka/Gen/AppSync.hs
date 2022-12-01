{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.AppSync
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.AppSync where

import Amazonka.AppSync
import qualified Data.Proxy as Proxy
import Test.Amazonka.AppSync.Internal
import Test.Amazonka.Fixture
import Test.Amazonka.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestAssociateApi $
--             newAssociateApi
--
--         , requestCreateApiCache $
--             newCreateApiCache
--
--         , requestCreateApiKey $
--             newCreateApiKey
--
--         , requestCreateDataSource $
--             newCreateDataSource
--
--         , requestCreateDomainName $
--             newCreateDomainName
--
--         , requestCreateFunction $
--             newCreateFunction
--
--         , requestCreateGraphqlApi $
--             newCreateGraphqlApi
--
--         , requestCreateResolver $
--             newCreateResolver
--
--         , requestCreateType $
--             newCreateType
--
--         , requestDeleteApiCache $
--             newDeleteApiCache
--
--         , requestDeleteApiKey $
--             newDeleteApiKey
--
--         , requestDeleteDataSource $
--             newDeleteDataSource
--
--         , requestDeleteDomainName $
--             newDeleteDomainName
--
--         , requestDeleteFunction $
--             newDeleteFunction
--
--         , requestDeleteGraphqlApi $
--             newDeleteGraphqlApi
--
--         , requestDeleteResolver $
--             newDeleteResolver
--
--         , requestDeleteType $
--             newDeleteType
--
--         , requestDisassociateApi $
--             newDisassociateApi
--
--         , requestEvaluateCode $
--             newEvaluateCode
--
--         , requestEvaluateMappingTemplate $
--             newEvaluateMappingTemplate
--
--         , requestFlushApiCache $
--             newFlushApiCache
--
--         , requestGetApiAssociation $
--             newGetApiAssociation
--
--         , requestGetApiCache $
--             newGetApiCache
--
--         , requestGetDataSource $
--             newGetDataSource
--
--         , requestGetDomainName $
--             newGetDomainName
--
--         , requestGetFunction $
--             newGetFunction
--
--         , requestGetGraphqlApi $
--             newGetGraphqlApi
--
--         , requestGetIntrospectionSchema $
--             newGetIntrospectionSchema
--
--         , requestGetResolver $
--             newGetResolver
--
--         , requestGetSchemaCreationStatus $
--             newGetSchemaCreationStatus
--
--         , requestGetType $
--             newGetType
--
--         , requestListApiKeys $
--             newListApiKeys
--
--         , requestListDataSources $
--             newListDataSources
--
--         , requestListDomainNames $
--             newListDomainNames
--
--         , requestListFunctions $
--             newListFunctions
--
--         , requestListGraphqlApis $
--             newListGraphqlApis
--
--         , requestListResolvers $
--             newListResolvers
--
--         , requestListResolversByFunction $
--             newListResolversByFunction
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestListTypes $
--             newListTypes
--
--         , requestStartSchemaCreation $
--             newStartSchemaCreation
--
--         , requestTagResource $
--             newTagResource
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestUpdateApiCache $
--             newUpdateApiCache
--
--         , requestUpdateApiKey $
--             newUpdateApiKey
--
--         , requestUpdateDataSource $
--             newUpdateDataSource
--
--         , requestUpdateDomainName $
--             newUpdateDomainName
--
--         , requestUpdateFunction $
--             newUpdateFunction
--
--         , requestUpdateGraphqlApi $
--             newUpdateGraphqlApi
--
--         , requestUpdateResolver $
--             newUpdateResolver
--
--         , requestUpdateType $
--             newUpdateType
--
--           ]

--     , testGroup "response"
--         [ responseAssociateApi $
--             newAssociateApiResponse
--
--         , responseCreateApiCache $
--             newCreateApiCacheResponse
--
--         , responseCreateApiKey $
--             newCreateApiKeyResponse
--
--         , responseCreateDataSource $
--             newCreateDataSourceResponse
--
--         , responseCreateDomainName $
--             newCreateDomainNameResponse
--
--         , responseCreateFunction $
--             newCreateFunctionResponse
--
--         , responseCreateGraphqlApi $
--             newCreateGraphqlApiResponse
--
--         , responseCreateResolver $
--             newCreateResolverResponse
--
--         , responseCreateType $
--             newCreateTypeResponse
--
--         , responseDeleteApiCache $
--             newDeleteApiCacheResponse
--
--         , responseDeleteApiKey $
--             newDeleteApiKeyResponse
--
--         , responseDeleteDataSource $
--             newDeleteDataSourceResponse
--
--         , responseDeleteDomainName $
--             newDeleteDomainNameResponse
--
--         , responseDeleteFunction $
--             newDeleteFunctionResponse
--
--         , responseDeleteGraphqlApi $
--             newDeleteGraphqlApiResponse
--
--         , responseDeleteResolver $
--             newDeleteResolverResponse
--
--         , responseDeleteType $
--             newDeleteTypeResponse
--
--         , responseDisassociateApi $
--             newDisassociateApiResponse
--
--         , responseEvaluateCode $
--             newEvaluateCodeResponse
--
--         , responseEvaluateMappingTemplate $
--             newEvaluateMappingTemplateResponse
--
--         , responseFlushApiCache $
--             newFlushApiCacheResponse
--
--         , responseGetApiAssociation $
--             newGetApiAssociationResponse
--
--         , responseGetApiCache $
--             newGetApiCacheResponse
--
--         , responseGetDataSource $
--             newGetDataSourceResponse
--
--         , responseGetDomainName $
--             newGetDomainNameResponse
--
--         , responseGetFunction $
--             newGetFunctionResponse
--
--         , responseGetGraphqlApi $
--             newGetGraphqlApiResponse
--
--         , responseGetIntrospectionSchema $
--             newGetIntrospectionSchemaResponse
--
--         , responseGetResolver $
--             newGetResolverResponse
--
--         , responseGetSchemaCreationStatus $
--             newGetSchemaCreationStatusResponse
--
--         , responseGetType $
--             newGetTypeResponse
--
--         , responseListApiKeys $
--             newListApiKeysResponse
--
--         , responseListDataSources $
--             newListDataSourcesResponse
--
--         , responseListDomainNames $
--             newListDomainNamesResponse
--
--         , responseListFunctions $
--             newListFunctionsResponse
--
--         , responseListGraphqlApis $
--             newListGraphqlApisResponse
--
--         , responseListResolvers $
--             newListResolversResponse
--
--         , responseListResolversByFunction $
--             newListResolversByFunctionResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseListTypes $
--             newListTypesResponse
--
--         , responseStartSchemaCreation $
--             newStartSchemaCreationResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseUpdateApiCache $
--             newUpdateApiCacheResponse
--
--         , responseUpdateApiKey $
--             newUpdateApiKeyResponse
--
--         , responseUpdateDataSource $
--             newUpdateDataSourceResponse
--
--         , responseUpdateDomainName $
--             newUpdateDomainNameResponse
--
--         , responseUpdateFunction $
--             newUpdateFunctionResponse
--
--         , responseUpdateGraphqlApi $
--             newUpdateGraphqlApiResponse
--
--         , responseUpdateResolver $
--             newUpdateResolverResponse
--
--         , responseUpdateType $
--             newUpdateTypeResponse
--
--           ]
--     ]

-- Requests

requestAssociateApi :: AssociateApi -> TestTree
requestAssociateApi =
  req
    "AssociateApi"
    "fixture/AssociateApi.yaml"

requestCreateApiCache :: CreateApiCache -> TestTree
requestCreateApiCache =
  req
    "CreateApiCache"
    "fixture/CreateApiCache.yaml"

requestCreateApiKey :: CreateApiKey -> TestTree
requestCreateApiKey =
  req
    "CreateApiKey"
    "fixture/CreateApiKey.yaml"

requestCreateDataSource :: CreateDataSource -> TestTree
requestCreateDataSource =
  req
    "CreateDataSource"
    "fixture/CreateDataSource.yaml"

requestCreateDomainName :: CreateDomainName -> TestTree
requestCreateDomainName =
  req
    "CreateDomainName"
    "fixture/CreateDomainName.yaml"

requestCreateFunction :: CreateFunction -> TestTree
requestCreateFunction =
  req
    "CreateFunction"
    "fixture/CreateFunction.yaml"

requestCreateGraphqlApi :: CreateGraphqlApi -> TestTree
requestCreateGraphqlApi =
  req
    "CreateGraphqlApi"
    "fixture/CreateGraphqlApi.yaml"

requestCreateResolver :: CreateResolver -> TestTree
requestCreateResolver =
  req
    "CreateResolver"
    "fixture/CreateResolver.yaml"

requestCreateType :: CreateType -> TestTree
requestCreateType =
  req
    "CreateType"
    "fixture/CreateType.yaml"

requestDeleteApiCache :: DeleteApiCache -> TestTree
requestDeleteApiCache =
  req
    "DeleteApiCache"
    "fixture/DeleteApiCache.yaml"

requestDeleteApiKey :: DeleteApiKey -> TestTree
requestDeleteApiKey =
  req
    "DeleteApiKey"
    "fixture/DeleteApiKey.yaml"

requestDeleteDataSource :: DeleteDataSource -> TestTree
requestDeleteDataSource =
  req
    "DeleteDataSource"
    "fixture/DeleteDataSource.yaml"

requestDeleteDomainName :: DeleteDomainName -> TestTree
requestDeleteDomainName =
  req
    "DeleteDomainName"
    "fixture/DeleteDomainName.yaml"

requestDeleteFunction :: DeleteFunction -> TestTree
requestDeleteFunction =
  req
    "DeleteFunction"
    "fixture/DeleteFunction.yaml"

requestDeleteGraphqlApi :: DeleteGraphqlApi -> TestTree
requestDeleteGraphqlApi =
  req
    "DeleteGraphqlApi"
    "fixture/DeleteGraphqlApi.yaml"

requestDeleteResolver :: DeleteResolver -> TestTree
requestDeleteResolver =
  req
    "DeleteResolver"
    "fixture/DeleteResolver.yaml"

requestDeleteType :: DeleteType -> TestTree
requestDeleteType =
  req
    "DeleteType"
    "fixture/DeleteType.yaml"

requestDisassociateApi :: DisassociateApi -> TestTree
requestDisassociateApi =
  req
    "DisassociateApi"
    "fixture/DisassociateApi.yaml"

requestEvaluateCode :: EvaluateCode -> TestTree
requestEvaluateCode =
  req
    "EvaluateCode"
    "fixture/EvaluateCode.yaml"

requestEvaluateMappingTemplate :: EvaluateMappingTemplate -> TestTree
requestEvaluateMappingTemplate =
  req
    "EvaluateMappingTemplate"
    "fixture/EvaluateMappingTemplate.yaml"

requestFlushApiCache :: FlushApiCache -> TestTree
requestFlushApiCache =
  req
    "FlushApiCache"
    "fixture/FlushApiCache.yaml"

requestGetApiAssociation :: GetApiAssociation -> TestTree
requestGetApiAssociation =
  req
    "GetApiAssociation"
    "fixture/GetApiAssociation.yaml"

requestGetApiCache :: GetApiCache -> TestTree
requestGetApiCache =
  req
    "GetApiCache"
    "fixture/GetApiCache.yaml"

requestGetDataSource :: GetDataSource -> TestTree
requestGetDataSource =
  req
    "GetDataSource"
    "fixture/GetDataSource.yaml"

requestGetDomainName :: GetDomainName -> TestTree
requestGetDomainName =
  req
    "GetDomainName"
    "fixture/GetDomainName.yaml"

requestGetFunction :: GetFunction -> TestTree
requestGetFunction =
  req
    "GetFunction"
    "fixture/GetFunction.yaml"

requestGetGraphqlApi :: GetGraphqlApi -> TestTree
requestGetGraphqlApi =
  req
    "GetGraphqlApi"
    "fixture/GetGraphqlApi.yaml"

requestGetIntrospectionSchema :: GetIntrospectionSchema -> TestTree
requestGetIntrospectionSchema =
  req
    "GetIntrospectionSchema"
    "fixture/GetIntrospectionSchema.yaml"

requestGetResolver :: GetResolver -> TestTree
requestGetResolver =
  req
    "GetResolver"
    "fixture/GetResolver.yaml"

requestGetSchemaCreationStatus :: GetSchemaCreationStatus -> TestTree
requestGetSchemaCreationStatus =
  req
    "GetSchemaCreationStatus"
    "fixture/GetSchemaCreationStatus.yaml"

requestGetType :: GetType -> TestTree
requestGetType =
  req
    "GetType"
    "fixture/GetType.yaml"

requestListApiKeys :: ListApiKeys -> TestTree
requestListApiKeys =
  req
    "ListApiKeys"
    "fixture/ListApiKeys.yaml"

requestListDataSources :: ListDataSources -> TestTree
requestListDataSources =
  req
    "ListDataSources"
    "fixture/ListDataSources.yaml"

requestListDomainNames :: ListDomainNames -> TestTree
requestListDomainNames =
  req
    "ListDomainNames"
    "fixture/ListDomainNames.yaml"

requestListFunctions :: ListFunctions -> TestTree
requestListFunctions =
  req
    "ListFunctions"
    "fixture/ListFunctions.yaml"

requestListGraphqlApis :: ListGraphqlApis -> TestTree
requestListGraphqlApis =
  req
    "ListGraphqlApis"
    "fixture/ListGraphqlApis.yaml"

requestListResolvers :: ListResolvers -> TestTree
requestListResolvers =
  req
    "ListResolvers"
    "fixture/ListResolvers.yaml"

requestListResolversByFunction :: ListResolversByFunction -> TestTree
requestListResolversByFunction =
  req
    "ListResolversByFunction"
    "fixture/ListResolversByFunction.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestListTypes :: ListTypes -> TestTree
requestListTypes =
  req
    "ListTypes"
    "fixture/ListTypes.yaml"

requestStartSchemaCreation :: StartSchemaCreation -> TestTree
requestStartSchemaCreation =
  req
    "StartSchemaCreation"
    "fixture/StartSchemaCreation.yaml"

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

requestUpdateApiCache :: UpdateApiCache -> TestTree
requestUpdateApiCache =
  req
    "UpdateApiCache"
    "fixture/UpdateApiCache.yaml"

requestUpdateApiKey :: UpdateApiKey -> TestTree
requestUpdateApiKey =
  req
    "UpdateApiKey"
    "fixture/UpdateApiKey.yaml"

requestUpdateDataSource :: UpdateDataSource -> TestTree
requestUpdateDataSource =
  req
    "UpdateDataSource"
    "fixture/UpdateDataSource.yaml"

requestUpdateDomainName :: UpdateDomainName -> TestTree
requestUpdateDomainName =
  req
    "UpdateDomainName"
    "fixture/UpdateDomainName.yaml"

requestUpdateFunction :: UpdateFunction -> TestTree
requestUpdateFunction =
  req
    "UpdateFunction"
    "fixture/UpdateFunction.yaml"

requestUpdateGraphqlApi :: UpdateGraphqlApi -> TestTree
requestUpdateGraphqlApi =
  req
    "UpdateGraphqlApi"
    "fixture/UpdateGraphqlApi.yaml"

requestUpdateResolver :: UpdateResolver -> TestTree
requestUpdateResolver =
  req
    "UpdateResolver"
    "fixture/UpdateResolver.yaml"

requestUpdateType :: UpdateType -> TestTree
requestUpdateType =
  req
    "UpdateType"
    "fixture/UpdateType.yaml"

-- Responses

responseAssociateApi :: AssociateApiResponse -> TestTree
responseAssociateApi =
  res
    "AssociateApiResponse"
    "fixture/AssociateApiResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociateApi)

responseCreateApiCache :: CreateApiCacheResponse -> TestTree
responseCreateApiCache =
  res
    "CreateApiCacheResponse"
    "fixture/CreateApiCacheResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateApiCache)

responseCreateApiKey :: CreateApiKeyResponse -> TestTree
responseCreateApiKey =
  res
    "CreateApiKeyResponse"
    "fixture/CreateApiKeyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateApiKey)

responseCreateDataSource :: CreateDataSourceResponse -> TestTree
responseCreateDataSource =
  res
    "CreateDataSourceResponse"
    "fixture/CreateDataSourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateDataSource)

responseCreateDomainName :: CreateDomainNameResponse -> TestTree
responseCreateDomainName =
  res
    "CreateDomainNameResponse"
    "fixture/CreateDomainNameResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateDomainName)

responseCreateFunction :: CreateFunctionResponse -> TestTree
responseCreateFunction =
  res
    "CreateFunctionResponse"
    "fixture/CreateFunctionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateFunction)

responseCreateGraphqlApi :: CreateGraphqlApiResponse -> TestTree
responseCreateGraphqlApi =
  res
    "CreateGraphqlApiResponse"
    "fixture/CreateGraphqlApiResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateGraphqlApi)

responseCreateResolver :: CreateResolverResponse -> TestTree
responseCreateResolver =
  res
    "CreateResolverResponse"
    "fixture/CreateResolverResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateResolver)

responseCreateType :: CreateTypeResponse -> TestTree
responseCreateType =
  res
    "CreateTypeResponse"
    "fixture/CreateTypeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateType)

responseDeleteApiCache :: DeleteApiCacheResponse -> TestTree
responseDeleteApiCache =
  res
    "DeleteApiCacheResponse"
    "fixture/DeleteApiCacheResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteApiCache)

responseDeleteApiKey :: DeleteApiKeyResponse -> TestTree
responseDeleteApiKey =
  res
    "DeleteApiKeyResponse"
    "fixture/DeleteApiKeyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteApiKey)

responseDeleteDataSource :: DeleteDataSourceResponse -> TestTree
responseDeleteDataSource =
  res
    "DeleteDataSourceResponse"
    "fixture/DeleteDataSourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteDataSource)

responseDeleteDomainName :: DeleteDomainNameResponse -> TestTree
responseDeleteDomainName =
  res
    "DeleteDomainNameResponse"
    "fixture/DeleteDomainNameResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteDomainName)

responseDeleteFunction :: DeleteFunctionResponse -> TestTree
responseDeleteFunction =
  res
    "DeleteFunctionResponse"
    "fixture/DeleteFunctionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteFunction)

responseDeleteGraphqlApi :: DeleteGraphqlApiResponse -> TestTree
responseDeleteGraphqlApi =
  res
    "DeleteGraphqlApiResponse"
    "fixture/DeleteGraphqlApiResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteGraphqlApi)

responseDeleteResolver :: DeleteResolverResponse -> TestTree
responseDeleteResolver =
  res
    "DeleteResolverResponse"
    "fixture/DeleteResolverResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteResolver)

responseDeleteType :: DeleteTypeResponse -> TestTree
responseDeleteType =
  res
    "DeleteTypeResponse"
    "fixture/DeleteTypeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteType)

responseDisassociateApi :: DisassociateApiResponse -> TestTree
responseDisassociateApi =
  res
    "DisassociateApiResponse"
    "fixture/DisassociateApiResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisassociateApi)

responseEvaluateCode :: EvaluateCodeResponse -> TestTree
responseEvaluateCode =
  res
    "EvaluateCodeResponse"
    "fixture/EvaluateCodeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy EvaluateCode)

responseEvaluateMappingTemplate :: EvaluateMappingTemplateResponse -> TestTree
responseEvaluateMappingTemplate =
  res
    "EvaluateMappingTemplateResponse"
    "fixture/EvaluateMappingTemplateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy EvaluateMappingTemplate)

responseFlushApiCache :: FlushApiCacheResponse -> TestTree
responseFlushApiCache =
  res
    "FlushApiCacheResponse"
    "fixture/FlushApiCacheResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy FlushApiCache)

responseGetApiAssociation :: GetApiAssociationResponse -> TestTree
responseGetApiAssociation =
  res
    "GetApiAssociationResponse"
    "fixture/GetApiAssociationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetApiAssociation)

responseGetApiCache :: GetApiCacheResponse -> TestTree
responseGetApiCache =
  res
    "GetApiCacheResponse"
    "fixture/GetApiCacheResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetApiCache)

responseGetDataSource :: GetDataSourceResponse -> TestTree
responseGetDataSource =
  res
    "GetDataSourceResponse"
    "fixture/GetDataSourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetDataSource)

responseGetDomainName :: GetDomainNameResponse -> TestTree
responseGetDomainName =
  res
    "GetDomainNameResponse"
    "fixture/GetDomainNameResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetDomainName)

responseGetFunction :: GetFunctionResponse -> TestTree
responseGetFunction =
  res
    "GetFunctionResponse"
    "fixture/GetFunctionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetFunction)

responseGetGraphqlApi :: GetGraphqlApiResponse -> TestTree
responseGetGraphqlApi =
  res
    "GetGraphqlApiResponse"
    "fixture/GetGraphqlApiResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetGraphqlApi)

responseGetIntrospectionSchema :: GetIntrospectionSchemaResponse -> TestTree
responseGetIntrospectionSchema =
  res
    "GetIntrospectionSchemaResponse"
    "fixture/GetIntrospectionSchemaResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetIntrospectionSchema)

responseGetResolver :: GetResolverResponse -> TestTree
responseGetResolver =
  res
    "GetResolverResponse"
    "fixture/GetResolverResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetResolver)

responseGetSchemaCreationStatus :: GetSchemaCreationStatusResponse -> TestTree
responseGetSchemaCreationStatus =
  res
    "GetSchemaCreationStatusResponse"
    "fixture/GetSchemaCreationStatusResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetSchemaCreationStatus)

responseGetType :: GetTypeResponse -> TestTree
responseGetType =
  res
    "GetTypeResponse"
    "fixture/GetTypeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetType)

responseListApiKeys :: ListApiKeysResponse -> TestTree
responseListApiKeys =
  res
    "ListApiKeysResponse"
    "fixture/ListApiKeysResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListApiKeys)

responseListDataSources :: ListDataSourcesResponse -> TestTree
responseListDataSources =
  res
    "ListDataSourcesResponse"
    "fixture/ListDataSourcesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListDataSources)

responseListDomainNames :: ListDomainNamesResponse -> TestTree
responseListDomainNames =
  res
    "ListDomainNamesResponse"
    "fixture/ListDomainNamesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListDomainNames)

responseListFunctions :: ListFunctionsResponse -> TestTree
responseListFunctions =
  res
    "ListFunctionsResponse"
    "fixture/ListFunctionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListFunctions)

responseListGraphqlApis :: ListGraphqlApisResponse -> TestTree
responseListGraphqlApis =
  res
    "ListGraphqlApisResponse"
    "fixture/ListGraphqlApisResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListGraphqlApis)

responseListResolvers :: ListResolversResponse -> TestTree
responseListResolvers =
  res
    "ListResolversResponse"
    "fixture/ListResolversResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListResolvers)

responseListResolversByFunction :: ListResolversByFunctionResponse -> TestTree
responseListResolversByFunction =
  res
    "ListResolversByFunctionResponse"
    "fixture/ListResolversByFunctionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListResolversByFunction)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseListTypes :: ListTypesResponse -> TestTree
responseListTypes =
  res
    "ListTypesResponse"
    "fixture/ListTypesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTypes)

responseStartSchemaCreation :: StartSchemaCreationResponse -> TestTree
responseStartSchemaCreation =
  res
    "StartSchemaCreationResponse"
    "fixture/StartSchemaCreationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartSchemaCreation)

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

responseUpdateApiCache :: UpdateApiCacheResponse -> TestTree
responseUpdateApiCache =
  res
    "UpdateApiCacheResponse"
    "fixture/UpdateApiCacheResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateApiCache)

responseUpdateApiKey :: UpdateApiKeyResponse -> TestTree
responseUpdateApiKey =
  res
    "UpdateApiKeyResponse"
    "fixture/UpdateApiKeyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateApiKey)

responseUpdateDataSource :: UpdateDataSourceResponse -> TestTree
responseUpdateDataSource =
  res
    "UpdateDataSourceResponse"
    "fixture/UpdateDataSourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateDataSource)

responseUpdateDomainName :: UpdateDomainNameResponse -> TestTree
responseUpdateDomainName =
  res
    "UpdateDomainNameResponse"
    "fixture/UpdateDomainNameResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateDomainName)

responseUpdateFunction :: UpdateFunctionResponse -> TestTree
responseUpdateFunction =
  res
    "UpdateFunctionResponse"
    "fixture/UpdateFunctionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateFunction)

responseUpdateGraphqlApi :: UpdateGraphqlApiResponse -> TestTree
responseUpdateGraphqlApi =
  res
    "UpdateGraphqlApiResponse"
    "fixture/UpdateGraphqlApiResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateGraphqlApi)

responseUpdateResolver :: UpdateResolverResponse -> TestTree
responseUpdateResolver =
  res
    "UpdateResolverResponse"
    "fixture/UpdateResolverResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateResolver)

responseUpdateType :: UpdateTypeResponse -> TestTree
responseUpdateType =
  res
    "UpdateTypeResponse"
    "fixture/UpdateTypeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateType)
