{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.AppSync
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.AWS.Gen.AppSync where

import qualified Data.Proxy as Proxy
import Network.AWS.AppSync
import Test.AWS.AppSync.Internal
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestUpdateDataSource $
--             newUpdateDataSource
--
--         , requestDeleteDataSource $
--             newDeleteDataSource
--
--         , requestCreateType $
--             newCreateType
--
--         , requestGetGraphqlApi $
--             newGetGraphqlApi
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestCreateGraphqlApi $
--             newCreateGraphqlApi
--
--         , requestStartSchemaCreation $
--             newStartSchemaCreation
--
--         , requestFlushApiCache $
--             newFlushApiCache
--
--         , requestDeleteGraphqlApi $
--             newDeleteGraphqlApi
--
--         , requestUpdateGraphqlApi $
--             newUpdateGraphqlApi
--
--         , requestGetIntrospectionSchema $
--             newGetIntrospectionSchema
--
--         , requestGetDataSource $
--             newGetDataSource
--
--         , requestListResolversByFunction $
--             newListResolversByFunction
--
--         , requestCreateFunction $
--             newCreateFunction
--
--         , requestDeleteApiKey $
--             newDeleteApiKey
--
--         , requestUpdateApiKey $
--             newUpdateApiKey
--
--         , requestUpdateType $
--             newUpdateType
--
--         , requestDeleteType $
--             newDeleteType
--
--         , requestCreateDataSource $
--             newCreateDataSource
--
--         , requestListTypes $
--             newListTypes
--
--         , requestGetFunction $
--             newGetFunction
--
--         , requestListDataSources $
--             newListDataSources
--
--         , requestUpdateResolver $
--             newUpdateResolver
--
--         , requestDeleteResolver $
--             newDeleteResolver
--
--         , requestListResolvers $
--             newListResolvers
--
--         , requestCreateResolver $
--             newCreateResolver
--
--         , requestGetSchemaCreationStatus $
--             newGetSchemaCreationStatus
--
--         , requestGetApiCache $
--             newGetApiCache
--
--         , requestUpdateApiCache $
--             newUpdateApiCache
--
--         , requestDeleteApiCache $
--             newDeleteApiCache
--
--         , requestListGraphqlApis $
--             newListGraphqlApis
--
--         , requestCreateApiCache $
--             newCreateApiCache
--
--         , requestTagResource $
--             newTagResource
--
--         , requestGetResolver $
--             newGetResolver
--
--         , requestUpdateFunction $
--             newUpdateFunction
--
--         , requestDeleteFunction $
--             newDeleteFunction
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestCreateApiKey $
--             newCreateApiKey
--
--         , requestListFunctions $
--             newListFunctions
--
--         , requestListApiKeys $
--             newListApiKeys
--
--         , requestGetType $
--             newGetType
--
--           ]

--     , testGroup "response"
--         [ responseUpdateDataSource $
--             newUpdateDataSourceResponse
--
--         , responseDeleteDataSource $
--             newDeleteDataSourceResponse
--
--         , responseCreateType $
--             newCreateTypeResponse
--
--         , responseGetGraphqlApi $
--             newGetGraphqlApiResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseCreateGraphqlApi $
--             newCreateGraphqlApiResponse
--
--         , responseStartSchemaCreation $
--             newStartSchemaCreationResponse
--
--         , responseFlushApiCache $
--             newFlushApiCacheResponse
--
--         , responseDeleteGraphqlApi $
--             newDeleteGraphqlApiResponse
--
--         , responseUpdateGraphqlApi $
--             newUpdateGraphqlApiResponse
--
--         , responseGetIntrospectionSchema $
--             newGetIntrospectionSchemaResponse
--
--         , responseGetDataSource $
--             newGetDataSourceResponse
--
--         , responseListResolversByFunction $
--             newListResolversByFunctionResponse
--
--         , responseCreateFunction $
--             newCreateFunctionResponse
--
--         , responseDeleteApiKey $
--             newDeleteApiKeyResponse
--
--         , responseUpdateApiKey $
--             newUpdateApiKeyResponse
--
--         , responseUpdateType $
--             newUpdateTypeResponse
--
--         , responseDeleteType $
--             newDeleteTypeResponse
--
--         , responseCreateDataSource $
--             newCreateDataSourceResponse
--
--         , responseListTypes $
--             newListTypesResponse
--
--         , responseGetFunction $
--             newGetFunctionResponse
--
--         , responseListDataSources $
--             newListDataSourcesResponse
--
--         , responseUpdateResolver $
--             newUpdateResolverResponse
--
--         , responseDeleteResolver $
--             newDeleteResolverResponse
--
--         , responseListResolvers $
--             newListResolversResponse
--
--         , responseCreateResolver $
--             newCreateResolverResponse
--
--         , responseGetSchemaCreationStatus $
--             newGetSchemaCreationStatusResponse
--
--         , responseGetApiCache $
--             newGetApiCacheResponse
--
--         , responseUpdateApiCache $
--             newUpdateApiCacheResponse
--
--         , responseDeleteApiCache $
--             newDeleteApiCacheResponse
--
--         , responseListGraphqlApis $
--             newListGraphqlApisResponse
--
--         , responseCreateApiCache $
--             newCreateApiCacheResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseGetResolver $
--             newGetResolverResponse
--
--         , responseUpdateFunction $
--             newUpdateFunctionResponse
--
--         , responseDeleteFunction $
--             newDeleteFunctionResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseCreateApiKey $
--             newCreateApiKeyResponse
--
--         , responseListFunctions $
--             newListFunctionsResponse
--
--         , responseListApiKeys $
--             newListApiKeysResponse
--
--         , responseGetType $
--             newGetTypeResponse
--
--           ]
--     ]

-- Requests

requestUpdateDataSource :: UpdateDataSource -> TestTree
requestUpdateDataSource =
  req
    "UpdateDataSource"
    "fixture/UpdateDataSource.yaml"

requestDeleteDataSource :: DeleteDataSource -> TestTree
requestDeleteDataSource =
  req
    "DeleteDataSource"
    "fixture/DeleteDataSource.yaml"

requestCreateType :: CreateType -> TestTree
requestCreateType =
  req
    "CreateType"
    "fixture/CreateType.yaml"

requestGetGraphqlApi :: GetGraphqlApi -> TestTree
requestGetGraphqlApi =
  req
    "GetGraphqlApi"
    "fixture/GetGraphqlApi.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestCreateGraphqlApi :: CreateGraphqlApi -> TestTree
requestCreateGraphqlApi =
  req
    "CreateGraphqlApi"
    "fixture/CreateGraphqlApi.yaml"

requestStartSchemaCreation :: StartSchemaCreation -> TestTree
requestStartSchemaCreation =
  req
    "StartSchemaCreation"
    "fixture/StartSchemaCreation.yaml"

requestFlushApiCache :: FlushApiCache -> TestTree
requestFlushApiCache =
  req
    "FlushApiCache"
    "fixture/FlushApiCache.yaml"

requestDeleteGraphqlApi :: DeleteGraphqlApi -> TestTree
requestDeleteGraphqlApi =
  req
    "DeleteGraphqlApi"
    "fixture/DeleteGraphqlApi.yaml"

requestUpdateGraphqlApi :: UpdateGraphqlApi -> TestTree
requestUpdateGraphqlApi =
  req
    "UpdateGraphqlApi"
    "fixture/UpdateGraphqlApi.yaml"

requestGetIntrospectionSchema :: GetIntrospectionSchema -> TestTree
requestGetIntrospectionSchema =
  req
    "GetIntrospectionSchema"
    "fixture/GetIntrospectionSchema.yaml"

requestGetDataSource :: GetDataSource -> TestTree
requestGetDataSource =
  req
    "GetDataSource"
    "fixture/GetDataSource.yaml"

requestListResolversByFunction :: ListResolversByFunction -> TestTree
requestListResolversByFunction =
  req
    "ListResolversByFunction"
    "fixture/ListResolversByFunction.yaml"

requestCreateFunction :: CreateFunction -> TestTree
requestCreateFunction =
  req
    "CreateFunction"
    "fixture/CreateFunction.yaml"

requestDeleteApiKey :: DeleteApiKey -> TestTree
requestDeleteApiKey =
  req
    "DeleteApiKey"
    "fixture/DeleteApiKey.yaml"

requestUpdateApiKey :: UpdateApiKey -> TestTree
requestUpdateApiKey =
  req
    "UpdateApiKey"
    "fixture/UpdateApiKey.yaml"

requestUpdateType :: UpdateType -> TestTree
requestUpdateType =
  req
    "UpdateType"
    "fixture/UpdateType.yaml"

requestDeleteType :: DeleteType -> TestTree
requestDeleteType =
  req
    "DeleteType"
    "fixture/DeleteType.yaml"

requestCreateDataSource :: CreateDataSource -> TestTree
requestCreateDataSource =
  req
    "CreateDataSource"
    "fixture/CreateDataSource.yaml"

requestListTypes :: ListTypes -> TestTree
requestListTypes =
  req
    "ListTypes"
    "fixture/ListTypes.yaml"

requestGetFunction :: GetFunction -> TestTree
requestGetFunction =
  req
    "GetFunction"
    "fixture/GetFunction.yaml"

requestListDataSources :: ListDataSources -> TestTree
requestListDataSources =
  req
    "ListDataSources"
    "fixture/ListDataSources.yaml"

requestUpdateResolver :: UpdateResolver -> TestTree
requestUpdateResolver =
  req
    "UpdateResolver"
    "fixture/UpdateResolver.yaml"

requestDeleteResolver :: DeleteResolver -> TestTree
requestDeleteResolver =
  req
    "DeleteResolver"
    "fixture/DeleteResolver.yaml"

requestListResolvers :: ListResolvers -> TestTree
requestListResolvers =
  req
    "ListResolvers"
    "fixture/ListResolvers.yaml"

requestCreateResolver :: CreateResolver -> TestTree
requestCreateResolver =
  req
    "CreateResolver"
    "fixture/CreateResolver.yaml"

requestGetSchemaCreationStatus :: GetSchemaCreationStatus -> TestTree
requestGetSchemaCreationStatus =
  req
    "GetSchemaCreationStatus"
    "fixture/GetSchemaCreationStatus.yaml"

requestGetApiCache :: GetApiCache -> TestTree
requestGetApiCache =
  req
    "GetApiCache"
    "fixture/GetApiCache.yaml"

requestUpdateApiCache :: UpdateApiCache -> TestTree
requestUpdateApiCache =
  req
    "UpdateApiCache"
    "fixture/UpdateApiCache.yaml"

requestDeleteApiCache :: DeleteApiCache -> TestTree
requestDeleteApiCache =
  req
    "DeleteApiCache"
    "fixture/DeleteApiCache.yaml"

requestListGraphqlApis :: ListGraphqlApis -> TestTree
requestListGraphqlApis =
  req
    "ListGraphqlApis"
    "fixture/ListGraphqlApis.yaml"

requestCreateApiCache :: CreateApiCache -> TestTree
requestCreateApiCache =
  req
    "CreateApiCache"
    "fixture/CreateApiCache.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestGetResolver :: GetResolver -> TestTree
requestGetResolver =
  req
    "GetResolver"
    "fixture/GetResolver.yaml"

requestUpdateFunction :: UpdateFunction -> TestTree
requestUpdateFunction =
  req
    "UpdateFunction"
    "fixture/UpdateFunction.yaml"

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

requestCreateApiKey :: CreateApiKey -> TestTree
requestCreateApiKey =
  req
    "CreateApiKey"
    "fixture/CreateApiKey.yaml"

requestListFunctions :: ListFunctions -> TestTree
requestListFunctions =
  req
    "ListFunctions"
    "fixture/ListFunctions.yaml"

requestListApiKeys :: ListApiKeys -> TestTree
requestListApiKeys =
  req
    "ListApiKeys"
    "fixture/ListApiKeys.yaml"

requestGetType :: GetType -> TestTree
requestGetType =
  req
    "GetType"
    "fixture/GetType.yaml"

-- Responses

responseUpdateDataSource :: UpdateDataSourceResponse -> TestTree
responseUpdateDataSource =
  res
    "UpdateDataSourceResponse"
    "fixture/UpdateDataSourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateDataSource)

responseDeleteDataSource :: DeleteDataSourceResponse -> TestTree
responseDeleteDataSource =
  res
    "DeleteDataSourceResponse"
    "fixture/DeleteDataSourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteDataSource)

responseCreateType :: CreateTypeResponse -> TestTree
responseCreateType =
  res
    "CreateTypeResponse"
    "fixture/CreateTypeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateType)

responseGetGraphqlApi :: GetGraphqlApiResponse -> TestTree
responseGetGraphqlApi =
  res
    "GetGraphqlApiResponse"
    "fixture/GetGraphqlApiResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetGraphqlApi)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseCreateGraphqlApi :: CreateGraphqlApiResponse -> TestTree
responseCreateGraphqlApi =
  res
    "CreateGraphqlApiResponse"
    "fixture/CreateGraphqlApiResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateGraphqlApi)

responseStartSchemaCreation :: StartSchemaCreationResponse -> TestTree
responseStartSchemaCreation =
  res
    "StartSchemaCreationResponse"
    "fixture/StartSchemaCreationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartSchemaCreation)

responseFlushApiCache :: FlushApiCacheResponse -> TestTree
responseFlushApiCache =
  res
    "FlushApiCacheResponse"
    "fixture/FlushApiCacheResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy FlushApiCache)

responseDeleteGraphqlApi :: DeleteGraphqlApiResponse -> TestTree
responseDeleteGraphqlApi =
  res
    "DeleteGraphqlApiResponse"
    "fixture/DeleteGraphqlApiResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteGraphqlApi)

responseUpdateGraphqlApi :: UpdateGraphqlApiResponse -> TestTree
responseUpdateGraphqlApi =
  res
    "UpdateGraphqlApiResponse"
    "fixture/UpdateGraphqlApiResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateGraphqlApi)

responseGetIntrospectionSchema :: GetIntrospectionSchemaResponse -> TestTree
responseGetIntrospectionSchema =
  res
    "GetIntrospectionSchemaResponse"
    "fixture/GetIntrospectionSchemaResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetIntrospectionSchema)

responseGetDataSource :: GetDataSourceResponse -> TestTree
responseGetDataSource =
  res
    "GetDataSourceResponse"
    "fixture/GetDataSourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetDataSource)

responseListResolversByFunction :: ListResolversByFunctionResponse -> TestTree
responseListResolversByFunction =
  res
    "ListResolversByFunctionResponse"
    "fixture/ListResolversByFunctionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListResolversByFunction)

responseCreateFunction :: CreateFunctionResponse -> TestTree
responseCreateFunction =
  res
    "CreateFunctionResponse"
    "fixture/CreateFunctionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateFunction)

responseDeleteApiKey :: DeleteApiKeyResponse -> TestTree
responseDeleteApiKey =
  res
    "DeleteApiKeyResponse"
    "fixture/DeleteApiKeyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteApiKey)

responseUpdateApiKey :: UpdateApiKeyResponse -> TestTree
responseUpdateApiKey =
  res
    "UpdateApiKeyResponse"
    "fixture/UpdateApiKeyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateApiKey)

responseUpdateType :: UpdateTypeResponse -> TestTree
responseUpdateType =
  res
    "UpdateTypeResponse"
    "fixture/UpdateTypeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateType)

responseDeleteType :: DeleteTypeResponse -> TestTree
responseDeleteType =
  res
    "DeleteTypeResponse"
    "fixture/DeleteTypeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteType)

responseCreateDataSource :: CreateDataSourceResponse -> TestTree
responseCreateDataSource =
  res
    "CreateDataSourceResponse"
    "fixture/CreateDataSourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateDataSource)

responseListTypes :: ListTypesResponse -> TestTree
responseListTypes =
  res
    "ListTypesResponse"
    "fixture/ListTypesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTypes)

responseGetFunction :: GetFunctionResponse -> TestTree
responseGetFunction =
  res
    "GetFunctionResponse"
    "fixture/GetFunctionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetFunction)

responseListDataSources :: ListDataSourcesResponse -> TestTree
responseListDataSources =
  res
    "ListDataSourcesResponse"
    "fixture/ListDataSourcesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListDataSources)

responseUpdateResolver :: UpdateResolverResponse -> TestTree
responseUpdateResolver =
  res
    "UpdateResolverResponse"
    "fixture/UpdateResolverResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateResolver)

responseDeleteResolver :: DeleteResolverResponse -> TestTree
responseDeleteResolver =
  res
    "DeleteResolverResponse"
    "fixture/DeleteResolverResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteResolver)

responseListResolvers :: ListResolversResponse -> TestTree
responseListResolvers =
  res
    "ListResolversResponse"
    "fixture/ListResolversResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListResolvers)

responseCreateResolver :: CreateResolverResponse -> TestTree
responseCreateResolver =
  res
    "CreateResolverResponse"
    "fixture/CreateResolverResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateResolver)

responseGetSchemaCreationStatus :: GetSchemaCreationStatusResponse -> TestTree
responseGetSchemaCreationStatus =
  res
    "GetSchemaCreationStatusResponse"
    "fixture/GetSchemaCreationStatusResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetSchemaCreationStatus)

responseGetApiCache :: GetApiCacheResponse -> TestTree
responseGetApiCache =
  res
    "GetApiCacheResponse"
    "fixture/GetApiCacheResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetApiCache)

responseUpdateApiCache :: UpdateApiCacheResponse -> TestTree
responseUpdateApiCache =
  res
    "UpdateApiCacheResponse"
    "fixture/UpdateApiCacheResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateApiCache)

responseDeleteApiCache :: DeleteApiCacheResponse -> TestTree
responseDeleteApiCache =
  res
    "DeleteApiCacheResponse"
    "fixture/DeleteApiCacheResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteApiCache)

responseListGraphqlApis :: ListGraphqlApisResponse -> TestTree
responseListGraphqlApis =
  res
    "ListGraphqlApisResponse"
    "fixture/ListGraphqlApisResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListGraphqlApis)

responseCreateApiCache :: CreateApiCacheResponse -> TestTree
responseCreateApiCache =
  res
    "CreateApiCacheResponse"
    "fixture/CreateApiCacheResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateApiCache)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TagResource)

responseGetResolver :: GetResolverResponse -> TestTree
responseGetResolver =
  res
    "GetResolverResponse"
    "fixture/GetResolverResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetResolver)

responseUpdateFunction :: UpdateFunctionResponse -> TestTree
responseUpdateFunction =
  res
    "UpdateFunctionResponse"
    "fixture/UpdateFunctionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateFunction)

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

responseCreateApiKey :: CreateApiKeyResponse -> TestTree
responseCreateApiKey =
  res
    "CreateApiKeyResponse"
    "fixture/CreateApiKeyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateApiKey)

responseListFunctions :: ListFunctionsResponse -> TestTree
responseListFunctions =
  res
    "ListFunctionsResponse"
    "fixture/ListFunctionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListFunctions)

responseListApiKeys :: ListApiKeysResponse -> TestTree
responseListApiKeys =
  res
    "ListApiKeysResponse"
    "fixture/ListApiKeysResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListApiKeys)

responseGetType :: GetTypeResponse -> TestTree
responseGetType =
  res
    "GetTypeResponse"
    "fixture/GetTypeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetType)
