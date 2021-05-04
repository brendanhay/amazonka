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

import Data.Proxy
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
--         [ requestListResolvers $
--             newListResolvers
--
--         , requestUpdateDataSource $
--             newUpdateDataSource
--
--         , requestListDataSources $
--             newListDataSources
--
--         , requestCreateType $
--             newCreateType
--
--         , requestGetFunction $
--             newGetFunction
--
--         , requestDeleteDataSource $
--             newDeleteDataSource
--
--         , requestCreateDataSource $
--             newCreateDataSource
--
--         , requestDeleteType $
--             newDeleteType
--
--         , requestUpdateType $
--             newUpdateType
--
--         , requestListResolversByFunction $
--             newListResolversByFunction
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestListFunctions $
--             newListFunctions
--
--         , requestGetIntrospectionSchema $
--             newGetIntrospectionSchema
--
--         , requestUpdateGraphqlApi $
--             newUpdateGraphqlApi
--
--         , requestListGraphqlApis $
--             newListGraphqlApis
--
--         , requestTagResource $
--             newTagResource
--
--         , requestDeleteGraphqlApi $
--             newDeleteGraphqlApi
--
--         , requestGetResolver $
--             newGetResolver
--
--         , requestDeleteApiCache $
--             newDeleteApiCache
--
--         , requestUpdateApiCache $
--             newUpdateApiCache
--
--         , requestGetSchemaCreationStatus $
--             newGetSchemaCreationStatus
--
--         , requestGetApiCache $
--             newGetApiCache
--
--         , requestUpdateResolver $
--             newUpdateResolver
--
--         , requestDeleteResolver $
--             newDeleteResolver
--
--         , requestGetGraphqlApi $
--             newGetGraphqlApi
--
--         , requestListTypes $
--             newListTypes
--
--         , requestGetType $
--             newGetType
--
--         , requestCreateFunction $
--             newCreateFunction
--
--         , requestListApiKeys $
--             newListApiKeys
--
--         , requestDeleteApiKey $
--             newDeleteApiKey
--
--         , requestUpdateApiKey $
--             newUpdateApiKey
--
--         , requestGetDataSource $
--             newGetDataSource
--
--         , requestDeleteFunction $
--             newDeleteFunction
--
--         , requestUpdateFunction $
--             newUpdateFunction
--
--         , requestCreateApiKey $
--             newCreateApiKey
--
--         , requestStartSchemaCreation $
--             newStartSchemaCreation
--
--         , requestFlushApiCache $
--             newFlushApiCache
--
--         , requestCreateApiCache $
--             newCreateApiCache
--
--         , requestCreateGraphqlApi $
--             newCreateGraphqlApi
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestCreateResolver $
--             newCreateResolver
--
--           ]

--     , testGroup "response"
--         [ responseListResolvers $
--             newListResolversResponse
--
--         , responseUpdateDataSource $
--             newUpdateDataSourceResponse
--
--         , responseListDataSources $
--             newListDataSourcesResponse
--
--         , responseCreateType $
--             newCreateTypeResponse
--
--         , responseGetFunction $
--             newGetFunctionResponse
--
--         , responseDeleteDataSource $
--             newDeleteDataSourceResponse
--
--         , responseCreateDataSource $
--             newCreateDataSourceResponse
--
--         , responseDeleteType $
--             newDeleteTypeResponse
--
--         , responseUpdateType $
--             newUpdateTypeResponse
--
--         , responseListResolversByFunction $
--             newListResolversByFunctionResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseListFunctions $
--             newListFunctionsResponse
--
--         , responseGetIntrospectionSchema $
--             newGetIntrospectionSchemaResponse
--
--         , responseUpdateGraphqlApi $
--             newUpdateGraphqlApiResponse
--
--         , responseListGraphqlApis $
--             newListGraphqlApisResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseDeleteGraphqlApi $
--             newDeleteGraphqlApiResponse
--
--         , responseGetResolver $
--             newGetResolverResponse
--
--         , responseDeleteApiCache $
--             newDeleteApiCacheResponse
--
--         , responseUpdateApiCache $
--             newUpdateApiCacheResponse
--
--         , responseGetSchemaCreationStatus $
--             newGetSchemaCreationStatusResponse
--
--         , responseGetApiCache $
--             newGetApiCacheResponse
--
--         , responseUpdateResolver $
--             newUpdateResolverResponse
--
--         , responseDeleteResolver $
--             newDeleteResolverResponse
--
--         , responseGetGraphqlApi $
--             newGetGraphqlApiResponse
--
--         , responseListTypes $
--             newListTypesResponse
--
--         , responseGetType $
--             newGetTypeResponse
--
--         , responseCreateFunction $
--             newCreateFunctionResponse
--
--         , responseListApiKeys $
--             newListApiKeysResponse
--
--         , responseDeleteApiKey $
--             newDeleteApiKeyResponse
--
--         , responseUpdateApiKey $
--             newUpdateApiKeyResponse
--
--         , responseGetDataSource $
--             newGetDataSourceResponse
--
--         , responseDeleteFunction $
--             newDeleteFunctionResponse
--
--         , responseUpdateFunction $
--             newUpdateFunctionResponse
--
--         , responseCreateApiKey $
--             newCreateApiKeyResponse
--
--         , responseStartSchemaCreation $
--             newStartSchemaCreationResponse
--
--         , responseFlushApiCache $
--             newFlushApiCacheResponse
--
--         , responseCreateApiCache $
--             newCreateApiCacheResponse
--
--         , responseCreateGraphqlApi $
--             newCreateGraphqlApiResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseCreateResolver $
--             newCreateResolverResponse
--
--           ]
--     ]

-- Requests

requestListResolvers :: ListResolvers -> TestTree
requestListResolvers =
  req
    "ListResolvers"
    "fixture/ListResolvers.yaml"

requestUpdateDataSource :: UpdateDataSource -> TestTree
requestUpdateDataSource =
  req
    "UpdateDataSource"
    "fixture/UpdateDataSource.yaml"

requestListDataSources :: ListDataSources -> TestTree
requestListDataSources =
  req
    "ListDataSources"
    "fixture/ListDataSources.yaml"

requestCreateType :: CreateType -> TestTree
requestCreateType =
  req
    "CreateType"
    "fixture/CreateType.yaml"

requestGetFunction :: GetFunction -> TestTree
requestGetFunction =
  req
    "GetFunction"
    "fixture/GetFunction.yaml"

requestDeleteDataSource :: DeleteDataSource -> TestTree
requestDeleteDataSource =
  req
    "DeleteDataSource"
    "fixture/DeleteDataSource.yaml"

requestCreateDataSource :: CreateDataSource -> TestTree
requestCreateDataSource =
  req
    "CreateDataSource"
    "fixture/CreateDataSource.yaml"

requestDeleteType :: DeleteType -> TestTree
requestDeleteType =
  req
    "DeleteType"
    "fixture/DeleteType.yaml"

requestUpdateType :: UpdateType -> TestTree
requestUpdateType =
  req
    "UpdateType"
    "fixture/UpdateType.yaml"

requestListResolversByFunction :: ListResolversByFunction -> TestTree
requestListResolversByFunction =
  req
    "ListResolversByFunction"
    "fixture/ListResolversByFunction.yaml"

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

requestGetIntrospectionSchema :: GetIntrospectionSchema -> TestTree
requestGetIntrospectionSchema =
  req
    "GetIntrospectionSchema"
    "fixture/GetIntrospectionSchema.yaml"

requestUpdateGraphqlApi :: UpdateGraphqlApi -> TestTree
requestUpdateGraphqlApi =
  req
    "UpdateGraphqlApi"
    "fixture/UpdateGraphqlApi.yaml"

requestListGraphqlApis :: ListGraphqlApis -> TestTree
requestListGraphqlApis =
  req
    "ListGraphqlApis"
    "fixture/ListGraphqlApis.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestDeleteGraphqlApi :: DeleteGraphqlApi -> TestTree
requestDeleteGraphqlApi =
  req
    "DeleteGraphqlApi"
    "fixture/DeleteGraphqlApi.yaml"

requestGetResolver :: GetResolver -> TestTree
requestGetResolver =
  req
    "GetResolver"
    "fixture/GetResolver.yaml"

requestDeleteApiCache :: DeleteApiCache -> TestTree
requestDeleteApiCache =
  req
    "DeleteApiCache"
    "fixture/DeleteApiCache.yaml"

requestUpdateApiCache :: UpdateApiCache -> TestTree
requestUpdateApiCache =
  req
    "UpdateApiCache"
    "fixture/UpdateApiCache.yaml"

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

requestGetGraphqlApi :: GetGraphqlApi -> TestTree
requestGetGraphqlApi =
  req
    "GetGraphqlApi"
    "fixture/GetGraphqlApi.yaml"

requestListTypes :: ListTypes -> TestTree
requestListTypes =
  req
    "ListTypes"
    "fixture/ListTypes.yaml"

requestGetType :: GetType -> TestTree
requestGetType =
  req
    "GetType"
    "fixture/GetType.yaml"

requestCreateFunction :: CreateFunction -> TestTree
requestCreateFunction =
  req
    "CreateFunction"
    "fixture/CreateFunction.yaml"

requestListApiKeys :: ListApiKeys -> TestTree
requestListApiKeys =
  req
    "ListApiKeys"
    "fixture/ListApiKeys.yaml"

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

requestGetDataSource :: GetDataSource -> TestTree
requestGetDataSource =
  req
    "GetDataSource"
    "fixture/GetDataSource.yaml"

requestDeleteFunction :: DeleteFunction -> TestTree
requestDeleteFunction =
  req
    "DeleteFunction"
    "fixture/DeleteFunction.yaml"

requestUpdateFunction :: UpdateFunction -> TestTree
requestUpdateFunction =
  req
    "UpdateFunction"
    "fixture/UpdateFunction.yaml"

requestCreateApiKey :: CreateApiKey -> TestTree
requestCreateApiKey =
  req
    "CreateApiKey"
    "fixture/CreateApiKey.yaml"

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

requestCreateApiCache :: CreateApiCache -> TestTree
requestCreateApiCache =
  req
    "CreateApiCache"
    "fixture/CreateApiCache.yaml"

requestCreateGraphqlApi :: CreateGraphqlApi -> TestTree
requestCreateGraphqlApi =
  req
    "CreateGraphqlApi"
    "fixture/CreateGraphqlApi.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestCreateResolver :: CreateResolver -> TestTree
requestCreateResolver =
  req
    "CreateResolver"
    "fixture/CreateResolver.yaml"

-- Responses

responseListResolvers :: ListResolversResponse -> TestTree
responseListResolvers =
  res
    "ListResolversResponse"
    "fixture/ListResolversResponse.proto"
    defaultService
    (Proxy :: Proxy ListResolvers)

responseUpdateDataSource :: UpdateDataSourceResponse -> TestTree
responseUpdateDataSource =
  res
    "UpdateDataSourceResponse"
    "fixture/UpdateDataSourceResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateDataSource)

responseListDataSources :: ListDataSourcesResponse -> TestTree
responseListDataSources =
  res
    "ListDataSourcesResponse"
    "fixture/ListDataSourcesResponse.proto"
    defaultService
    (Proxy :: Proxy ListDataSources)

responseCreateType :: CreateTypeResponse -> TestTree
responseCreateType =
  res
    "CreateTypeResponse"
    "fixture/CreateTypeResponse.proto"
    defaultService
    (Proxy :: Proxy CreateType)

responseGetFunction :: GetFunctionResponse -> TestTree
responseGetFunction =
  res
    "GetFunctionResponse"
    "fixture/GetFunctionResponse.proto"
    defaultService
    (Proxy :: Proxy GetFunction)

responseDeleteDataSource :: DeleteDataSourceResponse -> TestTree
responseDeleteDataSource =
  res
    "DeleteDataSourceResponse"
    "fixture/DeleteDataSourceResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteDataSource)

responseCreateDataSource :: CreateDataSourceResponse -> TestTree
responseCreateDataSource =
  res
    "CreateDataSourceResponse"
    "fixture/CreateDataSourceResponse.proto"
    defaultService
    (Proxy :: Proxy CreateDataSource)

responseDeleteType :: DeleteTypeResponse -> TestTree
responseDeleteType =
  res
    "DeleteTypeResponse"
    "fixture/DeleteTypeResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteType)

responseUpdateType :: UpdateTypeResponse -> TestTree
responseUpdateType =
  res
    "UpdateTypeResponse"
    "fixture/UpdateTypeResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateType)

responseListResolversByFunction :: ListResolversByFunctionResponse -> TestTree
responseListResolversByFunction =
  res
    "ListResolversByFunctionResponse"
    "fixture/ListResolversByFunctionResponse.proto"
    defaultService
    (Proxy :: Proxy ListResolversByFunction)

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

responseGetIntrospectionSchema :: GetIntrospectionSchemaResponse -> TestTree
responseGetIntrospectionSchema =
  res
    "GetIntrospectionSchemaResponse"
    "fixture/GetIntrospectionSchemaResponse.proto"
    defaultService
    (Proxy :: Proxy GetIntrospectionSchema)

responseUpdateGraphqlApi :: UpdateGraphqlApiResponse -> TestTree
responseUpdateGraphqlApi =
  res
    "UpdateGraphqlApiResponse"
    "fixture/UpdateGraphqlApiResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateGraphqlApi)

responseListGraphqlApis :: ListGraphqlApisResponse -> TestTree
responseListGraphqlApis =
  res
    "ListGraphqlApisResponse"
    "fixture/ListGraphqlApisResponse.proto"
    defaultService
    (Proxy :: Proxy ListGraphqlApis)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy :: Proxy TagResource)

responseDeleteGraphqlApi :: DeleteGraphqlApiResponse -> TestTree
responseDeleteGraphqlApi =
  res
    "DeleteGraphqlApiResponse"
    "fixture/DeleteGraphqlApiResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteGraphqlApi)

responseGetResolver :: GetResolverResponse -> TestTree
responseGetResolver =
  res
    "GetResolverResponse"
    "fixture/GetResolverResponse.proto"
    defaultService
    (Proxy :: Proxy GetResolver)

responseDeleteApiCache :: DeleteApiCacheResponse -> TestTree
responseDeleteApiCache =
  res
    "DeleteApiCacheResponse"
    "fixture/DeleteApiCacheResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteApiCache)

responseUpdateApiCache :: UpdateApiCacheResponse -> TestTree
responseUpdateApiCache =
  res
    "UpdateApiCacheResponse"
    "fixture/UpdateApiCacheResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateApiCache)

responseGetSchemaCreationStatus :: GetSchemaCreationStatusResponse -> TestTree
responseGetSchemaCreationStatus =
  res
    "GetSchemaCreationStatusResponse"
    "fixture/GetSchemaCreationStatusResponse.proto"
    defaultService
    (Proxy :: Proxy GetSchemaCreationStatus)

responseGetApiCache :: GetApiCacheResponse -> TestTree
responseGetApiCache =
  res
    "GetApiCacheResponse"
    "fixture/GetApiCacheResponse.proto"
    defaultService
    (Proxy :: Proxy GetApiCache)

responseUpdateResolver :: UpdateResolverResponse -> TestTree
responseUpdateResolver =
  res
    "UpdateResolverResponse"
    "fixture/UpdateResolverResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateResolver)

responseDeleteResolver :: DeleteResolverResponse -> TestTree
responseDeleteResolver =
  res
    "DeleteResolverResponse"
    "fixture/DeleteResolverResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteResolver)

responseGetGraphqlApi :: GetGraphqlApiResponse -> TestTree
responseGetGraphqlApi =
  res
    "GetGraphqlApiResponse"
    "fixture/GetGraphqlApiResponse.proto"
    defaultService
    (Proxy :: Proxy GetGraphqlApi)

responseListTypes :: ListTypesResponse -> TestTree
responseListTypes =
  res
    "ListTypesResponse"
    "fixture/ListTypesResponse.proto"
    defaultService
    (Proxy :: Proxy ListTypes)

responseGetType :: GetTypeResponse -> TestTree
responseGetType =
  res
    "GetTypeResponse"
    "fixture/GetTypeResponse.proto"
    defaultService
    (Proxy :: Proxy GetType)

responseCreateFunction :: CreateFunctionResponse -> TestTree
responseCreateFunction =
  res
    "CreateFunctionResponse"
    "fixture/CreateFunctionResponse.proto"
    defaultService
    (Proxy :: Proxy CreateFunction)

responseListApiKeys :: ListApiKeysResponse -> TestTree
responseListApiKeys =
  res
    "ListApiKeysResponse"
    "fixture/ListApiKeysResponse.proto"
    defaultService
    (Proxy :: Proxy ListApiKeys)

responseDeleteApiKey :: DeleteApiKeyResponse -> TestTree
responseDeleteApiKey =
  res
    "DeleteApiKeyResponse"
    "fixture/DeleteApiKeyResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteApiKey)

responseUpdateApiKey :: UpdateApiKeyResponse -> TestTree
responseUpdateApiKey =
  res
    "UpdateApiKeyResponse"
    "fixture/UpdateApiKeyResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateApiKey)

responseGetDataSource :: GetDataSourceResponse -> TestTree
responseGetDataSource =
  res
    "GetDataSourceResponse"
    "fixture/GetDataSourceResponse.proto"
    defaultService
    (Proxy :: Proxy GetDataSource)

responseDeleteFunction :: DeleteFunctionResponse -> TestTree
responseDeleteFunction =
  res
    "DeleteFunctionResponse"
    "fixture/DeleteFunctionResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteFunction)

responseUpdateFunction :: UpdateFunctionResponse -> TestTree
responseUpdateFunction =
  res
    "UpdateFunctionResponse"
    "fixture/UpdateFunctionResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateFunction)

responseCreateApiKey :: CreateApiKeyResponse -> TestTree
responseCreateApiKey =
  res
    "CreateApiKeyResponse"
    "fixture/CreateApiKeyResponse.proto"
    defaultService
    (Proxy :: Proxy CreateApiKey)

responseStartSchemaCreation :: StartSchemaCreationResponse -> TestTree
responseStartSchemaCreation =
  res
    "StartSchemaCreationResponse"
    "fixture/StartSchemaCreationResponse.proto"
    defaultService
    (Proxy :: Proxy StartSchemaCreation)

responseFlushApiCache :: FlushApiCacheResponse -> TestTree
responseFlushApiCache =
  res
    "FlushApiCacheResponse"
    "fixture/FlushApiCacheResponse.proto"
    defaultService
    (Proxy :: Proxy FlushApiCache)

responseCreateApiCache :: CreateApiCacheResponse -> TestTree
responseCreateApiCache =
  res
    "CreateApiCacheResponse"
    "fixture/CreateApiCacheResponse.proto"
    defaultService
    (Proxy :: Proxy CreateApiCache)

responseCreateGraphqlApi :: CreateGraphqlApiResponse -> TestTree
responseCreateGraphqlApi =
  res
    "CreateGraphqlApiResponse"
    "fixture/CreateGraphqlApiResponse.proto"
    defaultService
    (Proxy :: Proxy CreateGraphqlApi)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy :: Proxy ListTagsForResource)

responseCreateResolver :: CreateResolverResponse -> TestTree
responseCreateResolver =
  res
    "CreateResolverResponse"
    "fixture/CreateResolverResponse.proto"
    defaultService
    (Proxy :: Proxy CreateResolver)
