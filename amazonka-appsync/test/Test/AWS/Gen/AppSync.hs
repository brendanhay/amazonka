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
--         , requestGetFunction $
--             newGetFunction
--
--         , requestCreateType $
--             newCreateType
--
--         , requestDeleteDataSource $
--             newDeleteDataSource
--
--         , requestUpdateType $
--             newUpdateType
--
--         , requestCreateDataSource $
--             newCreateDataSource
--
--         , requestDeleteType $
--             newDeleteType
--
--         , requestListResolversByFunction $
--             newListResolversByFunction
--
--         , requestListFunctions $
--             newListFunctions
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestDeleteGraphqlApi $
--             newDeleteGraphqlApi
--
--         , requestGetResolver $
--             newGetResolver
--
--         , requestGetIntrospectionSchema $
--             newGetIntrospectionSchema
--
--         , requestTagResource $
--             newTagResource
--
--         , requestListGraphqlApis $
--             newListGraphqlApis
--
--         , requestUpdateGraphqlApi $
--             newUpdateGraphqlApi
--
--         , requestUpdateApiCache $
--             newUpdateApiCache
--
--         , requestDeleteApiCache $
--             newDeleteApiCache
--
--         , requestGetSchemaCreationStatus $
--             newGetSchemaCreationStatus
--
--         , requestGetApiCache $
--             newGetApiCache
--
--         , requestGetGraphqlApi $
--             newGetGraphqlApi
--
--         , requestUpdateResolver $
--             newUpdateResolver
--
--         , requestDeleteResolver $
--             newDeleteResolver
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
--         , requestDeleteApiKey $
--             newDeleteApiKey
--
--         , requestListApiKeys $
--             newListApiKeys
--
--         , requestUpdateApiKey $
--             newUpdateApiKey
--
--         , requestCreateApiKey $
--             newCreateApiKey
--
--         , requestDeleteFunction $
--             newDeleteFunction
--
--         , requestUpdateFunction $
--             newUpdateFunction
--
--         , requestGetDataSource $
--             newGetDataSource
--
--         , requestStartSchemaCreation $
--             newStartSchemaCreation
--
--         , requestCreateApiCache $
--             newCreateApiCache
--
--         , requestFlushApiCache $
--             newFlushApiCache
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
--         , responseGetFunction $
--             newGetFunctionResponse
--
--         , responseCreateType $
--             newCreateTypeResponse
--
--         , responseDeleteDataSource $
--             newDeleteDataSourceResponse
--
--         , responseUpdateType $
--             newUpdateTypeResponse
--
--         , responseCreateDataSource $
--             newCreateDataSourceResponse
--
--         , responseDeleteType $
--             newDeleteTypeResponse
--
--         , responseListResolversByFunction $
--             newListResolversByFunctionResponse
--
--         , responseListFunctions $
--             newListFunctionsResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseDeleteGraphqlApi $
--             newDeleteGraphqlApiResponse
--
--         , responseGetResolver $
--             newGetResolverResponse
--
--         , responseGetIntrospectionSchema $
--             newGetIntrospectionSchemaResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseListGraphqlApis $
--             newListGraphqlApisResponse
--
--         , responseUpdateGraphqlApi $
--             newUpdateGraphqlApiResponse
--
--         , responseUpdateApiCache $
--             newUpdateApiCacheResponse
--
--         , responseDeleteApiCache $
--             newDeleteApiCacheResponse
--
--         , responseGetSchemaCreationStatus $
--             newGetSchemaCreationStatusResponse
--
--         , responseGetApiCache $
--             newGetApiCacheResponse
--
--         , responseGetGraphqlApi $
--             newGetGraphqlApiResponse
--
--         , responseUpdateResolver $
--             newUpdateResolverResponse
--
--         , responseDeleteResolver $
--             newDeleteResolverResponse
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
--         , responseDeleteApiKey $
--             newDeleteApiKeyResponse
--
--         , responseListApiKeys $
--             newListApiKeysResponse
--
--         , responseUpdateApiKey $
--             newUpdateApiKeyResponse
--
--         , responseCreateApiKey $
--             newCreateApiKeyResponse
--
--         , responseDeleteFunction $
--             newDeleteFunctionResponse
--
--         , responseUpdateFunction $
--             newUpdateFunctionResponse
--
--         , responseGetDataSource $
--             newGetDataSourceResponse
--
--         , responseStartSchemaCreation $
--             newStartSchemaCreationResponse
--
--         , responseCreateApiCache $
--             newCreateApiCacheResponse
--
--         , responseFlushApiCache $
--             newFlushApiCacheResponse
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

requestGetFunction :: GetFunction -> TestTree
requestGetFunction =
  req
    "GetFunction"
    "fixture/GetFunction.yaml"

requestCreateType :: CreateType -> TestTree
requestCreateType =
  req
    "CreateType"
    "fixture/CreateType.yaml"

requestDeleteDataSource :: DeleteDataSource -> TestTree
requestDeleteDataSource =
  req
    "DeleteDataSource"
    "fixture/DeleteDataSource.yaml"

requestUpdateType :: UpdateType -> TestTree
requestUpdateType =
  req
    "UpdateType"
    "fixture/UpdateType.yaml"

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

requestListResolversByFunction :: ListResolversByFunction -> TestTree
requestListResolversByFunction =
  req
    "ListResolversByFunction"
    "fixture/ListResolversByFunction.yaml"

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

requestGetIntrospectionSchema :: GetIntrospectionSchema -> TestTree
requestGetIntrospectionSchema =
  req
    "GetIntrospectionSchema"
    "fixture/GetIntrospectionSchema.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestListGraphqlApis :: ListGraphqlApis -> TestTree
requestListGraphqlApis =
  req
    "ListGraphqlApis"
    "fixture/ListGraphqlApis.yaml"

requestUpdateGraphqlApi :: UpdateGraphqlApi -> TestTree
requestUpdateGraphqlApi =
  req
    "UpdateGraphqlApi"
    "fixture/UpdateGraphqlApi.yaml"

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

requestGetGraphqlApi :: GetGraphqlApi -> TestTree
requestGetGraphqlApi =
  req
    "GetGraphqlApi"
    "fixture/GetGraphqlApi.yaml"

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

requestDeleteApiKey :: DeleteApiKey -> TestTree
requestDeleteApiKey =
  req
    "DeleteApiKey"
    "fixture/DeleteApiKey.yaml"

requestListApiKeys :: ListApiKeys -> TestTree
requestListApiKeys =
  req
    "ListApiKeys"
    "fixture/ListApiKeys.yaml"

requestUpdateApiKey :: UpdateApiKey -> TestTree
requestUpdateApiKey =
  req
    "UpdateApiKey"
    "fixture/UpdateApiKey.yaml"

requestCreateApiKey :: CreateApiKey -> TestTree
requestCreateApiKey =
  req
    "CreateApiKey"
    "fixture/CreateApiKey.yaml"

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

requestGetDataSource :: GetDataSource -> TestTree
requestGetDataSource =
  req
    "GetDataSource"
    "fixture/GetDataSource.yaml"

requestStartSchemaCreation :: StartSchemaCreation -> TestTree
requestStartSchemaCreation =
  req
    "StartSchemaCreation"
    "fixture/StartSchemaCreation.yaml"

requestCreateApiCache :: CreateApiCache -> TestTree
requestCreateApiCache =
  req
    "CreateApiCache"
    "fixture/CreateApiCache.yaml"

requestFlushApiCache :: FlushApiCache -> TestTree
requestFlushApiCache =
  req
    "FlushApiCache"
    "fixture/FlushApiCache.yaml"

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

responseGetFunction :: GetFunctionResponse -> TestTree
responseGetFunction =
  res
    "GetFunctionResponse"
    "fixture/GetFunctionResponse.proto"
    defaultService
    (Proxy :: Proxy GetFunction)

responseCreateType :: CreateTypeResponse -> TestTree
responseCreateType =
  res
    "CreateTypeResponse"
    "fixture/CreateTypeResponse.proto"
    defaultService
    (Proxy :: Proxy CreateType)

responseDeleteDataSource :: DeleteDataSourceResponse -> TestTree
responseDeleteDataSource =
  res
    "DeleteDataSourceResponse"
    "fixture/DeleteDataSourceResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteDataSource)

responseUpdateType :: UpdateTypeResponse -> TestTree
responseUpdateType =
  res
    "UpdateTypeResponse"
    "fixture/UpdateTypeResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateType)

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

responseListResolversByFunction :: ListResolversByFunctionResponse -> TestTree
responseListResolversByFunction =
  res
    "ListResolversByFunctionResponse"
    "fixture/ListResolversByFunctionResponse.proto"
    defaultService
    (Proxy :: Proxy ListResolversByFunction)

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

responseGetIntrospectionSchema :: GetIntrospectionSchemaResponse -> TestTree
responseGetIntrospectionSchema =
  res
    "GetIntrospectionSchemaResponse"
    "fixture/GetIntrospectionSchemaResponse.proto"
    defaultService
    (Proxy :: Proxy GetIntrospectionSchema)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy :: Proxy TagResource)

responseListGraphqlApis :: ListGraphqlApisResponse -> TestTree
responseListGraphqlApis =
  res
    "ListGraphqlApisResponse"
    "fixture/ListGraphqlApisResponse.proto"
    defaultService
    (Proxy :: Proxy ListGraphqlApis)

responseUpdateGraphqlApi :: UpdateGraphqlApiResponse -> TestTree
responseUpdateGraphqlApi =
  res
    "UpdateGraphqlApiResponse"
    "fixture/UpdateGraphqlApiResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateGraphqlApi)

responseUpdateApiCache :: UpdateApiCacheResponse -> TestTree
responseUpdateApiCache =
  res
    "UpdateApiCacheResponse"
    "fixture/UpdateApiCacheResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateApiCache)

responseDeleteApiCache :: DeleteApiCacheResponse -> TestTree
responseDeleteApiCache =
  res
    "DeleteApiCacheResponse"
    "fixture/DeleteApiCacheResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteApiCache)

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

responseGetGraphqlApi :: GetGraphqlApiResponse -> TestTree
responseGetGraphqlApi =
  res
    "GetGraphqlApiResponse"
    "fixture/GetGraphqlApiResponse.proto"
    defaultService
    (Proxy :: Proxy GetGraphqlApi)

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

responseDeleteApiKey :: DeleteApiKeyResponse -> TestTree
responseDeleteApiKey =
  res
    "DeleteApiKeyResponse"
    "fixture/DeleteApiKeyResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteApiKey)

responseListApiKeys :: ListApiKeysResponse -> TestTree
responseListApiKeys =
  res
    "ListApiKeysResponse"
    "fixture/ListApiKeysResponse.proto"
    defaultService
    (Proxy :: Proxy ListApiKeys)

responseUpdateApiKey :: UpdateApiKeyResponse -> TestTree
responseUpdateApiKey =
  res
    "UpdateApiKeyResponse"
    "fixture/UpdateApiKeyResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateApiKey)

responseCreateApiKey :: CreateApiKeyResponse -> TestTree
responseCreateApiKey =
  res
    "CreateApiKeyResponse"
    "fixture/CreateApiKeyResponse.proto"
    defaultService
    (Proxy :: Proxy CreateApiKey)

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

responseGetDataSource :: GetDataSourceResponse -> TestTree
responseGetDataSource =
  res
    "GetDataSourceResponse"
    "fixture/GetDataSourceResponse.proto"
    defaultService
    (Proxy :: Proxy GetDataSource)

responseStartSchemaCreation :: StartSchemaCreationResponse -> TestTree
responseStartSchemaCreation =
  res
    "StartSchemaCreationResponse"
    "fixture/StartSchemaCreationResponse.proto"
    defaultService
    (Proxy :: Proxy StartSchemaCreation)

responseCreateApiCache :: CreateApiCacheResponse -> TestTree
responseCreateApiCache =
  res
    "CreateApiCacheResponse"
    "fixture/CreateApiCacheResponse.proto"
    defaultService
    (Proxy :: Proxy CreateApiCache)

responseFlushApiCache :: FlushApiCacheResponse -> TestTree
responseFlushApiCache =
  res
    "FlushApiCacheResponse"
    "fixture/FlushApiCacheResponse.proto"
    defaultService
    (Proxy :: Proxy FlushApiCache)

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
