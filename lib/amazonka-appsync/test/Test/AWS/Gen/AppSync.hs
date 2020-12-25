{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.AppSync
-- Copyright   : (c) 2013-2020 Brendan Hay
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
--         [ requestUpdateDataSource $
--             mkUpdateDataSource
--
--         , requestDeleteDataSource $
--             mkDeleteDataSource
--
--         , requestCreateType $
--             mkCreateType
--
--         , requestGetGraphqlApi $
--             mkGetGraphqlApi
--
--         , requestListTagsForResource $
--             mkListTagsForResource
--
--         , requestCreateGraphqlApi $
--             mkCreateGraphqlApi
--
--         , requestStartSchemaCreation $
--             mkStartSchemaCreation
--
--         , requestFlushApiCache $
--             mkFlushApiCache
--
--         , requestDeleteGraphqlApi $
--             mkDeleteGraphqlApi
--
--         , requestUpdateGraphqlApi $
--             mkUpdateGraphqlApi
--
--         , requestGetIntrospectionSchema $
--             mkGetIntrospectionSchema
--
--         , requestGetDataSource $
--             mkGetDataSource
--
--         , requestListResolversByFunction $
--             mkListResolversByFunction
--
--         , requestCreateFunction $
--             mkCreateFunction
--
--         , requestDeleteApiKey $
--             mkDeleteApiKey
--
--         , requestUpdateApiKey $
--             mkUpdateApiKey
--
--         , requestUpdateType $
--             mkUpdateType
--
--         , requestDeleteType $
--             mkDeleteType
--
--         , requestCreateDataSource $
--             mkCreateDataSource
--
--         , requestListTypes $
--             mkListTypes
--
--         , requestGetFunction $
--             mkGetFunction
--
--         , requestListDataSources $
--             mkListDataSources
--
--         , requestUpdateResolver $
--             mkUpdateResolver
--
--         , requestDeleteResolver $
--             mkDeleteResolver
--
--         , requestListResolvers $
--             mkListResolvers
--
--         , requestCreateResolver $
--             mkCreateResolver
--
--         , requestGetSchemaCreationStatus $
--             mkGetSchemaCreationStatus
--
--         , requestGetApiCache $
--             mkGetApiCache
--
--         , requestUpdateApiCache $
--             mkUpdateApiCache
--
--         , requestDeleteApiCache $
--             mkDeleteApiCache
--
--         , requestListGraphqlApis $
--             mkListGraphqlApis
--
--         , requestCreateApiCache $
--             mkCreateApiCache
--
--         , requestTagResource $
--             mkTagResource
--
--         , requestGetResolver $
--             mkGetResolver
--
--         , requestUpdateFunction $
--             mkUpdateFunction
--
--         , requestDeleteFunction $
--             mkDeleteFunction
--
--         , requestUntagResource $
--             mkUntagResource
--
--         , requestCreateApiKey $
--             mkCreateApiKey
--
--         , requestListFunctions $
--             mkListFunctions
--
--         , requestListApiKeys $
--             mkListApiKeys
--
--         , requestGetType $
--             mkGetType
--
--           ]

--     , testGroup "response"
--         [ responseUpdateDataSource $
--             mkUpdateDataSourceResponse
--
--         , responseDeleteDataSource $
--             mkDeleteDataSourceResponse
--
--         , responseCreateType $
--             mkCreateTypeResponse
--
--         , responseGetGraphqlApi $
--             mkGetGraphqlApiResponse
--
--         , responseListTagsForResource $
--             mkListTagsForResourceResponse
--
--         , responseCreateGraphqlApi $
--             mkCreateGraphqlApiResponse
--
--         , responseStartSchemaCreation $
--             mkStartSchemaCreationResponse
--
--         , responseFlushApiCache $
--             mkFlushApiCacheResponse
--
--         , responseDeleteGraphqlApi $
--             mkDeleteGraphqlApiResponse
--
--         , responseUpdateGraphqlApi $
--             mkUpdateGraphqlApiResponse
--
--         , responseGetIntrospectionSchema $
--             mkGetIntrospectionSchemaResponse
--
--         , responseGetDataSource $
--             mkGetDataSourceResponse
--
--         , responseListResolversByFunction $
--             mkListResolversByFunctionResponse
--
--         , responseCreateFunction $
--             mkCreateFunctionResponse
--
--         , responseDeleteApiKey $
--             mkDeleteApiKeyResponse
--
--         , responseUpdateApiKey $
--             mkUpdateApiKeyResponse
--
--         , responseUpdateType $
--             mkUpdateTypeResponse
--
--         , responseDeleteType $
--             mkDeleteTypeResponse
--
--         , responseCreateDataSource $
--             mkCreateDataSourceResponse
--
--         , responseListTypes $
--             mkListTypesResponse
--
--         , responseGetFunction $
--             mkGetFunctionResponse
--
--         , responseListDataSources $
--             mkListDataSourcesResponse
--
--         , responseUpdateResolver $
--             mkUpdateResolverResponse
--
--         , responseDeleteResolver $
--             mkDeleteResolverResponse
--
--         , responseListResolvers $
--             mkListResolversResponse
--
--         , responseCreateResolver $
--             mkCreateResolverResponse
--
--         , responseGetSchemaCreationStatus $
--             mkGetSchemaCreationStatusResponse
--
--         , responseGetApiCache $
--             mkGetApiCacheResponse
--
--         , responseUpdateApiCache $
--             mkUpdateApiCacheResponse
--
--         , responseDeleteApiCache $
--             mkDeleteApiCacheResponse
--
--         , responseListGraphqlApis $
--             mkListGraphqlApisResponse
--
--         , responseCreateApiCache $
--             mkCreateApiCacheResponse
--
--         , responseTagResource $
--             mkTagResourceResponse
--
--         , responseGetResolver $
--             mkGetResolverResponse
--
--         , responseUpdateFunction $
--             mkUpdateFunctionResponse
--
--         , responseDeleteFunction $
--             mkDeleteFunctionResponse
--
--         , responseUntagResource $
--             mkUntagResourceResponse
--
--         , responseCreateApiKey $
--             mkCreateApiKeyResponse
--
--         , responseListFunctions $
--             mkListFunctionsResponse
--
--         , responseListApiKeys $
--             mkListApiKeysResponse
--
--         , responseGetType $
--             mkGetTypeResponse
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
    mkServiceConfig
    (Proxy :: Proxy UpdateDataSource)

responseDeleteDataSource :: DeleteDataSourceResponse -> TestTree
responseDeleteDataSource =
  res
    "DeleteDataSourceResponse"
    "fixture/DeleteDataSourceResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteDataSource)

responseCreateType :: CreateTypeResponse -> TestTree
responseCreateType =
  res
    "CreateTypeResponse"
    "fixture/CreateTypeResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateType)

responseGetGraphqlApi :: GetGraphqlApiResponse -> TestTree
responseGetGraphqlApi =
  res
    "GetGraphqlApiResponse"
    "fixture/GetGraphqlApiResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetGraphqlApi)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListTagsForResource)

responseCreateGraphqlApi :: CreateGraphqlApiResponse -> TestTree
responseCreateGraphqlApi =
  res
    "CreateGraphqlApiResponse"
    "fixture/CreateGraphqlApiResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateGraphqlApi)

responseStartSchemaCreation :: StartSchemaCreationResponse -> TestTree
responseStartSchemaCreation =
  res
    "StartSchemaCreationResponse"
    "fixture/StartSchemaCreationResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy StartSchemaCreation)

responseFlushApiCache :: FlushApiCacheResponse -> TestTree
responseFlushApiCache =
  res
    "FlushApiCacheResponse"
    "fixture/FlushApiCacheResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy FlushApiCache)

responseDeleteGraphqlApi :: DeleteGraphqlApiResponse -> TestTree
responseDeleteGraphqlApi =
  res
    "DeleteGraphqlApiResponse"
    "fixture/DeleteGraphqlApiResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteGraphqlApi)

responseUpdateGraphqlApi :: UpdateGraphqlApiResponse -> TestTree
responseUpdateGraphqlApi =
  res
    "UpdateGraphqlApiResponse"
    "fixture/UpdateGraphqlApiResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateGraphqlApi)

responseGetIntrospectionSchema :: GetIntrospectionSchemaResponse -> TestTree
responseGetIntrospectionSchema =
  res
    "GetIntrospectionSchemaResponse"
    "fixture/GetIntrospectionSchemaResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetIntrospectionSchema)

responseGetDataSource :: GetDataSourceResponse -> TestTree
responseGetDataSource =
  res
    "GetDataSourceResponse"
    "fixture/GetDataSourceResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetDataSource)

responseListResolversByFunction :: ListResolversByFunctionResponse -> TestTree
responseListResolversByFunction =
  res
    "ListResolversByFunctionResponse"
    "fixture/ListResolversByFunctionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListResolversByFunction)

responseCreateFunction :: CreateFunctionResponse -> TestTree
responseCreateFunction =
  res
    "CreateFunctionResponse"
    "fixture/CreateFunctionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateFunction)

responseDeleteApiKey :: DeleteApiKeyResponse -> TestTree
responseDeleteApiKey =
  res
    "DeleteApiKeyResponse"
    "fixture/DeleteApiKeyResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteApiKey)

responseUpdateApiKey :: UpdateApiKeyResponse -> TestTree
responseUpdateApiKey =
  res
    "UpdateApiKeyResponse"
    "fixture/UpdateApiKeyResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateApiKey)

responseUpdateType :: UpdateTypeResponse -> TestTree
responseUpdateType =
  res
    "UpdateTypeResponse"
    "fixture/UpdateTypeResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateType)

responseDeleteType :: DeleteTypeResponse -> TestTree
responseDeleteType =
  res
    "DeleteTypeResponse"
    "fixture/DeleteTypeResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteType)

responseCreateDataSource :: CreateDataSourceResponse -> TestTree
responseCreateDataSource =
  res
    "CreateDataSourceResponse"
    "fixture/CreateDataSourceResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateDataSource)

responseListTypes :: ListTypesResponse -> TestTree
responseListTypes =
  res
    "ListTypesResponse"
    "fixture/ListTypesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListTypes)

responseGetFunction :: GetFunctionResponse -> TestTree
responseGetFunction =
  res
    "GetFunctionResponse"
    "fixture/GetFunctionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetFunction)

responseListDataSources :: ListDataSourcesResponse -> TestTree
responseListDataSources =
  res
    "ListDataSourcesResponse"
    "fixture/ListDataSourcesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListDataSources)

responseUpdateResolver :: UpdateResolverResponse -> TestTree
responseUpdateResolver =
  res
    "UpdateResolverResponse"
    "fixture/UpdateResolverResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateResolver)

responseDeleteResolver :: DeleteResolverResponse -> TestTree
responseDeleteResolver =
  res
    "DeleteResolverResponse"
    "fixture/DeleteResolverResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteResolver)

responseListResolvers :: ListResolversResponse -> TestTree
responseListResolvers =
  res
    "ListResolversResponse"
    "fixture/ListResolversResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListResolvers)

responseCreateResolver :: CreateResolverResponse -> TestTree
responseCreateResolver =
  res
    "CreateResolverResponse"
    "fixture/CreateResolverResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateResolver)

responseGetSchemaCreationStatus :: GetSchemaCreationStatusResponse -> TestTree
responseGetSchemaCreationStatus =
  res
    "GetSchemaCreationStatusResponse"
    "fixture/GetSchemaCreationStatusResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetSchemaCreationStatus)

responseGetApiCache :: GetApiCacheResponse -> TestTree
responseGetApiCache =
  res
    "GetApiCacheResponse"
    "fixture/GetApiCacheResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetApiCache)

responseUpdateApiCache :: UpdateApiCacheResponse -> TestTree
responseUpdateApiCache =
  res
    "UpdateApiCacheResponse"
    "fixture/UpdateApiCacheResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateApiCache)

responseDeleteApiCache :: DeleteApiCacheResponse -> TestTree
responseDeleteApiCache =
  res
    "DeleteApiCacheResponse"
    "fixture/DeleteApiCacheResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteApiCache)

responseListGraphqlApis :: ListGraphqlApisResponse -> TestTree
responseListGraphqlApis =
  res
    "ListGraphqlApisResponse"
    "fixture/ListGraphqlApisResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListGraphqlApis)

responseCreateApiCache :: CreateApiCacheResponse -> TestTree
responseCreateApiCache =
  res
    "CreateApiCacheResponse"
    "fixture/CreateApiCacheResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateApiCache)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy TagResource)

responseGetResolver :: GetResolverResponse -> TestTree
responseGetResolver =
  res
    "GetResolverResponse"
    "fixture/GetResolverResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetResolver)

responseUpdateFunction :: UpdateFunctionResponse -> TestTree
responseUpdateFunction =
  res
    "UpdateFunctionResponse"
    "fixture/UpdateFunctionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateFunction)

responseDeleteFunction :: DeleteFunctionResponse -> TestTree
responseDeleteFunction =
  res
    "DeleteFunctionResponse"
    "fixture/DeleteFunctionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteFunction)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UntagResource)

responseCreateApiKey :: CreateApiKeyResponse -> TestTree
responseCreateApiKey =
  res
    "CreateApiKeyResponse"
    "fixture/CreateApiKeyResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateApiKey)

responseListFunctions :: ListFunctionsResponse -> TestTree
responseListFunctions =
  res
    "ListFunctionsResponse"
    "fixture/ListFunctionsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListFunctions)

responseListApiKeys :: ListApiKeysResponse -> TestTree
responseListApiKeys =
  res
    "ListApiKeysResponse"
    "fixture/ListApiKeysResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListApiKeys)

responseGetType :: GetTypeResponse -> TestTree
responseGetType =
  res
    "GetTypeResponse"
    "fixture/GetTypeResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetType)
