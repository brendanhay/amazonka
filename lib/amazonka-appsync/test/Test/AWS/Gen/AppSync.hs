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
--             updateDataSource
--
--         , requestDeleteDataSource $
--             deleteDataSource
--
--         , requestCreateType $
--             createType
--
--         , requestGetGraphqlAPI $
--             getGraphqlAPI
--
--         , requestListTagsForResource $
--             listTagsForResource
--
--         , requestCreateGraphqlAPI $
--             createGraphqlAPI
--
--         , requestStartSchemaCreation $
--             startSchemaCreation
--
--         , requestFlushAPICache $
--             flushAPICache
--
--         , requestDeleteGraphqlAPI $
--             deleteGraphqlAPI
--
--         , requestUpdateGraphqlAPI $
--             updateGraphqlAPI
--
--         , requestGetIntrospectionSchema $
--             getIntrospectionSchema
--
--         , requestGetDataSource $
--             getDataSource
--
--         , requestListResolversByFunction $
--             listResolversByFunction
--
--         , requestCreateFunction $
--             createFunction
--
--         , requestDeleteAPIKey $
--             deleteAPIKey
--
--         , requestUpdateAPIKey $
--             updateAPIKey
--
--         , requestUpdateType $
--             updateType
--
--         , requestDeleteType $
--             deleteType
--
--         , requestCreateDataSource $
--             createDataSource
--
--         , requestListTypes $
--             listTypes
--
--         , requestGetFunction $
--             getFunction
--
--         , requestListDataSources $
--             listDataSources
--
--         , requestUpdateResolver $
--             updateResolver
--
--         , requestDeleteResolver $
--             deleteResolver
--
--         , requestListResolvers $
--             listResolvers
--
--         , requestCreateResolver $
--             createResolver
--
--         , requestGetSchemaCreationStatus $
--             getSchemaCreationStatus
--
--         , requestGetAPICache $
--             getAPICache
--
--         , requestUpdateAPICache $
--             updateAPICache
--
--         , requestDeleteAPICache $
--             deleteAPICache
--
--         , requestListGraphqlAPIs $
--             listGraphqlAPIs
--
--         , requestCreateAPICache $
--             createAPICache
--
--         , requestTagResource $
--             tagResource
--
--         , requestGetResolver $
--             getResolver
--
--         , requestUpdateFunction $
--             updateFunction
--
--         , requestDeleteFunction $
--             deleteFunction
--
--         , requestUntagResource $
--             untagResource
--
--         , requestCreateAPIKey $
--             createAPIKey
--
--         , requestListFunctions $
--             listFunctions
--
--         , requestListAPIKeys $
--             listAPIKeys
--
--         , requestGetType $
--             getType
--
--           ]

--     , testGroup "response"
--         [ responseUpdateDataSource $
--             updateDataSourceResponse
--
--         , responseDeleteDataSource $
--             deleteDataSourceResponse
--
--         , responseCreateType $
--             createTypeResponse
--
--         , responseGetGraphqlAPI $
--             getGraphqlAPIResponse
--
--         , responseListTagsForResource $
--             listTagsForResourceResponse
--
--         , responseCreateGraphqlAPI $
--             createGraphqlAPIResponse
--
--         , responseStartSchemaCreation $
--             startSchemaCreationResponse
--
--         , responseFlushAPICache $
--             flushAPICacheResponse
--
--         , responseDeleteGraphqlAPI $
--             deleteGraphqlAPIResponse
--
--         , responseUpdateGraphqlAPI $
--             updateGraphqlAPIResponse
--
--         , responseGetIntrospectionSchema $
--             getIntrospectionSchemaResponse
--
--         , responseGetDataSource $
--             getDataSourceResponse
--
--         , responseListResolversByFunction $
--             listResolversByFunctionResponse
--
--         , responseCreateFunction $
--             createFunctionResponse
--
--         , responseDeleteAPIKey $
--             deleteAPIKeyResponse
--
--         , responseUpdateAPIKey $
--             updateAPIKeyResponse
--
--         , responseUpdateType $
--             updateTypeResponse
--
--         , responseDeleteType $
--             deleteTypeResponse
--
--         , responseCreateDataSource $
--             createDataSourceResponse
--
--         , responseListTypes $
--             listTypesResponse
--
--         , responseGetFunction $
--             getFunctionResponse
--
--         , responseListDataSources $
--             listDataSourcesResponse
--
--         , responseUpdateResolver $
--             updateResolverResponse
--
--         , responseDeleteResolver $
--             deleteResolverResponse
--
--         , responseListResolvers $
--             listResolversResponse
--
--         , responseCreateResolver $
--             createResolverResponse
--
--         , responseGetSchemaCreationStatus $
--             getSchemaCreationStatusResponse
--
--         , responseGetAPICache $
--             getAPICacheResponse
--
--         , responseUpdateAPICache $
--             updateAPICacheResponse
--
--         , responseDeleteAPICache $
--             deleteAPICacheResponse
--
--         , responseListGraphqlAPIs $
--             listGraphqlAPIsResponse
--
--         , responseCreateAPICache $
--             createAPICacheResponse
--
--         , responseTagResource $
--             tagResourceResponse
--
--         , responseGetResolver $
--             getResolverResponse
--
--         , responseUpdateFunction $
--             updateFunctionResponse
--
--         , responseDeleteFunction $
--             deleteFunctionResponse
--
--         , responseUntagResource $
--             untagResourceResponse
--
--         , responseCreateAPIKey $
--             createAPIKeyResponse
--
--         , responseListFunctions $
--             listFunctionsResponse
--
--         , responseListAPIKeys $
--             listAPIKeysResponse
--
--         , responseGetType $
--             getTypeResponse
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

requestGetGraphqlAPI :: GetGraphqlAPI -> TestTree
requestGetGraphqlAPI =
  req
    "GetGraphqlAPI"
    "fixture/GetGraphqlAPI.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestCreateGraphqlAPI :: CreateGraphqlAPI -> TestTree
requestCreateGraphqlAPI =
  req
    "CreateGraphqlAPI"
    "fixture/CreateGraphqlAPI.yaml"

requestStartSchemaCreation :: StartSchemaCreation -> TestTree
requestStartSchemaCreation =
  req
    "StartSchemaCreation"
    "fixture/StartSchemaCreation.yaml"

requestFlushAPICache :: FlushAPICache -> TestTree
requestFlushAPICache =
  req
    "FlushAPICache"
    "fixture/FlushAPICache.yaml"

requestDeleteGraphqlAPI :: DeleteGraphqlAPI -> TestTree
requestDeleteGraphqlAPI =
  req
    "DeleteGraphqlAPI"
    "fixture/DeleteGraphqlAPI.yaml"

requestUpdateGraphqlAPI :: UpdateGraphqlAPI -> TestTree
requestUpdateGraphqlAPI =
  req
    "UpdateGraphqlAPI"
    "fixture/UpdateGraphqlAPI.yaml"

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

requestDeleteAPIKey :: DeleteAPIKey -> TestTree
requestDeleteAPIKey =
  req
    "DeleteAPIKey"
    "fixture/DeleteAPIKey.yaml"

requestUpdateAPIKey :: UpdateAPIKey -> TestTree
requestUpdateAPIKey =
  req
    "UpdateAPIKey"
    "fixture/UpdateAPIKey.yaml"

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

requestGetAPICache :: GetAPICache -> TestTree
requestGetAPICache =
  req
    "GetAPICache"
    "fixture/GetAPICache.yaml"

requestUpdateAPICache :: UpdateAPICache -> TestTree
requestUpdateAPICache =
  req
    "UpdateAPICache"
    "fixture/UpdateAPICache.yaml"

requestDeleteAPICache :: DeleteAPICache -> TestTree
requestDeleteAPICache =
  req
    "DeleteAPICache"
    "fixture/DeleteAPICache.yaml"

requestListGraphqlAPIs :: ListGraphqlAPIs -> TestTree
requestListGraphqlAPIs =
  req
    "ListGraphqlAPIs"
    "fixture/ListGraphqlAPIs.yaml"

requestCreateAPICache :: CreateAPICache -> TestTree
requestCreateAPICache =
  req
    "CreateAPICache"
    "fixture/CreateAPICache.yaml"

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

requestCreateAPIKey :: CreateAPIKey -> TestTree
requestCreateAPIKey =
  req
    "CreateAPIKey"
    "fixture/CreateAPIKey.yaml"

requestListFunctions :: ListFunctions -> TestTree
requestListFunctions =
  req
    "ListFunctions"
    "fixture/ListFunctions.yaml"

requestListAPIKeys :: ListAPIKeys -> TestTree
requestListAPIKeys =
  req
    "ListAPIKeys"
    "fixture/ListAPIKeys.yaml"

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
    appSync
    (Proxy :: Proxy UpdateDataSource)

responseDeleteDataSource :: DeleteDataSourceResponse -> TestTree
responseDeleteDataSource =
  res
    "DeleteDataSourceResponse"
    "fixture/DeleteDataSourceResponse.proto"
    appSync
    (Proxy :: Proxy DeleteDataSource)

responseCreateType :: CreateTypeResponse -> TestTree
responseCreateType =
  res
    "CreateTypeResponse"
    "fixture/CreateTypeResponse.proto"
    appSync
    (Proxy :: Proxy CreateType)

responseGetGraphqlAPI :: GetGraphqlAPIResponse -> TestTree
responseGetGraphqlAPI =
  res
    "GetGraphqlAPIResponse"
    "fixture/GetGraphqlAPIResponse.proto"
    appSync
    (Proxy :: Proxy GetGraphqlAPI)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    appSync
    (Proxy :: Proxy ListTagsForResource)

responseCreateGraphqlAPI :: CreateGraphqlAPIResponse -> TestTree
responseCreateGraphqlAPI =
  res
    "CreateGraphqlAPIResponse"
    "fixture/CreateGraphqlAPIResponse.proto"
    appSync
    (Proxy :: Proxy CreateGraphqlAPI)

responseStartSchemaCreation :: StartSchemaCreationResponse -> TestTree
responseStartSchemaCreation =
  res
    "StartSchemaCreationResponse"
    "fixture/StartSchemaCreationResponse.proto"
    appSync
    (Proxy :: Proxy StartSchemaCreation)

responseFlushAPICache :: FlushAPICacheResponse -> TestTree
responseFlushAPICache =
  res
    "FlushAPICacheResponse"
    "fixture/FlushAPICacheResponse.proto"
    appSync
    (Proxy :: Proxy FlushAPICache)

responseDeleteGraphqlAPI :: DeleteGraphqlAPIResponse -> TestTree
responseDeleteGraphqlAPI =
  res
    "DeleteGraphqlAPIResponse"
    "fixture/DeleteGraphqlAPIResponse.proto"
    appSync
    (Proxy :: Proxy DeleteGraphqlAPI)

responseUpdateGraphqlAPI :: UpdateGraphqlAPIResponse -> TestTree
responseUpdateGraphqlAPI =
  res
    "UpdateGraphqlAPIResponse"
    "fixture/UpdateGraphqlAPIResponse.proto"
    appSync
    (Proxy :: Proxy UpdateGraphqlAPI)

responseGetIntrospectionSchema :: GetIntrospectionSchemaResponse -> TestTree
responseGetIntrospectionSchema =
  res
    "GetIntrospectionSchemaResponse"
    "fixture/GetIntrospectionSchemaResponse.proto"
    appSync
    (Proxy :: Proxy GetIntrospectionSchema)

responseGetDataSource :: GetDataSourceResponse -> TestTree
responseGetDataSource =
  res
    "GetDataSourceResponse"
    "fixture/GetDataSourceResponse.proto"
    appSync
    (Proxy :: Proxy GetDataSource)

responseListResolversByFunction :: ListResolversByFunctionResponse -> TestTree
responseListResolversByFunction =
  res
    "ListResolversByFunctionResponse"
    "fixture/ListResolversByFunctionResponse.proto"
    appSync
    (Proxy :: Proxy ListResolversByFunction)

responseCreateFunction :: CreateFunctionResponse -> TestTree
responseCreateFunction =
  res
    "CreateFunctionResponse"
    "fixture/CreateFunctionResponse.proto"
    appSync
    (Proxy :: Proxy CreateFunction)

responseDeleteAPIKey :: DeleteAPIKeyResponse -> TestTree
responseDeleteAPIKey =
  res
    "DeleteAPIKeyResponse"
    "fixture/DeleteAPIKeyResponse.proto"
    appSync
    (Proxy :: Proxy DeleteAPIKey)

responseUpdateAPIKey :: UpdateAPIKeyResponse -> TestTree
responseUpdateAPIKey =
  res
    "UpdateAPIKeyResponse"
    "fixture/UpdateAPIKeyResponse.proto"
    appSync
    (Proxy :: Proxy UpdateAPIKey)

responseUpdateType :: UpdateTypeResponse -> TestTree
responseUpdateType =
  res
    "UpdateTypeResponse"
    "fixture/UpdateTypeResponse.proto"
    appSync
    (Proxy :: Proxy UpdateType)

responseDeleteType :: DeleteTypeResponse -> TestTree
responseDeleteType =
  res
    "DeleteTypeResponse"
    "fixture/DeleteTypeResponse.proto"
    appSync
    (Proxy :: Proxy DeleteType)

responseCreateDataSource :: CreateDataSourceResponse -> TestTree
responseCreateDataSource =
  res
    "CreateDataSourceResponse"
    "fixture/CreateDataSourceResponse.proto"
    appSync
    (Proxy :: Proxy CreateDataSource)

responseListTypes :: ListTypesResponse -> TestTree
responseListTypes =
  res
    "ListTypesResponse"
    "fixture/ListTypesResponse.proto"
    appSync
    (Proxy :: Proxy ListTypes)

responseGetFunction :: GetFunctionResponse -> TestTree
responseGetFunction =
  res
    "GetFunctionResponse"
    "fixture/GetFunctionResponse.proto"
    appSync
    (Proxy :: Proxy GetFunction)

responseListDataSources :: ListDataSourcesResponse -> TestTree
responseListDataSources =
  res
    "ListDataSourcesResponse"
    "fixture/ListDataSourcesResponse.proto"
    appSync
    (Proxy :: Proxy ListDataSources)

responseUpdateResolver :: UpdateResolverResponse -> TestTree
responseUpdateResolver =
  res
    "UpdateResolverResponse"
    "fixture/UpdateResolverResponse.proto"
    appSync
    (Proxy :: Proxy UpdateResolver)

responseDeleteResolver :: DeleteResolverResponse -> TestTree
responseDeleteResolver =
  res
    "DeleteResolverResponse"
    "fixture/DeleteResolverResponse.proto"
    appSync
    (Proxy :: Proxy DeleteResolver)

responseListResolvers :: ListResolversResponse -> TestTree
responseListResolvers =
  res
    "ListResolversResponse"
    "fixture/ListResolversResponse.proto"
    appSync
    (Proxy :: Proxy ListResolvers)

responseCreateResolver :: CreateResolverResponse -> TestTree
responseCreateResolver =
  res
    "CreateResolverResponse"
    "fixture/CreateResolverResponse.proto"
    appSync
    (Proxy :: Proxy CreateResolver)

responseGetSchemaCreationStatus :: GetSchemaCreationStatusResponse -> TestTree
responseGetSchemaCreationStatus =
  res
    "GetSchemaCreationStatusResponse"
    "fixture/GetSchemaCreationStatusResponse.proto"
    appSync
    (Proxy :: Proxy GetSchemaCreationStatus)

responseGetAPICache :: GetAPICacheResponse -> TestTree
responseGetAPICache =
  res
    "GetAPICacheResponse"
    "fixture/GetAPICacheResponse.proto"
    appSync
    (Proxy :: Proxy GetAPICache)

responseUpdateAPICache :: UpdateAPICacheResponse -> TestTree
responseUpdateAPICache =
  res
    "UpdateAPICacheResponse"
    "fixture/UpdateAPICacheResponse.proto"
    appSync
    (Proxy :: Proxy UpdateAPICache)

responseDeleteAPICache :: DeleteAPICacheResponse -> TestTree
responseDeleteAPICache =
  res
    "DeleteAPICacheResponse"
    "fixture/DeleteAPICacheResponse.proto"
    appSync
    (Proxy :: Proxy DeleteAPICache)

responseListGraphqlAPIs :: ListGraphqlAPIsResponse -> TestTree
responseListGraphqlAPIs =
  res
    "ListGraphqlAPIsResponse"
    "fixture/ListGraphqlAPIsResponse.proto"
    appSync
    (Proxy :: Proxy ListGraphqlAPIs)

responseCreateAPICache :: CreateAPICacheResponse -> TestTree
responseCreateAPICache =
  res
    "CreateAPICacheResponse"
    "fixture/CreateAPICacheResponse.proto"
    appSync
    (Proxy :: Proxy CreateAPICache)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    appSync
    (Proxy :: Proxy TagResource)

responseGetResolver :: GetResolverResponse -> TestTree
responseGetResolver =
  res
    "GetResolverResponse"
    "fixture/GetResolverResponse.proto"
    appSync
    (Proxy :: Proxy GetResolver)

responseUpdateFunction :: UpdateFunctionResponse -> TestTree
responseUpdateFunction =
  res
    "UpdateFunctionResponse"
    "fixture/UpdateFunctionResponse.proto"
    appSync
    (Proxy :: Proxy UpdateFunction)

responseDeleteFunction :: DeleteFunctionResponse -> TestTree
responseDeleteFunction =
  res
    "DeleteFunctionResponse"
    "fixture/DeleteFunctionResponse.proto"
    appSync
    (Proxy :: Proxy DeleteFunction)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    appSync
    (Proxy :: Proxy UntagResource)

responseCreateAPIKey :: CreateAPIKeyResponse -> TestTree
responseCreateAPIKey =
  res
    "CreateAPIKeyResponse"
    "fixture/CreateAPIKeyResponse.proto"
    appSync
    (Proxy :: Proxy CreateAPIKey)

responseListFunctions :: ListFunctionsResponse -> TestTree
responseListFunctions =
  res
    "ListFunctionsResponse"
    "fixture/ListFunctionsResponse.proto"
    appSync
    (Proxy :: Proxy ListFunctions)

responseListAPIKeys :: ListAPIKeysResponse -> TestTree
responseListAPIKeys =
  res
    "ListAPIKeysResponse"
    "fixture/ListAPIKeysResponse.proto"
    appSync
    (Proxy :: Proxy ListAPIKeys)

responseGetType :: GetTypeResponse -> TestTree
responseGetType =
  res
    "GetTypeResponse"
    "fixture/GetTypeResponse.proto"
    appSync
    (Proxy :: Proxy GetType)
