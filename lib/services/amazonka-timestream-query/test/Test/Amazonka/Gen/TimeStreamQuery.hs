{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.TimeStreamQuery
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.TimeStreamQuery where

import Amazonka.TimeStreamQuery
import qualified Data.Proxy as Proxy
import Test.Amazonka.Fixture
import Test.Amazonka.Prelude
import Test.Amazonka.TimeStreamQuery.Internal
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestCancelQuery $
--             newCancelQuery
--
--         , requestCreateScheduledQuery $
--             newCreateScheduledQuery
--
--         , requestDeleteScheduledQuery $
--             newDeleteScheduledQuery
--
--         , requestDescribeEndpoints $
--             newDescribeEndpoints
--
--         , requestDescribeScheduledQuery $
--             newDescribeScheduledQuery
--
--         , requestExecuteScheduledQuery $
--             newExecuteScheduledQuery
--
--         , requestListScheduledQueries $
--             newListScheduledQueries
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestPrepareQuery $
--             newPrepareQuery
--
--         , requestQuery $
--             newQuery
--
--         , requestTagResource $
--             newTagResource
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestUpdateScheduledQuery $
--             newUpdateScheduledQuery
--
--           ]

--     , testGroup "response"
--         [ responseCancelQuery $
--             newCancelQueryResponse
--
--         , responseCreateScheduledQuery $
--             newCreateScheduledQueryResponse
--
--         , responseDeleteScheduledQuery $
--             newDeleteScheduledQueryResponse
--
--         , responseDescribeEndpoints $
--             newDescribeEndpointsResponse
--
--         , responseDescribeScheduledQuery $
--             newDescribeScheduledQueryResponse
--
--         , responseExecuteScheduledQuery $
--             newExecuteScheduledQueryResponse
--
--         , responseListScheduledQueries $
--             newListScheduledQueriesResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responsePrepareQuery $
--             newPrepareQueryResponse
--
--         , responseQuery $
--             newQueryResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseUpdateScheduledQuery $
--             newUpdateScheduledQueryResponse
--
--           ]
--     ]

-- Requests

requestCancelQuery :: CancelQuery -> TestTree
requestCancelQuery =
  req
    "CancelQuery"
    "fixture/CancelQuery.yaml"

requestCreateScheduledQuery :: CreateScheduledQuery -> TestTree
requestCreateScheduledQuery =
  req
    "CreateScheduledQuery"
    "fixture/CreateScheduledQuery.yaml"

requestDeleteScheduledQuery :: DeleteScheduledQuery -> TestTree
requestDeleteScheduledQuery =
  req
    "DeleteScheduledQuery"
    "fixture/DeleteScheduledQuery.yaml"

requestDescribeEndpoints :: DescribeEndpoints -> TestTree
requestDescribeEndpoints =
  req
    "DescribeEndpoints"
    "fixture/DescribeEndpoints.yaml"

requestDescribeScheduledQuery :: DescribeScheduledQuery -> TestTree
requestDescribeScheduledQuery =
  req
    "DescribeScheduledQuery"
    "fixture/DescribeScheduledQuery.yaml"

requestExecuteScheduledQuery :: ExecuteScheduledQuery -> TestTree
requestExecuteScheduledQuery =
  req
    "ExecuteScheduledQuery"
    "fixture/ExecuteScheduledQuery.yaml"

requestListScheduledQueries :: ListScheduledQueries -> TestTree
requestListScheduledQueries =
  req
    "ListScheduledQueries"
    "fixture/ListScheduledQueries.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestPrepareQuery :: PrepareQuery -> TestTree
requestPrepareQuery =
  req
    "PrepareQuery"
    "fixture/PrepareQuery.yaml"

requestQuery :: Query -> TestTree
requestQuery =
  req
    "Query"
    "fixture/Query.yaml"

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

requestUpdateScheduledQuery :: UpdateScheduledQuery -> TestTree
requestUpdateScheduledQuery =
  req
    "UpdateScheduledQuery"
    "fixture/UpdateScheduledQuery.yaml"

-- Responses

responseCancelQuery :: CancelQueryResponse -> TestTree
responseCancelQuery =
  res
    "CancelQueryResponse"
    "fixture/CancelQueryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CancelQuery)

responseCreateScheduledQuery :: CreateScheduledQueryResponse -> TestTree
responseCreateScheduledQuery =
  res
    "CreateScheduledQueryResponse"
    "fixture/CreateScheduledQueryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateScheduledQuery)

responseDeleteScheduledQuery :: DeleteScheduledQueryResponse -> TestTree
responseDeleteScheduledQuery =
  res
    "DeleteScheduledQueryResponse"
    "fixture/DeleteScheduledQueryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteScheduledQuery)

responseDescribeEndpoints :: DescribeEndpointsResponse -> TestTree
responseDescribeEndpoints =
  res
    "DescribeEndpointsResponse"
    "fixture/DescribeEndpointsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeEndpoints)

responseDescribeScheduledQuery :: DescribeScheduledQueryResponse -> TestTree
responseDescribeScheduledQuery =
  res
    "DescribeScheduledQueryResponse"
    "fixture/DescribeScheduledQueryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeScheduledQuery)

responseExecuteScheduledQuery :: ExecuteScheduledQueryResponse -> TestTree
responseExecuteScheduledQuery =
  res
    "ExecuteScheduledQueryResponse"
    "fixture/ExecuteScheduledQueryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ExecuteScheduledQuery)

responseListScheduledQueries :: ListScheduledQueriesResponse -> TestTree
responseListScheduledQueries =
  res
    "ListScheduledQueriesResponse"
    "fixture/ListScheduledQueriesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListScheduledQueries)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responsePrepareQuery :: PrepareQueryResponse -> TestTree
responsePrepareQuery =
  res
    "PrepareQueryResponse"
    "fixture/PrepareQueryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PrepareQuery)

responseQuery :: QueryResponse -> TestTree
responseQuery =
  res
    "QueryResponse"
    "fixture/QueryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy Query)

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

responseUpdateScheduledQuery :: UpdateScheduledQueryResponse -> TestTree
responseUpdateScheduledQuery =
  res
    "UpdateScheduledQueryResponse"
    "fixture/UpdateScheduledQueryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateScheduledQuery)
