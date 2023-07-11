{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.MarketplaceCatalog
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.MarketplaceCatalog where

import Amazonka.MarketplaceCatalog
import qualified Data.Proxy as Proxy
import Test.Amazonka.Fixture
import Test.Amazonka.MarketplaceCatalog.Internal
import Test.Amazonka.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestCancelChangeSet $
--             newCancelChangeSet
--
--         , requestDescribeChangeSet $
--             newDescribeChangeSet
--
--         , requestDescribeEntity $
--             newDescribeEntity
--
--         , requestListChangeSets $
--             newListChangeSets
--
--         , requestListEntities $
--             newListEntities
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestStartChangeSet $
--             newStartChangeSet
--
--         , requestTagResource $
--             newTagResource
--
--         , requestUntagResource $
--             newUntagResource
--
--           ]

--     , testGroup "response"
--         [ responseCancelChangeSet $
--             newCancelChangeSetResponse
--
--         , responseDescribeChangeSet $
--             newDescribeChangeSetResponse
--
--         , responseDescribeEntity $
--             newDescribeEntityResponse
--
--         , responseListChangeSets $
--             newListChangeSetsResponse
--
--         , responseListEntities $
--             newListEntitiesResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseStartChangeSet $
--             newStartChangeSetResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--           ]
--     ]

-- Requests

requestCancelChangeSet :: CancelChangeSet -> TestTree
requestCancelChangeSet =
  req
    "CancelChangeSet"
    "fixture/CancelChangeSet.yaml"

requestDescribeChangeSet :: DescribeChangeSet -> TestTree
requestDescribeChangeSet =
  req
    "DescribeChangeSet"
    "fixture/DescribeChangeSet.yaml"

requestDescribeEntity :: DescribeEntity -> TestTree
requestDescribeEntity =
  req
    "DescribeEntity"
    "fixture/DescribeEntity.yaml"

requestListChangeSets :: ListChangeSets -> TestTree
requestListChangeSets =
  req
    "ListChangeSets"
    "fixture/ListChangeSets.yaml"

requestListEntities :: ListEntities -> TestTree
requestListEntities =
  req
    "ListEntities"
    "fixture/ListEntities.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestStartChangeSet :: StartChangeSet -> TestTree
requestStartChangeSet =
  req
    "StartChangeSet"
    "fixture/StartChangeSet.yaml"

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

-- Responses

responseCancelChangeSet :: CancelChangeSetResponse -> TestTree
responseCancelChangeSet =
  res
    "CancelChangeSetResponse"
    "fixture/CancelChangeSetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CancelChangeSet)

responseDescribeChangeSet :: DescribeChangeSetResponse -> TestTree
responseDescribeChangeSet =
  res
    "DescribeChangeSetResponse"
    "fixture/DescribeChangeSetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeChangeSet)

responseDescribeEntity :: DescribeEntityResponse -> TestTree
responseDescribeEntity =
  res
    "DescribeEntityResponse"
    "fixture/DescribeEntityResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeEntity)

responseListChangeSets :: ListChangeSetsResponse -> TestTree
responseListChangeSets =
  res
    "ListChangeSetsResponse"
    "fixture/ListChangeSetsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListChangeSets)

responseListEntities :: ListEntitiesResponse -> TestTree
responseListEntities =
  res
    "ListEntitiesResponse"
    "fixture/ListEntitiesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListEntities)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseStartChangeSet :: StartChangeSetResponse -> TestTree
responseStartChangeSet =
  res
    "StartChangeSetResponse"
    "fixture/StartChangeSetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartChangeSet)

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
