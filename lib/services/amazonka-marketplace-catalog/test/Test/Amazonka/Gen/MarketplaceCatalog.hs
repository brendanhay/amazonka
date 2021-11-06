{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.MarketplaceCatalog
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
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
--         [ requestListEntities $
--             newListEntities
--
--         , requestListChangeSets $
--             newListChangeSets
--
--         , requestStartChangeSet $
--             newStartChangeSet
--
--         , requestCancelChangeSet $
--             newCancelChangeSet
--
--         , requestDescribeEntity $
--             newDescribeEntity
--
--         , requestDescribeChangeSet $
--             newDescribeChangeSet
--
--           ]

--     , testGroup "response"
--         [ responseListEntities $
--             newListEntitiesResponse
--
--         , responseListChangeSets $
--             newListChangeSetsResponse
--
--         , responseStartChangeSet $
--             newStartChangeSetResponse
--
--         , responseCancelChangeSet $
--             newCancelChangeSetResponse
--
--         , responseDescribeEntity $
--             newDescribeEntityResponse
--
--         , responseDescribeChangeSet $
--             newDescribeChangeSetResponse
--
--           ]
--     ]

-- Requests

requestListEntities :: ListEntities -> TestTree
requestListEntities =
  req
    "ListEntities"
    "fixture/ListEntities.yaml"

requestListChangeSets :: ListChangeSets -> TestTree
requestListChangeSets =
  req
    "ListChangeSets"
    "fixture/ListChangeSets.yaml"

requestStartChangeSet :: StartChangeSet -> TestTree
requestStartChangeSet =
  req
    "StartChangeSet"
    "fixture/StartChangeSet.yaml"

requestCancelChangeSet :: CancelChangeSet -> TestTree
requestCancelChangeSet =
  req
    "CancelChangeSet"
    "fixture/CancelChangeSet.yaml"

requestDescribeEntity :: DescribeEntity -> TestTree
requestDescribeEntity =
  req
    "DescribeEntity"
    "fixture/DescribeEntity.yaml"

requestDescribeChangeSet :: DescribeChangeSet -> TestTree
requestDescribeChangeSet =
  req
    "DescribeChangeSet"
    "fixture/DescribeChangeSet.yaml"

-- Responses

responseListEntities :: ListEntitiesResponse -> TestTree
responseListEntities =
  res
    "ListEntitiesResponse"
    "fixture/ListEntitiesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListEntities)

responseListChangeSets :: ListChangeSetsResponse -> TestTree
responseListChangeSets =
  res
    "ListChangeSetsResponse"
    "fixture/ListChangeSetsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListChangeSets)

responseStartChangeSet :: StartChangeSetResponse -> TestTree
responseStartChangeSet =
  res
    "StartChangeSetResponse"
    "fixture/StartChangeSetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartChangeSet)

responseCancelChangeSet :: CancelChangeSetResponse -> TestTree
responseCancelChangeSet =
  res
    "CancelChangeSetResponse"
    "fixture/CancelChangeSetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CancelChangeSet)

responseDescribeEntity :: DescribeEntityResponse -> TestTree
responseDescribeEntity =
  res
    "DescribeEntityResponse"
    "fixture/DescribeEntityResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeEntity)

responseDescribeChangeSet :: DescribeChangeSetResponse -> TestTree
responseDescribeChangeSet =
  res
    "DescribeChangeSetResponse"
    "fixture/DescribeChangeSetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeChangeSet)
