{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.MarketplaceCatalog
-- Copyright   : (c) 2013-2023 Brendan Hay
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
--         [ requestCancelChangeSet $
--             newCancelChangeSet
--
--         , requestDeleteResourcePolicy $
--             newDeleteResourcePolicy
--
--         , requestDescribeChangeSet $
--             newDescribeChangeSet
--
--         , requestDescribeEntity $
--             newDescribeEntity
--
--         , requestGetResourcePolicy $
--             newGetResourcePolicy
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
--         , requestPutResourcePolicy $
--             newPutResourcePolicy
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
--         , responseDeleteResourcePolicy $
--             newDeleteResourcePolicyResponse
--
--         , responseDescribeChangeSet $
--             newDescribeChangeSetResponse
--
--         , responseDescribeEntity $
--             newDescribeEntityResponse
--
--         , responseGetResourcePolicy $
--             newGetResourcePolicyResponse
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
--         , responsePutResourcePolicy $
--             newPutResourcePolicyResponse
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

requestDeleteResourcePolicy :: DeleteResourcePolicy -> TestTree
requestDeleteResourcePolicy =
  req
    "DeleteResourcePolicy"
    "fixture/DeleteResourcePolicy.yaml"

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

requestGetResourcePolicy :: GetResourcePolicy -> TestTree
requestGetResourcePolicy =
  req
    "GetResourcePolicy"
    "fixture/GetResourcePolicy.yaml"

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

requestPutResourcePolicy :: PutResourcePolicy -> TestTree
requestPutResourcePolicy =
  req
    "PutResourcePolicy"
    "fixture/PutResourcePolicy.yaml"

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

responseDeleteResourcePolicy :: DeleteResourcePolicyResponse -> TestTree
responseDeleteResourcePolicy =
  res
    "DeleteResourcePolicyResponse"
    "fixture/DeleteResourcePolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteResourcePolicy)

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

responseGetResourcePolicy :: GetResourcePolicyResponse -> TestTree
responseGetResourcePolicy =
  res
    "GetResourcePolicyResponse"
    "fixture/GetResourcePolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetResourcePolicy)

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

responsePutResourcePolicy :: PutResourcePolicyResponse -> TestTree
responsePutResourcePolicy =
  res
    "PutResourcePolicyResponse"
    "fixture/PutResourcePolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutResourcePolicy)

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
