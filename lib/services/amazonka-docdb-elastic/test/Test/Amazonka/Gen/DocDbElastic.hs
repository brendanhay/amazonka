{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.DocDbElastic
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.DocDbElastic where

import Amazonka.DocDbElastic
import qualified Data.Proxy as Proxy
import Test.Amazonka.DocDbElastic.Internal
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
--         [ requestCreateCluster $
--             newCreateCluster
--
--         , requestCreateClusterSnapshot $
--             newCreateClusterSnapshot
--
--         , requestDeleteCluster $
--             newDeleteCluster
--
--         , requestDeleteClusterSnapshot $
--             newDeleteClusterSnapshot
--
--         , requestGetCluster $
--             newGetCluster
--
--         , requestGetClusterSnapshot $
--             newGetClusterSnapshot
--
--         , requestListClusterSnapshots $
--             newListClusterSnapshots
--
--         , requestListClusters $
--             newListClusters
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestRestoreClusterFromSnapshot $
--             newRestoreClusterFromSnapshot
--
--         , requestTagResource $
--             newTagResource
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestUpdateCluster $
--             newUpdateCluster
--
--           ]

--     , testGroup "response"
--         [ responseCreateCluster $
--             newCreateClusterResponse
--
--         , responseCreateClusterSnapshot $
--             newCreateClusterSnapshotResponse
--
--         , responseDeleteCluster $
--             newDeleteClusterResponse
--
--         , responseDeleteClusterSnapshot $
--             newDeleteClusterSnapshotResponse
--
--         , responseGetCluster $
--             newGetClusterResponse
--
--         , responseGetClusterSnapshot $
--             newGetClusterSnapshotResponse
--
--         , responseListClusterSnapshots $
--             newListClusterSnapshotsResponse
--
--         , responseListClusters $
--             newListClustersResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseRestoreClusterFromSnapshot $
--             newRestoreClusterFromSnapshotResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseUpdateCluster $
--             newUpdateClusterResponse
--
--           ]
--     ]

-- Requests

requestCreateCluster :: CreateCluster -> TestTree
requestCreateCluster =
  req
    "CreateCluster"
    "fixture/CreateCluster.yaml"

requestCreateClusterSnapshot :: CreateClusterSnapshot -> TestTree
requestCreateClusterSnapshot =
  req
    "CreateClusterSnapshot"
    "fixture/CreateClusterSnapshot.yaml"

requestDeleteCluster :: DeleteCluster -> TestTree
requestDeleteCluster =
  req
    "DeleteCluster"
    "fixture/DeleteCluster.yaml"

requestDeleteClusterSnapshot :: DeleteClusterSnapshot -> TestTree
requestDeleteClusterSnapshot =
  req
    "DeleteClusterSnapshot"
    "fixture/DeleteClusterSnapshot.yaml"

requestGetCluster :: GetCluster -> TestTree
requestGetCluster =
  req
    "GetCluster"
    "fixture/GetCluster.yaml"

requestGetClusterSnapshot :: GetClusterSnapshot -> TestTree
requestGetClusterSnapshot =
  req
    "GetClusterSnapshot"
    "fixture/GetClusterSnapshot.yaml"

requestListClusterSnapshots :: ListClusterSnapshots -> TestTree
requestListClusterSnapshots =
  req
    "ListClusterSnapshots"
    "fixture/ListClusterSnapshots.yaml"

requestListClusters :: ListClusters -> TestTree
requestListClusters =
  req
    "ListClusters"
    "fixture/ListClusters.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestRestoreClusterFromSnapshot :: RestoreClusterFromSnapshot -> TestTree
requestRestoreClusterFromSnapshot =
  req
    "RestoreClusterFromSnapshot"
    "fixture/RestoreClusterFromSnapshot.yaml"

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

requestUpdateCluster :: UpdateCluster -> TestTree
requestUpdateCluster =
  req
    "UpdateCluster"
    "fixture/UpdateCluster.yaml"

-- Responses

responseCreateCluster :: CreateClusterResponse -> TestTree
responseCreateCluster =
  res
    "CreateClusterResponse"
    "fixture/CreateClusterResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateCluster)

responseCreateClusterSnapshot :: CreateClusterSnapshotResponse -> TestTree
responseCreateClusterSnapshot =
  res
    "CreateClusterSnapshotResponse"
    "fixture/CreateClusterSnapshotResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateClusterSnapshot)

responseDeleteCluster :: DeleteClusterResponse -> TestTree
responseDeleteCluster =
  res
    "DeleteClusterResponse"
    "fixture/DeleteClusterResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteCluster)

responseDeleteClusterSnapshot :: DeleteClusterSnapshotResponse -> TestTree
responseDeleteClusterSnapshot =
  res
    "DeleteClusterSnapshotResponse"
    "fixture/DeleteClusterSnapshotResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteClusterSnapshot)

responseGetCluster :: GetClusterResponse -> TestTree
responseGetCluster =
  res
    "GetClusterResponse"
    "fixture/GetClusterResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetCluster)

responseGetClusterSnapshot :: GetClusterSnapshotResponse -> TestTree
responseGetClusterSnapshot =
  res
    "GetClusterSnapshotResponse"
    "fixture/GetClusterSnapshotResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetClusterSnapshot)

responseListClusterSnapshots :: ListClusterSnapshotsResponse -> TestTree
responseListClusterSnapshots =
  res
    "ListClusterSnapshotsResponse"
    "fixture/ListClusterSnapshotsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListClusterSnapshots)

responseListClusters :: ListClustersResponse -> TestTree
responseListClusters =
  res
    "ListClustersResponse"
    "fixture/ListClustersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListClusters)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseRestoreClusterFromSnapshot :: RestoreClusterFromSnapshotResponse -> TestTree
responseRestoreClusterFromSnapshot =
  res
    "RestoreClusterFromSnapshotResponse"
    "fixture/RestoreClusterFromSnapshotResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RestoreClusterFromSnapshot)

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

responseUpdateCluster :: UpdateClusterResponse -> TestTree
responseUpdateCluster =
  res
    "UpdateClusterResponse"
    "fixture/UpdateClusterResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateCluster)
