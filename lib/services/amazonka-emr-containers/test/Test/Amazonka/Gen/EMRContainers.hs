{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.EMRContainers
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.EMRContainers where

import Amazonka.EMRContainers
import qualified Data.Proxy as Proxy
import Test.Amazonka.EMRContainers.Internal
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
--         [ requestCancelJobRun $
--             newCancelJobRun
--
--         , requestCreateManagedEndpoint $
--             newCreateManagedEndpoint
--
--         , requestCreateVirtualCluster $
--             newCreateVirtualCluster
--
--         , requestDeleteManagedEndpoint $
--             newDeleteManagedEndpoint
--
--         , requestDeleteVirtualCluster $
--             newDeleteVirtualCluster
--
--         , requestDescribeJobRun $
--             newDescribeJobRun
--
--         , requestDescribeManagedEndpoint $
--             newDescribeManagedEndpoint
--
--         , requestDescribeVirtualCluster $
--             newDescribeVirtualCluster
--
--         , requestListJobRuns $
--             newListJobRuns
--
--         , requestListManagedEndpoints $
--             newListManagedEndpoints
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestListVirtualClusters $
--             newListVirtualClusters
--
--         , requestStartJobRun $
--             newStartJobRun
--
--         , requestTagResource $
--             newTagResource
--
--         , requestUntagResource $
--             newUntagResource
--
--           ]

--     , testGroup "response"
--         [ responseCancelJobRun $
--             newCancelJobRunResponse
--
--         , responseCreateManagedEndpoint $
--             newCreateManagedEndpointResponse
--
--         , responseCreateVirtualCluster $
--             newCreateVirtualClusterResponse
--
--         , responseDeleteManagedEndpoint $
--             newDeleteManagedEndpointResponse
--
--         , responseDeleteVirtualCluster $
--             newDeleteVirtualClusterResponse
--
--         , responseDescribeJobRun $
--             newDescribeJobRunResponse
--
--         , responseDescribeManagedEndpoint $
--             newDescribeManagedEndpointResponse
--
--         , responseDescribeVirtualCluster $
--             newDescribeVirtualClusterResponse
--
--         , responseListJobRuns $
--             newListJobRunsResponse
--
--         , responseListManagedEndpoints $
--             newListManagedEndpointsResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseListVirtualClusters $
--             newListVirtualClustersResponse
--
--         , responseStartJobRun $
--             newStartJobRunResponse
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

requestCancelJobRun :: CancelJobRun -> TestTree
requestCancelJobRun =
  req
    "CancelJobRun"
    "fixture/CancelJobRun.yaml"

requestCreateManagedEndpoint :: CreateManagedEndpoint -> TestTree
requestCreateManagedEndpoint =
  req
    "CreateManagedEndpoint"
    "fixture/CreateManagedEndpoint.yaml"

requestCreateVirtualCluster :: CreateVirtualCluster -> TestTree
requestCreateVirtualCluster =
  req
    "CreateVirtualCluster"
    "fixture/CreateVirtualCluster.yaml"

requestDeleteManagedEndpoint :: DeleteManagedEndpoint -> TestTree
requestDeleteManagedEndpoint =
  req
    "DeleteManagedEndpoint"
    "fixture/DeleteManagedEndpoint.yaml"

requestDeleteVirtualCluster :: DeleteVirtualCluster -> TestTree
requestDeleteVirtualCluster =
  req
    "DeleteVirtualCluster"
    "fixture/DeleteVirtualCluster.yaml"

requestDescribeJobRun :: DescribeJobRun -> TestTree
requestDescribeJobRun =
  req
    "DescribeJobRun"
    "fixture/DescribeJobRun.yaml"

requestDescribeManagedEndpoint :: DescribeManagedEndpoint -> TestTree
requestDescribeManagedEndpoint =
  req
    "DescribeManagedEndpoint"
    "fixture/DescribeManagedEndpoint.yaml"

requestDescribeVirtualCluster :: DescribeVirtualCluster -> TestTree
requestDescribeVirtualCluster =
  req
    "DescribeVirtualCluster"
    "fixture/DescribeVirtualCluster.yaml"

requestListJobRuns :: ListJobRuns -> TestTree
requestListJobRuns =
  req
    "ListJobRuns"
    "fixture/ListJobRuns.yaml"

requestListManagedEndpoints :: ListManagedEndpoints -> TestTree
requestListManagedEndpoints =
  req
    "ListManagedEndpoints"
    "fixture/ListManagedEndpoints.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestListVirtualClusters :: ListVirtualClusters -> TestTree
requestListVirtualClusters =
  req
    "ListVirtualClusters"
    "fixture/ListVirtualClusters.yaml"

requestStartJobRun :: StartJobRun -> TestTree
requestStartJobRun =
  req
    "StartJobRun"
    "fixture/StartJobRun.yaml"

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

responseCancelJobRun :: CancelJobRunResponse -> TestTree
responseCancelJobRun =
  res
    "CancelJobRunResponse"
    "fixture/CancelJobRunResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CancelJobRun)

responseCreateManagedEndpoint :: CreateManagedEndpointResponse -> TestTree
responseCreateManagedEndpoint =
  res
    "CreateManagedEndpointResponse"
    "fixture/CreateManagedEndpointResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateManagedEndpoint)

responseCreateVirtualCluster :: CreateVirtualClusterResponse -> TestTree
responseCreateVirtualCluster =
  res
    "CreateVirtualClusterResponse"
    "fixture/CreateVirtualClusterResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateVirtualCluster)

responseDeleteManagedEndpoint :: DeleteManagedEndpointResponse -> TestTree
responseDeleteManagedEndpoint =
  res
    "DeleteManagedEndpointResponse"
    "fixture/DeleteManagedEndpointResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteManagedEndpoint)

responseDeleteVirtualCluster :: DeleteVirtualClusterResponse -> TestTree
responseDeleteVirtualCluster =
  res
    "DeleteVirtualClusterResponse"
    "fixture/DeleteVirtualClusterResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteVirtualCluster)

responseDescribeJobRun :: DescribeJobRunResponse -> TestTree
responseDescribeJobRun =
  res
    "DescribeJobRunResponse"
    "fixture/DescribeJobRunResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeJobRun)

responseDescribeManagedEndpoint :: DescribeManagedEndpointResponse -> TestTree
responseDescribeManagedEndpoint =
  res
    "DescribeManagedEndpointResponse"
    "fixture/DescribeManagedEndpointResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeManagedEndpoint)

responseDescribeVirtualCluster :: DescribeVirtualClusterResponse -> TestTree
responseDescribeVirtualCluster =
  res
    "DescribeVirtualClusterResponse"
    "fixture/DescribeVirtualClusterResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeVirtualCluster)

responseListJobRuns :: ListJobRunsResponse -> TestTree
responseListJobRuns =
  res
    "ListJobRunsResponse"
    "fixture/ListJobRunsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListJobRuns)

responseListManagedEndpoints :: ListManagedEndpointsResponse -> TestTree
responseListManagedEndpoints =
  res
    "ListManagedEndpointsResponse"
    "fixture/ListManagedEndpointsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListManagedEndpoints)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseListVirtualClusters :: ListVirtualClustersResponse -> TestTree
responseListVirtualClusters =
  res
    "ListVirtualClustersResponse"
    "fixture/ListVirtualClustersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListVirtualClusters)

responseStartJobRun :: StartJobRunResponse -> TestTree
responseStartJobRun =
  res
    "StartJobRunResponse"
    "fixture/StartJobRunResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartJobRun)

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
