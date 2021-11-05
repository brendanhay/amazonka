{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.EMRContainers
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.AWS.Gen.EMRContainers where

import Amazonka.EMRContainers
import qualified Data.Proxy as Proxy
import Test.AWS.EMRContainers.Internal
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
--         [ requestListManagedEndpoints $
--             newListManagedEndpoints
--
--         , requestCreateVirtualCluster $
--             newCreateVirtualCluster
--
--         , requestDeleteVirtualCluster $
--             newDeleteVirtualCluster
--
--         , requestCreateManagedEndpoint $
--             newCreateManagedEndpoint
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestCancelJobRun $
--             newCancelJobRun
--
--         , requestDeleteManagedEndpoint $
--             newDeleteManagedEndpoint
--
--         , requestListJobRuns $
--             newListJobRuns
--
--         , requestListVirtualClusters $
--             newListVirtualClusters
--
--         , requestTagResource $
--             newTagResource
--
--         , requestDescribeManagedEndpoint $
--             newDescribeManagedEndpoint
--
--         , requestDescribeJobRun $
--             newDescribeJobRun
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestDescribeVirtualCluster $
--             newDescribeVirtualCluster
--
--         , requestStartJobRun $
--             newStartJobRun
--
--           ]

--     , testGroup "response"
--         [ responseListManagedEndpoints $
--             newListManagedEndpointsResponse
--
--         , responseCreateVirtualCluster $
--             newCreateVirtualClusterResponse
--
--         , responseDeleteVirtualCluster $
--             newDeleteVirtualClusterResponse
--
--         , responseCreateManagedEndpoint $
--             newCreateManagedEndpointResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseCancelJobRun $
--             newCancelJobRunResponse
--
--         , responseDeleteManagedEndpoint $
--             newDeleteManagedEndpointResponse
--
--         , responseListJobRuns $
--             newListJobRunsResponse
--
--         , responseListVirtualClusters $
--             newListVirtualClustersResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseDescribeManagedEndpoint $
--             newDescribeManagedEndpointResponse
--
--         , responseDescribeJobRun $
--             newDescribeJobRunResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseDescribeVirtualCluster $
--             newDescribeVirtualClusterResponse
--
--         , responseStartJobRun $
--             newStartJobRunResponse
--
--           ]
--     ]

-- Requests

requestListManagedEndpoints :: ListManagedEndpoints -> TestTree
requestListManagedEndpoints =
  req
    "ListManagedEndpoints"
    "fixture/ListManagedEndpoints.yaml"

requestCreateVirtualCluster :: CreateVirtualCluster -> TestTree
requestCreateVirtualCluster =
  req
    "CreateVirtualCluster"
    "fixture/CreateVirtualCluster.yaml"

requestDeleteVirtualCluster :: DeleteVirtualCluster -> TestTree
requestDeleteVirtualCluster =
  req
    "DeleteVirtualCluster"
    "fixture/DeleteVirtualCluster.yaml"

requestCreateManagedEndpoint :: CreateManagedEndpoint -> TestTree
requestCreateManagedEndpoint =
  req
    "CreateManagedEndpoint"
    "fixture/CreateManagedEndpoint.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestCancelJobRun :: CancelJobRun -> TestTree
requestCancelJobRun =
  req
    "CancelJobRun"
    "fixture/CancelJobRun.yaml"

requestDeleteManagedEndpoint :: DeleteManagedEndpoint -> TestTree
requestDeleteManagedEndpoint =
  req
    "DeleteManagedEndpoint"
    "fixture/DeleteManagedEndpoint.yaml"

requestListJobRuns :: ListJobRuns -> TestTree
requestListJobRuns =
  req
    "ListJobRuns"
    "fixture/ListJobRuns.yaml"

requestListVirtualClusters :: ListVirtualClusters -> TestTree
requestListVirtualClusters =
  req
    "ListVirtualClusters"
    "fixture/ListVirtualClusters.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestDescribeManagedEndpoint :: DescribeManagedEndpoint -> TestTree
requestDescribeManagedEndpoint =
  req
    "DescribeManagedEndpoint"
    "fixture/DescribeManagedEndpoint.yaml"

requestDescribeJobRun :: DescribeJobRun -> TestTree
requestDescribeJobRun =
  req
    "DescribeJobRun"
    "fixture/DescribeJobRun.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestDescribeVirtualCluster :: DescribeVirtualCluster -> TestTree
requestDescribeVirtualCluster =
  req
    "DescribeVirtualCluster"
    "fixture/DescribeVirtualCluster.yaml"

requestStartJobRun :: StartJobRun -> TestTree
requestStartJobRun =
  req
    "StartJobRun"
    "fixture/StartJobRun.yaml"

-- Responses

responseListManagedEndpoints :: ListManagedEndpointsResponse -> TestTree
responseListManagedEndpoints =
  res
    "ListManagedEndpointsResponse"
    "fixture/ListManagedEndpointsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListManagedEndpoints)

responseCreateVirtualCluster :: CreateVirtualClusterResponse -> TestTree
responseCreateVirtualCluster =
  res
    "CreateVirtualClusterResponse"
    "fixture/CreateVirtualClusterResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateVirtualCluster)

responseDeleteVirtualCluster :: DeleteVirtualClusterResponse -> TestTree
responseDeleteVirtualCluster =
  res
    "DeleteVirtualClusterResponse"
    "fixture/DeleteVirtualClusterResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteVirtualCluster)

responseCreateManagedEndpoint :: CreateManagedEndpointResponse -> TestTree
responseCreateManagedEndpoint =
  res
    "CreateManagedEndpointResponse"
    "fixture/CreateManagedEndpointResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateManagedEndpoint)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseCancelJobRun :: CancelJobRunResponse -> TestTree
responseCancelJobRun =
  res
    "CancelJobRunResponse"
    "fixture/CancelJobRunResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CancelJobRun)

responseDeleteManagedEndpoint :: DeleteManagedEndpointResponse -> TestTree
responseDeleteManagedEndpoint =
  res
    "DeleteManagedEndpointResponse"
    "fixture/DeleteManagedEndpointResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteManagedEndpoint)

responseListJobRuns :: ListJobRunsResponse -> TestTree
responseListJobRuns =
  res
    "ListJobRunsResponse"
    "fixture/ListJobRunsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListJobRuns)

responseListVirtualClusters :: ListVirtualClustersResponse -> TestTree
responseListVirtualClusters =
  res
    "ListVirtualClustersResponse"
    "fixture/ListVirtualClustersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListVirtualClusters)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TagResource)

responseDescribeManagedEndpoint :: DescribeManagedEndpointResponse -> TestTree
responseDescribeManagedEndpoint =
  res
    "DescribeManagedEndpointResponse"
    "fixture/DescribeManagedEndpointResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeManagedEndpoint)

responseDescribeJobRun :: DescribeJobRunResponse -> TestTree
responseDescribeJobRun =
  res
    "DescribeJobRunResponse"
    "fixture/DescribeJobRunResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeJobRun)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UntagResource)

responseDescribeVirtualCluster :: DescribeVirtualClusterResponse -> TestTree
responseDescribeVirtualCluster =
  res
    "DescribeVirtualClusterResponse"
    "fixture/DescribeVirtualClusterResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeVirtualCluster)

responseStartJobRun :: StartJobRunResponse -> TestTree
responseStartJobRun =
  res
    "StartJobRunResponse"
    "fixture/StartJobRunResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartJobRun)
