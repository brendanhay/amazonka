{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.DAX
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.AWS.Gen.DAX where

import Data.Proxy
import Network.AWS.DAX
import Test.AWS.DAX.Internal
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
--         [ requestDescribeParameters $
--             newDescribeParameters
--
--         , requestDescribeClusters $
--             newDescribeClusters
--
--         , requestDescribeDefaultParameters $
--             newDescribeDefaultParameters
--
--         , requestCreateCluster $
--             newCreateCluster
--
--         , requestDecreaseReplicationFactor $
--             newDecreaseReplicationFactor
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestTagResource $
--             newTagResource
--
--         , requestDescribeParameterGroups $
--             newDescribeParameterGroups
--
--         , requestIncreaseReplicationFactor $
--             newIncreaseReplicationFactor
--
--         , requestDescribeEvents $
--             newDescribeEvents
--
--         , requestUpdateParameterGroup $
--             newUpdateParameterGroup
--
--         , requestDeleteParameterGroup $
--             newDeleteParameterGroup
--
--         , requestRebootNode $
--             newRebootNode
--
--         , requestListTags $
--             newListTags
--
--         , requestUpdateCluster $
--             newUpdateCluster
--
--         , requestDeleteCluster $
--             newDeleteCluster
--
--         , requestCreateSubnetGroup $
--             newCreateSubnetGroup
--
--         , requestUpdateSubnetGroup $
--             newUpdateSubnetGroup
--
--         , requestDeleteSubnetGroup $
--             newDeleteSubnetGroup
--
--         , requestDescribeSubnetGroups $
--             newDescribeSubnetGroups
--
--         , requestCreateParameterGroup $
--             newCreateParameterGroup
--
--           ]

--     , testGroup "response"
--         [ responseDescribeParameters $
--             newDescribeParametersResponse
--
--         , responseDescribeClusters $
--             newDescribeClustersResponse
--
--         , responseDescribeDefaultParameters $
--             newDescribeDefaultParametersResponse
--
--         , responseCreateCluster $
--             newCreateClusterResponse
--
--         , responseDecreaseReplicationFactor $
--             newDecreaseReplicationFactorResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseDescribeParameterGroups $
--             newDescribeParameterGroupsResponse
--
--         , responseIncreaseReplicationFactor $
--             newIncreaseReplicationFactorResponse
--
--         , responseDescribeEvents $
--             newDescribeEventsResponse
--
--         , responseUpdateParameterGroup $
--             newUpdateParameterGroupResponse
--
--         , responseDeleteParameterGroup $
--             newDeleteParameterGroupResponse
--
--         , responseRebootNode $
--             newRebootNodeResponse
--
--         , responseListTags $
--             newListTagsResponse
--
--         , responseUpdateCluster $
--             newUpdateClusterResponse
--
--         , responseDeleteCluster $
--             newDeleteClusterResponse
--
--         , responseCreateSubnetGroup $
--             newCreateSubnetGroupResponse
--
--         , responseUpdateSubnetGroup $
--             newUpdateSubnetGroupResponse
--
--         , responseDeleteSubnetGroup $
--             newDeleteSubnetGroupResponse
--
--         , responseDescribeSubnetGroups $
--             newDescribeSubnetGroupsResponse
--
--         , responseCreateParameterGroup $
--             newCreateParameterGroupResponse
--
--           ]
--     ]

-- Requests

requestDescribeParameters :: DescribeParameters -> TestTree
requestDescribeParameters =
  req
    "DescribeParameters"
    "fixture/DescribeParameters.yaml"

requestDescribeClusters :: DescribeClusters -> TestTree
requestDescribeClusters =
  req
    "DescribeClusters"
    "fixture/DescribeClusters.yaml"

requestDescribeDefaultParameters :: DescribeDefaultParameters -> TestTree
requestDescribeDefaultParameters =
  req
    "DescribeDefaultParameters"
    "fixture/DescribeDefaultParameters.yaml"

requestCreateCluster :: CreateCluster -> TestTree
requestCreateCluster =
  req
    "CreateCluster"
    "fixture/CreateCluster.yaml"

requestDecreaseReplicationFactor :: DecreaseReplicationFactor -> TestTree
requestDecreaseReplicationFactor =
  req
    "DecreaseReplicationFactor"
    "fixture/DecreaseReplicationFactor.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestDescribeParameterGroups :: DescribeParameterGroups -> TestTree
requestDescribeParameterGroups =
  req
    "DescribeParameterGroups"
    "fixture/DescribeParameterGroups.yaml"

requestIncreaseReplicationFactor :: IncreaseReplicationFactor -> TestTree
requestIncreaseReplicationFactor =
  req
    "IncreaseReplicationFactor"
    "fixture/IncreaseReplicationFactor.yaml"

requestDescribeEvents :: DescribeEvents -> TestTree
requestDescribeEvents =
  req
    "DescribeEvents"
    "fixture/DescribeEvents.yaml"

requestUpdateParameterGroup :: UpdateParameterGroup -> TestTree
requestUpdateParameterGroup =
  req
    "UpdateParameterGroup"
    "fixture/UpdateParameterGroup.yaml"

requestDeleteParameterGroup :: DeleteParameterGroup -> TestTree
requestDeleteParameterGroup =
  req
    "DeleteParameterGroup"
    "fixture/DeleteParameterGroup.yaml"

requestRebootNode :: RebootNode -> TestTree
requestRebootNode =
  req
    "RebootNode"
    "fixture/RebootNode.yaml"

requestListTags :: ListTags -> TestTree
requestListTags =
  req
    "ListTags"
    "fixture/ListTags.yaml"

requestUpdateCluster :: UpdateCluster -> TestTree
requestUpdateCluster =
  req
    "UpdateCluster"
    "fixture/UpdateCluster.yaml"

requestDeleteCluster :: DeleteCluster -> TestTree
requestDeleteCluster =
  req
    "DeleteCluster"
    "fixture/DeleteCluster.yaml"

requestCreateSubnetGroup :: CreateSubnetGroup -> TestTree
requestCreateSubnetGroup =
  req
    "CreateSubnetGroup"
    "fixture/CreateSubnetGroup.yaml"

requestUpdateSubnetGroup :: UpdateSubnetGroup -> TestTree
requestUpdateSubnetGroup =
  req
    "UpdateSubnetGroup"
    "fixture/UpdateSubnetGroup.yaml"

requestDeleteSubnetGroup :: DeleteSubnetGroup -> TestTree
requestDeleteSubnetGroup =
  req
    "DeleteSubnetGroup"
    "fixture/DeleteSubnetGroup.yaml"

requestDescribeSubnetGroups :: DescribeSubnetGroups -> TestTree
requestDescribeSubnetGroups =
  req
    "DescribeSubnetGroups"
    "fixture/DescribeSubnetGroups.yaml"

requestCreateParameterGroup :: CreateParameterGroup -> TestTree
requestCreateParameterGroup =
  req
    "CreateParameterGroup"
    "fixture/CreateParameterGroup.yaml"

-- Responses

responseDescribeParameters :: DescribeParametersResponse -> TestTree
responseDescribeParameters =
  res
    "DescribeParametersResponse"
    "fixture/DescribeParametersResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeParameters)

responseDescribeClusters :: DescribeClustersResponse -> TestTree
responseDescribeClusters =
  res
    "DescribeClustersResponse"
    "fixture/DescribeClustersResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeClusters)

responseDescribeDefaultParameters :: DescribeDefaultParametersResponse -> TestTree
responseDescribeDefaultParameters =
  res
    "DescribeDefaultParametersResponse"
    "fixture/DescribeDefaultParametersResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeDefaultParameters)

responseCreateCluster :: CreateClusterResponse -> TestTree
responseCreateCluster =
  res
    "CreateClusterResponse"
    "fixture/CreateClusterResponse.proto"
    defaultService
    (Proxy :: Proxy CreateCluster)

responseDecreaseReplicationFactor :: DecreaseReplicationFactorResponse -> TestTree
responseDecreaseReplicationFactor =
  res
    "DecreaseReplicationFactorResponse"
    "fixture/DecreaseReplicationFactorResponse.proto"
    defaultService
    (Proxy :: Proxy DecreaseReplicationFactor)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy :: Proxy UntagResource)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy :: Proxy TagResource)

responseDescribeParameterGroups :: DescribeParameterGroupsResponse -> TestTree
responseDescribeParameterGroups =
  res
    "DescribeParameterGroupsResponse"
    "fixture/DescribeParameterGroupsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeParameterGroups)

responseIncreaseReplicationFactor :: IncreaseReplicationFactorResponse -> TestTree
responseIncreaseReplicationFactor =
  res
    "IncreaseReplicationFactorResponse"
    "fixture/IncreaseReplicationFactorResponse.proto"
    defaultService
    (Proxy :: Proxy IncreaseReplicationFactor)

responseDescribeEvents :: DescribeEventsResponse -> TestTree
responseDescribeEvents =
  res
    "DescribeEventsResponse"
    "fixture/DescribeEventsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeEvents)

responseUpdateParameterGroup :: UpdateParameterGroupResponse -> TestTree
responseUpdateParameterGroup =
  res
    "UpdateParameterGroupResponse"
    "fixture/UpdateParameterGroupResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateParameterGroup)

responseDeleteParameterGroup :: DeleteParameterGroupResponse -> TestTree
responseDeleteParameterGroup =
  res
    "DeleteParameterGroupResponse"
    "fixture/DeleteParameterGroupResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteParameterGroup)

responseRebootNode :: RebootNodeResponse -> TestTree
responseRebootNode =
  res
    "RebootNodeResponse"
    "fixture/RebootNodeResponse.proto"
    defaultService
    (Proxy :: Proxy RebootNode)

responseListTags :: ListTagsResponse -> TestTree
responseListTags =
  res
    "ListTagsResponse"
    "fixture/ListTagsResponse.proto"
    defaultService
    (Proxy :: Proxy ListTags)

responseUpdateCluster :: UpdateClusterResponse -> TestTree
responseUpdateCluster =
  res
    "UpdateClusterResponse"
    "fixture/UpdateClusterResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateCluster)

responseDeleteCluster :: DeleteClusterResponse -> TestTree
responseDeleteCluster =
  res
    "DeleteClusterResponse"
    "fixture/DeleteClusterResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteCluster)

responseCreateSubnetGroup :: CreateSubnetGroupResponse -> TestTree
responseCreateSubnetGroup =
  res
    "CreateSubnetGroupResponse"
    "fixture/CreateSubnetGroupResponse.proto"
    defaultService
    (Proxy :: Proxy CreateSubnetGroup)

responseUpdateSubnetGroup :: UpdateSubnetGroupResponse -> TestTree
responseUpdateSubnetGroup =
  res
    "UpdateSubnetGroupResponse"
    "fixture/UpdateSubnetGroupResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateSubnetGroup)

responseDeleteSubnetGroup :: DeleteSubnetGroupResponse -> TestTree
responseDeleteSubnetGroup =
  res
    "DeleteSubnetGroupResponse"
    "fixture/DeleteSubnetGroupResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteSubnetGroup)

responseDescribeSubnetGroups :: DescribeSubnetGroupsResponse -> TestTree
responseDescribeSubnetGroups =
  res
    "DescribeSubnetGroupsResponse"
    "fixture/DescribeSubnetGroupsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeSubnetGroups)

responseCreateParameterGroup :: CreateParameterGroupResponse -> TestTree
responseCreateParameterGroup =
  res
    "CreateParameterGroupResponse"
    "fixture/CreateParameterGroupResponse.proto"
    defaultService
    (Proxy :: Proxy CreateParameterGroup)
