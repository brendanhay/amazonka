{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.DAX
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
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
--         [ requestDescribeClusters $
--             describeClusters
--
--         , requestDescribeParameters $
--             describeParameters
--
--         , requestDescribeEvents $
--             describeEvents
--
--         , requestIncreaseReplicationFactor $
--             increaseReplicationFactor
--
--         , requestCreateSubnetGroup $
--             createSubnetGroup
--
--         , requestDeleteCluster $
--             deleteCluster
--
--         , requestUpdateCluster $
--             updateCluster
--
--         , requestCreateCluster $
--             createCluster
--
--         , requestDescribeDefaultParameters $
--             describeDefaultParameters
--
--         , requestDeleteParameterGroup $
--             deleteParameterGroup
--
--         , requestUpdateParameterGroup $
--             updateParameterGroup
--
--         , requestDescribeSubnetGroups $
--             describeSubnetGroups
--
--         , requestCreateParameterGroup $
--             createParameterGroup
--
--         , requestUpdateSubnetGroup $
--             updateSubnetGroup
--
--         , requestDeleteSubnetGroup $
--             deleteSubnetGroup
--
--         , requestDescribeParameterGroups $
--             describeParameterGroups
--
--         , requestTagResource $
--             tagResource
--
--         , requestListTags $
--             listTags
--
--         , requestDecreaseReplicationFactor $
--             decreaseReplicationFactor
--
--         , requestUntagResource $
--             untagResource
--
--         , requestRebootNode $
--             rebootNode
--
--           ]

--     , testGroup "response"
--         [ responseDescribeClusters $
--             describeClustersResponse
--
--         , responseDescribeParameters $
--             describeParametersResponse
--
--         , responseDescribeEvents $
--             describeEventsResponse
--
--         , responseIncreaseReplicationFactor $
--             increaseReplicationFactorResponse
--
--         , responseCreateSubnetGroup $
--             createSubnetGroupResponse
--
--         , responseDeleteCluster $
--             deleteClusterResponse
--
--         , responseUpdateCluster $
--             updateClusterResponse
--
--         , responseCreateCluster $
--             createClusterResponse
--
--         , responseDescribeDefaultParameters $
--             describeDefaultParametersResponse
--
--         , responseDeleteParameterGroup $
--             deleteParameterGroupResponse
--
--         , responseUpdateParameterGroup $
--             updateParameterGroupResponse
--
--         , responseDescribeSubnetGroups $
--             describeSubnetGroupsResponse
--
--         , responseCreateParameterGroup $
--             createParameterGroupResponse
--
--         , responseUpdateSubnetGroup $
--             updateSubnetGroupResponse
--
--         , responseDeleteSubnetGroup $
--             deleteSubnetGroupResponse
--
--         , responseDescribeParameterGroups $
--             describeParameterGroupsResponse
--
--         , responseTagResource $
--             tagResourceResponse
--
--         , responseListTags $
--             listTagsResponse
--
--         , responseDecreaseReplicationFactor $
--             decreaseReplicationFactorResponse
--
--         , responseUntagResource $
--             untagResourceResponse
--
--         , responseRebootNode $
--             rebootNodeResponse
--
--           ]
--     ]

-- Requests

requestDescribeClusters :: DescribeClusters -> TestTree
requestDescribeClusters = req
    "DescribeClusters"
    "fixture/DescribeClusters.yaml"

requestDescribeParameters :: DescribeParameters -> TestTree
requestDescribeParameters = req
    "DescribeParameters"
    "fixture/DescribeParameters.yaml"

requestDescribeEvents :: DescribeEvents -> TestTree
requestDescribeEvents = req
    "DescribeEvents"
    "fixture/DescribeEvents.yaml"

requestIncreaseReplicationFactor :: IncreaseReplicationFactor -> TestTree
requestIncreaseReplicationFactor = req
    "IncreaseReplicationFactor"
    "fixture/IncreaseReplicationFactor.yaml"

requestCreateSubnetGroup :: CreateSubnetGroup -> TestTree
requestCreateSubnetGroup = req
    "CreateSubnetGroup"
    "fixture/CreateSubnetGroup.yaml"

requestDeleteCluster :: DeleteCluster -> TestTree
requestDeleteCluster = req
    "DeleteCluster"
    "fixture/DeleteCluster.yaml"

requestUpdateCluster :: UpdateCluster -> TestTree
requestUpdateCluster = req
    "UpdateCluster"
    "fixture/UpdateCluster.yaml"

requestCreateCluster :: CreateCluster -> TestTree
requestCreateCluster = req
    "CreateCluster"
    "fixture/CreateCluster.yaml"

requestDescribeDefaultParameters :: DescribeDefaultParameters -> TestTree
requestDescribeDefaultParameters = req
    "DescribeDefaultParameters"
    "fixture/DescribeDefaultParameters.yaml"

requestDeleteParameterGroup :: DeleteParameterGroup -> TestTree
requestDeleteParameterGroup = req
    "DeleteParameterGroup"
    "fixture/DeleteParameterGroup.yaml"

requestUpdateParameterGroup :: UpdateParameterGroup -> TestTree
requestUpdateParameterGroup = req
    "UpdateParameterGroup"
    "fixture/UpdateParameterGroup.yaml"

requestDescribeSubnetGroups :: DescribeSubnetGroups -> TestTree
requestDescribeSubnetGroups = req
    "DescribeSubnetGroups"
    "fixture/DescribeSubnetGroups.yaml"

requestCreateParameterGroup :: CreateParameterGroup -> TestTree
requestCreateParameterGroup = req
    "CreateParameterGroup"
    "fixture/CreateParameterGroup.yaml"

requestUpdateSubnetGroup :: UpdateSubnetGroup -> TestTree
requestUpdateSubnetGroup = req
    "UpdateSubnetGroup"
    "fixture/UpdateSubnetGroup.yaml"

requestDeleteSubnetGroup :: DeleteSubnetGroup -> TestTree
requestDeleteSubnetGroup = req
    "DeleteSubnetGroup"
    "fixture/DeleteSubnetGroup.yaml"

requestDescribeParameterGroups :: DescribeParameterGroups -> TestTree
requestDescribeParameterGroups = req
    "DescribeParameterGroups"
    "fixture/DescribeParameterGroups.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource = req
    "TagResource"
    "fixture/TagResource.yaml"

requestListTags :: ListTags -> TestTree
requestListTags = req
    "ListTags"
    "fixture/ListTags.yaml"

requestDecreaseReplicationFactor :: DecreaseReplicationFactor -> TestTree
requestDecreaseReplicationFactor = req
    "DecreaseReplicationFactor"
    "fixture/DecreaseReplicationFactor.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource = req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestRebootNode :: RebootNode -> TestTree
requestRebootNode = req
    "RebootNode"
    "fixture/RebootNode.yaml"

-- Responses

responseDescribeClusters :: DescribeClustersResponse -> TestTree
responseDescribeClusters = res
    "DescribeClustersResponse"
    "fixture/DescribeClustersResponse.proto"
    dax
    (Proxy :: Proxy DescribeClusters)

responseDescribeParameters :: DescribeParametersResponse -> TestTree
responseDescribeParameters = res
    "DescribeParametersResponse"
    "fixture/DescribeParametersResponse.proto"
    dax
    (Proxy :: Proxy DescribeParameters)

responseDescribeEvents :: DescribeEventsResponse -> TestTree
responseDescribeEvents = res
    "DescribeEventsResponse"
    "fixture/DescribeEventsResponse.proto"
    dax
    (Proxy :: Proxy DescribeEvents)

responseIncreaseReplicationFactor :: IncreaseReplicationFactorResponse -> TestTree
responseIncreaseReplicationFactor = res
    "IncreaseReplicationFactorResponse"
    "fixture/IncreaseReplicationFactorResponse.proto"
    dax
    (Proxy :: Proxy IncreaseReplicationFactor)

responseCreateSubnetGroup :: CreateSubnetGroupResponse -> TestTree
responseCreateSubnetGroup = res
    "CreateSubnetGroupResponse"
    "fixture/CreateSubnetGroupResponse.proto"
    dax
    (Proxy :: Proxy CreateSubnetGroup)

responseDeleteCluster :: DeleteClusterResponse -> TestTree
responseDeleteCluster = res
    "DeleteClusterResponse"
    "fixture/DeleteClusterResponse.proto"
    dax
    (Proxy :: Proxy DeleteCluster)

responseUpdateCluster :: UpdateClusterResponse -> TestTree
responseUpdateCluster = res
    "UpdateClusterResponse"
    "fixture/UpdateClusterResponse.proto"
    dax
    (Proxy :: Proxy UpdateCluster)

responseCreateCluster :: CreateClusterResponse -> TestTree
responseCreateCluster = res
    "CreateClusterResponse"
    "fixture/CreateClusterResponse.proto"
    dax
    (Proxy :: Proxy CreateCluster)

responseDescribeDefaultParameters :: DescribeDefaultParametersResponse -> TestTree
responseDescribeDefaultParameters = res
    "DescribeDefaultParametersResponse"
    "fixture/DescribeDefaultParametersResponse.proto"
    dax
    (Proxy :: Proxy DescribeDefaultParameters)

responseDeleteParameterGroup :: DeleteParameterGroupResponse -> TestTree
responseDeleteParameterGroup = res
    "DeleteParameterGroupResponse"
    "fixture/DeleteParameterGroupResponse.proto"
    dax
    (Proxy :: Proxy DeleteParameterGroup)

responseUpdateParameterGroup :: UpdateParameterGroupResponse -> TestTree
responseUpdateParameterGroup = res
    "UpdateParameterGroupResponse"
    "fixture/UpdateParameterGroupResponse.proto"
    dax
    (Proxy :: Proxy UpdateParameterGroup)

responseDescribeSubnetGroups :: DescribeSubnetGroupsResponse -> TestTree
responseDescribeSubnetGroups = res
    "DescribeSubnetGroupsResponse"
    "fixture/DescribeSubnetGroupsResponse.proto"
    dax
    (Proxy :: Proxy DescribeSubnetGroups)

responseCreateParameterGroup :: CreateParameterGroupResponse -> TestTree
responseCreateParameterGroup = res
    "CreateParameterGroupResponse"
    "fixture/CreateParameterGroupResponse.proto"
    dax
    (Proxy :: Proxy CreateParameterGroup)

responseUpdateSubnetGroup :: UpdateSubnetGroupResponse -> TestTree
responseUpdateSubnetGroup = res
    "UpdateSubnetGroupResponse"
    "fixture/UpdateSubnetGroupResponse.proto"
    dax
    (Proxy :: Proxy UpdateSubnetGroup)

responseDeleteSubnetGroup :: DeleteSubnetGroupResponse -> TestTree
responseDeleteSubnetGroup = res
    "DeleteSubnetGroupResponse"
    "fixture/DeleteSubnetGroupResponse.proto"
    dax
    (Proxy :: Proxy DeleteSubnetGroup)

responseDescribeParameterGroups :: DescribeParameterGroupsResponse -> TestTree
responseDescribeParameterGroups = res
    "DescribeParameterGroupsResponse"
    "fixture/DescribeParameterGroupsResponse.proto"
    dax
    (Proxy :: Proxy DescribeParameterGroups)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource = res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    dax
    (Proxy :: Proxy TagResource)

responseListTags :: ListTagsResponse -> TestTree
responseListTags = res
    "ListTagsResponse"
    "fixture/ListTagsResponse.proto"
    dax
    (Proxy :: Proxy ListTags)

responseDecreaseReplicationFactor :: DecreaseReplicationFactorResponse -> TestTree
responseDecreaseReplicationFactor = res
    "DecreaseReplicationFactorResponse"
    "fixture/DecreaseReplicationFactorResponse.proto"
    dax
    (Proxy :: Proxy DecreaseReplicationFactor)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource = res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    dax
    (Proxy :: Proxy UntagResource)

responseRebootNode :: RebootNodeResponse -> TestTree
responseRebootNode = res
    "RebootNodeResponse"
    "fixture/RebootNodeResponse.proto"
    dax
    (Proxy :: Proxy RebootNode)
