{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.MemoryDb
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.MemoryDb where

import Amazonka.MemoryDb
import qualified Data.Proxy as Proxy
import Test.Amazonka.Fixture
import Test.Amazonka.MemoryDb.Internal
import Test.Amazonka.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestBatchUpdateCluster $
--             newBatchUpdateCluster
--
--         , requestCopySnapshot $
--             newCopySnapshot
--
--         , requestCreateACL $
--             newCreateACL
--
--         , requestCreateCluster $
--             newCreateCluster
--
--         , requestCreateParameterGroup $
--             newCreateParameterGroup
--
--         , requestCreateSnapshot $
--             newCreateSnapshot
--
--         , requestCreateSubnetGroup $
--             newCreateSubnetGroup
--
--         , requestCreateUser $
--             newCreateUser
--
--         , requestDeleteACL $
--             newDeleteACL
--
--         , requestDeleteCluster $
--             newDeleteCluster
--
--         , requestDeleteParameterGroup $
--             newDeleteParameterGroup
--
--         , requestDeleteSnapshot $
--             newDeleteSnapshot
--
--         , requestDeleteSubnetGroup $
--             newDeleteSubnetGroup
--
--         , requestDeleteUser $
--             newDeleteUser
--
--         , requestDescribeACLs $
--             newDescribeACLs
--
--         , requestDescribeClusters $
--             newDescribeClusters
--
--         , requestDescribeEngineVersions $
--             newDescribeEngineVersions
--
--         , requestDescribeEvents $
--             newDescribeEvents
--
--         , requestDescribeParameterGroups $
--             newDescribeParameterGroups
--
--         , requestDescribeParameters $
--             newDescribeParameters
--
--         , requestDescribeReservedNodes $
--             newDescribeReservedNodes
--
--         , requestDescribeReservedNodesOfferings $
--             newDescribeReservedNodesOfferings
--
--         , requestDescribeServiceUpdates $
--             newDescribeServiceUpdates
--
--         , requestDescribeSnapshots $
--             newDescribeSnapshots
--
--         , requestDescribeSubnetGroups $
--             newDescribeSubnetGroups
--
--         , requestDescribeUsers $
--             newDescribeUsers
--
--         , requestFailoverShard $
--             newFailoverShard
--
--         , requestListAllowedNodeTypeUpdates $
--             newListAllowedNodeTypeUpdates
--
--         , requestListTags $
--             newListTags
--
--         , requestPurchaseReservedNodesOffering $
--             newPurchaseReservedNodesOffering
--
--         , requestResetParameterGroup $
--             newResetParameterGroup
--
--         , requestTagResource $
--             newTagResource
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestUpdateACL $
--             newUpdateACL
--
--         , requestUpdateCluster $
--             newUpdateCluster
--
--         , requestUpdateParameterGroup $
--             newUpdateParameterGroup
--
--         , requestUpdateSubnetGroup $
--             newUpdateSubnetGroup
--
--         , requestUpdateUser $
--             newUpdateUser
--
--           ]

--     , testGroup "response"
--         [ responseBatchUpdateCluster $
--             newBatchUpdateClusterResponse
--
--         , responseCopySnapshot $
--             newCopySnapshotResponse
--
--         , responseCreateACL $
--             newCreateACLResponse
--
--         , responseCreateCluster $
--             newCreateClusterResponse
--
--         , responseCreateParameterGroup $
--             newCreateParameterGroupResponse
--
--         , responseCreateSnapshot $
--             newCreateSnapshotResponse
--
--         , responseCreateSubnetGroup $
--             newCreateSubnetGroupResponse
--
--         , responseCreateUser $
--             newCreateUserResponse
--
--         , responseDeleteACL $
--             newDeleteACLResponse
--
--         , responseDeleteCluster $
--             newDeleteClusterResponse
--
--         , responseDeleteParameterGroup $
--             newDeleteParameterGroupResponse
--
--         , responseDeleteSnapshot $
--             newDeleteSnapshotResponse
--
--         , responseDeleteSubnetGroup $
--             newDeleteSubnetGroupResponse
--
--         , responseDeleteUser $
--             newDeleteUserResponse
--
--         , responseDescribeACLs $
--             newDescribeACLsResponse
--
--         , responseDescribeClusters $
--             newDescribeClustersResponse
--
--         , responseDescribeEngineVersions $
--             newDescribeEngineVersionsResponse
--
--         , responseDescribeEvents $
--             newDescribeEventsResponse
--
--         , responseDescribeParameterGroups $
--             newDescribeParameterGroupsResponse
--
--         , responseDescribeParameters $
--             newDescribeParametersResponse
--
--         , responseDescribeReservedNodes $
--             newDescribeReservedNodesResponse
--
--         , responseDescribeReservedNodesOfferings $
--             newDescribeReservedNodesOfferingsResponse
--
--         , responseDescribeServiceUpdates $
--             newDescribeServiceUpdatesResponse
--
--         , responseDescribeSnapshots $
--             newDescribeSnapshotsResponse
--
--         , responseDescribeSubnetGroups $
--             newDescribeSubnetGroupsResponse
--
--         , responseDescribeUsers $
--             newDescribeUsersResponse
--
--         , responseFailoverShard $
--             newFailoverShardResponse
--
--         , responseListAllowedNodeTypeUpdates $
--             newListAllowedNodeTypeUpdatesResponse
--
--         , responseListTags $
--             newListTagsResponse
--
--         , responsePurchaseReservedNodesOffering $
--             newPurchaseReservedNodesOfferingResponse
--
--         , responseResetParameterGroup $
--             newResetParameterGroupResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseUpdateACL $
--             newUpdateACLResponse
--
--         , responseUpdateCluster $
--             newUpdateClusterResponse
--
--         , responseUpdateParameterGroup $
--             newUpdateParameterGroupResponse
--
--         , responseUpdateSubnetGroup $
--             newUpdateSubnetGroupResponse
--
--         , responseUpdateUser $
--             newUpdateUserResponse
--
--           ]
--     ]

-- Requests

requestBatchUpdateCluster :: BatchUpdateCluster -> TestTree
requestBatchUpdateCluster =
  req
    "BatchUpdateCluster"
    "fixture/BatchUpdateCluster.yaml"

requestCopySnapshot :: CopySnapshot -> TestTree
requestCopySnapshot =
  req
    "CopySnapshot"
    "fixture/CopySnapshot.yaml"

requestCreateACL :: CreateACL -> TestTree
requestCreateACL =
  req
    "CreateACL"
    "fixture/CreateACL.yaml"

requestCreateCluster :: CreateCluster -> TestTree
requestCreateCluster =
  req
    "CreateCluster"
    "fixture/CreateCluster.yaml"

requestCreateParameterGroup :: CreateParameterGroup -> TestTree
requestCreateParameterGroup =
  req
    "CreateParameterGroup"
    "fixture/CreateParameterGroup.yaml"

requestCreateSnapshot :: CreateSnapshot -> TestTree
requestCreateSnapshot =
  req
    "CreateSnapshot"
    "fixture/CreateSnapshot.yaml"

requestCreateSubnetGroup :: CreateSubnetGroup -> TestTree
requestCreateSubnetGroup =
  req
    "CreateSubnetGroup"
    "fixture/CreateSubnetGroup.yaml"

requestCreateUser :: CreateUser -> TestTree
requestCreateUser =
  req
    "CreateUser"
    "fixture/CreateUser.yaml"

requestDeleteACL :: DeleteACL -> TestTree
requestDeleteACL =
  req
    "DeleteACL"
    "fixture/DeleteACL.yaml"

requestDeleteCluster :: DeleteCluster -> TestTree
requestDeleteCluster =
  req
    "DeleteCluster"
    "fixture/DeleteCluster.yaml"

requestDeleteParameterGroup :: DeleteParameterGroup -> TestTree
requestDeleteParameterGroup =
  req
    "DeleteParameterGroup"
    "fixture/DeleteParameterGroup.yaml"

requestDeleteSnapshot :: DeleteSnapshot -> TestTree
requestDeleteSnapshot =
  req
    "DeleteSnapshot"
    "fixture/DeleteSnapshot.yaml"

requestDeleteSubnetGroup :: DeleteSubnetGroup -> TestTree
requestDeleteSubnetGroup =
  req
    "DeleteSubnetGroup"
    "fixture/DeleteSubnetGroup.yaml"

requestDeleteUser :: DeleteUser -> TestTree
requestDeleteUser =
  req
    "DeleteUser"
    "fixture/DeleteUser.yaml"

requestDescribeACLs :: DescribeACLs -> TestTree
requestDescribeACLs =
  req
    "DescribeACLs"
    "fixture/DescribeACLs.yaml"

requestDescribeClusters :: DescribeClusters -> TestTree
requestDescribeClusters =
  req
    "DescribeClusters"
    "fixture/DescribeClusters.yaml"

requestDescribeEngineVersions :: DescribeEngineVersions -> TestTree
requestDescribeEngineVersions =
  req
    "DescribeEngineVersions"
    "fixture/DescribeEngineVersions.yaml"

requestDescribeEvents :: DescribeEvents -> TestTree
requestDescribeEvents =
  req
    "DescribeEvents"
    "fixture/DescribeEvents.yaml"

requestDescribeParameterGroups :: DescribeParameterGroups -> TestTree
requestDescribeParameterGroups =
  req
    "DescribeParameterGroups"
    "fixture/DescribeParameterGroups.yaml"

requestDescribeParameters :: DescribeParameters -> TestTree
requestDescribeParameters =
  req
    "DescribeParameters"
    "fixture/DescribeParameters.yaml"

requestDescribeReservedNodes :: DescribeReservedNodes -> TestTree
requestDescribeReservedNodes =
  req
    "DescribeReservedNodes"
    "fixture/DescribeReservedNodes.yaml"

requestDescribeReservedNodesOfferings :: DescribeReservedNodesOfferings -> TestTree
requestDescribeReservedNodesOfferings =
  req
    "DescribeReservedNodesOfferings"
    "fixture/DescribeReservedNodesOfferings.yaml"

requestDescribeServiceUpdates :: DescribeServiceUpdates -> TestTree
requestDescribeServiceUpdates =
  req
    "DescribeServiceUpdates"
    "fixture/DescribeServiceUpdates.yaml"

requestDescribeSnapshots :: DescribeSnapshots -> TestTree
requestDescribeSnapshots =
  req
    "DescribeSnapshots"
    "fixture/DescribeSnapshots.yaml"

requestDescribeSubnetGroups :: DescribeSubnetGroups -> TestTree
requestDescribeSubnetGroups =
  req
    "DescribeSubnetGroups"
    "fixture/DescribeSubnetGroups.yaml"

requestDescribeUsers :: DescribeUsers -> TestTree
requestDescribeUsers =
  req
    "DescribeUsers"
    "fixture/DescribeUsers.yaml"

requestFailoverShard :: FailoverShard -> TestTree
requestFailoverShard =
  req
    "FailoverShard"
    "fixture/FailoverShard.yaml"

requestListAllowedNodeTypeUpdates :: ListAllowedNodeTypeUpdates -> TestTree
requestListAllowedNodeTypeUpdates =
  req
    "ListAllowedNodeTypeUpdates"
    "fixture/ListAllowedNodeTypeUpdates.yaml"

requestListTags :: ListTags -> TestTree
requestListTags =
  req
    "ListTags"
    "fixture/ListTags.yaml"

requestPurchaseReservedNodesOffering :: PurchaseReservedNodesOffering -> TestTree
requestPurchaseReservedNodesOffering =
  req
    "PurchaseReservedNodesOffering"
    "fixture/PurchaseReservedNodesOffering.yaml"

requestResetParameterGroup :: ResetParameterGroup -> TestTree
requestResetParameterGroup =
  req
    "ResetParameterGroup"
    "fixture/ResetParameterGroup.yaml"

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

requestUpdateACL :: UpdateACL -> TestTree
requestUpdateACL =
  req
    "UpdateACL"
    "fixture/UpdateACL.yaml"

requestUpdateCluster :: UpdateCluster -> TestTree
requestUpdateCluster =
  req
    "UpdateCluster"
    "fixture/UpdateCluster.yaml"

requestUpdateParameterGroup :: UpdateParameterGroup -> TestTree
requestUpdateParameterGroup =
  req
    "UpdateParameterGroup"
    "fixture/UpdateParameterGroup.yaml"

requestUpdateSubnetGroup :: UpdateSubnetGroup -> TestTree
requestUpdateSubnetGroup =
  req
    "UpdateSubnetGroup"
    "fixture/UpdateSubnetGroup.yaml"

requestUpdateUser :: UpdateUser -> TestTree
requestUpdateUser =
  req
    "UpdateUser"
    "fixture/UpdateUser.yaml"

-- Responses

responseBatchUpdateCluster :: BatchUpdateClusterResponse -> TestTree
responseBatchUpdateCluster =
  res
    "BatchUpdateClusterResponse"
    "fixture/BatchUpdateClusterResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchUpdateCluster)

responseCopySnapshot :: CopySnapshotResponse -> TestTree
responseCopySnapshot =
  res
    "CopySnapshotResponse"
    "fixture/CopySnapshotResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CopySnapshot)

responseCreateACL :: CreateACLResponse -> TestTree
responseCreateACL =
  res
    "CreateACLResponse"
    "fixture/CreateACLResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateACL)

responseCreateCluster :: CreateClusterResponse -> TestTree
responseCreateCluster =
  res
    "CreateClusterResponse"
    "fixture/CreateClusterResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateCluster)

responseCreateParameterGroup :: CreateParameterGroupResponse -> TestTree
responseCreateParameterGroup =
  res
    "CreateParameterGroupResponse"
    "fixture/CreateParameterGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateParameterGroup)

responseCreateSnapshot :: CreateSnapshotResponse -> TestTree
responseCreateSnapshot =
  res
    "CreateSnapshotResponse"
    "fixture/CreateSnapshotResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateSnapshot)

responseCreateSubnetGroup :: CreateSubnetGroupResponse -> TestTree
responseCreateSubnetGroup =
  res
    "CreateSubnetGroupResponse"
    "fixture/CreateSubnetGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateSubnetGroup)

responseCreateUser :: CreateUserResponse -> TestTree
responseCreateUser =
  res
    "CreateUserResponse"
    "fixture/CreateUserResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateUser)

responseDeleteACL :: DeleteACLResponse -> TestTree
responseDeleteACL =
  res
    "DeleteACLResponse"
    "fixture/DeleteACLResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteACL)

responseDeleteCluster :: DeleteClusterResponse -> TestTree
responseDeleteCluster =
  res
    "DeleteClusterResponse"
    "fixture/DeleteClusterResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteCluster)

responseDeleteParameterGroup :: DeleteParameterGroupResponse -> TestTree
responseDeleteParameterGroup =
  res
    "DeleteParameterGroupResponse"
    "fixture/DeleteParameterGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteParameterGroup)

responseDeleteSnapshot :: DeleteSnapshotResponse -> TestTree
responseDeleteSnapshot =
  res
    "DeleteSnapshotResponse"
    "fixture/DeleteSnapshotResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteSnapshot)

responseDeleteSubnetGroup :: DeleteSubnetGroupResponse -> TestTree
responseDeleteSubnetGroup =
  res
    "DeleteSubnetGroupResponse"
    "fixture/DeleteSubnetGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteSubnetGroup)

responseDeleteUser :: DeleteUserResponse -> TestTree
responseDeleteUser =
  res
    "DeleteUserResponse"
    "fixture/DeleteUserResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteUser)

responseDescribeACLs :: DescribeACLsResponse -> TestTree
responseDescribeACLs =
  res
    "DescribeACLsResponse"
    "fixture/DescribeACLsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeACLs)

responseDescribeClusters :: DescribeClustersResponse -> TestTree
responseDescribeClusters =
  res
    "DescribeClustersResponse"
    "fixture/DescribeClustersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeClusters)

responseDescribeEngineVersions :: DescribeEngineVersionsResponse -> TestTree
responseDescribeEngineVersions =
  res
    "DescribeEngineVersionsResponse"
    "fixture/DescribeEngineVersionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeEngineVersions)

responseDescribeEvents :: DescribeEventsResponse -> TestTree
responseDescribeEvents =
  res
    "DescribeEventsResponse"
    "fixture/DescribeEventsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeEvents)

responseDescribeParameterGroups :: DescribeParameterGroupsResponse -> TestTree
responseDescribeParameterGroups =
  res
    "DescribeParameterGroupsResponse"
    "fixture/DescribeParameterGroupsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeParameterGroups)

responseDescribeParameters :: DescribeParametersResponse -> TestTree
responseDescribeParameters =
  res
    "DescribeParametersResponse"
    "fixture/DescribeParametersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeParameters)

responseDescribeReservedNodes :: DescribeReservedNodesResponse -> TestTree
responseDescribeReservedNodes =
  res
    "DescribeReservedNodesResponse"
    "fixture/DescribeReservedNodesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeReservedNodes)

responseDescribeReservedNodesOfferings :: DescribeReservedNodesOfferingsResponse -> TestTree
responseDescribeReservedNodesOfferings =
  res
    "DescribeReservedNodesOfferingsResponse"
    "fixture/DescribeReservedNodesOfferingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeReservedNodesOfferings)

responseDescribeServiceUpdates :: DescribeServiceUpdatesResponse -> TestTree
responseDescribeServiceUpdates =
  res
    "DescribeServiceUpdatesResponse"
    "fixture/DescribeServiceUpdatesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeServiceUpdates)

responseDescribeSnapshots :: DescribeSnapshotsResponse -> TestTree
responseDescribeSnapshots =
  res
    "DescribeSnapshotsResponse"
    "fixture/DescribeSnapshotsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeSnapshots)

responseDescribeSubnetGroups :: DescribeSubnetGroupsResponse -> TestTree
responseDescribeSubnetGroups =
  res
    "DescribeSubnetGroupsResponse"
    "fixture/DescribeSubnetGroupsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeSubnetGroups)

responseDescribeUsers :: DescribeUsersResponse -> TestTree
responseDescribeUsers =
  res
    "DescribeUsersResponse"
    "fixture/DescribeUsersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeUsers)

responseFailoverShard :: FailoverShardResponse -> TestTree
responseFailoverShard =
  res
    "FailoverShardResponse"
    "fixture/FailoverShardResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy FailoverShard)

responseListAllowedNodeTypeUpdates :: ListAllowedNodeTypeUpdatesResponse -> TestTree
responseListAllowedNodeTypeUpdates =
  res
    "ListAllowedNodeTypeUpdatesResponse"
    "fixture/ListAllowedNodeTypeUpdatesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListAllowedNodeTypeUpdates)

responseListTags :: ListTagsResponse -> TestTree
responseListTags =
  res
    "ListTagsResponse"
    "fixture/ListTagsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTags)

responsePurchaseReservedNodesOffering :: PurchaseReservedNodesOfferingResponse -> TestTree
responsePurchaseReservedNodesOffering =
  res
    "PurchaseReservedNodesOfferingResponse"
    "fixture/PurchaseReservedNodesOfferingResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PurchaseReservedNodesOffering)

responseResetParameterGroup :: ResetParameterGroupResponse -> TestTree
responseResetParameterGroup =
  res
    "ResetParameterGroupResponse"
    "fixture/ResetParameterGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ResetParameterGroup)

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

responseUpdateACL :: UpdateACLResponse -> TestTree
responseUpdateACL =
  res
    "UpdateACLResponse"
    "fixture/UpdateACLResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateACL)

responseUpdateCluster :: UpdateClusterResponse -> TestTree
responseUpdateCluster =
  res
    "UpdateClusterResponse"
    "fixture/UpdateClusterResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateCluster)

responseUpdateParameterGroup :: UpdateParameterGroupResponse -> TestTree
responseUpdateParameterGroup =
  res
    "UpdateParameterGroupResponse"
    "fixture/UpdateParameterGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateParameterGroup)

responseUpdateSubnetGroup :: UpdateSubnetGroupResponse -> TestTree
responseUpdateSubnetGroup =
  res
    "UpdateSubnetGroupResponse"
    "fixture/UpdateSubnetGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateSubnetGroup)

responseUpdateUser :: UpdateUserResponse -> TestTree
responseUpdateUser =
  res
    "UpdateUserResponse"
    "fixture/UpdateUserResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateUser)
