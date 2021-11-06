{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.MemoryDb
-- Copyright   : (c) 2013-2021 Brendan Hay
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
--         [ requestDescribeClusters $
--             newDescribeClusters
--
--         , requestBatchUpdateCluster $
--             newBatchUpdateCluster
--
--         , requestDescribeUsers $
--             newDescribeUsers
--
--         , requestDescribeParameters $
--             newDescribeParameters
--
--         , requestDeleteACL $
--             newDeleteACL
--
--         , requestUpdateACL $
--             newUpdateACL
--
--         , requestDescribeEvents $
--             newDescribeEvents
--
--         , requestDescribeEngineVersions $
--             newDescribeEngineVersions
--
--         , requestDescribeACLs $
--             newDescribeACLs
--
--         , requestCreateSubnetGroup $
--             newCreateSubnetGroup
--
--         , requestDeleteCluster $
--             newDeleteCluster
--
--         , requestUpdateCluster $
--             newUpdateCluster
--
--         , requestCreateCluster $
--             newCreateCluster
--
--         , requestCopySnapshot $
--             newCopySnapshot
--
--         , requestDeleteParameterGroup $
--             newDeleteParameterGroup
--
--         , requestUpdateParameterGroup $
--             newUpdateParameterGroup
--
--         , requestDescribeSubnetGroups $
--             newDescribeSubnetGroups
--
--         , requestDescribeServiceUpdates $
--             newDescribeServiceUpdates
--
--         , requestCreateParameterGroup $
--             newCreateParameterGroup
--
--         , requestDescribeSnapshots $
--             newDescribeSnapshots
--
--         , requestCreateACL $
--             newCreateACL
--
--         , requestUpdateSubnetGroup $
--             newUpdateSubnetGroup
--
--         , requestDeleteSubnetGroup $
--             newDeleteSubnetGroup
--
--         , requestCreateUser $
--             newCreateUser
--
--         , requestDeleteSnapshot $
--             newDeleteSnapshot
--
--         , requestFailoverShard $
--             newFailoverShard
--
--         , requestUpdateUser $
--             newUpdateUser
--
--         , requestDeleteUser $
--             newDeleteUser
--
--         , requestListAllowedNodeTypeUpdates $
--             newListAllowedNodeTypeUpdates
--
--         , requestDescribeParameterGroups $
--             newDescribeParameterGroups
--
--         , requestCreateSnapshot $
--             newCreateSnapshot
--
--         , requestTagResource $
--             newTagResource
--
--         , requestListTags $
--             newListTags
--
--         , requestResetParameterGroup $
--             newResetParameterGroup
--
--         , requestUntagResource $
--             newUntagResource
--
--           ]

--     , testGroup "response"
--         [ responseDescribeClusters $
--             newDescribeClustersResponse
--
--         , responseBatchUpdateCluster $
--             newBatchUpdateClusterResponse
--
--         , responseDescribeUsers $
--             newDescribeUsersResponse
--
--         , responseDescribeParameters $
--             newDescribeParametersResponse
--
--         , responseDeleteACL $
--             newDeleteACLResponse
--
--         , responseUpdateACL $
--             newUpdateACLResponse
--
--         , responseDescribeEvents $
--             newDescribeEventsResponse
--
--         , responseDescribeEngineVersions $
--             newDescribeEngineVersionsResponse
--
--         , responseDescribeACLs $
--             newDescribeACLsResponse
--
--         , responseCreateSubnetGroup $
--             newCreateSubnetGroupResponse
--
--         , responseDeleteCluster $
--             newDeleteClusterResponse
--
--         , responseUpdateCluster $
--             newUpdateClusterResponse
--
--         , responseCreateCluster $
--             newCreateClusterResponse
--
--         , responseCopySnapshot $
--             newCopySnapshotResponse
--
--         , responseDeleteParameterGroup $
--             newDeleteParameterGroupResponse
--
--         , responseUpdateParameterGroup $
--             newUpdateParameterGroupResponse
--
--         , responseDescribeSubnetGroups $
--             newDescribeSubnetGroupsResponse
--
--         , responseDescribeServiceUpdates $
--             newDescribeServiceUpdatesResponse
--
--         , responseCreateParameterGroup $
--             newCreateParameterGroupResponse
--
--         , responseDescribeSnapshots $
--             newDescribeSnapshotsResponse
--
--         , responseCreateACL $
--             newCreateACLResponse
--
--         , responseUpdateSubnetGroup $
--             newUpdateSubnetGroupResponse
--
--         , responseDeleteSubnetGroup $
--             newDeleteSubnetGroupResponse
--
--         , responseCreateUser $
--             newCreateUserResponse
--
--         , responseDeleteSnapshot $
--             newDeleteSnapshotResponse
--
--         , responseFailoverShard $
--             newFailoverShardResponse
--
--         , responseUpdateUser $
--             newUpdateUserResponse
--
--         , responseDeleteUser $
--             newDeleteUserResponse
--
--         , responseListAllowedNodeTypeUpdates $
--             newListAllowedNodeTypeUpdatesResponse
--
--         , responseDescribeParameterGroups $
--             newDescribeParameterGroupsResponse
--
--         , responseCreateSnapshot $
--             newCreateSnapshotResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseListTags $
--             newListTagsResponse
--
--         , responseResetParameterGroup $
--             newResetParameterGroupResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--           ]
--     ]

-- Requests

requestDescribeClusters :: DescribeClusters -> TestTree
requestDescribeClusters =
  req
    "DescribeClusters"
    "fixture/DescribeClusters.yaml"

requestBatchUpdateCluster :: BatchUpdateCluster -> TestTree
requestBatchUpdateCluster =
  req
    "BatchUpdateCluster"
    "fixture/BatchUpdateCluster.yaml"

requestDescribeUsers :: DescribeUsers -> TestTree
requestDescribeUsers =
  req
    "DescribeUsers"
    "fixture/DescribeUsers.yaml"

requestDescribeParameters :: DescribeParameters -> TestTree
requestDescribeParameters =
  req
    "DescribeParameters"
    "fixture/DescribeParameters.yaml"

requestDeleteACL :: DeleteACL -> TestTree
requestDeleteACL =
  req
    "DeleteACL"
    "fixture/DeleteACL.yaml"

requestUpdateACL :: UpdateACL -> TestTree
requestUpdateACL =
  req
    "UpdateACL"
    "fixture/UpdateACL.yaml"

requestDescribeEvents :: DescribeEvents -> TestTree
requestDescribeEvents =
  req
    "DescribeEvents"
    "fixture/DescribeEvents.yaml"

requestDescribeEngineVersions :: DescribeEngineVersions -> TestTree
requestDescribeEngineVersions =
  req
    "DescribeEngineVersions"
    "fixture/DescribeEngineVersions.yaml"

requestDescribeACLs :: DescribeACLs -> TestTree
requestDescribeACLs =
  req
    "DescribeACLs"
    "fixture/DescribeACLs.yaml"

requestCreateSubnetGroup :: CreateSubnetGroup -> TestTree
requestCreateSubnetGroup =
  req
    "CreateSubnetGroup"
    "fixture/CreateSubnetGroup.yaml"

requestDeleteCluster :: DeleteCluster -> TestTree
requestDeleteCluster =
  req
    "DeleteCluster"
    "fixture/DeleteCluster.yaml"

requestUpdateCluster :: UpdateCluster -> TestTree
requestUpdateCluster =
  req
    "UpdateCluster"
    "fixture/UpdateCluster.yaml"

requestCreateCluster :: CreateCluster -> TestTree
requestCreateCluster =
  req
    "CreateCluster"
    "fixture/CreateCluster.yaml"

requestCopySnapshot :: CopySnapshot -> TestTree
requestCopySnapshot =
  req
    "CopySnapshot"
    "fixture/CopySnapshot.yaml"

requestDeleteParameterGroup :: DeleteParameterGroup -> TestTree
requestDeleteParameterGroup =
  req
    "DeleteParameterGroup"
    "fixture/DeleteParameterGroup.yaml"

requestUpdateParameterGroup :: UpdateParameterGroup -> TestTree
requestUpdateParameterGroup =
  req
    "UpdateParameterGroup"
    "fixture/UpdateParameterGroup.yaml"

requestDescribeSubnetGroups :: DescribeSubnetGroups -> TestTree
requestDescribeSubnetGroups =
  req
    "DescribeSubnetGroups"
    "fixture/DescribeSubnetGroups.yaml"

requestDescribeServiceUpdates :: DescribeServiceUpdates -> TestTree
requestDescribeServiceUpdates =
  req
    "DescribeServiceUpdates"
    "fixture/DescribeServiceUpdates.yaml"

requestCreateParameterGroup :: CreateParameterGroup -> TestTree
requestCreateParameterGroup =
  req
    "CreateParameterGroup"
    "fixture/CreateParameterGroup.yaml"

requestDescribeSnapshots :: DescribeSnapshots -> TestTree
requestDescribeSnapshots =
  req
    "DescribeSnapshots"
    "fixture/DescribeSnapshots.yaml"

requestCreateACL :: CreateACL -> TestTree
requestCreateACL =
  req
    "CreateACL"
    "fixture/CreateACL.yaml"

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

requestCreateUser :: CreateUser -> TestTree
requestCreateUser =
  req
    "CreateUser"
    "fixture/CreateUser.yaml"

requestDeleteSnapshot :: DeleteSnapshot -> TestTree
requestDeleteSnapshot =
  req
    "DeleteSnapshot"
    "fixture/DeleteSnapshot.yaml"

requestFailoverShard :: FailoverShard -> TestTree
requestFailoverShard =
  req
    "FailoverShard"
    "fixture/FailoverShard.yaml"

requestUpdateUser :: UpdateUser -> TestTree
requestUpdateUser =
  req
    "UpdateUser"
    "fixture/UpdateUser.yaml"

requestDeleteUser :: DeleteUser -> TestTree
requestDeleteUser =
  req
    "DeleteUser"
    "fixture/DeleteUser.yaml"

requestListAllowedNodeTypeUpdates :: ListAllowedNodeTypeUpdates -> TestTree
requestListAllowedNodeTypeUpdates =
  req
    "ListAllowedNodeTypeUpdates"
    "fixture/ListAllowedNodeTypeUpdates.yaml"

requestDescribeParameterGroups :: DescribeParameterGroups -> TestTree
requestDescribeParameterGroups =
  req
    "DescribeParameterGroups"
    "fixture/DescribeParameterGroups.yaml"

requestCreateSnapshot :: CreateSnapshot -> TestTree
requestCreateSnapshot =
  req
    "CreateSnapshot"
    "fixture/CreateSnapshot.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestListTags :: ListTags -> TestTree
requestListTags =
  req
    "ListTags"
    "fixture/ListTags.yaml"

requestResetParameterGroup :: ResetParameterGroup -> TestTree
requestResetParameterGroup =
  req
    "ResetParameterGroup"
    "fixture/ResetParameterGroup.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

-- Responses

responseDescribeClusters :: DescribeClustersResponse -> TestTree
responseDescribeClusters =
  res
    "DescribeClustersResponse"
    "fixture/DescribeClustersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeClusters)

responseBatchUpdateCluster :: BatchUpdateClusterResponse -> TestTree
responseBatchUpdateCluster =
  res
    "BatchUpdateClusterResponse"
    "fixture/BatchUpdateClusterResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchUpdateCluster)

responseDescribeUsers :: DescribeUsersResponse -> TestTree
responseDescribeUsers =
  res
    "DescribeUsersResponse"
    "fixture/DescribeUsersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeUsers)

responseDescribeParameters :: DescribeParametersResponse -> TestTree
responseDescribeParameters =
  res
    "DescribeParametersResponse"
    "fixture/DescribeParametersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeParameters)

responseDeleteACL :: DeleteACLResponse -> TestTree
responseDeleteACL =
  res
    "DeleteACLResponse"
    "fixture/DeleteACLResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteACL)

responseUpdateACL :: UpdateACLResponse -> TestTree
responseUpdateACL =
  res
    "UpdateACLResponse"
    "fixture/UpdateACLResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateACL)

responseDescribeEvents :: DescribeEventsResponse -> TestTree
responseDescribeEvents =
  res
    "DescribeEventsResponse"
    "fixture/DescribeEventsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeEvents)

responseDescribeEngineVersions :: DescribeEngineVersionsResponse -> TestTree
responseDescribeEngineVersions =
  res
    "DescribeEngineVersionsResponse"
    "fixture/DescribeEngineVersionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeEngineVersions)

responseDescribeACLs :: DescribeACLsResponse -> TestTree
responseDescribeACLs =
  res
    "DescribeACLsResponse"
    "fixture/DescribeACLsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeACLs)

responseCreateSubnetGroup :: CreateSubnetGroupResponse -> TestTree
responseCreateSubnetGroup =
  res
    "CreateSubnetGroupResponse"
    "fixture/CreateSubnetGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateSubnetGroup)

responseDeleteCluster :: DeleteClusterResponse -> TestTree
responseDeleteCluster =
  res
    "DeleteClusterResponse"
    "fixture/DeleteClusterResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteCluster)

responseUpdateCluster :: UpdateClusterResponse -> TestTree
responseUpdateCluster =
  res
    "UpdateClusterResponse"
    "fixture/UpdateClusterResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateCluster)

responseCreateCluster :: CreateClusterResponse -> TestTree
responseCreateCluster =
  res
    "CreateClusterResponse"
    "fixture/CreateClusterResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateCluster)

responseCopySnapshot :: CopySnapshotResponse -> TestTree
responseCopySnapshot =
  res
    "CopySnapshotResponse"
    "fixture/CopySnapshotResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CopySnapshot)

responseDeleteParameterGroup :: DeleteParameterGroupResponse -> TestTree
responseDeleteParameterGroup =
  res
    "DeleteParameterGroupResponse"
    "fixture/DeleteParameterGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteParameterGroup)

responseUpdateParameterGroup :: UpdateParameterGroupResponse -> TestTree
responseUpdateParameterGroup =
  res
    "UpdateParameterGroupResponse"
    "fixture/UpdateParameterGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateParameterGroup)

responseDescribeSubnetGroups :: DescribeSubnetGroupsResponse -> TestTree
responseDescribeSubnetGroups =
  res
    "DescribeSubnetGroupsResponse"
    "fixture/DescribeSubnetGroupsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeSubnetGroups)

responseDescribeServiceUpdates :: DescribeServiceUpdatesResponse -> TestTree
responseDescribeServiceUpdates =
  res
    "DescribeServiceUpdatesResponse"
    "fixture/DescribeServiceUpdatesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeServiceUpdates)

responseCreateParameterGroup :: CreateParameterGroupResponse -> TestTree
responseCreateParameterGroup =
  res
    "CreateParameterGroupResponse"
    "fixture/CreateParameterGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateParameterGroup)

responseDescribeSnapshots :: DescribeSnapshotsResponse -> TestTree
responseDescribeSnapshots =
  res
    "DescribeSnapshotsResponse"
    "fixture/DescribeSnapshotsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeSnapshots)

responseCreateACL :: CreateACLResponse -> TestTree
responseCreateACL =
  res
    "CreateACLResponse"
    "fixture/CreateACLResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateACL)

responseUpdateSubnetGroup :: UpdateSubnetGroupResponse -> TestTree
responseUpdateSubnetGroup =
  res
    "UpdateSubnetGroupResponse"
    "fixture/UpdateSubnetGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateSubnetGroup)

responseDeleteSubnetGroup :: DeleteSubnetGroupResponse -> TestTree
responseDeleteSubnetGroup =
  res
    "DeleteSubnetGroupResponse"
    "fixture/DeleteSubnetGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteSubnetGroup)

responseCreateUser :: CreateUserResponse -> TestTree
responseCreateUser =
  res
    "CreateUserResponse"
    "fixture/CreateUserResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateUser)

responseDeleteSnapshot :: DeleteSnapshotResponse -> TestTree
responseDeleteSnapshot =
  res
    "DeleteSnapshotResponse"
    "fixture/DeleteSnapshotResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteSnapshot)

responseFailoverShard :: FailoverShardResponse -> TestTree
responseFailoverShard =
  res
    "FailoverShardResponse"
    "fixture/FailoverShardResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy FailoverShard)

responseUpdateUser :: UpdateUserResponse -> TestTree
responseUpdateUser =
  res
    "UpdateUserResponse"
    "fixture/UpdateUserResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateUser)

responseDeleteUser :: DeleteUserResponse -> TestTree
responseDeleteUser =
  res
    "DeleteUserResponse"
    "fixture/DeleteUserResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteUser)

responseListAllowedNodeTypeUpdates :: ListAllowedNodeTypeUpdatesResponse -> TestTree
responseListAllowedNodeTypeUpdates =
  res
    "ListAllowedNodeTypeUpdatesResponse"
    "fixture/ListAllowedNodeTypeUpdatesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListAllowedNodeTypeUpdates)

responseDescribeParameterGroups :: DescribeParameterGroupsResponse -> TestTree
responseDescribeParameterGroups =
  res
    "DescribeParameterGroupsResponse"
    "fixture/DescribeParameterGroupsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeParameterGroups)

responseCreateSnapshot :: CreateSnapshotResponse -> TestTree
responseCreateSnapshot =
  res
    "CreateSnapshotResponse"
    "fixture/CreateSnapshotResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateSnapshot)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TagResource)

responseListTags :: ListTagsResponse -> TestTree
responseListTags =
  res
    "ListTagsResponse"
    "fixture/ListTagsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTags)

responseResetParameterGroup :: ResetParameterGroupResponse -> TestTree
responseResetParameterGroup =
  res
    "ResetParameterGroupResponse"
    "fixture/ResetParameterGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ResetParameterGroup)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UntagResource)
