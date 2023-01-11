{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.RedshiftServerLess
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.RedshiftServerLess where

import Amazonka.RedshiftServerLess
import qualified Data.Proxy as Proxy
import Test.Amazonka.Fixture
import Test.Amazonka.Prelude
import Test.Amazonka.RedshiftServerLess.Internal
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestConvertRecoveryPointToSnapshot $
--             newConvertRecoveryPointToSnapshot
--
--         , requestCreateEndpointAccess $
--             newCreateEndpointAccess
--
--         , requestCreateNamespace $
--             newCreateNamespace
--
--         , requestCreateSnapshot $
--             newCreateSnapshot
--
--         , requestCreateUsageLimit $
--             newCreateUsageLimit
--
--         , requestCreateWorkgroup $
--             newCreateWorkgroup
--
--         , requestDeleteEndpointAccess $
--             newDeleteEndpointAccess
--
--         , requestDeleteNamespace $
--             newDeleteNamespace
--
--         , requestDeleteResourcePolicy $
--             newDeleteResourcePolicy
--
--         , requestDeleteSnapshot $
--             newDeleteSnapshot
--
--         , requestDeleteUsageLimit $
--             newDeleteUsageLimit
--
--         , requestDeleteWorkgroup $
--             newDeleteWorkgroup
--
--         , requestGetCredentials $
--             newGetCredentials
--
--         , requestGetEndpointAccess $
--             newGetEndpointAccess
--
--         , requestGetNamespace $
--             newGetNamespace
--
--         , requestGetRecoveryPoint $
--             newGetRecoveryPoint
--
--         , requestGetResourcePolicy $
--             newGetResourcePolicy
--
--         , requestGetSnapshot $
--             newGetSnapshot
--
--         , requestGetTableRestoreStatus $
--             newGetTableRestoreStatus
--
--         , requestGetUsageLimit $
--             newGetUsageLimit
--
--         , requestGetWorkgroup $
--             newGetWorkgroup
--
--         , requestListEndpointAccess $
--             newListEndpointAccess
--
--         , requestListNamespaces $
--             newListNamespaces
--
--         , requestListRecoveryPoints $
--             newListRecoveryPoints
--
--         , requestListSnapshots $
--             newListSnapshots
--
--         , requestListTableRestoreStatus $
--             newListTableRestoreStatus
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestListUsageLimits $
--             newListUsageLimits
--
--         , requestListWorkgroups $
--             newListWorkgroups
--
--         , requestPutResourcePolicy $
--             newPutResourcePolicy
--
--         , requestRestoreFromRecoveryPoint $
--             newRestoreFromRecoveryPoint
--
--         , requestRestoreFromSnapshot $
--             newRestoreFromSnapshot
--
--         , requestRestoreTableFromSnapshot $
--             newRestoreTableFromSnapshot
--
--         , requestTagResource $
--             newTagResource
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestUpdateEndpointAccess $
--             newUpdateEndpointAccess
--
--         , requestUpdateNamespace $
--             newUpdateNamespace
--
--         , requestUpdateSnapshot $
--             newUpdateSnapshot
--
--         , requestUpdateUsageLimit $
--             newUpdateUsageLimit
--
--         , requestUpdateWorkgroup $
--             newUpdateWorkgroup
--
--           ]

--     , testGroup "response"
--         [ responseConvertRecoveryPointToSnapshot $
--             newConvertRecoveryPointToSnapshotResponse
--
--         , responseCreateEndpointAccess $
--             newCreateEndpointAccessResponse
--
--         , responseCreateNamespace $
--             newCreateNamespaceResponse
--
--         , responseCreateSnapshot $
--             newCreateSnapshotResponse
--
--         , responseCreateUsageLimit $
--             newCreateUsageLimitResponse
--
--         , responseCreateWorkgroup $
--             newCreateWorkgroupResponse
--
--         , responseDeleteEndpointAccess $
--             newDeleteEndpointAccessResponse
--
--         , responseDeleteNamespace $
--             newDeleteNamespaceResponse
--
--         , responseDeleteResourcePolicy $
--             newDeleteResourcePolicyResponse
--
--         , responseDeleteSnapshot $
--             newDeleteSnapshotResponse
--
--         , responseDeleteUsageLimit $
--             newDeleteUsageLimitResponse
--
--         , responseDeleteWorkgroup $
--             newDeleteWorkgroupResponse
--
--         , responseGetCredentials $
--             newGetCredentialsResponse
--
--         , responseGetEndpointAccess $
--             newGetEndpointAccessResponse
--
--         , responseGetNamespace $
--             newGetNamespaceResponse
--
--         , responseGetRecoveryPoint $
--             newGetRecoveryPointResponse
--
--         , responseGetResourcePolicy $
--             newGetResourcePolicyResponse
--
--         , responseGetSnapshot $
--             newGetSnapshotResponse
--
--         , responseGetTableRestoreStatus $
--             newGetTableRestoreStatusResponse
--
--         , responseGetUsageLimit $
--             newGetUsageLimitResponse
--
--         , responseGetWorkgroup $
--             newGetWorkgroupResponse
--
--         , responseListEndpointAccess $
--             newListEndpointAccessResponse
--
--         , responseListNamespaces $
--             newListNamespacesResponse
--
--         , responseListRecoveryPoints $
--             newListRecoveryPointsResponse
--
--         , responseListSnapshots $
--             newListSnapshotsResponse
--
--         , responseListTableRestoreStatus $
--             newListTableRestoreStatusResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseListUsageLimits $
--             newListUsageLimitsResponse
--
--         , responseListWorkgroups $
--             newListWorkgroupsResponse
--
--         , responsePutResourcePolicy $
--             newPutResourcePolicyResponse
--
--         , responseRestoreFromRecoveryPoint $
--             newRestoreFromRecoveryPointResponse
--
--         , responseRestoreFromSnapshot $
--             newRestoreFromSnapshotResponse
--
--         , responseRestoreTableFromSnapshot $
--             newRestoreTableFromSnapshotResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseUpdateEndpointAccess $
--             newUpdateEndpointAccessResponse
--
--         , responseUpdateNamespace $
--             newUpdateNamespaceResponse
--
--         , responseUpdateSnapshot $
--             newUpdateSnapshotResponse
--
--         , responseUpdateUsageLimit $
--             newUpdateUsageLimitResponse
--
--         , responseUpdateWorkgroup $
--             newUpdateWorkgroupResponse
--
--           ]
--     ]

-- Requests

requestConvertRecoveryPointToSnapshot :: ConvertRecoveryPointToSnapshot -> TestTree
requestConvertRecoveryPointToSnapshot =
  req
    "ConvertRecoveryPointToSnapshot"
    "fixture/ConvertRecoveryPointToSnapshot.yaml"

requestCreateEndpointAccess :: CreateEndpointAccess -> TestTree
requestCreateEndpointAccess =
  req
    "CreateEndpointAccess"
    "fixture/CreateEndpointAccess.yaml"

requestCreateNamespace :: CreateNamespace -> TestTree
requestCreateNamespace =
  req
    "CreateNamespace"
    "fixture/CreateNamespace.yaml"

requestCreateSnapshot :: CreateSnapshot -> TestTree
requestCreateSnapshot =
  req
    "CreateSnapshot"
    "fixture/CreateSnapshot.yaml"

requestCreateUsageLimit :: CreateUsageLimit -> TestTree
requestCreateUsageLimit =
  req
    "CreateUsageLimit"
    "fixture/CreateUsageLimit.yaml"

requestCreateWorkgroup :: CreateWorkgroup -> TestTree
requestCreateWorkgroup =
  req
    "CreateWorkgroup"
    "fixture/CreateWorkgroup.yaml"

requestDeleteEndpointAccess :: DeleteEndpointAccess -> TestTree
requestDeleteEndpointAccess =
  req
    "DeleteEndpointAccess"
    "fixture/DeleteEndpointAccess.yaml"

requestDeleteNamespace :: DeleteNamespace -> TestTree
requestDeleteNamespace =
  req
    "DeleteNamespace"
    "fixture/DeleteNamespace.yaml"

requestDeleteResourcePolicy :: DeleteResourcePolicy -> TestTree
requestDeleteResourcePolicy =
  req
    "DeleteResourcePolicy"
    "fixture/DeleteResourcePolicy.yaml"

requestDeleteSnapshot :: DeleteSnapshot -> TestTree
requestDeleteSnapshot =
  req
    "DeleteSnapshot"
    "fixture/DeleteSnapshot.yaml"

requestDeleteUsageLimit :: DeleteUsageLimit -> TestTree
requestDeleteUsageLimit =
  req
    "DeleteUsageLimit"
    "fixture/DeleteUsageLimit.yaml"

requestDeleteWorkgroup :: DeleteWorkgroup -> TestTree
requestDeleteWorkgroup =
  req
    "DeleteWorkgroup"
    "fixture/DeleteWorkgroup.yaml"

requestGetCredentials :: GetCredentials -> TestTree
requestGetCredentials =
  req
    "GetCredentials"
    "fixture/GetCredentials.yaml"

requestGetEndpointAccess :: GetEndpointAccess -> TestTree
requestGetEndpointAccess =
  req
    "GetEndpointAccess"
    "fixture/GetEndpointAccess.yaml"

requestGetNamespace :: GetNamespace -> TestTree
requestGetNamespace =
  req
    "GetNamespace"
    "fixture/GetNamespace.yaml"

requestGetRecoveryPoint :: GetRecoveryPoint -> TestTree
requestGetRecoveryPoint =
  req
    "GetRecoveryPoint"
    "fixture/GetRecoveryPoint.yaml"

requestGetResourcePolicy :: GetResourcePolicy -> TestTree
requestGetResourcePolicy =
  req
    "GetResourcePolicy"
    "fixture/GetResourcePolicy.yaml"

requestGetSnapshot :: GetSnapshot -> TestTree
requestGetSnapshot =
  req
    "GetSnapshot"
    "fixture/GetSnapshot.yaml"

requestGetTableRestoreStatus :: GetTableRestoreStatus -> TestTree
requestGetTableRestoreStatus =
  req
    "GetTableRestoreStatus"
    "fixture/GetTableRestoreStatus.yaml"

requestGetUsageLimit :: GetUsageLimit -> TestTree
requestGetUsageLimit =
  req
    "GetUsageLimit"
    "fixture/GetUsageLimit.yaml"

requestGetWorkgroup :: GetWorkgroup -> TestTree
requestGetWorkgroup =
  req
    "GetWorkgroup"
    "fixture/GetWorkgroup.yaml"

requestListEndpointAccess :: ListEndpointAccess -> TestTree
requestListEndpointAccess =
  req
    "ListEndpointAccess"
    "fixture/ListEndpointAccess.yaml"

requestListNamespaces :: ListNamespaces -> TestTree
requestListNamespaces =
  req
    "ListNamespaces"
    "fixture/ListNamespaces.yaml"

requestListRecoveryPoints :: ListRecoveryPoints -> TestTree
requestListRecoveryPoints =
  req
    "ListRecoveryPoints"
    "fixture/ListRecoveryPoints.yaml"

requestListSnapshots :: ListSnapshots -> TestTree
requestListSnapshots =
  req
    "ListSnapshots"
    "fixture/ListSnapshots.yaml"

requestListTableRestoreStatus :: ListTableRestoreStatus -> TestTree
requestListTableRestoreStatus =
  req
    "ListTableRestoreStatus"
    "fixture/ListTableRestoreStatus.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestListUsageLimits :: ListUsageLimits -> TestTree
requestListUsageLimits =
  req
    "ListUsageLimits"
    "fixture/ListUsageLimits.yaml"

requestListWorkgroups :: ListWorkgroups -> TestTree
requestListWorkgroups =
  req
    "ListWorkgroups"
    "fixture/ListWorkgroups.yaml"

requestPutResourcePolicy :: PutResourcePolicy -> TestTree
requestPutResourcePolicy =
  req
    "PutResourcePolicy"
    "fixture/PutResourcePolicy.yaml"

requestRestoreFromRecoveryPoint :: RestoreFromRecoveryPoint -> TestTree
requestRestoreFromRecoveryPoint =
  req
    "RestoreFromRecoveryPoint"
    "fixture/RestoreFromRecoveryPoint.yaml"

requestRestoreFromSnapshot :: RestoreFromSnapshot -> TestTree
requestRestoreFromSnapshot =
  req
    "RestoreFromSnapshot"
    "fixture/RestoreFromSnapshot.yaml"

requestRestoreTableFromSnapshot :: RestoreTableFromSnapshot -> TestTree
requestRestoreTableFromSnapshot =
  req
    "RestoreTableFromSnapshot"
    "fixture/RestoreTableFromSnapshot.yaml"

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

requestUpdateEndpointAccess :: UpdateEndpointAccess -> TestTree
requestUpdateEndpointAccess =
  req
    "UpdateEndpointAccess"
    "fixture/UpdateEndpointAccess.yaml"

requestUpdateNamespace :: UpdateNamespace -> TestTree
requestUpdateNamespace =
  req
    "UpdateNamespace"
    "fixture/UpdateNamespace.yaml"

requestUpdateSnapshot :: UpdateSnapshot -> TestTree
requestUpdateSnapshot =
  req
    "UpdateSnapshot"
    "fixture/UpdateSnapshot.yaml"

requestUpdateUsageLimit :: UpdateUsageLimit -> TestTree
requestUpdateUsageLimit =
  req
    "UpdateUsageLimit"
    "fixture/UpdateUsageLimit.yaml"

requestUpdateWorkgroup :: UpdateWorkgroup -> TestTree
requestUpdateWorkgroup =
  req
    "UpdateWorkgroup"
    "fixture/UpdateWorkgroup.yaml"

-- Responses

responseConvertRecoveryPointToSnapshot :: ConvertRecoveryPointToSnapshotResponse -> TestTree
responseConvertRecoveryPointToSnapshot =
  res
    "ConvertRecoveryPointToSnapshotResponse"
    "fixture/ConvertRecoveryPointToSnapshotResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ConvertRecoveryPointToSnapshot)

responseCreateEndpointAccess :: CreateEndpointAccessResponse -> TestTree
responseCreateEndpointAccess =
  res
    "CreateEndpointAccessResponse"
    "fixture/CreateEndpointAccessResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateEndpointAccess)

responseCreateNamespace :: CreateNamespaceResponse -> TestTree
responseCreateNamespace =
  res
    "CreateNamespaceResponse"
    "fixture/CreateNamespaceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateNamespace)

responseCreateSnapshot :: CreateSnapshotResponse -> TestTree
responseCreateSnapshot =
  res
    "CreateSnapshotResponse"
    "fixture/CreateSnapshotResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateSnapshot)

responseCreateUsageLimit :: CreateUsageLimitResponse -> TestTree
responseCreateUsageLimit =
  res
    "CreateUsageLimitResponse"
    "fixture/CreateUsageLimitResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateUsageLimit)

responseCreateWorkgroup :: CreateWorkgroupResponse -> TestTree
responseCreateWorkgroup =
  res
    "CreateWorkgroupResponse"
    "fixture/CreateWorkgroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateWorkgroup)

responseDeleteEndpointAccess :: DeleteEndpointAccessResponse -> TestTree
responseDeleteEndpointAccess =
  res
    "DeleteEndpointAccessResponse"
    "fixture/DeleteEndpointAccessResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteEndpointAccess)

responseDeleteNamespace :: DeleteNamespaceResponse -> TestTree
responseDeleteNamespace =
  res
    "DeleteNamespaceResponse"
    "fixture/DeleteNamespaceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteNamespace)

responseDeleteResourcePolicy :: DeleteResourcePolicyResponse -> TestTree
responseDeleteResourcePolicy =
  res
    "DeleteResourcePolicyResponse"
    "fixture/DeleteResourcePolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteResourcePolicy)

responseDeleteSnapshot :: DeleteSnapshotResponse -> TestTree
responseDeleteSnapshot =
  res
    "DeleteSnapshotResponse"
    "fixture/DeleteSnapshotResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteSnapshot)

responseDeleteUsageLimit :: DeleteUsageLimitResponse -> TestTree
responseDeleteUsageLimit =
  res
    "DeleteUsageLimitResponse"
    "fixture/DeleteUsageLimitResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteUsageLimit)

responseDeleteWorkgroup :: DeleteWorkgroupResponse -> TestTree
responseDeleteWorkgroup =
  res
    "DeleteWorkgroupResponse"
    "fixture/DeleteWorkgroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteWorkgroup)

responseGetCredentials :: GetCredentialsResponse -> TestTree
responseGetCredentials =
  res
    "GetCredentialsResponse"
    "fixture/GetCredentialsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetCredentials)

responseGetEndpointAccess :: GetEndpointAccessResponse -> TestTree
responseGetEndpointAccess =
  res
    "GetEndpointAccessResponse"
    "fixture/GetEndpointAccessResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetEndpointAccess)

responseGetNamespace :: GetNamespaceResponse -> TestTree
responseGetNamespace =
  res
    "GetNamespaceResponse"
    "fixture/GetNamespaceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetNamespace)

responseGetRecoveryPoint :: GetRecoveryPointResponse -> TestTree
responseGetRecoveryPoint =
  res
    "GetRecoveryPointResponse"
    "fixture/GetRecoveryPointResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetRecoveryPoint)

responseGetResourcePolicy :: GetResourcePolicyResponse -> TestTree
responseGetResourcePolicy =
  res
    "GetResourcePolicyResponse"
    "fixture/GetResourcePolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetResourcePolicy)

responseGetSnapshot :: GetSnapshotResponse -> TestTree
responseGetSnapshot =
  res
    "GetSnapshotResponse"
    "fixture/GetSnapshotResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetSnapshot)

responseGetTableRestoreStatus :: GetTableRestoreStatusResponse -> TestTree
responseGetTableRestoreStatus =
  res
    "GetTableRestoreStatusResponse"
    "fixture/GetTableRestoreStatusResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetTableRestoreStatus)

responseGetUsageLimit :: GetUsageLimitResponse -> TestTree
responseGetUsageLimit =
  res
    "GetUsageLimitResponse"
    "fixture/GetUsageLimitResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetUsageLimit)

responseGetWorkgroup :: GetWorkgroupResponse -> TestTree
responseGetWorkgroup =
  res
    "GetWorkgroupResponse"
    "fixture/GetWorkgroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetWorkgroup)

responseListEndpointAccess :: ListEndpointAccessResponse -> TestTree
responseListEndpointAccess =
  res
    "ListEndpointAccessResponse"
    "fixture/ListEndpointAccessResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListEndpointAccess)

responseListNamespaces :: ListNamespacesResponse -> TestTree
responseListNamespaces =
  res
    "ListNamespacesResponse"
    "fixture/ListNamespacesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListNamespaces)

responseListRecoveryPoints :: ListRecoveryPointsResponse -> TestTree
responseListRecoveryPoints =
  res
    "ListRecoveryPointsResponse"
    "fixture/ListRecoveryPointsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListRecoveryPoints)

responseListSnapshots :: ListSnapshotsResponse -> TestTree
responseListSnapshots =
  res
    "ListSnapshotsResponse"
    "fixture/ListSnapshotsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListSnapshots)

responseListTableRestoreStatus :: ListTableRestoreStatusResponse -> TestTree
responseListTableRestoreStatus =
  res
    "ListTableRestoreStatusResponse"
    "fixture/ListTableRestoreStatusResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTableRestoreStatus)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseListUsageLimits :: ListUsageLimitsResponse -> TestTree
responseListUsageLimits =
  res
    "ListUsageLimitsResponse"
    "fixture/ListUsageLimitsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListUsageLimits)

responseListWorkgroups :: ListWorkgroupsResponse -> TestTree
responseListWorkgroups =
  res
    "ListWorkgroupsResponse"
    "fixture/ListWorkgroupsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListWorkgroups)

responsePutResourcePolicy :: PutResourcePolicyResponse -> TestTree
responsePutResourcePolicy =
  res
    "PutResourcePolicyResponse"
    "fixture/PutResourcePolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutResourcePolicy)

responseRestoreFromRecoveryPoint :: RestoreFromRecoveryPointResponse -> TestTree
responseRestoreFromRecoveryPoint =
  res
    "RestoreFromRecoveryPointResponse"
    "fixture/RestoreFromRecoveryPointResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RestoreFromRecoveryPoint)

responseRestoreFromSnapshot :: RestoreFromSnapshotResponse -> TestTree
responseRestoreFromSnapshot =
  res
    "RestoreFromSnapshotResponse"
    "fixture/RestoreFromSnapshotResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RestoreFromSnapshot)

responseRestoreTableFromSnapshot :: RestoreTableFromSnapshotResponse -> TestTree
responseRestoreTableFromSnapshot =
  res
    "RestoreTableFromSnapshotResponse"
    "fixture/RestoreTableFromSnapshotResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RestoreTableFromSnapshot)

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

responseUpdateEndpointAccess :: UpdateEndpointAccessResponse -> TestTree
responseUpdateEndpointAccess =
  res
    "UpdateEndpointAccessResponse"
    "fixture/UpdateEndpointAccessResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateEndpointAccess)

responseUpdateNamespace :: UpdateNamespaceResponse -> TestTree
responseUpdateNamespace =
  res
    "UpdateNamespaceResponse"
    "fixture/UpdateNamespaceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateNamespace)

responseUpdateSnapshot :: UpdateSnapshotResponse -> TestTree
responseUpdateSnapshot =
  res
    "UpdateSnapshotResponse"
    "fixture/UpdateSnapshotResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateSnapshot)

responseUpdateUsageLimit :: UpdateUsageLimitResponse -> TestTree
responseUpdateUsageLimit =
  res
    "UpdateUsageLimitResponse"
    "fixture/UpdateUsageLimitResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateUsageLimit)

responseUpdateWorkgroup :: UpdateWorkgroupResponse -> TestTree
responseUpdateWorkgroup =
  res
    "UpdateWorkgroupResponse"
    "fixture/UpdateWorkgroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateWorkgroup)
