{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.DocumentDB
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.DocumentDB where

import Amazonka.DocumentDB
import qualified Data.Proxy as Proxy
import Test.Amazonka.DocumentDB.Internal
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
--         [ requestAddSourceIdentifierToSubscription $
--             newAddSourceIdentifierToSubscription
--
--         , requestAddTagsToResource $
--             newAddTagsToResource
--
--         , requestApplyPendingMaintenanceAction $
--             newApplyPendingMaintenanceAction
--
--         , requestCopyDBClusterParameterGroup $
--             newCopyDBClusterParameterGroup
--
--         , requestCopyDBClusterSnapshot $
--             newCopyDBClusterSnapshot
--
--         , requestCreateDBCluster $
--             newCreateDBCluster
--
--         , requestCreateDBClusterParameterGroup $
--             newCreateDBClusterParameterGroup
--
--         , requestCreateDBClusterSnapshot $
--             newCreateDBClusterSnapshot
--
--         , requestCreateDBInstance $
--             newCreateDBInstance
--
--         , requestCreateDBSubnetGroup $
--             newCreateDBSubnetGroup
--
--         , requestCreateEventSubscription $
--             newCreateEventSubscription
--
--         , requestCreateGlobalCluster $
--             newCreateGlobalCluster
--
--         , requestDeleteDBCluster $
--             newDeleteDBCluster
--
--         , requestDeleteDBClusterParameterGroup $
--             newDeleteDBClusterParameterGroup
--
--         , requestDeleteDBClusterSnapshot $
--             newDeleteDBClusterSnapshot
--
--         , requestDeleteDBInstance $
--             newDeleteDBInstance
--
--         , requestDeleteDBSubnetGroup $
--             newDeleteDBSubnetGroup
--
--         , requestDeleteEventSubscription $
--             newDeleteEventSubscription
--
--         , requestDeleteGlobalCluster $
--             newDeleteGlobalCluster
--
--         , requestDescribeCertificates $
--             newDescribeCertificates
--
--         , requestDescribeDBClusterParameterGroups $
--             newDescribeDBClusterParameterGroups
--
--         , requestDescribeDBClusterParameters $
--             newDescribeDBClusterParameters
--
--         , requestDescribeDBClusterSnapshotAttributes $
--             newDescribeDBClusterSnapshotAttributes
--
--         , requestDescribeDBClusterSnapshots $
--             newDescribeDBClusterSnapshots
--
--         , requestDescribeDBClusters $
--             newDescribeDBClusters
--
--         , requestDescribeDBEngineVersions $
--             newDescribeDBEngineVersions
--
--         , requestDescribeDBInstances $
--             newDescribeDBInstances
--
--         , requestDescribeDBSubnetGroups $
--             newDescribeDBSubnetGroups
--
--         , requestDescribeEngineDefaultClusterParameters $
--             newDescribeEngineDefaultClusterParameters
--
--         , requestDescribeEventCategories $
--             newDescribeEventCategories
--
--         , requestDescribeEventSubscriptions $
--             newDescribeEventSubscriptions
--
--         , requestDescribeEvents $
--             newDescribeEvents
--
--         , requestDescribeGlobalClusters $
--             newDescribeGlobalClusters
--
--         , requestDescribeOrderableDBInstanceOptions $
--             newDescribeOrderableDBInstanceOptions
--
--         , requestDescribePendingMaintenanceActions $
--             newDescribePendingMaintenanceActions
--
--         , requestFailoverDBCluster $
--             newFailoverDBCluster
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestModifyDBCluster $
--             newModifyDBCluster
--
--         , requestModifyDBClusterParameterGroup $
--             newModifyDBClusterParameterGroup
--
--         , requestModifyDBClusterSnapshotAttribute $
--             newModifyDBClusterSnapshotAttribute
--
--         , requestModifyDBInstance $
--             newModifyDBInstance
--
--         , requestModifyDBSubnetGroup $
--             newModifyDBSubnetGroup
--
--         , requestModifyEventSubscription $
--             newModifyEventSubscription
--
--         , requestModifyGlobalCluster $
--             newModifyGlobalCluster
--
--         , requestRebootDBInstance $
--             newRebootDBInstance
--
--         , requestRemoveFromGlobalCluster $
--             newRemoveFromGlobalCluster
--
--         , requestRemoveSourceIdentifierFromSubscription $
--             newRemoveSourceIdentifierFromSubscription
--
--         , requestRemoveTagsFromResource $
--             newRemoveTagsFromResource
--
--         , requestResetDBClusterParameterGroup $
--             newResetDBClusterParameterGroup
--
--         , requestRestoreDBClusterFromSnapshot $
--             newRestoreDBClusterFromSnapshot
--
--         , requestRestoreDBClusterToPointInTime $
--             newRestoreDBClusterToPointInTime
--
--         , requestStartDBCluster $
--             newStartDBCluster
--
--         , requestStopDBCluster $
--             newStopDBCluster
--
--           ]

--     , testGroup "response"
--         [ responseAddSourceIdentifierToSubscription $
--             newAddSourceIdentifierToSubscriptionResponse
--
--         , responseAddTagsToResource $
--             newAddTagsToResourceResponse
--
--         , responseApplyPendingMaintenanceAction $
--             newApplyPendingMaintenanceActionResponse
--
--         , responseCopyDBClusterParameterGroup $
--             newCopyDBClusterParameterGroupResponse
--
--         , responseCopyDBClusterSnapshot $
--             newCopyDBClusterSnapshotResponse
--
--         , responseCreateDBCluster $
--             newCreateDBClusterResponse
--
--         , responseCreateDBClusterParameterGroup $
--             newCreateDBClusterParameterGroupResponse
--
--         , responseCreateDBClusterSnapshot $
--             newCreateDBClusterSnapshotResponse
--
--         , responseCreateDBInstance $
--             newCreateDBInstanceResponse
--
--         , responseCreateDBSubnetGroup $
--             newCreateDBSubnetGroupResponse
--
--         , responseCreateEventSubscription $
--             newCreateEventSubscriptionResponse
--
--         , responseCreateGlobalCluster $
--             newCreateGlobalClusterResponse
--
--         , responseDeleteDBCluster $
--             newDeleteDBClusterResponse
--
--         , responseDeleteDBClusterParameterGroup $
--             newDeleteDBClusterParameterGroupResponse
--
--         , responseDeleteDBClusterSnapshot $
--             newDeleteDBClusterSnapshotResponse
--
--         , responseDeleteDBInstance $
--             newDeleteDBInstanceResponse
--
--         , responseDeleteDBSubnetGroup $
--             newDeleteDBSubnetGroupResponse
--
--         , responseDeleteEventSubscription $
--             newDeleteEventSubscriptionResponse
--
--         , responseDeleteGlobalCluster $
--             newDeleteGlobalClusterResponse
--
--         , responseDescribeCertificates $
--             newDescribeCertificatesResponse
--
--         , responseDescribeDBClusterParameterGroups $
--             newDescribeDBClusterParameterGroupsResponse
--
--         , responseDescribeDBClusterParameters $
--             newDescribeDBClusterParametersResponse
--
--         , responseDescribeDBClusterSnapshotAttributes $
--             newDescribeDBClusterSnapshotAttributesResponse
--
--         , responseDescribeDBClusterSnapshots $
--             newDescribeDBClusterSnapshotsResponse
--
--         , responseDescribeDBClusters $
--             newDescribeDBClustersResponse
--
--         , responseDescribeDBEngineVersions $
--             newDescribeDBEngineVersionsResponse
--
--         , responseDescribeDBInstances $
--             newDescribeDBInstancesResponse
--
--         , responseDescribeDBSubnetGroups $
--             newDescribeDBSubnetGroupsResponse
--
--         , responseDescribeEngineDefaultClusterParameters $
--             newDescribeEngineDefaultClusterParametersResponse
--
--         , responseDescribeEventCategories $
--             newDescribeEventCategoriesResponse
--
--         , responseDescribeEventSubscriptions $
--             newDescribeEventSubscriptionsResponse
--
--         , responseDescribeEvents $
--             newDescribeEventsResponse
--
--         , responseDescribeGlobalClusters $
--             newDescribeGlobalClustersResponse
--
--         , responseDescribeOrderableDBInstanceOptions $
--             newDescribeOrderableDBInstanceOptionsResponse
--
--         , responseDescribePendingMaintenanceActions $
--             newDescribePendingMaintenanceActionsResponse
--
--         , responseFailoverDBCluster $
--             newFailoverDBClusterResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseModifyDBCluster $
--             newModifyDBClusterResponse
--
--         , responseModifyDBClusterParameterGroup $
--             newDBClusterParameterGroupNameMessage
--
--         , responseModifyDBClusterSnapshotAttribute $
--             newModifyDBClusterSnapshotAttributeResponse
--
--         , responseModifyDBInstance $
--             newModifyDBInstanceResponse
--
--         , responseModifyDBSubnetGroup $
--             newModifyDBSubnetGroupResponse
--
--         , responseModifyEventSubscription $
--             newModifyEventSubscriptionResponse
--
--         , responseModifyGlobalCluster $
--             newModifyGlobalClusterResponse
--
--         , responseRebootDBInstance $
--             newRebootDBInstanceResponse
--
--         , responseRemoveFromGlobalCluster $
--             newRemoveFromGlobalClusterResponse
--
--         , responseRemoveSourceIdentifierFromSubscription $
--             newRemoveSourceIdentifierFromSubscriptionResponse
--
--         , responseRemoveTagsFromResource $
--             newRemoveTagsFromResourceResponse
--
--         , responseResetDBClusterParameterGroup $
--             newDBClusterParameterGroupNameMessage
--
--         , responseRestoreDBClusterFromSnapshot $
--             newRestoreDBClusterFromSnapshotResponse
--
--         , responseRestoreDBClusterToPointInTime $
--             newRestoreDBClusterToPointInTimeResponse
--
--         , responseStartDBCluster $
--             newStartDBClusterResponse
--
--         , responseStopDBCluster $
--             newStopDBClusterResponse
--
--           ]
--     ]

-- Requests

requestAddSourceIdentifierToSubscription :: AddSourceIdentifierToSubscription -> TestTree
requestAddSourceIdentifierToSubscription =
  req
    "AddSourceIdentifierToSubscription"
    "fixture/AddSourceIdentifierToSubscription.yaml"

requestAddTagsToResource :: AddTagsToResource -> TestTree
requestAddTagsToResource =
  req
    "AddTagsToResource"
    "fixture/AddTagsToResource.yaml"

requestApplyPendingMaintenanceAction :: ApplyPendingMaintenanceAction -> TestTree
requestApplyPendingMaintenanceAction =
  req
    "ApplyPendingMaintenanceAction"
    "fixture/ApplyPendingMaintenanceAction.yaml"

requestCopyDBClusterParameterGroup :: CopyDBClusterParameterGroup -> TestTree
requestCopyDBClusterParameterGroup =
  req
    "CopyDBClusterParameterGroup"
    "fixture/CopyDBClusterParameterGroup.yaml"

requestCopyDBClusterSnapshot :: CopyDBClusterSnapshot -> TestTree
requestCopyDBClusterSnapshot =
  req
    "CopyDBClusterSnapshot"
    "fixture/CopyDBClusterSnapshot.yaml"

requestCreateDBCluster :: CreateDBCluster -> TestTree
requestCreateDBCluster =
  req
    "CreateDBCluster"
    "fixture/CreateDBCluster.yaml"

requestCreateDBClusterParameterGroup :: CreateDBClusterParameterGroup -> TestTree
requestCreateDBClusterParameterGroup =
  req
    "CreateDBClusterParameterGroup"
    "fixture/CreateDBClusterParameterGroup.yaml"

requestCreateDBClusterSnapshot :: CreateDBClusterSnapshot -> TestTree
requestCreateDBClusterSnapshot =
  req
    "CreateDBClusterSnapshot"
    "fixture/CreateDBClusterSnapshot.yaml"

requestCreateDBInstance :: CreateDBInstance -> TestTree
requestCreateDBInstance =
  req
    "CreateDBInstance"
    "fixture/CreateDBInstance.yaml"

requestCreateDBSubnetGroup :: CreateDBSubnetGroup -> TestTree
requestCreateDBSubnetGroup =
  req
    "CreateDBSubnetGroup"
    "fixture/CreateDBSubnetGroup.yaml"

requestCreateEventSubscription :: CreateEventSubscription -> TestTree
requestCreateEventSubscription =
  req
    "CreateEventSubscription"
    "fixture/CreateEventSubscription.yaml"

requestCreateGlobalCluster :: CreateGlobalCluster -> TestTree
requestCreateGlobalCluster =
  req
    "CreateGlobalCluster"
    "fixture/CreateGlobalCluster.yaml"

requestDeleteDBCluster :: DeleteDBCluster -> TestTree
requestDeleteDBCluster =
  req
    "DeleteDBCluster"
    "fixture/DeleteDBCluster.yaml"

requestDeleteDBClusterParameterGroup :: DeleteDBClusterParameterGroup -> TestTree
requestDeleteDBClusterParameterGroup =
  req
    "DeleteDBClusterParameterGroup"
    "fixture/DeleteDBClusterParameterGroup.yaml"

requestDeleteDBClusterSnapshot :: DeleteDBClusterSnapshot -> TestTree
requestDeleteDBClusterSnapshot =
  req
    "DeleteDBClusterSnapshot"
    "fixture/DeleteDBClusterSnapshot.yaml"

requestDeleteDBInstance :: DeleteDBInstance -> TestTree
requestDeleteDBInstance =
  req
    "DeleteDBInstance"
    "fixture/DeleteDBInstance.yaml"

requestDeleteDBSubnetGroup :: DeleteDBSubnetGroup -> TestTree
requestDeleteDBSubnetGroup =
  req
    "DeleteDBSubnetGroup"
    "fixture/DeleteDBSubnetGroup.yaml"

requestDeleteEventSubscription :: DeleteEventSubscription -> TestTree
requestDeleteEventSubscription =
  req
    "DeleteEventSubscription"
    "fixture/DeleteEventSubscription.yaml"

requestDeleteGlobalCluster :: DeleteGlobalCluster -> TestTree
requestDeleteGlobalCluster =
  req
    "DeleteGlobalCluster"
    "fixture/DeleteGlobalCluster.yaml"

requestDescribeCertificates :: DescribeCertificates -> TestTree
requestDescribeCertificates =
  req
    "DescribeCertificates"
    "fixture/DescribeCertificates.yaml"

requestDescribeDBClusterParameterGroups :: DescribeDBClusterParameterGroups -> TestTree
requestDescribeDBClusterParameterGroups =
  req
    "DescribeDBClusterParameterGroups"
    "fixture/DescribeDBClusterParameterGroups.yaml"

requestDescribeDBClusterParameters :: DescribeDBClusterParameters -> TestTree
requestDescribeDBClusterParameters =
  req
    "DescribeDBClusterParameters"
    "fixture/DescribeDBClusterParameters.yaml"

requestDescribeDBClusterSnapshotAttributes :: DescribeDBClusterSnapshotAttributes -> TestTree
requestDescribeDBClusterSnapshotAttributes =
  req
    "DescribeDBClusterSnapshotAttributes"
    "fixture/DescribeDBClusterSnapshotAttributes.yaml"

requestDescribeDBClusterSnapshots :: DescribeDBClusterSnapshots -> TestTree
requestDescribeDBClusterSnapshots =
  req
    "DescribeDBClusterSnapshots"
    "fixture/DescribeDBClusterSnapshots.yaml"

requestDescribeDBClusters :: DescribeDBClusters -> TestTree
requestDescribeDBClusters =
  req
    "DescribeDBClusters"
    "fixture/DescribeDBClusters.yaml"

requestDescribeDBEngineVersions :: DescribeDBEngineVersions -> TestTree
requestDescribeDBEngineVersions =
  req
    "DescribeDBEngineVersions"
    "fixture/DescribeDBEngineVersions.yaml"

requestDescribeDBInstances :: DescribeDBInstances -> TestTree
requestDescribeDBInstances =
  req
    "DescribeDBInstances"
    "fixture/DescribeDBInstances.yaml"

requestDescribeDBSubnetGroups :: DescribeDBSubnetGroups -> TestTree
requestDescribeDBSubnetGroups =
  req
    "DescribeDBSubnetGroups"
    "fixture/DescribeDBSubnetGroups.yaml"

requestDescribeEngineDefaultClusterParameters :: DescribeEngineDefaultClusterParameters -> TestTree
requestDescribeEngineDefaultClusterParameters =
  req
    "DescribeEngineDefaultClusterParameters"
    "fixture/DescribeEngineDefaultClusterParameters.yaml"

requestDescribeEventCategories :: DescribeEventCategories -> TestTree
requestDescribeEventCategories =
  req
    "DescribeEventCategories"
    "fixture/DescribeEventCategories.yaml"

requestDescribeEventSubscriptions :: DescribeEventSubscriptions -> TestTree
requestDescribeEventSubscriptions =
  req
    "DescribeEventSubscriptions"
    "fixture/DescribeEventSubscriptions.yaml"

requestDescribeEvents :: DescribeEvents -> TestTree
requestDescribeEvents =
  req
    "DescribeEvents"
    "fixture/DescribeEvents.yaml"

requestDescribeGlobalClusters :: DescribeGlobalClusters -> TestTree
requestDescribeGlobalClusters =
  req
    "DescribeGlobalClusters"
    "fixture/DescribeGlobalClusters.yaml"

requestDescribeOrderableDBInstanceOptions :: DescribeOrderableDBInstanceOptions -> TestTree
requestDescribeOrderableDBInstanceOptions =
  req
    "DescribeOrderableDBInstanceOptions"
    "fixture/DescribeOrderableDBInstanceOptions.yaml"

requestDescribePendingMaintenanceActions :: DescribePendingMaintenanceActions -> TestTree
requestDescribePendingMaintenanceActions =
  req
    "DescribePendingMaintenanceActions"
    "fixture/DescribePendingMaintenanceActions.yaml"

requestFailoverDBCluster :: FailoverDBCluster -> TestTree
requestFailoverDBCluster =
  req
    "FailoverDBCluster"
    "fixture/FailoverDBCluster.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestModifyDBCluster :: ModifyDBCluster -> TestTree
requestModifyDBCluster =
  req
    "ModifyDBCluster"
    "fixture/ModifyDBCluster.yaml"

requestModifyDBClusterParameterGroup :: ModifyDBClusterParameterGroup -> TestTree
requestModifyDBClusterParameterGroup =
  req
    "ModifyDBClusterParameterGroup"
    "fixture/ModifyDBClusterParameterGroup.yaml"

requestModifyDBClusterSnapshotAttribute :: ModifyDBClusterSnapshotAttribute -> TestTree
requestModifyDBClusterSnapshotAttribute =
  req
    "ModifyDBClusterSnapshotAttribute"
    "fixture/ModifyDBClusterSnapshotAttribute.yaml"

requestModifyDBInstance :: ModifyDBInstance -> TestTree
requestModifyDBInstance =
  req
    "ModifyDBInstance"
    "fixture/ModifyDBInstance.yaml"

requestModifyDBSubnetGroup :: ModifyDBSubnetGroup -> TestTree
requestModifyDBSubnetGroup =
  req
    "ModifyDBSubnetGroup"
    "fixture/ModifyDBSubnetGroup.yaml"

requestModifyEventSubscription :: ModifyEventSubscription -> TestTree
requestModifyEventSubscription =
  req
    "ModifyEventSubscription"
    "fixture/ModifyEventSubscription.yaml"

requestModifyGlobalCluster :: ModifyGlobalCluster -> TestTree
requestModifyGlobalCluster =
  req
    "ModifyGlobalCluster"
    "fixture/ModifyGlobalCluster.yaml"

requestRebootDBInstance :: RebootDBInstance -> TestTree
requestRebootDBInstance =
  req
    "RebootDBInstance"
    "fixture/RebootDBInstance.yaml"

requestRemoveFromGlobalCluster :: RemoveFromGlobalCluster -> TestTree
requestRemoveFromGlobalCluster =
  req
    "RemoveFromGlobalCluster"
    "fixture/RemoveFromGlobalCluster.yaml"

requestRemoveSourceIdentifierFromSubscription :: RemoveSourceIdentifierFromSubscription -> TestTree
requestRemoveSourceIdentifierFromSubscription =
  req
    "RemoveSourceIdentifierFromSubscription"
    "fixture/RemoveSourceIdentifierFromSubscription.yaml"

requestRemoveTagsFromResource :: RemoveTagsFromResource -> TestTree
requestRemoveTagsFromResource =
  req
    "RemoveTagsFromResource"
    "fixture/RemoveTagsFromResource.yaml"

requestResetDBClusterParameterGroup :: ResetDBClusterParameterGroup -> TestTree
requestResetDBClusterParameterGroup =
  req
    "ResetDBClusterParameterGroup"
    "fixture/ResetDBClusterParameterGroup.yaml"

requestRestoreDBClusterFromSnapshot :: RestoreDBClusterFromSnapshot -> TestTree
requestRestoreDBClusterFromSnapshot =
  req
    "RestoreDBClusterFromSnapshot"
    "fixture/RestoreDBClusterFromSnapshot.yaml"

requestRestoreDBClusterToPointInTime :: RestoreDBClusterToPointInTime -> TestTree
requestRestoreDBClusterToPointInTime =
  req
    "RestoreDBClusterToPointInTime"
    "fixture/RestoreDBClusterToPointInTime.yaml"

requestStartDBCluster :: StartDBCluster -> TestTree
requestStartDBCluster =
  req
    "StartDBCluster"
    "fixture/StartDBCluster.yaml"

requestStopDBCluster :: StopDBCluster -> TestTree
requestStopDBCluster =
  req
    "StopDBCluster"
    "fixture/StopDBCluster.yaml"

-- Responses

responseAddSourceIdentifierToSubscription :: AddSourceIdentifierToSubscriptionResponse -> TestTree
responseAddSourceIdentifierToSubscription =
  res
    "AddSourceIdentifierToSubscriptionResponse"
    "fixture/AddSourceIdentifierToSubscriptionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AddSourceIdentifierToSubscription)

responseAddTagsToResource :: AddTagsToResourceResponse -> TestTree
responseAddTagsToResource =
  res
    "AddTagsToResourceResponse"
    "fixture/AddTagsToResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AddTagsToResource)

responseApplyPendingMaintenanceAction :: ApplyPendingMaintenanceActionResponse -> TestTree
responseApplyPendingMaintenanceAction =
  res
    "ApplyPendingMaintenanceActionResponse"
    "fixture/ApplyPendingMaintenanceActionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ApplyPendingMaintenanceAction)

responseCopyDBClusterParameterGroup :: CopyDBClusterParameterGroupResponse -> TestTree
responseCopyDBClusterParameterGroup =
  res
    "CopyDBClusterParameterGroupResponse"
    "fixture/CopyDBClusterParameterGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CopyDBClusterParameterGroup)

responseCopyDBClusterSnapshot :: CopyDBClusterSnapshotResponse -> TestTree
responseCopyDBClusterSnapshot =
  res
    "CopyDBClusterSnapshotResponse"
    "fixture/CopyDBClusterSnapshotResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CopyDBClusterSnapshot)

responseCreateDBCluster :: CreateDBClusterResponse -> TestTree
responseCreateDBCluster =
  res
    "CreateDBClusterResponse"
    "fixture/CreateDBClusterResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateDBCluster)

responseCreateDBClusterParameterGroup :: CreateDBClusterParameterGroupResponse -> TestTree
responseCreateDBClusterParameterGroup =
  res
    "CreateDBClusterParameterGroupResponse"
    "fixture/CreateDBClusterParameterGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateDBClusterParameterGroup)

responseCreateDBClusterSnapshot :: CreateDBClusterSnapshotResponse -> TestTree
responseCreateDBClusterSnapshot =
  res
    "CreateDBClusterSnapshotResponse"
    "fixture/CreateDBClusterSnapshotResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateDBClusterSnapshot)

responseCreateDBInstance :: CreateDBInstanceResponse -> TestTree
responseCreateDBInstance =
  res
    "CreateDBInstanceResponse"
    "fixture/CreateDBInstanceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateDBInstance)

responseCreateDBSubnetGroup :: CreateDBSubnetGroupResponse -> TestTree
responseCreateDBSubnetGroup =
  res
    "CreateDBSubnetGroupResponse"
    "fixture/CreateDBSubnetGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateDBSubnetGroup)

responseCreateEventSubscription :: CreateEventSubscriptionResponse -> TestTree
responseCreateEventSubscription =
  res
    "CreateEventSubscriptionResponse"
    "fixture/CreateEventSubscriptionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateEventSubscription)

responseCreateGlobalCluster :: CreateGlobalClusterResponse -> TestTree
responseCreateGlobalCluster =
  res
    "CreateGlobalClusterResponse"
    "fixture/CreateGlobalClusterResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateGlobalCluster)

responseDeleteDBCluster :: DeleteDBClusterResponse -> TestTree
responseDeleteDBCluster =
  res
    "DeleteDBClusterResponse"
    "fixture/DeleteDBClusterResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteDBCluster)

responseDeleteDBClusterParameterGroup :: DeleteDBClusterParameterGroupResponse -> TestTree
responseDeleteDBClusterParameterGroup =
  res
    "DeleteDBClusterParameterGroupResponse"
    "fixture/DeleteDBClusterParameterGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteDBClusterParameterGroup)

responseDeleteDBClusterSnapshot :: DeleteDBClusterSnapshotResponse -> TestTree
responseDeleteDBClusterSnapshot =
  res
    "DeleteDBClusterSnapshotResponse"
    "fixture/DeleteDBClusterSnapshotResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteDBClusterSnapshot)

responseDeleteDBInstance :: DeleteDBInstanceResponse -> TestTree
responseDeleteDBInstance =
  res
    "DeleteDBInstanceResponse"
    "fixture/DeleteDBInstanceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteDBInstance)

responseDeleteDBSubnetGroup :: DeleteDBSubnetGroupResponse -> TestTree
responseDeleteDBSubnetGroup =
  res
    "DeleteDBSubnetGroupResponse"
    "fixture/DeleteDBSubnetGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteDBSubnetGroup)

responseDeleteEventSubscription :: DeleteEventSubscriptionResponse -> TestTree
responseDeleteEventSubscription =
  res
    "DeleteEventSubscriptionResponse"
    "fixture/DeleteEventSubscriptionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteEventSubscription)

responseDeleteGlobalCluster :: DeleteGlobalClusterResponse -> TestTree
responseDeleteGlobalCluster =
  res
    "DeleteGlobalClusterResponse"
    "fixture/DeleteGlobalClusterResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteGlobalCluster)

responseDescribeCertificates :: DescribeCertificatesResponse -> TestTree
responseDescribeCertificates =
  res
    "DescribeCertificatesResponse"
    "fixture/DescribeCertificatesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeCertificates)

responseDescribeDBClusterParameterGroups :: DescribeDBClusterParameterGroupsResponse -> TestTree
responseDescribeDBClusterParameterGroups =
  res
    "DescribeDBClusterParameterGroupsResponse"
    "fixture/DescribeDBClusterParameterGroupsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeDBClusterParameterGroups)

responseDescribeDBClusterParameters :: DescribeDBClusterParametersResponse -> TestTree
responseDescribeDBClusterParameters =
  res
    "DescribeDBClusterParametersResponse"
    "fixture/DescribeDBClusterParametersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeDBClusterParameters)

responseDescribeDBClusterSnapshotAttributes :: DescribeDBClusterSnapshotAttributesResponse -> TestTree
responseDescribeDBClusterSnapshotAttributes =
  res
    "DescribeDBClusterSnapshotAttributesResponse"
    "fixture/DescribeDBClusterSnapshotAttributesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeDBClusterSnapshotAttributes)

responseDescribeDBClusterSnapshots :: DescribeDBClusterSnapshotsResponse -> TestTree
responseDescribeDBClusterSnapshots =
  res
    "DescribeDBClusterSnapshotsResponse"
    "fixture/DescribeDBClusterSnapshotsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeDBClusterSnapshots)

responseDescribeDBClusters :: DescribeDBClustersResponse -> TestTree
responseDescribeDBClusters =
  res
    "DescribeDBClustersResponse"
    "fixture/DescribeDBClustersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeDBClusters)

responseDescribeDBEngineVersions :: DescribeDBEngineVersionsResponse -> TestTree
responseDescribeDBEngineVersions =
  res
    "DescribeDBEngineVersionsResponse"
    "fixture/DescribeDBEngineVersionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeDBEngineVersions)

responseDescribeDBInstances :: DescribeDBInstancesResponse -> TestTree
responseDescribeDBInstances =
  res
    "DescribeDBInstancesResponse"
    "fixture/DescribeDBInstancesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeDBInstances)

responseDescribeDBSubnetGroups :: DescribeDBSubnetGroupsResponse -> TestTree
responseDescribeDBSubnetGroups =
  res
    "DescribeDBSubnetGroupsResponse"
    "fixture/DescribeDBSubnetGroupsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeDBSubnetGroups)

responseDescribeEngineDefaultClusterParameters :: DescribeEngineDefaultClusterParametersResponse -> TestTree
responseDescribeEngineDefaultClusterParameters =
  res
    "DescribeEngineDefaultClusterParametersResponse"
    "fixture/DescribeEngineDefaultClusterParametersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeEngineDefaultClusterParameters)

responseDescribeEventCategories :: DescribeEventCategoriesResponse -> TestTree
responseDescribeEventCategories =
  res
    "DescribeEventCategoriesResponse"
    "fixture/DescribeEventCategoriesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeEventCategories)

responseDescribeEventSubscriptions :: DescribeEventSubscriptionsResponse -> TestTree
responseDescribeEventSubscriptions =
  res
    "DescribeEventSubscriptionsResponse"
    "fixture/DescribeEventSubscriptionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeEventSubscriptions)

responseDescribeEvents :: DescribeEventsResponse -> TestTree
responseDescribeEvents =
  res
    "DescribeEventsResponse"
    "fixture/DescribeEventsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeEvents)

responseDescribeGlobalClusters :: DescribeGlobalClustersResponse -> TestTree
responseDescribeGlobalClusters =
  res
    "DescribeGlobalClustersResponse"
    "fixture/DescribeGlobalClustersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeGlobalClusters)

responseDescribeOrderableDBInstanceOptions :: DescribeOrderableDBInstanceOptionsResponse -> TestTree
responseDescribeOrderableDBInstanceOptions =
  res
    "DescribeOrderableDBInstanceOptionsResponse"
    "fixture/DescribeOrderableDBInstanceOptionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeOrderableDBInstanceOptions)

responseDescribePendingMaintenanceActions :: DescribePendingMaintenanceActionsResponse -> TestTree
responseDescribePendingMaintenanceActions =
  res
    "DescribePendingMaintenanceActionsResponse"
    "fixture/DescribePendingMaintenanceActionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribePendingMaintenanceActions)

responseFailoverDBCluster :: FailoverDBClusterResponse -> TestTree
responseFailoverDBCluster =
  res
    "FailoverDBClusterResponse"
    "fixture/FailoverDBClusterResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy FailoverDBCluster)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseModifyDBCluster :: ModifyDBClusterResponse -> TestTree
responseModifyDBCluster =
  res
    "ModifyDBClusterResponse"
    "fixture/ModifyDBClusterResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifyDBCluster)

responseModifyDBClusterParameterGroup :: DBClusterParameterGroupNameMessage -> TestTree
responseModifyDBClusterParameterGroup =
  res
    "ModifyDBClusterParameterGroupResponse"
    "fixture/ModifyDBClusterParameterGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifyDBClusterParameterGroup)

responseModifyDBClusterSnapshotAttribute :: ModifyDBClusterSnapshotAttributeResponse -> TestTree
responseModifyDBClusterSnapshotAttribute =
  res
    "ModifyDBClusterSnapshotAttributeResponse"
    "fixture/ModifyDBClusterSnapshotAttributeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifyDBClusterSnapshotAttribute)

responseModifyDBInstance :: ModifyDBInstanceResponse -> TestTree
responseModifyDBInstance =
  res
    "ModifyDBInstanceResponse"
    "fixture/ModifyDBInstanceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifyDBInstance)

responseModifyDBSubnetGroup :: ModifyDBSubnetGroupResponse -> TestTree
responseModifyDBSubnetGroup =
  res
    "ModifyDBSubnetGroupResponse"
    "fixture/ModifyDBSubnetGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifyDBSubnetGroup)

responseModifyEventSubscription :: ModifyEventSubscriptionResponse -> TestTree
responseModifyEventSubscription =
  res
    "ModifyEventSubscriptionResponse"
    "fixture/ModifyEventSubscriptionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifyEventSubscription)

responseModifyGlobalCluster :: ModifyGlobalClusterResponse -> TestTree
responseModifyGlobalCluster =
  res
    "ModifyGlobalClusterResponse"
    "fixture/ModifyGlobalClusterResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifyGlobalCluster)

responseRebootDBInstance :: RebootDBInstanceResponse -> TestTree
responseRebootDBInstance =
  res
    "RebootDBInstanceResponse"
    "fixture/RebootDBInstanceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RebootDBInstance)

responseRemoveFromGlobalCluster :: RemoveFromGlobalClusterResponse -> TestTree
responseRemoveFromGlobalCluster =
  res
    "RemoveFromGlobalClusterResponse"
    "fixture/RemoveFromGlobalClusterResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RemoveFromGlobalCluster)

responseRemoveSourceIdentifierFromSubscription :: RemoveSourceIdentifierFromSubscriptionResponse -> TestTree
responseRemoveSourceIdentifierFromSubscription =
  res
    "RemoveSourceIdentifierFromSubscriptionResponse"
    "fixture/RemoveSourceIdentifierFromSubscriptionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RemoveSourceIdentifierFromSubscription)

responseRemoveTagsFromResource :: RemoveTagsFromResourceResponse -> TestTree
responseRemoveTagsFromResource =
  res
    "RemoveTagsFromResourceResponse"
    "fixture/RemoveTagsFromResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RemoveTagsFromResource)

responseResetDBClusterParameterGroup :: DBClusterParameterGroupNameMessage -> TestTree
responseResetDBClusterParameterGroup =
  res
    "ResetDBClusterParameterGroupResponse"
    "fixture/ResetDBClusterParameterGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ResetDBClusterParameterGroup)

responseRestoreDBClusterFromSnapshot :: RestoreDBClusterFromSnapshotResponse -> TestTree
responseRestoreDBClusterFromSnapshot =
  res
    "RestoreDBClusterFromSnapshotResponse"
    "fixture/RestoreDBClusterFromSnapshotResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RestoreDBClusterFromSnapshot)

responseRestoreDBClusterToPointInTime :: RestoreDBClusterToPointInTimeResponse -> TestTree
responseRestoreDBClusterToPointInTime =
  res
    "RestoreDBClusterToPointInTimeResponse"
    "fixture/RestoreDBClusterToPointInTimeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RestoreDBClusterToPointInTime)

responseStartDBCluster :: StartDBClusterResponse -> TestTree
responseStartDBCluster =
  res
    "StartDBClusterResponse"
    "fixture/StartDBClusterResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartDBCluster)

responseStopDBCluster :: StopDBClusterResponse -> TestTree
responseStopDBCluster =
  res
    "StopDBClusterResponse"
    "fixture/StopDBClusterResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StopDBCluster)
