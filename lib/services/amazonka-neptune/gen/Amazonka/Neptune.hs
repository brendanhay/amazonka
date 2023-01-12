{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.Neptune
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2014-10-31@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- Amazon Neptune
--
-- Amazon Neptune is a fast, reliable, fully-managed graph database service
-- that makes it easy to build and run applications that work with highly
-- connected datasets. The core of Amazon Neptune is a purpose-built,
-- high-performance graph database engine optimized for storing billions of
-- relationships and querying the graph with milliseconds latency. Amazon
-- Neptune supports popular graph models Property Graph and W3C\'s RDF, and
-- their respective query languages Apache TinkerPop Gremlin and SPARQL,
-- allowing you to easily build queries that efficiently navigate highly
-- connected datasets. Neptune powers graph use cases such as
-- recommendation engines, fraud detection, knowledge graphs, drug
-- discovery, and network security.
--
-- This interface reference for Amazon Neptune contains documentation for a
-- programming or command line interface you can use to manage Amazon
-- Neptune. Note that Amazon Neptune is asynchronous, which means that some
-- interfaces might require techniques such as polling or callback
-- functions to determine when a command has been applied. In this
-- reference, the parameter descriptions indicate whether a command is
-- applied immediately, on the next instance reboot, or during the
-- maintenance window. The reference structure is as follows, and we list
-- following some related topics from the user guide.
module Amazonka.Neptune
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** AuthorizationNotFoundFault
    _AuthorizationNotFoundFault,

    -- ** CertificateNotFoundFault
    _CertificateNotFoundFault,

    -- ** DBClusterAlreadyExistsFault
    _DBClusterAlreadyExistsFault,

    -- ** DBClusterEndpointAlreadyExistsFault
    _DBClusterEndpointAlreadyExistsFault,

    -- ** DBClusterEndpointNotFoundFault
    _DBClusterEndpointNotFoundFault,

    -- ** DBClusterEndpointQuotaExceededFault
    _DBClusterEndpointQuotaExceededFault,

    -- ** DBClusterNotFoundFault
    _DBClusterNotFoundFault,

    -- ** DBClusterParameterGroupNotFoundFault
    _DBClusterParameterGroupNotFoundFault,

    -- ** DBClusterQuotaExceededFault
    _DBClusterQuotaExceededFault,

    -- ** DBClusterRoleAlreadyExistsFault
    _DBClusterRoleAlreadyExistsFault,

    -- ** DBClusterRoleNotFoundFault
    _DBClusterRoleNotFoundFault,

    -- ** DBClusterRoleQuotaExceededFault
    _DBClusterRoleQuotaExceededFault,

    -- ** DBClusterSnapshotAlreadyExistsFault
    _DBClusterSnapshotAlreadyExistsFault,

    -- ** DBClusterSnapshotNotFoundFault
    _DBClusterSnapshotNotFoundFault,

    -- ** DBInstanceAlreadyExistsFault
    _DBInstanceAlreadyExistsFault,

    -- ** DBInstanceNotFoundFault
    _DBInstanceNotFoundFault,

    -- ** DBParameterGroupAlreadyExistsFault
    _DBParameterGroupAlreadyExistsFault,

    -- ** DBParameterGroupNotFoundFault
    _DBParameterGroupNotFoundFault,

    -- ** DBParameterGroupQuotaExceededFault
    _DBParameterGroupQuotaExceededFault,

    -- ** DBSecurityGroupNotFoundFault
    _DBSecurityGroupNotFoundFault,

    -- ** DBSnapshotAlreadyExistsFault
    _DBSnapshotAlreadyExistsFault,

    -- ** DBSnapshotNotFoundFault
    _DBSnapshotNotFoundFault,

    -- ** DBSubnetGroupAlreadyExistsFault
    _DBSubnetGroupAlreadyExistsFault,

    -- ** DBSubnetGroupDoesNotCoverEnoughAZs
    _DBSubnetGroupDoesNotCoverEnoughAZs,

    -- ** DBSubnetGroupNotFoundFault
    _DBSubnetGroupNotFoundFault,

    -- ** DBSubnetGroupQuotaExceededFault
    _DBSubnetGroupQuotaExceededFault,

    -- ** DBSubnetQuotaExceededFault
    _DBSubnetQuotaExceededFault,

    -- ** DBUpgradeDependencyFailureFault
    _DBUpgradeDependencyFailureFault,

    -- ** DomainNotFoundFault
    _DomainNotFoundFault,

    -- ** EventSubscriptionQuotaExceededFault
    _EventSubscriptionQuotaExceededFault,

    -- ** GlobalClusterAlreadyExistsFault
    _GlobalClusterAlreadyExistsFault,

    -- ** GlobalClusterNotFoundFault
    _GlobalClusterNotFoundFault,

    -- ** GlobalClusterQuotaExceededFault
    _GlobalClusterQuotaExceededFault,

    -- ** InstanceQuotaExceededFault
    _InstanceQuotaExceededFault,

    -- ** InsufficientDBClusterCapacityFault
    _InsufficientDBClusterCapacityFault,

    -- ** InsufficientDBInstanceCapacityFault
    _InsufficientDBInstanceCapacityFault,

    -- ** InsufficientStorageClusterCapacityFault
    _InsufficientStorageClusterCapacityFault,

    -- ** InvalidDBClusterEndpointStateFault
    _InvalidDBClusterEndpointStateFault,

    -- ** InvalidDBClusterSnapshotStateFault
    _InvalidDBClusterSnapshotStateFault,

    -- ** InvalidDBClusterStateFault
    _InvalidDBClusterStateFault,

    -- ** InvalidDBInstanceStateFault
    _InvalidDBInstanceStateFault,

    -- ** InvalidDBParameterGroupStateFault
    _InvalidDBParameterGroupStateFault,

    -- ** InvalidDBSecurityGroupStateFault
    _InvalidDBSecurityGroupStateFault,

    -- ** InvalidDBSnapshotStateFault
    _InvalidDBSnapshotStateFault,

    -- ** InvalidDBSubnetGroupStateFault
    _InvalidDBSubnetGroupStateFault,

    -- ** InvalidDBSubnetStateFault
    _InvalidDBSubnetStateFault,

    -- ** InvalidEventSubscriptionStateFault
    _InvalidEventSubscriptionStateFault,

    -- ** InvalidGlobalClusterStateFault
    _InvalidGlobalClusterStateFault,

    -- ** InvalidRestoreFault
    _InvalidRestoreFault,

    -- ** InvalidSubnet
    _InvalidSubnet,

    -- ** InvalidVPCNetworkStateFault
    _InvalidVPCNetworkStateFault,

    -- ** KMSKeyNotAccessibleFault
    _KMSKeyNotAccessibleFault,

    -- ** OptionGroupNotFoundFault
    _OptionGroupNotFoundFault,

    -- ** ProvisionedIopsNotAvailableInAZFault
    _ProvisionedIopsNotAvailableInAZFault,

    -- ** ResourceNotFoundFault
    _ResourceNotFoundFault,

    -- ** SNSInvalidTopicFault
    _SNSInvalidTopicFault,

    -- ** SNSNoAuthorizationFault
    _SNSNoAuthorizationFault,

    -- ** SNSTopicArnNotFoundFault
    _SNSTopicArnNotFoundFault,

    -- ** SharedSnapshotQuotaExceededFault
    _SharedSnapshotQuotaExceededFault,

    -- ** SnapshotQuotaExceededFault
    _SnapshotQuotaExceededFault,

    -- ** SourceNotFoundFault
    _SourceNotFoundFault,

    -- ** StorageQuotaExceededFault
    _StorageQuotaExceededFault,

    -- ** StorageTypeNotSupportedFault
    _StorageTypeNotSupportedFault,

    -- ** SubnetAlreadyInUse
    _SubnetAlreadyInUse,

    -- ** SubscriptionAlreadyExistFault
    _SubscriptionAlreadyExistFault,

    -- ** SubscriptionCategoryNotFoundFault
    _SubscriptionCategoryNotFoundFault,

    -- ** SubscriptionNotFoundFault
    _SubscriptionNotFoundFault,

    -- * Waiters
    -- $waiters

    -- ** DBInstanceAvailable
    newDBInstanceAvailable,

    -- ** DBInstanceDeleted
    newDBInstanceDeleted,

    -- * Operations
    -- $operations

    -- ** AddRoleToDBCluster
    AddRoleToDBCluster (AddRoleToDBCluster'),
    newAddRoleToDBCluster,
    AddRoleToDBClusterResponse (AddRoleToDBClusterResponse'),
    newAddRoleToDBClusterResponse,

    -- ** AddSourceIdentifierToSubscription
    AddSourceIdentifierToSubscription (AddSourceIdentifierToSubscription'),
    newAddSourceIdentifierToSubscription,
    AddSourceIdentifierToSubscriptionResponse (AddSourceIdentifierToSubscriptionResponse'),
    newAddSourceIdentifierToSubscriptionResponse,

    -- ** AddTagsToResource
    AddTagsToResource (AddTagsToResource'),
    newAddTagsToResource,
    AddTagsToResourceResponse (AddTagsToResourceResponse'),
    newAddTagsToResourceResponse,

    -- ** ApplyPendingMaintenanceAction
    ApplyPendingMaintenanceAction (ApplyPendingMaintenanceAction'),
    newApplyPendingMaintenanceAction,
    ApplyPendingMaintenanceActionResponse (ApplyPendingMaintenanceActionResponse'),
    newApplyPendingMaintenanceActionResponse,

    -- ** CopyDBClusterParameterGroup
    CopyDBClusterParameterGroup (CopyDBClusterParameterGroup'),
    newCopyDBClusterParameterGroup,
    CopyDBClusterParameterGroupResponse (CopyDBClusterParameterGroupResponse'),
    newCopyDBClusterParameterGroupResponse,

    -- ** CopyDBClusterSnapshot
    CopyDBClusterSnapshot (CopyDBClusterSnapshot'),
    newCopyDBClusterSnapshot,
    CopyDBClusterSnapshotResponse (CopyDBClusterSnapshotResponse'),
    newCopyDBClusterSnapshotResponse,

    -- ** CopyDBParameterGroup
    CopyDBParameterGroup (CopyDBParameterGroup'),
    newCopyDBParameterGroup,
    CopyDBParameterGroupResponse (CopyDBParameterGroupResponse'),
    newCopyDBParameterGroupResponse,

    -- ** CreateDBCluster
    CreateDBCluster (CreateDBCluster'),
    newCreateDBCluster,
    CreateDBClusterResponse (CreateDBClusterResponse'),
    newCreateDBClusterResponse,

    -- ** CreateDBClusterEndpoint
    CreateDBClusterEndpoint (CreateDBClusterEndpoint'),
    newCreateDBClusterEndpoint,
    CreateDBClusterEndpointResponse (CreateDBClusterEndpointResponse'),
    newCreateDBClusterEndpointResponse,

    -- ** CreateDBClusterParameterGroup
    CreateDBClusterParameterGroup (CreateDBClusterParameterGroup'),
    newCreateDBClusterParameterGroup,
    CreateDBClusterParameterGroupResponse (CreateDBClusterParameterGroupResponse'),
    newCreateDBClusterParameterGroupResponse,

    -- ** CreateDBClusterSnapshot
    CreateDBClusterSnapshot (CreateDBClusterSnapshot'),
    newCreateDBClusterSnapshot,
    CreateDBClusterSnapshotResponse (CreateDBClusterSnapshotResponse'),
    newCreateDBClusterSnapshotResponse,

    -- ** CreateDBInstance
    CreateDBInstance (CreateDBInstance'),
    newCreateDBInstance,
    CreateDBInstanceResponse (CreateDBInstanceResponse'),
    newCreateDBInstanceResponse,

    -- ** CreateDBParameterGroup
    CreateDBParameterGroup (CreateDBParameterGroup'),
    newCreateDBParameterGroup,
    CreateDBParameterGroupResponse (CreateDBParameterGroupResponse'),
    newCreateDBParameterGroupResponse,

    -- ** CreateDBSubnetGroup
    CreateDBSubnetGroup (CreateDBSubnetGroup'),
    newCreateDBSubnetGroup,
    CreateDBSubnetGroupResponse (CreateDBSubnetGroupResponse'),
    newCreateDBSubnetGroupResponse,

    -- ** CreateEventSubscription
    CreateEventSubscription (CreateEventSubscription'),
    newCreateEventSubscription,
    CreateEventSubscriptionResponse (CreateEventSubscriptionResponse'),
    newCreateEventSubscriptionResponse,

    -- ** CreateGlobalCluster
    CreateGlobalCluster (CreateGlobalCluster'),
    newCreateGlobalCluster,
    CreateGlobalClusterResponse (CreateGlobalClusterResponse'),
    newCreateGlobalClusterResponse,

    -- ** DeleteDBCluster
    DeleteDBCluster (DeleteDBCluster'),
    newDeleteDBCluster,
    DeleteDBClusterResponse (DeleteDBClusterResponse'),
    newDeleteDBClusterResponse,

    -- ** DeleteDBClusterEndpoint
    DeleteDBClusterEndpoint (DeleteDBClusterEndpoint'),
    newDeleteDBClusterEndpoint,
    DeleteDBClusterEndpointResponse (DeleteDBClusterEndpointResponse'),
    newDeleteDBClusterEndpointResponse,

    -- ** DeleteDBClusterParameterGroup
    DeleteDBClusterParameterGroup (DeleteDBClusterParameterGroup'),
    newDeleteDBClusterParameterGroup,
    DeleteDBClusterParameterGroupResponse (DeleteDBClusterParameterGroupResponse'),
    newDeleteDBClusterParameterGroupResponse,

    -- ** DeleteDBClusterSnapshot
    DeleteDBClusterSnapshot (DeleteDBClusterSnapshot'),
    newDeleteDBClusterSnapshot,
    DeleteDBClusterSnapshotResponse (DeleteDBClusterSnapshotResponse'),
    newDeleteDBClusterSnapshotResponse,

    -- ** DeleteDBInstance
    DeleteDBInstance (DeleteDBInstance'),
    newDeleteDBInstance,
    DeleteDBInstanceResponse (DeleteDBInstanceResponse'),
    newDeleteDBInstanceResponse,

    -- ** DeleteDBParameterGroup
    DeleteDBParameterGroup (DeleteDBParameterGroup'),
    newDeleteDBParameterGroup,
    DeleteDBParameterGroupResponse (DeleteDBParameterGroupResponse'),
    newDeleteDBParameterGroupResponse,

    -- ** DeleteDBSubnetGroup
    DeleteDBSubnetGroup (DeleteDBSubnetGroup'),
    newDeleteDBSubnetGroup,
    DeleteDBSubnetGroupResponse (DeleteDBSubnetGroupResponse'),
    newDeleteDBSubnetGroupResponse,

    -- ** DeleteEventSubscription
    DeleteEventSubscription (DeleteEventSubscription'),
    newDeleteEventSubscription,
    DeleteEventSubscriptionResponse (DeleteEventSubscriptionResponse'),
    newDeleteEventSubscriptionResponse,

    -- ** DeleteGlobalCluster
    DeleteGlobalCluster (DeleteGlobalCluster'),
    newDeleteGlobalCluster,
    DeleteGlobalClusterResponse (DeleteGlobalClusterResponse'),
    newDeleteGlobalClusterResponse,

    -- ** DescribeDBClusterEndpoints (Paginated)
    DescribeDBClusterEndpoints (DescribeDBClusterEndpoints'),
    newDescribeDBClusterEndpoints,
    DescribeDBClusterEndpointsResponse (DescribeDBClusterEndpointsResponse'),
    newDescribeDBClusterEndpointsResponse,

    -- ** DescribeDBClusterParameterGroups (Paginated)
    DescribeDBClusterParameterGroups (DescribeDBClusterParameterGroups'),
    newDescribeDBClusterParameterGroups,
    DescribeDBClusterParameterGroupsResponse (DescribeDBClusterParameterGroupsResponse'),
    newDescribeDBClusterParameterGroupsResponse,

    -- ** DescribeDBClusterParameters (Paginated)
    DescribeDBClusterParameters (DescribeDBClusterParameters'),
    newDescribeDBClusterParameters,
    DescribeDBClusterParametersResponse (DescribeDBClusterParametersResponse'),
    newDescribeDBClusterParametersResponse,

    -- ** DescribeDBClusterSnapshotAttributes
    DescribeDBClusterSnapshotAttributes (DescribeDBClusterSnapshotAttributes'),
    newDescribeDBClusterSnapshotAttributes,
    DescribeDBClusterSnapshotAttributesResponse (DescribeDBClusterSnapshotAttributesResponse'),
    newDescribeDBClusterSnapshotAttributesResponse,

    -- ** DescribeDBClusterSnapshots (Paginated)
    DescribeDBClusterSnapshots (DescribeDBClusterSnapshots'),
    newDescribeDBClusterSnapshots,
    DescribeDBClusterSnapshotsResponse (DescribeDBClusterSnapshotsResponse'),
    newDescribeDBClusterSnapshotsResponse,

    -- ** DescribeDBClusters (Paginated)
    DescribeDBClusters (DescribeDBClusters'),
    newDescribeDBClusters,
    DescribeDBClustersResponse (DescribeDBClustersResponse'),
    newDescribeDBClustersResponse,

    -- ** DescribeDBEngineVersions (Paginated)
    DescribeDBEngineVersions (DescribeDBEngineVersions'),
    newDescribeDBEngineVersions,
    DescribeDBEngineVersionsResponse (DescribeDBEngineVersionsResponse'),
    newDescribeDBEngineVersionsResponse,

    -- ** DescribeDBInstances (Paginated)
    DescribeDBInstances (DescribeDBInstances'),
    newDescribeDBInstances,
    DescribeDBInstancesResponse (DescribeDBInstancesResponse'),
    newDescribeDBInstancesResponse,

    -- ** DescribeDBParameterGroups (Paginated)
    DescribeDBParameterGroups (DescribeDBParameterGroups'),
    newDescribeDBParameterGroups,
    DescribeDBParameterGroupsResponse (DescribeDBParameterGroupsResponse'),
    newDescribeDBParameterGroupsResponse,

    -- ** DescribeDBParameters (Paginated)
    DescribeDBParameters (DescribeDBParameters'),
    newDescribeDBParameters,
    DescribeDBParametersResponse (DescribeDBParametersResponse'),
    newDescribeDBParametersResponse,

    -- ** DescribeDBSubnetGroups (Paginated)
    DescribeDBSubnetGroups (DescribeDBSubnetGroups'),
    newDescribeDBSubnetGroups,
    DescribeDBSubnetGroupsResponse (DescribeDBSubnetGroupsResponse'),
    newDescribeDBSubnetGroupsResponse,

    -- ** DescribeEngineDefaultClusterParameters
    DescribeEngineDefaultClusterParameters (DescribeEngineDefaultClusterParameters'),
    newDescribeEngineDefaultClusterParameters,
    DescribeEngineDefaultClusterParametersResponse (DescribeEngineDefaultClusterParametersResponse'),
    newDescribeEngineDefaultClusterParametersResponse,

    -- ** DescribeEngineDefaultParameters (Paginated)
    DescribeEngineDefaultParameters (DescribeEngineDefaultParameters'),
    newDescribeEngineDefaultParameters,
    DescribeEngineDefaultParametersResponse (DescribeEngineDefaultParametersResponse'),
    newDescribeEngineDefaultParametersResponse,

    -- ** DescribeEventCategories
    DescribeEventCategories (DescribeEventCategories'),
    newDescribeEventCategories,
    DescribeEventCategoriesResponse (DescribeEventCategoriesResponse'),
    newDescribeEventCategoriesResponse,

    -- ** DescribeEventSubscriptions (Paginated)
    DescribeEventSubscriptions (DescribeEventSubscriptions'),
    newDescribeEventSubscriptions,
    DescribeEventSubscriptionsResponse (DescribeEventSubscriptionsResponse'),
    newDescribeEventSubscriptionsResponse,

    -- ** DescribeEvents (Paginated)
    DescribeEvents (DescribeEvents'),
    newDescribeEvents,
    DescribeEventsResponse (DescribeEventsResponse'),
    newDescribeEventsResponse,

    -- ** DescribeGlobalClusters (Paginated)
    DescribeGlobalClusters (DescribeGlobalClusters'),
    newDescribeGlobalClusters,
    DescribeGlobalClustersResponse (DescribeGlobalClustersResponse'),
    newDescribeGlobalClustersResponse,

    -- ** DescribeOrderableDBInstanceOptions (Paginated)
    DescribeOrderableDBInstanceOptions (DescribeOrderableDBInstanceOptions'),
    newDescribeOrderableDBInstanceOptions,
    DescribeOrderableDBInstanceOptionsResponse (DescribeOrderableDBInstanceOptionsResponse'),
    newDescribeOrderableDBInstanceOptionsResponse,

    -- ** DescribePendingMaintenanceActions (Paginated)
    DescribePendingMaintenanceActions (DescribePendingMaintenanceActions'),
    newDescribePendingMaintenanceActions,
    DescribePendingMaintenanceActionsResponse (DescribePendingMaintenanceActionsResponse'),
    newDescribePendingMaintenanceActionsResponse,

    -- ** DescribeValidDBInstanceModifications
    DescribeValidDBInstanceModifications (DescribeValidDBInstanceModifications'),
    newDescribeValidDBInstanceModifications,
    DescribeValidDBInstanceModificationsResponse (DescribeValidDBInstanceModificationsResponse'),
    newDescribeValidDBInstanceModificationsResponse,

    -- ** FailoverDBCluster
    FailoverDBCluster (FailoverDBCluster'),
    newFailoverDBCluster,
    FailoverDBClusterResponse (FailoverDBClusterResponse'),
    newFailoverDBClusterResponse,

    -- ** FailoverGlobalCluster
    FailoverGlobalCluster (FailoverGlobalCluster'),
    newFailoverGlobalCluster,
    FailoverGlobalClusterResponse (FailoverGlobalClusterResponse'),
    newFailoverGlobalClusterResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** ModifyDBCluster
    ModifyDBCluster (ModifyDBCluster'),
    newModifyDBCluster,
    ModifyDBClusterResponse (ModifyDBClusterResponse'),
    newModifyDBClusterResponse,

    -- ** ModifyDBClusterEndpoint
    ModifyDBClusterEndpoint (ModifyDBClusterEndpoint'),
    newModifyDBClusterEndpoint,
    ModifyDBClusterEndpointResponse (ModifyDBClusterEndpointResponse'),
    newModifyDBClusterEndpointResponse,

    -- ** ModifyDBClusterParameterGroup
    ModifyDBClusterParameterGroup (ModifyDBClusterParameterGroup'),
    newModifyDBClusterParameterGroup,
    DBClusterParameterGroupNameMessage (DBClusterParameterGroupNameMessage'),
    newDBClusterParameterGroupNameMessage,

    -- ** ModifyDBClusterSnapshotAttribute
    ModifyDBClusterSnapshotAttribute (ModifyDBClusterSnapshotAttribute'),
    newModifyDBClusterSnapshotAttribute,
    ModifyDBClusterSnapshotAttributeResponse (ModifyDBClusterSnapshotAttributeResponse'),
    newModifyDBClusterSnapshotAttributeResponse,

    -- ** ModifyDBInstance
    ModifyDBInstance (ModifyDBInstance'),
    newModifyDBInstance,
    ModifyDBInstanceResponse (ModifyDBInstanceResponse'),
    newModifyDBInstanceResponse,

    -- ** ModifyDBParameterGroup
    ModifyDBParameterGroup (ModifyDBParameterGroup'),
    newModifyDBParameterGroup,
    DBParameterGroupNameMessage (DBParameterGroupNameMessage'),
    newDBParameterGroupNameMessage,

    -- ** ModifyDBSubnetGroup
    ModifyDBSubnetGroup (ModifyDBSubnetGroup'),
    newModifyDBSubnetGroup,
    ModifyDBSubnetGroupResponse (ModifyDBSubnetGroupResponse'),
    newModifyDBSubnetGroupResponse,

    -- ** ModifyEventSubscription
    ModifyEventSubscription (ModifyEventSubscription'),
    newModifyEventSubscription,
    ModifyEventSubscriptionResponse (ModifyEventSubscriptionResponse'),
    newModifyEventSubscriptionResponse,

    -- ** ModifyGlobalCluster
    ModifyGlobalCluster (ModifyGlobalCluster'),
    newModifyGlobalCluster,
    ModifyGlobalClusterResponse (ModifyGlobalClusterResponse'),
    newModifyGlobalClusterResponse,

    -- ** PromoteReadReplicaDBCluster
    PromoteReadReplicaDBCluster (PromoteReadReplicaDBCluster'),
    newPromoteReadReplicaDBCluster,
    PromoteReadReplicaDBClusterResponse (PromoteReadReplicaDBClusterResponse'),
    newPromoteReadReplicaDBClusterResponse,

    -- ** RebootDBInstance
    RebootDBInstance (RebootDBInstance'),
    newRebootDBInstance,
    RebootDBInstanceResponse (RebootDBInstanceResponse'),
    newRebootDBInstanceResponse,

    -- ** RemoveFromGlobalCluster
    RemoveFromGlobalCluster (RemoveFromGlobalCluster'),
    newRemoveFromGlobalCluster,
    RemoveFromGlobalClusterResponse (RemoveFromGlobalClusterResponse'),
    newRemoveFromGlobalClusterResponse,

    -- ** RemoveRoleFromDBCluster
    RemoveRoleFromDBCluster (RemoveRoleFromDBCluster'),
    newRemoveRoleFromDBCluster,
    RemoveRoleFromDBClusterResponse (RemoveRoleFromDBClusterResponse'),
    newRemoveRoleFromDBClusterResponse,

    -- ** RemoveSourceIdentifierFromSubscription
    RemoveSourceIdentifierFromSubscription (RemoveSourceIdentifierFromSubscription'),
    newRemoveSourceIdentifierFromSubscription,
    RemoveSourceIdentifierFromSubscriptionResponse (RemoveSourceIdentifierFromSubscriptionResponse'),
    newRemoveSourceIdentifierFromSubscriptionResponse,

    -- ** RemoveTagsFromResource
    RemoveTagsFromResource (RemoveTagsFromResource'),
    newRemoveTagsFromResource,
    RemoveTagsFromResourceResponse (RemoveTagsFromResourceResponse'),
    newRemoveTagsFromResourceResponse,

    -- ** ResetDBClusterParameterGroup
    ResetDBClusterParameterGroup (ResetDBClusterParameterGroup'),
    newResetDBClusterParameterGroup,
    DBClusterParameterGroupNameMessage (DBClusterParameterGroupNameMessage'),
    newDBClusterParameterGroupNameMessage,

    -- ** ResetDBParameterGroup
    ResetDBParameterGroup (ResetDBParameterGroup'),
    newResetDBParameterGroup,
    DBParameterGroupNameMessage (DBParameterGroupNameMessage'),
    newDBParameterGroupNameMessage,

    -- ** RestoreDBClusterFromSnapshot
    RestoreDBClusterFromSnapshot (RestoreDBClusterFromSnapshot'),
    newRestoreDBClusterFromSnapshot,
    RestoreDBClusterFromSnapshotResponse (RestoreDBClusterFromSnapshotResponse'),
    newRestoreDBClusterFromSnapshotResponse,

    -- ** RestoreDBClusterToPointInTime
    RestoreDBClusterToPointInTime (RestoreDBClusterToPointInTime'),
    newRestoreDBClusterToPointInTime,
    RestoreDBClusterToPointInTimeResponse (RestoreDBClusterToPointInTimeResponse'),
    newRestoreDBClusterToPointInTimeResponse,

    -- ** StartDBCluster
    StartDBCluster (StartDBCluster'),
    newStartDBCluster,
    StartDBClusterResponse (StartDBClusterResponse'),
    newStartDBClusterResponse,

    -- ** StopDBCluster
    StopDBCluster (StopDBCluster'),
    newStopDBCluster,
    StopDBClusterResponse (StopDBClusterResponse'),
    newStopDBClusterResponse,

    -- * Types

    -- ** ApplyMethod
    ApplyMethod (..),

    -- ** SourceType
    SourceType (..),

    -- ** AvailabilityZone
    AvailabilityZone (AvailabilityZone'),
    newAvailabilityZone,

    -- ** CharacterSet
    CharacterSet (CharacterSet'),
    newCharacterSet,

    -- ** CloudwatchLogsExportConfiguration
    CloudwatchLogsExportConfiguration (CloudwatchLogsExportConfiguration'),
    newCloudwatchLogsExportConfiguration,

    -- ** DBCluster
    DBCluster (DBCluster'),
    newDBCluster,

    -- ** DBClusterEndpoint
    DBClusterEndpoint (DBClusterEndpoint'),
    newDBClusterEndpoint,

    -- ** DBClusterMember
    DBClusterMember (DBClusterMember'),
    newDBClusterMember,

    -- ** DBClusterOptionGroupStatus
    DBClusterOptionGroupStatus (DBClusterOptionGroupStatus'),
    newDBClusterOptionGroupStatus,

    -- ** DBClusterParameterGroup
    DBClusterParameterGroup (DBClusterParameterGroup'),
    newDBClusterParameterGroup,

    -- ** DBClusterParameterGroupNameMessage
    DBClusterParameterGroupNameMessage (DBClusterParameterGroupNameMessage'),
    newDBClusterParameterGroupNameMessage,

    -- ** DBClusterRole
    DBClusterRole (DBClusterRole'),
    newDBClusterRole,

    -- ** DBClusterSnapshot
    DBClusterSnapshot (DBClusterSnapshot'),
    newDBClusterSnapshot,

    -- ** DBClusterSnapshotAttribute
    DBClusterSnapshotAttribute (DBClusterSnapshotAttribute'),
    newDBClusterSnapshotAttribute,

    -- ** DBClusterSnapshotAttributesResult
    DBClusterSnapshotAttributesResult (DBClusterSnapshotAttributesResult'),
    newDBClusterSnapshotAttributesResult,

    -- ** DBEngineVersion
    DBEngineVersion (DBEngineVersion'),
    newDBEngineVersion,

    -- ** DBInstance
    DBInstance (DBInstance'),
    newDBInstance,

    -- ** DBInstanceStatusInfo
    DBInstanceStatusInfo (DBInstanceStatusInfo'),
    newDBInstanceStatusInfo,

    -- ** DBParameterGroup
    DBParameterGroup (DBParameterGroup'),
    newDBParameterGroup,

    -- ** DBParameterGroupNameMessage
    DBParameterGroupNameMessage (DBParameterGroupNameMessage'),
    newDBParameterGroupNameMessage,

    -- ** DBParameterGroupStatus
    DBParameterGroupStatus (DBParameterGroupStatus'),
    newDBParameterGroupStatus,

    -- ** DBSecurityGroupMembership
    DBSecurityGroupMembership (DBSecurityGroupMembership'),
    newDBSecurityGroupMembership,

    -- ** DBSubnetGroup
    DBSubnetGroup (DBSubnetGroup'),
    newDBSubnetGroup,

    -- ** DomainMembership
    DomainMembership (DomainMembership'),
    newDomainMembership,

    -- ** DoubleRange
    DoubleRange (DoubleRange'),
    newDoubleRange,

    -- ** Endpoint
    Endpoint (Endpoint'),
    newEndpoint,

    -- ** EngineDefaults
    EngineDefaults (EngineDefaults'),
    newEngineDefaults,

    -- ** Event
    Event (Event'),
    newEvent,

    -- ** EventCategoriesMap
    EventCategoriesMap (EventCategoriesMap'),
    newEventCategoriesMap,

    -- ** EventSubscription
    EventSubscription (EventSubscription'),
    newEventSubscription,

    -- ** Filter
    Filter (Filter'),
    newFilter,

    -- ** GlobalCluster
    GlobalCluster (GlobalCluster'),
    newGlobalCluster,

    -- ** GlobalClusterMember
    GlobalClusterMember (GlobalClusterMember'),
    newGlobalClusterMember,

    -- ** OptionGroupMembership
    OptionGroupMembership (OptionGroupMembership'),
    newOptionGroupMembership,

    -- ** OrderableDBInstanceOption
    OrderableDBInstanceOption (OrderableDBInstanceOption'),
    newOrderableDBInstanceOption,

    -- ** Parameter
    Parameter (Parameter'),
    newParameter,

    -- ** PendingCloudwatchLogsExports
    PendingCloudwatchLogsExports (PendingCloudwatchLogsExports'),
    newPendingCloudwatchLogsExports,

    -- ** PendingMaintenanceAction
    PendingMaintenanceAction (PendingMaintenanceAction'),
    newPendingMaintenanceAction,

    -- ** PendingModifiedValues
    PendingModifiedValues (PendingModifiedValues'),
    newPendingModifiedValues,

    -- ** Range
    Range (Range'),
    newRange,

    -- ** ResourcePendingMaintenanceActions
    ResourcePendingMaintenanceActions (ResourcePendingMaintenanceActions'),
    newResourcePendingMaintenanceActions,

    -- ** ServerlessV2ScalingConfiguration
    ServerlessV2ScalingConfiguration (ServerlessV2ScalingConfiguration'),
    newServerlessV2ScalingConfiguration,

    -- ** ServerlessV2ScalingConfigurationInfo
    ServerlessV2ScalingConfigurationInfo (ServerlessV2ScalingConfigurationInfo'),
    newServerlessV2ScalingConfigurationInfo,

    -- ** Subnet
    Subnet (Subnet'),
    newSubnet,

    -- ** Tag
    Tag (Tag'),
    newTag,

    -- ** Timezone
    Timezone (Timezone'),
    newTimezone,

    -- ** UpgradeTarget
    UpgradeTarget (UpgradeTarget'),
    newUpgradeTarget,

    -- ** ValidDBInstanceModificationsMessage
    ValidDBInstanceModificationsMessage (ValidDBInstanceModificationsMessage'),
    newValidDBInstanceModificationsMessage,

    -- ** ValidStorageOptions
    ValidStorageOptions (ValidStorageOptions'),
    newValidStorageOptions,

    -- ** VpcSecurityGroupMembership
    VpcSecurityGroupMembership (VpcSecurityGroupMembership'),
    newVpcSecurityGroupMembership,
  )
where

import Amazonka.Neptune.AddRoleToDBCluster
import Amazonka.Neptune.AddSourceIdentifierToSubscription
import Amazonka.Neptune.AddTagsToResource
import Amazonka.Neptune.ApplyPendingMaintenanceAction
import Amazonka.Neptune.CopyDBClusterParameterGroup
import Amazonka.Neptune.CopyDBClusterSnapshot
import Amazonka.Neptune.CopyDBParameterGroup
import Amazonka.Neptune.CreateDBCluster
import Amazonka.Neptune.CreateDBClusterEndpoint
import Amazonka.Neptune.CreateDBClusterParameterGroup
import Amazonka.Neptune.CreateDBClusterSnapshot
import Amazonka.Neptune.CreateDBInstance
import Amazonka.Neptune.CreateDBParameterGroup
import Amazonka.Neptune.CreateDBSubnetGroup
import Amazonka.Neptune.CreateEventSubscription
import Amazonka.Neptune.CreateGlobalCluster
import Amazonka.Neptune.DeleteDBCluster
import Amazonka.Neptune.DeleteDBClusterEndpoint
import Amazonka.Neptune.DeleteDBClusterParameterGroup
import Amazonka.Neptune.DeleteDBClusterSnapshot
import Amazonka.Neptune.DeleteDBInstance
import Amazonka.Neptune.DeleteDBParameterGroup
import Amazonka.Neptune.DeleteDBSubnetGroup
import Amazonka.Neptune.DeleteEventSubscription
import Amazonka.Neptune.DeleteGlobalCluster
import Amazonka.Neptune.DescribeDBClusterEndpoints
import Amazonka.Neptune.DescribeDBClusterParameterGroups
import Amazonka.Neptune.DescribeDBClusterParameters
import Amazonka.Neptune.DescribeDBClusterSnapshotAttributes
import Amazonka.Neptune.DescribeDBClusterSnapshots
import Amazonka.Neptune.DescribeDBClusters
import Amazonka.Neptune.DescribeDBEngineVersions
import Amazonka.Neptune.DescribeDBInstances
import Amazonka.Neptune.DescribeDBParameterGroups
import Amazonka.Neptune.DescribeDBParameters
import Amazonka.Neptune.DescribeDBSubnetGroups
import Amazonka.Neptune.DescribeEngineDefaultClusterParameters
import Amazonka.Neptune.DescribeEngineDefaultParameters
import Amazonka.Neptune.DescribeEventCategories
import Amazonka.Neptune.DescribeEventSubscriptions
import Amazonka.Neptune.DescribeEvents
import Amazonka.Neptune.DescribeGlobalClusters
import Amazonka.Neptune.DescribeOrderableDBInstanceOptions
import Amazonka.Neptune.DescribePendingMaintenanceActions
import Amazonka.Neptune.DescribeValidDBInstanceModifications
import Amazonka.Neptune.FailoverDBCluster
import Amazonka.Neptune.FailoverGlobalCluster
import Amazonka.Neptune.Lens
import Amazonka.Neptune.ListTagsForResource
import Amazonka.Neptune.ModifyDBCluster
import Amazonka.Neptune.ModifyDBClusterEndpoint
import Amazonka.Neptune.ModifyDBClusterParameterGroup
import Amazonka.Neptune.ModifyDBClusterSnapshotAttribute
import Amazonka.Neptune.ModifyDBInstance
import Amazonka.Neptune.ModifyDBParameterGroup
import Amazonka.Neptune.ModifyDBSubnetGroup
import Amazonka.Neptune.ModifyEventSubscription
import Amazonka.Neptune.ModifyGlobalCluster
import Amazonka.Neptune.PromoteReadReplicaDBCluster
import Amazonka.Neptune.RebootDBInstance
import Amazonka.Neptune.RemoveFromGlobalCluster
import Amazonka.Neptune.RemoveRoleFromDBCluster
import Amazonka.Neptune.RemoveSourceIdentifierFromSubscription
import Amazonka.Neptune.RemoveTagsFromResource
import Amazonka.Neptune.ResetDBClusterParameterGroup
import Amazonka.Neptune.ResetDBParameterGroup
import Amazonka.Neptune.RestoreDBClusterFromSnapshot
import Amazonka.Neptune.RestoreDBClusterToPointInTime
import Amazonka.Neptune.StartDBCluster
import Amazonka.Neptune.StopDBCluster
import Amazonka.Neptune.Types
import Amazonka.Neptune.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'Neptune'.

-- $operations
-- Some AWS operations return results that are incomplete and require subsequent
-- requests in order to obtain the entire result set. The process of sending
-- subsequent requests to continue where a previous request left off is called
-- pagination. For example, the 'ListObjects' operation of Amazon S3 returns up to
-- 1000 objects at a time, and you must send subsequent requests with the
-- appropriate Marker in order to retrieve the next page of results.
--
-- Operations that have an 'AWSPager' instance can transparently perform subsequent
-- requests, correctly setting Markers and other request facets to iterate through
-- the entire result set of a truncated API operation. Operations which support
-- this have an additional note in the documentation.
--
-- Many operations have the ability to filter results on the server side. See the
-- individual operation parameters for details.

-- $waiters
-- Waiters poll by repeatedly sending a request until some remote success condition
-- configured by the 'Wait' specification is fulfilled. The 'Wait' specification
-- determines how many attempts should be made, in addition to delay and retry strategies.
