{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Network.AWS.Neptune
-- Copyright   : (c) 2013-2021 Brendan Hay
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
module Network.AWS.Neptune
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** InvalidDBParameterGroupStateFault
    _InvalidDBParameterGroupStateFault,

    -- ** SourceNotFoundFault
    _SourceNotFoundFault,

    -- ** CertificateNotFoundFault
    _CertificateNotFoundFault,

    -- ** DBClusterSnapshotAlreadyExistsFault
    _DBClusterSnapshotAlreadyExistsFault,

    -- ** DBParameterGroupAlreadyExistsFault
    _DBParameterGroupAlreadyExistsFault,

    -- ** DBParameterGroupQuotaExceededFault
    _DBParameterGroupQuotaExceededFault,

    -- ** InsufficientDBClusterCapacityFault
    _InsufficientDBClusterCapacityFault,

    -- ** ProvisionedIopsNotAvailableInAZFault
    _ProvisionedIopsNotAvailableInAZFault,

    -- ** SubscriptionCategoryNotFoundFault
    _SubscriptionCategoryNotFoundFault,

    -- ** SubscriptionNotFoundFault
    _SubscriptionNotFoundFault,

    -- ** InvalidSubnet
    _InvalidSubnet,

    -- ** SharedSnapshotQuotaExceededFault
    _SharedSnapshotQuotaExceededFault,

    -- ** DBSubnetQuotaExceededFault
    _DBSubnetQuotaExceededFault,

    -- ** OptionGroupNotFoundFault
    _OptionGroupNotFoundFault,

    -- ** DBClusterNotFoundFault
    _DBClusterNotFoundFault,

    -- ** DBClusterAlreadyExistsFault
    _DBClusterAlreadyExistsFault,

    -- ** StorageTypeNotSupportedFault
    _StorageTypeNotSupportedFault,

    -- ** SNSTopicArnNotFoundFault
    _SNSTopicArnNotFoundFault,

    -- ** InvalidDBClusterEndpointStateFault
    _InvalidDBClusterEndpointStateFault,

    -- ** InvalidEventSubscriptionStateFault
    _InvalidEventSubscriptionStateFault,

    -- ** KMSKeyNotAccessibleFault
    _KMSKeyNotAccessibleFault,

    -- ** DBSnapshotNotFoundFault
    _DBSnapshotNotFoundFault,

    -- ** DBClusterParameterGroupNotFoundFault
    _DBClusterParameterGroupNotFoundFault,

    -- ** DBClusterQuotaExceededFault
    _DBClusterQuotaExceededFault,

    -- ** SnapshotQuotaExceededFault
    _SnapshotQuotaExceededFault,

    -- ** DBSubnetGroupAlreadyExistsFault
    _DBSubnetGroupAlreadyExistsFault,

    -- ** SNSNoAuthorizationFault
    _SNSNoAuthorizationFault,

    -- ** DBSecurityGroupNotFoundFault
    _DBSecurityGroupNotFoundFault,

    -- ** InstanceQuotaExceededFault
    _InstanceQuotaExceededFault,

    -- ** DomainNotFoundFault
    _DomainNotFoundFault,

    -- ** DBParameterGroupNotFoundFault
    _DBParameterGroupNotFoundFault,

    -- ** InvalidDBSubnetStateFault
    _InvalidDBSubnetStateFault,

    -- ** DBClusterSnapshotNotFoundFault
    _DBClusterSnapshotNotFoundFault,

    -- ** SNSInvalidTopicFault
    _SNSInvalidTopicFault,

    -- ** InsufficientDBInstanceCapacityFault
    _InsufficientDBInstanceCapacityFault,

    -- ** InvalidDBClusterSnapshotStateFault
    _InvalidDBClusterSnapshotStateFault,

    -- ** SubscriptionAlreadyExistFault
    _SubscriptionAlreadyExistFault,

    -- ** DBClusterRoleAlreadyExistsFault
    _DBClusterRoleAlreadyExistsFault,

    -- ** DBClusterRoleQuotaExceededFault
    _DBClusterRoleQuotaExceededFault,

    -- ** InvalidVPCNetworkStateFault
    _InvalidVPCNetworkStateFault,

    -- ** AuthorizationNotFoundFault
    _AuthorizationNotFoundFault,

    -- ** DBSubnetGroupQuotaExceededFault
    _DBSubnetGroupQuotaExceededFault,

    -- ** EventSubscriptionQuotaExceededFault
    _EventSubscriptionQuotaExceededFault,

    -- ** InsufficientStorageClusterCapacityFault
    _InsufficientStorageClusterCapacityFault,

    -- ** DBClusterEndpointQuotaExceededFault
    _DBClusterEndpointQuotaExceededFault,

    -- ** InvalidDBClusterStateFault
    _InvalidDBClusterStateFault,

    -- ** DBInstanceAlreadyExistsFault
    _DBInstanceAlreadyExistsFault,

    -- ** InvalidRestoreFault
    _InvalidRestoreFault,

    -- ** InvalidDBSecurityGroupStateFault
    _InvalidDBSecurityGroupStateFault,

    -- ** ResourceNotFoundFault
    _ResourceNotFoundFault,

    -- ** DBSubnetGroupNotFoundFault
    _DBSubnetGroupNotFoundFault,

    -- ** DBUpgradeDependencyFailureFault
    _DBUpgradeDependencyFailureFault,

    -- ** InvalidDBInstanceStateFault
    _InvalidDBInstanceStateFault,

    -- ** DBClusterEndpointAlreadyExistsFault
    _DBClusterEndpointAlreadyExistsFault,

    -- ** DBSnapshotAlreadyExistsFault
    _DBSnapshotAlreadyExistsFault,

    -- ** DBInstanceNotFoundFault
    _DBInstanceNotFoundFault,

    -- ** StorageQuotaExceededFault
    _StorageQuotaExceededFault,

    -- ** InvalidDBSnapshotStateFault
    _InvalidDBSnapshotStateFault,

    -- ** InvalidDBSubnetGroupStateFault
    _InvalidDBSubnetGroupStateFault,

    -- ** DBClusterEndpointNotFoundFault
    _DBClusterEndpointNotFoundFault,

    -- ** DBSubnetGroupDoesNotCoverEnoughAZs
    _DBSubnetGroupDoesNotCoverEnoughAZs,

    -- ** SubnetAlreadyInUse
    _SubnetAlreadyInUse,

    -- ** DBClusterRoleNotFoundFault
    _DBClusterRoleNotFoundFault,

    -- * Waiters
    -- $waiters

    -- ** DBInstanceAvailable
    newDBInstanceAvailable,

    -- ** DBInstanceDeleted
    newDBInstanceDeleted,

    -- * Operations
    -- $operations

    -- ** StartDBCluster
    StartDBCluster (StartDBCluster'),
    newStartDBCluster,
    StartDBClusterResponse (StartDBClusterResponse'),
    newStartDBClusterResponse,

    -- ** DescribeDBClusterParameterGroups (Paginated)
    DescribeDBClusterParameterGroups (DescribeDBClusterParameterGroups'),
    newDescribeDBClusterParameterGroups,
    DescribeDBClusterParameterGroupsResponse (DescribeDBClusterParameterGroupsResponse'),
    newDescribeDBClusterParameterGroupsResponse,

    -- ** DescribeDBEngineVersions (Paginated)
    DescribeDBEngineVersions (DescribeDBEngineVersions'),
    newDescribeDBEngineVersions,
    DescribeDBEngineVersionsResponse (DescribeDBEngineVersionsResponse'),
    newDescribeDBEngineVersionsResponse,

    -- ** ModifyDBClusterEndpoint
    ModifyDBClusterEndpoint (ModifyDBClusterEndpoint'),
    newModifyDBClusterEndpoint,
    ModifyDBClusterEndpointResponse (ModifyDBClusterEndpointResponse'),
    newModifyDBClusterEndpointResponse,

    -- ** AddSourceIdentifierToSubscription
    AddSourceIdentifierToSubscription (AddSourceIdentifierToSubscription'),
    newAddSourceIdentifierToSubscription,
    AddSourceIdentifierToSubscriptionResponse (AddSourceIdentifierToSubscriptionResponse'),
    newAddSourceIdentifierToSubscriptionResponse,

    -- ** ModifyDBInstance
    ModifyDBInstance (ModifyDBInstance'),
    newModifyDBInstance,
    ModifyDBInstanceResponse (ModifyDBInstanceResponse'),
    newModifyDBInstanceResponse,

    -- ** ModifyEventSubscription
    ModifyEventSubscription (ModifyEventSubscription'),
    newModifyEventSubscription,
    ModifyEventSubscriptionResponse (ModifyEventSubscriptionResponse'),
    newModifyEventSubscriptionResponse,

    -- ** ResetDBClusterParameterGroup
    ResetDBClusterParameterGroup (ResetDBClusterParameterGroup'),
    newResetDBClusterParameterGroup,
    DBClusterParameterGroupNameMessage (DBClusterParameterGroupNameMessage'),
    newDBClusterParameterGroupNameMessage,

    -- ** DescribeEvents (Paginated)
    DescribeEvents (DescribeEvents'),
    newDescribeEvents,
    DescribeEventsResponse (DescribeEventsResponse'),
    newDescribeEventsResponse,

    -- ** DescribeEngineDefaultParameters (Paginated)
    DescribeEngineDefaultParameters (DescribeEngineDefaultParameters'),
    newDescribeEngineDefaultParameters,
    DescribeEngineDefaultParametersResponse (DescribeEngineDefaultParametersResponse'),
    newDescribeEngineDefaultParametersResponse,

    -- ** DescribeDBClusters (Paginated)
    DescribeDBClusters (DescribeDBClusters'),
    newDescribeDBClusters,
    DescribeDBClustersResponse (DescribeDBClustersResponse'),
    newDescribeDBClustersResponse,

    -- ** ModifyDBSubnetGroup
    ModifyDBSubnetGroup (ModifyDBSubnetGroup'),
    newModifyDBSubnetGroup,
    ModifyDBSubnetGroupResponse (ModifyDBSubnetGroupResponse'),
    newModifyDBSubnetGroupResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** DeleteDBCluster
    DeleteDBCluster (DeleteDBCluster'),
    newDeleteDBCluster,
    DeleteDBClusterResponse (DeleteDBClusterResponse'),
    newDeleteDBClusterResponse,

    -- ** CopyDBParameterGroup
    CopyDBParameterGroup (CopyDBParameterGroup'),
    newCopyDBParameterGroup,
    CopyDBParameterGroupResponse (CopyDBParameterGroupResponse'),
    newCopyDBParameterGroupResponse,

    -- ** RemoveSourceIdentifierFromSubscription
    RemoveSourceIdentifierFromSubscription (RemoveSourceIdentifierFromSubscription'),
    newRemoveSourceIdentifierFromSubscription,
    RemoveSourceIdentifierFromSubscriptionResponse (RemoveSourceIdentifierFromSubscriptionResponse'),
    newRemoveSourceIdentifierFromSubscriptionResponse,

    -- ** DescribeEngineDefaultClusterParameters
    DescribeEngineDefaultClusterParameters (DescribeEngineDefaultClusterParameters'),
    newDescribeEngineDefaultClusterParameters,
    DescribeEngineDefaultClusterParametersResponse (DescribeEngineDefaultClusterParametersResponse'),
    newDescribeEngineDefaultClusterParametersResponse,

    -- ** PromoteReadReplicaDBCluster
    PromoteReadReplicaDBCluster (PromoteReadReplicaDBCluster'),
    newPromoteReadReplicaDBCluster,
    PromoteReadReplicaDBClusterResponse (PromoteReadReplicaDBClusterResponse'),
    newPromoteReadReplicaDBClusterResponse,

    -- ** RemoveTagsFromResource
    RemoveTagsFromResource (RemoveTagsFromResource'),
    newRemoveTagsFromResource,
    RemoveTagsFromResourceResponse (RemoveTagsFromResourceResponse'),
    newRemoveTagsFromResourceResponse,

    -- ** CreateEventSubscription
    CreateEventSubscription (CreateEventSubscription'),
    newCreateEventSubscription,
    CreateEventSubscriptionResponse (CreateEventSubscriptionResponse'),
    newCreateEventSubscriptionResponse,

    -- ** CreateDBInstance
    CreateDBInstance (CreateDBInstance'),
    newCreateDBInstance,
    CreateDBInstanceResponse (CreateDBInstanceResponse'),
    newCreateDBInstanceResponse,

    -- ** DeleteDBClusterParameterGroup
    DeleteDBClusterParameterGroup (DeleteDBClusterParameterGroup'),
    newDeleteDBClusterParameterGroup,
    DeleteDBClusterParameterGroupResponse (DeleteDBClusterParameterGroupResponse'),
    newDeleteDBClusterParameterGroupResponse,

    -- ** CreateDBClusterEndpoint
    CreateDBClusterEndpoint (CreateDBClusterEndpoint'),
    newCreateDBClusterEndpoint,
    CreateDBClusterEndpointResponse (CreateDBClusterEndpointResponse'),
    newCreateDBClusterEndpointResponse,

    -- ** RestoreDBClusterFromSnapshot
    RestoreDBClusterFromSnapshot (RestoreDBClusterFromSnapshot'),
    newRestoreDBClusterFromSnapshot,
    RestoreDBClusterFromSnapshotResponse (RestoreDBClusterFromSnapshotResponse'),
    newRestoreDBClusterFromSnapshotResponse,

    -- ** DescribeOrderableDBInstanceOptions (Paginated)
    DescribeOrderableDBInstanceOptions (DescribeOrderableDBInstanceOptions'),
    newDescribeOrderableDBInstanceOptions,
    DescribeOrderableDBInstanceOptionsResponse (DescribeOrderableDBInstanceOptionsResponse'),
    newDescribeOrderableDBInstanceOptionsResponse,

    -- ** DeleteDBClusterEndpoint
    DeleteDBClusterEndpoint (DeleteDBClusterEndpoint'),
    newDeleteDBClusterEndpoint,
    DeleteDBClusterEndpointResponse (DeleteDBClusterEndpointResponse'),
    newDeleteDBClusterEndpointResponse,

    -- ** CreateDBClusterParameterGroup
    CreateDBClusterParameterGroup (CreateDBClusterParameterGroup'),
    newCreateDBClusterParameterGroup,
    CreateDBClusterParameterGroupResponse (CreateDBClusterParameterGroupResponse'),
    newCreateDBClusterParameterGroupResponse,

    -- ** DeleteEventSubscription
    DeleteEventSubscription (DeleteEventSubscription'),
    newDeleteEventSubscription,
    DeleteEventSubscriptionResponse (DeleteEventSubscriptionResponse'),
    newDeleteEventSubscriptionResponse,

    -- ** DescribeDBParameterGroups (Paginated)
    DescribeDBParameterGroups (DescribeDBParameterGroups'),
    newDescribeDBParameterGroups,
    DescribeDBParameterGroupsResponse (DescribeDBParameterGroupsResponse'),
    newDescribeDBParameterGroupsResponse,

    -- ** DeleteDBClusterSnapshot
    DeleteDBClusterSnapshot (DeleteDBClusterSnapshot'),
    newDeleteDBClusterSnapshot,
    DeleteDBClusterSnapshotResponse (DeleteDBClusterSnapshotResponse'),
    newDeleteDBClusterSnapshotResponse,

    -- ** DescribeValidDBInstanceModifications
    DescribeValidDBInstanceModifications (DescribeValidDBInstanceModifications'),
    newDescribeValidDBInstanceModifications,
    DescribeValidDBInstanceModificationsResponse (DescribeValidDBInstanceModificationsResponse'),
    newDescribeValidDBInstanceModificationsResponse,

    -- ** DescribeDBClusterEndpoints (Paginated)
    DescribeDBClusterEndpoints (DescribeDBClusterEndpoints'),
    newDescribeDBClusterEndpoints,
    DescribeDBClusterEndpointsResponse (DescribeDBClusterEndpointsResponse'),
    newDescribeDBClusterEndpointsResponse,

    -- ** DescribeEventSubscriptions (Paginated)
    DescribeEventSubscriptions (DescribeEventSubscriptions'),
    newDescribeEventSubscriptions,
    DescribeEventSubscriptionsResponse (DescribeEventSubscriptionsResponse'),
    newDescribeEventSubscriptionsResponse,

    -- ** AddTagsToResource
    AddTagsToResource (AddTagsToResource'),
    newAddTagsToResource,
    AddTagsToResourceResponse (AddTagsToResourceResponse'),
    newAddTagsToResourceResponse,

    -- ** DescribeDBParameters (Paginated)
    DescribeDBParameters (DescribeDBParameters'),
    newDescribeDBParameters,
    DescribeDBParametersResponse (DescribeDBParametersResponse'),
    newDescribeDBParametersResponse,

    -- ** CreateDBClusterSnapshot
    CreateDBClusterSnapshot (CreateDBClusterSnapshot'),
    newCreateDBClusterSnapshot,
    CreateDBClusterSnapshotResponse (CreateDBClusterSnapshotResponse'),
    newCreateDBClusterSnapshotResponse,

    -- ** DescribeDBSubnetGroups (Paginated)
    DescribeDBSubnetGroups (DescribeDBSubnetGroups'),
    newDescribeDBSubnetGroups,
    DescribeDBSubnetGroupsResponse (DescribeDBSubnetGroupsResponse'),
    newDescribeDBSubnetGroupsResponse,

    -- ** StopDBCluster
    StopDBCluster (StopDBCluster'),
    newStopDBCluster,
    StopDBClusterResponse (StopDBClusterResponse'),
    newStopDBClusterResponse,

    -- ** CreateDBParameterGroup
    CreateDBParameterGroup (CreateDBParameterGroup'),
    newCreateDBParameterGroup,
    CreateDBParameterGroupResponse (CreateDBParameterGroupResponse'),
    newCreateDBParameterGroupResponse,

    -- ** ModifyDBClusterSnapshotAttribute
    ModifyDBClusterSnapshotAttribute (ModifyDBClusterSnapshotAttribute'),
    newModifyDBClusterSnapshotAttribute,
    ModifyDBClusterSnapshotAttributeResponse (ModifyDBClusterSnapshotAttributeResponse'),
    newModifyDBClusterSnapshotAttributeResponse,

    -- ** ModifyDBCluster
    ModifyDBCluster (ModifyDBCluster'),
    newModifyDBCluster,
    ModifyDBClusterResponse (ModifyDBClusterResponse'),
    newModifyDBClusterResponse,

    -- ** CopyDBClusterParameterGroup
    CopyDBClusterParameterGroup (CopyDBClusterParameterGroup'),
    newCopyDBClusterParameterGroup,
    CopyDBClusterParameterGroupResponse (CopyDBClusterParameterGroupResponse'),
    newCopyDBClusterParameterGroupResponse,

    -- ** DescribeEventCategories
    DescribeEventCategories (DescribeEventCategories'),
    newDescribeEventCategories,
    DescribeEventCategoriesResponse (DescribeEventCategoriesResponse'),
    newDescribeEventCategoriesResponse,

    -- ** ModifyDBClusterParameterGroup
    ModifyDBClusterParameterGroup (ModifyDBClusterParameterGroup'),
    newModifyDBClusterParameterGroup,
    DBClusterParameterGroupNameMessage (DBClusterParameterGroupNameMessage'),
    newDBClusterParameterGroupNameMessage,

    -- ** DescribeDBClusterSnapshotAttributes
    DescribeDBClusterSnapshotAttributes (DescribeDBClusterSnapshotAttributes'),
    newDescribeDBClusterSnapshotAttributes,
    DescribeDBClusterSnapshotAttributesResponse (DescribeDBClusterSnapshotAttributesResponse'),
    newDescribeDBClusterSnapshotAttributesResponse,

    -- ** DescribePendingMaintenanceActions (Paginated)
    DescribePendingMaintenanceActions (DescribePendingMaintenanceActions'),
    newDescribePendingMaintenanceActions,
    DescribePendingMaintenanceActionsResponse (DescribePendingMaintenanceActionsResponse'),
    newDescribePendingMaintenanceActionsResponse,

    -- ** AddRoleToDBCluster
    AddRoleToDBCluster (AddRoleToDBCluster'),
    newAddRoleToDBCluster,
    AddRoleToDBClusterResponse (AddRoleToDBClusterResponse'),
    newAddRoleToDBClusterResponse,

    -- ** CopyDBClusterSnapshot
    CopyDBClusterSnapshot (CopyDBClusterSnapshot'),
    newCopyDBClusterSnapshot,
    CopyDBClusterSnapshotResponse (CopyDBClusterSnapshotResponse'),
    newCopyDBClusterSnapshotResponse,

    -- ** ResetDBParameterGroup
    ResetDBParameterGroup (ResetDBParameterGroup'),
    newResetDBParameterGroup,
    DBParameterGroupNameMessage (DBParameterGroupNameMessage'),
    newDBParameterGroupNameMessage,

    -- ** CreateDBCluster
    CreateDBCluster (CreateDBCluster'),
    newCreateDBCluster,
    CreateDBClusterResponse (CreateDBClusterResponse'),
    newCreateDBClusterResponse,

    -- ** RemoveRoleFromDBCluster
    RemoveRoleFromDBCluster (RemoveRoleFromDBCluster'),
    newRemoveRoleFromDBCluster,
    RemoveRoleFromDBClusterResponse (RemoveRoleFromDBClusterResponse'),
    newRemoveRoleFromDBClusterResponse,

    -- ** FailoverDBCluster
    FailoverDBCluster (FailoverDBCluster'),
    newFailoverDBCluster,
    FailoverDBClusterResponse (FailoverDBClusterResponse'),
    newFailoverDBClusterResponse,

    -- ** ModifyDBParameterGroup
    ModifyDBParameterGroup (ModifyDBParameterGroup'),
    newModifyDBParameterGroup,
    DBParameterGroupNameMessage (DBParameterGroupNameMessage'),
    newDBParameterGroupNameMessage,

    -- ** ApplyPendingMaintenanceAction
    ApplyPendingMaintenanceAction (ApplyPendingMaintenanceAction'),
    newApplyPendingMaintenanceAction,
    ApplyPendingMaintenanceActionResponse (ApplyPendingMaintenanceActionResponse'),
    newApplyPendingMaintenanceActionResponse,

    -- ** DescribeDBClusterParameters (Paginated)
    DescribeDBClusterParameters (DescribeDBClusterParameters'),
    newDescribeDBClusterParameters,
    DescribeDBClusterParametersResponse (DescribeDBClusterParametersResponse'),
    newDescribeDBClusterParametersResponse,

    -- ** DeleteDBSubnetGroup
    DeleteDBSubnetGroup (DeleteDBSubnetGroup'),
    newDeleteDBSubnetGroup,
    DeleteDBSubnetGroupResponse (DeleteDBSubnetGroupResponse'),
    newDeleteDBSubnetGroupResponse,

    -- ** DescribeDBClusterSnapshots (Paginated)
    DescribeDBClusterSnapshots (DescribeDBClusterSnapshots'),
    newDescribeDBClusterSnapshots,
    DescribeDBClusterSnapshotsResponse (DescribeDBClusterSnapshotsResponse'),
    newDescribeDBClusterSnapshotsResponse,

    -- ** RebootDBInstance
    RebootDBInstance (RebootDBInstance'),
    newRebootDBInstance,
    RebootDBInstanceResponse (RebootDBInstanceResponse'),
    newRebootDBInstanceResponse,

    -- ** CreateDBSubnetGroup
    CreateDBSubnetGroup (CreateDBSubnetGroup'),
    newCreateDBSubnetGroup,
    CreateDBSubnetGroupResponse (CreateDBSubnetGroupResponse'),
    newCreateDBSubnetGroupResponse,

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

    -- ** RestoreDBClusterToPointInTime
    RestoreDBClusterToPointInTime (RestoreDBClusterToPointInTime'),
    newRestoreDBClusterToPointInTime,
    RestoreDBClusterToPointInTimeResponse (RestoreDBClusterToPointInTimeResponse'),
    newRestoreDBClusterToPointInTimeResponse,

    -- ** DescribeDBInstances (Paginated)
    DescribeDBInstances (DescribeDBInstances'),
    newDescribeDBInstances,
    DescribeDBInstancesResponse (DescribeDBInstancesResponse'),
    newDescribeDBInstancesResponse,

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

import Network.AWS.Neptune.AddRoleToDBCluster
import Network.AWS.Neptune.AddSourceIdentifierToSubscription
import Network.AWS.Neptune.AddTagsToResource
import Network.AWS.Neptune.ApplyPendingMaintenanceAction
import Network.AWS.Neptune.CopyDBClusterParameterGroup
import Network.AWS.Neptune.CopyDBClusterSnapshot
import Network.AWS.Neptune.CopyDBParameterGroup
import Network.AWS.Neptune.CreateDBCluster
import Network.AWS.Neptune.CreateDBClusterEndpoint
import Network.AWS.Neptune.CreateDBClusterParameterGroup
import Network.AWS.Neptune.CreateDBClusterSnapshot
import Network.AWS.Neptune.CreateDBInstance
import Network.AWS.Neptune.CreateDBParameterGroup
import Network.AWS.Neptune.CreateDBSubnetGroup
import Network.AWS.Neptune.CreateEventSubscription
import Network.AWS.Neptune.DeleteDBCluster
import Network.AWS.Neptune.DeleteDBClusterEndpoint
import Network.AWS.Neptune.DeleteDBClusterParameterGroup
import Network.AWS.Neptune.DeleteDBClusterSnapshot
import Network.AWS.Neptune.DeleteDBInstance
import Network.AWS.Neptune.DeleteDBParameterGroup
import Network.AWS.Neptune.DeleteDBSubnetGroup
import Network.AWS.Neptune.DeleteEventSubscription
import Network.AWS.Neptune.DescribeDBClusterEndpoints
import Network.AWS.Neptune.DescribeDBClusterParameterGroups
import Network.AWS.Neptune.DescribeDBClusterParameters
import Network.AWS.Neptune.DescribeDBClusterSnapshotAttributes
import Network.AWS.Neptune.DescribeDBClusterSnapshots
import Network.AWS.Neptune.DescribeDBClusters
import Network.AWS.Neptune.DescribeDBEngineVersions
import Network.AWS.Neptune.DescribeDBInstances
import Network.AWS.Neptune.DescribeDBParameterGroups
import Network.AWS.Neptune.DescribeDBParameters
import Network.AWS.Neptune.DescribeDBSubnetGroups
import Network.AWS.Neptune.DescribeEngineDefaultClusterParameters
import Network.AWS.Neptune.DescribeEngineDefaultParameters
import Network.AWS.Neptune.DescribeEventCategories
import Network.AWS.Neptune.DescribeEventSubscriptions
import Network.AWS.Neptune.DescribeEvents
import Network.AWS.Neptune.DescribeOrderableDBInstanceOptions
import Network.AWS.Neptune.DescribePendingMaintenanceActions
import Network.AWS.Neptune.DescribeValidDBInstanceModifications
import Network.AWS.Neptune.FailoverDBCluster
import Network.AWS.Neptune.Lens
import Network.AWS.Neptune.ListTagsForResource
import Network.AWS.Neptune.ModifyDBCluster
import Network.AWS.Neptune.ModifyDBClusterEndpoint
import Network.AWS.Neptune.ModifyDBClusterParameterGroup
import Network.AWS.Neptune.ModifyDBClusterSnapshotAttribute
import Network.AWS.Neptune.ModifyDBInstance
import Network.AWS.Neptune.ModifyDBParameterGroup
import Network.AWS.Neptune.ModifyDBSubnetGroup
import Network.AWS.Neptune.ModifyEventSubscription
import Network.AWS.Neptune.PromoteReadReplicaDBCluster
import Network.AWS.Neptune.RebootDBInstance
import Network.AWS.Neptune.RemoveRoleFromDBCluster
import Network.AWS.Neptune.RemoveSourceIdentifierFromSubscription
import Network.AWS.Neptune.RemoveTagsFromResource
import Network.AWS.Neptune.ResetDBClusterParameterGroup
import Network.AWS.Neptune.ResetDBParameterGroup
import Network.AWS.Neptune.RestoreDBClusterFromSnapshot
import Network.AWS.Neptune.RestoreDBClusterToPointInTime
import Network.AWS.Neptune.StartDBCluster
import Network.AWS.Neptune.StopDBCluster
import Network.AWS.Neptune.Types
import Network.AWS.Neptune.Waiters

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
