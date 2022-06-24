{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.Neptune
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
module Amazonka.Neptune
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** OptionGroupNotFoundFault
    _OptionGroupNotFoundFault,

    -- ** InsufficientDBInstanceCapacityFault
    _InsufficientDBInstanceCapacityFault,

    -- ** InvalidDBClusterSnapshotStateFault
    _InvalidDBClusterSnapshotStateFault,

    -- ** SubnetAlreadyInUse
    _SubnetAlreadyInUse,

    -- ** InvalidDBSecurityGroupStateFault
    _InvalidDBSecurityGroupStateFault,

    -- ** InvalidDBParameterGroupStateFault
    _InvalidDBParameterGroupStateFault,

    -- ** SnapshotQuotaExceededFault
    _SnapshotQuotaExceededFault,

    -- ** InvalidSubnet
    _InvalidSubnet,

    -- ** DBClusterAlreadyExistsFault
    _DBClusterAlreadyExistsFault,

    -- ** DBParameterGroupNotFoundFault
    _DBParameterGroupNotFoundFault,

    -- ** DBClusterEndpointAlreadyExistsFault
    _DBClusterEndpointAlreadyExistsFault,

    -- ** SubscriptionAlreadyExistFault
    _SubscriptionAlreadyExistFault,

    -- ** DBInstanceAlreadyExistsFault
    _DBInstanceAlreadyExistsFault,

    -- ** InvalidDBSubnetGroupStateFault
    _InvalidDBSubnetGroupStateFault,

    -- ** SNSInvalidTopicFault
    _SNSInvalidTopicFault,

    -- ** DBClusterEndpointNotFoundFault
    _DBClusterEndpointNotFoundFault,

    -- ** DBParameterGroupQuotaExceededFault
    _DBParameterGroupQuotaExceededFault,

    -- ** SubscriptionNotFoundFault
    _SubscriptionNotFoundFault,

    -- ** DBSnapshotNotFoundFault
    _DBSnapshotNotFoundFault,

    -- ** InvalidDBSubnetStateFault
    _InvalidDBSubnetStateFault,

    -- ** DBUpgradeDependencyFailureFault
    _DBUpgradeDependencyFailureFault,

    -- ** DBSubnetGroupAlreadyExistsFault
    _DBSubnetGroupAlreadyExistsFault,

    -- ** DBInstanceNotFoundFault
    _DBInstanceNotFoundFault,

    -- ** InstanceQuotaExceededFault
    _InstanceQuotaExceededFault,

    -- ** DBClusterRoleAlreadyExistsFault
    _DBClusterRoleAlreadyExistsFault,

    -- ** InvalidDBClusterStateFault
    _InvalidDBClusterStateFault,

    -- ** InvalidDBInstanceStateFault
    _InvalidDBInstanceStateFault,

    -- ** AuthorizationNotFoundFault
    _AuthorizationNotFoundFault,

    -- ** DBSubnetGroupQuotaExceededFault
    _DBSubnetGroupQuotaExceededFault,

    -- ** InsufficientStorageClusterCapacityFault
    _InsufficientStorageClusterCapacityFault,

    -- ** SubscriptionCategoryNotFoundFault
    _SubscriptionCategoryNotFoundFault,

    -- ** SNSNoAuthorizationFault
    _SNSNoAuthorizationFault,

    -- ** KMSKeyNotAccessibleFault
    _KMSKeyNotAccessibleFault,

    -- ** SNSTopicArnNotFoundFault
    _SNSTopicArnNotFoundFault,

    -- ** DBSubnetGroupDoesNotCoverEnoughAZs
    _DBSubnetGroupDoesNotCoverEnoughAZs,

    -- ** StorageTypeNotSupportedFault
    _StorageTypeNotSupportedFault,

    -- ** CertificateNotFoundFault
    _CertificateNotFoundFault,

    -- ** DBClusterEndpointQuotaExceededFault
    _DBClusterEndpointQuotaExceededFault,

    -- ** EventSubscriptionQuotaExceededFault
    _EventSubscriptionQuotaExceededFault,

    -- ** DBSnapshotAlreadyExistsFault
    _DBSnapshotAlreadyExistsFault,

    -- ** InvalidRestoreFault
    _InvalidRestoreFault,

    -- ** DBClusterQuotaExceededFault
    _DBClusterQuotaExceededFault,

    -- ** DBClusterParameterGroupNotFoundFault
    _DBClusterParameterGroupNotFoundFault,

    -- ** DBSubnetQuotaExceededFault
    _DBSubnetQuotaExceededFault,

    -- ** ResourceNotFoundFault
    _ResourceNotFoundFault,

    -- ** InsufficientDBClusterCapacityFault
    _InsufficientDBClusterCapacityFault,

    -- ** SourceNotFoundFault
    _SourceNotFoundFault,

    -- ** DBClusterSnapshotAlreadyExistsFault
    _DBClusterSnapshotAlreadyExistsFault,

    -- ** DBParameterGroupAlreadyExistsFault
    _DBParameterGroupAlreadyExistsFault,

    -- ** InvalidVPCNetworkStateFault
    _InvalidVPCNetworkStateFault,

    -- ** DBClusterNotFoundFault
    _DBClusterNotFoundFault,

    -- ** DBClusterRoleNotFoundFault
    _DBClusterRoleNotFoundFault,

    -- ** ProvisionedIopsNotAvailableInAZFault
    _ProvisionedIopsNotAvailableInAZFault,

    -- ** InvalidDBClusterEndpointStateFault
    _InvalidDBClusterEndpointStateFault,

    -- ** InvalidEventSubscriptionStateFault
    _InvalidEventSubscriptionStateFault,

    -- ** StorageQuotaExceededFault
    _StorageQuotaExceededFault,

    -- ** DBSubnetGroupNotFoundFault
    _DBSubnetGroupNotFoundFault,

    -- ** DBClusterRoleQuotaExceededFault
    _DBClusterRoleQuotaExceededFault,

    -- ** DBSecurityGroupNotFoundFault
    _DBSecurityGroupNotFoundFault,

    -- ** SharedSnapshotQuotaExceededFault
    _SharedSnapshotQuotaExceededFault,

    -- ** DBClusterSnapshotNotFoundFault
    _DBClusterSnapshotNotFoundFault,

    -- ** InvalidDBSnapshotStateFault
    _InvalidDBSnapshotStateFault,

    -- ** DomainNotFoundFault
    _DomainNotFoundFault,

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
import Amazonka.Neptune.DeleteDBCluster
import Amazonka.Neptune.DeleteDBClusterEndpoint
import Amazonka.Neptune.DeleteDBClusterParameterGroup
import Amazonka.Neptune.DeleteDBClusterSnapshot
import Amazonka.Neptune.DeleteDBInstance
import Amazonka.Neptune.DeleteDBParameterGroup
import Amazonka.Neptune.DeleteDBSubnetGroup
import Amazonka.Neptune.DeleteEventSubscription
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
import Amazonka.Neptune.DescribeOrderableDBInstanceOptions
import Amazonka.Neptune.DescribePendingMaintenanceActions
import Amazonka.Neptune.DescribeValidDBInstanceModifications
import Amazonka.Neptune.FailoverDBCluster
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
import Amazonka.Neptune.PromoteReadReplicaDBCluster
import Amazonka.Neptune.RebootDBInstance
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
