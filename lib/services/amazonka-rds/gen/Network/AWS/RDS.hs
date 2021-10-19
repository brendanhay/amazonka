{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Network.AWS.RDS
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2014-10-31@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- Amazon Relational Database Service
--
-- Amazon Relational Database Service (Amazon RDS) is a web service that
-- makes it easier to set up, operate, and scale a relational database in
-- the cloud. It provides cost-efficient, resizeable capacity for an
-- industry-standard relational database and manages common database
-- administration tasks, freeing up developers to focus on what makes their
-- applications and businesses unique.
--
-- Amazon RDS gives you access to the capabilities of a MySQL, MariaDB,
-- PostgreSQL, Microsoft SQL Server, Oracle, or Amazon Aurora database
-- server. These capabilities mean that the code, applications, and tools
-- you already use today with your existing databases work with Amazon RDS
-- without modification. Amazon RDS automatically backs up your database
-- and maintains the database software that powers your DB instance. Amazon
-- RDS is flexible: you can scale your DB instance\'s compute resources and
-- storage capacity to meet your application\'s demand. As with all Amazon
-- Web Services, there are no up-front investments, and you pay only for
-- the resources you use.
--
-- This interface reference for Amazon RDS contains documentation for a
-- programming or command line interface you can use to manage Amazon RDS.
-- Amazon RDS is asynchronous, which means that some interfaces might
-- require techniques such as polling or callback functions to determine
-- when a command has been applied. In this reference, the parameter
-- descriptions indicate whether a command is applied immediately, on the
-- next instance reboot, or during the maintenance window. The reference
-- structure is as follows, and we list following some related topics from
-- the user guide.
--
-- __Amazon RDS API Reference__
--
-- -   For the alphabetical list of API actions, see
--     <https://docs.aws.amazon.com/AmazonRDS/latest/APIReference/API_Operations.html API Actions>.
--
-- -   For the alphabetical list of data types, see
--     <https://docs.aws.amazon.com/AmazonRDS/latest/APIReference/API_Types.html Data Types>.
--
-- -   For a list of common query parameters, see
--     <https://docs.aws.amazon.com/AmazonRDS/latest/APIReference/CommonParameters.html Common Parameters>.
--
-- -   For descriptions of the error codes, see
--     <https://docs.aws.amazon.com/AmazonRDS/latest/APIReference/CommonErrors.html Common Errors>.
--
-- __Amazon RDS User Guide__
--
-- -   For a summary of the Amazon RDS interfaces, see
--     <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Welcome.html#Welcome.Interfaces Available RDS Interfaces>.
--
-- -   For more information about how to use the Query API, see
--     <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Using_the_Query_API.html Using the Query API>.
module Network.AWS.RDS
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** PointInTimeRestoreNotEnabledFault
    _PointInTimeRestoreNotEnabledFault,

    -- ** InvalidDBParameterGroupStateFault
    _InvalidDBParameterGroupStateFault,

    -- ** ReservedDBInstanceQuotaExceededFault
    _ReservedDBInstanceQuotaExceededFault,

    -- ** SourceNotFoundFault
    _SourceNotFoundFault,

    -- ** CertificateNotFoundFault
    _CertificateNotFoundFault,

    -- ** AuthorizationQuotaExceededFault
    _AuthorizationQuotaExceededFault,

    -- ** DBClusterSnapshotAlreadyExistsFault
    _DBClusterSnapshotAlreadyExistsFault,

    -- ** DBParameterGroupAlreadyExistsFault
    _DBParameterGroupAlreadyExistsFault,

    -- ** DBInstanceRoleQuotaExceededFault
    _DBInstanceRoleQuotaExceededFault,

    -- ** DBInstanceRoleAlreadyExistsFault
    _DBInstanceRoleAlreadyExistsFault,

    -- ** DBParameterGroupQuotaExceededFault
    _DBParameterGroupQuotaExceededFault,

    -- ** BackupPolicyNotFoundFault
    _BackupPolicyNotFoundFault,

    -- ** InsufficientDBClusterCapacityFault
    _InsufficientDBClusterCapacityFault,

    -- ** ReservedDBInstanceAlreadyExistsFault
    _ReservedDBInstanceAlreadyExistsFault,

    -- ** ProvisionedIopsNotAvailableInAZFault
    _ProvisionedIopsNotAvailableInAZFault,

    -- ** DBProxyTargetAlreadyRegisteredFault
    _DBProxyTargetAlreadyRegisteredFault,

    -- ** AuthorizationAlreadyExistsFault
    _AuthorizationAlreadyExistsFault,

    -- ** SubscriptionCategoryNotFoundFault
    _SubscriptionCategoryNotFoundFault,

    -- ** DBProxyTargetNotFoundFault
    _DBProxyTargetNotFoundFault,

    -- ** SubscriptionNotFoundFault
    _SubscriptionNotFoundFault,

    -- ** InvalidSubnet
    _InvalidSubnet,

    -- ** SharedSnapshotQuotaExceededFault
    _SharedSnapshotQuotaExceededFault,

    -- ** DBSubnetQuotaExceededFault
    _DBSubnetQuotaExceededFault,

    -- ** GlobalClusterAlreadyExistsFault
    _GlobalClusterAlreadyExistsFault,

    -- ** OptionGroupNotFoundFault
    _OptionGroupNotFoundFault,

    -- ** DBClusterNotFoundFault
    _DBClusterNotFoundFault,

    -- ** InvalidDBProxyEndpointStateFault
    _InvalidDBProxyEndpointStateFault,

    -- ** DBLogFileNotFoundFault
    _DBLogFileNotFoundFault,

    -- ** DBProxyTargetGroupNotFoundFault
    _DBProxyTargetGroupNotFoundFault,

    -- ** InvalidS3BucketFault
    _InvalidS3BucketFault,

    -- ** DBProxyQuotaExceededFault
    _DBProxyQuotaExceededFault,

    -- ** IamRoleNotFoundFault
    _IamRoleNotFoundFault,

    -- ** DBClusterAlreadyExistsFault
    _DBClusterAlreadyExistsFault,

    -- ** StorageTypeNotSupportedFault
    _StorageTypeNotSupportedFault,

    -- ** DBSecurityGroupQuotaExceededFault
    _DBSecurityGroupQuotaExceededFault,

    -- ** DBProxyEndpointNotFoundFault
    _DBProxyEndpointNotFoundFault,

    -- ** OptionGroupAlreadyExistsFault
    _OptionGroupAlreadyExistsFault,

    -- ** ExportTaskNotFoundFault
    _ExportTaskNotFoundFault,

    -- ** InsufficientAvailableIPsInSubnetFault
    _InsufficientAvailableIPsInSubnetFault,

    -- ** DBProxyNotFoundFault
    _DBProxyNotFoundFault,

    -- ** OptionGroupQuotaExceededFault
    _OptionGroupQuotaExceededFault,

    -- ** DBSecurityGroupAlreadyExistsFault
    _DBSecurityGroupAlreadyExistsFault,

    -- ** SNSTopicArnNotFoundFault
    _SNSTopicArnNotFoundFault,

    -- ** InvalidDBClusterEndpointStateFault
    _InvalidDBClusterEndpointStateFault,

    -- ** InvalidEventSubscriptionStateFault
    _InvalidEventSubscriptionStateFault,

    -- ** InvalidDBInstanceAutomatedBackupStateFault
    _InvalidDBInstanceAutomatedBackupStateFault,

    -- ** KMSKeyNotAccessibleFault
    _KMSKeyNotAccessibleFault,

    -- ** DBSnapshotNotFoundFault
    _DBSnapshotNotFoundFault,

    -- ** DBClusterParameterGroupNotFoundFault
    _DBClusterParameterGroupNotFoundFault,

    -- ** DBClusterQuotaExceededFault
    _DBClusterQuotaExceededFault,

    -- ** InvalidExportOnlyFault
    _InvalidExportOnlyFault,

    -- ** SnapshotQuotaExceededFault
    _SnapshotQuotaExceededFault,

    -- ** InvalidDBClusterCapacityFault
    _InvalidDBClusterCapacityFault,

    -- ** DBSubnetGroupAlreadyExistsFault
    _DBSubnetGroupAlreadyExistsFault,

    -- ** SNSNoAuthorizationFault
    _SNSNoAuthorizationFault,

    -- ** DBSecurityGroupNotFoundFault
    _DBSecurityGroupNotFoundFault,

    -- ** DBSecurityGroupNotSupportedFault
    _DBSecurityGroupNotSupportedFault,

    -- ** InvalidDBProxyStateFault
    _InvalidDBProxyStateFault,

    -- ** DBProxyEndpointQuotaExceededFault
    _DBProxyEndpointQuotaExceededFault,

    -- ** InstanceQuotaExceededFault
    _InstanceQuotaExceededFault,

    -- ** DBClusterBacktrackNotFoundFault
    _DBClusterBacktrackNotFoundFault,

    -- ** DomainNotFoundFault
    _DomainNotFoundFault,

    -- ** DBParameterGroupNotFoundFault
    _DBParameterGroupNotFoundFault,

    -- ** InvalidDBSubnetGroupFault
    _InvalidDBSubnetGroupFault,

    -- ** ReservedDBInstancesOfferingNotFoundFault
    _ReservedDBInstancesOfferingNotFoundFault,

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

    -- ** InstallationMediaAlreadyExistsFault
    _InstallationMediaAlreadyExistsFault,

    -- ** SubscriptionAlreadyExistFault
    _SubscriptionAlreadyExistFault,

    -- ** DBClusterRoleAlreadyExistsFault
    _DBClusterRoleAlreadyExistsFault,

    -- ** IamRoleMissingPermissionsFault
    _IamRoleMissingPermissionsFault,

    -- ** DBClusterRoleQuotaExceededFault
    _DBClusterRoleQuotaExceededFault,

    -- ** InvalidVPCNetworkStateFault
    _InvalidVPCNetworkStateFault,

    -- ** DBInstanceRoleNotFoundFault
    _DBInstanceRoleNotFoundFault,

    -- ** AuthorizationNotFoundFault
    _AuthorizationNotFoundFault,

    -- ** ReservedDBInstanceNotFoundFault
    _ReservedDBInstanceNotFoundFault,

    -- ** DBSubnetGroupQuotaExceededFault
    _DBSubnetGroupQuotaExceededFault,

    -- ** CustomAvailabilityZoneNotFoundFault
    _CustomAvailabilityZoneNotFoundFault,

    -- ** DBProxyEndpointAlreadyExistsFault
    _DBProxyEndpointAlreadyExistsFault,

    -- ** InvalidGlobalClusterStateFault
    _InvalidGlobalClusterStateFault,

    -- ** DBSubnetGroupNotAllowedFault
    _DBSubnetGroupNotAllowedFault,

    -- ** InvalidExportTaskStateFault
    _InvalidExportTaskStateFault,

    -- ** InvalidExportSourceStateFault
    _InvalidExportSourceStateFault,

    -- ** ExportTaskAlreadyExistsFault
    _ExportTaskAlreadyExistsFault,

    -- ** EventSubscriptionQuotaExceededFault
    _EventSubscriptionQuotaExceededFault,

    -- ** InsufficientStorageClusterCapacityFault
    _InsufficientStorageClusterCapacityFault,

    -- ** DBClusterEndpointQuotaExceededFault
    _DBClusterEndpointQuotaExceededFault,

    -- ** InvalidOptionGroupStateFault
    _InvalidOptionGroupStateFault,

    -- ** DBInstanceAutomatedBackupQuotaExceededFault
    _DBInstanceAutomatedBackupQuotaExceededFault,

    -- ** CustomAvailabilityZoneAlreadyExistsFault
    _CustomAvailabilityZoneAlreadyExistsFault,

    -- ** InvalidDBClusterStateFault
    _InvalidDBClusterStateFault,

    -- ** GlobalClusterNotFoundFault
    _GlobalClusterNotFoundFault,

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

    -- ** CustomAvailabilityZoneQuotaExceededFault
    _CustomAvailabilityZoneQuotaExceededFault,

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

    -- ** DBProxyAlreadyExistsFault
    _DBProxyAlreadyExistsFault,

    -- ** DBInstanceAutomatedBackupNotFoundFault
    _DBInstanceAutomatedBackupNotFoundFault,

    -- ** InvalidDBSnapshotStateFault
    _InvalidDBSnapshotStateFault,

    -- ** InvalidDBSubnetGroupStateFault
    _InvalidDBSubnetGroupStateFault,

    -- ** GlobalClusterQuotaExceededFault
    _GlobalClusterQuotaExceededFault,

    -- ** DBClusterEndpointNotFoundFault
    _DBClusterEndpointNotFoundFault,

    -- ** InstallationMediaNotFoundFault
    _InstallationMediaNotFoundFault,

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

    -- ** DBSnapshotCompleted
    newDBSnapshotCompleted,

    -- ** DBSnapshotDeleted
    newDBSnapshotDeleted,

    -- ** DBInstanceDeleted
    newDBInstanceDeleted,

    -- ** DBClusterSnapshotDeleted
    newDBClusterSnapshotDeleted,

    -- ** DBSnapshotAvailable
    newDBSnapshotAvailable,

    -- ** DBClusterSnapshotAvailable
    newDBClusterSnapshotAvailable,

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

    -- ** PromoteReadReplica
    PromoteReadReplica (PromoteReadReplica'),
    newPromoteReadReplica,
    PromoteReadReplicaResponse (PromoteReadReplicaResponse'),
    newPromoteReadReplicaResponse,

    -- ** DescribeDBEngineVersions (Paginated)
    DescribeDBEngineVersions (DescribeDBEngineVersions'),
    newDescribeDBEngineVersions,
    DescribeDBEngineVersionsResponse (DescribeDBEngineVersionsResponse'),
    newDescribeDBEngineVersionsResponse,

    -- ** StopDBInstance
    StopDBInstance (StopDBInstance'),
    newStopDBInstance,
    StopDBInstanceResponse (StopDBInstanceResponse'),
    newStopDBInstanceResponse,

    -- ** ModifyDBClusterEndpoint
    ModifyDBClusterEndpoint (ModifyDBClusterEndpoint'),
    newModifyDBClusterEndpoint,
    DBClusterEndpoint (DBClusterEndpoint'),
    newDBClusterEndpoint,

    -- ** DescribeDBProxyEndpoints (Paginated)
    DescribeDBProxyEndpoints (DescribeDBProxyEndpoints'),
    newDescribeDBProxyEndpoints,
    DescribeDBProxyEndpointsResponse (DescribeDBProxyEndpointsResponse'),
    newDescribeDBProxyEndpointsResponse,

    -- ** CopyDBSnapshot
    CopyDBSnapshot (CopyDBSnapshot'),
    newCopyDBSnapshot,
    CopyDBSnapshotResponse (CopyDBSnapshotResponse'),
    newCopyDBSnapshotResponse,

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

    -- ** DescribeCustomAvailabilityZones (Paginated)
    DescribeCustomAvailabilityZones (DescribeCustomAvailabilityZones'),
    newDescribeCustomAvailabilityZones,
    DescribeCustomAvailabilityZonesResponse (DescribeCustomAvailabilityZonesResponse'),
    newDescribeCustomAvailabilityZonesResponse,

    -- ** RestoreDBClusterFromS
    RestoreDBClusterFromS (RestoreDBClusterFromS'),
    newRestoreDBClusterFromS,
    RestoreDBClusterFromSResponse (RestoreDBClusterFromSResponse'),
    newRestoreDBClusterFromSResponse,

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

    -- ** DescribeOptionGroups (Paginated)
    DescribeOptionGroups (DescribeOptionGroups'),
    newDescribeOptionGroups,
    DescribeOptionGroupsResponse (DescribeOptionGroupsResponse'),
    newDescribeOptionGroupsResponse,

    -- ** DescribeDBLogFiles (Paginated)
    DescribeDBLogFiles (DescribeDBLogFiles'),
    newDescribeDBLogFiles,
    DescribeDBLogFilesResponse (DescribeDBLogFilesResponse'),
    newDescribeDBLogFilesResponse,

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

    -- ** DeleteOptionGroup
    DeleteOptionGroup (DeleteOptionGroup'),
    newDeleteOptionGroup,
    DeleteOptionGroupResponse (DeleteOptionGroupResponse'),
    newDeleteOptionGroupResponse,

    -- ** CreateDBProxyEndpoint
    CreateDBProxyEndpoint (CreateDBProxyEndpoint'),
    newCreateDBProxyEndpoint,
    CreateDBProxyEndpointResponse (CreateDBProxyEndpointResponse'),
    newCreateDBProxyEndpointResponse,

    -- ** DeleteDBCluster
    DeleteDBCluster (DeleteDBCluster'),
    newDeleteDBCluster,
    DeleteDBClusterResponse (DeleteDBClusterResponse'),
    newDeleteDBClusterResponse,

    -- ** DescribeReservedDBInstances (Paginated)
    DescribeReservedDBInstances (DescribeReservedDBInstances'),
    newDescribeReservedDBInstances,
    DescribeReservedDBInstancesResponse (DescribeReservedDBInstancesResponse'),
    newDescribeReservedDBInstancesResponse,

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

    -- ** DeleteCustomAvailabilityZone
    DeleteCustomAvailabilityZone (DeleteCustomAvailabilityZone'),
    newDeleteCustomAvailabilityZone,
    DeleteCustomAvailabilityZoneResponse (DeleteCustomAvailabilityZoneResponse'),
    newDeleteCustomAvailabilityZoneResponse,

    -- ** DescribeDBProxyTargets (Paginated)
    DescribeDBProxyTargets (DescribeDBProxyTargets'),
    newDescribeDBProxyTargets,
    DescribeDBProxyTargetsResponse (DescribeDBProxyTargetsResponse'),
    newDescribeDBProxyTargetsResponse,

    -- ** StartDBInstanceAutomatedBackupsReplication
    StartDBInstanceAutomatedBackupsReplication (StartDBInstanceAutomatedBackupsReplication'),
    newStartDBInstanceAutomatedBackupsReplication,
    StartDBInstanceAutomatedBackupsReplicationResponse (StartDBInstanceAutomatedBackupsReplicationResponse'),
    newStartDBInstanceAutomatedBackupsReplicationResponse,

    -- ** DescribeEngineDefaultClusterParameters (Paginated)
    DescribeEngineDefaultClusterParameters (DescribeEngineDefaultClusterParameters'),
    newDescribeEngineDefaultClusterParameters,
    DescribeEngineDefaultClusterParametersResponse (DescribeEngineDefaultClusterParametersResponse'),
    newDescribeEngineDefaultClusterParametersResponse,

    -- ** DescribeDBSnapshotAttributes
    DescribeDBSnapshotAttributes (DescribeDBSnapshotAttributes'),
    newDescribeDBSnapshotAttributes,
    DescribeDBSnapshotAttributesResponse (DescribeDBSnapshotAttributesResponse'),
    newDescribeDBSnapshotAttributesResponse,

    -- ** CreateCustomAvailabilityZone
    CreateCustomAvailabilityZone (CreateCustomAvailabilityZone'),
    newCreateCustomAvailabilityZone,
    CreateCustomAvailabilityZoneResponse (CreateCustomAvailabilityZoneResponse'),
    newCreateCustomAvailabilityZoneResponse,

    -- ** BacktrackDBCluster
    BacktrackDBCluster (BacktrackDBCluster'),
    newBacktrackDBCluster,
    DBClusterBacktrack (DBClusterBacktrack'),
    newDBClusterBacktrack,

    -- ** DeleteGlobalCluster
    DeleteGlobalCluster (DeleteGlobalCluster'),
    newDeleteGlobalCluster,
    DeleteGlobalClusterResponse (DeleteGlobalClusterResponse'),
    newDeleteGlobalClusterResponse,

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

    -- ** RestoreDBInstanceFromDBSnapshot
    RestoreDBInstanceFromDBSnapshot (RestoreDBInstanceFromDBSnapshot'),
    newRestoreDBInstanceFromDBSnapshot,
    RestoreDBInstanceFromDBSnapshotResponse (RestoreDBInstanceFromDBSnapshotResponse'),
    newRestoreDBInstanceFromDBSnapshotResponse,

    -- ** DeleteDBProxy
    DeleteDBProxy (DeleteDBProxy'),
    newDeleteDBProxy,
    DeleteDBProxyResponse (DeleteDBProxyResponse'),
    newDeleteDBProxyResponse,

    -- ** CreateEventSubscription
    CreateEventSubscription (CreateEventSubscription'),
    newCreateEventSubscription,
    CreateEventSubscriptionResponse (CreateEventSubscriptionResponse'),
    newCreateEventSubscriptionResponse,

    -- ** PurchaseReservedDBInstancesOffering
    PurchaseReservedDBInstancesOffering (PurchaseReservedDBInstancesOffering'),
    newPurchaseReservedDBInstancesOffering,
    PurchaseReservedDBInstancesOfferingResponse (PurchaseReservedDBInstancesOfferingResponse'),
    newPurchaseReservedDBInstancesOfferingResponse,

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

    -- ** DescribeCertificates (Paginated)
    DescribeCertificates (DescribeCertificates'),
    newDescribeCertificates,
    DescribeCertificatesResponse (DescribeCertificatesResponse'),
    newDescribeCertificatesResponse,

    -- ** AuthorizeDBSecurityGroupIngress
    AuthorizeDBSecurityGroupIngress (AuthorizeDBSecurityGroupIngress'),
    newAuthorizeDBSecurityGroupIngress,
    AuthorizeDBSecurityGroupIngressResponse (AuthorizeDBSecurityGroupIngressResponse'),
    newAuthorizeDBSecurityGroupIngressResponse,

    -- ** RemoveRoleFromDBInstance
    RemoveRoleFromDBInstance (RemoveRoleFromDBInstance'),
    newRemoveRoleFromDBInstance,
    RemoveRoleFromDBInstanceResponse (RemoveRoleFromDBInstanceResponse'),
    newRemoveRoleFromDBInstanceResponse,

    -- ** DescribeSourceRegions (Paginated)
    DescribeSourceRegions (DescribeSourceRegions'),
    newDescribeSourceRegions,
    DescribeSourceRegionsResponse (DescribeSourceRegionsResponse'),
    newDescribeSourceRegionsResponse,

    -- ** CreateDBClusterEndpoint
    CreateDBClusterEndpoint (CreateDBClusterEndpoint'),
    newCreateDBClusterEndpoint,
    DBClusterEndpoint (DBClusterEndpoint'),
    newDBClusterEndpoint,

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
    DBClusterEndpoint (DBClusterEndpoint'),
    newDBClusterEndpoint,

    -- ** CreateDBProxy
    CreateDBProxy (CreateDBProxy'),
    newCreateDBProxy,
    CreateDBProxyResponse (CreateDBProxyResponse'),
    newCreateDBProxyResponse,

    -- ** DeleteDBInstanceAutomatedBackup
    DeleteDBInstanceAutomatedBackup (DeleteDBInstanceAutomatedBackup'),
    newDeleteDBInstanceAutomatedBackup,
    DeleteDBInstanceAutomatedBackupResponse (DeleteDBInstanceAutomatedBackupResponse'),
    newDeleteDBInstanceAutomatedBackupResponse,

    -- ** CreateDBClusterParameterGroup
    CreateDBClusterParameterGroup (CreateDBClusterParameterGroup'),
    newCreateDBClusterParameterGroup,
    CreateDBClusterParameterGroupResponse (CreateDBClusterParameterGroupResponse'),
    newCreateDBClusterParameterGroupResponse,

    -- ** CreateDBSnapshot
    CreateDBSnapshot (CreateDBSnapshot'),
    newCreateDBSnapshot,
    CreateDBSnapshotResponse (CreateDBSnapshotResponse'),
    newCreateDBSnapshotResponse,

    -- ** DeleteEventSubscription
    DeleteEventSubscription (DeleteEventSubscription'),
    newDeleteEventSubscription,
    DeleteEventSubscriptionResponse (DeleteEventSubscriptionResponse'),
    newDeleteEventSubscriptionResponse,

    -- ** DescribeDBClusterBacktracks (Paginated)
    DescribeDBClusterBacktracks (DescribeDBClusterBacktracks'),
    newDescribeDBClusterBacktracks,
    DescribeDBClusterBacktracksResponse (DescribeDBClusterBacktracksResponse'),
    newDescribeDBClusterBacktracksResponse,

    -- ** DescribeDBParameterGroups (Paginated)
    DescribeDBParameterGroups (DescribeDBParameterGroups'),
    newDescribeDBParameterGroups,
    DescribeDBParameterGroupsResponse (DescribeDBParameterGroupsResponse'),
    newDescribeDBParameterGroupsResponse,

    -- ** ModifyDBSnapshotAttribute
    ModifyDBSnapshotAttribute (ModifyDBSnapshotAttribute'),
    newModifyDBSnapshotAttribute,
    ModifyDBSnapshotAttributeResponse (ModifyDBSnapshotAttributeResponse'),
    newModifyDBSnapshotAttributeResponse,

    -- ** DescribeDBInstanceAutomatedBackups (Paginated)
    DescribeDBInstanceAutomatedBackups (DescribeDBInstanceAutomatedBackups'),
    newDescribeDBInstanceAutomatedBackups,
    DescribeDBInstanceAutomatedBackupsResponse (DescribeDBInstanceAutomatedBackupsResponse'),
    newDescribeDBInstanceAutomatedBackupsResponse,

    -- ** RemoveFromGlobalCluster
    RemoveFromGlobalCluster (RemoveFromGlobalCluster'),
    newRemoveFromGlobalCluster,
    RemoveFromGlobalClusterResponse (RemoveFromGlobalClusterResponse'),
    newRemoveFromGlobalClusterResponse,

    -- ** AddRoleToDBInstance
    AddRoleToDBInstance (AddRoleToDBInstance'),
    newAddRoleToDBInstance,
    AddRoleToDBInstanceResponse (AddRoleToDBInstanceResponse'),
    newAddRoleToDBInstanceResponse,

    -- ** DeleteDBClusterSnapshot
    DeleteDBClusterSnapshot (DeleteDBClusterSnapshot'),
    newDeleteDBClusterSnapshot,
    DeleteDBClusterSnapshotResponse (DeleteDBClusterSnapshotResponse'),
    newDeleteDBClusterSnapshotResponse,

    -- ** ModifyDBProxyEndpoint
    ModifyDBProxyEndpoint (ModifyDBProxyEndpoint'),
    newModifyDBProxyEndpoint,
    ModifyDBProxyEndpointResponse (ModifyDBProxyEndpointResponse'),
    newModifyDBProxyEndpointResponse,

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

    -- ** DescribeOptionGroupOptions (Paginated)
    DescribeOptionGroupOptions (DescribeOptionGroupOptions'),
    newDescribeOptionGroupOptions,
    DescribeOptionGroupOptionsResponse (DescribeOptionGroupOptionsResponse'),
    newDescribeOptionGroupOptionsResponse,

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

    -- ** StopActivityStream
    StopActivityStream (StopActivityStream'),
    newStopActivityStream,
    StopActivityStreamResponse (StopActivityStreamResponse'),
    newStopActivityStreamResponse,

    -- ** CreateDBClusterSnapshot
    CreateDBClusterSnapshot (CreateDBClusterSnapshot'),
    newCreateDBClusterSnapshot,
    CreateDBClusterSnapshotResponse (CreateDBClusterSnapshotResponse'),
    newCreateDBClusterSnapshotResponse,

    -- ** DescribeDBSnapshots (Paginated)
    DescribeDBSnapshots (DescribeDBSnapshots'),
    newDescribeDBSnapshots,
    DescribeDBSnapshotsResponse (DescribeDBSnapshotsResponse'),
    newDescribeDBSnapshotsResponse,

    -- ** ModifyDBProxyTargetGroup
    ModifyDBProxyTargetGroup (ModifyDBProxyTargetGroup'),
    newModifyDBProxyTargetGroup,
    ModifyDBProxyTargetGroupResponse (ModifyDBProxyTargetGroupResponse'),
    newModifyDBProxyTargetGroupResponse,

    -- ** DescribeDBSubnetGroups (Paginated)
    DescribeDBSubnetGroups (DescribeDBSubnetGroups'),
    newDescribeDBSubnetGroups,
    DescribeDBSubnetGroupsResponse (DescribeDBSubnetGroupsResponse'),
    newDescribeDBSubnetGroupsResponse,

    -- ** ModifyOptionGroup
    ModifyOptionGroup (ModifyOptionGroup'),
    newModifyOptionGroup,
    ModifyOptionGroupResponse (ModifyOptionGroupResponse'),
    newModifyOptionGroupResponse,

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

    -- ** DescribeGlobalClusters (Paginated)
    DescribeGlobalClusters (DescribeGlobalClusters'),
    newDescribeGlobalClusters,
    DescribeGlobalClustersResponse (DescribeGlobalClustersResponse'),
    newDescribeGlobalClustersResponse,

    -- ** StartDBInstance
    StartDBInstance (StartDBInstance'),
    newStartDBInstance,
    StartDBInstanceResponse (StartDBInstanceResponse'),
    newStartDBInstanceResponse,

    -- ** DescribeExportTasks (Paginated)
    DescribeExportTasks (DescribeExportTasks'),
    newDescribeExportTasks,
    DescribeExportTasksResponse (DescribeExportTasksResponse'),
    newDescribeExportTasksResponse,

    -- ** CancelExportTask
    CancelExportTask (CancelExportTask'),
    newCancelExportTask,
    ExportTask (ExportTask'),
    newExportTask,

    -- ** ModifyDBClusterParameterGroup
    ModifyDBClusterParameterGroup (ModifyDBClusterParameterGroup'),
    newModifyDBClusterParameterGroup,
    DBClusterParameterGroupNameMessage (DBClusterParameterGroupNameMessage'),
    newDBClusterParameterGroupNameMessage,

    -- ** RestoreDBInstanceToPointInTime
    RestoreDBInstanceToPointInTime (RestoreDBInstanceToPointInTime'),
    newRestoreDBInstanceToPointInTime,
    RestoreDBInstanceToPointInTimeResponse (RestoreDBInstanceToPointInTimeResponse'),
    newRestoreDBInstanceToPointInTimeResponse,

    -- ** DescribeDBClusterSnapshotAttributes
    DescribeDBClusterSnapshotAttributes (DescribeDBClusterSnapshotAttributes'),
    newDescribeDBClusterSnapshotAttributes,
    DescribeDBClusterSnapshotAttributesResponse (DescribeDBClusterSnapshotAttributesResponse'),
    newDescribeDBClusterSnapshotAttributesResponse,

    -- ** ModifyDBSnapshot
    ModifyDBSnapshot (ModifyDBSnapshot'),
    newModifyDBSnapshot,
    ModifyDBSnapshotResponse (ModifyDBSnapshotResponse'),
    newModifyDBSnapshotResponse,

    -- ** DescribeDBProxyTargetGroups (Paginated)
    DescribeDBProxyTargetGroups (DescribeDBProxyTargetGroups'),
    newDescribeDBProxyTargetGroups,
    DescribeDBProxyTargetGroupsResponse (DescribeDBProxyTargetGroupsResponse'),
    newDescribeDBProxyTargetGroupsResponse,

    -- ** ModifyDBProxy
    ModifyDBProxy (ModifyDBProxy'),
    newModifyDBProxy,
    ModifyDBProxyResponse (ModifyDBProxyResponse'),
    newModifyDBProxyResponse,

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

    -- ** ImportInstallationMedia
    ImportInstallationMedia (ImportInstallationMedia'),
    newImportInstallationMedia,
    InstallationMedia (InstallationMedia'),
    newInstallationMedia,

    -- ** CreateGlobalCluster
    CreateGlobalCluster (CreateGlobalCluster'),
    newCreateGlobalCluster,
    CreateGlobalClusterResponse (CreateGlobalClusterResponse'),
    newCreateGlobalClusterResponse,

    -- ** ResetDBParameterGroup
    ResetDBParameterGroup (ResetDBParameterGroup'),
    newResetDBParameterGroup,
    DBParameterGroupNameMessage (DBParameterGroupNameMessage'),
    newDBParameterGroupNameMessage,

    -- ** FailoverGlobalCluster
    FailoverGlobalCluster (FailoverGlobalCluster'),
    newFailoverGlobalCluster,
    FailoverGlobalClusterResponse (FailoverGlobalClusterResponse'),
    newFailoverGlobalClusterResponse,

    -- ** DescribeInstallationMedia (Paginated)
    DescribeInstallationMedia (DescribeInstallationMedia'),
    newDescribeInstallationMedia,
    DescribeInstallationMediaResponse (DescribeInstallationMediaResponse'),
    newDescribeInstallationMediaResponse,

    -- ** DeregisterDBProxyTargets
    DeregisterDBProxyTargets (DeregisterDBProxyTargets'),
    newDeregisterDBProxyTargets,
    DeregisterDBProxyTargetsResponse (DeregisterDBProxyTargetsResponse'),
    newDeregisterDBProxyTargetsResponse,

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

    -- ** RevokeDBSecurityGroupIngress
    RevokeDBSecurityGroupIngress (RevokeDBSecurityGroupIngress'),
    newRevokeDBSecurityGroupIngress,
    RevokeDBSecurityGroupIngressResponse (RevokeDBSecurityGroupIngressResponse'),
    newRevokeDBSecurityGroupIngressResponse,

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

    -- ** DeleteDBProxyEndpoint
    DeleteDBProxyEndpoint (DeleteDBProxyEndpoint'),
    newDeleteDBProxyEndpoint,
    DeleteDBProxyEndpointResponse (DeleteDBProxyEndpointResponse'),
    newDeleteDBProxyEndpointResponse,

    -- ** StopDBInstanceAutomatedBackupsReplication
    StopDBInstanceAutomatedBackupsReplication (StopDBInstanceAutomatedBackupsReplication'),
    newStopDBInstanceAutomatedBackupsReplication,
    StopDBInstanceAutomatedBackupsReplicationResponse (StopDBInstanceAutomatedBackupsReplicationResponse'),
    newStopDBInstanceAutomatedBackupsReplicationResponse,

    -- ** CreateOptionGroup
    CreateOptionGroup (CreateOptionGroup'),
    newCreateOptionGroup,
    CreateOptionGroupResponse (CreateOptionGroupResponse'),
    newCreateOptionGroupResponse,

    -- ** DescribeAccountAttributes
    DescribeAccountAttributes (DescribeAccountAttributes'),
    newDescribeAccountAttributes,
    DescribeAccountAttributesResponse (DescribeAccountAttributesResponse'),
    newDescribeAccountAttributesResponse,

    -- ** DeleteDBSnapshot
    DeleteDBSnapshot (DeleteDBSnapshot'),
    newDeleteDBSnapshot,
    DeleteDBSnapshotResponse (DeleteDBSnapshotResponse'),
    newDeleteDBSnapshotResponse,

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

    -- ** CreateDBSecurityGroup
    CreateDBSecurityGroup (CreateDBSecurityGroup'),
    newCreateDBSecurityGroup,
    CreateDBSecurityGroupResponse (CreateDBSecurityGroupResponse'),
    newCreateDBSecurityGroupResponse,

    -- ** ModifyCertificates
    ModifyCertificates (ModifyCertificates'),
    newModifyCertificates,
    ModifyCertificatesResponse (ModifyCertificatesResponse'),
    newModifyCertificatesResponse,

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

    -- ** DescribeReservedDBInstancesOfferings (Paginated)
    DescribeReservedDBInstancesOfferings (DescribeReservedDBInstancesOfferings'),
    newDescribeReservedDBInstancesOfferings,
    DescribeReservedDBInstancesOfferingsResponse (DescribeReservedDBInstancesOfferingsResponse'),
    newDescribeReservedDBInstancesOfferingsResponse,

    -- ** DeleteDBSecurityGroup
    DeleteDBSecurityGroup (DeleteDBSecurityGroup'),
    newDeleteDBSecurityGroup,
    DeleteDBSecurityGroupResponse (DeleteDBSecurityGroupResponse'),
    newDeleteDBSecurityGroupResponse,

    -- ** DeleteDBInstance
    DeleteDBInstance (DeleteDBInstance'),
    newDeleteDBInstance,
    DeleteDBInstanceResponse (DeleteDBInstanceResponse'),
    newDeleteDBInstanceResponse,

    -- ** StartActivityStream
    StartActivityStream (StartActivityStream'),
    newStartActivityStream,
    StartActivityStreamResponse (StartActivityStreamResponse'),
    newStartActivityStreamResponse,

    -- ** CreateDBInstanceReadReplica
    CreateDBInstanceReadReplica (CreateDBInstanceReadReplica'),
    newCreateDBInstanceReadReplica,
    CreateDBInstanceReadReplicaResponse (CreateDBInstanceReadReplicaResponse'),
    newCreateDBInstanceReadReplicaResponse,

    -- ** DeleteDBParameterGroup
    DeleteDBParameterGroup (DeleteDBParameterGroup'),
    newDeleteDBParameterGroup,
    DeleteDBParameterGroupResponse (DeleteDBParameterGroupResponse'),
    newDeleteDBParameterGroupResponse,

    -- ** ModifyCurrentDBClusterCapacity
    ModifyCurrentDBClusterCapacity (ModifyCurrentDBClusterCapacity'),
    newModifyCurrentDBClusterCapacity,
    ModifyCurrentDBClusterCapacityResponse (ModifyCurrentDBClusterCapacityResponse'),
    newModifyCurrentDBClusterCapacityResponse,

    -- ** ModifyGlobalCluster
    ModifyGlobalCluster (ModifyGlobalCluster'),
    newModifyGlobalCluster,
    ModifyGlobalClusterResponse (ModifyGlobalClusterResponse'),
    newModifyGlobalClusterResponse,

    -- ** RegisterDBProxyTargets
    RegisterDBProxyTargets (RegisterDBProxyTargets'),
    newRegisterDBProxyTargets,
    RegisterDBProxyTargetsResponse (RegisterDBProxyTargetsResponse'),
    newRegisterDBProxyTargetsResponse,

    -- ** DescribeDBSecurityGroups (Paginated)
    DescribeDBSecurityGroups (DescribeDBSecurityGroups'),
    newDescribeDBSecurityGroups,
    DescribeDBSecurityGroupsResponse (DescribeDBSecurityGroupsResponse'),
    newDescribeDBSecurityGroupsResponse,

    -- ** CopyOptionGroup
    CopyOptionGroup (CopyOptionGroup'),
    newCopyOptionGroup,
    CopyOptionGroupResponse (CopyOptionGroupResponse'),
    newCopyOptionGroupResponse,

    -- ** RestoreDBClusterToPointInTime
    RestoreDBClusterToPointInTime (RestoreDBClusterToPointInTime'),
    newRestoreDBClusterToPointInTime,
    RestoreDBClusterToPointInTimeResponse (RestoreDBClusterToPointInTimeResponse'),
    newRestoreDBClusterToPointInTimeResponse,

    -- ** DeleteInstallationMedia
    DeleteInstallationMedia (DeleteInstallationMedia'),
    newDeleteInstallationMedia,
    InstallationMedia (InstallationMedia'),
    newInstallationMedia,

    -- ** DescribeDBInstances (Paginated)
    DescribeDBInstances (DescribeDBInstances'),
    newDescribeDBInstances,
    DescribeDBInstancesResponse (DescribeDBInstancesResponse'),
    newDescribeDBInstancesResponse,

    -- ** RestoreDBInstanceFromS
    RestoreDBInstanceFromS (RestoreDBInstanceFromS'),
    newRestoreDBInstanceFromS,
    RestoreDBInstanceFromSResponse (RestoreDBInstanceFromSResponse'),
    newRestoreDBInstanceFromSResponse,

    -- ** DownloadDBLogFilePortion (Paginated)
    DownloadDBLogFilePortion (DownloadDBLogFilePortion'),
    newDownloadDBLogFilePortion,
    DownloadDBLogFilePortionResponse (DownloadDBLogFilePortionResponse'),
    newDownloadDBLogFilePortionResponse,

    -- ** DescribeDBProxies (Paginated)
    DescribeDBProxies (DescribeDBProxies'),
    newDescribeDBProxies,
    DescribeDBProxiesResponse (DescribeDBProxiesResponse'),
    newDescribeDBProxiesResponse,

    -- ** StartExportTask
    StartExportTask (StartExportTask'),
    newStartExportTask,
    ExportTask (ExportTask'),
    newExportTask,

    -- * Types

    -- ** ActivityStreamMode
    ActivityStreamMode (..),

    -- ** ActivityStreamStatus
    ActivityStreamStatus (..),

    -- ** ApplyMethod
    ApplyMethod (..),

    -- ** AuthScheme
    AuthScheme (..),

    -- ** DBProxyEndpointStatus
    DBProxyEndpointStatus (..),

    -- ** DBProxyEndpointTargetRole
    DBProxyEndpointTargetRole (..),

    -- ** DBProxyStatus
    DBProxyStatus (..),

    -- ** EngineFamily
    EngineFamily (..),

    -- ** FailoverStatus
    FailoverStatus (..),

    -- ** IAMAuthMode
    IAMAuthMode (..),

    -- ** ReplicaMode
    ReplicaMode (..),

    -- ** SourceType
    SourceType (..),

    -- ** TargetHealthReason
    TargetHealthReason (..),

    -- ** TargetRole
    TargetRole (..),

    -- ** TargetState
    TargetState (..),

    -- ** TargetType
    TargetType (..),

    -- ** WriteForwardingStatus
    WriteForwardingStatus (..),

    -- ** AccountQuota
    AccountQuota (AccountQuota'),
    newAccountQuota,

    -- ** AvailabilityZone
    AvailabilityZone (AvailabilityZone'),
    newAvailabilityZone,

    -- ** AvailableProcessorFeature
    AvailableProcessorFeature (AvailableProcessorFeature'),
    newAvailableProcessorFeature,

    -- ** Certificate
    Certificate (Certificate'),
    newCertificate,

    -- ** CharacterSet
    CharacterSet (CharacterSet'),
    newCharacterSet,

    -- ** CloudwatchLogsExportConfiguration
    CloudwatchLogsExportConfiguration (CloudwatchLogsExportConfiguration'),
    newCloudwatchLogsExportConfiguration,

    -- ** ClusterPendingModifiedValues
    ClusterPendingModifiedValues (ClusterPendingModifiedValues'),
    newClusterPendingModifiedValues,

    -- ** ConnectionPoolConfiguration
    ConnectionPoolConfiguration (ConnectionPoolConfiguration'),
    newConnectionPoolConfiguration,

    -- ** ConnectionPoolConfigurationInfo
    ConnectionPoolConfigurationInfo (ConnectionPoolConfigurationInfo'),
    newConnectionPoolConfigurationInfo,

    -- ** CustomAvailabilityZone
    CustomAvailabilityZone (CustomAvailabilityZone'),
    newCustomAvailabilityZone,

    -- ** DBCluster
    DBCluster (DBCluster'),
    newDBCluster,

    -- ** DBClusterBacktrack
    DBClusterBacktrack (DBClusterBacktrack'),
    newDBClusterBacktrack,

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

    -- ** DBInstanceAutomatedBackup
    DBInstanceAutomatedBackup (DBInstanceAutomatedBackup'),
    newDBInstanceAutomatedBackup,

    -- ** DBInstanceAutomatedBackupsReplication
    DBInstanceAutomatedBackupsReplication (DBInstanceAutomatedBackupsReplication'),
    newDBInstanceAutomatedBackupsReplication,

    -- ** DBInstanceRole
    DBInstanceRole (DBInstanceRole'),
    newDBInstanceRole,

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

    -- ** DBProxy
    DBProxy (DBProxy'),
    newDBProxy,

    -- ** DBProxyEndpoint
    DBProxyEndpoint (DBProxyEndpoint'),
    newDBProxyEndpoint,

    -- ** DBProxyTarget
    DBProxyTarget (DBProxyTarget'),
    newDBProxyTarget,

    -- ** DBProxyTargetGroup
    DBProxyTargetGroup (DBProxyTargetGroup'),
    newDBProxyTargetGroup,

    -- ** DBSecurityGroup
    DBSecurityGroup (DBSecurityGroup'),
    newDBSecurityGroup,

    -- ** DBSecurityGroupMembership
    DBSecurityGroupMembership (DBSecurityGroupMembership'),
    newDBSecurityGroupMembership,

    -- ** DBSnapshot
    DBSnapshot (DBSnapshot'),
    newDBSnapshot,

    -- ** DBSnapshotAttribute
    DBSnapshotAttribute (DBSnapshotAttribute'),
    newDBSnapshotAttribute,

    -- ** DBSnapshotAttributesResult
    DBSnapshotAttributesResult (DBSnapshotAttributesResult'),
    newDBSnapshotAttributesResult,

    -- ** DBSubnetGroup
    DBSubnetGroup (DBSubnetGroup'),
    newDBSubnetGroup,

    -- ** DescribeDBLogFilesDetails
    DescribeDBLogFilesDetails (DescribeDBLogFilesDetails'),
    newDescribeDBLogFilesDetails,

    -- ** DomainMembership
    DomainMembership (DomainMembership'),
    newDomainMembership,

    -- ** DoubleRange
    DoubleRange (DoubleRange'),
    newDoubleRange,

    -- ** EC2SecurityGroup
    EC2SecurityGroup (EC2SecurityGroup'),
    newEC2SecurityGroup,

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

    -- ** ExportTask
    ExportTask (ExportTask'),
    newExportTask,

    -- ** FailoverState
    FailoverState (FailoverState'),
    newFailoverState,

    -- ** Filter
    Filter (Filter'),
    newFilter,

    -- ** GlobalCluster
    GlobalCluster (GlobalCluster'),
    newGlobalCluster,

    -- ** GlobalClusterMember
    GlobalClusterMember (GlobalClusterMember'),
    newGlobalClusterMember,

    -- ** IPRange
    IPRange (IPRange'),
    newIPRange,

    -- ** InstallationMedia
    InstallationMedia (InstallationMedia'),
    newInstallationMedia,

    -- ** InstallationMediaFailureCause
    InstallationMediaFailureCause (InstallationMediaFailureCause'),
    newInstallationMediaFailureCause,

    -- ** MinimumEngineVersionPerAllowedValue
    MinimumEngineVersionPerAllowedValue (MinimumEngineVersionPerAllowedValue'),
    newMinimumEngineVersionPerAllowedValue,

    -- ** Option
    Option (Option'),
    newOption,

    -- ** OptionConfiguration
    OptionConfiguration (OptionConfiguration'),
    newOptionConfiguration,

    -- ** OptionGroup
    OptionGroup (OptionGroup'),
    newOptionGroup,

    -- ** OptionGroupMembership
    OptionGroupMembership (OptionGroupMembership'),
    newOptionGroupMembership,

    -- ** OptionGroupOption
    OptionGroupOption (OptionGroupOption'),
    newOptionGroupOption,

    -- ** OptionGroupOptionSetting
    OptionGroupOptionSetting (OptionGroupOptionSetting'),
    newOptionGroupOptionSetting,

    -- ** OptionSetting
    OptionSetting (OptionSetting'),
    newOptionSetting,

    -- ** OptionVersion
    OptionVersion (OptionVersion'),
    newOptionVersion,

    -- ** OrderableDBInstanceOption
    OrderableDBInstanceOption (OrderableDBInstanceOption'),
    newOrderableDBInstanceOption,

    -- ** Outpost
    Outpost (Outpost'),
    newOutpost,

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

    -- ** ProcessorFeature
    ProcessorFeature (ProcessorFeature'),
    newProcessorFeature,

    -- ** Range
    Range (Range'),
    newRange,

    -- ** RecurringCharge
    RecurringCharge (RecurringCharge'),
    newRecurringCharge,

    -- ** ReservedDBInstance
    ReservedDBInstance (ReservedDBInstance'),
    newReservedDBInstance,

    -- ** ReservedDBInstancesOffering
    ReservedDBInstancesOffering (ReservedDBInstancesOffering'),
    newReservedDBInstancesOffering,

    -- ** ResourcePendingMaintenanceActions
    ResourcePendingMaintenanceActions (ResourcePendingMaintenanceActions'),
    newResourcePendingMaintenanceActions,

    -- ** RestoreWindow
    RestoreWindow (RestoreWindow'),
    newRestoreWindow,

    -- ** ScalingConfiguration
    ScalingConfiguration (ScalingConfiguration'),
    newScalingConfiguration,

    -- ** ScalingConfigurationInfo
    ScalingConfigurationInfo (ScalingConfigurationInfo'),
    newScalingConfigurationInfo,

    -- ** SourceRegion
    SourceRegion (SourceRegion'),
    newSourceRegion,

    -- ** Subnet
    Subnet (Subnet'),
    newSubnet,

    -- ** Tag
    Tag (Tag'),
    newTag,

    -- ** TargetHealth
    TargetHealth (TargetHealth'),
    newTargetHealth,

    -- ** Timezone
    Timezone (Timezone'),
    newTimezone,

    -- ** UpgradeTarget
    UpgradeTarget (UpgradeTarget'),
    newUpgradeTarget,

    -- ** UserAuthConfig
    UserAuthConfig (UserAuthConfig'),
    newUserAuthConfig,

    -- ** UserAuthConfigInfo
    UserAuthConfigInfo (UserAuthConfigInfo'),
    newUserAuthConfigInfo,

    -- ** ValidDBInstanceModificationsMessage
    ValidDBInstanceModificationsMessage (ValidDBInstanceModificationsMessage'),
    newValidDBInstanceModificationsMessage,

    -- ** ValidStorageOptions
    ValidStorageOptions (ValidStorageOptions'),
    newValidStorageOptions,

    -- ** VpcSecurityGroupMembership
    VpcSecurityGroupMembership (VpcSecurityGroupMembership'),
    newVpcSecurityGroupMembership,

    -- ** VpnDetails
    VpnDetails (VpnDetails'),
    newVpnDetails,
  )
where

import Network.AWS.RDS.AddRoleToDBCluster
import Network.AWS.RDS.AddRoleToDBInstance
import Network.AWS.RDS.AddSourceIdentifierToSubscription
import Network.AWS.RDS.AddTagsToResource
import Network.AWS.RDS.ApplyPendingMaintenanceAction
import Network.AWS.RDS.AuthorizeDBSecurityGroupIngress
import Network.AWS.RDS.BacktrackDBCluster
import Network.AWS.RDS.CancelExportTask
import Network.AWS.RDS.CopyDBClusterParameterGroup
import Network.AWS.RDS.CopyDBClusterSnapshot
import Network.AWS.RDS.CopyDBParameterGroup
import Network.AWS.RDS.CopyDBSnapshot
import Network.AWS.RDS.CopyOptionGroup
import Network.AWS.RDS.CreateCustomAvailabilityZone
import Network.AWS.RDS.CreateDBCluster
import Network.AWS.RDS.CreateDBClusterEndpoint
import Network.AWS.RDS.CreateDBClusterParameterGroup
import Network.AWS.RDS.CreateDBClusterSnapshot
import Network.AWS.RDS.CreateDBInstance
import Network.AWS.RDS.CreateDBInstanceReadReplica
import Network.AWS.RDS.CreateDBParameterGroup
import Network.AWS.RDS.CreateDBProxy
import Network.AWS.RDS.CreateDBProxyEndpoint
import Network.AWS.RDS.CreateDBSecurityGroup
import Network.AWS.RDS.CreateDBSnapshot
import Network.AWS.RDS.CreateDBSubnetGroup
import Network.AWS.RDS.CreateEventSubscription
import Network.AWS.RDS.CreateGlobalCluster
import Network.AWS.RDS.CreateOptionGroup
import Network.AWS.RDS.DeleteCustomAvailabilityZone
import Network.AWS.RDS.DeleteDBCluster
import Network.AWS.RDS.DeleteDBClusterEndpoint
import Network.AWS.RDS.DeleteDBClusterParameterGroup
import Network.AWS.RDS.DeleteDBClusterSnapshot
import Network.AWS.RDS.DeleteDBInstance
import Network.AWS.RDS.DeleteDBInstanceAutomatedBackup
import Network.AWS.RDS.DeleteDBParameterGroup
import Network.AWS.RDS.DeleteDBProxy
import Network.AWS.RDS.DeleteDBProxyEndpoint
import Network.AWS.RDS.DeleteDBSecurityGroup
import Network.AWS.RDS.DeleteDBSnapshot
import Network.AWS.RDS.DeleteDBSubnetGroup
import Network.AWS.RDS.DeleteEventSubscription
import Network.AWS.RDS.DeleteGlobalCluster
import Network.AWS.RDS.DeleteInstallationMedia
import Network.AWS.RDS.DeleteOptionGroup
import Network.AWS.RDS.DeregisterDBProxyTargets
import Network.AWS.RDS.DescribeAccountAttributes
import Network.AWS.RDS.DescribeCertificates
import Network.AWS.RDS.DescribeCustomAvailabilityZones
import Network.AWS.RDS.DescribeDBClusterBacktracks
import Network.AWS.RDS.DescribeDBClusterEndpoints
import Network.AWS.RDS.DescribeDBClusterParameterGroups
import Network.AWS.RDS.DescribeDBClusterParameters
import Network.AWS.RDS.DescribeDBClusterSnapshotAttributes
import Network.AWS.RDS.DescribeDBClusterSnapshots
import Network.AWS.RDS.DescribeDBClusters
import Network.AWS.RDS.DescribeDBEngineVersions
import Network.AWS.RDS.DescribeDBInstanceAutomatedBackups
import Network.AWS.RDS.DescribeDBInstances
import Network.AWS.RDS.DescribeDBLogFiles
import Network.AWS.RDS.DescribeDBParameterGroups
import Network.AWS.RDS.DescribeDBParameters
import Network.AWS.RDS.DescribeDBProxies
import Network.AWS.RDS.DescribeDBProxyEndpoints
import Network.AWS.RDS.DescribeDBProxyTargetGroups
import Network.AWS.RDS.DescribeDBProxyTargets
import Network.AWS.RDS.DescribeDBSecurityGroups
import Network.AWS.RDS.DescribeDBSnapshotAttributes
import Network.AWS.RDS.DescribeDBSnapshots
import Network.AWS.RDS.DescribeDBSubnetGroups
import Network.AWS.RDS.DescribeEngineDefaultClusterParameters
import Network.AWS.RDS.DescribeEngineDefaultParameters
import Network.AWS.RDS.DescribeEventCategories
import Network.AWS.RDS.DescribeEventSubscriptions
import Network.AWS.RDS.DescribeEvents
import Network.AWS.RDS.DescribeExportTasks
import Network.AWS.RDS.DescribeGlobalClusters
import Network.AWS.RDS.DescribeInstallationMedia
import Network.AWS.RDS.DescribeOptionGroupOptions
import Network.AWS.RDS.DescribeOptionGroups
import Network.AWS.RDS.DescribeOrderableDBInstanceOptions
import Network.AWS.RDS.DescribePendingMaintenanceActions
import Network.AWS.RDS.DescribeReservedDBInstances
import Network.AWS.RDS.DescribeReservedDBInstancesOfferings
import Network.AWS.RDS.DescribeSourceRegions
import Network.AWS.RDS.DescribeValidDBInstanceModifications
import Network.AWS.RDS.DownloadDBLogFilePortion
import Network.AWS.RDS.FailoverDBCluster
import Network.AWS.RDS.FailoverGlobalCluster
import Network.AWS.RDS.ImportInstallationMedia
import Network.AWS.RDS.Lens
import Network.AWS.RDS.ListTagsForResource
import Network.AWS.RDS.ModifyCertificates
import Network.AWS.RDS.ModifyCurrentDBClusterCapacity
import Network.AWS.RDS.ModifyDBCluster
import Network.AWS.RDS.ModifyDBClusterEndpoint
import Network.AWS.RDS.ModifyDBClusterParameterGroup
import Network.AWS.RDS.ModifyDBClusterSnapshotAttribute
import Network.AWS.RDS.ModifyDBInstance
import Network.AWS.RDS.ModifyDBParameterGroup
import Network.AWS.RDS.ModifyDBProxy
import Network.AWS.RDS.ModifyDBProxyEndpoint
import Network.AWS.RDS.ModifyDBProxyTargetGroup
import Network.AWS.RDS.ModifyDBSnapshot
import Network.AWS.RDS.ModifyDBSnapshotAttribute
import Network.AWS.RDS.ModifyDBSubnetGroup
import Network.AWS.RDS.ModifyEventSubscription
import Network.AWS.RDS.ModifyGlobalCluster
import Network.AWS.RDS.ModifyOptionGroup
import Network.AWS.RDS.PromoteReadReplica
import Network.AWS.RDS.PromoteReadReplicaDBCluster
import Network.AWS.RDS.PurchaseReservedDBInstancesOffering
import Network.AWS.RDS.RebootDBInstance
import Network.AWS.RDS.RegisterDBProxyTargets
import Network.AWS.RDS.RemoveFromGlobalCluster
import Network.AWS.RDS.RemoveRoleFromDBCluster
import Network.AWS.RDS.RemoveRoleFromDBInstance
import Network.AWS.RDS.RemoveSourceIdentifierFromSubscription
import Network.AWS.RDS.RemoveTagsFromResource
import Network.AWS.RDS.ResetDBClusterParameterGroup
import Network.AWS.RDS.ResetDBParameterGroup
import Network.AWS.RDS.RestoreDBClusterFromS
import Network.AWS.RDS.RestoreDBClusterFromSnapshot
import Network.AWS.RDS.RestoreDBClusterToPointInTime
import Network.AWS.RDS.RestoreDBInstanceFromDBSnapshot
import Network.AWS.RDS.RestoreDBInstanceFromS
import Network.AWS.RDS.RestoreDBInstanceToPointInTime
import Network.AWS.RDS.RevokeDBSecurityGroupIngress
import Network.AWS.RDS.StartActivityStream
import Network.AWS.RDS.StartDBCluster
import Network.AWS.RDS.StartDBInstance
import Network.AWS.RDS.StartDBInstanceAutomatedBackupsReplication
import Network.AWS.RDS.StartExportTask
import Network.AWS.RDS.StopActivityStream
import Network.AWS.RDS.StopDBCluster
import Network.AWS.RDS.StopDBInstance
import Network.AWS.RDS.StopDBInstanceAutomatedBackupsReplication
import Network.AWS.RDS.Types
import Network.AWS.RDS.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'RDS'.

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
