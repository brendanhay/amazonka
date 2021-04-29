{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
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

    -- ** DBInstanceRoleAlreadyExistsFault
    _DBInstanceRoleAlreadyExistsFault,

    -- ** ReservedDBInstanceAlreadyExistsFault
    _ReservedDBInstanceAlreadyExistsFault,

    -- ** DBParameterGroupQuotaExceededFault
    _DBParameterGroupQuotaExceededFault,

    -- ** InsufficientDBClusterCapacityFault
    _InsufficientDBClusterCapacityFault,

    -- ** InvalidVPCNetworkStateFault
    _InvalidVPCNetworkStateFault,

    -- ** ProvisionedIopsNotAvailableInAZFault
    _ProvisionedIopsNotAvailableInAZFault,

    -- ** DBClusterRoleQuotaExceededFault
    _DBClusterRoleQuotaExceededFault,

    -- ** CertificateNotFoundFault
    _CertificateNotFoundFault,

    -- ** DBClusterRoleAlreadyExistsFault
    _DBClusterRoleAlreadyExistsFault,

    -- ** InstallationMediaAlreadyExistsFault
    _InstallationMediaAlreadyExistsFault,

    -- ** DBParameterGroupAlreadyExistsFault
    _DBParameterGroupAlreadyExistsFault,

    -- ** InsufficientDBInstanceCapacityFault
    _InsufficientDBInstanceCapacityFault,

    -- ** SubscriptionAlreadyExistFault
    _SubscriptionAlreadyExistFault,

    -- ** DBInstanceRoleQuotaExceededFault
    _DBInstanceRoleQuotaExceededFault,

    -- ** ReservedDBInstanceQuotaExceededFault
    _ReservedDBInstanceQuotaExceededFault,

    -- ** ReservedDBInstancesOfferingNotFoundFault
    _ReservedDBInstancesOfferingNotFoundFault,

    -- ** DBClusterSnapshotNotFoundFault
    _DBClusterSnapshotNotFoundFault,

    -- ** DBInstanceAutomatedBackupNotFoundFault
    _DBInstanceAutomatedBackupNotFoundFault,

    -- ** SNSNoAuthorizationFault
    _SNSNoAuthorizationFault,

    -- ** InvalidDBSubnetGroupStateFault
    _InvalidDBSubnetGroupStateFault,

    -- ** DBSubnetGroupAlreadyExistsFault
    _DBSubnetGroupAlreadyExistsFault,

    -- ** DBClusterEndpointNotFoundFault
    _DBClusterEndpointNotFoundFault,

    -- ** DBSnapshotAlreadyExistsFault
    _DBSnapshotAlreadyExistsFault,

    -- ** DBUpgradeDependencyFailureFault
    _DBUpgradeDependencyFailureFault,

    -- ** InvalidDBInstanceAutomatedBackupStateFault
    _InvalidDBInstanceAutomatedBackupStateFault,

    -- ** ResourceNotFoundFault
    _ResourceNotFoundFault,

    -- ** SNSTopicArnNotFoundFault
    _SNSTopicArnNotFoundFault,

    -- ** InvalidRestoreFault
    _InvalidRestoreFault,

    -- ** InvalidEventSubscriptionStateFault
    _InvalidEventSubscriptionStateFault,

    -- ** DBClusterEndpointAlreadyExistsFault
    _DBClusterEndpointAlreadyExistsFault,

    -- ** InvalidDBInstanceStateFault
    _InvalidDBInstanceStateFault,

    -- ** DBClusterParameterGroupNotFoundFault
    _DBClusterParameterGroupNotFoundFault,

    -- ** KMSKeyNotAccessibleFault
    _KMSKeyNotAccessibleFault,

    -- ** DBProxyNotFoundFault
    _DBProxyNotFoundFault,

    -- ** InvalidDBClusterEndpointStateFault
    _InvalidDBClusterEndpointStateFault,

    -- ** SnapshotQuotaExceededFault
    _SnapshotQuotaExceededFault,

    -- ** InvalidExportOnlyFault
    _InvalidExportOnlyFault,

    -- ** InsufficientStorageClusterCapacityFault
    _InsufficientStorageClusterCapacityFault,

    -- ** DBInstanceAutomatedBackupQuotaExceededFault
    _DBInstanceAutomatedBackupQuotaExceededFault,

    -- ** StorageTypeNotSupportedFault
    _StorageTypeNotSupportedFault,

    -- ** IamRoleNotFoundFault
    _IamRoleNotFoundFault,

    -- ** EventSubscriptionQuotaExceededFault
    _EventSubscriptionQuotaExceededFault,

    -- ** DBClusterEndpointQuotaExceededFault
    _DBClusterEndpointQuotaExceededFault,

    -- ** InvalidGlobalClusterStateFault
    _InvalidGlobalClusterStateFault,

    -- ** DBProxyQuotaExceededFault
    _DBProxyQuotaExceededFault,

    -- ** DBSubnetGroupNotAllowedFault
    _DBSubnetGroupNotAllowedFault,

    -- ** ExportTaskAlreadyExistsFault
    _ExportTaskAlreadyExistsFault,

    -- ** DBProxyTargetGroupNotFoundFault
    _DBProxyTargetGroupNotFoundFault,

    -- ** GlobalClusterAlreadyExistsFault
    _GlobalClusterAlreadyExistsFault,

    -- ** DBProxyTargetNotFoundFault
    _DBProxyTargetNotFoundFault,

    -- ** SharedSnapshotQuotaExceededFault
    _SharedSnapshotQuotaExceededFault,

    -- ** ReservedDBInstanceNotFoundFault
    _ReservedDBInstanceNotFoundFault,

    -- ** DBSubnetQuotaExceededFault
    _DBSubnetQuotaExceededFault,

    -- ** DBInstanceRoleNotFoundFault
    _DBInstanceRoleNotFoundFault,

    -- ** BackupPolicyNotFoundFault
    _BackupPolicyNotFoundFault,

    -- ** IamRoleMissingPermissionsFault
    _IamRoleMissingPermissionsFault,

    -- ** DBProxyTargetAlreadyRegisteredFault
    _DBProxyTargetAlreadyRegisteredFault,

    -- ** AuthorizationAlreadyExistsFault
    _AuthorizationAlreadyExistsFault,

    -- ** InvalidDBParameterGroupStateFault
    _InvalidDBParameterGroupStateFault,

    -- ** SNSInvalidTopicFault
    _SNSInvalidTopicFault,

    -- ** SourceNotFoundFault
    _SourceNotFoundFault,

    -- ** PointInTimeRestoreNotEnabledFault
    _PointInTimeRestoreNotEnabledFault,

    -- ** InvalidDBClusterSnapshotStateFault
    _InvalidDBClusterSnapshotStateFault,

    -- ** DBClusterSnapshotAlreadyExistsFault
    _DBClusterSnapshotAlreadyExistsFault,

    -- ** AuthorizationQuotaExceededFault
    _AuthorizationQuotaExceededFault,

    -- ** InstallationMediaNotFoundFault
    _InstallationMediaNotFoundFault,

    -- ** DBParameterGroupNotFoundFault
    _DBParameterGroupNotFoundFault,

    -- ** DBClusterRoleNotFoundFault
    _DBClusterRoleNotFoundFault,

    -- ** DBSubnetGroupDoesNotCoverEnoughAZs
    _DBSubnetGroupDoesNotCoverEnoughAZs,

    -- ** InvalidDBSubnetStateFault
    _InvalidDBSubnetStateFault,

    -- ** DomainNotFoundFault
    _DomainNotFoundFault,

    -- ** InvalidDBSubnetGroupFault
    _InvalidDBSubnetGroupFault,

    -- ** SubnetAlreadyInUse
    _SubnetAlreadyInUse,

    -- ** DBClusterBacktrackNotFoundFault
    _DBClusterBacktrackNotFoundFault,

    -- ** DBProxyAlreadyExistsFault
    _DBProxyAlreadyExistsFault,

    -- ** DBSecurityGroupNotSupportedFault
    _DBSecurityGroupNotSupportedFault,

    -- ** StorageQuotaExceededFault
    _StorageQuotaExceededFault,

    -- ** InstanceQuotaExceededFault
    _InstanceQuotaExceededFault,

    -- ** InvalidDBSnapshotStateFault
    _InvalidDBSnapshotStateFault,

    -- ** DBInstanceNotFoundFault
    _DBInstanceNotFoundFault,

    -- ** InvalidDBProxyStateFault
    _InvalidDBProxyStateFault,

    -- ** GlobalClusterQuotaExceededFault
    _GlobalClusterQuotaExceededFault,

    -- ** DBSecurityGroupNotFoundFault
    _DBSecurityGroupNotFoundFault,

    -- ** DBSecurityGroupAlreadyExistsFault
    _DBSecurityGroupAlreadyExistsFault,

    -- ** DBInstanceAlreadyExistsFault
    _DBInstanceAlreadyExistsFault,

    -- ** OptionGroupQuotaExceededFault
    _OptionGroupQuotaExceededFault,

    -- ** InvalidDBSecurityGroupStateFault
    _InvalidDBSecurityGroupStateFault,

    -- ** DBSnapshotNotFoundFault
    _DBSnapshotNotFoundFault,

    -- ** DBSubnetGroupNotFoundFault
    _DBSubnetGroupNotFoundFault,

    -- ** DBClusterQuotaExceededFault
    _DBClusterQuotaExceededFault,

    -- ** InvalidDBClusterCapacityFault
    _InvalidDBClusterCapacityFault,

    -- ** CustomAvailabilityZoneQuotaExceededFault
    _CustomAvailabilityZoneQuotaExceededFault,

    -- ** ExportTaskNotFoundFault
    _ExportTaskNotFoundFault,

    -- ** InsufficientAvailableIPsInSubnetFault
    _InsufficientAvailableIPsInSubnetFault,

    -- ** DBSecurityGroupQuotaExceededFault
    _DBSecurityGroupQuotaExceededFault,

    -- ** InvalidDBClusterStateFault
    _InvalidDBClusterStateFault,

    -- ** OptionGroupAlreadyExistsFault
    _OptionGroupAlreadyExistsFault,

    -- ** CustomAvailabilityZoneAlreadyExistsFault
    _CustomAvailabilityZoneAlreadyExistsFault,

    -- ** DBClusterAlreadyExistsFault
    _DBClusterAlreadyExistsFault,

    -- ** InvalidOptionGroupStateFault
    _InvalidOptionGroupStateFault,

    -- ** GlobalClusterNotFoundFault
    _GlobalClusterNotFoundFault,

    -- ** InvalidS3BucketFault
    _InvalidS3BucketFault,

    -- ** InvalidExportSourceStateFault
    _InvalidExportSourceStateFault,

    -- ** DBClusterNotFoundFault
    _DBClusterNotFoundFault,

    -- ** CustomAvailabilityZoneNotFoundFault
    _CustomAvailabilityZoneNotFoundFault,

    -- ** DBSubnetGroupQuotaExceededFault
    _DBSubnetGroupQuotaExceededFault,

    -- ** OptionGroupNotFoundFault
    _OptionGroupNotFoundFault,

    -- ** DBLogFileNotFoundFault
    _DBLogFileNotFoundFault,

    -- ** InvalidExportTaskStateFault
    _InvalidExportTaskStateFault,

    -- ** SubscriptionCategoryNotFoundFault
    _SubscriptionCategoryNotFoundFault,

    -- ** AuthorizationNotFoundFault
    _AuthorizationNotFoundFault,

    -- ** InvalidSubnet
    _InvalidSubnet,

    -- ** SubscriptionNotFoundFault
    _SubscriptionNotFoundFault,

    -- * Waiters
    -- $waiters

    -- ** DBSnapshotDeleted
    newDBSnapshotDeleted,

    -- ** DBSnapshotCompleted
    newDBSnapshotCompleted,

    -- ** DBInstanceAvailable
    newDBInstanceAvailable,

    -- ** DBSnapshotAvailable
    newDBSnapshotAvailable,

    -- ** DBClusterSnapshotDeleted
    newDBClusterSnapshotDeleted,

    -- ** DBInstanceDeleted
    newDBInstanceDeleted,

    -- ** DBClusterSnapshotAvailable
    newDBClusterSnapshotAvailable,

    -- * Operations
    -- $operations

    -- ** ResetDBClusterParameterGroup
    ResetDBClusterParameterGroup (ResetDBClusterParameterGroup'),
    newResetDBClusterParameterGroup,
    DBClusterParameterGroupNameMessage (DBClusterParameterGroupNameMessage'),
    newDBClusterParameterGroupNameMessage,

    -- ** StopDBInstance
    StopDBInstance (StopDBInstance'),
    newStopDBInstance,
    StopDBInstanceResponse (StopDBInstanceResponse'),
    newStopDBInstanceResponse,

    -- ** ModifyDBInstance
    ModifyDBInstance (ModifyDBInstance'),
    newModifyDBInstance,
    ModifyDBInstanceResponse (ModifyDBInstanceResponse'),
    newModifyDBInstanceResponse,

    -- ** DescribeEventCategories
    DescribeEventCategories (DescribeEventCategories'),
    newDescribeEventCategories,
    DescribeEventCategoriesResponse (DescribeEventCategoriesResponse'),
    newDescribeEventCategoriesResponse,

    -- ** StartDBInstance
    StartDBInstance (StartDBInstance'),
    newStartDBInstance,
    StartDBInstanceResponse (StartDBInstanceResponse'),
    newStartDBInstanceResponse,

    -- ** CopyDBClusterParameterGroup
    CopyDBClusterParameterGroup (CopyDBClusterParameterGroup'),
    newCopyDBClusterParameterGroup,
    CopyDBClusterParameterGroupResponse (CopyDBClusterParameterGroupResponse'),
    newCopyDBClusterParameterGroupResponse,

    -- ** DescribeExportTasks (Paginated)
    DescribeExportTasks (DescribeExportTasks'),
    newDescribeExportTasks,
    DescribeExportTasksResponse (DescribeExportTasksResponse'),
    newDescribeExportTasksResponse,

    -- ** CopyDBSnapshot
    CopyDBSnapshot (CopyDBSnapshot'),
    newCopyDBSnapshot,
    CopyDBSnapshotResponse (CopyDBSnapshotResponse'),
    newCopyDBSnapshotResponse,

    -- ** PromoteReadReplica
    PromoteReadReplica (PromoteReadReplica'),
    newPromoteReadReplica,
    PromoteReadReplicaResponse (PromoteReadReplicaResponse'),
    newPromoteReadReplicaResponse,

    -- ** DescribeDBSnapshots (Paginated)
    DescribeDBSnapshots (DescribeDBSnapshots'),
    newDescribeDBSnapshots,
    DescribeDBSnapshotsResponse (DescribeDBSnapshotsResponse'),
    newDescribeDBSnapshotsResponse,

    -- ** ModifyDBCluster
    ModifyDBCluster (ModifyDBCluster'),
    newModifyDBCluster,
    ModifyDBClusterResponse (ModifyDBClusterResponse'),
    newModifyDBClusterResponse,

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

    -- ** CreateDBClusterSnapshot
    CreateDBClusterSnapshot (CreateDBClusterSnapshot'),
    newCreateDBClusterSnapshot,
    CreateDBClusterSnapshotResponse (CreateDBClusterSnapshotResponse'),
    newCreateDBClusterSnapshotResponse,

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

    -- ** StartDBCluster
    StartDBCluster (StartDBCluster'),
    newStartDBCluster,
    StartDBClusterResponse (StartDBClusterResponse'),
    newStartDBClusterResponse,

    -- ** DescribeOptionGroupOptions (Paginated)
    DescribeOptionGroupOptions (DescribeOptionGroupOptions'),
    newDescribeOptionGroupOptions,
    DescribeOptionGroupOptionsResponse (DescribeOptionGroupOptionsResponse'),
    newDescribeOptionGroupOptionsResponse,

    -- ** DeleteInstallationMedia
    DeleteInstallationMedia (DeleteInstallationMedia'),
    newDeleteInstallationMedia,
    InstallationMedia (InstallationMedia'),
    newInstallationMedia,

    -- ** CopyOptionGroup
    CopyOptionGroup (CopyOptionGroup'),
    newCopyOptionGroup,
    CopyOptionGroupResponse (CopyOptionGroupResponse'),
    newCopyOptionGroupResponse,

    -- ** DescribeDBInstanceAutomatedBackups (Paginated)
    DescribeDBInstanceAutomatedBackups (DescribeDBInstanceAutomatedBackups'),
    newDescribeDBInstanceAutomatedBackups,
    DescribeDBInstanceAutomatedBackupsResponse (DescribeDBInstanceAutomatedBackupsResponse'),
    newDescribeDBInstanceAutomatedBackupsResponse,

    -- ** ModifyGlobalCluster
    ModifyGlobalCluster (ModifyGlobalCluster'),
    newModifyGlobalCluster,
    ModifyGlobalClusterResponse (ModifyGlobalClusterResponse'),
    newModifyGlobalClusterResponse,

    -- ** DescribeDBProxies (Paginated)
    DescribeDBProxies (DescribeDBProxies'),
    newDescribeDBProxies,
    DescribeDBProxiesResponse (DescribeDBProxiesResponse'),
    newDescribeDBProxiesResponse,

    -- ** RestoreDBClusterToPointInTime
    RestoreDBClusterToPointInTime (RestoreDBClusterToPointInTime'),
    newRestoreDBClusterToPointInTime,
    RestoreDBClusterToPointInTimeResponse (RestoreDBClusterToPointInTimeResponse'),
    newRestoreDBClusterToPointInTimeResponse,

    -- ** StopActivityStream
    StopActivityStream (StopActivityStream'),
    newStopActivityStream,
    StopActivityStreamResponse (StopActivityStreamResponse'),
    newStopActivityStreamResponse,

    -- ** CreateDBInstanceReadReplica
    CreateDBInstanceReadReplica (CreateDBInstanceReadReplica'),
    newCreateDBInstanceReadReplica,
    CreateDBInstanceReadReplicaResponse (CreateDBInstanceReadReplicaResponse'),
    newCreateDBInstanceReadReplicaResponse,

    -- ** DescribeValidDBInstanceModifications
    DescribeValidDBInstanceModifications (DescribeValidDBInstanceModifications'),
    newDescribeValidDBInstanceModifications,
    DescribeValidDBInstanceModificationsResponse (DescribeValidDBInstanceModificationsResponse'),
    newDescribeValidDBInstanceModificationsResponse,

    -- ** StartExportTask
    StartExportTask (StartExportTask'),
    newStartExportTask,
    ExportTask (ExportTask'),
    newExportTask,

    -- ** DescribeDBParameters (Paginated)
    DescribeDBParameters (DescribeDBParameters'),
    newDescribeDBParameters,
    DescribeDBParametersResponse (DescribeDBParametersResponse'),
    newDescribeDBParametersResponse,

    -- ** DescribeDBInstances (Paginated)
    DescribeDBInstances (DescribeDBInstances'),
    newDescribeDBInstances,
    DescribeDBInstancesResponse (DescribeDBInstancesResponse'),
    newDescribeDBInstancesResponse,

    -- ** DescribeEventSubscriptions (Paginated)
    DescribeEventSubscriptions (DescribeEventSubscriptions'),
    newDescribeEventSubscriptions,
    DescribeEventSubscriptionsResponse (DescribeEventSubscriptionsResponse'),
    newDescribeEventSubscriptionsResponse,

    -- ** RestoreDBInstanceFromS
    RestoreDBInstanceFromS (RestoreDBInstanceFromS'),
    newRestoreDBInstanceFromS,
    RestoreDBInstanceFromSResponse (RestoreDBInstanceFromSResponse'),
    newRestoreDBInstanceFromSResponse,

    -- ** StartActivityStream
    StartActivityStream (StartActivityStream'),
    newStartActivityStream,
    StartActivityStreamResponse (StartActivityStreamResponse'),
    newStartActivityStreamResponse,

    -- ** DescribeDBClusterEndpoints (Paginated)
    DescribeDBClusterEndpoints (DescribeDBClusterEndpoints'),
    newDescribeDBClusterEndpoints,
    DescribeDBClusterEndpointsResponse (DescribeDBClusterEndpointsResponse'),
    newDescribeDBClusterEndpointsResponse,

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

    -- ** DeleteDBClusterEndpoint
    DeleteDBClusterEndpoint (DeleteDBClusterEndpoint'),
    newDeleteDBClusterEndpoint,
    DBClusterEndpoint (DBClusterEndpoint'),
    newDBClusterEndpoint,

    -- ** ModifyDBSnapshotAttribute
    ModifyDBSnapshotAttribute (ModifyDBSnapshotAttribute'),
    newModifyDBSnapshotAttribute,
    ModifyDBSnapshotAttributeResponse (ModifyDBSnapshotAttributeResponse'),
    newModifyDBSnapshotAttributeResponse,

    -- ** DescribeReservedDBInstancesOfferings (Paginated)
    DescribeReservedDBInstancesOfferings (DescribeReservedDBInstancesOfferings'),
    newDescribeReservedDBInstancesOfferings,
    DescribeReservedDBInstancesOfferingsResponse (DescribeReservedDBInstancesOfferingsResponse'),
    newDescribeReservedDBInstancesOfferingsResponse,

    -- ** DeleteDBInstance
    DeleteDBInstance (DeleteDBInstance'),
    newDeleteDBInstance,
    DeleteDBInstanceResponse (DeleteDBInstanceResponse'),
    newDeleteDBInstanceResponse,

    -- ** RebootDBInstance
    RebootDBInstance (RebootDBInstance'),
    newRebootDBInstance,
    RebootDBInstanceResponse (RebootDBInstanceResponse'),
    newRebootDBInstanceResponse,

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

    -- ** DeleteDBInstanceAutomatedBackup
    DeleteDBInstanceAutomatedBackup (DeleteDBInstanceAutomatedBackup'),
    newDeleteDBInstanceAutomatedBackup,
    DeleteDBInstanceAutomatedBackupResponse (DeleteDBInstanceAutomatedBackupResponse'),
    newDeleteDBInstanceAutomatedBackupResponse,

    -- ** DescribeDBClusterBacktracks (Paginated)
    DescribeDBClusterBacktracks (DescribeDBClusterBacktracks'),
    newDescribeDBClusterBacktracks,
    DescribeDBClusterBacktracksResponse (DescribeDBClusterBacktracksResponse'),
    newDescribeDBClusterBacktracksResponse,

    -- ** CreateDBClusterParameterGroup
    CreateDBClusterParameterGroup (CreateDBClusterParameterGroup'),
    newCreateDBClusterParameterGroup,
    CreateDBClusterParameterGroupResponse (CreateDBClusterParameterGroupResponse'),
    newCreateDBClusterParameterGroupResponse,

    -- ** CreateDBProxy
    CreateDBProxy (CreateDBProxy'),
    newCreateDBProxy,
    CreateDBProxyResponse (CreateDBProxyResponse'),
    newCreateDBProxyResponse,

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

    -- ** DescribeAccountAttributes
    DescribeAccountAttributes (DescribeAccountAttributes'),
    newDescribeAccountAttributes,
    DescribeAccountAttributesResponse (DescribeAccountAttributesResponse'),
    newDescribeAccountAttributesResponse,

    -- ** RemoveTagsFromResource
    RemoveTagsFromResource (RemoveTagsFromResource'),
    newRemoveTagsFromResource,
    RemoveTagsFromResourceResponse (RemoveTagsFromResourceResponse'),
    newRemoveTagsFromResourceResponse,

    -- ** CreateDBSecurityGroup
    CreateDBSecurityGroup (CreateDBSecurityGroup'),
    newCreateDBSecurityGroup,
    CreateDBSecurityGroupResponse (CreateDBSecurityGroupResponse'),
    newCreateDBSecurityGroupResponse,

    -- ** DescribeCertificates (Paginated)
    DescribeCertificates (DescribeCertificates'),
    newDescribeCertificates,
    DescribeCertificatesResponse (DescribeCertificatesResponse'),
    newDescribeCertificatesResponse,

    -- ** CreateDBInstance
    CreateDBInstance (CreateDBInstance'),
    newCreateDBInstance,
    CreateDBInstanceResponse (CreateDBInstanceResponse'),
    newCreateDBInstanceResponse,

    -- ** RemoveRoleFromDBInstance
    RemoveRoleFromDBInstance (RemoveRoleFromDBInstance'),
    newRemoveRoleFromDBInstance,
    RemoveRoleFromDBInstanceResponse (RemoveRoleFromDBInstanceResponse'),
    newRemoveRoleFromDBInstanceResponse,

    -- ** DescribeEngineDefaultClusterParameters (Paginated)
    DescribeEngineDefaultClusterParameters (DescribeEngineDefaultClusterParameters'),
    newDescribeEngineDefaultClusterParameters,
    DescribeEngineDefaultClusterParametersResponse (DescribeEngineDefaultClusterParametersResponse'),
    newDescribeEngineDefaultClusterParametersResponse,

    -- ** StartDBInstanceAutomatedBackupsReplication
    StartDBInstanceAutomatedBackupsReplication (StartDBInstanceAutomatedBackupsReplication'),
    newStartDBInstanceAutomatedBackupsReplication,
    StartDBInstanceAutomatedBackupsReplicationResponse (StartDBInstanceAutomatedBackupsReplicationResponse'),
    newStartDBInstanceAutomatedBackupsReplicationResponse,

    -- ** CreateOptionGroup
    CreateOptionGroup (CreateOptionGroup'),
    newCreateOptionGroup,
    CreateOptionGroupResponse (CreateOptionGroupResponse'),
    newCreateOptionGroupResponse,

    -- ** CreateCustomAvailabilityZone
    CreateCustomAvailabilityZone (CreateCustomAvailabilityZone'),
    newCreateCustomAvailabilityZone,
    CreateCustomAvailabilityZoneResponse (CreateCustomAvailabilityZoneResponse'),
    newCreateCustomAvailabilityZoneResponse,

    -- ** StopDBInstanceAutomatedBackupsReplication
    StopDBInstanceAutomatedBackupsReplication (StopDBInstanceAutomatedBackupsReplication'),
    newStopDBInstanceAutomatedBackupsReplication,
    StopDBInstanceAutomatedBackupsReplicationResponse (StopDBInstanceAutomatedBackupsReplicationResponse'),
    newStopDBInstanceAutomatedBackupsReplicationResponse,

    -- ** FailoverDBCluster
    FailoverDBCluster (FailoverDBCluster'),
    newFailoverDBCluster,
    FailoverDBClusterResponse (FailoverDBClusterResponse'),
    newFailoverDBClusterResponse,

    -- ** RemoveRoleFromDBCluster
    RemoveRoleFromDBCluster (RemoveRoleFromDBCluster'),
    newRemoveRoleFromDBCluster,
    RemoveRoleFromDBClusterResponse (RemoveRoleFromDBClusterResponse'),
    newRemoveRoleFromDBClusterResponse,

    -- ** DescribeDBSnapshotAttributes
    DescribeDBSnapshotAttributes (DescribeDBSnapshotAttributes'),
    newDescribeDBSnapshotAttributes,
    DescribeDBSnapshotAttributesResponse (DescribeDBSnapshotAttributesResponse'),
    newDescribeDBSnapshotAttributesResponse,

    -- ** CreateDBCluster
    CreateDBCluster (CreateDBCluster'),
    newCreateDBCluster,
    CreateDBClusterResponse (CreateDBClusterResponse'),
    newCreateDBClusterResponse,

    -- ** ApplyPendingMaintenanceAction
    ApplyPendingMaintenanceAction (ApplyPendingMaintenanceAction'),
    newApplyPendingMaintenanceAction,
    ApplyPendingMaintenanceActionResponse (ApplyPendingMaintenanceActionResponse'),
    newApplyPendingMaintenanceActionResponse,

    -- ** BacktrackDBCluster
    BacktrackDBCluster (BacktrackDBCluster'),
    newBacktrackDBCluster,
    DBClusterBacktrack (DBClusterBacktrack'),
    newDBClusterBacktrack,

    -- ** DescribeReservedDBInstances (Paginated)
    DescribeReservedDBInstances (DescribeReservedDBInstances'),
    newDescribeReservedDBInstances,
    DescribeReservedDBInstancesResponse (DescribeReservedDBInstancesResponse'),
    newDescribeReservedDBInstancesResponse,

    -- ** DeleteDBCluster
    DeleteDBCluster (DeleteDBCluster'),
    newDeleteDBCluster,
    DeleteDBClusterResponse (DeleteDBClusterResponse'),
    newDeleteDBClusterResponse,

    -- ** DeleteOptionGroup
    DeleteOptionGroup (DeleteOptionGroup'),
    newDeleteOptionGroup,
    DeleteOptionGroupResponse (DeleteOptionGroupResponse'),
    newDeleteOptionGroupResponse,

    -- ** DescribePendingMaintenanceActions (Paginated)
    DescribePendingMaintenanceActions (DescribePendingMaintenanceActions'),
    newDescribePendingMaintenanceActions,
    DescribePendingMaintenanceActionsResponse (DescribePendingMaintenanceActionsResponse'),
    newDescribePendingMaintenanceActionsResponse,

    -- ** DescribeDBProxyTargetGroups (Paginated)
    DescribeDBProxyTargetGroups (DescribeDBProxyTargetGroups'),
    newDescribeDBProxyTargetGroups,
    DescribeDBProxyTargetGroupsResponse (DescribeDBProxyTargetGroupsResponse'),
    newDescribeDBProxyTargetGroupsResponse,

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

    -- ** DescribeOptionGroups (Paginated)
    DescribeOptionGroups (DescribeOptionGroups'),
    newDescribeOptionGroups,
    DescribeOptionGroupsResponse (DescribeOptionGroupsResponse'),
    newDescribeOptionGroupsResponse,

    -- ** DescribeDBClusters (Paginated)
    DescribeDBClusters (DescribeDBClusters'),
    newDescribeDBClusters,
    DescribeDBClustersResponse (DescribeDBClustersResponse'),
    newDescribeDBClustersResponse,

    -- ** DescribeEvents (Paginated)
    DescribeEvents (DescribeEvents'),
    newDescribeEvents,
    DescribeEventsResponse (DescribeEventsResponse'),
    newDescribeEventsResponse,

    -- ** ModifyDBProxy
    ModifyDBProxy (ModifyDBProxy'),
    newModifyDBProxy,
    ModifyDBProxyResponse (ModifyDBProxyResponse'),
    newModifyDBProxyResponse,

    -- ** DescribeDBLogFiles (Paginated)
    DescribeDBLogFiles (DescribeDBLogFiles'),
    newDescribeDBLogFiles,
    DescribeDBLogFilesResponse (DescribeDBLogFilesResponse'),
    newDescribeDBLogFilesResponse,

    -- ** RestoreDBClusterFromS
    RestoreDBClusterFromS (RestoreDBClusterFromS'),
    newRestoreDBClusterFromS,
    RestoreDBClusterFromSResponse (RestoreDBClusterFromSResponse'),
    newRestoreDBClusterFromSResponse,

    -- ** CancelExportTask
    CancelExportTask (CancelExportTask'),
    newCancelExportTask,
    ExportTask (ExportTask'),
    newExportTask,

    -- ** DescribeGlobalClusters (Paginated)
    DescribeGlobalClusters (DescribeGlobalClusters'),
    newDescribeGlobalClusters,
    DescribeGlobalClustersResponse (DescribeGlobalClustersResponse'),
    newDescribeGlobalClustersResponse,

    -- ** AddSourceIdentifierToSubscription
    AddSourceIdentifierToSubscription (AddSourceIdentifierToSubscription'),
    newAddSourceIdentifierToSubscription,
    AddSourceIdentifierToSubscriptionResponse (AddSourceIdentifierToSubscriptionResponse'),
    newAddSourceIdentifierToSubscriptionResponse,

    -- ** ModifyEventSubscription
    ModifyEventSubscription (ModifyEventSubscription'),
    newModifyEventSubscription,
    ModifyEventSubscriptionResponse (ModifyEventSubscriptionResponse'),
    newModifyEventSubscriptionResponse,

    -- ** ModifyDBClusterEndpoint
    ModifyDBClusterEndpoint (ModifyDBClusterEndpoint'),
    newModifyDBClusterEndpoint,
    DBClusterEndpoint (DBClusterEndpoint'),
    newDBClusterEndpoint,

    -- ** ModifyDBClusterSnapshotAttribute
    ModifyDBClusterSnapshotAttribute (ModifyDBClusterSnapshotAttribute'),
    newModifyDBClusterSnapshotAttribute,
    ModifyDBClusterSnapshotAttributeResponse (ModifyDBClusterSnapshotAttributeResponse'),
    newModifyDBClusterSnapshotAttributeResponse,

    -- ** DescribeDBSubnetGroups (Paginated)
    DescribeDBSubnetGroups (DescribeDBSubnetGroups'),
    newDescribeDBSubnetGroups,
    DescribeDBSubnetGroupsResponse (DescribeDBSubnetGroupsResponse'),
    newDescribeDBSubnetGroupsResponse,

    -- ** ModifyDBProxyTargetGroup
    ModifyDBProxyTargetGroup (ModifyDBProxyTargetGroup'),
    newModifyDBProxyTargetGroup,
    ModifyDBProxyTargetGroupResponse (ModifyDBProxyTargetGroupResponse'),
    newModifyDBProxyTargetGroupResponse,

    -- ** CreateDBParameterGroup
    CreateDBParameterGroup (CreateDBParameterGroup'),
    newCreateDBParameterGroup,
    CreateDBParameterGroupResponse (CreateDBParameterGroupResponse'),
    newCreateDBParameterGroupResponse,

    -- ** DeleteDBClusterSnapshot
    DeleteDBClusterSnapshot (DeleteDBClusterSnapshot'),
    newDeleteDBClusterSnapshot,
    DeleteDBClusterSnapshotResponse (DeleteDBClusterSnapshotResponse'),
    newDeleteDBClusterSnapshotResponse,

    -- ** RemoveFromGlobalCluster
    RemoveFromGlobalCluster (RemoveFromGlobalCluster'),
    newRemoveFromGlobalCluster,
    RemoveFromGlobalClusterResponse (RemoveFromGlobalClusterResponse'),
    newRemoveFromGlobalClusterResponse,

    -- ** DescribeDBSecurityGroups (Paginated)
    DescribeDBSecurityGroups (DescribeDBSecurityGroups'),
    newDescribeDBSecurityGroups,
    DescribeDBSecurityGroupsResponse (DescribeDBSecurityGroupsResponse'),
    newDescribeDBSecurityGroupsResponse,

    -- ** AddTagsToResource
    AddTagsToResource (AddTagsToResource'),
    newAddTagsToResource,
    AddTagsToResourceResponse (AddTagsToResourceResponse'),
    newAddTagsToResourceResponse,

    -- ** RegisterDBProxyTargets
    RegisterDBProxyTargets (RegisterDBProxyTargets'),
    newRegisterDBProxyTargets,
    RegisterDBProxyTargetsResponse (RegisterDBProxyTargetsResponse'),
    newRegisterDBProxyTargetsResponse,

    -- ** DownloadDBLogFilePortion (Paginated)
    DownloadDBLogFilePortion (DownloadDBLogFilePortion'),
    newDownloadDBLogFilePortion,
    DownloadDBLogFilePortionResponse (DownloadDBLogFilePortionResponse'),
    newDownloadDBLogFilePortionResponse,

    -- ** ModifyCurrentDBClusterCapacity
    ModifyCurrentDBClusterCapacity (ModifyCurrentDBClusterCapacity'),
    newModifyCurrentDBClusterCapacity,
    ModifyCurrentDBClusterCapacityResponse (ModifyCurrentDBClusterCapacityResponse'),
    newModifyCurrentDBClusterCapacityResponse,

    -- ** AddRoleToDBInstance
    AddRoleToDBInstance (AddRoleToDBInstance'),
    newAddRoleToDBInstance,
    AddRoleToDBInstanceResponse (AddRoleToDBInstanceResponse'),
    newAddRoleToDBInstanceResponse,

    -- ** DeleteDBParameterGroup
    DeleteDBParameterGroup (DeleteDBParameterGroup'),
    newDeleteDBParameterGroup,
    DeleteDBParameterGroupResponse (DeleteDBParameterGroupResponse'),
    newDeleteDBParameterGroupResponse,

    -- ** CreateDBSnapshot
    CreateDBSnapshot (CreateDBSnapshot'),
    newCreateDBSnapshot,
    CreateDBSnapshotResponse (CreateDBSnapshotResponse'),
    newCreateDBSnapshotResponse,

    -- ** CreateDBSubnetGroup
    CreateDBSubnetGroup (CreateDBSubnetGroup'),
    newCreateDBSubnetGroup,
    CreateDBSubnetGroupResponse (CreateDBSubnetGroupResponse'),
    newCreateDBSubnetGroupResponse,

    -- ** DescribeOrderableDBInstanceOptions (Paginated)
    DescribeOrderableDBInstanceOptions (DescribeOrderableDBInstanceOptions'),
    newDescribeOrderableDBInstanceOptions,
    DescribeOrderableDBInstanceOptionsResponse (DescribeOrderableDBInstanceOptionsResponse'),
    newDescribeOrderableDBInstanceOptionsResponse,

    -- ** DeleteDBSecurityGroup
    DeleteDBSecurityGroup (DeleteDBSecurityGroup'),
    newDeleteDBSecurityGroup,
    DeleteDBSecurityGroupResponse (DeleteDBSecurityGroupResponse'),
    newDeleteDBSecurityGroupResponse,

    -- ** RestoreDBClusterFromSnapshot
    RestoreDBClusterFromSnapshot (RestoreDBClusterFromSnapshot'),
    newRestoreDBClusterFromSnapshot,
    RestoreDBClusterFromSnapshotResponse (RestoreDBClusterFromSnapshotResponse'),
    newRestoreDBClusterFromSnapshotResponse,

    -- ** RestoreDBInstanceFromDBSnapshot
    RestoreDBInstanceFromDBSnapshot (RestoreDBInstanceFromDBSnapshot'),
    newRestoreDBInstanceFromDBSnapshot,
    RestoreDBInstanceFromDBSnapshotResponse (RestoreDBInstanceFromDBSnapshotResponse'),
    newRestoreDBInstanceFromDBSnapshotResponse,

    -- ** CreateDBClusterEndpoint
    CreateDBClusterEndpoint (CreateDBClusterEndpoint'),
    newCreateDBClusterEndpoint,
    DBClusterEndpoint (DBClusterEndpoint'),
    newDBClusterEndpoint,

    -- ** CreateEventSubscription
    CreateEventSubscription (CreateEventSubscription'),
    newCreateEventSubscription,
    CreateEventSubscriptionResponse (CreateEventSubscriptionResponse'),
    newCreateEventSubscriptionResponse,

    -- ** DeleteDBSnapshot
    DeleteDBSnapshot (DeleteDBSnapshot'),
    newDeleteDBSnapshot,
    DeleteDBSnapshotResponse (DeleteDBSnapshotResponse'),
    newDeleteDBSnapshotResponse,

    -- ** DeleteDBClusterParameterGroup
    DeleteDBClusterParameterGroup (DeleteDBClusterParameterGroup'),
    newDeleteDBClusterParameterGroup,
    DeleteDBClusterParameterGroupResponse (DeleteDBClusterParameterGroupResponse'),
    newDeleteDBClusterParameterGroupResponse,

    -- ** DescribeSourceRegions (Paginated)
    DescribeSourceRegions (DescribeSourceRegions'),
    newDescribeSourceRegions,
    DescribeSourceRegionsResponse (DescribeSourceRegionsResponse'),
    newDescribeSourceRegionsResponse,

    -- ** PurchaseReservedDBInstancesOffering
    PurchaseReservedDBInstancesOffering (PurchaseReservedDBInstancesOffering'),
    newPurchaseReservedDBInstancesOffering,
    PurchaseReservedDBInstancesOfferingResponse (PurchaseReservedDBInstancesOfferingResponse'),
    newPurchaseReservedDBInstancesOfferingResponse,

    -- ** AuthorizeDBSecurityGroupIngress
    AuthorizeDBSecurityGroupIngress (AuthorizeDBSecurityGroupIngress'),
    newAuthorizeDBSecurityGroupIngress,
    AuthorizeDBSecurityGroupIngressResponse (AuthorizeDBSecurityGroupIngressResponse'),
    newAuthorizeDBSecurityGroupIngressResponse,

    -- ** DeleteDBProxy
    DeleteDBProxy (DeleteDBProxy'),
    newDeleteDBProxy,
    DeleteDBProxyResponse (DeleteDBProxyResponse'),
    newDeleteDBProxyResponse,

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

    -- ** DeregisterDBProxyTargets
    DeregisterDBProxyTargets (DeregisterDBProxyTargets'),
    newDeregisterDBProxyTargets,
    DeregisterDBProxyTargetsResponse (DeregisterDBProxyTargetsResponse'),
    newDeregisterDBProxyTargetsResponse,

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

    -- ** DescribeDBProxyTargets (Paginated)
    DescribeDBProxyTargets (DescribeDBProxyTargets'),
    newDescribeDBProxyTargets,
    DescribeDBProxyTargetsResponse (DescribeDBProxyTargetsResponse'),
    newDescribeDBProxyTargetsResponse,

    -- ** ImportInstallationMedia
    ImportInstallationMedia (ImportInstallationMedia'),
    newImportInstallationMedia,
    InstallationMedia (InstallationMedia'),
    newInstallationMedia,

    -- ** DeleteCustomAvailabilityZone
    DeleteCustomAvailabilityZone (DeleteCustomAvailabilityZone'),
    newDeleteCustomAvailabilityZone,
    DeleteCustomAvailabilityZoneResponse (DeleteCustomAvailabilityZoneResponse'),
    newDeleteCustomAvailabilityZoneResponse,

    -- ** FailoverGlobalCluster
    FailoverGlobalCluster (FailoverGlobalCluster'),
    newFailoverGlobalCluster,
    FailoverGlobalClusterResponse (FailoverGlobalClusterResponse'),
    newFailoverGlobalClusterResponse,

    -- ** RemoveSourceIdentifierFromSubscription
    RemoveSourceIdentifierFromSubscription (RemoveSourceIdentifierFromSubscription'),
    newRemoveSourceIdentifierFromSubscription,
    RemoveSourceIdentifierFromSubscriptionResponse (RemoveSourceIdentifierFromSubscriptionResponse'),
    newRemoveSourceIdentifierFromSubscriptionResponse,

    -- ** CopyDBClusterSnapshot
    CopyDBClusterSnapshot (CopyDBClusterSnapshot'),
    newCopyDBClusterSnapshot,
    CopyDBClusterSnapshotResponse (CopyDBClusterSnapshotResponse'),
    newCopyDBClusterSnapshotResponse,

    -- ** DescribeInstallationMedia (Paginated)
    DescribeInstallationMedia (DescribeInstallationMedia'),
    newDescribeInstallationMedia,
    DescribeInstallationMediaResponse (DescribeInstallationMediaResponse'),
    newDescribeInstallationMediaResponse,

    -- ** CopyDBParameterGroup
    CopyDBParameterGroup (CopyDBParameterGroup'),
    newCopyDBParameterGroup,
    CopyDBParameterGroupResponse (CopyDBParameterGroupResponse'),
    newCopyDBParameterGroupResponse,

    -- ** RestoreDBInstanceToPointInTime
    RestoreDBInstanceToPointInTime (RestoreDBInstanceToPointInTime'),
    newRestoreDBInstanceToPointInTime,
    RestoreDBInstanceToPointInTimeResponse (RestoreDBInstanceToPointInTimeResponse'),
    newRestoreDBInstanceToPointInTimeResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** ModifyDBSnapshot
    ModifyDBSnapshot (ModifyDBSnapshot'),
    newModifyDBSnapshot,
    ModifyDBSnapshotResponse (ModifyDBSnapshotResponse'),
    newModifyDBSnapshotResponse,

    -- ** ModifyDBSubnetGroup
    ModifyDBSubnetGroup (ModifyDBSubnetGroup'),
    newModifyDBSubnetGroup,
    ModifyDBSubnetGroupResponse (ModifyDBSubnetGroupResponse'),
    newModifyDBSubnetGroupResponse,

    -- ** DescribeEngineDefaultParameters (Paginated)
    DescribeEngineDefaultParameters (DescribeEngineDefaultParameters'),
    newDescribeEngineDefaultParameters,
    DescribeEngineDefaultParametersResponse (DescribeEngineDefaultParametersResponse'),
    newDescribeEngineDefaultParametersResponse,

    -- ** DescribeCustomAvailabilityZones (Paginated)
    DescribeCustomAvailabilityZones (DescribeCustomAvailabilityZones'),
    newDescribeCustomAvailabilityZones,
    DescribeCustomAvailabilityZonesResponse (DescribeCustomAvailabilityZonesResponse'),
    newDescribeCustomAvailabilityZonesResponse,

    -- ** AddRoleToDBCluster
    AddRoleToDBCluster (AddRoleToDBCluster'),
    newAddRoleToDBCluster,
    AddRoleToDBClusterResponse (AddRoleToDBClusterResponse'),
    newAddRoleToDBClusterResponse,

    -- * Types

    -- ** ActivityStreamMode
    ActivityStreamMode (..),

    -- ** ActivityStreamStatus
    ActivityStreamStatus (..),

    -- ** ApplyMethod
    ApplyMethod (..),

    -- ** AuthScheme
    AuthScheme (..),

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
