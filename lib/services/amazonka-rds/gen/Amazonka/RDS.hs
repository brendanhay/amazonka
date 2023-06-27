{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.RDS
-- Copyright   : (c) 2013-2023 Brendan Hay
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
module Amazonka.RDS
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** AuthorizationAlreadyExistsFault
    _AuthorizationAlreadyExistsFault,

    -- ** AuthorizationNotFoundFault
    _AuthorizationNotFoundFault,

    -- ** AuthorizationQuotaExceededFault
    _AuthorizationQuotaExceededFault,

    -- ** BackupPolicyNotFoundFault
    _BackupPolicyNotFoundFault,

    -- ** BlueGreenDeploymentAlreadyExistsFault
    _BlueGreenDeploymentAlreadyExistsFault,

    -- ** BlueGreenDeploymentNotFoundFault
    _BlueGreenDeploymentNotFoundFault,

    -- ** CertificateNotFoundFault
    _CertificateNotFoundFault,

    -- ** CreateCustomDBEngineVersionFault
    _CreateCustomDBEngineVersionFault,

    -- ** CustomAvailabilityZoneNotFoundFault
    _CustomAvailabilityZoneNotFoundFault,

    -- ** CustomDBEngineVersionAlreadyExistsFault
    _CustomDBEngineVersionAlreadyExistsFault,

    -- ** CustomDBEngineVersionNotFoundFault
    _CustomDBEngineVersionNotFoundFault,

    -- ** CustomDBEngineVersionQuotaExceededFault
    _CustomDBEngineVersionQuotaExceededFault,

    -- ** DBClusterAlreadyExistsFault
    _DBClusterAlreadyExistsFault,

    -- ** DBClusterBacktrackNotFoundFault
    _DBClusterBacktrackNotFoundFault,

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

    -- ** DBInstanceAutomatedBackupNotFoundFault
    _DBInstanceAutomatedBackupNotFoundFault,

    -- ** DBInstanceAutomatedBackupQuotaExceededFault
    _DBInstanceAutomatedBackupQuotaExceededFault,

    -- ** DBInstanceNotFoundFault
    _DBInstanceNotFoundFault,

    -- ** DBInstanceRoleAlreadyExistsFault
    _DBInstanceRoleAlreadyExistsFault,

    -- ** DBInstanceRoleNotFoundFault
    _DBInstanceRoleNotFoundFault,

    -- ** DBInstanceRoleQuotaExceededFault
    _DBInstanceRoleQuotaExceededFault,

    -- ** DBLogFileNotFoundFault
    _DBLogFileNotFoundFault,

    -- ** DBParameterGroupAlreadyExistsFault
    _DBParameterGroupAlreadyExistsFault,

    -- ** DBParameterGroupNotFoundFault
    _DBParameterGroupNotFoundFault,

    -- ** DBParameterGroupQuotaExceededFault
    _DBParameterGroupQuotaExceededFault,

    -- ** DBProxyAlreadyExistsFault
    _DBProxyAlreadyExistsFault,

    -- ** DBProxyEndpointAlreadyExistsFault
    _DBProxyEndpointAlreadyExistsFault,

    -- ** DBProxyEndpointNotFoundFault
    _DBProxyEndpointNotFoundFault,

    -- ** DBProxyEndpointQuotaExceededFault
    _DBProxyEndpointQuotaExceededFault,

    -- ** DBProxyNotFoundFault
    _DBProxyNotFoundFault,

    -- ** DBProxyQuotaExceededFault
    _DBProxyQuotaExceededFault,

    -- ** DBProxyTargetAlreadyRegisteredFault
    _DBProxyTargetAlreadyRegisteredFault,

    -- ** DBProxyTargetGroupNotFoundFault
    _DBProxyTargetGroupNotFoundFault,

    -- ** DBProxyTargetNotFoundFault
    _DBProxyTargetNotFoundFault,

    -- ** DBSecurityGroupAlreadyExistsFault
    _DBSecurityGroupAlreadyExistsFault,

    -- ** DBSecurityGroupNotFoundFault
    _DBSecurityGroupNotFoundFault,

    -- ** DBSecurityGroupNotSupportedFault
    _DBSecurityGroupNotSupportedFault,

    -- ** DBSecurityGroupQuotaExceededFault
    _DBSecurityGroupQuotaExceededFault,

    -- ** DBSnapshotAlreadyExistsFault
    _DBSnapshotAlreadyExistsFault,

    -- ** DBSnapshotNotFoundFault
    _DBSnapshotNotFoundFault,

    -- ** DBSubnetGroupAlreadyExistsFault
    _DBSubnetGroupAlreadyExistsFault,

    -- ** DBSubnetGroupDoesNotCoverEnoughAZs
    _DBSubnetGroupDoesNotCoverEnoughAZs,

    -- ** DBSubnetGroupNotAllowedFault
    _DBSubnetGroupNotAllowedFault,

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

    -- ** Ec2ImagePropertiesNotSupportedFault
    _Ec2ImagePropertiesNotSupportedFault,

    -- ** EventSubscriptionQuotaExceededFault
    _EventSubscriptionQuotaExceededFault,

    -- ** ExportTaskAlreadyExistsFault
    _ExportTaskAlreadyExistsFault,

    -- ** ExportTaskNotFoundFault
    _ExportTaskNotFoundFault,

    -- ** GlobalClusterAlreadyExistsFault
    _GlobalClusterAlreadyExistsFault,

    -- ** GlobalClusterNotFoundFault
    _GlobalClusterNotFoundFault,

    -- ** GlobalClusterQuotaExceededFault
    _GlobalClusterQuotaExceededFault,

    -- ** IamRoleMissingPermissionsFault
    _IamRoleMissingPermissionsFault,

    -- ** IamRoleNotFoundFault
    _IamRoleNotFoundFault,

    -- ** InstanceQuotaExceededFault
    _InstanceQuotaExceededFault,

    -- ** InsufficientAvailableIPsInSubnetFault
    _InsufficientAvailableIPsInSubnetFault,

    -- ** InsufficientDBClusterCapacityFault
    _InsufficientDBClusterCapacityFault,

    -- ** InsufficientDBInstanceCapacityFault
    _InsufficientDBInstanceCapacityFault,

    -- ** InsufficientStorageClusterCapacityFault
    _InsufficientStorageClusterCapacityFault,

    -- ** InvalidBlueGreenDeploymentStateFault
    _InvalidBlueGreenDeploymentStateFault,

    -- ** InvalidCustomDBEngineVersionStateFault
    _InvalidCustomDBEngineVersionStateFault,

    -- ** InvalidDBClusterCapacityFault
    _InvalidDBClusterCapacityFault,

    -- ** InvalidDBClusterEndpointStateFault
    _InvalidDBClusterEndpointStateFault,

    -- ** InvalidDBClusterSnapshotStateFault
    _InvalidDBClusterSnapshotStateFault,

    -- ** InvalidDBClusterStateFault
    _InvalidDBClusterStateFault,

    -- ** InvalidDBInstanceAutomatedBackupStateFault
    _InvalidDBInstanceAutomatedBackupStateFault,

    -- ** InvalidDBInstanceStateFault
    _InvalidDBInstanceStateFault,

    -- ** InvalidDBParameterGroupStateFault
    _InvalidDBParameterGroupStateFault,

    -- ** InvalidDBProxyEndpointStateFault
    _InvalidDBProxyEndpointStateFault,

    -- ** InvalidDBProxyStateFault
    _InvalidDBProxyStateFault,

    -- ** InvalidDBSecurityGroupStateFault
    _InvalidDBSecurityGroupStateFault,

    -- ** InvalidDBSnapshotStateFault
    _InvalidDBSnapshotStateFault,

    -- ** InvalidDBSubnetGroupFault
    _InvalidDBSubnetGroupFault,

    -- ** InvalidDBSubnetGroupStateFault
    _InvalidDBSubnetGroupStateFault,

    -- ** InvalidDBSubnetStateFault
    _InvalidDBSubnetStateFault,

    -- ** InvalidEventSubscriptionStateFault
    _InvalidEventSubscriptionStateFault,

    -- ** InvalidExportOnlyFault
    _InvalidExportOnlyFault,

    -- ** InvalidExportSourceStateFault
    _InvalidExportSourceStateFault,

    -- ** InvalidExportTaskStateFault
    _InvalidExportTaskStateFault,

    -- ** InvalidGlobalClusterStateFault
    _InvalidGlobalClusterStateFault,

    -- ** InvalidOptionGroupStateFault
    _InvalidOptionGroupStateFault,

    -- ** InvalidRestoreFault
    _InvalidRestoreFault,

    -- ** InvalidS3BucketFault
    _InvalidS3BucketFault,

    -- ** InvalidSubnet
    _InvalidSubnet,

    -- ** InvalidVPCNetworkStateFault
    _InvalidVPCNetworkStateFault,

    -- ** KMSKeyNotAccessibleFault
    _KMSKeyNotAccessibleFault,

    -- ** NetworkTypeNotSupported
    _NetworkTypeNotSupported,

    -- ** OptionGroupAlreadyExistsFault
    _OptionGroupAlreadyExistsFault,

    -- ** OptionGroupNotFoundFault
    _OptionGroupNotFoundFault,

    -- ** OptionGroupQuotaExceededFault
    _OptionGroupQuotaExceededFault,

    -- ** PointInTimeRestoreNotEnabledFault
    _PointInTimeRestoreNotEnabledFault,

    -- ** ProvisionedIopsNotAvailableInAZFault
    _ProvisionedIopsNotAvailableInAZFault,

    -- ** ReservedDBInstanceAlreadyExistsFault
    _ReservedDBInstanceAlreadyExistsFault,

    -- ** ReservedDBInstanceNotFoundFault
    _ReservedDBInstanceNotFoundFault,

    -- ** ReservedDBInstanceQuotaExceededFault
    _ReservedDBInstanceQuotaExceededFault,

    -- ** ReservedDBInstancesOfferingNotFoundFault
    _ReservedDBInstancesOfferingNotFoundFault,

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

    -- ** SourceClusterNotSupportedFault
    _SourceClusterNotSupportedFault,

    -- ** SourceDatabaseNotSupportedFault
    _SourceDatabaseNotSupportedFault,

    -- ** SourceNotFoundFault
    _SourceNotFoundFault,

    -- ** StorageQuotaExceededFault
    _StorageQuotaExceededFault,

    -- ** StorageTypeNotAvailableFault
    _StorageTypeNotAvailableFault,

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

    -- ** DBClusterAvailable
    newDBClusterAvailable,

    -- ** DBClusterDeleted
    newDBClusterDeleted,

    -- ** DBClusterSnapshotAvailable
    newDBClusterSnapshotAvailable,

    -- ** DBClusterSnapshotDeleted
    newDBClusterSnapshotDeleted,

    -- ** DBInstanceAvailable
    newDBInstanceAvailable,

    -- ** DBInstanceDeleted
    newDBInstanceDeleted,

    -- ** DBSnapshotAvailable
    newDBSnapshotAvailable,

    -- ** DBSnapshotCompleted
    newDBSnapshotCompleted,

    -- ** DBSnapshotDeleted
    newDBSnapshotDeleted,

    -- * Operations
    -- $operations

    -- ** AddRoleToDBCluster
    AddRoleToDBCluster (AddRoleToDBCluster'),
    newAddRoleToDBCluster,
    AddRoleToDBClusterResponse (AddRoleToDBClusterResponse'),
    newAddRoleToDBClusterResponse,

    -- ** AddRoleToDBInstance
    AddRoleToDBInstance (AddRoleToDBInstance'),
    newAddRoleToDBInstance,
    AddRoleToDBInstanceResponse (AddRoleToDBInstanceResponse'),
    newAddRoleToDBInstanceResponse,

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

    -- ** AuthorizeDBSecurityGroupIngress
    AuthorizeDBSecurityGroupIngress (AuthorizeDBSecurityGroupIngress'),
    newAuthorizeDBSecurityGroupIngress,
    AuthorizeDBSecurityGroupIngressResponse (AuthorizeDBSecurityGroupIngressResponse'),
    newAuthorizeDBSecurityGroupIngressResponse,

    -- ** BacktrackDBCluster
    BacktrackDBCluster (BacktrackDBCluster'),
    newBacktrackDBCluster,
    DBClusterBacktrack (DBClusterBacktrack'),
    newDBClusterBacktrack,

    -- ** CancelExportTask
    CancelExportTask (CancelExportTask'),
    newCancelExportTask,
    ExportTask (ExportTask'),
    newExportTask,

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

    -- ** CopyDBSnapshot
    CopyDBSnapshot (CopyDBSnapshot'),
    newCopyDBSnapshot,
    CopyDBSnapshotResponse (CopyDBSnapshotResponse'),
    newCopyDBSnapshotResponse,

    -- ** CopyOptionGroup
    CopyOptionGroup (CopyOptionGroup'),
    newCopyOptionGroup,
    CopyOptionGroupResponse (CopyOptionGroupResponse'),
    newCopyOptionGroupResponse,

    -- ** CreateBlueGreenDeployment
    CreateBlueGreenDeployment (CreateBlueGreenDeployment'),
    newCreateBlueGreenDeployment,
    CreateBlueGreenDeploymentResponse (CreateBlueGreenDeploymentResponse'),
    newCreateBlueGreenDeploymentResponse,

    -- ** CreateCustomDBEngineVersion
    CreateCustomDBEngineVersion (CreateCustomDBEngineVersion'),
    newCreateCustomDBEngineVersion,
    DBEngineVersion (DBEngineVersion'),
    newDBEngineVersion,

    -- ** CreateDBCluster
    CreateDBCluster (CreateDBCluster'),
    newCreateDBCluster,
    CreateDBClusterResponse (CreateDBClusterResponse'),
    newCreateDBClusterResponse,

    -- ** CreateDBClusterEndpoint
    CreateDBClusterEndpoint (CreateDBClusterEndpoint'),
    newCreateDBClusterEndpoint,
    DBClusterEndpoint (DBClusterEndpoint'),
    newDBClusterEndpoint,

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

    -- ** CreateDBInstanceReadReplica
    CreateDBInstanceReadReplica (CreateDBInstanceReadReplica'),
    newCreateDBInstanceReadReplica,
    CreateDBInstanceReadReplicaResponse (CreateDBInstanceReadReplicaResponse'),
    newCreateDBInstanceReadReplicaResponse,

    -- ** CreateDBParameterGroup
    CreateDBParameterGroup (CreateDBParameterGroup'),
    newCreateDBParameterGroup,
    CreateDBParameterGroupResponse (CreateDBParameterGroupResponse'),
    newCreateDBParameterGroupResponse,

    -- ** CreateDBProxy
    CreateDBProxy (CreateDBProxy'),
    newCreateDBProxy,
    CreateDBProxyResponse (CreateDBProxyResponse'),
    newCreateDBProxyResponse,

    -- ** CreateDBProxyEndpoint
    CreateDBProxyEndpoint (CreateDBProxyEndpoint'),
    newCreateDBProxyEndpoint,
    CreateDBProxyEndpointResponse (CreateDBProxyEndpointResponse'),
    newCreateDBProxyEndpointResponse,

    -- ** CreateDBSecurityGroup
    CreateDBSecurityGroup (CreateDBSecurityGroup'),
    newCreateDBSecurityGroup,
    CreateDBSecurityGroupResponse (CreateDBSecurityGroupResponse'),
    newCreateDBSecurityGroupResponse,

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

    -- ** CreateOptionGroup
    CreateOptionGroup (CreateOptionGroup'),
    newCreateOptionGroup,
    CreateOptionGroupResponse (CreateOptionGroupResponse'),
    newCreateOptionGroupResponse,

    -- ** DeleteBlueGreenDeployment
    DeleteBlueGreenDeployment (DeleteBlueGreenDeployment'),
    newDeleteBlueGreenDeployment,
    DeleteBlueGreenDeploymentResponse (DeleteBlueGreenDeploymentResponse'),
    newDeleteBlueGreenDeploymentResponse,

    -- ** DeleteCustomDBEngineVersion
    DeleteCustomDBEngineVersion (DeleteCustomDBEngineVersion'),
    newDeleteCustomDBEngineVersion,
    DBEngineVersion (DBEngineVersion'),
    newDBEngineVersion,

    -- ** DeleteDBCluster
    DeleteDBCluster (DeleteDBCluster'),
    newDeleteDBCluster,
    DeleteDBClusterResponse (DeleteDBClusterResponse'),
    newDeleteDBClusterResponse,

    -- ** DeleteDBClusterEndpoint
    DeleteDBClusterEndpoint (DeleteDBClusterEndpoint'),
    newDeleteDBClusterEndpoint,
    DBClusterEndpoint (DBClusterEndpoint'),
    newDBClusterEndpoint,

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

    -- ** DeleteDBInstanceAutomatedBackup
    DeleteDBInstanceAutomatedBackup (DeleteDBInstanceAutomatedBackup'),
    newDeleteDBInstanceAutomatedBackup,
    DeleteDBInstanceAutomatedBackupResponse (DeleteDBInstanceAutomatedBackupResponse'),
    newDeleteDBInstanceAutomatedBackupResponse,

    -- ** DeleteDBParameterGroup
    DeleteDBParameterGroup (DeleteDBParameterGroup'),
    newDeleteDBParameterGroup,
    DeleteDBParameterGroupResponse (DeleteDBParameterGroupResponse'),
    newDeleteDBParameterGroupResponse,

    -- ** DeleteDBProxy
    DeleteDBProxy (DeleteDBProxy'),
    newDeleteDBProxy,
    DeleteDBProxyResponse (DeleteDBProxyResponse'),
    newDeleteDBProxyResponse,

    -- ** DeleteDBProxyEndpoint
    DeleteDBProxyEndpoint (DeleteDBProxyEndpoint'),
    newDeleteDBProxyEndpoint,
    DeleteDBProxyEndpointResponse (DeleteDBProxyEndpointResponse'),
    newDeleteDBProxyEndpointResponse,

    -- ** DeleteDBSecurityGroup
    DeleteDBSecurityGroup (DeleteDBSecurityGroup'),
    newDeleteDBSecurityGroup,
    DeleteDBSecurityGroupResponse (DeleteDBSecurityGroupResponse'),
    newDeleteDBSecurityGroupResponse,

    -- ** DeleteDBSnapshot
    DeleteDBSnapshot (DeleteDBSnapshot'),
    newDeleteDBSnapshot,
    DeleteDBSnapshotResponse (DeleteDBSnapshotResponse'),
    newDeleteDBSnapshotResponse,

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

    -- ** DeleteOptionGroup
    DeleteOptionGroup (DeleteOptionGroup'),
    newDeleteOptionGroup,
    DeleteOptionGroupResponse (DeleteOptionGroupResponse'),
    newDeleteOptionGroupResponse,

    -- ** DeregisterDBProxyTargets
    DeregisterDBProxyTargets (DeregisterDBProxyTargets'),
    newDeregisterDBProxyTargets,
    DeregisterDBProxyTargetsResponse (DeregisterDBProxyTargetsResponse'),
    newDeregisterDBProxyTargetsResponse,

    -- ** DescribeAccountAttributes
    DescribeAccountAttributes (DescribeAccountAttributes'),
    newDescribeAccountAttributes,
    DescribeAccountAttributesResponse (DescribeAccountAttributesResponse'),
    newDescribeAccountAttributesResponse,

    -- ** DescribeBlueGreenDeployments (Paginated)
    DescribeBlueGreenDeployments (DescribeBlueGreenDeployments'),
    newDescribeBlueGreenDeployments,
    DescribeBlueGreenDeploymentsResponse (DescribeBlueGreenDeploymentsResponse'),
    newDescribeBlueGreenDeploymentsResponse,

    -- ** DescribeCertificates (Paginated)
    DescribeCertificates (DescribeCertificates'),
    newDescribeCertificates,
    DescribeCertificatesResponse (DescribeCertificatesResponse'),
    newDescribeCertificatesResponse,

    -- ** DescribeDBClusterBacktracks (Paginated)
    DescribeDBClusterBacktracks (DescribeDBClusterBacktracks'),
    newDescribeDBClusterBacktracks,
    DescribeDBClusterBacktracksResponse (DescribeDBClusterBacktracksResponse'),
    newDescribeDBClusterBacktracksResponse,

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

    -- ** DescribeDBInstanceAutomatedBackups (Paginated)
    DescribeDBInstanceAutomatedBackups (DescribeDBInstanceAutomatedBackups'),
    newDescribeDBInstanceAutomatedBackups,
    DescribeDBInstanceAutomatedBackupsResponse (DescribeDBInstanceAutomatedBackupsResponse'),
    newDescribeDBInstanceAutomatedBackupsResponse,

    -- ** DescribeDBInstances (Paginated)
    DescribeDBInstances (DescribeDBInstances'),
    newDescribeDBInstances,
    DescribeDBInstancesResponse (DescribeDBInstancesResponse'),
    newDescribeDBInstancesResponse,

    -- ** DescribeDBLogFiles (Paginated)
    DescribeDBLogFiles (DescribeDBLogFiles'),
    newDescribeDBLogFiles,
    DescribeDBLogFilesResponse (DescribeDBLogFilesResponse'),
    newDescribeDBLogFilesResponse,

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

    -- ** DescribeDBProxies (Paginated)
    DescribeDBProxies (DescribeDBProxies'),
    newDescribeDBProxies,
    DescribeDBProxiesResponse (DescribeDBProxiesResponse'),
    newDescribeDBProxiesResponse,

    -- ** DescribeDBProxyEndpoints (Paginated)
    DescribeDBProxyEndpoints (DescribeDBProxyEndpoints'),
    newDescribeDBProxyEndpoints,
    DescribeDBProxyEndpointsResponse (DescribeDBProxyEndpointsResponse'),
    newDescribeDBProxyEndpointsResponse,

    -- ** DescribeDBProxyTargetGroups (Paginated)
    DescribeDBProxyTargetGroups (DescribeDBProxyTargetGroups'),
    newDescribeDBProxyTargetGroups,
    DescribeDBProxyTargetGroupsResponse (DescribeDBProxyTargetGroupsResponse'),
    newDescribeDBProxyTargetGroupsResponse,

    -- ** DescribeDBProxyTargets (Paginated)
    DescribeDBProxyTargets (DescribeDBProxyTargets'),
    newDescribeDBProxyTargets,
    DescribeDBProxyTargetsResponse (DescribeDBProxyTargetsResponse'),
    newDescribeDBProxyTargetsResponse,

    -- ** DescribeDBSecurityGroups (Paginated)
    DescribeDBSecurityGroups (DescribeDBSecurityGroups'),
    newDescribeDBSecurityGroups,
    DescribeDBSecurityGroupsResponse (DescribeDBSecurityGroupsResponse'),
    newDescribeDBSecurityGroupsResponse,

    -- ** DescribeDBSnapshotAttributes
    DescribeDBSnapshotAttributes (DescribeDBSnapshotAttributes'),
    newDescribeDBSnapshotAttributes,
    DescribeDBSnapshotAttributesResponse (DescribeDBSnapshotAttributesResponse'),
    newDescribeDBSnapshotAttributesResponse,

    -- ** DescribeDBSnapshots (Paginated)
    DescribeDBSnapshots (DescribeDBSnapshots'),
    newDescribeDBSnapshots,
    DescribeDBSnapshotsResponse (DescribeDBSnapshotsResponse'),
    newDescribeDBSnapshotsResponse,

    -- ** DescribeDBSubnetGroups (Paginated)
    DescribeDBSubnetGroups (DescribeDBSubnetGroups'),
    newDescribeDBSubnetGroups,
    DescribeDBSubnetGroupsResponse (DescribeDBSubnetGroupsResponse'),
    newDescribeDBSubnetGroupsResponse,

    -- ** DescribeEngineDefaultClusterParameters (Paginated)
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

    -- ** DescribeExportTasks (Paginated)
    DescribeExportTasks (DescribeExportTasks'),
    newDescribeExportTasks,
    DescribeExportTasksResponse (DescribeExportTasksResponse'),
    newDescribeExportTasksResponse,

    -- ** DescribeGlobalClusters (Paginated)
    DescribeGlobalClusters (DescribeGlobalClusters'),
    newDescribeGlobalClusters,
    DescribeGlobalClustersResponse (DescribeGlobalClustersResponse'),
    newDescribeGlobalClustersResponse,

    -- ** DescribeOptionGroupOptions (Paginated)
    DescribeOptionGroupOptions (DescribeOptionGroupOptions'),
    newDescribeOptionGroupOptions,
    DescribeOptionGroupOptionsResponse (DescribeOptionGroupOptionsResponse'),
    newDescribeOptionGroupOptionsResponse,

    -- ** DescribeOptionGroups (Paginated)
    DescribeOptionGroups (DescribeOptionGroups'),
    newDescribeOptionGroups,
    DescribeOptionGroupsResponse (DescribeOptionGroupsResponse'),
    newDescribeOptionGroupsResponse,

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

    -- ** DescribeReservedDBInstances (Paginated)
    DescribeReservedDBInstances (DescribeReservedDBInstances'),
    newDescribeReservedDBInstances,
    DescribeReservedDBInstancesResponse (DescribeReservedDBInstancesResponse'),
    newDescribeReservedDBInstancesResponse,

    -- ** DescribeReservedDBInstancesOfferings (Paginated)
    DescribeReservedDBInstancesOfferings (DescribeReservedDBInstancesOfferings'),
    newDescribeReservedDBInstancesOfferings,
    DescribeReservedDBInstancesOfferingsResponse (DescribeReservedDBInstancesOfferingsResponse'),
    newDescribeReservedDBInstancesOfferingsResponse,

    -- ** DescribeSourceRegions (Paginated)
    DescribeSourceRegions (DescribeSourceRegions'),
    newDescribeSourceRegions,
    DescribeSourceRegionsResponse (DescribeSourceRegionsResponse'),
    newDescribeSourceRegionsResponse,

    -- ** DescribeValidDBInstanceModifications
    DescribeValidDBInstanceModifications (DescribeValidDBInstanceModifications'),
    newDescribeValidDBInstanceModifications,
    DescribeValidDBInstanceModificationsResponse (DescribeValidDBInstanceModificationsResponse'),
    newDescribeValidDBInstanceModificationsResponse,

    -- ** DownloadDBLogFilePortion (Paginated)
    DownloadDBLogFilePortion (DownloadDBLogFilePortion'),
    newDownloadDBLogFilePortion,
    DownloadDBLogFilePortionResponse (DownloadDBLogFilePortionResponse'),
    newDownloadDBLogFilePortionResponse,

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

    -- ** ModifyActivityStream
    ModifyActivityStream (ModifyActivityStream'),
    newModifyActivityStream,
    ModifyActivityStreamResponse (ModifyActivityStreamResponse'),
    newModifyActivityStreamResponse,

    -- ** ModifyCertificates
    ModifyCertificates (ModifyCertificates'),
    newModifyCertificates,
    ModifyCertificatesResponse (ModifyCertificatesResponse'),
    newModifyCertificatesResponse,

    -- ** ModifyCurrentDBClusterCapacity
    ModifyCurrentDBClusterCapacity (ModifyCurrentDBClusterCapacity'),
    newModifyCurrentDBClusterCapacity,
    ModifyCurrentDBClusterCapacityResponse (ModifyCurrentDBClusterCapacityResponse'),
    newModifyCurrentDBClusterCapacityResponse,

    -- ** ModifyCustomDBEngineVersion
    ModifyCustomDBEngineVersion (ModifyCustomDBEngineVersion'),
    newModifyCustomDBEngineVersion,
    DBEngineVersion (DBEngineVersion'),
    newDBEngineVersion,

    -- ** ModifyDBCluster
    ModifyDBCluster (ModifyDBCluster'),
    newModifyDBCluster,
    ModifyDBClusterResponse (ModifyDBClusterResponse'),
    newModifyDBClusterResponse,

    -- ** ModifyDBClusterEndpoint
    ModifyDBClusterEndpoint (ModifyDBClusterEndpoint'),
    newModifyDBClusterEndpoint,
    DBClusterEndpoint (DBClusterEndpoint'),
    newDBClusterEndpoint,

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

    -- ** ModifyDBProxy
    ModifyDBProxy (ModifyDBProxy'),
    newModifyDBProxy,
    ModifyDBProxyResponse (ModifyDBProxyResponse'),
    newModifyDBProxyResponse,

    -- ** ModifyDBProxyEndpoint
    ModifyDBProxyEndpoint (ModifyDBProxyEndpoint'),
    newModifyDBProxyEndpoint,
    ModifyDBProxyEndpointResponse (ModifyDBProxyEndpointResponse'),
    newModifyDBProxyEndpointResponse,

    -- ** ModifyDBProxyTargetGroup
    ModifyDBProxyTargetGroup (ModifyDBProxyTargetGroup'),
    newModifyDBProxyTargetGroup,
    ModifyDBProxyTargetGroupResponse (ModifyDBProxyTargetGroupResponse'),
    newModifyDBProxyTargetGroupResponse,

    -- ** ModifyDBSnapshot
    ModifyDBSnapshot (ModifyDBSnapshot'),
    newModifyDBSnapshot,
    ModifyDBSnapshotResponse (ModifyDBSnapshotResponse'),
    newModifyDBSnapshotResponse,

    -- ** ModifyDBSnapshotAttribute
    ModifyDBSnapshotAttribute (ModifyDBSnapshotAttribute'),
    newModifyDBSnapshotAttribute,
    ModifyDBSnapshotAttributeResponse (ModifyDBSnapshotAttributeResponse'),
    newModifyDBSnapshotAttributeResponse,

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

    -- ** ModifyOptionGroup
    ModifyOptionGroup (ModifyOptionGroup'),
    newModifyOptionGroup,
    ModifyOptionGroupResponse (ModifyOptionGroupResponse'),
    newModifyOptionGroupResponse,

    -- ** PromoteReadReplica
    PromoteReadReplica (PromoteReadReplica'),
    newPromoteReadReplica,
    PromoteReadReplicaResponse (PromoteReadReplicaResponse'),
    newPromoteReadReplicaResponse,

    -- ** PromoteReadReplicaDBCluster
    PromoteReadReplicaDBCluster (PromoteReadReplicaDBCluster'),
    newPromoteReadReplicaDBCluster,
    PromoteReadReplicaDBClusterResponse (PromoteReadReplicaDBClusterResponse'),
    newPromoteReadReplicaDBClusterResponse,

    -- ** PurchaseReservedDBInstancesOffering
    PurchaseReservedDBInstancesOffering (PurchaseReservedDBInstancesOffering'),
    newPurchaseReservedDBInstancesOffering,
    PurchaseReservedDBInstancesOfferingResponse (PurchaseReservedDBInstancesOfferingResponse'),
    newPurchaseReservedDBInstancesOfferingResponse,

    -- ** RebootDBCluster
    RebootDBCluster (RebootDBCluster'),
    newRebootDBCluster,
    RebootDBClusterResponse (RebootDBClusterResponse'),
    newRebootDBClusterResponse,

    -- ** RebootDBInstance
    RebootDBInstance (RebootDBInstance'),
    newRebootDBInstance,
    RebootDBInstanceResponse (RebootDBInstanceResponse'),
    newRebootDBInstanceResponse,

    -- ** RegisterDBProxyTargets
    RegisterDBProxyTargets (RegisterDBProxyTargets'),
    newRegisterDBProxyTargets,
    RegisterDBProxyTargetsResponse (RegisterDBProxyTargetsResponse'),
    newRegisterDBProxyTargetsResponse,

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

    -- ** RemoveRoleFromDBInstance
    RemoveRoleFromDBInstance (RemoveRoleFromDBInstance'),
    newRemoveRoleFromDBInstance,
    RemoveRoleFromDBInstanceResponse (RemoveRoleFromDBInstanceResponse'),
    newRemoveRoleFromDBInstanceResponse,

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

    -- ** RestoreDBClusterFromS3
    RestoreDBClusterFromS3 (RestoreDBClusterFromS3'),
    newRestoreDBClusterFromS3,
    RestoreDBClusterFromS3Response (RestoreDBClusterFromS3Response'),
    newRestoreDBClusterFromS3Response,

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

    -- ** RestoreDBInstanceFromDBSnapshot
    RestoreDBInstanceFromDBSnapshot (RestoreDBInstanceFromDBSnapshot'),
    newRestoreDBInstanceFromDBSnapshot,
    RestoreDBInstanceFromDBSnapshotResponse (RestoreDBInstanceFromDBSnapshotResponse'),
    newRestoreDBInstanceFromDBSnapshotResponse,

    -- ** RestoreDBInstanceFromS3
    RestoreDBInstanceFromS3 (RestoreDBInstanceFromS3'),
    newRestoreDBInstanceFromS3,
    RestoreDBInstanceFromS3Response (RestoreDBInstanceFromS3Response'),
    newRestoreDBInstanceFromS3Response,

    -- ** RestoreDBInstanceToPointInTime
    RestoreDBInstanceToPointInTime (RestoreDBInstanceToPointInTime'),
    newRestoreDBInstanceToPointInTime,
    RestoreDBInstanceToPointInTimeResponse (RestoreDBInstanceToPointInTimeResponse'),
    newRestoreDBInstanceToPointInTimeResponse,

    -- ** RevokeDBSecurityGroupIngress
    RevokeDBSecurityGroupIngress (RevokeDBSecurityGroupIngress'),
    newRevokeDBSecurityGroupIngress,
    RevokeDBSecurityGroupIngressResponse (RevokeDBSecurityGroupIngressResponse'),
    newRevokeDBSecurityGroupIngressResponse,

    -- ** StartActivityStream
    StartActivityStream (StartActivityStream'),
    newStartActivityStream,
    StartActivityStreamResponse (StartActivityStreamResponse'),
    newStartActivityStreamResponse,

    -- ** StartDBCluster
    StartDBCluster (StartDBCluster'),
    newStartDBCluster,
    StartDBClusterResponse (StartDBClusterResponse'),
    newStartDBClusterResponse,

    -- ** StartDBInstance
    StartDBInstance (StartDBInstance'),
    newStartDBInstance,
    StartDBInstanceResponse (StartDBInstanceResponse'),
    newStartDBInstanceResponse,

    -- ** StartDBInstanceAutomatedBackupsReplication
    StartDBInstanceAutomatedBackupsReplication (StartDBInstanceAutomatedBackupsReplication'),
    newStartDBInstanceAutomatedBackupsReplication,
    StartDBInstanceAutomatedBackupsReplicationResponse (StartDBInstanceAutomatedBackupsReplicationResponse'),
    newStartDBInstanceAutomatedBackupsReplicationResponse,

    -- ** StartExportTask
    StartExportTask (StartExportTask'),
    newStartExportTask,
    ExportTask (ExportTask'),
    newExportTask,

    -- ** StopActivityStream
    StopActivityStream (StopActivityStream'),
    newStopActivityStream,
    StopActivityStreamResponse (StopActivityStreamResponse'),
    newStopActivityStreamResponse,

    -- ** StopDBCluster
    StopDBCluster (StopDBCluster'),
    newStopDBCluster,
    StopDBClusterResponse (StopDBClusterResponse'),
    newStopDBClusterResponse,

    -- ** StopDBInstance
    StopDBInstance (StopDBInstance'),
    newStopDBInstance,
    StopDBInstanceResponse (StopDBInstanceResponse'),
    newStopDBInstanceResponse,

    -- ** StopDBInstanceAutomatedBackupsReplication
    StopDBInstanceAutomatedBackupsReplication (StopDBInstanceAutomatedBackupsReplication'),
    newStopDBInstanceAutomatedBackupsReplication,
    StopDBInstanceAutomatedBackupsReplicationResponse (StopDBInstanceAutomatedBackupsReplicationResponse'),
    newStopDBInstanceAutomatedBackupsReplicationResponse,

    -- ** SwitchoverBlueGreenDeployment
    SwitchoverBlueGreenDeployment (SwitchoverBlueGreenDeployment'),
    newSwitchoverBlueGreenDeployment,
    SwitchoverBlueGreenDeploymentResponse (SwitchoverBlueGreenDeploymentResponse'),
    newSwitchoverBlueGreenDeploymentResponse,

    -- ** SwitchoverReadReplica
    SwitchoverReadReplica (SwitchoverReadReplica'),
    newSwitchoverReadReplica,
    SwitchoverReadReplicaResponse (SwitchoverReadReplicaResponse'),
    newSwitchoverReadReplicaResponse,

    -- * Types

    -- ** ActivityStreamMode
    ActivityStreamMode (..),

    -- ** ActivityStreamPolicyStatus
    ActivityStreamPolicyStatus (..),

    -- ** ActivityStreamStatus
    ActivityStreamStatus (..),

    -- ** ApplyMethod
    ApplyMethod (..),

    -- ** AuditPolicyState
    AuditPolicyState (..),

    -- ** AuthScheme
    AuthScheme (..),

    -- ** AutomationMode
    AutomationMode (..),

    -- ** ClientPasswordAuthType
    ClientPasswordAuthType (..),

    -- ** CustomEngineVersionStatus
    CustomEngineVersionStatus (..),

    -- ** DBProxyEndpointStatus
    DBProxyEndpointStatus (..),

    -- ** DBProxyEndpointTargetRole
    DBProxyEndpointTargetRole (..),

    -- ** DBProxyStatus
    DBProxyStatus (..),

    -- ** EngineFamily
    EngineFamily (..),

    -- ** ExportSourceType
    ExportSourceType (..),

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

    -- ** BlueGreenDeployment
    BlueGreenDeployment (BlueGreenDeployment'),
    newBlueGreenDeployment,

    -- ** BlueGreenDeploymentTask
    BlueGreenDeploymentTask (BlueGreenDeploymentTask'),
    newBlueGreenDeploymentTask,

    -- ** Certificate
    Certificate (Certificate'),
    newCertificate,

    -- ** CertificateDetails
    CertificateDetails (CertificateDetails'),
    newCertificateDetails,

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

    -- ** CustomDBEngineVersionAMI
    CustomDBEngineVersionAMI (CustomDBEngineVersionAMI'),
    newCustomDBEngineVersionAMI,

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

    -- ** MasterUserSecret
    MasterUserSecret (MasterUserSecret'),
    newMasterUserSecret,

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

    -- ** ServerlessV2ScalingConfiguration
    ServerlessV2ScalingConfiguration (ServerlessV2ScalingConfiguration'),
    newServerlessV2ScalingConfiguration,

    -- ** ServerlessV2ScalingConfigurationInfo
    ServerlessV2ScalingConfigurationInfo (ServerlessV2ScalingConfigurationInfo'),
    newServerlessV2ScalingConfigurationInfo,

    -- ** SourceRegion
    SourceRegion (SourceRegion'),
    newSourceRegion,

    -- ** Subnet
    Subnet (Subnet'),
    newSubnet,

    -- ** SwitchoverDetail
    SwitchoverDetail (SwitchoverDetail'),
    newSwitchoverDetail,

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
  )
where

import Amazonka.RDS.AddRoleToDBCluster
import Amazonka.RDS.AddRoleToDBInstance
import Amazonka.RDS.AddSourceIdentifierToSubscription
import Amazonka.RDS.AddTagsToResource
import Amazonka.RDS.ApplyPendingMaintenanceAction
import Amazonka.RDS.AuthorizeDBSecurityGroupIngress
import Amazonka.RDS.BacktrackDBCluster
import Amazonka.RDS.CancelExportTask
import Amazonka.RDS.CopyDBClusterParameterGroup
import Amazonka.RDS.CopyDBClusterSnapshot
import Amazonka.RDS.CopyDBParameterGroup
import Amazonka.RDS.CopyDBSnapshot
import Amazonka.RDS.CopyOptionGroup
import Amazonka.RDS.CreateBlueGreenDeployment
import Amazonka.RDS.CreateCustomDBEngineVersion
import Amazonka.RDS.CreateDBCluster
import Amazonka.RDS.CreateDBClusterEndpoint
import Amazonka.RDS.CreateDBClusterParameterGroup
import Amazonka.RDS.CreateDBClusterSnapshot
import Amazonka.RDS.CreateDBInstance
import Amazonka.RDS.CreateDBInstanceReadReplica
import Amazonka.RDS.CreateDBParameterGroup
import Amazonka.RDS.CreateDBProxy
import Amazonka.RDS.CreateDBProxyEndpoint
import Amazonka.RDS.CreateDBSecurityGroup
import Amazonka.RDS.CreateDBSnapshot
import Amazonka.RDS.CreateDBSubnetGroup
import Amazonka.RDS.CreateEventSubscription
import Amazonka.RDS.CreateGlobalCluster
import Amazonka.RDS.CreateOptionGroup
import Amazonka.RDS.DeleteBlueGreenDeployment
import Amazonka.RDS.DeleteCustomDBEngineVersion
import Amazonka.RDS.DeleteDBCluster
import Amazonka.RDS.DeleteDBClusterEndpoint
import Amazonka.RDS.DeleteDBClusterParameterGroup
import Amazonka.RDS.DeleteDBClusterSnapshot
import Amazonka.RDS.DeleteDBInstance
import Amazonka.RDS.DeleteDBInstanceAutomatedBackup
import Amazonka.RDS.DeleteDBParameterGroup
import Amazonka.RDS.DeleteDBProxy
import Amazonka.RDS.DeleteDBProxyEndpoint
import Amazonka.RDS.DeleteDBSecurityGroup
import Amazonka.RDS.DeleteDBSnapshot
import Amazonka.RDS.DeleteDBSubnetGroup
import Amazonka.RDS.DeleteEventSubscription
import Amazonka.RDS.DeleteGlobalCluster
import Amazonka.RDS.DeleteOptionGroup
import Amazonka.RDS.DeregisterDBProxyTargets
import Amazonka.RDS.DescribeAccountAttributes
import Amazonka.RDS.DescribeBlueGreenDeployments
import Amazonka.RDS.DescribeCertificates
import Amazonka.RDS.DescribeDBClusterBacktracks
import Amazonka.RDS.DescribeDBClusterEndpoints
import Amazonka.RDS.DescribeDBClusterParameterGroups
import Amazonka.RDS.DescribeDBClusterParameters
import Amazonka.RDS.DescribeDBClusterSnapshotAttributes
import Amazonka.RDS.DescribeDBClusterSnapshots
import Amazonka.RDS.DescribeDBClusters
import Amazonka.RDS.DescribeDBEngineVersions
import Amazonka.RDS.DescribeDBInstanceAutomatedBackups
import Amazonka.RDS.DescribeDBInstances
import Amazonka.RDS.DescribeDBLogFiles
import Amazonka.RDS.DescribeDBParameterGroups
import Amazonka.RDS.DescribeDBParameters
import Amazonka.RDS.DescribeDBProxies
import Amazonka.RDS.DescribeDBProxyEndpoints
import Amazonka.RDS.DescribeDBProxyTargetGroups
import Amazonka.RDS.DescribeDBProxyTargets
import Amazonka.RDS.DescribeDBSecurityGroups
import Amazonka.RDS.DescribeDBSnapshotAttributes
import Amazonka.RDS.DescribeDBSnapshots
import Amazonka.RDS.DescribeDBSubnetGroups
import Amazonka.RDS.DescribeEngineDefaultClusterParameters
import Amazonka.RDS.DescribeEngineDefaultParameters
import Amazonka.RDS.DescribeEventCategories
import Amazonka.RDS.DescribeEventSubscriptions
import Amazonka.RDS.DescribeEvents
import Amazonka.RDS.DescribeExportTasks
import Amazonka.RDS.DescribeGlobalClusters
import Amazonka.RDS.DescribeOptionGroupOptions
import Amazonka.RDS.DescribeOptionGroups
import Amazonka.RDS.DescribeOrderableDBInstanceOptions
import Amazonka.RDS.DescribePendingMaintenanceActions
import Amazonka.RDS.DescribeReservedDBInstances
import Amazonka.RDS.DescribeReservedDBInstancesOfferings
import Amazonka.RDS.DescribeSourceRegions
import Amazonka.RDS.DescribeValidDBInstanceModifications
import Amazonka.RDS.DownloadDBLogFilePortion
import Amazonka.RDS.FailoverDBCluster
import Amazonka.RDS.FailoverGlobalCluster
import Amazonka.RDS.Lens
import Amazonka.RDS.ListTagsForResource
import Amazonka.RDS.ModifyActivityStream
import Amazonka.RDS.ModifyCertificates
import Amazonka.RDS.ModifyCurrentDBClusterCapacity
import Amazonka.RDS.ModifyCustomDBEngineVersion
import Amazonka.RDS.ModifyDBCluster
import Amazonka.RDS.ModifyDBClusterEndpoint
import Amazonka.RDS.ModifyDBClusterParameterGroup
import Amazonka.RDS.ModifyDBClusterSnapshotAttribute
import Amazonka.RDS.ModifyDBInstance
import Amazonka.RDS.ModifyDBParameterGroup
import Amazonka.RDS.ModifyDBProxy
import Amazonka.RDS.ModifyDBProxyEndpoint
import Amazonka.RDS.ModifyDBProxyTargetGroup
import Amazonka.RDS.ModifyDBSnapshot
import Amazonka.RDS.ModifyDBSnapshotAttribute
import Amazonka.RDS.ModifyDBSubnetGroup
import Amazonka.RDS.ModifyEventSubscription
import Amazonka.RDS.ModifyGlobalCluster
import Amazonka.RDS.ModifyOptionGroup
import Amazonka.RDS.PromoteReadReplica
import Amazonka.RDS.PromoteReadReplicaDBCluster
import Amazonka.RDS.PurchaseReservedDBInstancesOffering
import Amazonka.RDS.RebootDBCluster
import Amazonka.RDS.RebootDBInstance
import Amazonka.RDS.RegisterDBProxyTargets
import Amazonka.RDS.RemoveFromGlobalCluster
import Amazonka.RDS.RemoveRoleFromDBCluster
import Amazonka.RDS.RemoveRoleFromDBInstance
import Amazonka.RDS.RemoveSourceIdentifierFromSubscription
import Amazonka.RDS.RemoveTagsFromResource
import Amazonka.RDS.ResetDBClusterParameterGroup
import Amazonka.RDS.ResetDBParameterGroup
import Amazonka.RDS.RestoreDBClusterFromS3
import Amazonka.RDS.RestoreDBClusterFromSnapshot
import Amazonka.RDS.RestoreDBClusterToPointInTime
import Amazonka.RDS.RestoreDBInstanceFromDBSnapshot
import Amazonka.RDS.RestoreDBInstanceFromS3
import Amazonka.RDS.RestoreDBInstanceToPointInTime
import Amazonka.RDS.RevokeDBSecurityGroupIngress
import Amazonka.RDS.StartActivityStream
import Amazonka.RDS.StartDBCluster
import Amazonka.RDS.StartDBInstance
import Amazonka.RDS.StartDBInstanceAutomatedBackupsReplication
import Amazonka.RDS.StartExportTask
import Amazonka.RDS.StopActivityStream
import Amazonka.RDS.StopDBCluster
import Amazonka.RDS.StopDBInstance
import Amazonka.RDS.StopDBInstanceAutomatedBackupsReplication
import Amazonka.RDS.SwitchoverBlueGreenDeployment
import Amazonka.RDS.SwitchoverReadReplica
import Amazonka.RDS.Types
import Amazonka.RDS.Waiters

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
