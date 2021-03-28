{-# OPTIONS_GHC -fno-warn-unused-imports    #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- __Amazon Relational Database Service__ 
--
--
-- Amazon Relational Database Service (Amazon RDS) is a web service that makes it easier to set up, operate, and scale a relational database in the cloud. It provides cost-efficient, resizeable capacity for an industry-standard relational database and manages common database administration tasks, freeing up developers to focus on what makes their applications and businesses unique.
-- Amazon RDS gives you access to the capabilities of a MySQL, MariaDB, PostgreSQL, Microsoft SQL Server, Oracle, or Amazon Aurora database server. These capabilities mean that the code, applications, and tools you already use today with your existing databases work with Amazon RDS without modification. Amazon RDS automatically backs up your database and maintains the database software that powers your DB instance. Amazon RDS is flexible: you can scale your DB instance's compute resources and storage capacity to meet your application's demand. As with all Amazon Web Services, there are no up-front investments, and you pay only for the resources you use.
-- This interface reference for Amazon RDS contains documentation for a programming or command line interface you can use to manage Amazon RDS. Amazon RDS is asynchronous, which means that some interfaces might require techniques such as polling or callback functions to determine when a command has been applied. In this reference, the parameter descriptions indicate whether a command is applied immediately, on the next instance reboot, or during the maintenance window. The reference structure is as follows, and we list following some related topics from the user guide.
-- __Amazon RDS API Reference__ 
--
--     * For the alphabetical list of API actions, see <https://docs.aws.amazon.com/AmazonRDS/latest/APIReference/API_Operations.html API Actions> .
--
--
--     * For the alphabetical list of data types, see <https://docs.aws.amazon.com/AmazonRDS/latest/APIReference/API_Types.html Data Types> .
--
--
--     * For a list of common query parameters, see <https://docs.aws.amazon.com/AmazonRDS/latest/APIReference/CommonParameters.html Common Parameters> .
--
--
--     * For descriptions of the error codes, see <https://docs.aws.amazon.com/AmazonRDS/latest/APIReference/CommonErrors.html Common Errors> .
--
--
-- __Amazon RDS User Guide__ 
--
--     * For a summary of the Amazon RDS interfaces, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Welcome.html#Welcome.Interfaces Available RDS Interfaces> .
--
--
--     * For more information about how to use the Query API, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Using_the_Query_API.html Using the Query API> .
--
--
module Network.AWS.RDS
    (
    -- * Service configuration
      mkServiceConfig

    -- * Errors
    -- $errors

    -- ** PointInTimeRestoreNotEnabledFault
    , _PointInTimeRestoreNotEnabledFault

    -- ** InvalidDBParameterGroupStateFault
    , _InvalidDBParameterGroupStateFault

    -- ** ReservedDBInstanceQuotaExceededFault
    , _ReservedDBInstanceQuotaExceededFault

    -- ** SourceNotFoundFault
    , _SourceNotFoundFault

    -- ** CertificateNotFoundFault
    , _CertificateNotFoundFault

    -- ** AuthorizationQuotaExceededFault
    , _AuthorizationQuotaExceededFault

    -- ** DBClusterSnapshotAlreadyExistsFault
    , _DBClusterSnapshotAlreadyExistsFault

    -- ** DBParameterGroupAlreadyExistsFault
    , _DBParameterGroupAlreadyExistsFault

    -- ** DBInstanceRoleQuotaExceededFault
    , _DBInstanceRoleQuotaExceededFault

    -- ** DBInstanceRoleAlreadyExistsFault
    , _DBInstanceRoleAlreadyExistsFault

    -- ** DBParameterGroupQuotaExceededFault
    , _DBParameterGroupQuotaExceededFault

    -- ** BackupPolicyNotFoundFault
    , _BackupPolicyNotFoundFault

    -- ** InsufficientDBClusterCapacityFault
    , _InsufficientDBClusterCapacityFault

    -- ** ReservedDBInstanceAlreadyExistsFault
    , _ReservedDBInstanceAlreadyExistsFault

    -- ** ProvisionedIopsNotAvailableInAZFault
    , _ProvisionedIopsNotAvailableInAZFault

    -- ** DBProxyTargetAlreadyRegisteredFault
    , _DBProxyTargetAlreadyRegisteredFault

    -- ** AuthorizationAlreadyExistsFault
    , _AuthorizationAlreadyExistsFault

    -- ** SubscriptionCategoryNotFoundFault
    , _SubscriptionCategoryNotFoundFault

    -- ** DBProxyTargetNotFoundFault
    , _DBProxyTargetNotFoundFault

    -- ** SubscriptionNotFoundFault
    , _SubscriptionNotFoundFault

    -- ** InvalidSubnet
    , _InvalidSubnet

    -- ** SharedSnapshotQuotaExceededFault
    , _SharedSnapshotQuotaExceededFault

    -- ** DBSubnetQuotaExceededFault
    , _DBSubnetQuotaExceededFault

    -- ** GlobalClusterAlreadyExistsFault
    , _GlobalClusterAlreadyExistsFault

    -- ** OptionGroupNotFoundFault
    , _OptionGroupNotFoundFault

    -- ** DBClusterNotFoundFault
    , _DBClusterNotFoundFault

    -- ** DBLogFileNotFoundFault
    , _DBLogFileNotFoundFault

    -- ** DBProxyTargetGroupNotFoundFault
    , _DBProxyTargetGroupNotFoundFault

    -- ** InvalidS3BucketFault
    , _InvalidS3BucketFault

    -- ** DBProxyQuotaExceededFault
    , _DBProxyQuotaExceededFault

    -- ** IamRoleNotFoundFault
    , _IamRoleNotFoundFault

    -- ** DBClusterAlreadyExistsFault
    , _DBClusterAlreadyExistsFault

    -- ** StorageTypeNotSupportedFault
    , _StorageTypeNotSupportedFault

    -- ** DBSecurityGroupQuotaExceededFault
    , _DBSecurityGroupQuotaExceededFault

    -- ** OptionGroupAlreadyExistsFault
    , _OptionGroupAlreadyExistsFault

    -- ** ExportTaskNotFoundFault
    , _ExportTaskNotFoundFault

    -- ** InsufficientAvailableIPsInSubnetFault
    , _InsufficientAvailableIPsInSubnetFault

    -- ** DBProxyNotFoundFault
    , _DBProxyNotFoundFault

    -- ** OptionGroupQuotaExceededFault
    , _OptionGroupQuotaExceededFault

    -- ** DBSecurityGroupAlreadyExistsFault
    , _DBSecurityGroupAlreadyExistsFault

    -- ** SNSTopicArnNotFoundFault
    , _SNSTopicArnNotFoundFault

    -- ** InvalidDBClusterEndpointStateFault
    , _InvalidDBClusterEndpointStateFault

    -- ** InvalidEventSubscriptionStateFault
    , _InvalidEventSubscriptionStateFault

    -- ** InvalidDBInstanceAutomatedBackupStateFault
    , _InvalidDBInstanceAutomatedBackupStateFault

    -- ** KMSKeyNotAccessibleFault
    , _KMSKeyNotAccessibleFault

    -- ** DBSnapshotNotFoundFault
    , _DBSnapshotNotFoundFault

    -- ** DBClusterParameterGroupNotFoundFault
    , _DBClusterParameterGroupNotFoundFault

    -- ** DBClusterQuotaExceededFault
    , _DBClusterQuotaExceededFault

    -- ** InvalidExportOnlyFault
    , _InvalidExportOnlyFault

    -- ** SnapshotQuotaExceededFault
    , _SnapshotQuotaExceededFault

    -- ** InvalidDBClusterCapacityFault
    , _InvalidDBClusterCapacityFault

    -- ** DBSubnetGroupAlreadyExistsFault
    , _DBSubnetGroupAlreadyExistsFault

    -- ** SNSNoAuthorizationFault
    , _SNSNoAuthorizationFault

    -- ** DBSecurityGroupNotFoundFault
    , _DBSecurityGroupNotFoundFault

    -- ** DBSecurityGroupNotSupportedFault
    , _DBSecurityGroupNotSupportedFault

    -- ** InvalidDBProxyStateFault
    , _InvalidDBProxyStateFault

    -- ** InstanceQuotaExceededFault
    , _InstanceQuotaExceededFault

    -- ** DBClusterBacktrackNotFoundFault
    , _DBClusterBacktrackNotFoundFault

    -- ** DomainNotFoundFault
    , _DomainNotFoundFault

    -- ** DBParameterGroupNotFoundFault
    , _DBParameterGroupNotFoundFault

    -- ** InvalidDBSubnetGroupFault
    , _InvalidDBSubnetGroupFault

    -- ** ReservedDBInstancesOfferingNotFoundFault
    , _ReservedDBInstancesOfferingNotFoundFault

    -- ** InvalidDBSubnetStateFault
    , _InvalidDBSubnetStateFault

    -- ** DBClusterSnapshotNotFoundFault
    , _DBClusterSnapshotNotFoundFault

    -- ** SNSInvalidTopicFault
    , _SNSInvalidTopicFault

    -- ** InsufficientDBInstanceCapacityFault
    , _InsufficientDBInstanceCapacityFault

    -- ** InvalidDBClusterSnapshotStateFault
    , _InvalidDBClusterSnapshotStateFault

    -- ** InstallationMediaAlreadyExistsFault
    , _InstallationMediaAlreadyExistsFault

    -- ** SubscriptionAlreadyExistFault
    , _SubscriptionAlreadyExistFault

    -- ** DBClusterRoleAlreadyExistsFault
    , _DBClusterRoleAlreadyExistsFault

    -- ** IamRoleMissingPermissionsFault
    , _IamRoleMissingPermissionsFault

    -- ** DBClusterRoleQuotaExceededFault
    , _DBClusterRoleQuotaExceededFault

    -- ** InvalidVPCNetworkStateFault
    , _InvalidVPCNetworkStateFault

    -- ** DBInstanceRoleNotFoundFault
    , _DBInstanceRoleNotFoundFault

    -- ** AuthorizationNotFoundFault
    , _AuthorizationNotFoundFault

    -- ** ReservedDBInstanceNotFoundFault
    , _ReservedDBInstanceNotFoundFault

    -- ** DBSubnetGroupQuotaExceededFault
    , _DBSubnetGroupQuotaExceededFault

    -- ** CustomAvailabilityZoneNotFoundFault
    , _CustomAvailabilityZoneNotFoundFault

    -- ** InvalidGlobalClusterStateFault
    , _InvalidGlobalClusterStateFault

    -- ** DBSubnetGroupNotAllowedFault
    , _DBSubnetGroupNotAllowedFault

    -- ** InvalidExportTaskStateFault
    , _InvalidExportTaskStateFault

    -- ** InvalidExportSourceStateFault
    , _InvalidExportSourceStateFault

    -- ** ExportTaskAlreadyExistsFault
    , _ExportTaskAlreadyExistsFault

    -- ** EventSubscriptionQuotaExceededFault
    , _EventSubscriptionQuotaExceededFault

    -- ** InsufficientStorageClusterCapacityFault
    , _InsufficientStorageClusterCapacityFault

    -- ** DBClusterEndpointQuotaExceededFault
    , _DBClusterEndpointQuotaExceededFault

    -- ** InvalidOptionGroupStateFault
    , _InvalidOptionGroupStateFault

    -- ** DBInstanceAutomatedBackupQuotaExceededFault
    , _DBInstanceAutomatedBackupQuotaExceededFault

    -- ** CustomAvailabilityZoneAlreadyExistsFault
    , _CustomAvailabilityZoneAlreadyExistsFault

    -- ** InvalidDBClusterStateFault
    , _InvalidDBClusterStateFault

    -- ** GlobalClusterNotFoundFault
    , _GlobalClusterNotFoundFault

    -- ** DBInstanceAlreadyExistsFault
    , _DBInstanceAlreadyExistsFault

    -- ** InvalidRestoreFault
    , _InvalidRestoreFault

    -- ** InvalidDBSecurityGroupStateFault
    , _InvalidDBSecurityGroupStateFault

    -- ** ResourceNotFoundFault
    , _ResourceNotFoundFault

    -- ** DBSubnetGroupNotFoundFault
    , _DBSubnetGroupNotFoundFault

    -- ** DBUpgradeDependencyFailureFault
    , _DBUpgradeDependencyFailureFault

    -- ** CustomAvailabilityZoneQuotaExceededFault
    , _CustomAvailabilityZoneQuotaExceededFault

    -- ** InvalidDBInstanceStateFault
    , _InvalidDBInstanceStateFault

    -- ** DBClusterEndpointAlreadyExistsFault
    , _DBClusterEndpointAlreadyExistsFault

    -- ** DBSnapshotAlreadyExistsFault
    , _DBSnapshotAlreadyExistsFault

    -- ** DBInstanceNotFoundFault
    , _DBInstanceNotFoundFault

    -- ** StorageQuotaExceededFault
    , _StorageQuotaExceededFault

    -- ** DBProxyAlreadyExistsFault
    , _DBProxyAlreadyExistsFault

    -- ** DBInstanceAutomatedBackupNotFoundFault
    , _DBInstanceAutomatedBackupNotFoundFault

    -- ** InvalidDBSnapshotStateFault
    , _InvalidDBSnapshotStateFault

    -- ** InvalidDBSubnetGroupStateFault
    , _InvalidDBSubnetGroupStateFault

    -- ** GlobalClusterQuotaExceededFault
    , _GlobalClusterQuotaExceededFault

    -- ** DBClusterEndpointNotFoundFault
    , _DBClusterEndpointNotFoundFault

    -- ** InstallationMediaNotFoundFault
    , _InstallationMediaNotFoundFault

    -- ** DBSubnetGroupDoesNotCoverEnoughAZs
    , _DBSubnetGroupDoesNotCoverEnoughAZs

    -- ** SubnetAlreadyInUse
    , _SubnetAlreadyInUse

    -- ** DBClusterRoleNotFoundFault
    , _DBClusterRoleNotFoundFault

    -- * Waiters
    -- $waiters

    -- ** DBInstanceAvailable
    , mkDBInstanceAvailable

    -- ** DBSnapshotCompleted
    , mkDBSnapshotCompleted

    -- ** DBSnapshotDeleted
    , mkDBSnapshotDeleted

    -- ** DBInstanceDeleted
    , mkDBInstanceDeleted

    -- ** DBClusterSnapshotDeleted
    , mkDBClusterSnapshotDeleted

    -- ** DBSnapshotAvailable
    , mkDBSnapshotAvailable

    -- ** DBClusterSnapshotAvailable
    , mkDBClusterSnapshotAvailable

    -- * Operations
    -- $operations

    -- ** StartDBCluster 
    , module Network.AWS.RDS.StartDBCluster

    -- ** DescribeDBClusterParameterGroups (Paginated)
    , module Network.AWS.RDS.DescribeDBClusterParameterGroups

    -- ** PromoteReadReplica 
    , module Network.AWS.RDS.PromoteReadReplica

    -- ** DescribeDBEngineVersions (Paginated)
    , module Network.AWS.RDS.DescribeDBEngineVersions

    -- ** StopDBInstance 
    , module Network.AWS.RDS.StopDBInstance

    -- ** ModifyDBClusterEndpoint 
    , module Network.AWS.RDS.ModifyDBClusterEndpoint

    -- ** CopyDBSnapshot 
    , module Network.AWS.RDS.CopyDBSnapshot

    -- ** AddSourceIdentifierToSubscription 
    , module Network.AWS.RDS.AddSourceIdentifierToSubscription

    -- ** ModifyDBInstance 
    , module Network.AWS.RDS.ModifyDBInstance

    -- ** ModifyEventSubscription 
    , module Network.AWS.RDS.ModifyEventSubscription

    -- ** ResetDBClusterParameterGroup 
    , module Network.AWS.RDS.ResetDBClusterParameterGroup

    -- ** DescribeCustomAvailabilityZones (Paginated)
    , module Network.AWS.RDS.DescribeCustomAvailabilityZones

    -- ** RestoreDBClusterFromS3 
    , module Network.AWS.RDS.RestoreDBClusterFromS3

    -- ** DescribeEvents (Paginated)
    , module Network.AWS.RDS.DescribeEvents

    -- ** DescribeEngineDefaultParameters (Paginated)
    , module Network.AWS.RDS.DescribeEngineDefaultParameters

    -- ** DescribeOptionGroups (Paginated)
    , module Network.AWS.RDS.DescribeOptionGroups

    -- ** DescribeDBLogFiles (Paginated)
    , module Network.AWS.RDS.DescribeDBLogFiles

    -- ** DescribeDBClusters (Paginated)
    , module Network.AWS.RDS.DescribeDBClusters

    -- ** ModifyDBSubnetGroup 
    , module Network.AWS.RDS.ModifyDBSubnetGroup

    -- ** ListTagsForResource 
    , module Network.AWS.RDS.ListTagsForResource

    -- ** DeleteOptionGroup 
    , module Network.AWS.RDS.DeleteOptionGroup

    -- ** DeleteDBCluster 
    , module Network.AWS.RDS.DeleteDBCluster

    -- ** DescribeReservedDBInstances (Paginated)
    , module Network.AWS.RDS.DescribeReservedDBInstances

    -- ** CopyDBParameterGroup 
    , module Network.AWS.RDS.CopyDBParameterGroup

    -- ** RemoveSourceIdentifierFromSubscription 
    , module Network.AWS.RDS.RemoveSourceIdentifierFromSubscription

    -- ** DeleteCustomAvailabilityZone 
    , module Network.AWS.RDS.DeleteCustomAvailabilityZone

    -- ** DescribeDBProxyTargets (Paginated)
    , module Network.AWS.RDS.DescribeDBProxyTargets

    -- ** DescribeEngineDefaultClusterParameters (Paginated)
    , module Network.AWS.RDS.DescribeEngineDefaultClusterParameters

    -- ** DescribeDBSnapshotAttributes 
    , module Network.AWS.RDS.DescribeDBSnapshotAttributes

    -- ** CreateCustomAvailabilityZone 
    , module Network.AWS.RDS.CreateCustomAvailabilityZone

    -- ** BacktrackDBCluster 
    , module Network.AWS.RDS.BacktrackDBCluster

    -- ** DeleteGlobalCluster 
    , module Network.AWS.RDS.DeleteGlobalCluster

    -- ** PromoteReadReplicaDBCluster 
    , module Network.AWS.RDS.PromoteReadReplicaDBCluster

    -- ** RemoveTagsFromResource 
    , module Network.AWS.RDS.RemoveTagsFromResource

    -- ** RestoreDBInstanceFromDBSnapshot 
    , module Network.AWS.RDS.RestoreDBInstanceFromDBSnapshot

    -- ** DeleteDBProxy 
    , module Network.AWS.RDS.DeleteDBProxy

    -- ** CreateEventSubscription 
    , module Network.AWS.RDS.CreateEventSubscription

    -- ** PurchaseReservedDBInstancesOffering 
    , module Network.AWS.RDS.PurchaseReservedDBInstancesOffering

    -- ** CreateDBInstance 
    , module Network.AWS.RDS.CreateDBInstance

    -- ** DeleteDBClusterParameterGroup 
    , module Network.AWS.RDS.DeleteDBClusterParameterGroup

    -- ** DescribeCertificates (Paginated)
    , module Network.AWS.RDS.DescribeCertificates

    -- ** AuthorizeDBSecurityGroupIngress 
    , module Network.AWS.RDS.AuthorizeDBSecurityGroupIngress

    -- ** RemoveRoleFromDBInstance 
    , module Network.AWS.RDS.RemoveRoleFromDBInstance

    -- ** DescribeSourceRegions (Paginated)
    , module Network.AWS.RDS.DescribeSourceRegions

    -- ** CreateDBClusterEndpoint 
    , module Network.AWS.RDS.CreateDBClusterEndpoint

    -- ** RestoreDBClusterFromSnapshot 
    , module Network.AWS.RDS.RestoreDBClusterFromSnapshot

    -- ** DescribeOrderableDBInstanceOptions (Paginated)
    , module Network.AWS.RDS.DescribeOrderableDBInstanceOptions

    -- ** DeleteDBClusterEndpoint 
    , module Network.AWS.RDS.DeleteDBClusterEndpoint

    -- ** CreateDBProxy 
    , module Network.AWS.RDS.CreateDBProxy

    -- ** DeleteDBInstanceAutomatedBackup 
    , module Network.AWS.RDS.DeleteDBInstanceAutomatedBackup

    -- ** CreateDBClusterParameterGroup 
    , module Network.AWS.RDS.CreateDBClusterParameterGroup

    -- ** CreateDBSnapshot 
    , module Network.AWS.RDS.CreateDBSnapshot

    -- ** DeleteEventSubscription 
    , module Network.AWS.RDS.DeleteEventSubscription

    -- ** DescribeDBClusterBacktracks (Paginated)
    , module Network.AWS.RDS.DescribeDBClusterBacktracks

    -- ** DescribeDBParameterGroups (Paginated)
    , module Network.AWS.RDS.DescribeDBParameterGroups

    -- ** ModifyDBSnapshotAttribute 
    , module Network.AWS.RDS.ModifyDBSnapshotAttribute

    -- ** DescribeDBInstanceAutomatedBackups (Paginated)
    , module Network.AWS.RDS.DescribeDBInstanceAutomatedBackups

    -- ** RemoveFromGlobalCluster 
    , module Network.AWS.RDS.RemoveFromGlobalCluster

    -- ** AddRoleToDBInstance 
    , module Network.AWS.RDS.AddRoleToDBInstance

    -- ** DeleteDBClusterSnapshot 
    , module Network.AWS.RDS.DeleteDBClusterSnapshot

    -- ** DescribeValidDBInstanceModifications 
    , module Network.AWS.RDS.DescribeValidDBInstanceModifications

    -- ** DescribeDBClusterEndpoints (Paginated)
    , module Network.AWS.RDS.DescribeDBClusterEndpoints

    -- ** DescribeOptionGroupOptions (Paginated)
    , module Network.AWS.RDS.DescribeOptionGroupOptions

    -- ** DescribeEventSubscriptions (Paginated)
    , module Network.AWS.RDS.DescribeEventSubscriptions

    -- ** AddTagsToResource 
    , module Network.AWS.RDS.AddTagsToResource

    -- ** DescribeDBParameters (Paginated)
    , module Network.AWS.RDS.DescribeDBParameters

    -- ** StopActivityStream 
    , module Network.AWS.RDS.StopActivityStream

    -- ** CreateDBClusterSnapshot 
    , module Network.AWS.RDS.CreateDBClusterSnapshot

    -- ** DescribeDBSnapshots (Paginated)
    , module Network.AWS.RDS.DescribeDBSnapshots

    -- ** ModifyDBProxyTargetGroup 
    , module Network.AWS.RDS.ModifyDBProxyTargetGroup

    -- ** DescribeDBSubnetGroups (Paginated)
    , module Network.AWS.RDS.DescribeDBSubnetGroups

    -- ** ModifyOptionGroup 
    , module Network.AWS.RDS.ModifyOptionGroup

    -- ** StopDBCluster 
    , module Network.AWS.RDS.StopDBCluster

    -- ** CreateDBParameterGroup 
    , module Network.AWS.RDS.CreateDBParameterGroup

    -- ** ModifyDBClusterSnapshotAttribute 
    , module Network.AWS.RDS.ModifyDBClusterSnapshotAttribute

    -- ** ModifyDBCluster 
    , module Network.AWS.RDS.ModifyDBCluster

    -- ** CopyDBClusterParameterGroup 
    , module Network.AWS.RDS.CopyDBClusterParameterGroup

    -- ** DescribeEventCategories 
    , module Network.AWS.RDS.DescribeEventCategories

    -- ** DescribeGlobalClusters (Paginated)
    , module Network.AWS.RDS.DescribeGlobalClusters

    -- ** StartDBInstance 
    , module Network.AWS.RDS.StartDBInstance

    -- ** DescribeExportTasks (Paginated)
    , module Network.AWS.RDS.DescribeExportTasks

    -- ** CancelExportTask 
    , module Network.AWS.RDS.CancelExportTask

    -- ** ModifyDBClusterParameterGroup 
    , module Network.AWS.RDS.ModifyDBClusterParameterGroup

    -- ** RestoreDBInstanceToPointInTime 
    , module Network.AWS.RDS.RestoreDBInstanceToPointInTime

    -- ** DescribeDBClusterSnapshotAttributes 
    , module Network.AWS.RDS.DescribeDBClusterSnapshotAttributes

    -- ** ModifyDBSnapshot 
    , module Network.AWS.RDS.ModifyDBSnapshot

    -- ** DescribeDBProxyTargetGroups (Paginated)
    , module Network.AWS.RDS.DescribeDBProxyTargetGroups

    -- ** ModifyDBProxy 
    , module Network.AWS.RDS.ModifyDBProxy

    -- ** DescribePendingMaintenanceActions (Paginated)
    , module Network.AWS.RDS.DescribePendingMaintenanceActions

    -- ** AddRoleToDBCluster 
    , module Network.AWS.RDS.AddRoleToDBCluster

    -- ** CopyDBClusterSnapshot 
    , module Network.AWS.RDS.CopyDBClusterSnapshot

    -- ** ImportInstallationMedia 
    , module Network.AWS.RDS.ImportInstallationMedia

    -- ** CreateGlobalCluster 
    , module Network.AWS.RDS.CreateGlobalCluster

    -- ** ResetDBParameterGroup 
    , module Network.AWS.RDS.ResetDBParameterGroup

    -- ** DescribeInstallationMedia (Paginated)
    , module Network.AWS.RDS.DescribeInstallationMedia

    -- ** DeregisterDBProxyTargets 
    , module Network.AWS.RDS.DeregisterDBProxyTargets

    -- ** CreateDBCluster 
    , module Network.AWS.RDS.CreateDBCluster

    -- ** RemoveRoleFromDBCluster 
    , module Network.AWS.RDS.RemoveRoleFromDBCluster

    -- ** FailoverDBCluster 
    , module Network.AWS.RDS.FailoverDBCluster

    -- ** RevokeDBSecurityGroupIngress 
    , module Network.AWS.RDS.RevokeDBSecurityGroupIngress

    -- ** ModifyDBParameterGroup 
    , module Network.AWS.RDS.ModifyDBParameterGroup

    -- ** ApplyPendingMaintenanceAction 
    , module Network.AWS.RDS.ApplyPendingMaintenanceAction

    -- ** CreateOptionGroup 
    , module Network.AWS.RDS.CreateOptionGroup

    -- ** DescribeAccountAttributes 
    , module Network.AWS.RDS.DescribeAccountAttributes

    -- ** DeleteDBSnapshot 
    , module Network.AWS.RDS.DeleteDBSnapshot

    -- ** DescribeDBClusterParameters (Paginated)
    , module Network.AWS.RDS.DescribeDBClusterParameters

    -- ** DeleteDBSubnetGroup 
    , module Network.AWS.RDS.DeleteDBSubnetGroup

    -- ** CreateDBSecurityGroup 
    , module Network.AWS.RDS.CreateDBSecurityGroup

    -- ** ModifyCertificates 
    , module Network.AWS.RDS.ModifyCertificates

    -- ** DescribeDBClusterSnapshots (Paginated)
    , module Network.AWS.RDS.DescribeDBClusterSnapshots

    -- ** RebootDBInstance 
    , module Network.AWS.RDS.RebootDBInstance

    -- ** CreateDBSubnetGroup 
    , module Network.AWS.RDS.CreateDBSubnetGroup

    -- ** DescribeReservedDBInstancesOfferings (Paginated)
    , module Network.AWS.RDS.DescribeReservedDBInstancesOfferings

    -- ** DeleteDBSecurityGroup 
    , module Network.AWS.RDS.DeleteDBSecurityGroup

    -- ** DeleteDBInstance 
    , module Network.AWS.RDS.DeleteDBInstance

    -- ** StartActivityStream 
    , module Network.AWS.RDS.StartActivityStream

    -- ** CreateDBInstanceReadReplica 
    , module Network.AWS.RDS.CreateDBInstanceReadReplica

    -- ** DeleteDBParameterGroup 
    , module Network.AWS.RDS.DeleteDBParameterGroup

    -- ** ModifyCurrentDBClusterCapacity 
    , module Network.AWS.RDS.ModifyCurrentDBClusterCapacity

    -- ** ModifyGlobalCluster 
    , module Network.AWS.RDS.ModifyGlobalCluster

    -- ** RegisterDBProxyTargets 
    , module Network.AWS.RDS.RegisterDBProxyTargets

    -- ** DescribeDBSecurityGroups (Paginated)
    , module Network.AWS.RDS.DescribeDBSecurityGroups

    -- ** CopyOptionGroup 
    , module Network.AWS.RDS.CopyOptionGroup

    -- ** RestoreDBClusterToPointInTime 
    , module Network.AWS.RDS.RestoreDBClusterToPointInTime

    -- ** DeleteInstallationMedia 
    , module Network.AWS.RDS.DeleteInstallationMedia

    -- ** DescribeDBInstances (Paginated)
    , module Network.AWS.RDS.DescribeDBInstances

    -- ** RestoreDBInstanceFromS3 
    , module Network.AWS.RDS.RestoreDBInstanceFromS3

    -- ** DownloadDBLogFilePortion (Paginated)
    , module Network.AWS.RDS.DownloadDBLogFilePortion

    -- ** DescribeDBProxies (Paginated)
    , module Network.AWS.RDS.DescribeDBProxies

    -- ** StartExportTask 
    , module Network.AWS.RDS.StartExportTask

    -- * Types

    -- ** RestoreWindow
    , RestoreWindow (..)
    , mkRestoreWindow
    , rwEarliestTime
    , rwLatestTime

    -- ** DBProxyTargetGroup
    , DBProxyTargetGroup (..)
    , mkDBProxyTargetGroup
    , dbptgConnectionPoolConfig
    , dbptgCreatedDate
    , dbptgDBProxyName
    , dbptgIsDefault
    , dbptgStatus
    , dbptgTargetGroupArn
    , dbptgTargetGroupName
    , dbptgUpdatedDate

    -- ** PendingMaintenanceAction
    , PendingMaintenanceAction (..)
    , mkPendingMaintenanceAction
    , pmaAction
    , pmaAutoAppliedAfterDate
    , pmaCurrentApplyDate
    , pmaDescription
    , pmaForcedApplyDate
    , pmaOptInStatus

    -- ** DBCluster
    , DBCluster (..)
    , mkDBCluster
    , dbcActivityStreamKinesisStreamName
    , dbcActivityStreamKmsKeyId
    , dbcActivityStreamMode
    , dbcActivityStreamStatus
    , dbcAllocatedStorage
    , dbcAssociatedRoles
    , dbcAvailabilityZones
    , dbcBacktrackConsumedChangeRecords
    , dbcBacktrackWindow
    , dbcBackupRetentionPeriod
    , dbcCapacity
    , dbcCharacterSetName
    , dbcCloneGroupId
    , dbcClusterCreateTime
    , dbcCopyTagsToSnapshot
    , dbcCrossAccountClone
    , dbcCustomEndpoints
    , dbcDBClusterArn
    , dbcDBClusterIdentifier
    , dbcDBClusterMembers
    , dbcDBClusterOptionGroupMemberships
    , dbcDBClusterParameterGroup
    , dbcDBSubnetGroup
    , dbcDatabaseName
    , dbcDbClusterResourceId
    , dbcDeletionProtection
    , dbcDomainMemberships
    , dbcEarliestBacktrackTime
    , dbcEarliestRestorableTime
    , dbcEnabledCloudwatchLogsExports
    , dbcEndpoint
    , dbcEngine
    , dbcEngineMode
    , dbcEngineVersion
    , dbcGlobalWriteForwardingRequested
    , dbcGlobalWriteForwardingStatus
    , dbcHostedZoneId
    , dbcHttpEndpointEnabled
    , dbcIAMDatabaseAuthenticationEnabled
    , dbcKmsKeyId
    , dbcLatestRestorableTime
    , dbcMasterUsername
    , dbcMultiAZ
    , dbcPercentProgress
    , dbcPort
    , dbcPreferredBackupWindow
    , dbcPreferredMaintenanceWindow
    , dbcReadReplicaIdentifiers
    , dbcReaderEndpoint
    , dbcReplicationSourceIdentifier
    , dbcScalingConfigurationInfo
    , dbcStatus
    , dbcStorageEncrypted
    , dbcTagList
    , dbcVpcSecurityGroups

    -- ** DBClusterSnapshotAttribute
    , DBClusterSnapshotAttribute (..)
    , mkDBClusterSnapshotAttribute
    , dbcsaAttributeName
    , dbcsaAttributeValues

    -- ** OptionGroup
    , OptionGroup (..)
    , mkOptionGroup
    , ogAllowsVpcAndNonVpcInstanceMemberships
    , ogEngineName
    , ogMajorEngineVersion
    , ogOptionGroupArn
    , ogOptionGroupDescription
    , ogOptionGroupName
    , ogOptions
    , ogVpcId

    -- ** DBClusterOptionGroupStatus
    , DBClusterOptionGroupStatus (..)
    , mkDBClusterOptionGroupStatus
    , dbcogsDBClusterOptionGroupName
    , dbcogsStatus

    -- ** DBParameterGroupStatus
    , DBParameterGroupStatus (..)
    , mkDBParameterGroupStatus
    , dbpgsDBParameterGroupName
    , dbpgsParameterApplyStatus

    -- ** Event
    , Event (..)
    , mkEvent
    , eDate
    , eEventCategories
    , eMessage
    , eSourceArn
    , eSourceIdentifier
    , eSourceType

    -- ** DBSecurityGroup
    , DBSecurityGroup (..)
    , mkDBSecurityGroup
    , dbsgDBSecurityGroupArn
    , dbsgDBSecurityGroupDescription
    , dbsgDBSecurityGroupName
    , dbsgEC2SecurityGroups
    , dbsgIPRanges
    , dbsgOwnerId
    , dbsgVpcId

    -- ** ValidDBInstanceModificationsMessage
    , ValidDBInstanceModificationsMessage (..)
    , mkValidDBInstanceModificationsMessage
    , vdbimmStorage
    , vdbimmValidProcessorFeatures

    -- ** ValidStorageOptions
    , ValidStorageOptions (..)
    , mkValidStorageOptions
    , vsoIopsToStorageRatio
    , vsoProvisionedIops
    , vsoStorageSize
    , vsoStorageType
    , vsoSupportsStorageAutoscaling

    -- ** DomainMembership
    , DomainMembership (..)
    , mkDomainMembership
    , dmDomain
    , dmFQDN
    , dmIAMRoleName
    , dmStatus

    -- ** Tag
    , Tag (..)
    , mkTag
    , tKey
    , tValue

    -- ** VpnDetails
    , VpnDetails (..)
    , mkVpnDetails
    , vdVpnGatewayIp
    , vdVpnId
    , vdVpnName
    , vdVpnPSK
    , vdVpnState
    , vdVpnTunnelOriginatorIP

    -- ** DBEngineVersion
    , DBEngineVersion (..)
    , mkDBEngineVersion
    , dbevDBEngineDescription
    , dbevDBEngineVersionDescription
    , dbevDBParameterGroupFamily
    , dbevDefaultCharacterSet
    , dbevEngine
    , dbevEngineVersion
    , dbevExportableLogTypes
    , dbevStatus
    , dbevSupportedCharacterSets
    , dbevSupportedEngineModes
    , dbevSupportedFeatureNames
    , dbevSupportedNcharCharacterSets
    , dbevSupportedTimezones
    , dbevSupportsGlobalDatabases
    , dbevSupportsLogExportsToCloudwatchLogs
    , dbevSupportsParallelQuery
    , dbevSupportsReadReplica
    , dbevValidUpgradeTarget

    -- ** DBProxy
    , DBProxy (..)
    , mkDBProxy
    , dbpAuth
    , dbpCreatedDate
    , dbpDBProxyArn
    , dbpDBProxyName
    , dbpDebugLogging
    , dbpEndpoint
    , dbpEngineFamily
    , dbpIdleClientTimeout
    , dbpRequireTLS
    , dbpRoleArn
    , dbpStatus
    , dbpUpdatedDate
    , dbpVpcSecurityGroupIds
    , dbpVpcSubnetIds

    -- ** DBClusterParameterGroup
    , DBClusterParameterGroup (..)
    , mkDBClusterParameterGroup
    , dbcpgDBClusterParameterGroupArn
    , dbcpgDBClusterParameterGroupName
    , dbcpgDBParameterGroupFamily
    , dbcpgDescription

    -- ** DoubleRange
    , DoubleRange (..)
    , mkDoubleRange
    , drFrom
    , drTo

    -- ** DBSnapshot
    , DBSnapshot (..)
    , mkDBSnapshot
    , dbsAllocatedStorage
    , dbsAvailabilityZone
    , dbsDBInstanceIdentifier
    , dbsDBSnapshotArn
    , dbsDBSnapshotIdentifier
    , dbsDbiResourceId
    , dbsEncrypted
    , dbsEngine
    , dbsEngineVersion
    , dbsIAMDatabaseAuthenticationEnabled
    , dbsInstanceCreateTime
    , dbsIops
    , dbsKmsKeyId
    , dbsLicenseModel
    , dbsMasterUsername
    , dbsOptionGroupName
    , dbsPercentProgress
    , dbsPort
    , dbsProcessorFeatures
    , dbsSnapshotCreateTime
    , dbsSnapshotType
    , dbsSourceDBSnapshotIdentifier
    , dbsSourceRegion
    , dbsStatus
    , dbsStorageType
    , dbsTagList
    , dbsTdeCredentialArn
    , dbsTimezone
    , dbsVpcId

    -- ** DBSecurityGroupMembership
    , DBSecurityGroupMembership (..)
    , mkDBSecurityGroupMembership
    , dbsgmDBSecurityGroupName
    , dbsgmStatus

    -- ** TargetHealth
    , TargetHealth (..)
    , mkTargetHealth
    , thDescription
    , thReason
    , thState

    -- ** SourceRegion
    , SourceRegion (..)
    , mkSourceRegion
    , srEndpoint
    , srRegionName
    , srStatus

    -- ** EC2SecurityGroup
    , EC2SecurityGroup (..)
    , mkEC2SecurityGroup
    , ecsgEC2SecurityGroupId
    , ecsgEC2SecurityGroupName
    , ecsgEC2SecurityGroupOwnerId
    , ecsgStatus

    -- ** UserAuthConfigInfo
    , UserAuthConfigInfo (..)
    , mkUserAuthConfigInfo
    , uaciAuthScheme
    , uaciDescription
    , uaciIAMAuth
    , uaciSecretArn
    , uaciUserName

    -- ** SourceType
    , SourceType (..)

    -- ** ResourcePendingMaintenanceActions
    , ResourcePendingMaintenanceActions (..)
    , mkResourcePendingMaintenanceActions
    , rpmaPendingMaintenanceActionDetails
    , rpmaResourceIdentifier

    -- ** DBParameterGroup
    , DBParameterGroup (..)
    , mkDBParameterGroup
    , dbpgDBParameterGroupArn
    , dbpgDBParameterGroupFamily
    , dbpgDBParameterGroupName
    , dbpgDescription

    -- ** EngineFamily
    , EngineFamily (..)

    -- ** DBClusterBacktrack
    , DBClusterBacktrack (..)
    , mkDBClusterBacktrack
    , dbcbBacktrackIdentifier
    , dbcbBacktrackRequestCreationTime
    , dbcbBacktrackTo
    , dbcbBacktrackedFrom
    , dbcbDBClusterIdentifier
    , dbcbStatus

    -- ** ReservedDBInstancesOffering
    , ReservedDBInstancesOffering (..)
    , mkReservedDBInstancesOffering
    , rdbioCurrencyCode
    , rdbioDBInstanceClass
    , rdbioDuration
    , rdbioFixedPrice
    , rdbioMultiAZ
    , rdbioOfferingType
    , rdbioProductDescription
    , rdbioRecurringCharges
    , rdbioReservedDBInstancesOfferingId
    , rdbioUsagePrice

    -- ** DBClusterSnapshot
    , DBClusterSnapshot (..)
    , mkDBClusterSnapshot
    , dbcsAllocatedStorage
    , dbcsAvailabilityZones
    , dbcsClusterCreateTime
    , dbcsDBClusterIdentifier
    , dbcsDBClusterSnapshotArn
    , dbcsDBClusterSnapshotIdentifier
    , dbcsEngine
    , dbcsEngineVersion
    , dbcsIAMDatabaseAuthenticationEnabled
    , dbcsKmsKeyId
    , dbcsLicenseModel
    , dbcsMasterUsername
    , dbcsPercentProgress
    , dbcsPort
    , dbcsSnapshotCreateTime
    , dbcsSnapshotType
    , dbcsSourceDBClusterSnapshotArn
    , dbcsStatus
    , dbcsStorageEncrypted
    , dbcsTagList
    , dbcsVpcId

    -- ** ApplyMethod
    , ApplyMethod (..)

    -- ** ReplicaMode
    , ReplicaMode (..)

    -- ** UserAuthConfig
    , UserAuthConfig (..)
    , mkUserAuthConfig
    , uacAuthScheme
    , uacDescription
    , uacIAMAuth
    , uacSecretArn
    , uacUserName

    -- ** CharacterSet
    , CharacterSet (..)
    , mkCharacterSet
    , csCharacterSetDescription
    , csCharacterSetName

    -- ** Subnet
    , Subnet (..)
    , mkSubnet
    , sSubnetAvailabilityZone
    , sSubnetIdentifier
    , sSubnetOutpost
    , sSubnetStatus

    -- ** ReservedDBInstance
    , ReservedDBInstance (..)
    , mkReservedDBInstance
    , rdbiCurrencyCode
    , rdbiDBInstanceClass
    , rdbiDBInstanceCount
    , rdbiDuration
    , rdbiFixedPrice
    , rdbiLeaseId
    , rdbiMultiAZ
    , rdbiOfferingType
    , rdbiProductDescription
    , rdbiRecurringCharges
    , rdbiReservedDBInstanceArn
    , rdbiReservedDBInstanceId
    , rdbiReservedDBInstancesOfferingId
    , rdbiStartTime
    , rdbiState
    , rdbiUsagePrice

    -- ** CloudwatchLogsExportConfiguration
    , CloudwatchLogsExportConfiguration (..)
    , mkCloudwatchLogsExportConfiguration
    , clecDisableLogTypes
    , clecEnableLogTypes

    -- ** AvailableProcessorFeature
    , AvailableProcessorFeature (..)
    , mkAvailableProcessorFeature
    , apfAllowedValues
    , apfDefaultValue
    , apfName

    -- ** DBInstanceRole
    , DBInstanceRole (..)
    , mkDBInstanceRole
    , dbirFeatureName
    , dbirRoleArn
    , dbirStatus

    -- ** DBClusterSnapshotAttributesResult
    , DBClusterSnapshotAttributesResult (..)
    , mkDBClusterSnapshotAttributesResult
    , dbcsarDBClusterSnapshotAttributes
    , dbcsarDBClusterSnapshotIdentifier

    -- ** MinimumEngineVersionPerAllowedValue
    , MinimumEngineVersionPerAllowedValue (..)
    , mkMinimumEngineVersionPerAllowedValue
    , mevpavAllowedValue
    , mevpavMinimumEngineVersion

    -- ** EngineDefaults
    , EngineDefaults (..)
    , mkEngineDefaults
    , edDBParameterGroupFamily
    , edMarker
    , edParameters

    -- ** DBClusterParameterGroupNameMessage
    , DBClusterParameterGroupNameMessage (..)
    , mkDBClusterParameterGroupNameMessage
    , dbcpgnmDBClusterParameterGroupName

    -- ** InstallationMediaFailureCause
    , InstallationMediaFailureCause (..)
    , mkInstallationMediaFailureCause
    , imfcMessage

    -- ** GlobalCluster
    , GlobalCluster (..)
    , mkGlobalCluster
    , gcDatabaseName
    , gcDeletionProtection
    , gcEngine
    , gcEngineVersion
    , gcGlobalClusterArn
    , gcGlobalClusterIdentifier
    , gcGlobalClusterMembers
    , gcGlobalClusterResourceId
    , gcStatus
    , gcStorageEncrypted

    -- ** TargetType
    , TargetType (..)

    -- ** DBParameterGroupNameMessage
    , DBParameterGroupNameMessage (..)
    , mkDBParameterGroupNameMessage
    , dbpgnmDBParameterGroupName

    -- ** CustomAvailabilityZone
    , CustomAvailabilityZone (..)
    , mkCustomAvailabilityZone
    , cazCustomAvailabilityZoneId
    , cazCustomAvailabilityZoneName
    , cazCustomAvailabilityZoneStatus
    , cazVpnDetails

    -- ** DBSnapshotAttributesResult
    , DBSnapshotAttributesResult (..)
    , mkDBSnapshotAttributesResult
    , dbsarDBSnapshotAttributes
    , dbsarDBSnapshotIdentifier

    -- ** ConnectionPoolConfigurationInfo
    , ConnectionPoolConfigurationInfo (..)
    , mkConnectionPoolConfigurationInfo
    , cpciConnectionBorrowTimeout
    , cpciInitQuery
    , cpciMaxConnectionsPercent
    , cpciMaxIdleConnectionsPercent
    , cpciSessionPinningFilters

    -- ** DBClusterMember
    , DBClusterMember (..)
    , mkDBClusterMember
    , dbcmDBClusterParameterGroupStatus
    , dbcmDBInstanceIdentifier
    , dbcmIsClusterWriter
    , dbcmPromotionTier

    -- ** OptionGroupOption
    , OptionGroupOption (..)
    , mkOptionGroupOption
    , ogoDefaultPort
    , ogoDescription
    , ogoEngineName
    , ogoMajorEngineVersion
    , ogoMinimumRequiredMinorEngineVersion
    , ogoName
    , ogoOptionGroupOptionSettings
    , ogoOptionGroupOptionVersions
    , ogoOptionsConflictsWith
    , ogoOptionsDependedOn
    , ogoPermanent
    , ogoPersistent
    , ogoPortRequired
    , ogoRequiresAutoMinorEngineVersionUpgrade
    , ogoSupportsOptionVersionDowngrade
    , ogoVpcOnly

    -- ** Range
    , Range (..)
    , mkRange
    , rFrom
    , rStep
    , rTo

    -- ** DBInstance
    , DBInstance (..)
    , mkDBInstance
    , dbiAllocatedStorage
    , dbiAssociatedRoles
    , dbiAutoMinorVersionUpgrade
    , dbiAvailabilityZone
    , dbiBackupRetentionPeriod
    , dbiCACertificateIdentifier
    , dbiCharacterSetName
    , dbiCopyTagsToSnapshot
    , dbiDBClusterIdentifier
    , dbiDBInstanceArn
    , dbiDBInstanceClass
    , dbiDBInstanceIdentifier
    , dbiDBInstanceStatus
    , dbiDBName
    , dbiDBParameterGroups
    , dbiDBSecurityGroups
    , dbiDBSubnetGroup
    , dbiDbInstancePort
    , dbiDbiResourceId
    , dbiDeletionProtection
    , dbiDomainMemberships
    , dbiEnabledCloudwatchLogsExports
    , dbiEndpoint
    , dbiEngine
    , dbiEngineVersion
    , dbiEnhancedMonitoringResourceArn
    , dbiIAMDatabaseAuthenticationEnabled
    , dbiInstanceCreateTime
    , dbiIops
    , dbiKmsKeyId
    , dbiLatestRestorableTime
    , dbiLicenseModel
    , dbiListenerEndpoint
    , dbiMasterUsername
    , dbiMaxAllocatedStorage
    , dbiMonitoringInterval
    , dbiMonitoringRoleArn
    , dbiMultiAZ
    , dbiNcharCharacterSetName
    , dbiOptionGroupMemberships
    , dbiPendingModifiedValues
    , dbiPerformanceInsightsEnabled
    , dbiPerformanceInsightsKMSKeyId
    , dbiPerformanceInsightsRetentionPeriod
    , dbiPreferredBackupWindow
    , dbiPreferredMaintenanceWindow
    , dbiProcessorFeatures
    , dbiPromotionTier
    , dbiPubliclyAccessible
    , dbiReadReplicaDBClusterIdentifiers
    , dbiReadReplicaDBInstanceIdentifiers
    , dbiReadReplicaSourceDBInstanceIdentifier
    , dbiReplicaMode
    , dbiSecondaryAvailabilityZone
    , dbiStatusInfos
    , dbiStorageEncrypted
    , dbiStorageType
    , dbiTagList
    , dbiTdeCredentialArn
    , dbiTimezone
    , dbiVpcSecurityGroups

    -- ** PendingCloudwatchLogsExports
    , PendingCloudwatchLogsExports (..)
    , mkPendingCloudwatchLogsExports
    , pcleLogTypesToDisable
    , pcleLogTypesToEnable

    -- ** AccountQuota
    , AccountQuota (..)
    , mkAccountQuota
    , aqAccountQuotaName
    , aqMax
    , aqUsed

    -- ** AvailabilityZone
    , AvailabilityZone (..)
    , mkAvailabilityZone
    , azName

    -- ** DBClusterEndpoint
    , DBClusterEndpoint (..)
    , mkDBClusterEndpoint
    , dbceCustomEndpointType
    , dbceDBClusterEndpointArn
    , dbceDBClusterEndpointIdentifier
    , dbceDBClusterEndpointResourceIdentifier
    , dbceDBClusterIdentifier
    , dbceEndpoint
    , dbceEndpointType
    , dbceExcludedMembers
    , dbceStaticMembers
    , dbceStatus

    -- ** EventSubscription
    , EventSubscription (..)
    , mkEventSubscription
    , esCustSubscriptionId
    , esCustomerAwsId
    , esEnabled
    , esEventCategoriesList
    , esEventSubscriptionArn
    , esSnsTopicArn
    , esSourceIdsList
    , esSourceType
    , esStatus
    , esSubscriptionCreationTime

    -- ** DBInstanceAutomatedBackup
    , DBInstanceAutomatedBackup (..)
    , mkDBInstanceAutomatedBackup
    , dbiabAllocatedStorage
    , dbiabAvailabilityZone
    , dbiabDBInstanceArn
    , dbiabDBInstanceIdentifier
    , dbiabDbiResourceId
    , dbiabEncrypted
    , dbiabEngine
    , dbiabEngineVersion
    , dbiabIAMDatabaseAuthenticationEnabled
    , dbiabInstanceCreateTime
    , dbiabIops
    , dbiabKmsKeyId
    , dbiabLicenseModel
    , dbiabMasterUsername
    , dbiabOptionGroupName
    , dbiabPort
    , dbiabRegion
    , dbiabRestoreWindow
    , dbiabStatus
    , dbiabStorageType
    , dbiabTdeCredentialArn
    , dbiabTimezone
    , dbiabVpcId

    -- ** ConnectionPoolConfiguration
    , ConnectionPoolConfiguration (..)
    , mkConnectionPoolConfiguration
    , cpcConnectionBorrowTimeout
    , cpcInitQuery
    , cpcMaxConnectionsPercent
    , cpcMaxIdleConnectionsPercent
    , cpcSessionPinningFilters

    -- ** ProcessorFeature
    , ProcessorFeature (..)
    , mkProcessorFeature
    , pfName
    , pfValue

    -- ** DBSubnetGroup
    , DBSubnetGroup (..)
    , mkDBSubnetGroup
    , dDBSubnetGroupArn
    , dDBSubnetGroupDescription
    , dDBSubnetGroupName
    , dSubnetGroupStatus
    , dSubnets
    , dVpcId

    -- ** ActivityStreamMode
    , ActivityStreamMode (..)

    -- ** WriteForwardingStatus
    , WriteForwardingStatus (..)

    -- ** Certificate
    , Certificate (..)
    , mkCertificate
    , cCertificateArn
    , cCertificateIdentifier
    , cCertificateType
    , cCustomerOverride
    , cCustomerOverrideValidTill
    , cThumbprint
    , cValidFrom
    , cValidTill

    -- ** AuthScheme
    , AuthScheme (..)

    -- ** DBInstanceStatusInfo
    , DBInstanceStatusInfo (..)
    , mkDBInstanceStatusInfo
    , dbisiMessage
    , dbisiNormal
    , dbisiStatus
    , dbisiStatusType

    -- ** OptionSetting
    , OptionSetting (..)
    , mkOptionSetting
    , osAllowedValues
    , osApplyType
    , osDataType
    , osDefaultValue
    , osDescription
    , osIsCollection
    , osIsModifiable
    , osName
    , osValue

    -- ** ActivityStreamStatus
    , ActivityStreamStatus (..)

    -- ** ScalingConfiguration
    , ScalingConfiguration (..)
    , mkScalingConfiguration
    , scAutoPause
    , scMaxCapacity
    , scMinCapacity
    , scSecondsUntilAutoPause
    , scTimeoutAction

    -- ** DescribeDBLogFilesDetails
    , DescribeDBLogFilesDetails (..)
    , mkDescribeDBLogFilesDetails
    , ddblfdLastWritten
    , ddblfdLogFileName
    , ddblfdSize

    -- ** OrderableDBInstanceOption
    , OrderableDBInstanceOption (..)
    , mkOrderableDBInstanceOption
    , odbioAvailabilityZoneGroup
    , odbioAvailabilityZones
    , odbioAvailableProcessorFeatures
    , odbioDBInstanceClass
    , odbioEngine
    , odbioEngineVersion
    , odbioLicenseModel
    , odbioMaxIopsPerDbInstance
    , odbioMaxIopsPerGib
    , odbioMaxStorageSize
    , odbioMinIopsPerDbInstance
    , odbioMinIopsPerGib
    , odbioMinStorageSize
    , odbioMultiAZCapable
    , odbioOutpostCapable
    , odbioReadReplicaCapable
    , odbioStorageType
    , odbioSupportedEngineModes
    , odbioSupportsEnhancedMonitoring
    , odbioSupportsGlobalDatabases
    , odbioSupportsIAMDatabaseAuthentication
    , odbioSupportsIops
    , odbioSupportsKerberosAuthentication
    , odbioSupportsPerformanceInsights
    , odbioSupportsStorageAutoscaling
    , odbioSupportsStorageEncryption
    , odbioVpc

    -- ** DBClusterRole
    , DBClusterRole (..)
    , mkDBClusterRole
    , dbcrFeatureName
    , dbcrRoleArn
    , dbcrStatus

    -- ** InstallationMedia
    , InstallationMedia (..)
    , mkInstallationMedia
    , imCustomAvailabilityZoneId
    , imEngine
    , imEngineInstallationMediaPath
    , imEngineVersion
    , imFailureCause
    , imInstallationMediaId
    , imOSInstallationMediaPath
    , imStatus

    -- ** Filter
    , Filter (..)
    , mkFilter
    , fName
    , fValues

    -- ** RecurringCharge
    , RecurringCharge (..)
    , mkRecurringCharge
    , rcRecurringChargeAmount
    , rcRecurringChargeFrequency

    -- ** Timezone
    , Timezone (..)
    , mkTimezone
    , tTimezoneName

    -- ** DBProxyTarget
    , DBProxyTarget (..)
    , mkDBProxyTarget
    , dbptEndpoint
    , dbptPort
    , dbptRdsResourceId
    , dbptTargetArn
    , dbptTargetHealth
    , dbptTrackedClusterId
    , dbptType

    -- ** Endpoint
    , Endpoint (..)
    , mkEndpoint
    , eAddress
    , eHostedZoneId
    , ePort

    -- ** ScalingConfigurationInfo
    , ScalingConfigurationInfo (..)
    , mkScalingConfigurationInfo
    , sciAutoPause
    , sciMaxCapacity
    , sciMinCapacity
    , sciSecondsUntilAutoPause
    , sciTimeoutAction

    -- ** OptionConfiguration
    , OptionConfiguration (..)
    , mkOptionConfiguration
    , ocOptionName
    , ocDBSecurityGroupMemberships
    , ocOptionSettings
    , ocOptionVersion
    , ocPort
    , ocVpcSecurityGroupMemberships

    -- ** Option
    , Option (..)
    , mkOption
    , oDBSecurityGroupMemberships
    , oOptionDescription
    , oOptionName
    , oOptionSettings
    , oOptionVersion
    , oPermanent
    , oPersistent
    , oPort
    , oVpcSecurityGroupMemberships

    -- ** IPRange
    , IPRange (..)
    , mkIPRange
    , iprCIDRIP
    , iprStatus

    -- ** IAMAuthMode
    , IAMAuthMode (..)

    -- ** OptionGroupMembership
    , OptionGroupMembership (..)
    , mkOptionGroupMembership
    , ogmOptionGroupName
    , ogmStatus

    -- ** EventCategoriesMap
    , EventCategoriesMap (..)
    , mkEventCategoriesMap
    , ecmEventCategories
    , ecmSourceType

    -- ** DBSnapshotAttribute
    , DBSnapshotAttribute (..)
    , mkDBSnapshotAttribute
    , dbsaAttributeName
    , dbsaAttributeValues

    -- ** DBProxyStatus
    , DBProxyStatus (..)

    -- ** TargetState
    , TargetState (..)

    -- ** PendingModifiedValues
    , PendingModifiedValues (..)
    , mkPendingModifiedValues
    , pmvAllocatedStorage
    , pmvBackupRetentionPeriod
    , pmvCACertificateIdentifier
    , pmvDBInstanceClass
    , pmvDBInstanceIdentifier
    , pmvDBSubnetGroupName
    , pmvEngineVersion
    , pmvIops
    , pmvLicenseModel
    , pmvMasterUserPassword
    , pmvMultiAZ
    , pmvPendingCloudwatchLogsExports
    , pmvPort
    , pmvProcessorFeatures
    , pmvStorageType

    -- ** VpcSecurityGroupMembership
    , VpcSecurityGroupMembership (..)
    , mkVpcSecurityGroupMembership
    , vsgmStatus
    , vsgmVpcSecurityGroupId

    -- ** Outpost
    , Outpost (..)
    , mkOutpost
    , oArn

    -- ** UpgradeTarget
    , UpgradeTarget (..)
    , mkUpgradeTarget
    , utAutoUpgrade
    , utDescription
    , utEngine
    , utEngineVersion
    , utIsMajorVersionUpgrade

    -- ** Parameter
    , Parameter (..)
    , mkParameter
    , pAllowedValues
    , pApplyMethod
    , pApplyType
    , pDataType
    , pDescription
    , pIsModifiable
    , pMinimumEngineVersion
    , pParameterName
    , pParameterValue
    , pSource
    , pSupportedEngineModes

    -- ** OptionVersion
    , OptionVersion (..)
    , mkOptionVersion
    , ovIsDefault
    , ovVersion

    -- ** OptionGroupOptionSetting
    , OptionGroupOptionSetting (..)
    , mkOptionGroupOptionSetting
    , ogosAllowedValues
    , ogosApplyType
    , ogosDefaultValue
    , ogosIsModifiable
    , ogosIsRequired
    , ogosMinimumEngineVersionPerAllowedValue
    , ogosSettingDescription
    , ogosSettingName

    -- ** ExportTask
    , ExportTask (..)
    , mkExportTask
    , etExportOnly
    , etExportTaskIdentifier
    , etFailureCause
    , etIamRoleArn
    , etKmsKeyId
    , etPercentProgress
    , etS3Bucket
    , etS3Prefix
    , etSnapshotTime
    , etSourceArn
    , etStatus
    , etTaskEndTime
    , etTaskStartTime
    , etTotalExtractedDataInGB
    , etWarningMessage

    -- ** TargetHealthReason
    , TargetHealthReason (..)

    -- ** GlobalClusterMember
    , GlobalClusterMember (..)
    , mkGlobalClusterMember
    , gcmDBClusterArn
    , gcmGlobalWriteForwardingStatus
    , gcmIsWriter
    , gcmReaders

    -- ** VpnPSK
    , VpnPSK (..)

    -- * Serialization types
    , Lude.Base64 (..)
    , Lude._Base64
    , Lude.Sensitive (..)
    , Lude._Sensitive
    , Lude.UTCTime
    , Lude.NominalDiffTime
    ) where

import Network.AWS.RDS.Types
import Network.AWS.RDS.Waiters
import Network.AWS.RDS.StartDBCluster
import Network.AWS.RDS.DescribeDBClusterParameterGroups
import Network.AWS.RDS.PromoteReadReplica
import Network.AWS.RDS.DescribeDBEngineVersions
import Network.AWS.RDS.StopDBInstance
import Network.AWS.RDS.ModifyDBClusterEndpoint
import Network.AWS.RDS.CopyDBSnapshot
import Network.AWS.RDS.AddSourceIdentifierToSubscription
import Network.AWS.RDS.ModifyDBInstance
import Network.AWS.RDS.ModifyEventSubscription
import Network.AWS.RDS.ResetDBClusterParameterGroup
import Network.AWS.RDS.DescribeCustomAvailabilityZones
import Network.AWS.RDS.RestoreDBClusterFromS3
import Network.AWS.RDS.DescribeEvents
import Network.AWS.RDS.DescribeEngineDefaultParameters
import Network.AWS.RDS.DescribeOptionGroups
import Network.AWS.RDS.DescribeDBLogFiles
import Network.AWS.RDS.DescribeDBClusters
import Network.AWS.RDS.ModifyDBSubnetGroup
import Network.AWS.RDS.ListTagsForResource
import Network.AWS.RDS.DeleteOptionGroup
import Network.AWS.RDS.DeleteDBCluster
import Network.AWS.RDS.DescribeReservedDBInstances
import Network.AWS.RDS.CopyDBParameterGroup
import Network.AWS.RDS.RemoveSourceIdentifierFromSubscription
import Network.AWS.RDS.DeleteCustomAvailabilityZone
import Network.AWS.RDS.DescribeDBProxyTargets
import Network.AWS.RDS.DescribeEngineDefaultClusterParameters
import Network.AWS.RDS.DescribeDBSnapshotAttributes
import Network.AWS.RDS.CreateCustomAvailabilityZone
import Network.AWS.RDS.BacktrackDBCluster
import Network.AWS.RDS.DeleteGlobalCluster
import Network.AWS.RDS.PromoteReadReplicaDBCluster
import Network.AWS.RDS.RemoveTagsFromResource
import Network.AWS.RDS.RestoreDBInstanceFromDBSnapshot
import Network.AWS.RDS.DeleteDBProxy
import Network.AWS.RDS.CreateEventSubscription
import Network.AWS.RDS.PurchaseReservedDBInstancesOffering
import Network.AWS.RDS.CreateDBInstance
import Network.AWS.RDS.DeleteDBClusterParameterGroup
import Network.AWS.RDS.DescribeCertificates
import Network.AWS.RDS.AuthorizeDBSecurityGroupIngress
import Network.AWS.RDS.RemoveRoleFromDBInstance
import Network.AWS.RDS.DescribeSourceRegions
import Network.AWS.RDS.CreateDBClusterEndpoint
import Network.AWS.RDS.RestoreDBClusterFromSnapshot
import Network.AWS.RDS.DescribeOrderableDBInstanceOptions
import Network.AWS.RDS.DeleteDBClusterEndpoint
import Network.AWS.RDS.CreateDBProxy
import Network.AWS.RDS.DeleteDBInstanceAutomatedBackup
import Network.AWS.RDS.CreateDBClusterParameterGroup
import Network.AWS.RDS.CreateDBSnapshot
import Network.AWS.RDS.DeleteEventSubscription
import Network.AWS.RDS.DescribeDBClusterBacktracks
import Network.AWS.RDS.DescribeDBParameterGroups
import Network.AWS.RDS.ModifyDBSnapshotAttribute
import Network.AWS.RDS.DescribeDBInstanceAutomatedBackups
import Network.AWS.RDS.RemoveFromGlobalCluster
import Network.AWS.RDS.AddRoleToDBInstance
import Network.AWS.RDS.DeleteDBClusterSnapshot
import Network.AWS.RDS.DescribeValidDBInstanceModifications
import Network.AWS.RDS.DescribeDBClusterEndpoints
import Network.AWS.RDS.DescribeOptionGroupOptions
import Network.AWS.RDS.DescribeEventSubscriptions
import Network.AWS.RDS.AddTagsToResource
import Network.AWS.RDS.DescribeDBParameters
import Network.AWS.RDS.StopActivityStream
import Network.AWS.RDS.CreateDBClusterSnapshot
import Network.AWS.RDS.DescribeDBSnapshots
import Network.AWS.RDS.ModifyDBProxyTargetGroup
import Network.AWS.RDS.DescribeDBSubnetGroups
import Network.AWS.RDS.ModifyOptionGroup
import Network.AWS.RDS.StopDBCluster
import Network.AWS.RDS.CreateDBParameterGroup
import Network.AWS.RDS.ModifyDBClusterSnapshotAttribute
import Network.AWS.RDS.ModifyDBCluster
import Network.AWS.RDS.CopyDBClusterParameterGroup
import Network.AWS.RDS.DescribeEventCategories
import Network.AWS.RDS.DescribeGlobalClusters
import Network.AWS.RDS.StartDBInstance
import Network.AWS.RDS.DescribeExportTasks
import Network.AWS.RDS.CancelExportTask
import Network.AWS.RDS.ModifyDBClusterParameterGroup
import Network.AWS.RDS.RestoreDBInstanceToPointInTime
import Network.AWS.RDS.DescribeDBClusterSnapshotAttributes
import Network.AWS.RDS.ModifyDBSnapshot
import Network.AWS.RDS.DescribeDBProxyTargetGroups
import Network.AWS.RDS.ModifyDBProxy
import Network.AWS.RDS.DescribePendingMaintenanceActions
import Network.AWS.RDS.AddRoleToDBCluster
import Network.AWS.RDS.CopyDBClusterSnapshot
import Network.AWS.RDS.ImportInstallationMedia
import Network.AWS.RDS.CreateGlobalCluster
import Network.AWS.RDS.ResetDBParameterGroup
import Network.AWS.RDS.DescribeInstallationMedia
import Network.AWS.RDS.DeregisterDBProxyTargets
import Network.AWS.RDS.CreateDBCluster
import Network.AWS.RDS.RemoveRoleFromDBCluster
import Network.AWS.RDS.FailoverDBCluster
import Network.AWS.RDS.RevokeDBSecurityGroupIngress
import Network.AWS.RDS.ModifyDBParameterGroup
import Network.AWS.RDS.ApplyPendingMaintenanceAction
import Network.AWS.RDS.CreateOptionGroup
import Network.AWS.RDS.DescribeAccountAttributes
import Network.AWS.RDS.DeleteDBSnapshot
import Network.AWS.RDS.DescribeDBClusterParameters
import Network.AWS.RDS.DeleteDBSubnetGroup
import Network.AWS.RDS.CreateDBSecurityGroup
import Network.AWS.RDS.ModifyCertificates
import Network.AWS.RDS.DescribeDBClusterSnapshots
import Network.AWS.RDS.RebootDBInstance
import Network.AWS.RDS.CreateDBSubnetGroup
import Network.AWS.RDS.DescribeReservedDBInstancesOfferings
import Network.AWS.RDS.DeleteDBSecurityGroup
import Network.AWS.RDS.DeleteDBInstance
import Network.AWS.RDS.StartActivityStream
import Network.AWS.RDS.CreateDBInstanceReadReplica
import Network.AWS.RDS.DeleteDBParameterGroup
import Network.AWS.RDS.ModifyCurrentDBClusterCapacity
import Network.AWS.RDS.ModifyGlobalCluster
import Network.AWS.RDS.RegisterDBProxyTargets
import Network.AWS.RDS.DescribeDBSecurityGroups
import Network.AWS.RDS.CopyOptionGroup
import Network.AWS.RDS.RestoreDBClusterToPointInTime
import Network.AWS.RDS.DeleteInstallationMedia
import Network.AWS.RDS.DescribeDBInstances
import Network.AWS.RDS.RestoreDBInstanceFromS3
import Network.AWS.RDS.DownloadDBLogFilePortion
import Network.AWS.RDS.DescribeDBProxies
import Network.AWS.RDS.StartExportTask
import qualified Network.AWS.Prelude as Lude

{- $errors
Error matchers are designed for use with the functions provided by
<http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
This allows catching (and rethrowing) service specific errors returned
by 'RDS'.
-}

{- $operations
Some AWS operations return results that are incomplete and require subsequent
requests in order to obtain the entire result set. The process of sending
subsequent requests to continue where a previous request left off is called
pagination. For example, the 'ListObjects' operation of Amazon S3 returns up to
1000 objects at a time, and you must send subsequent requests with the
appropriate Marker in order to retrieve the next page of results.

Operations that have an 'AWSPager' instance can transparently perform subsequent
requests, correctly setting Markers and other request facets to iterate through
the entire result set of a truncated API operation. Operations which support
this have an additional note in the documentation.

Many operations have the ability to filter results on the server side. See the
individual operation parameters for details.
-}

{- $waiters
Waiters poll by repeatedly sending a request until some remote success condition
configured by the 'Wait' specification is fulfilled. The 'Wait' specification
determines how many attempts should be made, in addition to delay and retry strategies.
-}
