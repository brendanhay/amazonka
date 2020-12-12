{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

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
module Network.AWS.RDS
  ( -- * Service configuration
    rdsService,

    -- * Errors
    -- $errors

    -- * Waiters
    -- $waiters

    -- ** DBInstanceAvailable
    mkDBInstanceAvailable,

    -- ** DBSnapshotCompleted
    mkDBSnapshotCompleted,

    -- ** DBSnapshotDeleted
    mkDBSnapshotDeleted,

    -- ** DBInstanceDeleted
    mkDBInstanceDeleted,

    -- ** DBClusterSnapshotDeleted
    mkDBClusterSnapshotDeleted,

    -- ** DBSnapshotAvailable
    mkDBSnapshotAvailable,

    -- ** DBClusterSnapshotAvailable
    mkDBClusterSnapshotAvailable,

    -- * Operations
    -- $operations

    -- ** StartDBCluster
    module Network.AWS.RDS.StartDBCluster,

    -- ** DescribeDBClusterParameterGroups (Paginated)
    module Network.AWS.RDS.DescribeDBClusterParameterGroups,

    -- ** PromoteReadReplica
    module Network.AWS.RDS.PromoteReadReplica,

    -- ** DescribeDBEngineVersions (Paginated)
    module Network.AWS.RDS.DescribeDBEngineVersions,

    -- ** StopDBInstance
    module Network.AWS.RDS.StopDBInstance,

    -- ** ModifyDBClusterEndpoint
    module Network.AWS.RDS.ModifyDBClusterEndpoint,

    -- ** CopyDBSnapshot
    module Network.AWS.RDS.CopyDBSnapshot,

    -- ** AddSourceIdentifierToSubscription
    module Network.AWS.RDS.AddSourceIdentifierToSubscription,

    -- ** ModifyDBInstance
    module Network.AWS.RDS.ModifyDBInstance,

    -- ** ModifyEventSubscription
    module Network.AWS.RDS.ModifyEventSubscription,

    -- ** ResetDBClusterParameterGroup
    module Network.AWS.RDS.ResetDBClusterParameterGroup,

    -- ** DescribeCustomAvailabilityZones (Paginated)
    module Network.AWS.RDS.DescribeCustomAvailabilityZones,

    -- ** RestoreDBClusterFromS3
    module Network.AWS.RDS.RestoreDBClusterFromS3,

    -- ** DescribeEvents (Paginated)
    module Network.AWS.RDS.DescribeEvents,

    -- ** DescribeEngineDefaultParameters (Paginated)
    module Network.AWS.RDS.DescribeEngineDefaultParameters,

    -- ** DescribeOptionGroups (Paginated)
    module Network.AWS.RDS.DescribeOptionGroups,

    -- ** DescribeDBLogFiles (Paginated)
    module Network.AWS.RDS.DescribeDBLogFiles,

    -- ** DescribeDBClusters (Paginated)
    module Network.AWS.RDS.DescribeDBClusters,

    -- ** ModifyDBSubnetGroup
    module Network.AWS.RDS.ModifyDBSubnetGroup,

    -- ** ListTagsForResource
    module Network.AWS.RDS.ListTagsForResource,

    -- ** DeleteOptionGroup
    module Network.AWS.RDS.DeleteOptionGroup,

    -- ** DeleteDBCluster
    module Network.AWS.RDS.DeleteDBCluster,

    -- ** DescribeReservedDBInstances (Paginated)
    module Network.AWS.RDS.DescribeReservedDBInstances,

    -- ** CopyDBParameterGroup
    module Network.AWS.RDS.CopyDBParameterGroup,

    -- ** RemoveSourceIdentifierFromSubscription
    module Network.AWS.RDS.RemoveSourceIdentifierFromSubscription,

    -- ** DeleteCustomAvailabilityZone
    module Network.AWS.RDS.DeleteCustomAvailabilityZone,

    -- ** DescribeDBProxyTargets (Paginated)
    module Network.AWS.RDS.DescribeDBProxyTargets,

    -- ** DescribeEngineDefaultClusterParameters (Paginated)
    module Network.AWS.RDS.DescribeEngineDefaultClusterParameters,

    -- ** DescribeDBSnapshotAttributes
    module Network.AWS.RDS.DescribeDBSnapshotAttributes,

    -- ** CreateCustomAvailabilityZone
    module Network.AWS.RDS.CreateCustomAvailabilityZone,

    -- ** BacktrackDBCluster
    module Network.AWS.RDS.BacktrackDBCluster,

    -- ** DeleteGlobalCluster
    module Network.AWS.RDS.DeleteGlobalCluster,

    -- ** PromoteReadReplicaDBCluster
    module Network.AWS.RDS.PromoteReadReplicaDBCluster,

    -- ** RemoveTagsFromResource
    module Network.AWS.RDS.RemoveTagsFromResource,

    -- ** RestoreDBInstanceFromDBSnapshot
    module Network.AWS.RDS.RestoreDBInstanceFromDBSnapshot,

    -- ** DeleteDBProxy
    module Network.AWS.RDS.DeleteDBProxy,

    -- ** CreateEventSubscription
    module Network.AWS.RDS.CreateEventSubscription,

    -- ** PurchaseReservedDBInstancesOffering
    module Network.AWS.RDS.PurchaseReservedDBInstancesOffering,

    -- ** CreateDBInstance
    module Network.AWS.RDS.CreateDBInstance,

    -- ** DeleteDBClusterParameterGroup
    module Network.AWS.RDS.DeleteDBClusterParameterGroup,

    -- ** DescribeCertificates (Paginated)
    module Network.AWS.RDS.DescribeCertificates,

    -- ** AuthorizeDBSecurityGroupIngress
    module Network.AWS.RDS.AuthorizeDBSecurityGroupIngress,

    -- ** RemoveRoleFromDBInstance
    module Network.AWS.RDS.RemoveRoleFromDBInstance,

    -- ** DescribeSourceRegions (Paginated)
    module Network.AWS.RDS.DescribeSourceRegions,

    -- ** CreateDBClusterEndpoint
    module Network.AWS.RDS.CreateDBClusterEndpoint,

    -- ** RestoreDBClusterFromSnapshot
    module Network.AWS.RDS.RestoreDBClusterFromSnapshot,

    -- ** DescribeOrderableDBInstanceOptions (Paginated)
    module Network.AWS.RDS.DescribeOrderableDBInstanceOptions,

    -- ** DeleteDBClusterEndpoint
    module Network.AWS.RDS.DeleteDBClusterEndpoint,

    -- ** CreateDBProxy
    module Network.AWS.RDS.CreateDBProxy,

    -- ** DeleteDBInstanceAutomatedBackup
    module Network.AWS.RDS.DeleteDBInstanceAutomatedBackup,

    -- ** CreateDBClusterParameterGroup
    module Network.AWS.RDS.CreateDBClusterParameterGroup,

    -- ** CreateDBSnapshot
    module Network.AWS.RDS.CreateDBSnapshot,

    -- ** DeleteEventSubscription
    module Network.AWS.RDS.DeleteEventSubscription,

    -- ** DescribeDBClusterBacktracks (Paginated)
    module Network.AWS.RDS.DescribeDBClusterBacktracks,

    -- ** DescribeDBParameterGroups (Paginated)
    module Network.AWS.RDS.DescribeDBParameterGroups,

    -- ** ModifyDBSnapshotAttribute
    module Network.AWS.RDS.ModifyDBSnapshotAttribute,

    -- ** DescribeDBInstanceAutomatedBackups (Paginated)
    module Network.AWS.RDS.DescribeDBInstanceAutomatedBackups,

    -- ** RemoveFromGlobalCluster
    module Network.AWS.RDS.RemoveFromGlobalCluster,

    -- ** AddRoleToDBInstance
    module Network.AWS.RDS.AddRoleToDBInstance,

    -- ** DeleteDBClusterSnapshot
    module Network.AWS.RDS.DeleteDBClusterSnapshot,

    -- ** DescribeValidDBInstanceModifications
    module Network.AWS.RDS.DescribeValidDBInstanceModifications,

    -- ** DescribeDBClusterEndpoints (Paginated)
    module Network.AWS.RDS.DescribeDBClusterEndpoints,

    -- ** DescribeOptionGroupOptions (Paginated)
    module Network.AWS.RDS.DescribeOptionGroupOptions,

    -- ** DescribeEventSubscriptions (Paginated)
    module Network.AWS.RDS.DescribeEventSubscriptions,

    -- ** AddTagsToResource
    module Network.AWS.RDS.AddTagsToResource,

    -- ** DescribeDBParameters (Paginated)
    module Network.AWS.RDS.DescribeDBParameters,

    -- ** StopActivityStream
    module Network.AWS.RDS.StopActivityStream,

    -- ** CreateDBClusterSnapshot
    module Network.AWS.RDS.CreateDBClusterSnapshot,

    -- ** DescribeDBSnapshots (Paginated)
    module Network.AWS.RDS.DescribeDBSnapshots,

    -- ** ModifyDBProxyTargetGroup
    module Network.AWS.RDS.ModifyDBProxyTargetGroup,

    -- ** DescribeDBSubnetGroups (Paginated)
    module Network.AWS.RDS.DescribeDBSubnetGroups,

    -- ** ModifyOptionGroup
    module Network.AWS.RDS.ModifyOptionGroup,

    -- ** StopDBCluster
    module Network.AWS.RDS.StopDBCluster,

    -- ** CreateDBParameterGroup
    module Network.AWS.RDS.CreateDBParameterGroup,

    -- ** ModifyDBClusterSnapshotAttribute
    module Network.AWS.RDS.ModifyDBClusterSnapshotAttribute,

    -- ** ModifyDBCluster
    module Network.AWS.RDS.ModifyDBCluster,

    -- ** CopyDBClusterParameterGroup
    module Network.AWS.RDS.CopyDBClusterParameterGroup,

    -- ** DescribeEventCategories
    module Network.AWS.RDS.DescribeEventCategories,

    -- ** DescribeGlobalClusters (Paginated)
    module Network.AWS.RDS.DescribeGlobalClusters,

    -- ** StartDBInstance
    module Network.AWS.RDS.StartDBInstance,

    -- ** DescribeExportTasks (Paginated)
    module Network.AWS.RDS.DescribeExportTasks,

    -- ** CancelExportTask
    module Network.AWS.RDS.CancelExportTask,

    -- ** ModifyDBClusterParameterGroup
    module Network.AWS.RDS.ModifyDBClusterParameterGroup,

    -- ** RestoreDBInstanceToPointInTime
    module Network.AWS.RDS.RestoreDBInstanceToPointInTime,

    -- ** DescribeDBClusterSnapshotAttributes
    module Network.AWS.RDS.DescribeDBClusterSnapshotAttributes,

    -- ** ModifyDBSnapshot
    module Network.AWS.RDS.ModifyDBSnapshot,

    -- ** DescribeDBProxyTargetGroups (Paginated)
    module Network.AWS.RDS.DescribeDBProxyTargetGroups,

    -- ** ModifyDBProxy
    module Network.AWS.RDS.ModifyDBProxy,

    -- ** DescribePendingMaintenanceActions (Paginated)
    module Network.AWS.RDS.DescribePendingMaintenanceActions,

    -- ** AddRoleToDBCluster
    module Network.AWS.RDS.AddRoleToDBCluster,

    -- ** CopyDBClusterSnapshot
    module Network.AWS.RDS.CopyDBClusterSnapshot,

    -- ** ImportInstallationMedia
    module Network.AWS.RDS.ImportInstallationMedia,

    -- ** CreateGlobalCluster
    module Network.AWS.RDS.CreateGlobalCluster,

    -- ** ResetDBParameterGroup
    module Network.AWS.RDS.ResetDBParameterGroup,

    -- ** DescribeInstallationMedia (Paginated)
    module Network.AWS.RDS.DescribeInstallationMedia,

    -- ** DeregisterDBProxyTargets
    module Network.AWS.RDS.DeregisterDBProxyTargets,

    -- ** CreateDBCluster
    module Network.AWS.RDS.CreateDBCluster,

    -- ** RemoveRoleFromDBCluster
    module Network.AWS.RDS.RemoveRoleFromDBCluster,

    -- ** FailoverDBCluster
    module Network.AWS.RDS.FailoverDBCluster,

    -- ** RevokeDBSecurityGroupIngress
    module Network.AWS.RDS.RevokeDBSecurityGroupIngress,

    -- ** ModifyDBParameterGroup
    module Network.AWS.RDS.ModifyDBParameterGroup,

    -- ** ApplyPendingMaintenanceAction
    module Network.AWS.RDS.ApplyPendingMaintenanceAction,

    -- ** CreateOptionGroup
    module Network.AWS.RDS.CreateOptionGroup,

    -- ** DescribeAccountAttributes
    module Network.AWS.RDS.DescribeAccountAttributes,

    -- ** DeleteDBSnapshot
    module Network.AWS.RDS.DeleteDBSnapshot,

    -- ** DescribeDBClusterParameters (Paginated)
    module Network.AWS.RDS.DescribeDBClusterParameters,

    -- ** DeleteDBSubnetGroup
    module Network.AWS.RDS.DeleteDBSubnetGroup,

    -- ** CreateDBSecurityGroup
    module Network.AWS.RDS.CreateDBSecurityGroup,

    -- ** ModifyCertificates
    module Network.AWS.RDS.ModifyCertificates,

    -- ** DescribeDBClusterSnapshots (Paginated)
    module Network.AWS.RDS.DescribeDBClusterSnapshots,

    -- ** RebootDBInstance
    module Network.AWS.RDS.RebootDBInstance,

    -- ** CreateDBSubnetGroup
    module Network.AWS.RDS.CreateDBSubnetGroup,

    -- ** DescribeReservedDBInstancesOfferings (Paginated)
    module Network.AWS.RDS.DescribeReservedDBInstancesOfferings,

    -- ** DeleteDBSecurityGroup
    module Network.AWS.RDS.DeleteDBSecurityGroup,

    -- ** DeleteDBInstance
    module Network.AWS.RDS.DeleteDBInstance,

    -- ** StartActivityStream
    module Network.AWS.RDS.StartActivityStream,

    -- ** CreateDBInstanceReadReplica
    module Network.AWS.RDS.CreateDBInstanceReadReplica,

    -- ** DeleteDBParameterGroup
    module Network.AWS.RDS.DeleteDBParameterGroup,

    -- ** ModifyCurrentDBClusterCapacity
    module Network.AWS.RDS.ModifyCurrentDBClusterCapacity,

    -- ** ModifyGlobalCluster
    module Network.AWS.RDS.ModifyGlobalCluster,

    -- ** RegisterDBProxyTargets
    module Network.AWS.RDS.RegisterDBProxyTargets,

    -- ** DescribeDBSecurityGroups (Paginated)
    module Network.AWS.RDS.DescribeDBSecurityGroups,

    -- ** CopyOptionGroup
    module Network.AWS.RDS.CopyOptionGroup,

    -- ** RestoreDBClusterToPointInTime
    module Network.AWS.RDS.RestoreDBClusterToPointInTime,

    -- ** DeleteInstallationMedia
    module Network.AWS.RDS.DeleteInstallationMedia,

    -- ** DescribeDBInstances (Paginated)
    module Network.AWS.RDS.DescribeDBInstances,

    -- ** RestoreDBInstanceFromS3
    module Network.AWS.RDS.RestoreDBInstanceFromS3,

    -- ** DownloadDBLogFilePortion (Paginated)
    module Network.AWS.RDS.DownloadDBLogFilePortion,

    -- ** DescribeDBProxies (Paginated)
    module Network.AWS.RDS.DescribeDBProxies,

    -- ** StartExportTask
    module Network.AWS.RDS.StartExportTask,

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
    AccountQuota (..),
    mkAccountQuota,
    aqMax,
    aqUsed,
    aqAccountQuotaName,

    -- ** AvailabilityZone
    AvailabilityZone (..),
    mkAvailabilityZone,
    azName,

    -- ** AvailableProcessorFeature
    AvailableProcessorFeature (..),
    mkAvailableProcessorFeature,
    apfName,
    apfDefaultValue,
    apfAllowedValues,

    -- ** Certificate
    Certificate (..),
    mkCertificate,
    cCertificateType,
    cCustomerOverride,
    cCertificateARN,
    cCustomerOverrideValidTill,
    cValidTill,
    cCertificateIdentifier,
    cThumbprint,
    cValidFrom,

    -- ** CharacterSet
    CharacterSet (..),
    mkCharacterSet,
    csCharacterSetName,
    csCharacterSetDescription,

    -- ** CloudwatchLogsExportConfiguration
    CloudwatchLogsExportConfiguration (..),
    mkCloudwatchLogsExportConfiguration,
    clecDisableLogTypes,
    clecEnableLogTypes,

    -- ** ConnectionPoolConfiguration
    ConnectionPoolConfiguration (..),
    mkConnectionPoolConfiguration,
    cpcMaxIdleConnectionsPercent,
    cpcSessionPinningFilters,
    cpcMaxConnectionsPercent,
    cpcConnectionBorrowTimeout,
    cpcInitQuery,

    -- ** ConnectionPoolConfigurationInfo
    ConnectionPoolConfigurationInfo (..),
    mkConnectionPoolConfigurationInfo,
    cpciMaxIdleConnectionsPercent,
    cpciSessionPinningFilters,
    cpciMaxConnectionsPercent,
    cpciConnectionBorrowTimeout,
    cpciInitQuery,

    -- ** CustomAvailabilityZone
    CustomAvailabilityZone (..),
    mkCustomAvailabilityZone,
    cazVPNDetails,
    cazCustomAvailabilityZoneName,
    cazCustomAvailabilityZoneId,
    cazCustomAvailabilityZoneStatus,

    -- ** DBCluster
    DBCluster (..),
    mkDBCluster,
    dcBacktrackConsumedChangeRecords,
    dcEngineVersion,
    dcStatus,
    dcDeletionProtection,
    dcStorageEncrypted,
    dcDBClusterIdentifier,
    dcDBClusterMembers,
    dcReadReplicaIdentifiers,
    dcReplicationSourceIdentifier,
    dcActivityStreamKinesisStreamName,
    dcHostedZoneId,
    dcDBClusterParameterGroup,
    dcMasterUsername,
    dcIAMDatabaseAuthenticationEnabled,
    dcGlobalWriteForwardingRequested,
    dcEarliestBacktrackTime,
    dcBacktrackWindow,
    dcTagList,
    dcDBClusterResourceId,
    dcEarliestRestorableTime,
    dcCustomEndpoints,
    dcEngine,
    dcHTTPEndpointEnabled,
    dcDBClusterARN,
    dcCloneGroupId,
    dcLatestRestorableTime,
    dcCrossAccountClone,
    dcCapacity,
    dcPreferredMaintenanceWindow,
    dcAvailabilityZones,
    dcCharacterSetName,
    dcKMSKeyId,
    dcPreferredBackupWindow,
    dcAssociatedRoles,
    dcVPCSecurityGroups,
    dcBackupRetentionPeriod,
    dcDBSubnetGroup,
    dcActivityStreamMode,
    dcDatabaseName,
    dcMultiAZ,
    dcEngineMode,
    dcEnabledCloudwatchLogsExports,
    dcActivityStreamStatus,
    dcAllocatedStorage,
    dcCopyTagsToSnapshot,
    dcClusterCreateTime,
    dcEndpoint,
    dcScalingConfigurationInfo,
    dcActivityStreamKMSKeyId,
    dcPercentProgress,
    dcReaderEndpoint,
    dcGlobalWriteForwardingStatus,
    dcPort,
    dcDomainMemberships,
    dcDBClusterOptionGroupMemberships,

    -- ** DBClusterBacktrack
    DBClusterBacktrack (..),
    mkDBClusterBacktrack,
    dcbStatus,
    dcbBacktrackIdentifier,
    dcbBacktrackTo,
    dcbDBClusterIdentifier,
    dcbBacktrackedFrom,
    dcbBacktrackRequestCreationTime,

    -- ** DBClusterEndpoint
    DBClusterEndpoint (..),
    mkDBClusterEndpoint,
    dceStatus,
    dceDBClusterIdentifier,
    dceDBClusterEndpointARN,
    dceCustomEndpointType,
    dceStaticMembers,
    dceEndpointType,
    dceDBClusterEndpointIdentifier,
    dceEndpoint,
    dceDBClusterEndpointResourceIdentifier,
    dceExcludedMembers,

    -- ** DBClusterMember
    DBClusterMember (..),
    mkDBClusterMember,
    dcmPromotionTier,
    dcmDBInstanceIdentifier,
    dcmIsClusterWriter,
    dcmDBClusterParameterGroupStatus,

    -- ** DBClusterOptionGroupStatus
    DBClusterOptionGroupStatus (..),
    mkDBClusterOptionGroupStatus,
    dcogsStatus,
    dcogsDBClusterOptionGroupName,

    -- ** DBClusterParameterGroup
    DBClusterParameterGroup (..),
    mkDBClusterParameterGroup,
    dcpgDBClusterParameterGroupARN,
    dcpgDBParameterGroupFamily,
    dcpgDBClusterParameterGroupName,
    dcpgDescription,

    -- ** DBClusterParameterGroupNameMessage
    DBClusterParameterGroupNameMessage (..),
    mkDBClusterParameterGroupNameMessage,
    dcpgnmDBClusterParameterGroupName,

    -- ** DBClusterRole
    DBClusterRole (..),
    mkDBClusterRole,
    dcrStatus,
    dcrFeatureName,
    dcrRoleARN,

    -- ** DBClusterSnapshot
    DBClusterSnapshot (..),
    mkDBClusterSnapshot,
    dcsEngineVersion,
    dcsStatus,
    dcsStorageEncrypted,
    dcsDBClusterIdentifier,
    dcsMasterUsername,
    dcsIAMDatabaseAuthenticationEnabled,
    dcsDBClusterSnapshotARN,
    dcsVPCId,
    dcsTagList,
    dcsDBClusterSnapshotIdentifier,
    dcsEngine,
    dcsLicenseModel,
    dcsAvailabilityZones,
    dcsSnapshotType,
    dcsKMSKeyId,
    dcsSnapshotCreateTime,
    dcsAllocatedStorage,
    dcsSourceDBClusterSnapshotARN,
    dcsClusterCreateTime,
    dcsPercentProgress,
    dcsPort,

    -- ** DBClusterSnapshotAttribute
    DBClusterSnapshotAttribute (..),
    mkDBClusterSnapshotAttribute,
    dcsaAttributeValues,
    dcsaAttributeName,

    -- ** DBClusterSnapshotAttributesResult
    DBClusterSnapshotAttributesResult (..),
    mkDBClusterSnapshotAttributesResult,
    dcsarDBClusterSnapshotIdentifier,
    dcsarDBClusterSnapshotAttributes,

    -- ** DBEngineVersion
    DBEngineVersion (..),
    mkDBEngineVersion,
    devEngineVersion,
    devStatus,
    devDBEngineVersionDescription,
    devSupportedEngineModes,
    devDefaultCharacterSet,
    devEngine,
    devDBParameterGroupFamily,
    devSupportedCharacterSets,
    devDBEngineDescription,
    devSupportsGlobalDatabases,
    devValidUpgradeTarget,
    devSupportsParallelQuery,
    devSupportedNcharCharacterSets,
    devSupportsLogExportsToCloudwatchLogs,
    devSupportsReadReplica,
    devSupportedFeatureNames,
    devSupportedTimezones,
    devExportableLogTypes,

    -- ** DBInstance
    DBInstance (..),
    mkDBInstance,
    diEngineVersion,
    diDBSecurityGroups,
    diDeletionProtection,
    diStorageEncrypted,
    diDBClusterIdentifier,
    diPubliclyAccessible,
    diAutoMinorVersionUpgrade,
    diDBInstanceARN,
    diMasterUsername,
    diReadReplicaDBInstanceIdentifiers,
    diIAMDatabaseAuthenticationEnabled,
    diMonitoringRoleARN,
    diIOPS,
    diInstanceCreateTime,
    diTagList,
    diReadReplicaSourceDBInstanceIdentifier,
    diReplicaMode,
    diMonitoringInterval,
    diEngine,
    diProcessorFeatures,
    diLatestRestorableTime,
    diDBInstanceClass,
    diPromotionTier,
    diLicenseModel,
    diPreferredMaintenanceWindow,
    diPerformanceInsightsRetentionPeriod,
    diCACertificateIdentifier,
    diDBInstanceIdentifier,
    diCharacterSetName,
    diMaxAllocatedStorage,
    diKMSKeyId,
    diPreferredBackupWindow,
    diAssociatedRoles,
    diAvailabilityZone,
    diVPCSecurityGroups,
    diBackupRetentionPeriod,
    diNcharCharacterSetName,
    diPerformanceInsightsKMSKeyId,
    diDBSubnetGroup,
    diMultiAZ,
    diListenerEndpoint,
    diOptionGroupMemberships,
    diEnabledCloudwatchLogsExports,
    diEnhancedMonitoringResourceARN,
    diSecondaryAvailabilityZone,
    diPerformanceInsightsEnabled,
    diAllocatedStorage,
    diDBiResourceId,
    diDBParameterGroups,
    diCopyTagsToSnapshot,
    diTimezone,
    diTDECredentialARN,
    diEndpoint,
    diDBInstanceStatus,
    diDBInstancePort,
    diPendingModifiedValues,
    diReadReplicaDBClusterIdentifiers,
    diStorageType,
    diStatusInfos,
    diDomainMemberships,
    diDBName,

    -- ** DBInstanceAutomatedBackup
    DBInstanceAutomatedBackup (..),
    mkDBInstanceAutomatedBackup,
    diabRestoreWindow,
    diabEngineVersion,
    diabStatus,
    diabDBInstanceARN,
    diabMasterUsername,
    diabIAMDatabaseAuthenticationEnabled,
    diabIOPS,
    diabVPCId,
    diabInstanceCreateTime,
    diabEngine,
    diabEncrypted,
    diabLicenseModel,
    diabDBInstanceIdentifier,
    diabKMSKeyId,
    diabAvailabilityZone,
    diabRegion,
    diabAllocatedStorage,
    diabDBiResourceId,
    diabOptionGroupName,
    diabTimezone,
    diabTDECredentialARN,
    diabPort,
    diabStorageType,

    -- ** DBInstanceRole
    DBInstanceRole (..),
    mkDBInstanceRole,
    dirStatus,
    dirFeatureName,
    dirRoleARN,

    -- ** DBInstanceStatusInfo
    DBInstanceStatusInfo (..),
    mkDBInstanceStatusInfo,
    disiStatus,
    disiNormal,
    disiStatusType,
    disiMessage,

    -- ** DBParameterGroup
    DBParameterGroup (..),
    mkDBParameterGroup,
    dpgDBParameterGroupARN,
    dpgDBParameterGroupFamily,
    dpgDBParameterGroupName,
    dpgDescription,

    -- ** DBParameterGroupNameMessage
    DBParameterGroupNameMessage (..),
    mkDBParameterGroupNameMessage,
    dpgnmDBParameterGroupName,

    -- ** DBParameterGroupStatus
    DBParameterGroupStatus (..),
    mkDBParameterGroupStatus,
    dpgsDBParameterGroupName,
    dpgsParameterApplyStatus,

    -- ** DBProxy
    DBProxy (..),
    mkDBProxy,
    dpStatus,
    dpDBProxyARN,
    dpDebugLogging,
    dpVPCSubnetIds,
    dpEngineFamily,
    dpAuth,
    dpRequireTLS,
    dpIdleClientTimeout,
    dpUpdatedDate,
    dpCreatedDate,
    dpVPCSecurityGroupIds,
    dpDBProxyName,
    dpEndpoint,
    dpRoleARN,

    -- ** DBProxyTarget
    DBProxyTarget (..),
    mkDBProxyTarget,
    dptTargetARN,
    dptTargetHealth,
    dptTrackedClusterId,
    dptRDSResourceId,
    dptType,
    dptEndpoint,
    dptPort,

    -- ** DBProxyTargetGroup
    DBProxyTargetGroup (..),
    mkDBProxyTargetGroup,
    dptgStatus,
    dptgConnectionPoolConfig,
    dptgTargetGroupARN,
    dptgUpdatedDate,
    dptgCreatedDate,
    dptgDBProxyName,
    dptgTargetGroupName,
    dptgIsDefault,

    -- ** DBSecurityGroup
    DBSecurityGroup (..),
    mkDBSecurityGroup,
    dbsgVPCId,
    dbsgOwnerId,
    dbsgDBSecurityGroupARN,
    dbsgIPRanges,
    dbsgDBSecurityGroupName,
    dbsgEC2SecurityGroups,
    dbsgDBSecurityGroupDescription,

    -- ** DBSecurityGroupMembership
    DBSecurityGroupMembership (..),
    mkDBSecurityGroupMembership,
    dsgmStatus,
    dsgmDBSecurityGroupName,

    -- ** DBSnapshot
    DBSnapshot (..),
    mkDBSnapshot,
    dsEngineVersion,
    dsStatus,
    dsDBSnapshotARN,
    dsMasterUsername,
    dsSourceRegion,
    dsIAMDatabaseAuthenticationEnabled,
    dsIOPS,
    dsVPCId,
    dsInstanceCreateTime,
    dsTagList,
    dsEngine,
    dsEncrypted,
    dsDBSnapshotIdentifier,
    dsProcessorFeatures,
    dsLicenseModel,
    dsSourceDBSnapshotIdentifier,
    dsSnapshotType,
    dsDBInstanceIdentifier,
    dsKMSKeyId,
    dsAvailabilityZone,
    dsSnapshotCreateTime,
    dsAllocatedStorage,
    dsDBiResourceId,
    dsOptionGroupName,
    dsTimezone,
    dsTDECredentialARN,
    dsPercentProgress,
    dsPort,
    dsStorageType,

    -- ** DBSnapshotAttribute
    DBSnapshotAttribute (..),
    mkDBSnapshotAttribute,
    dsaAttributeValues,
    dsaAttributeName,

    -- ** DBSnapshotAttributesResult
    DBSnapshotAttributesResult (..),
    mkDBSnapshotAttributesResult,
    dsarDBSnapshotIdentifier,
    dsarDBSnapshotAttributes,

    -- ** DBSubnetGroup
    DBSubnetGroup (..),
    mkDBSubnetGroup,
    dsgDBSubnetGroupName,
    dsgVPCId,
    dsgSubnets,
    dsgDBSubnetGroupDescription,
    dsgDBSubnetGroupARN,
    dsgSubnetGroupStatus,

    -- ** DescribeDBLogFilesDetails
    DescribeDBLogFilesDetails (..),
    mkDescribeDBLogFilesDetails,
    ddlfdLastWritten,
    ddlfdSize,
    ddlfdLogFileName,

    -- ** DomainMembership
    DomainMembership (..),
    mkDomainMembership,
    dmStatus,
    dmFQDN,
    dmDomain,
    dmIAMRoleName,

    -- ** DoubleRange
    DoubleRange (..),
    mkDoubleRange,
    drTo,
    drFrom,

    -- ** EC2SecurityGroup
    EC2SecurityGroup (..),
    mkEC2SecurityGroup,
    esgStatus,
    esgEC2SecurityGroupOwnerId,
    esgEC2SecurityGroupName,
    esgEC2SecurityGroupId,

    -- ** Endpoint
    Endpoint (..),
    mkEndpoint,
    eHostedZoneId,
    eAddress,
    ePort,

    -- ** EngineDefaults
    EngineDefaults (..),
    mkEngineDefaults,
    edDBParameterGroupFamily,
    edMarker,
    edParameters,

    -- ** Event
    Event (..),
    mkEvent,
    eSourceType,
    eSourceARN,
    eSourceIdentifier,
    eDate,
    eEventCategories,
    eMessage,

    -- ** EventCategoriesMap
    EventCategoriesMap (..),
    mkEventCategoriesMap,
    ecmSourceType,
    ecmEventCategories,

    -- ** EventSubscription
    EventSubscription (..),
    mkEventSubscription,
    esStatus,
    esCustomerAWSId,
    esCustSubscriptionId,
    esSNSTopicARN,
    esEventSubscriptionARN,
    esEnabled,
    esSourceType,
    esSubscriptionCreationTime,
    esEventCategoriesList,
    esSourceIdsList,

    -- ** ExportTask
    ExportTask (..),
    mkExportTask,
    etTotalExtractedDataInGB,
    etStatus,
    etIAMRoleARN,
    etSourceARN,
    etExportOnly,
    etTaskStartTime,
    etWarningMessage,
    etSnapshotTime,
    etKMSKeyId,
    etTaskEndTime,
    etExportTaskIdentifier,
    etS3Prefix,
    etPercentProgress,
    etS3Bucket,
    etFailureCause,

    -- ** Filter
    Filter (..),
    mkFilter,
    fName,
    fValues,

    -- ** GlobalCluster
    GlobalCluster (..),
    mkGlobalCluster,
    gcEngineVersion,
    gcStatus,
    gcDeletionProtection,
    gcStorageEncrypted,
    gcGlobalClusterIdentifier,
    gcEngine,
    gcGlobalClusterARN,
    gcDatabaseName,
    gcGlobalClusterMembers,
    gcGlobalClusterResourceId,

    -- ** GlobalClusterMember
    GlobalClusterMember (..),
    mkGlobalClusterMember,
    gcmReaders,
    gcmDBClusterARN,
    gcmIsWriter,
    gcmGlobalWriteForwardingStatus,

    -- ** IPRange
    IPRange (..),
    mkIPRange,
    irStatus,
    irCIdRIP,

    -- ** InstallationMedia
    InstallationMedia (..),
    mkInstallationMedia,
    imEngineVersion,
    imStatus,
    imInstallationMediaId,
    imEngineInstallationMediaPath,
    imEngine,
    imOSInstallationMediaPath,
    imCustomAvailabilityZoneId,
    imFailureCause,

    -- ** InstallationMediaFailureCause
    InstallationMediaFailureCause (..),
    mkInstallationMediaFailureCause,
    imfcMessage,

    -- ** MinimumEngineVersionPerAllowedValue
    MinimumEngineVersionPerAllowedValue (..),
    mkMinimumEngineVersionPerAllowedValue,
    mevpavMinimumEngineVersion,
    mevpavAllowedValue,

    -- ** Option
    Option (..),
    mkOption,
    oOptionName,
    oPermanent,
    oPersistent,
    oOptionDescription,
    oOptionSettings,
    oVPCSecurityGroupMemberships,
    oDBSecurityGroupMemberships,
    oOptionVersion,
    oPort,

    -- ** OptionConfiguration
    OptionConfiguration (..),
    mkOptionConfiguration,
    ocOptionSettings,
    ocVPCSecurityGroupMemberships,
    ocDBSecurityGroupMemberships,
    ocOptionVersion,
    ocPort,
    ocOptionName,

    -- ** OptionGroup
    OptionGroup (..),
    mkOptionGroup,
    ogOptionGroupDescription,
    ogVPCId,
    ogAllowsVPCAndNonVPCInstanceMemberships,
    ogEngineName,
    ogOptionGroupARN,
    ogMajorEngineVersion,
    ogOptions,
    ogOptionGroupName,

    -- ** OptionGroupMembership
    OptionGroupMembership (..),
    mkOptionGroupMembership,
    ogmStatus,
    ogmOptionGroupName,

    -- ** OptionGroupOption
    OptionGroupOption (..),
    mkOptionGroupOption,
    ogoMinimumRequiredMinorEngineVersion,
    ogoOptionsConflictsWith,
    ogoPermanent,
    ogoPersistent,
    ogoOptionGroupOptionVersions,
    ogoEngineName,
    ogoMajorEngineVersion,
    ogoName,
    ogoSupportsOptionVersionDowngrade,
    ogoDefaultPort,
    ogoOptionGroupOptionSettings,
    ogoRequiresAutoMinorEngineVersionUpgrade,
    ogoPortRequired,
    ogoDescription,
    ogoOptionsDependedOn,
    ogoVPCOnly,

    -- ** OptionGroupOptionSetting
    OptionGroupOptionSetting (..),
    mkOptionGroupOptionSetting,
    ogosApplyType,
    ogosMinimumEngineVersionPerAllowedValue,
    ogosSettingName,
    ogosDefaultValue,
    ogosIsModifiable,
    ogosSettingDescription,
    ogosAllowedValues,
    ogosIsRequired,

    -- ** OptionSetting
    OptionSetting (..),
    mkOptionSetting,
    osIsCollection,
    osApplyType,
    osValue,
    osName,
    osDefaultValue,
    osIsModifiable,
    osDataType,
    osAllowedValues,
    osDescription,

    -- ** OptionVersion
    OptionVersion (..),
    mkOptionVersion,
    ovVersion,
    ovIsDefault,

    -- ** OrderableDBInstanceOption
    OrderableDBInstanceOption (..),
    mkOrderableDBInstanceOption,
    odioEngineVersion,
    odioMinIOPSPerGib,
    odioSupportsIAMDatabaseAuthentication,
    odioMinIOPSPerDBInstance,
    odioMultiAZCapable,
    odioMaxStorageSize,
    odioSupportedEngineModes,
    odioAvailabilityZoneGroup,
    odioAvailableProcessorFeatures,
    odioEngine,
    odioMinStorageSize,
    odioOutpostCapable,
    odioSupportsIOPS,
    odioSupportsKerberosAuthentication,
    odioSupportsPerformanceInsights,
    odioDBInstanceClass,
    odioSupportsGlobalDatabases,
    odioLicenseModel,
    odioAvailabilityZones,
    odioSupportsStorageAutoscaling,
    odioSupportsStorageEncryption,
    odioReadReplicaCapable,
    odioMaxIOPSPerGib,
    odioVPC,
    odioSupportsEnhancedMonitoring,
    odioMaxIOPSPerDBInstance,
    odioStorageType,

    -- ** Outpost
    Outpost (..),
    mkOutpost,
    oARN,

    -- ** Parameter
    Parameter (..),
    mkParameter,
    pApplyType,
    pParameterValue,
    pSupportedEngineModes,
    pApplyMethod,
    pMinimumEngineVersion,
    pSource,
    pIsModifiable,
    pDataType,
    pAllowedValues,
    pParameterName,
    pDescription,

    -- ** PendingCloudwatchLogsExports
    PendingCloudwatchLogsExports (..),
    mkPendingCloudwatchLogsExports,
    pcleLogTypesToEnable,
    pcleLogTypesToDisable,

    -- ** PendingMaintenanceAction
    PendingMaintenanceAction (..),
    mkPendingMaintenanceAction,
    pmaAutoAppliedAfterDate,
    pmaAction,
    pmaOptInStatus,
    pmaDescription,
    pmaForcedApplyDate,
    pmaCurrentApplyDate,

    -- ** PendingModifiedValues
    PendingModifiedValues (..),
    mkPendingModifiedValues,
    pmvEngineVersion,
    pmvMasterUserPassword,
    pmvDBSubnetGroupName,
    pmvIOPS,
    pmvProcessorFeatures,
    pmvDBInstanceClass,
    pmvLicenseModel,
    pmvCACertificateIdentifier,
    pmvDBInstanceIdentifier,
    pmvPendingCloudwatchLogsExports,
    pmvBackupRetentionPeriod,
    pmvMultiAZ,
    pmvAllocatedStorage,
    pmvPort,
    pmvStorageType,

    -- ** ProcessorFeature
    ProcessorFeature (..),
    mkProcessorFeature,
    pfValue,
    pfName,

    -- ** Range
    Range (..),
    mkRange,
    rTo,
    rFrom,
    rStep,

    -- ** RecurringCharge
    RecurringCharge (..),
    mkRecurringCharge,
    rcRecurringChargeFrequency,
    rcRecurringChargeAmount,

    -- ** ReservedDBInstance
    ReservedDBInstance (..),
    mkReservedDBInstance,
    rdiDBInstanceCount,
    rdiState,
    rdiCurrencyCode,
    rdiStartTime,
    rdiProductDescription,
    rdiLeaseId,
    rdiReservedDBInstanceId,
    rdiReservedDBInstanceARN,
    rdiDBInstanceClass,
    rdiMultiAZ,
    rdiReservedDBInstancesOfferingId,
    rdiRecurringCharges,
    rdiOfferingType,
    rdiUsagePrice,
    rdiFixedPrice,
    rdiDuration,

    -- ** ReservedDBInstancesOffering
    ReservedDBInstancesOffering (..),
    mkReservedDBInstancesOffering,
    rdioCurrencyCode,
    rdioProductDescription,
    rdioDBInstanceClass,
    rdioMultiAZ,
    rdioReservedDBInstancesOfferingId,
    rdioRecurringCharges,
    rdioOfferingType,
    rdioUsagePrice,
    rdioFixedPrice,
    rdioDuration,

    -- ** ResourcePendingMaintenanceActions
    ResourcePendingMaintenanceActions (..),
    mkResourcePendingMaintenanceActions,
    rpmaPendingMaintenanceActionDetails,
    rpmaResourceIdentifier,

    -- ** RestoreWindow
    RestoreWindow (..),
    mkRestoreWindow,
    rwLatestTime,
    rwEarliestTime,

    -- ** ScalingConfiguration
    ScalingConfiguration (..),
    mkScalingConfiguration,
    scSecondsUntilAutoPause,
    scTimeoutAction,
    scAutoPause,
    scMaxCapacity,
    scMinCapacity,

    -- ** ScalingConfigurationInfo
    ScalingConfigurationInfo (..),
    mkScalingConfigurationInfo,
    sciSecondsUntilAutoPause,
    sciTimeoutAction,
    sciAutoPause,
    sciMaxCapacity,
    sciMinCapacity,

    -- ** SourceRegion
    SourceRegion (..),
    mkSourceRegion,
    srStatus,
    srRegionName,
    srEndpoint,

    -- ** Subnet
    Subnet (..),
    mkSubnet,
    sSubnetStatus,
    sSubnetIdentifier,
    sSubnetAvailabilityZone,
    sSubnetOutpost,

    -- ** Tag
    Tag (..),
    mkTag,
    tValue,
    tKey,

    -- ** TargetHealth
    TargetHealth (..),
    mkTargetHealth,
    thState,
    thReason,
    thDescription,

    -- ** Timezone
    Timezone (..),
    mkTimezone,
    tTimezoneName,

    -- ** UpgradeTarget
    UpgradeTarget (..),
    mkUpgradeTarget,
    utEngineVersion,
    utIsMajorVersionUpgrade,
    utEngine,
    utAutoUpgrade,
    utDescription,

    -- ** UserAuthConfig
    UserAuthConfig (..),
    mkUserAuthConfig,
    uacIAMAuth,
    uacUserName,
    uacAuthScheme,
    uacSecretARN,
    uacDescription,

    -- ** UserAuthConfigInfo
    UserAuthConfigInfo (..),
    mkUserAuthConfigInfo,
    uaciIAMAuth,
    uaciUserName,
    uaciAuthScheme,
    uaciSecretARN,
    uaciDescription,

    -- ** VPCSecurityGroupMembership
    VPCSecurityGroupMembership (..),
    mkVPCSecurityGroupMembership,
    vsgmStatus,
    vsgmVPCSecurityGroupId,

    -- ** VPNDetails
    VPNDetails (..),
    mkVPNDetails,
    vdVPNName,
    vdVPNTunnelOriginatorIP,
    vdVPNId,
    vdVPNState,
    vdVPNPSK,
    vdVPNGatewayIP,

    -- ** ValidDBInstanceModificationsMessage
    ValidDBInstanceModificationsMessage (..),
    mkValidDBInstanceModificationsMessage,
    vdimmValidProcessorFeatures,
    vdimmStorage,

    -- ** ValidStorageOptions
    ValidStorageOptions (..),
    mkValidStorageOptions,
    vsoStorageSize,
    vsoProvisionedIOPS,
    vsoIOPSToStorageRatio,
    vsoSupportsStorageAutoscaling,
    vsoStorageType,

    -- * Serialization types
    Lude.Base64 (..),
    Lude._Base64,
    Lude.Sensitive (..),
    Lude._Sensitive,
    Lude.Time (..),
    Lude._Time,
    Lude.DateTime,
    Lude.Timestamp,
  )
where

import qualified Network.AWS.Prelude as Lude
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
import Network.AWS.RDS.ImportInstallationMedia
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
import Network.AWS.RDS.RestoreDBClusterFromS3
import Network.AWS.RDS.RestoreDBClusterFromSnapshot
import Network.AWS.RDS.RestoreDBClusterToPointInTime
import Network.AWS.RDS.RestoreDBInstanceFromDBSnapshot
import Network.AWS.RDS.RestoreDBInstanceFromS3
import Network.AWS.RDS.RestoreDBInstanceToPointInTime
import Network.AWS.RDS.RevokeDBSecurityGroupIngress
import Network.AWS.RDS.StartActivityStream
import Network.AWS.RDS.StartDBCluster
import Network.AWS.RDS.StartDBInstance
import Network.AWS.RDS.StartExportTask
import Network.AWS.RDS.StopActivityStream
import Network.AWS.RDS.StopDBCluster
import Network.AWS.RDS.StopDBInstance
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
