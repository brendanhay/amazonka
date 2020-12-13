{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.DBInstance
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.DBInstance
  ( DBInstance (..),

    -- * Smart constructor
    mkDBInstance,

    -- * Lenses
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
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.RDS.Types.DBInstanceRole
import Network.AWS.RDS.Types.DBInstanceStatusInfo
import Network.AWS.RDS.Types.DBParameterGroupStatus
import Network.AWS.RDS.Types.DBSecurityGroupMembership
import Network.AWS.RDS.Types.DBSubnetGroup
import Network.AWS.RDS.Types.DomainMembership
import Network.AWS.RDS.Types.Endpoint
import Network.AWS.RDS.Types.OptionGroupMembership
import Network.AWS.RDS.Types.PendingModifiedValues
import Network.AWS.RDS.Types.ProcessorFeature
import Network.AWS.RDS.Types.ReplicaMode
import Network.AWS.RDS.Types.Tag
import Network.AWS.RDS.Types.VPCSecurityGroupMembership

-- | Contains the details of an Amazon RDS DB instance.
--
-- This data type is used as a response element in the @DescribeDBInstances@ action.
--
-- /See:/ 'mkDBInstance' smart constructor.
data DBInstance = DBInstance'
  { -- | Indicates the database engine version.
    engineVersion :: Lude.Maybe Lude.Text,
    -- | A list of DB security group elements containing @DBSecurityGroup.Name@ and @DBSecurityGroup.Status@ subelements.
    dbSecurityGroups :: Lude.Maybe [DBSecurityGroupMembership],
    -- | Indicates if the DB instance has deletion protection enabled. The database can't be deleted when deletion protection is enabled. For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_DeleteInstance.html Deleting a DB Instance> .
    deletionProtection :: Lude.Maybe Lude.Bool,
    -- | Specifies whether the DB instance is encrypted.
    storageEncrypted :: Lude.Maybe Lude.Bool,
    -- | If the DB instance is a member of a DB cluster, contains the name of the DB cluster that the DB instance is a member of.
    dbClusterIdentifier :: Lude.Maybe Lude.Text,
    -- | Specifies the accessibility options for the DB instance.
    --
    -- When the DB instance is publicly accessible, its DNS endpoint resolves to the private IP address from within the DB instance's VPC, and to the public IP address from outside of the DB instance's VPC. Access to the DB instance is ultimately controlled by the security group it uses, and that public access is not permitted if the security group assigned to the DB instance doesn't permit it.
    -- When the DB instance isn't publicly accessible, it is an internal DB instance with a DNS name that resolves to a private IP address.
    -- For more information, see 'CreateDBInstance' .
    publiclyAccessible :: Lude.Maybe Lude.Bool,
    -- | Indicates that minor version patches are applied automatically.
    autoMinorVersionUpgrade :: Lude.Maybe Lude.Bool,
    -- | The Amazon Resource Name (ARN) for the DB instance.
    dbInstanceARN :: Lude.Maybe Lude.Text,
    -- | Contains the master username for the DB instance.
    masterUsername :: Lude.Maybe Lude.Text,
    -- | Contains one or more identifiers of the read replicas associated with this DB instance.
    readReplicaDBInstanceIdentifiers :: Lude.Maybe [Lude.Text],
    -- | True if mapping of AWS Identity and Access Management (IAM) accounts to database accounts is enabled, and otherwise false.
    --
    -- IAM database authentication can be enabled for the following database engines
    --
    --     * For MySQL 5.6, minor version 5.6.34 or higher
    --
    --
    --     * For MySQL 5.7, minor version 5.7.16 or higher
    --
    --
    --     * Aurora 5.6 or higher. To enable IAM database authentication for Aurora, see DBCluster Type.
    iamDatabaseAuthenticationEnabled :: Lude.Maybe Lude.Bool,
    -- | The ARN for the IAM role that permits RDS to send Enhanced Monitoring metrics to Amazon CloudWatch Logs.
    monitoringRoleARN :: Lude.Maybe Lude.Text,
    -- | Specifies the Provisioned IOPS (I/O operations per second) value.
    iops :: Lude.Maybe Lude.Int,
    -- | Provides the date and time the DB instance was created.
    instanceCreateTime :: Lude.Maybe Lude.DateTime,
    tagList :: Lude.Maybe [Tag],
    -- | Contains the identifier of the source DB instance if this DB instance is a read replica.
    readReplicaSourceDBInstanceIdentifier :: Lude.Maybe Lude.Text,
    -- | The open mode of an Oracle read replica. The default is @open-read-only@ . For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/oracle-read-replicas.html Working with Oracle Read Replicas for Amazon RDS> in the /Amazon RDS User Guide/ .
    replicaMode :: Lude.Maybe ReplicaMode,
    -- | The interval, in seconds, between points when Enhanced Monitoring metrics are collected for the DB instance.
    monitoringInterval :: Lude.Maybe Lude.Int,
    -- | The name of the database engine to be used for this DB instance.
    engine :: Lude.Maybe Lude.Text,
    -- | The number of CPU cores and the number of threads per core for the DB instance class of the DB instance.
    processorFeatures :: Lude.Maybe [ProcessorFeature],
    -- | Specifies the latest time to which a database can be restored with point-in-time restore.
    latestRestorableTime :: Lude.Maybe Lude.DateTime,
    -- | Contains the name of the compute and memory capacity class of the DB instance.
    dbInstanceClass :: Lude.Maybe Lude.Text,
    -- | A value that specifies the order in which an Aurora Replica is promoted to the primary instance after a failure of the existing primary instance. For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/Aurora.Managing.Backups.html#Aurora.Managing.FaultTolerance Fault Tolerance for an Aurora DB Cluster> in the /Amazon Aurora User Guide/ .
    promotionTier :: Lude.Maybe Lude.Int,
    -- | License model information for this DB instance.
    licenseModel :: Lude.Maybe Lude.Text,
    -- | Specifies the weekly time range during which system maintenance can occur, in Universal Coordinated Time (UTC).
    preferredMaintenanceWindow :: Lude.Maybe Lude.Text,
    -- | The amount of time, in days, to retain Performance Insights data. Valid values are 7 or 731 (2 years).
    performanceInsightsRetentionPeriod :: Lude.Maybe Lude.Int,
    -- | The identifier of the CA certificate for this DB instance.
    cACertificateIdentifier :: Lude.Maybe Lude.Text,
    -- | Contains a user-supplied database identifier. This identifier is the unique key that identifies a DB instance.
    dbInstanceIdentifier :: Lude.Maybe Lude.Text,
    -- | If present, specifies the name of the character set that this instance is associated with.
    characterSetName :: Lude.Maybe Lude.Text,
    -- | The upper limit to which Amazon RDS can automatically scale the storage of the DB instance.
    maxAllocatedStorage :: Lude.Maybe Lude.Int,
    -- | If @StorageEncrypted@ is true, the AWS KMS key identifier for the encrypted DB instance.
    kmsKeyId :: Lude.Maybe Lude.Text,
    -- | Specifies the daily time range during which automated backups are created if automated backups are enabled, as determined by the @BackupRetentionPeriod@ .
    preferredBackupWindow :: Lude.Maybe Lude.Text,
    -- | The AWS Identity and Access Management (IAM) roles associated with the DB instance.
    associatedRoles :: Lude.Maybe [DBInstanceRole],
    -- | Specifies the name of the Availability Zone the DB instance is located in.
    availabilityZone :: Lude.Maybe Lude.Text,
    -- | Provides a list of VPC security group elements that the DB instance belongs to.
    vpcSecurityGroups :: Lude.Maybe [VPCSecurityGroupMembership],
    -- | Specifies the number of days for which automatic DB snapshots are retained.
    backupRetentionPeriod :: Lude.Maybe Lude.Int,
    -- | The name of the NCHAR character set for the Oracle DB instance. This character set specifies the Unicode encoding for data stored in table columns of type NCHAR, NCLOB, or NVARCHAR2.
    ncharCharacterSetName :: Lude.Maybe Lude.Text,
    -- | The AWS KMS key identifier for encryption of Performance Insights data. The KMS key ID is the Amazon Resource Name (ARN), KMS key identifier, or the KMS key alias for the KMS encryption key.
    performanceInsightsKMSKeyId :: Lude.Maybe Lude.Text,
    -- | Specifies information on the subnet group associated with the DB instance, including the name, description, and subnets in the subnet group.
    dbSubnetGroup :: Lude.Maybe DBSubnetGroup,
    -- | Specifies if the DB instance is a Multi-AZ deployment.
    multiAZ :: Lude.Maybe Lude.Bool,
    -- | Specifies the listener connection endpoint for SQL Server Always On.
    listenerEndpoint :: Lude.Maybe Endpoint,
    -- | Provides the list of option group memberships for this DB instance.
    optionGroupMemberships :: Lude.Maybe [OptionGroupMembership],
    -- | A list of log types that this DB instance is configured to export to CloudWatch Logs.
    --
    -- Log types vary by DB engine. For information about the log types for each DB engine, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_LogAccess.html Amazon RDS Database Log Files> in the /Amazon RDS User Guide./
    enabledCloudwatchLogsExports :: Lude.Maybe [Lude.Text],
    -- | The Amazon Resource Name (ARN) of the Amazon CloudWatch Logs log stream that receives the Enhanced Monitoring metrics data for the DB instance.
    enhancedMonitoringResourceARN :: Lude.Maybe Lude.Text,
    -- | If present, specifies the name of the secondary Availability Zone for a DB instance with multi-AZ support.
    secondaryAvailabilityZone :: Lude.Maybe Lude.Text,
    -- | True if Performance Insights is enabled for the DB instance, and otherwise false.
    performanceInsightsEnabled :: Lude.Maybe Lude.Bool,
    -- | Specifies the allocated storage size specified in gibibytes.
    allocatedStorage :: Lude.Maybe Lude.Int,
    -- | The AWS Region-unique, immutable identifier for the DB instance. This identifier is found in AWS CloudTrail log entries whenever the AWS KMS key for the DB instance is accessed.
    dbiResourceId :: Lude.Maybe Lude.Text,
    -- | Provides the list of DB parameter groups applied to this DB instance.
    dbParameterGroups :: Lude.Maybe [DBParameterGroupStatus],
    -- | Specifies whether tags are copied from the DB instance to snapshots of the DB instance.
    --
    -- __Amazon Aurora__
    -- Not applicable. Copying tags to snapshots is managed by the DB cluster. Setting this value for an Aurora DB instance has no effect on the DB cluster setting. For more information, see @DBCluster@ .
    copyTagsToSnapshot :: Lude.Maybe Lude.Bool,
    -- | The time zone of the DB instance. In most cases, the @Timezone@ element is empty. @Timezone@ content appears only for Microsoft SQL Server DB instances that were created with a time zone specified.
    timezone :: Lude.Maybe Lude.Text,
    -- | The ARN from the key store with which the instance is associated for TDE encryption.
    tdeCredentialARN :: Lude.Maybe Lude.Text,
    -- | Specifies the connection endpoint.
    endpoint :: Lude.Maybe Endpoint,
    -- | Specifies the current state of this database.
    --
    -- For information about DB instance statuses, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Overview.DBInstance.Status.html DB Instance Status> in the /Amazon RDS User Guide./
    dbInstanceStatus :: Lude.Maybe Lude.Text,
    -- | Specifies the port that the DB instance listens on. If the DB instance is part of a DB cluster, this can be a different port than the DB cluster port.
    dbInstancePort :: Lude.Maybe Lude.Int,
    -- | Specifies that changes to the DB instance are pending. This element is only included when changes are pending. Specific changes are identified by subelements.
    pendingModifiedValues :: Lude.Maybe PendingModifiedValues,
    -- | Contains one or more identifiers of Aurora DB clusters to which the RDS DB instance is replicated as a read replica. For example, when you create an Aurora read replica of an RDS MySQL DB instance, the Aurora MySQL DB cluster for the Aurora read replica is shown. This output does not contain information about cross region Aurora read replicas.
    readReplicaDBClusterIdentifiers :: Lude.Maybe [Lude.Text],
    -- | Specifies the storage type associated with DB instance.
    storageType :: Lude.Maybe Lude.Text,
    -- | The status of a read replica. If the instance isn't a read replica, this is blank.
    statusInfos :: Lude.Maybe [DBInstanceStatusInfo],
    -- | The Active Directory Domain membership records associated with the DB instance.
    domainMemberships :: Lude.Maybe [DomainMembership],
    -- | The meaning of this parameter differs according to the database engine you use.
    --
    -- __MySQL, MariaDB, SQL Server, PostgreSQL__
    -- Contains the name of the initial database of this instance that was provided at create time, if one was specified when the DB instance was created. This same name is returned for the life of the DB instance.
    -- Type: String
    -- __Oracle__
    -- Contains the Oracle System ID (SID) of the created DB instance. Not shown when the returned parameters do not apply to an Oracle DB instance.
    dbName :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DBInstance' with the minimum fields required to make a request.
--
-- * 'engineVersion' - Indicates the database engine version.
-- * 'dbSecurityGroups' - A list of DB security group elements containing @DBSecurityGroup.Name@ and @DBSecurityGroup.Status@ subelements.
-- * 'deletionProtection' - Indicates if the DB instance has deletion protection enabled. The database can't be deleted when deletion protection is enabled. For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_DeleteInstance.html Deleting a DB Instance> .
-- * 'storageEncrypted' - Specifies whether the DB instance is encrypted.
-- * 'dbClusterIdentifier' - If the DB instance is a member of a DB cluster, contains the name of the DB cluster that the DB instance is a member of.
-- * 'publiclyAccessible' - Specifies the accessibility options for the DB instance.
--
-- When the DB instance is publicly accessible, its DNS endpoint resolves to the private IP address from within the DB instance's VPC, and to the public IP address from outside of the DB instance's VPC. Access to the DB instance is ultimately controlled by the security group it uses, and that public access is not permitted if the security group assigned to the DB instance doesn't permit it.
-- When the DB instance isn't publicly accessible, it is an internal DB instance with a DNS name that resolves to a private IP address.
-- For more information, see 'CreateDBInstance' .
-- * 'autoMinorVersionUpgrade' - Indicates that minor version patches are applied automatically.
-- * 'dbInstanceARN' - The Amazon Resource Name (ARN) for the DB instance.
-- * 'masterUsername' - Contains the master username for the DB instance.
-- * 'readReplicaDBInstanceIdentifiers' - Contains one or more identifiers of the read replicas associated with this DB instance.
-- * 'iamDatabaseAuthenticationEnabled' - True if mapping of AWS Identity and Access Management (IAM) accounts to database accounts is enabled, and otherwise false.
--
-- IAM database authentication can be enabled for the following database engines
--
--     * For MySQL 5.6, minor version 5.6.34 or higher
--
--
--     * For MySQL 5.7, minor version 5.7.16 or higher
--
--
--     * Aurora 5.6 or higher. To enable IAM database authentication for Aurora, see DBCluster Type.
--
--
-- * 'monitoringRoleARN' - The ARN for the IAM role that permits RDS to send Enhanced Monitoring metrics to Amazon CloudWatch Logs.
-- * 'iops' - Specifies the Provisioned IOPS (I/O operations per second) value.
-- * 'instanceCreateTime' - Provides the date and time the DB instance was created.
-- * 'tagList' -
-- * 'readReplicaSourceDBInstanceIdentifier' - Contains the identifier of the source DB instance if this DB instance is a read replica.
-- * 'replicaMode' - The open mode of an Oracle read replica. The default is @open-read-only@ . For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/oracle-read-replicas.html Working with Oracle Read Replicas for Amazon RDS> in the /Amazon RDS User Guide/ .
-- * 'monitoringInterval' - The interval, in seconds, between points when Enhanced Monitoring metrics are collected for the DB instance.
-- * 'engine' - The name of the database engine to be used for this DB instance.
-- * 'processorFeatures' - The number of CPU cores and the number of threads per core for the DB instance class of the DB instance.
-- * 'latestRestorableTime' - Specifies the latest time to which a database can be restored with point-in-time restore.
-- * 'dbInstanceClass' - Contains the name of the compute and memory capacity class of the DB instance.
-- * 'promotionTier' - A value that specifies the order in which an Aurora Replica is promoted to the primary instance after a failure of the existing primary instance. For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/Aurora.Managing.Backups.html#Aurora.Managing.FaultTolerance Fault Tolerance for an Aurora DB Cluster> in the /Amazon Aurora User Guide/ .
-- * 'licenseModel' - License model information for this DB instance.
-- * 'preferredMaintenanceWindow' - Specifies the weekly time range during which system maintenance can occur, in Universal Coordinated Time (UTC).
-- * 'performanceInsightsRetentionPeriod' - The amount of time, in days, to retain Performance Insights data. Valid values are 7 or 731 (2 years).
-- * 'cACertificateIdentifier' - The identifier of the CA certificate for this DB instance.
-- * 'dbInstanceIdentifier' - Contains a user-supplied database identifier. This identifier is the unique key that identifies a DB instance.
-- * 'characterSetName' - If present, specifies the name of the character set that this instance is associated with.
-- * 'maxAllocatedStorage' - The upper limit to which Amazon RDS can automatically scale the storage of the DB instance.
-- * 'kmsKeyId' - If @StorageEncrypted@ is true, the AWS KMS key identifier for the encrypted DB instance.
-- * 'preferredBackupWindow' - Specifies the daily time range during which automated backups are created if automated backups are enabled, as determined by the @BackupRetentionPeriod@ .
-- * 'associatedRoles' - The AWS Identity and Access Management (IAM) roles associated with the DB instance.
-- * 'availabilityZone' - Specifies the name of the Availability Zone the DB instance is located in.
-- * 'vpcSecurityGroups' - Provides a list of VPC security group elements that the DB instance belongs to.
-- * 'backupRetentionPeriod' - Specifies the number of days for which automatic DB snapshots are retained.
-- * 'ncharCharacterSetName' - The name of the NCHAR character set for the Oracle DB instance. This character set specifies the Unicode encoding for data stored in table columns of type NCHAR, NCLOB, or NVARCHAR2.
-- * 'performanceInsightsKMSKeyId' - The AWS KMS key identifier for encryption of Performance Insights data. The KMS key ID is the Amazon Resource Name (ARN), KMS key identifier, or the KMS key alias for the KMS encryption key.
-- * 'dbSubnetGroup' - Specifies information on the subnet group associated with the DB instance, including the name, description, and subnets in the subnet group.
-- * 'multiAZ' - Specifies if the DB instance is a Multi-AZ deployment.
-- * 'listenerEndpoint' - Specifies the listener connection endpoint for SQL Server Always On.
-- * 'optionGroupMemberships' - Provides the list of option group memberships for this DB instance.
-- * 'enabledCloudwatchLogsExports' - A list of log types that this DB instance is configured to export to CloudWatch Logs.
--
-- Log types vary by DB engine. For information about the log types for each DB engine, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_LogAccess.html Amazon RDS Database Log Files> in the /Amazon RDS User Guide./
-- * 'enhancedMonitoringResourceARN' - The Amazon Resource Name (ARN) of the Amazon CloudWatch Logs log stream that receives the Enhanced Monitoring metrics data for the DB instance.
-- * 'secondaryAvailabilityZone' - If present, specifies the name of the secondary Availability Zone for a DB instance with multi-AZ support.
-- * 'performanceInsightsEnabled' - True if Performance Insights is enabled for the DB instance, and otherwise false.
-- * 'allocatedStorage' - Specifies the allocated storage size specified in gibibytes.
-- * 'dbiResourceId' - The AWS Region-unique, immutable identifier for the DB instance. This identifier is found in AWS CloudTrail log entries whenever the AWS KMS key for the DB instance is accessed.
-- * 'dbParameterGroups' - Provides the list of DB parameter groups applied to this DB instance.
-- * 'copyTagsToSnapshot' - Specifies whether tags are copied from the DB instance to snapshots of the DB instance.
--
-- __Amazon Aurora__
-- Not applicable. Copying tags to snapshots is managed by the DB cluster. Setting this value for an Aurora DB instance has no effect on the DB cluster setting. For more information, see @DBCluster@ .
-- * 'timezone' - The time zone of the DB instance. In most cases, the @Timezone@ element is empty. @Timezone@ content appears only for Microsoft SQL Server DB instances that were created with a time zone specified.
-- * 'tdeCredentialARN' - The ARN from the key store with which the instance is associated for TDE encryption.
-- * 'endpoint' - Specifies the connection endpoint.
-- * 'dbInstanceStatus' - Specifies the current state of this database.
--
-- For information about DB instance statuses, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Overview.DBInstance.Status.html DB Instance Status> in the /Amazon RDS User Guide./
-- * 'dbInstancePort' - Specifies the port that the DB instance listens on. If the DB instance is part of a DB cluster, this can be a different port than the DB cluster port.
-- * 'pendingModifiedValues' - Specifies that changes to the DB instance are pending. This element is only included when changes are pending. Specific changes are identified by subelements.
-- * 'readReplicaDBClusterIdentifiers' - Contains one or more identifiers of Aurora DB clusters to which the RDS DB instance is replicated as a read replica. For example, when you create an Aurora read replica of an RDS MySQL DB instance, the Aurora MySQL DB cluster for the Aurora read replica is shown. This output does not contain information about cross region Aurora read replicas.
-- * 'storageType' - Specifies the storage type associated with DB instance.
-- * 'statusInfos' - The status of a read replica. If the instance isn't a read replica, this is blank.
-- * 'domainMemberships' - The Active Directory Domain membership records associated with the DB instance.
-- * 'dbName' - The meaning of this parameter differs according to the database engine you use.
--
-- __MySQL, MariaDB, SQL Server, PostgreSQL__
-- Contains the name of the initial database of this instance that was provided at create time, if one was specified when the DB instance was created. This same name is returned for the life of the DB instance.
-- Type: String
-- __Oracle__
-- Contains the Oracle System ID (SID) of the created DB instance. Not shown when the returned parameters do not apply to an Oracle DB instance.
mkDBInstance ::
  DBInstance
mkDBInstance =
  DBInstance'
    { engineVersion = Lude.Nothing,
      dbSecurityGroups = Lude.Nothing,
      deletionProtection = Lude.Nothing,
      storageEncrypted = Lude.Nothing,
      dbClusterIdentifier = Lude.Nothing,
      publiclyAccessible = Lude.Nothing,
      autoMinorVersionUpgrade = Lude.Nothing,
      dbInstanceARN = Lude.Nothing,
      masterUsername = Lude.Nothing,
      readReplicaDBInstanceIdentifiers = Lude.Nothing,
      iamDatabaseAuthenticationEnabled = Lude.Nothing,
      monitoringRoleARN = Lude.Nothing,
      iops = Lude.Nothing,
      instanceCreateTime = Lude.Nothing,
      tagList = Lude.Nothing,
      readReplicaSourceDBInstanceIdentifier = Lude.Nothing,
      replicaMode = Lude.Nothing,
      monitoringInterval = Lude.Nothing,
      engine = Lude.Nothing,
      processorFeatures = Lude.Nothing,
      latestRestorableTime = Lude.Nothing,
      dbInstanceClass = Lude.Nothing,
      promotionTier = Lude.Nothing,
      licenseModel = Lude.Nothing,
      preferredMaintenanceWindow = Lude.Nothing,
      performanceInsightsRetentionPeriod = Lude.Nothing,
      cACertificateIdentifier = Lude.Nothing,
      dbInstanceIdentifier = Lude.Nothing,
      characterSetName = Lude.Nothing,
      maxAllocatedStorage = Lude.Nothing,
      kmsKeyId = Lude.Nothing,
      preferredBackupWindow = Lude.Nothing,
      associatedRoles = Lude.Nothing,
      availabilityZone = Lude.Nothing,
      vpcSecurityGroups = Lude.Nothing,
      backupRetentionPeriod = Lude.Nothing,
      ncharCharacterSetName = Lude.Nothing,
      performanceInsightsKMSKeyId = Lude.Nothing,
      dbSubnetGroup = Lude.Nothing,
      multiAZ = Lude.Nothing,
      listenerEndpoint = Lude.Nothing,
      optionGroupMemberships = Lude.Nothing,
      enabledCloudwatchLogsExports = Lude.Nothing,
      enhancedMonitoringResourceARN = Lude.Nothing,
      secondaryAvailabilityZone = Lude.Nothing,
      performanceInsightsEnabled = Lude.Nothing,
      allocatedStorage = Lude.Nothing,
      dbiResourceId = Lude.Nothing,
      dbParameterGroups = Lude.Nothing,
      copyTagsToSnapshot = Lude.Nothing,
      timezone = Lude.Nothing,
      tdeCredentialARN = Lude.Nothing,
      endpoint = Lude.Nothing,
      dbInstanceStatus = Lude.Nothing,
      dbInstancePort = Lude.Nothing,
      pendingModifiedValues = Lude.Nothing,
      readReplicaDBClusterIdentifiers = Lude.Nothing,
      storageType = Lude.Nothing,
      statusInfos = Lude.Nothing,
      domainMemberships = Lude.Nothing,
      dbName = Lude.Nothing
    }

-- | Indicates the database engine version.
--
-- /Note:/ Consider using 'engineVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diEngineVersion :: Lens.Lens' DBInstance (Lude.Maybe Lude.Text)
diEngineVersion = Lens.lens (engineVersion :: DBInstance -> Lude.Maybe Lude.Text) (\s a -> s {engineVersion = a} :: DBInstance)
{-# DEPRECATED diEngineVersion "Use generic-lens or generic-optics with 'engineVersion' instead." #-}

-- | A list of DB security group elements containing @DBSecurityGroup.Name@ and @DBSecurityGroup.Status@ subelements.
--
-- /Note:/ Consider using 'dbSecurityGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diDBSecurityGroups :: Lens.Lens' DBInstance (Lude.Maybe [DBSecurityGroupMembership])
diDBSecurityGroups = Lens.lens (dbSecurityGroups :: DBInstance -> Lude.Maybe [DBSecurityGroupMembership]) (\s a -> s {dbSecurityGroups = a} :: DBInstance)
{-# DEPRECATED diDBSecurityGroups "Use generic-lens or generic-optics with 'dbSecurityGroups' instead." #-}

-- | Indicates if the DB instance has deletion protection enabled. The database can't be deleted when deletion protection is enabled. For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_DeleteInstance.html Deleting a DB Instance> .
--
-- /Note:/ Consider using 'deletionProtection' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diDeletionProtection :: Lens.Lens' DBInstance (Lude.Maybe Lude.Bool)
diDeletionProtection = Lens.lens (deletionProtection :: DBInstance -> Lude.Maybe Lude.Bool) (\s a -> s {deletionProtection = a} :: DBInstance)
{-# DEPRECATED diDeletionProtection "Use generic-lens or generic-optics with 'deletionProtection' instead." #-}

-- | Specifies whether the DB instance is encrypted.
--
-- /Note:/ Consider using 'storageEncrypted' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diStorageEncrypted :: Lens.Lens' DBInstance (Lude.Maybe Lude.Bool)
diStorageEncrypted = Lens.lens (storageEncrypted :: DBInstance -> Lude.Maybe Lude.Bool) (\s a -> s {storageEncrypted = a} :: DBInstance)
{-# DEPRECATED diStorageEncrypted "Use generic-lens or generic-optics with 'storageEncrypted' instead." #-}

-- | If the DB instance is a member of a DB cluster, contains the name of the DB cluster that the DB instance is a member of.
--
-- /Note:/ Consider using 'dbClusterIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diDBClusterIdentifier :: Lens.Lens' DBInstance (Lude.Maybe Lude.Text)
diDBClusterIdentifier = Lens.lens (dbClusterIdentifier :: DBInstance -> Lude.Maybe Lude.Text) (\s a -> s {dbClusterIdentifier = a} :: DBInstance)
{-# DEPRECATED diDBClusterIdentifier "Use generic-lens or generic-optics with 'dbClusterIdentifier' instead." #-}

-- | Specifies the accessibility options for the DB instance.
--
-- When the DB instance is publicly accessible, its DNS endpoint resolves to the private IP address from within the DB instance's VPC, and to the public IP address from outside of the DB instance's VPC. Access to the DB instance is ultimately controlled by the security group it uses, and that public access is not permitted if the security group assigned to the DB instance doesn't permit it.
-- When the DB instance isn't publicly accessible, it is an internal DB instance with a DNS name that resolves to a private IP address.
-- For more information, see 'CreateDBInstance' .
--
-- /Note:/ Consider using 'publiclyAccessible' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diPubliclyAccessible :: Lens.Lens' DBInstance (Lude.Maybe Lude.Bool)
diPubliclyAccessible = Lens.lens (publiclyAccessible :: DBInstance -> Lude.Maybe Lude.Bool) (\s a -> s {publiclyAccessible = a} :: DBInstance)
{-# DEPRECATED diPubliclyAccessible "Use generic-lens or generic-optics with 'publiclyAccessible' instead." #-}

-- | Indicates that minor version patches are applied automatically.
--
-- /Note:/ Consider using 'autoMinorVersionUpgrade' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diAutoMinorVersionUpgrade :: Lens.Lens' DBInstance (Lude.Maybe Lude.Bool)
diAutoMinorVersionUpgrade = Lens.lens (autoMinorVersionUpgrade :: DBInstance -> Lude.Maybe Lude.Bool) (\s a -> s {autoMinorVersionUpgrade = a} :: DBInstance)
{-# DEPRECATED diAutoMinorVersionUpgrade "Use generic-lens or generic-optics with 'autoMinorVersionUpgrade' instead." #-}

-- | The Amazon Resource Name (ARN) for the DB instance.
--
-- /Note:/ Consider using 'dbInstanceARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diDBInstanceARN :: Lens.Lens' DBInstance (Lude.Maybe Lude.Text)
diDBInstanceARN = Lens.lens (dbInstanceARN :: DBInstance -> Lude.Maybe Lude.Text) (\s a -> s {dbInstanceARN = a} :: DBInstance)
{-# DEPRECATED diDBInstanceARN "Use generic-lens or generic-optics with 'dbInstanceARN' instead." #-}

-- | Contains the master username for the DB instance.
--
-- /Note:/ Consider using 'masterUsername' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diMasterUsername :: Lens.Lens' DBInstance (Lude.Maybe Lude.Text)
diMasterUsername = Lens.lens (masterUsername :: DBInstance -> Lude.Maybe Lude.Text) (\s a -> s {masterUsername = a} :: DBInstance)
{-# DEPRECATED diMasterUsername "Use generic-lens or generic-optics with 'masterUsername' instead." #-}

-- | Contains one or more identifiers of the read replicas associated with this DB instance.
--
-- /Note:/ Consider using 'readReplicaDBInstanceIdentifiers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diReadReplicaDBInstanceIdentifiers :: Lens.Lens' DBInstance (Lude.Maybe [Lude.Text])
diReadReplicaDBInstanceIdentifiers = Lens.lens (readReplicaDBInstanceIdentifiers :: DBInstance -> Lude.Maybe [Lude.Text]) (\s a -> s {readReplicaDBInstanceIdentifiers = a} :: DBInstance)
{-# DEPRECATED diReadReplicaDBInstanceIdentifiers "Use generic-lens or generic-optics with 'readReplicaDBInstanceIdentifiers' instead." #-}

-- | True if mapping of AWS Identity and Access Management (IAM) accounts to database accounts is enabled, and otherwise false.
--
-- IAM database authentication can be enabled for the following database engines
--
--     * For MySQL 5.6, minor version 5.6.34 or higher
--
--
--     * For MySQL 5.7, minor version 5.7.16 or higher
--
--
--     * Aurora 5.6 or higher. To enable IAM database authentication for Aurora, see DBCluster Type.
--
--
--
-- /Note:/ Consider using 'iamDatabaseAuthenticationEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diIAMDatabaseAuthenticationEnabled :: Lens.Lens' DBInstance (Lude.Maybe Lude.Bool)
diIAMDatabaseAuthenticationEnabled = Lens.lens (iamDatabaseAuthenticationEnabled :: DBInstance -> Lude.Maybe Lude.Bool) (\s a -> s {iamDatabaseAuthenticationEnabled = a} :: DBInstance)
{-# DEPRECATED diIAMDatabaseAuthenticationEnabled "Use generic-lens or generic-optics with 'iamDatabaseAuthenticationEnabled' instead." #-}

-- | The ARN for the IAM role that permits RDS to send Enhanced Monitoring metrics to Amazon CloudWatch Logs.
--
-- /Note:/ Consider using 'monitoringRoleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diMonitoringRoleARN :: Lens.Lens' DBInstance (Lude.Maybe Lude.Text)
diMonitoringRoleARN = Lens.lens (monitoringRoleARN :: DBInstance -> Lude.Maybe Lude.Text) (\s a -> s {monitoringRoleARN = a} :: DBInstance)
{-# DEPRECATED diMonitoringRoleARN "Use generic-lens or generic-optics with 'monitoringRoleARN' instead." #-}

-- | Specifies the Provisioned IOPS (I/O operations per second) value.
--
-- /Note:/ Consider using 'iops' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diIOPS :: Lens.Lens' DBInstance (Lude.Maybe Lude.Int)
diIOPS = Lens.lens (iops :: DBInstance -> Lude.Maybe Lude.Int) (\s a -> s {iops = a} :: DBInstance)
{-# DEPRECATED diIOPS "Use generic-lens or generic-optics with 'iops' instead." #-}

-- | Provides the date and time the DB instance was created.
--
-- /Note:/ Consider using 'instanceCreateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diInstanceCreateTime :: Lens.Lens' DBInstance (Lude.Maybe Lude.DateTime)
diInstanceCreateTime = Lens.lens (instanceCreateTime :: DBInstance -> Lude.Maybe Lude.DateTime) (\s a -> s {instanceCreateTime = a} :: DBInstance)
{-# DEPRECATED diInstanceCreateTime "Use generic-lens or generic-optics with 'instanceCreateTime' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'tagList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diTagList :: Lens.Lens' DBInstance (Lude.Maybe [Tag])
diTagList = Lens.lens (tagList :: DBInstance -> Lude.Maybe [Tag]) (\s a -> s {tagList = a} :: DBInstance)
{-# DEPRECATED diTagList "Use generic-lens or generic-optics with 'tagList' instead." #-}

-- | Contains the identifier of the source DB instance if this DB instance is a read replica.
--
-- /Note:/ Consider using 'readReplicaSourceDBInstanceIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diReadReplicaSourceDBInstanceIdentifier :: Lens.Lens' DBInstance (Lude.Maybe Lude.Text)
diReadReplicaSourceDBInstanceIdentifier = Lens.lens (readReplicaSourceDBInstanceIdentifier :: DBInstance -> Lude.Maybe Lude.Text) (\s a -> s {readReplicaSourceDBInstanceIdentifier = a} :: DBInstance)
{-# DEPRECATED diReadReplicaSourceDBInstanceIdentifier "Use generic-lens or generic-optics with 'readReplicaSourceDBInstanceIdentifier' instead." #-}

-- | The open mode of an Oracle read replica. The default is @open-read-only@ . For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/oracle-read-replicas.html Working with Oracle Read Replicas for Amazon RDS> in the /Amazon RDS User Guide/ .
--
-- /Note:/ Consider using 'replicaMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diReplicaMode :: Lens.Lens' DBInstance (Lude.Maybe ReplicaMode)
diReplicaMode = Lens.lens (replicaMode :: DBInstance -> Lude.Maybe ReplicaMode) (\s a -> s {replicaMode = a} :: DBInstance)
{-# DEPRECATED diReplicaMode "Use generic-lens or generic-optics with 'replicaMode' instead." #-}

-- | The interval, in seconds, between points when Enhanced Monitoring metrics are collected for the DB instance.
--
-- /Note:/ Consider using 'monitoringInterval' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diMonitoringInterval :: Lens.Lens' DBInstance (Lude.Maybe Lude.Int)
diMonitoringInterval = Lens.lens (monitoringInterval :: DBInstance -> Lude.Maybe Lude.Int) (\s a -> s {monitoringInterval = a} :: DBInstance)
{-# DEPRECATED diMonitoringInterval "Use generic-lens or generic-optics with 'monitoringInterval' instead." #-}

-- | The name of the database engine to be used for this DB instance.
--
-- /Note:/ Consider using 'engine' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diEngine :: Lens.Lens' DBInstance (Lude.Maybe Lude.Text)
diEngine = Lens.lens (engine :: DBInstance -> Lude.Maybe Lude.Text) (\s a -> s {engine = a} :: DBInstance)
{-# DEPRECATED diEngine "Use generic-lens or generic-optics with 'engine' instead." #-}

-- | The number of CPU cores and the number of threads per core for the DB instance class of the DB instance.
--
-- /Note:/ Consider using 'processorFeatures' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diProcessorFeatures :: Lens.Lens' DBInstance (Lude.Maybe [ProcessorFeature])
diProcessorFeatures = Lens.lens (processorFeatures :: DBInstance -> Lude.Maybe [ProcessorFeature]) (\s a -> s {processorFeatures = a} :: DBInstance)
{-# DEPRECATED diProcessorFeatures "Use generic-lens or generic-optics with 'processorFeatures' instead." #-}

-- | Specifies the latest time to which a database can be restored with point-in-time restore.
--
-- /Note:/ Consider using 'latestRestorableTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diLatestRestorableTime :: Lens.Lens' DBInstance (Lude.Maybe Lude.DateTime)
diLatestRestorableTime = Lens.lens (latestRestorableTime :: DBInstance -> Lude.Maybe Lude.DateTime) (\s a -> s {latestRestorableTime = a} :: DBInstance)
{-# DEPRECATED diLatestRestorableTime "Use generic-lens or generic-optics with 'latestRestorableTime' instead." #-}

-- | Contains the name of the compute and memory capacity class of the DB instance.
--
-- /Note:/ Consider using 'dbInstanceClass' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diDBInstanceClass :: Lens.Lens' DBInstance (Lude.Maybe Lude.Text)
diDBInstanceClass = Lens.lens (dbInstanceClass :: DBInstance -> Lude.Maybe Lude.Text) (\s a -> s {dbInstanceClass = a} :: DBInstance)
{-# DEPRECATED diDBInstanceClass "Use generic-lens or generic-optics with 'dbInstanceClass' instead." #-}

-- | A value that specifies the order in which an Aurora Replica is promoted to the primary instance after a failure of the existing primary instance. For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/Aurora.Managing.Backups.html#Aurora.Managing.FaultTolerance Fault Tolerance for an Aurora DB Cluster> in the /Amazon Aurora User Guide/ .
--
-- /Note:/ Consider using 'promotionTier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diPromotionTier :: Lens.Lens' DBInstance (Lude.Maybe Lude.Int)
diPromotionTier = Lens.lens (promotionTier :: DBInstance -> Lude.Maybe Lude.Int) (\s a -> s {promotionTier = a} :: DBInstance)
{-# DEPRECATED diPromotionTier "Use generic-lens or generic-optics with 'promotionTier' instead." #-}

-- | License model information for this DB instance.
--
-- /Note:/ Consider using 'licenseModel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diLicenseModel :: Lens.Lens' DBInstance (Lude.Maybe Lude.Text)
diLicenseModel = Lens.lens (licenseModel :: DBInstance -> Lude.Maybe Lude.Text) (\s a -> s {licenseModel = a} :: DBInstance)
{-# DEPRECATED diLicenseModel "Use generic-lens or generic-optics with 'licenseModel' instead." #-}

-- | Specifies the weekly time range during which system maintenance can occur, in Universal Coordinated Time (UTC).
--
-- /Note:/ Consider using 'preferredMaintenanceWindow' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diPreferredMaintenanceWindow :: Lens.Lens' DBInstance (Lude.Maybe Lude.Text)
diPreferredMaintenanceWindow = Lens.lens (preferredMaintenanceWindow :: DBInstance -> Lude.Maybe Lude.Text) (\s a -> s {preferredMaintenanceWindow = a} :: DBInstance)
{-# DEPRECATED diPreferredMaintenanceWindow "Use generic-lens or generic-optics with 'preferredMaintenanceWindow' instead." #-}

-- | The amount of time, in days, to retain Performance Insights data. Valid values are 7 or 731 (2 years).
--
-- /Note:/ Consider using 'performanceInsightsRetentionPeriod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diPerformanceInsightsRetentionPeriod :: Lens.Lens' DBInstance (Lude.Maybe Lude.Int)
diPerformanceInsightsRetentionPeriod = Lens.lens (performanceInsightsRetentionPeriod :: DBInstance -> Lude.Maybe Lude.Int) (\s a -> s {performanceInsightsRetentionPeriod = a} :: DBInstance)
{-# DEPRECATED diPerformanceInsightsRetentionPeriod "Use generic-lens or generic-optics with 'performanceInsightsRetentionPeriod' instead." #-}

-- | The identifier of the CA certificate for this DB instance.
--
-- /Note:/ Consider using 'cACertificateIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diCACertificateIdentifier :: Lens.Lens' DBInstance (Lude.Maybe Lude.Text)
diCACertificateIdentifier = Lens.lens (cACertificateIdentifier :: DBInstance -> Lude.Maybe Lude.Text) (\s a -> s {cACertificateIdentifier = a} :: DBInstance)
{-# DEPRECATED diCACertificateIdentifier "Use generic-lens or generic-optics with 'cACertificateIdentifier' instead." #-}

-- | Contains a user-supplied database identifier. This identifier is the unique key that identifies a DB instance.
--
-- /Note:/ Consider using 'dbInstanceIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diDBInstanceIdentifier :: Lens.Lens' DBInstance (Lude.Maybe Lude.Text)
diDBInstanceIdentifier = Lens.lens (dbInstanceIdentifier :: DBInstance -> Lude.Maybe Lude.Text) (\s a -> s {dbInstanceIdentifier = a} :: DBInstance)
{-# DEPRECATED diDBInstanceIdentifier "Use generic-lens or generic-optics with 'dbInstanceIdentifier' instead." #-}

-- | If present, specifies the name of the character set that this instance is associated with.
--
-- /Note:/ Consider using 'characterSetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diCharacterSetName :: Lens.Lens' DBInstance (Lude.Maybe Lude.Text)
diCharacterSetName = Lens.lens (characterSetName :: DBInstance -> Lude.Maybe Lude.Text) (\s a -> s {characterSetName = a} :: DBInstance)
{-# DEPRECATED diCharacterSetName "Use generic-lens or generic-optics with 'characterSetName' instead." #-}

-- | The upper limit to which Amazon RDS can automatically scale the storage of the DB instance.
--
-- /Note:/ Consider using 'maxAllocatedStorage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diMaxAllocatedStorage :: Lens.Lens' DBInstance (Lude.Maybe Lude.Int)
diMaxAllocatedStorage = Lens.lens (maxAllocatedStorage :: DBInstance -> Lude.Maybe Lude.Int) (\s a -> s {maxAllocatedStorage = a} :: DBInstance)
{-# DEPRECATED diMaxAllocatedStorage "Use generic-lens or generic-optics with 'maxAllocatedStorage' instead." #-}

-- | If @StorageEncrypted@ is true, the AWS KMS key identifier for the encrypted DB instance.
--
-- /Note:/ Consider using 'kmsKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diKMSKeyId :: Lens.Lens' DBInstance (Lude.Maybe Lude.Text)
diKMSKeyId = Lens.lens (kmsKeyId :: DBInstance -> Lude.Maybe Lude.Text) (\s a -> s {kmsKeyId = a} :: DBInstance)
{-# DEPRECATED diKMSKeyId "Use generic-lens or generic-optics with 'kmsKeyId' instead." #-}

-- | Specifies the daily time range during which automated backups are created if automated backups are enabled, as determined by the @BackupRetentionPeriod@ .
--
-- /Note:/ Consider using 'preferredBackupWindow' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diPreferredBackupWindow :: Lens.Lens' DBInstance (Lude.Maybe Lude.Text)
diPreferredBackupWindow = Lens.lens (preferredBackupWindow :: DBInstance -> Lude.Maybe Lude.Text) (\s a -> s {preferredBackupWindow = a} :: DBInstance)
{-# DEPRECATED diPreferredBackupWindow "Use generic-lens or generic-optics with 'preferredBackupWindow' instead." #-}

-- | The AWS Identity and Access Management (IAM) roles associated with the DB instance.
--
-- /Note:/ Consider using 'associatedRoles' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diAssociatedRoles :: Lens.Lens' DBInstance (Lude.Maybe [DBInstanceRole])
diAssociatedRoles = Lens.lens (associatedRoles :: DBInstance -> Lude.Maybe [DBInstanceRole]) (\s a -> s {associatedRoles = a} :: DBInstance)
{-# DEPRECATED diAssociatedRoles "Use generic-lens or generic-optics with 'associatedRoles' instead." #-}

-- | Specifies the name of the Availability Zone the DB instance is located in.
--
-- /Note:/ Consider using 'availabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diAvailabilityZone :: Lens.Lens' DBInstance (Lude.Maybe Lude.Text)
diAvailabilityZone = Lens.lens (availabilityZone :: DBInstance -> Lude.Maybe Lude.Text) (\s a -> s {availabilityZone = a} :: DBInstance)
{-# DEPRECATED diAvailabilityZone "Use generic-lens or generic-optics with 'availabilityZone' instead." #-}

-- | Provides a list of VPC security group elements that the DB instance belongs to.
--
-- /Note:/ Consider using 'vpcSecurityGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diVPCSecurityGroups :: Lens.Lens' DBInstance (Lude.Maybe [VPCSecurityGroupMembership])
diVPCSecurityGroups = Lens.lens (vpcSecurityGroups :: DBInstance -> Lude.Maybe [VPCSecurityGroupMembership]) (\s a -> s {vpcSecurityGroups = a} :: DBInstance)
{-# DEPRECATED diVPCSecurityGroups "Use generic-lens or generic-optics with 'vpcSecurityGroups' instead." #-}

-- | Specifies the number of days for which automatic DB snapshots are retained.
--
-- /Note:/ Consider using 'backupRetentionPeriod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diBackupRetentionPeriod :: Lens.Lens' DBInstance (Lude.Maybe Lude.Int)
diBackupRetentionPeriod = Lens.lens (backupRetentionPeriod :: DBInstance -> Lude.Maybe Lude.Int) (\s a -> s {backupRetentionPeriod = a} :: DBInstance)
{-# DEPRECATED diBackupRetentionPeriod "Use generic-lens or generic-optics with 'backupRetentionPeriod' instead." #-}

-- | The name of the NCHAR character set for the Oracle DB instance. This character set specifies the Unicode encoding for data stored in table columns of type NCHAR, NCLOB, or NVARCHAR2.
--
-- /Note:/ Consider using 'ncharCharacterSetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diNcharCharacterSetName :: Lens.Lens' DBInstance (Lude.Maybe Lude.Text)
diNcharCharacterSetName = Lens.lens (ncharCharacterSetName :: DBInstance -> Lude.Maybe Lude.Text) (\s a -> s {ncharCharacterSetName = a} :: DBInstance)
{-# DEPRECATED diNcharCharacterSetName "Use generic-lens or generic-optics with 'ncharCharacterSetName' instead." #-}

-- | The AWS KMS key identifier for encryption of Performance Insights data. The KMS key ID is the Amazon Resource Name (ARN), KMS key identifier, or the KMS key alias for the KMS encryption key.
--
-- /Note:/ Consider using 'performanceInsightsKMSKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diPerformanceInsightsKMSKeyId :: Lens.Lens' DBInstance (Lude.Maybe Lude.Text)
diPerformanceInsightsKMSKeyId = Lens.lens (performanceInsightsKMSKeyId :: DBInstance -> Lude.Maybe Lude.Text) (\s a -> s {performanceInsightsKMSKeyId = a} :: DBInstance)
{-# DEPRECATED diPerformanceInsightsKMSKeyId "Use generic-lens or generic-optics with 'performanceInsightsKMSKeyId' instead." #-}

-- | Specifies information on the subnet group associated with the DB instance, including the name, description, and subnets in the subnet group.
--
-- /Note:/ Consider using 'dbSubnetGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diDBSubnetGroup :: Lens.Lens' DBInstance (Lude.Maybe DBSubnetGroup)
diDBSubnetGroup = Lens.lens (dbSubnetGroup :: DBInstance -> Lude.Maybe DBSubnetGroup) (\s a -> s {dbSubnetGroup = a} :: DBInstance)
{-# DEPRECATED diDBSubnetGroup "Use generic-lens or generic-optics with 'dbSubnetGroup' instead." #-}

-- | Specifies if the DB instance is a Multi-AZ deployment.
--
-- /Note:/ Consider using 'multiAZ' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diMultiAZ :: Lens.Lens' DBInstance (Lude.Maybe Lude.Bool)
diMultiAZ = Lens.lens (multiAZ :: DBInstance -> Lude.Maybe Lude.Bool) (\s a -> s {multiAZ = a} :: DBInstance)
{-# DEPRECATED diMultiAZ "Use generic-lens or generic-optics with 'multiAZ' instead." #-}

-- | Specifies the listener connection endpoint for SQL Server Always On.
--
-- /Note:/ Consider using 'listenerEndpoint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diListenerEndpoint :: Lens.Lens' DBInstance (Lude.Maybe Endpoint)
diListenerEndpoint = Lens.lens (listenerEndpoint :: DBInstance -> Lude.Maybe Endpoint) (\s a -> s {listenerEndpoint = a} :: DBInstance)
{-# DEPRECATED diListenerEndpoint "Use generic-lens or generic-optics with 'listenerEndpoint' instead." #-}

-- | Provides the list of option group memberships for this DB instance.
--
-- /Note:/ Consider using 'optionGroupMemberships' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diOptionGroupMemberships :: Lens.Lens' DBInstance (Lude.Maybe [OptionGroupMembership])
diOptionGroupMemberships = Lens.lens (optionGroupMemberships :: DBInstance -> Lude.Maybe [OptionGroupMembership]) (\s a -> s {optionGroupMemberships = a} :: DBInstance)
{-# DEPRECATED diOptionGroupMemberships "Use generic-lens or generic-optics with 'optionGroupMemberships' instead." #-}

-- | A list of log types that this DB instance is configured to export to CloudWatch Logs.
--
-- Log types vary by DB engine. For information about the log types for each DB engine, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_LogAccess.html Amazon RDS Database Log Files> in the /Amazon RDS User Guide./
--
-- /Note:/ Consider using 'enabledCloudwatchLogsExports' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diEnabledCloudwatchLogsExports :: Lens.Lens' DBInstance (Lude.Maybe [Lude.Text])
diEnabledCloudwatchLogsExports = Lens.lens (enabledCloudwatchLogsExports :: DBInstance -> Lude.Maybe [Lude.Text]) (\s a -> s {enabledCloudwatchLogsExports = a} :: DBInstance)
{-# DEPRECATED diEnabledCloudwatchLogsExports "Use generic-lens or generic-optics with 'enabledCloudwatchLogsExports' instead." #-}

-- | The Amazon Resource Name (ARN) of the Amazon CloudWatch Logs log stream that receives the Enhanced Monitoring metrics data for the DB instance.
--
-- /Note:/ Consider using 'enhancedMonitoringResourceARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diEnhancedMonitoringResourceARN :: Lens.Lens' DBInstance (Lude.Maybe Lude.Text)
diEnhancedMonitoringResourceARN = Lens.lens (enhancedMonitoringResourceARN :: DBInstance -> Lude.Maybe Lude.Text) (\s a -> s {enhancedMonitoringResourceARN = a} :: DBInstance)
{-# DEPRECATED diEnhancedMonitoringResourceARN "Use generic-lens or generic-optics with 'enhancedMonitoringResourceARN' instead." #-}

-- | If present, specifies the name of the secondary Availability Zone for a DB instance with multi-AZ support.
--
-- /Note:/ Consider using 'secondaryAvailabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diSecondaryAvailabilityZone :: Lens.Lens' DBInstance (Lude.Maybe Lude.Text)
diSecondaryAvailabilityZone = Lens.lens (secondaryAvailabilityZone :: DBInstance -> Lude.Maybe Lude.Text) (\s a -> s {secondaryAvailabilityZone = a} :: DBInstance)
{-# DEPRECATED diSecondaryAvailabilityZone "Use generic-lens or generic-optics with 'secondaryAvailabilityZone' instead." #-}

-- | True if Performance Insights is enabled for the DB instance, and otherwise false.
--
-- /Note:/ Consider using 'performanceInsightsEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diPerformanceInsightsEnabled :: Lens.Lens' DBInstance (Lude.Maybe Lude.Bool)
diPerformanceInsightsEnabled = Lens.lens (performanceInsightsEnabled :: DBInstance -> Lude.Maybe Lude.Bool) (\s a -> s {performanceInsightsEnabled = a} :: DBInstance)
{-# DEPRECATED diPerformanceInsightsEnabled "Use generic-lens or generic-optics with 'performanceInsightsEnabled' instead." #-}

-- | Specifies the allocated storage size specified in gibibytes.
--
-- /Note:/ Consider using 'allocatedStorage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diAllocatedStorage :: Lens.Lens' DBInstance (Lude.Maybe Lude.Int)
diAllocatedStorage = Lens.lens (allocatedStorage :: DBInstance -> Lude.Maybe Lude.Int) (\s a -> s {allocatedStorage = a} :: DBInstance)
{-# DEPRECATED diAllocatedStorage "Use generic-lens or generic-optics with 'allocatedStorage' instead." #-}

-- | The AWS Region-unique, immutable identifier for the DB instance. This identifier is found in AWS CloudTrail log entries whenever the AWS KMS key for the DB instance is accessed.
--
-- /Note:/ Consider using 'dbiResourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diDBiResourceId :: Lens.Lens' DBInstance (Lude.Maybe Lude.Text)
diDBiResourceId = Lens.lens (dbiResourceId :: DBInstance -> Lude.Maybe Lude.Text) (\s a -> s {dbiResourceId = a} :: DBInstance)
{-# DEPRECATED diDBiResourceId "Use generic-lens or generic-optics with 'dbiResourceId' instead." #-}

-- | Provides the list of DB parameter groups applied to this DB instance.
--
-- /Note:/ Consider using 'dbParameterGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diDBParameterGroups :: Lens.Lens' DBInstance (Lude.Maybe [DBParameterGroupStatus])
diDBParameterGroups = Lens.lens (dbParameterGroups :: DBInstance -> Lude.Maybe [DBParameterGroupStatus]) (\s a -> s {dbParameterGroups = a} :: DBInstance)
{-# DEPRECATED diDBParameterGroups "Use generic-lens or generic-optics with 'dbParameterGroups' instead." #-}

-- | Specifies whether tags are copied from the DB instance to snapshots of the DB instance.
--
-- __Amazon Aurora__
-- Not applicable. Copying tags to snapshots is managed by the DB cluster. Setting this value for an Aurora DB instance has no effect on the DB cluster setting. For more information, see @DBCluster@ .
--
-- /Note:/ Consider using 'copyTagsToSnapshot' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diCopyTagsToSnapshot :: Lens.Lens' DBInstance (Lude.Maybe Lude.Bool)
diCopyTagsToSnapshot = Lens.lens (copyTagsToSnapshot :: DBInstance -> Lude.Maybe Lude.Bool) (\s a -> s {copyTagsToSnapshot = a} :: DBInstance)
{-# DEPRECATED diCopyTagsToSnapshot "Use generic-lens or generic-optics with 'copyTagsToSnapshot' instead." #-}

-- | The time zone of the DB instance. In most cases, the @Timezone@ element is empty. @Timezone@ content appears only for Microsoft SQL Server DB instances that were created with a time zone specified.
--
-- /Note:/ Consider using 'timezone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diTimezone :: Lens.Lens' DBInstance (Lude.Maybe Lude.Text)
diTimezone = Lens.lens (timezone :: DBInstance -> Lude.Maybe Lude.Text) (\s a -> s {timezone = a} :: DBInstance)
{-# DEPRECATED diTimezone "Use generic-lens or generic-optics with 'timezone' instead." #-}

-- | The ARN from the key store with which the instance is associated for TDE encryption.
--
-- /Note:/ Consider using 'tdeCredentialARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diTDECredentialARN :: Lens.Lens' DBInstance (Lude.Maybe Lude.Text)
diTDECredentialARN = Lens.lens (tdeCredentialARN :: DBInstance -> Lude.Maybe Lude.Text) (\s a -> s {tdeCredentialARN = a} :: DBInstance)
{-# DEPRECATED diTDECredentialARN "Use generic-lens or generic-optics with 'tdeCredentialARN' instead." #-}

-- | Specifies the connection endpoint.
--
-- /Note:/ Consider using 'endpoint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diEndpoint :: Lens.Lens' DBInstance (Lude.Maybe Endpoint)
diEndpoint = Lens.lens (endpoint :: DBInstance -> Lude.Maybe Endpoint) (\s a -> s {endpoint = a} :: DBInstance)
{-# DEPRECATED diEndpoint "Use generic-lens or generic-optics with 'endpoint' instead." #-}

-- | Specifies the current state of this database.
--
-- For information about DB instance statuses, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Overview.DBInstance.Status.html DB Instance Status> in the /Amazon RDS User Guide./
--
-- /Note:/ Consider using 'dbInstanceStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diDBInstanceStatus :: Lens.Lens' DBInstance (Lude.Maybe Lude.Text)
diDBInstanceStatus = Lens.lens (dbInstanceStatus :: DBInstance -> Lude.Maybe Lude.Text) (\s a -> s {dbInstanceStatus = a} :: DBInstance)
{-# DEPRECATED diDBInstanceStatus "Use generic-lens or generic-optics with 'dbInstanceStatus' instead." #-}

-- | Specifies the port that the DB instance listens on. If the DB instance is part of a DB cluster, this can be a different port than the DB cluster port.
--
-- /Note:/ Consider using 'dbInstancePort' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diDBInstancePort :: Lens.Lens' DBInstance (Lude.Maybe Lude.Int)
diDBInstancePort = Lens.lens (dbInstancePort :: DBInstance -> Lude.Maybe Lude.Int) (\s a -> s {dbInstancePort = a} :: DBInstance)
{-# DEPRECATED diDBInstancePort "Use generic-lens or generic-optics with 'dbInstancePort' instead." #-}

-- | Specifies that changes to the DB instance are pending. This element is only included when changes are pending. Specific changes are identified by subelements.
--
-- /Note:/ Consider using 'pendingModifiedValues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diPendingModifiedValues :: Lens.Lens' DBInstance (Lude.Maybe PendingModifiedValues)
diPendingModifiedValues = Lens.lens (pendingModifiedValues :: DBInstance -> Lude.Maybe PendingModifiedValues) (\s a -> s {pendingModifiedValues = a} :: DBInstance)
{-# DEPRECATED diPendingModifiedValues "Use generic-lens or generic-optics with 'pendingModifiedValues' instead." #-}

-- | Contains one or more identifiers of Aurora DB clusters to which the RDS DB instance is replicated as a read replica. For example, when you create an Aurora read replica of an RDS MySQL DB instance, the Aurora MySQL DB cluster for the Aurora read replica is shown. This output does not contain information about cross region Aurora read replicas.
--
-- /Note:/ Consider using 'readReplicaDBClusterIdentifiers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diReadReplicaDBClusterIdentifiers :: Lens.Lens' DBInstance (Lude.Maybe [Lude.Text])
diReadReplicaDBClusterIdentifiers = Lens.lens (readReplicaDBClusterIdentifiers :: DBInstance -> Lude.Maybe [Lude.Text]) (\s a -> s {readReplicaDBClusterIdentifiers = a} :: DBInstance)
{-# DEPRECATED diReadReplicaDBClusterIdentifiers "Use generic-lens or generic-optics with 'readReplicaDBClusterIdentifiers' instead." #-}

-- | Specifies the storage type associated with DB instance.
--
-- /Note:/ Consider using 'storageType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diStorageType :: Lens.Lens' DBInstance (Lude.Maybe Lude.Text)
diStorageType = Lens.lens (storageType :: DBInstance -> Lude.Maybe Lude.Text) (\s a -> s {storageType = a} :: DBInstance)
{-# DEPRECATED diStorageType "Use generic-lens or generic-optics with 'storageType' instead." #-}

-- | The status of a read replica. If the instance isn't a read replica, this is blank.
--
-- /Note:/ Consider using 'statusInfos' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diStatusInfos :: Lens.Lens' DBInstance (Lude.Maybe [DBInstanceStatusInfo])
diStatusInfos = Lens.lens (statusInfos :: DBInstance -> Lude.Maybe [DBInstanceStatusInfo]) (\s a -> s {statusInfos = a} :: DBInstance)
{-# DEPRECATED diStatusInfos "Use generic-lens or generic-optics with 'statusInfos' instead." #-}

-- | The Active Directory Domain membership records associated with the DB instance.
--
-- /Note:/ Consider using 'domainMemberships' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diDomainMemberships :: Lens.Lens' DBInstance (Lude.Maybe [DomainMembership])
diDomainMemberships = Lens.lens (domainMemberships :: DBInstance -> Lude.Maybe [DomainMembership]) (\s a -> s {domainMemberships = a} :: DBInstance)
{-# DEPRECATED diDomainMemberships "Use generic-lens or generic-optics with 'domainMemberships' instead." #-}

-- | The meaning of this parameter differs according to the database engine you use.
--
-- __MySQL, MariaDB, SQL Server, PostgreSQL__
-- Contains the name of the initial database of this instance that was provided at create time, if one was specified when the DB instance was created. This same name is returned for the life of the DB instance.
-- Type: String
-- __Oracle__
-- Contains the Oracle System ID (SID) of the created DB instance. Not shown when the returned parameters do not apply to an Oracle DB instance.
--
-- /Note:/ Consider using 'dbName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diDBName :: Lens.Lens' DBInstance (Lude.Maybe Lude.Text)
diDBName = Lens.lens (dbName :: DBInstance -> Lude.Maybe Lude.Text) (\s a -> s {dbName = a} :: DBInstance)
{-# DEPRECATED diDBName "Use generic-lens or generic-optics with 'dbName' instead." #-}

instance Lude.FromXML DBInstance where
  parseXML x =
    DBInstance'
      Lude.<$> (x Lude..@? "EngineVersion")
      Lude.<*> ( x Lude..@? "DBSecurityGroups" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "DBSecurityGroup")
               )
      Lude.<*> (x Lude..@? "DeletionProtection")
      Lude.<*> (x Lude..@? "StorageEncrypted")
      Lude.<*> (x Lude..@? "DBClusterIdentifier")
      Lude.<*> (x Lude..@? "PubliclyAccessible")
      Lude.<*> (x Lude..@? "AutoMinorVersionUpgrade")
      Lude.<*> (x Lude..@? "DBInstanceArn")
      Lude.<*> (x Lude..@? "MasterUsername")
      Lude.<*> ( x Lude..@? "ReadReplicaDBInstanceIdentifiers" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "ReadReplicaDBInstanceIdentifier")
               )
      Lude.<*> (x Lude..@? "IAMDatabaseAuthenticationEnabled")
      Lude.<*> (x Lude..@? "MonitoringRoleArn")
      Lude.<*> (x Lude..@? "Iops")
      Lude.<*> (x Lude..@? "InstanceCreateTime")
      Lude.<*> ( x Lude..@? "TagList" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "Tag")
               )
      Lude.<*> (x Lude..@? "ReadReplicaSourceDBInstanceIdentifier")
      Lude.<*> (x Lude..@? "ReplicaMode")
      Lude.<*> (x Lude..@? "MonitoringInterval")
      Lude.<*> (x Lude..@? "Engine")
      Lude.<*> ( x Lude..@? "ProcessorFeatures" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "ProcessorFeature")
               )
      Lude.<*> (x Lude..@? "LatestRestorableTime")
      Lude.<*> (x Lude..@? "DBInstanceClass")
      Lude.<*> (x Lude..@? "PromotionTier")
      Lude.<*> (x Lude..@? "LicenseModel")
      Lude.<*> (x Lude..@? "PreferredMaintenanceWindow")
      Lude.<*> (x Lude..@? "PerformanceInsightsRetentionPeriod")
      Lude.<*> (x Lude..@? "CACertificateIdentifier")
      Lude.<*> (x Lude..@? "DBInstanceIdentifier")
      Lude.<*> (x Lude..@? "CharacterSetName")
      Lude.<*> (x Lude..@? "MaxAllocatedStorage")
      Lude.<*> (x Lude..@? "KmsKeyId")
      Lude.<*> (x Lude..@? "PreferredBackupWindow")
      Lude.<*> ( x Lude..@? "AssociatedRoles" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "DBInstanceRole")
               )
      Lude.<*> (x Lude..@? "AvailabilityZone")
      Lude.<*> ( x Lude..@? "VpcSecurityGroups" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "VpcSecurityGroupMembership")
               )
      Lude.<*> (x Lude..@? "BackupRetentionPeriod")
      Lude.<*> (x Lude..@? "NcharCharacterSetName")
      Lude.<*> (x Lude..@? "PerformanceInsightsKMSKeyId")
      Lude.<*> (x Lude..@? "DBSubnetGroup")
      Lude.<*> (x Lude..@? "MultiAZ")
      Lude.<*> (x Lude..@? "ListenerEndpoint")
      Lude.<*> ( x Lude..@? "OptionGroupMemberships" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "OptionGroupMembership")
               )
      Lude.<*> ( x Lude..@? "EnabledCloudwatchLogsExports" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "member")
               )
      Lude.<*> (x Lude..@? "EnhancedMonitoringResourceArn")
      Lude.<*> (x Lude..@? "SecondaryAvailabilityZone")
      Lude.<*> (x Lude..@? "PerformanceInsightsEnabled")
      Lude.<*> (x Lude..@? "AllocatedStorage")
      Lude.<*> (x Lude..@? "DbiResourceId")
      Lude.<*> ( x Lude..@? "DBParameterGroups" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "DBParameterGroup")
               )
      Lude.<*> (x Lude..@? "CopyTagsToSnapshot")
      Lude.<*> (x Lude..@? "Timezone")
      Lude.<*> (x Lude..@? "TdeCredentialArn")
      Lude.<*> (x Lude..@? "Endpoint")
      Lude.<*> (x Lude..@? "DBInstanceStatus")
      Lude.<*> (x Lude..@? "DbInstancePort")
      Lude.<*> (x Lude..@? "PendingModifiedValues")
      Lude.<*> ( x Lude..@? "ReadReplicaDBClusterIdentifiers" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "ReadReplicaDBClusterIdentifier")
               )
      Lude.<*> (x Lude..@? "StorageType")
      Lude.<*> ( x Lude..@? "StatusInfos" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "DBInstanceStatusInfo")
               )
      Lude.<*> ( x Lude..@? "DomainMemberships" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "DomainMembership")
               )
      Lude.<*> (x Lude..@? "DBName")
