{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.DBInstance
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.DBInstance where

import Network.AWS.Lens
import Network.AWS.Prelude
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
--
-- This data type is used as a response element in the @DescribeDBInstances@ action.
--
--
-- /See:/ 'dbInstance' smart constructor.
data DBInstance = DBInstance'
  { _diEngineVersion :: !(Maybe Text),
    _diDBSecurityGroups :: !(Maybe [DBSecurityGroupMembership]),
    _diDeletionProtection :: !(Maybe Bool),
    _diStorageEncrypted :: !(Maybe Bool),
    _diDBClusterIdentifier :: !(Maybe Text),
    _diPubliclyAccessible :: !(Maybe Bool),
    _diAutoMinorVersionUpgrade :: !(Maybe Bool),
    _diDBInstanceARN :: !(Maybe Text),
    _diMasterUsername :: !(Maybe Text),
    _diReadReplicaDBInstanceIdentifiers :: !(Maybe [Text]),
    _diIAMDatabaseAuthenticationEnabled :: !(Maybe Bool),
    _diMonitoringRoleARN :: !(Maybe Text),
    _diIOPS :: !(Maybe Int),
    _diInstanceCreateTime :: !(Maybe ISO8601),
    _diTagList :: !(Maybe [Tag]),
    _diReadReplicaSourceDBInstanceIdentifier :: !(Maybe Text),
    _diReplicaMode :: !(Maybe ReplicaMode),
    _diMonitoringInterval :: !(Maybe Int),
    _diEngine :: !(Maybe Text),
    _diProcessorFeatures :: !(Maybe [ProcessorFeature]),
    _diLatestRestorableTime :: !(Maybe ISO8601),
    _diDBInstanceClass :: !(Maybe Text),
    _diPromotionTier :: !(Maybe Int),
    _diLicenseModel :: !(Maybe Text),
    _diPreferredMaintenanceWindow :: !(Maybe Text),
    _diPerformanceInsightsRetentionPeriod :: !(Maybe Int),
    _diCACertificateIdentifier :: !(Maybe Text),
    _diDBInstanceIdentifier :: !(Maybe Text),
    _diCharacterSetName :: !(Maybe Text),
    _diMaxAllocatedStorage :: !(Maybe Int),
    _diKMSKeyId :: !(Maybe Text),
    _diPreferredBackupWindow :: !(Maybe Text),
    _diAssociatedRoles :: !(Maybe [DBInstanceRole]),
    _diAvailabilityZone :: !(Maybe Text),
    _diVPCSecurityGroups :: !(Maybe [VPCSecurityGroupMembership]),
    _diBackupRetentionPeriod :: !(Maybe Int),
    _diNcharCharacterSetName :: !(Maybe Text),
    _diPerformanceInsightsKMSKeyId :: !(Maybe Text),
    _diDBSubnetGroup :: !(Maybe DBSubnetGroup),
    _diMultiAZ :: !(Maybe Bool),
    _diListenerEndpoint :: !(Maybe Endpoint),
    _diOptionGroupMemberships :: !(Maybe [OptionGroupMembership]),
    _diEnabledCloudwatchLogsExports :: !(Maybe [Text]),
    _diEnhancedMonitoringResourceARN :: !(Maybe Text),
    _diSecondaryAvailabilityZone :: !(Maybe Text),
    _diPerformanceInsightsEnabled :: !(Maybe Bool),
    _diAllocatedStorage :: !(Maybe Int),
    _diDBiResourceId :: !(Maybe Text),
    _diDBParameterGroups :: !(Maybe [DBParameterGroupStatus]),
    _diCopyTagsToSnapshot :: !(Maybe Bool),
    _diTimezone :: !(Maybe Text),
    _diTDECredentialARN :: !(Maybe Text),
    _diEndpoint :: !(Maybe Endpoint),
    _diDBInstanceStatus :: !(Maybe Text),
    _diDBInstancePort :: !(Maybe Int),
    _diPendingModifiedValues :: !(Maybe PendingModifiedValues),
    _diReadReplicaDBClusterIdentifiers :: !(Maybe [Text]),
    _diStorageType :: !(Maybe Text),
    _diStatusInfos :: !(Maybe [DBInstanceStatusInfo]),
    _diDomainMemberships :: !(Maybe [DomainMembership]),
    _diDBName :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DBInstance' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'diEngineVersion' - Indicates the database engine version.
--
-- * 'diDBSecurityGroups' - A list of DB security group elements containing @DBSecurityGroup.Name@ and @DBSecurityGroup.Status@ subelements.
--
-- * 'diDeletionProtection' - Indicates if the DB instance has deletion protection enabled. The database can't be deleted when deletion protection is enabled. For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_DeleteInstance.html Deleting a DB Instance> .
--
-- * 'diStorageEncrypted' - Specifies whether the DB instance is encrypted.
--
-- * 'diDBClusterIdentifier' - If the DB instance is a member of a DB cluster, contains the name of the DB cluster that the DB instance is a member of.
--
-- * 'diPubliclyAccessible' - Specifies the accessibility options for the DB instance. When the DB instance is publicly accessible, its DNS endpoint resolves to the private IP address from within the DB instance's VPC, and to the public IP address from outside of the DB instance's VPC. Access to the DB instance is ultimately controlled by the security group it uses, and that public access is not permitted if the security group assigned to the DB instance doesn't permit it. When the DB instance isn't publicly accessible, it is an internal DB instance with a DNS name that resolves to a private IP address. For more information, see 'CreateDBInstance' .
--
-- * 'diAutoMinorVersionUpgrade' - Indicates that minor version patches are applied automatically.
--
-- * 'diDBInstanceARN' - The Amazon Resource Name (ARN) for the DB instance.
--
-- * 'diMasterUsername' - Contains the master username for the DB instance.
--
-- * 'diReadReplicaDBInstanceIdentifiers' - Contains one or more identifiers of the read replicas associated with this DB instance.
--
-- * 'diIAMDatabaseAuthenticationEnabled' - True if mapping of AWS Identity and Access Management (IAM) accounts to database accounts is enabled, and otherwise false. IAM database authentication can be enabled for the following database engines     * For MySQL 5.6, minor version 5.6.34 or higher     * For MySQL 5.7, minor version 5.7.16 or higher     * Aurora 5.6 or higher. To enable IAM database authentication for Aurora, see DBCluster Type.
--
-- * 'diMonitoringRoleARN' - The ARN for the IAM role that permits RDS to send Enhanced Monitoring metrics to Amazon CloudWatch Logs.
--
-- * 'diIOPS' - Specifies the Provisioned IOPS (I/O operations per second) value.
--
-- * 'diInstanceCreateTime' - Provides the date and time the DB instance was created.
--
-- * 'diTagList' - Undocumented member.
--
-- * 'diReadReplicaSourceDBInstanceIdentifier' - Contains the identifier of the source DB instance if this DB instance is a read replica.
--
-- * 'diReplicaMode' - The open mode of an Oracle read replica. The default is @open-read-only@ . For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/oracle-read-replicas.html Working with Oracle Read Replicas for Amazon RDS> in the /Amazon RDS User Guide/ .
--
-- * 'diMonitoringInterval' - The interval, in seconds, between points when Enhanced Monitoring metrics are collected for the DB instance.
--
-- * 'diEngine' - The name of the database engine to be used for this DB instance.
--
-- * 'diProcessorFeatures' - The number of CPU cores and the number of threads per core for the DB instance class of the DB instance.
--
-- * 'diLatestRestorableTime' - Specifies the latest time to which a database can be restored with point-in-time restore.
--
-- * 'diDBInstanceClass' - Contains the name of the compute and memory capacity class of the DB instance.
--
-- * 'diPromotionTier' - A value that specifies the order in which an Aurora Replica is promoted to the primary instance after a failure of the existing primary instance. For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/Aurora.Managing.Backups.html#Aurora.Managing.FaultTolerance Fault Tolerance for an Aurora DB Cluster> in the /Amazon Aurora User Guide/ .
--
-- * 'diLicenseModel' - License model information for this DB instance.
--
-- * 'diPreferredMaintenanceWindow' - Specifies the weekly time range during which system maintenance can occur, in Universal Coordinated Time (UTC).
--
-- * 'diPerformanceInsightsRetentionPeriod' - The amount of time, in days, to retain Performance Insights data. Valid values are 7 or 731 (2 years).
--
-- * 'diCACertificateIdentifier' - The identifier of the CA certificate for this DB instance.
--
-- * 'diDBInstanceIdentifier' - Contains a user-supplied database identifier. This identifier is the unique key that identifies a DB instance.
--
-- * 'diCharacterSetName' - If present, specifies the name of the character set that this instance is associated with.
--
-- * 'diMaxAllocatedStorage' - The upper limit to which Amazon RDS can automatically scale the storage of the DB instance.
--
-- * 'diKMSKeyId' - If @StorageEncrypted@ is true, the AWS KMS key identifier for the encrypted DB instance.
--
-- * 'diPreferredBackupWindow' - Specifies the daily time range during which automated backups are created if automated backups are enabled, as determined by the @BackupRetentionPeriod@ .
--
-- * 'diAssociatedRoles' - The AWS Identity and Access Management (IAM) roles associated with the DB instance.
--
-- * 'diAvailabilityZone' - Specifies the name of the Availability Zone the DB instance is located in.
--
-- * 'diVPCSecurityGroups' - Provides a list of VPC security group elements that the DB instance belongs to.
--
-- * 'diBackupRetentionPeriod' - Specifies the number of days for which automatic DB snapshots are retained.
--
-- * 'diNcharCharacterSetName' - The name of the NCHAR character set for the Oracle DB instance. This character set specifies the Unicode encoding for data stored in table columns of type NCHAR, NCLOB, or NVARCHAR2.
--
-- * 'diPerformanceInsightsKMSKeyId' - The AWS KMS key identifier for encryption of Performance Insights data. The KMS key ID is the Amazon Resource Name (ARN), KMS key identifier, or the KMS key alias for the KMS encryption key.
--
-- * 'diDBSubnetGroup' - Specifies information on the subnet group associated with the DB instance, including the name, description, and subnets in the subnet group.
--
-- * 'diMultiAZ' - Specifies if the DB instance is a Multi-AZ deployment.
--
-- * 'diListenerEndpoint' - Specifies the listener connection endpoint for SQL Server Always On.
--
-- * 'diOptionGroupMemberships' - Provides the list of option group memberships for this DB instance.
--
-- * 'diEnabledCloudwatchLogsExports' - A list of log types that this DB instance is configured to export to CloudWatch Logs. Log types vary by DB engine. For information about the log types for each DB engine, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_LogAccess.html Amazon RDS Database Log Files> in the /Amazon RDS User Guide./
--
-- * 'diEnhancedMonitoringResourceARN' - The Amazon Resource Name (ARN) of the Amazon CloudWatch Logs log stream that receives the Enhanced Monitoring metrics data for the DB instance.
--
-- * 'diSecondaryAvailabilityZone' - If present, specifies the name of the secondary Availability Zone for a DB instance with multi-AZ support.
--
-- * 'diPerformanceInsightsEnabled' - True if Performance Insights is enabled for the DB instance, and otherwise false.
--
-- * 'diAllocatedStorage' - Specifies the allocated storage size specified in gibibytes.
--
-- * 'diDBiResourceId' - The AWS Region-unique, immutable identifier for the DB instance. This identifier is found in AWS CloudTrail log entries whenever the AWS KMS key for the DB instance is accessed.
--
-- * 'diDBParameterGroups' - Provides the list of DB parameter groups applied to this DB instance.
--
-- * 'diCopyTagsToSnapshot' - Specifies whether tags are copied from the DB instance to snapshots of the DB instance. __Amazon Aurora__  Not applicable. Copying tags to snapshots is managed by the DB cluster. Setting this value for an Aurora DB instance has no effect on the DB cluster setting. For more information, see @DBCluster@ .
--
-- * 'diTimezone' - The time zone of the DB instance. In most cases, the @Timezone@ element is empty. @Timezone@ content appears only for Microsoft SQL Server DB instances that were created with a time zone specified.
--
-- * 'diTDECredentialARN' - The ARN from the key store with which the instance is associated for TDE encryption.
--
-- * 'diEndpoint' - Specifies the connection endpoint.
--
-- * 'diDBInstanceStatus' - Specifies the current state of this database. For information about DB instance statuses, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Overview.DBInstance.Status.html DB Instance Status> in the /Amazon RDS User Guide./
--
-- * 'diDBInstancePort' - Specifies the port that the DB instance listens on. If the DB instance is part of a DB cluster, this can be a different port than the DB cluster port.
--
-- * 'diPendingModifiedValues' - Specifies that changes to the DB instance are pending. This element is only included when changes are pending. Specific changes are identified by subelements.
--
-- * 'diReadReplicaDBClusterIdentifiers' - Contains one or more identifiers of Aurora DB clusters to which the RDS DB instance is replicated as a read replica. For example, when you create an Aurora read replica of an RDS MySQL DB instance, the Aurora MySQL DB cluster for the Aurora read replica is shown. This output does not contain information about cross region Aurora read replicas.
--
-- * 'diStorageType' - Specifies the storage type associated with DB instance.
--
-- * 'diStatusInfos' - The status of a read replica. If the instance isn't a read replica, this is blank.
--
-- * 'diDomainMemberships' - The Active Directory Domain membership records associated with the DB instance.
--
-- * 'diDBName' - The meaning of this parameter differs according to the database engine you use. __MySQL, MariaDB, SQL Server, PostgreSQL__  Contains the name of the initial database of this instance that was provided at create time, if one was specified when the DB instance was created. This same name is returned for the life of the DB instance. Type: String __Oracle__  Contains the Oracle System ID (SID) of the created DB instance. Not shown when the returned parameters do not apply to an Oracle DB instance.
dbInstance ::
  DBInstance
dbInstance =
  DBInstance'
    { _diEngineVersion = Nothing,
      _diDBSecurityGroups = Nothing,
      _diDeletionProtection = Nothing,
      _diStorageEncrypted = Nothing,
      _diDBClusterIdentifier = Nothing,
      _diPubliclyAccessible = Nothing,
      _diAutoMinorVersionUpgrade = Nothing,
      _diDBInstanceARN = Nothing,
      _diMasterUsername = Nothing,
      _diReadReplicaDBInstanceIdentifiers = Nothing,
      _diIAMDatabaseAuthenticationEnabled = Nothing,
      _diMonitoringRoleARN = Nothing,
      _diIOPS = Nothing,
      _diInstanceCreateTime = Nothing,
      _diTagList = Nothing,
      _diReadReplicaSourceDBInstanceIdentifier = Nothing,
      _diReplicaMode = Nothing,
      _diMonitoringInterval = Nothing,
      _diEngine = Nothing,
      _diProcessorFeatures = Nothing,
      _diLatestRestorableTime = Nothing,
      _diDBInstanceClass = Nothing,
      _diPromotionTier = Nothing,
      _diLicenseModel = Nothing,
      _diPreferredMaintenanceWindow = Nothing,
      _diPerformanceInsightsRetentionPeriod = Nothing,
      _diCACertificateIdentifier = Nothing,
      _diDBInstanceIdentifier = Nothing,
      _diCharacterSetName = Nothing,
      _diMaxAllocatedStorage = Nothing,
      _diKMSKeyId = Nothing,
      _diPreferredBackupWindow = Nothing,
      _diAssociatedRoles = Nothing,
      _diAvailabilityZone = Nothing,
      _diVPCSecurityGroups = Nothing,
      _diBackupRetentionPeriod = Nothing,
      _diNcharCharacterSetName = Nothing,
      _diPerformanceInsightsKMSKeyId = Nothing,
      _diDBSubnetGroup = Nothing,
      _diMultiAZ = Nothing,
      _diListenerEndpoint = Nothing,
      _diOptionGroupMemberships = Nothing,
      _diEnabledCloudwatchLogsExports = Nothing,
      _diEnhancedMonitoringResourceARN = Nothing,
      _diSecondaryAvailabilityZone = Nothing,
      _diPerformanceInsightsEnabled = Nothing,
      _diAllocatedStorage = Nothing,
      _diDBiResourceId = Nothing,
      _diDBParameterGroups = Nothing,
      _diCopyTagsToSnapshot = Nothing,
      _diTimezone = Nothing,
      _diTDECredentialARN = Nothing,
      _diEndpoint = Nothing,
      _diDBInstanceStatus = Nothing,
      _diDBInstancePort = Nothing,
      _diPendingModifiedValues = Nothing,
      _diReadReplicaDBClusterIdentifiers = Nothing,
      _diStorageType = Nothing,
      _diStatusInfos = Nothing,
      _diDomainMemberships = Nothing,
      _diDBName = Nothing
    }

-- | Indicates the database engine version.
diEngineVersion :: Lens' DBInstance (Maybe Text)
diEngineVersion = lens _diEngineVersion (\s a -> s {_diEngineVersion = a})

-- | A list of DB security group elements containing @DBSecurityGroup.Name@ and @DBSecurityGroup.Status@ subelements.
diDBSecurityGroups :: Lens' DBInstance [DBSecurityGroupMembership]
diDBSecurityGroups = lens _diDBSecurityGroups (\s a -> s {_diDBSecurityGroups = a}) . _Default . _Coerce

-- | Indicates if the DB instance has deletion protection enabled. The database can't be deleted when deletion protection is enabled. For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_DeleteInstance.html Deleting a DB Instance> .
diDeletionProtection :: Lens' DBInstance (Maybe Bool)
diDeletionProtection = lens _diDeletionProtection (\s a -> s {_diDeletionProtection = a})

-- | Specifies whether the DB instance is encrypted.
diStorageEncrypted :: Lens' DBInstance (Maybe Bool)
diStorageEncrypted = lens _diStorageEncrypted (\s a -> s {_diStorageEncrypted = a})

-- | If the DB instance is a member of a DB cluster, contains the name of the DB cluster that the DB instance is a member of.
diDBClusterIdentifier :: Lens' DBInstance (Maybe Text)
diDBClusterIdentifier = lens _diDBClusterIdentifier (\s a -> s {_diDBClusterIdentifier = a})

-- | Specifies the accessibility options for the DB instance. When the DB instance is publicly accessible, its DNS endpoint resolves to the private IP address from within the DB instance's VPC, and to the public IP address from outside of the DB instance's VPC. Access to the DB instance is ultimately controlled by the security group it uses, and that public access is not permitted if the security group assigned to the DB instance doesn't permit it. When the DB instance isn't publicly accessible, it is an internal DB instance with a DNS name that resolves to a private IP address. For more information, see 'CreateDBInstance' .
diPubliclyAccessible :: Lens' DBInstance (Maybe Bool)
diPubliclyAccessible = lens _diPubliclyAccessible (\s a -> s {_diPubliclyAccessible = a})

-- | Indicates that minor version patches are applied automatically.
diAutoMinorVersionUpgrade :: Lens' DBInstance (Maybe Bool)
diAutoMinorVersionUpgrade = lens _diAutoMinorVersionUpgrade (\s a -> s {_diAutoMinorVersionUpgrade = a})

-- | The Amazon Resource Name (ARN) for the DB instance.
diDBInstanceARN :: Lens' DBInstance (Maybe Text)
diDBInstanceARN = lens _diDBInstanceARN (\s a -> s {_diDBInstanceARN = a})

-- | Contains the master username for the DB instance.
diMasterUsername :: Lens' DBInstance (Maybe Text)
diMasterUsername = lens _diMasterUsername (\s a -> s {_diMasterUsername = a})

-- | Contains one or more identifiers of the read replicas associated with this DB instance.
diReadReplicaDBInstanceIdentifiers :: Lens' DBInstance [Text]
diReadReplicaDBInstanceIdentifiers = lens _diReadReplicaDBInstanceIdentifiers (\s a -> s {_diReadReplicaDBInstanceIdentifiers = a}) . _Default . _Coerce

-- | True if mapping of AWS Identity and Access Management (IAM) accounts to database accounts is enabled, and otherwise false. IAM database authentication can be enabled for the following database engines     * For MySQL 5.6, minor version 5.6.34 or higher     * For MySQL 5.7, minor version 5.7.16 or higher     * Aurora 5.6 or higher. To enable IAM database authentication for Aurora, see DBCluster Type.
diIAMDatabaseAuthenticationEnabled :: Lens' DBInstance (Maybe Bool)
diIAMDatabaseAuthenticationEnabled = lens _diIAMDatabaseAuthenticationEnabled (\s a -> s {_diIAMDatabaseAuthenticationEnabled = a})

-- | The ARN for the IAM role that permits RDS to send Enhanced Monitoring metrics to Amazon CloudWatch Logs.
diMonitoringRoleARN :: Lens' DBInstance (Maybe Text)
diMonitoringRoleARN = lens _diMonitoringRoleARN (\s a -> s {_diMonitoringRoleARN = a})

-- | Specifies the Provisioned IOPS (I/O operations per second) value.
diIOPS :: Lens' DBInstance (Maybe Int)
diIOPS = lens _diIOPS (\s a -> s {_diIOPS = a})

-- | Provides the date and time the DB instance was created.
diInstanceCreateTime :: Lens' DBInstance (Maybe UTCTime)
diInstanceCreateTime = lens _diInstanceCreateTime (\s a -> s {_diInstanceCreateTime = a}) . mapping _Time

-- | Undocumented member.
diTagList :: Lens' DBInstance [Tag]
diTagList = lens _diTagList (\s a -> s {_diTagList = a}) . _Default . _Coerce

-- | Contains the identifier of the source DB instance if this DB instance is a read replica.
diReadReplicaSourceDBInstanceIdentifier :: Lens' DBInstance (Maybe Text)
diReadReplicaSourceDBInstanceIdentifier = lens _diReadReplicaSourceDBInstanceIdentifier (\s a -> s {_diReadReplicaSourceDBInstanceIdentifier = a})

-- | The open mode of an Oracle read replica. The default is @open-read-only@ . For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/oracle-read-replicas.html Working with Oracle Read Replicas for Amazon RDS> in the /Amazon RDS User Guide/ .
diReplicaMode :: Lens' DBInstance (Maybe ReplicaMode)
diReplicaMode = lens _diReplicaMode (\s a -> s {_diReplicaMode = a})

-- | The interval, in seconds, between points when Enhanced Monitoring metrics are collected for the DB instance.
diMonitoringInterval :: Lens' DBInstance (Maybe Int)
diMonitoringInterval = lens _diMonitoringInterval (\s a -> s {_diMonitoringInterval = a})

-- | The name of the database engine to be used for this DB instance.
diEngine :: Lens' DBInstance (Maybe Text)
diEngine = lens _diEngine (\s a -> s {_diEngine = a})

-- | The number of CPU cores and the number of threads per core for the DB instance class of the DB instance.
diProcessorFeatures :: Lens' DBInstance [ProcessorFeature]
diProcessorFeatures = lens _diProcessorFeatures (\s a -> s {_diProcessorFeatures = a}) . _Default . _Coerce

-- | Specifies the latest time to which a database can be restored with point-in-time restore.
diLatestRestorableTime :: Lens' DBInstance (Maybe UTCTime)
diLatestRestorableTime = lens _diLatestRestorableTime (\s a -> s {_diLatestRestorableTime = a}) . mapping _Time

-- | Contains the name of the compute and memory capacity class of the DB instance.
diDBInstanceClass :: Lens' DBInstance (Maybe Text)
diDBInstanceClass = lens _diDBInstanceClass (\s a -> s {_diDBInstanceClass = a})

-- | A value that specifies the order in which an Aurora Replica is promoted to the primary instance after a failure of the existing primary instance. For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/Aurora.Managing.Backups.html#Aurora.Managing.FaultTolerance Fault Tolerance for an Aurora DB Cluster> in the /Amazon Aurora User Guide/ .
diPromotionTier :: Lens' DBInstance (Maybe Int)
diPromotionTier = lens _diPromotionTier (\s a -> s {_diPromotionTier = a})

-- | License model information for this DB instance.
diLicenseModel :: Lens' DBInstance (Maybe Text)
diLicenseModel = lens _diLicenseModel (\s a -> s {_diLicenseModel = a})

-- | Specifies the weekly time range during which system maintenance can occur, in Universal Coordinated Time (UTC).
diPreferredMaintenanceWindow :: Lens' DBInstance (Maybe Text)
diPreferredMaintenanceWindow = lens _diPreferredMaintenanceWindow (\s a -> s {_diPreferredMaintenanceWindow = a})

-- | The amount of time, in days, to retain Performance Insights data. Valid values are 7 or 731 (2 years).
diPerformanceInsightsRetentionPeriod :: Lens' DBInstance (Maybe Int)
diPerformanceInsightsRetentionPeriod = lens _diPerformanceInsightsRetentionPeriod (\s a -> s {_diPerformanceInsightsRetentionPeriod = a})

-- | The identifier of the CA certificate for this DB instance.
diCACertificateIdentifier :: Lens' DBInstance (Maybe Text)
diCACertificateIdentifier = lens _diCACertificateIdentifier (\s a -> s {_diCACertificateIdentifier = a})

-- | Contains a user-supplied database identifier. This identifier is the unique key that identifies a DB instance.
diDBInstanceIdentifier :: Lens' DBInstance (Maybe Text)
diDBInstanceIdentifier = lens _diDBInstanceIdentifier (\s a -> s {_diDBInstanceIdentifier = a})

-- | If present, specifies the name of the character set that this instance is associated with.
diCharacterSetName :: Lens' DBInstance (Maybe Text)
diCharacterSetName = lens _diCharacterSetName (\s a -> s {_diCharacterSetName = a})

-- | The upper limit to which Amazon RDS can automatically scale the storage of the DB instance.
diMaxAllocatedStorage :: Lens' DBInstance (Maybe Int)
diMaxAllocatedStorage = lens _diMaxAllocatedStorage (\s a -> s {_diMaxAllocatedStorage = a})

-- | If @StorageEncrypted@ is true, the AWS KMS key identifier for the encrypted DB instance.
diKMSKeyId :: Lens' DBInstance (Maybe Text)
diKMSKeyId = lens _diKMSKeyId (\s a -> s {_diKMSKeyId = a})

-- | Specifies the daily time range during which automated backups are created if automated backups are enabled, as determined by the @BackupRetentionPeriod@ .
diPreferredBackupWindow :: Lens' DBInstance (Maybe Text)
diPreferredBackupWindow = lens _diPreferredBackupWindow (\s a -> s {_diPreferredBackupWindow = a})

-- | The AWS Identity and Access Management (IAM) roles associated with the DB instance.
diAssociatedRoles :: Lens' DBInstance [DBInstanceRole]
diAssociatedRoles = lens _diAssociatedRoles (\s a -> s {_diAssociatedRoles = a}) . _Default . _Coerce

-- | Specifies the name of the Availability Zone the DB instance is located in.
diAvailabilityZone :: Lens' DBInstance (Maybe Text)
diAvailabilityZone = lens _diAvailabilityZone (\s a -> s {_diAvailabilityZone = a})

-- | Provides a list of VPC security group elements that the DB instance belongs to.
diVPCSecurityGroups :: Lens' DBInstance [VPCSecurityGroupMembership]
diVPCSecurityGroups = lens _diVPCSecurityGroups (\s a -> s {_diVPCSecurityGroups = a}) . _Default . _Coerce

-- | Specifies the number of days for which automatic DB snapshots are retained.
diBackupRetentionPeriod :: Lens' DBInstance (Maybe Int)
diBackupRetentionPeriod = lens _diBackupRetentionPeriod (\s a -> s {_diBackupRetentionPeriod = a})

-- | The name of the NCHAR character set for the Oracle DB instance. This character set specifies the Unicode encoding for data stored in table columns of type NCHAR, NCLOB, or NVARCHAR2.
diNcharCharacterSetName :: Lens' DBInstance (Maybe Text)
diNcharCharacterSetName = lens _diNcharCharacterSetName (\s a -> s {_diNcharCharacterSetName = a})

-- | The AWS KMS key identifier for encryption of Performance Insights data. The KMS key ID is the Amazon Resource Name (ARN), KMS key identifier, or the KMS key alias for the KMS encryption key.
diPerformanceInsightsKMSKeyId :: Lens' DBInstance (Maybe Text)
diPerformanceInsightsKMSKeyId = lens _diPerformanceInsightsKMSKeyId (\s a -> s {_diPerformanceInsightsKMSKeyId = a})

-- | Specifies information on the subnet group associated with the DB instance, including the name, description, and subnets in the subnet group.
diDBSubnetGroup :: Lens' DBInstance (Maybe DBSubnetGroup)
diDBSubnetGroup = lens _diDBSubnetGroup (\s a -> s {_diDBSubnetGroup = a})

-- | Specifies if the DB instance is a Multi-AZ deployment.
diMultiAZ :: Lens' DBInstance (Maybe Bool)
diMultiAZ = lens _diMultiAZ (\s a -> s {_diMultiAZ = a})

-- | Specifies the listener connection endpoint for SQL Server Always On.
diListenerEndpoint :: Lens' DBInstance (Maybe Endpoint)
diListenerEndpoint = lens _diListenerEndpoint (\s a -> s {_diListenerEndpoint = a})

-- | Provides the list of option group memberships for this DB instance.
diOptionGroupMemberships :: Lens' DBInstance [OptionGroupMembership]
diOptionGroupMemberships = lens _diOptionGroupMemberships (\s a -> s {_diOptionGroupMemberships = a}) . _Default . _Coerce

-- | A list of log types that this DB instance is configured to export to CloudWatch Logs. Log types vary by DB engine. For information about the log types for each DB engine, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_LogAccess.html Amazon RDS Database Log Files> in the /Amazon RDS User Guide./
diEnabledCloudwatchLogsExports :: Lens' DBInstance [Text]
diEnabledCloudwatchLogsExports = lens _diEnabledCloudwatchLogsExports (\s a -> s {_diEnabledCloudwatchLogsExports = a}) . _Default . _Coerce

-- | The Amazon Resource Name (ARN) of the Amazon CloudWatch Logs log stream that receives the Enhanced Monitoring metrics data for the DB instance.
diEnhancedMonitoringResourceARN :: Lens' DBInstance (Maybe Text)
diEnhancedMonitoringResourceARN = lens _diEnhancedMonitoringResourceARN (\s a -> s {_diEnhancedMonitoringResourceARN = a})

-- | If present, specifies the name of the secondary Availability Zone for a DB instance with multi-AZ support.
diSecondaryAvailabilityZone :: Lens' DBInstance (Maybe Text)
diSecondaryAvailabilityZone = lens _diSecondaryAvailabilityZone (\s a -> s {_diSecondaryAvailabilityZone = a})

-- | True if Performance Insights is enabled for the DB instance, and otherwise false.
diPerformanceInsightsEnabled :: Lens' DBInstance (Maybe Bool)
diPerformanceInsightsEnabled = lens _diPerformanceInsightsEnabled (\s a -> s {_diPerformanceInsightsEnabled = a})

-- | Specifies the allocated storage size specified in gibibytes.
diAllocatedStorage :: Lens' DBInstance (Maybe Int)
diAllocatedStorage = lens _diAllocatedStorage (\s a -> s {_diAllocatedStorage = a})

-- | The AWS Region-unique, immutable identifier for the DB instance. This identifier is found in AWS CloudTrail log entries whenever the AWS KMS key for the DB instance is accessed.
diDBiResourceId :: Lens' DBInstance (Maybe Text)
diDBiResourceId = lens _diDBiResourceId (\s a -> s {_diDBiResourceId = a})

-- | Provides the list of DB parameter groups applied to this DB instance.
diDBParameterGroups :: Lens' DBInstance [DBParameterGroupStatus]
diDBParameterGroups = lens _diDBParameterGroups (\s a -> s {_diDBParameterGroups = a}) . _Default . _Coerce

-- | Specifies whether tags are copied from the DB instance to snapshots of the DB instance. __Amazon Aurora__  Not applicable. Copying tags to snapshots is managed by the DB cluster. Setting this value for an Aurora DB instance has no effect on the DB cluster setting. For more information, see @DBCluster@ .
diCopyTagsToSnapshot :: Lens' DBInstance (Maybe Bool)
diCopyTagsToSnapshot = lens _diCopyTagsToSnapshot (\s a -> s {_diCopyTagsToSnapshot = a})

-- | The time zone of the DB instance. In most cases, the @Timezone@ element is empty. @Timezone@ content appears only for Microsoft SQL Server DB instances that were created with a time zone specified.
diTimezone :: Lens' DBInstance (Maybe Text)
diTimezone = lens _diTimezone (\s a -> s {_diTimezone = a})

-- | The ARN from the key store with which the instance is associated for TDE encryption.
diTDECredentialARN :: Lens' DBInstance (Maybe Text)
diTDECredentialARN = lens _diTDECredentialARN (\s a -> s {_diTDECredentialARN = a})

-- | Specifies the connection endpoint.
diEndpoint :: Lens' DBInstance (Maybe Endpoint)
diEndpoint = lens _diEndpoint (\s a -> s {_diEndpoint = a})

-- | Specifies the current state of this database. For information about DB instance statuses, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Overview.DBInstance.Status.html DB Instance Status> in the /Amazon RDS User Guide./
diDBInstanceStatus :: Lens' DBInstance (Maybe Text)
diDBInstanceStatus = lens _diDBInstanceStatus (\s a -> s {_diDBInstanceStatus = a})

-- | Specifies the port that the DB instance listens on. If the DB instance is part of a DB cluster, this can be a different port than the DB cluster port.
diDBInstancePort :: Lens' DBInstance (Maybe Int)
diDBInstancePort = lens _diDBInstancePort (\s a -> s {_diDBInstancePort = a})

-- | Specifies that changes to the DB instance are pending. This element is only included when changes are pending. Specific changes are identified by subelements.
diPendingModifiedValues :: Lens' DBInstance (Maybe PendingModifiedValues)
diPendingModifiedValues = lens _diPendingModifiedValues (\s a -> s {_diPendingModifiedValues = a})

-- | Contains one or more identifiers of Aurora DB clusters to which the RDS DB instance is replicated as a read replica. For example, when you create an Aurora read replica of an RDS MySQL DB instance, the Aurora MySQL DB cluster for the Aurora read replica is shown. This output does not contain information about cross region Aurora read replicas.
diReadReplicaDBClusterIdentifiers :: Lens' DBInstance [Text]
diReadReplicaDBClusterIdentifiers = lens _diReadReplicaDBClusterIdentifiers (\s a -> s {_diReadReplicaDBClusterIdentifiers = a}) . _Default . _Coerce

-- | Specifies the storage type associated with DB instance.
diStorageType :: Lens' DBInstance (Maybe Text)
diStorageType = lens _diStorageType (\s a -> s {_diStorageType = a})

-- | The status of a read replica. If the instance isn't a read replica, this is blank.
diStatusInfos :: Lens' DBInstance [DBInstanceStatusInfo]
diStatusInfos = lens _diStatusInfos (\s a -> s {_diStatusInfos = a}) . _Default . _Coerce

-- | The Active Directory Domain membership records associated with the DB instance.
diDomainMemberships :: Lens' DBInstance [DomainMembership]
diDomainMemberships = lens _diDomainMemberships (\s a -> s {_diDomainMemberships = a}) . _Default . _Coerce

-- | The meaning of this parameter differs according to the database engine you use. __MySQL, MariaDB, SQL Server, PostgreSQL__  Contains the name of the initial database of this instance that was provided at create time, if one was specified when the DB instance was created. This same name is returned for the life of the DB instance. Type: String __Oracle__  Contains the Oracle System ID (SID) of the created DB instance. Not shown when the returned parameters do not apply to an Oracle DB instance.
diDBName :: Lens' DBInstance (Maybe Text)
diDBName = lens _diDBName (\s a -> s {_diDBName = a})

instance FromXML DBInstance where
  parseXML x =
    DBInstance'
      <$> (x .@? "EngineVersion")
      <*> ( x .@? "DBSecurityGroups" .!@ mempty
              >>= may (parseXMLList "DBSecurityGroup")
          )
      <*> (x .@? "DeletionProtection")
      <*> (x .@? "StorageEncrypted")
      <*> (x .@? "DBClusterIdentifier")
      <*> (x .@? "PubliclyAccessible")
      <*> (x .@? "AutoMinorVersionUpgrade")
      <*> (x .@? "DBInstanceArn")
      <*> (x .@? "MasterUsername")
      <*> ( x .@? "ReadReplicaDBInstanceIdentifiers" .!@ mempty
              >>= may (parseXMLList "ReadReplicaDBInstanceIdentifier")
          )
      <*> (x .@? "IAMDatabaseAuthenticationEnabled")
      <*> (x .@? "MonitoringRoleArn")
      <*> (x .@? "Iops")
      <*> (x .@? "InstanceCreateTime")
      <*> (x .@? "TagList" .!@ mempty >>= may (parseXMLList "Tag"))
      <*> (x .@? "ReadReplicaSourceDBInstanceIdentifier")
      <*> (x .@? "ReplicaMode")
      <*> (x .@? "MonitoringInterval")
      <*> (x .@? "Engine")
      <*> ( x .@? "ProcessorFeatures" .!@ mempty
              >>= may (parseXMLList "ProcessorFeature")
          )
      <*> (x .@? "LatestRestorableTime")
      <*> (x .@? "DBInstanceClass")
      <*> (x .@? "PromotionTier")
      <*> (x .@? "LicenseModel")
      <*> (x .@? "PreferredMaintenanceWindow")
      <*> (x .@? "PerformanceInsightsRetentionPeriod")
      <*> (x .@? "CACertificateIdentifier")
      <*> (x .@? "DBInstanceIdentifier")
      <*> (x .@? "CharacterSetName")
      <*> (x .@? "MaxAllocatedStorage")
      <*> (x .@? "KmsKeyId")
      <*> (x .@? "PreferredBackupWindow")
      <*> ( x .@? "AssociatedRoles" .!@ mempty
              >>= may (parseXMLList "DBInstanceRole")
          )
      <*> (x .@? "AvailabilityZone")
      <*> ( x .@? "VpcSecurityGroups" .!@ mempty
              >>= may (parseXMLList "VpcSecurityGroupMembership")
          )
      <*> (x .@? "BackupRetentionPeriod")
      <*> (x .@? "NcharCharacterSetName")
      <*> (x .@? "PerformanceInsightsKMSKeyId")
      <*> (x .@? "DBSubnetGroup")
      <*> (x .@? "MultiAZ")
      <*> (x .@? "ListenerEndpoint")
      <*> ( x .@? "OptionGroupMemberships" .!@ mempty
              >>= may (parseXMLList "OptionGroupMembership")
          )
      <*> ( x .@? "EnabledCloudwatchLogsExports" .!@ mempty
              >>= may (parseXMLList "member")
          )
      <*> (x .@? "EnhancedMonitoringResourceArn")
      <*> (x .@? "SecondaryAvailabilityZone")
      <*> (x .@? "PerformanceInsightsEnabled")
      <*> (x .@? "AllocatedStorage")
      <*> (x .@? "DbiResourceId")
      <*> ( x .@? "DBParameterGroups" .!@ mempty
              >>= may (parseXMLList "DBParameterGroup")
          )
      <*> (x .@? "CopyTagsToSnapshot")
      <*> (x .@? "Timezone")
      <*> (x .@? "TdeCredentialArn")
      <*> (x .@? "Endpoint")
      <*> (x .@? "DBInstanceStatus")
      <*> (x .@? "DbInstancePort")
      <*> (x .@? "PendingModifiedValues")
      <*> ( x .@? "ReadReplicaDBClusterIdentifiers" .!@ mempty
              >>= may (parseXMLList "ReadReplicaDBClusterIdentifier")
          )
      <*> (x .@? "StorageType")
      <*> ( x .@? "StatusInfos" .!@ mempty
              >>= may (parseXMLList "DBInstanceStatusInfo")
          )
      <*> ( x .@? "DomainMemberships" .!@ mempty
              >>= may (parseXMLList "DomainMembership")
          )
      <*> (x .@? "DBName")

instance Hashable DBInstance

instance NFData DBInstance
