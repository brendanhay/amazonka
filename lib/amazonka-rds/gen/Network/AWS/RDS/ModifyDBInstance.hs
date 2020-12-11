{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.ModifyDBInstance
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies settings for a DB instance. You can change one or more database configuration parameters by specifying these parameters and the new values in the request. To learn what modifications you can make to your DB instance, call @DescribeValidDBInstanceModifications@ before you call @ModifyDBInstance@ .
module Network.AWS.RDS.ModifyDBInstance
  ( -- * Creating a request
    ModifyDBInstance (..),
    mkModifyDBInstance,

    -- ** Request lenses
    mdiEngineVersion,
    mdiDBSecurityGroups,
    mdiDBPortNumber,
    mdiDeletionProtection,
    mdiMasterUserPassword,
    mdiPubliclyAccessible,
    mdiAutoMinorVersionUpgrade,
    mdiDBSubnetGroupName,
    mdiMonitoringRoleARN,
    mdiIOPS,
    mdiAllowMajorVersionUpgrade,
    mdiNewDBInstanceIdentifier,
    mdiDomain,
    mdiReplicaMode,
    mdiMonitoringInterval,
    mdiCloudwatchLogsExportConfiguration,
    mdiCertificateRotationRestart,
    mdiTDECredentialPassword,
    mdiProcessorFeatures,
    mdiDBInstanceClass,
    mdiPromotionTier,
    mdiLicenseModel,
    mdiPreferredMaintenanceWindow,
    mdiPerformanceInsightsRetentionPeriod,
    mdiCACertificateIdentifier,
    mdiMaxAllocatedStorage,
    mdiEnablePerformanceInsights,
    mdiDBParameterGroupName,
    mdiPreferredBackupWindow,
    mdiBackupRetentionPeriod,
    mdiPerformanceInsightsKMSKeyId,
    mdiVPCSecurityGroupIds,
    mdiMultiAZ,
    mdiAllocatedStorage,
    mdiApplyImmediately,
    mdiOptionGroupName,
    mdiCopyTagsToSnapshot,
    mdiTDECredentialARN,
    mdiDomainIAMRoleName,
    mdiEnableIAMDatabaseAuthentication,
    mdiUseDefaultProcessorFeatures,
    mdiStorageType,
    mdiDBInstanceIdentifier,

    -- * Destructuring the response
    ModifyDBInstanceResponse (..),
    mkModifyDBInstanceResponse,

    -- ** Response lenses
    mdirsDBInstance,
    mdirsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.RDS.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- |
--
-- /See:/ 'mkModifyDBInstance' smart constructor.
data ModifyDBInstance = ModifyDBInstance'
  { engineVersion ::
      Lude.Maybe Lude.Text,
    dbSecurityGroups :: Lude.Maybe [Lude.Text],
    dbPortNumber :: Lude.Maybe Lude.Int,
    deletionProtection :: Lude.Maybe Lude.Bool,
    masterUserPassword :: Lude.Maybe Lude.Text,
    publiclyAccessible :: Lude.Maybe Lude.Bool,
    autoMinorVersionUpgrade :: Lude.Maybe Lude.Bool,
    dbSubnetGroupName :: Lude.Maybe Lude.Text,
    monitoringRoleARN :: Lude.Maybe Lude.Text,
    iops :: Lude.Maybe Lude.Int,
    allowMajorVersionUpgrade :: Lude.Maybe Lude.Bool,
    newDBInstanceIdentifier :: Lude.Maybe Lude.Text,
    domain :: Lude.Maybe Lude.Text,
    replicaMode :: Lude.Maybe ReplicaMode,
    monitoringInterval :: Lude.Maybe Lude.Int,
    cloudwatchLogsExportConfiguration ::
      Lude.Maybe CloudwatchLogsExportConfiguration,
    certificateRotationRestart :: Lude.Maybe Lude.Bool,
    tdeCredentialPassword :: Lude.Maybe Lude.Text,
    processorFeatures :: Lude.Maybe [ProcessorFeature],
    dbInstanceClass :: Lude.Maybe Lude.Text,
    promotionTier :: Lude.Maybe Lude.Int,
    licenseModel :: Lude.Maybe Lude.Text,
    preferredMaintenanceWindow :: Lude.Maybe Lude.Text,
    performanceInsightsRetentionPeriod :: Lude.Maybe Lude.Int,
    cACertificateIdentifier :: Lude.Maybe Lude.Text,
    maxAllocatedStorage :: Lude.Maybe Lude.Int,
    enablePerformanceInsights :: Lude.Maybe Lude.Bool,
    dbParameterGroupName :: Lude.Maybe Lude.Text,
    preferredBackupWindow :: Lude.Maybe Lude.Text,
    backupRetentionPeriod :: Lude.Maybe Lude.Int,
    performanceInsightsKMSKeyId :: Lude.Maybe Lude.Text,
    vpcSecurityGroupIds :: Lude.Maybe [Lude.Text],
    multiAZ :: Lude.Maybe Lude.Bool,
    allocatedStorage :: Lude.Maybe Lude.Int,
    applyImmediately :: Lude.Maybe Lude.Bool,
    optionGroupName :: Lude.Maybe Lude.Text,
    copyTagsToSnapshot :: Lude.Maybe Lude.Bool,
    tdeCredentialARN :: Lude.Maybe Lude.Text,
    domainIAMRoleName :: Lude.Maybe Lude.Text,
    enableIAMDatabaseAuthentication :: Lude.Maybe Lude.Bool,
    useDefaultProcessorFeatures :: Lude.Maybe Lude.Bool,
    storageType :: Lude.Maybe Lude.Text,
    dbInstanceIdentifier :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ModifyDBInstance' with the minimum fields required to make a request.
--
-- * 'allocatedStorage' - The new amount of storage (in gibibytes) to allocate for the DB instance.
--
-- For MariaDB, MySQL, Oracle, and PostgreSQL, the value supplied must be at least 10% greater than the current value. Values that are not at least 10% greater than the existing value are rounded up so that they are 10% greater than the current value.
-- For the valid values for allocated storage for each engine, see @CreateDBInstance@ .
-- * 'allowMajorVersionUpgrade' - A value that indicates whether major version upgrades are allowed. Changing this parameter doesn't result in an outage and the change is asynchronously applied as soon as possible.
--
-- Constraints: Major version upgrades must be allowed when specifying a value for the EngineVersion parameter that is a different major version than the DB instance's current version.
-- * 'applyImmediately' - A value that indicates whether the modifications in this request and any pending modifications are asynchronously applied as soon as possible, regardless of the @PreferredMaintenanceWindow@ setting for the DB instance. By default, this parameter is disabled.
--
-- If this parameter is disabled, changes to the DB instance are applied during the next maintenance window. Some parameter changes can cause an outage and are applied on the next call to 'RebootDBInstance' , or the next failure reboot. Review the table of parameters in <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Overview.DBInstance.Modifying.html Modifying a DB Instance> in the /Amazon RDS User Guide./ to see the impact of enabling or disabling @ApplyImmediately@ for each modified parameter and to determine when the changes are applied.
-- * 'autoMinorVersionUpgrade' - A value that indicates whether minor version upgrades are applied automatically to the DB instance during the maintenance window. Changing this parameter doesn't result in an outage except in the following case and the change is asynchronously applied as soon as possible. An outage results if this parameter is enabled during the maintenance window, and a newer minor version is available, and RDS has enabled auto patching for that engine version.
-- * 'backupRetentionPeriod' - The number of days to retain automated backups. Setting this parameter to a positive number enables backups. Setting this parameter to 0 disables automated backups.
--
-- Changing this parameter can result in an outage if you change from 0 to a non-zero value or from a non-zero value to 0. These changes are applied during the next maintenance window unless the @ApplyImmediately@ parameter is enabled for this request. If you change the parameter from one non-zero value to another non-zero value, the change is asynchronously applied as soon as possible.
-- __Amazon Aurora__
-- Not applicable. The retention period for automated backups is managed by the DB cluster. For more information, see @ModifyDBCluster@ .
-- Default: Uses existing setting
-- Constraints:
--
--     * Must be a value from 0 to 35
--
--
--     * Can be specified for a MySQL read replica only if the source is running MySQL 5.6 or later
--
--
--     * Can be specified for a PostgreSQL read replica only if the source is running PostgreSQL 9.3.5
--
--
--     * Can't be set to 0 if the DB instance is a source to read replicas
--
--
-- * 'cACertificateIdentifier' - Indicates the certificate that needs to be associated with the instance.
-- * 'certificateRotationRestart' - A value that indicates whether the DB instance is restarted when you rotate your SSL/TLS certificate.
--
-- By default, the DB instance is restarted when you rotate your SSL/TLS certificate. The certificate is not updated until the DB instance is restarted.
-- /Important:/ Set this parameter only if you are /not/ using SSL/TLS to connect to the DB instance.
-- If you are using SSL/TLS to connect to the DB instance, follow the appropriate instructions for your DB engine to rotate your SSL/TLS certificate:
--
--     * For more information about rotating your SSL/TLS certificate for RDS DB engines, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/UsingWithRDS.SSL-certificate-rotation.html Rotating Your SSL/TLS Certificate.> in the /Amazon RDS User Guide./
--
--
--     * For more information about rotating your SSL/TLS certificate for Aurora DB engines, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/UsingWithRDS.SSL-certificate-rotation.html Rotating Your SSL/TLS Certificate> in the /Amazon Aurora User Guide./
--
--
-- * 'cloudwatchLogsExportConfiguration' - The configuration setting for the log types to be enabled for export to CloudWatch Logs for a specific DB instance.
--
-- A change to the @CloudwatchLogsExportConfiguration@ parameter is always applied to the DB instance immediately. Therefore, the @ApplyImmediately@ parameter has no effect.
-- * 'copyTagsToSnapshot' - A value that indicates whether to copy all tags from the DB instance to snapshots of the DB instance. By default, tags are not copied.
--
-- __Amazon Aurora__
-- Not applicable. Copying tags to snapshots is managed by the DB cluster. Setting this value for an Aurora DB instance has no effect on the DB cluster setting. For more information, see @ModifyDBCluster@ .
-- * 'dbInstanceClass' - The new compute and memory capacity of the DB instance, for example, @db.m4.large@ . Not all DB instance classes are available in all AWS Regions, or for all database engines. For the full list of DB instance classes, and availability for your engine, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Concepts.DBInstanceClass.html DB Instance Class> in the /Amazon RDS User Guide./
--
-- If you modify the DB instance class, an outage occurs during the change. The change is applied during the next maintenance window, unless @ApplyImmediately@ is enabled for this request.
-- Default: Uses existing setting
-- * 'dbInstanceIdentifier' - The DB instance identifier. This value is stored as a lowercase string.
--
-- Constraints:
--
--     * Must match the identifier of an existing DBInstance.
--
--
-- * 'dbParameterGroupName' - The name of the DB parameter group to apply to the DB instance. Changing this setting doesn't result in an outage. The parameter group name itself is changed immediately, but the actual parameter changes are not applied until you reboot the instance without failover. In this case, the DB instance isn't rebooted automatically and the parameter changes isn't applied during the next maintenance window.
--
-- Default: Uses existing setting
-- Constraints: The DB parameter group must be in the same DB parameter group family as this DB instance.
-- * 'dbPortNumber' - The port number on which the database accepts connections.
--
-- The value of the @DBPortNumber@ parameter must not match any of the port values specified for options in the option group for the DB instance.
-- Your database will restart when you change the @DBPortNumber@ value regardless of the value of the @ApplyImmediately@ parameter.
-- __MySQL__
-- Default: @3306@
-- Valid values: @1150-65535@
-- __MariaDB__
-- Default: @3306@
-- Valid values: @1150-65535@
-- __PostgreSQL__
-- Default: @5432@
-- Valid values: @1150-65535@
-- Type: Integer
-- __Oracle__
-- Default: @1521@
-- Valid values: @1150-65535@
-- __SQL Server__
-- Default: @1433@
-- Valid values: @1150-65535@ except @1234@ , @1434@ , @3260@ , @3343@ , @3389@ , @47001@ , and @49152-49156@ .
-- __Amazon Aurora__
-- Default: @3306@
-- Valid values: @1150-65535@
-- * 'dbSecurityGroups' - A list of DB security groups to authorize on this DB instance. Changing this setting doesn't result in an outage and the change is asynchronously applied as soon as possible.
--
-- Constraints:
--
--     * If supplied, must match existing DBSecurityGroups.
--
--
-- * 'dbSubnetGroupName' - The new DB subnet group for the DB instance. You can use this parameter to move your DB instance to a different VPC. If your DB instance isn't in a VPC, you can also use this parameter to move your DB instance into a VPC. For more information, see <http://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_VPC.WorkingWithRDSInstanceinaVPC.html#USER_VPC.Non-VPC2VPC Updating the VPC for a DB Instance> in the /Amazon RDS User Guide./
--
-- Changing the subnet group causes an outage during the change. The change is applied during the next maintenance window, unless you enable @ApplyImmediately@ .
-- Constraints: If supplied, must match the name of an existing DBSubnetGroup.
-- Example: @mySubnetGroup@
-- * 'deletionProtection' - A value that indicates whether the DB instance has deletion protection enabled. The database can't be deleted when deletion protection is enabled. By default, deletion protection is disabled. For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_DeleteInstance.html Deleting a DB Instance> .
-- * 'domain' - The Active Directory directory ID to move the DB instance to. Specify @none@ to remove the instance from its current domain. The domain must be created prior to this operation. Currently, only MySQL, Microsoft SQL Server, Oracle, and PostgreSQL DB instances can be created in an Active Directory Domain.
--
-- For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/kerberos-authentication.html Kerberos Authentication> in the /Amazon RDS User Guide/ .
-- * 'domainIAMRoleName' - The name of the IAM role to use when making API calls to the Directory Service.
-- * 'enableIAMDatabaseAuthentication' - A value that indicates whether to enable mapping of AWS Identity and Access Management (IAM) accounts to database accounts. By default, mapping is disabled.
--
-- This setting doesn't apply to Amazon Aurora. Mapping AWS IAM accounts to database accounts is managed by the DB cluster.
-- For more information about IAM database authentication, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/UsingWithRDS.IAMDBAuth.html IAM Database Authentication for MySQL and PostgreSQL> in the /Amazon RDS User Guide./
-- * 'enablePerformanceInsights' - A value that indicates whether to enable Performance Insights for the DB instance.
--
-- For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_PerfInsights.html Using Amazon Performance Insights> in the /Amazon Relational Database Service User Guide/ .
-- * 'engineVersion' - The version number of the database engine to upgrade to. Changing this parameter results in an outage and the change is applied during the next maintenance window unless the @ApplyImmediately@ parameter is enabled for this request.
--
-- For major version upgrades, if a nondefault DB parameter group is currently in use, a new DB parameter group in the DB parameter group family for the new engine version must be specified. The new DB parameter group can be the default for that DB parameter group family.
-- For information about valid engine versions, see @CreateDBInstance@ , or call @DescribeDBEngineVersions@ .
-- * 'iops' - The new Provisioned IOPS (I/O operations per second) value for the RDS instance.
--
-- Changing this setting doesn't result in an outage and the change is applied during the next maintenance window unless the @ApplyImmediately@ parameter is enabled for this request. If you are migrating from Provisioned IOPS to standard storage, set this value to 0. The DB instance will require a reboot for the change in storage type to take effect.
-- If you choose to migrate your DB instance from using standard storage to using Provisioned IOPS, or from using Provisioned IOPS to using standard storage, the process can take time. The duration of the migration depends on several factors such as database load, storage size, storage type (standard or Provisioned IOPS), amount of IOPS provisioned (if any), and the number of prior scale storage operations. Typical migration times are under 24 hours, but the process can take up to several days in some cases. During the migration, the DB instance is available for use, but might experience performance degradation. While the migration takes place, nightly backups for the instance are suspended. No other Amazon RDS operations can take place for the instance, including modifying the instance, rebooting the instance, deleting the instance, creating a read replica for the instance, and creating a DB snapshot of the instance.
-- Constraints: For MariaDB, MySQL, Oracle, and PostgreSQL, the value supplied must be at least 10% greater than the current value. Values that are not at least 10% greater than the existing value are rounded up so that they are 10% greater than the current value.
-- Default: Uses existing setting
-- * 'licenseModel' - The license model for the DB instance.
--
-- Valid values: @license-included@ | @bring-your-own-license@ | @general-public-license@
-- * 'masterUserPassword' - The new password for the master user. The password can include any printable ASCII character except "/", """, or "@".
--
-- Changing this parameter doesn't result in an outage and the change is asynchronously applied as soon as possible. Between the time of the request and the completion of the request, the @MasterUserPassword@ element exists in the @PendingModifiedValues@ element of the operation response.
-- __Amazon Aurora__
-- Not applicable. The password for the master user is managed by the DB cluster. For more information, see @ModifyDBCluster@ .
-- Default: Uses existing setting
-- __MariaDB__
-- Constraints: Must contain from 8 to 41 characters.
-- __Microsoft SQL Server__
-- Constraints: Must contain from 8 to 128 characters.
-- __MySQL__
-- Constraints: Must contain from 8 to 41 characters.
-- __Oracle__
-- Constraints: Must contain from 8 to 30 characters.
-- __PostgreSQL__
-- Constraints: Must contain from 8 to 128 characters.
-- * 'maxAllocatedStorage' - The upper limit to which Amazon RDS can automatically scale the storage of the DB instance.
-- * 'monitoringInterval' - The interval, in seconds, between points when Enhanced Monitoring metrics are collected for the DB instance. To disable collecting Enhanced Monitoring metrics, specify 0. The default is 0.
--
-- If @MonitoringRoleArn@ is specified, then you must also set @MonitoringInterval@ to a value other than 0.
-- Valid Values: @0, 1, 5, 10, 15, 30, 60@
-- * 'monitoringRoleARN' - The ARN for the IAM role that permits RDS to send enhanced monitoring metrics to Amazon CloudWatch Logs. For example, @arn:aws:iam:123456789012:role/emaccess@ . For information on creating a monitoring role, go to <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_Monitoring.html#USER_Monitoring.OS.IAMRole To create an IAM role for Amazon RDS Enhanced Monitoring> in the /Amazon RDS User Guide./
--
-- If @MonitoringInterval@ is set to a value other than 0, then you must supply a @MonitoringRoleArn@ value.
-- * 'multiAZ' - A value that indicates whether the DB instance is a Multi-AZ deployment. Changing this parameter doesn't result in an outage and the change is applied during the next maintenance window unless the @ApplyImmediately@ parameter is enabled for this request.
-- * 'newDBInstanceIdentifier' - The new DB instance identifier for the DB instance when renaming a DB instance. When you change the DB instance identifier, an instance reboot occurs immediately if you enable @ApplyImmediately@ , or will occur during the next maintenance window if you disable Apply Immediately. This value is stored as a lowercase string.
--
-- Constraints:
--
--     * Must contain from 1 to 63 letters, numbers, or hyphens.
--
--
--     * The first character must be a letter.
--
--
--     * Can't end with a hyphen or contain two consecutive hyphens.
--
--
-- Example: @mydbinstance@
-- * 'optionGroupName' - Indicates that the DB instance should be associated with the specified option group. Changing this parameter doesn't result in an outage except in the following case and the change is applied during the next maintenance window unless the @ApplyImmediately@ parameter is enabled for this request. If the parameter change results in an option group that enables OEM, this change can cause a brief (sub-second) period during which new connections are rejected but existing connections are not interrupted.
--
-- Permanent options, such as the TDE option for Oracle Advanced Security TDE, can't be removed from an option group, and that option group can't be removed from a DB instance once it is associated with a DB instance
-- * 'performanceInsightsKMSKeyId' - The AWS KMS key identifier for encryption of Performance Insights data. The KMS key ID is the Amazon Resource Name (ARN), KMS key identifier, or the KMS key alias for the KMS encryption key.
--
-- If you do not specify a value for @PerformanceInsightsKMSKeyId@ , then Amazon RDS uses your default encryption key. AWS KMS creates the default encryption key for your AWS account. Your AWS account has a different default encryption key for each AWS Region.
-- * 'performanceInsightsRetentionPeriod' - The amount of time, in days, to retain Performance Insights data. Valid values are 7 or 731 (2 years).
-- * 'preferredBackupWindow' - The daily time range during which automated backups are created if automated backups are enabled, as determined by the @BackupRetentionPeriod@ parameter. Changing this parameter doesn't result in an outage and the change is asynchronously applied as soon as possible.
--
-- __Amazon Aurora__
-- Not applicable. The daily time range for creating automated backups is managed by the DB cluster. For more information, see @ModifyDBCluster@ .
-- Constraints:
--
--     * Must be in the format hh24:mi-hh24:mi
--
--
--     * Must be in Universal Time Coordinated (UTC)
--
--
--     * Must not conflict with the preferred maintenance window
--
--
--     * Must be at least 30 minutes
--
--
-- * 'preferredMaintenanceWindow' - The weekly time range (in UTC) during which system maintenance can occur, which might result in an outage. Changing this parameter doesn't result in an outage, except in the following situation, and the change is asynchronously applied as soon as possible. If there are pending actions that cause a reboot, and the maintenance window is changed to include the current time, then changing this parameter will cause a reboot of the DB instance. If moving this window to the current time, there must be at least 30 minutes between the current time and end of the window to ensure pending changes are applied.
--
-- Default: Uses existing setting
-- Format: ddd:hh24:mi-ddd:hh24:mi
-- Valid Days: Mon | Tue | Wed | Thu | Fri | Sat | Sun
-- Constraints: Must be at least 30 minutes
-- * 'processorFeatures' - The number of CPU cores and the number of threads per core for the DB instance class of the DB instance.
-- * 'promotionTier' - A value that specifies the order in which an Aurora Replica is promoted to the primary instance after a failure of the existing primary instance. For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/Aurora.Managing.Backups.html#Aurora.Managing.FaultTolerance Fault Tolerance for an Aurora DB Cluster> in the /Amazon Aurora User Guide/ .
--
-- Default: 1
-- Valid Values: 0 - 15
-- * 'publiclyAccessible' - A value that indicates whether the DB instance is publicly accessible.
--
-- When the DB instance is publicly accessible, its DNS endpoint resolves to the private IP address from within the DB instance's VPC, and to the public IP address from outside of the DB instance's VPC. Access to the DB instance is ultimately controlled by the security group it uses, and that public access is not permitted if the security group assigned to the DB instance doesn't permit it.
-- When the DB instance isn't publicly accessible, it is an internal DB instance with a DNS name that resolves to a private IP address.
-- @PubliclyAccessible@ only applies to DB instances in a VPC. The DB instance must be part of a public subnet and @PubliclyAccessible@ must be enabled for it to be publicly accessible.
-- Changes to the @PubliclyAccessible@ parameter are applied immediately regardless of the value of the @ApplyImmediately@ parameter.
-- * 'replicaMode' - A value that sets the open mode of a replica database to either mounted or read-only.
--
-- Mounted DB replicas are included in Oracle Enterprise Edition. The main use case for mounted replicas is cross-Region disaster recovery. The primary database doesn't use Active Data Guard to transmit information to the mounted replica. Because it doesn't accept user connections, a mounted replica can't serve a read-only workload. For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/oracle-read-replicas.html Working with Oracle Read Replicas for Amazon RDS> in the /Amazon RDS User Guide/ .
-- * 'storageType' - Specifies the storage type to be associated with the DB instance.
--
-- If you specify Provisioned IOPS (@io1@ ), you must also include a value for the @Iops@ parameter.
-- If you choose to migrate your DB instance from using standard storage to using Provisioned IOPS, or from using Provisioned IOPS to using standard storage, the process can take time. The duration of the migration depends on several factors such as database load, storage size, storage type (standard or Provisioned IOPS), amount of IOPS provisioned (if any), and the number of prior scale storage operations. Typical migration times are under 24 hours, but the process can take up to several days in some cases. During the migration, the DB instance is available for use, but might experience performance degradation. While the migration takes place, nightly backups for the instance are suspended. No other Amazon RDS operations can take place for the instance, including modifying the instance, rebooting the instance, deleting the instance, creating a read replica for the instance, and creating a DB snapshot of the instance.
-- Valid values: @standard | gp2 | io1@
-- Default: @io1@ if the @Iops@ parameter is specified, otherwise @gp2@
-- * 'tdeCredentialARN' - The ARN from the key store with which to associate the instance for TDE encryption.
-- * 'tdeCredentialPassword' - The password for the given ARN from the key store in order to access the device.
-- * 'useDefaultProcessorFeatures' - A value that indicates whether the DB instance class of the DB instance uses its default processor features.
-- * 'vpcSecurityGroupIds' - A list of EC2 VPC security groups to authorize on this DB instance. This change is asynchronously applied as soon as possible.
--
-- __Amazon Aurora__
-- Not applicable. The associated list of EC2 VPC security groups is managed by the DB cluster. For more information, see @ModifyDBCluster@ .
-- Constraints:
--
--     * If supplied, must match existing VpcSecurityGroupIds.
mkModifyDBInstance ::
  -- | 'dbInstanceIdentifier'
  Lude.Text ->
  ModifyDBInstance
mkModifyDBInstance pDBInstanceIdentifier_ =
  ModifyDBInstance'
    { engineVersion = Lude.Nothing,
      dbSecurityGroups = Lude.Nothing,
      dbPortNumber = Lude.Nothing,
      deletionProtection = Lude.Nothing,
      masterUserPassword = Lude.Nothing,
      publiclyAccessible = Lude.Nothing,
      autoMinorVersionUpgrade = Lude.Nothing,
      dbSubnetGroupName = Lude.Nothing,
      monitoringRoleARN = Lude.Nothing,
      iops = Lude.Nothing,
      allowMajorVersionUpgrade = Lude.Nothing,
      newDBInstanceIdentifier = Lude.Nothing,
      domain = Lude.Nothing,
      replicaMode = Lude.Nothing,
      monitoringInterval = Lude.Nothing,
      cloudwatchLogsExportConfiguration = Lude.Nothing,
      certificateRotationRestart = Lude.Nothing,
      tdeCredentialPassword = Lude.Nothing,
      processorFeatures = Lude.Nothing,
      dbInstanceClass = Lude.Nothing,
      promotionTier = Lude.Nothing,
      licenseModel = Lude.Nothing,
      preferredMaintenanceWindow = Lude.Nothing,
      performanceInsightsRetentionPeriod = Lude.Nothing,
      cACertificateIdentifier = Lude.Nothing,
      maxAllocatedStorage = Lude.Nothing,
      enablePerformanceInsights = Lude.Nothing,
      dbParameterGroupName = Lude.Nothing,
      preferredBackupWindow = Lude.Nothing,
      backupRetentionPeriod = Lude.Nothing,
      performanceInsightsKMSKeyId = Lude.Nothing,
      vpcSecurityGroupIds = Lude.Nothing,
      multiAZ = Lude.Nothing,
      allocatedStorage = Lude.Nothing,
      applyImmediately = Lude.Nothing,
      optionGroupName = Lude.Nothing,
      copyTagsToSnapshot = Lude.Nothing,
      tdeCredentialARN = Lude.Nothing,
      domainIAMRoleName = Lude.Nothing,
      enableIAMDatabaseAuthentication = Lude.Nothing,
      useDefaultProcessorFeatures = Lude.Nothing,
      storageType = Lude.Nothing,
      dbInstanceIdentifier = pDBInstanceIdentifier_
    }

-- | The version number of the database engine to upgrade to. Changing this parameter results in an outage and the change is applied during the next maintenance window unless the @ApplyImmediately@ parameter is enabled for this request.
--
-- For major version upgrades, if a nondefault DB parameter group is currently in use, a new DB parameter group in the DB parameter group family for the new engine version must be specified. The new DB parameter group can be the default for that DB parameter group family.
-- For information about valid engine versions, see @CreateDBInstance@ , or call @DescribeDBEngineVersions@ .
--
-- /Note:/ Consider using 'engineVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdiEngineVersion :: Lens.Lens' ModifyDBInstance (Lude.Maybe Lude.Text)
mdiEngineVersion = Lens.lens (engineVersion :: ModifyDBInstance -> Lude.Maybe Lude.Text) (\s a -> s {engineVersion = a} :: ModifyDBInstance)
{-# DEPRECATED mdiEngineVersion "Use generic-lens or generic-optics with 'engineVersion' instead." #-}

-- | A list of DB security groups to authorize on this DB instance. Changing this setting doesn't result in an outage and the change is asynchronously applied as soon as possible.
--
-- Constraints:
--
--     * If supplied, must match existing DBSecurityGroups.
--
--
--
-- /Note:/ Consider using 'dbSecurityGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdiDBSecurityGroups :: Lens.Lens' ModifyDBInstance (Lude.Maybe [Lude.Text])
mdiDBSecurityGroups = Lens.lens (dbSecurityGroups :: ModifyDBInstance -> Lude.Maybe [Lude.Text]) (\s a -> s {dbSecurityGroups = a} :: ModifyDBInstance)
{-# DEPRECATED mdiDBSecurityGroups "Use generic-lens or generic-optics with 'dbSecurityGroups' instead." #-}

-- | The port number on which the database accepts connections.
--
-- The value of the @DBPortNumber@ parameter must not match any of the port values specified for options in the option group for the DB instance.
-- Your database will restart when you change the @DBPortNumber@ value regardless of the value of the @ApplyImmediately@ parameter.
-- __MySQL__
-- Default: @3306@
-- Valid values: @1150-65535@
-- __MariaDB__
-- Default: @3306@
-- Valid values: @1150-65535@
-- __PostgreSQL__
-- Default: @5432@
-- Valid values: @1150-65535@
-- Type: Integer
-- __Oracle__
-- Default: @1521@
-- Valid values: @1150-65535@
-- __SQL Server__
-- Default: @1433@
-- Valid values: @1150-65535@ except @1234@ , @1434@ , @3260@ , @3343@ , @3389@ , @47001@ , and @49152-49156@ .
-- __Amazon Aurora__
-- Default: @3306@
-- Valid values: @1150-65535@
--
-- /Note:/ Consider using 'dbPortNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdiDBPortNumber :: Lens.Lens' ModifyDBInstance (Lude.Maybe Lude.Int)
mdiDBPortNumber = Lens.lens (dbPortNumber :: ModifyDBInstance -> Lude.Maybe Lude.Int) (\s a -> s {dbPortNumber = a} :: ModifyDBInstance)
{-# DEPRECATED mdiDBPortNumber "Use generic-lens or generic-optics with 'dbPortNumber' instead." #-}

-- | A value that indicates whether the DB instance has deletion protection enabled. The database can't be deleted when deletion protection is enabled. By default, deletion protection is disabled. For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_DeleteInstance.html Deleting a DB Instance> .
--
-- /Note:/ Consider using 'deletionProtection' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdiDeletionProtection :: Lens.Lens' ModifyDBInstance (Lude.Maybe Lude.Bool)
mdiDeletionProtection = Lens.lens (deletionProtection :: ModifyDBInstance -> Lude.Maybe Lude.Bool) (\s a -> s {deletionProtection = a} :: ModifyDBInstance)
{-# DEPRECATED mdiDeletionProtection "Use generic-lens or generic-optics with 'deletionProtection' instead." #-}

-- | The new password for the master user. The password can include any printable ASCII character except "/", """, or "@".
--
-- Changing this parameter doesn't result in an outage and the change is asynchronously applied as soon as possible. Between the time of the request and the completion of the request, the @MasterUserPassword@ element exists in the @PendingModifiedValues@ element of the operation response.
-- __Amazon Aurora__
-- Not applicable. The password for the master user is managed by the DB cluster. For more information, see @ModifyDBCluster@ .
-- Default: Uses existing setting
-- __MariaDB__
-- Constraints: Must contain from 8 to 41 characters.
-- __Microsoft SQL Server__
-- Constraints: Must contain from 8 to 128 characters.
-- __MySQL__
-- Constraints: Must contain from 8 to 41 characters.
-- __Oracle__
-- Constraints: Must contain from 8 to 30 characters.
-- __PostgreSQL__
-- Constraints: Must contain from 8 to 128 characters.
--
-- /Note:/ Consider using 'masterUserPassword' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdiMasterUserPassword :: Lens.Lens' ModifyDBInstance (Lude.Maybe Lude.Text)
mdiMasterUserPassword = Lens.lens (masterUserPassword :: ModifyDBInstance -> Lude.Maybe Lude.Text) (\s a -> s {masterUserPassword = a} :: ModifyDBInstance)
{-# DEPRECATED mdiMasterUserPassword "Use generic-lens or generic-optics with 'masterUserPassword' instead." #-}

-- | A value that indicates whether the DB instance is publicly accessible.
--
-- When the DB instance is publicly accessible, its DNS endpoint resolves to the private IP address from within the DB instance's VPC, and to the public IP address from outside of the DB instance's VPC. Access to the DB instance is ultimately controlled by the security group it uses, and that public access is not permitted if the security group assigned to the DB instance doesn't permit it.
-- When the DB instance isn't publicly accessible, it is an internal DB instance with a DNS name that resolves to a private IP address.
-- @PubliclyAccessible@ only applies to DB instances in a VPC. The DB instance must be part of a public subnet and @PubliclyAccessible@ must be enabled for it to be publicly accessible.
-- Changes to the @PubliclyAccessible@ parameter are applied immediately regardless of the value of the @ApplyImmediately@ parameter.
--
-- /Note:/ Consider using 'publiclyAccessible' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdiPubliclyAccessible :: Lens.Lens' ModifyDBInstance (Lude.Maybe Lude.Bool)
mdiPubliclyAccessible = Lens.lens (publiclyAccessible :: ModifyDBInstance -> Lude.Maybe Lude.Bool) (\s a -> s {publiclyAccessible = a} :: ModifyDBInstance)
{-# DEPRECATED mdiPubliclyAccessible "Use generic-lens or generic-optics with 'publiclyAccessible' instead." #-}

-- | A value that indicates whether minor version upgrades are applied automatically to the DB instance during the maintenance window. Changing this parameter doesn't result in an outage except in the following case and the change is asynchronously applied as soon as possible. An outage results if this parameter is enabled during the maintenance window, and a newer minor version is available, and RDS has enabled auto patching for that engine version.
--
-- /Note:/ Consider using 'autoMinorVersionUpgrade' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdiAutoMinorVersionUpgrade :: Lens.Lens' ModifyDBInstance (Lude.Maybe Lude.Bool)
mdiAutoMinorVersionUpgrade = Lens.lens (autoMinorVersionUpgrade :: ModifyDBInstance -> Lude.Maybe Lude.Bool) (\s a -> s {autoMinorVersionUpgrade = a} :: ModifyDBInstance)
{-# DEPRECATED mdiAutoMinorVersionUpgrade "Use generic-lens or generic-optics with 'autoMinorVersionUpgrade' instead." #-}

-- | The new DB subnet group for the DB instance. You can use this parameter to move your DB instance to a different VPC. If your DB instance isn't in a VPC, you can also use this parameter to move your DB instance into a VPC. For more information, see <http://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_VPC.WorkingWithRDSInstanceinaVPC.html#USER_VPC.Non-VPC2VPC Updating the VPC for a DB Instance> in the /Amazon RDS User Guide./
--
-- Changing the subnet group causes an outage during the change. The change is applied during the next maintenance window, unless you enable @ApplyImmediately@ .
-- Constraints: If supplied, must match the name of an existing DBSubnetGroup.
-- Example: @mySubnetGroup@
--
-- /Note:/ Consider using 'dbSubnetGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdiDBSubnetGroupName :: Lens.Lens' ModifyDBInstance (Lude.Maybe Lude.Text)
mdiDBSubnetGroupName = Lens.lens (dbSubnetGroupName :: ModifyDBInstance -> Lude.Maybe Lude.Text) (\s a -> s {dbSubnetGroupName = a} :: ModifyDBInstance)
{-# DEPRECATED mdiDBSubnetGroupName "Use generic-lens or generic-optics with 'dbSubnetGroupName' instead." #-}

-- | The ARN for the IAM role that permits RDS to send enhanced monitoring metrics to Amazon CloudWatch Logs. For example, @arn:aws:iam:123456789012:role/emaccess@ . For information on creating a monitoring role, go to <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_Monitoring.html#USER_Monitoring.OS.IAMRole To create an IAM role for Amazon RDS Enhanced Monitoring> in the /Amazon RDS User Guide./
--
-- If @MonitoringInterval@ is set to a value other than 0, then you must supply a @MonitoringRoleArn@ value.
--
-- /Note:/ Consider using 'monitoringRoleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdiMonitoringRoleARN :: Lens.Lens' ModifyDBInstance (Lude.Maybe Lude.Text)
mdiMonitoringRoleARN = Lens.lens (monitoringRoleARN :: ModifyDBInstance -> Lude.Maybe Lude.Text) (\s a -> s {monitoringRoleARN = a} :: ModifyDBInstance)
{-# DEPRECATED mdiMonitoringRoleARN "Use generic-lens or generic-optics with 'monitoringRoleARN' instead." #-}

-- | The new Provisioned IOPS (I/O operations per second) value for the RDS instance.
--
-- Changing this setting doesn't result in an outage and the change is applied during the next maintenance window unless the @ApplyImmediately@ parameter is enabled for this request. If you are migrating from Provisioned IOPS to standard storage, set this value to 0. The DB instance will require a reboot for the change in storage type to take effect.
-- If you choose to migrate your DB instance from using standard storage to using Provisioned IOPS, or from using Provisioned IOPS to using standard storage, the process can take time. The duration of the migration depends on several factors such as database load, storage size, storage type (standard or Provisioned IOPS), amount of IOPS provisioned (if any), and the number of prior scale storage operations. Typical migration times are under 24 hours, but the process can take up to several days in some cases. During the migration, the DB instance is available for use, but might experience performance degradation. While the migration takes place, nightly backups for the instance are suspended. No other Amazon RDS operations can take place for the instance, including modifying the instance, rebooting the instance, deleting the instance, creating a read replica for the instance, and creating a DB snapshot of the instance.
-- Constraints: For MariaDB, MySQL, Oracle, and PostgreSQL, the value supplied must be at least 10% greater than the current value. Values that are not at least 10% greater than the existing value are rounded up so that they are 10% greater than the current value.
-- Default: Uses existing setting
--
-- /Note:/ Consider using 'iops' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdiIOPS :: Lens.Lens' ModifyDBInstance (Lude.Maybe Lude.Int)
mdiIOPS = Lens.lens (iops :: ModifyDBInstance -> Lude.Maybe Lude.Int) (\s a -> s {iops = a} :: ModifyDBInstance)
{-# DEPRECATED mdiIOPS "Use generic-lens or generic-optics with 'iops' instead." #-}

-- | A value that indicates whether major version upgrades are allowed. Changing this parameter doesn't result in an outage and the change is asynchronously applied as soon as possible.
--
-- Constraints: Major version upgrades must be allowed when specifying a value for the EngineVersion parameter that is a different major version than the DB instance's current version.
--
-- /Note:/ Consider using 'allowMajorVersionUpgrade' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdiAllowMajorVersionUpgrade :: Lens.Lens' ModifyDBInstance (Lude.Maybe Lude.Bool)
mdiAllowMajorVersionUpgrade = Lens.lens (allowMajorVersionUpgrade :: ModifyDBInstance -> Lude.Maybe Lude.Bool) (\s a -> s {allowMajorVersionUpgrade = a} :: ModifyDBInstance)
{-# DEPRECATED mdiAllowMajorVersionUpgrade "Use generic-lens or generic-optics with 'allowMajorVersionUpgrade' instead." #-}

-- | The new DB instance identifier for the DB instance when renaming a DB instance. When you change the DB instance identifier, an instance reboot occurs immediately if you enable @ApplyImmediately@ , or will occur during the next maintenance window if you disable Apply Immediately. This value is stored as a lowercase string.
--
-- Constraints:
--
--     * Must contain from 1 to 63 letters, numbers, or hyphens.
--
--
--     * The first character must be a letter.
--
--
--     * Can't end with a hyphen or contain two consecutive hyphens.
--
--
-- Example: @mydbinstance@
--
-- /Note:/ Consider using 'newDBInstanceIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdiNewDBInstanceIdentifier :: Lens.Lens' ModifyDBInstance (Lude.Maybe Lude.Text)
mdiNewDBInstanceIdentifier = Lens.lens (newDBInstanceIdentifier :: ModifyDBInstance -> Lude.Maybe Lude.Text) (\s a -> s {newDBInstanceIdentifier = a} :: ModifyDBInstance)
{-# DEPRECATED mdiNewDBInstanceIdentifier "Use generic-lens or generic-optics with 'newDBInstanceIdentifier' instead." #-}

-- | The Active Directory directory ID to move the DB instance to. Specify @none@ to remove the instance from its current domain. The domain must be created prior to this operation. Currently, only MySQL, Microsoft SQL Server, Oracle, and PostgreSQL DB instances can be created in an Active Directory Domain.
--
-- For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/kerberos-authentication.html Kerberos Authentication> in the /Amazon RDS User Guide/ .
--
-- /Note:/ Consider using 'domain' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdiDomain :: Lens.Lens' ModifyDBInstance (Lude.Maybe Lude.Text)
mdiDomain = Lens.lens (domain :: ModifyDBInstance -> Lude.Maybe Lude.Text) (\s a -> s {domain = a} :: ModifyDBInstance)
{-# DEPRECATED mdiDomain "Use generic-lens or generic-optics with 'domain' instead." #-}

-- | A value that sets the open mode of a replica database to either mounted or read-only.
--
-- Mounted DB replicas are included in Oracle Enterprise Edition. The main use case for mounted replicas is cross-Region disaster recovery. The primary database doesn't use Active Data Guard to transmit information to the mounted replica. Because it doesn't accept user connections, a mounted replica can't serve a read-only workload. For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/oracle-read-replicas.html Working with Oracle Read Replicas for Amazon RDS> in the /Amazon RDS User Guide/ .
--
-- /Note:/ Consider using 'replicaMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdiReplicaMode :: Lens.Lens' ModifyDBInstance (Lude.Maybe ReplicaMode)
mdiReplicaMode = Lens.lens (replicaMode :: ModifyDBInstance -> Lude.Maybe ReplicaMode) (\s a -> s {replicaMode = a} :: ModifyDBInstance)
{-# DEPRECATED mdiReplicaMode "Use generic-lens or generic-optics with 'replicaMode' instead." #-}

-- | The interval, in seconds, between points when Enhanced Monitoring metrics are collected for the DB instance. To disable collecting Enhanced Monitoring metrics, specify 0. The default is 0.
--
-- If @MonitoringRoleArn@ is specified, then you must also set @MonitoringInterval@ to a value other than 0.
-- Valid Values: @0, 1, 5, 10, 15, 30, 60@
--
-- /Note:/ Consider using 'monitoringInterval' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdiMonitoringInterval :: Lens.Lens' ModifyDBInstance (Lude.Maybe Lude.Int)
mdiMonitoringInterval = Lens.lens (monitoringInterval :: ModifyDBInstance -> Lude.Maybe Lude.Int) (\s a -> s {monitoringInterval = a} :: ModifyDBInstance)
{-# DEPRECATED mdiMonitoringInterval "Use generic-lens or generic-optics with 'monitoringInterval' instead." #-}

-- | The configuration setting for the log types to be enabled for export to CloudWatch Logs for a specific DB instance.
--
-- A change to the @CloudwatchLogsExportConfiguration@ parameter is always applied to the DB instance immediately. Therefore, the @ApplyImmediately@ parameter has no effect.
--
-- /Note:/ Consider using 'cloudwatchLogsExportConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdiCloudwatchLogsExportConfiguration :: Lens.Lens' ModifyDBInstance (Lude.Maybe CloudwatchLogsExportConfiguration)
mdiCloudwatchLogsExportConfiguration = Lens.lens (cloudwatchLogsExportConfiguration :: ModifyDBInstance -> Lude.Maybe CloudwatchLogsExportConfiguration) (\s a -> s {cloudwatchLogsExportConfiguration = a} :: ModifyDBInstance)
{-# DEPRECATED mdiCloudwatchLogsExportConfiguration "Use generic-lens or generic-optics with 'cloudwatchLogsExportConfiguration' instead." #-}

-- | A value that indicates whether the DB instance is restarted when you rotate your SSL/TLS certificate.
--
-- By default, the DB instance is restarted when you rotate your SSL/TLS certificate. The certificate is not updated until the DB instance is restarted.
-- /Important:/ Set this parameter only if you are /not/ using SSL/TLS to connect to the DB instance.
-- If you are using SSL/TLS to connect to the DB instance, follow the appropriate instructions for your DB engine to rotate your SSL/TLS certificate:
--
--     * For more information about rotating your SSL/TLS certificate for RDS DB engines, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/UsingWithRDS.SSL-certificate-rotation.html Rotating Your SSL/TLS Certificate.> in the /Amazon RDS User Guide./
--
--
--     * For more information about rotating your SSL/TLS certificate for Aurora DB engines, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/UsingWithRDS.SSL-certificate-rotation.html Rotating Your SSL/TLS Certificate> in the /Amazon Aurora User Guide./
--
--
--
-- /Note:/ Consider using 'certificateRotationRestart' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdiCertificateRotationRestart :: Lens.Lens' ModifyDBInstance (Lude.Maybe Lude.Bool)
mdiCertificateRotationRestart = Lens.lens (certificateRotationRestart :: ModifyDBInstance -> Lude.Maybe Lude.Bool) (\s a -> s {certificateRotationRestart = a} :: ModifyDBInstance)
{-# DEPRECATED mdiCertificateRotationRestart "Use generic-lens or generic-optics with 'certificateRotationRestart' instead." #-}

-- | The password for the given ARN from the key store in order to access the device.
--
-- /Note:/ Consider using 'tdeCredentialPassword' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdiTDECredentialPassword :: Lens.Lens' ModifyDBInstance (Lude.Maybe Lude.Text)
mdiTDECredentialPassword = Lens.lens (tdeCredentialPassword :: ModifyDBInstance -> Lude.Maybe Lude.Text) (\s a -> s {tdeCredentialPassword = a} :: ModifyDBInstance)
{-# DEPRECATED mdiTDECredentialPassword "Use generic-lens or generic-optics with 'tdeCredentialPassword' instead." #-}

-- | The number of CPU cores and the number of threads per core for the DB instance class of the DB instance.
--
-- /Note:/ Consider using 'processorFeatures' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdiProcessorFeatures :: Lens.Lens' ModifyDBInstance (Lude.Maybe [ProcessorFeature])
mdiProcessorFeatures = Lens.lens (processorFeatures :: ModifyDBInstance -> Lude.Maybe [ProcessorFeature]) (\s a -> s {processorFeatures = a} :: ModifyDBInstance)
{-# DEPRECATED mdiProcessorFeatures "Use generic-lens or generic-optics with 'processorFeatures' instead." #-}

-- | The new compute and memory capacity of the DB instance, for example, @db.m4.large@ . Not all DB instance classes are available in all AWS Regions, or for all database engines. For the full list of DB instance classes, and availability for your engine, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Concepts.DBInstanceClass.html DB Instance Class> in the /Amazon RDS User Guide./
--
-- If you modify the DB instance class, an outage occurs during the change. The change is applied during the next maintenance window, unless @ApplyImmediately@ is enabled for this request.
-- Default: Uses existing setting
--
-- /Note:/ Consider using 'dbInstanceClass' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdiDBInstanceClass :: Lens.Lens' ModifyDBInstance (Lude.Maybe Lude.Text)
mdiDBInstanceClass = Lens.lens (dbInstanceClass :: ModifyDBInstance -> Lude.Maybe Lude.Text) (\s a -> s {dbInstanceClass = a} :: ModifyDBInstance)
{-# DEPRECATED mdiDBInstanceClass "Use generic-lens or generic-optics with 'dbInstanceClass' instead." #-}

-- | A value that specifies the order in which an Aurora Replica is promoted to the primary instance after a failure of the existing primary instance. For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/Aurora.Managing.Backups.html#Aurora.Managing.FaultTolerance Fault Tolerance for an Aurora DB Cluster> in the /Amazon Aurora User Guide/ .
--
-- Default: 1
-- Valid Values: 0 - 15
--
-- /Note:/ Consider using 'promotionTier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdiPromotionTier :: Lens.Lens' ModifyDBInstance (Lude.Maybe Lude.Int)
mdiPromotionTier = Lens.lens (promotionTier :: ModifyDBInstance -> Lude.Maybe Lude.Int) (\s a -> s {promotionTier = a} :: ModifyDBInstance)
{-# DEPRECATED mdiPromotionTier "Use generic-lens or generic-optics with 'promotionTier' instead." #-}

-- | The license model for the DB instance.
--
-- Valid values: @license-included@ | @bring-your-own-license@ | @general-public-license@
--
-- /Note:/ Consider using 'licenseModel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdiLicenseModel :: Lens.Lens' ModifyDBInstance (Lude.Maybe Lude.Text)
mdiLicenseModel = Lens.lens (licenseModel :: ModifyDBInstance -> Lude.Maybe Lude.Text) (\s a -> s {licenseModel = a} :: ModifyDBInstance)
{-# DEPRECATED mdiLicenseModel "Use generic-lens or generic-optics with 'licenseModel' instead." #-}

-- | The weekly time range (in UTC) during which system maintenance can occur, which might result in an outage. Changing this parameter doesn't result in an outage, except in the following situation, and the change is asynchronously applied as soon as possible. If there are pending actions that cause a reboot, and the maintenance window is changed to include the current time, then changing this parameter will cause a reboot of the DB instance. If moving this window to the current time, there must be at least 30 minutes between the current time and end of the window to ensure pending changes are applied.
--
-- Default: Uses existing setting
-- Format: ddd:hh24:mi-ddd:hh24:mi
-- Valid Days: Mon | Tue | Wed | Thu | Fri | Sat | Sun
-- Constraints: Must be at least 30 minutes
--
-- /Note:/ Consider using 'preferredMaintenanceWindow' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdiPreferredMaintenanceWindow :: Lens.Lens' ModifyDBInstance (Lude.Maybe Lude.Text)
mdiPreferredMaintenanceWindow = Lens.lens (preferredMaintenanceWindow :: ModifyDBInstance -> Lude.Maybe Lude.Text) (\s a -> s {preferredMaintenanceWindow = a} :: ModifyDBInstance)
{-# DEPRECATED mdiPreferredMaintenanceWindow "Use generic-lens or generic-optics with 'preferredMaintenanceWindow' instead." #-}

-- | The amount of time, in days, to retain Performance Insights data. Valid values are 7 or 731 (2 years).
--
-- /Note:/ Consider using 'performanceInsightsRetentionPeriod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdiPerformanceInsightsRetentionPeriod :: Lens.Lens' ModifyDBInstance (Lude.Maybe Lude.Int)
mdiPerformanceInsightsRetentionPeriod = Lens.lens (performanceInsightsRetentionPeriod :: ModifyDBInstance -> Lude.Maybe Lude.Int) (\s a -> s {performanceInsightsRetentionPeriod = a} :: ModifyDBInstance)
{-# DEPRECATED mdiPerformanceInsightsRetentionPeriod "Use generic-lens or generic-optics with 'performanceInsightsRetentionPeriod' instead." #-}

-- | Indicates the certificate that needs to be associated with the instance.
--
-- /Note:/ Consider using 'cACertificateIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdiCACertificateIdentifier :: Lens.Lens' ModifyDBInstance (Lude.Maybe Lude.Text)
mdiCACertificateIdentifier = Lens.lens (cACertificateIdentifier :: ModifyDBInstance -> Lude.Maybe Lude.Text) (\s a -> s {cACertificateIdentifier = a} :: ModifyDBInstance)
{-# DEPRECATED mdiCACertificateIdentifier "Use generic-lens or generic-optics with 'cACertificateIdentifier' instead." #-}

-- | The upper limit to which Amazon RDS can automatically scale the storage of the DB instance.
--
-- /Note:/ Consider using 'maxAllocatedStorage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdiMaxAllocatedStorage :: Lens.Lens' ModifyDBInstance (Lude.Maybe Lude.Int)
mdiMaxAllocatedStorage = Lens.lens (maxAllocatedStorage :: ModifyDBInstance -> Lude.Maybe Lude.Int) (\s a -> s {maxAllocatedStorage = a} :: ModifyDBInstance)
{-# DEPRECATED mdiMaxAllocatedStorage "Use generic-lens or generic-optics with 'maxAllocatedStorage' instead." #-}

-- | A value that indicates whether to enable Performance Insights for the DB instance.
--
-- For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_PerfInsights.html Using Amazon Performance Insights> in the /Amazon Relational Database Service User Guide/ .
--
-- /Note:/ Consider using 'enablePerformanceInsights' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdiEnablePerformanceInsights :: Lens.Lens' ModifyDBInstance (Lude.Maybe Lude.Bool)
mdiEnablePerformanceInsights = Lens.lens (enablePerformanceInsights :: ModifyDBInstance -> Lude.Maybe Lude.Bool) (\s a -> s {enablePerformanceInsights = a} :: ModifyDBInstance)
{-# DEPRECATED mdiEnablePerformanceInsights "Use generic-lens or generic-optics with 'enablePerformanceInsights' instead." #-}

-- | The name of the DB parameter group to apply to the DB instance. Changing this setting doesn't result in an outage. The parameter group name itself is changed immediately, but the actual parameter changes are not applied until you reboot the instance without failover. In this case, the DB instance isn't rebooted automatically and the parameter changes isn't applied during the next maintenance window.
--
-- Default: Uses existing setting
-- Constraints: The DB parameter group must be in the same DB parameter group family as this DB instance.
--
-- /Note:/ Consider using 'dbParameterGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdiDBParameterGroupName :: Lens.Lens' ModifyDBInstance (Lude.Maybe Lude.Text)
mdiDBParameterGroupName = Lens.lens (dbParameterGroupName :: ModifyDBInstance -> Lude.Maybe Lude.Text) (\s a -> s {dbParameterGroupName = a} :: ModifyDBInstance)
{-# DEPRECATED mdiDBParameterGroupName "Use generic-lens or generic-optics with 'dbParameterGroupName' instead." #-}

-- | The daily time range during which automated backups are created if automated backups are enabled, as determined by the @BackupRetentionPeriod@ parameter. Changing this parameter doesn't result in an outage and the change is asynchronously applied as soon as possible.
--
-- __Amazon Aurora__
-- Not applicable. The daily time range for creating automated backups is managed by the DB cluster. For more information, see @ModifyDBCluster@ .
-- Constraints:
--
--     * Must be in the format hh24:mi-hh24:mi
--
--
--     * Must be in Universal Time Coordinated (UTC)
--
--
--     * Must not conflict with the preferred maintenance window
--
--
--     * Must be at least 30 minutes
--
--
--
-- /Note:/ Consider using 'preferredBackupWindow' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdiPreferredBackupWindow :: Lens.Lens' ModifyDBInstance (Lude.Maybe Lude.Text)
mdiPreferredBackupWindow = Lens.lens (preferredBackupWindow :: ModifyDBInstance -> Lude.Maybe Lude.Text) (\s a -> s {preferredBackupWindow = a} :: ModifyDBInstance)
{-# DEPRECATED mdiPreferredBackupWindow "Use generic-lens or generic-optics with 'preferredBackupWindow' instead." #-}

-- | The number of days to retain automated backups. Setting this parameter to a positive number enables backups. Setting this parameter to 0 disables automated backups.
--
-- Changing this parameter can result in an outage if you change from 0 to a non-zero value or from a non-zero value to 0. These changes are applied during the next maintenance window unless the @ApplyImmediately@ parameter is enabled for this request. If you change the parameter from one non-zero value to another non-zero value, the change is asynchronously applied as soon as possible.
-- __Amazon Aurora__
-- Not applicable. The retention period for automated backups is managed by the DB cluster. For more information, see @ModifyDBCluster@ .
-- Default: Uses existing setting
-- Constraints:
--
--     * Must be a value from 0 to 35
--
--
--     * Can be specified for a MySQL read replica only if the source is running MySQL 5.6 or later
--
--
--     * Can be specified for a PostgreSQL read replica only if the source is running PostgreSQL 9.3.5
--
--
--     * Can't be set to 0 if the DB instance is a source to read replicas
--
--
--
-- /Note:/ Consider using 'backupRetentionPeriod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdiBackupRetentionPeriod :: Lens.Lens' ModifyDBInstance (Lude.Maybe Lude.Int)
mdiBackupRetentionPeriod = Lens.lens (backupRetentionPeriod :: ModifyDBInstance -> Lude.Maybe Lude.Int) (\s a -> s {backupRetentionPeriod = a} :: ModifyDBInstance)
{-# DEPRECATED mdiBackupRetentionPeriod "Use generic-lens or generic-optics with 'backupRetentionPeriod' instead." #-}

-- | The AWS KMS key identifier for encryption of Performance Insights data. The KMS key ID is the Amazon Resource Name (ARN), KMS key identifier, or the KMS key alias for the KMS encryption key.
--
-- If you do not specify a value for @PerformanceInsightsKMSKeyId@ , then Amazon RDS uses your default encryption key. AWS KMS creates the default encryption key for your AWS account. Your AWS account has a different default encryption key for each AWS Region.
--
-- /Note:/ Consider using 'performanceInsightsKMSKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdiPerformanceInsightsKMSKeyId :: Lens.Lens' ModifyDBInstance (Lude.Maybe Lude.Text)
mdiPerformanceInsightsKMSKeyId = Lens.lens (performanceInsightsKMSKeyId :: ModifyDBInstance -> Lude.Maybe Lude.Text) (\s a -> s {performanceInsightsKMSKeyId = a} :: ModifyDBInstance)
{-# DEPRECATED mdiPerformanceInsightsKMSKeyId "Use generic-lens or generic-optics with 'performanceInsightsKMSKeyId' instead." #-}

-- | A list of EC2 VPC security groups to authorize on this DB instance. This change is asynchronously applied as soon as possible.
--
-- __Amazon Aurora__
-- Not applicable. The associated list of EC2 VPC security groups is managed by the DB cluster. For more information, see @ModifyDBCluster@ .
-- Constraints:
--
--     * If supplied, must match existing VpcSecurityGroupIds.
--
--
--
-- /Note:/ Consider using 'vpcSecurityGroupIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdiVPCSecurityGroupIds :: Lens.Lens' ModifyDBInstance (Lude.Maybe [Lude.Text])
mdiVPCSecurityGroupIds = Lens.lens (vpcSecurityGroupIds :: ModifyDBInstance -> Lude.Maybe [Lude.Text]) (\s a -> s {vpcSecurityGroupIds = a} :: ModifyDBInstance)
{-# DEPRECATED mdiVPCSecurityGroupIds "Use generic-lens or generic-optics with 'vpcSecurityGroupIds' instead." #-}

-- | A value that indicates whether the DB instance is a Multi-AZ deployment. Changing this parameter doesn't result in an outage and the change is applied during the next maintenance window unless the @ApplyImmediately@ parameter is enabled for this request.
--
-- /Note:/ Consider using 'multiAZ' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdiMultiAZ :: Lens.Lens' ModifyDBInstance (Lude.Maybe Lude.Bool)
mdiMultiAZ = Lens.lens (multiAZ :: ModifyDBInstance -> Lude.Maybe Lude.Bool) (\s a -> s {multiAZ = a} :: ModifyDBInstance)
{-# DEPRECATED mdiMultiAZ "Use generic-lens or generic-optics with 'multiAZ' instead." #-}

-- | The new amount of storage (in gibibytes) to allocate for the DB instance.
--
-- For MariaDB, MySQL, Oracle, and PostgreSQL, the value supplied must be at least 10% greater than the current value. Values that are not at least 10% greater than the existing value are rounded up so that they are 10% greater than the current value.
-- For the valid values for allocated storage for each engine, see @CreateDBInstance@ .
--
-- /Note:/ Consider using 'allocatedStorage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdiAllocatedStorage :: Lens.Lens' ModifyDBInstance (Lude.Maybe Lude.Int)
mdiAllocatedStorage = Lens.lens (allocatedStorage :: ModifyDBInstance -> Lude.Maybe Lude.Int) (\s a -> s {allocatedStorage = a} :: ModifyDBInstance)
{-# DEPRECATED mdiAllocatedStorage "Use generic-lens or generic-optics with 'allocatedStorage' instead." #-}

-- | A value that indicates whether the modifications in this request and any pending modifications are asynchronously applied as soon as possible, regardless of the @PreferredMaintenanceWindow@ setting for the DB instance. By default, this parameter is disabled.
--
-- If this parameter is disabled, changes to the DB instance are applied during the next maintenance window. Some parameter changes can cause an outage and are applied on the next call to 'RebootDBInstance' , or the next failure reboot. Review the table of parameters in <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Overview.DBInstance.Modifying.html Modifying a DB Instance> in the /Amazon RDS User Guide./ to see the impact of enabling or disabling @ApplyImmediately@ for each modified parameter and to determine when the changes are applied.
--
-- /Note:/ Consider using 'applyImmediately' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdiApplyImmediately :: Lens.Lens' ModifyDBInstance (Lude.Maybe Lude.Bool)
mdiApplyImmediately = Lens.lens (applyImmediately :: ModifyDBInstance -> Lude.Maybe Lude.Bool) (\s a -> s {applyImmediately = a} :: ModifyDBInstance)
{-# DEPRECATED mdiApplyImmediately "Use generic-lens or generic-optics with 'applyImmediately' instead." #-}

-- | Indicates that the DB instance should be associated with the specified option group. Changing this parameter doesn't result in an outage except in the following case and the change is applied during the next maintenance window unless the @ApplyImmediately@ parameter is enabled for this request. If the parameter change results in an option group that enables OEM, this change can cause a brief (sub-second) period during which new connections are rejected but existing connections are not interrupted.
--
-- Permanent options, such as the TDE option for Oracle Advanced Security TDE, can't be removed from an option group, and that option group can't be removed from a DB instance once it is associated with a DB instance
--
-- /Note:/ Consider using 'optionGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdiOptionGroupName :: Lens.Lens' ModifyDBInstance (Lude.Maybe Lude.Text)
mdiOptionGroupName = Lens.lens (optionGroupName :: ModifyDBInstance -> Lude.Maybe Lude.Text) (\s a -> s {optionGroupName = a} :: ModifyDBInstance)
{-# DEPRECATED mdiOptionGroupName "Use generic-lens or generic-optics with 'optionGroupName' instead." #-}

-- | A value that indicates whether to copy all tags from the DB instance to snapshots of the DB instance. By default, tags are not copied.
--
-- __Amazon Aurora__
-- Not applicable. Copying tags to snapshots is managed by the DB cluster. Setting this value for an Aurora DB instance has no effect on the DB cluster setting. For more information, see @ModifyDBCluster@ .
--
-- /Note:/ Consider using 'copyTagsToSnapshot' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdiCopyTagsToSnapshot :: Lens.Lens' ModifyDBInstance (Lude.Maybe Lude.Bool)
mdiCopyTagsToSnapshot = Lens.lens (copyTagsToSnapshot :: ModifyDBInstance -> Lude.Maybe Lude.Bool) (\s a -> s {copyTagsToSnapshot = a} :: ModifyDBInstance)
{-# DEPRECATED mdiCopyTagsToSnapshot "Use generic-lens or generic-optics with 'copyTagsToSnapshot' instead." #-}

-- | The ARN from the key store with which to associate the instance for TDE encryption.
--
-- /Note:/ Consider using 'tdeCredentialARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdiTDECredentialARN :: Lens.Lens' ModifyDBInstance (Lude.Maybe Lude.Text)
mdiTDECredentialARN = Lens.lens (tdeCredentialARN :: ModifyDBInstance -> Lude.Maybe Lude.Text) (\s a -> s {tdeCredentialARN = a} :: ModifyDBInstance)
{-# DEPRECATED mdiTDECredentialARN "Use generic-lens or generic-optics with 'tdeCredentialARN' instead." #-}

-- | The name of the IAM role to use when making API calls to the Directory Service.
--
-- /Note:/ Consider using 'domainIAMRoleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdiDomainIAMRoleName :: Lens.Lens' ModifyDBInstance (Lude.Maybe Lude.Text)
mdiDomainIAMRoleName = Lens.lens (domainIAMRoleName :: ModifyDBInstance -> Lude.Maybe Lude.Text) (\s a -> s {domainIAMRoleName = a} :: ModifyDBInstance)
{-# DEPRECATED mdiDomainIAMRoleName "Use generic-lens or generic-optics with 'domainIAMRoleName' instead." #-}

-- | A value that indicates whether to enable mapping of AWS Identity and Access Management (IAM) accounts to database accounts. By default, mapping is disabled.
--
-- This setting doesn't apply to Amazon Aurora. Mapping AWS IAM accounts to database accounts is managed by the DB cluster.
-- For more information about IAM database authentication, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/UsingWithRDS.IAMDBAuth.html IAM Database Authentication for MySQL and PostgreSQL> in the /Amazon RDS User Guide./
--
-- /Note:/ Consider using 'enableIAMDatabaseAuthentication' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdiEnableIAMDatabaseAuthentication :: Lens.Lens' ModifyDBInstance (Lude.Maybe Lude.Bool)
mdiEnableIAMDatabaseAuthentication = Lens.lens (enableIAMDatabaseAuthentication :: ModifyDBInstance -> Lude.Maybe Lude.Bool) (\s a -> s {enableIAMDatabaseAuthentication = a} :: ModifyDBInstance)
{-# DEPRECATED mdiEnableIAMDatabaseAuthentication "Use generic-lens or generic-optics with 'enableIAMDatabaseAuthentication' instead." #-}

-- | A value that indicates whether the DB instance class of the DB instance uses its default processor features.
--
-- /Note:/ Consider using 'useDefaultProcessorFeatures' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdiUseDefaultProcessorFeatures :: Lens.Lens' ModifyDBInstance (Lude.Maybe Lude.Bool)
mdiUseDefaultProcessorFeatures = Lens.lens (useDefaultProcessorFeatures :: ModifyDBInstance -> Lude.Maybe Lude.Bool) (\s a -> s {useDefaultProcessorFeatures = a} :: ModifyDBInstance)
{-# DEPRECATED mdiUseDefaultProcessorFeatures "Use generic-lens or generic-optics with 'useDefaultProcessorFeatures' instead." #-}

-- | Specifies the storage type to be associated with the DB instance.
--
-- If you specify Provisioned IOPS (@io1@ ), you must also include a value for the @Iops@ parameter.
-- If you choose to migrate your DB instance from using standard storage to using Provisioned IOPS, or from using Provisioned IOPS to using standard storage, the process can take time. The duration of the migration depends on several factors such as database load, storage size, storage type (standard or Provisioned IOPS), amount of IOPS provisioned (if any), and the number of prior scale storage operations. Typical migration times are under 24 hours, but the process can take up to several days in some cases. During the migration, the DB instance is available for use, but might experience performance degradation. While the migration takes place, nightly backups for the instance are suspended. No other Amazon RDS operations can take place for the instance, including modifying the instance, rebooting the instance, deleting the instance, creating a read replica for the instance, and creating a DB snapshot of the instance.
-- Valid values: @standard | gp2 | io1@
-- Default: @io1@ if the @Iops@ parameter is specified, otherwise @gp2@
--
-- /Note:/ Consider using 'storageType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdiStorageType :: Lens.Lens' ModifyDBInstance (Lude.Maybe Lude.Text)
mdiStorageType = Lens.lens (storageType :: ModifyDBInstance -> Lude.Maybe Lude.Text) (\s a -> s {storageType = a} :: ModifyDBInstance)
{-# DEPRECATED mdiStorageType "Use generic-lens or generic-optics with 'storageType' instead." #-}

-- | The DB instance identifier. This value is stored as a lowercase string.
--
-- Constraints:
--
--     * Must match the identifier of an existing DBInstance.
--
--
--
-- /Note:/ Consider using 'dbInstanceIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdiDBInstanceIdentifier :: Lens.Lens' ModifyDBInstance Lude.Text
mdiDBInstanceIdentifier = Lens.lens (dbInstanceIdentifier :: ModifyDBInstance -> Lude.Text) (\s a -> s {dbInstanceIdentifier = a} :: ModifyDBInstance)
{-# DEPRECATED mdiDBInstanceIdentifier "Use generic-lens or generic-optics with 'dbInstanceIdentifier' instead." #-}

instance Lude.AWSRequest ModifyDBInstance where
  type Rs ModifyDBInstance = ModifyDBInstanceResponse
  request = Req.postQuery rdsService
  response =
    Res.receiveXMLWrapper
      "ModifyDBInstanceResult"
      ( \s h x ->
          ModifyDBInstanceResponse'
            Lude.<$> (x Lude..@? "DBInstance") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ModifyDBInstance where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ModifyDBInstance where
  toPath = Lude.const "/"

instance Lude.ToQuery ModifyDBInstance where
  toQuery ModifyDBInstance' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("ModifyDBInstance" :: Lude.ByteString),
        "Version" Lude.=: ("2014-10-31" :: Lude.ByteString),
        "EngineVersion" Lude.=: engineVersion,
        "DBSecurityGroups"
          Lude.=: Lude.toQuery
            (Lude.toQueryList "DBSecurityGroupName" Lude.<$> dbSecurityGroups),
        "DBPortNumber" Lude.=: dbPortNumber,
        "DeletionProtection" Lude.=: deletionProtection,
        "MasterUserPassword" Lude.=: masterUserPassword,
        "PubliclyAccessible" Lude.=: publiclyAccessible,
        "AutoMinorVersionUpgrade" Lude.=: autoMinorVersionUpgrade,
        "DBSubnetGroupName" Lude.=: dbSubnetGroupName,
        "MonitoringRoleArn" Lude.=: monitoringRoleARN,
        "Iops" Lude.=: iops,
        "AllowMajorVersionUpgrade" Lude.=: allowMajorVersionUpgrade,
        "NewDBInstanceIdentifier" Lude.=: newDBInstanceIdentifier,
        "Domain" Lude.=: domain,
        "ReplicaMode" Lude.=: replicaMode,
        "MonitoringInterval" Lude.=: monitoringInterval,
        "CloudwatchLogsExportConfiguration"
          Lude.=: cloudwatchLogsExportConfiguration,
        "CertificateRotationRestart" Lude.=: certificateRotationRestart,
        "TdeCredentialPassword" Lude.=: tdeCredentialPassword,
        "ProcessorFeatures"
          Lude.=: Lude.toQuery
            (Lude.toQueryList "ProcessorFeature" Lude.<$> processorFeatures),
        "DBInstanceClass" Lude.=: dbInstanceClass,
        "PromotionTier" Lude.=: promotionTier,
        "LicenseModel" Lude.=: licenseModel,
        "PreferredMaintenanceWindow" Lude.=: preferredMaintenanceWindow,
        "PerformanceInsightsRetentionPeriod"
          Lude.=: performanceInsightsRetentionPeriod,
        "CACertificateIdentifier" Lude.=: cACertificateIdentifier,
        "MaxAllocatedStorage" Lude.=: maxAllocatedStorage,
        "EnablePerformanceInsights" Lude.=: enablePerformanceInsights,
        "DBParameterGroupName" Lude.=: dbParameterGroupName,
        "PreferredBackupWindow" Lude.=: preferredBackupWindow,
        "BackupRetentionPeriod" Lude.=: backupRetentionPeriod,
        "PerformanceInsightsKMSKeyId" Lude.=: performanceInsightsKMSKeyId,
        "VpcSecurityGroupIds"
          Lude.=: Lude.toQuery
            ( Lude.toQueryList "VpcSecurityGroupId"
                Lude.<$> vpcSecurityGroupIds
            ),
        "MultiAZ" Lude.=: multiAZ,
        "AllocatedStorage" Lude.=: allocatedStorage,
        "ApplyImmediately" Lude.=: applyImmediately,
        "OptionGroupName" Lude.=: optionGroupName,
        "CopyTagsToSnapshot" Lude.=: copyTagsToSnapshot,
        "TdeCredentialArn" Lude.=: tdeCredentialARN,
        "DomainIAMRoleName" Lude.=: domainIAMRoleName,
        "EnableIAMDatabaseAuthentication"
          Lude.=: enableIAMDatabaseAuthentication,
        "UseDefaultProcessorFeatures" Lude.=: useDefaultProcessorFeatures,
        "StorageType" Lude.=: storageType,
        "DBInstanceIdentifier" Lude.=: dbInstanceIdentifier
      ]

-- | /See:/ 'mkModifyDBInstanceResponse' smart constructor.
data ModifyDBInstanceResponse = ModifyDBInstanceResponse'
  { dbInstance ::
      Lude.Maybe DBInstance,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ModifyDBInstanceResponse' with the minimum fields required to make a request.
--
-- * 'dbInstance' - Undocumented field.
-- * 'responseStatus' - The response status code.
mkModifyDBInstanceResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ModifyDBInstanceResponse
mkModifyDBInstanceResponse pResponseStatus_ =
  ModifyDBInstanceResponse'
    { dbInstance = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'dbInstance' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdirsDBInstance :: Lens.Lens' ModifyDBInstanceResponse (Lude.Maybe DBInstance)
mdirsDBInstance = Lens.lens (dbInstance :: ModifyDBInstanceResponse -> Lude.Maybe DBInstance) (\s a -> s {dbInstance = a} :: ModifyDBInstanceResponse)
{-# DEPRECATED mdirsDBInstance "Use generic-lens or generic-optics with 'dbInstance' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdirsResponseStatus :: Lens.Lens' ModifyDBInstanceResponse Lude.Int
mdirsResponseStatus = Lens.lens (responseStatus :: ModifyDBInstanceResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ModifyDBInstanceResponse)
{-# DEPRECATED mdirsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
