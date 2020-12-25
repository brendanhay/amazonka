{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
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
    mdbiDBInstanceIdentifier,
    mdbiAllocatedStorage,
    mdbiAllowMajorVersionUpgrade,
    mdbiApplyImmediately,
    mdbiAutoMinorVersionUpgrade,
    mdbiBackupRetentionPeriod,
    mdbiCACertificateIdentifier,
    mdbiCertificateRotationRestart,
    mdbiCloudwatchLogsExportConfiguration,
    mdbiCopyTagsToSnapshot,
    mdbiDBInstanceClass,
    mdbiDBParameterGroupName,
    mdbiDBPortNumber,
    mdbiDBSecurityGroups,
    mdbiDBSubnetGroupName,
    mdbiDeletionProtection,
    mdbiDomain,
    mdbiDomainIAMRoleName,
    mdbiEnableIAMDatabaseAuthentication,
    mdbiEnablePerformanceInsights,
    mdbiEngineVersion,
    mdbiIops,
    mdbiLicenseModel,
    mdbiMasterUserPassword,
    mdbiMaxAllocatedStorage,
    mdbiMonitoringInterval,
    mdbiMonitoringRoleArn,
    mdbiMultiAZ,
    mdbiNewDBInstanceIdentifier,
    mdbiOptionGroupName,
    mdbiPerformanceInsightsKMSKeyId,
    mdbiPerformanceInsightsRetentionPeriod,
    mdbiPreferredBackupWindow,
    mdbiPreferredMaintenanceWindow,
    mdbiProcessorFeatures,
    mdbiPromotionTier,
    mdbiPubliclyAccessible,
    mdbiReplicaMode,
    mdbiStorageType,
    mdbiTdeCredentialArn,
    mdbiTdeCredentialPassword,
    mdbiUseDefaultProcessorFeatures,
    mdbiVpcSecurityGroupIds,

    -- * Destructuring the response
    ModifyDBInstanceResponse (..),
    mkModifyDBInstanceResponse,

    -- ** Response lenses
    mdbirrsDBInstance,
    mdbirrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'mkModifyDBInstance' smart constructor.
data ModifyDBInstance = ModifyDBInstance'
  { -- | The DB instance identifier. This value is stored as a lowercase string.
    --
    -- Constraints:
    --
    --     * Must match the identifier of an existing DBInstance.
    dBInstanceIdentifier :: Types.DBInstanceIdentifier,
    -- | The new amount of storage (in gibibytes) to allocate for the DB instance.
    --
    -- For MariaDB, MySQL, Oracle, and PostgreSQL, the value supplied must be at least 10% greater than the current value. Values that are not at least 10% greater than the existing value are rounded up so that they are 10% greater than the current value.
    -- For the valid values for allocated storage for each engine, see @CreateDBInstance@ .
    allocatedStorage :: Core.Maybe Core.Int,
    -- | A value that indicates whether major version upgrades are allowed. Changing this parameter doesn't result in an outage and the change is asynchronously applied as soon as possible.
    --
    -- Constraints: Major version upgrades must be allowed when specifying a value for the EngineVersion parameter that is a different major version than the DB instance's current version.
    allowMajorVersionUpgrade :: Core.Maybe Core.Bool,
    -- | A value that indicates whether the modifications in this request and any pending modifications are asynchronously applied as soon as possible, regardless of the @PreferredMaintenanceWindow@ setting for the DB instance. By default, this parameter is disabled.
    --
    -- If this parameter is disabled, changes to the DB instance are applied during the next maintenance window. Some parameter changes can cause an outage and are applied on the next call to 'RebootDBInstance' , or the next failure reboot. Review the table of parameters in <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Overview.DBInstance.Modifying.html Modifying a DB Instance> in the /Amazon RDS User Guide./ to see the impact of enabling or disabling @ApplyImmediately@ for each modified parameter and to determine when the changes are applied.
    applyImmediately :: Core.Maybe Core.Bool,
    -- | A value that indicates whether minor version upgrades are applied automatically to the DB instance during the maintenance window. Changing this parameter doesn't result in an outage except in the following case and the change is asynchronously applied as soon as possible. An outage results if this parameter is enabled during the maintenance window, and a newer minor version is available, and RDS has enabled auto patching for that engine version.
    autoMinorVersionUpgrade :: Core.Maybe Core.Bool,
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
    backupRetentionPeriod :: Core.Maybe Core.Int,
    -- | Indicates the certificate that needs to be associated with the instance.
    cACertificateIdentifier :: Core.Maybe Types.CACertificateIdentifier,
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
    certificateRotationRestart :: Core.Maybe Core.Bool,
    -- | The configuration setting for the log types to be enabled for export to CloudWatch Logs for a specific DB instance.
    --
    -- A change to the @CloudwatchLogsExportConfiguration@ parameter is always applied to the DB instance immediately. Therefore, the @ApplyImmediately@ parameter has no effect.
    cloudwatchLogsExportConfiguration :: Core.Maybe Types.CloudwatchLogsExportConfiguration,
    -- | A value that indicates whether to copy all tags from the DB instance to snapshots of the DB instance. By default, tags are not copied.
    --
    -- __Amazon Aurora__
    -- Not applicable. Copying tags to snapshots is managed by the DB cluster. Setting this value for an Aurora DB instance has no effect on the DB cluster setting. For more information, see @ModifyDBCluster@ .
    copyTagsToSnapshot :: Core.Maybe Core.Bool,
    -- | The new compute and memory capacity of the DB instance, for example, @db.m4.large@ . Not all DB instance classes are available in all AWS Regions, or for all database engines. For the full list of DB instance classes, and availability for your engine, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Concepts.DBInstanceClass.html DB Instance Class> in the /Amazon RDS User Guide./
    --
    -- If you modify the DB instance class, an outage occurs during the change. The change is applied during the next maintenance window, unless @ApplyImmediately@ is enabled for this request.
    -- Default: Uses existing setting
    dBInstanceClass :: Core.Maybe Types.DBInstanceClass,
    -- | The name of the DB parameter group to apply to the DB instance. Changing this setting doesn't result in an outage. The parameter group name itself is changed immediately, but the actual parameter changes are not applied until you reboot the instance without failover. In this case, the DB instance isn't rebooted automatically and the parameter changes isn't applied during the next maintenance window.
    --
    -- Default: Uses existing setting
    -- Constraints: The DB parameter group must be in the same DB parameter group family as this DB instance.
    dBParameterGroupName :: Core.Maybe Types.DBParameterGroupName,
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
    dBPortNumber :: Core.Maybe Core.Int,
    -- | A list of DB security groups to authorize on this DB instance. Changing this setting doesn't result in an outage and the change is asynchronously applied as soon as possible.
    --
    -- Constraints:
    --
    --     * If supplied, must match existing DBSecurityGroups.
    dBSecurityGroups :: Core.Maybe [Types.String],
    -- | The new DB subnet group for the DB instance. You can use this parameter to move your DB instance to a different VPC. If your DB instance isn't in a VPC, you can also use this parameter to move your DB instance into a VPC. For more information, see <http://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_VPC.WorkingWithRDSInstanceinaVPC.html#USER_VPC.Non-VPC2VPC Updating the VPC for a DB Instance> in the /Amazon RDS User Guide./
    --
    -- Changing the subnet group causes an outage during the change. The change is applied during the next maintenance window, unless you enable @ApplyImmediately@ .
    -- Constraints: If supplied, must match the name of an existing DBSubnetGroup.
    -- Example: @mySubnetGroup@
    dBSubnetGroupName :: Core.Maybe Types.DBSubnetGroupName,
    -- | A value that indicates whether the DB instance has deletion protection enabled. The database can't be deleted when deletion protection is enabled. By default, deletion protection is disabled. For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_DeleteInstance.html Deleting a DB Instance> .
    deletionProtection :: Core.Maybe Core.Bool,
    -- | The Active Directory directory ID to move the DB instance to. Specify @none@ to remove the instance from its current domain. The domain must be created prior to this operation. Currently, only MySQL, Microsoft SQL Server, Oracle, and PostgreSQL DB instances can be created in an Active Directory Domain.
    --
    -- For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/kerberos-authentication.html Kerberos Authentication> in the /Amazon RDS User Guide/ .
    domain :: Core.Maybe Types.Domain,
    -- | The name of the IAM role to use when making API calls to the Directory Service.
    domainIAMRoleName :: Core.Maybe Types.DomainIAMRoleName,
    -- | A value that indicates whether to enable mapping of AWS Identity and Access Management (IAM) accounts to database accounts. By default, mapping is disabled.
    --
    -- This setting doesn't apply to Amazon Aurora. Mapping AWS IAM accounts to database accounts is managed by the DB cluster.
    -- For more information about IAM database authentication, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/UsingWithRDS.IAMDBAuth.html IAM Database Authentication for MySQL and PostgreSQL> in the /Amazon RDS User Guide./
    enableIAMDatabaseAuthentication :: Core.Maybe Core.Bool,
    -- | A value that indicates whether to enable Performance Insights for the DB instance.
    --
    -- For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_PerfInsights.html Using Amazon Performance Insights> in the /Amazon Relational Database Service User Guide/ .
    enablePerformanceInsights :: Core.Maybe Core.Bool,
    -- | The version number of the database engine to upgrade to. Changing this parameter results in an outage and the change is applied during the next maintenance window unless the @ApplyImmediately@ parameter is enabled for this request.
    --
    -- For major version upgrades, if a nondefault DB parameter group is currently in use, a new DB parameter group in the DB parameter group family for the new engine version must be specified. The new DB parameter group can be the default for that DB parameter group family.
    -- For information about valid engine versions, see @CreateDBInstance@ , or call @DescribeDBEngineVersions@ .
    engineVersion :: Core.Maybe Types.EngineVersion,
    -- | The new Provisioned IOPS (I/O operations per second) value for the RDS instance.
    --
    -- Changing this setting doesn't result in an outage and the change is applied during the next maintenance window unless the @ApplyImmediately@ parameter is enabled for this request. If you are migrating from Provisioned IOPS to standard storage, set this value to 0. The DB instance will require a reboot for the change in storage type to take effect.
    -- If you choose to migrate your DB instance from using standard storage to using Provisioned IOPS, or from using Provisioned IOPS to using standard storage, the process can take time. The duration of the migration depends on several factors such as database load, storage size, storage type (standard or Provisioned IOPS), amount of IOPS provisioned (if any), and the number of prior scale storage operations. Typical migration times are under 24 hours, but the process can take up to several days in some cases. During the migration, the DB instance is available for use, but might experience performance degradation. While the migration takes place, nightly backups for the instance are suspended. No other Amazon RDS operations can take place for the instance, including modifying the instance, rebooting the instance, deleting the instance, creating a read replica for the instance, and creating a DB snapshot of the instance.
    -- Constraints: For MariaDB, MySQL, Oracle, and PostgreSQL, the value supplied must be at least 10% greater than the current value. Values that are not at least 10% greater than the existing value are rounded up so that they are 10% greater than the current value.
    -- Default: Uses existing setting
    iops :: Core.Maybe Core.Int,
    -- | The license model for the DB instance.
    --
    -- Valid values: @license-included@ | @bring-your-own-license@ | @general-public-license@
    licenseModel :: Core.Maybe Types.LicenseModel,
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
    masterUserPassword :: Core.Maybe Types.MasterUserPassword,
    -- | The upper limit to which Amazon RDS can automatically scale the storage of the DB instance.
    maxAllocatedStorage :: Core.Maybe Core.Int,
    -- | The interval, in seconds, between points when Enhanced Monitoring metrics are collected for the DB instance. To disable collecting Enhanced Monitoring metrics, specify 0. The default is 0.
    --
    -- If @MonitoringRoleArn@ is specified, then you must also set @MonitoringInterval@ to a value other than 0.
    -- Valid Values: @0, 1, 5, 10, 15, 30, 60@
    monitoringInterval :: Core.Maybe Core.Int,
    -- | The ARN for the IAM role that permits RDS to send enhanced monitoring metrics to Amazon CloudWatch Logs. For example, @arn:aws:iam:123456789012:role/emaccess@ . For information on creating a monitoring role, go to <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_Monitoring.html#USER_Monitoring.OS.IAMRole To create an IAM role for Amazon RDS Enhanced Monitoring> in the /Amazon RDS User Guide./
    --
    -- If @MonitoringInterval@ is set to a value other than 0, then you must supply a @MonitoringRoleArn@ value.
    monitoringRoleArn :: Core.Maybe Types.MonitoringRoleArn,
    -- | A value that indicates whether the DB instance is a Multi-AZ deployment. Changing this parameter doesn't result in an outage and the change is applied during the next maintenance window unless the @ApplyImmediately@ parameter is enabled for this request.
    multiAZ :: Core.Maybe Core.Bool,
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
    newDBInstanceIdentifier :: Core.Maybe Types.NewDBInstanceIdentifier,
    -- | Indicates that the DB instance should be associated with the specified option group. Changing this parameter doesn't result in an outage except in the following case and the change is applied during the next maintenance window unless the @ApplyImmediately@ parameter is enabled for this request. If the parameter change results in an option group that enables OEM, this change can cause a brief (sub-second) period during which new connections are rejected but existing connections are not interrupted.
    --
    -- Permanent options, such as the TDE option for Oracle Advanced Security TDE, can't be removed from an option group, and that option group can't be removed from a DB instance once it is associated with a DB instance
    optionGroupName :: Core.Maybe Types.OptionGroupName,
    -- | The AWS KMS key identifier for encryption of Performance Insights data. The KMS key ID is the Amazon Resource Name (ARN), KMS key identifier, or the KMS key alias for the KMS encryption key.
    --
    -- If you do not specify a value for @PerformanceInsightsKMSKeyId@ , then Amazon RDS uses your default encryption key. AWS KMS creates the default encryption key for your AWS account. Your AWS account has a different default encryption key for each AWS Region.
    performanceInsightsKMSKeyId :: Core.Maybe Types.PerformanceInsightsKMSKeyId,
    -- | The amount of time, in days, to retain Performance Insights data. Valid values are 7 or 731 (2 years).
    performanceInsightsRetentionPeriod :: Core.Maybe Core.Int,
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
    preferredBackupWindow :: Core.Maybe Types.PreferredBackupWindow,
    -- | The weekly time range (in UTC) during which system maintenance can occur, which might result in an outage. Changing this parameter doesn't result in an outage, except in the following situation, and the change is asynchronously applied as soon as possible. If there are pending actions that cause a reboot, and the maintenance window is changed to include the current time, then changing this parameter will cause a reboot of the DB instance. If moving this window to the current time, there must be at least 30 minutes between the current time and end of the window to ensure pending changes are applied.
    --
    -- Default: Uses existing setting
    -- Format: ddd:hh24:mi-ddd:hh24:mi
    -- Valid Days: Mon | Tue | Wed | Thu | Fri | Sat | Sun
    -- Constraints: Must be at least 30 minutes
    preferredMaintenanceWindow :: Core.Maybe Types.PreferredMaintenanceWindow,
    -- | The number of CPU cores and the number of threads per core for the DB instance class of the DB instance.
    processorFeatures :: Core.Maybe [Types.ProcessorFeature],
    -- | A value that specifies the order in which an Aurora Replica is promoted to the primary instance after a failure of the existing primary instance. For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/Aurora.Managing.Backups.html#Aurora.Managing.FaultTolerance Fault Tolerance for an Aurora DB Cluster> in the /Amazon Aurora User Guide/ .
    --
    -- Default: 1
    -- Valid Values: 0 - 15
    promotionTier :: Core.Maybe Core.Int,
    -- | A value that indicates whether the DB instance is publicly accessible.
    --
    -- When the DB instance is publicly accessible, its DNS endpoint resolves to the private IP address from within the DB instance's VPC, and to the public IP address from outside of the DB instance's VPC. Access to the DB instance is ultimately controlled by the security group it uses, and that public access is not permitted if the security group assigned to the DB instance doesn't permit it.
    -- When the DB instance isn't publicly accessible, it is an internal DB instance with a DNS name that resolves to a private IP address.
    -- @PubliclyAccessible@ only applies to DB instances in a VPC. The DB instance must be part of a public subnet and @PubliclyAccessible@ must be enabled for it to be publicly accessible.
    -- Changes to the @PubliclyAccessible@ parameter are applied immediately regardless of the value of the @ApplyImmediately@ parameter.
    publiclyAccessible :: Core.Maybe Core.Bool,
    -- | A value that sets the open mode of a replica database to either mounted or read-only.
    --
    -- Mounted DB replicas are included in Oracle Enterprise Edition. The main use case for mounted replicas is cross-Region disaster recovery. The primary database doesn't use Active Data Guard to transmit information to the mounted replica. Because it doesn't accept user connections, a mounted replica can't serve a read-only workload. For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/oracle-read-replicas.html Working with Oracle Read Replicas for Amazon RDS> in the /Amazon RDS User Guide/ .
    replicaMode :: Core.Maybe Types.ReplicaMode,
    -- | Specifies the storage type to be associated with the DB instance.
    --
    -- If you specify Provisioned IOPS (@io1@ ), you must also include a value for the @Iops@ parameter.
    -- If you choose to migrate your DB instance from using standard storage to using Provisioned IOPS, or from using Provisioned IOPS to using standard storage, the process can take time. The duration of the migration depends on several factors such as database load, storage size, storage type (standard or Provisioned IOPS), amount of IOPS provisioned (if any), and the number of prior scale storage operations. Typical migration times are under 24 hours, but the process can take up to several days in some cases. During the migration, the DB instance is available for use, but might experience performance degradation. While the migration takes place, nightly backups for the instance are suspended. No other Amazon RDS operations can take place for the instance, including modifying the instance, rebooting the instance, deleting the instance, creating a read replica for the instance, and creating a DB snapshot of the instance.
    -- Valid values: @standard | gp2 | io1@
    -- Default: @io1@ if the @Iops@ parameter is specified, otherwise @gp2@
    storageType :: Core.Maybe Types.StorageType,
    -- | The ARN from the key store with which to associate the instance for TDE encryption.
    tdeCredentialArn :: Core.Maybe Types.TdeCredentialArn,
    -- | The password for the given ARN from the key store in order to access the device.
    tdeCredentialPassword :: Core.Maybe Types.TdeCredentialPassword,
    -- | A value that indicates whether the DB instance class of the DB instance uses its default processor features.
    useDefaultProcessorFeatures :: Core.Maybe Core.Bool,
    -- | A list of EC2 VPC security groups to authorize on this DB instance. This change is asynchronously applied as soon as possible.
    --
    -- __Amazon Aurora__
    -- Not applicable. The associated list of EC2 VPC security groups is managed by the DB cluster. For more information, see @ModifyDBCluster@ .
    -- Constraints:
    --
    --     * If supplied, must match existing VpcSecurityGroupIds.
    vpcSecurityGroupIds :: Core.Maybe [Types.String]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyDBInstance' value with any optional fields omitted.
mkModifyDBInstance ::
  -- | 'dBInstanceIdentifier'
  Types.DBInstanceIdentifier ->
  ModifyDBInstance
mkModifyDBInstance dBInstanceIdentifier =
  ModifyDBInstance'
    { dBInstanceIdentifier,
      allocatedStorage = Core.Nothing,
      allowMajorVersionUpgrade = Core.Nothing,
      applyImmediately = Core.Nothing,
      autoMinorVersionUpgrade = Core.Nothing,
      backupRetentionPeriod = Core.Nothing,
      cACertificateIdentifier = Core.Nothing,
      certificateRotationRestart = Core.Nothing,
      cloudwatchLogsExportConfiguration = Core.Nothing,
      copyTagsToSnapshot = Core.Nothing,
      dBInstanceClass = Core.Nothing,
      dBParameterGroupName = Core.Nothing,
      dBPortNumber = Core.Nothing,
      dBSecurityGroups = Core.Nothing,
      dBSubnetGroupName = Core.Nothing,
      deletionProtection = Core.Nothing,
      domain = Core.Nothing,
      domainIAMRoleName = Core.Nothing,
      enableIAMDatabaseAuthentication = Core.Nothing,
      enablePerformanceInsights = Core.Nothing,
      engineVersion = Core.Nothing,
      iops = Core.Nothing,
      licenseModel = Core.Nothing,
      masterUserPassword = Core.Nothing,
      maxAllocatedStorage = Core.Nothing,
      monitoringInterval = Core.Nothing,
      monitoringRoleArn = Core.Nothing,
      multiAZ = Core.Nothing,
      newDBInstanceIdentifier = Core.Nothing,
      optionGroupName = Core.Nothing,
      performanceInsightsKMSKeyId = Core.Nothing,
      performanceInsightsRetentionPeriod = Core.Nothing,
      preferredBackupWindow = Core.Nothing,
      preferredMaintenanceWindow = Core.Nothing,
      processorFeatures = Core.Nothing,
      promotionTier = Core.Nothing,
      publiclyAccessible = Core.Nothing,
      replicaMode = Core.Nothing,
      storageType = Core.Nothing,
      tdeCredentialArn = Core.Nothing,
      tdeCredentialPassword = Core.Nothing,
      useDefaultProcessorFeatures = Core.Nothing,
      vpcSecurityGroupIds = Core.Nothing
    }

-- | The DB instance identifier. This value is stored as a lowercase string.
--
-- Constraints:
--
--     * Must match the identifier of an existing DBInstance.
--
--
--
-- /Note:/ Consider using 'dBInstanceIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdbiDBInstanceIdentifier :: Lens.Lens' ModifyDBInstance Types.DBInstanceIdentifier
mdbiDBInstanceIdentifier = Lens.field @"dBInstanceIdentifier"
{-# DEPRECATED mdbiDBInstanceIdentifier "Use generic-lens or generic-optics with 'dBInstanceIdentifier' instead." #-}

-- | The new amount of storage (in gibibytes) to allocate for the DB instance.
--
-- For MariaDB, MySQL, Oracle, and PostgreSQL, the value supplied must be at least 10% greater than the current value. Values that are not at least 10% greater than the existing value are rounded up so that they are 10% greater than the current value.
-- For the valid values for allocated storage for each engine, see @CreateDBInstance@ .
--
-- /Note:/ Consider using 'allocatedStorage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdbiAllocatedStorage :: Lens.Lens' ModifyDBInstance (Core.Maybe Core.Int)
mdbiAllocatedStorage = Lens.field @"allocatedStorage"
{-# DEPRECATED mdbiAllocatedStorage "Use generic-lens or generic-optics with 'allocatedStorage' instead." #-}

-- | A value that indicates whether major version upgrades are allowed. Changing this parameter doesn't result in an outage and the change is asynchronously applied as soon as possible.
--
-- Constraints: Major version upgrades must be allowed when specifying a value for the EngineVersion parameter that is a different major version than the DB instance's current version.
--
-- /Note:/ Consider using 'allowMajorVersionUpgrade' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdbiAllowMajorVersionUpgrade :: Lens.Lens' ModifyDBInstance (Core.Maybe Core.Bool)
mdbiAllowMajorVersionUpgrade = Lens.field @"allowMajorVersionUpgrade"
{-# DEPRECATED mdbiAllowMajorVersionUpgrade "Use generic-lens or generic-optics with 'allowMajorVersionUpgrade' instead." #-}

-- | A value that indicates whether the modifications in this request and any pending modifications are asynchronously applied as soon as possible, regardless of the @PreferredMaintenanceWindow@ setting for the DB instance. By default, this parameter is disabled.
--
-- If this parameter is disabled, changes to the DB instance are applied during the next maintenance window. Some parameter changes can cause an outage and are applied on the next call to 'RebootDBInstance' , or the next failure reboot. Review the table of parameters in <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Overview.DBInstance.Modifying.html Modifying a DB Instance> in the /Amazon RDS User Guide./ to see the impact of enabling or disabling @ApplyImmediately@ for each modified parameter and to determine when the changes are applied.
--
-- /Note:/ Consider using 'applyImmediately' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdbiApplyImmediately :: Lens.Lens' ModifyDBInstance (Core.Maybe Core.Bool)
mdbiApplyImmediately = Lens.field @"applyImmediately"
{-# DEPRECATED mdbiApplyImmediately "Use generic-lens or generic-optics with 'applyImmediately' instead." #-}

-- | A value that indicates whether minor version upgrades are applied automatically to the DB instance during the maintenance window. Changing this parameter doesn't result in an outage except in the following case and the change is asynchronously applied as soon as possible. An outage results if this parameter is enabled during the maintenance window, and a newer minor version is available, and RDS has enabled auto patching for that engine version.
--
-- /Note:/ Consider using 'autoMinorVersionUpgrade' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdbiAutoMinorVersionUpgrade :: Lens.Lens' ModifyDBInstance (Core.Maybe Core.Bool)
mdbiAutoMinorVersionUpgrade = Lens.field @"autoMinorVersionUpgrade"
{-# DEPRECATED mdbiAutoMinorVersionUpgrade "Use generic-lens or generic-optics with 'autoMinorVersionUpgrade' instead." #-}

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
mdbiBackupRetentionPeriod :: Lens.Lens' ModifyDBInstance (Core.Maybe Core.Int)
mdbiBackupRetentionPeriod = Lens.field @"backupRetentionPeriod"
{-# DEPRECATED mdbiBackupRetentionPeriod "Use generic-lens or generic-optics with 'backupRetentionPeriod' instead." #-}

-- | Indicates the certificate that needs to be associated with the instance.
--
-- /Note:/ Consider using 'cACertificateIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdbiCACertificateIdentifier :: Lens.Lens' ModifyDBInstance (Core.Maybe Types.CACertificateIdentifier)
mdbiCACertificateIdentifier = Lens.field @"cACertificateIdentifier"
{-# DEPRECATED mdbiCACertificateIdentifier "Use generic-lens or generic-optics with 'cACertificateIdentifier' instead." #-}

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
mdbiCertificateRotationRestart :: Lens.Lens' ModifyDBInstance (Core.Maybe Core.Bool)
mdbiCertificateRotationRestart = Lens.field @"certificateRotationRestart"
{-# DEPRECATED mdbiCertificateRotationRestart "Use generic-lens or generic-optics with 'certificateRotationRestart' instead." #-}

-- | The configuration setting for the log types to be enabled for export to CloudWatch Logs for a specific DB instance.
--
-- A change to the @CloudwatchLogsExportConfiguration@ parameter is always applied to the DB instance immediately. Therefore, the @ApplyImmediately@ parameter has no effect.
--
-- /Note:/ Consider using 'cloudwatchLogsExportConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdbiCloudwatchLogsExportConfiguration :: Lens.Lens' ModifyDBInstance (Core.Maybe Types.CloudwatchLogsExportConfiguration)
mdbiCloudwatchLogsExportConfiguration = Lens.field @"cloudwatchLogsExportConfiguration"
{-# DEPRECATED mdbiCloudwatchLogsExportConfiguration "Use generic-lens or generic-optics with 'cloudwatchLogsExportConfiguration' instead." #-}

-- | A value that indicates whether to copy all tags from the DB instance to snapshots of the DB instance. By default, tags are not copied.
--
-- __Amazon Aurora__
-- Not applicable. Copying tags to snapshots is managed by the DB cluster. Setting this value for an Aurora DB instance has no effect on the DB cluster setting. For more information, see @ModifyDBCluster@ .
--
-- /Note:/ Consider using 'copyTagsToSnapshot' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdbiCopyTagsToSnapshot :: Lens.Lens' ModifyDBInstance (Core.Maybe Core.Bool)
mdbiCopyTagsToSnapshot = Lens.field @"copyTagsToSnapshot"
{-# DEPRECATED mdbiCopyTagsToSnapshot "Use generic-lens or generic-optics with 'copyTagsToSnapshot' instead." #-}

-- | The new compute and memory capacity of the DB instance, for example, @db.m4.large@ . Not all DB instance classes are available in all AWS Regions, or for all database engines. For the full list of DB instance classes, and availability for your engine, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Concepts.DBInstanceClass.html DB Instance Class> in the /Amazon RDS User Guide./
--
-- If you modify the DB instance class, an outage occurs during the change. The change is applied during the next maintenance window, unless @ApplyImmediately@ is enabled for this request.
-- Default: Uses existing setting
--
-- /Note:/ Consider using 'dBInstanceClass' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdbiDBInstanceClass :: Lens.Lens' ModifyDBInstance (Core.Maybe Types.DBInstanceClass)
mdbiDBInstanceClass = Lens.field @"dBInstanceClass"
{-# DEPRECATED mdbiDBInstanceClass "Use generic-lens or generic-optics with 'dBInstanceClass' instead." #-}

-- | The name of the DB parameter group to apply to the DB instance. Changing this setting doesn't result in an outage. The parameter group name itself is changed immediately, but the actual parameter changes are not applied until you reboot the instance without failover. In this case, the DB instance isn't rebooted automatically and the parameter changes isn't applied during the next maintenance window.
--
-- Default: Uses existing setting
-- Constraints: The DB parameter group must be in the same DB parameter group family as this DB instance.
--
-- /Note:/ Consider using 'dBParameterGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdbiDBParameterGroupName :: Lens.Lens' ModifyDBInstance (Core.Maybe Types.DBParameterGroupName)
mdbiDBParameterGroupName = Lens.field @"dBParameterGroupName"
{-# DEPRECATED mdbiDBParameterGroupName "Use generic-lens or generic-optics with 'dBParameterGroupName' instead." #-}

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
-- /Note:/ Consider using 'dBPortNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdbiDBPortNumber :: Lens.Lens' ModifyDBInstance (Core.Maybe Core.Int)
mdbiDBPortNumber = Lens.field @"dBPortNumber"
{-# DEPRECATED mdbiDBPortNumber "Use generic-lens or generic-optics with 'dBPortNumber' instead." #-}

-- | A list of DB security groups to authorize on this DB instance. Changing this setting doesn't result in an outage and the change is asynchronously applied as soon as possible.
--
-- Constraints:
--
--     * If supplied, must match existing DBSecurityGroups.
--
--
--
-- /Note:/ Consider using 'dBSecurityGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdbiDBSecurityGroups :: Lens.Lens' ModifyDBInstance (Core.Maybe [Types.String])
mdbiDBSecurityGroups = Lens.field @"dBSecurityGroups"
{-# DEPRECATED mdbiDBSecurityGroups "Use generic-lens or generic-optics with 'dBSecurityGroups' instead." #-}

-- | The new DB subnet group for the DB instance. You can use this parameter to move your DB instance to a different VPC. If your DB instance isn't in a VPC, you can also use this parameter to move your DB instance into a VPC. For more information, see <http://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_VPC.WorkingWithRDSInstanceinaVPC.html#USER_VPC.Non-VPC2VPC Updating the VPC for a DB Instance> in the /Amazon RDS User Guide./
--
-- Changing the subnet group causes an outage during the change. The change is applied during the next maintenance window, unless you enable @ApplyImmediately@ .
-- Constraints: If supplied, must match the name of an existing DBSubnetGroup.
-- Example: @mySubnetGroup@
--
-- /Note:/ Consider using 'dBSubnetGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdbiDBSubnetGroupName :: Lens.Lens' ModifyDBInstance (Core.Maybe Types.DBSubnetGroupName)
mdbiDBSubnetGroupName = Lens.field @"dBSubnetGroupName"
{-# DEPRECATED mdbiDBSubnetGroupName "Use generic-lens or generic-optics with 'dBSubnetGroupName' instead." #-}

-- | A value that indicates whether the DB instance has deletion protection enabled. The database can't be deleted when deletion protection is enabled. By default, deletion protection is disabled. For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_DeleteInstance.html Deleting a DB Instance> .
--
-- /Note:/ Consider using 'deletionProtection' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdbiDeletionProtection :: Lens.Lens' ModifyDBInstance (Core.Maybe Core.Bool)
mdbiDeletionProtection = Lens.field @"deletionProtection"
{-# DEPRECATED mdbiDeletionProtection "Use generic-lens or generic-optics with 'deletionProtection' instead." #-}

-- | The Active Directory directory ID to move the DB instance to. Specify @none@ to remove the instance from its current domain. The domain must be created prior to this operation. Currently, only MySQL, Microsoft SQL Server, Oracle, and PostgreSQL DB instances can be created in an Active Directory Domain.
--
-- For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/kerberos-authentication.html Kerberos Authentication> in the /Amazon RDS User Guide/ .
--
-- /Note:/ Consider using 'domain' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdbiDomain :: Lens.Lens' ModifyDBInstance (Core.Maybe Types.Domain)
mdbiDomain = Lens.field @"domain"
{-# DEPRECATED mdbiDomain "Use generic-lens or generic-optics with 'domain' instead." #-}

-- | The name of the IAM role to use when making API calls to the Directory Service.
--
-- /Note:/ Consider using 'domainIAMRoleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdbiDomainIAMRoleName :: Lens.Lens' ModifyDBInstance (Core.Maybe Types.DomainIAMRoleName)
mdbiDomainIAMRoleName = Lens.field @"domainIAMRoleName"
{-# DEPRECATED mdbiDomainIAMRoleName "Use generic-lens or generic-optics with 'domainIAMRoleName' instead." #-}

-- | A value that indicates whether to enable mapping of AWS Identity and Access Management (IAM) accounts to database accounts. By default, mapping is disabled.
--
-- This setting doesn't apply to Amazon Aurora. Mapping AWS IAM accounts to database accounts is managed by the DB cluster.
-- For more information about IAM database authentication, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/UsingWithRDS.IAMDBAuth.html IAM Database Authentication for MySQL and PostgreSQL> in the /Amazon RDS User Guide./
--
-- /Note:/ Consider using 'enableIAMDatabaseAuthentication' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdbiEnableIAMDatabaseAuthentication :: Lens.Lens' ModifyDBInstance (Core.Maybe Core.Bool)
mdbiEnableIAMDatabaseAuthentication = Lens.field @"enableIAMDatabaseAuthentication"
{-# DEPRECATED mdbiEnableIAMDatabaseAuthentication "Use generic-lens or generic-optics with 'enableIAMDatabaseAuthentication' instead." #-}

-- | A value that indicates whether to enable Performance Insights for the DB instance.
--
-- For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_PerfInsights.html Using Amazon Performance Insights> in the /Amazon Relational Database Service User Guide/ .
--
-- /Note:/ Consider using 'enablePerformanceInsights' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdbiEnablePerformanceInsights :: Lens.Lens' ModifyDBInstance (Core.Maybe Core.Bool)
mdbiEnablePerformanceInsights = Lens.field @"enablePerformanceInsights"
{-# DEPRECATED mdbiEnablePerformanceInsights "Use generic-lens or generic-optics with 'enablePerformanceInsights' instead." #-}

-- | The version number of the database engine to upgrade to. Changing this parameter results in an outage and the change is applied during the next maintenance window unless the @ApplyImmediately@ parameter is enabled for this request.
--
-- For major version upgrades, if a nondefault DB parameter group is currently in use, a new DB parameter group in the DB parameter group family for the new engine version must be specified. The new DB parameter group can be the default for that DB parameter group family.
-- For information about valid engine versions, see @CreateDBInstance@ , or call @DescribeDBEngineVersions@ .
--
-- /Note:/ Consider using 'engineVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdbiEngineVersion :: Lens.Lens' ModifyDBInstance (Core.Maybe Types.EngineVersion)
mdbiEngineVersion = Lens.field @"engineVersion"
{-# DEPRECATED mdbiEngineVersion "Use generic-lens or generic-optics with 'engineVersion' instead." #-}

-- | The new Provisioned IOPS (I/O operations per second) value for the RDS instance.
--
-- Changing this setting doesn't result in an outage and the change is applied during the next maintenance window unless the @ApplyImmediately@ parameter is enabled for this request. If you are migrating from Provisioned IOPS to standard storage, set this value to 0. The DB instance will require a reboot for the change in storage type to take effect.
-- If you choose to migrate your DB instance from using standard storage to using Provisioned IOPS, or from using Provisioned IOPS to using standard storage, the process can take time. The duration of the migration depends on several factors such as database load, storage size, storage type (standard or Provisioned IOPS), amount of IOPS provisioned (if any), and the number of prior scale storage operations. Typical migration times are under 24 hours, but the process can take up to several days in some cases. During the migration, the DB instance is available for use, but might experience performance degradation. While the migration takes place, nightly backups for the instance are suspended. No other Amazon RDS operations can take place for the instance, including modifying the instance, rebooting the instance, deleting the instance, creating a read replica for the instance, and creating a DB snapshot of the instance.
-- Constraints: For MariaDB, MySQL, Oracle, and PostgreSQL, the value supplied must be at least 10% greater than the current value. Values that are not at least 10% greater than the existing value are rounded up so that they are 10% greater than the current value.
-- Default: Uses existing setting
--
-- /Note:/ Consider using 'iops' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdbiIops :: Lens.Lens' ModifyDBInstance (Core.Maybe Core.Int)
mdbiIops = Lens.field @"iops"
{-# DEPRECATED mdbiIops "Use generic-lens or generic-optics with 'iops' instead." #-}

-- | The license model for the DB instance.
--
-- Valid values: @license-included@ | @bring-your-own-license@ | @general-public-license@
--
-- /Note:/ Consider using 'licenseModel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdbiLicenseModel :: Lens.Lens' ModifyDBInstance (Core.Maybe Types.LicenseModel)
mdbiLicenseModel = Lens.field @"licenseModel"
{-# DEPRECATED mdbiLicenseModel "Use generic-lens or generic-optics with 'licenseModel' instead." #-}

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
mdbiMasterUserPassword :: Lens.Lens' ModifyDBInstance (Core.Maybe Types.MasterUserPassword)
mdbiMasterUserPassword = Lens.field @"masterUserPassword"
{-# DEPRECATED mdbiMasterUserPassword "Use generic-lens or generic-optics with 'masterUserPassword' instead." #-}

-- | The upper limit to which Amazon RDS can automatically scale the storage of the DB instance.
--
-- /Note:/ Consider using 'maxAllocatedStorage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdbiMaxAllocatedStorage :: Lens.Lens' ModifyDBInstance (Core.Maybe Core.Int)
mdbiMaxAllocatedStorage = Lens.field @"maxAllocatedStorage"
{-# DEPRECATED mdbiMaxAllocatedStorage "Use generic-lens or generic-optics with 'maxAllocatedStorage' instead." #-}

-- | The interval, in seconds, between points when Enhanced Monitoring metrics are collected for the DB instance. To disable collecting Enhanced Monitoring metrics, specify 0. The default is 0.
--
-- If @MonitoringRoleArn@ is specified, then you must also set @MonitoringInterval@ to a value other than 0.
-- Valid Values: @0, 1, 5, 10, 15, 30, 60@
--
-- /Note:/ Consider using 'monitoringInterval' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdbiMonitoringInterval :: Lens.Lens' ModifyDBInstance (Core.Maybe Core.Int)
mdbiMonitoringInterval = Lens.field @"monitoringInterval"
{-# DEPRECATED mdbiMonitoringInterval "Use generic-lens or generic-optics with 'monitoringInterval' instead." #-}

-- | The ARN for the IAM role that permits RDS to send enhanced monitoring metrics to Amazon CloudWatch Logs. For example, @arn:aws:iam:123456789012:role/emaccess@ . For information on creating a monitoring role, go to <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_Monitoring.html#USER_Monitoring.OS.IAMRole To create an IAM role for Amazon RDS Enhanced Monitoring> in the /Amazon RDS User Guide./
--
-- If @MonitoringInterval@ is set to a value other than 0, then you must supply a @MonitoringRoleArn@ value.
--
-- /Note:/ Consider using 'monitoringRoleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdbiMonitoringRoleArn :: Lens.Lens' ModifyDBInstance (Core.Maybe Types.MonitoringRoleArn)
mdbiMonitoringRoleArn = Lens.field @"monitoringRoleArn"
{-# DEPRECATED mdbiMonitoringRoleArn "Use generic-lens or generic-optics with 'monitoringRoleArn' instead." #-}

-- | A value that indicates whether the DB instance is a Multi-AZ deployment. Changing this parameter doesn't result in an outage and the change is applied during the next maintenance window unless the @ApplyImmediately@ parameter is enabled for this request.
--
-- /Note:/ Consider using 'multiAZ' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdbiMultiAZ :: Lens.Lens' ModifyDBInstance (Core.Maybe Core.Bool)
mdbiMultiAZ = Lens.field @"multiAZ"
{-# DEPRECATED mdbiMultiAZ "Use generic-lens or generic-optics with 'multiAZ' instead." #-}

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
mdbiNewDBInstanceIdentifier :: Lens.Lens' ModifyDBInstance (Core.Maybe Types.NewDBInstanceIdentifier)
mdbiNewDBInstanceIdentifier = Lens.field @"newDBInstanceIdentifier"
{-# DEPRECATED mdbiNewDBInstanceIdentifier "Use generic-lens or generic-optics with 'newDBInstanceIdentifier' instead." #-}

-- | Indicates that the DB instance should be associated with the specified option group. Changing this parameter doesn't result in an outage except in the following case and the change is applied during the next maintenance window unless the @ApplyImmediately@ parameter is enabled for this request. If the parameter change results in an option group that enables OEM, this change can cause a brief (sub-second) period during which new connections are rejected but existing connections are not interrupted.
--
-- Permanent options, such as the TDE option for Oracle Advanced Security TDE, can't be removed from an option group, and that option group can't be removed from a DB instance once it is associated with a DB instance
--
-- /Note:/ Consider using 'optionGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdbiOptionGroupName :: Lens.Lens' ModifyDBInstance (Core.Maybe Types.OptionGroupName)
mdbiOptionGroupName = Lens.field @"optionGroupName"
{-# DEPRECATED mdbiOptionGroupName "Use generic-lens or generic-optics with 'optionGroupName' instead." #-}

-- | The AWS KMS key identifier for encryption of Performance Insights data. The KMS key ID is the Amazon Resource Name (ARN), KMS key identifier, or the KMS key alias for the KMS encryption key.
--
-- If you do not specify a value for @PerformanceInsightsKMSKeyId@ , then Amazon RDS uses your default encryption key. AWS KMS creates the default encryption key for your AWS account. Your AWS account has a different default encryption key for each AWS Region.
--
-- /Note:/ Consider using 'performanceInsightsKMSKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdbiPerformanceInsightsKMSKeyId :: Lens.Lens' ModifyDBInstance (Core.Maybe Types.PerformanceInsightsKMSKeyId)
mdbiPerformanceInsightsKMSKeyId = Lens.field @"performanceInsightsKMSKeyId"
{-# DEPRECATED mdbiPerformanceInsightsKMSKeyId "Use generic-lens or generic-optics with 'performanceInsightsKMSKeyId' instead." #-}

-- | The amount of time, in days, to retain Performance Insights data. Valid values are 7 or 731 (2 years).
--
-- /Note:/ Consider using 'performanceInsightsRetentionPeriod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdbiPerformanceInsightsRetentionPeriod :: Lens.Lens' ModifyDBInstance (Core.Maybe Core.Int)
mdbiPerformanceInsightsRetentionPeriod = Lens.field @"performanceInsightsRetentionPeriod"
{-# DEPRECATED mdbiPerformanceInsightsRetentionPeriod "Use generic-lens or generic-optics with 'performanceInsightsRetentionPeriod' instead." #-}

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
mdbiPreferredBackupWindow :: Lens.Lens' ModifyDBInstance (Core.Maybe Types.PreferredBackupWindow)
mdbiPreferredBackupWindow = Lens.field @"preferredBackupWindow"
{-# DEPRECATED mdbiPreferredBackupWindow "Use generic-lens or generic-optics with 'preferredBackupWindow' instead." #-}

-- | The weekly time range (in UTC) during which system maintenance can occur, which might result in an outage. Changing this parameter doesn't result in an outage, except in the following situation, and the change is asynchronously applied as soon as possible. If there are pending actions that cause a reboot, and the maintenance window is changed to include the current time, then changing this parameter will cause a reboot of the DB instance. If moving this window to the current time, there must be at least 30 minutes between the current time and end of the window to ensure pending changes are applied.
--
-- Default: Uses existing setting
-- Format: ddd:hh24:mi-ddd:hh24:mi
-- Valid Days: Mon | Tue | Wed | Thu | Fri | Sat | Sun
-- Constraints: Must be at least 30 minutes
--
-- /Note:/ Consider using 'preferredMaintenanceWindow' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdbiPreferredMaintenanceWindow :: Lens.Lens' ModifyDBInstance (Core.Maybe Types.PreferredMaintenanceWindow)
mdbiPreferredMaintenanceWindow = Lens.field @"preferredMaintenanceWindow"
{-# DEPRECATED mdbiPreferredMaintenanceWindow "Use generic-lens or generic-optics with 'preferredMaintenanceWindow' instead." #-}

-- | The number of CPU cores and the number of threads per core for the DB instance class of the DB instance.
--
-- /Note:/ Consider using 'processorFeatures' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdbiProcessorFeatures :: Lens.Lens' ModifyDBInstance (Core.Maybe [Types.ProcessorFeature])
mdbiProcessorFeatures = Lens.field @"processorFeatures"
{-# DEPRECATED mdbiProcessorFeatures "Use generic-lens or generic-optics with 'processorFeatures' instead." #-}

-- | A value that specifies the order in which an Aurora Replica is promoted to the primary instance after a failure of the existing primary instance. For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/Aurora.Managing.Backups.html#Aurora.Managing.FaultTolerance Fault Tolerance for an Aurora DB Cluster> in the /Amazon Aurora User Guide/ .
--
-- Default: 1
-- Valid Values: 0 - 15
--
-- /Note:/ Consider using 'promotionTier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdbiPromotionTier :: Lens.Lens' ModifyDBInstance (Core.Maybe Core.Int)
mdbiPromotionTier = Lens.field @"promotionTier"
{-# DEPRECATED mdbiPromotionTier "Use generic-lens or generic-optics with 'promotionTier' instead." #-}

-- | A value that indicates whether the DB instance is publicly accessible.
--
-- When the DB instance is publicly accessible, its DNS endpoint resolves to the private IP address from within the DB instance's VPC, and to the public IP address from outside of the DB instance's VPC. Access to the DB instance is ultimately controlled by the security group it uses, and that public access is not permitted if the security group assigned to the DB instance doesn't permit it.
-- When the DB instance isn't publicly accessible, it is an internal DB instance with a DNS name that resolves to a private IP address.
-- @PubliclyAccessible@ only applies to DB instances in a VPC. The DB instance must be part of a public subnet and @PubliclyAccessible@ must be enabled for it to be publicly accessible.
-- Changes to the @PubliclyAccessible@ parameter are applied immediately regardless of the value of the @ApplyImmediately@ parameter.
--
-- /Note:/ Consider using 'publiclyAccessible' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdbiPubliclyAccessible :: Lens.Lens' ModifyDBInstance (Core.Maybe Core.Bool)
mdbiPubliclyAccessible = Lens.field @"publiclyAccessible"
{-# DEPRECATED mdbiPubliclyAccessible "Use generic-lens or generic-optics with 'publiclyAccessible' instead." #-}

-- | A value that sets the open mode of a replica database to either mounted or read-only.
--
-- Mounted DB replicas are included in Oracle Enterprise Edition. The main use case for mounted replicas is cross-Region disaster recovery. The primary database doesn't use Active Data Guard to transmit information to the mounted replica. Because it doesn't accept user connections, a mounted replica can't serve a read-only workload. For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/oracle-read-replicas.html Working with Oracle Read Replicas for Amazon RDS> in the /Amazon RDS User Guide/ .
--
-- /Note:/ Consider using 'replicaMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdbiReplicaMode :: Lens.Lens' ModifyDBInstance (Core.Maybe Types.ReplicaMode)
mdbiReplicaMode = Lens.field @"replicaMode"
{-# DEPRECATED mdbiReplicaMode "Use generic-lens or generic-optics with 'replicaMode' instead." #-}

-- | Specifies the storage type to be associated with the DB instance.
--
-- If you specify Provisioned IOPS (@io1@ ), you must also include a value for the @Iops@ parameter.
-- If you choose to migrate your DB instance from using standard storage to using Provisioned IOPS, or from using Provisioned IOPS to using standard storage, the process can take time. The duration of the migration depends on several factors such as database load, storage size, storage type (standard or Provisioned IOPS), amount of IOPS provisioned (if any), and the number of prior scale storage operations. Typical migration times are under 24 hours, but the process can take up to several days in some cases. During the migration, the DB instance is available for use, but might experience performance degradation. While the migration takes place, nightly backups for the instance are suspended. No other Amazon RDS operations can take place for the instance, including modifying the instance, rebooting the instance, deleting the instance, creating a read replica for the instance, and creating a DB snapshot of the instance.
-- Valid values: @standard | gp2 | io1@
-- Default: @io1@ if the @Iops@ parameter is specified, otherwise @gp2@
--
-- /Note:/ Consider using 'storageType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdbiStorageType :: Lens.Lens' ModifyDBInstance (Core.Maybe Types.StorageType)
mdbiStorageType = Lens.field @"storageType"
{-# DEPRECATED mdbiStorageType "Use generic-lens or generic-optics with 'storageType' instead." #-}

-- | The ARN from the key store with which to associate the instance for TDE encryption.
--
-- /Note:/ Consider using 'tdeCredentialArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdbiTdeCredentialArn :: Lens.Lens' ModifyDBInstance (Core.Maybe Types.TdeCredentialArn)
mdbiTdeCredentialArn = Lens.field @"tdeCredentialArn"
{-# DEPRECATED mdbiTdeCredentialArn "Use generic-lens or generic-optics with 'tdeCredentialArn' instead." #-}

-- | The password for the given ARN from the key store in order to access the device.
--
-- /Note:/ Consider using 'tdeCredentialPassword' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdbiTdeCredentialPassword :: Lens.Lens' ModifyDBInstance (Core.Maybe Types.TdeCredentialPassword)
mdbiTdeCredentialPassword = Lens.field @"tdeCredentialPassword"
{-# DEPRECATED mdbiTdeCredentialPassword "Use generic-lens or generic-optics with 'tdeCredentialPassword' instead." #-}

-- | A value that indicates whether the DB instance class of the DB instance uses its default processor features.
--
-- /Note:/ Consider using 'useDefaultProcessorFeatures' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdbiUseDefaultProcessorFeatures :: Lens.Lens' ModifyDBInstance (Core.Maybe Core.Bool)
mdbiUseDefaultProcessorFeatures = Lens.field @"useDefaultProcessorFeatures"
{-# DEPRECATED mdbiUseDefaultProcessorFeatures "Use generic-lens or generic-optics with 'useDefaultProcessorFeatures' instead." #-}

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
mdbiVpcSecurityGroupIds :: Lens.Lens' ModifyDBInstance (Core.Maybe [Types.String])
mdbiVpcSecurityGroupIds = Lens.field @"vpcSecurityGroupIds"
{-# DEPRECATED mdbiVpcSecurityGroupIds "Use generic-lens or generic-optics with 'vpcSecurityGroupIds' instead." #-}

instance Core.AWSRequest ModifyDBInstance where
  type Rs ModifyDBInstance = ModifyDBInstanceResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "ModifyDBInstance")
                Core.<> (Core.pure ("Version", "2014-10-31"))
                Core.<> (Core.toQueryValue "DBInstanceIdentifier" dBInstanceIdentifier)
                Core.<> (Core.toQueryValue "AllocatedStorage" Core.<$> allocatedStorage)
                Core.<> ( Core.toQueryValue "AllowMajorVersionUpgrade"
                            Core.<$> allowMajorVersionUpgrade
                        )
                Core.<> (Core.toQueryValue "ApplyImmediately" Core.<$> applyImmediately)
                Core.<> ( Core.toQueryValue "AutoMinorVersionUpgrade"
                            Core.<$> autoMinorVersionUpgrade
                        )
                Core.<> ( Core.toQueryValue "BackupRetentionPeriod"
                            Core.<$> backupRetentionPeriod
                        )
                Core.<> ( Core.toQueryValue "CACertificateIdentifier"
                            Core.<$> cACertificateIdentifier
                        )
                Core.<> ( Core.toQueryValue "CertificateRotationRestart"
                            Core.<$> certificateRotationRestart
                        )
                Core.<> ( Core.toQueryValue "CloudwatchLogsExportConfiguration"
                            Core.<$> cloudwatchLogsExportConfiguration
                        )
                Core.<> ( Core.toQueryValue "CopyTagsToSnapshot"
                            Core.<$> copyTagsToSnapshot
                        )
                Core.<> (Core.toQueryValue "DBInstanceClass" Core.<$> dBInstanceClass)
                Core.<> ( Core.toQueryValue "DBParameterGroupName"
                            Core.<$> dBParameterGroupName
                        )
                Core.<> (Core.toQueryValue "DBPortNumber" Core.<$> dBPortNumber)
                Core.<> ( Core.toQueryValue
                            "DBSecurityGroups"
                            ( Core.toQueryList "DBSecurityGroupName"
                                Core.<$> dBSecurityGroups
                            )
                        )
                Core.<> (Core.toQueryValue "DBSubnetGroupName" Core.<$> dBSubnetGroupName)
                Core.<> ( Core.toQueryValue "DeletionProtection"
                            Core.<$> deletionProtection
                        )
                Core.<> (Core.toQueryValue "Domain" Core.<$> domain)
                Core.<> (Core.toQueryValue "DomainIAMRoleName" Core.<$> domainIAMRoleName)
                Core.<> ( Core.toQueryValue "EnableIAMDatabaseAuthentication"
                            Core.<$> enableIAMDatabaseAuthentication
                        )
                Core.<> ( Core.toQueryValue "EnablePerformanceInsights"
                            Core.<$> enablePerformanceInsights
                        )
                Core.<> (Core.toQueryValue "EngineVersion" Core.<$> engineVersion)
                Core.<> (Core.toQueryValue "Iops" Core.<$> iops)
                Core.<> (Core.toQueryValue "LicenseModel" Core.<$> licenseModel)
                Core.<> ( Core.toQueryValue "MasterUserPassword"
                            Core.<$> masterUserPassword
                        )
                Core.<> ( Core.toQueryValue "MaxAllocatedStorage"
                            Core.<$> maxAllocatedStorage
                        )
                Core.<> ( Core.toQueryValue "MonitoringInterval"
                            Core.<$> monitoringInterval
                        )
                Core.<> (Core.toQueryValue "MonitoringRoleArn" Core.<$> monitoringRoleArn)
                Core.<> (Core.toQueryValue "MultiAZ" Core.<$> multiAZ)
                Core.<> ( Core.toQueryValue "NewDBInstanceIdentifier"
                            Core.<$> newDBInstanceIdentifier
                        )
                Core.<> (Core.toQueryValue "OptionGroupName" Core.<$> optionGroupName)
                Core.<> ( Core.toQueryValue "PerformanceInsightsKMSKeyId"
                            Core.<$> performanceInsightsKMSKeyId
                        )
                Core.<> ( Core.toQueryValue "PerformanceInsightsRetentionPeriod"
                            Core.<$> performanceInsightsRetentionPeriod
                        )
                Core.<> ( Core.toQueryValue "PreferredBackupWindow"
                            Core.<$> preferredBackupWindow
                        )
                Core.<> ( Core.toQueryValue "PreferredMaintenanceWindow"
                            Core.<$> preferredMaintenanceWindow
                        )
                Core.<> ( Core.toQueryValue
                            "ProcessorFeatures"
                            (Core.toQueryList "ProcessorFeature" Core.<$> processorFeatures)
                        )
                Core.<> (Core.toQueryValue "PromotionTier" Core.<$> promotionTier)
                Core.<> ( Core.toQueryValue "PubliclyAccessible"
                            Core.<$> publiclyAccessible
                        )
                Core.<> (Core.toQueryValue "ReplicaMode" Core.<$> replicaMode)
                Core.<> (Core.toQueryValue "StorageType" Core.<$> storageType)
                Core.<> (Core.toQueryValue "TdeCredentialArn" Core.<$> tdeCredentialArn)
                Core.<> ( Core.toQueryValue "TdeCredentialPassword"
                            Core.<$> tdeCredentialPassword
                        )
                Core.<> ( Core.toQueryValue "UseDefaultProcessorFeatures"
                            Core.<$> useDefaultProcessorFeatures
                        )
                Core.<> ( Core.toQueryValue
                            "VpcSecurityGroupIds"
                            ( Core.toQueryList "VpcSecurityGroupId"
                                Core.<$> vpcSecurityGroupIds
                            )
                        )
            )
      }
  response =
    Response.receiveXMLWrapper
      "ModifyDBInstanceResult"
      ( \s h x ->
          ModifyDBInstanceResponse'
            Core.<$> (x Core..@? "DBInstance") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkModifyDBInstanceResponse' smart constructor.
data ModifyDBInstanceResponse = ModifyDBInstanceResponse'
  { dBInstance :: Core.Maybe Types.DBInstance,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ModifyDBInstanceResponse' value with any optional fields omitted.
mkModifyDBInstanceResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ModifyDBInstanceResponse
mkModifyDBInstanceResponse responseStatus =
  ModifyDBInstanceResponse'
    { dBInstance = Core.Nothing,
      responseStatus
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'dBInstance' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdbirrsDBInstance :: Lens.Lens' ModifyDBInstanceResponse (Core.Maybe Types.DBInstance)
mdbirrsDBInstance = Lens.field @"dBInstance"
{-# DEPRECATED mdbirrsDBInstance "Use generic-lens or generic-optics with 'dBInstance' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdbirrsResponseStatus :: Lens.Lens' ModifyDBInstanceResponse Core.Int
mdbirrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED mdbirrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
