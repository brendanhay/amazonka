{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.RestoreDBInstanceToPointInTime
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Restores a DB instance to an arbitrary point in time. You can restore to any point in time before the time identified by the LatestRestorableTime property. You can restore to a point up to the number of days specified by the BackupRetentionPeriod property.
--
-- The target database is created with most of the original configuration, but in a system-selected Availability Zone, with the default security group, the default subnet group, and the default DB parameter group. By default, the new DB instance is created as a single-AZ deployment except when the instance is a SQL Server instance that has an option group that is associated with mirroring; in this case, the instance becomes a mirrored deployment and not a single-AZ deployment.
module Network.AWS.RDS.RestoreDBInstanceToPointInTime
  ( -- * Creating a request
    RestoreDBInstanceToPointInTime (..),
    mkRestoreDBInstanceToPointInTime,

    -- ** Request lenses
    rdbitpitTargetDBInstanceIdentifier,
    rdbitpitAutoMinorVersionUpgrade,
    rdbitpitAvailabilityZone,
    rdbitpitCopyTagsToSnapshot,
    rdbitpitDBInstanceClass,
    rdbitpitDBName,
    rdbitpitDBParameterGroupName,
    rdbitpitDBSubnetGroupName,
    rdbitpitDeletionProtection,
    rdbitpitDomain,
    rdbitpitDomainIAMRoleName,
    rdbitpitEnableCloudwatchLogsExports,
    rdbitpitEnableIAMDatabaseAuthentication,
    rdbitpitEngine,
    rdbitpitIops,
    rdbitpitLicenseModel,
    rdbitpitMaxAllocatedStorage,
    rdbitpitMultiAZ,
    rdbitpitOptionGroupName,
    rdbitpitPort,
    rdbitpitProcessorFeatures,
    rdbitpitPubliclyAccessible,
    rdbitpitRestoreTime,
    rdbitpitSourceDBInstanceIdentifier,
    rdbitpitSourceDbiResourceId,
    rdbitpitStorageType,
    rdbitpitTags,
    rdbitpitTdeCredentialArn,
    rdbitpitTdeCredentialPassword,
    rdbitpitUseDefaultProcessorFeatures,
    rdbitpitUseLatestRestorableTime,
    rdbitpitVpcSecurityGroupIds,

    -- * Destructuring the response
    RestoreDBInstanceToPointInTimeResponse (..),
    mkRestoreDBInstanceToPointInTimeResponse,

    -- ** Response lenses
    rdbitpitrrsDBInstance,
    rdbitpitrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'mkRestoreDBInstanceToPointInTime' smart constructor.
data RestoreDBInstanceToPointInTime = RestoreDBInstanceToPointInTime'
  { -- | The name of the new DB instance to be created.
    --
    -- Constraints:
    --
    --     * Must contain from 1 to 63 letters, numbers, or hyphens
    --
    --
    --     * First character must be a letter
    --
    --
    --     * Can't end with a hyphen or contain two consecutive hyphens
    targetDBInstanceIdentifier :: Types.String,
    -- | A value that indicates whether minor version upgrades are applied automatically to the DB instance during the maintenance window.
    autoMinorVersionUpgrade :: Core.Maybe Core.Bool,
    -- | The Availability Zone (AZ) where the DB instance will be created.
    --
    -- Default: A random, system-chosen Availability Zone.
    -- Constraint: You can't specify the @AvailabilityZone@ parameter if the DB instance is a Multi-AZ deployment.
    -- Example: @us-east-1a@
    availabilityZone :: Core.Maybe Types.String,
    -- | A value that indicates whether to copy all tags from the restored DB instance to snapshots of the DB instance. By default, tags are not copied.
    copyTagsToSnapshot :: Core.Maybe Core.Bool,
    -- | The compute and memory capacity of the Amazon RDS DB instance, for example, @db.m4.large@ . Not all DB instance classes are available in all AWS Regions, or for all database engines. For the full list of DB instance classes, and availability for your engine, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Concepts.DBInstanceClass.html DB Instance Class> in the /Amazon RDS User Guide./
    --
    -- Default: The same DBInstanceClass as the original DB instance.
    dBInstanceClass :: Core.Maybe Types.String,
    -- | The database name for the restored DB instance.
    dBName :: Core.Maybe Types.String,
    -- | The name of the DB parameter group to associate with this DB instance.
    --
    -- If you do not specify a value for @DBParameterGroupName@ , then the default @DBParameterGroup@ for the specified DB engine is used.
    -- Constraints:
    --
    --     * If supplied, must match the name of an existing DBParameterGroup.
    --
    --
    --     * Must be 1 to 255 letters, numbers, or hyphens.
    --
    --
    --     * First character must be a letter.
    --
    --
    --     * Can't end with a hyphen or contain two consecutive hyphens.
    dBParameterGroupName :: Core.Maybe Types.String,
    -- | The DB subnet group name to use for the new instance.
    --
    -- Constraints: If supplied, must match the name of an existing DBSubnetGroup.
    -- Example: @mySubnetgroup@
    dBSubnetGroupName :: Core.Maybe Types.String,
    -- | A value that indicates whether the DB instance has deletion protection enabled. The database can't be deleted when deletion protection is enabled. By default, deletion protection is disabled. For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_DeleteInstance.html Deleting a DB Instance> .
    deletionProtection :: Core.Maybe Core.Bool,
    -- | Specify the Active Directory directory ID to restore the DB instance in. The domain must be created prior to this operation. Currently, only MySQL, Microsoft SQL Server, Oracle, and PostgreSQL DB instances can be created in an Active Directory Domain.
    --
    -- For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/kerberos-authentication.html Kerberos Authentication> in the /Amazon RDS User Guide/ .
    domain :: Core.Maybe Types.String,
    -- | Specify the name of the IAM role to be used when making API calls to the Directory Service.
    domainIAMRoleName :: Core.Maybe Types.String,
    -- | The list of logs that the restored DB instance is to export to CloudWatch Logs. The values in the list depend on the DB engine being used. For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_LogAccess.html#USER_LogAccess.Procedural.UploadtoCloudWatch Publishing Database Logs to Amazon CloudWatch Logs> in the /Amazon RDS User Guide/ .
    enableCloudwatchLogsExports :: Core.Maybe [Types.String],
    -- | A value that indicates whether to enable mapping of AWS Identity and Access Management (IAM) accounts to database accounts. By default, mapping is disabled.
    --
    -- For more information about IAM database authentication, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/UsingWithRDS.IAMDBAuth.html IAM Database Authentication for MySQL and PostgreSQL> in the /Amazon RDS User Guide./
    enableIAMDatabaseAuthentication :: Core.Maybe Core.Bool,
    -- | The database engine to use for the new instance.
    --
    -- Default: The same as source
    -- Constraint: Must be compatible with the engine of the source
    -- Valid Values:
    --
    --     * @mariadb@
    --
    --
    --     * @mysql@
    --
    --
    --     * @oracle-ee@
    --
    --
    --     * @oracle-se2@
    --
    --
    --     * @oracle-se1@
    --
    --
    --     * @oracle-se@
    --
    --
    --     * @postgres@
    --
    --
    --     * @sqlserver-ee@
    --
    --
    --     * @sqlserver-se@
    --
    --
    --     * @sqlserver-ex@
    --
    --
    --     * @sqlserver-web@
    engine :: Core.Maybe Types.String,
    -- | The amount of Provisioned IOPS (input/output operations per second) to be initially allocated for the DB instance.
    --
    -- Constraints: Must be an integer greater than 1000.
    -- __SQL Server__
    -- Setting the IOPS value for the SQL Server database engine isn't supported.
    iops :: Core.Maybe Core.Int,
    -- | License model information for the restored DB instance.
    --
    -- Default: Same as source.
    -- Valid values: @license-included@ | @bring-your-own-license@ | @general-public-license@
    licenseModel :: Core.Maybe Types.String,
    -- | The upper limit to which Amazon RDS can automatically scale the storage of the DB instance.
    maxAllocatedStorage :: Core.Maybe Core.Int,
    -- | A value that indicates whether the DB instance is a Multi-AZ deployment.
    --
    -- Constraint: You can't specify the @AvailabilityZone@ parameter if the DB instance is a Multi-AZ deployment.
    multiAZ :: Core.Maybe Core.Bool,
    -- | The name of the option group to be used for the restored DB instance.
    --
    -- Permanent options, such as the TDE option for Oracle Advanced Security TDE, can't be removed from an option group, and that option group can't be removed from a DB instance once it is associated with a DB instance
    optionGroupName :: Core.Maybe Types.String,
    -- | The port number on which the database accepts connections.
    --
    -- Constraints: Value must be @1150-65535@
    -- Default: The same port as the original DB instance.
    port :: Core.Maybe Core.Int,
    -- | The number of CPU cores and the number of threads per core for the DB instance class of the DB instance.
    processorFeatures :: Core.Maybe [Types.ProcessorFeature],
    -- | A value that indicates whether the DB instance is publicly accessible.
    --
    -- When the DB instance is publicly accessible, its DNS endpoint resolves to the private IP address from within the DB instance's VPC, and to the public IP address from outside of the DB instance's VPC. Access to the DB instance is ultimately controlled by the security group it uses, and that public access is not permitted if the security group assigned to the DB instance doesn't permit it.
    -- When the DB instance isn't publicly accessible, it is an internal DB instance with a DNS name that resolves to a private IP address.
    -- For more information, see 'CreateDBInstance' .
    publiclyAccessible :: Core.Maybe Core.Bool,
    -- | The date and time to restore from.
    --
    -- Valid Values: Value must be a time in Universal Coordinated Time (UTC) format
    -- Constraints:
    --
    --     * Must be before the latest restorable time for the DB instance
    --
    --
    --     * Can't be specified if the @UseLatestRestorableTime@ parameter is enabled
    --
    --
    -- Example: @2009-09-07T23:45:00Z@
    restoreTime :: Core.Maybe Core.UTCTime,
    -- | The identifier of the source DB instance from which to restore.
    --
    -- Constraints:
    --
    --     * Must match the identifier of an existing DB instance.
    sourceDBInstanceIdentifier :: Core.Maybe Types.String,
    -- | The resource ID of the source DB instance from which to restore.
    sourceDbiResourceId :: Core.Maybe Types.String,
    -- | Specifies the storage type to be associated with the DB instance.
    --
    -- Valid values: @standard | gp2 | io1@
    -- If you specify @io1@ , you must also include a value for the @Iops@ parameter.
    -- Default: @io1@ if the @Iops@ parameter is specified, otherwise @gp2@
    storageType :: Core.Maybe Types.String,
    tags :: Core.Maybe [Types.Tag],
    -- | The ARN from the key store with which to associate the instance for TDE encryption.
    tdeCredentialArn :: Core.Maybe Types.String,
    -- | The password for the given ARN from the key store in order to access the device.
    tdeCredentialPassword :: Core.Maybe Types.String,
    -- | A value that indicates whether the DB instance class of the DB instance uses its default processor features.
    useDefaultProcessorFeatures :: Core.Maybe Core.Bool,
    -- | A value that indicates whether the DB instance is restored from the latest backup time. By default, the DB instance isn't restored from the latest backup time.
    --
    -- Constraints: Can't be specified if the @RestoreTime@ parameter is provided.
    useLatestRestorableTime :: Core.Maybe Core.Bool,
    -- | A list of EC2 VPC security groups to associate with this DB instance.
    --
    -- Default: The default EC2 VPC security group for the DB subnet group's VPC.
    vpcSecurityGroupIds :: Core.Maybe [Types.String]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'RestoreDBInstanceToPointInTime' value with any optional fields omitted.
mkRestoreDBInstanceToPointInTime ::
  -- | 'targetDBInstanceIdentifier'
  Types.String ->
  RestoreDBInstanceToPointInTime
mkRestoreDBInstanceToPointInTime targetDBInstanceIdentifier =
  RestoreDBInstanceToPointInTime'
    { targetDBInstanceIdentifier,
      autoMinorVersionUpgrade = Core.Nothing,
      availabilityZone = Core.Nothing,
      copyTagsToSnapshot = Core.Nothing,
      dBInstanceClass = Core.Nothing,
      dBName = Core.Nothing,
      dBParameterGroupName = Core.Nothing,
      dBSubnetGroupName = Core.Nothing,
      deletionProtection = Core.Nothing,
      domain = Core.Nothing,
      domainIAMRoleName = Core.Nothing,
      enableCloudwatchLogsExports = Core.Nothing,
      enableIAMDatabaseAuthentication = Core.Nothing,
      engine = Core.Nothing,
      iops = Core.Nothing,
      licenseModel = Core.Nothing,
      maxAllocatedStorage = Core.Nothing,
      multiAZ = Core.Nothing,
      optionGroupName = Core.Nothing,
      port = Core.Nothing,
      processorFeatures = Core.Nothing,
      publiclyAccessible = Core.Nothing,
      restoreTime = Core.Nothing,
      sourceDBInstanceIdentifier = Core.Nothing,
      sourceDbiResourceId = Core.Nothing,
      storageType = Core.Nothing,
      tags = Core.Nothing,
      tdeCredentialArn = Core.Nothing,
      tdeCredentialPassword = Core.Nothing,
      useDefaultProcessorFeatures = Core.Nothing,
      useLatestRestorableTime = Core.Nothing,
      vpcSecurityGroupIds = Core.Nothing
    }

-- | The name of the new DB instance to be created.
--
-- Constraints:
--
--     * Must contain from 1 to 63 letters, numbers, or hyphens
--
--
--     * First character must be a letter
--
--
--     * Can't end with a hyphen or contain two consecutive hyphens
--
--
--
-- /Note:/ Consider using 'targetDBInstanceIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbitpitTargetDBInstanceIdentifier :: Lens.Lens' RestoreDBInstanceToPointInTime Types.String
rdbitpitTargetDBInstanceIdentifier = Lens.field @"targetDBInstanceIdentifier"
{-# DEPRECATED rdbitpitTargetDBInstanceIdentifier "Use generic-lens or generic-optics with 'targetDBInstanceIdentifier' instead." #-}

-- | A value that indicates whether minor version upgrades are applied automatically to the DB instance during the maintenance window.
--
-- /Note:/ Consider using 'autoMinorVersionUpgrade' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbitpitAutoMinorVersionUpgrade :: Lens.Lens' RestoreDBInstanceToPointInTime (Core.Maybe Core.Bool)
rdbitpitAutoMinorVersionUpgrade = Lens.field @"autoMinorVersionUpgrade"
{-# DEPRECATED rdbitpitAutoMinorVersionUpgrade "Use generic-lens or generic-optics with 'autoMinorVersionUpgrade' instead." #-}

-- | The Availability Zone (AZ) where the DB instance will be created.
--
-- Default: A random, system-chosen Availability Zone.
-- Constraint: You can't specify the @AvailabilityZone@ parameter if the DB instance is a Multi-AZ deployment.
-- Example: @us-east-1a@
--
-- /Note:/ Consider using 'availabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbitpitAvailabilityZone :: Lens.Lens' RestoreDBInstanceToPointInTime (Core.Maybe Types.String)
rdbitpitAvailabilityZone = Lens.field @"availabilityZone"
{-# DEPRECATED rdbitpitAvailabilityZone "Use generic-lens or generic-optics with 'availabilityZone' instead." #-}

-- | A value that indicates whether to copy all tags from the restored DB instance to snapshots of the DB instance. By default, tags are not copied.
--
-- /Note:/ Consider using 'copyTagsToSnapshot' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbitpitCopyTagsToSnapshot :: Lens.Lens' RestoreDBInstanceToPointInTime (Core.Maybe Core.Bool)
rdbitpitCopyTagsToSnapshot = Lens.field @"copyTagsToSnapshot"
{-# DEPRECATED rdbitpitCopyTagsToSnapshot "Use generic-lens or generic-optics with 'copyTagsToSnapshot' instead." #-}

-- | The compute and memory capacity of the Amazon RDS DB instance, for example, @db.m4.large@ . Not all DB instance classes are available in all AWS Regions, or for all database engines. For the full list of DB instance classes, and availability for your engine, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Concepts.DBInstanceClass.html DB Instance Class> in the /Amazon RDS User Guide./
--
-- Default: The same DBInstanceClass as the original DB instance.
--
-- /Note:/ Consider using 'dBInstanceClass' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbitpitDBInstanceClass :: Lens.Lens' RestoreDBInstanceToPointInTime (Core.Maybe Types.String)
rdbitpitDBInstanceClass = Lens.field @"dBInstanceClass"
{-# DEPRECATED rdbitpitDBInstanceClass "Use generic-lens or generic-optics with 'dBInstanceClass' instead." #-}

-- | The database name for the restored DB instance.
--
-- /Note:/ Consider using 'dBName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbitpitDBName :: Lens.Lens' RestoreDBInstanceToPointInTime (Core.Maybe Types.String)
rdbitpitDBName = Lens.field @"dBName"
{-# DEPRECATED rdbitpitDBName "Use generic-lens or generic-optics with 'dBName' instead." #-}

-- | The name of the DB parameter group to associate with this DB instance.
--
-- If you do not specify a value for @DBParameterGroupName@ , then the default @DBParameterGroup@ for the specified DB engine is used.
-- Constraints:
--
--     * If supplied, must match the name of an existing DBParameterGroup.
--
--
--     * Must be 1 to 255 letters, numbers, or hyphens.
--
--
--     * First character must be a letter.
--
--
--     * Can't end with a hyphen or contain two consecutive hyphens.
--
--
--
-- /Note:/ Consider using 'dBParameterGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbitpitDBParameterGroupName :: Lens.Lens' RestoreDBInstanceToPointInTime (Core.Maybe Types.String)
rdbitpitDBParameterGroupName = Lens.field @"dBParameterGroupName"
{-# DEPRECATED rdbitpitDBParameterGroupName "Use generic-lens or generic-optics with 'dBParameterGroupName' instead." #-}

-- | The DB subnet group name to use for the new instance.
--
-- Constraints: If supplied, must match the name of an existing DBSubnetGroup.
-- Example: @mySubnetgroup@
--
-- /Note:/ Consider using 'dBSubnetGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbitpitDBSubnetGroupName :: Lens.Lens' RestoreDBInstanceToPointInTime (Core.Maybe Types.String)
rdbitpitDBSubnetGroupName = Lens.field @"dBSubnetGroupName"
{-# DEPRECATED rdbitpitDBSubnetGroupName "Use generic-lens or generic-optics with 'dBSubnetGroupName' instead." #-}

-- | A value that indicates whether the DB instance has deletion protection enabled. The database can't be deleted when deletion protection is enabled. By default, deletion protection is disabled. For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_DeleteInstance.html Deleting a DB Instance> .
--
-- /Note:/ Consider using 'deletionProtection' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbitpitDeletionProtection :: Lens.Lens' RestoreDBInstanceToPointInTime (Core.Maybe Core.Bool)
rdbitpitDeletionProtection = Lens.field @"deletionProtection"
{-# DEPRECATED rdbitpitDeletionProtection "Use generic-lens or generic-optics with 'deletionProtection' instead." #-}

-- | Specify the Active Directory directory ID to restore the DB instance in. The domain must be created prior to this operation. Currently, only MySQL, Microsoft SQL Server, Oracle, and PostgreSQL DB instances can be created in an Active Directory Domain.
--
-- For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/kerberos-authentication.html Kerberos Authentication> in the /Amazon RDS User Guide/ .
--
-- /Note:/ Consider using 'domain' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbitpitDomain :: Lens.Lens' RestoreDBInstanceToPointInTime (Core.Maybe Types.String)
rdbitpitDomain = Lens.field @"domain"
{-# DEPRECATED rdbitpitDomain "Use generic-lens or generic-optics with 'domain' instead." #-}

-- | Specify the name of the IAM role to be used when making API calls to the Directory Service.
--
-- /Note:/ Consider using 'domainIAMRoleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbitpitDomainIAMRoleName :: Lens.Lens' RestoreDBInstanceToPointInTime (Core.Maybe Types.String)
rdbitpitDomainIAMRoleName = Lens.field @"domainIAMRoleName"
{-# DEPRECATED rdbitpitDomainIAMRoleName "Use generic-lens or generic-optics with 'domainIAMRoleName' instead." #-}

-- | The list of logs that the restored DB instance is to export to CloudWatch Logs. The values in the list depend on the DB engine being used. For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_LogAccess.html#USER_LogAccess.Procedural.UploadtoCloudWatch Publishing Database Logs to Amazon CloudWatch Logs> in the /Amazon RDS User Guide/ .
--
-- /Note:/ Consider using 'enableCloudwatchLogsExports' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbitpitEnableCloudwatchLogsExports :: Lens.Lens' RestoreDBInstanceToPointInTime (Core.Maybe [Types.String])
rdbitpitEnableCloudwatchLogsExports = Lens.field @"enableCloudwatchLogsExports"
{-# DEPRECATED rdbitpitEnableCloudwatchLogsExports "Use generic-lens or generic-optics with 'enableCloudwatchLogsExports' instead." #-}

-- | A value that indicates whether to enable mapping of AWS Identity and Access Management (IAM) accounts to database accounts. By default, mapping is disabled.
--
-- For more information about IAM database authentication, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/UsingWithRDS.IAMDBAuth.html IAM Database Authentication for MySQL and PostgreSQL> in the /Amazon RDS User Guide./
--
-- /Note:/ Consider using 'enableIAMDatabaseAuthentication' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbitpitEnableIAMDatabaseAuthentication :: Lens.Lens' RestoreDBInstanceToPointInTime (Core.Maybe Core.Bool)
rdbitpitEnableIAMDatabaseAuthentication = Lens.field @"enableIAMDatabaseAuthentication"
{-# DEPRECATED rdbitpitEnableIAMDatabaseAuthentication "Use generic-lens or generic-optics with 'enableIAMDatabaseAuthentication' instead." #-}

-- | The database engine to use for the new instance.
--
-- Default: The same as source
-- Constraint: Must be compatible with the engine of the source
-- Valid Values:
--
--     * @mariadb@
--
--
--     * @mysql@
--
--
--     * @oracle-ee@
--
--
--     * @oracle-se2@
--
--
--     * @oracle-se1@
--
--
--     * @oracle-se@
--
--
--     * @postgres@
--
--
--     * @sqlserver-ee@
--
--
--     * @sqlserver-se@
--
--
--     * @sqlserver-ex@
--
--
--     * @sqlserver-web@
--
--
--
-- /Note:/ Consider using 'engine' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbitpitEngine :: Lens.Lens' RestoreDBInstanceToPointInTime (Core.Maybe Types.String)
rdbitpitEngine = Lens.field @"engine"
{-# DEPRECATED rdbitpitEngine "Use generic-lens or generic-optics with 'engine' instead." #-}

-- | The amount of Provisioned IOPS (input/output operations per second) to be initially allocated for the DB instance.
--
-- Constraints: Must be an integer greater than 1000.
-- __SQL Server__
-- Setting the IOPS value for the SQL Server database engine isn't supported.
--
-- /Note:/ Consider using 'iops' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbitpitIops :: Lens.Lens' RestoreDBInstanceToPointInTime (Core.Maybe Core.Int)
rdbitpitIops = Lens.field @"iops"
{-# DEPRECATED rdbitpitIops "Use generic-lens or generic-optics with 'iops' instead." #-}

-- | License model information for the restored DB instance.
--
-- Default: Same as source.
-- Valid values: @license-included@ | @bring-your-own-license@ | @general-public-license@
--
-- /Note:/ Consider using 'licenseModel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbitpitLicenseModel :: Lens.Lens' RestoreDBInstanceToPointInTime (Core.Maybe Types.String)
rdbitpitLicenseModel = Lens.field @"licenseModel"
{-# DEPRECATED rdbitpitLicenseModel "Use generic-lens or generic-optics with 'licenseModel' instead." #-}

-- | The upper limit to which Amazon RDS can automatically scale the storage of the DB instance.
--
-- /Note:/ Consider using 'maxAllocatedStorage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbitpitMaxAllocatedStorage :: Lens.Lens' RestoreDBInstanceToPointInTime (Core.Maybe Core.Int)
rdbitpitMaxAllocatedStorage = Lens.field @"maxAllocatedStorage"
{-# DEPRECATED rdbitpitMaxAllocatedStorage "Use generic-lens or generic-optics with 'maxAllocatedStorage' instead." #-}

-- | A value that indicates whether the DB instance is a Multi-AZ deployment.
--
-- Constraint: You can't specify the @AvailabilityZone@ parameter if the DB instance is a Multi-AZ deployment.
--
-- /Note:/ Consider using 'multiAZ' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbitpitMultiAZ :: Lens.Lens' RestoreDBInstanceToPointInTime (Core.Maybe Core.Bool)
rdbitpitMultiAZ = Lens.field @"multiAZ"
{-# DEPRECATED rdbitpitMultiAZ "Use generic-lens or generic-optics with 'multiAZ' instead." #-}

-- | The name of the option group to be used for the restored DB instance.
--
-- Permanent options, such as the TDE option for Oracle Advanced Security TDE, can't be removed from an option group, and that option group can't be removed from a DB instance once it is associated with a DB instance
--
-- /Note:/ Consider using 'optionGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbitpitOptionGroupName :: Lens.Lens' RestoreDBInstanceToPointInTime (Core.Maybe Types.String)
rdbitpitOptionGroupName = Lens.field @"optionGroupName"
{-# DEPRECATED rdbitpitOptionGroupName "Use generic-lens or generic-optics with 'optionGroupName' instead." #-}

-- | The port number on which the database accepts connections.
--
-- Constraints: Value must be @1150-65535@
-- Default: The same port as the original DB instance.
--
-- /Note:/ Consider using 'port' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbitpitPort :: Lens.Lens' RestoreDBInstanceToPointInTime (Core.Maybe Core.Int)
rdbitpitPort = Lens.field @"port"
{-# DEPRECATED rdbitpitPort "Use generic-lens or generic-optics with 'port' instead." #-}

-- | The number of CPU cores and the number of threads per core for the DB instance class of the DB instance.
--
-- /Note:/ Consider using 'processorFeatures' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbitpitProcessorFeatures :: Lens.Lens' RestoreDBInstanceToPointInTime (Core.Maybe [Types.ProcessorFeature])
rdbitpitProcessorFeatures = Lens.field @"processorFeatures"
{-# DEPRECATED rdbitpitProcessorFeatures "Use generic-lens or generic-optics with 'processorFeatures' instead." #-}

-- | A value that indicates whether the DB instance is publicly accessible.
--
-- When the DB instance is publicly accessible, its DNS endpoint resolves to the private IP address from within the DB instance's VPC, and to the public IP address from outside of the DB instance's VPC. Access to the DB instance is ultimately controlled by the security group it uses, and that public access is not permitted if the security group assigned to the DB instance doesn't permit it.
-- When the DB instance isn't publicly accessible, it is an internal DB instance with a DNS name that resolves to a private IP address.
-- For more information, see 'CreateDBInstance' .
--
-- /Note:/ Consider using 'publiclyAccessible' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbitpitPubliclyAccessible :: Lens.Lens' RestoreDBInstanceToPointInTime (Core.Maybe Core.Bool)
rdbitpitPubliclyAccessible = Lens.field @"publiclyAccessible"
{-# DEPRECATED rdbitpitPubliclyAccessible "Use generic-lens or generic-optics with 'publiclyAccessible' instead." #-}

-- | The date and time to restore from.
--
-- Valid Values: Value must be a time in Universal Coordinated Time (UTC) format
-- Constraints:
--
--     * Must be before the latest restorable time for the DB instance
--
--
--     * Can't be specified if the @UseLatestRestorableTime@ parameter is enabled
--
--
-- Example: @2009-09-07T23:45:00Z@
--
-- /Note:/ Consider using 'restoreTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbitpitRestoreTime :: Lens.Lens' RestoreDBInstanceToPointInTime (Core.Maybe Core.UTCTime)
rdbitpitRestoreTime = Lens.field @"restoreTime"
{-# DEPRECATED rdbitpitRestoreTime "Use generic-lens or generic-optics with 'restoreTime' instead." #-}

-- | The identifier of the source DB instance from which to restore.
--
-- Constraints:
--
--     * Must match the identifier of an existing DB instance.
--
--
--
-- /Note:/ Consider using 'sourceDBInstanceIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbitpitSourceDBInstanceIdentifier :: Lens.Lens' RestoreDBInstanceToPointInTime (Core.Maybe Types.String)
rdbitpitSourceDBInstanceIdentifier = Lens.field @"sourceDBInstanceIdentifier"
{-# DEPRECATED rdbitpitSourceDBInstanceIdentifier "Use generic-lens or generic-optics with 'sourceDBInstanceIdentifier' instead." #-}

-- | The resource ID of the source DB instance from which to restore.
--
-- /Note:/ Consider using 'sourceDbiResourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbitpitSourceDbiResourceId :: Lens.Lens' RestoreDBInstanceToPointInTime (Core.Maybe Types.String)
rdbitpitSourceDbiResourceId = Lens.field @"sourceDbiResourceId"
{-# DEPRECATED rdbitpitSourceDbiResourceId "Use generic-lens or generic-optics with 'sourceDbiResourceId' instead." #-}

-- | Specifies the storage type to be associated with the DB instance.
--
-- Valid values: @standard | gp2 | io1@
-- If you specify @io1@ , you must also include a value for the @Iops@ parameter.
-- Default: @io1@ if the @Iops@ parameter is specified, otherwise @gp2@
--
-- /Note:/ Consider using 'storageType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbitpitStorageType :: Lens.Lens' RestoreDBInstanceToPointInTime (Core.Maybe Types.String)
rdbitpitStorageType = Lens.field @"storageType"
{-# DEPRECATED rdbitpitStorageType "Use generic-lens or generic-optics with 'storageType' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbitpitTags :: Lens.Lens' RestoreDBInstanceToPointInTime (Core.Maybe [Types.Tag])
rdbitpitTags = Lens.field @"tags"
{-# DEPRECATED rdbitpitTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The ARN from the key store with which to associate the instance for TDE encryption.
--
-- /Note:/ Consider using 'tdeCredentialArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbitpitTdeCredentialArn :: Lens.Lens' RestoreDBInstanceToPointInTime (Core.Maybe Types.String)
rdbitpitTdeCredentialArn = Lens.field @"tdeCredentialArn"
{-# DEPRECATED rdbitpitTdeCredentialArn "Use generic-lens or generic-optics with 'tdeCredentialArn' instead." #-}

-- | The password for the given ARN from the key store in order to access the device.
--
-- /Note:/ Consider using 'tdeCredentialPassword' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbitpitTdeCredentialPassword :: Lens.Lens' RestoreDBInstanceToPointInTime (Core.Maybe Types.String)
rdbitpitTdeCredentialPassword = Lens.field @"tdeCredentialPassword"
{-# DEPRECATED rdbitpitTdeCredentialPassword "Use generic-lens or generic-optics with 'tdeCredentialPassword' instead." #-}

-- | A value that indicates whether the DB instance class of the DB instance uses its default processor features.
--
-- /Note:/ Consider using 'useDefaultProcessorFeatures' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbitpitUseDefaultProcessorFeatures :: Lens.Lens' RestoreDBInstanceToPointInTime (Core.Maybe Core.Bool)
rdbitpitUseDefaultProcessorFeatures = Lens.field @"useDefaultProcessorFeatures"
{-# DEPRECATED rdbitpitUseDefaultProcessorFeatures "Use generic-lens or generic-optics with 'useDefaultProcessorFeatures' instead." #-}

-- | A value that indicates whether the DB instance is restored from the latest backup time. By default, the DB instance isn't restored from the latest backup time.
--
-- Constraints: Can't be specified if the @RestoreTime@ parameter is provided.
--
-- /Note:/ Consider using 'useLatestRestorableTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbitpitUseLatestRestorableTime :: Lens.Lens' RestoreDBInstanceToPointInTime (Core.Maybe Core.Bool)
rdbitpitUseLatestRestorableTime = Lens.field @"useLatestRestorableTime"
{-# DEPRECATED rdbitpitUseLatestRestorableTime "Use generic-lens or generic-optics with 'useLatestRestorableTime' instead." #-}

-- | A list of EC2 VPC security groups to associate with this DB instance.
--
-- Default: The default EC2 VPC security group for the DB subnet group's VPC.
--
-- /Note:/ Consider using 'vpcSecurityGroupIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbitpitVpcSecurityGroupIds :: Lens.Lens' RestoreDBInstanceToPointInTime (Core.Maybe [Types.String])
rdbitpitVpcSecurityGroupIds = Lens.field @"vpcSecurityGroupIds"
{-# DEPRECATED rdbitpitVpcSecurityGroupIds "Use generic-lens or generic-optics with 'vpcSecurityGroupIds' instead." #-}

instance Core.AWSRequest RestoreDBInstanceToPointInTime where
  type
    Rs RestoreDBInstanceToPointInTime =
      RestoreDBInstanceToPointInTimeResponse
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
            ( Core.pure ("Action", "RestoreDBInstanceToPointInTime")
                Core.<> (Core.pure ("Version", "2014-10-31"))
                Core.<> ( Core.toQueryValue
                            "TargetDBInstanceIdentifier"
                            targetDBInstanceIdentifier
                        )
                Core.<> ( Core.toQueryValue "AutoMinorVersionUpgrade"
                            Core.<$> autoMinorVersionUpgrade
                        )
                Core.<> (Core.toQueryValue "AvailabilityZone" Core.<$> availabilityZone)
                Core.<> ( Core.toQueryValue "CopyTagsToSnapshot"
                            Core.<$> copyTagsToSnapshot
                        )
                Core.<> (Core.toQueryValue "DBInstanceClass" Core.<$> dBInstanceClass)
                Core.<> (Core.toQueryValue "DBName" Core.<$> dBName)
                Core.<> ( Core.toQueryValue "DBParameterGroupName"
                            Core.<$> dBParameterGroupName
                        )
                Core.<> (Core.toQueryValue "DBSubnetGroupName" Core.<$> dBSubnetGroupName)
                Core.<> ( Core.toQueryValue "DeletionProtection"
                            Core.<$> deletionProtection
                        )
                Core.<> (Core.toQueryValue "Domain" Core.<$> domain)
                Core.<> (Core.toQueryValue "DomainIAMRoleName" Core.<$> domainIAMRoleName)
                Core.<> ( Core.toQueryValue
                            "EnableCloudwatchLogsExports"
                            (Core.toQueryList "member" Core.<$> enableCloudwatchLogsExports)
                        )
                Core.<> ( Core.toQueryValue "EnableIAMDatabaseAuthentication"
                            Core.<$> enableIAMDatabaseAuthentication
                        )
                Core.<> (Core.toQueryValue "Engine" Core.<$> engine)
                Core.<> (Core.toQueryValue "Iops" Core.<$> iops)
                Core.<> (Core.toQueryValue "LicenseModel" Core.<$> licenseModel)
                Core.<> ( Core.toQueryValue "MaxAllocatedStorage"
                            Core.<$> maxAllocatedStorage
                        )
                Core.<> (Core.toQueryValue "MultiAZ" Core.<$> multiAZ)
                Core.<> (Core.toQueryValue "OptionGroupName" Core.<$> optionGroupName)
                Core.<> (Core.toQueryValue "Port" Core.<$> port)
                Core.<> ( Core.toQueryValue
                            "ProcessorFeatures"
                            (Core.toQueryList "ProcessorFeature" Core.<$> processorFeatures)
                        )
                Core.<> ( Core.toQueryValue "PubliclyAccessible"
                            Core.<$> publiclyAccessible
                        )
                Core.<> (Core.toQueryValue "RestoreTime" Core.<$> restoreTime)
                Core.<> ( Core.toQueryValue "SourceDBInstanceIdentifier"
                            Core.<$> sourceDBInstanceIdentifier
                        )
                Core.<> ( Core.toQueryValue "SourceDbiResourceId"
                            Core.<$> sourceDbiResourceId
                        )
                Core.<> (Core.toQueryValue "StorageType" Core.<$> storageType)
                Core.<> (Core.toQueryValue "Tags" (Core.toQueryList "Tag" Core.<$> tags))
                Core.<> (Core.toQueryValue "TdeCredentialArn" Core.<$> tdeCredentialArn)
                Core.<> ( Core.toQueryValue "TdeCredentialPassword"
                            Core.<$> tdeCredentialPassword
                        )
                Core.<> ( Core.toQueryValue "UseDefaultProcessorFeatures"
                            Core.<$> useDefaultProcessorFeatures
                        )
                Core.<> ( Core.toQueryValue "UseLatestRestorableTime"
                            Core.<$> useLatestRestorableTime
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
      "RestoreDBInstanceToPointInTimeResult"
      ( \s h x ->
          RestoreDBInstanceToPointInTimeResponse'
            Core.<$> (x Core..@? "DBInstance") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkRestoreDBInstanceToPointInTimeResponse' smart constructor.
data RestoreDBInstanceToPointInTimeResponse = RestoreDBInstanceToPointInTimeResponse'
  { dBInstance :: Core.Maybe Types.DBInstance,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'RestoreDBInstanceToPointInTimeResponse' value with any optional fields omitted.
mkRestoreDBInstanceToPointInTimeResponse ::
  -- | 'responseStatus'
  Core.Int ->
  RestoreDBInstanceToPointInTimeResponse
mkRestoreDBInstanceToPointInTimeResponse responseStatus =
  RestoreDBInstanceToPointInTimeResponse'
    { dBInstance =
        Core.Nothing,
      responseStatus
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'dBInstance' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbitpitrrsDBInstance :: Lens.Lens' RestoreDBInstanceToPointInTimeResponse (Core.Maybe Types.DBInstance)
rdbitpitrrsDBInstance = Lens.field @"dBInstance"
{-# DEPRECATED rdbitpitrrsDBInstance "Use generic-lens or generic-optics with 'dBInstance' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbitpitrrsResponseStatus :: Lens.Lens' RestoreDBInstanceToPointInTimeResponse Core.Int
rdbitpitrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED rdbitpitrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
