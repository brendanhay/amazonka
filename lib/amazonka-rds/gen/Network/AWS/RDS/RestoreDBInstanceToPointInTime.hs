{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    rditpitDeletionProtection,
    rditpitUseLatestRestorableTime,
    rditpitPubliclyAccessible,
    rditpitAutoMinorVersionUpgrade,
    rditpitDBSubnetGroupName,
    rditpitRestoreTime,
    rditpitIOPS,
    rditpitDomain,
    rditpitEngine,
    rditpitTDECredentialPassword,
    rditpitSourceDBInstanceIdentifier,
    rditpitProcessorFeatures,
    rditpitDBInstanceClass,
    rditpitLicenseModel,
    rditpitMaxAllocatedStorage,
    rditpitDBParameterGroupName,
    rditpitAvailabilityZone,
    rditpitVPCSecurityGroupIds,
    rditpitMultiAZ,
    rditpitSourceDBiResourceId,
    rditpitOptionGroupName,
    rditpitCopyTagsToSnapshot,
    rditpitTDECredentialARN,
    rditpitDomainIAMRoleName,
    rditpitTags,
    rditpitPort,
    rditpitEnableIAMDatabaseAuthentication,
    rditpitUseDefaultProcessorFeatures,
    rditpitStorageType,
    rditpitEnableCloudwatchLogsExports,
    rditpitDBName,
    rditpitTargetDBInstanceIdentifier,

    -- * Destructuring the response
    RestoreDBInstanceToPointInTimeResponse (..),
    mkRestoreDBInstanceToPointInTimeResponse,

    -- ** Response lenses
    rditpitrsDBInstance,
    rditpitrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.RDS.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- |
--
-- /See:/ 'mkRestoreDBInstanceToPointInTime' smart constructor.
data RestoreDBInstanceToPointInTime = RestoreDBInstanceToPointInTime'
  { deletionProtection ::
      Lude.Maybe Lude.Bool,
    useLatestRestorableTime ::
      Lude.Maybe Lude.Bool,
    publiclyAccessible ::
      Lude.Maybe Lude.Bool,
    autoMinorVersionUpgrade ::
      Lude.Maybe Lude.Bool,
    dbSubnetGroupName ::
      Lude.Maybe Lude.Text,
    restoreTime ::
      Lude.Maybe Lude.DateTime,
    iops :: Lude.Maybe Lude.Int,
    domain ::
      Lude.Maybe Lude.Text,
    engine ::
      Lude.Maybe Lude.Text,
    tdeCredentialPassword ::
      Lude.Maybe Lude.Text,
    sourceDBInstanceIdentifier ::
      Lude.Maybe Lude.Text,
    processorFeatures ::
      Lude.Maybe [ProcessorFeature],
    dbInstanceClass ::
      Lude.Maybe Lude.Text,
    licenseModel ::
      Lude.Maybe Lude.Text,
    maxAllocatedStorage ::
      Lude.Maybe Lude.Int,
    dbParameterGroupName ::
      Lude.Maybe Lude.Text,
    availabilityZone ::
      Lude.Maybe Lude.Text,
    vpcSecurityGroupIds ::
      Lude.Maybe [Lude.Text],
    multiAZ ::
      Lude.Maybe Lude.Bool,
    sourceDBiResourceId ::
      Lude.Maybe Lude.Text,
    optionGroupName ::
      Lude.Maybe Lude.Text,
    copyTagsToSnapshot ::
      Lude.Maybe Lude.Bool,
    tdeCredentialARN ::
      Lude.Maybe Lude.Text,
    domainIAMRoleName ::
      Lude.Maybe Lude.Text,
    tags :: Lude.Maybe [Tag],
    port :: Lude.Maybe Lude.Int,
    enableIAMDatabaseAuthentication ::
      Lude.Maybe Lude.Bool,
    useDefaultProcessorFeatures ::
      Lude.Maybe Lude.Bool,
    storageType ::
      Lude.Maybe Lude.Text,
    enableCloudwatchLogsExports ::
      Lude.Maybe [Lude.Text],
    dbName ::
      Lude.Maybe Lude.Text,
    targetDBInstanceIdentifier ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RestoreDBInstanceToPointInTime' with the minimum fields required to make a request.
--
-- * 'autoMinorVersionUpgrade' - A value that indicates whether minor version upgrades are applied automatically to the DB instance during the maintenance window.
-- * 'availabilityZone' - The Availability Zone (AZ) where the DB instance will be created.
--
-- Default: A random, system-chosen Availability Zone.
-- Constraint: You can't specify the @AvailabilityZone@ parameter if the DB instance is a Multi-AZ deployment.
-- Example: @us-east-1a@
-- * 'copyTagsToSnapshot' - A value that indicates whether to copy all tags from the restored DB instance to snapshots of the DB instance. By default, tags are not copied.
-- * 'dbInstanceClass' - The compute and memory capacity of the Amazon RDS DB instance, for example, @db.m4.large@ . Not all DB instance classes are available in all AWS Regions, or for all database engines. For the full list of DB instance classes, and availability for your engine, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Concepts.DBInstanceClass.html DB Instance Class> in the /Amazon RDS User Guide./
--
-- Default: The same DBInstanceClass as the original DB instance.
-- * 'dbName' - The database name for the restored DB instance.
-- * 'dbParameterGroupName' - The name of the DB parameter group to associate with this DB instance.
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
-- * 'dbSubnetGroupName' - The DB subnet group name to use for the new instance.
--
-- Constraints: If supplied, must match the name of an existing DBSubnetGroup.
-- Example: @mySubnetgroup@
-- * 'deletionProtection' - A value that indicates whether the DB instance has deletion protection enabled. The database can't be deleted when deletion protection is enabled. By default, deletion protection is disabled. For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_DeleteInstance.html Deleting a DB Instance> .
-- * 'domain' - Specify the Active Directory directory ID to restore the DB instance in. The domain must be created prior to this operation. Currently, only MySQL, Microsoft SQL Server, Oracle, and PostgreSQL DB instances can be created in an Active Directory Domain.
--
-- For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/kerberos-authentication.html Kerberos Authentication> in the /Amazon RDS User Guide/ .
-- * 'domainIAMRoleName' - Specify the name of the IAM role to be used when making API calls to the Directory Service.
-- * 'enableCloudwatchLogsExports' - The list of logs that the restored DB instance is to export to CloudWatch Logs. The values in the list depend on the DB engine being used. For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_LogAccess.html#USER_LogAccess.Procedural.UploadtoCloudWatch Publishing Database Logs to Amazon CloudWatch Logs> in the /Amazon RDS User Guide/ .
-- * 'enableIAMDatabaseAuthentication' - A value that indicates whether to enable mapping of AWS Identity and Access Management (IAM) accounts to database accounts. By default, mapping is disabled.
--
-- For more information about IAM database authentication, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/UsingWithRDS.IAMDBAuth.html IAM Database Authentication for MySQL and PostgreSQL> in the /Amazon RDS User Guide./
-- * 'engine' - The database engine to use for the new instance.
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
-- * 'iops' - The amount of Provisioned IOPS (input/output operations per second) to be initially allocated for the DB instance.
--
-- Constraints: Must be an integer greater than 1000.
-- __SQL Server__
-- Setting the IOPS value for the SQL Server database engine isn't supported.
-- * 'licenseModel' - License model information for the restored DB instance.
--
-- Default: Same as source.
-- Valid values: @license-included@ | @bring-your-own-license@ | @general-public-license@
-- * 'maxAllocatedStorage' - The upper limit to which Amazon RDS can automatically scale the storage of the DB instance.
-- * 'multiAZ' - A value that indicates whether the DB instance is a Multi-AZ deployment.
--
-- Constraint: You can't specify the @AvailabilityZone@ parameter if the DB instance is a Multi-AZ deployment.
-- * 'optionGroupName' - The name of the option group to be used for the restored DB instance.
--
-- Permanent options, such as the TDE option for Oracle Advanced Security TDE, can't be removed from an option group, and that option group can't be removed from a DB instance once it is associated with a DB instance
-- * 'port' - The port number on which the database accepts connections.
--
-- Constraints: Value must be @1150-65535@
-- Default: The same port as the original DB instance.
-- * 'processorFeatures' - The number of CPU cores and the number of threads per core for the DB instance class of the DB instance.
-- * 'publiclyAccessible' - A value that indicates whether the DB instance is publicly accessible.
--
-- When the DB instance is publicly accessible, its DNS endpoint resolves to the private IP address from within the DB instance's VPC, and to the public IP address from outside of the DB instance's VPC. Access to the DB instance is ultimately controlled by the security group it uses, and that public access is not permitted if the security group assigned to the DB instance doesn't permit it.
-- When the DB instance isn't publicly accessible, it is an internal DB instance with a DNS name that resolves to a private IP address.
-- For more information, see 'CreateDBInstance' .
-- * 'restoreTime' - The date and time to restore from.
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
-- * 'sourceDBInstanceIdentifier' - The identifier of the source DB instance from which to restore.
--
-- Constraints:
--
--     * Must match the identifier of an existing DB instance.
--
--
-- * 'sourceDBiResourceId' - The resource ID of the source DB instance from which to restore.
-- * 'storageType' - Specifies the storage type to be associated with the DB instance.
--
-- Valid values: @standard | gp2 | io1@
-- If you specify @io1@ , you must also include a value for the @Iops@ parameter.
-- Default: @io1@ if the @Iops@ parameter is specified, otherwise @gp2@
-- * 'tags' - Undocumented field.
-- * 'targetDBInstanceIdentifier' - The name of the new DB instance to be created.
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
-- * 'tdeCredentialARN' - The ARN from the key store with which to associate the instance for TDE encryption.
-- * 'tdeCredentialPassword' - The password for the given ARN from the key store in order to access the device.
-- * 'useDefaultProcessorFeatures' - A value that indicates whether the DB instance class of the DB instance uses its default processor features.
-- * 'useLatestRestorableTime' - A value that indicates whether the DB instance is restored from the latest backup time. By default, the DB instance isn't restored from the latest backup time.
--
-- Constraints: Can't be specified if the @RestoreTime@ parameter is provided.
-- * 'vpcSecurityGroupIds' - A list of EC2 VPC security groups to associate with this DB instance.
--
-- Default: The default EC2 VPC security group for the DB subnet group's VPC.
mkRestoreDBInstanceToPointInTime ::
  -- | 'targetDBInstanceIdentifier'
  Lude.Text ->
  RestoreDBInstanceToPointInTime
mkRestoreDBInstanceToPointInTime pTargetDBInstanceIdentifier_ =
  RestoreDBInstanceToPointInTime'
    { deletionProtection =
        Lude.Nothing,
      useLatestRestorableTime = Lude.Nothing,
      publiclyAccessible = Lude.Nothing,
      autoMinorVersionUpgrade = Lude.Nothing,
      dbSubnetGroupName = Lude.Nothing,
      restoreTime = Lude.Nothing,
      iops = Lude.Nothing,
      domain = Lude.Nothing,
      engine = Lude.Nothing,
      tdeCredentialPassword = Lude.Nothing,
      sourceDBInstanceIdentifier = Lude.Nothing,
      processorFeatures = Lude.Nothing,
      dbInstanceClass = Lude.Nothing,
      licenseModel = Lude.Nothing,
      maxAllocatedStorage = Lude.Nothing,
      dbParameterGroupName = Lude.Nothing,
      availabilityZone = Lude.Nothing,
      vpcSecurityGroupIds = Lude.Nothing,
      multiAZ = Lude.Nothing,
      sourceDBiResourceId = Lude.Nothing,
      optionGroupName = Lude.Nothing,
      copyTagsToSnapshot = Lude.Nothing,
      tdeCredentialARN = Lude.Nothing,
      domainIAMRoleName = Lude.Nothing,
      tags = Lude.Nothing,
      port = Lude.Nothing,
      enableIAMDatabaseAuthentication = Lude.Nothing,
      useDefaultProcessorFeatures = Lude.Nothing,
      storageType = Lude.Nothing,
      enableCloudwatchLogsExports = Lude.Nothing,
      dbName = Lude.Nothing,
      targetDBInstanceIdentifier = pTargetDBInstanceIdentifier_
    }

-- | A value that indicates whether the DB instance has deletion protection enabled. The database can't be deleted when deletion protection is enabled. By default, deletion protection is disabled. For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_DeleteInstance.html Deleting a DB Instance> .
--
-- /Note:/ Consider using 'deletionProtection' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rditpitDeletionProtection :: Lens.Lens' RestoreDBInstanceToPointInTime (Lude.Maybe Lude.Bool)
rditpitDeletionProtection = Lens.lens (deletionProtection :: RestoreDBInstanceToPointInTime -> Lude.Maybe Lude.Bool) (\s a -> s {deletionProtection = a} :: RestoreDBInstanceToPointInTime)
{-# DEPRECATED rditpitDeletionProtection "Use generic-lens or generic-optics with 'deletionProtection' instead." #-}

-- | A value that indicates whether the DB instance is restored from the latest backup time. By default, the DB instance isn't restored from the latest backup time.
--
-- Constraints: Can't be specified if the @RestoreTime@ parameter is provided.
--
-- /Note:/ Consider using 'useLatestRestorableTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rditpitUseLatestRestorableTime :: Lens.Lens' RestoreDBInstanceToPointInTime (Lude.Maybe Lude.Bool)
rditpitUseLatestRestorableTime = Lens.lens (useLatestRestorableTime :: RestoreDBInstanceToPointInTime -> Lude.Maybe Lude.Bool) (\s a -> s {useLatestRestorableTime = a} :: RestoreDBInstanceToPointInTime)
{-# DEPRECATED rditpitUseLatestRestorableTime "Use generic-lens or generic-optics with 'useLatestRestorableTime' instead." #-}

-- | A value that indicates whether the DB instance is publicly accessible.
--
-- When the DB instance is publicly accessible, its DNS endpoint resolves to the private IP address from within the DB instance's VPC, and to the public IP address from outside of the DB instance's VPC. Access to the DB instance is ultimately controlled by the security group it uses, and that public access is not permitted if the security group assigned to the DB instance doesn't permit it.
-- When the DB instance isn't publicly accessible, it is an internal DB instance with a DNS name that resolves to a private IP address.
-- For more information, see 'CreateDBInstance' .
--
-- /Note:/ Consider using 'publiclyAccessible' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rditpitPubliclyAccessible :: Lens.Lens' RestoreDBInstanceToPointInTime (Lude.Maybe Lude.Bool)
rditpitPubliclyAccessible = Lens.lens (publiclyAccessible :: RestoreDBInstanceToPointInTime -> Lude.Maybe Lude.Bool) (\s a -> s {publiclyAccessible = a} :: RestoreDBInstanceToPointInTime)
{-# DEPRECATED rditpitPubliclyAccessible "Use generic-lens or generic-optics with 'publiclyAccessible' instead." #-}

-- | A value that indicates whether minor version upgrades are applied automatically to the DB instance during the maintenance window.
--
-- /Note:/ Consider using 'autoMinorVersionUpgrade' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rditpitAutoMinorVersionUpgrade :: Lens.Lens' RestoreDBInstanceToPointInTime (Lude.Maybe Lude.Bool)
rditpitAutoMinorVersionUpgrade = Lens.lens (autoMinorVersionUpgrade :: RestoreDBInstanceToPointInTime -> Lude.Maybe Lude.Bool) (\s a -> s {autoMinorVersionUpgrade = a} :: RestoreDBInstanceToPointInTime)
{-# DEPRECATED rditpitAutoMinorVersionUpgrade "Use generic-lens or generic-optics with 'autoMinorVersionUpgrade' instead." #-}

-- | The DB subnet group name to use for the new instance.
--
-- Constraints: If supplied, must match the name of an existing DBSubnetGroup.
-- Example: @mySubnetgroup@
--
-- /Note:/ Consider using 'dbSubnetGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rditpitDBSubnetGroupName :: Lens.Lens' RestoreDBInstanceToPointInTime (Lude.Maybe Lude.Text)
rditpitDBSubnetGroupName = Lens.lens (dbSubnetGroupName :: RestoreDBInstanceToPointInTime -> Lude.Maybe Lude.Text) (\s a -> s {dbSubnetGroupName = a} :: RestoreDBInstanceToPointInTime)
{-# DEPRECATED rditpitDBSubnetGroupName "Use generic-lens or generic-optics with 'dbSubnetGroupName' instead." #-}

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
rditpitRestoreTime :: Lens.Lens' RestoreDBInstanceToPointInTime (Lude.Maybe Lude.DateTime)
rditpitRestoreTime = Lens.lens (restoreTime :: RestoreDBInstanceToPointInTime -> Lude.Maybe Lude.DateTime) (\s a -> s {restoreTime = a} :: RestoreDBInstanceToPointInTime)
{-# DEPRECATED rditpitRestoreTime "Use generic-lens or generic-optics with 'restoreTime' instead." #-}

-- | The amount of Provisioned IOPS (input/output operations per second) to be initially allocated for the DB instance.
--
-- Constraints: Must be an integer greater than 1000.
-- __SQL Server__
-- Setting the IOPS value for the SQL Server database engine isn't supported.
--
-- /Note:/ Consider using 'iops' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rditpitIOPS :: Lens.Lens' RestoreDBInstanceToPointInTime (Lude.Maybe Lude.Int)
rditpitIOPS = Lens.lens (iops :: RestoreDBInstanceToPointInTime -> Lude.Maybe Lude.Int) (\s a -> s {iops = a} :: RestoreDBInstanceToPointInTime)
{-# DEPRECATED rditpitIOPS "Use generic-lens or generic-optics with 'iops' instead." #-}

-- | Specify the Active Directory directory ID to restore the DB instance in. The domain must be created prior to this operation. Currently, only MySQL, Microsoft SQL Server, Oracle, and PostgreSQL DB instances can be created in an Active Directory Domain.
--
-- For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/kerberos-authentication.html Kerberos Authentication> in the /Amazon RDS User Guide/ .
--
-- /Note:/ Consider using 'domain' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rditpitDomain :: Lens.Lens' RestoreDBInstanceToPointInTime (Lude.Maybe Lude.Text)
rditpitDomain = Lens.lens (domain :: RestoreDBInstanceToPointInTime -> Lude.Maybe Lude.Text) (\s a -> s {domain = a} :: RestoreDBInstanceToPointInTime)
{-# DEPRECATED rditpitDomain "Use generic-lens or generic-optics with 'domain' instead." #-}

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
rditpitEngine :: Lens.Lens' RestoreDBInstanceToPointInTime (Lude.Maybe Lude.Text)
rditpitEngine = Lens.lens (engine :: RestoreDBInstanceToPointInTime -> Lude.Maybe Lude.Text) (\s a -> s {engine = a} :: RestoreDBInstanceToPointInTime)
{-# DEPRECATED rditpitEngine "Use generic-lens or generic-optics with 'engine' instead." #-}

-- | The password for the given ARN from the key store in order to access the device.
--
-- /Note:/ Consider using 'tdeCredentialPassword' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rditpitTDECredentialPassword :: Lens.Lens' RestoreDBInstanceToPointInTime (Lude.Maybe Lude.Text)
rditpitTDECredentialPassword = Lens.lens (tdeCredentialPassword :: RestoreDBInstanceToPointInTime -> Lude.Maybe Lude.Text) (\s a -> s {tdeCredentialPassword = a} :: RestoreDBInstanceToPointInTime)
{-# DEPRECATED rditpitTDECredentialPassword "Use generic-lens or generic-optics with 'tdeCredentialPassword' instead." #-}

-- | The identifier of the source DB instance from which to restore.
--
-- Constraints:
--
--     * Must match the identifier of an existing DB instance.
--
--
--
-- /Note:/ Consider using 'sourceDBInstanceIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rditpitSourceDBInstanceIdentifier :: Lens.Lens' RestoreDBInstanceToPointInTime (Lude.Maybe Lude.Text)
rditpitSourceDBInstanceIdentifier = Lens.lens (sourceDBInstanceIdentifier :: RestoreDBInstanceToPointInTime -> Lude.Maybe Lude.Text) (\s a -> s {sourceDBInstanceIdentifier = a} :: RestoreDBInstanceToPointInTime)
{-# DEPRECATED rditpitSourceDBInstanceIdentifier "Use generic-lens or generic-optics with 'sourceDBInstanceIdentifier' instead." #-}

-- | The number of CPU cores and the number of threads per core for the DB instance class of the DB instance.
--
-- /Note:/ Consider using 'processorFeatures' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rditpitProcessorFeatures :: Lens.Lens' RestoreDBInstanceToPointInTime (Lude.Maybe [ProcessorFeature])
rditpitProcessorFeatures = Lens.lens (processorFeatures :: RestoreDBInstanceToPointInTime -> Lude.Maybe [ProcessorFeature]) (\s a -> s {processorFeatures = a} :: RestoreDBInstanceToPointInTime)
{-# DEPRECATED rditpitProcessorFeatures "Use generic-lens or generic-optics with 'processorFeatures' instead." #-}

-- | The compute and memory capacity of the Amazon RDS DB instance, for example, @db.m4.large@ . Not all DB instance classes are available in all AWS Regions, or for all database engines. For the full list of DB instance classes, and availability for your engine, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Concepts.DBInstanceClass.html DB Instance Class> in the /Amazon RDS User Guide./
--
-- Default: The same DBInstanceClass as the original DB instance.
--
-- /Note:/ Consider using 'dbInstanceClass' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rditpitDBInstanceClass :: Lens.Lens' RestoreDBInstanceToPointInTime (Lude.Maybe Lude.Text)
rditpitDBInstanceClass = Lens.lens (dbInstanceClass :: RestoreDBInstanceToPointInTime -> Lude.Maybe Lude.Text) (\s a -> s {dbInstanceClass = a} :: RestoreDBInstanceToPointInTime)
{-# DEPRECATED rditpitDBInstanceClass "Use generic-lens or generic-optics with 'dbInstanceClass' instead." #-}

-- | License model information for the restored DB instance.
--
-- Default: Same as source.
-- Valid values: @license-included@ | @bring-your-own-license@ | @general-public-license@
--
-- /Note:/ Consider using 'licenseModel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rditpitLicenseModel :: Lens.Lens' RestoreDBInstanceToPointInTime (Lude.Maybe Lude.Text)
rditpitLicenseModel = Lens.lens (licenseModel :: RestoreDBInstanceToPointInTime -> Lude.Maybe Lude.Text) (\s a -> s {licenseModel = a} :: RestoreDBInstanceToPointInTime)
{-# DEPRECATED rditpitLicenseModel "Use generic-lens or generic-optics with 'licenseModel' instead." #-}

-- | The upper limit to which Amazon RDS can automatically scale the storage of the DB instance.
--
-- /Note:/ Consider using 'maxAllocatedStorage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rditpitMaxAllocatedStorage :: Lens.Lens' RestoreDBInstanceToPointInTime (Lude.Maybe Lude.Int)
rditpitMaxAllocatedStorage = Lens.lens (maxAllocatedStorage :: RestoreDBInstanceToPointInTime -> Lude.Maybe Lude.Int) (\s a -> s {maxAllocatedStorage = a} :: RestoreDBInstanceToPointInTime)
{-# DEPRECATED rditpitMaxAllocatedStorage "Use generic-lens or generic-optics with 'maxAllocatedStorage' instead." #-}

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
-- /Note:/ Consider using 'dbParameterGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rditpitDBParameterGroupName :: Lens.Lens' RestoreDBInstanceToPointInTime (Lude.Maybe Lude.Text)
rditpitDBParameterGroupName = Lens.lens (dbParameterGroupName :: RestoreDBInstanceToPointInTime -> Lude.Maybe Lude.Text) (\s a -> s {dbParameterGroupName = a} :: RestoreDBInstanceToPointInTime)
{-# DEPRECATED rditpitDBParameterGroupName "Use generic-lens or generic-optics with 'dbParameterGroupName' instead." #-}

-- | The Availability Zone (AZ) where the DB instance will be created.
--
-- Default: A random, system-chosen Availability Zone.
-- Constraint: You can't specify the @AvailabilityZone@ parameter if the DB instance is a Multi-AZ deployment.
-- Example: @us-east-1a@
--
-- /Note:/ Consider using 'availabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rditpitAvailabilityZone :: Lens.Lens' RestoreDBInstanceToPointInTime (Lude.Maybe Lude.Text)
rditpitAvailabilityZone = Lens.lens (availabilityZone :: RestoreDBInstanceToPointInTime -> Lude.Maybe Lude.Text) (\s a -> s {availabilityZone = a} :: RestoreDBInstanceToPointInTime)
{-# DEPRECATED rditpitAvailabilityZone "Use generic-lens or generic-optics with 'availabilityZone' instead." #-}

-- | A list of EC2 VPC security groups to associate with this DB instance.
--
-- Default: The default EC2 VPC security group for the DB subnet group's VPC.
--
-- /Note:/ Consider using 'vpcSecurityGroupIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rditpitVPCSecurityGroupIds :: Lens.Lens' RestoreDBInstanceToPointInTime (Lude.Maybe [Lude.Text])
rditpitVPCSecurityGroupIds = Lens.lens (vpcSecurityGroupIds :: RestoreDBInstanceToPointInTime -> Lude.Maybe [Lude.Text]) (\s a -> s {vpcSecurityGroupIds = a} :: RestoreDBInstanceToPointInTime)
{-# DEPRECATED rditpitVPCSecurityGroupIds "Use generic-lens or generic-optics with 'vpcSecurityGroupIds' instead." #-}

-- | A value that indicates whether the DB instance is a Multi-AZ deployment.
--
-- Constraint: You can't specify the @AvailabilityZone@ parameter if the DB instance is a Multi-AZ deployment.
--
-- /Note:/ Consider using 'multiAZ' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rditpitMultiAZ :: Lens.Lens' RestoreDBInstanceToPointInTime (Lude.Maybe Lude.Bool)
rditpitMultiAZ = Lens.lens (multiAZ :: RestoreDBInstanceToPointInTime -> Lude.Maybe Lude.Bool) (\s a -> s {multiAZ = a} :: RestoreDBInstanceToPointInTime)
{-# DEPRECATED rditpitMultiAZ "Use generic-lens or generic-optics with 'multiAZ' instead." #-}

-- | The resource ID of the source DB instance from which to restore.
--
-- /Note:/ Consider using 'sourceDBiResourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rditpitSourceDBiResourceId :: Lens.Lens' RestoreDBInstanceToPointInTime (Lude.Maybe Lude.Text)
rditpitSourceDBiResourceId = Lens.lens (sourceDBiResourceId :: RestoreDBInstanceToPointInTime -> Lude.Maybe Lude.Text) (\s a -> s {sourceDBiResourceId = a} :: RestoreDBInstanceToPointInTime)
{-# DEPRECATED rditpitSourceDBiResourceId "Use generic-lens or generic-optics with 'sourceDBiResourceId' instead." #-}

-- | The name of the option group to be used for the restored DB instance.
--
-- Permanent options, such as the TDE option for Oracle Advanced Security TDE, can't be removed from an option group, and that option group can't be removed from a DB instance once it is associated with a DB instance
--
-- /Note:/ Consider using 'optionGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rditpitOptionGroupName :: Lens.Lens' RestoreDBInstanceToPointInTime (Lude.Maybe Lude.Text)
rditpitOptionGroupName = Lens.lens (optionGroupName :: RestoreDBInstanceToPointInTime -> Lude.Maybe Lude.Text) (\s a -> s {optionGroupName = a} :: RestoreDBInstanceToPointInTime)
{-# DEPRECATED rditpitOptionGroupName "Use generic-lens or generic-optics with 'optionGroupName' instead." #-}

-- | A value that indicates whether to copy all tags from the restored DB instance to snapshots of the DB instance. By default, tags are not copied.
--
-- /Note:/ Consider using 'copyTagsToSnapshot' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rditpitCopyTagsToSnapshot :: Lens.Lens' RestoreDBInstanceToPointInTime (Lude.Maybe Lude.Bool)
rditpitCopyTagsToSnapshot = Lens.lens (copyTagsToSnapshot :: RestoreDBInstanceToPointInTime -> Lude.Maybe Lude.Bool) (\s a -> s {copyTagsToSnapshot = a} :: RestoreDBInstanceToPointInTime)
{-# DEPRECATED rditpitCopyTagsToSnapshot "Use generic-lens or generic-optics with 'copyTagsToSnapshot' instead." #-}

-- | The ARN from the key store with which to associate the instance for TDE encryption.
--
-- /Note:/ Consider using 'tdeCredentialARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rditpitTDECredentialARN :: Lens.Lens' RestoreDBInstanceToPointInTime (Lude.Maybe Lude.Text)
rditpitTDECredentialARN = Lens.lens (tdeCredentialARN :: RestoreDBInstanceToPointInTime -> Lude.Maybe Lude.Text) (\s a -> s {tdeCredentialARN = a} :: RestoreDBInstanceToPointInTime)
{-# DEPRECATED rditpitTDECredentialARN "Use generic-lens or generic-optics with 'tdeCredentialARN' instead." #-}

-- | Specify the name of the IAM role to be used when making API calls to the Directory Service.
--
-- /Note:/ Consider using 'domainIAMRoleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rditpitDomainIAMRoleName :: Lens.Lens' RestoreDBInstanceToPointInTime (Lude.Maybe Lude.Text)
rditpitDomainIAMRoleName = Lens.lens (domainIAMRoleName :: RestoreDBInstanceToPointInTime -> Lude.Maybe Lude.Text) (\s a -> s {domainIAMRoleName = a} :: RestoreDBInstanceToPointInTime)
{-# DEPRECATED rditpitDomainIAMRoleName "Use generic-lens or generic-optics with 'domainIAMRoleName' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rditpitTags :: Lens.Lens' RestoreDBInstanceToPointInTime (Lude.Maybe [Tag])
rditpitTags = Lens.lens (tags :: RestoreDBInstanceToPointInTime -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: RestoreDBInstanceToPointInTime)
{-# DEPRECATED rditpitTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The port number on which the database accepts connections.
--
-- Constraints: Value must be @1150-65535@
-- Default: The same port as the original DB instance.
--
-- /Note:/ Consider using 'port' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rditpitPort :: Lens.Lens' RestoreDBInstanceToPointInTime (Lude.Maybe Lude.Int)
rditpitPort = Lens.lens (port :: RestoreDBInstanceToPointInTime -> Lude.Maybe Lude.Int) (\s a -> s {port = a} :: RestoreDBInstanceToPointInTime)
{-# DEPRECATED rditpitPort "Use generic-lens or generic-optics with 'port' instead." #-}

-- | A value that indicates whether to enable mapping of AWS Identity and Access Management (IAM) accounts to database accounts. By default, mapping is disabled.
--
-- For more information about IAM database authentication, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/UsingWithRDS.IAMDBAuth.html IAM Database Authentication for MySQL and PostgreSQL> in the /Amazon RDS User Guide./
--
-- /Note:/ Consider using 'enableIAMDatabaseAuthentication' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rditpitEnableIAMDatabaseAuthentication :: Lens.Lens' RestoreDBInstanceToPointInTime (Lude.Maybe Lude.Bool)
rditpitEnableIAMDatabaseAuthentication = Lens.lens (enableIAMDatabaseAuthentication :: RestoreDBInstanceToPointInTime -> Lude.Maybe Lude.Bool) (\s a -> s {enableIAMDatabaseAuthentication = a} :: RestoreDBInstanceToPointInTime)
{-# DEPRECATED rditpitEnableIAMDatabaseAuthentication "Use generic-lens or generic-optics with 'enableIAMDatabaseAuthentication' instead." #-}

-- | A value that indicates whether the DB instance class of the DB instance uses its default processor features.
--
-- /Note:/ Consider using 'useDefaultProcessorFeatures' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rditpitUseDefaultProcessorFeatures :: Lens.Lens' RestoreDBInstanceToPointInTime (Lude.Maybe Lude.Bool)
rditpitUseDefaultProcessorFeatures = Lens.lens (useDefaultProcessorFeatures :: RestoreDBInstanceToPointInTime -> Lude.Maybe Lude.Bool) (\s a -> s {useDefaultProcessorFeatures = a} :: RestoreDBInstanceToPointInTime)
{-# DEPRECATED rditpitUseDefaultProcessorFeatures "Use generic-lens or generic-optics with 'useDefaultProcessorFeatures' instead." #-}

-- | Specifies the storage type to be associated with the DB instance.
--
-- Valid values: @standard | gp2 | io1@
-- If you specify @io1@ , you must also include a value for the @Iops@ parameter.
-- Default: @io1@ if the @Iops@ parameter is specified, otherwise @gp2@
--
-- /Note:/ Consider using 'storageType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rditpitStorageType :: Lens.Lens' RestoreDBInstanceToPointInTime (Lude.Maybe Lude.Text)
rditpitStorageType = Lens.lens (storageType :: RestoreDBInstanceToPointInTime -> Lude.Maybe Lude.Text) (\s a -> s {storageType = a} :: RestoreDBInstanceToPointInTime)
{-# DEPRECATED rditpitStorageType "Use generic-lens or generic-optics with 'storageType' instead." #-}

-- | The list of logs that the restored DB instance is to export to CloudWatch Logs. The values in the list depend on the DB engine being used. For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_LogAccess.html#USER_LogAccess.Procedural.UploadtoCloudWatch Publishing Database Logs to Amazon CloudWatch Logs> in the /Amazon RDS User Guide/ .
--
-- /Note:/ Consider using 'enableCloudwatchLogsExports' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rditpitEnableCloudwatchLogsExports :: Lens.Lens' RestoreDBInstanceToPointInTime (Lude.Maybe [Lude.Text])
rditpitEnableCloudwatchLogsExports = Lens.lens (enableCloudwatchLogsExports :: RestoreDBInstanceToPointInTime -> Lude.Maybe [Lude.Text]) (\s a -> s {enableCloudwatchLogsExports = a} :: RestoreDBInstanceToPointInTime)
{-# DEPRECATED rditpitEnableCloudwatchLogsExports "Use generic-lens or generic-optics with 'enableCloudwatchLogsExports' instead." #-}

-- | The database name for the restored DB instance.
--
-- /Note:/ Consider using 'dbName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rditpitDBName :: Lens.Lens' RestoreDBInstanceToPointInTime (Lude.Maybe Lude.Text)
rditpitDBName = Lens.lens (dbName :: RestoreDBInstanceToPointInTime -> Lude.Maybe Lude.Text) (\s a -> s {dbName = a} :: RestoreDBInstanceToPointInTime)
{-# DEPRECATED rditpitDBName "Use generic-lens or generic-optics with 'dbName' instead." #-}

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
rditpitTargetDBInstanceIdentifier :: Lens.Lens' RestoreDBInstanceToPointInTime Lude.Text
rditpitTargetDBInstanceIdentifier = Lens.lens (targetDBInstanceIdentifier :: RestoreDBInstanceToPointInTime -> Lude.Text) (\s a -> s {targetDBInstanceIdentifier = a} :: RestoreDBInstanceToPointInTime)
{-# DEPRECATED rditpitTargetDBInstanceIdentifier "Use generic-lens or generic-optics with 'targetDBInstanceIdentifier' instead." #-}

instance Lude.AWSRequest RestoreDBInstanceToPointInTime where
  type
    Rs RestoreDBInstanceToPointInTime =
      RestoreDBInstanceToPointInTimeResponse
  request = Req.postQuery rdsService
  response =
    Res.receiveXMLWrapper
      "RestoreDBInstanceToPointInTimeResult"
      ( \s h x ->
          RestoreDBInstanceToPointInTimeResponse'
            Lude.<$> (x Lude..@? "DBInstance") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders RestoreDBInstanceToPointInTime where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath RestoreDBInstanceToPointInTime where
  toPath = Lude.const "/"

instance Lude.ToQuery RestoreDBInstanceToPointInTime where
  toQuery RestoreDBInstanceToPointInTime' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("RestoreDBInstanceToPointInTime" :: Lude.ByteString),
        "Version" Lude.=: ("2014-10-31" :: Lude.ByteString),
        "DeletionProtection" Lude.=: deletionProtection,
        "UseLatestRestorableTime" Lude.=: useLatestRestorableTime,
        "PubliclyAccessible" Lude.=: publiclyAccessible,
        "AutoMinorVersionUpgrade" Lude.=: autoMinorVersionUpgrade,
        "DBSubnetGroupName" Lude.=: dbSubnetGroupName,
        "RestoreTime" Lude.=: restoreTime,
        "Iops" Lude.=: iops,
        "Domain" Lude.=: domain,
        "Engine" Lude.=: engine,
        "TdeCredentialPassword" Lude.=: tdeCredentialPassword,
        "SourceDBInstanceIdentifier" Lude.=: sourceDBInstanceIdentifier,
        "ProcessorFeatures"
          Lude.=: Lude.toQuery
            (Lude.toQueryList "ProcessorFeature" Lude.<$> processorFeatures),
        "DBInstanceClass" Lude.=: dbInstanceClass,
        "LicenseModel" Lude.=: licenseModel,
        "MaxAllocatedStorage" Lude.=: maxAllocatedStorage,
        "DBParameterGroupName" Lude.=: dbParameterGroupName,
        "AvailabilityZone" Lude.=: availabilityZone,
        "VpcSecurityGroupIds"
          Lude.=: Lude.toQuery
            ( Lude.toQueryList "VpcSecurityGroupId"
                Lude.<$> vpcSecurityGroupIds
            ),
        "MultiAZ" Lude.=: multiAZ,
        "SourceDbiResourceId" Lude.=: sourceDBiResourceId,
        "OptionGroupName" Lude.=: optionGroupName,
        "CopyTagsToSnapshot" Lude.=: copyTagsToSnapshot,
        "TdeCredentialArn" Lude.=: tdeCredentialARN,
        "DomainIAMRoleName" Lude.=: domainIAMRoleName,
        "Tags" Lude.=: Lude.toQuery (Lude.toQueryList "Tag" Lude.<$> tags),
        "Port" Lude.=: port,
        "EnableIAMDatabaseAuthentication"
          Lude.=: enableIAMDatabaseAuthentication,
        "UseDefaultProcessorFeatures" Lude.=: useDefaultProcessorFeatures,
        "StorageType" Lude.=: storageType,
        "EnableCloudwatchLogsExports"
          Lude.=: Lude.toQuery
            (Lude.toQueryList "member" Lude.<$> enableCloudwatchLogsExports),
        "DBName" Lude.=: dbName,
        "TargetDBInstanceIdentifier" Lude.=: targetDBInstanceIdentifier
      ]

-- | /See:/ 'mkRestoreDBInstanceToPointInTimeResponse' smart constructor.
data RestoreDBInstanceToPointInTimeResponse = RestoreDBInstanceToPointInTimeResponse'
  { dbInstance ::
      Lude.Maybe
        DBInstance,
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RestoreDBInstanceToPointInTimeResponse' with the minimum fields required to make a request.
--
-- * 'dbInstance' - Undocumented field.
-- * 'responseStatus' - The response status code.
mkRestoreDBInstanceToPointInTimeResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  RestoreDBInstanceToPointInTimeResponse
mkRestoreDBInstanceToPointInTimeResponse pResponseStatus_ =
  RestoreDBInstanceToPointInTimeResponse'
    { dbInstance =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'dbInstance' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rditpitrsDBInstance :: Lens.Lens' RestoreDBInstanceToPointInTimeResponse (Lude.Maybe DBInstance)
rditpitrsDBInstance = Lens.lens (dbInstance :: RestoreDBInstanceToPointInTimeResponse -> Lude.Maybe DBInstance) (\s a -> s {dbInstance = a} :: RestoreDBInstanceToPointInTimeResponse)
{-# DEPRECATED rditpitrsDBInstance "Use generic-lens or generic-optics with 'dbInstance' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rditpitrsResponseStatus :: Lens.Lens' RestoreDBInstanceToPointInTimeResponse Lude.Int
rditpitrsResponseStatus = Lens.lens (responseStatus :: RestoreDBInstanceToPointInTimeResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: RestoreDBInstanceToPointInTimeResponse)
{-# DEPRECATED rditpitrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
