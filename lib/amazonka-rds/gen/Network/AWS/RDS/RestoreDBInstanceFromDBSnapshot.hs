{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.RestoreDBInstanceFromDBSnapshot
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new DB instance from a DB snapshot. The target database is created from the source database restore point with the most of original configuration with the default security group and the default DB parameter group. By default, the new DB instance is created as a single-AZ deployment except when the instance is a SQL Server instance that has an option group that is associated with mirroring; in this case, the instance becomes a mirrored AZ deployment and not a single-AZ deployment.
--
-- If your intent is to replace your original DB instance with the new, restored DB instance, then rename your original DB instance before you call the RestoreDBInstanceFromDBSnapshot action. RDS doesn't allow two DB instances with the same name. Once you have renamed your original DB instance with a different identifier, then you can pass the original name of the DB instance as the DBInstanceIdentifier in the call to the RestoreDBInstanceFromDBSnapshot action. The result is that you will replace the original DB instance with the DB instance created from the snapshot.
-- If you are restoring from a shared manual DB snapshot, the @DBSnapshotIdentifier@ must be the ARN of the shared DB snapshot.
module Network.AWS.RDS.RestoreDBInstanceFromDBSnapshot
  ( -- * Creating a request
    RestoreDBInstanceFromDBSnapshot (..),
    mkRestoreDBInstanceFromDBSnapshot,

    -- ** Request lenses
    rdifdsDeletionProtection,
    rdifdsPubliclyAccessible,
    rdifdsAutoMinorVersionUpgrade,
    rdifdsDBSubnetGroupName,
    rdifdsIOPS,
    rdifdsDomain,
    rdifdsEngine,
    rdifdsTDECredentialPassword,
    rdifdsProcessorFeatures,
    rdifdsDBInstanceClass,
    rdifdsLicenseModel,
    rdifdsDBParameterGroupName,
    rdifdsAvailabilityZone,
    rdifdsVPCSecurityGroupIds,
    rdifdsMultiAZ,
    rdifdsOptionGroupName,
    rdifdsCopyTagsToSnapshot,
    rdifdsTDECredentialARN,
    rdifdsDomainIAMRoleName,
    rdifdsTags,
    rdifdsPort,
    rdifdsEnableIAMDatabaseAuthentication,
    rdifdsUseDefaultProcessorFeatures,
    rdifdsStorageType,
    rdifdsEnableCloudwatchLogsExports,
    rdifdsDBName,
    rdifdsDBInstanceIdentifier,
    rdifdsDBSnapshotIdentifier,

    -- * Destructuring the response
    RestoreDBInstanceFromDBSnapshotResponse (..),
    mkRestoreDBInstanceFromDBSnapshotResponse,

    -- ** Response lenses
    rdifdsrsDBInstance,
    rdifdsrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.RDS.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- |
--
-- /See:/ 'mkRestoreDBInstanceFromDBSnapshot' smart constructor.
data RestoreDBInstanceFromDBSnapshot = RestoreDBInstanceFromDBSnapshot'
  { deletionProtection ::
      Lude.Maybe Lude.Bool,
    publiclyAccessible ::
      Lude.Maybe Lude.Bool,
    autoMinorVersionUpgrade ::
      Lude.Maybe Lude.Bool,
    dbSubnetGroupName ::
      Lude.Maybe Lude.Text,
    iops :: Lude.Maybe Lude.Int,
    domain ::
      Lude.Maybe Lude.Text,
    engine ::
      Lude.Maybe Lude.Text,
    tdeCredentialPassword ::
      Lude.Maybe Lude.Text,
    processorFeatures ::
      Lude.Maybe
        [ProcessorFeature],
    dbInstanceClass ::
      Lude.Maybe Lude.Text,
    licenseModel ::
      Lude.Maybe Lude.Text,
    dbParameterGroupName ::
      Lude.Maybe Lude.Text,
    availabilityZone ::
      Lude.Maybe Lude.Text,
    vpcSecurityGroupIds ::
      Lude.Maybe [Lude.Text],
    multiAZ ::
      Lude.Maybe Lude.Bool,
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
    dbInstanceIdentifier ::
      Lude.Text,
    dbSnapshotIdentifier ::
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

-- | Creates a value of 'RestoreDBInstanceFromDBSnapshot' with the minimum fields required to make a request.
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
-- * 'dbInstanceIdentifier' - Name of the DB instance to create from the DB snapshot. This parameter isn't case-sensitive.
--
-- Constraints:
--
--     * Must contain from 1 to 63 numbers, letters, or hyphens
--
--
--     * First character must be a letter
--
--
--     * Can't end with a hyphen or contain two consecutive hyphens
--
--
-- Example: @my-snapshot-id@
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
-- * 'dbSnapshotIdentifier' - The identifier for the DB snapshot to restore from.
--
-- Constraints:
--
--     * Must match the identifier of an existing DBSnapshot.
--
--
--     * If you are restoring from a shared manual DB snapshot, the @DBSnapshotIdentifier@ must be the ARN of the shared DB snapshot.
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
-- Constraint: Must be compatible with the engine of the source. For example, you can restore a MariaDB 10.1 DB instance from a MySQL 5.6 snapshot.
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
-- * 'iops' - Specifies the amount of provisioned IOPS for the DB instance, expressed in I/O operations per second. If this parameter isn't specified, the IOPS value is taken from the backup. If this parameter is set to 0, the new instance is converted to a non-PIOPS instance. The conversion takes additional time, though your DB instance is available for connections before the conversion starts.
--
-- The provisioned IOPS value must follow the requirements for your database engine. For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/CHAP_Storage.html#USER_PIOPS Amazon RDS Provisioned IOPS Storage to Improve Performance> in the /Amazon RDS User Guide./
-- Constraints: Must be an integer greater than 1000.
-- * 'licenseModel' - License model information for the restored DB instance.
--
-- Default: Same as source.
-- Valid values: @license-included@ | @bring-your-own-license@ | @general-public-license@
-- * 'multiAZ' - A value that indicates whether the DB instance is a Multi-AZ deployment.
--
-- Constraint: You can't specify the @AvailabilityZone@ parameter if the DB instance is a Multi-AZ deployment.
-- * 'optionGroupName' - The name of the option group to be used for the restored DB instance.
--
-- Permanent options, such as the TDE option for Oracle Advanced Security TDE, can't be removed from an option group, and that option group can't be removed from a DB instance once it is associated with a DB instance
-- * 'port' - The port number on which the database accepts connections.
--
-- Default: The same port as the original DB instance
-- Constraints: Value must be @1150-65535@
-- * 'processorFeatures' - The number of CPU cores and the number of threads per core for the DB instance class of the DB instance.
-- * 'publiclyAccessible' - A value that indicates whether the DB instance is publicly accessible.
--
-- When the DB instance is publicly accessible, its DNS endpoint resolves to the private IP address from within the DB instance's VPC, and to the public IP address from outside of the DB instance's VPC. Access to the DB instance is ultimately controlled by the security group it uses, and that public access is not permitted if the security group assigned to the DB instance doesn't permit it.
-- When the DB instance isn't publicly accessible, it is an internal DB instance with a DNS name that resolves to a private IP address.
-- For more information, see 'CreateDBInstance' .
-- * 'storageType' - Specifies the storage type to be associated with the DB instance.
--
-- Valid values: @standard | gp2 | io1@
-- If you specify @io1@ , you must also include a value for the @Iops@ parameter.
-- Default: @io1@ if the @Iops@ parameter is specified, otherwise @gp2@
-- * 'tags' - Undocumented field.
-- * 'tdeCredentialARN' - The ARN from the key store with which to associate the instance for TDE encryption.
-- * 'tdeCredentialPassword' - The password for the given ARN from the key store in order to access the device.
-- * 'useDefaultProcessorFeatures' - A value that indicates whether the DB instance class of the DB instance uses its default processor features.
-- * 'vpcSecurityGroupIds' - A list of EC2 VPC security groups to associate with this DB instance.
--
-- Default: The default EC2 VPC security group for the DB subnet group's VPC.
mkRestoreDBInstanceFromDBSnapshot ::
  -- | 'dbInstanceIdentifier'
  Lude.Text ->
  -- | 'dbSnapshotIdentifier'
  Lude.Text ->
  RestoreDBInstanceFromDBSnapshot
mkRestoreDBInstanceFromDBSnapshot
  pDBInstanceIdentifier_
  pDBSnapshotIdentifier_ =
    RestoreDBInstanceFromDBSnapshot'
      { deletionProtection =
          Lude.Nothing,
        publiclyAccessible = Lude.Nothing,
        autoMinorVersionUpgrade = Lude.Nothing,
        dbSubnetGroupName = Lude.Nothing,
        iops = Lude.Nothing,
        domain = Lude.Nothing,
        engine = Lude.Nothing,
        tdeCredentialPassword = Lude.Nothing,
        processorFeatures = Lude.Nothing,
        dbInstanceClass = Lude.Nothing,
        licenseModel = Lude.Nothing,
        dbParameterGroupName = Lude.Nothing,
        availabilityZone = Lude.Nothing,
        vpcSecurityGroupIds = Lude.Nothing,
        multiAZ = Lude.Nothing,
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
        dbInstanceIdentifier = pDBInstanceIdentifier_,
        dbSnapshotIdentifier = pDBSnapshotIdentifier_
      }

-- | A value that indicates whether the DB instance has deletion protection enabled. The database can't be deleted when deletion protection is enabled. By default, deletion protection is disabled. For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_DeleteInstance.html Deleting a DB Instance> .
--
-- /Note:/ Consider using 'deletionProtection' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdifdsDeletionProtection :: Lens.Lens' RestoreDBInstanceFromDBSnapshot (Lude.Maybe Lude.Bool)
rdifdsDeletionProtection = Lens.lens (deletionProtection :: RestoreDBInstanceFromDBSnapshot -> Lude.Maybe Lude.Bool) (\s a -> s {deletionProtection = a} :: RestoreDBInstanceFromDBSnapshot)
{-# DEPRECATED rdifdsDeletionProtection "Use generic-lens or generic-optics with 'deletionProtection' instead." #-}

-- | A value that indicates whether the DB instance is publicly accessible.
--
-- When the DB instance is publicly accessible, its DNS endpoint resolves to the private IP address from within the DB instance's VPC, and to the public IP address from outside of the DB instance's VPC. Access to the DB instance is ultimately controlled by the security group it uses, and that public access is not permitted if the security group assigned to the DB instance doesn't permit it.
-- When the DB instance isn't publicly accessible, it is an internal DB instance with a DNS name that resolves to a private IP address.
-- For more information, see 'CreateDBInstance' .
--
-- /Note:/ Consider using 'publiclyAccessible' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdifdsPubliclyAccessible :: Lens.Lens' RestoreDBInstanceFromDBSnapshot (Lude.Maybe Lude.Bool)
rdifdsPubliclyAccessible = Lens.lens (publiclyAccessible :: RestoreDBInstanceFromDBSnapshot -> Lude.Maybe Lude.Bool) (\s a -> s {publiclyAccessible = a} :: RestoreDBInstanceFromDBSnapshot)
{-# DEPRECATED rdifdsPubliclyAccessible "Use generic-lens or generic-optics with 'publiclyAccessible' instead." #-}

-- | A value that indicates whether minor version upgrades are applied automatically to the DB instance during the maintenance window.
--
-- /Note:/ Consider using 'autoMinorVersionUpgrade' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdifdsAutoMinorVersionUpgrade :: Lens.Lens' RestoreDBInstanceFromDBSnapshot (Lude.Maybe Lude.Bool)
rdifdsAutoMinorVersionUpgrade = Lens.lens (autoMinorVersionUpgrade :: RestoreDBInstanceFromDBSnapshot -> Lude.Maybe Lude.Bool) (\s a -> s {autoMinorVersionUpgrade = a} :: RestoreDBInstanceFromDBSnapshot)
{-# DEPRECATED rdifdsAutoMinorVersionUpgrade "Use generic-lens or generic-optics with 'autoMinorVersionUpgrade' instead." #-}

-- | The DB subnet group name to use for the new instance.
--
-- Constraints: If supplied, must match the name of an existing DBSubnetGroup.
-- Example: @mySubnetgroup@
--
-- /Note:/ Consider using 'dbSubnetGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdifdsDBSubnetGroupName :: Lens.Lens' RestoreDBInstanceFromDBSnapshot (Lude.Maybe Lude.Text)
rdifdsDBSubnetGroupName = Lens.lens (dbSubnetGroupName :: RestoreDBInstanceFromDBSnapshot -> Lude.Maybe Lude.Text) (\s a -> s {dbSubnetGroupName = a} :: RestoreDBInstanceFromDBSnapshot)
{-# DEPRECATED rdifdsDBSubnetGroupName "Use generic-lens or generic-optics with 'dbSubnetGroupName' instead." #-}

-- | Specifies the amount of provisioned IOPS for the DB instance, expressed in I/O operations per second. If this parameter isn't specified, the IOPS value is taken from the backup. If this parameter is set to 0, the new instance is converted to a non-PIOPS instance. The conversion takes additional time, though your DB instance is available for connections before the conversion starts.
--
-- The provisioned IOPS value must follow the requirements for your database engine. For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/CHAP_Storage.html#USER_PIOPS Amazon RDS Provisioned IOPS Storage to Improve Performance> in the /Amazon RDS User Guide./
-- Constraints: Must be an integer greater than 1000.
--
-- /Note:/ Consider using 'iops' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdifdsIOPS :: Lens.Lens' RestoreDBInstanceFromDBSnapshot (Lude.Maybe Lude.Int)
rdifdsIOPS = Lens.lens (iops :: RestoreDBInstanceFromDBSnapshot -> Lude.Maybe Lude.Int) (\s a -> s {iops = a} :: RestoreDBInstanceFromDBSnapshot)
{-# DEPRECATED rdifdsIOPS "Use generic-lens or generic-optics with 'iops' instead." #-}

-- | Specify the Active Directory directory ID to restore the DB instance in. The domain must be created prior to this operation. Currently, only MySQL, Microsoft SQL Server, Oracle, and PostgreSQL DB instances can be created in an Active Directory Domain.
--
-- For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/kerberos-authentication.html Kerberos Authentication> in the /Amazon RDS User Guide/ .
--
-- /Note:/ Consider using 'domain' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdifdsDomain :: Lens.Lens' RestoreDBInstanceFromDBSnapshot (Lude.Maybe Lude.Text)
rdifdsDomain = Lens.lens (domain :: RestoreDBInstanceFromDBSnapshot -> Lude.Maybe Lude.Text) (\s a -> s {domain = a} :: RestoreDBInstanceFromDBSnapshot)
{-# DEPRECATED rdifdsDomain "Use generic-lens or generic-optics with 'domain' instead." #-}

-- | The database engine to use for the new instance.
--
-- Default: The same as source
-- Constraint: Must be compatible with the engine of the source. For example, you can restore a MariaDB 10.1 DB instance from a MySQL 5.6 snapshot.
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
rdifdsEngine :: Lens.Lens' RestoreDBInstanceFromDBSnapshot (Lude.Maybe Lude.Text)
rdifdsEngine = Lens.lens (engine :: RestoreDBInstanceFromDBSnapshot -> Lude.Maybe Lude.Text) (\s a -> s {engine = a} :: RestoreDBInstanceFromDBSnapshot)
{-# DEPRECATED rdifdsEngine "Use generic-lens or generic-optics with 'engine' instead." #-}

-- | The password for the given ARN from the key store in order to access the device.
--
-- /Note:/ Consider using 'tdeCredentialPassword' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdifdsTDECredentialPassword :: Lens.Lens' RestoreDBInstanceFromDBSnapshot (Lude.Maybe Lude.Text)
rdifdsTDECredentialPassword = Lens.lens (tdeCredentialPassword :: RestoreDBInstanceFromDBSnapshot -> Lude.Maybe Lude.Text) (\s a -> s {tdeCredentialPassword = a} :: RestoreDBInstanceFromDBSnapshot)
{-# DEPRECATED rdifdsTDECredentialPassword "Use generic-lens or generic-optics with 'tdeCredentialPassword' instead." #-}

-- | The number of CPU cores and the number of threads per core for the DB instance class of the DB instance.
--
-- /Note:/ Consider using 'processorFeatures' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdifdsProcessorFeatures :: Lens.Lens' RestoreDBInstanceFromDBSnapshot (Lude.Maybe [ProcessorFeature])
rdifdsProcessorFeatures = Lens.lens (processorFeatures :: RestoreDBInstanceFromDBSnapshot -> Lude.Maybe [ProcessorFeature]) (\s a -> s {processorFeatures = a} :: RestoreDBInstanceFromDBSnapshot)
{-# DEPRECATED rdifdsProcessorFeatures "Use generic-lens or generic-optics with 'processorFeatures' instead." #-}

-- | The compute and memory capacity of the Amazon RDS DB instance, for example, @db.m4.large@ . Not all DB instance classes are available in all AWS Regions, or for all database engines. For the full list of DB instance classes, and availability for your engine, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Concepts.DBInstanceClass.html DB Instance Class> in the /Amazon RDS User Guide./
--
-- Default: The same DBInstanceClass as the original DB instance.
--
-- /Note:/ Consider using 'dbInstanceClass' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdifdsDBInstanceClass :: Lens.Lens' RestoreDBInstanceFromDBSnapshot (Lude.Maybe Lude.Text)
rdifdsDBInstanceClass = Lens.lens (dbInstanceClass :: RestoreDBInstanceFromDBSnapshot -> Lude.Maybe Lude.Text) (\s a -> s {dbInstanceClass = a} :: RestoreDBInstanceFromDBSnapshot)
{-# DEPRECATED rdifdsDBInstanceClass "Use generic-lens or generic-optics with 'dbInstanceClass' instead." #-}

-- | License model information for the restored DB instance.
--
-- Default: Same as source.
-- Valid values: @license-included@ | @bring-your-own-license@ | @general-public-license@
--
-- /Note:/ Consider using 'licenseModel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdifdsLicenseModel :: Lens.Lens' RestoreDBInstanceFromDBSnapshot (Lude.Maybe Lude.Text)
rdifdsLicenseModel = Lens.lens (licenseModel :: RestoreDBInstanceFromDBSnapshot -> Lude.Maybe Lude.Text) (\s a -> s {licenseModel = a} :: RestoreDBInstanceFromDBSnapshot)
{-# DEPRECATED rdifdsLicenseModel "Use generic-lens or generic-optics with 'licenseModel' instead." #-}

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
rdifdsDBParameterGroupName :: Lens.Lens' RestoreDBInstanceFromDBSnapshot (Lude.Maybe Lude.Text)
rdifdsDBParameterGroupName = Lens.lens (dbParameterGroupName :: RestoreDBInstanceFromDBSnapshot -> Lude.Maybe Lude.Text) (\s a -> s {dbParameterGroupName = a} :: RestoreDBInstanceFromDBSnapshot)
{-# DEPRECATED rdifdsDBParameterGroupName "Use generic-lens or generic-optics with 'dbParameterGroupName' instead." #-}

-- | The Availability Zone (AZ) where the DB instance will be created.
--
-- Default: A random, system-chosen Availability Zone.
-- Constraint: You can't specify the @AvailabilityZone@ parameter if the DB instance is a Multi-AZ deployment.
-- Example: @us-east-1a@
--
-- /Note:/ Consider using 'availabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdifdsAvailabilityZone :: Lens.Lens' RestoreDBInstanceFromDBSnapshot (Lude.Maybe Lude.Text)
rdifdsAvailabilityZone = Lens.lens (availabilityZone :: RestoreDBInstanceFromDBSnapshot -> Lude.Maybe Lude.Text) (\s a -> s {availabilityZone = a} :: RestoreDBInstanceFromDBSnapshot)
{-# DEPRECATED rdifdsAvailabilityZone "Use generic-lens or generic-optics with 'availabilityZone' instead." #-}

-- | A list of EC2 VPC security groups to associate with this DB instance.
--
-- Default: The default EC2 VPC security group for the DB subnet group's VPC.
--
-- /Note:/ Consider using 'vpcSecurityGroupIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdifdsVPCSecurityGroupIds :: Lens.Lens' RestoreDBInstanceFromDBSnapshot (Lude.Maybe [Lude.Text])
rdifdsVPCSecurityGroupIds = Lens.lens (vpcSecurityGroupIds :: RestoreDBInstanceFromDBSnapshot -> Lude.Maybe [Lude.Text]) (\s a -> s {vpcSecurityGroupIds = a} :: RestoreDBInstanceFromDBSnapshot)
{-# DEPRECATED rdifdsVPCSecurityGroupIds "Use generic-lens or generic-optics with 'vpcSecurityGroupIds' instead." #-}

-- | A value that indicates whether the DB instance is a Multi-AZ deployment.
--
-- Constraint: You can't specify the @AvailabilityZone@ parameter if the DB instance is a Multi-AZ deployment.
--
-- /Note:/ Consider using 'multiAZ' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdifdsMultiAZ :: Lens.Lens' RestoreDBInstanceFromDBSnapshot (Lude.Maybe Lude.Bool)
rdifdsMultiAZ = Lens.lens (multiAZ :: RestoreDBInstanceFromDBSnapshot -> Lude.Maybe Lude.Bool) (\s a -> s {multiAZ = a} :: RestoreDBInstanceFromDBSnapshot)
{-# DEPRECATED rdifdsMultiAZ "Use generic-lens or generic-optics with 'multiAZ' instead." #-}

-- | The name of the option group to be used for the restored DB instance.
--
-- Permanent options, such as the TDE option for Oracle Advanced Security TDE, can't be removed from an option group, and that option group can't be removed from a DB instance once it is associated with a DB instance
--
-- /Note:/ Consider using 'optionGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdifdsOptionGroupName :: Lens.Lens' RestoreDBInstanceFromDBSnapshot (Lude.Maybe Lude.Text)
rdifdsOptionGroupName = Lens.lens (optionGroupName :: RestoreDBInstanceFromDBSnapshot -> Lude.Maybe Lude.Text) (\s a -> s {optionGroupName = a} :: RestoreDBInstanceFromDBSnapshot)
{-# DEPRECATED rdifdsOptionGroupName "Use generic-lens or generic-optics with 'optionGroupName' instead." #-}

-- | A value that indicates whether to copy all tags from the restored DB instance to snapshots of the DB instance. By default, tags are not copied.
--
-- /Note:/ Consider using 'copyTagsToSnapshot' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdifdsCopyTagsToSnapshot :: Lens.Lens' RestoreDBInstanceFromDBSnapshot (Lude.Maybe Lude.Bool)
rdifdsCopyTagsToSnapshot = Lens.lens (copyTagsToSnapshot :: RestoreDBInstanceFromDBSnapshot -> Lude.Maybe Lude.Bool) (\s a -> s {copyTagsToSnapshot = a} :: RestoreDBInstanceFromDBSnapshot)
{-# DEPRECATED rdifdsCopyTagsToSnapshot "Use generic-lens or generic-optics with 'copyTagsToSnapshot' instead." #-}

-- | The ARN from the key store with which to associate the instance for TDE encryption.
--
-- /Note:/ Consider using 'tdeCredentialARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdifdsTDECredentialARN :: Lens.Lens' RestoreDBInstanceFromDBSnapshot (Lude.Maybe Lude.Text)
rdifdsTDECredentialARN = Lens.lens (tdeCredentialARN :: RestoreDBInstanceFromDBSnapshot -> Lude.Maybe Lude.Text) (\s a -> s {tdeCredentialARN = a} :: RestoreDBInstanceFromDBSnapshot)
{-# DEPRECATED rdifdsTDECredentialARN "Use generic-lens or generic-optics with 'tdeCredentialARN' instead." #-}

-- | Specify the name of the IAM role to be used when making API calls to the Directory Service.
--
-- /Note:/ Consider using 'domainIAMRoleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdifdsDomainIAMRoleName :: Lens.Lens' RestoreDBInstanceFromDBSnapshot (Lude.Maybe Lude.Text)
rdifdsDomainIAMRoleName = Lens.lens (domainIAMRoleName :: RestoreDBInstanceFromDBSnapshot -> Lude.Maybe Lude.Text) (\s a -> s {domainIAMRoleName = a} :: RestoreDBInstanceFromDBSnapshot)
{-# DEPRECATED rdifdsDomainIAMRoleName "Use generic-lens or generic-optics with 'domainIAMRoleName' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdifdsTags :: Lens.Lens' RestoreDBInstanceFromDBSnapshot (Lude.Maybe [Tag])
rdifdsTags = Lens.lens (tags :: RestoreDBInstanceFromDBSnapshot -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: RestoreDBInstanceFromDBSnapshot)
{-# DEPRECATED rdifdsTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The port number on which the database accepts connections.
--
-- Default: The same port as the original DB instance
-- Constraints: Value must be @1150-65535@
--
-- /Note:/ Consider using 'port' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdifdsPort :: Lens.Lens' RestoreDBInstanceFromDBSnapshot (Lude.Maybe Lude.Int)
rdifdsPort = Lens.lens (port :: RestoreDBInstanceFromDBSnapshot -> Lude.Maybe Lude.Int) (\s a -> s {port = a} :: RestoreDBInstanceFromDBSnapshot)
{-# DEPRECATED rdifdsPort "Use generic-lens or generic-optics with 'port' instead." #-}

-- | A value that indicates whether to enable mapping of AWS Identity and Access Management (IAM) accounts to database accounts. By default, mapping is disabled.
--
-- For more information about IAM database authentication, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/UsingWithRDS.IAMDBAuth.html IAM Database Authentication for MySQL and PostgreSQL> in the /Amazon RDS User Guide./
--
-- /Note:/ Consider using 'enableIAMDatabaseAuthentication' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdifdsEnableIAMDatabaseAuthentication :: Lens.Lens' RestoreDBInstanceFromDBSnapshot (Lude.Maybe Lude.Bool)
rdifdsEnableIAMDatabaseAuthentication = Lens.lens (enableIAMDatabaseAuthentication :: RestoreDBInstanceFromDBSnapshot -> Lude.Maybe Lude.Bool) (\s a -> s {enableIAMDatabaseAuthentication = a} :: RestoreDBInstanceFromDBSnapshot)
{-# DEPRECATED rdifdsEnableIAMDatabaseAuthentication "Use generic-lens or generic-optics with 'enableIAMDatabaseAuthentication' instead." #-}

-- | A value that indicates whether the DB instance class of the DB instance uses its default processor features.
--
-- /Note:/ Consider using 'useDefaultProcessorFeatures' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdifdsUseDefaultProcessorFeatures :: Lens.Lens' RestoreDBInstanceFromDBSnapshot (Lude.Maybe Lude.Bool)
rdifdsUseDefaultProcessorFeatures = Lens.lens (useDefaultProcessorFeatures :: RestoreDBInstanceFromDBSnapshot -> Lude.Maybe Lude.Bool) (\s a -> s {useDefaultProcessorFeatures = a} :: RestoreDBInstanceFromDBSnapshot)
{-# DEPRECATED rdifdsUseDefaultProcessorFeatures "Use generic-lens or generic-optics with 'useDefaultProcessorFeatures' instead." #-}

-- | Specifies the storage type to be associated with the DB instance.
--
-- Valid values: @standard | gp2 | io1@
-- If you specify @io1@ , you must also include a value for the @Iops@ parameter.
-- Default: @io1@ if the @Iops@ parameter is specified, otherwise @gp2@
--
-- /Note:/ Consider using 'storageType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdifdsStorageType :: Lens.Lens' RestoreDBInstanceFromDBSnapshot (Lude.Maybe Lude.Text)
rdifdsStorageType = Lens.lens (storageType :: RestoreDBInstanceFromDBSnapshot -> Lude.Maybe Lude.Text) (\s a -> s {storageType = a} :: RestoreDBInstanceFromDBSnapshot)
{-# DEPRECATED rdifdsStorageType "Use generic-lens or generic-optics with 'storageType' instead." #-}

-- | The list of logs that the restored DB instance is to export to CloudWatch Logs. The values in the list depend on the DB engine being used. For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_LogAccess.html#USER_LogAccess.Procedural.UploadtoCloudWatch Publishing Database Logs to Amazon CloudWatch Logs> in the /Amazon RDS User Guide/ .
--
-- /Note:/ Consider using 'enableCloudwatchLogsExports' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdifdsEnableCloudwatchLogsExports :: Lens.Lens' RestoreDBInstanceFromDBSnapshot (Lude.Maybe [Lude.Text])
rdifdsEnableCloudwatchLogsExports = Lens.lens (enableCloudwatchLogsExports :: RestoreDBInstanceFromDBSnapshot -> Lude.Maybe [Lude.Text]) (\s a -> s {enableCloudwatchLogsExports = a} :: RestoreDBInstanceFromDBSnapshot)
{-# DEPRECATED rdifdsEnableCloudwatchLogsExports "Use generic-lens or generic-optics with 'enableCloudwatchLogsExports' instead." #-}

-- | The database name for the restored DB instance.
--
-- /Note:/ Consider using 'dbName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdifdsDBName :: Lens.Lens' RestoreDBInstanceFromDBSnapshot (Lude.Maybe Lude.Text)
rdifdsDBName = Lens.lens (dbName :: RestoreDBInstanceFromDBSnapshot -> Lude.Maybe Lude.Text) (\s a -> s {dbName = a} :: RestoreDBInstanceFromDBSnapshot)
{-# DEPRECATED rdifdsDBName "Use generic-lens or generic-optics with 'dbName' instead." #-}

-- | Name of the DB instance to create from the DB snapshot. This parameter isn't case-sensitive.
--
-- Constraints:
--
--     * Must contain from 1 to 63 numbers, letters, or hyphens
--
--
--     * First character must be a letter
--
--
--     * Can't end with a hyphen or contain two consecutive hyphens
--
--
-- Example: @my-snapshot-id@
--
-- /Note:/ Consider using 'dbInstanceIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdifdsDBInstanceIdentifier :: Lens.Lens' RestoreDBInstanceFromDBSnapshot Lude.Text
rdifdsDBInstanceIdentifier = Lens.lens (dbInstanceIdentifier :: RestoreDBInstanceFromDBSnapshot -> Lude.Text) (\s a -> s {dbInstanceIdentifier = a} :: RestoreDBInstanceFromDBSnapshot)
{-# DEPRECATED rdifdsDBInstanceIdentifier "Use generic-lens or generic-optics with 'dbInstanceIdentifier' instead." #-}

-- | The identifier for the DB snapshot to restore from.
--
-- Constraints:
--
--     * Must match the identifier of an existing DBSnapshot.
--
--
--     * If you are restoring from a shared manual DB snapshot, the @DBSnapshotIdentifier@ must be the ARN of the shared DB snapshot.
--
--
--
-- /Note:/ Consider using 'dbSnapshotIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdifdsDBSnapshotIdentifier :: Lens.Lens' RestoreDBInstanceFromDBSnapshot Lude.Text
rdifdsDBSnapshotIdentifier = Lens.lens (dbSnapshotIdentifier :: RestoreDBInstanceFromDBSnapshot -> Lude.Text) (\s a -> s {dbSnapshotIdentifier = a} :: RestoreDBInstanceFromDBSnapshot)
{-# DEPRECATED rdifdsDBSnapshotIdentifier "Use generic-lens or generic-optics with 'dbSnapshotIdentifier' instead." #-}

instance Lude.AWSRequest RestoreDBInstanceFromDBSnapshot where
  type
    Rs RestoreDBInstanceFromDBSnapshot =
      RestoreDBInstanceFromDBSnapshotResponse
  request = Req.postQuery rdsService
  response =
    Res.receiveXMLWrapper
      "RestoreDBInstanceFromDBSnapshotResult"
      ( \s h x ->
          RestoreDBInstanceFromDBSnapshotResponse'
            Lude.<$> (x Lude..@? "DBInstance") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders RestoreDBInstanceFromDBSnapshot where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath RestoreDBInstanceFromDBSnapshot where
  toPath = Lude.const "/"

instance Lude.ToQuery RestoreDBInstanceFromDBSnapshot where
  toQuery RestoreDBInstanceFromDBSnapshot' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("RestoreDBInstanceFromDBSnapshot" :: Lude.ByteString),
        "Version" Lude.=: ("2014-10-31" :: Lude.ByteString),
        "DeletionProtection" Lude.=: deletionProtection,
        "PubliclyAccessible" Lude.=: publiclyAccessible,
        "AutoMinorVersionUpgrade" Lude.=: autoMinorVersionUpgrade,
        "DBSubnetGroupName" Lude.=: dbSubnetGroupName,
        "Iops" Lude.=: iops,
        "Domain" Lude.=: domain,
        "Engine" Lude.=: engine,
        "TdeCredentialPassword" Lude.=: tdeCredentialPassword,
        "ProcessorFeatures"
          Lude.=: Lude.toQuery
            (Lude.toQueryList "ProcessorFeature" Lude.<$> processorFeatures),
        "DBInstanceClass" Lude.=: dbInstanceClass,
        "LicenseModel" Lude.=: licenseModel,
        "DBParameterGroupName" Lude.=: dbParameterGroupName,
        "AvailabilityZone" Lude.=: availabilityZone,
        "VpcSecurityGroupIds"
          Lude.=: Lude.toQuery
            ( Lude.toQueryList "VpcSecurityGroupId"
                Lude.<$> vpcSecurityGroupIds
            ),
        "MultiAZ" Lude.=: multiAZ,
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
        "DBInstanceIdentifier" Lude.=: dbInstanceIdentifier,
        "DBSnapshotIdentifier" Lude.=: dbSnapshotIdentifier
      ]

-- | /See:/ 'mkRestoreDBInstanceFromDBSnapshotResponse' smart constructor.
data RestoreDBInstanceFromDBSnapshotResponse = RestoreDBInstanceFromDBSnapshotResponse'
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

-- | Creates a value of 'RestoreDBInstanceFromDBSnapshotResponse' with the minimum fields required to make a request.
--
-- * 'dbInstance' - Undocumented field.
-- * 'responseStatus' - The response status code.
mkRestoreDBInstanceFromDBSnapshotResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  RestoreDBInstanceFromDBSnapshotResponse
mkRestoreDBInstanceFromDBSnapshotResponse pResponseStatus_ =
  RestoreDBInstanceFromDBSnapshotResponse'
    { dbInstance =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'dbInstance' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdifdsrsDBInstance :: Lens.Lens' RestoreDBInstanceFromDBSnapshotResponse (Lude.Maybe DBInstance)
rdifdsrsDBInstance = Lens.lens (dbInstance :: RestoreDBInstanceFromDBSnapshotResponse -> Lude.Maybe DBInstance) (\s a -> s {dbInstance = a} :: RestoreDBInstanceFromDBSnapshotResponse)
{-# DEPRECATED rdifdsrsDBInstance "Use generic-lens or generic-optics with 'dbInstance' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdifdsrsResponseStatus :: Lens.Lens' RestoreDBInstanceFromDBSnapshotResponse Lude.Int
rdifdsrsResponseStatus = Lens.lens (responseStatus :: RestoreDBInstanceFromDBSnapshotResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: RestoreDBInstanceFromDBSnapshotResponse)
{-# DEPRECATED rdifdsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
