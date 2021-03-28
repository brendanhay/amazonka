{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      RestoreDBInstanceFromDBSnapshot (..)
    , mkRestoreDBInstanceFromDBSnapshot
    -- ** Request lenses
    , rdbifdbsDBInstanceIdentifier
    , rdbifdbsDBSnapshotIdentifier
    , rdbifdbsAutoMinorVersionUpgrade
    , rdbifdbsAvailabilityZone
    , rdbifdbsCopyTagsToSnapshot
    , rdbifdbsDBInstanceClass
    , rdbifdbsDBName
    , rdbifdbsDBParameterGroupName
    , rdbifdbsDBSubnetGroupName
    , rdbifdbsDeletionProtection
    , rdbifdbsDomain
    , rdbifdbsDomainIAMRoleName
    , rdbifdbsEnableCloudwatchLogsExports
    , rdbifdbsEnableIAMDatabaseAuthentication
    , rdbifdbsEngine
    , rdbifdbsIops
    , rdbifdbsLicenseModel
    , rdbifdbsMultiAZ
    , rdbifdbsOptionGroupName
    , rdbifdbsPort
    , rdbifdbsProcessorFeatures
    , rdbifdbsPubliclyAccessible
    , rdbifdbsStorageType
    , rdbifdbsTags
    , rdbifdbsTdeCredentialArn
    , rdbifdbsTdeCredentialPassword
    , rdbifdbsUseDefaultProcessorFeatures
    , rdbifdbsVpcSecurityGroupIds

    -- * Destructuring the response
    , RestoreDBInstanceFromDBSnapshotResponse (..)
    , mkRestoreDBInstanceFromDBSnapshotResponse
    -- ** Response lenses
    , rdbifdbsrrsDBInstance
    , rdbifdbsrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | 
--
-- /See:/ 'mkRestoreDBInstanceFromDBSnapshot' smart constructor.
data RestoreDBInstanceFromDBSnapshot = RestoreDBInstanceFromDBSnapshot'
  { dBInstanceIdentifier :: Core.Text
    -- ^ Name of the DB instance to create from the DB snapshot. This parameter isn't case-sensitive.
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
  , dBSnapshotIdentifier :: Core.Text
    -- ^ The identifier for the DB snapshot to restore from.
--
-- Constraints:
--
--     * Must match the identifier of an existing DBSnapshot.
--
--
--     * If you are restoring from a shared manual DB snapshot, the @DBSnapshotIdentifier@ must be the ARN of the shared DB snapshot.
--
--
  , autoMinorVersionUpgrade :: Core.Maybe Core.Bool
    -- ^ A value that indicates whether minor version upgrades are applied automatically to the DB instance during the maintenance window.
  , availabilityZone :: Core.Maybe Core.Text
    -- ^ The Availability Zone (AZ) where the DB instance will be created.
--
-- Default: A random, system-chosen Availability Zone.
-- Constraint: You can't specify the @AvailabilityZone@ parameter if the DB instance is a Multi-AZ deployment.
-- Example: @us-east-1a@ 
  , copyTagsToSnapshot :: Core.Maybe Core.Bool
    -- ^ A value that indicates whether to copy all tags from the restored DB instance to snapshots of the DB instance. By default, tags are not copied.
  , dBInstanceClass :: Core.Maybe Core.Text
    -- ^ The compute and memory capacity of the Amazon RDS DB instance, for example, @db.m4.large@ . Not all DB instance classes are available in all AWS Regions, or for all database engines. For the full list of DB instance classes, and availability for your engine, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Concepts.DBInstanceClass.html DB Instance Class> in the /Amazon RDS User Guide./ 
--
-- Default: The same DBInstanceClass as the original DB instance.
  , dBName :: Core.Maybe Core.Text
    -- ^ The database name for the restored DB instance.
  , dBParameterGroupName :: Core.Maybe Core.Text
    -- ^ The name of the DB parameter group to associate with this DB instance.
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
  , dBSubnetGroupName :: Core.Maybe Core.Text
    -- ^ The DB subnet group name to use for the new instance.
--
-- Constraints: If supplied, must match the name of an existing DBSubnetGroup.
-- Example: @mySubnetgroup@ 
  , deletionProtection :: Core.Maybe Core.Bool
    -- ^ A value that indicates whether the DB instance has deletion protection enabled. The database can't be deleted when deletion protection is enabled. By default, deletion protection is disabled. For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_DeleteInstance.html Deleting a DB Instance> . 
  , domain :: Core.Maybe Core.Text
    -- ^ Specify the Active Directory directory ID to restore the DB instance in. The domain must be created prior to this operation. Currently, only MySQL, Microsoft SQL Server, Oracle, and PostgreSQL DB instances can be created in an Active Directory Domain.
--
-- For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/kerberos-authentication.html Kerberos Authentication> in the /Amazon RDS User Guide/ .
  , domainIAMRoleName :: Core.Maybe Core.Text
    -- ^ Specify the name of the IAM role to be used when making API calls to the Directory Service.
  , enableCloudwatchLogsExports :: Core.Maybe [Core.Text]
    -- ^ The list of logs that the restored DB instance is to export to CloudWatch Logs. The values in the list depend on the DB engine being used. For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_LogAccess.html#USER_LogAccess.Procedural.UploadtoCloudWatch Publishing Database Logs to Amazon CloudWatch Logs> in the /Amazon RDS User Guide/ .
  , enableIAMDatabaseAuthentication :: Core.Maybe Core.Bool
    -- ^ A value that indicates whether to enable mapping of AWS Identity and Access Management (IAM) accounts to database accounts. By default, mapping is disabled.
--
-- For more information about IAM database authentication, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/UsingWithRDS.IAMDBAuth.html IAM Database Authentication for MySQL and PostgreSQL> in the /Amazon RDS User Guide./ 
  , engine :: Core.Maybe Core.Text
    -- ^ The database engine to use for the new instance.
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
  , iops :: Core.Maybe Core.Int
    -- ^ Specifies the amount of provisioned IOPS for the DB instance, expressed in I/O operations per second. If this parameter isn't specified, the IOPS value is taken from the backup. If this parameter is set to 0, the new instance is converted to a non-PIOPS instance. The conversion takes additional time, though your DB instance is available for connections before the conversion starts. 
--
-- The provisioned IOPS value must follow the requirements for your database engine. For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/CHAP_Storage.html#USER_PIOPS Amazon RDS Provisioned IOPS Storage to Improve Performance> in the /Amazon RDS User Guide./ 
-- Constraints: Must be an integer greater than 1000.
  , licenseModel :: Core.Maybe Core.Text
    -- ^ License model information for the restored DB instance.
--
-- Default: Same as source.
-- Valid values: @license-included@ | @bring-your-own-license@ | @general-public-license@ 
  , multiAZ :: Core.Maybe Core.Bool
    -- ^ A value that indicates whether the DB instance is a Multi-AZ deployment.
--
-- Constraint: You can't specify the @AvailabilityZone@ parameter if the DB instance is a Multi-AZ deployment.
  , optionGroupName :: Core.Maybe Core.Text
    -- ^ The name of the option group to be used for the restored DB instance.
--
-- Permanent options, such as the TDE option for Oracle Advanced Security TDE, can't be removed from an option group, and that option group can't be removed from a DB instance once it is associated with a DB instance
  , port :: Core.Maybe Core.Int
    -- ^ The port number on which the database accepts connections.
--
-- Default: The same port as the original DB instance
-- Constraints: Value must be @1150-65535@ 
  , processorFeatures :: Core.Maybe [Types.ProcessorFeature]
    -- ^ The number of CPU cores and the number of threads per core for the DB instance class of the DB instance.
  , publiclyAccessible :: Core.Maybe Core.Bool
    -- ^ A value that indicates whether the DB instance is publicly accessible.
--
-- When the DB instance is publicly accessible, its DNS endpoint resolves to the private IP address from within the DB instance's VPC, and to the public IP address from outside of the DB instance's VPC. Access to the DB instance is ultimately controlled by the security group it uses, and that public access is not permitted if the security group assigned to the DB instance doesn't permit it.
-- When the DB instance isn't publicly accessible, it is an internal DB instance with a DNS name that resolves to a private IP address.
-- For more information, see 'CreateDBInstance' .
  , storageType :: Core.Maybe Core.Text
    -- ^ Specifies the storage type to be associated with the DB instance.
--
-- Valid values: @standard | gp2 | io1@ 
-- If you specify @io1@ , you must also include a value for the @Iops@ parameter. 
-- Default: @io1@ if the @Iops@ parameter is specified, otherwise @gp2@ 
  , tags :: Core.Maybe [Types.Tag]
  , tdeCredentialArn :: Core.Maybe Core.Text
    -- ^ The ARN from the key store with which to associate the instance for TDE encryption.
  , tdeCredentialPassword :: Core.Maybe Core.Text
    -- ^ The password for the given ARN from the key store in order to access the device.
  , useDefaultProcessorFeatures :: Core.Maybe Core.Bool
    -- ^ A value that indicates whether the DB instance class of the DB instance uses its default processor features.
  , vpcSecurityGroupIds :: Core.Maybe [Core.Text]
    -- ^ A list of EC2 VPC security groups to associate with this DB instance. 
--
-- Default: The default EC2 VPC security group for the DB subnet group's VPC. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RestoreDBInstanceFromDBSnapshot' value with any optional fields omitted.
mkRestoreDBInstanceFromDBSnapshot
    :: Core.Text -- ^ 'dBInstanceIdentifier'
    -> Core.Text -- ^ 'dBSnapshotIdentifier'
    -> RestoreDBInstanceFromDBSnapshot
mkRestoreDBInstanceFromDBSnapshot dBInstanceIdentifier
  dBSnapshotIdentifier
  = RestoreDBInstanceFromDBSnapshot'{dBInstanceIdentifier,
                                     dBSnapshotIdentifier, autoMinorVersionUpgrade = Core.Nothing,
                                     availabilityZone = Core.Nothing,
                                     copyTagsToSnapshot = Core.Nothing,
                                     dBInstanceClass = Core.Nothing, dBName = Core.Nothing,
                                     dBParameterGroupName = Core.Nothing,
                                     dBSubnetGroupName = Core.Nothing,
                                     deletionProtection = Core.Nothing, domain = Core.Nothing,
                                     domainIAMRoleName = Core.Nothing,
                                     enableCloudwatchLogsExports = Core.Nothing,
                                     enableIAMDatabaseAuthentication = Core.Nothing,
                                     engine = Core.Nothing, iops = Core.Nothing,
                                     licenseModel = Core.Nothing, multiAZ = Core.Nothing,
                                     optionGroupName = Core.Nothing, port = Core.Nothing,
                                     processorFeatures = Core.Nothing,
                                     publiclyAccessible = Core.Nothing, storageType = Core.Nothing,
                                     tags = Core.Nothing, tdeCredentialArn = Core.Nothing,
                                     tdeCredentialPassword = Core.Nothing,
                                     useDefaultProcessorFeatures = Core.Nothing,
                                     vpcSecurityGroupIds = Core.Nothing}

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
-- /Note:/ Consider using 'dBInstanceIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbifdbsDBInstanceIdentifier :: Lens.Lens' RestoreDBInstanceFromDBSnapshot Core.Text
rdbifdbsDBInstanceIdentifier = Lens.field @"dBInstanceIdentifier"
{-# INLINEABLE rdbifdbsDBInstanceIdentifier #-}
{-# DEPRECATED dBInstanceIdentifier "Use generic-lens or generic-optics with 'dBInstanceIdentifier' instead"  #-}

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
-- /Note:/ Consider using 'dBSnapshotIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbifdbsDBSnapshotIdentifier :: Lens.Lens' RestoreDBInstanceFromDBSnapshot Core.Text
rdbifdbsDBSnapshotIdentifier = Lens.field @"dBSnapshotIdentifier"
{-# INLINEABLE rdbifdbsDBSnapshotIdentifier #-}
{-# DEPRECATED dBSnapshotIdentifier "Use generic-lens or generic-optics with 'dBSnapshotIdentifier' instead"  #-}

-- | A value that indicates whether minor version upgrades are applied automatically to the DB instance during the maintenance window.
--
-- /Note:/ Consider using 'autoMinorVersionUpgrade' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbifdbsAutoMinorVersionUpgrade :: Lens.Lens' RestoreDBInstanceFromDBSnapshot (Core.Maybe Core.Bool)
rdbifdbsAutoMinorVersionUpgrade = Lens.field @"autoMinorVersionUpgrade"
{-# INLINEABLE rdbifdbsAutoMinorVersionUpgrade #-}
{-# DEPRECATED autoMinorVersionUpgrade "Use generic-lens or generic-optics with 'autoMinorVersionUpgrade' instead"  #-}

-- | The Availability Zone (AZ) where the DB instance will be created.
--
-- Default: A random, system-chosen Availability Zone.
-- Constraint: You can't specify the @AvailabilityZone@ parameter if the DB instance is a Multi-AZ deployment.
-- Example: @us-east-1a@ 
--
-- /Note:/ Consider using 'availabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbifdbsAvailabilityZone :: Lens.Lens' RestoreDBInstanceFromDBSnapshot (Core.Maybe Core.Text)
rdbifdbsAvailabilityZone = Lens.field @"availabilityZone"
{-# INLINEABLE rdbifdbsAvailabilityZone #-}
{-# DEPRECATED availabilityZone "Use generic-lens or generic-optics with 'availabilityZone' instead"  #-}

-- | A value that indicates whether to copy all tags from the restored DB instance to snapshots of the DB instance. By default, tags are not copied.
--
-- /Note:/ Consider using 'copyTagsToSnapshot' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbifdbsCopyTagsToSnapshot :: Lens.Lens' RestoreDBInstanceFromDBSnapshot (Core.Maybe Core.Bool)
rdbifdbsCopyTagsToSnapshot = Lens.field @"copyTagsToSnapshot"
{-# INLINEABLE rdbifdbsCopyTagsToSnapshot #-}
{-# DEPRECATED copyTagsToSnapshot "Use generic-lens or generic-optics with 'copyTagsToSnapshot' instead"  #-}

-- | The compute and memory capacity of the Amazon RDS DB instance, for example, @db.m4.large@ . Not all DB instance classes are available in all AWS Regions, or for all database engines. For the full list of DB instance classes, and availability for your engine, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Concepts.DBInstanceClass.html DB Instance Class> in the /Amazon RDS User Guide./ 
--
-- Default: The same DBInstanceClass as the original DB instance.
--
-- /Note:/ Consider using 'dBInstanceClass' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbifdbsDBInstanceClass :: Lens.Lens' RestoreDBInstanceFromDBSnapshot (Core.Maybe Core.Text)
rdbifdbsDBInstanceClass = Lens.field @"dBInstanceClass"
{-# INLINEABLE rdbifdbsDBInstanceClass #-}
{-# DEPRECATED dBInstanceClass "Use generic-lens or generic-optics with 'dBInstanceClass' instead"  #-}

-- | The database name for the restored DB instance.
--
-- /Note:/ Consider using 'dBName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbifdbsDBName :: Lens.Lens' RestoreDBInstanceFromDBSnapshot (Core.Maybe Core.Text)
rdbifdbsDBName = Lens.field @"dBName"
{-# INLINEABLE rdbifdbsDBName #-}
{-# DEPRECATED dBName "Use generic-lens or generic-optics with 'dBName' instead"  #-}

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
rdbifdbsDBParameterGroupName :: Lens.Lens' RestoreDBInstanceFromDBSnapshot (Core.Maybe Core.Text)
rdbifdbsDBParameterGroupName = Lens.field @"dBParameterGroupName"
{-# INLINEABLE rdbifdbsDBParameterGroupName #-}
{-# DEPRECATED dBParameterGroupName "Use generic-lens or generic-optics with 'dBParameterGroupName' instead"  #-}

-- | The DB subnet group name to use for the new instance.
--
-- Constraints: If supplied, must match the name of an existing DBSubnetGroup.
-- Example: @mySubnetgroup@ 
--
-- /Note:/ Consider using 'dBSubnetGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbifdbsDBSubnetGroupName :: Lens.Lens' RestoreDBInstanceFromDBSnapshot (Core.Maybe Core.Text)
rdbifdbsDBSubnetGroupName = Lens.field @"dBSubnetGroupName"
{-# INLINEABLE rdbifdbsDBSubnetGroupName #-}
{-# DEPRECATED dBSubnetGroupName "Use generic-lens or generic-optics with 'dBSubnetGroupName' instead"  #-}

-- | A value that indicates whether the DB instance has deletion protection enabled. The database can't be deleted when deletion protection is enabled. By default, deletion protection is disabled. For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_DeleteInstance.html Deleting a DB Instance> . 
--
-- /Note:/ Consider using 'deletionProtection' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbifdbsDeletionProtection :: Lens.Lens' RestoreDBInstanceFromDBSnapshot (Core.Maybe Core.Bool)
rdbifdbsDeletionProtection = Lens.field @"deletionProtection"
{-# INLINEABLE rdbifdbsDeletionProtection #-}
{-# DEPRECATED deletionProtection "Use generic-lens or generic-optics with 'deletionProtection' instead"  #-}

-- | Specify the Active Directory directory ID to restore the DB instance in. The domain must be created prior to this operation. Currently, only MySQL, Microsoft SQL Server, Oracle, and PostgreSQL DB instances can be created in an Active Directory Domain.
--
-- For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/kerberos-authentication.html Kerberos Authentication> in the /Amazon RDS User Guide/ .
--
-- /Note:/ Consider using 'domain' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbifdbsDomain :: Lens.Lens' RestoreDBInstanceFromDBSnapshot (Core.Maybe Core.Text)
rdbifdbsDomain = Lens.field @"domain"
{-# INLINEABLE rdbifdbsDomain #-}
{-# DEPRECATED domain "Use generic-lens or generic-optics with 'domain' instead"  #-}

-- | Specify the name of the IAM role to be used when making API calls to the Directory Service.
--
-- /Note:/ Consider using 'domainIAMRoleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbifdbsDomainIAMRoleName :: Lens.Lens' RestoreDBInstanceFromDBSnapshot (Core.Maybe Core.Text)
rdbifdbsDomainIAMRoleName = Lens.field @"domainIAMRoleName"
{-# INLINEABLE rdbifdbsDomainIAMRoleName #-}
{-# DEPRECATED domainIAMRoleName "Use generic-lens or generic-optics with 'domainIAMRoleName' instead"  #-}

-- | The list of logs that the restored DB instance is to export to CloudWatch Logs. The values in the list depend on the DB engine being used. For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_LogAccess.html#USER_LogAccess.Procedural.UploadtoCloudWatch Publishing Database Logs to Amazon CloudWatch Logs> in the /Amazon RDS User Guide/ .
--
-- /Note:/ Consider using 'enableCloudwatchLogsExports' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbifdbsEnableCloudwatchLogsExports :: Lens.Lens' RestoreDBInstanceFromDBSnapshot (Core.Maybe [Core.Text])
rdbifdbsEnableCloudwatchLogsExports = Lens.field @"enableCloudwatchLogsExports"
{-# INLINEABLE rdbifdbsEnableCloudwatchLogsExports #-}
{-# DEPRECATED enableCloudwatchLogsExports "Use generic-lens or generic-optics with 'enableCloudwatchLogsExports' instead"  #-}

-- | A value that indicates whether to enable mapping of AWS Identity and Access Management (IAM) accounts to database accounts. By default, mapping is disabled.
--
-- For more information about IAM database authentication, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/UsingWithRDS.IAMDBAuth.html IAM Database Authentication for MySQL and PostgreSQL> in the /Amazon RDS User Guide./ 
--
-- /Note:/ Consider using 'enableIAMDatabaseAuthentication' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbifdbsEnableIAMDatabaseAuthentication :: Lens.Lens' RestoreDBInstanceFromDBSnapshot (Core.Maybe Core.Bool)
rdbifdbsEnableIAMDatabaseAuthentication = Lens.field @"enableIAMDatabaseAuthentication"
{-# INLINEABLE rdbifdbsEnableIAMDatabaseAuthentication #-}
{-# DEPRECATED enableIAMDatabaseAuthentication "Use generic-lens or generic-optics with 'enableIAMDatabaseAuthentication' instead"  #-}

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
rdbifdbsEngine :: Lens.Lens' RestoreDBInstanceFromDBSnapshot (Core.Maybe Core.Text)
rdbifdbsEngine = Lens.field @"engine"
{-# INLINEABLE rdbifdbsEngine #-}
{-# DEPRECATED engine "Use generic-lens or generic-optics with 'engine' instead"  #-}

-- | Specifies the amount of provisioned IOPS for the DB instance, expressed in I/O operations per second. If this parameter isn't specified, the IOPS value is taken from the backup. If this parameter is set to 0, the new instance is converted to a non-PIOPS instance. The conversion takes additional time, though your DB instance is available for connections before the conversion starts. 
--
-- The provisioned IOPS value must follow the requirements for your database engine. For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/CHAP_Storage.html#USER_PIOPS Amazon RDS Provisioned IOPS Storage to Improve Performance> in the /Amazon RDS User Guide./ 
-- Constraints: Must be an integer greater than 1000.
--
-- /Note:/ Consider using 'iops' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbifdbsIops :: Lens.Lens' RestoreDBInstanceFromDBSnapshot (Core.Maybe Core.Int)
rdbifdbsIops = Lens.field @"iops"
{-# INLINEABLE rdbifdbsIops #-}
{-# DEPRECATED iops "Use generic-lens or generic-optics with 'iops' instead"  #-}

-- | License model information for the restored DB instance.
--
-- Default: Same as source.
-- Valid values: @license-included@ | @bring-your-own-license@ | @general-public-license@ 
--
-- /Note:/ Consider using 'licenseModel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbifdbsLicenseModel :: Lens.Lens' RestoreDBInstanceFromDBSnapshot (Core.Maybe Core.Text)
rdbifdbsLicenseModel = Lens.field @"licenseModel"
{-# INLINEABLE rdbifdbsLicenseModel #-}
{-# DEPRECATED licenseModel "Use generic-lens or generic-optics with 'licenseModel' instead"  #-}

-- | A value that indicates whether the DB instance is a Multi-AZ deployment.
--
-- Constraint: You can't specify the @AvailabilityZone@ parameter if the DB instance is a Multi-AZ deployment.
--
-- /Note:/ Consider using 'multiAZ' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbifdbsMultiAZ :: Lens.Lens' RestoreDBInstanceFromDBSnapshot (Core.Maybe Core.Bool)
rdbifdbsMultiAZ = Lens.field @"multiAZ"
{-# INLINEABLE rdbifdbsMultiAZ #-}
{-# DEPRECATED multiAZ "Use generic-lens or generic-optics with 'multiAZ' instead"  #-}

-- | The name of the option group to be used for the restored DB instance.
--
-- Permanent options, such as the TDE option for Oracle Advanced Security TDE, can't be removed from an option group, and that option group can't be removed from a DB instance once it is associated with a DB instance
--
-- /Note:/ Consider using 'optionGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbifdbsOptionGroupName :: Lens.Lens' RestoreDBInstanceFromDBSnapshot (Core.Maybe Core.Text)
rdbifdbsOptionGroupName = Lens.field @"optionGroupName"
{-# INLINEABLE rdbifdbsOptionGroupName #-}
{-# DEPRECATED optionGroupName "Use generic-lens or generic-optics with 'optionGroupName' instead"  #-}

-- | The port number on which the database accepts connections.
--
-- Default: The same port as the original DB instance
-- Constraints: Value must be @1150-65535@ 
--
-- /Note:/ Consider using 'port' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbifdbsPort :: Lens.Lens' RestoreDBInstanceFromDBSnapshot (Core.Maybe Core.Int)
rdbifdbsPort = Lens.field @"port"
{-# INLINEABLE rdbifdbsPort #-}
{-# DEPRECATED port "Use generic-lens or generic-optics with 'port' instead"  #-}

-- | The number of CPU cores and the number of threads per core for the DB instance class of the DB instance.
--
-- /Note:/ Consider using 'processorFeatures' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbifdbsProcessorFeatures :: Lens.Lens' RestoreDBInstanceFromDBSnapshot (Core.Maybe [Types.ProcessorFeature])
rdbifdbsProcessorFeatures = Lens.field @"processorFeatures"
{-# INLINEABLE rdbifdbsProcessorFeatures #-}
{-# DEPRECATED processorFeatures "Use generic-lens or generic-optics with 'processorFeatures' instead"  #-}

-- | A value that indicates whether the DB instance is publicly accessible.
--
-- When the DB instance is publicly accessible, its DNS endpoint resolves to the private IP address from within the DB instance's VPC, and to the public IP address from outside of the DB instance's VPC. Access to the DB instance is ultimately controlled by the security group it uses, and that public access is not permitted if the security group assigned to the DB instance doesn't permit it.
-- When the DB instance isn't publicly accessible, it is an internal DB instance with a DNS name that resolves to a private IP address.
-- For more information, see 'CreateDBInstance' .
--
-- /Note:/ Consider using 'publiclyAccessible' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbifdbsPubliclyAccessible :: Lens.Lens' RestoreDBInstanceFromDBSnapshot (Core.Maybe Core.Bool)
rdbifdbsPubliclyAccessible = Lens.field @"publiclyAccessible"
{-# INLINEABLE rdbifdbsPubliclyAccessible #-}
{-# DEPRECATED publiclyAccessible "Use generic-lens or generic-optics with 'publiclyAccessible' instead"  #-}

-- | Specifies the storage type to be associated with the DB instance.
--
-- Valid values: @standard | gp2 | io1@ 
-- If you specify @io1@ , you must also include a value for the @Iops@ parameter. 
-- Default: @io1@ if the @Iops@ parameter is specified, otherwise @gp2@ 
--
-- /Note:/ Consider using 'storageType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbifdbsStorageType :: Lens.Lens' RestoreDBInstanceFromDBSnapshot (Core.Maybe Core.Text)
rdbifdbsStorageType = Lens.field @"storageType"
{-# INLINEABLE rdbifdbsStorageType #-}
{-# DEPRECATED storageType "Use generic-lens or generic-optics with 'storageType' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbifdbsTags :: Lens.Lens' RestoreDBInstanceFromDBSnapshot (Core.Maybe [Types.Tag])
rdbifdbsTags = Lens.field @"tags"
{-# INLINEABLE rdbifdbsTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

-- | The ARN from the key store with which to associate the instance for TDE encryption.
--
-- /Note:/ Consider using 'tdeCredentialArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbifdbsTdeCredentialArn :: Lens.Lens' RestoreDBInstanceFromDBSnapshot (Core.Maybe Core.Text)
rdbifdbsTdeCredentialArn = Lens.field @"tdeCredentialArn"
{-# INLINEABLE rdbifdbsTdeCredentialArn #-}
{-# DEPRECATED tdeCredentialArn "Use generic-lens or generic-optics with 'tdeCredentialArn' instead"  #-}

-- | The password for the given ARN from the key store in order to access the device.
--
-- /Note:/ Consider using 'tdeCredentialPassword' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbifdbsTdeCredentialPassword :: Lens.Lens' RestoreDBInstanceFromDBSnapshot (Core.Maybe Core.Text)
rdbifdbsTdeCredentialPassword = Lens.field @"tdeCredentialPassword"
{-# INLINEABLE rdbifdbsTdeCredentialPassword #-}
{-# DEPRECATED tdeCredentialPassword "Use generic-lens or generic-optics with 'tdeCredentialPassword' instead"  #-}

-- | A value that indicates whether the DB instance class of the DB instance uses its default processor features.
--
-- /Note:/ Consider using 'useDefaultProcessorFeatures' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbifdbsUseDefaultProcessorFeatures :: Lens.Lens' RestoreDBInstanceFromDBSnapshot (Core.Maybe Core.Bool)
rdbifdbsUseDefaultProcessorFeatures = Lens.field @"useDefaultProcessorFeatures"
{-# INLINEABLE rdbifdbsUseDefaultProcessorFeatures #-}
{-# DEPRECATED useDefaultProcessorFeatures "Use generic-lens or generic-optics with 'useDefaultProcessorFeatures' instead"  #-}

-- | A list of EC2 VPC security groups to associate with this DB instance. 
--
-- Default: The default EC2 VPC security group for the DB subnet group's VPC. 
--
-- /Note:/ Consider using 'vpcSecurityGroupIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbifdbsVpcSecurityGroupIds :: Lens.Lens' RestoreDBInstanceFromDBSnapshot (Core.Maybe [Core.Text])
rdbifdbsVpcSecurityGroupIds = Lens.field @"vpcSecurityGroupIds"
{-# INLINEABLE rdbifdbsVpcSecurityGroupIds #-}
{-# DEPRECATED vpcSecurityGroupIds "Use generic-lens or generic-optics with 'vpcSecurityGroupIds' instead"  #-}

instance Core.ToQuery RestoreDBInstanceFromDBSnapshot where
        toQuery RestoreDBInstanceFromDBSnapshot{..}
          = Core.toQueryPair "Action"
              ("RestoreDBInstanceFromDBSnapshot" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2014-10-31" :: Core.Text)
              Core.<>
              Core.toQueryPair "DBInstanceIdentifier" dBInstanceIdentifier
              Core.<>
              Core.toQueryPair "DBSnapshotIdentifier" dBSnapshotIdentifier
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "AutoMinorVersionUpgrade")
                autoMinorVersionUpgrade
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "AvailabilityZone")
                availabilityZone
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "CopyTagsToSnapshot")
                copyTagsToSnapshot
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "DBInstanceClass")
                dBInstanceClass
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DBName") dBName
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "DBParameterGroupName")
                dBParameterGroupName
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "DBSubnetGroupName")
                dBSubnetGroupName
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "DeletionProtection")
                deletionProtection
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "Domain") domain
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "DomainIAMRoleName")
                domainIAMRoleName
              Core.<>
              Core.toQueryPair "EnableCloudwatchLogsExports"
                (Core.maybe Core.mempty (Core.toQueryList "member")
                   enableCloudwatchLogsExports)
              Core.<>
              Core.maybe Core.mempty
                (Core.toQueryPair "EnableIAMDatabaseAuthentication")
                enableIAMDatabaseAuthentication
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "Engine") engine
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "Iops") iops
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "LicenseModel")
                licenseModel
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "MultiAZ") multiAZ
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "OptionGroupName")
                optionGroupName
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "Port") port
              Core.<>
              Core.toQueryPair "ProcessorFeatures"
                (Core.maybe Core.mempty (Core.toQueryList "ProcessorFeature")
                   processorFeatures)
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "PubliclyAccessible")
                publiclyAccessible
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "StorageType") storageType
              Core.<>
              Core.toQueryPair "Tags"
                (Core.maybe Core.mempty (Core.toQueryList "Tag") tags)
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "TdeCredentialArn")
                tdeCredentialArn
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "TdeCredentialPassword")
                tdeCredentialPassword
              Core.<>
              Core.maybe Core.mempty
                (Core.toQueryPair "UseDefaultProcessorFeatures")
                useDefaultProcessorFeatures
              Core.<>
              Core.toQueryPair "VpcSecurityGroupIds"
                (Core.maybe Core.mempty (Core.toQueryList "VpcSecurityGroupId")
                   vpcSecurityGroupIds)

instance Core.ToHeaders RestoreDBInstanceFromDBSnapshot where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest RestoreDBInstanceFromDBSnapshot where
        type Rs RestoreDBInstanceFromDBSnapshot =
             RestoreDBInstanceFromDBSnapshotResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXMLWrapper
              "RestoreDBInstanceFromDBSnapshotResult"
              (\ s h x ->
                 RestoreDBInstanceFromDBSnapshotResponse' Core.<$>
                   (x Core..@? "DBInstance") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkRestoreDBInstanceFromDBSnapshotResponse' smart constructor.
data RestoreDBInstanceFromDBSnapshotResponse = RestoreDBInstanceFromDBSnapshotResponse'
  { dBInstance :: Core.Maybe Types.DBInstance
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'RestoreDBInstanceFromDBSnapshotResponse' value with any optional fields omitted.
mkRestoreDBInstanceFromDBSnapshotResponse
    :: Core.Int -- ^ 'responseStatus'
    -> RestoreDBInstanceFromDBSnapshotResponse
mkRestoreDBInstanceFromDBSnapshotResponse responseStatus
  = RestoreDBInstanceFromDBSnapshotResponse'{dBInstance =
                                               Core.Nothing,
                                             responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'dBInstance' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbifdbsrrsDBInstance :: Lens.Lens' RestoreDBInstanceFromDBSnapshotResponse (Core.Maybe Types.DBInstance)
rdbifdbsrrsDBInstance = Lens.field @"dBInstance"
{-# INLINEABLE rdbifdbsrrsDBInstance #-}
{-# DEPRECATED dBInstance "Use generic-lens or generic-optics with 'dBInstance' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbifdbsrrsResponseStatus :: Lens.Lens' RestoreDBInstanceFromDBSnapshotResponse Core.Int
rdbifdbsrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE rdbifdbsrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
