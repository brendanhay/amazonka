{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      RestoreDBInstanceToPointInTime (..)
    , mkRestoreDBInstanceToPointInTime
    -- ** Request lenses
    , rdbitpitTargetDBInstanceIdentifier
    , rdbitpitAutoMinorVersionUpgrade
    , rdbitpitAvailabilityZone
    , rdbitpitCopyTagsToSnapshot
    , rdbitpitDBInstanceClass
    , rdbitpitDBName
    , rdbitpitDBParameterGroupName
    , rdbitpitDBSubnetGroupName
    , rdbitpitDeletionProtection
    , rdbitpitDomain
    , rdbitpitDomainIAMRoleName
    , rdbitpitEnableCloudwatchLogsExports
    , rdbitpitEnableIAMDatabaseAuthentication
    , rdbitpitEngine
    , rdbitpitIops
    , rdbitpitLicenseModel
    , rdbitpitMaxAllocatedStorage
    , rdbitpitMultiAZ
    , rdbitpitOptionGroupName
    , rdbitpitPort
    , rdbitpitProcessorFeatures
    , rdbitpitPubliclyAccessible
    , rdbitpitRestoreTime
    , rdbitpitSourceDBInstanceIdentifier
    , rdbitpitSourceDbiResourceId
    , rdbitpitStorageType
    , rdbitpitTags
    , rdbitpitTdeCredentialArn
    , rdbitpitTdeCredentialPassword
    , rdbitpitUseDefaultProcessorFeatures
    , rdbitpitUseLatestRestorableTime
    , rdbitpitVpcSecurityGroupIds

    -- * Destructuring the response
    , RestoreDBInstanceToPointInTimeResponse (..)
    , mkRestoreDBInstanceToPointInTimeResponse
    -- ** Response lenses
    , rdbitpitrrsDBInstance
    , rdbitpitrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | 
--
-- /See:/ 'mkRestoreDBInstanceToPointInTime' smart constructor.
data RestoreDBInstanceToPointInTime = RestoreDBInstanceToPointInTime'
  { targetDBInstanceIdentifier :: Core.Text
    -- ^ The name of the new DB instance to be created.
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
  , iops :: Core.Maybe Core.Int
    -- ^ The amount of Provisioned IOPS (input/output operations per second) to be initially allocated for the DB instance.
--
-- Constraints: Must be an integer greater than 1000.
-- __SQL Server__ 
-- Setting the IOPS value for the SQL Server database engine isn't supported.
  , licenseModel :: Core.Maybe Core.Text
    -- ^ License model information for the restored DB instance.
--
-- Default: Same as source.
-- Valid values: @license-included@ | @bring-your-own-license@ | @general-public-license@ 
  , maxAllocatedStorage :: Core.Maybe Core.Int
    -- ^ The upper limit to which Amazon RDS can automatically scale the storage of the DB instance.
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
-- Constraints: Value must be @1150-65535@ 
-- Default: The same port as the original DB instance.
  , processorFeatures :: Core.Maybe [Types.ProcessorFeature]
    -- ^ The number of CPU cores and the number of threads per core for the DB instance class of the DB instance.
  , publiclyAccessible :: Core.Maybe Core.Bool
    -- ^ A value that indicates whether the DB instance is publicly accessible.
--
-- When the DB instance is publicly accessible, its DNS endpoint resolves to the private IP address from within the DB instance's VPC, and to the public IP address from outside of the DB instance's VPC. Access to the DB instance is ultimately controlled by the security group it uses, and that public access is not permitted if the security group assigned to the DB instance doesn't permit it.
-- When the DB instance isn't publicly accessible, it is an internal DB instance with a DNS name that resolves to a private IP address.
-- For more information, see 'CreateDBInstance' .
  , restoreTime :: Core.Maybe Core.UTCTime
    -- ^ The date and time to restore from.
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
  , sourceDBInstanceIdentifier :: Core.Maybe Core.Text
    -- ^ The identifier of the source DB instance from which to restore.
--
-- Constraints:
--
--     * Must match the identifier of an existing DB instance.
--
--
  , sourceDbiResourceId :: Core.Maybe Core.Text
    -- ^ The resource ID of the source DB instance from which to restore.
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
  , useLatestRestorableTime :: Core.Maybe Core.Bool
    -- ^ A value that indicates whether the DB instance is restored from the latest backup time. By default, the DB instance isn't restored from the latest backup time. 
--
-- Constraints: Can't be specified if the @RestoreTime@ parameter is provided.
  , vpcSecurityGroupIds :: Core.Maybe [Core.Text]
    -- ^ A list of EC2 VPC security groups to associate with this DB instance. 
--
-- Default: The default EC2 VPC security group for the DB subnet group's VPC. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'RestoreDBInstanceToPointInTime' value with any optional fields omitted.
mkRestoreDBInstanceToPointInTime
    :: Core.Text -- ^ 'targetDBInstanceIdentifier'
    -> RestoreDBInstanceToPointInTime
mkRestoreDBInstanceToPointInTime targetDBInstanceIdentifier
  = RestoreDBInstanceToPointInTime'{targetDBInstanceIdentifier,
                                    autoMinorVersionUpgrade = Core.Nothing,
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
                                    licenseModel = Core.Nothing, maxAllocatedStorage = Core.Nothing,
                                    multiAZ = Core.Nothing, optionGroupName = Core.Nothing,
                                    port = Core.Nothing, processorFeatures = Core.Nothing,
                                    publiclyAccessible = Core.Nothing, restoreTime = Core.Nothing,
                                    sourceDBInstanceIdentifier = Core.Nothing,
                                    sourceDbiResourceId = Core.Nothing, storageType = Core.Nothing,
                                    tags = Core.Nothing, tdeCredentialArn = Core.Nothing,
                                    tdeCredentialPassword = Core.Nothing,
                                    useDefaultProcessorFeatures = Core.Nothing,
                                    useLatestRestorableTime = Core.Nothing,
                                    vpcSecurityGroupIds = Core.Nothing}

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
rdbitpitTargetDBInstanceIdentifier :: Lens.Lens' RestoreDBInstanceToPointInTime Core.Text
rdbitpitTargetDBInstanceIdentifier = Lens.field @"targetDBInstanceIdentifier"
{-# INLINEABLE rdbitpitTargetDBInstanceIdentifier #-}
{-# DEPRECATED targetDBInstanceIdentifier "Use generic-lens or generic-optics with 'targetDBInstanceIdentifier' instead"  #-}

-- | A value that indicates whether minor version upgrades are applied automatically to the DB instance during the maintenance window.
--
-- /Note:/ Consider using 'autoMinorVersionUpgrade' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbitpitAutoMinorVersionUpgrade :: Lens.Lens' RestoreDBInstanceToPointInTime (Core.Maybe Core.Bool)
rdbitpitAutoMinorVersionUpgrade = Lens.field @"autoMinorVersionUpgrade"
{-# INLINEABLE rdbitpitAutoMinorVersionUpgrade #-}
{-# DEPRECATED autoMinorVersionUpgrade "Use generic-lens or generic-optics with 'autoMinorVersionUpgrade' instead"  #-}

-- | The Availability Zone (AZ) where the DB instance will be created.
--
-- Default: A random, system-chosen Availability Zone.
-- Constraint: You can't specify the @AvailabilityZone@ parameter if the DB instance is a Multi-AZ deployment.
-- Example: @us-east-1a@ 
--
-- /Note:/ Consider using 'availabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbitpitAvailabilityZone :: Lens.Lens' RestoreDBInstanceToPointInTime (Core.Maybe Core.Text)
rdbitpitAvailabilityZone = Lens.field @"availabilityZone"
{-# INLINEABLE rdbitpitAvailabilityZone #-}
{-# DEPRECATED availabilityZone "Use generic-lens or generic-optics with 'availabilityZone' instead"  #-}

-- | A value that indicates whether to copy all tags from the restored DB instance to snapshots of the DB instance. By default, tags are not copied.
--
-- /Note:/ Consider using 'copyTagsToSnapshot' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbitpitCopyTagsToSnapshot :: Lens.Lens' RestoreDBInstanceToPointInTime (Core.Maybe Core.Bool)
rdbitpitCopyTagsToSnapshot = Lens.field @"copyTagsToSnapshot"
{-# INLINEABLE rdbitpitCopyTagsToSnapshot #-}
{-# DEPRECATED copyTagsToSnapshot "Use generic-lens or generic-optics with 'copyTagsToSnapshot' instead"  #-}

-- | The compute and memory capacity of the Amazon RDS DB instance, for example, @db.m4.large@ . Not all DB instance classes are available in all AWS Regions, or for all database engines. For the full list of DB instance classes, and availability for your engine, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Concepts.DBInstanceClass.html DB Instance Class> in the /Amazon RDS User Guide./ 
--
-- Default: The same DBInstanceClass as the original DB instance.
--
-- /Note:/ Consider using 'dBInstanceClass' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbitpitDBInstanceClass :: Lens.Lens' RestoreDBInstanceToPointInTime (Core.Maybe Core.Text)
rdbitpitDBInstanceClass = Lens.field @"dBInstanceClass"
{-# INLINEABLE rdbitpitDBInstanceClass #-}
{-# DEPRECATED dBInstanceClass "Use generic-lens or generic-optics with 'dBInstanceClass' instead"  #-}

-- | The database name for the restored DB instance.
--
-- /Note:/ Consider using 'dBName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbitpitDBName :: Lens.Lens' RestoreDBInstanceToPointInTime (Core.Maybe Core.Text)
rdbitpitDBName = Lens.field @"dBName"
{-# INLINEABLE rdbitpitDBName #-}
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
rdbitpitDBParameterGroupName :: Lens.Lens' RestoreDBInstanceToPointInTime (Core.Maybe Core.Text)
rdbitpitDBParameterGroupName = Lens.field @"dBParameterGroupName"
{-# INLINEABLE rdbitpitDBParameterGroupName #-}
{-# DEPRECATED dBParameterGroupName "Use generic-lens or generic-optics with 'dBParameterGroupName' instead"  #-}

-- | The DB subnet group name to use for the new instance.
--
-- Constraints: If supplied, must match the name of an existing DBSubnetGroup.
-- Example: @mySubnetgroup@ 
--
-- /Note:/ Consider using 'dBSubnetGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbitpitDBSubnetGroupName :: Lens.Lens' RestoreDBInstanceToPointInTime (Core.Maybe Core.Text)
rdbitpitDBSubnetGroupName = Lens.field @"dBSubnetGroupName"
{-# INLINEABLE rdbitpitDBSubnetGroupName #-}
{-# DEPRECATED dBSubnetGroupName "Use generic-lens or generic-optics with 'dBSubnetGroupName' instead"  #-}

-- | A value that indicates whether the DB instance has deletion protection enabled. The database can't be deleted when deletion protection is enabled. By default, deletion protection is disabled. For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_DeleteInstance.html Deleting a DB Instance> . 
--
-- /Note:/ Consider using 'deletionProtection' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbitpitDeletionProtection :: Lens.Lens' RestoreDBInstanceToPointInTime (Core.Maybe Core.Bool)
rdbitpitDeletionProtection = Lens.field @"deletionProtection"
{-# INLINEABLE rdbitpitDeletionProtection #-}
{-# DEPRECATED deletionProtection "Use generic-lens or generic-optics with 'deletionProtection' instead"  #-}

-- | Specify the Active Directory directory ID to restore the DB instance in. The domain must be created prior to this operation. Currently, only MySQL, Microsoft SQL Server, Oracle, and PostgreSQL DB instances can be created in an Active Directory Domain.
--
-- For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/kerberos-authentication.html Kerberos Authentication> in the /Amazon RDS User Guide/ .
--
-- /Note:/ Consider using 'domain' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbitpitDomain :: Lens.Lens' RestoreDBInstanceToPointInTime (Core.Maybe Core.Text)
rdbitpitDomain = Lens.field @"domain"
{-# INLINEABLE rdbitpitDomain #-}
{-# DEPRECATED domain "Use generic-lens or generic-optics with 'domain' instead"  #-}

-- | Specify the name of the IAM role to be used when making API calls to the Directory Service.
--
-- /Note:/ Consider using 'domainIAMRoleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbitpitDomainIAMRoleName :: Lens.Lens' RestoreDBInstanceToPointInTime (Core.Maybe Core.Text)
rdbitpitDomainIAMRoleName = Lens.field @"domainIAMRoleName"
{-# INLINEABLE rdbitpitDomainIAMRoleName #-}
{-# DEPRECATED domainIAMRoleName "Use generic-lens or generic-optics with 'domainIAMRoleName' instead"  #-}

-- | The list of logs that the restored DB instance is to export to CloudWatch Logs. The values in the list depend on the DB engine being used. For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_LogAccess.html#USER_LogAccess.Procedural.UploadtoCloudWatch Publishing Database Logs to Amazon CloudWatch Logs> in the /Amazon RDS User Guide/ .
--
-- /Note:/ Consider using 'enableCloudwatchLogsExports' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbitpitEnableCloudwatchLogsExports :: Lens.Lens' RestoreDBInstanceToPointInTime (Core.Maybe [Core.Text])
rdbitpitEnableCloudwatchLogsExports = Lens.field @"enableCloudwatchLogsExports"
{-# INLINEABLE rdbitpitEnableCloudwatchLogsExports #-}
{-# DEPRECATED enableCloudwatchLogsExports "Use generic-lens or generic-optics with 'enableCloudwatchLogsExports' instead"  #-}

-- | A value that indicates whether to enable mapping of AWS Identity and Access Management (IAM) accounts to database accounts. By default, mapping is disabled.
--
-- For more information about IAM database authentication, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/UsingWithRDS.IAMDBAuth.html IAM Database Authentication for MySQL and PostgreSQL> in the /Amazon RDS User Guide./ 
--
-- /Note:/ Consider using 'enableIAMDatabaseAuthentication' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbitpitEnableIAMDatabaseAuthentication :: Lens.Lens' RestoreDBInstanceToPointInTime (Core.Maybe Core.Bool)
rdbitpitEnableIAMDatabaseAuthentication = Lens.field @"enableIAMDatabaseAuthentication"
{-# INLINEABLE rdbitpitEnableIAMDatabaseAuthentication #-}
{-# DEPRECATED enableIAMDatabaseAuthentication "Use generic-lens or generic-optics with 'enableIAMDatabaseAuthentication' instead"  #-}

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
rdbitpitEngine :: Lens.Lens' RestoreDBInstanceToPointInTime (Core.Maybe Core.Text)
rdbitpitEngine = Lens.field @"engine"
{-# INLINEABLE rdbitpitEngine #-}
{-# DEPRECATED engine "Use generic-lens or generic-optics with 'engine' instead"  #-}

-- | The amount of Provisioned IOPS (input/output operations per second) to be initially allocated for the DB instance.
--
-- Constraints: Must be an integer greater than 1000.
-- __SQL Server__ 
-- Setting the IOPS value for the SQL Server database engine isn't supported.
--
-- /Note:/ Consider using 'iops' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbitpitIops :: Lens.Lens' RestoreDBInstanceToPointInTime (Core.Maybe Core.Int)
rdbitpitIops = Lens.field @"iops"
{-# INLINEABLE rdbitpitIops #-}
{-# DEPRECATED iops "Use generic-lens or generic-optics with 'iops' instead"  #-}

-- | License model information for the restored DB instance.
--
-- Default: Same as source.
-- Valid values: @license-included@ | @bring-your-own-license@ | @general-public-license@ 
--
-- /Note:/ Consider using 'licenseModel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbitpitLicenseModel :: Lens.Lens' RestoreDBInstanceToPointInTime (Core.Maybe Core.Text)
rdbitpitLicenseModel = Lens.field @"licenseModel"
{-# INLINEABLE rdbitpitLicenseModel #-}
{-# DEPRECATED licenseModel "Use generic-lens or generic-optics with 'licenseModel' instead"  #-}

-- | The upper limit to which Amazon RDS can automatically scale the storage of the DB instance.
--
-- /Note:/ Consider using 'maxAllocatedStorage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbitpitMaxAllocatedStorage :: Lens.Lens' RestoreDBInstanceToPointInTime (Core.Maybe Core.Int)
rdbitpitMaxAllocatedStorage = Lens.field @"maxAllocatedStorage"
{-# INLINEABLE rdbitpitMaxAllocatedStorage #-}
{-# DEPRECATED maxAllocatedStorage "Use generic-lens or generic-optics with 'maxAllocatedStorage' instead"  #-}

-- | A value that indicates whether the DB instance is a Multi-AZ deployment.
--
-- Constraint: You can't specify the @AvailabilityZone@ parameter if the DB instance is a Multi-AZ deployment.
--
-- /Note:/ Consider using 'multiAZ' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbitpitMultiAZ :: Lens.Lens' RestoreDBInstanceToPointInTime (Core.Maybe Core.Bool)
rdbitpitMultiAZ = Lens.field @"multiAZ"
{-# INLINEABLE rdbitpitMultiAZ #-}
{-# DEPRECATED multiAZ "Use generic-lens or generic-optics with 'multiAZ' instead"  #-}

-- | The name of the option group to be used for the restored DB instance.
--
-- Permanent options, such as the TDE option for Oracle Advanced Security TDE, can't be removed from an option group, and that option group can't be removed from a DB instance once it is associated with a DB instance
--
-- /Note:/ Consider using 'optionGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbitpitOptionGroupName :: Lens.Lens' RestoreDBInstanceToPointInTime (Core.Maybe Core.Text)
rdbitpitOptionGroupName = Lens.field @"optionGroupName"
{-# INLINEABLE rdbitpitOptionGroupName #-}
{-# DEPRECATED optionGroupName "Use generic-lens or generic-optics with 'optionGroupName' instead"  #-}

-- | The port number on which the database accepts connections.
--
-- Constraints: Value must be @1150-65535@ 
-- Default: The same port as the original DB instance.
--
-- /Note:/ Consider using 'port' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbitpitPort :: Lens.Lens' RestoreDBInstanceToPointInTime (Core.Maybe Core.Int)
rdbitpitPort = Lens.field @"port"
{-# INLINEABLE rdbitpitPort #-}
{-# DEPRECATED port "Use generic-lens or generic-optics with 'port' instead"  #-}

-- | The number of CPU cores and the number of threads per core for the DB instance class of the DB instance.
--
-- /Note:/ Consider using 'processorFeatures' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbitpitProcessorFeatures :: Lens.Lens' RestoreDBInstanceToPointInTime (Core.Maybe [Types.ProcessorFeature])
rdbitpitProcessorFeatures = Lens.field @"processorFeatures"
{-# INLINEABLE rdbitpitProcessorFeatures #-}
{-# DEPRECATED processorFeatures "Use generic-lens or generic-optics with 'processorFeatures' instead"  #-}

-- | A value that indicates whether the DB instance is publicly accessible.
--
-- When the DB instance is publicly accessible, its DNS endpoint resolves to the private IP address from within the DB instance's VPC, and to the public IP address from outside of the DB instance's VPC. Access to the DB instance is ultimately controlled by the security group it uses, and that public access is not permitted if the security group assigned to the DB instance doesn't permit it.
-- When the DB instance isn't publicly accessible, it is an internal DB instance with a DNS name that resolves to a private IP address.
-- For more information, see 'CreateDBInstance' .
--
-- /Note:/ Consider using 'publiclyAccessible' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbitpitPubliclyAccessible :: Lens.Lens' RestoreDBInstanceToPointInTime (Core.Maybe Core.Bool)
rdbitpitPubliclyAccessible = Lens.field @"publiclyAccessible"
{-# INLINEABLE rdbitpitPubliclyAccessible #-}
{-# DEPRECATED publiclyAccessible "Use generic-lens or generic-optics with 'publiclyAccessible' instead"  #-}

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
{-# INLINEABLE rdbitpitRestoreTime #-}
{-# DEPRECATED restoreTime "Use generic-lens or generic-optics with 'restoreTime' instead"  #-}

-- | The identifier of the source DB instance from which to restore.
--
-- Constraints:
--
--     * Must match the identifier of an existing DB instance.
--
--
--
-- /Note:/ Consider using 'sourceDBInstanceIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbitpitSourceDBInstanceIdentifier :: Lens.Lens' RestoreDBInstanceToPointInTime (Core.Maybe Core.Text)
rdbitpitSourceDBInstanceIdentifier = Lens.field @"sourceDBInstanceIdentifier"
{-# INLINEABLE rdbitpitSourceDBInstanceIdentifier #-}
{-# DEPRECATED sourceDBInstanceIdentifier "Use generic-lens or generic-optics with 'sourceDBInstanceIdentifier' instead"  #-}

-- | The resource ID of the source DB instance from which to restore.
--
-- /Note:/ Consider using 'sourceDbiResourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbitpitSourceDbiResourceId :: Lens.Lens' RestoreDBInstanceToPointInTime (Core.Maybe Core.Text)
rdbitpitSourceDbiResourceId = Lens.field @"sourceDbiResourceId"
{-# INLINEABLE rdbitpitSourceDbiResourceId #-}
{-# DEPRECATED sourceDbiResourceId "Use generic-lens or generic-optics with 'sourceDbiResourceId' instead"  #-}

-- | Specifies the storage type to be associated with the DB instance.
--
-- Valid values: @standard | gp2 | io1@ 
-- If you specify @io1@ , you must also include a value for the @Iops@ parameter. 
-- Default: @io1@ if the @Iops@ parameter is specified, otherwise @gp2@ 
--
-- /Note:/ Consider using 'storageType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbitpitStorageType :: Lens.Lens' RestoreDBInstanceToPointInTime (Core.Maybe Core.Text)
rdbitpitStorageType = Lens.field @"storageType"
{-# INLINEABLE rdbitpitStorageType #-}
{-# DEPRECATED storageType "Use generic-lens or generic-optics with 'storageType' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbitpitTags :: Lens.Lens' RestoreDBInstanceToPointInTime (Core.Maybe [Types.Tag])
rdbitpitTags = Lens.field @"tags"
{-# INLINEABLE rdbitpitTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

-- | The ARN from the key store with which to associate the instance for TDE encryption.
--
-- /Note:/ Consider using 'tdeCredentialArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbitpitTdeCredentialArn :: Lens.Lens' RestoreDBInstanceToPointInTime (Core.Maybe Core.Text)
rdbitpitTdeCredentialArn = Lens.field @"tdeCredentialArn"
{-# INLINEABLE rdbitpitTdeCredentialArn #-}
{-# DEPRECATED tdeCredentialArn "Use generic-lens or generic-optics with 'tdeCredentialArn' instead"  #-}

-- | The password for the given ARN from the key store in order to access the device.
--
-- /Note:/ Consider using 'tdeCredentialPassword' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbitpitTdeCredentialPassword :: Lens.Lens' RestoreDBInstanceToPointInTime (Core.Maybe Core.Text)
rdbitpitTdeCredentialPassword = Lens.field @"tdeCredentialPassword"
{-# INLINEABLE rdbitpitTdeCredentialPassword #-}
{-# DEPRECATED tdeCredentialPassword "Use generic-lens or generic-optics with 'tdeCredentialPassword' instead"  #-}

-- | A value that indicates whether the DB instance class of the DB instance uses its default processor features.
--
-- /Note:/ Consider using 'useDefaultProcessorFeatures' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbitpitUseDefaultProcessorFeatures :: Lens.Lens' RestoreDBInstanceToPointInTime (Core.Maybe Core.Bool)
rdbitpitUseDefaultProcessorFeatures = Lens.field @"useDefaultProcessorFeatures"
{-# INLINEABLE rdbitpitUseDefaultProcessorFeatures #-}
{-# DEPRECATED useDefaultProcessorFeatures "Use generic-lens or generic-optics with 'useDefaultProcessorFeatures' instead"  #-}

-- | A value that indicates whether the DB instance is restored from the latest backup time. By default, the DB instance isn't restored from the latest backup time. 
--
-- Constraints: Can't be specified if the @RestoreTime@ parameter is provided.
--
-- /Note:/ Consider using 'useLatestRestorableTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbitpitUseLatestRestorableTime :: Lens.Lens' RestoreDBInstanceToPointInTime (Core.Maybe Core.Bool)
rdbitpitUseLatestRestorableTime = Lens.field @"useLatestRestorableTime"
{-# INLINEABLE rdbitpitUseLatestRestorableTime #-}
{-# DEPRECATED useLatestRestorableTime "Use generic-lens or generic-optics with 'useLatestRestorableTime' instead"  #-}

-- | A list of EC2 VPC security groups to associate with this DB instance. 
--
-- Default: The default EC2 VPC security group for the DB subnet group's VPC. 
--
-- /Note:/ Consider using 'vpcSecurityGroupIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbitpitVpcSecurityGroupIds :: Lens.Lens' RestoreDBInstanceToPointInTime (Core.Maybe [Core.Text])
rdbitpitVpcSecurityGroupIds = Lens.field @"vpcSecurityGroupIds"
{-# INLINEABLE rdbitpitVpcSecurityGroupIds #-}
{-# DEPRECATED vpcSecurityGroupIds "Use generic-lens or generic-optics with 'vpcSecurityGroupIds' instead"  #-}

instance Core.ToQuery RestoreDBInstanceToPointInTime where
        toQuery RestoreDBInstanceToPointInTime{..}
          = Core.toQueryPair "Action"
              ("RestoreDBInstanceToPointInTime" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2014-10-31" :: Core.Text)
              Core.<>
              Core.toQueryPair "TargetDBInstanceIdentifier"
                targetDBInstanceIdentifier
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
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "MaxAllocatedStorage")
                maxAllocatedStorage
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
              Core.maybe Core.mempty (Core.toQueryPair "RestoreTime") restoreTime
              Core.<>
              Core.maybe Core.mempty
                (Core.toQueryPair "SourceDBInstanceIdentifier")
                sourceDBInstanceIdentifier
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "SourceDbiResourceId")
                sourceDbiResourceId
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
              Core.maybe Core.mempty (Core.toQueryPair "UseLatestRestorableTime")
                useLatestRestorableTime
              Core.<>
              Core.toQueryPair "VpcSecurityGroupIds"
                (Core.maybe Core.mempty (Core.toQueryList "VpcSecurityGroupId")
                   vpcSecurityGroupIds)

instance Core.ToHeaders RestoreDBInstanceToPointInTime where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest RestoreDBInstanceToPointInTime where
        type Rs RestoreDBInstanceToPointInTime =
             RestoreDBInstanceToPointInTimeResponse
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
          = Response.receiveXMLWrapper "RestoreDBInstanceToPointInTimeResult"
              (\ s h x ->
                 RestoreDBInstanceToPointInTimeResponse' Core.<$>
                   (x Core..@? "DBInstance") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkRestoreDBInstanceToPointInTimeResponse' smart constructor.
data RestoreDBInstanceToPointInTimeResponse = RestoreDBInstanceToPointInTimeResponse'
  { dBInstance :: Core.Maybe Types.DBInstance
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'RestoreDBInstanceToPointInTimeResponse' value with any optional fields omitted.
mkRestoreDBInstanceToPointInTimeResponse
    :: Core.Int -- ^ 'responseStatus'
    -> RestoreDBInstanceToPointInTimeResponse
mkRestoreDBInstanceToPointInTimeResponse responseStatus
  = RestoreDBInstanceToPointInTimeResponse'{dBInstance =
                                              Core.Nothing,
                                            responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'dBInstance' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbitpitrrsDBInstance :: Lens.Lens' RestoreDBInstanceToPointInTimeResponse (Core.Maybe Types.DBInstance)
rdbitpitrrsDBInstance = Lens.field @"dBInstance"
{-# INLINEABLE rdbitpitrrsDBInstance #-}
{-# DEPRECATED dBInstance "Use generic-lens or generic-optics with 'dBInstance' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbitpitrrsResponseStatus :: Lens.Lens' RestoreDBInstanceToPointInTimeResponse Core.Int
rdbitpitrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE rdbitpitrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
