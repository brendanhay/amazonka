{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.RestoreDBInstanceToPointInTime
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Restores a DB instance to an arbitrary point in time. You can restore to
-- any point in time before the time identified by the LatestRestorableTime
-- property. You can restore to a point up to the number of days specified
-- by the BackupRetentionPeriod property.
--
-- The target database is created with most of the original configuration,
-- but in a system-selected Availability Zone, with the default security
-- group, the default subnet group, and the default DB parameter group. By
-- default, the new DB instance is created as a single-AZ deployment except
-- when the instance is a SQL Server instance that has an option group that
-- is associated with mirroring; in this case, the instance becomes a
-- mirrored deployment and not a single-AZ deployment.
--
-- This command doesn\'t apply to Aurora MySQL and Aurora PostgreSQL. For
-- Aurora, use @RestoreDBClusterToPointInTime@.
module Network.AWS.RDS.RestoreDBInstanceToPointInTime
  ( -- * Creating a Request
    RestoreDBInstanceToPointInTime (..),
    newRestoreDBInstanceToPointInTime,

    -- * Request Lenses
    restoreDBInstanceToPointInTime_deletionProtection,
    restoreDBInstanceToPointInTime_useLatestRestorableTime,
    restoreDBInstanceToPointInTime_publiclyAccessible,
    restoreDBInstanceToPointInTime_autoMinorVersionUpgrade,
    restoreDBInstanceToPointInTime_dbSubnetGroupName,
    restoreDBInstanceToPointInTime_restoreTime,
    restoreDBInstanceToPointInTime_iops,
    restoreDBInstanceToPointInTime_domain,
    restoreDBInstanceToPointInTime_enableCustomerOwnedIp,
    restoreDBInstanceToPointInTime_engine,
    restoreDBInstanceToPointInTime_tdeCredentialPassword,
    restoreDBInstanceToPointInTime_sourceDBInstanceIdentifier,
    restoreDBInstanceToPointInTime_processorFeatures,
    restoreDBInstanceToPointInTime_dbInstanceClass,
    restoreDBInstanceToPointInTime_licenseModel,
    restoreDBInstanceToPointInTime_sourceDBInstanceAutomatedBackupsArn,
    restoreDBInstanceToPointInTime_maxAllocatedStorage,
    restoreDBInstanceToPointInTime_dbParameterGroupName,
    restoreDBInstanceToPointInTime_availabilityZone,
    restoreDBInstanceToPointInTime_vpcSecurityGroupIds,
    restoreDBInstanceToPointInTime_multiAZ,
    restoreDBInstanceToPointInTime_sourceDbiResourceId,
    restoreDBInstanceToPointInTime_optionGroupName,
    restoreDBInstanceToPointInTime_copyTagsToSnapshot,
    restoreDBInstanceToPointInTime_tdeCredentialArn,
    restoreDBInstanceToPointInTime_domainIAMRoleName,
    restoreDBInstanceToPointInTime_tags,
    restoreDBInstanceToPointInTime_port,
    restoreDBInstanceToPointInTime_enableIAMDatabaseAuthentication,
    restoreDBInstanceToPointInTime_useDefaultProcessorFeatures,
    restoreDBInstanceToPointInTime_storageType,
    restoreDBInstanceToPointInTime_enableCloudwatchLogsExports,
    restoreDBInstanceToPointInTime_dbName,
    restoreDBInstanceToPointInTime_targetDBInstanceIdentifier,

    -- * Destructuring the Response
    RestoreDBInstanceToPointInTimeResponse (..),
    newRestoreDBInstanceToPointInTimeResponse,

    -- * Response Lenses
    restoreDBInstanceToPointInTimeResponse_dbInstance,
    restoreDBInstanceToPointInTimeResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.RDS.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'newRestoreDBInstanceToPointInTime' smart constructor.
data RestoreDBInstanceToPointInTime = RestoreDBInstanceToPointInTime'
  { -- | A value that indicates whether the DB instance has deletion protection
    -- enabled. The database can\'t be deleted when deletion protection is
    -- enabled. By default, deletion protection is disabled. For more
    -- information, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_DeleteInstance.html Deleting a DB Instance>.
    deletionProtection :: Prelude.Maybe Prelude.Bool,
    -- | A value that indicates whether the DB instance is restored from the
    -- latest backup time. By default, the DB instance isn\'t restored from the
    -- latest backup time.
    --
    -- Constraints: Can\'t be specified if the @RestoreTime@ parameter is
    -- provided.
    useLatestRestorableTime :: Prelude.Maybe Prelude.Bool,
    -- | A value that indicates whether the DB instance is publicly accessible.
    --
    -- When the DB instance is publicly accessible, its DNS endpoint resolves
    -- to the private IP address from within the DB instance\'s VPC, and to the
    -- public IP address from outside of the DB instance\'s VPC. Access to the
    -- DB instance is ultimately controlled by the security group it uses, and
    -- that public access is not permitted if the security group assigned to
    -- the DB instance doesn\'t permit it.
    --
    -- When the DB instance isn\'t publicly accessible, it is an internal DB
    -- instance with a DNS name that resolves to a private IP address.
    --
    -- For more information, see CreateDBInstance.
    publiclyAccessible :: Prelude.Maybe Prelude.Bool,
    -- | A value that indicates whether minor version upgrades are applied
    -- automatically to the DB instance during the maintenance window.
    autoMinorVersionUpgrade :: Prelude.Maybe Prelude.Bool,
    -- | The DB subnet group name to use for the new instance.
    --
    -- Constraints: If supplied, must match the name of an existing
    -- DBSubnetGroup.
    --
    -- Example: @mySubnetgroup@
    dbSubnetGroupName :: Prelude.Maybe Prelude.Text,
    -- | The date and time to restore from.
    --
    -- Valid Values: Value must be a time in Universal Coordinated Time (UTC)
    -- format
    --
    -- Constraints:
    --
    -- -   Must be before the latest restorable time for the DB instance
    --
    -- -   Can\'t be specified if the @UseLatestRestorableTime@ parameter is
    --     enabled
    --
    -- Example: @2009-09-07T23:45:00Z@
    restoreTime :: Prelude.Maybe Core.ISO8601,
    -- | The amount of Provisioned IOPS (input\/output operations per second) to
    -- be initially allocated for the DB instance.
    --
    -- Constraints: Must be an integer greater than 1000.
    --
    -- __SQL Server__
    --
    -- Setting the IOPS value for the SQL Server database engine isn\'t
    -- supported.
    iops :: Prelude.Maybe Prelude.Int,
    -- | Specify the Active Directory directory ID to restore the DB instance in.
    -- The domain must be created prior to this operation. Currently, only
    -- MySQL, Microsoft SQL Server, Oracle, and PostgreSQL DB instances can be
    -- created in an Active Directory Domain.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/kerberos-authentication.html Kerberos Authentication>
    -- in the /Amazon RDS User Guide/.
    domain :: Prelude.Maybe Prelude.Text,
    -- | A value that indicates whether to enable a customer-owned IP address
    -- (CoIP) for an RDS on Outposts DB instance.
    --
    -- A /CoIP/ provides local or external connectivity to resources in your
    -- Outpost subnets through your on-premises network. For some use cases, a
    -- CoIP can provide lower latency for connections to the DB instance from
    -- outside of its virtual private cloud (VPC) on your local network.
    --
    -- For more information about RDS on Outposts, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/rds-on-outposts.html Working with Amazon RDS on Amazon Web Services Outposts>
    -- in the /Amazon RDS User Guide/.
    --
    -- For more information about CoIPs, see
    -- <https://docs.aws.amazon.com/outposts/latest/userguide/outposts-networking-components.html#ip-addressing Customer-owned IP addresses>
    -- in the /Amazon Web Services Outposts User Guide/.
    enableCustomerOwnedIp :: Prelude.Maybe Prelude.Bool,
    -- | The database engine to use for the new instance.
    --
    -- Default: The same as source
    --
    -- Constraint: Must be compatible with the engine of the source
    --
    -- Valid Values:
    --
    -- -   @mariadb@
    --
    -- -   @mysql@
    --
    -- -   @oracle-ee@
    --
    -- -   @oracle-ee-cdb@
    --
    -- -   @oracle-se2@
    --
    -- -   @oracle-se2-cdb@
    --
    -- -   @postgres@
    --
    -- -   @sqlserver-ee@
    --
    -- -   @sqlserver-se@
    --
    -- -   @sqlserver-ex@
    --
    -- -   @sqlserver-web@
    engine :: Prelude.Maybe Prelude.Text,
    -- | The password for the given ARN from the key store in order to access the
    -- device.
    tdeCredentialPassword :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the source DB instance from which to restore.
    --
    -- Constraints:
    --
    -- -   Must match the identifier of an existing DB instance.
    sourceDBInstanceIdentifier :: Prelude.Maybe Prelude.Text,
    -- | The number of CPU cores and the number of threads per core for the DB
    -- instance class of the DB instance.
    processorFeatures :: Prelude.Maybe [ProcessorFeature],
    -- | The compute and memory capacity of the Amazon RDS DB instance, for
    -- example, @db.m4.large@. Not all DB instance classes are available in all
    -- Amazon Web Services Regions, or for all database engines. For the full
    -- list of DB instance classes, and availability for your engine, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Concepts.DBInstanceClass.html DB Instance Class>
    -- in the /Amazon RDS User Guide./
    --
    -- Default: The same DBInstanceClass as the original DB instance.
    dbInstanceClass :: Prelude.Maybe Prelude.Text,
    -- | License model information for the restored DB instance.
    --
    -- Default: Same as source.
    --
    -- Valid values: @license-included@ | @bring-your-own-license@ |
    -- @general-public-license@
    licenseModel :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the replicated automated backups from
    -- which to restore, for example,
    -- @arn:aws:rds:useast-1:123456789012:auto-backup:ab-L2IJCEXJP7XQ7HOJ4SIEXAMPLE@.
    sourceDBInstanceAutomatedBackupsArn :: Prelude.Maybe Prelude.Text,
    -- | The upper limit in gibibytes (GiB) to which Amazon RDS can automatically
    -- scale the storage of the DB instance.
    --
    -- For more information about this setting, including limitations that
    -- apply to it, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_PIOPS.StorageTypes.html#USER_PIOPS.Autoscaling Managing capacity automatically with Amazon RDS storage autoscaling>
    -- in the /Amazon RDS User Guide/.
    maxAllocatedStorage :: Prelude.Maybe Prelude.Int,
    -- | The name of the DB parameter group to associate with this DB instance.
    --
    -- If you do not specify a value for @DBParameterGroupName@, then the
    -- default @DBParameterGroup@ for the specified DB engine is used.
    --
    -- Constraints:
    --
    -- -   If supplied, must match the name of an existing DBParameterGroup.
    --
    -- -   Must be 1 to 255 letters, numbers, or hyphens.
    --
    -- -   First character must be a letter.
    --
    -- -   Can\'t end with a hyphen or contain two consecutive hyphens.
    dbParameterGroupName :: Prelude.Maybe Prelude.Text,
    -- | The Availability Zone (AZ) where the DB instance will be created.
    --
    -- Default: A random, system-chosen Availability Zone.
    --
    -- Constraint: You can\'t specify the @AvailabilityZone@ parameter if the
    -- DB instance is a Multi-AZ deployment.
    --
    -- Example: @us-east-1a@
    availabilityZone :: Prelude.Maybe Prelude.Text,
    -- | A list of EC2 VPC security groups to associate with this DB instance.
    --
    -- Default: The default EC2 VPC security group for the DB subnet group\'s
    -- VPC.
    vpcSecurityGroupIds :: Prelude.Maybe [Prelude.Text],
    -- | A value that indicates whether the DB instance is a Multi-AZ deployment.
    --
    -- Constraint: You can\'t specify the @AvailabilityZone@ parameter if the
    -- DB instance is a Multi-AZ deployment.
    multiAZ :: Prelude.Maybe Prelude.Bool,
    -- | The resource ID of the source DB instance from which to restore.
    sourceDbiResourceId :: Prelude.Maybe Prelude.Text,
    -- | The name of the option group to be used for the restored DB instance.
    --
    -- Permanent options, such as the TDE option for Oracle Advanced Security
    -- TDE, can\'t be removed from an option group, and that option group
    -- can\'t be removed from a DB instance once it is associated with a DB
    -- instance
    optionGroupName :: Prelude.Maybe Prelude.Text,
    -- | A value that indicates whether to copy all tags from the restored DB
    -- instance to snapshots of the DB instance. By default, tags are not
    -- copied.
    copyTagsToSnapshot :: Prelude.Maybe Prelude.Bool,
    -- | The ARN from the key store with which to associate the instance for TDE
    -- encryption.
    tdeCredentialArn :: Prelude.Maybe Prelude.Text,
    -- | Specify the name of the IAM role to be used when making API calls to the
    -- Directory Service.
    domainIAMRoleName :: Prelude.Maybe Prelude.Text,
    tags :: Prelude.Maybe [Tag],
    -- | The port number on which the database accepts connections.
    --
    -- Constraints: Value must be @1150-65535@
    --
    -- Default: The same port as the original DB instance.
    port :: Prelude.Maybe Prelude.Int,
    -- | A value that indicates whether to enable mapping of Amazon Web Services
    -- Identity and Access Management (IAM) accounts to database accounts. By
    -- default, mapping is disabled.
    --
    -- For more information about IAM database authentication, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/UsingWithRDS.IAMDBAuth.html IAM Database Authentication for MySQL and PostgreSQL>
    -- in the /Amazon RDS User Guide./
    enableIAMDatabaseAuthentication :: Prelude.Maybe Prelude.Bool,
    -- | A value that indicates whether the DB instance class of the DB instance
    -- uses its default processor features.
    useDefaultProcessorFeatures :: Prelude.Maybe Prelude.Bool,
    -- | Specifies the storage type to be associated with the DB instance.
    --
    -- Valid values: @standard | gp2 | io1@
    --
    -- If you specify @io1@, you must also include a value for the @Iops@
    -- parameter.
    --
    -- Default: @io1@ if the @Iops@ parameter is specified, otherwise @gp2@
    storageType :: Prelude.Maybe Prelude.Text,
    -- | The list of logs that the restored DB instance is to export to
    -- CloudWatch Logs. The values in the list depend on the DB engine being
    -- used. For more information, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_LogAccess.html#USER_LogAccess.Procedural.UploadtoCloudWatch Publishing Database Logs to Amazon CloudWatch Logs>
    -- in the /Amazon RDS User Guide/.
    enableCloudwatchLogsExports :: Prelude.Maybe [Prelude.Text],
    -- | The database name for the restored DB instance.
    --
    -- This parameter isn\'t used for the MySQL or MariaDB engines.
    dbName :: Prelude.Maybe Prelude.Text,
    -- | The name of the new DB instance to be created.
    --
    -- Constraints:
    --
    -- -   Must contain from 1 to 63 letters, numbers, or hyphens
    --
    -- -   First character must be a letter
    --
    -- -   Can\'t end with a hyphen or contain two consecutive hyphens
    targetDBInstanceIdentifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RestoreDBInstanceToPointInTime' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deletionProtection', 'restoreDBInstanceToPointInTime_deletionProtection' - A value that indicates whether the DB instance has deletion protection
-- enabled. The database can\'t be deleted when deletion protection is
-- enabled. By default, deletion protection is disabled. For more
-- information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_DeleteInstance.html Deleting a DB Instance>.
--
-- 'useLatestRestorableTime', 'restoreDBInstanceToPointInTime_useLatestRestorableTime' - A value that indicates whether the DB instance is restored from the
-- latest backup time. By default, the DB instance isn\'t restored from the
-- latest backup time.
--
-- Constraints: Can\'t be specified if the @RestoreTime@ parameter is
-- provided.
--
-- 'publiclyAccessible', 'restoreDBInstanceToPointInTime_publiclyAccessible' - A value that indicates whether the DB instance is publicly accessible.
--
-- When the DB instance is publicly accessible, its DNS endpoint resolves
-- to the private IP address from within the DB instance\'s VPC, and to the
-- public IP address from outside of the DB instance\'s VPC. Access to the
-- DB instance is ultimately controlled by the security group it uses, and
-- that public access is not permitted if the security group assigned to
-- the DB instance doesn\'t permit it.
--
-- When the DB instance isn\'t publicly accessible, it is an internal DB
-- instance with a DNS name that resolves to a private IP address.
--
-- For more information, see CreateDBInstance.
--
-- 'autoMinorVersionUpgrade', 'restoreDBInstanceToPointInTime_autoMinorVersionUpgrade' - A value that indicates whether minor version upgrades are applied
-- automatically to the DB instance during the maintenance window.
--
-- 'dbSubnetGroupName', 'restoreDBInstanceToPointInTime_dbSubnetGroupName' - The DB subnet group name to use for the new instance.
--
-- Constraints: If supplied, must match the name of an existing
-- DBSubnetGroup.
--
-- Example: @mySubnetgroup@
--
-- 'restoreTime', 'restoreDBInstanceToPointInTime_restoreTime' - The date and time to restore from.
--
-- Valid Values: Value must be a time in Universal Coordinated Time (UTC)
-- format
--
-- Constraints:
--
-- -   Must be before the latest restorable time for the DB instance
--
-- -   Can\'t be specified if the @UseLatestRestorableTime@ parameter is
--     enabled
--
-- Example: @2009-09-07T23:45:00Z@
--
-- 'iops', 'restoreDBInstanceToPointInTime_iops' - The amount of Provisioned IOPS (input\/output operations per second) to
-- be initially allocated for the DB instance.
--
-- Constraints: Must be an integer greater than 1000.
--
-- __SQL Server__
--
-- Setting the IOPS value for the SQL Server database engine isn\'t
-- supported.
--
-- 'domain', 'restoreDBInstanceToPointInTime_domain' - Specify the Active Directory directory ID to restore the DB instance in.
-- The domain must be created prior to this operation. Currently, only
-- MySQL, Microsoft SQL Server, Oracle, and PostgreSQL DB instances can be
-- created in an Active Directory Domain.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/kerberos-authentication.html Kerberos Authentication>
-- in the /Amazon RDS User Guide/.
--
-- 'enableCustomerOwnedIp', 'restoreDBInstanceToPointInTime_enableCustomerOwnedIp' - A value that indicates whether to enable a customer-owned IP address
-- (CoIP) for an RDS on Outposts DB instance.
--
-- A /CoIP/ provides local or external connectivity to resources in your
-- Outpost subnets through your on-premises network. For some use cases, a
-- CoIP can provide lower latency for connections to the DB instance from
-- outside of its virtual private cloud (VPC) on your local network.
--
-- For more information about RDS on Outposts, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/rds-on-outposts.html Working with Amazon RDS on Amazon Web Services Outposts>
-- in the /Amazon RDS User Guide/.
--
-- For more information about CoIPs, see
-- <https://docs.aws.amazon.com/outposts/latest/userguide/outposts-networking-components.html#ip-addressing Customer-owned IP addresses>
-- in the /Amazon Web Services Outposts User Guide/.
--
-- 'engine', 'restoreDBInstanceToPointInTime_engine' - The database engine to use for the new instance.
--
-- Default: The same as source
--
-- Constraint: Must be compatible with the engine of the source
--
-- Valid Values:
--
-- -   @mariadb@
--
-- -   @mysql@
--
-- -   @oracle-ee@
--
-- -   @oracle-ee-cdb@
--
-- -   @oracle-se2@
--
-- -   @oracle-se2-cdb@
--
-- -   @postgres@
--
-- -   @sqlserver-ee@
--
-- -   @sqlserver-se@
--
-- -   @sqlserver-ex@
--
-- -   @sqlserver-web@
--
-- 'tdeCredentialPassword', 'restoreDBInstanceToPointInTime_tdeCredentialPassword' - The password for the given ARN from the key store in order to access the
-- device.
--
-- 'sourceDBInstanceIdentifier', 'restoreDBInstanceToPointInTime_sourceDBInstanceIdentifier' - The identifier of the source DB instance from which to restore.
--
-- Constraints:
--
-- -   Must match the identifier of an existing DB instance.
--
-- 'processorFeatures', 'restoreDBInstanceToPointInTime_processorFeatures' - The number of CPU cores and the number of threads per core for the DB
-- instance class of the DB instance.
--
-- 'dbInstanceClass', 'restoreDBInstanceToPointInTime_dbInstanceClass' - The compute and memory capacity of the Amazon RDS DB instance, for
-- example, @db.m4.large@. Not all DB instance classes are available in all
-- Amazon Web Services Regions, or for all database engines. For the full
-- list of DB instance classes, and availability for your engine, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Concepts.DBInstanceClass.html DB Instance Class>
-- in the /Amazon RDS User Guide./
--
-- Default: The same DBInstanceClass as the original DB instance.
--
-- 'licenseModel', 'restoreDBInstanceToPointInTime_licenseModel' - License model information for the restored DB instance.
--
-- Default: Same as source.
--
-- Valid values: @license-included@ | @bring-your-own-license@ |
-- @general-public-license@
--
-- 'sourceDBInstanceAutomatedBackupsArn', 'restoreDBInstanceToPointInTime_sourceDBInstanceAutomatedBackupsArn' - The Amazon Resource Name (ARN) of the replicated automated backups from
-- which to restore, for example,
-- @arn:aws:rds:useast-1:123456789012:auto-backup:ab-L2IJCEXJP7XQ7HOJ4SIEXAMPLE@.
--
-- 'maxAllocatedStorage', 'restoreDBInstanceToPointInTime_maxAllocatedStorage' - The upper limit in gibibytes (GiB) to which Amazon RDS can automatically
-- scale the storage of the DB instance.
--
-- For more information about this setting, including limitations that
-- apply to it, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_PIOPS.StorageTypes.html#USER_PIOPS.Autoscaling Managing capacity automatically with Amazon RDS storage autoscaling>
-- in the /Amazon RDS User Guide/.
--
-- 'dbParameterGroupName', 'restoreDBInstanceToPointInTime_dbParameterGroupName' - The name of the DB parameter group to associate with this DB instance.
--
-- If you do not specify a value for @DBParameterGroupName@, then the
-- default @DBParameterGroup@ for the specified DB engine is used.
--
-- Constraints:
--
-- -   If supplied, must match the name of an existing DBParameterGroup.
--
-- -   Must be 1 to 255 letters, numbers, or hyphens.
--
-- -   First character must be a letter.
--
-- -   Can\'t end with a hyphen or contain two consecutive hyphens.
--
-- 'availabilityZone', 'restoreDBInstanceToPointInTime_availabilityZone' - The Availability Zone (AZ) where the DB instance will be created.
--
-- Default: A random, system-chosen Availability Zone.
--
-- Constraint: You can\'t specify the @AvailabilityZone@ parameter if the
-- DB instance is a Multi-AZ deployment.
--
-- Example: @us-east-1a@
--
-- 'vpcSecurityGroupIds', 'restoreDBInstanceToPointInTime_vpcSecurityGroupIds' - A list of EC2 VPC security groups to associate with this DB instance.
--
-- Default: The default EC2 VPC security group for the DB subnet group\'s
-- VPC.
--
-- 'multiAZ', 'restoreDBInstanceToPointInTime_multiAZ' - A value that indicates whether the DB instance is a Multi-AZ deployment.
--
-- Constraint: You can\'t specify the @AvailabilityZone@ parameter if the
-- DB instance is a Multi-AZ deployment.
--
-- 'sourceDbiResourceId', 'restoreDBInstanceToPointInTime_sourceDbiResourceId' - The resource ID of the source DB instance from which to restore.
--
-- 'optionGroupName', 'restoreDBInstanceToPointInTime_optionGroupName' - The name of the option group to be used for the restored DB instance.
--
-- Permanent options, such as the TDE option for Oracle Advanced Security
-- TDE, can\'t be removed from an option group, and that option group
-- can\'t be removed from a DB instance once it is associated with a DB
-- instance
--
-- 'copyTagsToSnapshot', 'restoreDBInstanceToPointInTime_copyTagsToSnapshot' - A value that indicates whether to copy all tags from the restored DB
-- instance to snapshots of the DB instance. By default, tags are not
-- copied.
--
-- 'tdeCredentialArn', 'restoreDBInstanceToPointInTime_tdeCredentialArn' - The ARN from the key store with which to associate the instance for TDE
-- encryption.
--
-- 'domainIAMRoleName', 'restoreDBInstanceToPointInTime_domainIAMRoleName' - Specify the name of the IAM role to be used when making API calls to the
-- Directory Service.
--
-- 'tags', 'restoreDBInstanceToPointInTime_tags' - Undocumented member.
--
-- 'port', 'restoreDBInstanceToPointInTime_port' - The port number on which the database accepts connections.
--
-- Constraints: Value must be @1150-65535@
--
-- Default: The same port as the original DB instance.
--
-- 'enableIAMDatabaseAuthentication', 'restoreDBInstanceToPointInTime_enableIAMDatabaseAuthentication' - A value that indicates whether to enable mapping of Amazon Web Services
-- Identity and Access Management (IAM) accounts to database accounts. By
-- default, mapping is disabled.
--
-- For more information about IAM database authentication, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/UsingWithRDS.IAMDBAuth.html IAM Database Authentication for MySQL and PostgreSQL>
-- in the /Amazon RDS User Guide./
--
-- 'useDefaultProcessorFeatures', 'restoreDBInstanceToPointInTime_useDefaultProcessorFeatures' - A value that indicates whether the DB instance class of the DB instance
-- uses its default processor features.
--
-- 'storageType', 'restoreDBInstanceToPointInTime_storageType' - Specifies the storage type to be associated with the DB instance.
--
-- Valid values: @standard | gp2 | io1@
--
-- If you specify @io1@, you must also include a value for the @Iops@
-- parameter.
--
-- Default: @io1@ if the @Iops@ parameter is specified, otherwise @gp2@
--
-- 'enableCloudwatchLogsExports', 'restoreDBInstanceToPointInTime_enableCloudwatchLogsExports' - The list of logs that the restored DB instance is to export to
-- CloudWatch Logs. The values in the list depend on the DB engine being
-- used. For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_LogAccess.html#USER_LogAccess.Procedural.UploadtoCloudWatch Publishing Database Logs to Amazon CloudWatch Logs>
-- in the /Amazon RDS User Guide/.
--
-- 'dbName', 'restoreDBInstanceToPointInTime_dbName' - The database name for the restored DB instance.
--
-- This parameter isn\'t used for the MySQL or MariaDB engines.
--
-- 'targetDBInstanceIdentifier', 'restoreDBInstanceToPointInTime_targetDBInstanceIdentifier' - The name of the new DB instance to be created.
--
-- Constraints:
--
-- -   Must contain from 1 to 63 letters, numbers, or hyphens
--
-- -   First character must be a letter
--
-- -   Can\'t end with a hyphen or contain two consecutive hyphens
newRestoreDBInstanceToPointInTime ::
  -- | 'targetDBInstanceIdentifier'
  Prelude.Text ->
  RestoreDBInstanceToPointInTime
newRestoreDBInstanceToPointInTime
  pTargetDBInstanceIdentifier_ =
    RestoreDBInstanceToPointInTime'
      { deletionProtection =
          Prelude.Nothing,
        useLatestRestorableTime = Prelude.Nothing,
        publiclyAccessible = Prelude.Nothing,
        autoMinorVersionUpgrade = Prelude.Nothing,
        dbSubnetGroupName = Prelude.Nothing,
        restoreTime = Prelude.Nothing,
        iops = Prelude.Nothing,
        domain = Prelude.Nothing,
        enableCustomerOwnedIp = Prelude.Nothing,
        engine = Prelude.Nothing,
        tdeCredentialPassword = Prelude.Nothing,
        sourceDBInstanceIdentifier =
          Prelude.Nothing,
        processorFeatures = Prelude.Nothing,
        dbInstanceClass = Prelude.Nothing,
        licenseModel = Prelude.Nothing,
        sourceDBInstanceAutomatedBackupsArn =
          Prelude.Nothing,
        maxAllocatedStorage = Prelude.Nothing,
        dbParameterGroupName = Prelude.Nothing,
        availabilityZone = Prelude.Nothing,
        vpcSecurityGroupIds = Prelude.Nothing,
        multiAZ = Prelude.Nothing,
        sourceDbiResourceId = Prelude.Nothing,
        optionGroupName = Prelude.Nothing,
        copyTagsToSnapshot = Prelude.Nothing,
        tdeCredentialArn = Prelude.Nothing,
        domainIAMRoleName = Prelude.Nothing,
        tags = Prelude.Nothing,
        port = Prelude.Nothing,
        enableIAMDatabaseAuthentication =
          Prelude.Nothing,
        useDefaultProcessorFeatures =
          Prelude.Nothing,
        storageType = Prelude.Nothing,
        enableCloudwatchLogsExports =
          Prelude.Nothing,
        dbName = Prelude.Nothing,
        targetDBInstanceIdentifier =
          pTargetDBInstanceIdentifier_
      }

-- | A value that indicates whether the DB instance has deletion protection
-- enabled. The database can\'t be deleted when deletion protection is
-- enabled. By default, deletion protection is disabled. For more
-- information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_DeleteInstance.html Deleting a DB Instance>.
restoreDBInstanceToPointInTime_deletionProtection :: Lens.Lens' RestoreDBInstanceToPointInTime (Prelude.Maybe Prelude.Bool)
restoreDBInstanceToPointInTime_deletionProtection = Lens.lens (\RestoreDBInstanceToPointInTime' {deletionProtection} -> deletionProtection) (\s@RestoreDBInstanceToPointInTime' {} a -> s {deletionProtection = a} :: RestoreDBInstanceToPointInTime)

-- | A value that indicates whether the DB instance is restored from the
-- latest backup time. By default, the DB instance isn\'t restored from the
-- latest backup time.
--
-- Constraints: Can\'t be specified if the @RestoreTime@ parameter is
-- provided.
restoreDBInstanceToPointInTime_useLatestRestorableTime :: Lens.Lens' RestoreDBInstanceToPointInTime (Prelude.Maybe Prelude.Bool)
restoreDBInstanceToPointInTime_useLatestRestorableTime = Lens.lens (\RestoreDBInstanceToPointInTime' {useLatestRestorableTime} -> useLatestRestorableTime) (\s@RestoreDBInstanceToPointInTime' {} a -> s {useLatestRestorableTime = a} :: RestoreDBInstanceToPointInTime)

-- | A value that indicates whether the DB instance is publicly accessible.
--
-- When the DB instance is publicly accessible, its DNS endpoint resolves
-- to the private IP address from within the DB instance\'s VPC, and to the
-- public IP address from outside of the DB instance\'s VPC. Access to the
-- DB instance is ultimately controlled by the security group it uses, and
-- that public access is not permitted if the security group assigned to
-- the DB instance doesn\'t permit it.
--
-- When the DB instance isn\'t publicly accessible, it is an internal DB
-- instance with a DNS name that resolves to a private IP address.
--
-- For more information, see CreateDBInstance.
restoreDBInstanceToPointInTime_publiclyAccessible :: Lens.Lens' RestoreDBInstanceToPointInTime (Prelude.Maybe Prelude.Bool)
restoreDBInstanceToPointInTime_publiclyAccessible = Lens.lens (\RestoreDBInstanceToPointInTime' {publiclyAccessible} -> publiclyAccessible) (\s@RestoreDBInstanceToPointInTime' {} a -> s {publiclyAccessible = a} :: RestoreDBInstanceToPointInTime)

-- | A value that indicates whether minor version upgrades are applied
-- automatically to the DB instance during the maintenance window.
restoreDBInstanceToPointInTime_autoMinorVersionUpgrade :: Lens.Lens' RestoreDBInstanceToPointInTime (Prelude.Maybe Prelude.Bool)
restoreDBInstanceToPointInTime_autoMinorVersionUpgrade = Lens.lens (\RestoreDBInstanceToPointInTime' {autoMinorVersionUpgrade} -> autoMinorVersionUpgrade) (\s@RestoreDBInstanceToPointInTime' {} a -> s {autoMinorVersionUpgrade = a} :: RestoreDBInstanceToPointInTime)

-- | The DB subnet group name to use for the new instance.
--
-- Constraints: If supplied, must match the name of an existing
-- DBSubnetGroup.
--
-- Example: @mySubnetgroup@
restoreDBInstanceToPointInTime_dbSubnetGroupName :: Lens.Lens' RestoreDBInstanceToPointInTime (Prelude.Maybe Prelude.Text)
restoreDBInstanceToPointInTime_dbSubnetGroupName = Lens.lens (\RestoreDBInstanceToPointInTime' {dbSubnetGroupName} -> dbSubnetGroupName) (\s@RestoreDBInstanceToPointInTime' {} a -> s {dbSubnetGroupName = a} :: RestoreDBInstanceToPointInTime)

-- | The date and time to restore from.
--
-- Valid Values: Value must be a time in Universal Coordinated Time (UTC)
-- format
--
-- Constraints:
--
-- -   Must be before the latest restorable time for the DB instance
--
-- -   Can\'t be specified if the @UseLatestRestorableTime@ parameter is
--     enabled
--
-- Example: @2009-09-07T23:45:00Z@
restoreDBInstanceToPointInTime_restoreTime :: Lens.Lens' RestoreDBInstanceToPointInTime (Prelude.Maybe Prelude.UTCTime)
restoreDBInstanceToPointInTime_restoreTime = Lens.lens (\RestoreDBInstanceToPointInTime' {restoreTime} -> restoreTime) (\s@RestoreDBInstanceToPointInTime' {} a -> s {restoreTime = a} :: RestoreDBInstanceToPointInTime) Prelude.. Lens.mapping Core._Time

-- | The amount of Provisioned IOPS (input\/output operations per second) to
-- be initially allocated for the DB instance.
--
-- Constraints: Must be an integer greater than 1000.
--
-- __SQL Server__
--
-- Setting the IOPS value for the SQL Server database engine isn\'t
-- supported.
restoreDBInstanceToPointInTime_iops :: Lens.Lens' RestoreDBInstanceToPointInTime (Prelude.Maybe Prelude.Int)
restoreDBInstanceToPointInTime_iops = Lens.lens (\RestoreDBInstanceToPointInTime' {iops} -> iops) (\s@RestoreDBInstanceToPointInTime' {} a -> s {iops = a} :: RestoreDBInstanceToPointInTime)

-- | Specify the Active Directory directory ID to restore the DB instance in.
-- The domain must be created prior to this operation. Currently, only
-- MySQL, Microsoft SQL Server, Oracle, and PostgreSQL DB instances can be
-- created in an Active Directory Domain.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/kerberos-authentication.html Kerberos Authentication>
-- in the /Amazon RDS User Guide/.
restoreDBInstanceToPointInTime_domain :: Lens.Lens' RestoreDBInstanceToPointInTime (Prelude.Maybe Prelude.Text)
restoreDBInstanceToPointInTime_domain = Lens.lens (\RestoreDBInstanceToPointInTime' {domain} -> domain) (\s@RestoreDBInstanceToPointInTime' {} a -> s {domain = a} :: RestoreDBInstanceToPointInTime)

-- | A value that indicates whether to enable a customer-owned IP address
-- (CoIP) for an RDS on Outposts DB instance.
--
-- A /CoIP/ provides local or external connectivity to resources in your
-- Outpost subnets through your on-premises network. For some use cases, a
-- CoIP can provide lower latency for connections to the DB instance from
-- outside of its virtual private cloud (VPC) on your local network.
--
-- For more information about RDS on Outposts, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/rds-on-outposts.html Working with Amazon RDS on Amazon Web Services Outposts>
-- in the /Amazon RDS User Guide/.
--
-- For more information about CoIPs, see
-- <https://docs.aws.amazon.com/outposts/latest/userguide/outposts-networking-components.html#ip-addressing Customer-owned IP addresses>
-- in the /Amazon Web Services Outposts User Guide/.
restoreDBInstanceToPointInTime_enableCustomerOwnedIp :: Lens.Lens' RestoreDBInstanceToPointInTime (Prelude.Maybe Prelude.Bool)
restoreDBInstanceToPointInTime_enableCustomerOwnedIp = Lens.lens (\RestoreDBInstanceToPointInTime' {enableCustomerOwnedIp} -> enableCustomerOwnedIp) (\s@RestoreDBInstanceToPointInTime' {} a -> s {enableCustomerOwnedIp = a} :: RestoreDBInstanceToPointInTime)

-- | The database engine to use for the new instance.
--
-- Default: The same as source
--
-- Constraint: Must be compatible with the engine of the source
--
-- Valid Values:
--
-- -   @mariadb@
--
-- -   @mysql@
--
-- -   @oracle-ee@
--
-- -   @oracle-ee-cdb@
--
-- -   @oracle-se2@
--
-- -   @oracle-se2-cdb@
--
-- -   @postgres@
--
-- -   @sqlserver-ee@
--
-- -   @sqlserver-se@
--
-- -   @sqlserver-ex@
--
-- -   @sqlserver-web@
restoreDBInstanceToPointInTime_engine :: Lens.Lens' RestoreDBInstanceToPointInTime (Prelude.Maybe Prelude.Text)
restoreDBInstanceToPointInTime_engine = Lens.lens (\RestoreDBInstanceToPointInTime' {engine} -> engine) (\s@RestoreDBInstanceToPointInTime' {} a -> s {engine = a} :: RestoreDBInstanceToPointInTime)

-- | The password for the given ARN from the key store in order to access the
-- device.
restoreDBInstanceToPointInTime_tdeCredentialPassword :: Lens.Lens' RestoreDBInstanceToPointInTime (Prelude.Maybe Prelude.Text)
restoreDBInstanceToPointInTime_tdeCredentialPassword = Lens.lens (\RestoreDBInstanceToPointInTime' {tdeCredentialPassword} -> tdeCredentialPassword) (\s@RestoreDBInstanceToPointInTime' {} a -> s {tdeCredentialPassword = a} :: RestoreDBInstanceToPointInTime)

-- | The identifier of the source DB instance from which to restore.
--
-- Constraints:
--
-- -   Must match the identifier of an existing DB instance.
restoreDBInstanceToPointInTime_sourceDBInstanceIdentifier :: Lens.Lens' RestoreDBInstanceToPointInTime (Prelude.Maybe Prelude.Text)
restoreDBInstanceToPointInTime_sourceDBInstanceIdentifier = Lens.lens (\RestoreDBInstanceToPointInTime' {sourceDBInstanceIdentifier} -> sourceDBInstanceIdentifier) (\s@RestoreDBInstanceToPointInTime' {} a -> s {sourceDBInstanceIdentifier = a} :: RestoreDBInstanceToPointInTime)

-- | The number of CPU cores and the number of threads per core for the DB
-- instance class of the DB instance.
restoreDBInstanceToPointInTime_processorFeatures :: Lens.Lens' RestoreDBInstanceToPointInTime (Prelude.Maybe [ProcessorFeature])
restoreDBInstanceToPointInTime_processorFeatures = Lens.lens (\RestoreDBInstanceToPointInTime' {processorFeatures} -> processorFeatures) (\s@RestoreDBInstanceToPointInTime' {} a -> s {processorFeatures = a} :: RestoreDBInstanceToPointInTime) Prelude.. Lens.mapping Lens.coerced

-- | The compute and memory capacity of the Amazon RDS DB instance, for
-- example, @db.m4.large@. Not all DB instance classes are available in all
-- Amazon Web Services Regions, or for all database engines. For the full
-- list of DB instance classes, and availability for your engine, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Concepts.DBInstanceClass.html DB Instance Class>
-- in the /Amazon RDS User Guide./
--
-- Default: The same DBInstanceClass as the original DB instance.
restoreDBInstanceToPointInTime_dbInstanceClass :: Lens.Lens' RestoreDBInstanceToPointInTime (Prelude.Maybe Prelude.Text)
restoreDBInstanceToPointInTime_dbInstanceClass = Lens.lens (\RestoreDBInstanceToPointInTime' {dbInstanceClass} -> dbInstanceClass) (\s@RestoreDBInstanceToPointInTime' {} a -> s {dbInstanceClass = a} :: RestoreDBInstanceToPointInTime)

-- | License model information for the restored DB instance.
--
-- Default: Same as source.
--
-- Valid values: @license-included@ | @bring-your-own-license@ |
-- @general-public-license@
restoreDBInstanceToPointInTime_licenseModel :: Lens.Lens' RestoreDBInstanceToPointInTime (Prelude.Maybe Prelude.Text)
restoreDBInstanceToPointInTime_licenseModel = Lens.lens (\RestoreDBInstanceToPointInTime' {licenseModel} -> licenseModel) (\s@RestoreDBInstanceToPointInTime' {} a -> s {licenseModel = a} :: RestoreDBInstanceToPointInTime)

-- | The Amazon Resource Name (ARN) of the replicated automated backups from
-- which to restore, for example,
-- @arn:aws:rds:useast-1:123456789012:auto-backup:ab-L2IJCEXJP7XQ7HOJ4SIEXAMPLE@.
restoreDBInstanceToPointInTime_sourceDBInstanceAutomatedBackupsArn :: Lens.Lens' RestoreDBInstanceToPointInTime (Prelude.Maybe Prelude.Text)
restoreDBInstanceToPointInTime_sourceDBInstanceAutomatedBackupsArn = Lens.lens (\RestoreDBInstanceToPointInTime' {sourceDBInstanceAutomatedBackupsArn} -> sourceDBInstanceAutomatedBackupsArn) (\s@RestoreDBInstanceToPointInTime' {} a -> s {sourceDBInstanceAutomatedBackupsArn = a} :: RestoreDBInstanceToPointInTime)

-- | The upper limit in gibibytes (GiB) to which Amazon RDS can automatically
-- scale the storage of the DB instance.
--
-- For more information about this setting, including limitations that
-- apply to it, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_PIOPS.StorageTypes.html#USER_PIOPS.Autoscaling Managing capacity automatically with Amazon RDS storage autoscaling>
-- in the /Amazon RDS User Guide/.
restoreDBInstanceToPointInTime_maxAllocatedStorage :: Lens.Lens' RestoreDBInstanceToPointInTime (Prelude.Maybe Prelude.Int)
restoreDBInstanceToPointInTime_maxAllocatedStorage = Lens.lens (\RestoreDBInstanceToPointInTime' {maxAllocatedStorage} -> maxAllocatedStorage) (\s@RestoreDBInstanceToPointInTime' {} a -> s {maxAllocatedStorage = a} :: RestoreDBInstanceToPointInTime)

-- | The name of the DB parameter group to associate with this DB instance.
--
-- If you do not specify a value for @DBParameterGroupName@, then the
-- default @DBParameterGroup@ for the specified DB engine is used.
--
-- Constraints:
--
-- -   If supplied, must match the name of an existing DBParameterGroup.
--
-- -   Must be 1 to 255 letters, numbers, or hyphens.
--
-- -   First character must be a letter.
--
-- -   Can\'t end with a hyphen or contain two consecutive hyphens.
restoreDBInstanceToPointInTime_dbParameterGroupName :: Lens.Lens' RestoreDBInstanceToPointInTime (Prelude.Maybe Prelude.Text)
restoreDBInstanceToPointInTime_dbParameterGroupName = Lens.lens (\RestoreDBInstanceToPointInTime' {dbParameterGroupName} -> dbParameterGroupName) (\s@RestoreDBInstanceToPointInTime' {} a -> s {dbParameterGroupName = a} :: RestoreDBInstanceToPointInTime)

-- | The Availability Zone (AZ) where the DB instance will be created.
--
-- Default: A random, system-chosen Availability Zone.
--
-- Constraint: You can\'t specify the @AvailabilityZone@ parameter if the
-- DB instance is a Multi-AZ deployment.
--
-- Example: @us-east-1a@
restoreDBInstanceToPointInTime_availabilityZone :: Lens.Lens' RestoreDBInstanceToPointInTime (Prelude.Maybe Prelude.Text)
restoreDBInstanceToPointInTime_availabilityZone = Lens.lens (\RestoreDBInstanceToPointInTime' {availabilityZone} -> availabilityZone) (\s@RestoreDBInstanceToPointInTime' {} a -> s {availabilityZone = a} :: RestoreDBInstanceToPointInTime)

-- | A list of EC2 VPC security groups to associate with this DB instance.
--
-- Default: The default EC2 VPC security group for the DB subnet group\'s
-- VPC.
restoreDBInstanceToPointInTime_vpcSecurityGroupIds :: Lens.Lens' RestoreDBInstanceToPointInTime (Prelude.Maybe [Prelude.Text])
restoreDBInstanceToPointInTime_vpcSecurityGroupIds = Lens.lens (\RestoreDBInstanceToPointInTime' {vpcSecurityGroupIds} -> vpcSecurityGroupIds) (\s@RestoreDBInstanceToPointInTime' {} a -> s {vpcSecurityGroupIds = a} :: RestoreDBInstanceToPointInTime) Prelude.. Lens.mapping Lens.coerced

-- | A value that indicates whether the DB instance is a Multi-AZ deployment.
--
-- Constraint: You can\'t specify the @AvailabilityZone@ parameter if the
-- DB instance is a Multi-AZ deployment.
restoreDBInstanceToPointInTime_multiAZ :: Lens.Lens' RestoreDBInstanceToPointInTime (Prelude.Maybe Prelude.Bool)
restoreDBInstanceToPointInTime_multiAZ = Lens.lens (\RestoreDBInstanceToPointInTime' {multiAZ} -> multiAZ) (\s@RestoreDBInstanceToPointInTime' {} a -> s {multiAZ = a} :: RestoreDBInstanceToPointInTime)

-- | The resource ID of the source DB instance from which to restore.
restoreDBInstanceToPointInTime_sourceDbiResourceId :: Lens.Lens' RestoreDBInstanceToPointInTime (Prelude.Maybe Prelude.Text)
restoreDBInstanceToPointInTime_sourceDbiResourceId = Lens.lens (\RestoreDBInstanceToPointInTime' {sourceDbiResourceId} -> sourceDbiResourceId) (\s@RestoreDBInstanceToPointInTime' {} a -> s {sourceDbiResourceId = a} :: RestoreDBInstanceToPointInTime)

-- | The name of the option group to be used for the restored DB instance.
--
-- Permanent options, such as the TDE option for Oracle Advanced Security
-- TDE, can\'t be removed from an option group, and that option group
-- can\'t be removed from a DB instance once it is associated with a DB
-- instance
restoreDBInstanceToPointInTime_optionGroupName :: Lens.Lens' RestoreDBInstanceToPointInTime (Prelude.Maybe Prelude.Text)
restoreDBInstanceToPointInTime_optionGroupName = Lens.lens (\RestoreDBInstanceToPointInTime' {optionGroupName} -> optionGroupName) (\s@RestoreDBInstanceToPointInTime' {} a -> s {optionGroupName = a} :: RestoreDBInstanceToPointInTime)

-- | A value that indicates whether to copy all tags from the restored DB
-- instance to snapshots of the DB instance. By default, tags are not
-- copied.
restoreDBInstanceToPointInTime_copyTagsToSnapshot :: Lens.Lens' RestoreDBInstanceToPointInTime (Prelude.Maybe Prelude.Bool)
restoreDBInstanceToPointInTime_copyTagsToSnapshot = Lens.lens (\RestoreDBInstanceToPointInTime' {copyTagsToSnapshot} -> copyTagsToSnapshot) (\s@RestoreDBInstanceToPointInTime' {} a -> s {copyTagsToSnapshot = a} :: RestoreDBInstanceToPointInTime)

-- | The ARN from the key store with which to associate the instance for TDE
-- encryption.
restoreDBInstanceToPointInTime_tdeCredentialArn :: Lens.Lens' RestoreDBInstanceToPointInTime (Prelude.Maybe Prelude.Text)
restoreDBInstanceToPointInTime_tdeCredentialArn = Lens.lens (\RestoreDBInstanceToPointInTime' {tdeCredentialArn} -> tdeCredentialArn) (\s@RestoreDBInstanceToPointInTime' {} a -> s {tdeCredentialArn = a} :: RestoreDBInstanceToPointInTime)

-- | Specify the name of the IAM role to be used when making API calls to the
-- Directory Service.
restoreDBInstanceToPointInTime_domainIAMRoleName :: Lens.Lens' RestoreDBInstanceToPointInTime (Prelude.Maybe Prelude.Text)
restoreDBInstanceToPointInTime_domainIAMRoleName = Lens.lens (\RestoreDBInstanceToPointInTime' {domainIAMRoleName} -> domainIAMRoleName) (\s@RestoreDBInstanceToPointInTime' {} a -> s {domainIAMRoleName = a} :: RestoreDBInstanceToPointInTime)

-- | Undocumented member.
restoreDBInstanceToPointInTime_tags :: Lens.Lens' RestoreDBInstanceToPointInTime (Prelude.Maybe [Tag])
restoreDBInstanceToPointInTime_tags = Lens.lens (\RestoreDBInstanceToPointInTime' {tags} -> tags) (\s@RestoreDBInstanceToPointInTime' {} a -> s {tags = a} :: RestoreDBInstanceToPointInTime) Prelude.. Lens.mapping Lens.coerced

-- | The port number on which the database accepts connections.
--
-- Constraints: Value must be @1150-65535@
--
-- Default: The same port as the original DB instance.
restoreDBInstanceToPointInTime_port :: Lens.Lens' RestoreDBInstanceToPointInTime (Prelude.Maybe Prelude.Int)
restoreDBInstanceToPointInTime_port = Lens.lens (\RestoreDBInstanceToPointInTime' {port} -> port) (\s@RestoreDBInstanceToPointInTime' {} a -> s {port = a} :: RestoreDBInstanceToPointInTime)

-- | A value that indicates whether to enable mapping of Amazon Web Services
-- Identity and Access Management (IAM) accounts to database accounts. By
-- default, mapping is disabled.
--
-- For more information about IAM database authentication, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/UsingWithRDS.IAMDBAuth.html IAM Database Authentication for MySQL and PostgreSQL>
-- in the /Amazon RDS User Guide./
restoreDBInstanceToPointInTime_enableIAMDatabaseAuthentication :: Lens.Lens' RestoreDBInstanceToPointInTime (Prelude.Maybe Prelude.Bool)
restoreDBInstanceToPointInTime_enableIAMDatabaseAuthentication = Lens.lens (\RestoreDBInstanceToPointInTime' {enableIAMDatabaseAuthentication} -> enableIAMDatabaseAuthentication) (\s@RestoreDBInstanceToPointInTime' {} a -> s {enableIAMDatabaseAuthentication = a} :: RestoreDBInstanceToPointInTime)

-- | A value that indicates whether the DB instance class of the DB instance
-- uses its default processor features.
restoreDBInstanceToPointInTime_useDefaultProcessorFeatures :: Lens.Lens' RestoreDBInstanceToPointInTime (Prelude.Maybe Prelude.Bool)
restoreDBInstanceToPointInTime_useDefaultProcessorFeatures = Lens.lens (\RestoreDBInstanceToPointInTime' {useDefaultProcessorFeatures} -> useDefaultProcessorFeatures) (\s@RestoreDBInstanceToPointInTime' {} a -> s {useDefaultProcessorFeatures = a} :: RestoreDBInstanceToPointInTime)

-- | Specifies the storage type to be associated with the DB instance.
--
-- Valid values: @standard | gp2 | io1@
--
-- If you specify @io1@, you must also include a value for the @Iops@
-- parameter.
--
-- Default: @io1@ if the @Iops@ parameter is specified, otherwise @gp2@
restoreDBInstanceToPointInTime_storageType :: Lens.Lens' RestoreDBInstanceToPointInTime (Prelude.Maybe Prelude.Text)
restoreDBInstanceToPointInTime_storageType = Lens.lens (\RestoreDBInstanceToPointInTime' {storageType} -> storageType) (\s@RestoreDBInstanceToPointInTime' {} a -> s {storageType = a} :: RestoreDBInstanceToPointInTime)

-- | The list of logs that the restored DB instance is to export to
-- CloudWatch Logs. The values in the list depend on the DB engine being
-- used. For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_LogAccess.html#USER_LogAccess.Procedural.UploadtoCloudWatch Publishing Database Logs to Amazon CloudWatch Logs>
-- in the /Amazon RDS User Guide/.
restoreDBInstanceToPointInTime_enableCloudwatchLogsExports :: Lens.Lens' RestoreDBInstanceToPointInTime (Prelude.Maybe [Prelude.Text])
restoreDBInstanceToPointInTime_enableCloudwatchLogsExports = Lens.lens (\RestoreDBInstanceToPointInTime' {enableCloudwatchLogsExports} -> enableCloudwatchLogsExports) (\s@RestoreDBInstanceToPointInTime' {} a -> s {enableCloudwatchLogsExports = a} :: RestoreDBInstanceToPointInTime) Prelude.. Lens.mapping Lens.coerced

-- | The database name for the restored DB instance.
--
-- This parameter isn\'t used for the MySQL or MariaDB engines.
restoreDBInstanceToPointInTime_dbName :: Lens.Lens' RestoreDBInstanceToPointInTime (Prelude.Maybe Prelude.Text)
restoreDBInstanceToPointInTime_dbName = Lens.lens (\RestoreDBInstanceToPointInTime' {dbName} -> dbName) (\s@RestoreDBInstanceToPointInTime' {} a -> s {dbName = a} :: RestoreDBInstanceToPointInTime)

-- | The name of the new DB instance to be created.
--
-- Constraints:
--
-- -   Must contain from 1 to 63 letters, numbers, or hyphens
--
-- -   First character must be a letter
--
-- -   Can\'t end with a hyphen or contain two consecutive hyphens
restoreDBInstanceToPointInTime_targetDBInstanceIdentifier :: Lens.Lens' RestoreDBInstanceToPointInTime Prelude.Text
restoreDBInstanceToPointInTime_targetDBInstanceIdentifier = Lens.lens (\RestoreDBInstanceToPointInTime' {targetDBInstanceIdentifier} -> targetDBInstanceIdentifier) (\s@RestoreDBInstanceToPointInTime' {} a -> s {targetDBInstanceIdentifier = a} :: RestoreDBInstanceToPointInTime)

instance
  Core.AWSRequest
    RestoreDBInstanceToPointInTime
  where
  type
    AWSResponse RestoreDBInstanceToPointInTime =
      RestoreDBInstanceToPointInTimeResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "RestoreDBInstanceToPointInTimeResult"
      ( \s h x ->
          RestoreDBInstanceToPointInTimeResponse'
            Prelude.<$> (x Core..@? "DBInstance")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    RestoreDBInstanceToPointInTime

instance
  Prelude.NFData
    RestoreDBInstanceToPointInTime

instance
  Core.ToHeaders
    RestoreDBInstanceToPointInTime
  where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath RestoreDBInstanceToPointInTime where
  toPath = Prelude.const "/"

instance Core.ToQuery RestoreDBInstanceToPointInTime where
  toQuery RestoreDBInstanceToPointInTime' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ( "RestoreDBInstanceToPointInTime" ::
                      Prelude.ByteString
                  ),
        "Version"
          Core.=: ("2014-10-31" :: Prelude.ByteString),
        "DeletionProtection" Core.=: deletionProtection,
        "UseLatestRestorableTime"
          Core.=: useLatestRestorableTime,
        "PubliclyAccessible" Core.=: publiclyAccessible,
        "AutoMinorVersionUpgrade"
          Core.=: autoMinorVersionUpgrade,
        "DBSubnetGroupName" Core.=: dbSubnetGroupName,
        "RestoreTime" Core.=: restoreTime,
        "Iops" Core.=: iops,
        "Domain" Core.=: domain,
        "EnableCustomerOwnedIp"
          Core.=: enableCustomerOwnedIp,
        "Engine" Core.=: engine,
        "TdeCredentialPassword"
          Core.=: tdeCredentialPassword,
        "SourceDBInstanceIdentifier"
          Core.=: sourceDBInstanceIdentifier,
        "ProcessorFeatures"
          Core.=: Core.toQuery
            ( Core.toQueryList "ProcessorFeature"
                Prelude.<$> processorFeatures
            ),
        "DBInstanceClass" Core.=: dbInstanceClass,
        "LicenseModel" Core.=: licenseModel,
        "SourceDBInstanceAutomatedBackupsArn"
          Core.=: sourceDBInstanceAutomatedBackupsArn,
        "MaxAllocatedStorage" Core.=: maxAllocatedStorage,
        "DBParameterGroupName" Core.=: dbParameterGroupName,
        "AvailabilityZone" Core.=: availabilityZone,
        "VpcSecurityGroupIds"
          Core.=: Core.toQuery
            ( Core.toQueryList "VpcSecurityGroupId"
                Prelude.<$> vpcSecurityGroupIds
            ),
        "MultiAZ" Core.=: multiAZ,
        "SourceDbiResourceId" Core.=: sourceDbiResourceId,
        "OptionGroupName" Core.=: optionGroupName,
        "CopyTagsToSnapshot" Core.=: copyTagsToSnapshot,
        "TdeCredentialArn" Core.=: tdeCredentialArn,
        "DomainIAMRoleName" Core.=: domainIAMRoleName,
        "Tags"
          Core.=: Core.toQuery
            (Core.toQueryList "Tag" Prelude.<$> tags),
        "Port" Core.=: port,
        "EnableIAMDatabaseAuthentication"
          Core.=: enableIAMDatabaseAuthentication,
        "UseDefaultProcessorFeatures"
          Core.=: useDefaultProcessorFeatures,
        "StorageType" Core.=: storageType,
        "EnableCloudwatchLogsExports"
          Core.=: Core.toQuery
            ( Core.toQueryList "member"
                Prelude.<$> enableCloudwatchLogsExports
            ),
        "DBName" Core.=: dbName,
        "TargetDBInstanceIdentifier"
          Core.=: targetDBInstanceIdentifier
      ]

-- | /See:/ 'newRestoreDBInstanceToPointInTimeResponse' smart constructor.
data RestoreDBInstanceToPointInTimeResponse = RestoreDBInstanceToPointInTimeResponse'
  { dbInstance :: Prelude.Maybe DBInstance,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RestoreDBInstanceToPointInTimeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dbInstance', 'restoreDBInstanceToPointInTimeResponse_dbInstance' - Undocumented member.
--
-- 'httpStatus', 'restoreDBInstanceToPointInTimeResponse_httpStatus' - The response's http status code.
newRestoreDBInstanceToPointInTimeResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  RestoreDBInstanceToPointInTimeResponse
newRestoreDBInstanceToPointInTimeResponse
  pHttpStatus_ =
    RestoreDBInstanceToPointInTimeResponse'
      { dbInstance =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Undocumented member.
restoreDBInstanceToPointInTimeResponse_dbInstance :: Lens.Lens' RestoreDBInstanceToPointInTimeResponse (Prelude.Maybe DBInstance)
restoreDBInstanceToPointInTimeResponse_dbInstance = Lens.lens (\RestoreDBInstanceToPointInTimeResponse' {dbInstance} -> dbInstance) (\s@RestoreDBInstanceToPointInTimeResponse' {} a -> s {dbInstance = a} :: RestoreDBInstanceToPointInTimeResponse)

-- | The response's http status code.
restoreDBInstanceToPointInTimeResponse_httpStatus :: Lens.Lens' RestoreDBInstanceToPointInTimeResponse Prelude.Int
restoreDBInstanceToPointInTimeResponse_httpStatus = Lens.lens (\RestoreDBInstanceToPointInTimeResponse' {httpStatus} -> httpStatus) (\s@RestoreDBInstanceToPointInTimeResponse' {} a -> s {httpStatus = a} :: RestoreDBInstanceToPointInTimeResponse)

instance
  Prelude.NFData
    RestoreDBInstanceToPointInTimeResponse
