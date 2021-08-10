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
-- Module      : Network.AWS.RDS.RestoreDBInstanceFromDBSnapshot
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new DB instance from a DB snapshot. The target database is
-- created from the source database restore point with most of the
-- source\'s original configuration, including the default security group
-- and DB parameter group. By default, the new DB instance is created as a
-- Single-AZ deployment, except when the instance is a SQL Server instance
-- that has an option group associated with mirroring. In this case, the
-- instance becomes a Multi-AZ deployment, not a Single-AZ deployment.
--
-- If you want to replace your original DB instance with the new, restored
-- DB instance, then rename your original DB instance before you call the
-- RestoreDBInstanceFromDBSnapshot action. RDS doesn\'t allow two DB
-- instances with the same name. After you have renamed your original DB
-- instance with a different identifier, then you can pass the original
-- name of the DB instance as the DBInstanceIdentifier in the call to the
-- RestoreDBInstanceFromDBSnapshot action. The result is that you replace
-- the original DB instance with the DB instance created from the snapshot.
--
-- If you are restoring from a shared manual DB snapshot, the
-- @DBSnapshotIdentifier@ must be the ARN of the shared DB snapshot.
--
-- This command doesn\'t apply to Aurora MySQL and Aurora PostgreSQL. For
-- Aurora, use @RestoreDBClusterFromSnapshot@.
module Network.AWS.RDS.RestoreDBInstanceFromDBSnapshot
  ( -- * Creating a Request
    RestoreDBInstanceFromDBSnapshot (..),
    newRestoreDBInstanceFromDBSnapshot,

    -- * Request Lenses
    restoreDBInstanceFromDBSnapshot_deletionProtection,
    restoreDBInstanceFromDBSnapshot_enableIAMDatabaseAuthentication,
    restoreDBInstanceFromDBSnapshot_enableCloudwatchLogsExports,
    restoreDBInstanceFromDBSnapshot_storageType,
    restoreDBInstanceFromDBSnapshot_useDefaultProcessorFeatures,
    restoreDBInstanceFromDBSnapshot_optionGroupName,
    restoreDBInstanceFromDBSnapshot_domain,
    restoreDBInstanceFromDBSnapshot_dbSubnetGroupName,
    restoreDBInstanceFromDBSnapshot_multiAZ,
    restoreDBInstanceFromDBSnapshot_publiclyAccessible,
    restoreDBInstanceFromDBSnapshot_vpcSecurityGroupIds,
    restoreDBInstanceFromDBSnapshot_dbParameterGroupName,
    restoreDBInstanceFromDBSnapshot_availabilityZone,
    restoreDBInstanceFromDBSnapshot_licenseModel,
    restoreDBInstanceFromDBSnapshot_tdeCredentialPassword,
    restoreDBInstanceFromDBSnapshot_tags,
    restoreDBInstanceFromDBSnapshot_processorFeatures,
    restoreDBInstanceFromDBSnapshot_port,
    restoreDBInstanceFromDBSnapshot_dbInstanceClass,
    restoreDBInstanceFromDBSnapshot_dbName,
    restoreDBInstanceFromDBSnapshot_domainIAMRoleName,
    restoreDBInstanceFromDBSnapshot_engine,
    restoreDBInstanceFromDBSnapshot_tdeCredentialArn,
    restoreDBInstanceFromDBSnapshot_enableCustomerOwnedIp,
    restoreDBInstanceFromDBSnapshot_copyTagsToSnapshot,
    restoreDBInstanceFromDBSnapshot_iops,
    restoreDBInstanceFromDBSnapshot_autoMinorVersionUpgrade,
    restoreDBInstanceFromDBSnapshot_dbInstanceIdentifier,
    restoreDBInstanceFromDBSnapshot_dbSnapshotIdentifier,

    -- * Destructuring the Response
    RestoreDBInstanceFromDBSnapshotResponse (..),
    newRestoreDBInstanceFromDBSnapshotResponse,

    -- * Response Lenses
    restoreDBInstanceFromDBSnapshotResponse_dbInstance,
    restoreDBInstanceFromDBSnapshotResponse_httpStatus,
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
-- /See:/ 'newRestoreDBInstanceFromDBSnapshot' smart constructor.
data RestoreDBInstanceFromDBSnapshot = RestoreDBInstanceFromDBSnapshot'
  { -- | A value that indicates whether the DB instance has deletion protection
    -- enabled. The database can\'t be deleted when deletion protection is
    -- enabled. By default, deletion protection is disabled. For more
    -- information, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_DeleteInstance.html Deleting a DB Instance>.
    deletionProtection :: Prelude.Maybe Prelude.Bool,
    -- | A value that indicates whether to enable mapping of AWS Identity and
    -- Access Management (IAM) accounts to database accounts. By default,
    -- mapping is disabled.
    --
    -- For more information about IAM database authentication, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/UsingWithRDS.IAMDBAuth.html IAM Database Authentication for MySQL and PostgreSQL>
    -- in the /Amazon RDS User Guide./
    enableIAMDatabaseAuthentication :: Prelude.Maybe Prelude.Bool,
    -- | The list of logs that the restored DB instance is to export to
    -- CloudWatch Logs. The values in the list depend on the DB engine being
    -- used. For more information, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_LogAccess.html#USER_LogAccess.Procedural.UploadtoCloudWatch Publishing Database Logs to Amazon CloudWatch Logs>
    -- in the /Amazon RDS User Guide/.
    enableCloudwatchLogsExports :: Prelude.Maybe [Prelude.Text],
    -- | Specifies the storage type to be associated with the DB instance.
    --
    -- Valid values: @standard | gp2 | io1@
    --
    -- If you specify @io1@, you must also include a value for the @Iops@
    -- parameter.
    --
    -- Default: @io1@ if the @Iops@ parameter is specified, otherwise @gp2@
    storageType :: Prelude.Maybe Prelude.Text,
    -- | A value that indicates whether the DB instance class of the DB instance
    -- uses its default processor features.
    useDefaultProcessorFeatures :: Prelude.Maybe Prelude.Bool,
    -- | The name of the option group to be used for the restored DB instance.
    --
    -- Permanent options, such as the TDE option for Oracle Advanced Security
    -- TDE, can\'t be removed from an option group, and that option group
    -- can\'t be removed from a DB instance once it is associated with a DB
    -- instance
    optionGroupName :: Prelude.Maybe Prelude.Text,
    -- | Specify the Active Directory directory ID to restore the DB instance in.
    -- The domain must be created prior to this operation. Currently, only
    -- MySQL, Microsoft SQL Server, Oracle, and PostgreSQL DB instances can be
    -- created in an Active Directory Domain.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/kerberos-authentication.html Kerberos Authentication>
    -- in the /Amazon RDS User Guide/.
    domain :: Prelude.Maybe Prelude.Text,
    -- | The DB subnet group name to use for the new instance.
    --
    -- Constraints: If supplied, must match the name of an existing
    -- DBSubnetGroup.
    --
    -- Example: @mySubnetgroup@
    dbSubnetGroupName :: Prelude.Maybe Prelude.Text,
    -- | A value that indicates whether the DB instance is a Multi-AZ deployment.
    --
    -- Constraint: You can\'t specify the @AvailabilityZone@ parameter if the
    -- DB instance is a Multi-AZ deployment.
    multiAZ :: Prelude.Maybe Prelude.Bool,
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
    -- | A list of EC2 VPC security groups to associate with this DB instance.
    --
    -- Default: The default EC2 VPC security group for the DB subnet group\'s
    -- VPC.
    vpcSecurityGroupIds :: Prelude.Maybe [Prelude.Text],
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
    -- | License model information for the restored DB instance.
    --
    -- Default: Same as source.
    --
    -- Valid values: @license-included@ | @bring-your-own-license@ |
    -- @general-public-license@
    licenseModel :: Prelude.Maybe Prelude.Text,
    -- | The password for the given ARN from the key store in order to access the
    -- device.
    tdeCredentialPassword :: Prelude.Maybe Prelude.Text,
    tags :: Prelude.Maybe [Tag],
    -- | The number of CPU cores and the number of threads per core for the DB
    -- instance class of the DB instance.
    processorFeatures :: Prelude.Maybe [ProcessorFeature],
    -- | The port number on which the database accepts connections.
    --
    -- Default: The same port as the original DB instance
    --
    -- Constraints: Value must be @1150-65535@
    port :: Prelude.Maybe Prelude.Int,
    -- | The compute and memory capacity of the Amazon RDS DB instance, for
    -- example, @db.m4.large@. Not all DB instance classes are available in all
    -- AWS Regions, or for all database engines. For the full list of DB
    -- instance classes, and availability for your engine, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Concepts.DBInstanceClass.html DB Instance Class>
    -- in the /Amazon RDS User Guide./
    --
    -- Default: The same DBInstanceClass as the original DB instance.
    dbInstanceClass :: Prelude.Maybe Prelude.Text,
    -- | The database name for the restored DB instance.
    --
    -- This parameter doesn\'t apply to the MySQL, PostgreSQL, or MariaDB
    -- engines.
    dbName :: Prelude.Maybe Prelude.Text,
    -- | Specify the name of the IAM role to be used when making API calls to the
    -- Directory Service.
    domainIAMRoleName :: Prelude.Maybe Prelude.Text,
    -- | The database engine to use for the new instance.
    --
    -- Default: The same as source
    --
    -- Constraint: Must be compatible with the engine of the source. For
    -- example, you can restore a MariaDB 10.1 DB instance from a MySQL 5.6
    -- snapshot.
    --
    -- Valid Values:
    --
    -- -   @mariadb@
    --
    -- -   @mysql@
    --
    -- -   @oracle-ee@
    --
    -- -   @oracle-se2@
    --
    -- -   @oracle-se1@
    --
    -- -   @oracle-se@
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
    -- | The ARN from the key store with which to associate the instance for TDE
    -- encryption.
    tdeCredentialArn :: Prelude.Maybe Prelude.Text,
    -- | A value that indicates whether to enable a customer-owned IP address
    -- (CoIP) for an RDS on Outposts DB instance.
    --
    -- A /CoIP/ provides local or external connectivity to resources in your
    -- Outpost subnets through your on-premises network. For some use cases, a
    -- CoIP can provide lower latency for connections to the DB instance from
    -- outside of its virtual private cloud (VPC) on your local network.
    --
    -- For more information about RDS on Outposts, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/rds-on-outposts.html Working with Amazon RDS on AWS Outposts>
    -- in the /Amazon RDS User Guide/.
    --
    -- For more information about CoIPs, see
    -- <https://docs.aws.amazon.com/outposts/latest/userguide/outposts-networking-components.html#ip-addressing Customer-owned IP addresses>
    -- in the /AWS Outposts User Guide/.
    enableCustomerOwnedIp :: Prelude.Maybe Prelude.Bool,
    -- | A value that indicates whether to copy all tags from the restored DB
    -- instance to snapshots of the DB instance. By default, tags are not
    -- copied.
    copyTagsToSnapshot :: Prelude.Maybe Prelude.Bool,
    -- | Specifies the amount of provisioned IOPS for the DB instance, expressed
    -- in I\/O operations per second. If this parameter isn\'t specified, the
    -- IOPS value is taken from the backup. If this parameter is set to 0, the
    -- new instance is converted to a non-PIOPS instance. The conversion takes
    -- additional time, though your DB instance is available for connections
    -- before the conversion starts.
    --
    -- The provisioned IOPS value must follow the requirements for your
    -- database engine. For more information, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/CHAP_Storage.html#USER_PIOPS Amazon RDS Provisioned IOPS Storage to Improve Performance>
    -- in the /Amazon RDS User Guide./
    --
    -- Constraints: Must be an integer greater than 1000.
    iops :: Prelude.Maybe Prelude.Int,
    -- | A value that indicates whether minor version upgrades are applied
    -- automatically to the DB instance during the maintenance window.
    autoMinorVersionUpgrade :: Prelude.Maybe Prelude.Bool,
    -- | Name of the DB instance to create from the DB snapshot. This parameter
    -- isn\'t case-sensitive.
    --
    -- Constraints:
    --
    -- -   Must contain from 1 to 63 numbers, letters, or hyphens
    --
    -- -   First character must be a letter
    --
    -- -   Can\'t end with a hyphen or contain two consecutive hyphens
    --
    -- Example: @my-snapshot-id@
    dbInstanceIdentifier :: Prelude.Text,
    -- | The identifier for the DB snapshot to restore from.
    --
    -- Constraints:
    --
    -- -   Must match the identifier of an existing DBSnapshot.
    --
    -- -   If you are restoring from a shared manual DB snapshot, the
    --     @DBSnapshotIdentifier@ must be the ARN of the shared DB snapshot.
    dbSnapshotIdentifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RestoreDBInstanceFromDBSnapshot' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deletionProtection', 'restoreDBInstanceFromDBSnapshot_deletionProtection' - A value that indicates whether the DB instance has deletion protection
-- enabled. The database can\'t be deleted when deletion protection is
-- enabled. By default, deletion protection is disabled. For more
-- information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_DeleteInstance.html Deleting a DB Instance>.
--
-- 'enableIAMDatabaseAuthentication', 'restoreDBInstanceFromDBSnapshot_enableIAMDatabaseAuthentication' - A value that indicates whether to enable mapping of AWS Identity and
-- Access Management (IAM) accounts to database accounts. By default,
-- mapping is disabled.
--
-- For more information about IAM database authentication, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/UsingWithRDS.IAMDBAuth.html IAM Database Authentication for MySQL and PostgreSQL>
-- in the /Amazon RDS User Guide./
--
-- 'enableCloudwatchLogsExports', 'restoreDBInstanceFromDBSnapshot_enableCloudwatchLogsExports' - The list of logs that the restored DB instance is to export to
-- CloudWatch Logs. The values in the list depend on the DB engine being
-- used. For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_LogAccess.html#USER_LogAccess.Procedural.UploadtoCloudWatch Publishing Database Logs to Amazon CloudWatch Logs>
-- in the /Amazon RDS User Guide/.
--
-- 'storageType', 'restoreDBInstanceFromDBSnapshot_storageType' - Specifies the storage type to be associated with the DB instance.
--
-- Valid values: @standard | gp2 | io1@
--
-- If you specify @io1@, you must also include a value for the @Iops@
-- parameter.
--
-- Default: @io1@ if the @Iops@ parameter is specified, otherwise @gp2@
--
-- 'useDefaultProcessorFeatures', 'restoreDBInstanceFromDBSnapshot_useDefaultProcessorFeatures' - A value that indicates whether the DB instance class of the DB instance
-- uses its default processor features.
--
-- 'optionGroupName', 'restoreDBInstanceFromDBSnapshot_optionGroupName' - The name of the option group to be used for the restored DB instance.
--
-- Permanent options, such as the TDE option for Oracle Advanced Security
-- TDE, can\'t be removed from an option group, and that option group
-- can\'t be removed from a DB instance once it is associated with a DB
-- instance
--
-- 'domain', 'restoreDBInstanceFromDBSnapshot_domain' - Specify the Active Directory directory ID to restore the DB instance in.
-- The domain must be created prior to this operation. Currently, only
-- MySQL, Microsoft SQL Server, Oracle, and PostgreSQL DB instances can be
-- created in an Active Directory Domain.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/kerberos-authentication.html Kerberos Authentication>
-- in the /Amazon RDS User Guide/.
--
-- 'dbSubnetGroupName', 'restoreDBInstanceFromDBSnapshot_dbSubnetGroupName' - The DB subnet group name to use for the new instance.
--
-- Constraints: If supplied, must match the name of an existing
-- DBSubnetGroup.
--
-- Example: @mySubnetgroup@
--
-- 'multiAZ', 'restoreDBInstanceFromDBSnapshot_multiAZ' - A value that indicates whether the DB instance is a Multi-AZ deployment.
--
-- Constraint: You can\'t specify the @AvailabilityZone@ parameter if the
-- DB instance is a Multi-AZ deployment.
--
-- 'publiclyAccessible', 'restoreDBInstanceFromDBSnapshot_publiclyAccessible' - A value that indicates whether the DB instance is publicly accessible.
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
-- 'vpcSecurityGroupIds', 'restoreDBInstanceFromDBSnapshot_vpcSecurityGroupIds' - A list of EC2 VPC security groups to associate with this DB instance.
--
-- Default: The default EC2 VPC security group for the DB subnet group\'s
-- VPC.
--
-- 'dbParameterGroupName', 'restoreDBInstanceFromDBSnapshot_dbParameterGroupName' - The name of the DB parameter group to associate with this DB instance.
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
-- 'availabilityZone', 'restoreDBInstanceFromDBSnapshot_availabilityZone' - The Availability Zone (AZ) where the DB instance will be created.
--
-- Default: A random, system-chosen Availability Zone.
--
-- Constraint: You can\'t specify the @AvailabilityZone@ parameter if the
-- DB instance is a Multi-AZ deployment.
--
-- Example: @us-east-1a@
--
-- 'licenseModel', 'restoreDBInstanceFromDBSnapshot_licenseModel' - License model information for the restored DB instance.
--
-- Default: Same as source.
--
-- Valid values: @license-included@ | @bring-your-own-license@ |
-- @general-public-license@
--
-- 'tdeCredentialPassword', 'restoreDBInstanceFromDBSnapshot_tdeCredentialPassword' - The password for the given ARN from the key store in order to access the
-- device.
--
-- 'tags', 'restoreDBInstanceFromDBSnapshot_tags' - Undocumented member.
--
-- 'processorFeatures', 'restoreDBInstanceFromDBSnapshot_processorFeatures' - The number of CPU cores and the number of threads per core for the DB
-- instance class of the DB instance.
--
-- 'port', 'restoreDBInstanceFromDBSnapshot_port' - The port number on which the database accepts connections.
--
-- Default: The same port as the original DB instance
--
-- Constraints: Value must be @1150-65535@
--
-- 'dbInstanceClass', 'restoreDBInstanceFromDBSnapshot_dbInstanceClass' - The compute and memory capacity of the Amazon RDS DB instance, for
-- example, @db.m4.large@. Not all DB instance classes are available in all
-- AWS Regions, or for all database engines. For the full list of DB
-- instance classes, and availability for your engine, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Concepts.DBInstanceClass.html DB Instance Class>
-- in the /Amazon RDS User Guide./
--
-- Default: The same DBInstanceClass as the original DB instance.
--
-- 'dbName', 'restoreDBInstanceFromDBSnapshot_dbName' - The database name for the restored DB instance.
--
-- This parameter doesn\'t apply to the MySQL, PostgreSQL, or MariaDB
-- engines.
--
-- 'domainIAMRoleName', 'restoreDBInstanceFromDBSnapshot_domainIAMRoleName' - Specify the name of the IAM role to be used when making API calls to the
-- Directory Service.
--
-- 'engine', 'restoreDBInstanceFromDBSnapshot_engine' - The database engine to use for the new instance.
--
-- Default: The same as source
--
-- Constraint: Must be compatible with the engine of the source. For
-- example, you can restore a MariaDB 10.1 DB instance from a MySQL 5.6
-- snapshot.
--
-- Valid Values:
--
-- -   @mariadb@
--
-- -   @mysql@
--
-- -   @oracle-ee@
--
-- -   @oracle-se2@
--
-- -   @oracle-se1@
--
-- -   @oracle-se@
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
-- 'tdeCredentialArn', 'restoreDBInstanceFromDBSnapshot_tdeCredentialArn' - The ARN from the key store with which to associate the instance for TDE
-- encryption.
--
-- 'enableCustomerOwnedIp', 'restoreDBInstanceFromDBSnapshot_enableCustomerOwnedIp' - A value that indicates whether to enable a customer-owned IP address
-- (CoIP) for an RDS on Outposts DB instance.
--
-- A /CoIP/ provides local or external connectivity to resources in your
-- Outpost subnets through your on-premises network. For some use cases, a
-- CoIP can provide lower latency for connections to the DB instance from
-- outside of its virtual private cloud (VPC) on your local network.
--
-- For more information about RDS on Outposts, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/rds-on-outposts.html Working with Amazon RDS on AWS Outposts>
-- in the /Amazon RDS User Guide/.
--
-- For more information about CoIPs, see
-- <https://docs.aws.amazon.com/outposts/latest/userguide/outposts-networking-components.html#ip-addressing Customer-owned IP addresses>
-- in the /AWS Outposts User Guide/.
--
-- 'copyTagsToSnapshot', 'restoreDBInstanceFromDBSnapshot_copyTagsToSnapshot' - A value that indicates whether to copy all tags from the restored DB
-- instance to snapshots of the DB instance. By default, tags are not
-- copied.
--
-- 'iops', 'restoreDBInstanceFromDBSnapshot_iops' - Specifies the amount of provisioned IOPS for the DB instance, expressed
-- in I\/O operations per second. If this parameter isn\'t specified, the
-- IOPS value is taken from the backup. If this parameter is set to 0, the
-- new instance is converted to a non-PIOPS instance. The conversion takes
-- additional time, though your DB instance is available for connections
-- before the conversion starts.
--
-- The provisioned IOPS value must follow the requirements for your
-- database engine. For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/CHAP_Storage.html#USER_PIOPS Amazon RDS Provisioned IOPS Storage to Improve Performance>
-- in the /Amazon RDS User Guide./
--
-- Constraints: Must be an integer greater than 1000.
--
-- 'autoMinorVersionUpgrade', 'restoreDBInstanceFromDBSnapshot_autoMinorVersionUpgrade' - A value that indicates whether minor version upgrades are applied
-- automatically to the DB instance during the maintenance window.
--
-- 'dbInstanceIdentifier', 'restoreDBInstanceFromDBSnapshot_dbInstanceIdentifier' - Name of the DB instance to create from the DB snapshot. This parameter
-- isn\'t case-sensitive.
--
-- Constraints:
--
-- -   Must contain from 1 to 63 numbers, letters, or hyphens
--
-- -   First character must be a letter
--
-- -   Can\'t end with a hyphen or contain two consecutive hyphens
--
-- Example: @my-snapshot-id@
--
-- 'dbSnapshotIdentifier', 'restoreDBInstanceFromDBSnapshot_dbSnapshotIdentifier' - The identifier for the DB snapshot to restore from.
--
-- Constraints:
--
-- -   Must match the identifier of an existing DBSnapshot.
--
-- -   If you are restoring from a shared manual DB snapshot, the
--     @DBSnapshotIdentifier@ must be the ARN of the shared DB snapshot.
newRestoreDBInstanceFromDBSnapshot ::
  -- | 'dbInstanceIdentifier'
  Prelude.Text ->
  -- | 'dbSnapshotIdentifier'
  Prelude.Text ->
  RestoreDBInstanceFromDBSnapshot
newRestoreDBInstanceFromDBSnapshot
  pDBInstanceIdentifier_
  pDBSnapshotIdentifier_ =
    RestoreDBInstanceFromDBSnapshot'
      { deletionProtection =
          Prelude.Nothing,
        enableIAMDatabaseAuthentication =
          Prelude.Nothing,
        enableCloudwatchLogsExports =
          Prelude.Nothing,
        storageType = Prelude.Nothing,
        useDefaultProcessorFeatures =
          Prelude.Nothing,
        optionGroupName = Prelude.Nothing,
        domain = Prelude.Nothing,
        dbSubnetGroupName = Prelude.Nothing,
        multiAZ = Prelude.Nothing,
        publiclyAccessible = Prelude.Nothing,
        vpcSecurityGroupIds = Prelude.Nothing,
        dbParameterGroupName = Prelude.Nothing,
        availabilityZone = Prelude.Nothing,
        licenseModel = Prelude.Nothing,
        tdeCredentialPassword = Prelude.Nothing,
        tags = Prelude.Nothing,
        processorFeatures = Prelude.Nothing,
        port = Prelude.Nothing,
        dbInstanceClass = Prelude.Nothing,
        dbName = Prelude.Nothing,
        domainIAMRoleName = Prelude.Nothing,
        engine = Prelude.Nothing,
        tdeCredentialArn = Prelude.Nothing,
        enableCustomerOwnedIp = Prelude.Nothing,
        copyTagsToSnapshot = Prelude.Nothing,
        iops = Prelude.Nothing,
        autoMinorVersionUpgrade = Prelude.Nothing,
        dbInstanceIdentifier =
          pDBInstanceIdentifier_,
        dbSnapshotIdentifier =
          pDBSnapshotIdentifier_
      }

-- | A value that indicates whether the DB instance has deletion protection
-- enabled. The database can\'t be deleted when deletion protection is
-- enabled. By default, deletion protection is disabled. For more
-- information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_DeleteInstance.html Deleting a DB Instance>.
restoreDBInstanceFromDBSnapshot_deletionProtection :: Lens.Lens' RestoreDBInstanceFromDBSnapshot (Prelude.Maybe Prelude.Bool)
restoreDBInstanceFromDBSnapshot_deletionProtection = Lens.lens (\RestoreDBInstanceFromDBSnapshot' {deletionProtection} -> deletionProtection) (\s@RestoreDBInstanceFromDBSnapshot' {} a -> s {deletionProtection = a} :: RestoreDBInstanceFromDBSnapshot)

-- | A value that indicates whether to enable mapping of AWS Identity and
-- Access Management (IAM) accounts to database accounts. By default,
-- mapping is disabled.
--
-- For more information about IAM database authentication, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/UsingWithRDS.IAMDBAuth.html IAM Database Authentication for MySQL and PostgreSQL>
-- in the /Amazon RDS User Guide./
restoreDBInstanceFromDBSnapshot_enableIAMDatabaseAuthentication :: Lens.Lens' RestoreDBInstanceFromDBSnapshot (Prelude.Maybe Prelude.Bool)
restoreDBInstanceFromDBSnapshot_enableIAMDatabaseAuthentication = Lens.lens (\RestoreDBInstanceFromDBSnapshot' {enableIAMDatabaseAuthentication} -> enableIAMDatabaseAuthentication) (\s@RestoreDBInstanceFromDBSnapshot' {} a -> s {enableIAMDatabaseAuthentication = a} :: RestoreDBInstanceFromDBSnapshot)

-- | The list of logs that the restored DB instance is to export to
-- CloudWatch Logs. The values in the list depend on the DB engine being
-- used. For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_LogAccess.html#USER_LogAccess.Procedural.UploadtoCloudWatch Publishing Database Logs to Amazon CloudWatch Logs>
-- in the /Amazon RDS User Guide/.
restoreDBInstanceFromDBSnapshot_enableCloudwatchLogsExports :: Lens.Lens' RestoreDBInstanceFromDBSnapshot (Prelude.Maybe [Prelude.Text])
restoreDBInstanceFromDBSnapshot_enableCloudwatchLogsExports = Lens.lens (\RestoreDBInstanceFromDBSnapshot' {enableCloudwatchLogsExports} -> enableCloudwatchLogsExports) (\s@RestoreDBInstanceFromDBSnapshot' {} a -> s {enableCloudwatchLogsExports = a} :: RestoreDBInstanceFromDBSnapshot) Prelude.. Lens.mapping Lens._Coerce

-- | Specifies the storage type to be associated with the DB instance.
--
-- Valid values: @standard | gp2 | io1@
--
-- If you specify @io1@, you must also include a value for the @Iops@
-- parameter.
--
-- Default: @io1@ if the @Iops@ parameter is specified, otherwise @gp2@
restoreDBInstanceFromDBSnapshot_storageType :: Lens.Lens' RestoreDBInstanceFromDBSnapshot (Prelude.Maybe Prelude.Text)
restoreDBInstanceFromDBSnapshot_storageType = Lens.lens (\RestoreDBInstanceFromDBSnapshot' {storageType} -> storageType) (\s@RestoreDBInstanceFromDBSnapshot' {} a -> s {storageType = a} :: RestoreDBInstanceFromDBSnapshot)

-- | A value that indicates whether the DB instance class of the DB instance
-- uses its default processor features.
restoreDBInstanceFromDBSnapshot_useDefaultProcessorFeatures :: Lens.Lens' RestoreDBInstanceFromDBSnapshot (Prelude.Maybe Prelude.Bool)
restoreDBInstanceFromDBSnapshot_useDefaultProcessorFeatures = Lens.lens (\RestoreDBInstanceFromDBSnapshot' {useDefaultProcessorFeatures} -> useDefaultProcessorFeatures) (\s@RestoreDBInstanceFromDBSnapshot' {} a -> s {useDefaultProcessorFeatures = a} :: RestoreDBInstanceFromDBSnapshot)

-- | The name of the option group to be used for the restored DB instance.
--
-- Permanent options, such as the TDE option for Oracle Advanced Security
-- TDE, can\'t be removed from an option group, and that option group
-- can\'t be removed from a DB instance once it is associated with a DB
-- instance
restoreDBInstanceFromDBSnapshot_optionGroupName :: Lens.Lens' RestoreDBInstanceFromDBSnapshot (Prelude.Maybe Prelude.Text)
restoreDBInstanceFromDBSnapshot_optionGroupName = Lens.lens (\RestoreDBInstanceFromDBSnapshot' {optionGroupName} -> optionGroupName) (\s@RestoreDBInstanceFromDBSnapshot' {} a -> s {optionGroupName = a} :: RestoreDBInstanceFromDBSnapshot)

-- | Specify the Active Directory directory ID to restore the DB instance in.
-- The domain must be created prior to this operation. Currently, only
-- MySQL, Microsoft SQL Server, Oracle, and PostgreSQL DB instances can be
-- created in an Active Directory Domain.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/kerberos-authentication.html Kerberos Authentication>
-- in the /Amazon RDS User Guide/.
restoreDBInstanceFromDBSnapshot_domain :: Lens.Lens' RestoreDBInstanceFromDBSnapshot (Prelude.Maybe Prelude.Text)
restoreDBInstanceFromDBSnapshot_domain = Lens.lens (\RestoreDBInstanceFromDBSnapshot' {domain} -> domain) (\s@RestoreDBInstanceFromDBSnapshot' {} a -> s {domain = a} :: RestoreDBInstanceFromDBSnapshot)

-- | The DB subnet group name to use for the new instance.
--
-- Constraints: If supplied, must match the name of an existing
-- DBSubnetGroup.
--
-- Example: @mySubnetgroup@
restoreDBInstanceFromDBSnapshot_dbSubnetGroupName :: Lens.Lens' RestoreDBInstanceFromDBSnapshot (Prelude.Maybe Prelude.Text)
restoreDBInstanceFromDBSnapshot_dbSubnetGroupName = Lens.lens (\RestoreDBInstanceFromDBSnapshot' {dbSubnetGroupName} -> dbSubnetGroupName) (\s@RestoreDBInstanceFromDBSnapshot' {} a -> s {dbSubnetGroupName = a} :: RestoreDBInstanceFromDBSnapshot)

-- | A value that indicates whether the DB instance is a Multi-AZ deployment.
--
-- Constraint: You can\'t specify the @AvailabilityZone@ parameter if the
-- DB instance is a Multi-AZ deployment.
restoreDBInstanceFromDBSnapshot_multiAZ :: Lens.Lens' RestoreDBInstanceFromDBSnapshot (Prelude.Maybe Prelude.Bool)
restoreDBInstanceFromDBSnapshot_multiAZ = Lens.lens (\RestoreDBInstanceFromDBSnapshot' {multiAZ} -> multiAZ) (\s@RestoreDBInstanceFromDBSnapshot' {} a -> s {multiAZ = a} :: RestoreDBInstanceFromDBSnapshot)

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
restoreDBInstanceFromDBSnapshot_publiclyAccessible :: Lens.Lens' RestoreDBInstanceFromDBSnapshot (Prelude.Maybe Prelude.Bool)
restoreDBInstanceFromDBSnapshot_publiclyAccessible = Lens.lens (\RestoreDBInstanceFromDBSnapshot' {publiclyAccessible} -> publiclyAccessible) (\s@RestoreDBInstanceFromDBSnapshot' {} a -> s {publiclyAccessible = a} :: RestoreDBInstanceFromDBSnapshot)

-- | A list of EC2 VPC security groups to associate with this DB instance.
--
-- Default: The default EC2 VPC security group for the DB subnet group\'s
-- VPC.
restoreDBInstanceFromDBSnapshot_vpcSecurityGroupIds :: Lens.Lens' RestoreDBInstanceFromDBSnapshot (Prelude.Maybe [Prelude.Text])
restoreDBInstanceFromDBSnapshot_vpcSecurityGroupIds = Lens.lens (\RestoreDBInstanceFromDBSnapshot' {vpcSecurityGroupIds} -> vpcSecurityGroupIds) (\s@RestoreDBInstanceFromDBSnapshot' {} a -> s {vpcSecurityGroupIds = a} :: RestoreDBInstanceFromDBSnapshot) Prelude.. Lens.mapping Lens._Coerce

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
restoreDBInstanceFromDBSnapshot_dbParameterGroupName :: Lens.Lens' RestoreDBInstanceFromDBSnapshot (Prelude.Maybe Prelude.Text)
restoreDBInstanceFromDBSnapshot_dbParameterGroupName = Lens.lens (\RestoreDBInstanceFromDBSnapshot' {dbParameterGroupName} -> dbParameterGroupName) (\s@RestoreDBInstanceFromDBSnapshot' {} a -> s {dbParameterGroupName = a} :: RestoreDBInstanceFromDBSnapshot)

-- | The Availability Zone (AZ) where the DB instance will be created.
--
-- Default: A random, system-chosen Availability Zone.
--
-- Constraint: You can\'t specify the @AvailabilityZone@ parameter if the
-- DB instance is a Multi-AZ deployment.
--
-- Example: @us-east-1a@
restoreDBInstanceFromDBSnapshot_availabilityZone :: Lens.Lens' RestoreDBInstanceFromDBSnapshot (Prelude.Maybe Prelude.Text)
restoreDBInstanceFromDBSnapshot_availabilityZone = Lens.lens (\RestoreDBInstanceFromDBSnapshot' {availabilityZone} -> availabilityZone) (\s@RestoreDBInstanceFromDBSnapshot' {} a -> s {availabilityZone = a} :: RestoreDBInstanceFromDBSnapshot)

-- | License model information for the restored DB instance.
--
-- Default: Same as source.
--
-- Valid values: @license-included@ | @bring-your-own-license@ |
-- @general-public-license@
restoreDBInstanceFromDBSnapshot_licenseModel :: Lens.Lens' RestoreDBInstanceFromDBSnapshot (Prelude.Maybe Prelude.Text)
restoreDBInstanceFromDBSnapshot_licenseModel = Lens.lens (\RestoreDBInstanceFromDBSnapshot' {licenseModel} -> licenseModel) (\s@RestoreDBInstanceFromDBSnapshot' {} a -> s {licenseModel = a} :: RestoreDBInstanceFromDBSnapshot)

-- | The password for the given ARN from the key store in order to access the
-- device.
restoreDBInstanceFromDBSnapshot_tdeCredentialPassword :: Lens.Lens' RestoreDBInstanceFromDBSnapshot (Prelude.Maybe Prelude.Text)
restoreDBInstanceFromDBSnapshot_tdeCredentialPassword = Lens.lens (\RestoreDBInstanceFromDBSnapshot' {tdeCredentialPassword} -> tdeCredentialPassword) (\s@RestoreDBInstanceFromDBSnapshot' {} a -> s {tdeCredentialPassword = a} :: RestoreDBInstanceFromDBSnapshot)

-- | Undocumented member.
restoreDBInstanceFromDBSnapshot_tags :: Lens.Lens' RestoreDBInstanceFromDBSnapshot (Prelude.Maybe [Tag])
restoreDBInstanceFromDBSnapshot_tags = Lens.lens (\RestoreDBInstanceFromDBSnapshot' {tags} -> tags) (\s@RestoreDBInstanceFromDBSnapshot' {} a -> s {tags = a} :: RestoreDBInstanceFromDBSnapshot) Prelude.. Lens.mapping Lens._Coerce

-- | The number of CPU cores and the number of threads per core for the DB
-- instance class of the DB instance.
restoreDBInstanceFromDBSnapshot_processorFeatures :: Lens.Lens' RestoreDBInstanceFromDBSnapshot (Prelude.Maybe [ProcessorFeature])
restoreDBInstanceFromDBSnapshot_processorFeatures = Lens.lens (\RestoreDBInstanceFromDBSnapshot' {processorFeatures} -> processorFeatures) (\s@RestoreDBInstanceFromDBSnapshot' {} a -> s {processorFeatures = a} :: RestoreDBInstanceFromDBSnapshot) Prelude.. Lens.mapping Lens._Coerce

-- | The port number on which the database accepts connections.
--
-- Default: The same port as the original DB instance
--
-- Constraints: Value must be @1150-65535@
restoreDBInstanceFromDBSnapshot_port :: Lens.Lens' RestoreDBInstanceFromDBSnapshot (Prelude.Maybe Prelude.Int)
restoreDBInstanceFromDBSnapshot_port = Lens.lens (\RestoreDBInstanceFromDBSnapshot' {port} -> port) (\s@RestoreDBInstanceFromDBSnapshot' {} a -> s {port = a} :: RestoreDBInstanceFromDBSnapshot)

-- | The compute and memory capacity of the Amazon RDS DB instance, for
-- example, @db.m4.large@. Not all DB instance classes are available in all
-- AWS Regions, or for all database engines. For the full list of DB
-- instance classes, and availability for your engine, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Concepts.DBInstanceClass.html DB Instance Class>
-- in the /Amazon RDS User Guide./
--
-- Default: The same DBInstanceClass as the original DB instance.
restoreDBInstanceFromDBSnapshot_dbInstanceClass :: Lens.Lens' RestoreDBInstanceFromDBSnapshot (Prelude.Maybe Prelude.Text)
restoreDBInstanceFromDBSnapshot_dbInstanceClass = Lens.lens (\RestoreDBInstanceFromDBSnapshot' {dbInstanceClass} -> dbInstanceClass) (\s@RestoreDBInstanceFromDBSnapshot' {} a -> s {dbInstanceClass = a} :: RestoreDBInstanceFromDBSnapshot)

-- | The database name for the restored DB instance.
--
-- This parameter doesn\'t apply to the MySQL, PostgreSQL, or MariaDB
-- engines.
restoreDBInstanceFromDBSnapshot_dbName :: Lens.Lens' RestoreDBInstanceFromDBSnapshot (Prelude.Maybe Prelude.Text)
restoreDBInstanceFromDBSnapshot_dbName = Lens.lens (\RestoreDBInstanceFromDBSnapshot' {dbName} -> dbName) (\s@RestoreDBInstanceFromDBSnapshot' {} a -> s {dbName = a} :: RestoreDBInstanceFromDBSnapshot)

-- | Specify the name of the IAM role to be used when making API calls to the
-- Directory Service.
restoreDBInstanceFromDBSnapshot_domainIAMRoleName :: Lens.Lens' RestoreDBInstanceFromDBSnapshot (Prelude.Maybe Prelude.Text)
restoreDBInstanceFromDBSnapshot_domainIAMRoleName = Lens.lens (\RestoreDBInstanceFromDBSnapshot' {domainIAMRoleName} -> domainIAMRoleName) (\s@RestoreDBInstanceFromDBSnapshot' {} a -> s {domainIAMRoleName = a} :: RestoreDBInstanceFromDBSnapshot)

-- | The database engine to use for the new instance.
--
-- Default: The same as source
--
-- Constraint: Must be compatible with the engine of the source. For
-- example, you can restore a MariaDB 10.1 DB instance from a MySQL 5.6
-- snapshot.
--
-- Valid Values:
--
-- -   @mariadb@
--
-- -   @mysql@
--
-- -   @oracle-ee@
--
-- -   @oracle-se2@
--
-- -   @oracle-se1@
--
-- -   @oracle-se@
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
restoreDBInstanceFromDBSnapshot_engine :: Lens.Lens' RestoreDBInstanceFromDBSnapshot (Prelude.Maybe Prelude.Text)
restoreDBInstanceFromDBSnapshot_engine = Lens.lens (\RestoreDBInstanceFromDBSnapshot' {engine} -> engine) (\s@RestoreDBInstanceFromDBSnapshot' {} a -> s {engine = a} :: RestoreDBInstanceFromDBSnapshot)

-- | The ARN from the key store with which to associate the instance for TDE
-- encryption.
restoreDBInstanceFromDBSnapshot_tdeCredentialArn :: Lens.Lens' RestoreDBInstanceFromDBSnapshot (Prelude.Maybe Prelude.Text)
restoreDBInstanceFromDBSnapshot_tdeCredentialArn = Lens.lens (\RestoreDBInstanceFromDBSnapshot' {tdeCredentialArn} -> tdeCredentialArn) (\s@RestoreDBInstanceFromDBSnapshot' {} a -> s {tdeCredentialArn = a} :: RestoreDBInstanceFromDBSnapshot)

-- | A value that indicates whether to enable a customer-owned IP address
-- (CoIP) for an RDS on Outposts DB instance.
--
-- A /CoIP/ provides local or external connectivity to resources in your
-- Outpost subnets through your on-premises network. For some use cases, a
-- CoIP can provide lower latency for connections to the DB instance from
-- outside of its virtual private cloud (VPC) on your local network.
--
-- For more information about RDS on Outposts, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/rds-on-outposts.html Working with Amazon RDS on AWS Outposts>
-- in the /Amazon RDS User Guide/.
--
-- For more information about CoIPs, see
-- <https://docs.aws.amazon.com/outposts/latest/userguide/outposts-networking-components.html#ip-addressing Customer-owned IP addresses>
-- in the /AWS Outposts User Guide/.
restoreDBInstanceFromDBSnapshot_enableCustomerOwnedIp :: Lens.Lens' RestoreDBInstanceFromDBSnapshot (Prelude.Maybe Prelude.Bool)
restoreDBInstanceFromDBSnapshot_enableCustomerOwnedIp = Lens.lens (\RestoreDBInstanceFromDBSnapshot' {enableCustomerOwnedIp} -> enableCustomerOwnedIp) (\s@RestoreDBInstanceFromDBSnapshot' {} a -> s {enableCustomerOwnedIp = a} :: RestoreDBInstanceFromDBSnapshot)

-- | A value that indicates whether to copy all tags from the restored DB
-- instance to snapshots of the DB instance. By default, tags are not
-- copied.
restoreDBInstanceFromDBSnapshot_copyTagsToSnapshot :: Lens.Lens' RestoreDBInstanceFromDBSnapshot (Prelude.Maybe Prelude.Bool)
restoreDBInstanceFromDBSnapshot_copyTagsToSnapshot = Lens.lens (\RestoreDBInstanceFromDBSnapshot' {copyTagsToSnapshot} -> copyTagsToSnapshot) (\s@RestoreDBInstanceFromDBSnapshot' {} a -> s {copyTagsToSnapshot = a} :: RestoreDBInstanceFromDBSnapshot)

-- | Specifies the amount of provisioned IOPS for the DB instance, expressed
-- in I\/O operations per second. If this parameter isn\'t specified, the
-- IOPS value is taken from the backup. If this parameter is set to 0, the
-- new instance is converted to a non-PIOPS instance. The conversion takes
-- additional time, though your DB instance is available for connections
-- before the conversion starts.
--
-- The provisioned IOPS value must follow the requirements for your
-- database engine. For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/CHAP_Storage.html#USER_PIOPS Amazon RDS Provisioned IOPS Storage to Improve Performance>
-- in the /Amazon RDS User Guide./
--
-- Constraints: Must be an integer greater than 1000.
restoreDBInstanceFromDBSnapshot_iops :: Lens.Lens' RestoreDBInstanceFromDBSnapshot (Prelude.Maybe Prelude.Int)
restoreDBInstanceFromDBSnapshot_iops = Lens.lens (\RestoreDBInstanceFromDBSnapshot' {iops} -> iops) (\s@RestoreDBInstanceFromDBSnapshot' {} a -> s {iops = a} :: RestoreDBInstanceFromDBSnapshot)

-- | A value that indicates whether minor version upgrades are applied
-- automatically to the DB instance during the maintenance window.
restoreDBInstanceFromDBSnapshot_autoMinorVersionUpgrade :: Lens.Lens' RestoreDBInstanceFromDBSnapshot (Prelude.Maybe Prelude.Bool)
restoreDBInstanceFromDBSnapshot_autoMinorVersionUpgrade = Lens.lens (\RestoreDBInstanceFromDBSnapshot' {autoMinorVersionUpgrade} -> autoMinorVersionUpgrade) (\s@RestoreDBInstanceFromDBSnapshot' {} a -> s {autoMinorVersionUpgrade = a} :: RestoreDBInstanceFromDBSnapshot)

-- | Name of the DB instance to create from the DB snapshot. This parameter
-- isn\'t case-sensitive.
--
-- Constraints:
--
-- -   Must contain from 1 to 63 numbers, letters, or hyphens
--
-- -   First character must be a letter
--
-- -   Can\'t end with a hyphen or contain two consecutive hyphens
--
-- Example: @my-snapshot-id@
restoreDBInstanceFromDBSnapshot_dbInstanceIdentifier :: Lens.Lens' RestoreDBInstanceFromDBSnapshot Prelude.Text
restoreDBInstanceFromDBSnapshot_dbInstanceIdentifier = Lens.lens (\RestoreDBInstanceFromDBSnapshot' {dbInstanceIdentifier} -> dbInstanceIdentifier) (\s@RestoreDBInstanceFromDBSnapshot' {} a -> s {dbInstanceIdentifier = a} :: RestoreDBInstanceFromDBSnapshot)

-- | The identifier for the DB snapshot to restore from.
--
-- Constraints:
--
-- -   Must match the identifier of an existing DBSnapshot.
--
-- -   If you are restoring from a shared manual DB snapshot, the
--     @DBSnapshotIdentifier@ must be the ARN of the shared DB snapshot.
restoreDBInstanceFromDBSnapshot_dbSnapshotIdentifier :: Lens.Lens' RestoreDBInstanceFromDBSnapshot Prelude.Text
restoreDBInstanceFromDBSnapshot_dbSnapshotIdentifier = Lens.lens (\RestoreDBInstanceFromDBSnapshot' {dbSnapshotIdentifier} -> dbSnapshotIdentifier) (\s@RestoreDBInstanceFromDBSnapshot' {} a -> s {dbSnapshotIdentifier = a} :: RestoreDBInstanceFromDBSnapshot)

instance
  Core.AWSRequest
    RestoreDBInstanceFromDBSnapshot
  where
  type
    AWSResponse RestoreDBInstanceFromDBSnapshot =
      RestoreDBInstanceFromDBSnapshotResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "RestoreDBInstanceFromDBSnapshotResult"
      ( \s h x ->
          RestoreDBInstanceFromDBSnapshotResponse'
            Prelude.<$> (x Core..@? "DBInstance")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    RestoreDBInstanceFromDBSnapshot

instance
  Prelude.NFData
    RestoreDBInstanceFromDBSnapshot

instance
  Core.ToHeaders
    RestoreDBInstanceFromDBSnapshot
  where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath RestoreDBInstanceFromDBSnapshot where
  toPath = Prelude.const "/"

instance Core.ToQuery RestoreDBInstanceFromDBSnapshot where
  toQuery RestoreDBInstanceFromDBSnapshot' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ( "RestoreDBInstanceFromDBSnapshot" ::
                      Prelude.ByteString
                  ),
        "Version"
          Core.=: ("2014-10-31" :: Prelude.ByteString),
        "DeletionProtection" Core.=: deletionProtection,
        "EnableIAMDatabaseAuthentication"
          Core.=: enableIAMDatabaseAuthentication,
        "EnableCloudwatchLogsExports"
          Core.=: Core.toQuery
            ( Core.toQueryList "member"
                Prelude.<$> enableCloudwatchLogsExports
            ),
        "StorageType" Core.=: storageType,
        "UseDefaultProcessorFeatures"
          Core.=: useDefaultProcessorFeatures,
        "OptionGroupName" Core.=: optionGroupName,
        "Domain" Core.=: domain,
        "DBSubnetGroupName" Core.=: dbSubnetGroupName,
        "MultiAZ" Core.=: multiAZ,
        "PubliclyAccessible" Core.=: publiclyAccessible,
        "VpcSecurityGroupIds"
          Core.=: Core.toQuery
            ( Core.toQueryList "VpcSecurityGroupId"
                Prelude.<$> vpcSecurityGroupIds
            ),
        "DBParameterGroupName" Core.=: dbParameterGroupName,
        "AvailabilityZone" Core.=: availabilityZone,
        "LicenseModel" Core.=: licenseModel,
        "TdeCredentialPassword"
          Core.=: tdeCredentialPassword,
        "Tags"
          Core.=: Core.toQuery
            (Core.toQueryList "Tag" Prelude.<$> tags),
        "ProcessorFeatures"
          Core.=: Core.toQuery
            ( Core.toQueryList "ProcessorFeature"
                Prelude.<$> processorFeatures
            ),
        "Port" Core.=: port,
        "DBInstanceClass" Core.=: dbInstanceClass,
        "DBName" Core.=: dbName,
        "DomainIAMRoleName" Core.=: domainIAMRoleName,
        "Engine" Core.=: engine,
        "TdeCredentialArn" Core.=: tdeCredentialArn,
        "EnableCustomerOwnedIp"
          Core.=: enableCustomerOwnedIp,
        "CopyTagsToSnapshot" Core.=: copyTagsToSnapshot,
        "Iops" Core.=: iops,
        "AutoMinorVersionUpgrade"
          Core.=: autoMinorVersionUpgrade,
        "DBInstanceIdentifier" Core.=: dbInstanceIdentifier,
        "DBSnapshotIdentifier" Core.=: dbSnapshotIdentifier
      ]

-- | /See:/ 'newRestoreDBInstanceFromDBSnapshotResponse' smart constructor.
data RestoreDBInstanceFromDBSnapshotResponse = RestoreDBInstanceFromDBSnapshotResponse'
  { dbInstance :: Prelude.Maybe DBInstance,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RestoreDBInstanceFromDBSnapshotResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dbInstance', 'restoreDBInstanceFromDBSnapshotResponse_dbInstance' - Undocumented member.
--
-- 'httpStatus', 'restoreDBInstanceFromDBSnapshotResponse_httpStatus' - The response's http status code.
newRestoreDBInstanceFromDBSnapshotResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  RestoreDBInstanceFromDBSnapshotResponse
newRestoreDBInstanceFromDBSnapshotResponse
  pHttpStatus_ =
    RestoreDBInstanceFromDBSnapshotResponse'
      { dbInstance =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Undocumented member.
restoreDBInstanceFromDBSnapshotResponse_dbInstance :: Lens.Lens' RestoreDBInstanceFromDBSnapshotResponse (Prelude.Maybe DBInstance)
restoreDBInstanceFromDBSnapshotResponse_dbInstance = Lens.lens (\RestoreDBInstanceFromDBSnapshotResponse' {dbInstance} -> dbInstance) (\s@RestoreDBInstanceFromDBSnapshotResponse' {} a -> s {dbInstance = a} :: RestoreDBInstanceFromDBSnapshotResponse)

-- | The response's http status code.
restoreDBInstanceFromDBSnapshotResponse_httpStatus :: Lens.Lens' RestoreDBInstanceFromDBSnapshotResponse Prelude.Int
restoreDBInstanceFromDBSnapshotResponse_httpStatus = Lens.lens (\RestoreDBInstanceFromDBSnapshotResponse' {httpStatus} -> httpStatus) (\s@RestoreDBInstanceFromDBSnapshotResponse' {} a -> s {httpStatus = a} :: RestoreDBInstanceFromDBSnapshotResponse)

instance
  Prelude.NFData
    RestoreDBInstanceFromDBSnapshotResponse
