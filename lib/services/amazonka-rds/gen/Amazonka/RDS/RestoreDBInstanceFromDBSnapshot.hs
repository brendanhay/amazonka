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
-- Module      : Amazonka.RDS.RestoreDBInstanceFromDBSnapshot
-- Copyright   : (c) 2013-2022 Brendan Hay
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
module Amazonka.RDS.RestoreDBInstanceFromDBSnapshot
  ( -- * Creating a Request
    RestoreDBInstanceFromDBSnapshot (..),
    newRestoreDBInstanceFromDBSnapshot,

    -- * Request Lenses
    restoreDBInstanceFromDBSnapshot_tags,
    restoreDBInstanceFromDBSnapshot_port,
    restoreDBInstanceFromDBSnapshot_vpcSecurityGroupIds,
    restoreDBInstanceFromDBSnapshot_dbParameterGroupName,
    restoreDBInstanceFromDBSnapshot_backupTarget,
    restoreDBInstanceFromDBSnapshot_storageThroughput,
    restoreDBInstanceFromDBSnapshot_dbInstanceClass,
    restoreDBInstanceFromDBSnapshot_copyTagsToSnapshot,
    restoreDBInstanceFromDBSnapshot_domainIAMRoleName,
    restoreDBInstanceFromDBSnapshot_dbClusterSnapshotIdentifier,
    restoreDBInstanceFromDBSnapshot_dbSubnetGroupName,
    restoreDBInstanceFromDBSnapshot_autoMinorVersionUpgrade,
    restoreDBInstanceFromDBSnapshot_dbSnapshotIdentifier,
    restoreDBInstanceFromDBSnapshot_domain,
    restoreDBInstanceFromDBSnapshot_optionGroupName,
    restoreDBInstanceFromDBSnapshot_enableIAMDatabaseAuthentication,
    restoreDBInstanceFromDBSnapshot_tdeCredentialPassword,
    restoreDBInstanceFromDBSnapshot_availabilityZone,
    restoreDBInstanceFromDBSnapshot_publiclyAccessible,
    restoreDBInstanceFromDBSnapshot_storageType,
    restoreDBInstanceFromDBSnapshot_enableCloudwatchLogsExports,
    restoreDBInstanceFromDBSnapshot_processorFeatures,
    restoreDBInstanceFromDBSnapshot_tdeCredentialArn,
    restoreDBInstanceFromDBSnapshot_engine,
    restoreDBInstanceFromDBSnapshot_deletionProtection,
    restoreDBInstanceFromDBSnapshot_customIamInstanceProfile,
    restoreDBInstanceFromDBSnapshot_iops,
    restoreDBInstanceFromDBSnapshot_dbName,
    restoreDBInstanceFromDBSnapshot_networkType,
    restoreDBInstanceFromDBSnapshot_multiAZ,
    restoreDBInstanceFromDBSnapshot_enableCustomerOwnedIp,
    restoreDBInstanceFromDBSnapshot_licenseModel,
    restoreDBInstanceFromDBSnapshot_useDefaultProcessorFeatures,
    restoreDBInstanceFromDBSnapshot_dbInstanceIdentifier,

    -- * Destructuring the Response
    RestoreDBInstanceFromDBSnapshotResponse (..),
    newRestoreDBInstanceFromDBSnapshotResponse,

    -- * Response Lenses
    restoreDBInstanceFromDBSnapshotResponse_dbInstance,
    restoreDBInstanceFromDBSnapshotResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.RDS.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- |
--
-- /See:/ 'newRestoreDBInstanceFromDBSnapshot' smart constructor.
data RestoreDBInstanceFromDBSnapshot = RestoreDBInstanceFromDBSnapshot'
  { tags :: Prelude.Maybe [Tag],
    -- | The port number on which the database accepts connections.
    --
    -- Default: The same port as the original DB instance
    --
    -- Constraints: Value must be @1150-65535@
    port :: Prelude.Maybe Prelude.Int,
    -- | A list of EC2 VPC security groups to associate with this DB instance.
    --
    -- Default: The default EC2 VPC security group for the DB subnet group\'s
    -- VPC.
    vpcSecurityGroupIds :: Prelude.Maybe [Prelude.Text],
    -- | The name of the DB parameter group to associate with this DB instance.
    --
    -- If you don\'t specify a value for @DBParameterGroupName@, then RDS uses
    -- the default @DBParameterGroup@ for the specified DB engine.
    --
    -- This setting doesn\'t apply to RDS Custom.
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
    -- | Specifies where automated backups and manual snapshots are stored for
    -- the restored DB instance.
    --
    -- Possible values are @outposts@ (Amazon Web Services Outposts) and
    -- @region@ (Amazon Web Services Region). The default is @region@.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/rds-on-outposts.html Working with Amazon RDS on Amazon Web Services Outposts>
    -- in the /Amazon RDS User Guide/.
    backupTarget :: Prelude.Maybe Prelude.Text,
    -- | Specifies the storage throughput value for the DB instance.
    --
    -- This setting doesn\'t apply to RDS Custom or Amazon Aurora.
    storageThroughput :: Prelude.Maybe Prelude.Int,
    -- | The compute and memory capacity of the Amazon RDS DB instance, for
    -- example db.m4.large. Not all DB instance classes are available in all
    -- Amazon Web Services Regions, or for all database engines. For the full
    -- list of DB instance classes, and availability for your engine, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Concepts.DBInstanceClass.html DB Instance Class>
    -- in the /Amazon RDS User Guide./
    --
    -- Default: The same DBInstanceClass as the original DB instance.
    dbInstanceClass :: Prelude.Maybe Prelude.Text,
    -- | A value that indicates whether to copy all tags from the restored DB
    -- instance to snapshots of the DB instance.
    --
    -- In most cases, tags aren\'t copied by default. However, when you restore
    -- a DB instance from a DB snapshot, RDS checks whether you specify new
    -- tags. If yes, the new tags are added to the restored DB instance. If
    -- there are no new tags, RDS looks for the tags from the source DB
    -- instance for the DB snapshot, and then adds those tags to the restored
    -- DB instance.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_Tagging.html#USER_Tagging.CopyTags Copying tags to DB instance snapshots>
    -- in the /Amazon RDS User Guide/.
    copyTagsToSnapshot :: Prelude.Maybe Prelude.Bool,
    -- | Specify the name of the IAM role to be used when making API calls to the
    -- Directory Service.
    --
    -- This setting doesn\'t apply to RDS Custom.
    domainIAMRoleName :: Prelude.Maybe Prelude.Text,
    -- | The identifier for the RDS for MySQL Multi-AZ DB cluster snapshot to
    -- restore from.
    --
    -- For more information on Multi-AZ DB clusters, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/multi-az-db-clusters-concepts.html Multi-AZ deployments with two readable standby DB instances>
    -- in the /Amazon RDS User Guide/.
    --
    -- Constraints:
    --
    -- -   Must match the identifier of an existing Multi-AZ DB cluster
    --     snapshot.
    --
    -- -   Can\'t be specified when @DBSnapshotIdentifier@ is specified.
    --
    -- -   Must be specified when @DBSnapshotIdentifier@ isn\'t specified.
    --
    -- -   If you are restoring from a shared manual Multi-AZ DB cluster
    --     snapshot, the @DBClusterSnapshotIdentifier@ must be the ARN of the
    --     shared snapshot.
    --
    -- -   Can\'t be the identifier of an Aurora DB cluster snapshot.
    --
    -- -   Can\'t be the identifier of an RDS for PostgreSQL Multi-AZ DB
    --     cluster snapshot.
    dbClusterSnapshotIdentifier :: Prelude.Maybe Prelude.Text,
    -- | The DB subnet group name to use for the new instance.
    --
    -- Constraints: If supplied, must match the name of an existing
    -- DBSubnetGroup.
    --
    -- Example: @mydbsubnetgroup@
    dbSubnetGroupName :: Prelude.Maybe Prelude.Text,
    -- | A value that indicates whether minor version upgrades are applied
    -- automatically to the DB instance during the maintenance window.
    --
    -- If you restore an RDS Custom DB instance, you must disable this
    -- parameter.
    autoMinorVersionUpgrade :: Prelude.Maybe Prelude.Bool,
    -- | The identifier for the DB snapshot to restore from.
    --
    -- Constraints:
    --
    -- -   Must match the identifier of an existing DBSnapshot.
    --
    -- -   Can\'t be specified when @DBClusterSnapshotIdentifier@ is specified.
    --
    -- -   Must be specified when @DBClusterSnapshotIdentifier@ isn\'t
    --     specified.
    --
    -- -   If you are restoring from a shared manual DB snapshot, the
    --     @DBSnapshotIdentifier@ must be the ARN of the shared DB snapshot.
    dbSnapshotIdentifier :: Prelude.Maybe Prelude.Text,
    -- | Specify the Active Directory directory ID to restore the DB instance in.
    -- The domain\/ must be created prior to this operation. Currently, you can
    -- create only MySQL, Microsoft SQL Server, Oracle, and PostgreSQL DB
    -- instances in an Active Directory Domain.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/kerberos-authentication.html Kerberos Authentication>
    -- in the /Amazon RDS User Guide/.
    --
    -- This setting doesn\'t apply to RDS Custom.
    domain :: Prelude.Maybe Prelude.Text,
    -- | The name of the option group to be used for the restored DB instance.
    --
    -- Permanent options, such as the TDE option for Oracle Advanced Security
    -- TDE, can\'t be removed from an option group, and that option group
    -- can\'t be removed from a DB instance after it is associated with a DB
    -- instance.
    --
    -- This setting doesn\'t apply to RDS Custom.
    optionGroupName :: Prelude.Maybe Prelude.Text,
    -- | A value that indicates whether to enable mapping of Amazon Web Services
    -- Identity and Access Management (IAM) accounts to database accounts. By
    -- default, mapping is disabled.
    --
    -- For more information about IAM database authentication, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/UsingWithRDS.IAMDBAuth.html IAM Database Authentication for MySQL and PostgreSQL>
    -- in the /Amazon RDS User Guide./
    --
    -- This setting doesn\'t apply to RDS Custom.
    enableIAMDatabaseAuthentication :: Prelude.Maybe Prelude.Bool,
    -- | The password for the given ARN from the key store in order to access the
    -- device.
    --
    -- This setting doesn\'t apply to RDS Custom.
    tdeCredentialPassword :: Prelude.Maybe Prelude.Text,
    -- | The Availability Zone (AZ) where the DB instance will be created.
    --
    -- Default: A random, system-chosen Availability Zone.
    --
    -- Constraint: You can\'t specify the @AvailabilityZone@ parameter if the
    -- DB instance is a Multi-AZ deployment.
    --
    -- Example: @us-east-1a@
    availabilityZone :: Prelude.Maybe Prelude.Text,
    -- | A value that indicates whether the DB instance is publicly accessible.
    --
    -- When the DB instance is publicly accessible, its Domain Name System
    -- (DNS) endpoint resolves to the private IP address from within the DB
    -- instance\'s virtual private cloud (VPC). It resolves to the public IP
    -- address from outside of the DB instance\'s VPC. Access to the DB
    -- instance is ultimately controlled by the security group it uses. That
    -- public access is not permitted if the security group assigned to the DB
    -- instance doesn\'t permit it.
    --
    -- When the DB instance isn\'t publicly accessible, it is an internal DB
    -- instance with a DNS name that resolves to a private IP address.
    --
    -- For more information, see CreateDBInstance.
    publiclyAccessible :: Prelude.Maybe Prelude.Bool,
    -- | Specifies the storage type to be associated with the DB instance.
    --
    -- Valid values: @gp2 | gp3 | io1 | standard@
    --
    -- If you specify @io1@ or @gp3@, you must also include a value for the
    -- @Iops@ parameter.
    --
    -- Default: @io1@ if the @Iops@ parameter is specified, otherwise @gp2@
    storageType :: Prelude.Maybe Prelude.Text,
    -- | The list of logs that the restored DB instance is to export to
    -- CloudWatch Logs. The values in the list depend on the DB engine being
    -- used. For more information, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_LogAccess.html#USER_LogAccess.Procedural.UploadtoCloudWatch Publishing Database Logs to Amazon CloudWatch Logs>
    -- in the /Amazon RDS User Guide/.
    --
    -- This setting doesn\'t apply to RDS Custom.
    enableCloudwatchLogsExports :: Prelude.Maybe [Prelude.Text],
    -- | The number of CPU cores and the number of threads per core for the DB
    -- instance class of the DB instance.
    --
    -- This setting doesn\'t apply to RDS Custom.
    processorFeatures :: Prelude.Maybe [ProcessorFeature],
    -- | The ARN from the key store with which to associate the instance for TDE
    -- encryption.
    --
    -- This setting doesn\'t apply to RDS Custom.
    tdeCredentialArn :: Prelude.Maybe Prelude.Text,
    -- | The database engine to use for the new instance.
    --
    -- This setting doesn\'t apply to RDS Custom.
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
    -- | A value that indicates whether the DB instance has deletion protection
    -- enabled. The database can\'t be deleted when deletion protection is
    -- enabled. By default, deletion protection isn\'t enabled. For more
    -- information, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_DeleteInstance.html Deleting a DB Instance>.
    deletionProtection :: Prelude.Maybe Prelude.Bool,
    -- | The instance profile associated with the underlying Amazon EC2 instance
    -- of an RDS Custom DB instance. The instance profile must meet the
    -- following requirements:
    --
    -- -   The profile must exist in your account.
    --
    -- -   The profile must have an IAM role that Amazon EC2 has permissions to
    --     assume.
    --
    -- -   The instance profile name and the associated IAM role name must
    --     start with the prefix @AWSRDSCustom@.
    --
    -- For the list of permissions required for the IAM role, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/custom-setup-orcl.html#custom-setup-orcl.iam-vpc Configure IAM and your VPC>
    -- in the /Amazon RDS User Guide/.
    --
    -- This setting is required for RDS Custom.
    customIamInstanceProfile :: Prelude.Maybe Prelude.Text,
    -- | Specifies the amount of provisioned IOPS for the DB instance, expressed
    -- in I\/O operations per second. If this parameter isn\'t specified, the
    -- IOPS value is taken from the backup. If this parameter is set to 0, the
    -- new instance is converted to a non-PIOPS instance. The conversion takes
    -- additional time, though your DB instance is available for connections
    -- before the conversion starts.
    --
    -- The provisioned IOPS value must follow the requirements for your
    -- database engine. For more information, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/CHAP_Storage.html#USER_PIOPS Amazon RDS Provisioned IOPS storage>
    -- in the /Amazon RDS User Guide./
    --
    -- Constraints: Must be an integer greater than 1000.
    iops :: Prelude.Maybe Prelude.Int,
    -- | The database name for the restored DB instance.
    --
    -- This parameter doesn\'t apply to the MySQL, PostgreSQL, or MariaDB
    -- engines. It also doesn\'t apply to RDS Custom DB instances.
    dbName :: Prelude.Maybe Prelude.Text,
    -- | The network type of the DB instance.
    --
    -- Valid values:
    --
    -- -   @IPV4@
    --
    -- -   @DUAL@
    --
    -- The network type is determined by the @DBSubnetGroup@ specified for the
    -- DB instance. A @DBSubnetGroup@ can support only the IPv4 protocol or the
    -- IPv4 and the IPv6 protocols (@DUAL@).
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_VPC.WorkingWithRDSInstanceinaVPC.html Working with a DB instance in a VPC>
    -- in the /Amazon RDS User Guide./
    networkType :: Prelude.Maybe Prelude.Text,
    -- | A value that indicates whether the DB instance is a Multi-AZ deployment.
    --
    -- This setting doesn\'t apply to RDS Custom.
    --
    -- Constraint: You can\'t specify the @AvailabilityZone@ parameter if the
    -- DB instance is a Multi-AZ deployment.
    multiAZ :: Prelude.Maybe Prelude.Bool,
    -- | A value that indicates whether to enable a customer-owned IP address
    -- (CoIP) for an RDS on Outposts DB instance.
    --
    -- A /CoIP/ provides local or external connectivity to resources in your
    -- Outpost subnets through your on-premises network. For some use cases, a
    -- CoIP can provide lower latency for connections to the DB instance from
    -- outside of its virtual private cloud (VPC) on your local network.
    --
    -- This setting doesn\'t apply to RDS Custom.
    --
    -- For more information about RDS on Outposts, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/rds-on-outposts.html Working with Amazon RDS on Amazon Web Services Outposts>
    -- in the /Amazon RDS User Guide/.
    --
    -- For more information about CoIPs, see
    -- <https://docs.aws.amazon.com/outposts/latest/userguide/outposts-networking-components.html#ip-addressing Customer-owned IP addresses>
    -- in the /Amazon Web Services Outposts User Guide/.
    enableCustomerOwnedIp :: Prelude.Maybe Prelude.Bool,
    -- | License model information for the restored DB instance.
    --
    -- This setting doesn\'t apply to RDS Custom.
    --
    -- Default: Same as source.
    --
    -- Valid values: @license-included@ | @bring-your-own-license@ |
    -- @general-public-license@
    licenseModel :: Prelude.Maybe Prelude.Text,
    -- | A value that indicates whether the DB instance class of the DB instance
    -- uses its default processor features.
    --
    -- This setting doesn\'t apply to RDS Custom.
    useDefaultProcessorFeatures :: Prelude.Maybe Prelude.Bool,
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
    dbInstanceIdentifier :: Prelude.Text
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
-- 'tags', 'restoreDBInstanceFromDBSnapshot_tags' - Undocumented member.
--
-- 'port', 'restoreDBInstanceFromDBSnapshot_port' - The port number on which the database accepts connections.
--
-- Default: The same port as the original DB instance
--
-- Constraints: Value must be @1150-65535@
--
-- 'vpcSecurityGroupIds', 'restoreDBInstanceFromDBSnapshot_vpcSecurityGroupIds' - A list of EC2 VPC security groups to associate with this DB instance.
--
-- Default: The default EC2 VPC security group for the DB subnet group\'s
-- VPC.
--
-- 'dbParameterGroupName', 'restoreDBInstanceFromDBSnapshot_dbParameterGroupName' - The name of the DB parameter group to associate with this DB instance.
--
-- If you don\'t specify a value for @DBParameterGroupName@, then RDS uses
-- the default @DBParameterGroup@ for the specified DB engine.
--
-- This setting doesn\'t apply to RDS Custom.
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
-- 'backupTarget', 'restoreDBInstanceFromDBSnapshot_backupTarget' - Specifies where automated backups and manual snapshots are stored for
-- the restored DB instance.
--
-- Possible values are @outposts@ (Amazon Web Services Outposts) and
-- @region@ (Amazon Web Services Region). The default is @region@.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/rds-on-outposts.html Working with Amazon RDS on Amazon Web Services Outposts>
-- in the /Amazon RDS User Guide/.
--
-- 'storageThroughput', 'restoreDBInstanceFromDBSnapshot_storageThroughput' - Specifies the storage throughput value for the DB instance.
--
-- This setting doesn\'t apply to RDS Custom or Amazon Aurora.
--
-- 'dbInstanceClass', 'restoreDBInstanceFromDBSnapshot_dbInstanceClass' - The compute and memory capacity of the Amazon RDS DB instance, for
-- example db.m4.large. Not all DB instance classes are available in all
-- Amazon Web Services Regions, or for all database engines. For the full
-- list of DB instance classes, and availability for your engine, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Concepts.DBInstanceClass.html DB Instance Class>
-- in the /Amazon RDS User Guide./
--
-- Default: The same DBInstanceClass as the original DB instance.
--
-- 'copyTagsToSnapshot', 'restoreDBInstanceFromDBSnapshot_copyTagsToSnapshot' - A value that indicates whether to copy all tags from the restored DB
-- instance to snapshots of the DB instance.
--
-- In most cases, tags aren\'t copied by default. However, when you restore
-- a DB instance from a DB snapshot, RDS checks whether you specify new
-- tags. If yes, the new tags are added to the restored DB instance. If
-- there are no new tags, RDS looks for the tags from the source DB
-- instance for the DB snapshot, and then adds those tags to the restored
-- DB instance.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_Tagging.html#USER_Tagging.CopyTags Copying tags to DB instance snapshots>
-- in the /Amazon RDS User Guide/.
--
-- 'domainIAMRoleName', 'restoreDBInstanceFromDBSnapshot_domainIAMRoleName' - Specify the name of the IAM role to be used when making API calls to the
-- Directory Service.
--
-- This setting doesn\'t apply to RDS Custom.
--
-- 'dbClusterSnapshotIdentifier', 'restoreDBInstanceFromDBSnapshot_dbClusterSnapshotIdentifier' - The identifier for the RDS for MySQL Multi-AZ DB cluster snapshot to
-- restore from.
--
-- For more information on Multi-AZ DB clusters, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/multi-az-db-clusters-concepts.html Multi-AZ deployments with two readable standby DB instances>
-- in the /Amazon RDS User Guide/.
--
-- Constraints:
--
-- -   Must match the identifier of an existing Multi-AZ DB cluster
--     snapshot.
--
-- -   Can\'t be specified when @DBSnapshotIdentifier@ is specified.
--
-- -   Must be specified when @DBSnapshotIdentifier@ isn\'t specified.
--
-- -   If you are restoring from a shared manual Multi-AZ DB cluster
--     snapshot, the @DBClusterSnapshotIdentifier@ must be the ARN of the
--     shared snapshot.
--
-- -   Can\'t be the identifier of an Aurora DB cluster snapshot.
--
-- -   Can\'t be the identifier of an RDS for PostgreSQL Multi-AZ DB
--     cluster snapshot.
--
-- 'dbSubnetGroupName', 'restoreDBInstanceFromDBSnapshot_dbSubnetGroupName' - The DB subnet group name to use for the new instance.
--
-- Constraints: If supplied, must match the name of an existing
-- DBSubnetGroup.
--
-- Example: @mydbsubnetgroup@
--
-- 'autoMinorVersionUpgrade', 'restoreDBInstanceFromDBSnapshot_autoMinorVersionUpgrade' - A value that indicates whether minor version upgrades are applied
-- automatically to the DB instance during the maintenance window.
--
-- If you restore an RDS Custom DB instance, you must disable this
-- parameter.
--
-- 'dbSnapshotIdentifier', 'restoreDBInstanceFromDBSnapshot_dbSnapshotIdentifier' - The identifier for the DB snapshot to restore from.
--
-- Constraints:
--
-- -   Must match the identifier of an existing DBSnapshot.
--
-- -   Can\'t be specified when @DBClusterSnapshotIdentifier@ is specified.
--
-- -   Must be specified when @DBClusterSnapshotIdentifier@ isn\'t
--     specified.
--
-- -   If you are restoring from a shared manual DB snapshot, the
--     @DBSnapshotIdentifier@ must be the ARN of the shared DB snapshot.
--
-- 'domain', 'restoreDBInstanceFromDBSnapshot_domain' - Specify the Active Directory directory ID to restore the DB instance in.
-- The domain\/ must be created prior to this operation. Currently, you can
-- create only MySQL, Microsoft SQL Server, Oracle, and PostgreSQL DB
-- instances in an Active Directory Domain.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/kerberos-authentication.html Kerberos Authentication>
-- in the /Amazon RDS User Guide/.
--
-- This setting doesn\'t apply to RDS Custom.
--
-- 'optionGroupName', 'restoreDBInstanceFromDBSnapshot_optionGroupName' - The name of the option group to be used for the restored DB instance.
--
-- Permanent options, such as the TDE option for Oracle Advanced Security
-- TDE, can\'t be removed from an option group, and that option group
-- can\'t be removed from a DB instance after it is associated with a DB
-- instance.
--
-- This setting doesn\'t apply to RDS Custom.
--
-- 'enableIAMDatabaseAuthentication', 'restoreDBInstanceFromDBSnapshot_enableIAMDatabaseAuthentication' - A value that indicates whether to enable mapping of Amazon Web Services
-- Identity and Access Management (IAM) accounts to database accounts. By
-- default, mapping is disabled.
--
-- For more information about IAM database authentication, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/UsingWithRDS.IAMDBAuth.html IAM Database Authentication for MySQL and PostgreSQL>
-- in the /Amazon RDS User Guide./
--
-- This setting doesn\'t apply to RDS Custom.
--
-- 'tdeCredentialPassword', 'restoreDBInstanceFromDBSnapshot_tdeCredentialPassword' - The password for the given ARN from the key store in order to access the
-- device.
--
-- This setting doesn\'t apply to RDS Custom.
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
-- 'publiclyAccessible', 'restoreDBInstanceFromDBSnapshot_publiclyAccessible' - A value that indicates whether the DB instance is publicly accessible.
--
-- When the DB instance is publicly accessible, its Domain Name System
-- (DNS) endpoint resolves to the private IP address from within the DB
-- instance\'s virtual private cloud (VPC). It resolves to the public IP
-- address from outside of the DB instance\'s VPC. Access to the DB
-- instance is ultimately controlled by the security group it uses. That
-- public access is not permitted if the security group assigned to the DB
-- instance doesn\'t permit it.
--
-- When the DB instance isn\'t publicly accessible, it is an internal DB
-- instance with a DNS name that resolves to a private IP address.
--
-- For more information, see CreateDBInstance.
--
-- 'storageType', 'restoreDBInstanceFromDBSnapshot_storageType' - Specifies the storage type to be associated with the DB instance.
--
-- Valid values: @gp2 | gp3 | io1 | standard@
--
-- If you specify @io1@ or @gp3@, you must also include a value for the
-- @Iops@ parameter.
--
-- Default: @io1@ if the @Iops@ parameter is specified, otherwise @gp2@
--
-- 'enableCloudwatchLogsExports', 'restoreDBInstanceFromDBSnapshot_enableCloudwatchLogsExports' - The list of logs that the restored DB instance is to export to
-- CloudWatch Logs. The values in the list depend on the DB engine being
-- used. For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_LogAccess.html#USER_LogAccess.Procedural.UploadtoCloudWatch Publishing Database Logs to Amazon CloudWatch Logs>
-- in the /Amazon RDS User Guide/.
--
-- This setting doesn\'t apply to RDS Custom.
--
-- 'processorFeatures', 'restoreDBInstanceFromDBSnapshot_processorFeatures' - The number of CPU cores and the number of threads per core for the DB
-- instance class of the DB instance.
--
-- This setting doesn\'t apply to RDS Custom.
--
-- 'tdeCredentialArn', 'restoreDBInstanceFromDBSnapshot_tdeCredentialArn' - The ARN from the key store with which to associate the instance for TDE
-- encryption.
--
-- This setting doesn\'t apply to RDS Custom.
--
-- 'engine', 'restoreDBInstanceFromDBSnapshot_engine' - The database engine to use for the new instance.
--
-- This setting doesn\'t apply to RDS Custom.
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
-- 'deletionProtection', 'restoreDBInstanceFromDBSnapshot_deletionProtection' - A value that indicates whether the DB instance has deletion protection
-- enabled. The database can\'t be deleted when deletion protection is
-- enabled. By default, deletion protection isn\'t enabled. For more
-- information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_DeleteInstance.html Deleting a DB Instance>.
--
-- 'customIamInstanceProfile', 'restoreDBInstanceFromDBSnapshot_customIamInstanceProfile' - The instance profile associated with the underlying Amazon EC2 instance
-- of an RDS Custom DB instance. The instance profile must meet the
-- following requirements:
--
-- -   The profile must exist in your account.
--
-- -   The profile must have an IAM role that Amazon EC2 has permissions to
--     assume.
--
-- -   The instance profile name and the associated IAM role name must
--     start with the prefix @AWSRDSCustom@.
--
-- For the list of permissions required for the IAM role, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/custom-setup-orcl.html#custom-setup-orcl.iam-vpc Configure IAM and your VPC>
-- in the /Amazon RDS User Guide/.
--
-- This setting is required for RDS Custom.
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
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/CHAP_Storage.html#USER_PIOPS Amazon RDS Provisioned IOPS storage>
-- in the /Amazon RDS User Guide./
--
-- Constraints: Must be an integer greater than 1000.
--
-- 'dbName', 'restoreDBInstanceFromDBSnapshot_dbName' - The database name for the restored DB instance.
--
-- This parameter doesn\'t apply to the MySQL, PostgreSQL, or MariaDB
-- engines. It also doesn\'t apply to RDS Custom DB instances.
--
-- 'networkType', 'restoreDBInstanceFromDBSnapshot_networkType' - The network type of the DB instance.
--
-- Valid values:
--
-- -   @IPV4@
--
-- -   @DUAL@
--
-- The network type is determined by the @DBSubnetGroup@ specified for the
-- DB instance. A @DBSubnetGroup@ can support only the IPv4 protocol or the
-- IPv4 and the IPv6 protocols (@DUAL@).
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_VPC.WorkingWithRDSInstanceinaVPC.html Working with a DB instance in a VPC>
-- in the /Amazon RDS User Guide./
--
-- 'multiAZ', 'restoreDBInstanceFromDBSnapshot_multiAZ' - A value that indicates whether the DB instance is a Multi-AZ deployment.
--
-- This setting doesn\'t apply to RDS Custom.
--
-- Constraint: You can\'t specify the @AvailabilityZone@ parameter if the
-- DB instance is a Multi-AZ deployment.
--
-- 'enableCustomerOwnedIp', 'restoreDBInstanceFromDBSnapshot_enableCustomerOwnedIp' - A value that indicates whether to enable a customer-owned IP address
-- (CoIP) for an RDS on Outposts DB instance.
--
-- A /CoIP/ provides local or external connectivity to resources in your
-- Outpost subnets through your on-premises network. For some use cases, a
-- CoIP can provide lower latency for connections to the DB instance from
-- outside of its virtual private cloud (VPC) on your local network.
--
-- This setting doesn\'t apply to RDS Custom.
--
-- For more information about RDS on Outposts, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/rds-on-outposts.html Working with Amazon RDS on Amazon Web Services Outposts>
-- in the /Amazon RDS User Guide/.
--
-- For more information about CoIPs, see
-- <https://docs.aws.amazon.com/outposts/latest/userguide/outposts-networking-components.html#ip-addressing Customer-owned IP addresses>
-- in the /Amazon Web Services Outposts User Guide/.
--
-- 'licenseModel', 'restoreDBInstanceFromDBSnapshot_licenseModel' - License model information for the restored DB instance.
--
-- This setting doesn\'t apply to RDS Custom.
--
-- Default: Same as source.
--
-- Valid values: @license-included@ | @bring-your-own-license@ |
-- @general-public-license@
--
-- 'useDefaultProcessorFeatures', 'restoreDBInstanceFromDBSnapshot_useDefaultProcessorFeatures' - A value that indicates whether the DB instance class of the DB instance
-- uses its default processor features.
--
-- This setting doesn\'t apply to RDS Custom.
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
newRestoreDBInstanceFromDBSnapshot ::
  -- | 'dbInstanceIdentifier'
  Prelude.Text ->
  RestoreDBInstanceFromDBSnapshot
newRestoreDBInstanceFromDBSnapshot
  pDBInstanceIdentifier_ =
    RestoreDBInstanceFromDBSnapshot'
      { tags =
          Prelude.Nothing,
        port = Prelude.Nothing,
        vpcSecurityGroupIds = Prelude.Nothing,
        dbParameterGroupName = Prelude.Nothing,
        backupTarget = Prelude.Nothing,
        storageThroughput = Prelude.Nothing,
        dbInstanceClass = Prelude.Nothing,
        copyTagsToSnapshot = Prelude.Nothing,
        domainIAMRoleName = Prelude.Nothing,
        dbClusterSnapshotIdentifier =
          Prelude.Nothing,
        dbSubnetGroupName = Prelude.Nothing,
        autoMinorVersionUpgrade = Prelude.Nothing,
        dbSnapshotIdentifier = Prelude.Nothing,
        domain = Prelude.Nothing,
        optionGroupName = Prelude.Nothing,
        enableIAMDatabaseAuthentication =
          Prelude.Nothing,
        tdeCredentialPassword = Prelude.Nothing,
        availabilityZone = Prelude.Nothing,
        publiclyAccessible = Prelude.Nothing,
        storageType = Prelude.Nothing,
        enableCloudwatchLogsExports =
          Prelude.Nothing,
        processorFeatures = Prelude.Nothing,
        tdeCredentialArn = Prelude.Nothing,
        engine = Prelude.Nothing,
        deletionProtection = Prelude.Nothing,
        customIamInstanceProfile = Prelude.Nothing,
        iops = Prelude.Nothing,
        dbName = Prelude.Nothing,
        networkType = Prelude.Nothing,
        multiAZ = Prelude.Nothing,
        enableCustomerOwnedIp = Prelude.Nothing,
        licenseModel = Prelude.Nothing,
        useDefaultProcessorFeatures =
          Prelude.Nothing,
        dbInstanceIdentifier =
          pDBInstanceIdentifier_
      }

-- | Undocumented member.
restoreDBInstanceFromDBSnapshot_tags :: Lens.Lens' RestoreDBInstanceFromDBSnapshot (Prelude.Maybe [Tag])
restoreDBInstanceFromDBSnapshot_tags = Lens.lens (\RestoreDBInstanceFromDBSnapshot' {tags} -> tags) (\s@RestoreDBInstanceFromDBSnapshot' {} a -> s {tags = a} :: RestoreDBInstanceFromDBSnapshot) Prelude.. Lens.mapping Lens.coerced

-- | The port number on which the database accepts connections.
--
-- Default: The same port as the original DB instance
--
-- Constraints: Value must be @1150-65535@
restoreDBInstanceFromDBSnapshot_port :: Lens.Lens' RestoreDBInstanceFromDBSnapshot (Prelude.Maybe Prelude.Int)
restoreDBInstanceFromDBSnapshot_port = Lens.lens (\RestoreDBInstanceFromDBSnapshot' {port} -> port) (\s@RestoreDBInstanceFromDBSnapshot' {} a -> s {port = a} :: RestoreDBInstanceFromDBSnapshot)

-- | A list of EC2 VPC security groups to associate with this DB instance.
--
-- Default: The default EC2 VPC security group for the DB subnet group\'s
-- VPC.
restoreDBInstanceFromDBSnapshot_vpcSecurityGroupIds :: Lens.Lens' RestoreDBInstanceFromDBSnapshot (Prelude.Maybe [Prelude.Text])
restoreDBInstanceFromDBSnapshot_vpcSecurityGroupIds = Lens.lens (\RestoreDBInstanceFromDBSnapshot' {vpcSecurityGroupIds} -> vpcSecurityGroupIds) (\s@RestoreDBInstanceFromDBSnapshot' {} a -> s {vpcSecurityGroupIds = a} :: RestoreDBInstanceFromDBSnapshot) Prelude.. Lens.mapping Lens.coerced

-- | The name of the DB parameter group to associate with this DB instance.
--
-- If you don\'t specify a value for @DBParameterGroupName@, then RDS uses
-- the default @DBParameterGroup@ for the specified DB engine.
--
-- This setting doesn\'t apply to RDS Custom.
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

-- | Specifies where automated backups and manual snapshots are stored for
-- the restored DB instance.
--
-- Possible values are @outposts@ (Amazon Web Services Outposts) and
-- @region@ (Amazon Web Services Region). The default is @region@.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/rds-on-outposts.html Working with Amazon RDS on Amazon Web Services Outposts>
-- in the /Amazon RDS User Guide/.
restoreDBInstanceFromDBSnapshot_backupTarget :: Lens.Lens' RestoreDBInstanceFromDBSnapshot (Prelude.Maybe Prelude.Text)
restoreDBInstanceFromDBSnapshot_backupTarget = Lens.lens (\RestoreDBInstanceFromDBSnapshot' {backupTarget} -> backupTarget) (\s@RestoreDBInstanceFromDBSnapshot' {} a -> s {backupTarget = a} :: RestoreDBInstanceFromDBSnapshot)

-- | Specifies the storage throughput value for the DB instance.
--
-- This setting doesn\'t apply to RDS Custom or Amazon Aurora.
restoreDBInstanceFromDBSnapshot_storageThroughput :: Lens.Lens' RestoreDBInstanceFromDBSnapshot (Prelude.Maybe Prelude.Int)
restoreDBInstanceFromDBSnapshot_storageThroughput = Lens.lens (\RestoreDBInstanceFromDBSnapshot' {storageThroughput} -> storageThroughput) (\s@RestoreDBInstanceFromDBSnapshot' {} a -> s {storageThroughput = a} :: RestoreDBInstanceFromDBSnapshot)

-- | The compute and memory capacity of the Amazon RDS DB instance, for
-- example db.m4.large. Not all DB instance classes are available in all
-- Amazon Web Services Regions, or for all database engines. For the full
-- list of DB instance classes, and availability for your engine, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Concepts.DBInstanceClass.html DB Instance Class>
-- in the /Amazon RDS User Guide./
--
-- Default: The same DBInstanceClass as the original DB instance.
restoreDBInstanceFromDBSnapshot_dbInstanceClass :: Lens.Lens' RestoreDBInstanceFromDBSnapshot (Prelude.Maybe Prelude.Text)
restoreDBInstanceFromDBSnapshot_dbInstanceClass = Lens.lens (\RestoreDBInstanceFromDBSnapshot' {dbInstanceClass} -> dbInstanceClass) (\s@RestoreDBInstanceFromDBSnapshot' {} a -> s {dbInstanceClass = a} :: RestoreDBInstanceFromDBSnapshot)

-- | A value that indicates whether to copy all tags from the restored DB
-- instance to snapshots of the DB instance.
--
-- In most cases, tags aren\'t copied by default. However, when you restore
-- a DB instance from a DB snapshot, RDS checks whether you specify new
-- tags. If yes, the new tags are added to the restored DB instance. If
-- there are no new tags, RDS looks for the tags from the source DB
-- instance for the DB snapshot, and then adds those tags to the restored
-- DB instance.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_Tagging.html#USER_Tagging.CopyTags Copying tags to DB instance snapshots>
-- in the /Amazon RDS User Guide/.
restoreDBInstanceFromDBSnapshot_copyTagsToSnapshot :: Lens.Lens' RestoreDBInstanceFromDBSnapshot (Prelude.Maybe Prelude.Bool)
restoreDBInstanceFromDBSnapshot_copyTagsToSnapshot = Lens.lens (\RestoreDBInstanceFromDBSnapshot' {copyTagsToSnapshot} -> copyTagsToSnapshot) (\s@RestoreDBInstanceFromDBSnapshot' {} a -> s {copyTagsToSnapshot = a} :: RestoreDBInstanceFromDBSnapshot)

-- | Specify the name of the IAM role to be used when making API calls to the
-- Directory Service.
--
-- This setting doesn\'t apply to RDS Custom.
restoreDBInstanceFromDBSnapshot_domainIAMRoleName :: Lens.Lens' RestoreDBInstanceFromDBSnapshot (Prelude.Maybe Prelude.Text)
restoreDBInstanceFromDBSnapshot_domainIAMRoleName = Lens.lens (\RestoreDBInstanceFromDBSnapshot' {domainIAMRoleName} -> domainIAMRoleName) (\s@RestoreDBInstanceFromDBSnapshot' {} a -> s {domainIAMRoleName = a} :: RestoreDBInstanceFromDBSnapshot)

-- | The identifier for the RDS for MySQL Multi-AZ DB cluster snapshot to
-- restore from.
--
-- For more information on Multi-AZ DB clusters, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/multi-az-db-clusters-concepts.html Multi-AZ deployments with two readable standby DB instances>
-- in the /Amazon RDS User Guide/.
--
-- Constraints:
--
-- -   Must match the identifier of an existing Multi-AZ DB cluster
--     snapshot.
--
-- -   Can\'t be specified when @DBSnapshotIdentifier@ is specified.
--
-- -   Must be specified when @DBSnapshotIdentifier@ isn\'t specified.
--
-- -   If you are restoring from a shared manual Multi-AZ DB cluster
--     snapshot, the @DBClusterSnapshotIdentifier@ must be the ARN of the
--     shared snapshot.
--
-- -   Can\'t be the identifier of an Aurora DB cluster snapshot.
--
-- -   Can\'t be the identifier of an RDS for PostgreSQL Multi-AZ DB
--     cluster snapshot.
restoreDBInstanceFromDBSnapshot_dbClusterSnapshotIdentifier :: Lens.Lens' RestoreDBInstanceFromDBSnapshot (Prelude.Maybe Prelude.Text)
restoreDBInstanceFromDBSnapshot_dbClusterSnapshotIdentifier = Lens.lens (\RestoreDBInstanceFromDBSnapshot' {dbClusterSnapshotIdentifier} -> dbClusterSnapshotIdentifier) (\s@RestoreDBInstanceFromDBSnapshot' {} a -> s {dbClusterSnapshotIdentifier = a} :: RestoreDBInstanceFromDBSnapshot)

-- | The DB subnet group name to use for the new instance.
--
-- Constraints: If supplied, must match the name of an existing
-- DBSubnetGroup.
--
-- Example: @mydbsubnetgroup@
restoreDBInstanceFromDBSnapshot_dbSubnetGroupName :: Lens.Lens' RestoreDBInstanceFromDBSnapshot (Prelude.Maybe Prelude.Text)
restoreDBInstanceFromDBSnapshot_dbSubnetGroupName = Lens.lens (\RestoreDBInstanceFromDBSnapshot' {dbSubnetGroupName} -> dbSubnetGroupName) (\s@RestoreDBInstanceFromDBSnapshot' {} a -> s {dbSubnetGroupName = a} :: RestoreDBInstanceFromDBSnapshot)

-- | A value that indicates whether minor version upgrades are applied
-- automatically to the DB instance during the maintenance window.
--
-- If you restore an RDS Custom DB instance, you must disable this
-- parameter.
restoreDBInstanceFromDBSnapshot_autoMinorVersionUpgrade :: Lens.Lens' RestoreDBInstanceFromDBSnapshot (Prelude.Maybe Prelude.Bool)
restoreDBInstanceFromDBSnapshot_autoMinorVersionUpgrade = Lens.lens (\RestoreDBInstanceFromDBSnapshot' {autoMinorVersionUpgrade} -> autoMinorVersionUpgrade) (\s@RestoreDBInstanceFromDBSnapshot' {} a -> s {autoMinorVersionUpgrade = a} :: RestoreDBInstanceFromDBSnapshot)

-- | The identifier for the DB snapshot to restore from.
--
-- Constraints:
--
-- -   Must match the identifier of an existing DBSnapshot.
--
-- -   Can\'t be specified when @DBClusterSnapshotIdentifier@ is specified.
--
-- -   Must be specified when @DBClusterSnapshotIdentifier@ isn\'t
--     specified.
--
-- -   If you are restoring from a shared manual DB snapshot, the
--     @DBSnapshotIdentifier@ must be the ARN of the shared DB snapshot.
restoreDBInstanceFromDBSnapshot_dbSnapshotIdentifier :: Lens.Lens' RestoreDBInstanceFromDBSnapshot (Prelude.Maybe Prelude.Text)
restoreDBInstanceFromDBSnapshot_dbSnapshotIdentifier = Lens.lens (\RestoreDBInstanceFromDBSnapshot' {dbSnapshotIdentifier} -> dbSnapshotIdentifier) (\s@RestoreDBInstanceFromDBSnapshot' {} a -> s {dbSnapshotIdentifier = a} :: RestoreDBInstanceFromDBSnapshot)

-- | Specify the Active Directory directory ID to restore the DB instance in.
-- The domain\/ must be created prior to this operation. Currently, you can
-- create only MySQL, Microsoft SQL Server, Oracle, and PostgreSQL DB
-- instances in an Active Directory Domain.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/kerberos-authentication.html Kerberos Authentication>
-- in the /Amazon RDS User Guide/.
--
-- This setting doesn\'t apply to RDS Custom.
restoreDBInstanceFromDBSnapshot_domain :: Lens.Lens' RestoreDBInstanceFromDBSnapshot (Prelude.Maybe Prelude.Text)
restoreDBInstanceFromDBSnapshot_domain = Lens.lens (\RestoreDBInstanceFromDBSnapshot' {domain} -> domain) (\s@RestoreDBInstanceFromDBSnapshot' {} a -> s {domain = a} :: RestoreDBInstanceFromDBSnapshot)

-- | The name of the option group to be used for the restored DB instance.
--
-- Permanent options, such as the TDE option for Oracle Advanced Security
-- TDE, can\'t be removed from an option group, and that option group
-- can\'t be removed from a DB instance after it is associated with a DB
-- instance.
--
-- This setting doesn\'t apply to RDS Custom.
restoreDBInstanceFromDBSnapshot_optionGroupName :: Lens.Lens' RestoreDBInstanceFromDBSnapshot (Prelude.Maybe Prelude.Text)
restoreDBInstanceFromDBSnapshot_optionGroupName = Lens.lens (\RestoreDBInstanceFromDBSnapshot' {optionGroupName} -> optionGroupName) (\s@RestoreDBInstanceFromDBSnapshot' {} a -> s {optionGroupName = a} :: RestoreDBInstanceFromDBSnapshot)

-- | A value that indicates whether to enable mapping of Amazon Web Services
-- Identity and Access Management (IAM) accounts to database accounts. By
-- default, mapping is disabled.
--
-- For more information about IAM database authentication, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/UsingWithRDS.IAMDBAuth.html IAM Database Authentication for MySQL and PostgreSQL>
-- in the /Amazon RDS User Guide./
--
-- This setting doesn\'t apply to RDS Custom.
restoreDBInstanceFromDBSnapshot_enableIAMDatabaseAuthentication :: Lens.Lens' RestoreDBInstanceFromDBSnapshot (Prelude.Maybe Prelude.Bool)
restoreDBInstanceFromDBSnapshot_enableIAMDatabaseAuthentication = Lens.lens (\RestoreDBInstanceFromDBSnapshot' {enableIAMDatabaseAuthentication} -> enableIAMDatabaseAuthentication) (\s@RestoreDBInstanceFromDBSnapshot' {} a -> s {enableIAMDatabaseAuthentication = a} :: RestoreDBInstanceFromDBSnapshot)

-- | The password for the given ARN from the key store in order to access the
-- device.
--
-- This setting doesn\'t apply to RDS Custom.
restoreDBInstanceFromDBSnapshot_tdeCredentialPassword :: Lens.Lens' RestoreDBInstanceFromDBSnapshot (Prelude.Maybe Prelude.Text)
restoreDBInstanceFromDBSnapshot_tdeCredentialPassword = Lens.lens (\RestoreDBInstanceFromDBSnapshot' {tdeCredentialPassword} -> tdeCredentialPassword) (\s@RestoreDBInstanceFromDBSnapshot' {} a -> s {tdeCredentialPassword = a} :: RestoreDBInstanceFromDBSnapshot)

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

-- | A value that indicates whether the DB instance is publicly accessible.
--
-- When the DB instance is publicly accessible, its Domain Name System
-- (DNS) endpoint resolves to the private IP address from within the DB
-- instance\'s virtual private cloud (VPC). It resolves to the public IP
-- address from outside of the DB instance\'s VPC. Access to the DB
-- instance is ultimately controlled by the security group it uses. That
-- public access is not permitted if the security group assigned to the DB
-- instance doesn\'t permit it.
--
-- When the DB instance isn\'t publicly accessible, it is an internal DB
-- instance with a DNS name that resolves to a private IP address.
--
-- For more information, see CreateDBInstance.
restoreDBInstanceFromDBSnapshot_publiclyAccessible :: Lens.Lens' RestoreDBInstanceFromDBSnapshot (Prelude.Maybe Prelude.Bool)
restoreDBInstanceFromDBSnapshot_publiclyAccessible = Lens.lens (\RestoreDBInstanceFromDBSnapshot' {publiclyAccessible} -> publiclyAccessible) (\s@RestoreDBInstanceFromDBSnapshot' {} a -> s {publiclyAccessible = a} :: RestoreDBInstanceFromDBSnapshot)

-- | Specifies the storage type to be associated with the DB instance.
--
-- Valid values: @gp2 | gp3 | io1 | standard@
--
-- If you specify @io1@ or @gp3@, you must also include a value for the
-- @Iops@ parameter.
--
-- Default: @io1@ if the @Iops@ parameter is specified, otherwise @gp2@
restoreDBInstanceFromDBSnapshot_storageType :: Lens.Lens' RestoreDBInstanceFromDBSnapshot (Prelude.Maybe Prelude.Text)
restoreDBInstanceFromDBSnapshot_storageType = Lens.lens (\RestoreDBInstanceFromDBSnapshot' {storageType} -> storageType) (\s@RestoreDBInstanceFromDBSnapshot' {} a -> s {storageType = a} :: RestoreDBInstanceFromDBSnapshot)

-- | The list of logs that the restored DB instance is to export to
-- CloudWatch Logs. The values in the list depend on the DB engine being
-- used. For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_LogAccess.html#USER_LogAccess.Procedural.UploadtoCloudWatch Publishing Database Logs to Amazon CloudWatch Logs>
-- in the /Amazon RDS User Guide/.
--
-- This setting doesn\'t apply to RDS Custom.
restoreDBInstanceFromDBSnapshot_enableCloudwatchLogsExports :: Lens.Lens' RestoreDBInstanceFromDBSnapshot (Prelude.Maybe [Prelude.Text])
restoreDBInstanceFromDBSnapshot_enableCloudwatchLogsExports = Lens.lens (\RestoreDBInstanceFromDBSnapshot' {enableCloudwatchLogsExports} -> enableCloudwatchLogsExports) (\s@RestoreDBInstanceFromDBSnapshot' {} a -> s {enableCloudwatchLogsExports = a} :: RestoreDBInstanceFromDBSnapshot) Prelude.. Lens.mapping Lens.coerced

-- | The number of CPU cores and the number of threads per core for the DB
-- instance class of the DB instance.
--
-- This setting doesn\'t apply to RDS Custom.
restoreDBInstanceFromDBSnapshot_processorFeatures :: Lens.Lens' RestoreDBInstanceFromDBSnapshot (Prelude.Maybe [ProcessorFeature])
restoreDBInstanceFromDBSnapshot_processorFeatures = Lens.lens (\RestoreDBInstanceFromDBSnapshot' {processorFeatures} -> processorFeatures) (\s@RestoreDBInstanceFromDBSnapshot' {} a -> s {processorFeatures = a} :: RestoreDBInstanceFromDBSnapshot) Prelude.. Lens.mapping Lens.coerced

-- | The ARN from the key store with which to associate the instance for TDE
-- encryption.
--
-- This setting doesn\'t apply to RDS Custom.
restoreDBInstanceFromDBSnapshot_tdeCredentialArn :: Lens.Lens' RestoreDBInstanceFromDBSnapshot (Prelude.Maybe Prelude.Text)
restoreDBInstanceFromDBSnapshot_tdeCredentialArn = Lens.lens (\RestoreDBInstanceFromDBSnapshot' {tdeCredentialArn} -> tdeCredentialArn) (\s@RestoreDBInstanceFromDBSnapshot' {} a -> s {tdeCredentialArn = a} :: RestoreDBInstanceFromDBSnapshot)

-- | The database engine to use for the new instance.
--
-- This setting doesn\'t apply to RDS Custom.
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
restoreDBInstanceFromDBSnapshot_engine :: Lens.Lens' RestoreDBInstanceFromDBSnapshot (Prelude.Maybe Prelude.Text)
restoreDBInstanceFromDBSnapshot_engine = Lens.lens (\RestoreDBInstanceFromDBSnapshot' {engine} -> engine) (\s@RestoreDBInstanceFromDBSnapshot' {} a -> s {engine = a} :: RestoreDBInstanceFromDBSnapshot)

-- | A value that indicates whether the DB instance has deletion protection
-- enabled. The database can\'t be deleted when deletion protection is
-- enabled. By default, deletion protection isn\'t enabled. For more
-- information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_DeleteInstance.html Deleting a DB Instance>.
restoreDBInstanceFromDBSnapshot_deletionProtection :: Lens.Lens' RestoreDBInstanceFromDBSnapshot (Prelude.Maybe Prelude.Bool)
restoreDBInstanceFromDBSnapshot_deletionProtection = Lens.lens (\RestoreDBInstanceFromDBSnapshot' {deletionProtection} -> deletionProtection) (\s@RestoreDBInstanceFromDBSnapshot' {} a -> s {deletionProtection = a} :: RestoreDBInstanceFromDBSnapshot)

-- | The instance profile associated with the underlying Amazon EC2 instance
-- of an RDS Custom DB instance. The instance profile must meet the
-- following requirements:
--
-- -   The profile must exist in your account.
--
-- -   The profile must have an IAM role that Amazon EC2 has permissions to
--     assume.
--
-- -   The instance profile name and the associated IAM role name must
--     start with the prefix @AWSRDSCustom@.
--
-- For the list of permissions required for the IAM role, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/custom-setup-orcl.html#custom-setup-orcl.iam-vpc Configure IAM and your VPC>
-- in the /Amazon RDS User Guide/.
--
-- This setting is required for RDS Custom.
restoreDBInstanceFromDBSnapshot_customIamInstanceProfile :: Lens.Lens' RestoreDBInstanceFromDBSnapshot (Prelude.Maybe Prelude.Text)
restoreDBInstanceFromDBSnapshot_customIamInstanceProfile = Lens.lens (\RestoreDBInstanceFromDBSnapshot' {customIamInstanceProfile} -> customIamInstanceProfile) (\s@RestoreDBInstanceFromDBSnapshot' {} a -> s {customIamInstanceProfile = a} :: RestoreDBInstanceFromDBSnapshot)

-- | Specifies the amount of provisioned IOPS for the DB instance, expressed
-- in I\/O operations per second. If this parameter isn\'t specified, the
-- IOPS value is taken from the backup. If this parameter is set to 0, the
-- new instance is converted to a non-PIOPS instance. The conversion takes
-- additional time, though your DB instance is available for connections
-- before the conversion starts.
--
-- The provisioned IOPS value must follow the requirements for your
-- database engine. For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/CHAP_Storage.html#USER_PIOPS Amazon RDS Provisioned IOPS storage>
-- in the /Amazon RDS User Guide./
--
-- Constraints: Must be an integer greater than 1000.
restoreDBInstanceFromDBSnapshot_iops :: Lens.Lens' RestoreDBInstanceFromDBSnapshot (Prelude.Maybe Prelude.Int)
restoreDBInstanceFromDBSnapshot_iops = Lens.lens (\RestoreDBInstanceFromDBSnapshot' {iops} -> iops) (\s@RestoreDBInstanceFromDBSnapshot' {} a -> s {iops = a} :: RestoreDBInstanceFromDBSnapshot)

-- | The database name for the restored DB instance.
--
-- This parameter doesn\'t apply to the MySQL, PostgreSQL, or MariaDB
-- engines. It also doesn\'t apply to RDS Custom DB instances.
restoreDBInstanceFromDBSnapshot_dbName :: Lens.Lens' RestoreDBInstanceFromDBSnapshot (Prelude.Maybe Prelude.Text)
restoreDBInstanceFromDBSnapshot_dbName = Lens.lens (\RestoreDBInstanceFromDBSnapshot' {dbName} -> dbName) (\s@RestoreDBInstanceFromDBSnapshot' {} a -> s {dbName = a} :: RestoreDBInstanceFromDBSnapshot)

-- | The network type of the DB instance.
--
-- Valid values:
--
-- -   @IPV4@
--
-- -   @DUAL@
--
-- The network type is determined by the @DBSubnetGroup@ specified for the
-- DB instance. A @DBSubnetGroup@ can support only the IPv4 protocol or the
-- IPv4 and the IPv6 protocols (@DUAL@).
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_VPC.WorkingWithRDSInstanceinaVPC.html Working with a DB instance in a VPC>
-- in the /Amazon RDS User Guide./
restoreDBInstanceFromDBSnapshot_networkType :: Lens.Lens' RestoreDBInstanceFromDBSnapshot (Prelude.Maybe Prelude.Text)
restoreDBInstanceFromDBSnapshot_networkType = Lens.lens (\RestoreDBInstanceFromDBSnapshot' {networkType} -> networkType) (\s@RestoreDBInstanceFromDBSnapshot' {} a -> s {networkType = a} :: RestoreDBInstanceFromDBSnapshot)

-- | A value that indicates whether the DB instance is a Multi-AZ deployment.
--
-- This setting doesn\'t apply to RDS Custom.
--
-- Constraint: You can\'t specify the @AvailabilityZone@ parameter if the
-- DB instance is a Multi-AZ deployment.
restoreDBInstanceFromDBSnapshot_multiAZ :: Lens.Lens' RestoreDBInstanceFromDBSnapshot (Prelude.Maybe Prelude.Bool)
restoreDBInstanceFromDBSnapshot_multiAZ = Lens.lens (\RestoreDBInstanceFromDBSnapshot' {multiAZ} -> multiAZ) (\s@RestoreDBInstanceFromDBSnapshot' {} a -> s {multiAZ = a} :: RestoreDBInstanceFromDBSnapshot)

-- | A value that indicates whether to enable a customer-owned IP address
-- (CoIP) for an RDS on Outposts DB instance.
--
-- A /CoIP/ provides local or external connectivity to resources in your
-- Outpost subnets through your on-premises network. For some use cases, a
-- CoIP can provide lower latency for connections to the DB instance from
-- outside of its virtual private cloud (VPC) on your local network.
--
-- This setting doesn\'t apply to RDS Custom.
--
-- For more information about RDS on Outposts, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/rds-on-outposts.html Working with Amazon RDS on Amazon Web Services Outposts>
-- in the /Amazon RDS User Guide/.
--
-- For more information about CoIPs, see
-- <https://docs.aws.amazon.com/outposts/latest/userguide/outposts-networking-components.html#ip-addressing Customer-owned IP addresses>
-- in the /Amazon Web Services Outposts User Guide/.
restoreDBInstanceFromDBSnapshot_enableCustomerOwnedIp :: Lens.Lens' RestoreDBInstanceFromDBSnapshot (Prelude.Maybe Prelude.Bool)
restoreDBInstanceFromDBSnapshot_enableCustomerOwnedIp = Lens.lens (\RestoreDBInstanceFromDBSnapshot' {enableCustomerOwnedIp} -> enableCustomerOwnedIp) (\s@RestoreDBInstanceFromDBSnapshot' {} a -> s {enableCustomerOwnedIp = a} :: RestoreDBInstanceFromDBSnapshot)

-- | License model information for the restored DB instance.
--
-- This setting doesn\'t apply to RDS Custom.
--
-- Default: Same as source.
--
-- Valid values: @license-included@ | @bring-your-own-license@ |
-- @general-public-license@
restoreDBInstanceFromDBSnapshot_licenseModel :: Lens.Lens' RestoreDBInstanceFromDBSnapshot (Prelude.Maybe Prelude.Text)
restoreDBInstanceFromDBSnapshot_licenseModel = Lens.lens (\RestoreDBInstanceFromDBSnapshot' {licenseModel} -> licenseModel) (\s@RestoreDBInstanceFromDBSnapshot' {} a -> s {licenseModel = a} :: RestoreDBInstanceFromDBSnapshot)

-- | A value that indicates whether the DB instance class of the DB instance
-- uses its default processor features.
--
-- This setting doesn\'t apply to RDS Custom.
restoreDBInstanceFromDBSnapshot_useDefaultProcessorFeatures :: Lens.Lens' RestoreDBInstanceFromDBSnapshot (Prelude.Maybe Prelude.Bool)
restoreDBInstanceFromDBSnapshot_useDefaultProcessorFeatures = Lens.lens (\RestoreDBInstanceFromDBSnapshot' {useDefaultProcessorFeatures} -> useDefaultProcessorFeatures) (\s@RestoreDBInstanceFromDBSnapshot' {} a -> s {useDefaultProcessorFeatures = a} :: RestoreDBInstanceFromDBSnapshot)

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

instance
  Core.AWSRequest
    RestoreDBInstanceFromDBSnapshot
  where
  type
    AWSResponse RestoreDBInstanceFromDBSnapshot =
      RestoreDBInstanceFromDBSnapshotResponse
  request overrides =
    Request.postQuery (overrides defaultService)
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
  where
  hashWithSalt
    _salt
    RestoreDBInstanceFromDBSnapshot' {..} =
      _salt `Prelude.hashWithSalt` tags
        `Prelude.hashWithSalt` port
        `Prelude.hashWithSalt` vpcSecurityGroupIds
        `Prelude.hashWithSalt` dbParameterGroupName
        `Prelude.hashWithSalt` backupTarget
        `Prelude.hashWithSalt` storageThroughput
        `Prelude.hashWithSalt` dbInstanceClass
        `Prelude.hashWithSalt` copyTagsToSnapshot
        `Prelude.hashWithSalt` domainIAMRoleName
        `Prelude.hashWithSalt` dbClusterSnapshotIdentifier
        `Prelude.hashWithSalt` dbSubnetGroupName
        `Prelude.hashWithSalt` autoMinorVersionUpgrade
        `Prelude.hashWithSalt` dbSnapshotIdentifier
        `Prelude.hashWithSalt` domain
        `Prelude.hashWithSalt` optionGroupName
        `Prelude.hashWithSalt` enableIAMDatabaseAuthentication
        `Prelude.hashWithSalt` tdeCredentialPassword
        `Prelude.hashWithSalt` availabilityZone
        `Prelude.hashWithSalt` publiclyAccessible
        `Prelude.hashWithSalt` storageType
        `Prelude.hashWithSalt` enableCloudwatchLogsExports
        `Prelude.hashWithSalt` processorFeatures
        `Prelude.hashWithSalt` tdeCredentialArn
        `Prelude.hashWithSalt` engine
        `Prelude.hashWithSalt` deletionProtection
        `Prelude.hashWithSalt` customIamInstanceProfile
        `Prelude.hashWithSalt` iops
        `Prelude.hashWithSalt` dbName
        `Prelude.hashWithSalt` networkType
        `Prelude.hashWithSalt` multiAZ
        `Prelude.hashWithSalt` enableCustomerOwnedIp
        `Prelude.hashWithSalt` licenseModel
        `Prelude.hashWithSalt` useDefaultProcessorFeatures
        `Prelude.hashWithSalt` dbInstanceIdentifier

instance
  Prelude.NFData
    RestoreDBInstanceFromDBSnapshot
  where
  rnf RestoreDBInstanceFromDBSnapshot' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf port
      `Prelude.seq` Prelude.rnf vpcSecurityGroupIds
      `Prelude.seq` Prelude.rnf dbParameterGroupName
      `Prelude.seq` Prelude.rnf backupTarget
      `Prelude.seq` Prelude.rnf storageThroughput
      `Prelude.seq` Prelude.rnf dbInstanceClass
      `Prelude.seq` Prelude.rnf copyTagsToSnapshot
      `Prelude.seq` Prelude.rnf domainIAMRoleName
      `Prelude.seq` Prelude.rnf dbClusterSnapshotIdentifier
      `Prelude.seq` Prelude.rnf dbSubnetGroupName
      `Prelude.seq` Prelude.rnf autoMinorVersionUpgrade
      `Prelude.seq` Prelude.rnf dbSnapshotIdentifier
      `Prelude.seq` Prelude.rnf domain
      `Prelude.seq` Prelude.rnf optionGroupName
      `Prelude.seq` Prelude.rnf
        enableIAMDatabaseAuthentication
      `Prelude.seq` Prelude.rnf tdeCredentialPassword
      `Prelude.seq` Prelude.rnf availabilityZone
      `Prelude.seq` Prelude.rnf publiclyAccessible
      `Prelude.seq` Prelude.rnf storageType
      `Prelude.seq` Prelude.rnf
        enableCloudwatchLogsExports
      `Prelude.seq` Prelude.rnf
        processorFeatures
      `Prelude.seq` Prelude.rnf
        tdeCredentialArn
      `Prelude.seq` Prelude.rnf engine
      `Prelude.seq` Prelude.rnf
        deletionProtection
      `Prelude.seq` Prelude.rnf
        customIamInstanceProfile
      `Prelude.seq` Prelude.rnf iops
      `Prelude.seq` Prelude.rnf
        dbName
      `Prelude.seq` Prelude.rnf
        networkType
      `Prelude.seq` Prelude.rnf
        multiAZ
      `Prelude.seq` Prelude.rnf
        enableCustomerOwnedIp
      `Prelude.seq` Prelude.rnf
        licenseModel
      `Prelude.seq` Prelude.rnf
        useDefaultProcessorFeatures
      `Prelude.seq` Prelude.rnf
        dbInstanceIdentifier

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
        "Tags"
          Core.=: Core.toQuery
            (Core.toQueryList "Tag" Prelude.<$> tags),
        "Port" Core.=: port,
        "VpcSecurityGroupIds"
          Core.=: Core.toQuery
            ( Core.toQueryList "VpcSecurityGroupId"
                Prelude.<$> vpcSecurityGroupIds
            ),
        "DBParameterGroupName" Core.=: dbParameterGroupName,
        "BackupTarget" Core.=: backupTarget,
        "StorageThroughput" Core.=: storageThroughput,
        "DBInstanceClass" Core.=: dbInstanceClass,
        "CopyTagsToSnapshot" Core.=: copyTagsToSnapshot,
        "DomainIAMRoleName" Core.=: domainIAMRoleName,
        "DBClusterSnapshotIdentifier"
          Core.=: dbClusterSnapshotIdentifier,
        "DBSubnetGroupName" Core.=: dbSubnetGroupName,
        "AutoMinorVersionUpgrade"
          Core.=: autoMinorVersionUpgrade,
        "DBSnapshotIdentifier" Core.=: dbSnapshotIdentifier,
        "Domain" Core.=: domain,
        "OptionGroupName" Core.=: optionGroupName,
        "EnableIAMDatabaseAuthentication"
          Core.=: enableIAMDatabaseAuthentication,
        "TdeCredentialPassword"
          Core.=: tdeCredentialPassword,
        "AvailabilityZone" Core.=: availabilityZone,
        "PubliclyAccessible" Core.=: publiclyAccessible,
        "StorageType" Core.=: storageType,
        "EnableCloudwatchLogsExports"
          Core.=: Core.toQuery
            ( Core.toQueryList "member"
                Prelude.<$> enableCloudwatchLogsExports
            ),
        "ProcessorFeatures"
          Core.=: Core.toQuery
            ( Core.toQueryList "ProcessorFeature"
                Prelude.<$> processorFeatures
            ),
        "TdeCredentialArn" Core.=: tdeCredentialArn,
        "Engine" Core.=: engine,
        "DeletionProtection" Core.=: deletionProtection,
        "CustomIamInstanceProfile"
          Core.=: customIamInstanceProfile,
        "Iops" Core.=: iops,
        "DBName" Core.=: dbName,
        "NetworkType" Core.=: networkType,
        "MultiAZ" Core.=: multiAZ,
        "EnableCustomerOwnedIp"
          Core.=: enableCustomerOwnedIp,
        "LicenseModel" Core.=: licenseModel,
        "UseDefaultProcessorFeatures"
          Core.=: useDefaultProcessorFeatures,
        "DBInstanceIdentifier" Core.=: dbInstanceIdentifier
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
  where
  rnf RestoreDBInstanceFromDBSnapshotResponse' {..} =
    Prelude.rnf dbInstance
      `Prelude.seq` Prelude.rnf httpStatus
