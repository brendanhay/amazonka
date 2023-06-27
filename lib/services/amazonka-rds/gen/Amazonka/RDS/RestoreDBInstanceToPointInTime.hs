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
-- Module      : Amazonka.RDS.RestoreDBInstanceToPointInTime
-- Copyright   : (c) 2013-2023 Brendan Hay
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
module Amazonka.RDS.RestoreDBInstanceToPointInTime
  ( -- * Creating a Request
    RestoreDBInstanceToPointInTime (..),
    newRestoreDBInstanceToPointInTime,

    -- * Request Lenses
    restoreDBInstanceToPointInTime_allocatedStorage,
    restoreDBInstanceToPointInTime_autoMinorVersionUpgrade,
    restoreDBInstanceToPointInTime_availabilityZone,
    restoreDBInstanceToPointInTime_backupTarget,
    restoreDBInstanceToPointInTime_copyTagsToSnapshot,
    restoreDBInstanceToPointInTime_customIamInstanceProfile,
    restoreDBInstanceToPointInTime_dbInstanceClass,
    restoreDBInstanceToPointInTime_dbName,
    restoreDBInstanceToPointInTime_dbParameterGroupName,
    restoreDBInstanceToPointInTime_dbSubnetGroupName,
    restoreDBInstanceToPointInTime_deletionProtection,
    restoreDBInstanceToPointInTime_domain,
    restoreDBInstanceToPointInTime_domainIAMRoleName,
    restoreDBInstanceToPointInTime_enableCloudwatchLogsExports,
    restoreDBInstanceToPointInTime_enableCustomerOwnedIp,
    restoreDBInstanceToPointInTime_enableIAMDatabaseAuthentication,
    restoreDBInstanceToPointInTime_engine,
    restoreDBInstanceToPointInTime_iops,
    restoreDBInstanceToPointInTime_licenseModel,
    restoreDBInstanceToPointInTime_maxAllocatedStorage,
    restoreDBInstanceToPointInTime_multiAZ,
    restoreDBInstanceToPointInTime_networkType,
    restoreDBInstanceToPointInTime_optionGroupName,
    restoreDBInstanceToPointInTime_port,
    restoreDBInstanceToPointInTime_processorFeatures,
    restoreDBInstanceToPointInTime_publiclyAccessible,
    restoreDBInstanceToPointInTime_restoreTime,
    restoreDBInstanceToPointInTime_sourceDBInstanceAutomatedBackupsArn,
    restoreDBInstanceToPointInTime_sourceDBInstanceIdentifier,
    restoreDBInstanceToPointInTime_sourceDbiResourceId,
    restoreDBInstanceToPointInTime_storageThroughput,
    restoreDBInstanceToPointInTime_storageType,
    restoreDBInstanceToPointInTime_tags,
    restoreDBInstanceToPointInTime_tdeCredentialArn,
    restoreDBInstanceToPointInTime_tdeCredentialPassword,
    restoreDBInstanceToPointInTime_useDefaultProcessorFeatures,
    restoreDBInstanceToPointInTime_useLatestRestorableTime,
    restoreDBInstanceToPointInTime_vpcSecurityGroupIds,
    restoreDBInstanceToPointInTime_targetDBInstanceIdentifier,

    -- * Destructuring the Response
    RestoreDBInstanceToPointInTimeResponse (..),
    newRestoreDBInstanceToPointInTimeResponse,

    -- * Response Lenses
    restoreDBInstanceToPointInTimeResponse_dbInstance,
    restoreDBInstanceToPointInTimeResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.RDS.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- |
--
-- /See:/ 'newRestoreDBInstanceToPointInTime' smart constructor.
data RestoreDBInstanceToPointInTime = RestoreDBInstanceToPointInTime'
  { -- | The amount of storage (in gibibytes) to allocate initially for the DB
    -- instance. Follow the allocation rules specified in @CreateDBInstance@.
    --
    -- Be sure to allocate enough storage for your new DB instance so that the
    -- restore operation can succeed. You can also allocate additional storage
    -- for future growth.
    allocatedStorage :: Prelude.Maybe Prelude.Int,
    -- | A value that indicates whether minor version upgrades are applied
    -- automatically to the DB instance during the maintenance window.
    --
    -- This setting doesn\'t apply to RDS Custom.
    autoMinorVersionUpgrade :: Prelude.Maybe Prelude.Bool,
    -- | The Availability Zone (AZ) where the DB instance will be created.
    --
    -- Default: A random, system-chosen Availability Zone.
    --
    -- Constraint: You can\'t specify the @AvailabilityZone@ parameter if the
    -- DB instance is a Multi-AZ deployment.
    --
    -- Example: @us-east-1a@
    availabilityZone :: Prelude.Maybe Prelude.Text,
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
    -- | A value that indicates whether to copy all tags from the restored DB
    -- instance to snapshots of the DB instance. By default, tags are not
    -- copied.
    copyTagsToSnapshot :: Prelude.Maybe Prelude.Bool,
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
    -- | The compute and memory capacity of the Amazon RDS DB instance, for
    -- example db.m4.large. Not all DB instance classes are available in all
    -- Amazon Web Services Regions, or for all database engines. For the full
    -- list of DB instance classes, and availability for your engine, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Concepts.DBInstanceClass.html DB Instance Class>
    -- in the /Amazon RDS User Guide/.
    --
    -- Default: The same DBInstanceClass as the original DB instance.
    dbInstanceClass :: Prelude.Maybe Prelude.Text,
    -- | The database name for the restored DB instance.
    --
    -- This parameter isn\'t supported for the MySQL or MariaDB engines. It
    -- also doesn\'t apply to RDS Custom.
    dbName :: Prelude.Maybe Prelude.Text,
    -- | The name of the DB parameter group to associate with this DB instance.
    --
    -- If you do not specify a value for @DBParameterGroupName@, then the
    -- default @DBParameterGroup@ for the specified DB engine is used.
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
    -- | The DB subnet group name to use for the new instance.
    --
    -- Constraints: If supplied, must match the name of an existing
    -- DBSubnetGroup.
    --
    -- Example: @mydbsubnetgroup@
    dbSubnetGroupName :: Prelude.Maybe Prelude.Text,
    -- | A value that indicates whether the DB instance has deletion protection
    -- enabled. The database can\'t be deleted when deletion protection is
    -- enabled. By default, deletion protection isn\'t enabled. For more
    -- information, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_DeleteInstance.html Deleting a DB Instance>.
    deletionProtection :: Prelude.Maybe Prelude.Bool,
    -- | Specify the Active Directory directory ID to restore the DB instance in.
    -- Create the domain before running this command. Currently, you can create
    -- only the MySQL, Microsoft SQL Server, Oracle, and PostgreSQL DB
    -- instances in an Active Directory Domain.
    --
    -- This setting doesn\'t apply to RDS Custom.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/kerberos-authentication.html Kerberos Authentication>
    -- in the /Amazon RDS User Guide/.
    domain :: Prelude.Maybe Prelude.Text,
    -- | Specify the name of the IAM role to be used when making API calls to the
    -- Directory Service.
    --
    -- This setting doesn\'t apply to RDS Custom.
    domainIAMRoleName :: Prelude.Maybe Prelude.Text,
    -- | The list of logs that the restored DB instance is to export to
    -- CloudWatch Logs. The values in the list depend on the DB engine being
    -- used. For more information, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_LogAccess.html#USER_LogAccess.Procedural.UploadtoCloudWatch Publishing Database Logs to Amazon CloudWatch Logs>
    -- in the /Amazon RDS User Guide/.
    --
    -- This setting doesn\'t apply to RDS Custom.
    enableCloudwatchLogsExports :: Prelude.Maybe [Prelude.Text],
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
    -- <https://docs.aws.amazon.com/outposts/latest/userguide/routing.html#ip-addressing Customer-owned IP addresses>
    -- in the /Amazon Web Services Outposts User Guide/.
    enableCustomerOwnedIp :: Prelude.Maybe Prelude.Bool,
    -- | A value that indicates whether to enable mapping of Amazon Web Services
    -- Identity and Access Management (IAM) accounts to database accounts. By
    -- default, mapping isn\'t enabled.
    --
    -- This setting doesn\'t apply to RDS Custom.
    --
    -- For more information about IAM database authentication, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/UsingWithRDS.IAMDBAuth.html IAM Database Authentication for MySQL and PostgreSQL>
    -- in the /Amazon RDS User Guide./
    enableIAMDatabaseAuthentication :: Prelude.Maybe Prelude.Bool,
    -- | The database engine to use for the new instance.
    --
    -- This setting doesn\'t apply to RDS Custom.
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
    -- | License model information for the restored DB instance.
    --
    -- This setting doesn\'t apply to RDS Custom.
    --
    -- Default: Same as source.
    --
    -- Valid values: @license-included@ | @bring-your-own-license@ |
    -- @general-public-license@
    licenseModel :: Prelude.Maybe Prelude.Text,
    -- | The upper limit in gibibytes (GiB) to which Amazon RDS can automatically
    -- scale the storage of the DB instance.
    --
    -- For more information about this setting, including limitations that
    -- apply to it, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_PIOPS.StorageTypes.html#USER_PIOPS.Autoscaling Managing capacity automatically with Amazon RDS storage autoscaling>
    -- in the /Amazon RDS User Guide/.
    --
    -- This setting doesn\'t apply to RDS Custom.
    maxAllocatedStorage :: Prelude.Maybe Prelude.Int,
    -- | A value that indicates whether the DB instance is a Multi-AZ deployment.
    --
    -- This setting doesn\'t apply to RDS Custom.
    --
    -- Constraint: You can\'t specify the @AvailabilityZone@ parameter if the
    -- DB instance is a Multi-AZ deployment.
    multiAZ :: Prelude.Maybe Prelude.Bool,
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
    -- | The name of the option group to be used for the restored DB instance.
    --
    -- Permanent options, such as the TDE option for Oracle Advanced Security
    -- TDE, can\'t be removed from an option group, and that option group
    -- can\'t be removed from a DB instance after it is associated with a DB
    -- instance
    --
    -- This setting doesn\'t apply to RDS Custom.
    optionGroupName :: Prelude.Maybe Prelude.Text,
    -- | The port number on which the database accepts connections.
    --
    -- Constraints: Value must be @1150-65535@
    --
    -- Default: The same port as the original DB instance.
    port :: Prelude.Maybe Prelude.Int,
    -- | The number of CPU cores and the number of threads per core for the DB
    -- instance class of the DB instance.
    --
    -- This setting doesn\'t apply to RDS Custom.
    processorFeatures :: Prelude.Maybe [ProcessorFeature],
    -- | A value that indicates whether the DB instance is publicly accessible.
    --
    -- When the DB cluster is publicly accessible, its Domain Name System (DNS)
    -- endpoint resolves to the private IP address from within the DB
    -- cluster\'s virtual private cloud (VPC). It resolves to the public IP
    -- address from outside of the DB cluster\'s VPC. Access to the DB cluster
    -- is ultimately controlled by the security group it uses. That public
    -- access isn\'t permitted if the security group assigned to the DB cluster
    -- doesn\'t permit it.
    --
    -- When the DB instance isn\'t publicly accessible, it is an internal DB
    -- instance with a DNS name that resolves to a private IP address.
    --
    -- For more information, see CreateDBInstance.
    publiclyAccessible :: Prelude.Maybe Prelude.Bool,
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
    restoreTime :: Prelude.Maybe Data.ISO8601,
    -- | The Amazon Resource Name (ARN) of the replicated automated backups from
    -- which to restore, for example,
    -- @arn:aws:rds:useast-1:123456789012:auto-backup:ab-L2IJCEXJP7XQ7HOJ4SIEXAMPLE@.
    --
    -- This setting doesn\'t apply to RDS Custom.
    sourceDBInstanceAutomatedBackupsArn :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the source DB instance from which to restore.
    --
    -- Constraints:
    --
    -- -   Must match the identifier of an existing DB instance.
    sourceDBInstanceIdentifier :: Prelude.Maybe Prelude.Text,
    -- | The resource ID of the source DB instance from which to restore.
    sourceDbiResourceId :: Prelude.Maybe Prelude.Text,
    -- | Specifies the storage throughput value for the DB instance.
    --
    -- This setting doesn\'t apply to RDS Custom or Amazon Aurora.
    storageThroughput :: Prelude.Maybe Prelude.Int,
    -- | Specifies the storage type to be associated with the DB instance.
    --
    -- Valid values: @gp2 | gp3 | io1 | standard@
    --
    -- If you specify @io1@ or @gp3@, you must also include a value for the
    -- @Iops@ parameter.
    --
    -- Default: @io1@ if the @Iops@ parameter is specified, otherwise @gp2@
    storageType :: Prelude.Maybe Prelude.Text,
    tags :: Prelude.Maybe [Tag],
    -- | The ARN from the key store with which to associate the instance for TDE
    -- encryption.
    --
    -- This setting doesn\'t apply to RDS Custom.
    tdeCredentialArn :: Prelude.Maybe Prelude.Text,
    -- | The password for the given ARN from the key store in order to access the
    -- device.
    --
    -- This setting doesn\'t apply to RDS Custom.
    tdeCredentialPassword :: Prelude.Maybe Prelude.Text,
    -- | A value that indicates whether the DB instance class of the DB instance
    -- uses its default processor features.
    --
    -- This setting doesn\'t apply to RDS Custom.
    useDefaultProcessorFeatures :: Prelude.Maybe Prelude.Bool,
    -- | A value that indicates whether the DB instance is restored from the
    -- latest backup time. By default, the DB instance isn\'t restored from the
    -- latest backup time.
    --
    -- Constraints: Can\'t be specified if the @RestoreTime@ parameter is
    -- provided.
    useLatestRestorableTime :: Prelude.Maybe Prelude.Bool,
    -- | A list of EC2 VPC security groups to associate with this DB instance.
    --
    -- Default: The default EC2 VPC security group for the DB subnet group\'s
    -- VPC.
    vpcSecurityGroupIds :: Prelude.Maybe [Prelude.Text],
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
-- 'allocatedStorage', 'restoreDBInstanceToPointInTime_allocatedStorage' - The amount of storage (in gibibytes) to allocate initially for the DB
-- instance. Follow the allocation rules specified in @CreateDBInstance@.
--
-- Be sure to allocate enough storage for your new DB instance so that the
-- restore operation can succeed. You can also allocate additional storage
-- for future growth.
--
-- 'autoMinorVersionUpgrade', 'restoreDBInstanceToPointInTime_autoMinorVersionUpgrade' - A value that indicates whether minor version upgrades are applied
-- automatically to the DB instance during the maintenance window.
--
-- This setting doesn\'t apply to RDS Custom.
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
-- 'backupTarget', 'restoreDBInstanceToPointInTime_backupTarget' - Specifies where automated backups and manual snapshots are stored for
-- the restored DB instance.
--
-- Possible values are @outposts@ (Amazon Web Services Outposts) and
-- @region@ (Amazon Web Services Region). The default is @region@.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/rds-on-outposts.html Working with Amazon RDS on Amazon Web Services Outposts>
-- in the /Amazon RDS User Guide/.
--
-- 'copyTagsToSnapshot', 'restoreDBInstanceToPointInTime_copyTagsToSnapshot' - A value that indicates whether to copy all tags from the restored DB
-- instance to snapshots of the DB instance. By default, tags are not
-- copied.
--
-- 'customIamInstanceProfile', 'restoreDBInstanceToPointInTime_customIamInstanceProfile' - The instance profile associated with the underlying Amazon EC2 instance
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
-- 'dbInstanceClass', 'restoreDBInstanceToPointInTime_dbInstanceClass' - The compute and memory capacity of the Amazon RDS DB instance, for
-- example db.m4.large. Not all DB instance classes are available in all
-- Amazon Web Services Regions, or for all database engines. For the full
-- list of DB instance classes, and availability for your engine, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Concepts.DBInstanceClass.html DB Instance Class>
-- in the /Amazon RDS User Guide/.
--
-- Default: The same DBInstanceClass as the original DB instance.
--
-- 'dbName', 'restoreDBInstanceToPointInTime_dbName' - The database name for the restored DB instance.
--
-- This parameter isn\'t supported for the MySQL or MariaDB engines. It
-- also doesn\'t apply to RDS Custom.
--
-- 'dbParameterGroupName', 'restoreDBInstanceToPointInTime_dbParameterGroupName' - The name of the DB parameter group to associate with this DB instance.
--
-- If you do not specify a value for @DBParameterGroupName@, then the
-- default @DBParameterGroup@ for the specified DB engine is used.
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
-- 'dbSubnetGroupName', 'restoreDBInstanceToPointInTime_dbSubnetGroupName' - The DB subnet group name to use for the new instance.
--
-- Constraints: If supplied, must match the name of an existing
-- DBSubnetGroup.
--
-- Example: @mydbsubnetgroup@
--
-- 'deletionProtection', 'restoreDBInstanceToPointInTime_deletionProtection' - A value that indicates whether the DB instance has deletion protection
-- enabled. The database can\'t be deleted when deletion protection is
-- enabled. By default, deletion protection isn\'t enabled. For more
-- information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_DeleteInstance.html Deleting a DB Instance>.
--
-- 'domain', 'restoreDBInstanceToPointInTime_domain' - Specify the Active Directory directory ID to restore the DB instance in.
-- Create the domain before running this command. Currently, you can create
-- only the MySQL, Microsoft SQL Server, Oracle, and PostgreSQL DB
-- instances in an Active Directory Domain.
--
-- This setting doesn\'t apply to RDS Custom.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/kerberos-authentication.html Kerberos Authentication>
-- in the /Amazon RDS User Guide/.
--
-- 'domainIAMRoleName', 'restoreDBInstanceToPointInTime_domainIAMRoleName' - Specify the name of the IAM role to be used when making API calls to the
-- Directory Service.
--
-- This setting doesn\'t apply to RDS Custom.
--
-- 'enableCloudwatchLogsExports', 'restoreDBInstanceToPointInTime_enableCloudwatchLogsExports' - The list of logs that the restored DB instance is to export to
-- CloudWatch Logs. The values in the list depend on the DB engine being
-- used. For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_LogAccess.html#USER_LogAccess.Procedural.UploadtoCloudWatch Publishing Database Logs to Amazon CloudWatch Logs>
-- in the /Amazon RDS User Guide/.
--
-- This setting doesn\'t apply to RDS Custom.
--
-- 'enableCustomerOwnedIp', 'restoreDBInstanceToPointInTime_enableCustomerOwnedIp' - A value that indicates whether to enable a customer-owned IP address
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
-- <https://docs.aws.amazon.com/outposts/latest/userguide/routing.html#ip-addressing Customer-owned IP addresses>
-- in the /Amazon Web Services Outposts User Guide/.
--
-- 'enableIAMDatabaseAuthentication', 'restoreDBInstanceToPointInTime_enableIAMDatabaseAuthentication' - A value that indicates whether to enable mapping of Amazon Web Services
-- Identity and Access Management (IAM) accounts to database accounts. By
-- default, mapping isn\'t enabled.
--
-- This setting doesn\'t apply to RDS Custom.
--
-- For more information about IAM database authentication, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/UsingWithRDS.IAMDBAuth.html IAM Database Authentication for MySQL and PostgreSQL>
-- in the /Amazon RDS User Guide./
--
-- 'engine', 'restoreDBInstanceToPointInTime_engine' - The database engine to use for the new instance.
--
-- This setting doesn\'t apply to RDS Custom.
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
-- 'licenseModel', 'restoreDBInstanceToPointInTime_licenseModel' - License model information for the restored DB instance.
--
-- This setting doesn\'t apply to RDS Custom.
--
-- Default: Same as source.
--
-- Valid values: @license-included@ | @bring-your-own-license@ |
-- @general-public-license@
--
-- 'maxAllocatedStorage', 'restoreDBInstanceToPointInTime_maxAllocatedStorage' - The upper limit in gibibytes (GiB) to which Amazon RDS can automatically
-- scale the storage of the DB instance.
--
-- For more information about this setting, including limitations that
-- apply to it, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_PIOPS.StorageTypes.html#USER_PIOPS.Autoscaling Managing capacity automatically with Amazon RDS storage autoscaling>
-- in the /Amazon RDS User Guide/.
--
-- This setting doesn\'t apply to RDS Custom.
--
-- 'multiAZ', 'restoreDBInstanceToPointInTime_multiAZ' - A value that indicates whether the DB instance is a Multi-AZ deployment.
--
-- This setting doesn\'t apply to RDS Custom.
--
-- Constraint: You can\'t specify the @AvailabilityZone@ parameter if the
-- DB instance is a Multi-AZ deployment.
--
-- 'networkType', 'restoreDBInstanceToPointInTime_networkType' - The network type of the DB instance.
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
-- 'optionGroupName', 'restoreDBInstanceToPointInTime_optionGroupName' - The name of the option group to be used for the restored DB instance.
--
-- Permanent options, such as the TDE option for Oracle Advanced Security
-- TDE, can\'t be removed from an option group, and that option group
-- can\'t be removed from a DB instance after it is associated with a DB
-- instance
--
-- This setting doesn\'t apply to RDS Custom.
--
-- 'port', 'restoreDBInstanceToPointInTime_port' - The port number on which the database accepts connections.
--
-- Constraints: Value must be @1150-65535@
--
-- Default: The same port as the original DB instance.
--
-- 'processorFeatures', 'restoreDBInstanceToPointInTime_processorFeatures' - The number of CPU cores and the number of threads per core for the DB
-- instance class of the DB instance.
--
-- This setting doesn\'t apply to RDS Custom.
--
-- 'publiclyAccessible', 'restoreDBInstanceToPointInTime_publiclyAccessible' - A value that indicates whether the DB instance is publicly accessible.
--
-- When the DB cluster is publicly accessible, its Domain Name System (DNS)
-- endpoint resolves to the private IP address from within the DB
-- cluster\'s virtual private cloud (VPC). It resolves to the public IP
-- address from outside of the DB cluster\'s VPC. Access to the DB cluster
-- is ultimately controlled by the security group it uses. That public
-- access isn\'t permitted if the security group assigned to the DB cluster
-- doesn\'t permit it.
--
-- When the DB instance isn\'t publicly accessible, it is an internal DB
-- instance with a DNS name that resolves to a private IP address.
--
-- For more information, see CreateDBInstance.
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
-- 'sourceDBInstanceAutomatedBackupsArn', 'restoreDBInstanceToPointInTime_sourceDBInstanceAutomatedBackupsArn' - The Amazon Resource Name (ARN) of the replicated automated backups from
-- which to restore, for example,
-- @arn:aws:rds:useast-1:123456789012:auto-backup:ab-L2IJCEXJP7XQ7HOJ4SIEXAMPLE@.
--
-- This setting doesn\'t apply to RDS Custom.
--
-- 'sourceDBInstanceIdentifier', 'restoreDBInstanceToPointInTime_sourceDBInstanceIdentifier' - The identifier of the source DB instance from which to restore.
--
-- Constraints:
--
-- -   Must match the identifier of an existing DB instance.
--
-- 'sourceDbiResourceId', 'restoreDBInstanceToPointInTime_sourceDbiResourceId' - The resource ID of the source DB instance from which to restore.
--
-- 'storageThroughput', 'restoreDBInstanceToPointInTime_storageThroughput' - Specifies the storage throughput value for the DB instance.
--
-- This setting doesn\'t apply to RDS Custom or Amazon Aurora.
--
-- 'storageType', 'restoreDBInstanceToPointInTime_storageType' - Specifies the storage type to be associated with the DB instance.
--
-- Valid values: @gp2 | gp3 | io1 | standard@
--
-- If you specify @io1@ or @gp3@, you must also include a value for the
-- @Iops@ parameter.
--
-- Default: @io1@ if the @Iops@ parameter is specified, otherwise @gp2@
--
-- 'tags', 'restoreDBInstanceToPointInTime_tags' - Undocumented member.
--
-- 'tdeCredentialArn', 'restoreDBInstanceToPointInTime_tdeCredentialArn' - The ARN from the key store with which to associate the instance for TDE
-- encryption.
--
-- This setting doesn\'t apply to RDS Custom.
--
-- 'tdeCredentialPassword', 'restoreDBInstanceToPointInTime_tdeCredentialPassword' - The password for the given ARN from the key store in order to access the
-- device.
--
-- This setting doesn\'t apply to RDS Custom.
--
-- 'useDefaultProcessorFeatures', 'restoreDBInstanceToPointInTime_useDefaultProcessorFeatures' - A value that indicates whether the DB instance class of the DB instance
-- uses its default processor features.
--
-- This setting doesn\'t apply to RDS Custom.
--
-- 'useLatestRestorableTime', 'restoreDBInstanceToPointInTime_useLatestRestorableTime' - A value that indicates whether the DB instance is restored from the
-- latest backup time. By default, the DB instance isn\'t restored from the
-- latest backup time.
--
-- Constraints: Can\'t be specified if the @RestoreTime@ parameter is
-- provided.
--
-- 'vpcSecurityGroupIds', 'restoreDBInstanceToPointInTime_vpcSecurityGroupIds' - A list of EC2 VPC security groups to associate with this DB instance.
--
-- Default: The default EC2 VPC security group for the DB subnet group\'s
-- VPC.
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
      { allocatedStorage =
          Prelude.Nothing,
        autoMinorVersionUpgrade = Prelude.Nothing,
        availabilityZone = Prelude.Nothing,
        backupTarget = Prelude.Nothing,
        copyTagsToSnapshot = Prelude.Nothing,
        customIamInstanceProfile = Prelude.Nothing,
        dbInstanceClass = Prelude.Nothing,
        dbName = Prelude.Nothing,
        dbParameterGroupName = Prelude.Nothing,
        dbSubnetGroupName = Prelude.Nothing,
        deletionProtection = Prelude.Nothing,
        domain = Prelude.Nothing,
        domainIAMRoleName = Prelude.Nothing,
        enableCloudwatchLogsExports =
          Prelude.Nothing,
        enableCustomerOwnedIp = Prelude.Nothing,
        enableIAMDatabaseAuthentication =
          Prelude.Nothing,
        engine = Prelude.Nothing,
        iops = Prelude.Nothing,
        licenseModel = Prelude.Nothing,
        maxAllocatedStorage = Prelude.Nothing,
        multiAZ = Prelude.Nothing,
        networkType = Prelude.Nothing,
        optionGroupName = Prelude.Nothing,
        port = Prelude.Nothing,
        processorFeatures = Prelude.Nothing,
        publiclyAccessible = Prelude.Nothing,
        restoreTime = Prelude.Nothing,
        sourceDBInstanceAutomatedBackupsArn =
          Prelude.Nothing,
        sourceDBInstanceIdentifier =
          Prelude.Nothing,
        sourceDbiResourceId = Prelude.Nothing,
        storageThroughput = Prelude.Nothing,
        storageType = Prelude.Nothing,
        tags = Prelude.Nothing,
        tdeCredentialArn = Prelude.Nothing,
        tdeCredentialPassword = Prelude.Nothing,
        useDefaultProcessorFeatures =
          Prelude.Nothing,
        useLatestRestorableTime = Prelude.Nothing,
        vpcSecurityGroupIds = Prelude.Nothing,
        targetDBInstanceIdentifier =
          pTargetDBInstanceIdentifier_
      }

-- | The amount of storage (in gibibytes) to allocate initially for the DB
-- instance. Follow the allocation rules specified in @CreateDBInstance@.
--
-- Be sure to allocate enough storage for your new DB instance so that the
-- restore operation can succeed. You can also allocate additional storage
-- for future growth.
restoreDBInstanceToPointInTime_allocatedStorage :: Lens.Lens' RestoreDBInstanceToPointInTime (Prelude.Maybe Prelude.Int)
restoreDBInstanceToPointInTime_allocatedStorage = Lens.lens (\RestoreDBInstanceToPointInTime' {allocatedStorage} -> allocatedStorage) (\s@RestoreDBInstanceToPointInTime' {} a -> s {allocatedStorage = a} :: RestoreDBInstanceToPointInTime)

-- | A value that indicates whether minor version upgrades are applied
-- automatically to the DB instance during the maintenance window.
--
-- This setting doesn\'t apply to RDS Custom.
restoreDBInstanceToPointInTime_autoMinorVersionUpgrade :: Lens.Lens' RestoreDBInstanceToPointInTime (Prelude.Maybe Prelude.Bool)
restoreDBInstanceToPointInTime_autoMinorVersionUpgrade = Lens.lens (\RestoreDBInstanceToPointInTime' {autoMinorVersionUpgrade} -> autoMinorVersionUpgrade) (\s@RestoreDBInstanceToPointInTime' {} a -> s {autoMinorVersionUpgrade = a} :: RestoreDBInstanceToPointInTime)

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

-- | Specifies where automated backups and manual snapshots are stored for
-- the restored DB instance.
--
-- Possible values are @outposts@ (Amazon Web Services Outposts) and
-- @region@ (Amazon Web Services Region). The default is @region@.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/rds-on-outposts.html Working with Amazon RDS on Amazon Web Services Outposts>
-- in the /Amazon RDS User Guide/.
restoreDBInstanceToPointInTime_backupTarget :: Lens.Lens' RestoreDBInstanceToPointInTime (Prelude.Maybe Prelude.Text)
restoreDBInstanceToPointInTime_backupTarget = Lens.lens (\RestoreDBInstanceToPointInTime' {backupTarget} -> backupTarget) (\s@RestoreDBInstanceToPointInTime' {} a -> s {backupTarget = a} :: RestoreDBInstanceToPointInTime)

-- | A value that indicates whether to copy all tags from the restored DB
-- instance to snapshots of the DB instance. By default, tags are not
-- copied.
restoreDBInstanceToPointInTime_copyTagsToSnapshot :: Lens.Lens' RestoreDBInstanceToPointInTime (Prelude.Maybe Prelude.Bool)
restoreDBInstanceToPointInTime_copyTagsToSnapshot = Lens.lens (\RestoreDBInstanceToPointInTime' {copyTagsToSnapshot} -> copyTagsToSnapshot) (\s@RestoreDBInstanceToPointInTime' {} a -> s {copyTagsToSnapshot = a} :: RestoreDBInstanceToPointInTime)

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
restoreDBInstanceToPointInTime_customIamInstanceProfile :: Lens.Lens' RestoreDBInstanceToPointInTime (Prelude.Maybe Prelude.Text)
restoreDBInstanceToPointInTime_customIamInstanceProfile = Lens.lens (\RestoreDBInstanceToPointInTime' {customIamInstanceProfile} -> customIamInstanceProfile) (\s@RestoreDBInstanceToPointInTime' {} a -> s {customIamInstanceProfile = a} :: RestoreDBInstanceToPointInTime)

-- | The compute and memory capacity of the Amazon RDS DB instance, for
-- example db.m4.large. Not all DB instance classes are available in all
-- Amazon Web Services Regions, or for all database engines. For the full
-- list of DB instance classes, and availability for your engine, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Concepts.DBInstanceClass.html DB Instance Class>
-- in the /Amazon RDS User Guide/.
--
-- Default: The same DBInstanceClass as the original DB instance.
restoreDBInstanceToPointInTime_dbInstanceClass :: Lens.Lens' RestoreDBInstanceToPointInTime (Prelude.Maybe Prelude.Text)
restoreDBInstanceToPointInTime_dbInstanceClass = Lens.lens (\RestoreDBInstanceToPointInTime' {dbInstanceClass} -> dbInstanceClass) (\s@RestoreDBInstanceToPointInTime' {} a -> s {dbInstanceClass = a} :: RestoreDBInstanceToPointInTime)

-- | The database name for the restored DB instance.
--
-- This parameter isn\'t supported for the MySQL or MariaDB engines. It
-- also doesn\'t apply to RDS Custom.
restoreDBInstanceToPointInTime_dbName :: Lens.Lens' RestoreDBInstanceToPointInTime (Prelude.Maybe Prelude.Text)
restoreDBInstanceToPointInTime_dbName = Lens.lens (\RestoreDBInstanceToPointInTime' {dbName} -> dbName) (\s@RestoreDBInstanceToPointInTime' {} a -> s {dbName = a} :: RestoreDBInstanceToPointInTime)

-- | The name of the DB parameter group to associate with this DB instance.
--
-- If you do not specify a value for @DBParameterGroupName@, then the
-- default @DBParameterGroup@ for the specified DB engine is used.
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
restoreDBInstanceToPointInTime_dbParameterGroupName :: Lens.Lens' RestoreDBInstanceToPointInTime (Prelude.Maybe Prelude.Text)
restoreDBInstanceToPointInTime_dbParameterGroupName = Lens.lens (\RestoreDBInstanceToPointInTime' {dbParameterGroupName} -> dbParameterGroupName) (\s@RestoreDBInstanceToPointInTime' {} a -> s {dbParameterGroupName = a} :: RestoreDBInstanceToPointInTime)

-- | The DB subnet group name to use for the new instance.
--
-- Constraints: If supplied, must match the name of an existing
-- DBSubnetGroup.
--
-- Example: @mydbsubnetgroup@
restoreDBInstanceToPointInTime_dbSubnetGroupName :: Lens.Lens' RestoreDBInstanceToPointInTime (Prelude.Maybe Prelude.Text)
restoreDBInstanceToPointInTime_dbSubnetGroupName = Lens.lens (\RestoreDBInstanceToPointInTime' {dbSubnetGroupName} -> dbSubnetGroupName) (\s@RestoreDBInstanceToPointInTime' {} a -> s {dbSubnetGroupName = a} :: RestoreDBInstanceToPointInTime)

-- | A value that indicates whether the DB instance has deletion protection
-- enabled. The database can\'t be deleted when deletion protection is
-- enabled. By default, deletion protection isn\'t enabled. For more
-- information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_DeleteInstance.html Deleting a DB Instance>.
restoreDBInstanceToPointInTime_deletionProtection :: Lens.Lens' RestoreDBInstanceToPointInTime (Prelude.Maybe Prelude.Bool)
restoreDBInstanceToPointInTime_deletionProtection = Lens.lens (\RestoreDBInstanceToPointInTime' {deletionProtection} -> deletionProtection) (\s@RestoreDBInstanceToPointInTime' {} a -> s {deletionProtection = a} :: RestoreDBInstanceToPointInTime)

-- | Specify the Active Directory directory ID to restore the DB instance in.
-- Create the domain before running this command. Currently, you can create
-- only the MySQL, Microsoft SQL Server, Oracle, and PostgreSQL DB
-- instances in an Active Directory Domain.
--
-- This setting doesn\'t apply to RDS Custom.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/kerberos-authentication.html Kerberos Authentication>
-- in the /Amazon RDS User Guide/.
restoreDBInstanceToPointInTime_domain :: Lens.Lens' RestoreDBInstanceToPointInTime (Prelude.Maybe Prelude.Text)
restoreDBInstanceToPointInTime_domain = Lens.lens (\RestoreDBInstanceToPointInTime' {domain} -> domain) (\s@RestoreDBInstanceToPointInTime' {} a -> s {domain = a} :: RestoreDBInstanceToPointInTime)

-- | Specify the name of the IAM role to be used when making API calls to the
-- Directory Service.
--
-- This setting doesn\'t apply to RDS Custom.
restoreDBInstanceToPointInTime_domainIAMRoleName :: Lens.Lens' RestoreDBInstanceToPointInTime (Prelude.Maybe Prelude.Text)
restoreDBInstanceToPointInTime_domainIAMRoleName = Lens.lens (\RestoreDBInstanceToPointInTime' {domainIAMRoleName} -> domainIAMRoleName) (\s@RestoreDBInstanceToPointInTime' {} a -> s {domainIAMRoleName = a} :: RestoreDBInstanceToPointInTime)

-- | The list of logs that the restored DB instance is to export to
-- CloudWatch Logs. The values in the list depend on the DB engine being
-- used. For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_LogAccess.html#USER_LogAccess.Procedural.UploadtoCloudWatch Publishing Database Logs to Amazon CloudWatch Logs>
-- in the /Amazon RDS User Guide/.
--
-- This setting doesn\'t apply to RDS Custom.
restoreDBInstanceToPointInTime_enableCloudwatchLogsExports :: Lens.Lens' RestoreDBInstanceToPointInTime (Prelude.Maybe [Prelude.Text])
restoreDBInstanceToPointInTime_enableCloudwatchLogsExports = Lens.lens (\RestoreDBInstanceToPointInTime' {enableCloudwatchLogsExports} -> enableCloudwatchLogsExports) (\s@RestoreDBInstanceToPointInTime' {} a -> s {enableCloudwatchLogsExports = a} :: RestoreDBInstanceToPointInTime) Prelude.. Lens.mapping Lens.coerced

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
-- <https://docs.aws.amazon.com/outposts/latest/userguide/routing.html#ip-addressing Customer-owned IP addresses>
-- in the /Amazon Web Services Outposts User Guide/.
restoreDBInstanceToPointInTime_enableCustomerOwnedIp :: Lens.Lens' RestoreDBInstanceToPointInTime (Prelude.Maybe Prelude.Bool)
restoreDBInstanceToPointInTime_enableCustomerOwnedIp = Lens.lens (\RestoreDBInstanceToPointInTime' {enableCustomerOwnedIp} -> enableCustomerOwnedIp) (\s@RestoreDBInstanceToPointInTime' {} a -> s {enableCustomerOwnedIp = a} :: RestoreDBInstanceToPointInTime)

-- | A value that indicates whether to enable mapping of Amazon Web Services
-- Identity and Access Management (IAM) accounts to database accounts. By
-- default, mapping isn\'t enabled.
--
-- This setting doesn\'t apply to RDS Custom.
--
-- For more information about IAM database authentication, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/UsingWithRDS.IAMDBAuth.html IAM Database Authentication for MySQL and PostgreSQL>
-- in the /Amazon RDS User Guide./
restoreDBInstanceToPointInTime_enableIAMDatabaseAuthentication :: Lens.Lens' RestoreDBInstanceToPointInTime (Prelude.Maybe Prelude.Bool)
restoreDBInstanceToPointInTime_enableIAMDatabaseAuthentication = Lens.lens (\RestoreDBInstanceToPointInTime' {enableIAMDatabaseAuthentication} -> enableIAMDatabaseAuthentication) (\s@RestoreDBInstanceToPointInTime' {} a -> s {enableIAMDatabaseAuthentication = a} :: RestoreDBInstanceToPointInTime)

-- | The database engine to use for the new instance.
--
-- This setting doesn\'t apply to RDS Custom.
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

-- | License model information for the restored DB instance.
--
-- This setting doesn\'t apply to RDS Custom.
--
-- Default: Same as source.
--
-- Valid values: @license-included@ | @bring-your-own-license@ |
-- @general-public-license@
restoreDBInstanceToPointInTime_licenseModel :: Lens.Lens' RestoreDBInstanceToPointInTime (Prelude.Maybe Prelude.Text)
restoreDBInstanceToPointInTime_licenseModel = Lens.lens (\RestoreDBInstanceToPointInTime' {licenseModel} -> licenseModel) (\s@RestoreDBInstanceToPointInTime' {} a -> s {licenseModel = a} :: RestoreDBInstanceToPointInTime)

-- | The upper limit in gibibytes (GiB) to which Amazon RDS can automatically
-- scale the storage of the DB instance.
--
-- For more information about this setting, including limitations that
-- apply to it, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_PIOPS.StorageTypes.html#USER_PIOPS.Autoscaling Managing capacity automatically with Amazon RDS storage autoscaling>
-- in the /Amazon RDS User Guide/.
--
-- This setting doesn\'t apply to RDS Custom.
restoreDBInstanceToPointInTime_maxAllocatedStorage :: Lens.Lens' RestoreDBInstanceToPointInTime (Prelude.Maybe Prelude.Int)
restoreDBInstanceToPointInTime_maxAllocatedStorage = Lens.lens (\RestoreDBInstanceToPointInTime' {maxAllocatedStorage} -> maxAllocatedStorage) (\s@RestoreDBInstanceToPointInTime' {} a -> s {maxAllocatedStorage = a} :: RestoreDBInstanceToPointInTime)

-- | A value that indicates whether the DB instance is a Multi-AZ deployment.
--
-- This setting doesn\'t apply to RDS Custom.
--
-- Constraint: You can\'t specify the @AvailabilityZone@ parameter if the
-- DB instance is a Multi-AZ deployment.
restoreDBInstanceToPointInTime_multiAZ :: Lens.Lens' RestoreDBInstanceToPointInTime (Prelude.Maybe Prelude.Bool)
restoreDBInstanceToPointInTime_multiAZ = Lens.lens (\RestoreDBInstanceToPointInTime' {multiAZ} -> multiAZ) (\s@RestoreDBInstanceToPointInTime' {} a -> s {multiAZ = a} :: RestoreDBInstanceToPointInTime)

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
restoreDBInstanceToPointInTime_networkType :: Lens.Lens' RestoreDBInstanceToPointInTime (Prelude.Maybe Prelude.Text)
restoreDBInstanceToPointInTime_networkType = Lens.lens (\RestoreDBInstanceToPointInTime' {networkType} -> networkType) (\s@RestoreDBInstanceToPointInTime' {} a -> s {networkType = a} :: RestoreDBInstanceToPointInTime)

-- | The name of the option group to be used for the restored DB instance.
--
-- Permanent options, such as the TDE option for Oracle Advanced Security
-- TDE, can\'t be removed from an option group, and that option group
-- can\'t be removed from a DB instance after it is associated with a DB
-- instance
--
-- This setting doesn\'t apply to RDS Custom.
restoreDBInstanceToPointInTime_optionGroupName :: Lens.Lens' RestoreDBInstanceToPointInTime (Prelude.Maybe Prelude.Text)
restoreDBInstanceToPointInTime_optionGroupName = Lens.lens (\RestoreDBInstanceToPointInTime' {optionGroupName} -> optionGroupName) (\s@RestoreDBInstanceToPointInTime' {} a -> s {optionGroupName = a} :: RestoreDBInstanceToPointInTime)

-- | The port number on which the database accepts connections.
--
-- Constraints: Value must be @1150-65535@
--
-- Default: The same port as the original DB instance.
restoreDBInstanceToPointInTime_port :: Lens.Lens' RestoreDBInstanceToPointInTime (Prelude.Maybe Prelude.Int)
restoreDBInstanceToPointInTime_port = Lens.lens (\RestoreDBInstanceToPointInTime' {port} -> port) (\s@RestoreDBInstanceToPointInTime' {} a -> s {port = a} :: RestoreDBInstanceToPointInTime)

-- | The number of CPU cores and the number of threads per core for the DB
-- instance class of the DB instance.
--
-- This setting doesn\'t apply to RDS Custom.
restoreDBInstanceToPointInTime_processorFeatures :: Lens.Lens' RestoreDBInstanceToPointInTime (Prelude.Maybe [ProcessorFeature])
restoreDBInstanceToPointInTime_processorFeatures = Lens.lens (\RestoreDBInstanceToPointInTime' {processorFeatures} -> processorFeatures) (\s@RestoreDBInstanceToPointInTime' {} a -> s {processorFeatures = a} :: RestoreDBInstanceToPointInTime) Prelude.. Lens.mapping Lens.coerced

-- | A value that indicates whether the DB instance is publicly accessible.
--
-- When the DB cluster is publicly accessible, its Domain Name System (DNS)
-- endpoint resolves to the private IP address from within the DB
-- cluster\'s virtual private cloud (VPC). It resolves to the public IP
-- address from outside of the DB cluster\'s VPC. Access to the DB cluster
-- is ultimately controlled by the security group it uses. That public
-- access isn\'t permitted if the security group assigned to the DB cluster
-- doesn\'t permit it.
--
-- When the DB instance isn\'t publicly accessible, it is an internal DB
-- instance with a DNS name that resolves to a private IP address.
--
-- For more information, see CreateDBInstance.
restoreDBInstanceToPointInTime_publiclyAccessible :: Lens.Lens' RestoreDBInstanceToPointInTime (Prelude.Maybe Prelude.Bool)
restoreDBInstanceToPointInTime_publiclyAccessible = Lens.lens (\RestoreDBInstanceToPointInTime' {publiclyAccessible} -> publiclyAccessible) (\s@RestoreDBInstanceToPointInTime' {} a -> s {publiclyAccessible = a} :: RestoreDBInstanceToPointInTime)

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
restoreDBInstanceToPointInTime_restoreTime = Lens.lens (\RestoreDBInstanceToPointInTime' {restoreTime} -> restoreTime) (\s@RestoreDBInstanceToPointInTime' {} a -> s {restoreTime = a} :: RestoreDBInstanceToPointInTime) Prelude.. Lens.mapping Data._Time

-- | The Amazon Resource Name (ARN) of the replicated automated backups from
-- which to restore, for example,
-- @arn:aws:rds:useast-1:123456789012:auto-backup:ab-L2IJCEXJP7XQ7HOJ4SIEXAMPLE@.
--
-- This setting doesn\'t apply to RDS Custom.
restoreDBInstanceToPointInTime_sourceDBInstanceAutomatedBackupsArn :: Lens.Lens' RestoreDBInstanceToPointInTime (Prelude.Maybe Prelude.Text)
restoreDBInstanceToPointInTime_sourceDBInstanceAutomatedBackupsArn = Lens.lens (\RestoreDBInstanceToPointInTime' {sourceDBInstanceAutomatedBackupsArn} -> sourceDBInstanceAutomatedBackupsArn) (\s@RestoreDBInstanceToPointInTime' {} a -> s {sourceDBInstanceAutomatedBackupsArn = a} :: RestoreDBInstanceToPointInTime)

-- | The identifier of the source DB instance from which to restore.
--
-- Constraints:
--
-- -   Must match the identifier of an existing DB instance.
restoreDBInstanceToPointInTime_sourceDBInstanceIdentifier :: Lens.Lens' RestoreDBInstanceToPointInTime (Prelude.Maybe Prelude.Text)
restoreDBInstanceToPointInTime_sourceDBInstanceIdentifier = Lens.lens (\RestoreDBInstanceToPointInTime' {sourceDBInstanceIdentifier} -> sourceDBInstanceIdentifier) (\s@RestoreDBInstanceToPointInTime' {} a -> s {sourceDBInstanceIdentifier = a} :: RestoreDBInstanceToPointInTime)

-- | The resource ID of the source DB instance from which to restore.
restoreDBInstanceToPointInTime_sourceDbiResourceId :: Lens.Lens' RestoreDBInstanceToPointInTime (Prelude.Maybe Prelude.Text)
restoreDBInstanceToPointInTime_sourceDbiResourceId = Lens.lens (\RestoreDBInstanceToPointInTime' {sourceDbiResourceId} -> sourceDbiResourceId) (\s@RestoreDBInstanceToPointInTime' {} a -> s {sourceDbiResourceId = a} :: RestoreDBInstanceToPointInTime)

-- | Specifies the storage throughput value for the DB instance.
--
-- This setting doesn\'t apply to RDS Custom or Amazon Aurora.
restoreDBInstanceToPointInTime_storageThroughput :: Lens.Lens' RestoreDBInstanceToPointInTime (Prelude.Maybe Prelude.Int)
restoreDBInstanceToPointInTime_storageThroughput = Lens.lens (\RestoreDBInstanceToPointInTime' {storageThroughput} -> storageThroughput) (\s@RestoreDBInstanceToPointInTime' {} a -> s {storageThroughput = a} :: RestoreDBInstanceToPointInTime)

-- | Specifies the storage type to be associated with the DB instance.
--
-- Valid values: @gp2 | gp3 | io1 | standard@
--
-- If you specify @io1@ or @gp3@, you must also include a value for the
-- @Iops@ parameter.
--
-- Default: @io1@ if the @Iops@ parameter is specified, otherwise @gp2@
restoreDBInstanceToPointInTime_storageType :: Lens.Lens' RestoreDBInstanceToPointInTime (Prelude.Maybe Prelude.Text)
restoreDBInstanceToPointInTime_storageType = Lens.lens (\RestoreDBInstanceToPointInTime' {storageType} -> storageType) (\s@RestoreDBInstanceToPointInTime' {} a -> s {storageType = a} :: RestoreDBInstanceToPointInTime)

-- | Undocumented member.
restoreDBInstanceToPointInTime_tags :: Lens.Lens' RestoreDBInstanceToPointInTime (Prelude.Maybe [Tag])
restoreDBInstanceToPointInTime_tags = Lens.lens (\RestoreDBInstanceToPointInTime' {tags} -> tags) (\s@RestoreDBInstanceToPointInTime' {} a -> s {tags = a} :: RestoreDBInstanceToPointInTime) Prelude.. Lens.mapping Lens.coerced

-- | The ARN from the key store with which to associate the instance for TDE
-- encryption.
--
-- This setting doesn\'t apply to RDS Custom.
restoreDBInstanceToPointInTime_tdeCredentialArn :: Lens.Lens' RestoreDBInstanceToPointInTime (Prelude.Maybe Prelude.Text)
restoreDBInstanceToPointInTime_tdeCredentialArn = Lens.lens (\RestoreDBInstanceToPointInTime' {tdeCredentialArn} -> tdeCredentialArn) (\s@RestoreDBInstanceToPointInTime' {} a -> s {tdeCredentialArn = a} :: RestoreDBInstanceToPointInTime)

-- | The password for the given ARN from the key store in order to access the
-- device.
--
-- This setting doesn\'t apply to RDS Custom.
restoreDBInstanceToPointInTime_tdeCredentialPassword :: Lens.Lens' RestoreDBInstanceToPointInTime (Prelude.Maybe Prelude.Text)
restoreDBInstanceToPointInTime_tdeCredentialPassword = Lens.lens (\RestoreDBInstanceToPointInTime' {tdeCredentialPassword} -> tdeCredentialPassword) (\s@RestoreDBInstanceToPointInTime' {} a -> s {tdeCredentialPassword = a} :: RestoreDBInstanceToPointInTime)

-- | A value that indicates whether the DB instance class of the DB instance
-- uses its default processor features.
--
-- This setting doesn\'t apply to RDS Custom.
restoreDBInstanceToPointInTime_useDefaultProcessorFeatures :: Lens.Lens' RestoreDBInstanceToPointInTime (Prelude.Maybe Prelude.Bool)
restoreDBInstanceToPointInTime_useDefaultProcessorFeatures = Lens.lens (\RestoreDBInstanceToPointInTime' {useDefaultProcessorFeatures} -> useDefaultProcessorFeatures) (\s@RestoreDBInstanceToPointInTime' {} a -> s {useDefaultProcessorFeatures = a} :: RestoreDBInstanceToPointInTime)

-- | A value that indicates whether the DB instance is restored from the
-- latest backup time. By default, the DB instance isn\'t restored from the
-- latest backup time.
--
-- Constraints: Can\'t be specified if the @RestoreTime@ parameter is
-- provided.
restoreDBInstanceToPointInTime_useLatestRestorableTime :: Lens.Lens' RestoreDBInstanceToPointInTime (Prelude.Maybe Prelude.Bool)
restoreDBInstanceToPointInTime_useLatestRestorableTime = Lens.lens (\RestoreDBInstanceToPointInTime' {useLatestRestorableTime} -> useLatestRestorableTime) (\s@RestoreDBInstanceToPointInTime' {} a -> s {useLatestRestorableTime = a} :: RestoreDBInstanceToPointInTime)

-- | A list of EC2 VPC security groups to associate with this DB instance.
--
-- Default: The default EC2 VPC security group for the DB subnet group\'s
-- VPC.
restoreDBInstanceToPointInTime_vpcSecurityGroupIds :: Lens.Lens' RestoreDBInstanceToPointInTime (Prelude.Maybe [Prelude.Text])
restoreDBInstanceToPointInTime_vpcSecurityGroupIds = Lens.lens (\RestoreDBInstanceToPointInTime' {vpcSecurityGroupIds} -> vpcSecurityGroupIds) (\s@RestoreDBInstanceToPointInTime' {} a -> s {vpcSecurityGroupIds = a} :: RestoreDBInstanceToPointInTime) Prelude.. Lens.mapping Lens.coerced

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
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "RestoreDBInstanceToPointInTimeResult"
      ( \s h x ->
          RestoreDBInstanceToPointInTimeResponse'
            Prelude.<$> (x Data..@? "DBInstance")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    RestoreDBInstanceToPointInTime
  where
  hashWithSalt
    _salt
    RestoreDBInstanceToPointInTime' {..} =
      _salt
        `Prelude.hashWithSalt` allocatedStorage
        `Prelude.hashWithSalt` autoMinorVersionUpgrade
        `Prelude.hashWithSalt` availabilityZone
        `Prelude.hashWithSalt` backupTarget
        `Prelude.hashWithSalt` copyTagsToSnapshot
        `Prelude.hashWithSalt` customIamInstanceProfile
        `Prelude.hashWithSalt` dbInstanceClass
        `Prelude.hashWithSalt` dbName
        `Prelude.hashWithSalt` dbParameterGroupName
        `Prelude.hashWithSalt` dbSubnetGroupName
        `Prelude.hashWithSalt` deletionProtection
        `Prelude.hashWithSalt` domain
        `Prelude.hashWithSalt` domainIAMRoleName
        `Prelude.hashWithSalt` enableCloudwatchLogsExports
        `Prelude.hashWithSalt` enableCustomerOwnedIp
        `Prelude.hashWithSalt` enableIAMDatabaseAuthentication
        `Prelude.hashWithSalt` engine
        `Prelude.hashWithSalt` iops
        `Prelude.hashWithSalt` licenseModel
        `Prelude.hashWithSalt` maxAllocatedStorage
        `Prelude.hashWithSalt` multiAZ
        `Prelude.hashWithSalt` networkType
        `Prelude.hashWithSalt` optionGroupName
        `Prelude.hashWithSalt` port
        `Prelude.hashWithSalt` processorFeatures
        `Prelude.hashWithSalt` publiclyAccessible
        `Prelude.hashWithSalt` restoreTime
        `Prelude.hashWithSalt` sourceDBInstanceAutomatedBackupsArn
        `Prelude.hashWithSalt` sourceDBInstanceIdentifier
        `Prelude.hashWithSalt` sourceDbiResourceId
        `Prelude.hashWithSalt` storageThroughput
        `Prelude.hashWithSalt` storageType
        `Prelude.hashWithSalt` tags
        `Prelude.hashWithSalt` tdeCredentialArn
        `Prelude.hashWithSalt` tdeCredentialPassword
        `Prelude.hashWithSalt` useDefaultProcessorFeatures
        `Prelude.hashWithSalt` useLatestRestorableTime
        `Prelude.hashWithSalt` vpcSecurityGroupIds
        `Prelude.hashWithSalt` targetDBInstanceIdentifier

instance
  Prelude.NFData
    RestoreDBInstanceToPointInTime
  where
  rnf RestoreDBInstanceToPointInTime' {..} =
    Prelude.rnf allocatedStorage
      `Prelude.seq` Prelude.rnf autoMinorVersionUpgrade
      `Prelude.seq` Prelude.rnf availabilityZone
      `Prelude.seq` Prelude.rnf backupTarget
      `Prelude.seq` Prelude.rnf copyTagsToSnapshot
      `Prelude.seq` Prelude.rnf customIamInstanceProfile
      `Prelude.seq` Prelude.rnf dbInstanceClass
      `Prelude.seq` Prelude.rnf dbName
      `Prelude.seq` Prelude.rnf dbParameterGroupName
      `Prelude.seq` Prelude.rnf dbSubnetGroupName
      `Prelude.seq` Prelude.rnf deletionProtection
      `Prelude.seq` Prelude.rnf domain
      `Prelude.seq` Prelude.rnf domainIAMRoleName
      `Prelude.seq` Prelude.rnf enableCloudwatchLogsExports
      `Prelude.seq` Prelude.rnf enableCustomerOwnedIp
      `Prelude.seq` Prelude.rnf
        enableIAMDatabaseAuthentication
      `Prelude.seq` Prelude.rnf engine
      `Prelude.seq` Prelude.rnf iops
      `Prelude.seq` Prelude.rnf licenseModel
      `Prelude.seq` Prelude.rnf
        maxAllocatedStorage
      `Prelude.seq` Prelude.rnf multiAZ
      `Prelude.seq` Prelude.rnf networkType
      `Prelude.seq` Prelude.rnf
        optionGroupName
      `Prelude.seq` Prelude.rnf port
      `Prelude.seq` Prelude.rnf
        processorFeatures
      `Prelude.seq` Prelude.rnf
        publiclyAccessible
      `Prelude.seq` Prelude.rnf
        restoreTime
      `Prelude.seq` Prelude.rnf
        sourceDBInstanceAutomatedBackupsArn
      `Prelude.seq` Prelude.rnf
        sourceDBInstanceIdentifier
      `Prelude.seq` Prelude.rnf
        sourceDbiResourceId
      `Prelude.seq` Prelude.rnf
        storageThroughput
      `Prelude.seq` Prelude.rnf
        storageType
      `Prelude.seq` Prelude.rnf
        tags
      `Prelude.seq` Prelude.rnf
        tdeCredentialArn
      `Prelude.seq` Prelude.rnf
        tdeCredentialPassword
      `Prelude.seq` Prelude.rnf
        useDefaultProcessorFeatures
      `Prelude.seq` Prelude.rnf
        useLatestRestorableTime
      `Prelude.seq` Prelude.rnf
        vpcSecurityGroupIds
      `Prelude.seq` Prelude.rnf
        targetDBInstanceIdentifier

instance
  Data.ToHeaders
    RestoreDBInstanceToPointInTime
  where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath RestoreDBInstanceToPointInTime where
  toPath = Prelude.const "/"

instance Data.ToQuery RestoreDBInstanceToPointInTime where
  toQuery RestoreDBInstanceToPointInTime' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "RestoreDBInstanceToPointInTime" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2014-10-31" :: Prelude.ByteString),
        "AllocatedStorage" Data.=: allocatedStorage,
        "AutoMinorVersionUpgrade"
          Data.=: autoMinorVersionUpgrade,
        "AvailabilityZone" Data.=: availabilityZone,
        "BackupTarget" Data.=: backupTarget,
        "CopyTagsToSnapshot" Data.=: copyTagsToSnapshot,
        "CustomIamInstanceProfile"
          Data.=: customIamInstanceProfile,
        "DBInstanceClass" Data.=: dbInstanceClass,
        "DBName" Data.=: dbName,
        "DBParameterGroupName" Data.=: dbParameterGroupName,
        "DBSubnetGroupName" Data.=: dbSubnetGroupName,
        "DeletionProtection" Data.=: deletionProtection,
        "Domain" Data.=: domain,
        "DomainIAMRoleName" Data.=: domainIAMRoleName,
        "EnableCloudwatchLogsExports"
          Data.=: Data.toQuery
            ( Data.toQueryList "member"
                Prelude.<$> enableCloudwatchLogsExports
            ),
        "EnableCustomerOwnedIp"
          Data.=: enableCustomerOwnedIp,
        "EnableIAMDatabaseAuthentication"
          Data.=: enableIAMDatabaseAuthentication,
        "Engine" Data.=: engine,
        "Iops" Data.=: iops,
        "LicenseModel" Data.=: licenseModel,
        "MaxAllocatedStorage" Data.=: maxAllocatedStorage,
        "MultiAZ" Data.=: multiAZ,
        "NetworkType" Data.=: networkType,
        "OptionGroupName" Data.=: optionGroupName,
        "Port" Data.=: port,
        "ProcessorFeatures"
          Data.=: Data.toQuery
            ( Data.toQueryList "ProcessorFeature"
                Prelude.<$> processorFeatures
            ),
        "PubliclyAccessible" Data.=: publiclyAccessible,
        "RestoreTime" Data.=: restoreTime,
        "SourceDBInstanceAutomatedBackupsArn"
          Data.=: sourceDBInstanceAutomatedBackupsArn,
        "SourceDBInstanceIdentifier"
          Data.=: sourceDBInstanceIdentifier,
        "SourceDbiResourceId" Data.=: sourceDbiResourceId,
        "StorageThroughput" Data.=: storageThroughput,
        "StorageType" Data.=: storageType,
        "Tags"
          Data.=: Data.toQuery
            (Data.toQueryList "Tag" Prelude.<$> tags),
        "TdeCredentialArn" Data.=: tdeCredentialArn,
        "TdeCredentialPassword"
          Data.=: tdeCredentialPassword,
        "UseDefaultProcessorFeatures"
          Data.=: useDefaultProcessorFeatures,
        "UseLatestRestorableTime"
          Data.=: useLatestRestorableTime,
        "VpcSecurityGroupIds"
          Data.=: Data.toQuery
            ( Data.toQueryList "VpcSecurityGroupId"
                Prelude.<$> vpcSecurityGroupIds
            ),
        "TargetDBInstanceIdentifier"
          Data.=: targetDBInstanceIdentifier
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
  where
  rnf RestoreDBInstanceToPointInTimeResponse' {..} =
    Prelude.rnf dbInstance
      `Prelude.seq` Prelude.rnf httpStatus
