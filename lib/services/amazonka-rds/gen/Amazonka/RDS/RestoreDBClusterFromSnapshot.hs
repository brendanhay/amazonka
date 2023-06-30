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
-- Module      : Amazonka.RDS.RestoreDBClusterFromSnapshot
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new DB cluster from a DB snapshot or DB cluster snapshot.
--
-- The target DB cluster is created from the source snapshot with a default
-- configuration. If you don\'t specify a security group, the new DB
-- cluster is associated with the default security group.
--
-- This action only restores the DB cluster, not the DB instances for that
-- DB cluster. You must invoke the @CreateDBInstance@ action to create DB
-- instances for the restored DB cluster, specifying the identifier of the
-- restored DB cluster in @DBClusterIdentifier@. You can create DB
-- instances only after the @RestoreDBClusterFromSnapshot@ action has
-- completed and the DB cluster is available.
--
-- For more information on Amazon Aurora DB clusters, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/CHAP_AuroraOverview.html What is Amazon Aurora?>
-- in the /Amazon Aurora User Guide/.
--
-- For more information on Multi-AZ DB clusters, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/multi-az-db-clusters-concepts.html Multi-AZ deployments with two readable standby DB instances>
-- in the /Amazon RDS User Guide./
module Amazonka.RDS.RestoreDBClusterFromSnapshot
  ( -- * Creating a Request
    RestoreDBClusterFromSnapshot (..),
    newRestoreDBClusterFromSnapshot,

    -- * Request Lenses
    restoreDBClusterFromSnapshot_availabilityZones,
    restoreDBClusterFromSnapshot_backtrackWindow,
    restoreDBClusterFromSnapshot_copyTagsToSnapshot,
    restoreDBClusterFromSnapshot_dbClusterInstanceClass,
    restoreDBClusterFromSnapshot_dbClusterParameterGroupName,
    restoreDBClusterFromSnapshot_dbSubnetGroupName,
    restoreDBClusterFromSnapshot_databaseName,
    restoreDBClusterFromSnapshot_deletionProtection,
    restoreDBClusterFromSnapshot_domain,
    restoreDBClusterFromSnapshot_domainIAMRoleName,
    restoreDBClusterFromSnapshot_enableCloudwatchLogsExports,
    restoreDBClusterFromSnapshot_enableIAMDatabaseAuthentication,
    restoreDBClusterFromSnapshot_engineMode,
    restoreDBClusterFromSnapshot_engineVersion,
    restoreDBClusterFromSnapshot_iops,
    restoreDBClusterFromSnapshot_kmsKeyId,
    restoreDBClusterFromSnapshot_networkType,
    restoreDBClusterFromSnapshot_optionGroupName,
    restoreDBClusterFromSnapshot_port,
    restoreDBClusterFromSnapshot_publiclyAccessible,
    restoreDBClusterFromSnapshot_scalingConfiguration,
    restoreDBClusterFromSnapshot_serverlessV2ScalingConfiguration,
    restoreDBClusterFromSnapshot_storageType,
    restoreDBClusterFromSnapshot_tags,
    restoreDBClusterFromSnapshot_vpcSecurityGroupIds,
    restoreDBClusterFromSnapshot_dbClusterIdentifier,
    restoreDBClusterFromSnapshot_snapshotIdentifier,
    restoreDBClusterFromSnapshot_engine,

    -- * Destructuring the Response
    RestoreDBClusterFromSnapshotResponse (..),
    newRestoreDBClusterFromSnapshotResponse,

    -- * Response Lenses
    restoreDBClusterFromSnapshotResponse_dbCluster,
    restoreDBClusterFromSnapshotResponse_httpStatus,
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
-- /See:/ 'newRestoreDBClusterFromSnapshot' smart constructor.
data RestoreDBClusterFromSnapshot = RestoreDBClusterFromSnapshot'
  { -- | Provides the list of Availability Zones (AZs) where instances in the
    -- restored DB cluster can be created.
    --
    -- Valid for: Aurora DB clusters only
    availabilityZones :: Prelude.Maybe [Prelude.Text],
    -- | The target backtrack window, in seconds. To disable backtracking, set
    -- this value to 0.
    --
    -- Currently, Backtrack is only supported for Aurora MySQL DB clusters.
    --
    -- Default: 0
    --
    -- Constraints:
    --
    -- -   If specified, this value must be set to a number from 0 to 259,200
    --     (72 hours).
    --
    -- Valid for: Aurora DB clusters only
    backtrackWindow :: Prelude.Maybe Prelude.Integer,
    -- | A value that indicates whether to copy all tags from the restored DB
    -- cluster to snapshots of the restored DB cluster. The default is not to
    -- copy them.
    --
    -- Valid for: Aurora DB clusters and Multi-AZ DB clusters
    copyTagsToSnapshot :: Prelude.Maybe Prelude.Bool,
    -- | The compute and memory capacity of the each DB instance in the Multi-AZ
    -- DB cluster, for example db.m6gd.xlarge. Not all DB instance classes are
    -- available in all Amazon Web Services Regions, or for all database
    -- engines.
    --
    -- For the full list of DB instance classes, and availability for your
    -- engine, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Concepts.DBInstanceClass.html DB Instance Class>
    -- in the /Amazon RDS User Guide./
    --
    -- Valid for: Multi-AZ DB clusters only
    dbClusterInstanceClass :: Prelude.Maybe Prelude.Text,
    -- | The name of the DB cluster parameter group to associate with this DB
    -- cluster. If this argument is omitted, the default DB cluster parameter
    -- group for the specified engine is used.
    --
    -- Constraints:
    --
    -- -   If supplied, must match the name of an existing default DB cluster
    --     parameter group.
    --
    -- -   Must be 1 to 255 letters, numbers, or hyphens.
    --
    -- -   First character must be a letter.
    --
    -- -   Can\'t end with a hyphen or contain two consecutive hyphens.
    --
    -- Valid for: Aurora DB clusters and Multi-AZ DB clusters
    dbClusterParameterGroupName :: Prelude.Maybe Prelude.Text,
    -- | The name of the DB subnet group to use for the new DB cluster.
    --
    -- Constraints: If supplied, must match the name of an existing DB subnet
    -- group.
    --
    -- Example: @mydbsubnetgroup@
    --
    -- Valid for: Aurora DB clusters and Multi-AZ DB clusters
    dbSubnetGroupName :: Prelude.Maybe Prelude.Text,
    -- | The database name for the restored DB cluster.
    --
    -- Valid for: Aurora DB clusters and Multi-AZ DB clusters
    databaseName :: Prelude.Maybe Prelude.Text,
    -- | A value that indicates whether the DB cluster has deletion protection
    -- enabled. The database can\'t be deleted when deletion protection is
    -- enabled. By default, deletion protection isn\'t enabled.
    --
    -- Valid for: Aurora DB clusters and Multi-AZ DB clusters
    deletionProtection :: Prelude.Maybe Prelude.Bool,
    -- | Specify the Active Directory directory ID to restore the DB cluster in.
    -- The domain must be created prior to this operation. Currently, only
    -- MySQL, Microsoft SQL Server, Oracle, and PostgreSQL DB instances can be
    -- created in an Active Directory Domain.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/kerberos-authentication.html Kerberos Authentication>
    -- in the /Amazon RDS User Guide/.
    --
    -- Valid for: Aurora DB clusters only
    domain :: Prelude.Maybe Prelude.Text,
    -- | Specify the name of the IAM role to be used when making API calls to the
    -- Directory Service.
    --
    -- Valid for: Aurora DB clusters only
    domainIAMRoleName :: Prelude.Maybe Prelude.Text,
    -- | The list of logs that the restored DB cluster is to export to Amazon
    -- CloudWatch Logs. The values in the list depend on the DB engine being
    -- used.
    --
    -- __RDS for MySQL__
    --
    -- Possible values are @error@, @general@, and @slowquery@.
    --
    -- __RDS for PostgreSQL__
    --
    -- Possible values are @postgresql@ and @upgrade@.
    --
    -- __Aurora MySQL__
    --
    -- Possible values are @audit@, @error@, @general@, and @slowquery@.
    --
    -- __Aurora PostgreSQL__
    --
    -- Possible value is @postgresql@.
    --
    -- For more information about exporting CloudWatch Logs for Amazon RDS, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_LogAccess.html#USER_LogAccess.Procedural.UploadtoCloudWatch Publishing Database Logs to Amazon CloudWatch Logs>
    -- in the /Amazon RDS User Guide/.
    --
    -- For more information about exporting CloudWatch Logs for Amazon Aurora,
    -- see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/USER_LogAccess.html#USER_LogAccess.Procedural.UploadtoCloudWatch Publishing Database Logs to Amazon CloudWatch Logs>
    -- in the /Amazon Aurora User Guide/.
    --
    -- Valid for: Aurora DB clusters and Multi-AZ DB clusters
    enableCloudwatchLogsExports :: Prelude.Maybe [Prelude.Text],
    -- | A value that indicates whether to enable mapping of Amazon Web Services
    -- Identity and Access Management (IAM) accounts to database accounts. By
    -- default, mapping isn\'t enabled.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/UsingWithRDS.IAMDBAuth.html IAM Database Authentication>
    -- in the /Amazon Aurora User Guide/.
    --
    -- Valid for: Aurora DB clusters only
    enableIAMDatabaseAuthentication :: Prelude.Maybe Prelude.Bool,
    -- | The DB engine mode of the DB cluster, either @provisioned@,
    -- @serverless@, @parallelquery@, @global@, or @multimaster@.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/APIReference/API_CreateDBCluster.html CreateDBCluster>.
    --
    -- Valid for: Aurora DB clusters only
    engineMode :: Prelude.Maybe Prelude.Text,
    -- | The version of the database engine to use for the new DB cluster.
    --
    -- To list all of the available engine versions for MySQL 5.6-compatible
    -- Aurora, use the following command:
    --
    -- @aws rds describe-db-engine-versions --engine aurora --query \"DBEngineVersions[].EngineVersion\"@
    --
    -- To list all of the available engine versions for MySQL 5.7-compatible
    -- and MySQL 8.0-compatible Aurora, use the following command:
    --
    -- @aws rds describe-db-engine-versions --engine aurora-mysql --query \"DBEngineVersions[].EngineVersion\"@
    --
    -- To list all of the available engine versions for Aurora PostgreSQL, use
    -- the following command:
    --
    -- @aws rds describe-db-engine-versions --engine aurora-postgresql --query \"DBEngineVersions[].EngineVersion\"@
    --
    -- To list all of the available engine versions for RDS for MySQL, use the
    -- following command:
    --
    -- @aws rds describe-db-engine-versions --engine mysql --query \"DBEngineVersions[].EngineVersion\"@
    --
    -- To list all of the available engine versions for RDS for PostgreSQL, use
    -- the following command:
    --
    -- @aws rds describe-db-engine-versions --engine postgres --query \"DBEngineVersions[].EngineVersion\"@
    --
    -- __Aurora MySQL__
    --
    -- See
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/AuroraMySQL.Updates.html MySQL on Amazon RDS Versions>
    -- in the /Amazon Aurora User Guide/.
    --
    -- __Aurora PostgreSQL__
    --
    -- See
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/AuroraPostgreSQL.Updates.20180305.html Amazon Aurora PostgreSQL releases and engine versions>
    -- in the /Amazon Aurora User Guide/.
    --
    -- __MySQL__
    --
    -- See
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/CHAP_MySQL.html#MySQL.Concepts.VersionMgmt MySQL on Amazon RDS Versions>
    -- in the /Amazon RDS User Guide./
    --
    -- __PostgreSQL__
    --
    -- See
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/CHAP_PostgreSQL.html#PostgreSQL.Concepts Amazon RDS for PostgreSQL versions and extensions>
    -- in the /Amazon RDS User Guide./
    --
    -- Valid for: Aurora DB clusters and Multi-AZ DB clusters
    engineVersion :: Prelude.Maybe Prelude.Text,
    -- | The amount of Provisioned IOPS (input\/output operations per second) to
    -- be initially allocated for each DB instance in the Multi-AZ DB cluster.
    --
    -- For information about valid IOPS values, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/CHAP_Storage.html#USER_PIOPS Amazon RDS Provisioned IOPS storage>
    -- in the /Amazon RDS User Guide/.
    --
    -- Constraints: Must be a multiple between .5 and 50 of the storage amount
    -- for the DB instance.
    --
    -- Valid for: Aurora DB clusters and Multi-AZ DB clusters
    iops :: Prelude.Maybe Prelude.Int,
    -- | The Amazon Web Services KMS key identifier to use when restoring an
    -- encrypted DB cluster from a DB snapshot or DB cluster snapshot.
    --
    -- The Amazon Web Services KMS key identifier is the key ARN, key ID, alias
    -- ARN, or alias name for the KMS key. To use a KMS key in a different
    -- Amazon Web Services account, specify the key ARN or alias ARN.
    --
    -- When you don\'t specify a value for the @KmsKeyId@ parameter, then the
    -- following occurs:
    --
    -- -   If the DB snapshot or DB cluster snapshot in @SnapshotIdentifier@ is
    --     encrypted, then the restored DB cluster is encrypted using the KMS
    --     key that was used to encrypt the DB snapshot or DB cluster snapshot.
    --
    -- -   If the DB snapshot or DB cluster snapshot in @SnapshotIdentifier@
    --     isn\'t encrypted, then the restored DB cluster isn\'t encrypted.
    --
    -- Valid for: Aurora DB clusters and Multi-AZ DB clusters
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | The network type of the DB cluster.
    --
    -- Valid values:
    --
    -- -   @IPV4@
    --
    -- -   @DUAL@
    --
    -- The network type is determined by the @DBSubnetGroup@ specified for the
    -- DB cluster. A @DBSubnetGroup@ can support only the IPv4 protocol or the
    -- IPv4 and the IPv6 protocols (@DUAL@).
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/USER_VPC.WorkingWithRDSInstanceinaVPC.html Working with a DB instance in a VPC>
    -- in the /Amazon Aurora User Guide./
    --
    -- Valid for: Aurora DB clusters only
    networkType :: Prelude.Maybe Prelude.Text,
    -- | The name of the option group to use for the restored DB cluster.
    --
    -- DB clusters are associated with a default option group that can\'t be
    -- modified.
    optionGroupName :: Prelude.Maybe Prelude.Text,
    -- | The port number on which the new DB cluster accepts connections.
    --
    -- Constraints: This value must be @1150-65535@
    --
    -- Default: The same port as the original DB cluster.
    --
    -- Valid for: Aurora DB clusters and Multi-AZ DB clusters
    port :: Prelude.Maybe Prelude.Int,
    -- | A value that indicates whether the DB cluster is publicly accessible.
    --
    -- When the DB cluster is publicly accessible, its Domain Name System (DNS)
    -- endpoint resolves to the private IP address from within the DB
    -- cluster\'s virtual private cloud (VPC). It resolves to the public IP
    -- address from outside of the DB cluster\'s VPC. Access to the DB cluster
    -- is ultimately controlled by the security group it uses. That public
    -- access is not permitted if the security group assigned to the DB cluster
    -- doesn\'t permit it.
    --
    -- When the DB cluster isn\'t publicly accessible, it is an internal DB
    -- cluster with a DNS name that resolves to a private IP address.
    --
    -- Default: The default behavior varies depending on whether
    -- @DBSubnetGroupName@ is specified.
    --
    -- If @DBSubnetGroupName@ isn\'t specified, and @PubliclyAccessible@ isn\'t
    -- specified, the following applies:
    --
    -- -   If the default VPC in the target Region doesn’t have an internet
    --     gateway attached to it, the DB cluster is private.
    --
    -- -   If the default VPC in the target Region has an internet gateway
    --     attached to it, the DB cluster is public.
    --
    -- If @DBSubnetGroupName@ is specified, and @PubliclyAccessible@ isn\'t
    -- specified, the following applies:
    --
    -- -   If the subnets are part of a VPC that doesn’t have an internet
    --     gateway attached to it, the DB cluster is private.
    --
    -- -   If the subnets are part of a VPC that has an internet gateway
    --     attached to it, the DB cluster is public.
    --
    -- Valid for: Aurora DB clusters and Multi-AZ DB clusters
    publiclyAccessible :: Prelude.Maybe Prelude.Bool,
    -- | For DB clusters in @serverless@ DB engine mode, the scaling properties
    -- of the DB cluster.
    --
    -- Valid for: Aurora DB clusters only
    scalingConfiguration :: Prelude.Maybe ScalingConfiguration,
    serverlessV2ScalingConfiguration :: Prelude.Maybe ServerlessV2ScalingConfiguration,
    -- | Specifies the storage type to be associated with the each DB instance in
    -- the Multi-AZ DB cluster.
    --
    -- Valid values: @io1@
    --
    -- When specified, a value for the @Iops@ parameter is required.
    --
    -- Default: @io1@
    --
    -- Valid for: Aurora DB clusters and Multi-AZ DB clusters
    storageType :: Prelude.Maybe Prelude.Text,
    -- | The tags to be assigned to the restored DB cluster.
    --
    -- Valid for: Aurora DB clusters and Multi-AZ DB clusters
    tags :: Prelude.Maybe [Tag],
    -- | A list of VPC security groups that the new DB cluster will belong to.
    --
    -- Valid for: Aurora DB clusters and Multi-AZ DB clusters
    vpcSecurityGroupIds :: Prelude.Maybe [Prelude.Text],
    -- | The name of the DB cluster to create from the DB snapshot or DB cluster
    -- snapshot. This parameter isn\'t case-sensitive.
    --
    -- Constraints:
    --
    -- -   Must contain from 1 to 63 letters, numbers, or hyphens
    --
    -- -   First character must be a letter
    --
    -- -   Can\'t end with a hyphen or contain two consecutive hyphens
    --
    -- Example: @my-snapshot-id@
    --
    -- Valid for: Aurora DB clusters and Multi-AZ DB clusters
    dbClusterIdentifier :: Prelude.Text,
    -- | The identifier for the DB snapshot or DB cluster snapshot to restore
    -- from.
    --
    -- You can use either the name or the Amazon Resource Name (ARN) to specify
    -- a DB cluster snapshot. However, you can use only the ARN to specify a DB
    -- snapshot.
    --
    -- Constraints:
    --
    -- -   Must match the identifier of an existing Snapshot.
    --
    -- Valid for: Aurora DB clusters and Multi-AZ DB clusters
    snapshotIdentifier :: Prelude.Text,
    -- | The database engine to use for the new DB cluster.
    --
    -- Default: The same as source
    --
    -- Constraint: Must be compatible with the engine of the source
    --
    -- Valid for: Aurora DB clusters and Multi-AZ DB clusters
    engine :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RestoreDBClusterFromSnapshot' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'availabilityZones', 'restoreDBClusterFromSnapshot_availabilityZones' - Provides the list of Availability Zones (AZs) where instances in the
-- restored DB cluster can be created.
--
-- Valid for: Aurora DB clusters only
--
-- 'backtrackWindow', 'restoreDBClusterFromSnapshot_backtrackWindow' - The target backtrack window, in seconds. To disable backtracking, set
-- this value to 0.
--
-- Currently, Backtrack is only supported for Aurora MySQL DB clusters.
--
-- Default: 0
--
-- Constraints:
--
-- -   If specified, this value must be set to a number from 0 to 259,200
--     (72 hours).
--
-- Valid for: Aurora DB clusters only
--
-- 'copyTagsToSnapshot', 'restoreDBClusterFromSnapshot_copyTagsToSnapshot' - A value that indicates whether to copy all tags from the restored DB
-- cluster to snapshots of the restored DB cluster. The default is not to
-- copy them.
--
-- Valid for: Aurora DB clusters and Multi-AZ DB clusters
--
-- 'dbClusterInstanceClass', 'restoreDBClusterFromSnapshot_dbClusterInstanceClass' - The compute and memory capacity of the each DB instance in the Multi-AZ
-- DB cluster, for example db.m6gd.xlarge. Not all DB instance classes are
-- available in all Amazon Web Services Regions, or for all database
-- engines.
--
-- For the full list of DB instance classes, and availability for your
-- engine, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Concepts.DBInstanceClass.html DB Instance Class>
-- in the /Amazon RDS User Guide./
--
-- Valid for: Multi-AZ DB clusters only
--
-- 'dbClusterParameterGroupName', 'restoreDBClusterFromSnapshot_dbClusterParameterGroupName' - The name of the DB cluster parameter group to associate with this DB
-- cluster. If this argument is omitted, the default DB cluster parameter
-- group for the specified engine is used.
--
-- Constraints:
--
-- -   If supplied, must match the name of an existing default DB cluster
--     parameter group.
--
-- -   Must be 1 to 255 letters, numbers, or hyphens.
--
-- -   First character must be a letter.
--
-- -   Can\'t end with a hyphen or contain two consecutive hyphens.
--
-- Valid for: Aurora DB clusters and Multi-AZ DB clusters
--
-- 'dbSubnetGroupName', 'restoreDBClusterFromSnapshot_dbSubnetGroupName' - The name of the DB subnet group to use for the new DB cluster.
--
-- Constraints: If supplied, must match the name of an existing DB subnet
-- group.
--
-- Example: @mydbsubnetgroup@
--
-- Valid for: Aurora DB clusters and Multi-AZ DB clusters
--
-- 'databaseName', 'restoreDBClusterFromSnapshot_databaseName' - The database name for the restored DB cluster.
--
-- Valid for: Aurora DB clusters and Multi-AZ DB clusters
--
-- 'deletionProtection', 'restoreDBClusterFromSnapshot_deletionProtection' - A value that indicates whether the DB cluster has deletion protection
-- enabled. The database can\'t be deleted when deletion protection is
-- enabled. By default, deletion protection isn\'t enabled.
--
-- Valid for: Aurora DB clusters and Multi-AZ DB clusters
--
-- 'domain', 'restoreDBClusterFromSnapshot_domain' - Specify the Active Directory directory ID to restore the DB cluster in.
-- The domain must be created prior to this operation. Currently, only
-- MySQL, Microsoft SQL Server, Oracle, and PostgreSQL DB instances can be
-- created in an Active Directory Domain.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/kerberos-authentication.html Kerberos Authentication>
-- in the /Amazon RDS User Guide/.
--
-- Valid for: Aurora DB clusters only
--
-- 'domainIAMRoleName', 'restoreDBClusterFromSnapshot_domainIAMRoleName' - Specify the name of the IAM role to be used when making API calls to the
-- Directory Service.
--
-- Valid for: Aurora DB clusters only
--
-- 'enableCloudwatchLogsExports', 'restoreDBClusterFromSnapshot_enableCloudwatchLogsExports' - The list of logs that the restored DB cluster is to export to Amazon
-- CloudWatch Logs. The values in the list depend on the DB engine being
-- used.
--
-- __RDS for MySQL__
--
-- Possible values are @error@, @general@, and @slowquery@.
--
-- __RDS for PostgreSQL__
--
-- Possible values are @postgresql@ and @upgrade@.
--
-- __Aurora MySQL__
--
-- Possible values are @audit@, @error@, @general@, and @slowquery@.
--
-- __Aurora PostgreSQL__
--
-- Possible value is @postgresql@.
--
-- For more information about exporting CloudWatch Logs for Amazon RDS, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_LogAccess.html#USER_LogAccess.Procedural.UploadtoCloudWatch Publishing Database Logs to Amazon CloudWatch Logs>
-- in the /Amazon RDS User Guide/.
--
-- For more information about exporting CloudWatch Logs for Amazon Aurora,
-- see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/USER_LogAccess.html#USER_LogAccess.Procedural.UploadtoCloudWatch Publishing Database Logs to Amazon CloudWatch Logs>
-- in the /Amazon Aurora User Guide/.
--
-- Valid for: Aurora DB clusters and Multi-AZ DB clusters
--
-- 'enableIAMDatabaseAuthentication', 'restoreDBClusterFromSnapshot_enableIAMDatabaseAuthentication' - A value that indicates whether to enable mapping of Amazon Web Services
-- Identity and Access Management (IAM) accounts to database accounts. By
-- default, mapping isn\'t enabled.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/UsingWithRDS.IAMDBAuth.html IAM Database Authentication>
-- in the /Amazon Aurora User Guide/.
--
-- Valid for: Aurora DB clusters only
--
-- 'engineMode', 'restoreDBClusterFromSnapshot_engineMode' - The DB engine mode of the DB cluster, either @provisioned@,
-- @serverless@, @parallelquery@, @global@, or @multimaster@.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/APIReference/API_CreateDBCluster.html CreateDBCluster>.
--
-- Valid for: Aurora DB clusters only
--
-- 'engineVersion', 'restoreDBClusterFromSnapshot_engineVersion' - The version of the database engine to use for the new DB cluster.
--
-- To list all of the available engine versions for MySQL 5.6-compatible
-- Aurora, use the following command:
--
-- @aws rds describe-db-engine-versions --engine aurora --query \"DBEngineVersions[].EngineVersion\"@
--
-- To list all of the available engine versions for MySQL 5.7-compatible
-- and MySQL 8.0-compatible Aurora, use the following command:
--
-- @aws rds describe-db-engine-versions --engine aurora-mysql --query \"DBEngineVersions[].EngineVersion\"@
--
-- To list all of the available engine versions for Aurora PostgreSQL, use
-- the following command:
--
-- @aws rds describe-db-engine-versions --engine aurora-postgresql --query \"DBEngineVersions[].EngineVersion\"@
--
-- To list all of the available engine versions for RDS for MySQL, use the
-- following command:
--
-- @aws rds describe-db-engine-versions --engine mysql --query \"DBEngineVersions[].EngineVersion\"@
--
-- To list all of the available engine versions for RDS for PostgreSQL, use
-- the following command:
--
-- @aws rds describe-db-engine-versions --engine postgres --query \"DBEngineVersions[].EngineVersion\"@
--
-- __Aurora MySQL__
--
-- See
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/AuroraMySQL.Updates.html MySQL on Amazon RDS Versions>
-- in the /Amazon Aurora User Guide/.
--
-- __Aurora PostgreSQL__
--
-- See
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/AuroraPostgreSQL.Updates.20180305.html Amazon Aurora PostgreSQL releases and engine versions>
-- in the /Amazon Aurora User Guide/.
--
-- __MySQL__
--
-- See
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/CHAP_MySQL.html#MySQL.Concepts.VersionMgmt MySQL on Amazon RDS Versions>
-- in the /Amazon RDS User Guide./
--
-- __PostgreSQL__
--
-- See
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/CHAP_PostgreSQL.html#PostgreSQL.Concepts Amazon RDS for PostgreSQL versions and extensions>
-- in the /Amazon RDS User Guide./
--
-- Valid for: Aurora DB clusters and Multi-AZ DB clusters
--
-- 'iops', 'restoreDBClusterFromSnapshot_iops' - The amount of Provisioned IOPS (input\/output operations per second) to
-- be initially allocated for each DB instance in the Multi-AZ DB cluster.
--
-- For information about valid IOPS values, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/CHAP_Storage.html#USER_PIOPS Amazon RDS Provisioned IOPS storage>
-- in the /Amazon RDS User Guide/.
--
-- Constraints: Must be a multiple between .5 and 50 of the storage amount
-- for the DB instance.
--
-- Valid for: Aurora DB clusters and Multi-AZ DB clusters
--
-- 'kmsKeyId', 'restoreDBClusterFromSnapshot_kmsKeyId' - The Amazon Web Services KMS key identifier to use when restoring an
-- encrypted DB cluster from a DB snapshot or DB cluster snapshot.
--
-- The Amazon Web Services KMS key identifier is the key ARN, key ID, alias
-- ARN, or alias name for the KMS key. To use a KMS key in a different
-- Amazon Web Services account, specify the key ARN or alias ARN.
--
-- When you don\'t specify a value for the @KmsKeyId@ parameter, then the
-- following occurs:
--
-- -   If the DB snapshot or DB cluster snapshot in @SnapshotIdentifier@ is
--     encrypted, then the restored DB cluster is encrypted using the KMS
--     key that was used to encrypt the DB snapshot or DB cluster snapshot.
--
-- -   If the DB snapshot or DB cluster snapshot in @SnapshotIdentifier@
--     isn\'t encrypted, then the restored DB cluster isn\'t encrypted.
--
-- Valid for: Aurora DB clusters and Multi-AZ DB clusters
--
-- 'networkType', 'restoreDBClusterFromSnapshot_networkType' - The network type of the DB cluster.
--
-- Valid values:
--
-- -   @IPV4@
--
-- -   @DUAL@
--
-- The network type is determined by the @DBSubnetGroup@ specified for the
-- DB cluster. A @DBSubnetGroup@ can support only the IPv4 protocol or the
-- IPv4 and the IPv6 protocols (@DUAL@).
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/USER_VPC.WorkingWithRDSInstanceinaVPC.html Working with a DB instance in a VPC>
-- in the /Amazon Aurora User Guide./
--
-- Valid for: Aurora DB clusters only
--
-- 'optionGroupName', 'restoreDBClusterFromSnapshot_optionGroupName' - The name of the option group to use for the restored DB cluster.
--
-- DB clusters are associated with a default option group that can\'t be
-- modified.
--
-- 'port', 'restoreDBClusterFromSnapshot_port' - The port number on which the new DB cluster accepts connections.
--
-- Constraints: This value must be @1150-65535@
--
-- Default: The same port as the original DB cluster.
--
-- Valid for: Aurora DB clusters and Multi-AZ DB clusters
--
-- 'publiclyAccessible', 'restoreDBClusterFromSnapshot_publiclyAccessible' - A value that indicates whether the DB cluster is publicly accessible.
--
-- When the DB cluster is publicly accessible, its Domain Name System (DNS)
-- endpoint resolves to the private IP address from within the DB
-- cluster\'s virtual private cloud (VPC). It resolves to the public IP
-- address from outside of the DB cluster\'s VPC. Access to the DB cluster
-- is ultimately controlled by the security group it uses. That public
-- access is not permitted if the security group assigned to the DB cluster
-- doesn\'t permit it.
--
-- When the DB cluster isn\'t publicly accessible, it is an internal DB
-- cluster with a DNS name that resolves to a private IP address.
--
-- Default: The default behavior varies depending on whether
-- @DBSubnetGroupName@ is specified.
--
-- If @DBSubnetGroupName@ isn\'t specified, and @PubliclyAccessible@ isn\'t
-- specified, the following applies:
--
-- -   If the default VPC in the target Region doesn’t have an internet
--     gateway attached to it, the DB cluster is private.
--
-- -   If the default VPC in the target Region has an internet gateway
--     attached to it, the DB cluster is public.
--
-- If @DBSubnetGroupName@ is specified, and @PubliclyAccessible@ isn\'t
-- specified, the following applies:
--
-- -   If the subnets are part of a VPC that doesn’t have an internet
--     gateway attached to it, the DB cluster is private.
--
-- -   If the subnets are part of a VPC that has an internet gateway
--     attached to it, the DB cluster is public.
--
-- Valid for: Aurora DB clusters and Multi-AZ DB clusters
--
-- 'scalingConfiguration', 'restoreDBClusterFromSnapshot_scalingConfiguration' - For DB clusters in @serverless@ DB engine mode, the scaling properties
-- of the DB cluster.
--
-- Valid for: Aurora DB clusters only
--
-- 'serverlessV2ScalingConfiguration', 'restoreDBClusterFromSnapshot_serverlessV2ScalingConfiguration' - Undocumented member.
--
-- 'storageType', 'restoreDBClusterFromSnapshot_storageType' - Specifies the storage type to be associated with the each DB instance in
-- the Multi-AZ DB cluster.
--
-- Valid values: @io1@
--
-- When specified, a value for the @Iops@ parameter is required.
--
-- Default: @io1@
--
-- Valid for: Aurora DB clusters and Multi-AZ DB clusters
--
-- 'tags', 'restoreDBClusterFromSnapshot_tags' - The tags to be assigned to the restored DB cluster.
--
-- Valid for: Aurora DB clusters and Multi-AZ DB clusters
--
-- 'vpcSecurityGroupIds', 'restoreDBClusterFromSnapshot_vpcSecurityGroupIds' - A list of VPC security groups that the new DB cluster will belong to.
--
-- Valid for: Aurora DB clusters and Multi-AZ DB clusters
--
-- 'dbClusterIdentifier', 'restoreDBClusterFromSnapshot_dbClusterIdentifier' - The name of the DB cluster to create from the DB snapshot or DB cluster
-- snapshot. This parameter isn\'t case-sensitive.
--
-- Constraints:
--
-- -   Must contain from 1 to 63 letters, numbers, or hyphens
--
-- -   First character must be a letter
--
-- -   Can\'t end with a hyphen or contain two consecutive hyphens
--
-- Example: @my-snapshot-id@
--
-- Valid for: Aurora DB clusters and Multi-AZ DB clusters
--
-- 'snapshotIdentifier', 'restoreDBClusterFromSnapshot_snapshotIdentifier' - The identifier for the DB snapshot or DB cluster snapshot to restore
-- from.
--
-- You can use either the name or the Amazon Resource Name (ARN) to specify
-- a DB cluster snapshot. However, you can use only the ARN to specify a DB
-- snapshot.
--
-- Constraints:
--
-- -   Must match the identifier of an existing Snapshot.
--
-- Valid for: Aurora DB clusters and Multi-AZ DB clusters
--
-- 'engine', 'restoreDBClusterFromSnapshot_engine' - The database engine to use for the new DB cluster.
--
-- Default: The same as source
--
-- Constraint: Must be compatible with the engine of the source
--
-- Valid for: Aurora DB clusters and Multi-AZ DB clusters
newRestoreDBClusterFromSnapshot ::
  -- | 'dbClusterIdentifier'
  Prelude.Text ->
  -- | 'snapshotIdentifier'
  Prelude.Text ->
  -- | 'engine'
  Prelude.Text ->
  RestoreDBClusterFromSnapshot
newRestoreDBClusterFromSnapshot
  pDBClusterIdentifier_
  pSnapshotIdentifier_
  pEngine_ =
    RestoreDBClusterFromSnapshot'
      { availabilityZones =
          Prelude.Nothing,
        backtrackWindow = Prelude.Nothing,
        copyTagsToSnapshot = Prelude.Nothing,
        dbClusterInstanceClass = Prelude.Nothing,
        dbClusterParameterGroupName = Prelude.Nothing,
        dbSubnetGroupName = Prelude.Nothing,
        databaseName = Prelude.Nothing,
        deletionProtection = Prelude.Nothing,
        domain = Prelude.Nothing,
        domainIAMRoleName = Prelude.Nothing,
        enableCloudwatchLogsExports = Prelude.Nothing,
        enableIAMDatabaseAuthentication =
          Prelude.Nothing,
        engineMode = Prelude.Nothing,
        engineVersion = Prelude.Nothing,
        iops = Prelude.Nothing,
        kmsKeyId = Prelude.Nothing,
        networkType = Prelude.Nothing,
        optionGroupName = Prelude.Nothing,
        port = Prelude.Nothing,
        publiclyAccessible = Prelude.Nothing,
        scalingConfiguration = Prelude.Nothing,
        serverlessV2ScalingConfiguration =
          Prelude.Nothing,
        storageType = Prelude.Nothing,
        tags = Prelude.Nothing,
        vpcSecurityGroupIds = Prelude.Nothing,
        dbClusterIdentifier = pDBClusterIdentifier_,
        snapshotIdentifier = pSnapshotIdentifier_,
        engine = pEngine_
      }

-- | Provides the list of Availability Zones (AZs) where instances in the
-- restored DB cluster can be created.
--
-- Valid for: Aurora DB clusters only
restoreDBClusterFromSnapshot_availabilityZones :: Lens.Lens' RestoreDBClusterFromSnapshot (Prelude.Maybe [Prelude.Text])
restoreDBClusterFromSnapshot_availabilityZones = Lens.lens (\RestoreDBClusterFromSnapshot' {availabilityZones} -> availabilityZones) (\s@RestoreDBClusterFromSnapshot' {} a -> s {availabilityZones = a} :: RestoreDBClusterFromSnapshot) Prelude.. Lens.mapping Lens.coerced

-- | The target backtrack window, in seconds. To disable backtracking, set
-- this value to 0.
--
-- Currently, Backtrack is only supported for Aurora MySQL DB clusters.
--
-- Default: 0
--
-- Constraints:
--
-- -   If specified, this value must be set to a number from 0 to 259,200
--     (72 hours).
--
-- Valid for: Aurora DB clusters only
restoreDBClusterFromSnapshot_backtrackWindow :: Lens.Lens' RestoreDBClusterFromSnapshot (Prelude.Maybe Prelude.Integer)
restoreDBClusterFromSnapshot_backtrackWindow = Lens.lens (\RestoreDBClusterFromSnapshot' {backtrackWindow} -> backtrackWindow) (\s@RestoreDBClusterFromSnapshot' {} a -> s {backtrackWindow = a} :: RestoreDBClusterFromSnapshot)

-- | A value that indicates whether to copy all tags from the restored DB
-- cluster to snapshots of the restored DB cluster. The default is not to
-- copy them.
--
-- Valid for: Aurora DB clusters and Multi-AZ DB clusters
restoreDBClusterFromSnapshot_copyTagsToSnapshot :: Lens.Lens' RestoreDBClusterFromSnapshot (Prelude.Maybe Prelude.Bool)
restoreDBClusterFromSnapshot_copyTagsToSnapshot = Lens.lens (\RestoreDBClusterFromSnapshot' {copyTagsToSnapshot} -> copyTagsToSnapshot) (\s@RestoreDBClusterFromSnapshot' {} a -> s {copyTagsToSnapshot = a} :: RestoreDBClusterFromSnapshot)

-- | The compute and memory capacity of the each DB instance in the Multi-AZ
-- DB cluster, for example db.m6gd.xlarge. Not all DB instance classes are
-- available in all Amazon Web Services Regions, or for all database
-- engines.
--
-- For the full list of DB instance classes, and availability for your
-- engine, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Concepts.DBInstanceClass.html DB Instance Class>
-- in the /Amazon RDS User Guide./
--
-- Valid for: Multi-AZ DB clusters only
restoreDBClusterFromSnapshot_dbClusterInstanceClass :: Lens.Lens' RestoreDBClusterFromSnapshot (Prelude.Maybe Prelude.Text)
restoreDBClusterFromSnapshot_dbClusterInstanceClass = Lens.lens (\RestoreDBClusterFromSnapshot' {dbClusterInstanceClass} -> dbClusterInstanceClass) (\s@RestoreDBClusterFromSnapshot' {} a -> s {dbClusterInstanceClass = a} :: RestoreDBClusterFromSnapshot)

-- | The name of the DB cluster parameter group to associate with this DB
-- cluster. If this argument is omitted, the default DB cluster parameter
-- group for the specified engine is used.
--
-- Constraints:
--
-- -   If supplied, must match the name of an existing default DB cluster
--     parameter group.
--
-- -   Must be 1 to 255 letters, numbers, or hyphens.
--
-- -   First character must be a letter.
--
-- -   Can\'t end with a hyphen or contain two consecutive hyphens.
--
-- Valid for: Aurora DB clusters and Multi-AZ DB clusters
restoreDBClusterFromSnapshot_dbClusterParameterGroupName :: Lens.Lens' RestoreDBClusterFromSnapshot (Prelude.Maybe Prelude.Text)
restoreDBClusterFromSnapshot_dbClusterParameterGroupName = Lens.lens (\RestoreDBClusterFromSnapshot' {dbClusterParameterGroupName} -> dbClusterParameterGroupName) (\s@RestoreDBClusterFromSnapshot' {} a -> s {dbClusterParameterGroupName = a} :: RestoreDBClusterFromSnapshot)

-- | The name of the DB subnet group to use for the new DB cluster.
--
-- Constraints: If supplied, must match the name of an existing DB subnet
-- group.
--
-- Example: @mydbsubnetgroup@
--
-- Valid for: Aurora DB clusters and Multi-AZ DB clusters
restoreDBClusterFromSnapshot_dbSubnetGroupName :: Lens.Lens' RestoreDBClusterFromSnapshot (Prelude.Maybe Prelude.Text)
restoreDBClusterFromSnapshot_dbSubnetGroupName = Lens.lens (\RestoreDBClusterFromSnapshot' {dbSubnetGroupName} -> dbSubnetGroupName) (\s@RestoreDBClusterFromSnapshot' {} a -> s {dbSubnetGroupName = a} :: RestoreDBClusterFromSnapshot)

-- | The database name for the restored DB cluster.
--
-- Valid for: Aurora DB clusters and Multi-AZ DB clusters
restoreDBClusterFromSnapshot_databaseName :: Lens.Lens' RestoreDBClusterFromSnapshot (Prelude.Maybe Prelude.Text)
restoreDBClusterFromSnapshot_databaseName = Lens.lens (\RestoreDBClusterFromSnapshot' {databaseName} -> databaseName) (\s@RestoreDBClusterFromSnapshot' {} a -> s {databaseName = a} :: RestoreDBClusterFromSnapshot)

-- | A value that indicates whether the DB cluster has deletion protection
-- enabled. The database can\'t be deleted when deletion protection is
-- enabled. By default, deletion protection isn\'t enabled.
--
-- Valid for: Aurora DB clusters and Multi-AZ DB clusters
restoreDBClusterFromSnapshot_deletionProtection :: Lens.Lens' RestoreDBClusterFromSnapshot (Prelude.Maybe Prelude.Bool)
restoreDBClusterFromSnapshot_deletionProtection = Lens.lens (\RestoreDBClusterFromSnapshot' {deletionProtection} -> deletionProtection) (\s@RestoreDBClusterFromSnapshot' {} a -> s {deletionProtection = a} :: RestoreDBClusterFromSnapshot)

-- | Specify the Active Directory directory ID to restore the DB cluster in.
-- The domain must be created prior to this operation. Currently, only
-- MySQL, Microsoft SQL Server, Oracle, and PostgreSQL DB instances can be
-- created in an Active Directory Domain.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/kerberos-authentication.html Kerberos Authentication>
-- in the /Amazon RDS User Guide/.
--
-- Valid for: Aurora DB clusters only
restoreDBClusterFromSnapshot_domain :: Lens.Lens' RestoreDBClusterFromSnapshot (Prelude.Maybe Prelude.Text)
restoreDBClusterFromSnapshot_domain = Lens.lens (\RestoreDBClusterFromSnapshot' {domain} -> domain) (\s@RestoreDBClusterFromSnapshot' {} a -> s {domain = a} :: RestoreDBClusterFromSnapshot)

-- | Specify the name of the IAM role to be used when making API calls to the
-- Directory Service.
--
-- Valid for: Aurora DB clusters only
restoreDBClusterFromSnapshot_domainIAMRoleName :: Lens.Lens' RestoreDBClusterFromSnapshot (Prelude.Maybe Prelude.Text)
restoreDBClusterFromSnapshot_domainIAMRoleName = Lens.lens (\RestoreDBClusterFromSnapshot' {domainIAMRoleName} -> domainIAMRoleName) (\s@RestoreDBClusterFromSnapshot' {} a -> s {domainIAMRoleName = a} :: RestoreDBClusterFromSnapshot)

-- | The list of logs that the restored DB cluster is to export to Amazon
-- CloudWatch Logs. The values in the list depend on the DB engine being
-- used.
--
-- __RDS for MySQL__
--
-- Possible values are @error@, @general@, and @slowquery@.
--
-- __RDS for PostgreSQL__
--
-- Possible values are @postgresql@ and @upgrade@.
--
-- __Aurora MySQL__
--
-- Possible values are @audit@, @error@, @general@, and @slowquery@.
--
-- __Aurora PostgreSQL__
--
-- Possible value is @postgresql@.
--
-- For more information about exporting CloudWatch Logs for Amazon RDS, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_LogAccess.html#USER_LogAccess.Procedural.UploadtoCloudWatch Publishing Database Logs to Amazon CloudWatch Logs>
-- in the /Amazon RDS User Guide/.
--
-- For more information about exporting CloudWatch Logs for Amazon Aurora,
-- see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/USER_LogAccess.html#USER_LogAccess.Procedural.UploadtoCloudWatch Publishing Database Logs to Amazon CloudWatch Logs>
-- in the /Amazon Aurora User Guide/.
--
-- Valid for: Aurora DB clusters and Multi-AZ DB clusters
restoreDBClusterFromSnapshot_enableCloudwatchLogsExports :: Lens.Lens' RestoreDBClusterFromSnapshot (Prelude.Maybe [Prelude.Text])
restoreDBClusterFromSnapshot_enableCloudwatchLogsExports = Lens.lens (\RestoreDBClusterFromSnapshot' {enableCloudwatchLogsExports} -> enableCloudwatchLogsExports) (\s@RestoreDBClusterFromSnapshot' {} a -> s {enableCloudwatchLogsExports = a} :: RestoreDBClusterFromSnapshot) Prelude.. Lens.mapping Lens.coerced

-- | A value that indicates whether to enable mapping of Amazon Web Services
-- Identity and Access Management (IAM) accounts to database accounts. By
-- default, mapping isn\'t enabled.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/UsingWithRDS.IAMDBAuth.html IAM Database Authentication>
-- in the /Amazon Aurora User Guide/.
--
-- Valid for: Aurora DB clusters only
restoreDBClusterFromSnapshot_enableIAMDatabaseAuthentication :: Lens.Lens' RestoreDBClusterFromSnapshot (Prelude.Maybe Prelude.Bool)
restoreDBClusterFromSnapshot_enableIAMDatabaseAuthentication = Lens.lens (\RestoreDBClusterFromSnapshot' {enableIAMDatabaseAuthentication} -> enableIAMDatabaseAuthentication) (\s@RestoreDBClusterFromSnapshot' {} a -> s {enableIAMDatabaseAuthentication = a} :: RestoreDBClusterFromSnapshot)

-- | The DB engine mode of the DB cluster, either @provisioned@,
-- @serverless@, @parallelquery@, @global@, or @multimaster@.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/APIReference/API_CreateDBCluster.html CreateDBCluster>.
--
-- Valid for: Aurora DB clusters only
restoreDBClusterFromSnapshot_engineMode :: Lens.Lens' RestoreDBClusterFromSnapshot (Prelude.Maybe Prelude.Text)
restoreDBClusterFromSnapshot_engineMode = Lens.lens (\RestoreDBClusterFromSnapshot' {engineMode} -> engineMode) (\s@RestoreDBClusterFromSnapshot' {} a -> s {engineMode = a} :: RestoreDBClusterFromSnapshot)

-- | The version of the database engine to use for the new DB cluster.
--
-- To list all of the available engine versions for MySQL 5.6-compatible
-- Aurora, use the following command:
--
-- @aws rds describe-db-engine-versions --engine aurora --query \"DBEngineVersions[].EngineVersion\"@
--
-- To list all of the available engine versions for MySQL 5.7-compatible
-- and MySQL 8.0-compatible Aurora, use the following command:
--
-- @aws rds describe-db-engine-versions --engine aurora-mysql --query \"DBEngineVersions[].EngineVersion\"@
--
-- To list all of the available engine versions for Aurora PostgreSQL, use
-- the following command:
--
-- @aws rds describe-db-engine-versions --engine aurora-postgresql --query \"DBEngineVersions[].EngineVersion\"@
--
-- To list all of the available engine versions for RDS for MySQL, use the
-- following command:
--
-- @aws rds describe-db-engine-versions --engine mysql --query \"DBEngineVersions[].EngineVersion\"@
--
-- To list all of the available engine versions for RDS for PostgreSQL, use
-- the following command:
--
-- @aws rds describe-db-engine-versions --engine postgres --query \"DBEngineVersions[].EngineVersion\"@
--
-- __Aurora MySQL__
--
-- See
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/AuroraMySQL.Updates.html MySQL on Amazon RDS Versions>
-- in the /Amazon Aurora User Guide/.
--
-- __Aurora PostgreSQL__
--
-- See
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/AuroraPostgreSQL.Updates.20180305.html Amazon Aurora PostgreSQL releases and engine versions>
-- in the /Amazon Aurora User Guide/.
--
-- __MySQL__
--
-- See
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/CHAP_MySQL.html#MySQL.Concepts.VersionMgmt MySQL on Amazon RDS Versions>
-- in the /Amazon RDS User Guide./
--
-- __PostgreSQL__
--
-- See
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/CHAP_PostgreSQL.html#PostgreSQL.Concepts Amazon RDS for PostgreSQL versions and extensions>
-- in the /Amazon RDS User Guide./
--
-- Valid for: Aurora DB clusters and Multi-AZ DB clusters
restoreDBClusterFromSnapshot_engineVersion :: Lens.Lens' RestoreDBClusterFromSnapshot (Prelude.Maybe Prelude.Text)
restoreDBClusterFromSnapshot_engineVersion = Lens.lens (\RestoreDBClusterFromSnapshot' {engineVersion} -> engineVersion) (\s@RestoreDBClusterFromSnapshot' {} a -> s {engineVersion = a} :: RestoreDBClusterFromSnapshot)

-- | The amount of Provisioned IOPS (input\/output operations per second) to
-- be initially allocated for each DB instance in the Multi-AZ DB cluster.
--
-- For information about valid IOPS values, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/CHAP_Storage.html#USER_PIOPS Amazon RDS Provisioned IOPS storage>
-- in the /Amazon RDS User Guide/.
--
-- Constraints: Must be a multiple between .5 and 50 of the storage amount
-- for the DB instance.
--
-- Valid for: Aurora DB clusters and Multi-AZ DB clusters
restoreDBClusterFromSnapshot_iops :: Lens.Lens' RestoreDBClusterFromSnapshot (Prelude.Maybe Prelude.Int)
restoreDBClusterFromSnapshot_iops = Lens.lens (\RestoreDBClusterFromSnapshot' {iops} -> iops) (\s@RestoreDBClusterFromSnapshot' {} a -> s {iops = a} :: RestoreDBClusterFromSnapshot)

-- | The Amazon Web Services KMS key identifier to use when restoring an
-- encrypted DB cluster from a DB snapshot or DB cluster snapshot.
--
-- The Amazon Web Services KMS key identifier is the key ARN, key ID, alias
-- ARN, or alias name for the KMS key. To use a KMS key in a different
-- Amazon Web Services account, specify the key ARN or alias ARN.
--
-- When you don\'t specify a value for the @KmsKeyId@ parameter, then the
-- following occurs:
--
-- -   If the DB snapshot or DB cluster snapshot in @SnapshotIdentifier@ is
--     encrypted, then the restored DB cluster is encrypted using the KMS
--     key that was used to encrypt the DB snapshot or DB cluster snapshot.
--
-- -   If the DB snapshot or DB cluster snapshot in @SnapshotIdentifier@
--     isn\'t encrypted, then the restored DB cluster isn\'t encrypted.
--
-- Valid for: Aurora DB clusters and Multi-AZ DB clusters
restoreDBClusterFromSnapshot_kmsKeyId :: Lens.Lens' RestoreDBClusterFromSnapshot (Prelude.Maybe Prelude.Text)
restoreDBClusterFromSnapshot_kmsKeyId = Lens.lens (\RestoreDBClusterFromSnapshot' {kmsKeyId} -> kmsKeyId) (\s@RestoreDBClusterFromSnapshot' {} a -> s {kmsKeyId = a} :: RestoreDBClusterFromSnapshot)

-- | The network type of the DB cluster.
--
-- Valid values:
--
-- -   @IPV4@
--
-- -   @DUAL@
--
-- The network type is determined by the @DBSubnetGroup@ specified for the
-- DB cluster. A @DBSubnetGroup@ can support only the IPv4 protocol or the
-- IPv4 and the IPv6 protocols (@DUAL@).
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/USER_VPC.WorkingWithRDSInstanceinaVPC.html Working with a DB instance in a VPC>
-- in the /Amazon Aurora User Guide./
--
-- Valid for: Aurora DB clusters only
restoreDBClusterFromSnapshot_networkType :: Lens.Lens' RestoreDBClusterFromSnapshot (Prelude.Maybe Prelude.Text)
restoreDBClusterFromSnapshot_networkType = Lens.lens (\RestoreDBClusterFromSnapshot' {networkType} -> networkType) (\s@RestoreDBClusterFromSnapshot' {} a -> s {networkType = a} :: RestoreDBClusterFromSnapshot)

-- | The name of the option group to use for the restored DB cluster.
--
-- DB clusters are associated with a default option group that can\'t be
-- modified.
restoreDBClusterFromSnapshot_optionGroupName :: Lens.Lens' RestoreDBClusterFromSnapshot (Prelude.Maybe Prelude.Text)
restoreDBClusterFromSnapshot_optionGroupName = Lens.lens (\RestoreDBClusterFromSnapshot' {optionGroupName} -> optionGroupName) (\s@RestoreDBClusterFromSnapshot' {} a -> s {optionGroupName = a} :: RestoreDBClusterFromSnapshot)

-- | The port number on which the new DB cluster accepts connections.
--
-- Constraints: This value must be @1150-65535@
--
-- Default: The same port as the original DB cluster.
--
-- Valid for: Aurora DB clusters and Multi-AZ DB clusters
restoreDBClusterFromSnapshot_port :: Lens.Lens' RestoreDBClusterFromSnapshot (Prelude.Maybe Prelude.Int)
restoreDBClusterFromSnapshot_port = Lens.lens (\RestoreDBClusterFromSnapshot' {port} -> port) (\s@RestoreDBClusterFromSnapshot' {} a -> s {port = a} :: RestoreDBClusterFromSnapshot)

-- | A value that indicates whether the DB cluster is publicly accessible.
--
-- When the DB cluster is publicly accessible, its Domain Name System (DNS)
-- endpoint resolves to the private IP address from within the DB
-- cluster\'s virtual private cloud (VPC). It resolves to the public IP
-- address from outside of the DB cluster\'s VPC. Access to the DB cluster
-- is ultimately controlled by the security group it uses. That public
-- access is not permitted if the security group assigned to the DB cluster
-- doesn\'t permit it.
--
-- When the DB cluster isn\'t publicly accessible, it is an internal DB
-- cluster with a DNS name that resolves to a private IP address.
--
-- Default: The default behavior varies depending on whether
-- @DBSubnetGroupName@ is specified.
--
-- If @DBSubnetGroupName@ isn\'t specified, and @PubliclyAccessible@ isn\'t
-- specified, the following applies:
--
-- -   If the default VPC in the target Region doesn’t have an internet
--     gateway attached to it, the DB cluster is private.
--
-- -   If the default VPC in the target Region has an internet gateway
--     attached to it, the DB cluster is public.
--
-- If @DBSubnetGroupName@ is specified, and @PubliclyAccessible@ isn\'t
-- specified, the following applies:
--
-- -   If the subnets are part of a VPC that doesn’t have an internet
--     gateway attached to it, the DB cluster is private.
--
-- -   If the subnets are part of a VPC that has an internet gateway
--     attached to it, the DB cluster is public.
--
-- Valid for: Aurora DB clusters and Multi-AZ DB clusters
restoreDBClusterFromSnapshot_publiclyAccessible :: Lens.Lens' RestoreDBClusterFromSnapshot (Prelude.Maybe Prelude.Bool)
restoreDBClusterFromSnapshot_publiclyAccessible = Lens.lens (\RestoreDBClusterFromSnapshot' {publiclyAccessible} -> publiclyAccessible) (\s@RestoreDBClusterFromSnapshot' {} a -> s {publiclyAccessible = a} :: RestoreDBClusterFromSnapshot)

-- | For DB clusters in @serverless@ DB engine mode, the scaling properties
-- of the DB cluster.
--
-- Valid for: Aurora DB clusters only
restoreDBClusterFromSnapshot_scalingConfiguration :: Lens.Lens' RestoreDBClusterFromSnapshot (Prelude.Maybe ScalingConfiguration)
restoreDBClusterFromSnapshot_scalingConfiguration = Lens.lens (\RestoreDBClusterFromSnapshot' {scalingConfiguration} -> scalingConfiguration) (\s@RestoreDBClusterFromSnapshot' {} a -> s {scalingConfiguration = a} :: RestoreDBClusterFromSnapshot)

-- | Undocumented member.
restoreDBClusterFromSnapshot_serverlessV2ScalingConfiguration :: Lens.Lens' RestoreDBClusterFromSnapshot (Prelude.Maybe ServerlessV2ScalingConfiguration)
restoreDBClusterFromSnapshot_serverlessV2ScalingConfiguration = Lens.lens (\RestoreDBClusterFromSnapshot' {serverlessV2ScalingConfiguration} -> serverlessV2ScalingConfiguration) (\s@RestoreDBClusterFromSnapshot' {} a -> s {serverlessV2ScalingConfiguration = a} :: RestoreDBClusterFromSnapshot)

-- | Specifies the storage type to be associated with the each DB instance in
-- the Multi-AZ DB cluster.
--
-- Valid values: @io1@
--
-- When specified, a value for the @Iops@ parameter is required.
--
-- Default: @io1@
--
-- Valid for: Aurora DB clusters and Multi-AZ DB clusters
restoreDBClusterFromSnapshot_storageType :: Lens.Lens' RestoreDBClusterFromSnapshot (Prelude.Maybe Prelude.Text)
restoreDBClusterFromSnapshot_storageType = Lens.lens (\RestoreDBClusterFromSnapshot' {storageType} -> storageType) (\s@RestoreDBClusterFromSnapshot' {} a -> s {storageType = a} :: RestoreDBClusterFromSnapshot)

-- | The tags to be assigned to the restored DB cluster.
--
-- Valid for: Aurora DB clusters and Multi-AZ DB clusters
restoreDBClusterFromSnapshot_tags :: Lens.Lens' RestoreDBClusterFromSnapshot (Prelude.Maybe [Tag])
restoreDBClusterFromSnapshot_tags = Lens.lens (\RestoreDBClusterFromSnapshot' {tags} -> tags) (\s@RestoreDBClusterFromSnapshot' {} a -> s {tags = a} :: RestoreDBClusterFromSnapshot) Prelude.. Lens.mapping Lens.coerced

-- | A list of VPC security groups that the new DB cluster will belong to.
--
-- Valid for: Aurora DB clusters and Multi-AZ DB clusters
restoreDBClusterFromSnapshot_vpcSecurityGroupIds :: Lens.Lens' RestoreDBClusterFromSnapshot (Prelude.Maybe [Prelude.Text])
restoreDBClusterFromSnapshot_vpcSecurityGroupIds = Lens.lens (\RestoreDBClusterFromSnapshot' {vpcSecurityGroupIds} -> vpcSecurityGroupIds) (\s@RestoreDBClusterFromSnapshot' {} a -> s {vpcSecurityGroupIds = a} :: RestoreDBClusterFromSnapshot) Prelude.. Lens.mapping Lens.coerced

-- | The name of the DB cluster to create from the DB snapshot or DB cluster
-- snapshot. This parameter isn\'t case-sensitive.
--
-- Constraints:
--
-- -   Must contain from 1 to 63 letters, numbers, or hyphens
--
-- -   First character must be a letter
--
-- -   Can\'t end with a hyphen or contain two consecutive hyphens
--
-- Example: @my-snapshot-id@
--
-- Valid for: Aurora DB clusters and Multi-AZ DB clusters
restoreDBClusterFromSnapshot_dbClusterIdentifier :: Lens.Lens' RestoreDBClusterFromSnapshot Prelude.Text
restoreDBClusterFromSnapshot_dbClusterIdentifier = Lens.lens (\RestoreDBClusterFromSnapshot' {dbClusterIdentifier} -> dbClusterIdentifier) (\s@RestoreDBClusterFromSnapshot' {} a -> s {dbClusterIdentifier = a} :: RestoreDBClusterFromSnapshot)

-- | The identifier for the DB snapshot or DB cluster snapshot to restore
-- from.
--
-- You can use either the name or the Amazon Resource Name (ARN) to specify
-- a DB cluster snapshot. However, you can use only the ARN to specify a DB
-- snapshot.
--
-- Constraints:
--
-- -   Must match the identifier of an existing Snapshot.
--
-- Valid for: Aurora DB clusters and Multi-AZ DB clusters
restoreDBClusterFromSnapshot_snapshotIdentifier :: Lens.Lens' RestoreDBClusterFromSnapshot Prelude.Text
restoreDBClusterFromSnapshot_snapshotIdentifier = Lens.lens (\RestoreDBClusterFromSnapshot' {snapshotIdentifier} -> snapshotIdentifier) (\s@RestoreDBClusterFromSnapshot' {} a -> s {snapshotIdentifier = a} :: RestoreDBClusterFromSnapshot)

-- | The database engine to use for the new DB cluster.
--
-- Default: The same as source
--
-- Constraint: Must be compatible with the engine of the source
--
-- Valid for: Aurora DB clusters and Multi-AZ DB clusters
restoreDBClusterFromSnapshot_engine :: Lens.Lens' RestoreDBClusterFromSnapshot Prelude.Text
restoreDBClusterFromSnapshot_engine = Lens.lens (\RestoreDBClusterFromSnapshot' {engine} -> engine) (\s@RestoreDBClusterFromSnapshot' {} a -> s {engine = a} :: RestoreDBClusterFromSnapshot)

instance Core.AWSRequest RestoreDBClusterFromSnapshot where
  type
    AWSResponse RestoreDBClusterFromSnapshot =
      RestoreDBClusterFromSnapshotResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "RestoreDBClusterFromSnapshotResult"
      ( \s h x ->
          RestoreDBClusterFromSnapshotResponse'
            Prelude.<$> (x Data..@? "DBCluster")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    RestoreDBClusterFromSnapshot
  where
  hashWithSalt _salt RestoreDBClusterFromSnapshot' {..} =
    _salt
      `Prelude.hashWithSalt` availabilityZones
      `Prelude.hashWithSalt` backtrackWindow
      `Prelude.hashWithSalt` copyTagsToSnapshot
      `Prelude.hashWithSalt` dbClusterInstanceClass
      `Prelude.hashWithSalt` dbClusterParameterGroupName
      `Prelude.hashWithSalt` dbSubnetGroupName
      `Prelude.hashWithSalt` databaseName
      `Prelude.hashWithSalt` deletionProtection
      `Prelude.hashWithSalt` domain
      `Prelude.hashWithSalt` domainIAMRoleName
      `Prelude.hashWithSalt` enableCloudwatchLogsExports
      `Prelude.hashWithSalt` enableIAMDatabaseAuthentication
      `Prelude.hashWithSalt` engineMode
      `Prelude.hashWithSalt` engineVersion
      `Prelude.hashWithSalt` iops
      `Prelude.hashWithSalt` kmsKeyId
      `Prelude.hashWithSalt` networkType
      `Prelude.hashWithSalt` optionGroupName
      `Prelude.hashWithSalt` port
      `Prelude.hashWithSalt` publiclyAccessible
      `Prelude.hashWithSalt` scalingConfiguration
      `Prelude.hashWithSalt` serverlessV2ScalingConfiguration
      `Prelude.hashWithSalt` storageType
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` vpcSecurityGroupIds
      `Prelude.hashWithSalt` dbClusterIdentifier
      `Prelude.hashWithSalt` snapshotIdentifier
      `Prelude.hashWithSalt` engine

instance Prelude.NFData RestoreDBClusterFromSnapshot where
  rnf RestoreDBClusterFromSnapshot' {..} =
    Prelude.rnf availabilityZones
      `Prelude.seq` Prelude.rnf backtrackWindow
      `Prelude.seq` Prelude.rnf copyTagsToSnapshot
      `Prelude.seq` Prelude.rnf dbClusterInstanceClass
      `Prelude.seq` Prelude.rnf dbClusterParameterGroupName
      `Prelude.seq` Prelude.rnf dbSubnetGroupName
      `Prelude.seq` Prelude.rnf databaseName
      `Prelude.seq` Prelude.rnf deletionProtection
      `Prelude.seq` Prelude.rnf domain
      `Prelude.seq` Prelude.rnf domainIAMRoleName
      `Prelude.seq` Prelude.rnf enableCloudwatchLogsExports
      `Prelude.seq` Prelude.rnf enableIAMDatabaseAuthentication
      `Prelude.seq` Prelude.rnf engineMode
      `Prelude.seq` Prelude.rnf engineVersion
      `Prelude.seq` Prelude.rnf iops
      `Prelude.seq` Prelude.rnf kmsKeyId
      `Prelude.seq` Prelude.rnf networkType
      `Prelude.seq` Prelude.rnf optionGroupName
      `Prelude.seq` Prelude.rnf port
      `Prelude.seq` Prelude.rnf publiclyAccessible
      `Prelude.seq` Prelude.rnf
        scalingConfiguration
      `Prelude.seq` Prelude.rnf
        serverlessV2ScalingConfiguration
      `Prelude.seq` Prelude.rnf storageType
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf
        vpcSecurityGroupIds
      `Prelude.seq` Prelude.rnf
        dbClusterIdentifier
      `Prelude.seq` Prelude.rnf
        snapshotIdentifier
      `Prelude.seq` Prelude.rnf
        engine

instance Data.ToHeaders RestoreDBClusterFromSnapshot where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath RestoreDBClusterFromSnapshot where
  toPath = Prelude.const "/"

instance Data.ToQuery RestoreDBClusterFromSnapshot where
  toQuery RestoreDBClusterFromSnapshot' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "RestoreDBClusterFromSnapshot" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2014-10-31" :: Prelude.ByteString),
        "AvailabilityZones"
          Data.=: Data.toQuery
            ( Data.toQueryList "AvailabilityZone"
                Prelude.<$> availabilityZones
            ),
        "BacktrackWindow" Data.=: backtrackWindow,
        "CopyTagsToSnapshot" Data.=: copyTagsToSnapshot,
        "DBClusterInstanceClass"
          Data.=: dbClusterInstanceClass,
        "DBClusterParameterGroupName"
          Data.=: dbClusterParameterGroupName,
        "DBSubnetGroupName" Data.=: dbSubnetGroupName,
        "DatabaseName" Data.=: databaseName,
        "DeletionProtection" Data.=: deletionProtection,
        "Domain" Data.=: domain,
        "DomainIAMRoleName" Data.=: domainIAMRoleName,
        "EnableCloudwatchLogsExports"
          Data.=: Data.toQuery
            ( Data.toQueryList "member"
                Prelude.<$> enableCloudwatchLogsExports
            ),
        "EnableIAMDatabaseAuthentication"
          Data.=: enableIAMDatabaseAuthentication,
        "EngineMode" Data.=: engineMode,
        "EngineVersion" Data.=: engineVersion,
        "Iops" Data.=: iops,
        "KmsKeyId" Data.=: kmsKeyId,
        "NetworkType" Data.=: networkType,
        "OptionGroupName" Data.=: optionGroupName,
        "Port" Data.=: port,
        "PubliclyAccessible" Data.=: publiclyAccessible,
        "ScalingConfiguration" Data.=: scalingConfiguration,
        "ServerlessV2ScalingConfiguration"
          Data.=: serverlessV2ScalingConfiguration,
        "StorageType" Data.=: storageType,
        "Tags"
          Data.=: Data.toQuery
            (Data.toQueryList "Tag" Prelude.<$> tags),
        "VpcSecurityGroupIds"
          Data.=: Data.toQuery
            ( Data.toQueryList "VpcSecurityGroupId"
                Prelude.<$> vpcSecurityGroupIds
            ),
        "DBClusterIdentifier" Data.=: dbClusterIdentifier,
        "SnapshotIdentifier" Data.=: snapshotIdentifier,
        "Engine" Data.=: engine
      ]

-- | /See:/ 'newRestoreDBClusterFromSnapshotResponse' smart constructor.
data RestoreDBClusterFromSnapshotResponse = RestoreDBClusterFromSnapshotResponse'
  { dbCluster :: Prelude.Maybe DBCluster,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RestoreDBClusterFromSnapshotResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dbCluster', 'restoreDBClusterFromSnapshotResponse_dbCluster' - Undocumented member.
--
-- 'httpStatus', 'restoreDBClusterFromSnapshotResponse_httpStatus' - The response's http status code.
newRestoreDBClusterFromSnapshotResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  RestoreDBClusterFromSnapshotResponse
newRestoreDBClusterFromSnapshotResponse pHttpStatus_ =
  RestoreDBClusterFromSnapshotResponse'
    { dbCluster =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
restoreDBClusterFromSnapshotResponse_dbCluster :: Lens.Lens' RestoreDBClusterFromSnapshotResponse (Prelude.Maybe DBCluster)
restoreDBClusterFromSnapshotResponse_dbCluster = Lens.lens (\RestoreDBClusterFromSnapshotResponse' {dbCluster} -> dbCluster) (\s@RestoreDBClusterFromSnapshotResponse' {} a -> s {dbCluster = a} :: RestoreDBClusterFromSnapshotResponse)

-- | The response's http status code.
restoreDBClusterFromSnapshotResponse_httpStatus :: Lens.Lens' RestoreDBClusterFromSnapshotResponse Prelude.Int
restoreDBClusterFromSnapshotResponse_httpStatus = Lens.lens (\RestoreDBClusterFromSnapshotResponse' {httpStatus} -> httpStatus) (\s@RestoreDBClusterFromSnapshotResponse' {} a -> s {httpStatus = a} :: RestoreDBClusterFromSnapshotResponse)

instance
  Prelude.NFData
    RestoreDBClusterFromSnapshotResponse
  where
  rnf RestoreDBClusterFromSnapshotResponse' {..} =
    Prelude.rnf dbCluster
      `Prelude.seq` Prelude.rnf httpStatus
