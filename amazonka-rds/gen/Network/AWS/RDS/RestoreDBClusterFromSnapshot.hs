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
-- Module      : Network.AWS.RDS.RestoreDBClusterFromSnapshot
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new DB cluster from a DB snapshot or DB cluster snapshot. This
-- action only applies to Aurora DB clusters.
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
-- For more information on Amazon Aurora, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/CHAP_AuroraOverview.html What Is Amazon Aurora?>
-- in the /Amazon Aurora User Guide./
--
-- This action only applies to Aurora DB clusters.
module Network.AWS.RDS.RestoreDBClusterFromSnapshot
  ( -- * Creating a Request
    RestoreDBClusterFromSnapshot (..),
    newRestoreDBClusterFromSnapshot,

    -- * Request Lenses
    restoreDBClusterFromSnapshot_deletionProtection,
    restoreDBClusterFromSnapshot_availabilityZones,
    restoreDBClusterFromSnapshot_enableIAMDatabaseAuthentication,
    restoreDBClusterFromSnapshot_enableCloudwatchLogsExports,
    restoreDBClusterFromSnapshot_optionGroupName,
    restoreDBClusterFromSnapshot_domain,
    restoreDBClusterFromSnapshot_engineMode,
    restoreDBClusterFromSnapshot_scalingConfiguration,
    restoreDBClusterFromSnapshot_dbSubnetGroupName,
    restoreDBClusterFromSnapshot_vpcSecurityGroupIds,
    restoreDBClusterFromSnapshot_kmsKeyId,
    restoreDBClusterFromSnapshot_engineVersion,
    restoreDBClusterFromSnapshot_tags,
    restoreDBClusterFromSnapshot_port,
    restoreDBClusterFromSnapshot_domainIAMRoleName,
    restoreDBClusterFromSnapshot_copyTagsToSnapshot,
    restoreDBClusterFromSnapshot_backtrackWindow,
    restoreDBClusterFromSnapshot_dbClusterParameterGroupName,
    restoreDBClusterFromSnapshot_databaseName,
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.RDS.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'newRestoreDBClusterFromSnapshot' smart constructor.
data RestoreDBClusterFromSnapshot = RestoreDBClusterFromSnapshot'
  { -- | A value that indicates whether the DB cluster has deletion protection
    -- enabled. The database can\'t be deleted when deletion protection is
    -- enabled. By default, deletion protection is disabled.
    deletionProtection :: Prelude.Maybe Prelude.Bool,
    -- | Provides the list of Availability Zones (AZs) where instances in the
    -- restored DB cluster can be created.
    availabilityZones :: Prelude.Maybe [Prelude.Text],
    -- | A value that indicates whether to enable mapping of AWS Identity and
    -- Access Management (IAM) accounts to database accounts. By default,
    -- mapping is disabled.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/UsingWithRDS.IAMDBAuth.html IAM Database Authentication>
    -- in the /Amazon Aurora User Guide./
    enableIAMDatabaseAuthentication :: Prelude.Maybe Prelude.Bool,
    -- | The list of logs that the restored DB cluster is to export to Amazon
    -- CloudWatch Logs. The values in the list depend on the DB engine being
    -- used. For more information, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/USER_LogAccess.html#USER_LogAccess.Procedural.UploadtoCloudWatch Publishing Database Logs to Amazon CloudWatch Logs>
    -- in the /Amazon Aurora User Guide/.
    enableCloudwatchLogsExports :: Prelude.Maybe [Prelude.Text],
    -- | The name of the option group to use for the restored DB cluster.
    optionGroupName :: Prelude.Maybe Prelude.Text,
    -- | Specify the Active Directory directory ID to restore the DB cluster in.
    -- The domain must be created prior to this operation. Currently, only
    -- MySQL, Microsoft SQL Server, Oracle, and PostgreSQL DB instances can be
    -- created in an Active Directory Domain.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/kerberos-authentication.html Kerberos Authentication>
    -- in the /Amazon RDS User Guide/.
    domain :: Prelude.Maybe Prelude.Text,
    -- | The DB engine mode of the DB cluster, either @provisioned@,
    -- @serverless@, @parallelquery@, @global@, or @multimaster@.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/APIReference/API_CreateDBCluster.html CreateDBCluster>.
    engineMode :: Prelude.Maybe Prelude.Text,
    -- | For DB clusters in @serverless@ DB engine mode, the scaling properties
    -- of the DB cluster.
    scalingConfiguration :: Prelude.Maybe ScalingConfiguration,
    -- | The name of the DB subnet group to use for the new DB cluster.
    --
    -- Constraints: If supplied, must match the name of an existing DB subnet
    -- group.
    --
    -- Example: @mySubnetgroup@
    dbSubnetGroupName :: Prelude.Maybe Prelude.Text,
    -- | A list of VPC security groups that the new DB cluster will belong to.
    vpcSecurityGroupIds :: Prelude.Maybe [Prelude.Text],
    -- | The AWS KMS key identifier to use when restoring an encrypted DB cluster
    -- from a DB snapshot or DB cluster snapshot.
    --
    -- The AWS KMS key identifier is the key ARN, key ID, alias ARN, or alias
    -- name for the AWS KMS customer master key (CMK). To use a CMK in a
    -- different AWS account, specify the key ARN or alias ARN.
    --
    -- When you don\'t specify a value for the @KmsKeyId@ parameter, then the
    -- following occurs:
    --
    -- -   If the DB snapshot or DB cluster snapshot in @SnapshotIdentifier@ is
    --     encrypted, then the restored DB cluster is encrypted using the AWS
    --     KMS CMK that was used to encrypt the DB snapshot or DB cluster
    --     snapshot.
    --
    -- -   If the DB snapshot or DB cluster snapshot in @SnapshotIdentifier@
    --     isn\'t encrypted, then the restored DB cluster isn\'t encrypted.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | The version of the database engine to use for the new DB cluster.
    --
    -- To list all of the available engine versions for @aurora@ (for MySQL
    -- 5.6-compatible Aurora), use the following command:
    --
    -- @aws rds describe-db-engine-versions --engine aurora --query \"DBEngineVersions[].EngineVersion\"@
    --
    -- To list all of the available engine versions for @aurora-mysql@ (for
    -- MySQL 5.7-compatible Aurora), use the following command:
    --
    -- @aws rds describe-db-engine-versions --engine aurora-mysql --query \"DBEngineVersions[].EngineVersion\"@
    --
    -- To list all of the available engine versions for @aurora-postgresql@,
    -- use the following command:
    --
    -- @aws rds describe-db-engine-versions --engine aurora-postgresql --query \"DBEngineVersions[].EngineVersion\"@
    --
    -- If you aren\'t using the default engine version, then you must specify
    -- the engine version.
    --
    -- __Aurora MySQL__
    --
    -- Example: @5.6.10a@, @5.6.mysql_aurora.1.19.2@, @5.7.12@,
    -- @5.7.mysql_aurora.2.04.5@
    --
    -- __Aurora PostgreSQL__
    --
    -- Example: @9.6.3@, @10.7@
    engineVersion :: Prelude.Maybe Prelude.Text,
    -- | The tags to be assigned to the restored DB cluster.
    tags :: Prelude.Maybe [Tag],
    -- | The port number on which the new DB cluster accepts connections.
    --
    -- Constraints: This value must be @1150-65535@
    --
    -- Default: The same port as the original DB cluster.
    port :: Prelude.Maybe Prelude.Int,
    -- | Specify the name of the IAM role to be used when making API calls to the
    -- Directory Service.
    domainIAMRoleName :: Prelude.Maybe Prelude.Text,
    -- | A value that indicates whether to copy all tags from the restored DB
    -- cluster to snapshots of the restored DB cluster. The default is not to
    -- copy them.
    copyTagsToSnapshot :: Prelude.Maybe Prelude.Bool,
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
    backtrackWindow :: Prelude.Maybe Prelude.Integer,
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
    dbClusterParameterGroupName :: Prelude.Maybe Prelude.Text,
    -- | The database name for the restored DB cluster.
    databaseName :: Prelude.Maybe Prelude.Text,
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
    snapshotIdentifier :: Prelude.Text,
    -- | The database engine to use for the new DB cluster.
    --
    -- Default: The same as source
    --
    -- Constraint: Must be compatible with the engine of the source
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
-- 'deletionProtection', 'restoreDBClusterFromSnapshot_deletionProtection' - A value that indicates whether the DB cluster has deletion protection
-- enabled. The database can\'t be deleted when deletion protection is
-- enabled. By default, deletion protection is disabled.
--
-- 'availabilityZones', 'restoreDBClusterFromSnapshot_availabilityZones' - Provides the list of Availability Zones (AZs) where instances in the
-- restored DB cluster can be created.
--
-- 'enableIAMDatabaseAuthentication', 'restoreDBClusterFromSnapshot_enableIAMDatabaseAuthentication' - A value that indicates whether to enable mapping of AWS Identity and
-- Access Management (IAM) accounts to database accounts. By default,
-- mapping is disabled.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/UsingWithRDS.IAMDBAuth.html IAM Database Authentication>
-- in the /Amazon Aurora User Guide./
--
-- 'enableCloudwatchLogsExports', 'restoreDBClusterFromSnapshot_enableCloudwatchLogsExports' - The list of logs that the restored DB cluster is to export to Amazon
-- CloudWatch Logs. The values in the list depend on the DB engine being
-- used. For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/USER_LogAccess.html#USER_LogAccess.Procedural.UploadtoCloudWatch Publishing Database Logs to Amazon CloudWatch Logs>
-- in the /Amazon Aurora User Guide/.
--
-- 'optionGroupName', 'restoreDBClusterFromSnapshot_optionGroupName' - The name of the option group to use for the restored DB cluster.
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
-- 'engineMode', 'restoreDBClusterFromSnapshot_engineMode' - The DB engine mode of the DB cluster, either @provisioned@,
-- @serverless@, @parallelquery@, @global@, or @multimaster@.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/APIReference/API_CreateDBCluster.html CreateDBCluster>.
--
-- 'scalingConfiguration', 'restoreDBClusterFromSnapshot_scalingConfiguration' - For DB clusters in @serverless@ DB engine mode, the scaling properties
-- of the DB cluster.
--
-- 'dbSubnetGroupName', 'restoreDBClusterFromSnapshot_dbSubnetGroupName' - The name of the DB subnet group to use for the new DB cluster.
--
-- Constraints: If supplied, must match the name of an existing DB subnet
-- group.
--
-- Example: @mySubnetgroup@
--
-- 'vpcSecurityGroupIds', 'restoreDBClusterFromSnapshot_vpcSecurityGroupIds' - A list of VPC security groups that the new DB cluster will belong to.
--
-- 'kmsKeyId', 'restoreDBClusterFromSnapshot_kmsKeyId' - The AWS KMS key identifier to use when restoring an encrypted DB cluster
-- from a DB snapshot or DB cluster snapshot.
--
-- The AWS KMS key identifier is the key ARN, key ID, alias ARN, or alias
-- name for the AWS KMS customer master key (CMK). To use a CMK in a
-- different AWS account, specify the key ARN or alias ARN.
--
-- When you don\'t specify a value for the @KmsKeyId@ parameter, then the
-- following occurs:
--
-- -   If the DB snapshot or DB cluster snapshot in @SnapshotIdentifier@ is
--     encrypted, then the restored DB cluster is encrypted using the AWS
--     KMS CMK that was used to encrypt the DB snapshot or DB cluster
--     snapshot.
--
-- -   If the DB snapshot or DB cluster snapshot in @SnapshotIdentifier@
--     isn\'t encrypted, then the restored DB cluster isn\'t encrypted.
--
-- 'engineVersion', 'restoreDBClusterFromSnapshot_engineVersion' - The version of the database engine to use for the new DB cluster.
--
-- To list all of the available engine versions for @aurora@ (for MySQL
-- 5.6-compatible Aurora), use the following command:
--
-- @aws rds describe-db-engine-versions --engine aurora --query \"DBEngineVersions[].EngineVersion\"@
--
-- To list all of the available engine versions for @aurora-mysql@ (for
-- MySQL 5.7-compatible Aurora), use the following command:
--
-- @aws rds describe-db-engine-versions --engine aurora-mysql --query \"DBEngineVersions[].EngineVersion\"@
--
-- To list all of the available engine versions for @aurora-postgresql@,
-- use the following command:
--
-- @aws rds describe-db-engine-versions --engine aurora-postgresql --query \"DBEngineVersions[].EngineVersion\"@
--
-- If you aren\'t using the default engine version, then you must specify
-- the engine version.
--
-- __Aurora MySQL__
--
-- Example: @5.6.10a@, @5.6.mysql_aurora.1.19.2@, @5.7.12@,
-- @5.7.mysql_aurora.2.04.5@
--
-- __Aurora PostgreSQL__
--
-- Example: @9.6.3@, @10.7@
--
-- 'tags', 'restoreDBClusterFromSnapshot_tags' - The tags to be assigned to the restored DB cluster.
--
-- 'port', 'restoreDBClusterFromSnapshot_port' - The port number on which the new DB cluster accepts connections.
--
-- Constraints: This value must be @1150-65535@
--
-- Default: The same port as the original DB cluster.
--
-- 'domainIAMRoleName', 'restoreDBClusterFromSnapshot_domainIAMRoleName' - Specify the name of the IAM role to be used when making API calls to the
-- Directory Service.
--
-- 'copyTagsToSnapshot', 'restoreDBClusterFromSnapshot_copyTagsToSnapshot' - A value that indicates whether to copy all tags from the restored DB
-- cluster to snapshots of the restored DB cluster. The default is not to
-- copy them.
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
-- 'databaseName', 'restoreDBClusterFromSnapshot_databaseName' - The database name for the restored DB cluster.
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
-- 'engine', 'restoreDBClusterFromSnapshot_engine' - The database engine to use for the new DB cluster.
--
-- Default: The same as source
--
-- Constraint: Must be compatible with the engine of the source
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
      { deletionProtection =
          Prelude.Nothing,
        availabilityZones = Prelude.Nothing,
        enableIAMDatabaseAuthentication =
          Prelude.Nothing,
        enableCloudwatchLogsExports = Prelude.Nothing,
        optionGroupName = Prelude.Nothing,
        domain = Prelude.Nothing,
        engineMode = Prelude.Nothing,
        scalingConfiguration = Prelude.Nothing,
        dbSubnetGroupName = Prelude.Nothing,
        vpcSecurityGroupIds = Prelude.Nothing,
        kmsKeyId = Prelude.Nothing,
        engineVersion = Prelude.Nothing,
        tags = Prelude.Nothing,
        port = Prelude.Nothing,
        domainIAMRoleName = Prelude.Nothing,
        copyTagsToSnapshot = Prelude.Nothing,
        backtrackWindow = Prelude.Nothing,
        dbClusterParameterGroupName = Prelude.Nothing,
        databaseName = Prelude.Nothing,
        dbClusterIdentifier = pDBClusterIdentifier_,
        snapshotIdentifier = pSnapshotIdentifier_,
        engine = pEngine_
      }

-- | A value that indicates whether the DB cluster has deletion protection
-- enabled. The database can\'t be deleted when deletion protection is
-- enabled. By default, deletion protection is disabled.
restoreDBClusterFromSnapshot_deletionProtection :: Lens.Lens' RestoreDBClusterFromSnapshot (Prelude.Maybe Prelude.Bool)
restoreDBClusterFromSnapshot_deletionProtection = Lens.lens (\RestoreDBClusterFromSnapshot' {deletionProtection} -> deletionProtection) (\s@RestoreDBClusterFromSnapshot' {} a -> s {deletionProtection = a} :: RestoreDBClusterFromSnapshot)

-- | Provides the list of Availability Zones (AZs) where instances in the
-- restored DB cluster can be created.
restoreDBClusterFromSnapshot_availabilityZones :: Lens.Lens' RestoreDBClusterFromSnapshot (Prelude.Maybe [Prelude.Text])
restoreDBClusterFromSnapshot_availabilityZones = Lens.lens (\RestoreDBClusterFromSnapshot' {availabilityZones} -> availabilityZones) (\s@RestoreDBClusterFromSnapshot' {} a -> s {availabilityZones = a} :: RestoreDBClusterFromSnapshot) Prelude.. Lens.mapping Lens._Coerce

-- | A value that indicates whether to enable mapping of AWS Identity and
-- Access Management (IAM) accounts to database accounts. By default,
-- mapping is disabled.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/UsingWithRDS.IAMDBAuth.html IAM Database Authentication>
-- in the /Amazon Aurora User Guide./
restoreDBClusterFromSnapshot_enableIAMDatabaseAuthentication :: Lens.Lens' RestoreDBClusterFromSnapshot (Prelude.Maybe Prelude.Bool)
restoreDBClusterFromSnapshot_enableIAMDatabaseAuthentication = Lens.lens (\RestoreDBClusterFromSnapshot' {enableIAMDatabaseAuthentication} -> enableIAMDatabaseAuthentication) (\s@RestoreDBClusterFromSnapshot' {} a -> s {enableIAMDatabaseAuthentication = a} :: RestoreDBClusterFromSnapshot)

-- | The list of logs that the restored DB cluster is to export to Amazon
-- CloudWatch Logs. The values in the list depend on the DB engine being
-- used. For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/USER_LogAccess.html#USER_LogAccess.Procedural.UploadtoCloudWatch Publishing Database Logs to Amazon CloudWatch Logs>
-- in the /Amazon Aurora User Guide/.
restoreDBClusterFromSnapshot_enableCloudwatchLogsExports :: Lens.Lens' RestoreDBClusterFromSnapshot (Prelude.Maybe [Prelude.Text])
restoreDBClusterFromSnapshot_enableCloudwatchLogsExports = Lens.lens (\RestoreDBClusterFromSnapshot' {enableCloudwatchLogsExports} -> enableCloudwatchLogsExports) (\s@RestoreDBClusterFromSnapshot' {} a -> s {enableCloudwatchLogsExports = a} :: RestoreDBClusterFromSnapshot) Prelude.. Lens.mapping Lens._Coerce

-- | The name of the option group to use for the restored DB cluster.
restoreDBClusterFromSnapshot_optionGroupName :: Lens.Lens' RestoreDBClusterFromSnapshot (Prelude.Maybe Prelude.Text)
restoreDBClusterFromSnapshot_optionGroupName = Lens.lens (\RestoreDBClusterFromSnapshot' {optionGroupName} -> optionGroupName) (\s@RestoreDBClusterFromSnapshot' {} a -> s {optionGroupName = a} :: RestoreDBClusterFromSnapshot)

-- | Specify the Active Directory directory ID to restore the DB cluster in.
-- The domain must be created prior to this operation. Currently, only
-- MySQL, Microsoft SQL Server, Oracle, and PostgreSQL DB instances can be
-- created in an Active Directory Domain.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/kerberos-authentication.html Kerberos Authentication>
-- in the /Amazon RDS User Guide/.
restoreDBClusterFromSnapshot_domain :: Lens.Lens' RestoreDBClusterFromSnapshot (Prelude.Maybe Prelude.Text)
restoreDBClusterFromSnapshot_domain = Lens.lens (\RestoreDBClusterFromSnapshot' {domain} -> domain) (\s@RestoreDBClusterFromSnapshot' {} a -> s {domain = a} :: RestoreDBClusterFromSnapshot)

-- | The DB engine mode of the DB cluster, either @provisioned@,
-- @serverless@, @parallelquery@, @global@, or @multimaster@.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/APIReference/API_CreateDBCluster.html CreateDBCluster>.
restoreDBClusterFromSnapshot_engineMode :: Lens.Lens' RestoreDBClusterFromSnapshot (Prelude.Maybe Prelude.Text)
restoreDBClusterFromSnapshot_engineMode = Lens.lens (\RestoreDBClusterFromSnapshot' {engineMode} -> engineMode) (\s@RestoreDBClusterFromSnapshot' {} a -> s {engineMode = a} :: RestoreDBClusterFromSnapshot)

-- | For DB clusters in @serverless@ DB engine mode, the scaling properties
-- of the DB cluster.
restoreDBClusterFromSnapshot_scalingConfiguration :: Lens.Lens' RestoreDBClusterFromSnapshot (Prelude.Maybe ScalingConfiguration)
restoreDBClusterFromSnapshot_scalingConfiguration = Lens.lens (\RestoreDBClusterFromSnapshot' {scalingConfiguration} -> scalingConfiguration) (\s@RestoreDBClusterFromSnapshot' {} a -> s {scalingConfiguration = a} :: RestoreDBClusterFromSnapshot)

-- | The name of the DB subnet group to use for the new DB cluster.
--
-- Constraints: If supplied, must match the name of an existing DB subnet
-- group.
--
-- Example: @mySubnetgroup@
restoreDBClusterFromSnapshot_dbSubnetGroupName :: Lens.Lens' RestoreDBClusterFromSnapshot (Prelude.Maybe Prelude.Text)
restoreDBClusterFromSnapshot_dbSubnetGroupName = Lens.lens (\RestoreDBClusterFromSnapshot' {dbSubnetGroupName} -> dbSubnetGroupName) (\s@RestoreDBClusterFromSnapshot' {} a -> s {dbSubnetGroupName = a} :: RestoreDBClusterFromSnapshot)

-- | A list of VPC security groups that the new DB cluster will belong to.
restoreDBClusterFromSnapshot_vpcSecurityGroupIds :: Lens.Lens' RestoreDBClusterFromSnapshot (Prelude.Maybe [Prelude.Text])
restoreDBClusterFromSnapshot_vpcSecurityGroupIds = Lens.lens (\RestoreDBClusterFromSnapshot' {vpcSecurityGroupIds} -> vpcSecurityGroupIds) (\s@RestoreDBClusterFromSnapshot' {} a -> s {vpcSecurityGroupIds = a} :: RestoreDBClusterFromSnapshot) Prelude.. Lens.mapping Lens._Coerce

-- | The AWS KMS key identifier to use when restoring an encrypted DB cluster
-- from a DB snapshot or DB cluster snapshot.
--
-- The AWS KMS key identifier is the key ARN, key ID, alias ARN, or alias
-- name for the AWS KMS customer master key (CMK). To use a CMK in a
-- different AWS account, specify the key ARN or alias ARN.
--
-- When you don\'t specify a value for the @KmsKeyId@ parameter, then the
-- following occurs:
--
-- -   If the DB snapshot or DB cluster snapshot in @SnapshotIdentifier@ is
--     encrypted, then the restored DB cluster is encrypted using the AWS
--     KMS CMK that was used to encrypt the DB snapshot or DB cluster
--     snapshot.
--
-- -   If the DB snapshot or DB cluster snapshot in @SnapshotIdentifier@
--     isn\'t encrypted, then the restored DB cluster isn\'t encrypted.
restoreDBClusterFromSnapshot_kmsKeyId :: Lens.Lens' RestoreDBClusterFromSnapshot (Prelude.Maybe Prelude.Text)
restoreDBClusterFromSnapshot_kmsKeyId = Lens.lens (\RestoreDBClusterFromSnapshot' {kmsKeyId} -> kmsKeyId) (\s@RestoreDBClusterFromSnapshot' {} a -> s {kmsKeyId = a} :: RestoreDBClusterFromSnapshot)

-- | The version of the database engine to use for the new DB cluster.
--
-- To list all of the available engine versions for @aurora@ (for MySQL
-- 5.6-compatible Aurora), use the following command:
--
-- @aws rds describe-db-engine-versions --engine aurora --query \"DBEngineVersions[].EngineVersion\"@
--
-- To list all of the available engine versions for @aurora-mysql@ (for
-- MySQL 5.7-compatible Aurora), use the following command:
--
-- @aws rds describe-db-engine-versions --engine aurora-mysql --query \"DBEngineVersions[].EngineVersion\"@
--
-- To list all of the available engine versions for @aurora-postgresql@,
-- use the following command:
--
-- @aws rds describe-db-engine-versions --engine aurora-postgresql --query \"DBEngineVersions[].EngineVersion\"@
--
-- If you aren\'t using the default engine version, then you must specify
-- the engine version.
--
-- __Aurora MySQL__
--
-- Example: @5.6.10a@, @5.6.mysql_aurora.1.19.2@, @5.7.12@,
-- @5.7.mysql_aurora.2.04.5@
--
-- __Aurora PostgreSQL__
--
-- Example: @9.6.3@, @10.7@
restoreDBClusterFromSnapshot_engineVersion :: Lens.Lens' RestoreDBClusterFromSnapshot (Prelude.Maybe Prelude.Text)
restoreDBClusterFromSnapshot_engineVersion = Lens.lens (\RestoreDBClusterFromSnapshot' {engineVersion} -> engineVersion) (\s@RestoreDBClusterFromSnapshot' {} a -> s {engineVersion = a} :: RestoreDBClusterFromSnapshot)

-- | The tags to be assigned to the restored DB cluster.
restoreDBClusterFromSnapshot_tags :: Lens.Lens' RestoreDBClusterFromSnapshot (Prelude.Maybe [Tag])
restoreDBClusterFromSnapshot_tags = Lens.lens (\RestoreDBClusterFromSnapshot' {tags} -> tags) (\s@RestoreDBClusterFromSnapshot' {} a -> s {tags = a} :: RestoreDBClusterFromSnapshot) Prelude.. Lens.mapping Lens._Coerce

-- | The port number on which the new DB cluster accepts connections.
--
-- Constraints: This value must be @1150-65535@
--
-- Default: The same port as the original DB cluster.
restoreDBClusterFromSnapshot_port :: Lens.Lens' RestoreDBClusterFromSnapshot (Prelude.Maybe Prelude.Int)
restoreDBClusterFromSnapshot_port = Lens.lens (\RestoreDBClusterFromSnapshot' {port} -> port) (\s@RestoreDBClusterFromSnapshot' {} a -> s {port = a} :: RestoreDBClusterFromSnapshot)

-- | Specify the name of the IAM role to be used when making API calls to the
-- Directory Service.
restoreDBClusterFromSnapshot_domainIAMRoleName :: Lens.Lens' RestoreDBClusterFromSnapshot (Prelude.Maybe Prelude.Text)
restoreDBClusterFromSnapshot_domainIAMRoleName = Lens.lens (\RestoreDBClusterFromSnapshot' {domainIAMRoleName} -> domainIAMRoleName) (\s@RestoreDBClusterFromSnapshot' {} a -> s {domainIAMRoleName = a} :: RestoreDBClusterFromSnapshot)

-- | A value that indicates whether to copy all tags from the restored DB
-- cluster to snapshots of the restored DB cluster. The default is not to
-- copy them.
restoreDBClusterFromSnapshot_copyTagsToSnapshot :: Lens.Lens' RestoreDBClusterFromSnapshot (Prelude.Maybe Prelude.Bool)
restoreDBClusterFromSnapshot_copyTagsToSnapshot = Lens.lens (\RestoreDBClusterFromSnapshot' {copyTagsToSnapshot} -> copyTagsToSnapshot) (\s@RestoreDBClusterFromSnapshot' {} a -> s {copyTagsToSnapshot = a} :: RestoreDBClusterFromSnapshot)

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
restoreDBClusterFromSnapshot_backtrackWindow :: Lens.Lens' RestoreDBClusterFromSnapshot (Prelude.Maybe Prelude.Integer)
restoreDBClusterFromSnapshot_backtrackWindow = Lens.lens (\RestoreDBClusterFromSnapshot' {backtrackWindow} -> backtrackWindow) (\s@RestoreDBClusterFromSnapshot' {} a -> s {backtrackWindow = a} :: RestoreDBClusterFromSnapshot)

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
restoreDBClusterFromSnapshot_dbClusterParameterGroupName :: Lens.Lens' RestoreDBClusterFromSnapshot (Prelude.Maybe Prelude.Text)
restoreDBClusterFromSnapshot_dbClusterParameterGroupName = Lens.lens (\RestoreDBClusterFromSnapshot' {dbClusterParameterGroupName} -> dbClusterParameterGroupName) (\s@RestoreDBClusterFromSnapshot' {} a -> s {dbClusterParameterGroupName = a} :: RestoreDBClusterFromSnapshot)

-- | The database name for the restored DB cluster.
restoreDBClusterFromSnapshot_databaseName :: Lens.Lens' RestoreDBClusterFromSnapshot (Prelude.Maybe Prelude.Text)
restoreDBClusterFromSnapshot_databaseName = Lens.lens (\RestoreDBClusterFromSnapshot' {databaseName} -> databaseName) (\s@RestoreDBClusterFromSnapshot' {} a -> s {databaseName = a} :: RestoreDBClusterFromSnapshot)

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
restoreDBClusterFromSnapshot_snapshotIdentifier :: Lens.Lens' RestoreDBClusterFromSnapshot Prelude.Text
restoreDBClusterFromSnapshot_snapshotIdentifier = Lens.lens (\RestoreDBClusterFromSnapshot' {snapshotIdentifier} -> snapshotIdentifier) (\s@RestoreDBClusterFromSnapshot' {} a -> s {snapshotIdentifier = a} :: RestoreDBClusterFromSnapshot)

-- | The database engine to use for the new DB cluster.
--
-- Default: The same as source
--
-- Constraint: Must be compatible with the engine of the source
restoreDBClusterFromSnapshot_engine :: Lens.Lens' RestoreDBClusterFromSnapshot Prelude.Text
restoreDBClusterFromSnapshot_engine = Lens.lens (\RestoreDBClusterFromSnapshot' {engine} -> engine) (\s@RestoreDBClusterFromSnapshot' {} a -> s {engine = a} :: RestoreDBClusterFromSnapshot)

instance Core.AWSRequest RestoreDBClusterFromSnapshot where
  type
    AWSResponse RestoreDBClusterFromSnapshot =
      RestoreDBClusterFromSnapshotResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "RestoreDBClusterFromSnapshotResult"
      ( \s h x ->
          RestoreDBClusterFromSnapshotResponse'
            Prelude.<$> (x Core..@? "DBCluster")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    RestoreDBClusterFromSnapshot

instance Prelude.NFData RestoreDBClusterFromSnapshot

instance Core.ToHeaders RestoreDBClusterFromSnapshot where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath RestoreDBClusterFromSnapshot where
  toPath = Prelude.const "/"

instance Core.ToQuery RestoreDBClusterFromSnapshot where
  toQuery RestoreDBClusterFromSnapshot' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ( "RestoreDBClusterFromSnapshot" ::
                      Prelude.ByteString
                  ),
        "Version"
          Core.=: ("2014-10-31" :: Prelude.ByteString),
        "DeletionProtection" Core.=: deletionProtection,
        "AvailabilityZones"
          Core.=: Core.toQuery
            ( Core.toQueryList "AvailabilityZone"
                Prelude.<$> availabilityZones
            ),
        "EnableIAMDatabaseAuthentication"
          Core.=: enableIAMDatabaseAuthentication,
        "EnableCloudwatchLogsExports"
          Core.=: Core.toQuery
            ( Core.toQueryList "member"
                Prelude.<$> enableCloudwatchLogsExports
            ),
        "OptionGroupName" Core.=: optionGroupName,
        "Domain" Core.=: domain,
        "EngineMode" Core.=: engineMode,
        "ScalingConfiguration" Core.=: scalingConfiguration,
        "DBSubnetGroupName" Core.=: dbSubnetGroupName,
        "VpcSecurityGroupIds"
          Core.=: Core.toQuery
            ( Core.toQueryList "VpcSecurityGroupId"
                Prelude.<$> vpcSecurityGroupIds
            ),
        "KmsKeyId" Core.=: kmsKeyId,
        "EngineVersion" Core.=: engineVersion,
        "Tags"
          Core.=: Core.toQuery
            (Core.toQueryList "Tag" Prelude.<$> tags),
        "Port" Core.=: port,
        "DomainIAMRoleName" Core.=: domainIAMRoleName,
        "CopyTagsToSnapshot" Core.=: copyTagsToSnapshot,
        "BacktrackWindow" Core.=: backtrackWindow,
        "DBClusterParameterGroupName"
          Core.=: dbClusterParameterGroupName,
        "DatabaseName" Core.=: databaseName,
        "DBClusterIdentifier" Core.=: dbClusterIdentifier,
        "SnapshotIdentifier" Core.=: snapshotIdentifier,
        "Engine" Core.=: engine
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
