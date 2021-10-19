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
-- Module      : Network.AWS.RDS.RestoreDBClusterFromS
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an Amazon Aurora DB cluster from MySQL data stored in an Amazon
-- S3 bucket. Amazon RDS must be authorized to access the Amazon S3 bucket
-- and the data must be created using the Percona XtraBackup utility as
-- described in
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/AuroraMySQL.Migrating.ExtMySQL.html#AuroraMySQL.Migrating.ExtMySQL.S3 Migrating Data from MySQL by Using an Amazon S3 Bucket>
-- in the /Amazon Aurora User Guide/.
--
-- This action only restores the DB cluster, not the DB instances for that
-- DB cluster. You must invoke the @CreateDBInstance@ action to create DB
-- instances for the restored DB cluster, specifying the identifier of the
-- restored DB cluster in @DBClusterIdentifier@. You can create DB
-- instances only after the @RestoreDBClusterFromS3@ action has completed
-- and the DB cluster is available.
--
-- For more information on Amazon Aurora, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/CHAP_AuroraOverview.html What Is Amazon Aurora?>
-- in the /Amazon Aurora User Guide./
--
-- This action only applies to Aurora DB clusters. The source DB engine
-- must be MySQL.
module Network.AWS.RDS.RestoreDBClusterFromS
  ( -- * Creating a Request
    RestoreDBClusterFromS (..),
    newRestoreDBClusterFromS,

    -- * Request Lenses
    restoreDBClusterFromS_engineVersion,
    restoreDBClusterFromS_deletionProtection,
    restoreDBClusterFromS_storageEncrypted,
    restoreDBClusterFromS_dbSubnetGroupName,
    restoreDBClusterFromS_domain,
    restoreDBClusterFromS_backtrackWindow,
    restoreDBClusterFromS_preferredMaintenanceWindow,
    restoreDBClusterFromS_availabilityZones,
    restoreDBClusterFromS_characterSetName,
    restoreDBClusterFromS_kmsKeyId,
    restoreDBClusterFromS_preferredBackupWindow,
    restoreDBClusterFromS_backupRetentionPeriod,
    restoreDBClusterFromS_vpcSecurityGroupIds,
    restoreDBClusterFromS_databaseName,
    restoreDBClusterFromS_dbClusterParameterGroupName,
    restoreDBClusterFromS_s3Prefix,
    restoreDBClusterFromS_optionGroupName,
    restoreDBClusterFromS_copyTagsToSnapshot,
    restoreDBClusterFromS_domainIAMRoleName,
    restoreDBClusterFromS_tags,
    restoreDBClusterFromS_port,
    restoreDBClusterFromS_enableIAMDatabaseAuthentication,
    restoreDBClusterFromS_enableCloudwatchLogsExports,
    restoreDBClusterFromS_dbClusterIdentifier,
    restoreDBClusterFromS_engine,
    restoreDBClusterFromS_masterUsername,
    restoreDBClusterFromS_masterUserPassword,
    restoreDBClusterFromS_sourceEngine,
    restoreDBClusterFromS_sourceEngineVersion,
    restoreDBClusterFromS_s3BucketName,
    restoreDBClusterFromS_s3IngestionRoleArn,

    -- * Destructuring the Response
    RestoreDBClusterFromSResponse (..),
    newRestoreDBClusterFromSResponse,

    -- * Response Lenses
    restoreDBClusterFromSResponse_dbCluster,
    restoreDBClusterFromSResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.RDS.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newRestoreDBClusterFromS' smart constructor.
data RestoreDBClusterFromS = RestoreDBClusterFromS'
  { -- | The version number of the database engine to use.
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
    -- __Aurora MySQL__
    --
    -- Example: @5.6.10a@, @5.6.mysql_aurora.1.19.2@, @5.7.12@,
    -- @5.7.mysql_aurora.2.04.5@
    --
    -- __Aurora PostgreSQL__
    --
    -- Example: @9.6.3@, @10.7@
    engineVersion :: Prelude.Maybe Prelude.Text,
    -- | A value that indicates whether the DB cluster has deletion protection
    -- enabled. The database can\'t be deleted when deletion protection is
    -- enabled. By default, deletion protection is disabled.
    deletionProtection :: Prelude.Maybe Prelude.Bool,
    -- | A value that indicates whether the restored DB cluster is encrypted.
    storageEncrypted :: Prelude.Maybe Prelude.Bool,
    -- | A DB subnet group to associate with the restored DB cluster.
    --
    -- Constraints: If supplied, must match the name of an existing
    -- DBSubnetGroup.
    --
    -- Example: @mySubnetgroup@
    dbSubnetGroupName :: Prelude.Maybe Prelude.Text,
    -- | Specify the Active Directory directory ID to restore the DB cluster in.
    -- The domain must be created prior to this operation.
    --
    -- For Amazon Aurora DB clusters, Amazon RDS can use Kerberos
    -- Authentication to authenticate users that connect to the DB cluster. For
    -- more information, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/kerberos-authentication.html Kerberos Authentication>
    -- in the /Amazon Aurora User Guide/.
    domain :: Prelude.Maybe Prelude.Text,
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
    -- | The weekly time range during which system maintenance can occur, in
    -- Universal Coordinated Time (UTC).
    --
    -- Format: @ddd:hh24:mi-ddd:hh24:mi@
    --
    -- The default is a 30-minute window selected at random from an 8-hour
    -- block of time for each Amazon Web Services Region, occurring on a random
    -- day of the week. To see the time blocks available, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/USER_UpgradeDBInstance.Maintenance.html#AdjustingTheMaintenanceWindow.Aurora Adjusting the Preferred Maintenance Window>
    -- in the /Amazon Aurora User Guide./
    --
    -- Valid Days: Mon, Tue, Wed, Thu, Fri, Sat, Sun.
    --
    -- Constraints: Minimum 30-minute window.
    preferredMaintenanceWindow :: Prelude.Maybe Prelude.Text,
    -- | A list of Availability Zones (AZs) where instances in the restored DB
    -- cluster can be created.
    availabilityZones :: Prelude.Maybe [Prelude.Text],
    -- | A value that indicates that the restored DB cluster should be associated
    -- with the specified CharacterSet.
    characterSetName :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services KMS key identifier for an encrypted DB cluster.
    --
    -- The Amazon Web Services KMS key identifier is the key ARN, key ID, alias
    -- ARN, or alias name for the Amazon Web Services KMS customer master key
    -- (CMK). To use a CMK in a different Amazon Web Services account, specify
    -- the key ARN or alias ARN.
    --
    -- If the StorageEncrypted parameter is enabled, and you do not specify a
    -- value for the @KmsKeyId@ parameter, then Amazon RDS will use your
    -- default CMK. There is a default CMK for your Amazon Web Services
    -- account. Your Amazon Web Services account has a different default CMK
    -- for each Amazon Web Services Region.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | The daily time range during which automated backups are created if
    -- automated backups are enabled using the @BackupRetentionPeriod@
    -- parameter.
    --
    -- The default is a 30-minute window selected at random from an 8-hour
    -- block of time for each Amazon Web Services Region. To view the time
    -- blocks available, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/Aurora.Managing.Backups.html#Aurora.Managing.Backups.BackupWindow Backup window>
    -- in the /Amazon Aurora User Guide./
    --
    -- Constraints:
    --
    -- -   Must be in the format @hh24:mi-hh24:mi@.
    --
    -- -   Must be in Universal Coordinated Time (UTC).
    --
    -- -   Must not conflict with the preferred maintenance window.
    --
    -- -   Must be at least 30 minutes.
    preferredBackupWindow :: Prelude.Maybe Prelude.Text,
    -- | The number of days for which automated backups of the restored DB
    -- cluster are retained. You must specify a minimum value of 1.
    --
    -- Default: 1
    --
    -- Constraints:
    --
    -- -   Must be a value from 1 to 35
    backupRetentionPeriod :: Prelude.Maybe Prelude.Int,
    -- | A list of EC2 VPC security groups to associate with the restored DB
    -- cluster.
    vpcSecurityGroupIds :: Prelude.Maybe [Prelude.Text],
    -- | The database name for the restored DB cluster.
    databaseName :: Prelude.Maybe Prelude.Text,
    -- | The name of the DB cluster parameter group to associate with the
    -- restored DB cluster. If this argument is omitted, @default.aurora5.6@ is
    -- used.
    --
    -- Constraints:
    --
    -- -   If supplied, must match the name of an existing
    --     DBClusterParameterGroup.
    dbClusterParameterGroupName :: Prelude.Maybe Prelude.Text,
    -- | The prefix for all of the file names that contain the data used to
    -- create the Amazon Aurora DB cluster. If you do not specify a
    -- __SourceS3Prefix__ value, then the Amazon Aurora DB cluster is created
    -- by using all of the files in the Amazon S3 bucket.
    s3Prefix :: Prelude.Maybe Prelude.Text,
    -- | A value that indicates that the restored DB cluster should be associated
    -- with the specified option group.
    --
    -- Permanent options can\'t be removed from an option group. An option
    -- group can\'t be removed from a DB cluster once it is associated with a
    -- DB cluster.
    optionGroupName :: Prelude.Maybe Prelude.Text,
    -- | A value that indicates whether to copy all tags from the restored DB
    -- cluster to snapshots of the restored DB cluster. The default is not to
    -- copy them.
    copyTagsToSnapshot :: Prelude.Maybe Prelude.Bool,
    -- | Specify the name of the IAM role to be used when making API calls to the
    -- Directory Service.
    domainIAMRoleName :: Prelude.Maybe Prelude.Text,
    tags :: Prelude.Maybe [Tag],
    -- | The port number on which the instances in the restored DB cluster accept
    -- connections.
    --
    -- Default: @3306@
    port :: Prelude.Maybe Prelude.Int,
    -- | A value that indicates whether to enable mapping of Amazon Web Services
    -- Identity and Access Management (IAM) accounts to database accounts. By
    -- default, mapping is disabled.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/UsingWithRDS.IAMDBAuth.html IAM Database Authentication>
    -- in the /Amazon Aurora User Guide./
    enableIAMDatabaseAuthentication :: Prelude.Maybe Prelude.Bool,
    -- | The list of logs that the restored DB cluster is to export to CloudWatch
    -- Logs. The values in the list depend on the DB engine being used. For
    -- more information, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/USER_LogAccess.html#USER_LogAccess.Procedural.UploadtoCloudWatch Publishing Database Logs to Amazon CloudWatch Logs>
    -- in the /Amazon Aurora User Guide/.
    enableCloudwatchLogsExports :: Prelude.Maybe [Prelude.Text],
    -- | The name of the DB cluster to create from the source data in the Amazon
    -- S3 bucket. This parameter isn\'t case-sensitive.
    --
    -- Constraints:
    --
    -- -   Must contain from 1 to 63 letters, numbers, or hyphens.
    --
    -- -   First character must be a letter.
    --
    -- -   Can\'t end with a hyphen or contain two consecutive hyphens.
    --
    -- Example: @my-cluster1@
    dbClusterIdentifier :: Prelude.Text,
    -- | The name of the database engine to be used for this DB cluster.
    --
    -- Valid Values: @aurora@ (for MySQL 5.6-compatible Aurora), @aurora-mysql@
    -- (for MySQL 5.7-compatible Aurora), and @aurora-postgresql@
    engine :: Prelude.Text,
    -- | The name of the master user for the restored DB cluster.
    --
    -- Constraints:
    --
    -- -   Must be 1 to 16 letters or numbers.
    --
    -- -   First character must be a letter.
    --
    -- -   Can\'t be a reserved word for the chosen database engine.
    masterUsername :: Prelude.Text,
    -- | The password for the master database user. This password can contain any
    -- printable ASCII character except \"\/\", \"\"\", or \"\@\".
    --
    -- Constraints: Must contain from 8 to 41 characters.
    masterUserPassword :: Prelude.Text,
    -- | The identifier for the database engine that was backed up to create the
    -- files stored in the Amazon S3 bucket.
    --
    -- Valid values: @mysql@
    sourceEngine :: Prelude.Text,
    -- | The version of the database that the backup files were created from.
    --
    -- MySQL versions 5.5, 5.6, and 5.7 are supported.
    --
    -- Example: @5.6.40@, @5.7.28@
    sourceEngineVersion :: Prelude.Text,
    -- | The name of the Amazon S3 bucket that contains the data used to create
    -- the Amazon Aurora DB cluster.
    s3BucketName :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the Amazon Web Services Identity and
    -- Access Management (IAM) role that authorizes Amazon RDS to access the
    -- Amazon S3 bucket on your behalf.
    s3IngestionRoleArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RestoreDBClusterFromS' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'engineVersion', 'restoreDBClusterFromS_engineVersion' - The version number of the database engine to use.
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
-- __Aurora MySQL__
--
-- Example: @5.6.10a@, @5.6.mysql_aurora.1.19.2@, @5.7.12@,
-- @5.7.mysql_aurora.2.04.5@
--
-- __Aurora PostgreSQL__
--
-- Example: @9.6.3@, @10.7@
--
-- 'deletionProtection', 'restoreDBClusterFromS_deletionProtection' - A value that indicates whether the DB cluster has deletion protection
-- enabled. The database can\'t be deleted when deletion protection is
-- enabled. By default, deletion protection is disabled.
--
-- 'storageEncrypted', 'restoreDBClusterFromS_storageEncrypted' - A value that indicates whether the restored DB cluster is encrypted.
--
-- 'dbSubnetGroupName', 'restoreDBClusterFromS_dbSubnetGroupName' - A DB subnet group to associate with the restored DB cluster.
--
-- Constraints: If supplied, must match the name of an existing
-- DBSubnetGroup.
--
-- Example: @mySubnetgroup@
--
-- 'domain', 'restoreDBClusterFromS_domain' - Specify the Active Directory directory ID to restore the DB cluster in.
-- The domain must be created prior to this operation.
--
-- For Amazon Aurora DB clusters, Amazon RDS can use Kerberos
-- Authentication to authenticate users that connect to the DB cluster. For
-- more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/kerberos-authentication.html Kerberos Authentication>
-- in the /Amazon Aurora User Guide/.
--
-- 'backtrackWindow', 'restoreDBClusterFromS_backtrackWindow' - The target backtrack window, in seconds. To disable backtracking, set
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
-- 'preferredMaintenanceWindow', 'restoreDBClusterFromS_preferredMaintenanceWindow' - The weekly time range during which system maintenance can occur, in
-- Universal Coordinated Time (UTC).
--
-- Format: @ddd:hh24:mi-ddd:hh24:mi@
--
-- The default is a 30-minute window selected at random from an 8-hour
-- block of time for each Amazon Web Services Region, occurring on a random
-- day of the week. To see the time blocks available, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/USER_UpgradeDBInstance.Maintenance.html#AdjustingTheMaintenanceWindow.Aurora Adjusting the Preferred Maintenance Window>
-- in the /Amazon Aurora User Guide./
--
-- Valid Days: Mon, Tue, Wed, Thu, Fri, Sat, Sun.
--
-- Constraints: Minimum 30-minute window.
--
-- 'availabilityZones', 'restoreDBClusterFromS_availabilityZones' - A list of Availability Zones (AZs) where instances in the restored DB
-- cluster can be created.
--
-- 'characterSetName', 'restoreDBClusterFromS_characterSetName' - A value that indicates that the restored DB cluster should be associated
-- with the specified CharacterSet.
--
-- 'kmsKeyId', 'restoreDBClusterFromS_kmsKeyId' - The Amazon Web Services KMS key identifier for an encrypted DB cluster.
--
-- The Amazon Web Services KMS key identifier is the key ARN, key ID, alias
-- ARN, or alias name for the Amazon Web Services KMS customer master key
-- (CMK). To use a CMK in a different Amazon Web Services account, specify
-- the key ARN or alias ARN.
--
-- If the StorageEncrypted parameter is enabled, and you do not specify a
-- value for the @KmsKeyId@ parameter, then Amazon RDS will use your
-- default CMK. There is a default CMK for your Amazon Web Services
-- account. Your Amazon Web Services account has a different default CMK
-- for each Amazon Web Services Region.
--
-- 'preferredBackupWindow', 'restoreDBClusterFromS_preferredBackupWindow' - The daily time range during which automated backups are created if
-- automated backups are enabled using the @BackupRetentionPeriod@
-- parameter.
--
-- The default is a 30-minute window selected at random from an 8-hour
-- block of time for each Amazon Web Services Region. To view the time
-- blocks available, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/Aurora.Managing.Backups.html#Aurora.Managing.Backups.BackupWindow Backup window>
-- in the /Amazon Aurora User Guide./
--
-- Constraints:
--
-- -   Must be in the format @hh24:mi-hh24:mi@.
--
-- -   Must be in Universal Coordinated Time (UTC).
--
-- -   Must not conflict with the preferred maintenance window.
--
-- -   Must be at least 30 minutes.
--
-- 'backupRetentionPeriod', 'restoreDBClusterFromS_backupRetentionPeriod' - The number of days for which automated backups of the restored DB
-- cluster are retained. You must specify a minimum value of 1.
--
-- Default: 1
--
-- Constraints:
--
-- -   Must be a value from 1 to 35
--
-- 'vpcSecurityGroupIds', 'restoreDBClusterFromS_vpcSecurityGroupIds' - A list of EC2 VPC security groups to associate with the restored DB
-- cluster.
--
-- 'databaseName', 'restoreDBClusterFromS_databaseName' - The database name for the restored DB cluster.
--
-- 'dbClusterParameterGroupName', 'restoreDBClusterFromS_dbClusterParameterGroupName' - The name of the DB cluster parameter group to associate with the
-- restored DB cluster. If this argument is omitted, @default.aurora5.6@ is
-- used.
--
-- Constraints:
--
-- -   If supplied, must match the name of an existing
--     DBClusterParameterGroup.
--
-- 's3Prefix', 'restoreDBClusterFromS_s3Prefix' - The prefix for all of the file names that contain the data used to
-- create the Amazon Aurora DB cluster. If you do not specify a
-- __SourceS3Prefix__ value, then the Amazon Aurora DB cluster is created
-- by using all of the files in the Amazon S3 bucket.
--
-- 'optionGroupName', 'restoreDBClusterFromS_optionGroupName' - A value that indicates that the restored DB cluster should be associated
-- with the specified option group.
--
-- Permanent options can\'t be removed from an option group. An option
-- group can\'t be removed from a DB cluster once it is associated with a
-- DB cluster.
--
-- 'copyTagsToSnapshot', 'restoreDBClusterFromS_copyTagsToSnapshot' - A value that indicates whether to copy all tags from the restored DB
-- cluster to snapshots of the restored DB cluster. The default is not to
-- copy them.
--
-- 'domainIAMRoleName', 'restoreDBClusterFromS_domainIAMRoleName' - Specify the name of the IAM role to be used when making API calls to the
-- Directory Service.
--
-- 'tags', 'restoreDBClusterFromS_tags' - Undocumented member.
--
-- 'port', 'restoreDBClusterFromS_port' - The port number on which the instances in the restored DB cluster accept
-- connections.
--
-- Default: @3306@
--
-- 'enableIAMDatabaseAuthentication', 'restoreDBClusterFromS_enableIAMDatabaseAuthentication' - A value that indicates whether to enable mapping of Amazon Web Services
-- Identity and Access Management (IAM) accounts to database accounts. By
-- default, mapping is disabled.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/UsingWithRDS.IAMDBAuth.html IAM Database Authentication>
-- in the /Amazon Aurora User Guide./
--
-- 'enableCloudwatchLogsExports', 'restoreDBClusterFromS_enableCloudwatchLogsExports' - The list of logs that the restored DB cluster is to export to CloudWatch
-- Logs. The values in the list depend on the DB engine being used. For
-- more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/USER_LogAccess.html#USER_LogAccess.Procedural.UploadtoCloudWatch Publishing Database Logs to Amazon CloudWatch Logs>
-- in the /Amazon Aurora User Guide/.
--
-- 'dbClusterIdentifier', 'restoreDBClusterFromS_dbClusterIdentifier' - The name of the DB cluster to create from the source data in the Amazon
-- S3 bucket. This parameter isn\'t case-sensitive.
--
-- Constraints:
--
-- -   Must contain from 1 to 63 letters, numbers, or hyphens.
--
-- -   First character must be a letter.
--
-- -   Can\'t end with a hyphen or contain two consecutive hyphens.
--
-- Example: @my-cluster1@
--
-- 'engine', 'restoreDBClusterFromS_engine' - The name of the database engine to be used for this DB cluster.
--
-- Valid Values: @aurora@ (for MySQL 5.6-compatible Aurora), @aurora-mysql@
-- (for MySQL 5.7-compatible Aurora), and @aurora-postgresql@
--
-- 'masterUsername', 'restoreDBClusterFromS_masterUsername' - The name of the master user for the restored DB cluster.
--
-- Constraints:
--
-- -   Must be 1 to 16 letters or numbers.
--
-- -   First character must be a letter.
--
-- -   Can\'t be a reserved word for the chosen database engine.
--
-- 'masterUserPassword', 'restoreDBClusterFromS_masterUserPassword' - The password for the master database user. This password can contain any
-- printable ASCII character except \"\/\", \"\"\", or \"\@\".
--
-- Constraints: Must contain from 8 to 41 characters.
--
-- 'sourceEngine', 'restoreDBClusterFromS_sourceEngine' - The identifier for the database engine that was backed up to create the
-- files stored in the Amazon S3 bucket.
--
-- Valid values: @mysql@
--
-- 'sourceEngineVersion', 'restoreDBClusterFromS_sourceEngineVersion' - The version of the database that the backup files were created from.
--
-- MySQL versions 5.5, 5.6, and 5.7 are supported.
--
-- Example: @5.6.40@, @5.7.28@
--
-- 's3BucketName', 'restoreDBClusterFromS_s3BucketName' - The name of the Amazon S3 bucket that contains the data used to create
-- the Amazon Aurora DB cluster.
--
-- 's3IngestionRoleArn', 'restoreDBClusterFromS_s3IngestionRoleArn' - The Amazon Resource Name (ARN) of the Amazon Web Services Identity and
-- Access Management (IAM) role that authorizes Amazon RDS to access the
-- Amazon S3 bucket on your behalf.
newRestoreDBClusterFromS ::
  -- | 'dbClusterIdentifier'
  Prelude.Text ->
  -- | 'engine'
  Prelude.Text ->
  -- | 'masterUsername'
  Prelude.Text ->
  -- | 'masterUserPassword'
  Prelude.Text ->
  -- | 'sourceEngine'
  Prelude.Text ->
  -- | 'sourceEngineVersion'
  Prelude.Text ->
  -- | 's3BucketName'
  Prelude.Text ->
  -- | 's3IngestionRoleArn'
  Prelude.Text ->
  RestoreDBClusterFromS
newRestoreDBClusterFromS
  pDBClusterIdentifier_
  pEngine_
  pMasterUsername_
  pMasterUserPassword_
  pSourceEngine_
  pSourceEngineVersion_
  pS3BucketName_
  pS3IngestionRoleArn_ =
    RestoreDBClusterFromS'
      { engineVersion =
          Prelude.Nothing,
        deletionProtection = Prelude.Nothing,
        storageEncrypted = Prelude.Nothing,
        dbSubnetGroupName = Prelude.Nothing,
        domain = Prelude.Nothing,
        backtrackWindow = Prelude.Nothing,
        preferredMaintenanceWindow = Prelude.Nothing,
        availabilityZones = Prelude.Nothing,
        characterSetName = Prelude.Nothing,
        kmsKeyId = Prelude.Nothing,
        preferredBackupWindow = Prelude.Nothing,
        backupRetentionPeriod = Prelude.Nothing,
        vpcSecurityGroupIds = Prelude.Nothing,
        databaseName = Prelude.Nothing,
        dbClusterParameterGroupName = Prelude.Nothing,
        s3Prefix = Prelude.Nothing,
        optionGroupName = Prelude.Nothing,
        copyTagsToSnapshot = Prelude.Nothing,
        domainIAMRoleName = Prelude.Nothing,
        tags = Prelude.Nothing,
        port = Prelude.Nothing,
        enableIAMDatabaseAuthentication = Prelude.Nothing,
        enableCloudwatchLogsExports = Prelude.Nothing,
        dbClusterIdentifier = pDBClusterIdentifier_,
        engine = pEngine_,
        masterUsername = pMasterUsername_,
        masterUserPassword = pMasterUserPassword_,
        sourceEngine = pSourceEngine_,
        sourceEngineVersion = pSourceEngineVersion_,
        s3BucketName = pS3BucketName_,
        s3IngestionRoleArn = pS3IngestionRoleArn_
      }

-- | The version number of the database engine to use.
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
-- __Aurora MySQL__
--
-- Example: @5.6.10a@, @5.6.mysql_aurora.1.19.2@, @5.7.12@,
-- @5.7.mysql_aurora.2.04.5@
--
-- __Aurora PostgreSQL__
--
-- Example: @9.6.3@, @10.7@
restoreDBClusterFromS_engineVersion :: Lens.Lens' RestoreDBClusterFromS (Prelude.Maybe Prelude.Text)
restoreDBClusterFromS_engineVersion = Lens.lens (\RestoreDBClusterFromS' {engineVersion} -> engineVersion) (\s@RestoreDBClusterFromS' {} a -> s {engineVersion = a} :: RestoreDBClusterFromS)

-- | A value that indicates whether the DB cluster has deletion protection
-- enabled. The database can\'t be deleted when deletion protection is
-- enabled. By default, deletion protection is disabled.
restoreDBClusterFromS_deletionProtection :: Lens.Lens' RestoreDBClusterFromS (Prelude.Maybe Prelude.Bool)
restoreDBClusterFromS_deletionProtection = Lens.lens (\RestoreDBClusterFromS' {deletionProtection} -> deletionProtection) (\s@RestoreDBClusterFromS' {} a -> s {deletionProtection = a} :: RestoreDBClusterFromS)

-- | A value that indicates whether the restored DB cluster is encrypted.
restoreDBClusterFromS_storageEncrypted :: Lens.Lens' RestoreDBClusterFromS (Prelude.Maybe Prelude.Bool)
restoreDBClusterFromS_storageEncrypted = Lens.lens (\RestoreDBClusterFromS' {storageEncrypted} -> storageEncrypted) (\s@RestoreDBClusterFromS' {} a -> s {storageEncrypted = a} :: RestoreDBClusterFromS)

-- | A DB subnet group to associate with the restored DB cluster.
--
-- Constraints: If supplied, must match the name of an existing
-- DBSubnetGroup.
--
-- Example: @mySubnetgroup@
restoreDBClusterFromS_dbSubnetGroupName :: Lens.Lens' RestoreDBClusterFromS (Prelude.Maybe Prelude.Text)
restoreDBClusterFromS_dbSubnetGroupName = Lens.lens (\RestoreDBClusterFromS' {dbSubnetGroupName} -> dbSubnetGroupName) (\s@RestoreDBClusterFromS' {} a -> s {dbSubnetGroupName = a} :: RestoreDBClusterFromS)

-- | Specify the Active Directory directory ID to restore the DB cluster in.
-- The domain must be created prior to this operation.
--
-- For Amazon Aurora DB clusters, Amazon RDS can use Kerberos
-- Authentication to authenticate users that connect to the DB cluster. For
-- more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/kerberos-authentication.html Kerberos Authentication>
-- in the /Amazon Aurora User Guide/.
restoreDBClusterFromS_domain :: Lens.Lens' RestoreDBClusterFromS (Prelude.Maybe Prelude.Text)
restoreDBClusterFromS_domain = Lens.lens (\RestoreDBClusterFromS' {domain} -> domain) (\s@RestoreDBClusterFromS' {} a -> s {domain = a} :: RestoreDBClusterFromS)

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
restoreDBClusterFromS_backtrackWindow :: Lens.Lens' RestoreDBClusterFromS (Prelude.Maybe Prelude.Integer)
restoreDBClusterFromS_backtrackWindow = Lens.lens (\RestoreDBClusterFromS' {backtrackWindow} -> backtrackWindow) (\s@RestoreDBClusterFromS' {} a -> s {backtrackWindow = a} :: RestoreDBClusterFromS)

-- | The weekly time range during which system maintenance can occur, in
-- Universal Coordinated Time (UTC).
--
-- Format: @ddd:hh24:mi-ddd:hh24:mi@
--
-- The default is a 30-minute window selected at random from an 8-hour
-- block of time for each Amazon Web Services Region, occurring on a random
-- day of the week. To see the time blocks available, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/USER_UpgradeDBInstance.Maintenance.html#AdjustingTheMaintenanceWindow.Aurora Adjusting the Preferred Maintenance Window>
-- in the /Amazon Aurora User Guide./
--
-- Valid Days: Mon, Tue, Wed, Thu, Fri, Sat, Sun.
--
-- Constraints: Minimum 30-minute window.
restoreDBClusterFromS_preferredMaintenanceWindow :: Lens.Lens' RestoreDBClusterFromS (Prelude.Maybe Prelude.Text)
restoreDBClusterFromS_preferredMaintenanceWindow = Lens.lens (\RestoreDBClusterFromS' {preferredMaintenanceWindow} -> preferredMaintenanceWindow) (\s@RestoreDBClusterFromS' {} a -> s {preferredMaintenanceWindow = a} :: RestoreDBClusterFromS)

-- | A list of Availability Zones (AZs) where instances in the restored DB
-- cluster can be created.
restoreDBClusterFromS_availabilityZones :: Lens.Lens' RestoreDBClusterFromS (Prelude.Maybe [Prelude.Text])
restoreDBClusterFromS_availabilityZones = Lens.lens (\RestoreDBClusterFromS' {availabilityZones} -> availabilityZones) (\s@RestoreDBClusterFromS' {} a -> s {availabilityZones = a} :: RestoreDBClusterFromS) Prelude.. Lens.mapping Lens.coerced

-- | A value that indicates that the restored DB cluster should be associated
-- with the specified CharacterSet.
restoreDBClusterFromS_characterSetName :: Lens.Lens' RestoreDBClusterFromS (Prelude.Maybe Prelude.Text)
restoreDBClusterFromS_characterSetName = Lens.lens (\RestoreDBClusterFromS' {characterSetName} -> characterSetName) (\s@RestoreDBClusterFromS' {} a -> s {characterSetName = a} :: RestoreDBClusterFromS)

-- | The Amazon Web Services KMS key identifier for an encrypted DB cluster.
--
-- The Amazon Web Services KMS key identifier is the key ARN, key ID, alias
-- ARN, or alias name for the Amazon Web Services KMS customer master key
-- (CMK). To use a CMK in a different Amazon Web Services account, specify
-- the key ARN or alias ARN.
--
-- If the StorageEncrypted parameter is enabled, and you do not specify a
-- value for the @KmsKeyId@ parameter, then Amazon RDS will use your
-- default CMK. There is a default CMK for your Amazon Web Services
-- account. Your Amazon Web Services account has a different default CMK
-- for each Amazon Web Services Region.
restoreDBClusterFromS_kmsKeyId :: Lens.Lens' RestoreDBClusterFromS (Prelude.Maybe Prelude.Text)
restoreDBClusterFromS_kmsKeyId = Lens.lens (\RestoreDBClusterFromS' {kmsKeyId} -> kmsKeyId) (\s@RestoreDBClusterFromS' {} a -> s {kmsKeyId = a} :: RestoreDBClusterFromS)

-- | The daily time range during which automated backups are created if
-- automated backups are enabled using the @BackupRetentionPeriod@
-- parameter.
--
-- The default is a 30-minute window selected at random from an 8-hour
-- block of time for each Amazon Web Services Region. To view the time
-- blocks available, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/Aurora.Managing.Backups.html#Aurora.Managing.Backups.BackupWindow Backup window>
-- in the /Amazon Aurora User Guide./
--
-- Constraints:
--
-- -   Must be in the format @hh24:mi-hh24:mi@.
--
-- -   Must be in Universal Coordinated Time (UTC).
--
-- -   Must not conflict with the preferred maintenance window.
--
-- -   Must be at least 30 minutes.
restoreDBClusterFromS_preferredBackupWindow :: Lens.Lens' RestoreDBClusterFromS (Prelude.Maybe Prelude.Text)
restoreDBClusterFromS_preferredBackupWindow = Lens.lens (\RestoreDBClusterFromS' {preferredBackupWindow} -> preferredBackupWindow) (\s@RestoreDBClusterFromS' {} a -> s {preferredBackupWindow = a} :: RestoreDBClusterFromS)

-- | The number of days for which automated backups of the restored DB
-- cluster are retained. You must specify a minimum value of 1.
--
-- Default: 1
--
-- Constraints:
--
-- -   Must be a value from 1 to 35
restoreDBClusterFromS_backupRetentionPeriod :: Lens.Lens' RestoreDBClusterFromS (Prelude.Maybe Prelude.Int)
restoreDBClusterFromS_backupRetentionPeriod = Lens.lens (\RestoreDBClusterFromS' {backupRetentionPeriod} -> backupRetentionPeriod) (\s@RestoreDBClusterFromS' {} a -> s {backupRetentionPeriod = a} :: RestoreDBClusterFromS)

-- | A list of EC2 VPC security groups to associate with the restored DB
-- cluster.
restoreDBClusterFromS_vpcSecurityGroupIds :: Lens.Lens' RestoreDBClusterFromS (Prelude.Maybe [Prelude.Text])
restoreDBClusterFromS_vpcSecurityGroupIds = Lens.lens (\RestoreDBClusterFromS' {vpcSecurityGroupIds} -> vpcSecurityGroupIds) (\s@RestoreDBClusterFromS' {} a -> s {vpcSecurityGroupIds = a} :: RestoreDBClusterFromS) Prelude.. Lens.mapping Lens.coerced

-- | The database name for the restored DB cluster.
restoreDBClusterFromS_databaseName :: Lens.Lens' RestoreDBClusterFromS (Prelude.Maybe Prelude.Text)
restoreDBClusterFromS_databaseName = Lens.lens (\RestoreDBClusterFromS' {databaseName} -> databaseName) (\s@RestoreDBClusterFromS' {} a -> s {databaseName = a} :: RestoreDBClusterFromS)

-- | The name of the DB cluster parameter group to associate with the
-- restored DB cluster. If this argument is omitted, @default.aurora5.6@ is
-- used.
--
-- Constraints:
--
-- -   If supplied, must match the name of an existing
--     DBClusterParameterGroup.
restoreDBClusterFromS_dbClusterParameterGroupName :: Lens.Lens' RestoreDBClusterFromS (Prelude.Maybe Prelude.Text)
restoreDBClusterFromS_dbClusterParameterGroupName = Lens.lens (\RestoreDBClusterFromS' {dbClusterParameterGroupName} -> dbClusterParameterGroupName) (\s@RestoreDBClusterFromS' {} a -> s {dbClusterParameterGroupName = a} :: RestoreDBClusterFromS)

-- | The prefix for all of the file names that contain the data used to
-- create the Amazon Aurora DB cluster. If you do not specify a
-- __SourceS3Prefix__ value, then the Amazon Aurora DB cluster is created
-- by using all of the files in the Amazon S3 bucket.
restoreDBClusterFromS_s3Prefix :: Lens.Lens' RestoreDBClusterFromS (Prelude.Maybe Prelude.Text)
restoreDBClusterFromS_s3Prefix = Lens.lens (\RestoreDBClusterFromS' {s3Prefix} -> s3Prefix) (\s@RestoreDBClusterFromS' {} a -> s {s3Prefix = a} :: RestoreDBClusterFromS)

-- | A value that indicates that the restored DB cluster should be associated
-- with the specified option group.
--
-- Permanent options can\'t be removed from an option group. An option
-- group can\'t be removed from a DB cluster once it is associated with a
-- DB cluster.
restoreDBClusterFromS_optionGroupName :: Lens.Lens' RestoreDBClusterFromS (Prelude.Maybe Prelude.Text)
restoreDBClusterFromS_optionGroupName = Lens.lens (\RestoreDBClusterFromS' {optionGroupName} -> optionGroupName) (\s@RestoreDBClusterFromS' {} a -> s {optionGroupName = a} :: RestoreDBClusterFromS)

-- | A value that indicates whether to copy all tags from the restored DB
-- cluster to snapshots of the restored DB cluster. The default is not to
-- copy them.
restoreDBClusterFromS_copyTagsToSnapshot :: Lens.Lens' RestoreDBClusterFromS (Prelude.Maybe Prelude.Bool)
restoreDBClusterFromS_copyTagsToSnapshot = Lens.lens (\RestoreDBClusterFromS' {copyTagsToSnapshot} -> copyTagsToSnapshot) (\s@RestoreDBClusterFromS' {} a -> s {copyTagsToSnapshot = a} :: RestoreDBClusterFromS)

-- | Specify the name of the IAM role to be used when making API calls to the
-- Directory Service.
restoreDBClusterFromS_domainIAMRoleName :: Lens.Lens' RestoreDBClusterFromS (Prelude.Maybe Prelude.Text)
restoreDBClusterFromS_domainIAMRoleName = Lens.lens (\RestoreDBClusterFromS' {domainIAMRoleName} -> domainIAMRoleName) (\s@RestoreDBClusterFromS' {} a -> s {domainIAMRoleName = a} :: RestoreDBClusterFromS)

-- | Undocumented member.
restoreDBClusterFromS_tags :: Lens.Lens' RestoreDBClusterFromS (Prelude.Maybe [Tag])
restoreDBClusterFromS_tags = Lens.lens (\RestoreDBClusterFromS' {tags} -> tags) (\s@RestoreDBClusterFromS' {} a -> s {tags = a} :: RestoreDBClusterFromS) Prelude.. Lens.mapping Lens.coerced

-- | The port number on which the instances in the restored DB cluster accept
-- connections.
--
-- Default: @3306@
restoreDBClusterFromS_port :: Lens.Lens' RestoreDBClusterFromS (Prelude.Maybe Prelude.Int)
restoreDBClusterFromS_port = Lens.lens (\RestoreDBClusterFromS' {port} -> port) (\s@RestoreDBClusterFromS' {} a -> s {port = a} :: RestoreDBClusterFromS)

-- | A value that indicates whether to enable mapping of Amazon Web Services
-- Identity and Access Management (IAM) accounts to database accounts. By
-- default, mapping is disabled.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/UsingWithRDS.IAMDBAuth.html IAM Database Authentication>
-- in the /Amazon Aurora User Guide./
restoreDBClusterFromS_enableIAMDatabaseAuthentication :: Lens.Lens' RestoreDBClusterFromS (Prelude.Maybe Prelude.Bool)
restoreDBClusterFromS_enableIAMDatabaseAuthentication = Lens.lens (\RestoreDBClusterFromS' {enableIAMDatabaseAuthentication} -> enableIAMDatabaseAuthentication) (\s@RestoreDBClusterFromS' {} a -> s {enableIAMDatabaseAuthentication = a} :: RestoreDBClusterFromS)

-- | The list of logs that the restored DB cluster is to export to CloudWatch
-- Logs. The values in the list depend on the DB engine being used. For
-- more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/USER_LogAccess.html#USER_LogAccess.Procedural.UploadtoCloudWatch Publishing Database Logs to Amazon CloudWatch Logs>
-- in the /Amazon Aurora User Guide/.
restoreDBClusterFromS_enableCloudwatchLogsExports :: Lens.Lens' RestoreDBClusterFromS (Prelude.Maybe [Prelude.Text])
restoreDBClusterFromS_enableCloudwatchLogsExports = Lens.lens (\RestoreDBClusterFromS' {enableCloudwatchLogsExports} -> enableCloudwatchLogsExports) (\s@RestoreDBClusterFromS' {} a -> s {enableCloudwatchLogsExports = a} :: RestoreDBClusterFromS) Prelude.. Lens.mapping Lens.coerced

-- | The name of the DB cluster to create from the source data in the Amazon
-- S3 bucket. This parameter isn\'t case-sensitive.
--
-- Constraints:
--
-- -   Must contain from 1 to 63 letters, numbers, or hyphens.
--
-- -   First character must be a letter.
--
-- -   Can\'t end with a hyphen or contain two consecutive hyphens.
--
-- Example: @my-cluster1@
restoreDBClusterFromS_dbClusterIdentifier :: Lens.Lens' RestoreDBClusterFromS Prelude.Text
restoreDBClusterFromS_dbClusterIdentifier = Lens.lens (\RestoreDBClusterFromS' {dbClusterIdentifier} -> dbClusterIdentifier) (\s@RestoreDBClusterFromS' {} a -> s {dbClusterIdentifier = a} :: RestoreDBClusterFromS)

-- | The name of the database engine to be used for this DB cluster.
--
-- Valid Values: @aurora@ (for MySQL 5.6-compatible Aurora), @aurora-mysql@
-- (for MySQL 5.7-compatible Aurora), and @aurora-postgresql@
restoreDBClusterFromS_engine :: Lens.Lens' RestoreDBClusterFromS Prelude.Text
restoreDBClusterFromS_engine = Lens.lens (\RestoreDBClusterFromS' {engine} -> engine) (\s@RestoreDBClusterFromS' {} a -> s {engine = a} :: RestoreDBClusterFromS)

-- | The name of the master user for the restored DB cluster.
--
-- Constraints:
--
-- -   Must be 1 to 16 letters or numbers.
--
-- -   First character must be a letter.
--
-- -   Can\'t be a reserved word for the chosen database engine.
restoreDBClusterFromS_masterUsername :: Lens.Lens' RestoreDBClusterFromS Prelude.Text
restoreDBClusterFromS_masterUsername = Lens.lens (\RestoreDBClusterFromS' {masterUsername} -> masterUsername) (\s@RestoreDBClusterFromS' {} a -> s {masterUsername = a} :: RestoreDBClusterFromS)

-- | The password for the master database user. This password can contain any
-- printable ASCII character except \"\/\", \"\"\", or \"\@\".
--
-- Constraints: Must contain from 8 to 41 characters.
restoreDBClusterFromS_masterUserPassword :: Lens.Lens' RestoreDBClusterFromS Prelude.Text
restoreDBClusterFromS_masterUserPassword = Lens.lens (\RestoreDBClusterFromS' {masterUserPassword} -> masterUserPassword) (\s@RestoreDBClusterFromS' {} a -> s {masterUserPassword = a} :: RestoreDBClusterFromS)

-- | The identifier for the database engine that was backed up to create the
-- files stored in the Amazon S3 bucket.
--
-- Valid values: @mysql@
restoreDBClusterFromS_sourceEngine :: Lens.Lens' RestoreDBClusterFromS Prelude.Text
restoreDBClusterFromS_sourceEngine = Lens.lens (\RestoreDBClusterFromS' {sourceEngine} -> sourceEngine) (\s@RestoreDBClusterFromS' {} a -> s {sourceEngine = a} :: RestoreDBClusterFromS)

-- | The version of the database that the backup files were created from.
--
-- MySQL versions 5.5, 5.6, and 5.7 are supported.
--
-- Example: @5.6.40@, @5.7.28@
restoreDBClusterFromS_sourceEngineVersion :: Lens.Lens' RestoreDBClusterFromS Prelude.Text
restoreDBClusterFromS_sourceEngineVersion = Lens.lens (\RestoreDBClusterFromS' {sourceEngineVersion} -> sourceEngineVersion) (\s@RestoreDBClusterFromS' {} a -> s {sourceEngineVersion = a} :: RestoreDBClusterFromS)

-- | The name of the Amazon S3 bucket that contains the data used to create
-- the Amazon Aurora DB cluster.
restoreDBClusterFromS_s3BucketName :: Lens.Lens' RestoreDBClusterFromS Prelude.Text
restoreDBClusterFromS_s3BucketName = Lens.lens (\RestoreDBClusterFromS' {s3BucketName} -> s3BucketName) (\s@RestoreDBClusterFromS' {} a -> s {s3BucketName = a} :: RestoreDBClusterFromS)

-- | The Amazon Resource Name (ARN) of the Amazon Web Services Identity and
-- Access Management (IAM) role that authorizes Amazon RDS to access the
-- Amazon S3 bucket on your behalf.
restoreDBClusterFromS_s3IngestionRoleArn :: Lens.Lens' RestoreDBClusterFromS Prelude.Text
restoreDBClusterFromS_s3IngestionRoleArn = Lens.lens (\RestoreDBClusterFromS' {s3IngestionRoleArn} -> s3IngestionRoleArn) (\s@RestoreDBClusterFromS' {} a -> s {s3IngestionRoleArn = a} :: RestoreDBClusterFromS)

instance Core.AWSRequest RestoreDBClusterFromS where
  type
    AWSResponse RestoreDBClusterFromS =
      RestoreDBClusterFromSResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "RestoreDBClusterFromS3Result"
      ( \s h x ->
          RestoreDBClusterFromSResponse'
            Prelude.<$> (x Core..@? "DBCluster")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable RestoreDBClusterFromS

instance Prelude.NFData RestoreDBClusterFromS

instance Core.ToHeaders RestoreDBClusterFromS where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath RestoreDBClusterFromS where
  toPath = Prelude.const "/"

instance Core.ToQuery RestoreDBClusterFromS where
  toQuery RestoreDBClusterFromS' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("RestoreDBClusterFromS" :: Prelude.ByteString),
        "Version"
          Core.=: ("2014-10-31" :: Prelude.ByteString),
        "EngineVersion" Core.=: engineVersion,
        "DeletionProtection" Core.=: deletionProtection,
        "StorageEncrypted" Core.=: storageEncrypted,
        "DBSubnetGroupName" Core.=: dbSubnetGroupName,
        "Domain" Core.=: domain,
        "BacktrackWindow" Core.=: backtrackWindow,
        "PreferredMaintenanceWindow"
          Core.=: preferredMaintenanceWindow,
        "AvailabilityZones"
          Core.=: Core.toQuery
            ( Core.toQueryList "AvailabilityZone"
                Prelude.<$> availabilityZones
            ),
        "CharacterSetName" Core.=: characterSetName,
        "KmsKeyId" Core.=: kmsKeyId,
        "PreferredBackupWindow"
          Core.=: preferredBackupWindow,
        "BackupRetentionPeriod"
          Core.=: backupRetentionPeriod,
        "VpcSecurityGroupIds"
          Core.=: Core.toQuery
            ( Core.toQueryList "VpcSecurityGroupId"
                Prelude.<$> vpcSecurityGroupIds
            ),
        "DatabaseName" Core.=: databaseName,
        "DBClusterParameterGroupName"
          Core.=: dbClusterParameterGroupName,
        "S3Prefix" Core.=: s3Prefix,
        "OptionGroupName" Core.=: optionGroupName,
        "CopyTagsToSnapshot" Core.=: copyTagsToSnapshot,
        "DomainIAMRoleName" Core.=: domainIAMRoleName,
        "Tags"
          Core.=: Core.toQuery
            (Core.toQueryList "Tag" Prelude.<$> tags),
        "Port" Core.=: port,
        "EnableIAMDatabaseAuthentication"
          Core.=: enableIAMDatabaseAuthentication,
        "EnableCloudwatchLogsExports"
          Core.=: Core.toQuery
            ( Core.toQueryList "member"
                Prelude.<$> enableCloudwatchLogsExports
            ),
        "DBClusterIdentifier" Core.=: dbClusterIdentifier,
        "Engine" Core.=: engine,
        "MasterUsername" Core.=: masterUsername,
        "MasterUserPassword" Core.=: masterUserPassword,
        "SourceEngine" Core.=: sourceEngine,
        "SourceEngineVersion" Core.=: sourceEngineVersion,
        "S3BucketName" Core.=: s3BucketName,
        "S3IngestionRoleArn" Core.=: s3IngestionRoleArn
      ]

-- | /See:/ 'newRestoreDBClusterFromSResponse' smart constructor.
data RestoreDBClusterFromSResponse = RestoreDBClusterFromSResponse'
  { dbCluster :: Prelude.Maybe DBCluster,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RestoreDBClusterFromSResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dbCluster', 'restoreDBClusterFromSResponse_dbCluster' - Undocumented member.
--
-- 'httpStatus', 'restoreDBClusterFromSResponse_httpStatus' - The response's http status code.
newRestoreDBClusterFromSResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  RestoreDBClusterFromSResponse
newRestoreDBClusterFromSResponse pHttpStatus_ =
  RestoreDBClusterFromSResponse'
    { dbCluster =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
restoreDBClusterFromSResponse_dbCluster :: Lens.Lens' RestoreDBClusterFromSResponse (Prelude.Maybe DBCluster)
restoreDBClusterFromSResponse_dbCluster = Lens.lens (\RestoreDBClusterFromSResponse' {dbCluster} -> dbCluster) (\s@RestoreDBClusterFromSResponse' {} a -> s {dbCluster = a} :: RestoreDBClusterFromSResponse)

-- | The response's http status code.
restoreDBClusterFromSResponse_httpStatus :: Lens.Lens' RestoreDBClusterFromSResponse Prelude.Int
restoreDBClusterFromSResponse_httpStatus = Lens.lens (\RestoreDBClusterFromSResponse' {httpStatus} -> httpStatus) (\s@RestoreDBClusterFromSResponse' {} a -> s {httpStatus = a} :: RestoreDBClusterFromSResponse)

instance Prelude.NFData RestoreDBClusterFromSResponse
