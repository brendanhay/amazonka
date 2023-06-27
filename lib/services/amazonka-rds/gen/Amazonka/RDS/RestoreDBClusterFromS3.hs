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
-- Module      : Amazonka.RDS.RestoreDBClusterFromS3
-- Copyright   : (c) 2013-2023 Brendan Hay
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
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/CHAP_AuroraOverview.html What is Amazon Aurora?>
-- in the /Amazon Aurora User Guide/.
--
-- This action only applies to Aurora DB clusters. The source DB engine
-- must be MySQL.
module Amazonka.RDS.RestoreDBClusterFromS3
  ( -- * Creating a Request
    RestoreDBClusterFromS3 (..),
    newRestoreDBClusterFromS3,

    -- * Request Lenses
    restoreDBClusterFromS3_availabilityZones,
    restoreDBClusterFromS3_backtrackWindow,
    restoreDBClusterFromS3_backupRetentionPeriod,
    restoreDBClusterFromS3_characterSetName,
    restoreDBClusterFromS3_copyTagsToSnapshot,
    restoreDBClusterFromS3_dbClusterParameterGroupName,
    restoreDBClusterFromS3_dbSubnetGroupName,
    restoreDBClusterFromS3_databaseName,
    restoreDBClusterFromS3_deletionProtection,
    restoreDBClusterFromS3_domain,
    restoreDBClusterFromS3_domainIAMRoleName,
    restoreDBClusterFromS3_enableCloudwatchLogsExports,
    restoreDBClusterFromS3_enableIAMDatabaseAuthentication,
    restoreDBClusterFromS3_engineVersion,
    restoreDBClusterFromS3_kmsKeyId,
    restoreDBClusterFromS3_manageMasterUserPassword,
    restoreDBClusterFromS3_masterUserPassword,
    restoreDBClusterFromS3_masterUserSecretKmsKeyId,
    restoreDBClusterFromS3_networkType,
    restoreDBClusterFromS3_optionGroupName,
    restoreDBClusterFromS3_port,
    restoreDBClusterFromS3_preferredBackupWindow,
    restoreDBClusterFromS3_preferredMaintenanceWindow,
    restoreDBClusterFromS3_s3Prefix,
    restoreDBClusterFromS3_serverlessV2ScalingConfiguration,
    restoreDBClusterFromS3_storageEncrypted,
    restoreDBClusterFromS3_storageType,
    restoreDBClusterFromS3_tags,
    restoreDBClusterFromS3_vpcSecurityGroupIds,
    restoreDBClusterFromS3_dbClusterIdentifier,
    restoreDBClusterFromS3_engine,
    restoreDBClusterFromS3_masterUsername,
    restoreDBClusterFromS3_sourceEngine,
    restoreDBClusterFromS3_sourceEngineVersion,
    restoreDBClusterFromS3_s3BucketName,
    restoreDBClusterFromS3_s3IngestionRoleArn,

    -- * Destructuring the Response
    RestoreDBClusterFromS3Response (..),
    newRestoreDBClusterFromS3Response,

    -- * Response Lenses
    restoreDBClusterFromS3Response_dbCluster,
    restoreDBClusterFromS3Response_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.RDS.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newRestoreDBClusterFromS3' smart constructor.
data RestoreDBClusterFromS3 = RestoreDBClusterFromS3'
  { -- | A list of Availability Zones (AZs) where instances in the restored DB
    -- cluster can be created.
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
    backtrackWindow :: Prelude.Maybe Prelude.Integer,
    -- | The number of days for which automated backups of the restored DB
    -- cluster are retained. You must specify a minimum value of 1.
    --
    -- Default: 1
    --
    -- Constraints:
    --
    -- -   Must be a value from 1 to 35
    backupRetentionPeriod :: Prelude.Maybe Prelude.Int,
    -- | A value that indicates that the restored DB cluster should be associated
    -- with the specified CharacterSet.
    characterSetName :: Prelude.Maybe Prelude.Text,
    -- | A value that indicates whether to copy all tags from the restored DB
    -- cluster to snapshots of the restored DB cluster. The default is not to
    -- copy them.
    copyTagsToSnapshot :: Prelude.Maybe Prelude.Bool,
    -- | The name of the DB cluster parameter group to associate with the
    -- restored DB cluster. If this argument is omitted, the default parameter
    -- group for the engine version is used.
    --
    -- Constraints:
    --
    -- -   If supplied, must match the name of an existing
    --     DBClusterParameterGroup.
    dbClusterParameterGroupName :: Prelude.Maybe Prelude.Text,
    -- | A DB subnet group to associate with the restored DB cluster.
    --
    -- Constraints: If supplied, must match the name of an existing
    -- DBSubnetGroup.
    --
    -- Example: @mydbsubnetgroup@
    dbSubnetGroupName :: Prelude.Maybe Prelude.Text,
    -- | The database name for the restored DB cluster.
    databaseName :: Prelude.Maybe Prelude.Text,
    -- | A value that indicates whether the DB cluster has deletion protection
    -- enabled. The database can\'t be deleted when deletion protection is
    -- enabled. By default, deletion protection isn\'t enabled.
    deletionProtection :: Prelude.Maybe Prelude.Bool,
    -- | Specify the Active Directory directory ID to restore the DB cluster in.
    -- The domain must be created prior to this operation.
    --
    -- For Amazon Aurora DB clusters, Amazon RDS can use Kerberos
    -- Authentication to authenticate users that connect to the DB cluster. For
    -- more information, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/kerberos-authentication.html Kerberos Authentication>
    -- in the /Amazon Aurora User Guide/.
    domain :: Prelude.Maybe Prelude.Text,
    -- | Specify the name of the IAM role to be used when making API calls to the
    -- Directory Service.
    domainIAMRoleName :: Prelude.Maybe Prelude.Text,
    -- | The list of logs that the restored DB cluster is to export to CloudWatch
    -- Logs. The values in the list depend on the DB engine being used.
    --
    -- __Aurora MySQL__
    --
    -- Possible values are @audit@, @error@, @general@, and @slowquery@.
    --
    -- For more information about exporting CloudWatch Logs for Amazon Aurora,
    -- see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/USER_LogAccess.html#USER_LogAccess.Procedural.UploadtoCloudWatch Publishing Database Logs to Amazon CloudWatch Logs>
    -- in the /Amazon Aurora User Guide/.
    enableCloudwatchLogsExports :: Prelude.Maybe [Prelude.Text],
    -- | A value that indicates whether to enable mapping of Amazon Web Services
    -- Identity and Access Management (IAM) accounts to database accounts. By
    -- default, mapping isn\'t enabled.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/UsingWithRDS.IAMDBAuth.html IAM Database Authentication>
    -- in the /Amazon Aurora User Guide/.
    enableIAMDatabaseAuthentication :: Prelude.Maybe Prelude.Bool,
    -- | The version number of the database engine to use.
    --
    -- To list all of the available engine versions for @aurora-mysql@ (Aurora
    -- MySQL), use the following command:
    --
    -- @aws rds describe-db-engine-versions --engine aurora-mysql --query \"DBEngineVersions[].EngineVersion\"@
    --
    -- __Aurora MySQL__
    --
    -- Examples: @5.7.mysql_aurora.2.07.1@, @8.0.mysql_aurora.3.02.0@
    engineVersion :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services KMS key identifier for an encrypted DB cluster.
    --
    -- The Amazon Web Services KMS key identifier is the key ARN, key ID, alias
    -- ARN, or alias name for the KMS key. To use a KMS key in a different
    -- Amazon Web Services account, specify the key ARN or alias ARN.
    --
    -- If the StorageEncrypted parameter is enabled, and you do not specify a
    -- value for the @KmsKeyId@ parameter, then Amazon RDS will use your
    -- default KMS key. There is a default KMS key for your Amazon Web Services
    -- account. Your Amazon Web Services account has a different default KMS
    -- key for each Amazon Web Services Region.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | A value that indicates whether to manage the master user password with
    -- Amazon Web Services Secrets Manager.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/rds-secrets-manager.html Password management with Amazon Web Services Secrets Manager>
    -- in the /Amazon RDS User Guide/ and
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/rds-secrets-manager.html Password management with Amazon Web Services Secrets Manager>
    -- in the /Amazon Aurora User Guide./
    --
    -- Constraints:
    --
    -- -   Can\'t manage the master user password with Amazon Web Services
    --     Secrets Manager if @MasterUserPassword@ is specified.
    manageMasterUserPassword :: Prelude.Maybe Prelude.Bool,
    -- | The password for the master database user. This password can contain any
    -- printable ASCII character except \"\/\", \"\"\", or \"\@\".
    --
    -- Constraints:
    --
    -- -   Must contain from 8 to 41 characters.
    --
    -- -   Can\'t be specified if @ManageMasterUserPassword@ is turned on.
    masterUserPassword :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services KMS key identifier to encrypt a secret that is
    -- automatically generated and managed in Amazon Web Services Secrets
    -- Manager.
    --
    -- This setting is valid only if the master user password is managed by RDS
    -- in Amazon Web Services Secrets Manager for the DB cluster.
    --
    -- The Amazon Web Services KMS key identifier is the key ARN, key ID, alias
    -- ARN, or alias name for the KMS key. To use a KMS key in a different
    -- Amazon Web Services account, specify the key ARN or alias ARN.
    --
    -- If you don\'t specify @MasterUserSecretKmsKeyId@, then the
    -- @aws\/secretsmanager@ KMS key is used to encrypt the secret. If the
    -- secret is in a different Amazon Web Services account, then you can\'t
    -- use the @aws\/secretsmanager@ KMS key to encrypt the secret, and you
    -- must use a customer managed KMS key.
    --
    -- There is a default KMS key for your Amazon Web Services account. Your
    -- Amazon Web Services account has a different default KMS key for each
    -- Amazon Web Services Region.
    masterUserSecretKmsKeyId :: Prelude.Maybe Prelude.Text,
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
    networkType :: Prelude.Maybe Prelude.Text,
    -- | A value that indicates that the restored DB cluster should be associated
    -- with the specified option group.
    --
    -- Permanent options can\'t be removed from an option group. An option
    -- group can\'t be removed from a DB cluster once it is associated with a
    -- DB cluster.
    optionGroupName :: Prelude.Maybe Prelude.Text,
    -- | The port number on which the instances in the restored DB cluster accept
    -- connections.
    --
    -- Default: @3306@
    port :: Prelude.Maybe Prelude.Int,
    -- | The daily time range during which automated backups are created if
    -- automated backups are enabled using the @BackupRetentionPeriod@
    -- parameter.
    --
    -- The default is a 30-minute window selected at random from an 8-hour
    -- block of time for each Amazon Web Services Region. To view the time
    -- blocks available, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/Aurora.Managing.Backups.html#Aurora.Managing.Backups.BackupWindow Backup window>
    -- in the /Amazon Aurora User Guide/.
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
    -- | The weekly time range during which system maintenance can occur, in
    -- Universal Coordinated Time (UTC).
    --
    -- Format: @ddd:hh24:mi-ddd:hh24:mi@
    --
    -- The default is a 30-minute window selected at random from an 8-hour
    -- block of time for each Amazon Web Services Region, occurring on a random
    -- day of the week. To see the time blocks available, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/USER_UpgradeDBInstance.Maintenance.html#AdjustingTheMaintenanceWindow.Aurora Adjusting the Preferred Maintenance Window>
    -- in the /Amazon Aurora User Guide/.
    --
    -- Valid Days: Mon, Tue, Wed, Thu, Fri, Sat, Sun.
    --
    -- Constraints: Minimum 30-minute window.
    preferredMaintenanceWindow :: Prelude.Maybe Prelude.Text,
    -- | The prefix for all of the file names that contain the data used to
    -- create the Amazon Aurora DB cluster. If you do not specify a
    -- __SourceS3Prefix__ value, then the Amazon Aurora DB cluster is created
    -- by using all of the files in the Amazon S3 bucket.
    s3Prefix :: Prelude.Maybe Prelude.Text,
    serverlessV2ScalingConfiguration :: Prelude.Maybe ServerlessV2ScalingConfiguration,
    -- | A value that indicates whether the restored DB cluster is encrypted.
    storageEncrypted :: Prelude.Maybe Prelude.Bool,
    -- | Specifies the storage type to be associated with the DB cluster.
    --
    -- Valid values: @aurora@, @aurora-iopt1@
    --
    -- Default: @aurora@
    --
    -- Valid for: Aurora DB clusters only
    storageType :: Prelude.Maybe Prelude.Text,
    tags :: Prelude.Maybe [Tag],
    -- | A list of EC2 VPC security groups to associate with the restored DB
    -- cluster.
    vpcSecurityGroupIds :: Prelude.Maybe [Prelude.Text],
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
    -- Valid Values: @aurora-mysql@ (for Aurora MySQL)
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
-- Create a value of 'RestoreDBClusterFromS3' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'availabilityZones', 'restoreDBClusterFromS3_availabilityZones' - A list of Availability Zones (AZs) where instances in the restored DB
-- cluster can be created.
--
-- 'backtrackWindow', 'restoreDBClusterFromS3_backtrackWindow' - The target backtrack window, in seconds. To disable backtracking, set
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
-- 'backupRetentionPeriod', 'restoreDBClusterFromS3_backupRetentionPeriod' - The number of days for which automated backups of the restored DB
-- cluster are retained. You must specify a minimum value of 1.
--
-- Default: 1
--
-- Constraints:
--
-- -   Must be a value from 1 to 35
--
-- 'characterSetName', 'restoreDBClusterFromS3_characterSetName' - A value that indicates that the restored DB cluster should be associated
-- with the specified CharacterSet.
--
-- 'copyTagsToSnapshot', 'restoreDBClusterFromS3_copyTagsToSnapshot' - A value that indicates whether to copy all tags from the restored DB
-- cluster to snapshots of the restored DB cluster. The default is not to
-- copy them.
--
-- 'dbClusterParameterGroupName', 'restoreDBClusterFromS3_dbClusterParameterGroupName' - The name of the DB cluster parameter group to associate with the
-- restored DB cluster. If this argument is omitted, the default parameter
-- group for the engine version is used.
--
-- Constraints:
--
-- -   If supplied, must match the name of an existing
--     DBClusterParameterGroup.
--
-- 'dbSubnetGroupName', 'restoreDBClusterFromS3_dbSubnetGroupName' - A DB subnet group to associate with the restored DB cluster.
--
-- Constraints: If supplied, must match the name of an existing
-- DBSubnetGroup.
--
-- Example: @mydbsubnetgroup@
--
-- 'databaseName', 'restoreDBClusterFromS3_databaseName' - The database name for the restored DB cluster.
--
-- 'deletionProtection', 'restoreDBClusterFromS3_deletionProtection' - A value that indicates whether the DB cluster has deletion protection
-- enabled. The database can\'t be deleted when deletion protection is
-- enabled. By default, deletion protection isn\'t enabled.
--
-- 'domain', 'restoreDBClusterFromS3_domain' - Specify the Active Directory directory ID to restore the DB cluster in.
-- The domain must be created prior to this operation.
--
-- For Amazon Aurora DB clusters, Amazon RDS can use Kerberos
-- Authentication to authenticate users that connect to the DB cluster. For
-- more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/kerberos-authentication.html Kerberos Authentication>
-- in the /Amazon Aurora User Guide/.
--
-- 'domainIAMRoleName', 'restoreDBClusterFromS3_domainIAMRoleName' - Specify the name of the IAM role to be used when making API calls to the
-- Directory Service.
--
-- 'enableCloudwatchLogsExports', 'restoreDBClusterFromS3_enableCloudwatchLogsExports' - The list of logs that the restored DB cluster is to export to CloudWatch
-- Logs. The values in the list depend on the DB engine being used.
--
-- __Aurora MySQL__
--
-- Possible values are @audit@, @error@, @general@, and @slowquery@.
--
-- For more information about exporting CloudWatch Logs for Amazon Aurora,
-- see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/USER_LogAccess.html#USER_LogAccess.Procedural.UploadtoCloudWatch Publishing Database Logs to Amazon CloudWatch Logs>
-- in the /Amazon Aurora User Guide/.
--
-- 'enableIAMDatabaseAuthentication', 'restoreDBClusterFromS3_enableIAMDatabaseAuthentication' - A value that indicates whether to enable mapping of Amazon Web Services
-- Identity and Access Management (IAM) accounts to database accounts. By
-- default, mapping isn\'t enabled.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/UsingWithRDS.IAMDBAuth.html IAM Database Authentication>
-- in the /Amazon Aurora User Guide/.
--
-- 'engineVersion', 'restoreDBClusterFromS3_engineVersion' - The version number of the database engine to use.
--
-- To list all of the available engine versions for @aurora-mysql@ (Aurora
-- MySQL), use the following command:
--
-- @aws rds describe-db-engine-versions --engine aurora-mysql --query \"DBEngineVersions[].EngineVersion\"@
--
-- __Aurora MySQL__
--
-- Examples: @5.7.mysql_aurora.2.07.1@, @8.0.mysql_aurora.3.02.0@
--
-- 'kmsKeyId', 'restoreDBClusterFromS3_kmsKeyId' - The Amazon Web Services KMS key identifier for an encrypted DB cluster.
--
-- The Amazon Web Services KMS key identifier is the key ARN, key ID, alias
-- ARN, or alias name for the KMS key. To use a KMS key in a different
-- Amazon Web Services account, specify the key ARN or alias ARN.
--
-- If the StorageEncrypted parameter is enabled, and you do not specify a
-- value for the @KmsKeyId@ parameter, then Amazon RDS will use your
-- default KMS key. There is a default KMS key for your Amazon Web Services
-- account. Your Amazon Web Services account has a different default KMS
-- key for each Amazon Web Services Region.
--
-- 'manageMasterUserPassword', 'restoreDBClusterFromS3_manageMasterUserPassword' - A value that indicates whether to manage the master user password with
-- Amazon Web Services Secrets Manager.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/rds-secrets-manager.html Password management with Amazon Web Services Secrets Manager>
-- in the /Amazon RDS User Guide/ and
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/rds-secrets-manager.html Password management with Amazon Web Services Secrets Manager>
-- in the /Amazon Aurora User Guide./
--
-- Constraints:
--
-- -   Can\'t manage the master user password with Amazon Web Services
--     Secrets Manager if @MasterUserPassword@ is specified.
--
-- 'masterUserPassword', 'restoreDBClusterFromS3_masterUserPassword' - The password for the master database user. This password can contain any
-- printable ASCII character except \"\/\", \"\"\", or \"\@\".
--
-- Constraints:
--
-- -   Must contain from 8 to 41 characters.
--
-- -   Can\'t be specified if @ManageMasterUserPassword@ is turned on.
--
-- 'masterUserSecretKmsKeyId', 'restoreDBClusterFromS3_masterUserSecretKmsKeyId' - The Amazon Web Services KMS key identifier to encrypt a secret that is
-- automatically generated and managed in Amazon Web Services Secrets
-- Manager.
--
-- This setting is valid only if the master user password is managed by RDS
-- in Amazon Web Services Secrets Manager for the DB cluster.
--
-- The Amazon Web Services KMS key identifier is the key ARN, key ID, alias
-- ARN, or alias name for the KMS key. To use a KMS key in a different
-- Amazon Web Services account, specify the key ARN or alias ARN.
--
-- If you don\'t specify @MasterUserSecretKmsKeyId@, then the
-- @aws\/secretsmanager@ KMS key is used to encrypt the secret. If the
-- secret is in a different Amazon Web Services account, then you can\'t
-- use the @aws\/secretsmanager@ KMS key to encrypt the secret, and you
-- must use a customer managed KMS key.
--
-- There is a default KMS key for your Amazon Web Services account. Your
-- Amazon Web Services account has a different default KMS key for each
-- Amazon Web Services Region.
--
-- 'networkType', 'restoreDBClusterFromS3_networkType' - The network type of the DB cluster.
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
-- 'optionGroupName', 'restoreDBClusterFromS3_optionGroupName' - A value that indicates that the restored DB cluster should be associated
-- with the specified option group.
--
-- Permanent options can\'t be removed from an option group. An option
-- group can\'t be removed from a DB cluster once it is associated with a
-- DB cluster.
--
-- 'port', 'restoreDBClusterFromS3_port' - The port number on which the instances in the restored DB cluster accept
-- connections.
--
-- Default: @3306@
--
-- 'preferredBackupWindow', 'restoreDBClusterFromS3_preferredBackupWindow' - The daily time range during which automated backups are created if
-- automated backups are enabled using the @BackupRetentionPeriod@
-- parameter.
--
-- The default is a 30-minute window selected at random from an 8-hour
-- block of time for each Amazon Web Services Region. To view the time
-- blocks available, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/Aurora.Managing.Backups.html#Aurora.Managing.Backups.BackupWindow Backup window>
-- in the /Amazon Aurora User Guide/.
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
-- 'preferredMaintenanceWindow', 'restoreDBClusterFromS3_preferredMaintenanceWindow' - The weekly time range during which system maintenance can occur, in
-- Universal Coordinated Time (UTC).
--
-- Format: @ddd:hh24:mi-ddd:hh24:mi@
--
-- The default is a 30-minute window selected at random from an 8-hour
-- block of time for each Amazon Web Services Region, occurring on a random
-- day of the week. To see the time blocks available, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/USER_UpgradeDBInstance.Maintenance.html#AdjustingTheMaintenanceWindow.Aurora Adjusting the Preferred Maintenance Window>
-- in the /Amazon Aurora User Guide/.
--
-- Valid Days: Mon, Tue, Wed, Thu, Fri, Sat, Sun.
--
-- Constraints: Minimum 30-minute window.
--
-- 's3Prefix', 'restoreDBClusterFromS3_s3Prefix' - The prefix for all of the file names that contain the data used to
-- create the Amazon Aurora DB cluster. If you do not specify a
-- __SourceS3Prefix__ value, then the Amazon Aurora DB cluster is created
-- by using all of the files in the Amazon S3 bucket.
--
-- 'serverlessV2ScalingConfiguration', 'restoreDBClusterFromS3_serverlessV2ScalingConfiguration' - Undocumented member.
--
-- 'storageEncrypted', 'restoreDBClusterFromS3_storageEncrypted' - A value that indicates whether the restored DB cluster is encrypted.
--
-- 'storageType', 'restoreDBClusterFromS3_storageType' - Specifies the storage type to be associated with the DB cluster.
--
-- Valid values: @aurora@, @aurora-iopt1@
--
-- Default: @aurora@
--
-- Valid for: Aurora DB clusters only
--
-- 'tags', 'restoreDBClusterFromS3_tags' - Undocumented member.
--
-- 'vpcSecurityGroupIds', 'restoreDBClusterFromS3_vpcSecurityGroupIds' - A list of EC2 VPC security groups to associate with the restored DB
-- cluster.
--
-- 'dbClusterIdentifier', 'restoreDBClusterFromS3_dbClusterIdentifier' - The name of the DB cluster to create from the source data in the Amazon
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
-- 'engine', 'restoreDBClusterFromS3_engine' - The name of the database engine to be used for this DB cluster.
--
-- Valid Values: @aurora-mysql@ (for Aurora MySQL)
--
-- 'masterUsername', 'restoreDBClusterFromS3_masterUsername' - The name of the master user for the restored DB cluster.
--
-- Constraints:
--
-- -   Must be 1 to 16 letters or numbers.
--
-- -   First character must be a letter.
--
-- -   Can\'t be a reserved word for the chosen database engine.
--
-- 'sourceEngine', 'restoreDBClusterFromS3_sourceEngine' - The identifier for the database engine that was backed up to create the
-- files stored in the Amazon S3 bucket.
--
-- Valid values: @mysql@
--
-- 'sourceEngineVersion', 'restoreDBClusterFromS3_sourceEngineVersion' - The version of the database that the backup files were created from.
--
-- MySQL versions 5.5, 5.6, and 5.7 are supported.
--
-- Example: @5.6.40@, @5.7.28@
--
-- 's3BucketName', 'restoreDBClusterFromS3_s3BucketName' - The name of the Amazon S3 bucket that contains the data used to create
-- the Amazon Aurora DB cluster.
--
-- 's3IngestionRoleArn', 'restoreDBClusterFromS3_s3IngestionRoleArn' - The Amazon Resource Name (ARN) of the Amazon Web Services Identity and
-- Access Management (IAM) role that authorizes Amazon RDS to access the
-- Amazon S3 bucket on your behalf.
newRestoreDBClusterFromS3 ::
  -- | 'dbClusterIdentifier'
  Prelude.Text ->
  -- | 'engine'
  Prelude.Text ->
  -- | 'masterUsername'
  Prelude.Text ->
  -- | 'sourceEngine'
  Prelude.Text ->
  -- | 'sourceEngineVersion'
  Prelude.Text ->
  -- | 's3BucketName'
  Prelude.Text ->
  -- | 's3IngestionRoleArn'
  Prelude.Text ->
  RestoreDBClusterFromS3
newRestoreDBClusterFromS3
  pDBClusterIdentifier_
  pEngine_
  pMasterUsername_
  pSourceEngine_
  pSourceEngineVersion_
  pS3BucketName_
  pS3IngestionRoleArn_ =
    RestoreDBClusterFromS3'
      { availabilityZones =
          Prelude.Nothing,
        backtrackWindow = Prelude.Nothing,
        backupRetentionPeriod = Prelude.Nothing,
        characterSetName = Prelude.Nothing,
        copyTagsToSnapshot = Prelude.Nothing,
        dbClusterParameterGroupName = Prelude.Nothing,
        dbSubnetGroupName = Prelude.Nothing,
        databaseName = Prelude.Nothing,
        deletionProtection = Prelude.Nothing,
        domain = Prelude.Nothing,
        domainIAMRoleName = Prelude.Nothing,
        enableCloudwatchLogsExports = Prelude.Nothing,
        enableIAMDatabaseAuthentication = Prelude.Nothing,
        engineVersion = Prelude.Nothing,
        kmsKeyId = Prelude.Nothing,
        manageMasterUserPassword = Prelude.Nothing,
        masterUserPassword = Prelude.Nothing,
        masterUserSecretKmsKeyId = Prelude.Nothing,
        networkType = Prelude.Nothing,
        optionGroupName = Prelude.Nothing,
        port = Prelude.Nothing,
        preferredBackupWindow = Prelude.Nothing,
        preferredMaintenanceWindow = Prelude.Nothing,
        s3Prefix = Prelude.Nothing,
        serverlessV2ScalingConfiguration = Prelude.Nothing,
        storageEncrypted = Prelude.Nothing,
        storageType = Prelude.Nothing,
        tags = Prelude.Nothing,
        vpcSecurityGroupIds = Prelude.Nothing,
        dbClusterIdentifier = pDBClusterIdentifier_,
        engine = pEngine_,
        masterUsername = pMasterUsername_,
        sourceEngine = pSourceEngine_,
        sourceEngineVersion = pSourceEngineVersion_,
        s3BucketName = pS3BucketName_,
        s3IngestionRoleArn = pS3IngestionRoleArn_
      }

-- | A list of Availability Zones (AZs) where instances in the restored DB
-- cluster can be created.
restoreDBClusterFromS3_availabilityZones :: Lens.Lens' RestoreDBClusterFromS3 (Prelude.Maybe [Prelude.Text])
restoreDBClusterFromS3_availabilityZones = Lens.lens (\RestoreDBClusterFromS3' {availabilityZones} -> availabilityZones) (\s@RestoreDBClusterFromS3' {} a -> s {availabilityZones = a} :: RestoreDBClusterFromS3) Prelude.. Lens.mapping Lens.coerced

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
restoreDBClusterFromS3_backtrackWindow :: Lens.Lens' RestoreDBClusterFromS3 (Prelude.Maybe Prelude.Integer)
restoreDBClusterFromS3_backtrackWindow = Lens.lens (\RestoreDBClusterFromS3' {backtrackWindow} -> backtrackWindow) (\s@RestoreDBClusterFromS3' {} a -> s {backtrackWindow = a} :: RestoreDBClusterFromS3)

-- | The number of days for which automated backups of the restored DB
-- cluster are retained. You must specify a minimum value of 1.
--
-- Default: 1
--
-- Constraints:
--
-- -   Must be a value from 1 to 35
restoreDBClusterFromS3_backupRetentionPeriod :: Lens.Lens' RestoreDBClusterFromS3 (Prelude.Maybe Prelude.Int)
restoreDBClusterFromS3_backupRetentionPeriod = Lens.lens (\RestoreDBClusterFromS3' {backupRetentionPeriod} -> backupRetentionPeriod) (\s@RestoreDBClusterFromS3' {} a -> s {backupRetentionPeriod = a} :: RestoreDBClusterFromS3)

-- | A value that indicates that the restored DB cluster should be associated
-- with the specified CharacterSet.
restoreDBClusterFromS3_characterSetName :: Lens.Lens' RestoreDBClusterFromS3 (Prelude.Maybe Prelude.Text)
restoreDBClusterFromS3_characterSetName = Lens.lens (\RestoreDBClusterFromS3' {characterSetName} -> characterSetName) (\s@RestoreDBClusterFromS3' {} a -> s {characterSetName = a} :: RestoreDBClusterFromS3)

-- | A value that indicates whether to copy all tags from the restored DB
-- cluster to snapshots of the restored DB cluster. The default is not to
-- copy them.
restoreDBClusterFromS3_copyTagsToSnapshot :: Lens.Lens' RestoreDBClusterFromS3 (Prelude.Maybe Prelude.Bool)
restoreDBClusterFromS3_copyTagsToSnapshot = Lens.lens (\RestoreDBClusterFromS3' {copyTagsToSnapshot} -> copyTagsToSnapshot) (\s@RestoreDBClusterFromS3' {} a -> s {copyTagsToSnapshot = a} :: RestoreDBClusterFromS3)

-- | The name of the DB cluster parameter group to associate with the
-- restored DB cluster. If this argument is omitted, the default parameter
-- group for the engine version is used.
--
-- Constraints:
--
-- -   If supplied, must match the name of an existing
--     DBClusterParameterGroup.
restoreDBClusterFromS3_dbClusterParameterGroupName :: Lens.Lens' RestoreDBClusterFromS3 (Prelude.Maybe Prelude.Text)
restoreDBClusterFromS3_dbClusterParameterGroupName = Lens.lens (\RestoreDBClusterFromS3' {dbClusterParameterGroupName} -> dbClusterParameterGroupName) (\s@RestoreDBClusterFromS3' {} a -> s {dbClusterParameterGroupName = a} :: RestoreDBClusterFromS3)

-- | A DB subnet group to associate with the restored DB cluster.
--
-- Constraints: If supplied, must match the name of an existing
-- DBSubnetGroup.
--
-- Example: @mydbsubnetgroup@
restoreDBClusterFromS3_dbSubnetGroupName :: Lens.Lens' RestoreDBClusterFromS3 (Prelude.Maybe Prelude.Text)
restoreDBClusterFromS3_dbSubnetGroupName = Lens.lens (\RestoreDBClusterFromS3' {dbSubnetGroupName} -> dbSubnetGroupName) (\s@RestoreDBClusterFromS3' {} a -> s {dbSubnetGroupName = a} :: RestoreDBClusterFromS3)

-- | The database name for the restored DB cluster.
restoreDBClusterFromS3_databaseName :: Lens.Lens' RestoreDBClusterFromS3 (Prelude.Maybe Prelude.Text)
restoreDBClusterFromS3_databaseName = Lens.lens (\RestoreDBClusterFromS3' {databaseName} -> databaseName) (\s@RestoreDBClusterFromS3' {} a -> s {databaseName = a} :: RestoreDBClusterFromS3)

-- | A value that indicates whether the DB cluster has deletion protection
-- enabled. The database can\'t be deleted when deletion protection is
-- enabled. By default, deletion protection isn\'t enabled.
restoreDBClusterFromS3_deletionProtection :: Lens.Lens' RestoreDBClusterFromS3 (Prelude.Maybe Prelude.Bool)
restoreDBClusterFromS3_deletionProtection = Lens.lens (\RestoreDBClusterFromS3' {deletionProtection} -> deletionProtection) (\s@RestoreDBClusterFromS3' {} a -> s {deletionProtection = a} :: RestoreDBClusterFromS3)

-- | Specify the Active Directory directory ID to restore the DB cluster in.
-- The domain must be created prior to this operation.
--
-- For Amazon Aurora DB clusters, Amazon RDS can use Kerberos
-- Authentication to authenticate users that connect to the DB cluster. For
-- more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/kerberos-authentication.html Kerberos Authentication>
-- in the /Amazon Aurora User Guide/.
restoreDBClusterFromS3_domain :: Lens.Lens' RestoreDBClusterFromS3 (Prelude.Maybe Prelude.Text)
restoreDBClusterFromS3_domain = Lens.lens (\RestoreDBClusterFromS3' {domain} -> domain) (\s@RestoreDBClusterFromS3' {} a -> s {domain = a} :: RestoreDBClusterFromS3)

-- | Specify the name of the IAM role to be used when making API calls to the
-- Directory Service.
restoreDBClusterFromS3_domainIAMRoleName :: Lens.Lens' RestoreDBClusterFromS3 (Prelude.Maybe Prelude.Text)
restoreDBClusterFromS3_domainIAMRoleName = Lens.lens (\RestoreDBClusterFromS3' {domainIAMRoleName} -> domainIAMRoleName) (\s@RestoreDBClusterFromS3' {} a -> s {domainIAMRoleName = a} :: RestoreDBClusterFromS3)

-- | The list of logs that the restored DB cluster is to export to CloudWatch
-- Logs. The values in the list depend on the DB engine being used.
--
-- __Aurora MySQL__
--
-- Possible values are @audit@, @error@, @general@, and @slowquery@.
--
-- For more information about exporting CloudWatch Logs for Amazon Aurora,
-- see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/USER_LogAccess.html#USER_LogAccess.Procedural.UploadtoCloudWatch Publishing Database Logs to Amazon CloudWatch Logs>
-- in the /Amazon Aurora User Guide/.
restoreDBClusterFromS3_enableCloudwatchLogsExports :: Lens.Lens' RestoreDBClusterFromS3 (Prelude.Maybe [Prelude.Text])
restoreDBClusterFromS3_enableCloudwatchLogsExports = Lens.lens (\RestoreDBClusterFromS3' {enableCloudwatchLogsExports} -> enableCloudwatchLogsExports) (\s@RestoreDBClusterFromS3' {} a -> s {enableCloudwatchLogsExports = a} :: RestoreDBClusterFromS3) Prelude.. Lens.mapping Lens.coerced

-- | A value that indicates whether to enable mapping of Amazon Web Services
-- Identity and Access Management (IAM) accounts to database accounts. By
-- default, mapping isn\'t enabled.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/UsingWithRDS.IAMDBAuth.html IAM Database Authentication>
-- in the /Amazon Aurora User Guide/.
restoreDBClusterFromS3_enableIAMDatabaseAuthentication :: Lens.Lens' RestoreDBClusterFromS3 (Prelude.Maybe Prelude.Bool)
restoreDBClusterFromS3_enableIAMDatabaseAuthentication = Lens.lens (\RestoreDBClusterFromS3' {enableIAMDatabaseAuthentication} -> enableIAMDatabaseAuthentication) (\s@RestoreDBClusterFromS3' {} a -> s {enableIAMDatabaseAuthentication = a} :: RestoreDBClusterFromS3)

-- | The version number of the database engine to use.
--
-- To list all of the available engine versions for @aurora-mysql@ (Aurora
-- MySQL), use the following command:
--
-- @aws rds describe-db-engine-versions --engine aurora-mysql --query \"DBEngineVersions[].EngineVersion\"@
--
-- __Aurora MySQL__
--
-- Examples: @5.7.mysql_aurora.2.07.1@, @8.0.mysql_aurora.3.02.0@
restoreDBClusterFromS3_engineVersion :: Lens.Lens' RestoreDBClusterFromS3 (Prelude.Maybe Prelude.Text)
restoreDBClusterFromS3_engineVersion = Lens.lens (\RestoreDBClusterFromS3' {engineVersion} -> engineVersion) (\s@RestoreDBClusterFromS3' {} a -> s {engineVersion = a} :: RestoreDBClusterFromS3)

-- | The Amazon Web Services KMS key identifier for an encrypted DB cluster.
--
-- The Amazon Web Services KMS key identifier is the key ARN, key ID, alias
-- ARN, or alias name for the KMS key. To use a KMS key in a different
-- Amazon Web Services account, specify the key ARN or alias ARN.
--
-- If the StorageEncrypted parameter is enabled, and you do not specify a
-- value for the @KmsKeyId@ parameter, then Amazon RDS will use your
-- default KMS key. There is a default KMS key for your Amazon Web Services
-- account. Your Amazon Web Services account has a different default KMS
-- key for each Amazon Web Services Region.
restoreDBClusterFromS3_kmsKeyId :: Lens.Lens' RestoreDBClusterFromS3 (Prelude.Maybe Prelude.Text)
restoreDBClusterFromS3_kmsKeyId = Lens.lens (\RestoreDBClusterFromS3' {kmsKeyId} -> kmsKeyId) (\s@RestoreDBClusterFromS3' {} a -> s {kmsKeyId = a} :: RestoreDBClusterFromS3)

-- | A value that indicates whether to manage the master user password with
-- Amazon Web Services Secrets Manager.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/rds-secrets-manager.html Password management with Amazon Web Services Secrets Manager>
-- in the /Amazon RDS User Guide/ and
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/rds-secrets-manager.html Password management with Amazon Web Services Secrets Manager>
-- in the /Amazon Aurora User Guide./
--
-- Constraints:
--
-- -   Can\'t manage the master user password with Amazon Web Services
--     Secrets Manager if @MasterUserPassword@ is specified.
restoreDBClusterFromS3_manageMasterUserPassword :: Lens.Lens' RestoreDBClusterFromS3 (Prelude.Maybe Prelude.Bool)
restoreDBClusterFromS3_manageMasterUserPassword = Lens.lens (\RestoreDBClusterFromS3' {manageMasterUserPassword} -> manageMasterUserPassword) (\s@RestoreDBClusterFromS3' {} a -> s {manageMasterUserPassword = a} :: RestoreDBClusterFromS3)

-- | The password for the master database user. This password can contain any
-- printable ASCII character except \"\/\", \"\"\", or \"\@\".
--
-- Constraints:
--
-- -   Must contain from 8 to 41 characters.
--
-- -   Can\'t be specified if @ManageMasterUserPassword@ is turned on.
restoreDBClusterFromS3_masterUserPassword :: Lens.Lens' RestoreDBClusterFromS3 (Prelude.Maybe Prelude.Text)
restoreDBClusterFromS3_masterUserPassword = Lens.lens (\RestoreDBClusterFromS3' {masterUserPassword} -> masterUserPassword) (\s@RestoreDBClusterFromS3' {} a -> s {masterUserPassword = a} :: RestoreDBClusterFromS3)

-- | The Amazon Web Services KMS key identifier to encrypt a secret that is
-- automatically generated and managed in Amazon Web Services Secrets
-- Manager.
--
-- This setting is valid only if the master user password is managed by RDS
-- in Amazon Web Services Secrets Manager for the DB cluster.
--
-- The Amazon Web Services KMS key identifier is the key ARN, key ID, alias
-- ARN, or alias name for the KMS key. To use a KMS key in a different
-- Amazon Web Services account, specify the key ARN or alias ARN.
--
-- If you don\'t specify @MasterUserSecretKmsKeyId@, then the
-- @aws\/secretsmanager@ KMS key is used to encrypt the secret. If the
-- secret is in a different Amazon Web Services account, then you can\'t
-- use the @aws\/secretsmanager@ KMS key to encrypt the secret, and you
-- must use a customer managed KMS key.
--
-- There is a default KMS key for your Amazon Web Services account. Your
-- Amazon Web Services account has a different default KMS key for each
-- Amazon Web Services Region.
restoreDBClusterFromS3_masterUserSecretKmsKeyId :: Lens.Lens' RestoreDBClusterFromS3 (Prelude.Maybe Prelude.Text)
restoreDBClusterFromS3_masterUserSecretKmsKeyId = Lens.lens (\RestoreDBClusterFromS3' {masterUserSecretKmsKeyId} -> masterUserSecretKmsKeyId) (\s@RestoreDBClusterFromS3' {} a -> s {masterUserSecretKmsKeyId = a} :: RestoreDBClusterFromS3)

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
restoreDBClusterFromS3_networkType :: Lens.Lens' RestoreDBClusterFromS3 (Prelude.Maybe Prelude.Text)
restoreDBClusterFromS3_networkType = Lens.lens (\RestoreDBClusterFromS3' {networkType} -> networkType) (\s@RestoreDBClusterFromS3' {} a -> s {networkType = a} :: RestoreDBClusterFromS3)

-- | A value that indicates that the restored DB cluster should be associated
-- with the specified option group.
--
-- Permanent options can\'t be removed from an option group. An option
-- group can\'t be removed from a DB cluster once it is associated with a
-- DB cluster.
restoreDBClusterFromS3_optionGroupName :: Lens.Lens' RestoreDBClusterFromS3 (Prelude.Maybe Prelude.Text)
restoreDBClusterFromS3_optionGroupName = Lens.lens (\RestoreDBClusterFromS3' {optionGroupName} -> optionGroupName) (\s@RestoreDBClusterFromS3' {} a -> s {optionGroupName = a} :: RestoreDBClusterFromS3)

-- | The port number on which the instances in the restored DB cluster accept
-- connections.
--
-- Default: @3306@
restoreDBClusterFromS3_port :: Lens.Lens' RestoreDBClusterFromS3 (Prelude.Maybe Prelude.Int)
restoreDBClusterFromS3_port = Lens.lens (\RestoreDBClusterFromS3' {port} -> port) (\s@RestoreDBClusterFromS3' {} a -> s {port = a} :: RestoreDBClusterFromS3)

-- | The daily time range during which automated backups are created if
-- automated backups are enabled using the @BackupRetentionPeriod@
-- parameter.
--
-- The default is a 30-minute window selected at random from an 8-hour
-- block of time for each Amazon Web Services Region. To view the time
-- blocks available, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/Aurora.Managing.Backups.html#Aurora.Managing.Backups.BackupWindow Backup window>
-- in the /Amazon Aurora User Guide/.
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
restoreDBClusterFromS3_preferredBackupWindow :: Lens.Lens' RestoreDBClusterFromS3 (Prelude.Maybe Prelude.Text)
restoreDBClusterFromS3_preferredBackupWindow = Lens.lens (\RestoreDBClusterFromS3' {preferredBackupWindow} -> preferredBackupWindow) (\s@RestoreDBClusterFromS3' {} a -> s {preferredBackupWindow = a} :: RestoreDBClusterFromS3)

-- | The weekly time range during which system maintenance can occur, in
-- Universal Coordinated Time (UTC).
--
-- Format: @ddd:hh24:mi-ddd:hh24:mi@
--
-- The default is a 30-minute window selected at random from an 8-hour
-- block of time for each Amazon Web Services Region, occurring on a random
-- day of the week. To see the time blocks available, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/USER_UpgradeDBInstance.Maintenance.html#AdjustingTheMaintenanceWindow.Aurora Adjusting the Preferred Maintenance Window>
-- in the /Amazon Aurora User Guide/.
--
-- Valid Days: Mon, Tue, Wed, Thu, Fri, Sat, Sun.
--
-- Constraints: Minimum 30-minute window.
restoreDBClusterFromS3_preferredMaintenanceWindow :: Lens.Lens' RestoreDBClusterFromS3 (Prelude.Maybe Prelude.Text)
restoreDBClusterFromS3_preferredMaintenanceWindow = Lens.lens (\RestoreDBClusterFromS3' {preferredMaintenanceWindow} -> preferredMaintenanceWindow) (\s@RestoreDBClusterFromS3' {} a -> s {preferredMaintenanceWindow = a} :: RestoreDBClusterFromS3)

-- | The prefix for all of the file names that contain the data used to
-- create the Amazon Aurora DB cluster. If you do not specify a
-- __SourceS3Prefix__ value, then the Amazon Aurora DB cluster is created
-- by using all of the files in the Amazon S3 bucket.
restoreDBClusterFromS3_s3Prefix :: Lens.Lens' RestoreDBClusterFromS3 (Prelude.Maybe Prelude.Text)
restoreDBClusterFromS3_s3Prefix = Lens.lens (\RestoreDBClusterFromS3' {s3Prefix} -> s3Prefix) (\s@RestoreDBClusterFromS3' {} a -> s {s3Prefix = a} :: RestoreDBClusterFromS3)

-- | Undocumented member.
restoreDBClusterFromS3_serverlessV2ScalingConfiguration :: Lens.Lens' RestoreDBClusterFromS3 (Prelude.Maybe ServerlessV2ScalingConfiguration)
restoreDBClusterFromS3_serverlessV2ScalingConfiguration = Lens.lens (\RestoreDBClusterFromS3' {serverlessV2ScalingConfiguration} -> serverlessV2ScalingConfiguration) (\s@RestoreDBClusterFromS3' {} a -> s {serverlessV2ScalingConfiguration = a} :: RestoreDBClusterFromS3)

-- | A value that indicates whether the restored DB cluster is encrypted.
restoreDBClusterFromS3_storageEncrypted :: Lens.Lens' RestoreDBClusterFromS3 (Prelude.Maybe Prelude.Bool)
restoreDBClusterFromS3_storageEncrypted = Lens.lens (\RestoreDBClusterFromS3' {storageEncrypted} -> storageEncrypted) (\s@RestoreDBClusterFromS3' {} a -> s {storageEncrypted = a} :: RestoreDBClusterFromS3)

-- | Specifies the storage type to be associated with the DB cluster.
--
-- Valid values: @aurora@, @aurora-iopt1@
--
-- Default: @aurora@
--
-- Valid for: Aurora DB clusters only
restoreDBClusterFromS3_storageType :: Lens.Lens' RestoreDBClusterFromS3 (Prelude.Maybe Prelude.Text)
restoreDBClusterFromS3_storageType = Lens.lens (\RestoreDBClusterFromS3' {storageType} -> storageType) (\s@RestoreDBClusterFromS3' {} a -> s {storageType = a} :: RestoreDBClusterFromS3)

-- | Undocumented member.
restoreDBClusterFromS3_tags :: Lens.Lens' RestoreDBClusterFromS3 (Prelude.Maybe [Tag])
restoreDBClusterFromS3_tags = Lens.lens (\RestoreDBClusterFromS3' {tags} -> tags) (\s@RestoreDBClusterFromS3' {} a -> s {tags = a} :: RestoreDBClusterFromS3) Prelude.. Lens.mapping Lens.coerced

-- | A list of EC2 VPC security groups to associate with the restored DB
-- cluster.
restoreDBClusterFromS3_vpcSecurityGroupIds :: Lens.Lens' RestoreDBClusterFromS3 (Prelude.Maybe [Prelude.Text])
restoreDBClusterFromS3_vpcSecurityGroupIds = Lens.lens (\RestoreDBClusterFromS3' {vpcSecurityGroupIds} -> vpcSecurityGroupIds) (\s@RestoreDBClusterFromS3' {} a -> s {vpcSecurityGroupIds = a} :: RestoreDBClusterFromS3) Prelude.. Lens.mapping Lens.coerced

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
restoreDBClusterFromS3_dbClusterIdentifier :: Lens.Lens' RestoreDBClusterFromS3 Prelude.Text
restoreDBClusterFromS3_dbClusterIdentifier = Lens.lens (\RestoreDBClusterFromS3' {dbClusterIdentifier} -> dbClusterIdentifier) (\s@RestoreDBClusterFromS3' {} a -> s {dbClusterIdentifier = a} :: RestoreDBClusterFromS3)

-- | The name of the database engine to be used for this DB cluster.
--
-- Valid Values: @aurora-mysql@ (for Aurora MySQL)
restoreDBClusterFromS3_engine :: Lens.Lens' RestoreDBClusterFromS3 Prelude.Text
restoreDBClusterFromS3_engine = Lens.lens (\RestoreDBClusterFromS3' {engine} -> engine) (\s@RestoreDBClusterFromS3' {} a -> s {engine = a} :: RestoreDBClusterFromS3)

-- | The name of the master user for the restored DB cluster.
--
-- Constraints:
--
-- -   Must be 1 to 16 letters or numbers.
--
-- -   First character must be a letter.
--
-- -   Can\'t be a reserved word for the chosen database engine.
restoreDBClusterFromS3_masterUsername :: Lens.Lens' RestoreDBClusterFromS3 Prelude.Text
restoreDBClusterFromS3_masterUsername = Lens.lens (\RestoreDBClusterFromS3' {masterUsername} -> masterUsername) (\s@RestoreDBClusterFromS3' {} a -> s {masterUsername = a} :: RestoreDBClusterFromS3)

-- | The identifier for the database engine that was backed up to create the
-- files stored in the Amazon S3 bucket.
--
-- Valid values: @mysql@
restoreDBClusterFromS3_sourceEngine :: Lens.Lens' RestoreDBClusterFromS3 Prelude.Text
restoreDBClusterFromS3_sourceEngine = Lens.lens (\RestoreDBClusterFromS3' {sourceEngine} -> sourceEngine) (\s@RestoreDBClusterFromS3' {} a -> s {sourceEngine = a} :: RestoreDBClusterFromS3)

-- | The version of the database that the backup files were created from.
--
-- MySQL versions 5.5, 5.6, and 5.7 are supported.
--
-- Example: @5.6.40@, @5.7.28@
restoreDBClusterFromS3_sourceEngineVersion :: Lens.Lens' RestoreDBClusterFromS3 Prelude.Text
restoreDBClusterFromS3_sourceEngineVersion = Lens.lens (\RestoreDBClusterFromS3' {sourceEngineVersion} -> sourceEngineVersion) (\s@RestoreDBClusterFromS3' {} a -> s {sourceEngineVersion = a} :: RestoreDBClusterFromS3)

-- | The name of the Amazon S3 bucket that contains the data used to create
-- the Amazon Aurora DB cluster.
restoreDBClusterFromS3_s3BucketName :: Lens.Lens' RestoreDBClusterFromS3 Prelude.Text
restoreDBClusterFromS3_s3BucketName = Lens.lens (\RestoreDBClusterFromS3' {s3BucketName} -> s3BucketName) (\s@RestoreDBClusterFromS3' {} a -> s {s3BucketName = a} :: RestoreDBClusterFromS3)

-- | The Amazon Resource Name (ARN) of the Amazon Web Services Identity and
-- Access Management (IAM) role that authorizes Amazon RDS to access the
-- Amazon S3 bucket on your behalf.
restoreDBClusterFromS3_s3IngestionRoleArn :: Lens.Lens' RestoreDBClusterFromS3 Prelude.Text
restoreDBClusterFromS3_s3IngestionRoleArn = Lens.lens (\RestoreDBClusterFromS3' {s3IngestionRoleArn} -> s3IngestionRoleArn) (\s@RestoreDBClusterFromS3' {} a -> s {s3IngestionRoleArn = a} :: RestoreDBClusterFromS3)

instance Core.AWSRequest RestoreDBClusterFromS3 where
  type
    AWSResponse RestoreDBClusterFromS3 =
      RestoreDBClusterFromS3Response
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "RestoreDBClusterFromS3Result"
      ( \s h x ->
          RestoreDBClusterFromS3Response'
            Prelude.<$> (x Data..@? "DBCluster")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable RestoreDBClusterFromS3 where
  hashWithSalt _salt RestoreDBClusterFromS3' {..} =
    _salt
      `Prelude.hashWithSalt` availabilityZones
      `Prelude.hashWithSalt` backtrackWindow
      `Prelude.hashWithSalt` backupRetentionPeriod
      `Prelude.hashWithSalt` characterSetName
      `Prelude.hashWithSalt` copyTagsToSnapshot
      `Prelude.hashWithSalt` dbClusterParameterGroupName
      `Prelude.hashWithSalt` dbSubnetGroupName
      `Prelude.hashWithSalt` databaseName
      `Prelude.hashWithSalt` deletionProtection
      `Prelude.hashWithSalt` domain
      `Prelude.hashWithSalt` domainIAMRoleName
      `Prelude.hashWithSalt` enableCloudwatchLogsExports
      `Prelude.hashWithSalt` enableIAMDatabaseAuthentication
      `Prelude.hashWithSalt` engineVersion
      `Prelude.hashWithSalt` kmsKeyId
      `Prelude.hashWithSalt` manageMasterUserPassword
      `Prelude.hashWithSalt` masterUserPassword
      `Prelude.hashWithSalt` masterUserSecretKmsKeyId
      `Prelude.hashWithSalt` networkType
      `Prelude.hashWithSalt` optionGroupName
      `Prelude.hashWithSalt` port
      `Prelude.hashWithSalt` preferredBackupWindow
      `Prelude.hashWithSalt` preferredMaintenanceWindow
      `Prelude.hashWithSalt` s3Prefix
      `Prelude.hashWithSalt` serverlessV2ScalingConfiguration
      `Prelude.hashWithSalt` storageEncrypted
      `Prelude.hashWithSalt` storageType
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` vpcSecurityGroupIds
      `Prelude.hashWithSalt` dbClusterIdentifier
      `Prelude.hashWithSalt` engine
      `Prelude.hashWithSalt` masterUsername
      `Prelude.hashWithSalt` sourceEngine
      `Prelude.hashWithSalt` sourceEngineVersion
      `Prelude.hashWithSalt` s3BucketName
      `Prelude.hashWithSalt` s3IngestionRoleArn

instance Prelude.NFData RestoreDBClusterFromS3 where
  rnf RestoreDBClusterFromS3' {..} =
    Prelude.rnf availabilityZones
      `Prelude.seq` Prelude.rnf backtrackWindow
      `Prelude.seq` Prelude.rnf backupRetentionPeriod
      `Prelude.seq` Prelude.rnf characterSetName
      `Prelude.seq` Prelude.rnf copyTagsToSnapshot
      `Prelude.seq` Prelude.rnf dbClusterParameterGroupName
      `Prelude.seq` Prelude.rnf dbSubnetGroupName
      `Prelude.seq` Prelude.rnf databaseName
      `Prelude.seq` Prelude.rnf deletionProtection
      `Prelude.seq` Prelude.rnf domain
      `Prelude.seq` Prelude.rnf domainIAMRoleName
      `Prelude.seq` Prelude.rnf enableCloudwatchLogsExports
      `Prelude.seq` Prelude.rnf enableIAMDatabaseAuthentication
      `Prelude.seq` Prelude.rnf engineVersion
      `Prelude.seq` Prelude.rnf kmsKeyId
      `Prelude.seq` Prelude.rnf manageMasterUserPassword
      `Prelude.seq` Prelude.rnf masterUserPassword
      `Prelude.seq` Prelude.rnf
        masterUserSecretKmsKeyId
      `Prelude.seq` Prelude.rnf networkType
      `Prelude.seq` Prelude.rnf optionGroupName
      `Prelude.seq` Prelude.rnf port
      `Prelude.seq` Prelude.rnf
        preferredBackupWindow
      `Prelude.seq` Prelude.rnf
        preferredMaintenanceWindow
      `Prelude.seq` Prelude.rnf s3Prefix
      `Prelude.seq` Prelude.rnf
        serverlessV2ScalingConfiguration
      `Prelude.seq` Prelude.rnf
        storageEncrypted
      `Prelude.seq` Prelude.rnf
        storageType
      `Prelude.seq` Prelude.rnf
        tags
      `Prelude.seq` Prelude.rnf
        vpcSecurityGroupIds
      `Prelude.seq` Prelude.rnf
        dbClusterIdentifier
      `Prelude.seq` Prelude.rnf
        engine
      `Prelude.seq` Prelude.rnf
        masterUsername
      `Prelude.seq` Prelude.rnf
        sourceEngine
      `Prelude.seq` Prelude.rnf
        sourceEngineVersion
      `Prelude.seq` Prelude.rnf
        s3BucketName
      `Prelude.seq` Prelude.rnf
        s3IngestionRoleArn

instance Data.ToHeaders RestoreDBClusterFromS3 where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath RestoreDBClusterFromS3 where
  toPath = Prelude.const "/"

instance Data.ToQuery RestoreDBClusterFromS3 where
  toQuery RestoreDBClusterFromS3' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("RestoreDBClusterFromS3" :: Prelude.ByteString),
        "Version"
          Data.=: ("2014-10-31" :: Prelude.ByteString),
        "AvailabilityZones"
          Data.=: Data.toQuery
            ( Data.toQueryList "AvailabilityZone"
                Prelude.<$> availabilityZones
            ),
        "BacktrackWindow" Data.=: backtrackWindow,
        "BackupRetentionPeriod"
          Data.=: backupRetentionPeriod,
        "CharacterSetName" Data.=: characterSetName,
        "CopyTagsToSnapshot" Data.=: copyTagsToSnapshot,
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
        "EngineVersion" Data.=: engineVersion,
        "KmsKeyId" Data.=: kmsKeyId,
        "ManageMasterUserPassword"
          Data.=: manageMasterUserPassword,
        "MasterUserPassword" Data.=: masterUserPassword,
        "MasterUserSecretKmsKeyId"
          Data.=: masterUserSecretKmsKeyId,
        "NetworkType" Data.=: networkType,
        "OptionGroupName" Data.=: optionGroupName,
        "Port" Data.=: port,
        "PreferredBackupWindow"
          Data.=: preferredBackupWindow,
        "PreferredMaintenanceWindow"
          Data.=: preferredMaintenanceWindow,
        "S3Prefix" Data.=: s3Prefix,
        "ServerlessV2ScalingConfiguration"
          Data.=: serverlessV2ScalingConfiguration,
        "StorageEncrypted" Data.=: storageEncrypted,
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
        "Engine" Data.=: engine,
        "MasterUsername" Data.=: masterUsername,
        "SourceEngine" Data.=: sourceEngine,
        "SourceEngineVersion" Data.=: sourceEngineVersion,
        "S3BucketName" Data.=: s3BucketName,
        "S3IngestionRoleArn" Data.=: s3IngestionRoleArn
      ]

-- | /See:/ 'newRestoreDBClusterFromS3Response' smart constructor.
data RestoreDBClusterFromS3Response = RestoreDBClusterFromS3Response'
  { dbCluster :: Prelude.Maybe DBCluster,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RestoreDBClusterFromS3Response' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dbCluster', 'restoreDBClusterFromS3Response_dbCluster' - Undocumented member.
--
-- 'httpStatus', 'restoreDBClusterFromS3Response_httpStatus' - The response's http status code.
newRestoreDBClusterFromS3Response ::
  -- | 'httpStatus'
  Prelude.Int ->
  RestoreDBClusterFromS3Response
newRestoreDBClusterFromS3Response pHttpStatus_ =
  RestoreDBClusterFromS3Response'
    { dbCluster =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
restoreDBClusterFromS3Response_dbCluster :: Lens.Lens' RestoreDBClusterFromS3Response (Prelude.Maybe DBCluster)
restoreDBClusterFromS3Response_dbCluster = Lens.lens (\RestoreDBClusterFromS3Response' {dbCluster} -> dbCluster) (\s@RestoreDBClusterFromS3Response' {} a -> s {dbCluster = a} :: RestoreDBClusterFromS3Response)

-- | The response's http status code.
restoreDBClusterFromS3Response_httpStatus :: Lens.Lens' RestoreDBClusterFromS3Response Prelude.Int
restoreDBClusterFromS3Response_httpStatus = Lens.lens (\RestoreDBClusterFromS3Response' {httpStatus} -> httpStatus) (\s@RestoreDBClusterFromS3Response' {} a -> s {httpStatus = a} :: RestoreDBClusterFromS3Response)

instance
  Prelude.NFData
    RestoreDBClusterFromS3Response
  where
  rnf RestoreDBClusterFromS3Response' {..} =
    Prelude.rnf dbCluster
      `Prelude.seq` Prelude.rnf httpStatus
