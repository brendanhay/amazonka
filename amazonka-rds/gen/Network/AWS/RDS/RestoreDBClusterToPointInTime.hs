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
-- Module      : Network.AWS.RDS.RestoreDBClusterToPointInTime
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Restores a DB cluster to an arbitrary point in time. Users can restore
-- to any point in time before @LatestRestorableTime@ for up to
-- @BackupRetentionPeriod@ days. The target DB cluster is created from the
-- source DB cluster with the same configuration as the original DB
-- cluster, except that the new DB cluster is created with the default DB
-- security group.
--
-- This action only restores the DB cluster, not the DB instances for that
-- DB cluster. You must invoke the @CreateDBInstance@ action to create DB
-- instances for the restored DB cluster, specifying the identifier of the
-- restored DB cluster in @DBClusterIdentifier@. You can create DB
-- instances only after the @RestoreDBClusterToPointInTime@ action has
-- completed and the DB cluster is available.
--
-- For more information on Amazon Aurora, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/CHAP_AuroraOverview.html What Is Amazon Aurora?>
-- in the /Amazon Aurora User Guide./
--
-- This action only applies to Aurora DB clusters.
module Network.AWS.RDS.RestoreDBClusterToPointInTime
  ( -- * Creating a Request
    RestoreDBClusterToPointInTime (..),
    newRestoreDBClusterToPointInTime,

    -- * Request Lenses
    restoreDBClusterToPointInTime_deletionProtection,
    restoreDBClusterToPointInTime_enableIAMDatabaseAuthentication,
    restoreDBClusterToPointInTime_enableCloudwatchLogsExports,
    restoreDBClusterToPointInTime_optionGroupName,
    restoreDBClusterToPointInTime_restoreType,
    restoreDBClusterToPointInTime_domain,
    restoreDBClusterToPointInTime_dbSubnetGroupName,
    restoreDBClusterToPointInTime_vpcSecurityGroupIds,
    restoreDBClusterToPointInTime_kmsKeyId,
    restoreDBClusterToPointInTime_tags,
    restoreDBClusterToPointInTime_port,
    restoreDBClusterToPointInTime_domainIAMRoleName,
    restoreDBClusterToPointInTime_restoreToTime,
    restoreDBClusterToPointInTime_copyTagsToSnapshot,
    restoreDBClusterToPointInTime_backtrackWindow,
    restoreDBClusterToPointInTime_dbClusterParameterGroupName,
    restoreDBClusterToPointInTime_useLatestRestorableTime,
    restoreDBClusterToPointInTime_dbClusterIdentifier,
    restoreDBClusterToPointInTime_sourceDBClusterIdentifier,

    -- * Destructuring the Response
    RestoreDBClusterToPointInTimeResponse (..),
    newRestoreDBClusterToPointInTimeResponse,

    -- * Response Lenses
    restoreDBClusterToPointInTimeResponse_dbCluster,
    restoreDBClusterToPointInTimeResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.RDS.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'newRestoreDBClusterToPointInTime' smart constructor.
data RestoreDBClusterToPointInTime = RestoreDBClusterToPointInTime'
  { -- | A value that indicates whether the DB cluster has deletion protection
    -- enabled. The database can\'t be deleted when deletion protection is
    -- enabled. By default, deletion protection is disabled.
    deletionProtection :: Core.Maybe Core.Bool,
    -- | A value that indicates whether to enable mapping of AWS Identity and
    -- Access Management (IAM) accounts to database accounts. By default,
    -- mapping is disabled.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/UsingWithRDS.IAMDBAuth.html IAM Database Authentication>
    -- in the /Amazon Aurora User Guide./
    enableIAMDatabaseAuthentication :: Core.Maybe Core.Bool,
    -- | The list of logs that the restored DB cluster is to export to CloudWatch
    -- Logs. The values in the list depend on the DB engine being used. For
    -- more information, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/USER_LogAccess.html#USER_LogAccess.Procedural.UploadtoCloudWatch Publishing Database Logs to Amazon CloudWatch Logs>
    -- in the /Amazon Aurora User Guide/.
    enableCloudwatchLogsExports :: Core.Maybe [Core.Text],
    -- | The name of the option group for the new DB cluster.
    optionGroupName :: Core.Maybe Core.Text,
    -- | The type of restore to be performed. You can specify one of the
    -- following values:
    --
    -- -   @full-copy@ - The new DB cluster is restored as a full copy of the
    --     source DB cluster.
    --
    -- -   @copy-on-write@ - The new DB cluster is restored as a clone of the
    --     source DB cluster.
    --
    -- Constraints: You can\'t specify @copy-on-write@ if the engine version of
    -- the source DB cluster is earlier than 1.11.
    --
    -- If you don\'t specify a @RestoreType@ value, then the new DB cluster is
    -- restored as a full copy of the source DB cluster.
    restoreType :: Core.Maybe Core.Text,
    -- | Specify the Active Directory directory ID to restore the DB cluster in.
    -- The domain must be created prior to this operation.
    --
    -- For Amazon Aurora DB clusters, Amazon RDS can use Kerberos
    -- Authentication to authenticate users that connect to the DB cluster. For
    -- more information, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/kerberos-authentication.html Kerberos Authentication>
    -- in the /Amazon Aurora User Guide/.
    domain :: Core.Maybe Core.Text,
    -- | The DB subnet group name to use for the new DB cluster.
    --
    -- Constraints: If supplied, must match the name of an existing
    -- DBSubnetGroup.
    --
    -- Example: @mySubnetgroup@
    dbSubnetGroupName :: Core.Maybe Core.Text,
    -- | A list of VPC security groups that the new DB cluster belongs to.
    vpcSecurityGroupIds :: Core.Maybe [Core.Text],
    -- | The AWS KMS key identifier to use when restoring an encrypted DB cluster
    -- from an encrypted DB cluster.
    --
    -- The AWS KMS key identifier is the key ARN, key ID, alias ARN, or alias
    -- name for the AWS KMS customer master key (CMK). To use a CMK in a
    -- different AWS account, specify the key ARN or alias ARN.
    --
    -- You can restore to a new DB cluster and encrypt the new DB cluster with
    -- a AWS KMS CMK that is different than the AWS KMS key used to encrypt the
    -- source DB cluster. The new DB cluster is encrypted with the AWS KMS CMK
    -- identified by the @KmsKeyId@ parameter.
    --
    -- If you don\'t specify a value for the @KmsKeyId@ parameter, then the
    -- following occurs:
    --
    -- -   If the DB cluster is encrypted, then the restored DB cluster is
    --     encrypted using the AWS KMS CMK that was used to encrypt the source
    --     DB cluster.
    --
    -- -   If the DB cluster isn\'t encrypted, then the restored DB cluster
    --     isn\'t encrypted.
    --
    -- If @DBClusterIdentifier@ refers to a DB cluster that isn\'t encrypted,
    -- then the restore request is rejected.
    kmsKeyId :: Core.Maybe Core.Text,
    tags :: Core.Maybe [Tag],
    -- | The port number on which the new DB cluster accepts connections.
    --
    -- Constraints: A value from @1150-65535@.
    --
    -- Default: The default port for the engine.
    port :: Core.Maybe Core.Int,
    -- | Specify the name of the IAM role to be used when making API calls to the
    -- Directory Service.
    domainIAMRoleName :: Core.Maybe Core.Text,
    -- | The date and time to restore the DB cluster to.
    --
    -- Valid Values: Value must be a time in Universal Coordinated Time (UTC)
    -- format
    --
    -- Constraints:
    --
    -- -   Must be before the latest restorable time for the DB instance
    --
    -- -   Must be specified if @UseLatestRestorableTime@ parameter isn\'t
    --     provided
    --
    -- -   Can\'t be specified if the @UseLatestRestorableTime@ parameter is
    --     enabled
    --
    -- -   Can\'t be specified if the @RestoreType@ parameter is
    --     @copy-on-write@
    --
    -- Example: @2015-03-07T23:45:00Z@
    restoreToTime :: Core.Maybe Core.ISO8601,
    -- | A value that indicates whether to copy all tags from the restored DB
    -- cluster to snapshots of the restored DB cluster. The default is not to
    -- copy them.
    copyTagsToSnapshot :: Core.Maybe Core.Bool,
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
    backtrackWindow :: Core.Maybe Core.Integer,
    -- | The name of the DB cluster parameter group to associate with this DB
    -- cluster. If this argument is omitted, the default DB cluster parameter
    -- group for the specified engine is used.
    --
    -- Constraints:
    --
    -- -   If supplied, must match the name of an existing DB cluster parameter
    --     group.
    --
    -- -   Must be 1 to 255 letters, numbers, or hyphens.
    --
    -- -   First character must be a letter.
    --
    -- -   Can\'t end with a hyphen or contain two consecutive hyphens.
    dbClusterParameterGroupName :: Core.Maybe Core.Text,
    -- | A value that indicates whether to restore the DB cluster to the latest
    -- restorable backup time. By default, the DB cluster isn\'t restored to
    -- the latest restorable backup time.
    --
    -- Constraints: Can\'t be specified if @RestoreToTime@ parameter is
    -- provided.
    useLatestRestorableTime :: Core.Maybe Core.Bool,
    -- | The name of the new DB cluster to be created.
    --
    -- Constraints:
    --
    -- -   Must contain from 1 to 63 letters, numbers, or hyphens
    --
    -- -   First character must be a letter
    --
    -- -   Can\'t end with a hyphen or contain two consecutive hyphens
    dbClusterIdentifier :: Core.Text,
    -- | The identifier of the source DB cluster from which to restore.
    --
    -- Constraints:
    --
    -- -   Must match the identifier of an existing DBCluster.
    sourceDBClusterIdentifier :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'RestoreDBClusterToPointInTime' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deletionProtection', 'restoreDBClusterToPointInTime_deletionProtection' - A value that indicates whether the DB cluster has deletion protection
-- enabled. The database can\'t be deleted when deletion protection is
-- enabled. By default, deletion protection is disabled.
--
-- 'enableIAMDatabaseAuthentication', 'restoreDBClusterToPointInTime_enableIAMDatabaseAuthentication' - A value that indicates whether to enable mapping of AWS Identity and
-- Access Management (IAM) accounts to database accounts. By default,
-- mapping is disabled.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/UsingWithRDS.IAMDBAuth.html IAM Database Authentication>
-- in the /Amazon Aurora User Guide./
--
-- 'enableCloudwatchLogsExports', 'restoreDBClusterToPointInTime_enableCloudwatchLogsExports' - The list of logs that the restored DB cluster is to export to CloudWatch
-- Logs. The values in the list depend on the DB engine being used. For
-- more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/USER_LogAccess.html#USER_LogAccess.Procedural.UploadtoCloudWatch Publishing Database Logs to Amazon CloudWatch Logs>
-- in the /Amazon Aurora User Guide/.
--
-- 'optionGroupName', 'restoreDBClusterToPointInTime_optionGroupName' - The name of the option group for the new DB cluster.
--
-- 'restoreType', 'restoreDBClusterToPointInTime_restoreType' - The type of restore to be performed. You can specify one of the
-- following values:
--
-- -   @full-copy@ - The new DB cluster is restored as a full copy of the
--     source DB cluster.
--
-- -   @copy-on-write@ - The new DB cluster is restored as a clone of the
--     source DB cluster.
--
-- Constraints: You can\'t specify @copy-on-write@ if the engine version of
-- the source DB cluster is earlier than 1.11.
--
-- If you don\'t specify a @RestoreType@ value, then the new DB cluster is
-- restored as a full copy of the source DB cluster.
--
-- 'domain', 'restoreDBClusterToPointInTime_domain' - Specify the Active Directory directory ID to restore the DB cluster in.
-- The domain must be created prior to this operation.
--
-- For Amazon Aurora DB clusters, Amazon RDS can use Kerberos
-- Authentication to authenticate users that connect to the DB cluster. For
-- more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/kerberos-authentication.html Kerberos Authentication>
-- in the /Amazon Aurora User Guide/.
--
-- 'dbSubnetGroupName', 'restoreDBClusterToPointInTime_dbSubnetGroupName' - The DB subnet group name to use for the new DB cluster.
--
-- Constraints: If supplied, must match the name of an existing
-- DBSubnetGroup.
--
-- Example: @mySubnetgroup@
--
-- 'vpcSecurityGroupIds', 'restoreDBClusterToPointInTime_vpcSecurityGroupIds' - A list of VPC security groups that the new DB cluster belongs to.
--
-- 'kmsKeyId', 'restoreDBClusterToPointInTime_kmsKeyId' - The AWS KMS key identifier to use when restoring an encrypted DB cluster
-- from an encrypted DB cluster.
--
-- The AWS KMS key identifier is the key ARN, key ID, alias ARN, or alias
-- name for the AWS KMS customer master key (CMK). To use a CMK in a
-- different AWS account, specify the key ARN or alias ARN.
--
-- You can restore to a new DB cluster and encrypt the new DB cluster with
-- a AWS KMS CMK that is different than the AWS KMS key used to encrypt the
-- source DB cluster. The new DB cluster is encrypted with the AWS KMS CMK
-- identified by the @KmsKeyId@ parameter.
--
-- If you don\'t specify a value for the @KmsKeyId@ parameter, then the
-- following occurs:
--
-- -   If the DB cluster is encrypted, then the restored DB cluster is
--     encrypted using the AWS KMS CMK that was used to encrypt the source
--     DB cluster.
--
-- -   If the DB cluster isn\'t encrypted, then the restored DB cluster
--     isn\'t encrypted.
--
-- If @DBClusterIdentifier@ refers to a DB cluster that isn\'t encrypted,
-- then the restore request is rejected.
--
-- 'tags', 'restoreDBClusterToPointInTime_tags' - Undocumented member.
--
-- 'port', 'restoreDBClusterToPointInTime_port' - The port number on which the new DB cluster accepts connections.
--
-- Constraints: A value from @1150-65535@.
--
-- Default: The default port for the engine.
--
-- 'domainIAMRoleName', 'restoreDBClusterToPointInTime_domainIAMRoleName' - Specify the name of the IAM role to be used when making API calls to the
-- Directory Service.
--
-- 'restoreToTime', 'restoreDBClusterToPointInTime_restoreToTime' - The date and time to restore the DB cluster to.
--
-- Valid Values: Value must be a time in Universal Coordinated Time (UTC)
-- format
--
-- Constraints:
--
-- -   Must be before the latest restorable time for the DB instance
--
-- -   Must be specified if @UseLatestRestorableTime@ parameter isn\'t
--     provided
--
-- -   Can\'t be specified if the @UseLatestRestorableTime@ parameter is
--     enabled
--
-- -   Can\'t be specified if the @RestoreType@ parameter is
--     @copy-on-write@
--
-- Example: @2015-03-07T23:45:00Z@
--
-- 'copyTagsToSnapshot', 'restoreDBClusterToPointInTime_copyTagsToSnapshot' - A value that indicates whether to copy all tags from the restored DB
-- cluster to snapshots of the restored DB cluster. The default is not to
-- copy them.
--
-- 'backtrackWindow', 'restoreDBClusterToPointInTime_backtrackWindow' - The target backtrack window, in seconds. To disable backtracking, set
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
-- 'dbClusterParameterGroupName', 'restoreDBClusterToPointInTime_dbClusterParameterGroupName' - The name of the DB cluster parameter group to associate with this DB
-- cluster. If this argument is omitted, the default DB cluster parameter
-- group for the specified engine is used.
--
-- Constraints:
--
-- -   If supplied, must match the name of an existing DB cluster parameter
--     group.
--
-- -   Must be 1 to 255 letters, numbers, or hyphens.
--
-- -   First character must be a letter.
--
-- -   Can\'t end with a hyphen or contain two consecutive hyphens.
--
-- 'useLatestRestorableTime', 'restoreDBClusterToPointInTime_useLatestRestorableTime' - A value that indicates whether to restore the DB cluster to the latest
-- restorable backup time. By default, the DB cluster isn\'t restored to
-- the latest restorable backup time.
--
-- Constraints: Can\'t be specified if @RestoreToTime@ parameter is
-- provided.
--
-- 'dbClusterIdentifier', 'restoreDBClusterToPointInTime_dbClusterIdentifier' - The name of the new DB cluster to be created.
--
-- Constraints:
--
-- -   Must contain from 1 to 63 letters, numbers, or hyphens
--
-- -   First character must be a letter
--
-- -   Can\'t end with a hyphen or contain two consecutive hyphens
--
-- 'sourceDBClusterIdentifier', 'restoreDBClusterToPointInTime_sourceDBClusterIdentifier' - The identifier of the source DB cluster from which to restore.
--
-- Constraints:
--
-- -   Must match the identifier of an existing DBCluster.
newRestoreDBClusterToPointInTime ::
  -- | 'dbClusterIdentifier'
  Core.Text ->
  -- | 'sourceDBClusterIdentifier'
  Core.Text ->
  RestoreDBClusterToPointInTime
newRestoreDBClusterToPointInTime
  pDBClusterIdentifier_
  pSourceDBClusterIdentifier_ =
    RestoreDBClusterToPointInTime'
      { deletionProtection =
          Core.Nothing,
        enableIAMDatabaseAuthentication =
          Core.Nothing,
        enableCloudwatchLogsExports = Core.Nothing,
        optionGroupName = Core.Nothing,
        restoreType = Core.Nothing,
        domain = Core.Nothing,
        dbSubnetGroupName = Core.Nothing,
        vpcSecurityGroupIds = Core.Nothing,
        kmsKeyId = Core.Nothing,
        tags = Core.Nothing,
        port = Core.Nothing,
        domainIAMRoleName = Core.Nothing,
        restoreToTime = Core.Nothing,
        copyTagsToSnapshot = Core.Nothing,
        backtrackWindow = Core.Nothing,
        dbClusterParameterGroupName = Core.Nothing,
        useLatestRestorableTime = Core.Nothing,
        dbClusterIdentifier = pDBClusterIdentifier_,
        sourceDBClusterIdentifier =
          pSourceDBClusterIdentifier_
      }

-- | A value that indicates whether the DB cluster has deletion protection
-- enabled. The database can\'t be deleted when deletion protection is
-- enabled. By default, deletion protection is disabled.
restoreDBClusterToPointInTime_deletionProtection :: Lens.Lens' RestoreDBClusterToPointInTime (Core.Maybe Core.Bool)
restoreDBClusterToPointInTime_deletionProtection = Lens.lens (\RestoreDBClusterToPointInTime' {deletionProtection} -> deletionProtection) (\s@RestoreDBClusterToPointInTime' {} a -> s {deletionProtection = a} :: RestoreDBClusterToPointInTime)

-- | A value that indicates whether to enable mapping of AWS Identity and
-- Access Management (IAM) accounts to database accounts. By default,
-- mapping is disabled.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/UsingWithRDS.IAMDBAuth.html IAM Database Authentication>
-- in the /Amazon Aurora User Guide./
restoreDBClusterToPointInTime_enableIAMDatabaseAuthentication :: Lens.Lens' RestoreDBClusterToPointInTime (Core.Maybe Core.Bool)
restoreDBClusterToPointInTime_enableIAMDatabaseAuthentication = Lens.lens (\RestoreDBClusterToPointInTime' {enableIAMDatabaseAuthentication} -> enableIAMDatabaseAuthentication) (\s@RestoreDBClusterToPointInTime' {} a -> s {enableIAMDatabaseAuthentication = a} :: RestoreDBClusterToPointInTime)

-- | The list of logs that the restored DB cluster is to export to CloudWatch
-- Logs. The values in the list depend on the DB engine being used. For
-- more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/USER_LogAccess.html#USER_LogAccess.Procedural.UploadtoCloudWatch Publishing Database Logs to Amazon CloudWatch Logs>
-- in the /Amazon Aurora User Guide/.
restoreDBClusterToPointInTime_enableCloudwatchLogsExports :: Lens.Lens' RestoreDBClusterToPointInTime (Core.Maybe [Core.Text])
restoreDBClusterToPointInTime_enableCloudwatchLogsExports = Lens.lens (\RestoreDBClusterToPointInTime' {enableCloudwatchLogsExports} -> enableCloudwatchLogsExports) (\s@RestoreDBClusterToPointInTime' {} a -> s {enableCloudwatchLogsExports = a} :: RestoreDBClusterToPointInTime) Core.. Lens.mapping Lens._Coerce

-- | The name of the option group for the new DB cluster.
restoreDBClusterToPointInTime_optionGroupName :: Lens.Lens' RestoreDBClusterToPointInTime (Core.Maybe Core.Text)
restoreDBClusterToPointInTime_optionGroupName = Lens.lens (\RestoreDBClusterToPointInTime' {optionGroupName} -> optionGroupName) (\s@RestoreDBClusterToPointInTime' {} a -> s {optionGroupName = a} :: RestoreDBClusterToPointInTime)

-- | The type of restore to be performed. You can specify one of the
-- following values:
--
-- -   @full-copy@ - The new DB cluster is restored as a full copy of the
--     source DB cluster.
--
-- -   @copy-on-write@ - The new DB cluster is restored as a clone of the
--     source DB cluster.
--
-- Constraints: You can\'t specify @copy-on-write@ if the engine version of
-- the source DB cluster is earlier than 1.11.
--
-- If you don\'t specify a @RestoreType@ value, then the new DB cluster is
-- restored as a full copy of the source DB cluster.
restoreDBClusterToPointInTime_restoreType :: Lens.Lens' RestoreDBClusterToPointInTime (Core.Maybe Core.Text)
restoreDBClusterToPointInTime_restoreType = Lens.lens (\RestoreDBClusterToPointInTime' {restoreType} -> restoreType) (\s@RestoreDBClusterToPointInTime' {} a -> s {restoreType = a} :: RestoreDBClusterToPointInTime)

-- | Specify the Active Directory directory ID to restore the DB cluster in.
-- The domain must be created prior to this operation.
--
-- For Amazon Aurora DB clusters, Amazon RDS can use Kerberos
-- Authentication to authenticate users that connect to the DB cluster. For
-- more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/kerberos-authentication.html Kerberos Authentication>
-- in the /Amazon Aurora User Guide/.
restoreDBClusterToPointInTime_domain :: Lens.Lens' RestoreDBClusterToPointInTime (Core.Maybe Core.Text)
restoreDBClusterToPointInTime_domain = Lens.lens (\RestoreDBClusterToPointInTime' {domain} -> domain) (\s@RestoreDBClusterToPointInTime' {} a -> s {domain = a} :: RestoreDBClusterToPointInTime)

-- | The DB subnet group name to use for the new DB cluster.
--
-- Constraints: If supplied, must match the name of an existing
-- DBSubnetGroup.
--
-- Example: @mySubnetgroup@
restoreDBClusterToPointInTime_dbSubnetGroupName :: Lens.Lens' RestoreDBClusterToPointInTime (Core.Maybe Core.Text)
restoreDBClusterToPointInTime_dbSubnetGroupName = Lens.lens (\RestoreDBClusterToPointInTime' {dbSubnetGroupName} -> dbSubnetGroupName) (\s@RestoreDBClusterToPointInTime' {} a -> s {dbSubnetGroupName = a} :: RestoreDBClusterToPointInTime)

-- | A list of VPC security groups that the new DB cluster belongs to.
restoreDBClusterToPointInTime_vpcSecurityGroupIds :: Lens.Lens' RestoreDBClusterToPointInTime (Core.Maybe [Core.Text])
restoreDBClusterToPointInTime_vpcSecurityGroupIds = Lens.lens (\RestoreDBClusterToPointInTime' {vpcSecurityGroupIds} -> vpcSecurityGroupIds) (\s@RestoreDBClusterToPointInTime' {} a -> s {vpcSecurityGroupIds = a} :: RestoreDBClusterToPointInTime) Core.. Lens.mapping Lens._Coerce

-- | The AWS KMS key identifier to use when restoring an encrypted DB cluster
-- from an encrypted DB cluster.
--
-- The AWS KMS key identifier is the key ARN, key ID, alias ARN, or alias
-- name for the AWS KMS customer master key (CMK). To use a CMK in a
-- different AWS account, specify the key ARN or alias ARN.
--
-- You can restore to a new DB cluster and encrypt the new DB cluster with
-- a AWS KMS CMK that is different than the AWS KMS key used to encrypt the
-- source DB cluster. The new DB cluster is encrypted with the AWS KMS CMK
-- identified by the @KmsKeyId@ parameter.
--
-- If you don\'t specify a value for the @KmsKeyId@ parameter, then the
-- following occurs:
--
-- -   If the DB cluster is encrypted, then the restored DB cluster is
--     encrypted using the AWS KMS CMK that was used to encrypt the source
--     DB cluster.
--
-- -   If the DB cluster isn\'t encrypted, then the restored DB cluster
--     isn\'t encrypted.
--
-- If @DBClusterIdentifier@ refers to a DB cluster that isn\'t encrypted,
-- then the restore request is rejected.
restoreDBClusterToPointInTime_kmsKeyId :: Lens.Lens' RestoreDBClusterToPointInTime (Core.Maybe Core.Text)
restoreDBClusterToPointInTime_kmsKeyId = Lens.lens (\RestoreDBClusterToPointInTime' {kmsKeyId} -> kmsKeyId) (\s@RestoreDBClusterToPointInTime' {} a -> s {kmsKeyId = a} :: RestoreDBClusterToPointInTime)

-- | Undocumented member.
restoreDBClusterToPointInTime_tags :: Lens.Lens' RestoreDBClusterToPointInTime (Core.Maybe [Tag])
restoreDBClusterToPointInTime_tags = Lens.lens (\RestoreDBClusterToPointInTime' {tags} -> tags) (\s@RestoreDBClusterToPointInTime' {} a -> s {tags = a} :: RestoreDBClusterToPointInTime) Core.. Lens.mapping Lens._Coerce

-- | The port number on which the new DB cluster accepts connections.
--
-- Constraints: A value from @1150-65535@.
--
-- Default: The default port for the engine.
restoreDBClusterToPointInTime_port :: Lens.Lens' RestoreDBClusterToPointInTime (Core.Maybe Core.Int)
restoreDBClusterToPointInTime_port = Lens.lens (\RestoreDBClusterToPointInTime' {port} -> port) (\s@RestoreDBClusterToPointInTime' {} a -> s {port = a} :: RestoreDBClusterToPointInTime)

-- | Specify the name of the IAM role to be used when making API calls to the
-- Directory Service.
restoreDBClusterToPointInTime_domainIAMRoleName :: Lens.Lens' RestoreDBClusterToPointInTime (Core.Maybe Core.Text)
restoreDBClusterToPointInTime_domainIAMRoleName = Lens.lens (\RestoreDBClusterToPointInTime' {domainIAMRoleName} -> domainIAMRoleName) (\s@RestoreDBClusterToPointInTime' {} a -> s {domainIAMRoleName = a} :: RestoreDBClusterToPointInTime)

-- | The date and time to restore the DB cluster to.
--
-- Valid Values: Value must be a time in Universal Coordinated Time (UTC)
-- format
--
-- Constraints:
--
-- -   Must be before the latest restorable time for the DB instance
--
-- -   Must be specified if @UseLatestRestorableTime@ parameter isn\'t
--     provided
--
-- -   Can\'t be specified if the @UseLatestRestorableTime@ parameter is
--     enabled
--
-- -   Can\'t be specified if the @RestoreType@ parameter is
--     @copy-on-write@
--
-- Example: @2015-03-07T23:45:00Z@
restoreDBClusterToPointInTime_restoreToTime :: Lens.Lens' RestoreDBClusterToPointInTime (Core.Maybe Core.UTCTime)
restoreDBClusterToPointInTime_restoreToTime = Lens.lens (\RestoreDBClusterToPointInTime' {restoreToTime} -> restoreToTime) (\s@RestoreDBClusterToPointInTime' {} a -> s {restoreToTime = a} :: RestoreDBClusterToPointInTime) Core.. Lens.mapping Core._Time

-- | A value that indicates whether to copy all tags from the restored DB
-- cluster to snapshots of the restored DB cluster. The default is not to
-- copy them.
restoreDBClusterToPointInTime_copyTagsToSnapshot :: Lens.Lens' RestoreDBClusterToPointInTime (Core.Maybe Core.Bool)
restoreDBClusterToPointInTime_copyTagsToSnapshot = Lens.lens (\RestoreDBClusterToPointInTime' {copyTagsToSnapshot} -> copyTagsToSnapshot) (\s@RestoreDBClusterToPointInTime' {} a -> s {copyTagsToSnapshot = a} :: RestoreDBClusterToPointInTime)

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
restoreDBClusterToPointInTime_backtrackWindow :: Lens.Lens' RestoreDBClusterToPointInTime (Core.Maybe Core.Integer)
restoreDBClusterToPointInTime_backtrackWindow = Lens.lens (\RestoreDBClusterToPointInTime' {backtrackWindow} -> backtrackWindow) (\s@RestoreDBClusterToPointInTime' {} a -> s {backtrackWindow = a} :: RestoreDBClusterToPointInTime)

-- | The name of the DB cluster parameter group to associate with this DB
-- cluster. If this argument is omitted, the default DB cluster parameter
-- group for the specified engine is used.
--
-- Constraints:
--
-- -   If supplied, must match the name of an existing DB cluster parameter
--     group.
--
-- -   Must be 1 to 255 letters, numbers, or hyphens.
--
-- -   First character must be a letter.
--
-- -   Can\'t end with a hyphen or contain two consecutive hyphens.
restoreDBClusterToPointInTime_dbClusterParameterGroupName :: Lens.Lens' RestoreDBClusterToPointInTime (Core.Maybe Core.Text)
restoreDBClusterToPointInTime_dbClusterParameterGroupName = Lens.lens (\RestoreDBClusterToPointInTime' {dbClusterParameterGroupName} -> dbClusterParameterGroupName) (\s@RestoreDBClusterToPointInTime' {} a -> s {dbClusterParameterGroupName = a} :: RestoreDBClusterToPointInTime)

-- | A value that indicates whether to restore the DB cluster to the latest
-- restorable backup time. By default, the DB cluster isn\'t restored to
-- the latest restorable backup time.
--
-- Constraints: Can\'t be specified if @RestoreToTime@ parameter is
-- provided.
restoreDBClusterToPointInTime_useLatestRestorableTime :: Lens.Lens' RestoreDBClusterToPointInTime (Core.Maybe Core.Bool)
restoreDBClusterToPointInTime_useLatestRestorableTime = Lens.lens (\RestoreDBClusterToPointInTime' {useLatestRestorableTime} -> useLatestRestorableTime) (\s@RestoreDBClusterToPointInTime' {} a -> s {useLatestRestorableTime = a} :: RestoreDBClusterToPointInTime)

-- | The name of the new DB cluster to be created.
--
-- Constraints:
--
-- -   Must contain from 1 to 63 letters, numbers, or hyphens
--
-- -   First character must be a letter
--
-- -   Can\'t end with a hyphen or contain two consecutive hyphens
restoreDBClusterToPointInTime_dbClusterIdentifier :: Lens.Lens' RestoreDBClusterToPointInTime Core.Text
restoreDBClusterToPointInTime_dbClusterIdentifier = Lens.lens (\RestoreDBClusterToPointInTime' {dbClusterIdentifier} -> dbClusterIdentifier) (\s@RestoreDBClusterToPointInTime' {} a -> s {dbClusterIdentifier = a} :: RestoreDBClusterToPointInTime)

-- | The identifier of the source DB cluster from which to restore.
--
-- Constraints:
--
-- -   Must match the identifier of an existing DBCluster.
restoreDBClusterToPointInTime_sourceDBClusterIdentifier :: Lens.Lens' RestoreDBClusterToPointInTime Core.Text
restoreDBClusterToPointInTime_sourceDBClusterIdentifier = Lens.lens (\RestoreDBClusterToPointInTime' {sourceDBClusterIdentifier} -> sourceDBClusterIdentifier) (\s@RestoreDBClusterToPointInTime' {} a -> s {sourceDBClusterIdentifier = a} :: RestoreDBClusterToPointInTime)

instance
  Core.AWSRequest
    RestoreDBClusterToPointInTime
  where
  type
    AWSResponse RestoreDBClusterToPointInTime =
      RestoreDBClusterToPointInTimeResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "RestoreDBClusterToPointInTimeResult"
      ( \s h x ->
          RestoreDBClusterToPointInTimeResponse'
            Core.<$> (x Core..@? "DBCluster")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable RestoreDBClusterToPointInTime

instance Core.NFData RestoreDBClusterToPointInTime

instance Core.ToHeaders RestoreDBClusterToPointInTime where
  toHeaders = Core.const Core.mempty

instance Core.ToPath RestoreDBClusterToPointInTime where
  toPath = Core.const "/"

instance Core.ToQuery RestoreDBClusterToPointInTime where
  toQuery RestoreDBClusterToPointInTime' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("RestoreDBClusterToPointInTime" :: Core.ByteString),
        "Version" Core.=: ("2014-10-31" :: Core.ByteString),
        "DeletionProtection" Core.=: deletionProtection,
        "EnableIAMDatabaseAuthentication"
          Core.=: enableIAMDatabaseAuthentication,
        "EnableCloudwatchLogsExports"
          Core.=: Core.toQuery
            ( Core.toQueryList "member"
                Core.<$> enableCloudwatchLogsExports
            ),
        "OptionGroupName" Core.=: optionGroupName,
        "RestoreType" Core.=: restoreType,
        "Domain" Core.=: domain,
        "DBSubnetGroupName" Core.=: dbSubnetGroupName,
        "VpcSecurityGroupIds"
          Core.=: Core.toQuery
            ( Core.toQueryList "VpcSecurityGroupId"
                Core.<$> vpcSecurityGroupIds
            ),
        "KmsKeyId" Core.=: kmsKeyId,
        "Tags"
          Core.=: Core.toQuery (Core.toQueryList "Tag" Core.<$> tags),
        "Port" Core.=: port,
        "DomainIAMRoleName" Core.=: domainIAMRoleName,
        "RestoreToTime" Core.=: restoreToTime,
        "CopyTagsToSnapshot" Core.=: copyTagsToSnapshot,
        "BacktrackWindow" Core.=: backtrackWindow,
        "DBClusterParameterGroupName"
          Core.=: dbClusterParameterGroupName,
        "UseLatestRestorableTime"
          Core.=: useLatestRestorableTime,
        "DBClusterIdentifier" Core.=: dbClusterIdentifier,
        "SourceDBClusterIdentifier"
          Core.=: sourceDBClusterIdentifier
      ]

-- | /See:/ 'newRestoreDBClusterToPointInTimeResponse' smart constructor.
data RestoreDBClusterToPointInTimeResponse = RestoreDBClusterToPointInTimeResponse'
  { dbCluster :: Core.Maybe DBCluster,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'RestoreDBClusterToPointInTimeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dbCluster', 'restoreDBClusterToPointInTimeResponse_dbCluster' - Undocumented member.
--
-- 'httpStatus', 'restoreDBClusterToPointInTimeResponse_httpStatus' - The response's http status code.
newRestoreDBClusterToPointInTimeResponse ::
  -- | 'httpStatus'
  Core.Int ->
  RestoreDBClusterToPointInTimeResponse
newRestoreDBClusterToPointInTimeResponse pHttpStatus_ =
  RestoreDBClusterToPointInTimeResponse'
    { dbCluster =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
restoreDBClusterToPointInTimeResponse_dbCluster :: Lens.Lens' RestoreDBClusterToPointInTimeResponse (Core.Maybe DBCluster)
restoreDBClusterToPointInTimeResponse_dbCluster = Lens.lens (\RestoreDBClusterToPointInTimeResponse' {dbCluster} -> dbCluster) (\s@RestoreDBClusterToPointInTimeResponse' {} a -> s {dbCluster = a} :: RestoreDBClusterToPointInTimeResponse)

-- | The response's http status code.
restoreDBClusterToPointInTimeResponse_httpStatus :: Lens.Lens' RestoreDBClusterToPointInTimeResponse Core.Int
restoreDBClusterToPointInTimeResponse_httpStatus = Lens.lens (\RestoreDBClusterToPointInTimeResponse' {httpStatus} -> httpStatus) (\s@RestoreDBClusterToPointInTimeResponse' {} a -> s {httpStatus = a} :: RestoreDBClusterToPointInTimeResponse)

instance
  Core.NFData
    RestoreDBClusterToPointInTimeResponse
