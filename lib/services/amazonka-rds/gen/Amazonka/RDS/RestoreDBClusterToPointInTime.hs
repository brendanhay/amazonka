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
-- Module      : Amazonka.RDS.RestoreDBClusterToPointInTime
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
module Amazonka.RDS.RestoreDBClusterToPointInTime
  ( -- * Creating a Request
    RestoreDBClusterToPointInTime (..),
    newRestoreDBClusterToPointInTime,

    -- * Request Lenses
    restoreDBClusterToPointInTime_deletionProtection,
    restoreDBClusterToPointInTime_useLatestRestorableTime,
    restoreDBClusterToPointInTime_dbSubnetGroupName,
    restoreDBClusterToPointInTime_domain,
    restoreDBClusterToPointInTime_backtrackWindow,
    restoreDBClusterToPointInTime_kmsKeyId,
    restoreDBClusterToPointInTime_vpcSecurityGroupIds,
    restoreDBClusterToPointInTime_dbClusterParameterGroupName,
    restoreDBClusterToPointInTime_engineMode,
    restoreDBClusterToPointInTime_scalingConfiguration,
    restoreDBClusterToPointInTime_restoreType,
    restoreDBClusterToPointInTime_optionGroupName,
    restoreDBClusterToPointInTime_copyTagsToSnapshot,
    restoreDBClusterToPointInTime_restoreToTime,
    restoreDBClusterToPointInTime_domainIAMRoleName,
    restoreDBClusterToPointInTime_tags,
    restoreDBClusterToPointInTime_port,
    restoreDBClusterToPointInTime_enableIAMDatabaseAuthentication,
    restoreDBClusterToPointInTime_enableCloudwatchLogsExports,
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.RDS.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- |
--
-- /See:/ 'newRestoreDBClusterToPointInTime' smart constructor.
data RestoreDBClusterToPointInTime = RestoreDBClusterToPointInTime'
  { -- | A value that indicates whether the DB cluster has deletion protection
    -- enabled. The database can\'t be deleted when deletion protection is
    -- enabled. By default, deletion protection is disabled.
    deletionProtection :: Prelude.Maybe Prelude.Bool,
    -- | A value that indicates whether to restore the DB cluster to the latest
    -- restorable backup time. By default, the DB cluster isn\'t restored to
    -- the latest restorable backup time.
    --
    -- Constraints: Can\'t be specified if @RestoreToTime@ parameter is
    -- provided.
    useLatestRestorableTime :: Prelude.Maybe Prelude.Bool,
    -- | The DB subnet group name to use for the new DB cluster.
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
    -- | The Amazon Web Services KMS key identifier to use when restoring an
    -- encrypted DB cluster from an encrypted DB cluster.
    --
    -- The Amazon Web Services KMS key identifier is the key ARN, key ID, alias
    -- ARN, or alias name for the Amazon Web Services KMS customer master key
    -- (CMK). To use a CMK in a different Amazon Web Services account, specify
    -- the key ARN or alias ARN.
    --
    -- You can restore to a new DB cluster and encrypt the new DB cluster with
    -- a Amazon Web Services KMS CMK that is different than the Amazon Web
    -- Services KMS key used to encrypt the source DB cluster. The new DB
    -- cluster is encrypted with the Amazon Web Services KMS CMK identified by
    -- the @KmsKeyId@ parameter.
    --
    -- If you don\'t specify a value for the @KmsKeyId@ parameter, then the
    -- following occurs:
    --
    -- -   If the DB cluster is encrypted, then the restored DB cluster is
    --     encrypted using the Amazon Web Services KMS CMK that was used to
    --     encrypt the source DB cluster.
    --
    -- -   If the DB cluster isn\'t encrypted, then the restored DB cluster
    --     isn\'t encrypted.
    --
    -- If @DBClusterIdentifier@ refers to a DB cluster that isn\'t encrypted,
    -- then the restore request is rejected.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | A list of VPC security groups that the new DB cluster belongs to.
    vpcSecurityGroupIds :: Prelude.Maybe [Prelude.Text],
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
    dbClusterParameterGroupName :: Prelude.Maybe Prelude.Text,
    -- | The engine mode of the new cluster. Specify @provisioned@ or
    -- @serverless@, depending on the type of the cluster you are creating. You
    -- can create an Aurora Serverless clone from a provisioned cluster, or a
    -- provisioned clone from an Aurora Serverless cluster. To create a clone
    -- that is an Aurora Serverless cluster, the original cluster must be an
    -- Aurora Serverless cluster or an encrypted provisioned cluster.
    engineMode :: Prelude.Maybe Prelude.Text,
    -- | For DB clusters in @serverless@ DB engine mode, the scaling properties
    -- of the DB cluster.
    scalingConfiguration :: Prelude.Maybe ScalingConfiguration,
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
    restoreType :: Prelude.Maybe Prelude.Text,
    -- | The name of the option group for the new DB cluster.
    optionGroupName :: Prelude.Maybe Prelude.Text,
    -- | A value that indicates whether to copy all tags from the restored DB
    -- cluster to snapshots of the restored DB cluster. The default is not to
    -- copy them.
    copyTagsToSnapshot :: Prelude.Maybe Prelude.Bool,
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
    restoreToTime :: Prelude.Maybe Core.ISO8601,
    -- | Specify the name of the IAM role to be used when making API calls to the
    -- Directory Service.
    domainIAMRoleName :: Prelude.Maybe Prelude.Text,
    tags :: Prelude.Maybe [Tag],
    -- | The port number on which the new DB cluster accepts connections.
    --
    -- Constraints: A value from @1150-65535@.
    --
    -- Default: The default port for the engine.
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
    -- | The name of the new DB cluster to be created.
    --
    -- Constraints:
    --
    -- -   Must contain from 1 to 63 letters, numbers, or hyphens
    --
    -- -   First character must be a letter
    --
    -- -   Can\'t end with a hyphen or contain two consecutive hyphens
    dbClusterIdentifier :: Prelude.Text,
    -- | The identifier of the source DB cluster from which to restore.
    --
    -- Constraints:
    --
    -- -   Must match the identifier of an existing DBCluster.
    sourceDBClusterIdentifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'useLatestRestorableTime', 'restoreDBClusterToPointInTime_useLatestRestorableTime' - A value that indicates whether to restore the DB cluster to the latest
-- restorable backup time. By default, the DB cluster isn\'t restored to
-- the latest restorable backup time.
--
-- Constraints: Can\'t be specified if @RestoreToTime@ parameter is
-- provided.
--
-- 'dbSubnetGroupName', 'restoreDBClusterToPointInTime_dbSubnetGroupName' - The DB subnet group name to use for the new DB cluster.
--
-- Constraints: If supplied, must match the name of an existing
-- DBSubnetGroup.
--
-- Example: @mySubnetgroup@
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
-- 'kmsKeyId', 'restoreDBClusterToPointInTime_kmsKeyId' - The Amazon Web Services KMS key identifier to use when restoring an
-- encrypted DB cluster from an encrypted DB cluster.
--
-- The Amazon Web Services KMS key identifier is the key ARN, key ID, alias
-- ARN, or alias name for the Amazon Web Services KMS customer master key
-- (CMK). To use a CMK in a different Amazon Web Services account, specify
-- the key ARN or alias ARN.
--
-- You can restore to a new DB cluster and encrypt the new DB cluster with
-- a Amazon Web Services KMS CMK that is different than the Amazon Web
-- Services KMS key used to encrypt the source DB cluster. The new DB
-- cluster is encrypted with the Amazon Web Services KMS CMK identified by
-- the @KmsKeyId@ parameter.
--
-- If you don\'t specify a value for the @KmsKeyId@ parameter, then the
-- following occurs:
--
-- -   If the DB cluster is encrypted, then the restored DB cluster is
--     encrypted using the Amazon Web Services KMS CMK that was used to
--     encrypt the source DB cluster.
--
-- -   If the DB cluster isn\'t encrypted, then the restored DB cluster
--     isn\'t encrypted.
--
-- If @DBClusterIdentifier@ refers to a DB cluster that isn\'t encrypted,
-- then the restore request is rejected.
--
-- 'vpcSecurityGroupIds', 'restoreDBClusterToPointInTime_vpcSecurityGroupIds' - A list of VPC security groups that the new DB cluster belongs to.
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
-- 'engineMode', 'restoreDBClusterToPointInTime_engineMode' - The engine mode of the new cluster. Specify @provisioned@ or
-- @serverless@, depending on the type of the cluster you are creating. You
-- can create an Aurora Serverless clone from a provisioned cluster, or a
-- provisioned clone from an Aurora Serverless cluster. To create a clone
-- that is an Aurora Serverless cluster, the original cluster must be an
-- Aurora Serverless cluster or an encrypted provisioned cluster.
--
-- 'scalingConfiguration', 'restoreDBClusterToPointInTime_scalingConfiguration' - For DB clusters in @serverless@ DB engine mode, the scaling properties
-- of the DB cluster.
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
-- 'optionGroupName', 'restoreDBClusterToPointInTime_optionGroupName' - The name of the option group for the new DB cluster.
--
-- 'copyTagsToSnapshot', 'restoreDBClusterToPointInTime_copyTagsToSnapshot' - A value that indicates whether to copy all tags from the restored DB
-- cluster to snapshots of the restored DB cluster. The default is not to
-- copy them.
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
-- 'domainIAMRoleName', 'restoreDBClusterToPointInTime_domainIAMRoleName' - Specify the name of the IAM role to be used when making API calls to the
-- Directory Service.
--
-- 'tags', 'restoreDBClusterToPointInTime_tags' - Undocumented member.
--
-- 'port', 'restoreDBClusterToPointInTime_port' - The port number on which the new DB cluster accepts connections.
--
-- Constraints: A value from @1150-65535@.
--
-- Default: The default port for the engine.
--
-- 'enableIAMDatabaseAuthentication', 'restoreDBClusterToPointInTime_enableIAMDatabaseAuthentication' - A value that indicates whether to enable mapping of Amazon Web Services
-- Identity and Access Management (IAM) accounts to database accounts. By
-- default, mapping is disabled.
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
  Prelude.Text ->
  -- | 'sourceDBClusterIdentifier'
  Prelude.Text ->
  RestoreDBClusterToPointInTime
newRestoreDBClusterToPointInTime
  pDBClusterIdentifier_
  pSourceDBClusterIdentifier_ =
    RestoreDBClusterToPointInTime'
      { deletionProtection =
          Prelude.Nothing,
        useLatestRestorableTime = Prelude.Nothing,
        dbSubnetGroupName = Prelude.Nothing,
        domain = Prelude.Nothing,
        backtrackWindow = Prelude.Nothing,
        kmsKeyId = Prelude.Nothing,
        vpcSecurityGroupIds = Prelude.Nothing,
        dbClusterParameterGroupName =
          Prelude.Nothing,
        engineMode = Prelude.Nothing,
        scalingConfiguration = Prelude.Nothing,
        restoreType = Prelude.Nothing,
        optionGroupName = Prelude.Nothing,
        copyTagsToSnapshot = Prelude.Nothing,
        restoreToTime = Prelude.Nothing,
        domainIAMRoleName = Prelude.Nothing,
        tags = Prelude.Nothing,
        port = Prelude.Nothing,
        enableIAMDatabaseAuthentication =
          Prelude.Nothing,
        enableCloudwatchLogsExports =
          Prelude.Nothing,
        dbClusterIdentifier = pDBClusterIdentifier_,
        sourceDBClusterIdentifier =
          pSourceDBClusterIdentifier_
      }

-- | A value that indicates whether the DB cluster has deletion protection
-- enabled. The database can\'t be deleted when deletion protection is
-- enabled. By default, deletion protection is disabled.
restoreDBClusterToPointInTime_deletionProtection :: Lens.Lens' RestoreDBClusterToPointInTime (Prelude.Maybe Prelude.Bool)
restoreDBClusterToPointInTime_deletionProtection = Lens.lens (\RestoreDBClusterToPointInTime' {deletionProtection} -> deletionProtection) (\s@RestoreDBClusterToPointInTime' {} a -> s {deletionProtection = a} :: RestoreDBClusterToPointInTime)

-- | A value that indicates whether to restore the DB cluster to the latest
-- restorable backup time. By default, the DB cluster isn\'t restored to
-- the latest restorable backup time.
--
-- Constraints: Can\'t be specified if @RestoreToTime@ parameter is
-- provided.
restoreDBClusterToPointInTime_useLatestRestorableTime :: Lens.Lens' RestoreDBClusterToPointInTime (Prelude.Maybe Prelude.Bool)
restoreDBClusterToPointInTime_useLatestRestorableTime = Lens.lens (\RestoreDBClusterToPointInTime' {useLatestRestorableTime} -> useLatestRestorableTime) (\s@RestoreDBClusterToPointInTime' {} a -> s {useLatestRestorableTime = a} :: RestoreDBClusterToPointInTime)

-- | The DB subnet group name to use for the new DB cluster.
--
-- Constraints: If supplied, must match the name of an existing
-- DBSubnetGroup.
--
-- Example: @mySubnetgroup@
restoreDBClusterToPointInTime_dbSubnetGroupName :: Lens.Lens' RestoreDBClusterToPointInTime (Prelude.Maybe Prelude.Text)
restoreDBClusterToPointInTime_dbSubnetGroupName = Lens.lens (\RestoreDBClusterToPointInTime' {dbSubnetGroupName} -> dbSubnetGroupName) (\s@RestoreDBClusterToPointInTime' {} a -> s {dbSubnetGroupName = a} :: RestoreDBClusterToPointInTime)

-- | Specify the Active Directory directory ID to restore the DB cluster in.
-- The domain must be created prior to this operation.
--
-- For Amazon Aurora DB clusters, Amazon RDS can use Kerberos
-- Authentication to authenticate users that connect to the DB cluster. For
-- more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/kerberos-authentication.html Kerberos Authentication>
-- in the /Amazon Aurora User Guide/.
restoreDBClusterToPointInTime_domain :: Lens.Lens' RestoreDBClusterToPointInTime (Prelude.Maybe Prelude.Text)
restoreDBClusterToPointInTime_domain = Lens.lens (\RestoreDBClusterToPointInTime' {domain} -> domain) (\s@RestoreDBClusterToPointInTime' {} a -> s {domain = a} :: RestoreDBClusterToPointInTime)

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
restoreDBClusterToPointInTime_backtrackWindow :: Lens.Lens' RestoreDBClusterToPointInTime (Prelude.Maybe Prelude.Integer)
restoreDBClusterToPointInTime_backtrackWindow = Lens.lens (\RestoreDBClusterToPointInTime' {backtrackWindow} -> backtrackWindow) (\s@RestoreDBClusterToPointInTime' {} a -> s {backtrackWindow = a} :: RestoreDBClusterToPointInTime)

-- | The Amazon Web Services KMS key identifier to use when restoring an
-- encrypted DB cluster from an encrypted DB cluster.
--
-- The Amazon Web Services KMS key identifier is the key ARN, key ID, alias
-- ARN, or alias name for the Amazon Web Services KMS customer master key
-- (CMK). To use a CMK in a different Amazon Web Services account, specify
-- the key ARN or alias ARN.
--
-- You can restore to a new DB cluster and encrypt the new DB cluster with
-- a Amazon Web Services KMS CMK that is different than the Amazon Web
-- Services KMS key used to encrypt the source DB cluster. The new DB
-- cluster is encrypted with the Amazon Web Services KMS CMK identified by
-- the @KmsKeyId@ parameter.
--
-- If you don\'t specify a value for the @KmsKeyId@ parameter, then the
-- following occurs:
--
-- -   If the DB cluster is encrypted, then the restored DB cluster is
--     encrypted using the Amazon Web Services KMS CMK that was used to
--     encrypt the source DB cluster.
--
-- -   If the DB cluster isn\'t encrypted, then the restored DB cluster
--     isn\'t encrypted.
--
-- If @DBClusterIdentifier@ refers to a DB cluster that isn\'t encrypted,
-- then the restore request is rejected.
restoreDBClusterToPointInTime_kmsKeyId :: Lens.Lens' RestoreDBClusterToPointInTime (Prelude.Maybe Prelude.Text)
restoreDBClusterToPointInTime_kmsKeyId = Lens.lens (\RestoreDBClusterToPointInTime' {kmsKeyId} -> kmsKeyId) (\s@RestoreDBClusterToPointInTime' {} a -> s {kmsKeyId = a} :: RestoreDBClusterToPointInTime)

-- | A list of VPC security groups that the new DB cluster belongs to.
restoreDBClusterToPointInTime_vpcSecurityGroupIds :: Lens.Lens' RestoreDBClusterToPointInTime (Prelude.Maybe [Prelude.Text])
restoreDBClusterToPointInTime_vpcSecurityGroupIds = Lens.lens (\RestoreDBClusterToPointInTime' {vpcSecurityGroupIds} -> vpcSecurityGroupIds) (\s@RestoreDBClusterToPointInTime' {} a -> s {vpcSecurityGroupIds = a} :: RestoreDBClusterToPointInTime) Prelude.. Lens.mapping Lens.coerced

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
restoreDBClusterToPointInTime_dbClusterParameterGroupName :: Lens.Lens' RestoreDBClusterToPointInTime (Prelude.Maybe Prelude.Text)
restoreDBClusterToPointInTime_dbClusterParameterGroupName = Lens.lens (\RestoreDBClusterToPointInTime' {dbClusterParameterGroupName} -> dbClusterParameterGroupName) (\s@RestoreDBClusterToPointInTime' {} a -> s {dbClusterParameterGroupName = a} :: RestoreDBClusterToPointInTime)

-- | The engine mode of the new cluster. Specify @provisioned@ or
-- @serverless@, depending on the type of the cluster you are creating. You
-- can create an Aurora Serverless clone from a provisioned cluster, or a
-- provisioned clone from an Aurora Serverless cluster. To create a clone
-- that is an Aurora Serverless cluster, the original cluster must be an
-- Aurora Serverless cluster or an encrypted provisioned cluster.
restoreDBClusterToPointInTime_engineMode :: Lens.Lens' RestoreDBClusterToPointInTime (Prelude.Maybe Prelude.Text)
restoreDBClusterToPointInTime_engineMode = Lens.lens (\RestoreDBClusterToPointInTime' {engineMode} -> engineMode) (\s@RestoreDBClusterToPointInTime' {} a -> s {engineMode = a} :: RestoreDBClusterToPointInTime)

-- | For DB clusters in @serverless@ DB engine mode, the scaling properties
-- of the DB cluster.
restoreDBClusterToPointInTime_scalingConfiguration :: Lens.Lens' RestoreDBClusterToPointInTime (Prelude.Maybe ScalingConfiguration)
restoreDBClusterToPointInTime_scalingConfiguration = Lens.lens (\RestoreDBClusterToPointInTime' {scalingConfiguration} -> scalingConfiguration) (\s@RestoreDBClusterToPointInTime' {} a -> s {scalingConfiguration = a} :: RestoreDBClusterToPointInTime)

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
restoreDBClusterToPointInTime_restoreType :: Lens.Lens' RestoreDBClusterToPointInTime (Prelude.Maybe Prelude.Text)
restoreDBClusterToPointInTime_restoreType = Lens.lens (\RestoreDBClusterToPointInTime' {restoreType} -> restoreType) (\s@RestoreDBClusterToPointInTime' {} a -> s {restoreType = a} :: RestoreDBClusterToPointInTime)

-- | The name of the option group for the new DB cluster.
restoreDBClusterToPointInTime_optionGroupName :: Lens.Lens' RestoreDBClusterToPointInTime (Prelude.Maybe Prelude.Text)
restoreDBClusterToPointInTime_optionGroupName = Lens.lens (\RestoreDBClusterToPointInTime' {optionGroupName} -> optionGroupName) (\s@RestoreDBClusterToPointInTime' {} a -> s {optionGroupName = a} :: RestoreDBClusterToPointInTime)

-- | A value that indicates whether to copy all tags from the restored DB
-- cluster to snapshots of the restored DB cluster. The default is not to
-- copy them.
restoreDBClusterToPointInTime_copyTagsToSnapshot :: Lens.Lens' RestoreDBClusterToPointInTime (Prelude.Maybe Prelude.Bool)
restoreDBClusterToPointInTime_copyTagsToSnapshot = Lens.lens (\RestoreDBClusterToPointInTime' {copyTagsToSnapshot} -> copyTagsToSnapshot) (\s@RestoreDBClusterToPointInTime' {} a -> s {copyTagsToSnapshot = a} :: RestoreDBClusterToPointInTime)

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
restoreDBClusterToPointInTime_restoreToTime :: Lens.Lens' RestoreDBClusterToPointInTime (Prelude.Maybe Prelude.UTCTime)
restoreDBClusterToPointInTime_restoreToTime = Lens.lens (\RestoreDBClusterToPointInTime' {restoreToTime} -> restoreToTime) (\s@RestoreDBClusterToPointInTime' {} a -> s {restoreToTime = a} :: RestoreDBClusterToPointInTime) Prelude.. Lens.mapping Core._Time

-- | Specify the name of the IAM role to be used when making API calls to the
-- Directory Service.
restoreDBClusterToPointInTime_domainIAMRoleName :: Lens.Lens' RestoreDBClusterToPointInTime (Prelude.Maybe Prelude.Text)
restoreDBClusterToPointInTime_domainIAMRoleName = Lens.lens (\RestoreDBClusterToPointInTime' {domainIAMRoleName} -> domainIAMRoleName) (\s@RestoreDBClusterToPointInTime' {} a -> s {domainIAMRoleName = a} :: RestoreDBClusterToPointInTime)

-- | Undocumented member.
restoreDBClusterToPointInTime_tags :: Lens.Lens' RestoreDBClusterToPointInTime (Prelude.Maybe [Tag])
restoreDBClusterToPointInTime_tags = Lens.lens (\RestoreDBClusterToPointInTime' {tags} -> tags) (\s@RestoreDBClusterToPointInTime' {} a -> s {tags = a} :: RestoreDBClusterToPointInTime) Prelude.. Lens.mapping Lens.coerced

-- | The port number on which the new DB cluster accepts connections.
--
-- Constraints: A value from @1150-65535@.
--
-- Default: The default port for the engine.
restoreDBClusterToPointInTime_port :: Lens.Lens' RestoreDBClusterToPointInTime (Prelude.Maybe Prelude.Int)
restoreDBClusterToPointInTime_port = Lens.lens (\RestoreDBClusterToPointInTime' {port} -> port) (\s@RestoreDBClusterToPointInTime' {} a -> s {port = a} :: RestoreDBClusterToPointInTime)

-- | A value that indicates whether to enable mapping of Amazon Web Services
-- Identity and Access Management (IAM) accounts to database accounts. By
-- default, mapping is disabled.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/UsingWithRDS.IAMDBAuth.html IAM Database Authentication>
-- in the /Amazon Aurora User Guide./
restoreDBClusterToPointInTime_enableIAMDatabaseAuthentication :: Lens.Lens' RestoreDBClusterToPointInTime (Prelude.Maybe Prelude.Bool)
restoreDBClusterToPointInTime_enableIAMDatabaseAuthentication = Lens.lens (\RestoreDBClusterToPointInTime' {enableIAMDatabaseAuthentication} -> enableIAMDatabaseAuthentication) (\s@RestoreDBClusterToPointInTime' {} a -> s {enableIAMDatabaseAuthentication = a} :: RestoreDBClusterToPointInTime)

-- | The list of logs that the restored DB cluster is to export to CloudWatch
-- Logs. The values in the list depend on the DB engine being used. For
-- more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/USER_LogAccess.html#USER_LogAccess.Procedural.UploadtoCloudWatch Publishing Database Logs to Amazon CloudWatch Logs>
-- in the /Amazon Aurora User Guide/.
restoreDBClusterToPointInTime_enableCloudwatchLogsExports :: Lens.Lens' RestoreDBClusterToPointInTime (Prelude.Maybe [Prelude.Text])
restoreDBClusterToPointInTime_enableCloudwatchLogsExports = Lens.lens (\RestoreDBClusterToPointInTime' {enableCloudwatchLogsExports} -> enableCloudwatchLogsExports) (\s@RestoreDBClusterToPointInTime' {} a -> s {enableCloudwatchLogsExports = a} :: RestoreDBClusterToPointInTime) Prelude.. Lens.mapping Lens.coerced

-- | The name of the new DB cluster to be created.
--
-- Constraints:
--
-- -   Must contain from 1 to 63 letters, numbers, or hyphens
--
-- -   First character must be a letter
--
-- -   Can\'t end with a hyphen or contain two consecutive hyphens
restoreDBClusterToPointInTime_dbClusterIdentifier :: Lens.Lens' RestoreDBClusterToPointInTime Prelude.Text
restoreDBClusterToPointInTime_dbClusterIdentifier = Lens.lens (\RestoreDBClusterToPointInTime' {dbClusterIdentifier} -> dbClusterIdentifier) (\s@RestoreDBClusterToPointInTime' {} a -> s {dbClusterIdentifier = a} :: RestoreDBClusterToPointInTime)

-- | The identifier of the source DB cluster from which to restore.
--
-- Constraints:
--
-- -   Must match the identifier of an existing DBCluster.
restoreDBClusterToPointInTime_sourceDBClusterIdentifier :: Lens.Lens' RestoreDBClusterToPointInTime Prelude.Text
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
            Prelude.<$> (x Core..@? "DBCluster")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    RestoreDBClusterToPointInTime
  where
  hashWithSalt salt' RestoreDBClusterToPointInTime' {..} =
    salt'
      `Prelude.hashWithSalt` sourceDBClusterIdentifier
      `Prelude.hashWithSalt` dbClusterIdentifier
      `Prelude.hashWithSalt` enableCloudwatchLogsExports
      `Prelude.hashWithSalt` enableIAMDatabaseAuthentication
      `Prelude.hashWithSalt` port
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` domainIAMRoleName
      `Prelude.hashWithSalt` restoreToTime
      `Prelude.hashWithSalt` copyTagsToSnapshot
      `Prelude.hashWithSalt` optionGroupName
      `Prelude.hashWithSalt` restoreType
      `Prelude.hashWithSalt` scalingConfiguration
      `Prelude.hashWithSalt` engineMode
      `Prelude.hashWithSalt` dbClusterParameterGroupName
      `Prelude.hashWithSalt` vpcSecurityGroupIds
      `Prelude.hashWithSalt` kmsKeyId
      `Prelude.hashWithSalt` backtrackWindow
      `Prelude.hashWithSalt` domain
      `Prelude.hashWithSalt` dbSubnetGroupName
      `Prelude.hashWithSalt` useLatestRestorableTime
      `Prelude.hashWithSalt` deletionProtection

instance Prelude.NFData RestoreDBClusterToPointInTime where
  rnf RestoreDBClusterToPointInTime' {..} =
    Prelude.rnf deletionProtection
      `Prelude.seq` Prelude.rnf sourceDBClusterIdentifier
      `Prelude.seq` Prelude.rnf dbClusterIdentifier
      `Prelude.seq` Prelude.rnf enableCloudwatchLogsExports
      `Prelude.seq` Prelude.rnf enableIAMDatabaseAuthentication
      `Prelude.seq` Prelude.rnf port
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf domainIAMRoleName
      `Prelude.seq` Prelude.rnf restoreToTime
      `Prelude.seq` Prelude.rnf copyTagsToSnapshot
      `Prelude.seq` Prelude.rnf optionGroupName
      `Prelude.seq` Prelude.rnf restoreType
      `Prelude.seq` Prelude.rnf scalingConfiguration
      `Prelude.seq` Prelude.rnf engineMode
      `Prelude.seq` Prelude.rnf dbClusterParameterGroupName
      `Prelude.seq` Prelude.rnf vpcSecurityGroupIds
      `Prelude.seq` Prelude.rnf kmsKeyId
      `Prelude.seq` Prelude.rnf backtrackWindow
      `Prelude.seq` Prelude.rnf domain
      `Prelude.seq` Prelude.rnf dbSubnetGroupName
      `Prelude.seq` Prelude.rnf useLatestRestorableTime

instance Core.ToHeaders RestoreDBClusterToPointInTime where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath RestoreDBClusterToPointInTime where
  toPath = Prelude.const "/"

instance Core.ToQuery RestoreDBClusterToPointInTime where
  toQuery RestoreDBClusterToPointInTime' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ( "RestoreDBClusterToPointInTime" ::
                      Prelude.ByteString
                  ),
        "Version"
          Core.=: ("2014-10-31" :: Prelude.ByteString),
        "DeletionProtection" Core.=: deletionProtection,
        "UseLatestRestorableTime"
          Core.=: useLatestRestorableTime,
        "DBSubnetGroupName" Core.=: dbSubnetGroupName,
        "Domain" Core.=: domain,
        "BacktrackWindow" Core.=: backtrackWindow,
        "KmsKeyId" Core.=: kmsKeyId,
        "VpcSecurityGroupIds"
          Core.=: Core.toQuery
            ( Core.toQueryList "VpcSecurityGroupId"
                Prelude.<$> vpcSecurityGroupIds
            ),
        "DBClusterParameterGroupName"
          Core.=: dbClusterParameterGroupName,
        "EngineMode" Core.=: engineMode,
        "ScalingConfiguration" Core.=: scalingConfiguration,
        "RestoreType" Core.=: restoreType,
        "OptionGroupName" Core.=: optionGroupName,
        "CopyTagsToSnapshot" Core.=: copyTagsToSnapshot,
        "RestoreToTime" Core.=: restoreToTime,
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
        "SourceDBClusterIdentifier"
          Core.=: sourceDBClusterIdentifier
      ]

-- | /See:/ 'newRestoreDBClusterToPointInTimeResponse' smart constructor.
data RestoreDBClusterToPointInTimeResponse = RestoreDBClusterToPointInTimeResponse'
  { dbCluster :: Prelude.Maybe DBCluster,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  RestoreDBClusterToPointInTimeResponse
newRestoreDBClusterToPointInTimeResponse pHttpStatus_ =
  RestoreDBClusterToPointInTimeResponse'
    { dbCluster =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
restoreDBClusterToPointInTimeResponse_dbCluster :: Lens.Lens' RestoreDBClusterToPointInTimeResponse (Prelude.Maybe DBCluster)
restoreDBClusterToPointInTimeResponse_dbCluster = Lens.lens (\RestoreDBClusterToPointInTimeResponse' {dbCluster} -> dbCluster) (\s@RestoreDBClusterToPointInTimeResponse' {} a -> s {dbCluster = a} :: RestoreDBClusterToPointInTimeResponse)

-- | The response's http status code.
restoreDBClusterToPointInTimeResponse_httpStatus :: Lens.Lens' RestoreDBClusterToPointInTimeResponse Prelude.Int
restoreDBClusterToPointInTimeResponse_httpStatus = Lens.lens (\RestoreDBClusterToPointInTimeResponse' {httpStatus} -> httpStatus) (\s@RestoreDBClusterToPointInTimeResponse' {} a -> s {httpStatus = a} :: RestoreDBClusterToPointInTimeResponse)

instance
  Prelude.NFData
    RestoreDBClusterToPointInTimeResponse
  where
  rnf RestoreDBClusterToPointInTimeResponse' {..} =
    Prelude.rnf dbCluster
      `Prelude.seq` Prelude.rnf httpStatus
