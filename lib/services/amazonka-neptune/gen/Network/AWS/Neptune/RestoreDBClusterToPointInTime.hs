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
-- Module      : Network.AWS.Neptune.RestoreDBClusterToPointInTime
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
-- DB cluster. You must invoke the CreateDBInstance action to create DB
-- instances for the restored DB cluster, specifying the identifier of the
-- restored DB cluster in @DBClusterIdentifier@. You can create DB
-- instances only after the @RestoreDBClusterToPointInTime@ action has
-- completed and the DB cluster is available.
module Network.AWS.Neptune.RestoreDBClusterToPointInTime
  ( -- * Creating a Request
    RestoreDBClusterToPointInTime (..),
    newRestoreDBClusterToPointInTime,

    -- * Request Lenses
    restoreDBClusterToPointInTime_deletionProtection,
    restoreDBClusterToPointInTime_useLatestRestorableTime,
    restoreDBClusterToPointInTime_dbSubnetGroupName,
    restoreDBClusterToPointInTime_kmsKeyId,
    restoreDBClusterToPointInTime_vpcSecurityGroupIds,
    restoreDBClusterToPointInTime_dbClusterParameterGroupName,
    restoreDBClusterToPointInTime_restoreType,
    restoreDBClusterToPointInTime_optionGroupName,
    restoreDBClusterToPointInTime_restoreToTime,
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Neptune.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newRestoreDBClusterToPointInTime' smart constructor.
data RestoreDBClusterToPointInTime = RestoreDBClusterToPointInTime'
  { -- | A value that indicates whether the DB cluster has deletion protection
    -- enabled. The database can\'t be deleted when deletion protection is
    -- enabled. By default, deletion protection is disabled.
    deletionProtection :: Prelude.Maybe Prelude.Bool,
    -- | A value that is set to @true@ to restore the DB cluster to the latest
    -- restorable backup time, and @false@ otherwise.
    --
    -- Default: @false@
    --
    -- Constraints: Cannot be specified if @RestoreToTime@ parameter is
    -- provided.
    useLatestRestorableTime :: Prelude.Maybe Prelude.Bool,
    -- | The DB subnet group name to use for the new DB cluster.
    --
    -- Constraints: If supplied, must match the name of an existing
    -- DBSubnetGroup.
    --
    -- Example: @mySubnetgroup@
    dbSubnetGroupName :: Prelude.Maybe Prelude.Text,
    -- | The Amazon KMS key identifier to use when restoring an encrypted DB
    -- cluster from an encrypted DB cluster.
    --
    -- The KMS key identifier is the Amazon Resource Name (ARN) for the KMS
    -- encryption key. If you are restoring a DB cluster with the same Amazon
    -- account that owns the KMS encryption key used to encrypt the new DB
    -- cluster, then you can use the KMS key alias instead of the ARN for the
    -- KMS encryption key.
    --
    -- You can restore to a new DB cluster and encrypt the new DB cluster with
    -- a KMS key that is different than the KMS key used to encrypt the source
    -- DB cluster. The new DB cluster is encrypted with the KMS key identified
    -- by the @KmsKeyId@ parameter.
    --
    -- If you do not specify a value for the @KmsKeyId@ parameter, then the
    -- following will occur:
    --
    -- -   If the DB cluster is encrypted, then the restored DB cluster is
    --     encrypted using the KMS key that was used to encrypt the source DB
    --     cluster.
    --
    -- -   If the DB cluster is not encrypted, then the restored DB cluster is
    --     not encrypted.
    --
    -- If @DBClusterIdentifier@ refers to a DB cluster that is not encrypted,
    -- then the restore request is rejected.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | A list of VPC security groups that the new DB cluster belongs to.
    vpcSecurityGroupIds :: Prelude.Maybe [Prelude.Text],
    -- | The name of the DB cluster parameter group to associate with the new DB
    -- cluster.
    --
    -- Constraints:
    --
    -- -   If supplied, must match the name of an existing
    --     DBClusterParameterGroup.
    dbClusterParameterGroupName :: Prelude.Maybe Prelude.Text,
    -- | The type of restore to be performed. You can specify one of the
    -- following values:
    --
    -- -   @full-copy@ - The new DB cluster is restored as a full copy of the
    --     source DB cluster.
    --
    -- -   @copy-on-write@ - The new DB cluster is restored as a clone of the
    --     source DB cluster.
    --
    -- If you don\'t specify a @RestoreType@ value, then the new DB cluster is
    -- restored as a full copy of the source DB cluster.
    restoreType :: Prelude.Maybe Prelude.Text,
    -- | /(Not supported by Neptune)/
    optionGroupName :: Prelude.Maybe Prelude.Text,
    -- | The date and time to restore the DB cluster to.
    --
    -- Valid Values: Value must be a time in Universal Coordinated Time (UTC)
    -- format
    --
    -- Constraints:
    --
    -- -   Must be before the latest restorable time for the DB instance
    --
    -- -   Must be specified if @UseLatestRestorableTime@ parameter is not
    --     provided
    --
    -- -   Cannot be specified if @UseLatestRestorableTime@ parameter is true
    --
    -- -   Cannot be specified if @RestoreType@ parameter is @copy-on-write@
    --
    -- Example: @2015-03-07T23:45:00Z@
    restoreToTime :: Prelude.Maybe Core.ISO8601,
    -- | The tags to be applied to the restored DB cluster.
    tags :: Prelude.Maybe [Tag],
    -- | The port number on which the new DB cluster accepts connections.
    --
    -- Constraints: Value must be @1150-65535@
    --
    -- Default: The same port as the original DB cluster.
    port :: Prelude.Maybe Prelude.Int,
    -- | True to enable mapping of Amazon Identity and Access Management (IAM)
    -- accounts to database accounts, and otherwise false.
    --
    -- Default: @false@
    enableIAMDatabaseAuthentication :: Prelude.Maybe Prelude.Bool,
    -- | The list of logs that the restored DB cluster is to export to CloudWatch
    -- Logs.
    enableCloudwatchLogsExports :: Prelude.Maybe [Prelude.Text],
    -- | The name of the new DB cluster to be created.
    --
    -- Constraints:
    --
    -- -   Must contain from 1 to 63 letters, numbers, or hyphens
    --
    -- -   First character must be a letter
    --
    -- -   Cannot end with a hyphen or contain two consecutive hyphens
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
-- 'useLatestRestorableTime', 'restoreDBClusterToPointInTime_useLatestRestorableTime' - A value that is set to @true@ to restore the DB cluster to the latest
-- restorable backup time, and @false@ otherwise.
--
-- Default: @false@
--
-- Constraints: Cannot be specified if @RestoreToTime@ parameter is
-- provided.
--
-- 'dbSubnetGroupName', 'restoreDBClusterToPointInTime_dbSubnetGroupName' - The DB subnet group name to use for the new DB cluster.
--
-- Constraints: If supplied, must match the name of an existing
-- DBSubnetGroup.
--
-- Example: @mySubnetgroup@
--
-- 'kmsKeyId', 'restoreDBClusterToPointInTime_kmsKeyId' - The Amazon KMS key identifier to use when restoring an encrypted DB
-- cluster from an encrypted DB cluster.
--
-- The KMS key identifier is the Amazon Resource Name (ARN) for the KMS
-- encryption key. If you are restoring a DB cluster with the same Amazon
-- account that owns the KMS encryption key used to encrypt the new DB
-- cluster, then you can use the KMS key alias instead of the ARN for the
-- KMS encryption key.
--
-- You can restore to a new DB cluster and encrypt the new DB cluster with
-- a KMS key that is different than the KMS key used to encrypt the source
-- DB cluster. The new DB cluster is encrypted with the KMS key identified
-- by the @KmsKeyId@ parameter.
--
-- If you do not specify a value for the @KmsKeyId@ parameter, then the
-- following will occur:
--
-- -   If the DB cluster is encrypted, then the restored DB cluster is
--     encrypted using the KMS key that was used to encrypt the source DB
--     cluster.
--
-- -   If the DB cluster is not encrypted, then the restored DB cluster is
--     not encrypted.
--
-- If @DBClusterIdentifier@ refers to a DB cluster that is not encrypted,
-- then the restore request is rejected.
--
-- 'vpcSecurityGroupIds', 'restoreDBClusterToPointInTime_vpcSecurityGroupIds' - A list of VPC security groups that the new DB cluster belongs to.
--
-- 'dbClusterParameterGroupName', 'restoreDBClusterToPointInTime_dbClusterParameterGroupName' - The name of the DB cluster parameter group to associate with the new DB
-- cluster.
--
-- Constraints:
--
-- -   If supplied, must match the name of an existing
--     DBClusterParameterGroup.
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
-- If you don\'t specify a @RestoreType@ value, then the new DB cluster is
-- restored as a full copy of the source DB cluster.
--
-- 'optionGroupName', 'restoreDBClusterToPointInTime_optionGroupName' - /(Not supported by Neptune)/
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
-- -   Must be specified if @UseLatestRestorableTime@ parameter is not
--     provided
--
-- -   Cannot be specified if @UseLatestRestorableTime@ parameter is true
--
-- -   Cannot be specified if @RestoreType@ parameter is @copy-on-write@
--
-- Example: @2015-03-07T23:45:00Z@
--
-- 'tags', 'restoreDBClusterToPointInTime_tags' - The tags to be applied to the restored DB cluster.
--
-- 'port', 'restoreDBClusterToPointInTime_port' - The port number on which the new DB cluster accepts connections.
--
-- Constraints: Value must be @1150-65535@
--
-- Default: The same port as the original DB cluster.
--
-- 'enableIAMDatabaseAuthentication', 'restoreDBClusterToPointInTime_enableIAMDatabaseAuthentication' - True to enable mapping of Amazon Identity and Access Management (IAM)
-- accounts to database accounts, and otherwise false.
--
-- Default: @false@
--
-- 'enableCloudwatchLogsExports', 'restoreDBClusterToPointInTime_enableCloudwatchLogsExports' - The list of logs that the restored DB cluster is to export to CloudWatch
-- Logs.
--
-- 'dbClusterIdentifier', 'restoreDBClusterToPointInTime_dbClusterIdentifier' - The name of the new DB cluster to be created.
--
-- Constraints:
--
-- -   Must contain from 1 to 63 letters, numbers, or hyphens
--
-- -   First character must be a letter
--
-- -   Cannot end with a hyphen or contain two consecutive hyphens
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
        kmsKeyId = Prelude.Nothing,
        vpcSecurityGroupIds = Prelude.Nothing,
        dbClusterParameterGroupName =
          Prelude.Nothing,
        restoreType = Prelude.Nothing,
        optionGroupName = Prelude.Nothing,
        restoreToTime = Prelude.Nothing,
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

-- | A value that is set to @true@ to restore the DB cluster to the latest
-- restorable backup time, and @false@ otherwise.
--
-- Default: @false@
--
-- Constraints: Cannot be specified if @RestoreToTime@ parameter is
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

-- | The Amazon KMS key identifier to use when restoring an encrypted DB
-- cluster from an encrypted DB cluster.
--
-- The KMS key identifier is the Amazon Resource Name (ARN) for the KMS
-- encryption key. If you are restoring a DB cluster with the same Amazon
-- account that owns the KMS encryption key used to encrypt the new DB
-- cluster, then you can use the KMS key alias instead of the ARN for the
-- KMS encryption key.
--
-- You can restore to a new DB cluster and encrypt the new DB cluster with
-- a KMS key that is different than the KMS key used to encrypt the source
-- DB cluster. The new DB cluster is encrypted with the KMS key identified
-- by the @KmsKeyId@ parameter.
--
-- If you do not specify a value for the @KmsKeyId@ parameter, then the
-- following will occur:
--
-- -   If the DB cluster is encrypted, then the restored DB cluster is
--     encrypted using the KMS key that was used to encrypt the source DB
--     cluster.
--
-- -   If the DB cluster is not encrypted, then the restored DB cluster is
--     not encrypted.
--
-- If @DBClusterIdentifier@ refers to a DB cluster that is not encrypted,
-- then the restore request is rejected.
restoreDBClusterToPointInTime_kmsKeyId :: Lens.Lens' RestoreDBClusterToPointInTime (Prelude.Maybe Prelude.Text)
restoreDBClusterToPointInTime_kmsKeyId = Lens.lens (\RestoreDBClusterToPointInTime' {kmsKeyId} -> kmsKeyId) (\s@RestoreDBClusterToPointInTime' {} a -> s {kmsKeyId = a} :: RestoreDBClusterToPointInTime)

-- | A list of VPC security groups that the new DB cluster belongs to.
restoreDBClusterToPointInTime_vpcSecurityGroupIds :: Lens.Lens' RestoreDBClusterToPointInTime (Prelude.Maybe [Prelude.Text])
restoreDBClusterToPointInTime_vpcSecurityGroupIds = Lens.lens (\RestoreDBClusterToPointInTime' {vpcSecurityGroupIds} -> vpcSecurityGroupIds) (\s@RestoreDBClusterToPointInTime' {} a -> s {vpcSecurityGroupIds = a} :: RestoreDBClusterToPointInTime) Prelude.. Lens.mapping Lens.coerced

-- | The name of the DB cluster parameter group to associate with the new DB
-- cluster.
--
-- Constraints:
--
-- -   If supplied, must match the name of an existing
--     DBClusterParameterGroup.
restoreDBClusterToPointInTime_dbClusterParameterGroupName :: Lens.Lens' RestoreDBClusterToPointInTime (Prelude.Maybe Prelude.Text)
restoreDBClusterToPointInTime_dbClusterParameterGroupName = Lens.lens (\RestoreDBClusterToPointInTime' {dbClusterParameterGroupName} -> dbClusterParameterGroupName) (\s@RestoreDBClusterToPointInTime' {} a -> s {dbClusterParameterGroupName = a} :: RestoreDBClusterToPointInTime)

-- | The type of restore to be performed. You can specify one of the
-- following values:
--
-- -   @full-copy@ - The new DB cluster is restored as a full copy of the
--     source DB cluster.
--
-- -   @copy-on-write@ - The new DB cluster is restored as a clone of the
--     source DB cluster.
--
-- If you don\'t specify a @RestoreType@ value, then the new DB cluster is
-- restored as a full copy of the source DB cluster.
restoreDBClusterToPointInTime_restoreType :: Lens.Lens' RestoreDBClusterToPointInTime (Prelude.Maybe Prelude.Text)
restoreDBClusterToPointInTime_restoreType = Lens.lens (\RestoreDBClusterToPointInTime' {restoreType} -> restoreType) (\s@RestoreDBClusterToPointInTime' {} a -> s {restoreType = a} :: RestoreDBClusterToPointInTime)

-- | /(Not supported by Neptune)/
restoreDBClusterToPointInTime_optionGroupName :: Lens.Lens' RestoreDBClusterToPointInTime (Prelude.Maybe Prelude.Text)
restoreDBClusterToPointInTime_optionGroupName = Lens.lens (\RestoreDBClusterToPointInTime' {optionGroupName} -> optionGroupName) (\s@RestoreDBClusterToPointInTime' {} a -> s {optionGroupName = a} :: RestoreDBClusterToPointInTime)

-- | The date and time to restore the DB cluster to.
--
-- Valid Values: Value must be a time in Universal Coordinated Time (UTC)
-- format
--
-- Constraints:
--
-- -   Must be before the latest restorable time for the DB instance
--
-- -   Must be specified if @UseLatestRestorableTime@ parameter is not
--     provided
--
-- -   Cannot be specified if @UseLatestRestorableTime@ parameter is true
--
-- -   Cannot be specified if @RestoreType@ parameter is @copy-on-write@
--
-- Example: @2015-03-07T23:45:00Z@
restoreDBClusterToPointInTime_restoreToTime :: Lens.Lens' RestoreDBClusterToPointInTime (Prelude.Maybe Prelude.UTCTime)
restoreDBClusterToPointInTime_restoreToTime = Lens.lens (\RestoreDBClusterToPointInTime' {restoreToTime} -> restoreToTime) (\s@RestoreDBClusterToPointInTime' {} a -> s {restoreToTime = a} :: RestoreDBClusterToPointInTime) Prelude.. Lens.mapping Core._Time

-- | The tags to be applied to the restored DB cluster.
restoreDBClusterToPointInTime_tags :: Lens.Lens' RestoreDBClusterToPointInTime (Prelude.Maybe [Tag])
restoreDBClusterToPointInTime_tags = Lens.lens (\RestoreDBClusterToPointInTime' {tags} -> tags) (\s@RestoreDBClusterToPointInTime' {} a -> s {tags = a} :: RestoreDBClusterToPointInTime) Prelude.. Lens.mapping Lens.coerced

-- | The port number on which the new DB cluster accepts connections.
--
-- Constraints: Value must be @1150-65535@
--
-- Default: The same port as the original DB cluster.
restoreDBClusterToPointInTime_port :: Lens.Lens' RestoreDBClusterToPointInTime (Prelude.Maybe Prelude.Int)
restoreDBClusterToPointInTime_port = Lens.lens (\RestoreDBClusterToPointInTime' {port} -> port) (\s@RestoreDBClusterToPointInTime' {} a -> s {port = a} :: RestoreDBClusterToPointInTime)

-- | True to enable mapping of Amazon Identity and Access Management (IAM)
-- accounts to database accounts, and otherwise false.
--
-- Default: @false@
restoreDBClusterToPointInTime_enableIAMDatabaseAuthentication :: Lens.Lens' RestoreDBClusterToPointInTime (Prelude.Maybe Prelude.Bool)
restoreDBClusterToPointInTime_enableIAMDatabaseAuthentication = Lens.lens (\RestoreDBClusterToPointInTime' {enableIAMDatabaseAuthentication} -> enableIAMDatabaseAuthentication) (\s@RestoreDBClusterToPointInTime' {} a -> s {enableIAMDatabaseAuthentication = a} :: RestoreDBClusterToPointInTime)

-- | The list of logs that the restored DB cluster is to export to CloudWatch
-- Logs.
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
-- -   Cannot end with a hyphen or contain two consecutive hyphens
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

instance Prelude.NFData RestoreDBClusterToPointInTime

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
        "KmsKeyId" Core.=: kmsKeyId,
        "VpcSecurityGroupIds"
          Core.=: Core.toQuery
            ( Core.toQueryList "VpcSecurityGroupId"
                Prelude.<$> vpcSecurityGroupIds
            ),
        "DBClusterParameterGroupName"
          Core.=: dbClusterParameterGroupName,
        "RestoreType" Core.=: restoreType,
        "OptionGroupName" Core.=: optionGroupName,
        "RestoreToTime" Core.=: restoreToTime,
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
