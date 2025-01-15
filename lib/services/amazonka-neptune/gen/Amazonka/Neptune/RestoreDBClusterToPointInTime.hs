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
-- Module      : Amazonka.Neptune.RestoreDBClusterToPointInTime
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
module Amazonka.Neptune.RestoreDBClusterToPointInTime
  ( -- * Creating a Request
    RestoreDBClusterToPointInTime (..),
    newRestoreDBClusterToPointInTime,

    -- * Request Lenses
    restoreDBClusterToPointInTime_dbClusterParameterGroupName,
    restoreDBClusterToPointInTime_dbSubnetGroupName,
    restoreDBClusterToPointInTime_deletionProtection,
    restoreDBClusterToPointInTime_enableCloudwatchLogsExports,
    restoreDBClusterToPointInTime_enableIAMDatabaseAuthentication,
    restoreDBClusterToPointInTime_kmsKeyId,
    restoreDBClusterToPointInTime_optionGroupName,
    restoreDBClusterToPointInTime_port,
    restoreDBClusterToPointInTime_restoreToTime,
    restoreDBClusterToPointInTime_restoreType,
    restoreDBClusterToPointInTime_serverlessV2ScalingConfiguration,
    restoreDBClusterToPointInTime_tags,
    restoreDBClusterToPointInTime_useLatestRestorableTime,
    restoreDBClusterToPointInTime_vpcSecurityGroupIds,
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
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Neptune.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newRestoreDBClusterToPointInTime' smart constructor.
data RestoreDBClusterToPointInTime = RestoreDBClusterToPointInTime'
  { -- | The name of the DB cluster parameter group to associate with the new DB
    -- cluster.
    --
    -- Constraints:
    --
    -- -   If supplied, must match the name of an existing
    --     DBClusterParameterGroup.
    dbClusterParameterGroupName :: Prelude.Maybe Prelude.Text,
    -- | The DB subnet group name to use for the new DB cluster.
    --
    -- Constraints: If supplied, must match the name of an existing
    -- DBSubnetGroup.
    --
    -- Example: @mySubnetgroup@
    dbSubnetGroupName :: Prelude.Maybe Prelude.Text,
    -- | A value that indicates whether the DB cluster has deletion protection
    -- enabled. The database can\'t be deleted when deletion protection is
    -- enabled. By default, deletion protection is disabled.
    deletionProtection :: Prelude.Maybe Prelude.Bool,
    -- | The list of logs that the restored DB cluster is to export to CloudWatch
    -- Logs.
    enableCloudwatchLogsExports :: Prelude.Maybe [Prelude.Text],
    -- | True to enable mapping of Amazon Identity and Access Management (IAM)
    -- accounts to database accounts, and otherwise false.
    --
    -- Default: @false@
    enableIAMDatabaseAuthentication :: Prelude.Maybe Prelude.Bool,
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
    -- | /(Not supported by Neptune)/
    optionGroupName :: Prelude.Maybe Prelude.Text,
    -- | The port number on which the new DB cluster accepts connections.
    --
    -- Constraints: Value must be @1150-65535@
    --
    -- Default: The same port as the original DB cluster.
    port :: Prelude.Maybe Prelude.Int,
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
    restoreToTime :: Prelude.Maybe Data.ISO8601,
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
    serverlessV2ScalingConfiguration :: Prelude.Maybe ServerlessV2ScalingConfiguration,
    -- | The tags to be applied to the restored DB cluster.
    tags :: Prelude.Maybe [Tag],
    -- | A value that is set to @true@ to restore the DB cluster to the latest
    -- restorable backup time, and @false@ otherwise.
    --
    -- Default: @false@
    --
    -- Constraints: Cannot be specified if @RestoreToTime@ parameter is
    -- provided.
    useLatestRestorableTime :: Prelude.Maybe Prelude.Bool,
    -- | A list of VPC security groups that the new DB cluster belongs to.
    vpcSecurityGroupIds :: Prelude.Maybe [Prelude.Text],
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
-- 'dbClusterParameterGroupName', 'restoreDBClusterToPointInTime_dbClusterParameterGroupName' - The name of the DB cluster parameter group to associate with the new DB
-- cluster.
--
-- Constraints:
--
-- -   If supplied, must match the name of an existing
--     DBClusterParameterGroup.
--
-- 'dbSubnetGroupName', 'restoreDBClusterToPointInTime_dbSubnetGroupName' - The DB subnet group name to use for the new DB cluster.
--
-- Constraints: If supplied, must match the name of an existing
-- DBSubnetGroup.
--
-- Example: @mySubnetgroup@
--
-- 'deletionProtection', 'restoreDBClusterToPointInTime_deletionProtection' - A value that indicates whether the DB cluster has deletion protection
-- enabled. The database can\'t be deleted when deletion protection is
-- enabled. By default, deletion protection is disabled.
--
-- 'enableCloudwatchLogsExports', 'restoreDBClusterToPointInTime_enableCloudwatchLogsExports' - The list of logs that the restored DB cluster is to export to CloudWatch
-- Logs.
--
-- 'enableIAMDatabaseAuthentication', 'restoreDBClusterToPointInTime_enableIAMDatabaseAuthentication' - True to enable mapping of Amazon Identity and Access Management (IAM)
-- accounts to database accounts, and otherwise false.
--
-- Default: @false@
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
-- 'optionGroupName', 'restoreDBClusterToPointInTime_optionGroupName' - /(Not supported by Neptune)/
--
-- 'port', 'restoreDBClusterToPointInTime_port' - The port number on which the new DB cluster accepts connections.
--
-- Constraints: Value must be @1150-65535@
--
-- Default: The same port as the original DB cluster.
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
-- 'serverlessV2ScalingConfiguration', 'restoreDBClusterToPointInTime_serverlessV2ScalingConfiguration' - Undocumented member.
--
-- 'tags', 'restoreDBClusterToPointInTime_tags' - The tags to be applied to the restored DB cluster.
--
-- 'useLatestRestorableTime', 'restoreDBClusterToPointInTime_useLatestRestorableTime' - A value that is set to @true@ to restore the DB cluster to the latest
-- restorable backup time, and @false@ otherwise.
--
-- Default: @false@
--
-- Constraints: Cannot be specified if @RestoreToTime@ parameter is
-- provided.
--
-- 'vpcSecurityGroupIds', 'restoreDBClusterToPointInTime_vpcSecurityGroupIds' - A list of VPC security groups that the new DB cluster belongs to.
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
      { dbClusterParameterGroupName =
          Prelude.Nothing,
        dbSubnetGroupName = Prelude.Nothing,
        deletionProtection = Prelude.Nothing,
        enableCloudwatchLogsExports =
          Prelude.Nothing,
        enableIAMDatabaseAuthentication =
          Prelude.Nothing,
        kmsKeyId = Prelude.Nothing,
        optionGroupName = Prelude.Nothing,
        port = Prelude.Nothing,
        restoreToTime = Prelude.Nothing,
        restoreType = Prelude.Nothing,
        serverlessV2ScalingConfiguration =
          Prelude.Nothing,
        tags = Prelude.Nothing,
        useLatestRestorableTime = Prelude.Nothing,
        vpcSecurityGroupIds = Prelude.Nothing,
        dbClusterIdentifier = pDBClusterIdentifier_,
        sourceDBClusterIdentifier =
          pSourceDBClusterIdentifier_
      }

-- | The name of the DB cluster parameter group to associate with the new DB
-- cluster.
--
-- Constraints:
--
-- -   If supplied, must match the name of an existing
--     DBClusterParameterGroup.
restoreDBClusterToPointInTime_dbClusterParameterGroupName :: Lens.Lens' RestoreDBClusterToPointInTime (Prelude.Maybe Prelude.Text)
restoreDBClusterToPointInTime_dbClusterParameterGroupName = Lens.lens (\RestoreDBClusterToPointInTime' {dbClusterParameterGroupName} -> dbClusterParameterGroupName) (\s@RestoreDBClusterToPointInTime' {} a -> s {dbClusterParameterGroupName = a} :: RestoreDBClusterToPointInTime)

-- | The DB subnet group name to use for the new DB cluster.
--
-- Constraints: If supplied, must match the name of an existing
-- DBSubnetGroup.
--
-- Example: @mySubnetgroup@
restoreDBClusterToPointInTime_dbSubnetGroupName :: Lens.Lens' RestoreDBClusterToPointInTime (Prelude.Maybe Prelude.Text)
restoreDBClusterToPointInTime_dbSubnetGroupName = Lens.lens (\RestoreDBClusterToPointInTime' {dbSubnetGroupName} -> dbSubnetGroupName) (\s@RestoreDBClusterToPointInTime' {} a -> s {dbSubnetGroupName = a} :: RestoreDBClusterToPointInTime)

-- | A value that indicates whether the DB cluster has deletion protection
-- enabled. The database can\'t be deleted when deletion protection is
-- enabled. By default, deletion protection is disabled.
restoreDBClusterToPointInTime_deletionProtection :: Lens.Lens' RestoreDBClusterToPointInTime (Prelude.Maybe Prelude.Bool)
restoreDBClusterToPointInTime_deletionProtection = Lens.lens (\RestoreDBClusterToPointInTime' {deletionProtection} -> deletionProtection) (\s@RestoreDBClusterToPointInTime' {} a -> s {deletionProtection = a} :: RestoreDBClusterToPointInTime)

-- | The list of logs that the restored DB cluster is to export to CloudWatch
-- Logs.
restoreDBClusterToPointInTime_enableCloudwatchLogsExports :: Lens.Lens' RestoreDBClusterToPointInTime (Prelude.Maybe [Prelude.Text])
restoreDBClusterToPointInTime_enableCloudwatchLogsExports = Lens.lens (\RestoreDBClusterToPointInTime' {enableCloudwatchLogsExports} -> enableCloudwatchLogsExports) (\s@RestoreDBClusterToPointInTime' {} a -> s {enableCloudwatchLogsExports = a} :: RestoreDBClusterToPointInTime) Prelude.. Lens.mapping Lens.coerced

-- | True to enable mapping of Amazon Identity and Access Management (IAM)
-- accounts to database accounts, and otherwise false.
--
-- Default: @false@
restoreDBClusterToPointInTime_enableIAMDatabaseAuthentication :: Lens.Lens' RestoreDBClusterToPointInTime (Prelude.Maybe Prelude.Bool)
restoreDBClusterToPointInTime_enableIAMDatabaseAuthentication = Lens.lens (\RestoreDBClusterToPointInTime' {enableIAMDatabaseAuthentication} -> enableIAMDatabaseAuthentication) (\s@RestoreDBClusterToPointInTime' {} a -> s {enableIAMDatabaseAuthentication = a} :: RestoreDBClusterToPointInTime)

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

-- | /(Not supported by Neptune)/
restoreDBClusterToPointInTime_optionGroupName :: Lens.Lens' RestoreDBClusterToPointInTime (Prelude.Maybe Prelude.Text)
restoreDBClusterToPointInTime_optionGroupName = Lens.lens (\RestoreDBClusterToPointInTime' {optionGroupName} -> optionGroupName) (\s@RestoreDBClusterToPointInTime' {} a -> s {optionGroupName = a} :: RestoreDBClusterToPointInTime)

-- | The port number on which the new DB cluster accepts connections.
--
-- Constraints: Value must be @1150-65535@
--
-- Default: The same port as the original DB cluster.
restoreDBClusterToPointInTime_port :: Lens.Lens' RestoreDBClusterToPointInTime (Prelude.Maybe Prelude.Int)
restoreDBClusterToPointInTime_port = Lens.lens (\RestoreDBClusterToPointInTime' {port} -> port) (\s@RestoreDBClusterToPointInTime' {} a -> s {port = a} :: RestoreDBClusterToPointInTime)

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
restoreDBClusterToPointInTime_restoreToTime = Lens.lens (\RestoreDBClusterToPointInTime' {restoreToTime} -> restoreToTime) (\s@RestoreDBClusterToPointInTime' {} a -> s {restoreToTime = a} :: RestoreDBClusterToPointInTime) Prelude.. Lens.mapping Data._Time

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

-- | Undocumented member.
restoreDBClusterToPointInTime_serverlessV2ScalingConfiguration :: Lens.Lens' RestoreDBClusterToPointInTime (Prelude.Maybe ServerlessV2ScalingConfiguration)
restoreDBClusterToPointInTime_serverlessV2ScalingConfiguration = Lens.lens (\RestoreDBClusterToPointInTime' {serverlessV2ScalingConfiguration} -> serverlessV2ScalingConfiguration) (\s@RestoreDBClusterToPointInTime' {} a -> s {serverlessV2ScalingConfiguration = a} :: RestoreDBClusterToPointInTime)

-- | The tags to be applied to the restored DB cluster.
restoreDBClusterToPointInTime_tags :: Lens.Lens' RestoreDBClusterToPointInTime (Prelude.Maybe [Tag])
restoreDBClusterToPointInTime_tags = Lens.lens (\RestoreDBClusterToPointInTime' {tags} -> tags) (\s@RestoreDBClusterToPointInTime' {} a -> s {tags = a} :: RestoreDBClusterToPointInTime) Prelude.. Lens.mapping Lens.coerced

-- | A value that is set to @true@ to restore the DB cluster to the latest
-- restorable backup time, and @false@ otherwise.
--
-- Default: @false@
--
-- Constraints: Cannot be specified if @RestoreToTime@ parameter is
-- provided.
restoreDBClusterToPointInTime_useLatestRestorableTime :: Lens.Lens' RestoreDBClusterToPointInTime (Prelude.Maybe Prelude.Bool)
restoreDBClusterToPointInTime_useLatestRestorableTime = Lens.lens (\RestoreDBClusterToPointInTime' {useLatestRestorableTime} -> useLatestRestorableTime) (\s@RestoreDBClusterToPointInTime' {} a -> s {useLatestRestorableTime = a} :: RestoreDBClusterToPointInTime)

-- | A list of VPC security groups that the new DB cluster belongs to.
restoreDBClusterToPointInTime_vpcSecurityGroupIds :: Lens.Lens' RestoreDBClusterToPointInTime (Prelude.Maybe [Prelude.Text])
restoreDBClusterToPointInTime_vpcSecurityGroupIds = Lens.lens (\RestoreDBClusterToPointInTime' {vpcSecurityGroupIds} -> vpcSecurityGroupIds) (\s@RestoreDBClusterToPointInTime' {} a -> s {vpcSecurityGroupIds = a} :: RestoreDBClusterToPointInTime) Prelude.. Lens.mapping Lens.coerced

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
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "RestoreDBClusterToPointInTimeResult"
      ( \s h x ->
          RestoreDBClusterToPointInTimeResponse'
            Prelude.<$> (x Data..@? "DBCluster")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    RestoreDBClusterToPointInTime
  where
  hashWithSalt _salt RestoreDBClusterToPointInTime' {..} =
    _salt
      `Prelude.hashWithSalt` dbClusterParameterGroupName
      `Prelude.hashWithSalt` dbSubnetGroupName
      `Prelude.hashWithSalt` deletionProtection
      `Prelude.hashWithSalt` enableCloudwatchLogsExports
      `Prelude.hashWithSalt` enableIAMDatabaseAuthentication
      `Prelude.hashWithSalt` kmsKeyId
      `Prelude.hashWithSalt` optionGroupName
      `Prelude.hashWithSalt` port
      `Prelude.hashWithSalt` restoreToTime
      `Prelude.hashWithSalt` restoreType
      `Prelude.hashWithSalt` serverlessV2ScalingConfiguration
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` useLatestRestorableTime
      `Prelude.hashWithSalt` vpcSecurityGroupIds
      `Prelude.hashWithSalt` dbClusterIdentifier
      `Prelude.hashWithSalt` sourceDBClusterIdentifier

instance Prelude.NFData RestoreDBClusterToPointInTime where
  rnf RestoreDBClusterToPointInTime' {..} =
    Prelude.rnf dbClusterParameterGroupName `Prelude.seq`
      Prelude.rnf dbSubnetGroupName `Prelude.seq`
        Prelude.rnf deletionProtection `Prelude.seq`
          Prelude.rnf enableCloudwatchLogsExports `Prelude.seq`
            Prelude.rnf enableIAMDatabaseAuthentication `Prelude.seq`
              Prelude.rnf kmsKeyId `Prelude.seq`
                Prelude.rnf optionGroupName `Prelude.seq`
                  Prelude.rnf port `Prelude.seq`
                    Prelude.rnf restoreToTime `Prelude.seq`
                      Prelude.rnf restoreType `Prelude.seq`
                        Prelude.rnf serverlessV2ScalingConfiguration `Prelude.seq`
                          Prelude.rnf tags `Prelude.seq`
                            Prelude.rnf useLatestRestorableTime `Prelude.seq`
                              Prelude.rnf vpcSecurityGroupIds `Prelude.seq`
                                Prelude.rnf dbClusterIdentifier `Prelude.seq`
                                  Prelude.rnf sourceDBClusterIdentifier

instance Data.ToHeaders RestoreDBClusterToPointInTime where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath RestoreDBClusterToPointInTime where
  toPath = Prelude.const "/"

instance Data.ToQuery RestoreDBClusterToPointInTime where
  toQuery RestoreDBClusterToPointInTime' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "RestoreDBClusterToPointInTime" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2014-10-31" :: Prelude.ByteString),
        "DBClusterParameterGroupName"
          Data.=: dbClusterParameterGroupName,
        "DBSubnetGroupName" Data.=: dbSubnetGroupName,
        "DeletionProtection" Data.=: deletionProtection,
        "EnableCloudwatchLogsExports"
          Data.=: Data.toQuery
            ( Data.toQueryList "member"
                Prelude.<$> enableCloudwatchLogsExports
            ),
        "EnableIAMDatabaseAuthentication"
          Data.=: enableIAMDatabaseAuthentication,
        "KmsKeyId" Data.=: kmsKeyId,
        "OptionGroupName" Data.=: optionGroupName,
        "Port" Data.=: port,
        "RestoreToTime" Data.=: restoreToTime,
        "RestoreType" Data.=: restoreType,
        "ServerlessV2ScalingConfiguration"
          Data.=: serverlessV2ScalingConfiguration,
        "Tags"
          Data.=: Data.toQuery
            (Data.toQueryList "Tag" Prelude.<$> tags),
        "UseLatestRestorableTime"
          Data.=: useLatestRestorableTime,
        "VpcSecurityGroupIds"
          Data.=: Data.toQuery
            ( Data.toQueryList "VpcSecurityGroupId"
                Prelude.<$> vpcSecurityGroupIds
            ),
        "DBClusterIdentifier" Data.=: dbClusterIdentifier,
        "SourceDBClusterIdentifier"
          Data.=: sourceDBClusterIdentifier
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
    Prelude.rnf dbCluster `Prelude.seq`
      Prelude.rnf httpStatus
