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
-- Module      : Amazonka.DocumentDB.RestoreDBClusterToPointInTime
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Restores a cluster to an arbitrary point in time. Users can restore to
-- any point in time before @LatestRestorableTime@ for up to
-- @BackupRetentionPeriod@ days. The target cluster is created from the
-- source cluster with the same configuration as the original cluster,
-- except that the new cluster is created with the default security group.
module Amazonka.DocumentDB.RestoreDBClusterToPointInTime
  ( -- * Creating a Request
    RestoreDBClusterToPointInTime (..),
    newRestoreDBClusterToPointInTime,

    -- * Request Lenses
    restoreDBClusterToPointInTime_dbSubnetGroupName,
    restoreDBClusterToPointInTime_deletionProtection,
    restoreDBClusterToPointInTime_enableCloudwatchLogsExports,
    restoreDBClusterToPointInTime_kmsKeyId,
    restoreDBClusterToPointInTime_port,
    restoreDBClusterToPointInTime_restoreToTime,
    restoreDBClusterToPointInTime_restoreType,
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
import Amazonka.DocumentDB.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Represents the input to RestoreDBClusterToPointInTime.
--
-- /See:/ 'newRestoreDBClusterToPointInTime' smart constructor.
data RestoreDBClusterToPointInTime = RestoreDBClusterToPointInTime'
  { -- | The subnet group name to use for the new cluster.
    --
    -- Constraints: If provided, must match the name of an existing
    -- @DBSubnetGroup@.
    --
    -- Example: @mySubnetgroup@
    dbSubnetGroupName :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether this cluster can be deleted. If @DeletionProtection@
    -- is enabled, the cluster cannot be deleted unless it is modified and
    -- @DeletionProtection@ is disabled. @DeletionProtection@ protects clusters
    -- from being accidentally deleted.
    deletionProtection :: Prelude.Maybe Prelude.Bool,
    -- | A list of log types that must be enabled for exporting to Amazon
    -- CloudWatch Logs.
    enableCloudwatchLogsExports :: Prelude.Maybe [Prelude.Text],
    -- | The KMS key identifier to use when restoring an encrypted cluster from
    -- an encrypted cluster.
    --
    -- The KMS key identifier is the Amazon Resource Name (ARN) for the KMS
    -- encryption key. If you are restoring a cluster with the same Amazon Web
    -- Services account that owns the KMS encryption key used to encrypt the
    -- new cluster, then you can use the KMS key alias instead of the ARN for
    -- the KMS encryption key.
    --
    -- You can restore to a new cluster and encrypt the new cluster with an KMS
    -- key that is different from the KMS key used to encrypt the source
    -- cluster. The new DB cluster is encrypted with the KMS key identified by
    -- the @KmsKeyId@ parameter.
    --
    -- If you do not specify a value for the @KmsKeyId@ parameter, then the
    -- following occurs:
    --
    -- -   If the cluster is encrypted, then the restored cluster is encrypted
    --     using the KMS key that was used to encrypt the source cluster.
    --
    -- -   If the cluster is not encrypted, then the restored cluster is not
    --     encrypted.
    --
    -- If @DBClusterIdentifier@ refers to a cluster that is not encrypted, then
    -- the restore request is rejected.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | The port number on which the new cluster accepts connections.
    --
    -- Constraints: Must be a value from @1150@ to @65535@.
    --
    -- Default: The default port for the engine.
    port :: Prelude.Maybe Prelude.Int,
    -- | The date and time to restore the cluster to.
    --
    -- Valid values: A time in Universal Coordinated Time (UTC) format.
    --
    -- Constraints:
    --
    -- -   Must be before the latest restorable time for the instance.
    --
    -- -   Must be specified if the @UseLatestRestorableTime@ parameter is not
    --     provided.
    --
    -- -   Cannot be specified if the @UseLatestRestorableTime@ parameter is
    --     @true@.
    --
    -- -   Cannot be specified if the @RestoreType@ parameter is
    --     @copy-on-write@.
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
    -- | The tags to be assigned to the restored cluster.
    tags :: Prelude.Maybe [Tag],
    -- | A value that is set to @true@ to restore the cluster to the latest
    -- restorable backup time, and @false@ otherwise.
    --
    -- Default: @false@
    --
    -- Constraints: Cannot be specified if the @RestoreToTime@ parameter is
    -- provided.
    useLatestRestorableTime :: Prelude.Maybe Prelude.Bool,
    -- | A list of VPC security groups that the new cluster belongs to.
    vpcSecurityGroupIds :: Prelude.Maybe [Prelude.Text],
    -- | The name of the new cluster to be created.
    --
    -- Constraints:
    --
    -- -   Must contain from 1 to 63 letters, numbers, or hyphens.
    --
    -- -   The first character must be a letter.
    --
    -- -   Cannot end with a hyphen or contain two consecutive hyphens.
    dbClusterIdentifier :: Prelude.Text,
    -- | The identifier of the source cluster from which to restore.
    --
    -- Constraints:
    --
    -- -   Must match the identifier of an existing @DBCluster@.
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
-- 'dbSubnetGroupName', 'restoreDBClusterToPointInTime_dbSubnetGroupName' - The subnet group name to use for the new cluster.
--
-- Constraints: If provided, must match the name of an existing
-- @DBSubnetGroup@.
--
-- Example: @mySubnetgroup@
--
-- 'deletionProtection', 'restoreDBClusterToPointInTime_deletionProtection' - Specifies whether this cluster can be deleted. If @DeletionProtection@
-- is enabled, the cluster cannot be deleted unless it is modified and
-- @DeletionProtection@ is disabled. @DeletionProtection@ protects clusters
-- from being accidentally deleted.
--
-- 'enableCloudwatchLogsExports', 'restoreDBClusterToPointInTime_enableCloudwatchLogsExports' - A list of log types that must be enabled for exporting to Amazon
-- CloudWatch Logs.
--
-- 'kmsKeyId', 'restoreDBClusterToPointInTime_kmsKeyId' - The KMS key identifier to use when restoring an encrypted cluster from
-- an encrypted cluster.
--
-- The KMS key identifier is the Amazon Resource Name (ARN) for the KMS
-- encryption key. If you are restoring a cluster with the same Amazon Web
-- Services account that owns the KMS encryption key used to encrypt the
-- new cluster, then you can use the KMS key alias instead of the ARN for
-- the KMS encryption key.
--
-- You can restore to a new cluster and encrypt the new cluster with an KMS
-- key that is different from the KMS key used to encrypt the source
-- cluster. The new DB cluster is encrypted with the KMS key identified by
-- the @KmsKeyId@ parameter.
--
-- If you do not specify a value for the @KmsKeyId@ parameter, then the
-- following occurs:
--
-- -   If the cluster is encrypted, then the restored cluster is encrypted
--     using the KMS key that was used to encrypt the source cluster.
--
-- -   If the cluster is not encrypted, then the restored cluster is not
--     encrypted.
--
-- If @DBClusterIdentifier@ refers to a cluster that is not encrypted, then
-- the restore request is rejected.
--
-- 'port', 'restoreDBClusterToPointInTime_port' - The port number on which the new cluster accepts connections.
--
-- Constraints: Must be a value from @1150@ to @65535@.
--
-- Default: The default port for the engine.
--
-- 'restoreToTime', 'restoreDBClusterToPointInTime_restoreToTime' - The date and time to restore the cluster to.
--
-- Valid values: A time in Universal Coordinated Time (UTC) format.
--
-- Constraints:
--
-- -   Must be before the latest restorable time for the instance.
--
-- -   Must be specified if the @UseLatestRestorableTime@ parameter is not
--     provided.
--
-- -   Cannot be specified if the @UseLatestRestorableTime@ parameter is
--     @true@.
--
-- -   Cannot be specified if the @RestoreType@ parameter is
--     @copy-on-write@.
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
-- 'tags', 'restoreDBClusterToPointInTime_tags' - The tags to be assigned to the restored cluster.
--
-- 'useLatestRestorableTime', 'restoreDBClusterToPointInTime_useLatestRestorableTime' - A value that is set to @true@ to restore the cluster to the latest
-- restorable backup time, and @false@ otherwise.
--
-- Default: @false@
--
-- Constraints: Cannot be specified if the @RestoreToTime@ parameter is
-- provided.
--
-- 'vpcSecurityGroupIds', 'restoreDBClusterToPointInTime_vpcSecurityGroupIds' - A list of VPC security groups that the new cluster belongs to.
--
-- 'dbClusterIdentifier', 'restoreDBClusterToPointInTime_dbClusterIdentifier' - The name of the new cluster to be created.
--
-- Constraints:
--
-- -   Must contain from 1 to 63 letters, numbers, or hyphens.
--
-- -   The first character must be a letter.
--
-- -   Cannot end with a hyphen or contain two consecutive hyphens.
--
-- 'sourceDBClusterIdentifier', 'restoreDBClusterToPointInTime_sourceDBClusterIdentifier' - The identifier of the source cluster from which to restore.
--
-- Constraints:
--
-- -   Must match the identifier of an existing @DBCluster@.
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
      { dbSubnetGroupName =
          Prelude.Nothing,
        deletionProtection = Prelude.Nothing,
        enableCloudwatchLogsExports =
          Prelude.Nothing,
        kmsKeyId = Prelude.Nothing,
        port = Prelude.Nothing,
        restoreToTime = Prelude.Nothing,
        restoreType = Prelude.Nothing,
        tags = Prelude.Nothing,
        useLatestRestorableTime = Prelude.Nothing,
        vpcSecurityGroupIds = Prelude.Nothing,
        dbClusterIdentifier = pDBClusterIdentifier_,
        sourceDBClusterIdentifier =
          pSourceDBClusterIdentifier_
      }

-- | The subnet group name to use for the new cluster.
--
-- Constraints: If provided, must match the name of an existing
-- @DBSubnetGroup@.
--
-- Example: @mySubnetgroup@
restoreDBClusterToPointInTime_dbSubnetGroupName :: Lens.Lens' RestoreDBClusterToPointInTime (Prelude.Maybe Prelude.Text)
restoreDBClusterToPointInTime_dbSubnetGroupName = Lens.lens (\RestoreDBClusterToPointInTime' {dbSubnetGroupName} -> dbSubnetGroupName) (\s@RestoreDBClusterToPointInTime' {} a -> s {dbSubnetGroupName = a} :: RestoreDBClusterToPointInTime)

-- | Specifies whether this cluster can be deleted. If @DeletionProtection@
-- is enabled, the cluster cannot be deleted unless it is modified and
-- @DeletionProtection@ is disabled. @DeletionProtection@ protects clusters
-- from being accidentally deleted.
restoreDBClusterToPointInTime_deletionProtection :: Lens.Lens' RestoreDBClusterToPointInTime (Prelude.Maybe Prelude.Bool)
restoreDBClusterToPointInTime_deletionProtection = Lens.lens (\RestoreDBClusterToPointInTime' {deletionProtection} -> deletionProtection) (\s@RestoreDBClusterToPointInTime' {} a -> s {deletionProtection = a} :: RestoreDBClusterToPointInTime)

-- | A list of log types that must be enabled for exporting to Amazon
-- CloudWatch Logs.
restoreDBClusterToPointInTime_enableCloudwatchLogsExports :: Lens.Lens' RestoreDBClusterToPointInTime (Prelude.Maybe [Prelude.Text])
restoreDBClusterToPointInTime_enableCloudwatchLogsExports = Lens.lens (\RestoreDBClusterToPointInTime' {enableCloudwatchLogsExports} -> enableCloudwatchLogsExports) (\s@RestoreDBClusterToPointInTime' {} a -> s {enableCloudwatchLogsExports = a} :: RestoreDBClusterToPointInTime) Prelude.. Lens.mapping Lens.coerced

-- | The KMS key identifier to use when restoring an encrypted cluster from
-- an encrypted cluster.
--
-- The KMS key identifier is the Amazon Resource Name (ARN) for the KMS
-- encryption key. If you are restoring a cluster with the same Amazon Web
-- Services account that owns the KMS encryption key used to encrypt the
-- new cluster, then you can use the KMS key alias instead of the ARN for
-- the KMS encryption key.
--
-- You can restore to a new cluster and encrypt the new cluster with an KMS
-- key that is different from the KMS key used to encrypt the source
-- cluster. The new DB cluster is encrypted with the KMS key identified by
-- the @KmsKeyId@ parameter.
--
-- If you do not specify a value for the @KmsKeyId@ parameter, then the
-- following occurs:
--
-- -   If the cluster is encrypted, then the restored cluster is encrypted
--     using the KMS key that was used to encrypt the source cluster.
--
-- -   If the cluster is not encrypted, then the restored cluster is not
--     encrypted.
--
-- If @DBClusterIdentifier@ refers to a cluster that is not encrypted, then
-- the restore request is rejected.
restoreDBClusterToPointInTime_kmsKeyId :: Lens.Lens' RestoreDBClusterToPointInTime (Prelude.Maybe Prelude.Text)
restoreDBClusterToPointInTime_kmsKeyId = Lens.lens (\RestoreDBClusterToPointInTime' {kmsKeyId} -> kmsKeyId) (\s@RestoreDBClusterToPointInTime' {} a -> s {kmsKeyId = a} :: RestoreDBClusterToPointInTime)

-- | The port number on which the new cluster accepts connections.
--
-- Constraints: Must be a value from @1150@ to @65535@.
--
-- Default: The default port for the engine.
restoreDBClusterToPointInTime_port :: Lens.Lens' RestoreDBClusterToPointInTime (Prelude.Maybe Prelude.Int)
restoreDBClusterToPointInTime_port = Lens.lens (\RestoreDBClusterToPointInTime' {port} -> port) (\s@RestoreDBClusterToPointInTime' {} a -> s {port = a} :: RestoreDBClusterToPointInTime)

-- | The date and time to restore the cluster to.
--
-- Valid values: A time in Universal Coordinated Time (UTC) format.
--
-- Constraints:
--
-- -   Must be before the latest restorable time for the instance.
--
-- -   Must be specified if the @UseLatestRestorableTime@ parameter is not
--     provided.
--
-- -   Cannot be specified if the @UseLatestRestorableTime@ parameter is
--     @true@.
--
-- -   Cannot be specified if the @RestoreType@ parameter is
--     @copy-on-write@.
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

-- | The tags to be assigned to the restored cluster.
restoreDBClusterToPointInTime_tags :: Lens.Lens' RestoreDBClusterToPointInTime (Prelude.Maybe [Tag])
restoreDBClusterToPointInTime_tags = Lens.lens (\RestoreDBClusterToPointInTime' {tags} -> tags) (\s@RestoreDBClusterToPointInTime' {} a -> s {tags = a} :: RestoreDBClusterToPointInTime) Prelude.. Lens.mapping Lens.coerced

-- | A value that is set to @true@ to restore the cluster to the latest
-- restorable backup time, and @false@ otherwise.
--
-- Default: @false@
--
-- Constraints: Cannot be specified if the @RestoreToTime@ parameter is
-- provided.
restoreDBClusterToPointInTime_useLatestRestorableTime :: Lens.Lens' RestoreDBClusterToPointInTime (Prelude.Maybe Prelude.Bool)
restoreDBClusterToPointInTime_useLatestRestorableTime = Lens.lens (\RestoreDBClusterToPointInTime' {useLatestRestorableTime} -> useLatestRestorableTime) (\s@RestoreDBClusterToPointInTime' {} a -> s {useLatestRestorableTime = a} :: RestoreDBClusterToPointInTime)

-- | A list of VPC security groups that the new cluster belongs to.
restoreDBClusterToPointInTime_vpcSecurityGroupIds :: Lens.Lens' RestoreDBClusterToPointInTime (Prelude.Maybe [Prelude.Text])
restoreDBClusterToPointInTime_vpcSecurityGroupIds = Lens.lens (\RestoreDBClusterToPointInTime' {vpcSecurityGroupIds} -> vpcSecurityGroupIds) (\s@RestoreDBClusterToPointInTime' {} a -> s {vpcSecurityGroupIds = a} :: RestoreDBClusterToPointInTime) Prelude.. Lens.mapping Lens.coerced

-- | The name of the new cluster to be created.
--
-- Constraints:
--
-- -   Must contain from 1 to 63 letters, numbers, or hyphens.
--
-- -   The first character must be a letter.
--
-- -   Cannot end with a hyphen or contain two consecutive hyphens.
restoreDBClusterToPointInTime_dbClusterIdentifier :: Lens.Lens' RestoreDBClusterToPointInTime Prelude.Text
restoreDBClusterToPointInTime_dbClusterIdentifier = Lens.lens (\RestoreDBClusterToPointInTime' {dbClusterIdentifier} -> dbClusterIdentifier) (\s@RestoreDBClusterToPointInTime' {} a -> s {dbClusterIdentifier = a} :: RestoreDBClusterToPointInTime)

-- | The identifier of the source cluster from which to restore.
--
-- Constraints:
--
-- -   Must match the identifier of an existing @DBCluster@.
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
      `Prelude.hashWithSalt` dbSubnetGroupName
      `Prelude.hashWithSalt` deletionProtection
      `Prelude.hashWithSalt` enableCloudwatchLogsExports
      `Prelude.hashWithSalt` kmsKeyId
      `Prelude.hashWithSalt` port
      `Prelude.hashWithSalt` restoreToTime
      `Prelude.hashWithSalt` restoreType
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` useLatestRestorableTime
      `Prelude.hashWithSalt` vpcSecurityGroupIds
      `Prelude.hashWithSalt` dbClusterIdentifier
      `Prelude.hashWithSalt` sourceDBClusterIdentifier

instance Prelude.NFData RestoreDBClusterToPointInTime where
  rnf RestoreDBClusterToPointInTime' {..} =
    Prelude.rnf dbSubnetGroupName `Prelude.seq`
      Prelude.rnf deletionProtection `Prelude.seq`
        Prelude.rnf enableCloudwatchLogsExports `Prelude.seq`
          Prelude.rnf kmsKeyId `Prelude.seq`
            Prelude.rnf port `Prelude.seq`
              Prelude.rnf restoreToTime `Prelude.seq`
                Prelude.rnf restoreType `Prelude.seq`
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
        "DBSubnetGroupName" Data.=: dbSubnetGroupName,
        "DeletionProtection" Data.=: deletionProtection,
        "EnableCloudwatchLogsExports"
          Data.=: Data.toQuery
            ( Data.toQueryList "member"
                Prelude.<$> enableCloudwatchLogsExports
            ),
        "KmsKeyId" Data.=: kmsKeyId,
        "Port" Data.=: port,
        "RestoreToTime" Data.=: restoreToTime,
        "RestoreType" Data.=: restoreType,
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
