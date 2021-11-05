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
-- Module      : Amazonka.DocumentDB.RestoreDBClusterFromSnapshot
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new cluster from a snapshot or cluster snapshot.
--
-- If a snapshot is specified, the target cluster is created from the
-- source DB snapshot with a default configuration and default security
-- group.
--
-- If a cluster snapshot is specified, the target cluster is created from
-- the source cluster restore point with the same configuration as the
-- original source DB cluster, except that the new cluster is created with
-- the default security group.
module Amazonka.DocumentDB.RestoreDBClusterFromSnapshot
  ( -- * Creating a Request
    RestoreDBClusterFromSnapshot (..),
    newRestoreDBClusterFromSnapshot,

    -- * Request Lenses
    restoreDBClusterFromSnapshot_engineVersion,
    restoreDBClusterFromSnapshot_deletionProtection,
    restoreDBClusterFromSnapshot_dbSubnetGroupName,
    restoreDBClusterFromSnapshot_availabilityZones,
    restoreDBClusterFromSnapshot_kmsKeyId,
    restoreDBClusterFromSnapshot_vpcSecurityGroupIds,
    restoreDBClusterFromSnapshot_tags,
    restoreDBClusterFromSnapshot_port,
    restoreDBClusterFromSnapshot_enableCloudwatchLogsExports,
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
import Amazonka.DocumentDB.Types
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Represents the input to RestoreDBClusterFromSnapshot.
--
-- /See:/ 'newRestoreDBClusterFromSnapshot' smart constructor.
data RestoreDBClusterFromSnapshot = RestoreDBClusterFromSnapshot'
  { -- | The version of the database engine to use for the new cluster.
    engineVersion :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether this cluster can be deleted. If @DeletionProtection@
    -- is enabled, the cluster cannot be deleted unless it is modified and
    -- @DeletionProtection@ is disabled. @DeletionProtection@ protects clusters
    -- from being accidentally deleted.
    deletionProtection :: Prelude.Maybe Prelude.Bool,
    -- | The name of the subnet group to use for the new cluster.
    --
    -- Constraints: If provided, must match the name of an existing
    -- @DBSubnetGroup@.
    --
    -- Example: @mySubnetgroup@
    dbSubnetGroupName :: Prelude.Maybe Prelude.Text,
    -- | Provides the list of Amazon EC2 Availability Zones that instances in the
    -- restored DB cluster can be created in.
    availabilityZones :: Prelude.Maybe [Prelude.Text],
    -- | The KMS key identifier to use when restoring an encrypted cluster from a
    -- DB snapshot or cluster snapshot.
    --
    -- The KMS key identifier is the Amazon Resource Name (ARN) for the KMS
    -- encryption key. If you are restoring a cluster with the same account
    -- that owns the KMS encryption key used to encrypt the new cluster, then
    -- you can use the KMS key alias instead of the ARN for the KMS encryption
    -- key.
    --
    -- If you do not specify a value for the @KmsKeyId@ parameter, then the
    -- following occurs:
    --
    -- -   If the snapshot or cluster snapshot in @SnapshotIdentifier@ is
    --     encrypted, then the restored cluster is encrypted using the KMS key
    --     that was used to encrypt the snapshot or the cluster snapshot.
    --
    -- -   If the snapshot or the cluster snapshot in @SnapshotIdentifier@ is
    --     not encrypted, then the restored DB cluster is not encrypted.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | A list of virtual private cloud (VPC) security groups that the new
    -- cluster will belong to.
    vpcSecurityGroupIds :: Prelude.Maybe [Prelude.Text],
    -- | The tags to be assigned to the restored cluster.
    tags :: Prelude.Maybe [Tag],
    -- | The port number on which the new cluster accepts connections.
    --
    -- Constraints: Must be a value from @1150@ to @65535@.
    --
    -- Default: The same port as the original cluster.
    port :: Prelude.Maybe Prelude.Int,
    -- | A list of log types that must be enabled for exporting to Amazon
    -- CloudWatch Logs.
    enableCloudwatchLogsExports :: Prelude.Maybe [Prelude.Text],
    -- | The name of the cluster to create from the snapshot or cluster snapshot.
    -- This parameter isn\'t case sensitive.
    --
    -- Constraints:
    --
    -- -   Must contain from 1 to 63 letters, numbers, or hyphens.
    --
    -- -   The first character must be a letter.
    --
    -- -   Cannot end with a hyphen or contain two consecutive hyphens.
    --
    -- Example: @my-snapshot-id@
    dbClusterIdentifier :: Prelude.Text,
    -- | The identifier for the snapshot or cluster snapshot to restore from.
    --
    -- You can use either the name or the Amazon Resource Name (ARN) to specify
    -- a cluster snapshot. However, you can use only the ARN to specify a
    -- snapshot.
    --
    -- Constraints:
    --
    -- -   Must match the identifier of an existing snapshot.
    snapshotIdentifier :: Prelude.Text,
    -- | The database engine to use for the new cluster.
    --
    -- Default: The same as source.
    --
    -- Constraint: Must be compatible with the engine of the source.
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
-- 'engineVersion', 'restoreDBClusterFromSnapshot_engineVersion' - The version of the database engine to use for the new cluster.
--
-- 'deletionProtection', 'restoreDBClusterFromSnapshot_deletionProtection' - Specifies whether this cluster can be deleted. If @DeletionProtection@
-- is enabled, the cluster cannot be deleted unless it is modified and
-- @DeletionProtection@ is disabled. @DeletionProtection@ protects clusters
-- from being accidentally deleted.
--
-- 'dbSubnetGroupName', 'restoreDBClusterFromSnapshot_dbSubnetGroupName' - The name of the subnet group to use for the new cluster.
--
-- Constraints: If provided, must match the name of an existing
-- @DBSubnetGroup@.
--
-- Example: @mySubnetgroup@
--
-- 'availabilityZones', 'restoreDBClusterFromSnapshot_availabilityZones' - Provides the list of Amazon EC2 Availability Zones that instances in the
-- restored DB cluster can be created in.
--
-- 'kmsKeyId', 'restoreDBClusterFromSnapshot_kmsKeyId' - The KMS key identifier to use when restoring an encrypted cluster from a
-- DB snapshot or cluster snapshot.
--
-- The KMS key identifier is the Amazon Resource Name (ARN) for the KMS
-- encryption key. If you are restoring a cluster with the same account
-- that owns the KMS encryption key used to encrypt the new cluster, then
-- you can use the KMS key alias instead of the ARN for the KMS encryption
-- key.
--
-- If you do not specify a value for the @KmsKeyId@ parameter, then the
-- following occurs:
--
-- -   If the snapshot or cluster snapshot in @SnapshotIdentifier@ is
--     encrypted, then the restored cluster is encrypted using the KMS key
--     that was used to encrypt the snapshot or the cluster snapshot.
--
-- -   If the snapshot or the cluster snapshot in @SnapshotIdentifier@ is
--     not encrypted, then the restored DB cluster is not encrypted.
--
-- 'vpcSecurityGroupIds', 'restoreDBClusterFromSnapshot_vpcSecurityGroupIds' - A list of virtual private cloud (VPC) security groups that the new
-- cluster will belong to.
--
-- 'tags', 'restoreDBClusterFromSnapshot_tags' - The tags to be assigned to the restored cluster.
--
-- 'port', 'restoreDBClusterFromSnapshot_port' - The port number on which the new cluster accepts connections.
--
-- Constraints: Must be a value from @1150@ to @65535@.
--
-- Default: The same port as the original cluster.
--
-- 'enableCloudwatchLogsExports', 'restoreDBClusterFromSnapshot_enableCloudwatchLogsExports' - A list of log types that must be enabled for exporting to Amazon
-- CloudWatch Logs.
--
-- 'dbClusterIdentifier', 'restoreDBClusterFromSnapshot_dbClusterIdentifier' - The name of the cluster to create from the snapshot or cluster snapshot.
-- This parameter isn\'t case sensitive.
--
-- Constraints:
--
-- -   Must contain from 1 to 63 letters, numbers, or hyphens.
--
-- -   The first character must be a letter.
--
-- -   Cannot end with a hyphen or contain two consecutive hyphens.
--
-- Example: @my-snapshot-id@
--
-- 'snapshotIdentifier', 'restoreDBClusterFromSnapshot_snapshotIdentifier' - The identifier for the snapshot or cluster snapshot to restore from.
--
-- You can use either the name or the Amazon Resource Name (ARN) to specify
-- a cluster snapshot. However, you can use only the ARN to specify a
-- snapshot.
--
-- Constraints:
--
-- -   Must match the identifier of an existing snapshot.
--
-- 'engine', 'restoreDBClusterFromSnapshot_engine' - The database engine to use for the new cluster.
--
-- Default: The same as source.
--
-- Constraint: Must be compatible with the engine of the source.
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
      { engineVersion =
          Prelude.Nothing,
        deletionProtection = Prelude.Nothing,
        dbSubnetGroupName = Prelude.Nothing,
        availabilityZones = Prelude.Nothing,
        kmsKeyId = Prelude.Nothing,
        vpcSecurityGroupIds = Prelude.Nothing,
        tags = Prelude.Nothing,
        port = Prelude.Nothing,
        enableCloudwatchLogsExports = Prelude.Nothing,
        dbClusterIdentifier = pDBClusterIdentifier_,
        snapshotIdentifier = pSnapshotIdentifier_,
        engine = pEngine_
      }

-- | The version of the database engine to use for the new cluster.
restoreDBClusterFromSnapshot_engineVersion :: Lens.Lens' RestoreDBClusterFromSnapshot (Prelude.Maybe Prelude.Text)
restoreDBClusterFromSnapshot_engineVersion = Lens.lens (\RestoreDBClusterFromSnapshot' {engineVersion} -> engineVersion) (\s@RestoreDBClusterFromSnapshot' {} a -> s {engineVersion = a} :: RestoreDBClusterFromSnapshot)

-- | Specifies whether this cluster can be deleted. If @DeletionProtection@
-- is enabled, the cluster cannot be deleted unless it is modified and
-- @DeletionProtection@ is disabled. @DeletionProtection@ protects clusters
-- from being accidentally deleted.
restoreDBClusterFromSnapshot_deletionProtection :: Lens.Lens' RestoreDBClusterFromSnapshot (Prelude.Maybe Prelude.Bool)
restoreDBClusterFromSnapshot_deletionProtection = Lens.lens (\RestoreDBClusterFromSnapshot' {deletionProtection} -> deletionProtection) (\s@RestoreDBClusterFromSnapshot' {} a -> s {deletionProtection = a} :: RestoreDBClusterFromSnapshot)

-- | The name of the subnet group to use for the new cluster.
--
-- Constraints: If provided, must match the name of an existing
-- @DBSubnetGroup@.
--
-- Example: @mySubnetgroup@
restoreDBClusterFromSnapshot_dbSubnetGroupName :: Lens.Lens' RestoreDBClusterFromSnapshot (Prelude.Maybe Prelude.Text)
restoreDBClusterFromSnapshot_dbSubnetGroupName = Lens.lens (\RestoreDBClusterFromSnapshot' {dbSubnetGroupName} -> dbSubnetGroupName) (\s@RestoreDBClusterFromSnapshot' {} a -> s {dbSubnetGroupName = a} :: RestoreDBClusterFromSnapshot)

-- | Provides the list of Amazon EC2 Availability Zones that instances in the
-- restored DB cluster can be created in.
restoreDBClusterFromSnapshot_availabilityZones :: Lens.Lens' RestoreDBClusterFromSnapshot (Prelude.Maybe [Prelude.Text])
restoreDBClusterFromSnapshot_availabilityZones = Lens.lens (\RestoreDBClusterFromSnapshot' {availabilityZones} -> availabilityZones) (\s@RestoreDBClusterFromSnapshot' {} a -> s {availabilityZones = a} :: RestoreDBClusterFromSnapshot) Prelude.. Lens.mapping Lens.coerced

-- | The KMS key identifier to use when restoring an encrypted cluster from a
-- DB snapshot or cluster snapshot.
--
-- The KMS key identifier is the Amazon Resource Name (ARN) for the KMS
-- encryption key. If you are restoring a cluster with the same account
-- that owns the KMS encryption key used to encrypt the new cluster, then
-- you can use the KMS key alias instead of the ARN for the KMS encryption
-- key.
--
-- If you do not specify a value for the @KmsKeyId@ parameter, then the
-- following occurs:
--
-- -   If the snapshot or cluster snapshot in @SnapshotIdentifier@ is
--     encrypted, then the restored cluster is encrypted using the KMS key
--     that was used to encrypt the snapshot or the cluster snapshot.
--
-- -   If the snapshot or the cluster snapshot in @SnapshotIdentifier@ is
--     not encrypted, then the restored DB cluster is not encrypted.
restoreDBClusterFromSnapshot_kmsKeyId :: Lens.Lens' RestoreDBClusterFromSnapshot (Prelude.Maybe Prelude.Text)
restoreDBClusterFromSnapshot_kmsKeyId = Lens.lens (\RestoreDBClusterFromSnapshot' {kmsKeyId} -> kmsKeyId) (\s@RestoreDBClusterFromSnapshot' {} a -> s {kmsKeyId = a} :: RestoreDBClusterFromSnapshot)

-- | A list of virtual private cloud (VPC) security groups that the new
-- cluster will belong to.
restoreDBClusterFromSnapshot_vpcSecurityGroupIds :: Lens.Lens' RestoreDBClusterFromSnapshot (Prelude.Maybe [Prelude.Text])
restoreDBClusterFromSnapshot_vpcSecurityGroupIds = Lens.lens (\RestoreDBClusterFromSnapshot' {vpcSecurityGroupIds} -> vpcSecurityGroupIds) (\s@RestoreDBClusterFromSnapshot' {} a -> s {vpcSecurityGroupIds = a} :: RestoreDBClusterFromSnapshot) Prelude.. Lens.mapping Lens.coerced

-- | The tags to be assigned to the restored cluster.
restoreDBClusterFromSnapshot_tags :: Lens.Lens' RestoreDBClusterFromSnapshot (Prelude.Maybe [Tag])
restoreDBClusterFromSnapshot_tags = Lens.lens (\RestoreDBClusterFromSnapshot' {tags} -> tags) (\s@RestoreDBClusterFromSnapshot' {} a -> s {tags = a} :: RestoreDBClusterFromSnapshot) Prelude.. Lens.mapping Lens.coerced

-- | The port number on which the new cluster accepts connections.
--
-- Constraints: Must be a value from @1150@ to @65535@.
--
-- Default: The same port as the original cluster.
restoreDBClusterFromSnapshot_port :: Lens.Lens' RestoreDBClusterFromSnapshot (Prelude.Maybe Prelude.Int)
restoreDBClusterFromSnapshot_port = Lens.lens (\RestoreDBClusterFromSnapshot' {port} -> port) (\s@RestoreDBClusterFromSnapshot' {} a -> s {port = a} :: RestoreDBClusterFromSnapshot)

-- | A list of log types that must be enabled for exporting to Amazon
-- CloudWatch Logs.
restoreDBClusterFromSnapshot_enableCloudwatchLogsExports :: Lens.Lens' RestoreDBClusterFromSnapshot (Prelude.Maybe [Prelude.Text])
restoreDBClusterFromSnapshot_enableCloudwatchLogsExports = Lens.lens (\RestoreDBClusterFromSnapshot' {enableCloudwatchLogsExports} -> enableCloudwatchLogsExports) (\s@RestoreDBClusterFromSnapshot' {} a -> s {enableCloudwatchLogsExports = a} :: RestoreDBClusterFromSnapshot) Prelude.. Lens.mapping Lens.coerced

-- | The name of the cluster to create from the snapshot or cluster snapshot.
-- This parameter isn\'t case sensitive.
--
-- Constraints:
--
-- -   Must contain from 1 to 63 letters, numbers, or hyphens.
--
-- -   The first character must be a letter.
--
-- -   Cannot end with a hyphen or contain two consecutive hyphens.
--
-- Example: @my-snapshot-id@
restoreDBClusterFromSnapshot_dbClusterIdentifier :: Lens.Lens' RestoreDBClusterFromSnapshot Prelude.Text
restoreDBClusterFromSnapshot_dbClusterIdentifier = Lens.lens (\RestoreDBClusterFromSnapshot' {dbClusterIdentifier} -> dbClusterIdentifier) (\s@RestoreDBClusterFromSnapshot' {} a -> s {dbClusterIdentifier = a} :: RestoreDBClusterFromSnapshot)

-- | The identifier for the snapshot or cluster snapshot to restore from.
--
-- You can use either the name or the Amazon Resource Name (ARN) to specify
-- a cluster snapshot. However, you can use only the ARN to specify a
-- snapshot.
--
-- Constraints:
--
-- -   Must match the identifier of an existing snapshot.
restoreDBClusterFromSnapshot_snapshotIdentifier :: Lens.Lens' RestoreDBClusterFromSnapshot Prelude.Text
restoreDBClusterFromSnapshot_snapshotIdentifier = Lens.lens (\RestoreDBClusterFromSnapshot' {snapshotIdentifier} -> snapshotIdentifier) (\s@RestoreDBClusterFromSnapshot' {} a -> s {snapshotIdentifier = a} :: RestoreDBClusterFromSnapshot)

-- | The database engine to use for the new cluster.
--
-- Default: The same as source.
--
-- Constraint: Must be compatible with the engine of the source.
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
        "EngineVersion" Core.=: engineVersion,
        "DeletionProtection" Core.=: deletionProtection,
        "DBSubnetGroupName" Core.=: dbSubnetGroupName,
        "AvailabilityZones"
          Core.=: Core.toQuery
            ( Core.toQueryList "AvailabilityZone"
                Prelude.<$> availabilityZones
            ),
        "KmsKeyId" Core.=: kmsKeyId,
        "VpcSecurityGroupIds"
          Core.=: Core.toQuery
            ( Core.toQueryList "VpcSecurityGroupId"
                Prelude.<$> vpcSecurityGroupIds
            ),
        "Tags"
          Core.=: Core.toQuery
            (Core.toQueryList "Tag" Prelude.<$> tags),
        "Port" Core.=: port,
        "EnableCloudwatchLogsExports"
          Core.=: Core.toQuery
            ( Core.toQueryList "member"
                Prelude.<$> enableCloudwatchLogsExports
            ),
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
