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
-- Module      : Network.AWS.Redshift.CreateClusterSnapshot
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a manual snapshot of the specified cluster. The cluster must be
-- in the @available@ state.
--
-- For more information about working with snapshots, go to
-- <https://docs.aws.amazon.com/redshift/latest/mgmt/working-with-snapshots.html Amazon Redshift Snapshots>
-- in the /Amazon Redshift Cluster Management Guide/.
module Network.AWS.Redshift.CreateClusterSnapshot
  ( -- * Creating a Request
    CreateClusterSnapshot (..),
    newCreateClusterSnapshot,

    -- * Request Lenses
    createClusterSnapshot_manualSnapshotRetentionPeriod,
    createClusterSnapshot_tags,
    createClusterSnapshot_snapshotIdentifier,
    createClusterSnapshot_clusterIdentifier,

    -- * Destructuring the Response
    CreateClusterSnapshotResponse (..),
    newCreateClusterSnapshotResponse,

    -- * Response Lenses
    createClusterSnapshotResponse_snapshot,
    createClusterSnapshotResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Redshift.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'newCreateClusterSnapshot' smart constructor.
data CreateClusterSnapshot = CreateClusterSnapshot'
  { -- | The number of days that a manual snapshot is retained. If the value is
    -- -1, the manual snapshot is retained indefinitely.
    --
    -- The value must be either -1 or an integer between 1 and 3,653.
    --
    -- The default value is -1.
    manualSnapshotRetentionPeriod :: Core.Maybe Core.Int,
    -- | A list of tag instances.
    tags :: Core.Maybe [Tag],
    -- | A unique identifier for the snapshot that you are requesting. This
    -- identifier must be unique for all snapshots within the AWS account.
    --
    -- Constraints:
    --
    -- -   Cannot be null, empty, or blank
    --
    -- -   Must contain from 1 to 255 alphanumeric characters or hyphens
    --
    -- -   First character must be a letter
    --
    -- -   Cannot end with a hyphen or contain two consecutive hyphens
    --
    -- Example: @my-snapshot-id@
    snapshotIdentifier :: Core.Text,
    -- | The cluster identifier for which you want a snapshot.
    clusterIdentifier :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateClusterSnapshot' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'manualSnapshotRetentionPeriod', 'createClusterSnapshot_manualSnapshotRetentionPeriod' - The number of days that a manual snapshot is retained. If the value is
-- -1, the manual snapshot is retained indefinitely.
--
-- The value must be either -1 or an integer between 1 and 3,653.
--
-- The default value is -1.
--
-- 'tags', 'createClusterSnapshot_tags' - A list of tag instances.
--
-- 'snapshotIdentifier', 'createClusterSnapshot_snapshotIdentifier' - A unique identifier for the snapshot that you are requesting. This
-- identifier must be unique for all snapshots within the AWS account.
--
-- Constraints:
--
-- -   Cannot be null, empty, or blank
--
-- -   Must contain from 1 to 255 alphanumeric characters or hyphens
--
-- -   First character must be a letter
--
-- -   Cannot end with a hyphen or contain two consecutive hyphens
--
-- Example: @my-snapshot-id@
--
-- 'clusterIdentifier', 'createClusterSnapshot_clusterIdentifier' - The cluster identifier for which you want a snapshot.
newCreateClusterSnapshot ::
  -- | 'snapshotIdentifier'
  Core.Text ->
  -- | 'clusterIdentifier'
  Core.Text ->
  CreateClusterSnapshot
newCreateClusterSnapshot
  pSnapshotIdentifier_
  pClusterIdentifier_ =
    CreateClusterSnapshot'
      { manualSnapshotRetentionPeriod =
          Core.Nothing,
        tags = Core.Nothing,
        snapshotIdentifier = pSnapshotIdentifier_,
        clusterIdentifier = pClusterIdentifier_
      }

-- | The number of days that a manual snapshot is retained. If the value is
-- -1, the manual snapshot is retained indefinitely.
--
-- The value must be either -1 or an integer between 1 and 3,653.
--
-- The default value is -1.
createClusterSnapshot_manualSnapshotRetentionPeriod :: Lens.Lens' CreateClusterSnapshot (Core.Maybe Core.Int)
createClusterSnapshot_manualSnapshotRetentionPeriod = Lens.lens (\CreateClusterSnapshot' {manualSnapshotRetentionPeriod} -> manualSnapshotRetentionPeriod) (\s@CreateClusterSnapshot' {} a -> s {manualSnapshotRetentionPeriod = a} :: CreateClusterSnapshot)

-- | A list of tag instances.
createClusterSnapshot_tags :: Lens.Lens' CreateClusterSnapshot (Core.Maybe [Tag])
createClusterSnapshot_tags = Lens.lens (\CreateClusterSnapshot' {tags} -> tags) (\s@CreateClusterSnapshot' {} a -> s {tags = a} :: CreateClusterSnapshot) Core.. Lens.mapping Lens._Coerce

-- | A unique identifier for the snapshot that you are requesting. This
-- identifier must be unique for all snapshots within the AWS account.
--
-- Constraints:
--
-- -   Cannot be null, empty, or blank
--
-- -   Must contain from 1 to 255 alphanumeric characters or hyphens
--
-- -   First character must be a letter
--
-- -   Cannot end with a hyphen or contain two consecutive hyphens
--
-- Example: @my-snapshot-id@
createClusterSnapshot_snapshotIdentifier :: Lens.Lens' CreateClusterSnapshot Core.Text
createClusterSnapshot_snapshotIdentifier = Lens.lens (\CreateClusterSnapshot' {snapshotIdentifier} -> snapshotIdentifier) (\s@CreateClusterSnapshot' {} a -> s {snapshotIdentifier = a} :: CreateClusterSnapshot)

-- | The cluster identifier for which you want a snapshot.
createClusterSnapshot_clusterIdentifier :: Lens.Lens' CreateClusterSnapshot Core.Text
createClusterSnapshot_clusterIdentifier = Lens.lens (\CreateClusterSnapshot' {clusterIdentifier} -> clusterIdentifier) (\s@CreateClusterSnapshot' {} a -> s {clusterIdentifier = a} :: CreateClusterSnapshot)

instance Core.AWSRequest CreateClusterSnapshot where
  type
    AWSResponse CreateClusterSnapshot =
      CreateClusterSnapshotResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "CreateClusterSnapshotResult"
      ( \s h x ->
          CreateClusterSnapshotResponse'
            Core.<$> (x Core..@? "Snapshot")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreateClusterSnapshot

instance Core.NFData CreateClusterSnapshot

instance Core.ToHeaders CreateClusterSnapshot where
  toHeaders = Core.const Core.mempty

instance Core.ToPath CreateClusterSnapshot where
  toPath = Core.const "/"

instance Core.ToQuery CreateClusterSnapshot where
  toQuery CreateClusterSnapshot' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("CreateClusterSnapshot" :: Core.ByteString),
        "Version" Core.=: ("2012-12-01" :: Core.ByteString),
        "ManualSnapshotRetentionPeriod"
          Core.=: manualSnapshotRetentionPeriod,
        "Tags"
          Core.=: Core.toQuery (Core.toQueryList "Tag" Core.<$> tags),
        "SnapshotIdentifier" Core.=: snapshotIdentifier,
        "ClusterIdentifier" Core.=: clusterIdentifier
      ]

-- | /See:/ 'newCreateClusterSnapshotResponse' smart constructor.
data CreateClusterSnapshotResponse = CreateClusterSnapshotResponse'
  { snapshot :: Core.Maybe Snapshot,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateClusterSnapshotResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'snapshot', 'createClusterSnapshotResponse_snapshot' - Undocumented member.
--
-- 'httpStatus', 'createClusterSnapshotResponse_httpStatus' - The response's http status code.
newCreateClusterSnapshotResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CreateClusterSnapshotResponse
newCreateClusterSnapshotResponse pHttpStatus_ =
  CreateClusterSnapshotResponse'
    { snapshot =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
createClusterSnapshotResponse_snapshot :: Lens.Lens' CreateClusterSnapshotResponse (Core.Maybe Snapshot)
createClusterSnapshotResponse_snapshot = Lens.lens (\CreateClusterSnapshotResponse' {snapshot} -> snapshot) (\s@CreateClusterSnapshotResponse' {} a -> s {snapshot = a} :: CreateClusterSnapshotResponse)

-- | The response's http status code.
createClusterSnapshotResponse_httpStatus :: Lens.Lens' CreateClusterSnapshotResponse Core.Int
createClusterSnapshotResponse_httpStatus = Lens.lens (\CreateClusterSnapshotResponse' {httpStatus} -> httpStatus) (\s@CreateClusterSnapshotResponse' {} a -> s {httpStatus = a} :: CreateClusterSnapshotResponse)

instance Core.NFData CreateClusterSnapshotResponse
