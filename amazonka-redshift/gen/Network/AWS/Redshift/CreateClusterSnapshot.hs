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
import qualified Network.AWS.Prelude as Prelude
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
    manualSnapshotRetentionPeriod :: Prelude.Maybe Prelude.Int,
    -- | A list of tag instances.
    tags :: Prelude.Maybe [Tag],
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
    snapshotIdentifier :: Prelude.Text,
    -- | The cluster identifier for which you want a snapshot.
    clusterIdentifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'clusterIdentifier'
  Prelude.Text ->
  CreateClusterSnapshot
newCreateClusterSnapshot
  pSnapshotIdentifier_
  pClusterIdentifier_ =
    CreateClusterSnapshot'
      { manualSnapshotRetentionPeriod =
          Prelude.Nothing,
        tags = Prelude.Nothing,
        snapshotIdentifier = pSnapshotIdentifier_,
        clusterIdentifier = pClusterIdentifier_
      }

-- | The number of days that a manual snapshot is retained. If the value is
-- -1, the manual snapshot is retained indefinitely.
--
-- The value must be either -1 or an integer between 1 and 3,653.
--
-- The default value is -1.
createClusterSnapshot_manualSnapshotRetentionPeriod :: Lens.Lens' CreateClusterSnapshot (Prelude.Maybe Prelude.Int)
createClusterSnapshot_manualSnapshotRetentionPeriod = Lens.lens (\CreateClusterSnapshot' {manualSnapshotRetentionPeriod} -> manualSnapshotRetentionPeriod) (\s@CreateClusterSnapshot' {} a -> s {manualSnapshotRetentionPeriod = a} :: CreateClusterSnapshot)

-- | A list of tag instances.
createClusterSnapshot_tags :: Lens.Lens' CreateClusterSnapshot (Prelude.Maybe [Tag])
createClusterSnapshot_tags = Lens.lens (\CreateClusterSnapshot' {tags} -> tags) (\s@CreateClusterSnapshot' {} a -> s {tags = a} :: CreateClusterSnapshot) Prelude.. Lens.mapping Lens._Coerce

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
createClusterSnapshot_snapshotIdentifier :: Lens.Lens' CreateClusterSnapshot Prelude.Text
createClusterSnapshot_snapshotIdentifier = Lens.lens (\CreateClusterSnapshot' {snapshotIdentifier} -> snapshotIdentifier) (\s@CreateClusterSnapshot' {} a -> s {snapshotIdentifier = a} :: CreateClusterSnapshot)

-- | The cluster identifier for which you want a snapshot.
createClusterSnapshot_clusterIdentifier :: Lens.Lens' CreateClusterSnapshot Prelude.Text
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
            Prelude.<$> (x Core..@? "Snapshot")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateClusterSnapshot

instance Prelude.NFData CreateClusterSnapshot

instance Core.ToHeaders CreateClusterSnapshot where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath CreateClusterSnapshot where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateClusterSnapshot where
  toQuery CreateClusterSnapshot' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("CreateClusterSnapshot" :: Prelude.ByteString),
        "Version"
          Core.=: ("2012-12-01" :: Prelude.ByteString),
        "ManualSnapshotRetentionPeriod"
          Core.=: manualSnapshotRetentionPeriod,
        "Tags"
          Core.=: Core.toQuery
            (Core.toQueryList "Tag" Prelude.<$> tags),
        "SnapshotIdentifier" Core.=: snapshotIdentifier,
        "ClusterIdentifier" Core.=: clusterIdentifier
      ]

-- | /See:/ 'newCreateClusterSnapshotResponse' smart constructor.
data CreateClusterSnapshotResponse = CreateClusterSnapshotResponse'
  { snapshot :: Prelude.Maybe Snapshot,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  CreateClusterSnapshotResponse
newCreateClusterSnapshotResponse pHttpStatus_ =
  CreateClusterSnapshotResponse'
    { snapshot =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
createClusterSnapshotResponse_snapshot :: Lens.Lens' CreateClusterSnapshotResponse (Prelude.Maybe Snapshot)
createClusterSnapshotResponse_snapshot = Lens.lens (\CreateClusterSnapshotResponse' {snapshot} -> snapshot) (\s@CreateClusterSnapshotResponse' {} a -> s {snapshot = a} :: CreateClusterSnapshotResponse)

-- | The response's http status code.
createClusterSnapshotResponse_httpStatus :: Lens.Lens' CreateClusterSnapshotResponse Prelude.Int
createClusterSnapshotResponse_httpStatus = Lens.lens (\CreateClusterSnapshotResponse' {httpStatus} -> httpStatus) (\s@CreateClusterSnapshotResponse' {} a -> s {httpStatus = a} :: CreateClusterSnapshotResponse)

instance Prelude.NFData CreateClusterSnapshotResponse
