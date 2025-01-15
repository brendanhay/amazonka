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
-- Module      : Amazonka.Redshift.CopyClusterSnapshot
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Copies the specified automated cluster snapshot to a new manual cluster
-- snapshot. The source must be an automated snapshot and it must be in the
-- available state.
--
-- When you delete a cluster, Amazon Redshift deletes any automated
-- snapshots of the cluster. Also, when the retention period of the
-- snapshot expires, Amazon Redshift automatically deletes it. If you want
-- to keep an automated snapshot for a longer period, you can make a manual
-- copy of the snapshot. Manual snapshots are retained until you delete
-- them.
--
-- For more information about working with snapshots, go to
-- <https://docs.aws.amazon.com/redshift/latest/mgmt/working-with-snapshots.html Amazon Redshift Snapshots>
-- in the /Amazon Redshift Cluster Management Guide/.
module Amazonka.Redshift.CopyClusterSnapshot
  ( -- * Creating a Request
    CopyClusterSnapshot (..),
    newCopyClusterSnapshot,

    -- * Request Lenses
    copyClusterSnapshot_manualSnapshotRetentionPeriod,
    copyClusterSnapshot_sourceSnapshotClusterIdentifier,
    copyClusterSnapshot_sourceSnapshotIdentifier,
    copyClusterSnapshot_targetSnapshotIdentifier,

    -- * Destructuring the Response
    CopyClusterSnapshotResponse (..),
    newCopyClusterSnapshotResponse,

    -- * Response Lenses
    copyClusterSnapshotResponse_snapshot,
    copyClusterSnapshotResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Redshift.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- |
--
-- /See:/ 'newCopyClusterSnapshot' smart constructor.
data CopyClusterSnapshot = CopyClusterSnapshot'
  { -- | The number of days that a manual snapshot is retained. If the value is
    -- -1, the manual snapshot is retained indefinitely.
    --
    -- The value must be either -1 or an integer between 1 and 3,653.
    --
    -- The default value is -1.
    manualSnapshotRetentionPeriod :: Prelude.Maybe Prelude.Int,
    -- | The identifier of the cluster the source snapshot was created from. This
    -- parameter is required if your IAM user has a policy containing a
    -- snapshot resource element that specifies anything other than * for the
    -- cluster name.
    --
    -- Constraints:
    --
    -- -   Must be the identifier for a valid cluster.
    sourceSnapshotClusterIdentifier :: Prelude.Maybe Prelude.Text,
    -- | The identifier for the source snapshot.
    --
    -- Constraints:
    --
    -- -   Must be the identifier for a valid automated snapshot whose state is
    --     @available@.
    sourceSnapshotIdentifier :: Prelude.Text,
    -- | The identifier given to the new manual snapshot.
    --
    -- Constraints:
    --
    -- -   Cannot be null, empty, or blank.
    --
    -- -   Must contain from 1 to 255 alphanumeric characters or hyphens.
    --
    -- -   First character must be a letter.
    --
    -- -   Cannot end with a hyphen or contain two consecutive hyphens.
    --
    -- -   Must be unique for the Amazon Web Services account that is making
    --     the request.
    targetSnapshotIdentifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CopyClusterSnapshot' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'manualSnapshotRetentionPeriod', 'copyClusterSnapshot_manualSnapshotRetentionPeriod' - The number of days that a manual snapshot is retained. If the value is
-- -1, the manual snapshot is retained indefinitely.
--
-- The value must be either -1 or an integer between 1 and 3,653.
--
-- The default value is -1.
--
-- 'sourceSnapshotClusterIdentifier', 'copyClusterSnapshot_sourceSnapshotClusterIdentifier' - The identifier of the cluster the source snapshot was created from. This
-- parameter is required if your IAM user has a policy containing a
-- snapshot resource element that specifies anything other than * for the
-- cluster name.
--
-- Constraints:
--
-- -   Must be the identifier for a valid cluster.
--
-- 'sourceSnapshotIdentifier', 'copyClusterSnapshot_sourceSnapshotIdentifier' - The identifier for the source snapshot.
--
-- Constraints:
--
-- -   Must be the identifier for a valid automated snapshot whose state is
--     @available@.
--
-- 'targetSnapshotIdentifier', 'copyClusterSnapshot_targetSnapshotIdentifier' - The identifier given to the new manual snapshot.
--
-- Constraints:
--
-- -   Cannot be null, empty, or blank.
--
-- -   Must contain from 1 to 255 alphanumeric characters or hyphens.
--
-- -   First character must be a letter.
--
-- -   Cannot end with a hyphen or contain two consecutive hyphens.
--
-- -   Must be unique for the Amazon Web Services account that is making
--     the request.
newCopyClusterSnapshot ::
  -- | 'sourceSnapshotIdentifier'
  Prelude.Text ->
  -- | 'targetSnapshotIdentifier'
  Prelude.Text ->
  CopyClusterSnapshot
newCopyClusterSnapshot
  pSourceSnapshotIdentifier_
  pTargetSnapshotIdentifier_ =
    CopyClusterSnapshot'
      { manualSnapshotRetentionPeriod =
          Prelude.Nothing,
        sourceSnapshotClusterIdentifier = Prelude.Nothing,
        sourceSnapshotIdentifier =
          pSourceSnapshotIdentifier_,
        targetSnapshotIdentifier =
          pTargetSnapshotIdentifier_
      }

-- | The number of days that a manual snapshot is retained. If the value is
-- -1, the manual snapshot is retained indefinitely.
--
-- The value must be either -1 or an integer between 1 and 3,653.
--
-- The default value is -1.
copyClusterSnapshot_manualSnapshotRetentionPeriod :: Lens.Lens' CopyClusterSnapshot (Prelude.Maybe Prelude.Int)
copyClusterSnapshot_manualSnapshotRetentionPeriod = Lens.lens (\CopyClusterSnapshot' {manualSnapshotRetentionPeriod} -> manualSnapshotRetentionPeriod) (\s@CopyClusterSnapshot' {} a -> s {manualSnapshotRetentionPeriod = a} :: CopyClusterSnapshot)

-- | The identifier of the cluster the source snapshot was created from. This
-- parameter is required if your IAM user has a policy containing a
-- snapshot resource element that specifies anything other than * for the
-- cluster name.
--
-- Constraints:
--
-- -   Must be the identifier for a valid cluster.
copyClusterSnapshot_sourceSnapshotClusterIdentifier :: Lens.Lens' CopyClusterSnapshot (Prelude.Maybe Prelude.Text)
copyClusterSnapshot_sourceSnapshotClusterIdentifier = Lens.lens (\CopyClusterSnapshot' {sourceSnapshotClusterIdentifier} -> sourceSnapshotClusterIdentifier) (\s@CopyClusterSnapshot' {} a -> s {sourceSnapshotClusterIdentifier = a} :: CopyClusterSnapshot)

-- | The identifier for the source snapshot.
--
-- Constraints:
--
-- -   Must be the identifier for a valid automated snapshot whose state is
--     @available@.
copyClusterSnapshot_sourceSnapshotIdentifier :: Lens.Lens' CopyClusterSnapshot Prelude.Text
copyClusterSnapshot_sourceSnapshotIdentifier = Lens.lens (\CopyClusterSnapshot' {sourceSnapshotIdentifier} -> sourceSnapshotIdentifier) (\s@CopyClusterSnapshot' {} a -> s {sourceSnapshotIdentifier = a} :: CopyClusterSnapshot)

-- | The identifier given to the new manual snapshot.
--
-- Constraints:
--
-- -   Cannot be null, empty, or blank.
--
-- -   Must contain from 1 to 255 alphanumeric characters or hyphens.
--
-- -   First character must be a letter.
--
-- -   Cannot end with a hyphen or contain two consecutive hyphens.
--
-- -   Must be unique for the Amazon Web Services account that is making
--     the request.
copyClusterSnapshot_targetSnapshotIdentifier :: Lens.Lens' CopyClusterSnapshot Prelude.Text
copyClusterSnapshot_targetSnapshotIdentifier = Lens.lens (\CopyClusterSnapshot' {targetSnapshotIdentifier} -> targetSnapshotIdentifier) (\s@CopyClusterSnapshot' {} a -> s {targetSnapshotIdentifier = a} :: CopyClusterSnapshot)

instance Core.AWSRequest CopyClusterSnapshot where
  type
    AWSResponse CopyClusterSnapshot =
      CopyClusterSnapshotResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "CopyClusterSnapshotResult"
      ( \s h x ->
          CopyClusterSnapshotResponse'
            Prelude.<$> (x Data..@? "Snapshot")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CopyClusterSnapshot where
  hashWithSalt _salt CopyClusterSnapshot' {..} =
    _salt
      `Prelude.hashWithSalt` manualSnapshotRetentionPeriod
      `Prelude.hashWithSalt` sourceSnapshotClusterIdentifier
      `Prelude.hashWithSalt` sourceSnapshotIdentifier
      `Prelude.hashWithSalt` targetSnapshotIdentifier

instance Prelude.NFData CopyClusterSnapshot where
  rnf CopyClusterSnapshot' {..} =
    Prelude.rnf manualSnapshotRetentionPeriod `Prelude.seq`
      Prelude.rnf sourceSnapshotClusterIdentifier `Prelude.seq`
        Prelude.rnf sourceSnapshotIdentifier `Prelude.seq`
          Prelude.rnf targetSnapshotIdentifier

instance Data.ToHeaders CopyClusterSnapshot where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath CopyClusterSnapshot where
  toPath = Prelude.const "/"

instance Data.ToQuery CopyClusterSnapshot where
  toQuery CopyClusterSnapshot' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("CopyClusterSnapshot" :: Prelude.ByteString),
        "Version"
          Data.=: ("2012-12-01" :: Prelude.ByteString),
        "ManualSnapshotRetentionPeriod"
          Data.=: manualSnapshotRetentionPeriod,
        "SourceSnapshotClusterIdentifier"
          Data.=: sourceSnapshotClusterIdentifier,
        "SourceSnapshotIdentifier"
          Data.=: sourceSnapshotIdentifier,
        "TargetSnapshotIdentifier"
          Data.=: targetSnapshotIdentifier
      ]

-- | /See:/ 'newCopyClusterSnapshotResponse' smart constructor.
data CopyClusterSnapshotResponse = CopyClusterSnapshotResponse'
  { snapshot :: Prelude.Maybe Snapshot,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CopyClusterSnapshotResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'snapshot', 'copyClusterSnapshotResponse_snapshot' - Undocumented member.
--
-- 'httpStatus', 'copyClusterSnapshotResponse_httpStatus' - The response's http status code.
newCopyClusterSnapshotResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CopyClusterSnapshotResponse
newCopyClusterSnapshotResponse pHttpStatus_ =
  CopyClusterSnapshotResponse'
    { snapshot =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
copyClusterSnapshotResponse_snapshot :: Lens.Lens' CopyClusterSnapshotResponse (Prelude.Maybe Snapshot)
copyClusterSnapshotResponse_snapshot = Lens.lens (\CopyClusterSnapshotResponse' {snapshot} -> snapshot) (\s@CopyClusterSnapshotResponse' {} a -> s {snapshot = a} :: CopyClusterSnapshotResponse)

-- | The response's http status code.
copyClusterSnapshotResponse_httpStatus :: Lens.Lens' CopyClusterSnapshotResponse Prelude.Int
copyClusterSnapshotResponse_httpStatus = Lens.lens (\CopyClusterSnapshotResponse' {httpStatus} -> httpStatus) (\s@CopyClusterSnapshotResponse' {} a -> s {httpStatus = a} :: CopyClusterSnapshotResponse)

instance Prelude.NFData CopyClusterSnapshotResponse where
  rnf CopyClusterSnapshotResponse' {..} =
    Prelude.rnf snapshot `Prelude.seq`
      Prelude.rnf httpStatus
