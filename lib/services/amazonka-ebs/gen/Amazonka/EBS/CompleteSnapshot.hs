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
-- Module      : Amazonka.EBS.CompleteSnapshot
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Seals and completes the snapshot after all of the required blocks of
-- data have been written to it. Completing the snapshot changes the status
-- to @completed@. You cannot write new blocks to a snapshot after it has
-- been completed.
module Amazonka.EBS.CompleteSnapshot
  ( -- * Creating a Request
    CompleteSnapshot (..),
    newCompleteSnapshot,

    -- * Request Lenses
    completeSnapshot_checksumAlgorithm,
    completeSnapshot_checksumAggregationMethod,
    completeSnapshot_checksum,
    completeSnapshot_snapshotId,
    completeSnapshot_changedBlocksCount,

    -- * Destructuring the Response
    CompleteSnapshotResponse (..),
    newCompleteSnapshotResponse,

    -- * Response Lenses
    completeSnapshotResponse_status,
    completeSnapshotResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EBS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCompleteSnapshot' smart constructor.
data CompleteSnapshot = CompleteSnapshot'
  { -- | The algorithm used to generate the checksum. Currently, the only
    -- supported algorithm is @SHA256@.
    checksumAlgorithm :: Prelude.Maybe ChecksumAlgorithm,
    -- | The aggregation method used to generate the checksum. Currently, the
    -- only supported aggregation method is @LINEAR@.
    checksumAggregationMethod :: Prelude.Maybe ChecksumAggregationMethod,
    -- | An aggregated Base-64 SHA256 checksum based on the checksums of each
    -- written block.
    --
    -- To generate the aggregated checksum using the linear aggregation method,
    -- arrange the checksums for each written block in ascending order of their
    -- block index, concatenate them to form a single string, and then generate
    -- the checksum on the entire string using the SHA256 algorithm.
    checksum :: Prelude.Maybe Prelude.Text,
    -- | The ID of the snapshot.
    snapshotId :: Prelude.Text,
    -- | The number of blocks that were written to the snapshot.
    changedBlocksCount :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CompleteSnapshot' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'checksumAlgorithm', 'completeSnapshot_checksumAlgorithm' - The algorithm used to generate the checksum. Currently, the only
-- supported algorithm is @SHA256@.
--
-- 'checksumAggregationMethod', 'completeSnapshot_checksumAggregationMethod' - The aggregation method used to generate the checksum. Currently, the
-- only supported aggregation method is @LINEAR@.
--
-- 'checksum', 'completeSnapshot_checksum' - An aggregated Base-64 SHA256 checksum based on the checksums of each
-- written block.
--
-- To generate the aggregated checksum using the linear aggregation method,
-- arrange the checksums for each written block in ascending order of their
-- block index, concatenate them to form a single string, and then generate
-- the checksum on the entire string using the SHA256 algorithm.
--
-- 'snapshotId', 'completeSnapshot_snapshotId' - The ID of the snapshot.
--
-- 'changedBlocksCount', 'completeSnapshot_changedBlocksCount' - The number of blocks that were written to the snapshot.
newCompleteSnapshot ::
  -- | 'snapshotId'
  Prelude.Text ->
  -- | 'changedBlocksCount'
  Prelude.Natural ->
  CompleteSnapshot
newCompleteSnapshot pSnapshotId_ pChangedBlocksCount_ =
  CompleteSnapshot'
    { checksumAlgorithm =
        Prelude.Nothing,
      checksumAggregationMethod = Prelude.Nothing,
      checksum = Prelude.Nothing,
      snapshotId = pSnapshotId_,
      changedBlocksCount = pChangedBlocksCount_
    }

-- | The algorithm used to generate the checksum. Currently, the only
-- supported algorithm is @SHA256@.
completeSnapshot_checksumAlgorithm :: Lens.Lens' CompleteSnapshot (Prelude.Maybe ChecksumAlgorithm)
completeSnapshot_checksumAlgorithm = Lens.lens (\CompleteSnapshot' {checksumAlgorithm} -> checksumAlgorithm) (\s@CompleteSnapshot' {} a -> s {checksumAlgorithm = a} :: CompleteSnapshot)

-- | The aggregation method used to generate the checksum. Currently, the
-- only supported aggregation method is @LINEAR@.
completeSnapshot_checksumAggregationMethod :: Lens.Lens' CompleteSnapshot (Prelude.Maybe ChecksumAggregationMethod)
completeSnapshot_checksumAggregationMethod = Lens.lens (\CompleteSnapshot' {checksumAggregationMethod} -> checksumAggregationMethod) (\s@CompleteSnapshot' {} a -> s {checksumAggregationMethod = a} :: CompleteSnapshot)

-- | An aggregated Base-64 SHA256 checksum based on the checksums of each
-- written block.
--
-- To generate the aggregated checksum using the linear aggregation method,
-- arrange the checksums for each written block in ascending order of their
-- block index, concatenate them to form a single string, and then generate
-- the checksum on the entire string using the SHA256 algorithm.
completeSnapshot_checksum :: Lens.Lens' CompleteSnapshot (Prelude.Maybe Prelude.Text)
completeSnapshot_checksum = Lens.lens (\CompleteSnapshot' {checksum} -> checksum) (\s@CompleteSnapshot' {} a -> s {checksum = a} :: CompleteSnapshot)

-- | The ID of the snapshot.
completeSnapshot_snapshotId :: Lens.Lens' CompleteSnapshot Prelude.Text
completeSnapshot_snapshotId = Lens.lens (\CompleteSnapshot' {snapshotId} -> snapshotId) (\s@CompleteSnapshot' {} a -> s {snapshotId = a} :: CompleteSnapshot)

-- | The number of blocks that were written to the snapshot.
completeSnapshot_changedBlocksCount :: Lens.Lens' CompleteSnapshot Prelude.Natural
completeSnapshot_changedBlocksCount = Lens.lens (\CompleteSnapshot' {changedBlocksCount} -> changedBlocksCount) (\s@CompleteSnapshot' {} a -> s {changedBlocksCount = a} :: CompleteSnapshot)

instance Core.AWSRequest CompleteSnapshot where
  type
    AWSResponse CompleteSnapshot =
      CompleteSnapshotResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CompleteSnapshotResponse'
            Prelude.<$> (x Data..?> "Status")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CompleteSnapshot where
  hashWithSalt _salt CompleteSnapshot' {..} =
    _salt `Prelude.hashWithSalt` checksumAlgorithm
      `Prelude.hashWithSalt` checksumAggregationMethod
      `Prelude.hashWithSalt` checksum
      `Prelude.hashWithSalt` snapshotId
      `Prelude.hashWithSalt` changedBlocksCount

instance Prelude.NFData CompleteSnapshot where
  rnf CompleteSnapshot' {..} =
    Prelude.rnf checksumAlgorithm
      `Prelude.seq` Prelude.rnf checksumAggregationMethod
      `Prelude.seq` Prelude.rnf checksum
      `Prelude.seq` Prelude.rnf snapshotId
      `Prelude.seq` Prelude.rnf changedBlocksCount

instance Data.ToHeaders CompleteSnapshot where
  toHeaders CompleteSnapshot' {..} =
    Prelude.mconcat
      [ "x-amz-Checksum-Algorithm"
          Data.=# checksumAlgorithm,
        "x-amz-Checksum-Aggregation-Method"
          Data.=# checksumAggregationMethod,
        "x-amz-Checksum" Data.=# checksum,
        "x-amz-ChangedBlocksCount"
          Data.=# changedBlocksCount,
        "Content-Type"
          Data.=# ("application/x-amz-json-1.1" :: Prelude.ByteString)
      ]

instance Data.ToJSON CompleteSnapshot where
  toJSON = Prelude.const (Data.Object Prelude.mempty)

instance Data.ToPath CompleteSnapshot where
  toPath CompleteSnapshot' {..} =
    Prelude.mconcat
      ["/snapshots/completion/", Data.toBS snapshotId]

instance Data.ToQuery CompleteSnapshot where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCompleteSnapshotResponse' smart constructor.
data CompleteSnapshotResponse = CompleteSnapshotResponse'
  { -- | The status of the snapshot.
    status :: Prelude.Maybe Status,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CompleteSnapshotResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'completeSnapshotResponse_status' - The status of the snapshot.
--
-- 'httpStatus', 'completeSnapshotResponse_httpStatus' - The response's http status code.
newCompleteSnapshotResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CompleteSnapshotResponse
newCompleteSnapshotResponse pHttpStatus_ =
  CompleteSnapshotResponse'
    { status = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The status of the snapshot.
completeSnapshotResponse_status :: Lens.Lens' CompleteSnapshotResponse (Prelude.Maybe Status)
completeSnapshotResponse_status = Lens.lens (\CompleteSnapshotResponse' {status} -> status) (\s@CompleteSnapshotResponse' {} a -> s {status = a} :: CompleteSnapshotResponse)

-- | The response's http status code.
completeSnapshotResponse_httpStatus :: Lens.Lens' CompleteSnapshotResponse Prelude.Int
completeSnapshotResponse_httpStatus = Lens.lens (\CompleteSnapshotResponse' {httpStatus} -> httpStatus) (\s@CompleteSnapshotResponse' {} a -> s {httpStatus = a} :: CompleteSnapshotResponse)

instance Prelude.NFData CompleteSnapshotResponse where
  rnf CompleteSnapshotResponse' {..} =
    Prelude.rnf status
      `Prelude.seq` Prelude.rnf httpStatus
