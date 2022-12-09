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
-- Module      : Amazonka.EBS.GetSnapshotBlock
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the data in a block in an Amazon Elastic Block Store snapshot.
module Amazonka.EBS.GetSnapshotBlock
  ( -- * Creating a Request
    GetSnapshotBlock (..),
    newGetSnapshotBlock,

    -- * Request Lenses
    getSnapshotBlock_snapshotId,
    getSnapshotBlock_blockIndex,
    getSnapshotBlock_blockToken,

    -- * Destructuring the Response
    GetSnapshotBlockResponse (..),
    newGetSnapshotBlockResponse,

    -- * Response Lenses
    getSnapshotBlockResponse_checksum,
    getSnapshotBlockResponse_checksumAlgorithm,
    getSnapshotBlockResponse_dataLength,
    getSnapshotBlockResponse_httpStatus,
    getSnapshotBlockResponse_blockData,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EBS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetSnapshotBlock' smart constructor.
data GetSnapshotBlock = GetSnapshotBlock'
  { -- | The ID of the snapshot containing the block from which to get data.
    --
    -- If the specified snapshot is encrypted, you must have permission to use
    -- the KMS key that was used to encrypt the snapshot. For more information,
    -- see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ebsapis-using-encryption.html Using encryption>
    -- in the /Amazon Elastic Compute Cloud User Guide/.
    snapshotId :: Prelude.Text,
    -- | The block index of the block in which to read the data. A block index is
    -- a logical index in units of @512@ KiB blocks. To identify the block
    -- index, divide the logical offset of the data in the logical volume by
    -- the block size (logical offset of data\/@524288@). The logical offset of
    -- the data must be @512@ KiB aligned.
    blockIndex :: Prelude.Natural,
    -- | The block token of the block from which to get data. You can obtain the
    -- @BlockToken@ by running the @ListChangedBlocks@ or @ListSnapshotBlocks@
    -- operations.
    blockToken :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetSnapshotBlock' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'snapshotId', 'getSnapshotBlock_snapshotId' - The ID of the snapshot containing the block from which to get data.
--
-- If the specified snapshot is encrypted, you must have permission to use
-- the KMS key that was used to encrypt the snapshot. For more information,
-- see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ebsapis-using-encryption.html Using encryption>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- 'blockIndex', 'getSnapshotBlock_blockIndex' - The block index of the block in which to read the data. A block index is
-- a logical index in units of @512@ KiB blocks. To identify the block
-- index, divide the logical offset of the data in the logical volume by
-- the block size (logical offset of data\/@524288@). The logical offset of
-- the data must be @512@ KiB aligned.
--
-- 'blockToken', 'getSnapshotBlock_blockToken' - The block token of the block from which to get data. You can obtain the
-- @BlockToken@ by running the @ListChangedBlocks@ or @ListSnapshotBlocks@
-- operations.
newGetSnapshotBlock ::
  -- | 'snapshotId'
  Prelude.Text ->
  -- | 'blockIndex'
  Prelude.Natural ->
  -- | 'blockToken'
  Prelude.Text ->
  GetSnapshotBlock
newGetSnapshotBlock
  pSnapshotId_
  pBlockIndex_
  pBlockToken_ =
    GetSnapshotBlock'
      { snapshotId = pSnapshotId_,
        blockIndex = pBlockIndex_,
        blockToken = pBlockToken_
      }

-- | The ID of the snapshot containing the block from which to get data.
--
-- If the specified snapshot is encrypted, you must have permission to use
-- the KMS key that was used to encrypt the snapshot. For more information,
-- see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ebsapis-using-encryption.html Using encryption>
-- in the /Amazon Elastic Compute Cloud User Guide/.
getSnapshotBlock_snapshotId :: Lens.Lens' GetSnapshotBlock Prelude.Text
getSnapshotBlock_snapshotId = Lens.lens (\GetSnapshotBlock' {snapshotId} -> snapshotId) (\s@GetSnapshotBlock' {} a -> s {snapshotId = a} :: GetSnapshotBlock)

-- | The block index of the block in which to read the data. A block index is
-- a logical index in units of @512@ KiB blocks. To identify the block
-- index, divide the logical offset of the data in the logical volume by
-- the block size (logical offset of data\/@524288@). The logical offset of
-- the data must be @512@ KiB aligned.
getSnapshotBlock_blockIndex :: Lens.Lens' GetSnapshotBlock Prelude.Natural
getSnapshotBlock_blockIndex = Lens.lens (\GetSnapshotBlock' {blockIndex} -> blockIndex) (\s@GetSnapshotBlock' {} a -> s {blockIndex = a} :: GetSnapshotBlock)

-- | The block token of the block from which to get data. You can obtain the
-- @BlockToken@ by running the @ListChangedBlocks@ or @ListSnapshotBlocks@
-- operations.
getSnapshotBlock_blockToken :: Lens.Lens' GetSnapshotBlock Prelude.Text
getSnapshotBlock_blockToken = Lens.lens (\GetSnapshotBlock' {blockToken} -> blockToken) (\s@GetSnapshotBlock' {} a -> s {blockToken = a} :: GetSnapshotBlock)

instance Core.AWSRequest GetSnapshotBlock where
  type
    AWSResponse GetSnapshotBlock =
      GetSnapshotBlockResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveBody
      ( \s h x ->
          GetSnapshotBlockResponse'
            Prelude.<$> (h Data..#? "x-amz-Checksum")
            Prelude.<*> (h Data..#? "x-amz-Checksum-Algorithm")
            Prelude.<*> (h Data..#? "x-amz-Data-Length")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (Prelude.pure x)
      )

instance Prelude.Hashable GetSnapshotBlock where
  hashWithSalt _salt GetSnapshotBlock' {..} =
    _salt `Prelude.hashWithSalt` snapshotId
      `Prelude.hashWithSalt` blockIndex
      `Prelude.hashWithSalt` blockToken

instance Prelude.NFData GetSnapshotBlock where
  rnf GetSnapshotBlock' {..} =
    Prelude.rnf snapshotId
      `Prelude.seq` Prelude.rnf blockIndex
      `Prelude.seq` Prelude.rnf blockToken

instance Data.ToHeaders GetSnapshotBlock where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetSnapshotBlock where
  toPath GetSnapshotBlock' {..} =
    Prelude.mconcat
      [ "/snapshots/",
        Data.toBS snapshotId,
        "/blocks/",
        Data.toBS blockIndex
      ]

instance Data.ToQuery GetSnapshotBlock where
  toQuery GetSnapshotBlock' {..} =
    Prelude.mconcat ["blockToken" Data.=: blockToken]

-- | /See:/ 'newGetSnapshotBlockResponse' smart constructor.
data GetSnapshotBlockResponse = GetSnapshotBlockResponse'
  { -- | The checksum generated for the block, which is Base64 encoded.
    checksum :: Prelude.Maybe Prelude.Text,
    -- | The algorithm used to generate the checksum for the block, such as
    -- SHA256.
    checksumAlgorithm :: Prelude.Maybe ChecksumAlgorithm,
    -- | The size of the data in the block.
    dataLength :: Prelude.Maybe Prelude.Int,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The data content of the block.
    blockData :: Data.ResponseBody
  }
  deriving (Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetSnapshotBlockResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'checksum', 'getSnapshotBlockResponse_checksum' - The checksum generated for the block, which is Base64 encoded.
--
-- 'checksumAlgorithm', 'getSnapshotBlockResponse_checksumAlgorithm' - The algorithm used to generate the checksum for the block, such as
-- SHA256.
--
-- 'dataLength', 'getSnapshotBlockResponse_dataLength' - The size of the data in the block.
--
-- 'httpStatus', 'getSnapshotBlockResponse_httpStatus' - The response's http status code.
--
-- 'blockData', 'getSnapshotBlockResponse_blockData' - The data content of the block.
newGetSnapshotBlockResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'blockData'
  Data.ResponseBody ->
  GetSnapshotBlockResponse
newGetSnapshotBlockResponse pHttpStatus_ pBlockData_ =
  GetSnapshotBlockResponse'
    { checksum =
        Prelude.Nothing,
      checksumAlgorithm = Prelude.Nothing,
      dataLength = Prelude.Nothing,
      httpStatus = pHttpStatus_,
      blockData = pBlockData_
    }

-- | The checksum generated for the block, which is Base64 encoded.
getSnapshotBlockResponse_checksum :: Lens.Lens' GetSnapshotBlockResponse (Prelude.Maybe Prelude.Text)
getSnapshotBlockResponse_checksum = Lens.lens (\GetSnapshotBlockResponse' {checksum} -> checksum) (\s@GetSnapshotBlockResponse' {} a -> s {checksum = a} :: GetSnapshotBlockResponse)

-- | The algorithm used to generate the checksum for the block, such as
-- SHA256.
getSnapshotBlockResponse_checksumAlgorithm :: Lens.Lens' GetSnapshotBlockResponse (Prelude.Maybe ChecksumAlgorithm)
getSnapshotBlockResponse_checksumAlgorithm = Lens.lens (\GetSnapshotBlockResponse' {checksumAlgorithm} -> checksumAlgorithm) (\s@GetSnapshotBlockResponse' {} a -> s {checksumAlgorithm = a} :: GetSnapshotBlockResponse)

-- | The size of the data in the block.
getSnapshotBlockResponse_dataLength :: Lens.Lens' GetSnapshotBlockResponse (Prelude.Maybe Prelude.Int)
getSnapshotBlockResponse_dataLength = Lens.lens (\GetSnapshotBlockResponse' {dataLength} -> dataLength) (\s@GetSnapshotBlockResponse' {} a -> s {dataLength = a} :: GetSnapshotBlockResponse)

-- | The response's http status code.
getSnapshotBlockResponse_httpStatus :: Lens.Lens' GetSnapshotBlockResponse Prelude.Int
getSnapshotBlockResponse_httpStatus = Lens.lens (\GetSnapshotBlockResponse' {httpStatus} -> httpStatus) (\s@GetSnapshotBlockResponse' {} a -> s {httpStatus = a} :: GetSnapshotBlockResponse)

-- | The data content of the block.
getSnapshotBlockResponse_blockData :: Lens.Lens' GetSnapshotBlockResponse Data.ResponseBody
getSnapshotBlockResponse_blockData = Lens.lens (\GetSnapshotBlockResponse' {blockData} -> blockData) (\s@GetSnapshotBlockResponse' {} a -> s {blockData = a} :: GetSnapshotBlockResponse)
