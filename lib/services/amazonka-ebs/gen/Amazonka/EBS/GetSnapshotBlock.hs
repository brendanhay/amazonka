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
-- Copyright   : (c) 2013-2021 Brendan Hay
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
    getSnapshotBlockResponse_dataLength,
    getSnapshotBlockResponse_checksumAlgorithm,
    getSnapshotBlockResponse_checksum,
    getSnapshotBlockResponse_httpStatus,
    getSnapshotBlockResponse_blockData,
  )
where

import qualified Amazonka.Core as Core
import Amazonka.EBS.Types
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetSnapshotBlock' smart constructor.
data GetSnapshotBlock = GetSnapshotBlock'
  { -- | The ID of the snapshot containing the block from which to get data.
    snapshotId :: Prelude.Text,
    -- | The block index of the block from which to get data.
    --
    -- Obtain the @BlockIndex@ by running the @ListChangedBlocks@ or
    -- @ListSnapshotBlocks@ operations.
    blockIndex :: Prelude.Natural,
    -- | The block token of the block from which to get data.
    --
    -- Obtain the @BlockToken@ by running the @ListChangedBlocks@ or
    -- @ListSnapshotBlocks@ operations.
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
-- 'blockIndex', 'getSnapshotBlock_blockIndex' - The block index of the block from which to get data.
--
-- Obtain the @BlockIndex@ by running the @ListChangedBlocks@ or
-- @ListSnapshotBlocks@ operations.
--
-- 'blockToken', 'getSnapshotBlock_blockToken' - The block token of the block from which to get data.
--
-- Obtain the @BlockToken@ by running the @ListChangedBlocks@ or
-- @ListSnapshotBlocks@ operations.
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
getSnapshotBlock_snapshotId :: Lens.Lens' GetSnapshotBlock Prelude.Text
getSnapshotBlock_snapshotId = Lens.lens (\GetSnapshotBlock' {snapshotId} -> snapshotId) (\s@GetSnapshotBlock' {} a -> s {snapshotId = a} :: GetSnapshotBlock)

-- | The block index of the block from which to get data.
--
-- Obtain the @BlockIndex@ by running the @ListChangedBlocks@ or
-- @ListSnapshotBlocks@ operations.
getSnapshotBlock_blockIndex :: Lens.Lens' GetSnapshotBlock Prelude.Natural
getSnapshotBlock_blockIndex = Lens.lens (\GetSnapshotBlock' {blockIndex} -> blockIndex) (\s@GetSnapshotBlock' {} a -> s {blockIndex = a} :: GetSnapshotBlock)

-- | The block token of the block from which to get data.
--
-- Obtain the @BlockToken@ by running the @ListChangedBlocks@ or
-- @ListSnapshotBlocks@ operations.
getSnapshotBlock_blockToken :: Lens.Lens' GetSnapshotBlock Prelude.Text
getSnapshotBlock_blockToken = Lens.lens (\GetSnapshotBlock' {blockToken} -> blockToken) (\s@GetSnapshotBlock' {} a -> s {blockToken = a} :: GetSnapshotBlock)

instance Core.AWSRequest GetSnapshotBlock where
  type
    AWSResponse GetSnapshotBlock =
      GetSnapshotBlockResponse
  request = Request.get defaultService
  response =
    Response.receiveBody
      ( \s h x ->
          GetSnapshotBlockResponse'
            Prelude.<$> (h Core..#? "x-amz-Data-Length")
            Prelude.<*> (h Core..#? "x-amz-Checksum-Algorithm")
            Prelude.<*> (h Core..#? "x-amz-Checksum")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (Prelude.pure x)
      )

instance Prelude.Hashable GetSnapshotBlock

instance Prelude.NFData GetSnapshotBlock

instance Core.ToHeaders GetSnapshotBlock where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath GetSnapshotBlock where
  toPath GetSnapshotBlock' {..} =
    Prelude.mconcat
      [ "/snapshots/",
        Core.toBS snapshotId,
        "/blocks/",
        Core.toBS blockIndex
      ]

instance Core.ToQuery GetSnapshotBlock where
  toQuery GetSnapshotBlock' {..} =
    Prelude.mconcat ["blockToken" Core.=: blockToken]

-- | /See:/ 'newGetSnapshotBlockResponse' smart constructor.
data GetSnapshotBlockResponse = GetSnapshotBlockResponse'
  { -- | The size of the data in the block.
    dataLength :: Prelude.Maybe Prelude.Int,
    -- | The algorithm used to generate the checksum for the block, such as
    -- SHA256.
    checksumAlgorithm :: Prelude.Maybe ChecksumAlgorithm,
    -- | The checksum generated for the block, which is Base64 encoded.
    checksum :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The data content of the block.
    blockData :: Core.ResponseBody
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
-- 'dataLength', 'getSnapshotBlockResponse_dataLength' - The size of the data in the block.
--
-- 'checksumAlgorithm', 'getSnapshotBlockResponse_checksumAlgorithm' - The algorithm used to generate the checksum for the block, such as
-- SHA256.
--
-- 'checksum', 'getSnapshotBlockResponse_checksum' - The checksum generated for the block, which is Base64 encoded.
--
-- 'httpStatus', 'getSnapshotBlockResponse_httpStatus' - The response's http status code.
--
-- 'blockData', 'getSnapshotBlockResponse_blockData' - The data content of the block.
newGetSnapshotBlockResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'blockData'
  Core.ResponseBody ->
  GetSnapshotBlockResponse
newGetSnapshotBlockResponse pHttpStatus_ pBlockData_ =
  GetSnapshotBlockResponse'
    { dataLength =
        Prelude.Nothing,
      checksumAlgorithm = Prelude.Nothing,
      checksum = Prelude.Nothing,
      httpStatus = pHttpStatus_,
      blockData = pBlockData_
    }

-- | The size of the data in the block.
getSnapshotBlockResponse_dataLength :: Lens.Lens' GetSnapshotBlockResponse (Prelude.Maybe Prelude.Int)
getSnapshotBlockResponse_dataLength = Lens.lens (\GetSnapshotBlockResponse' {dataLength} -> dataLength) (\s@GetSnapshotBlockResponse' {} a -> s {dataLength = a} :: GetSnapshotBlockResponse)

-- | The algorithm used to generate the checksum for the block, such as
-- SHA256.
getSnapshotBlockResponse_checksumAlgorithm :: Lens.Lens' GetSnapshotBlockResponse (Prelude.Maybe ChecksumAlgorithm)
getSnapshotBlockResponse_checksumAlgorithm = Lens.lens (\GetSnapshotBlockResponse' {checksumAlgorithm} -> checksumAlgorithm) (\s@GetSnapshotBlockResponse' {} a -> s {checksumAlgorithm = a} :: GetSnapshotBlockResponse)

-- | The checksum generated for the block, which is Base64 encoded.
getSnapshotBlockResponse_checksum :: Lens.Lens' GetSnapshotBlockResponse (Prelude.Maybe Prelude.Text)
getSnapshotBlockResponse_checksum = Lens.lens (\GetSnapshotBlockResponse' {checksum} -> checksum) (\s@GetSnapshotBlockResponse' {} a -> s {checksum = a} :: GetSnapshotBlockResponse)

-- | The response's http status code.
getSnapshotBlockResponse_httpStatus :: Lens.Lens' GetSnapshotBlockResponse Prelude.Int
getSnapshotBlockResponse_httpStatus = Lens.lens (\GetSnapshotBlockResponse' {httpStatus} -> httpStatus) (\s@GetSnapshotBlockResponse' {} a -> s {httpStatus = a} :: GetSnapshotBlockResponse)

-- | The data content of the block.
getSnapshotBlockResponse_blockData :: Lens.Lens' GetSnapshotBlockResponse Core.ResponseBody
getSnapshotBlockResponse_blockData = Lens.lens (\GetSnapshotBlockResponse' {blockData} -> blockData) (\s@GetSnapshotBlockResponse' {} a -> s {blockData = a} :: GetSnapshotBlockResponse)
