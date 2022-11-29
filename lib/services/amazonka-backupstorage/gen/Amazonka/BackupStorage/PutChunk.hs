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
-- Module      : Amazonka.BackupStorage.PutChunk
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Upload chunk.
module Amazonka.BackupStorage.PutChunk
  ( -- * Creating a Request
    PutChunk (..),
    newPutChunk,

    -- * Request Lenses
    putChunk_backupJobId,
    putChunk_uploadId,
    putChunk_chunkIndex,
    putChunk_length,
    putChunk_checksum,
    putChunk_checksumAlgorithm,
    putChunk_data,

    -- * Destructuring the Response
    PutChunkResponse (..),
    newPutChunkResponse,

    -- * Response Lenses
    putChunkResponse_httpStatus,
    putChunkResponse_chunkChecksum,
    putChunkResponse_chunkChecksumAlgorithm,
  )
where

import Amazonka.BackupStorage.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newPutChunk' smart constructor.
data PutChunk = PutChunk'
  { -- | Backup job Id for the in-progress backup.
    backupJobId :: Prelude.Text,
    -- | Upload Id for the in-progress upload.
    uploadId :: Prelude.Text,
    -- | Describes this chunk\'s position relative to the other chunks
    chunkIndex :: Prelude.Integer,
    -- | Data length
    length :: Prelude.Integer,
    -- | Data checksum
    checksum :: Prelude.Text,
    -- | Checksum algorithm
    checksumAlgorithm :: DataChecksumAlgorithm,
    -- | Data to be uploaded
    data' :: Core.HashedBody
  }
  deriving (Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutChunk' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'backupJobId', 'putChunk_backupJobId' - Backup job Id for the in-progress backup.
--
-- 'uploadId', 'putChunk_uploadId' - Upload Id for the in-progress upload.
--
-- 'chunkIndex', 'putChunk_chunkIndex' - Describes this chunk\'s position relative to the other chunks
--
-- 'length', 'putChunk_length' - Data length
--
-- 'checksum', 'putChunk_checksum' - Data checksum
--
-- 'checksumAlgorithm', 'putChunk_checksumAlgorithm' - Checksum algorithm
--
-- 'data'', 'putChunk_data' - Data to be uploaded
newPutChunk ::
  -- | 'backupJobId'
  Prelude.Text ->
  -- | 'uploadId'
  Prelude.Text ->
  -- | 'chunkIndex'
  Prelude.Integer ->
  -- | 'length'
  Prelude.Integer ->
  -- | 'checksum'
  Prelude.Text ->
  -- | 'checksumAlgorithm'
  DataChecksumAlgorithm ->
  -- | 'data''
  Core.HashedBody ->
  PutChunk
newPutChunk
  pBackupJobId_
  pUploadId_
  pChunkIndex_
  pLength_
  pChecksum_
  pChecksumAlgorithm_
  pData_ =
    PutChunk'
      { backupJobId = pBackupJobId_,
        uploadId = pUploadId_,
        chunkIndex = pChunkIndex_,
        length = pLength_,
        checksum = pChecksum_,
        checksumAlgorithm = pChecksumAlgorithm_,
        data' = pData_
      }

-- | Backup job Id for the in-progress backup.
putChunk_backupJobId :: Lens.Lens' PutChunk Prelude.Text
putChunk_backupJobId = Lens.lens (\PutChunk' {backupJobId} -> backupJobId) (\s@PutChunk' {} a -> s {backupJobId = a} :: PutChunk)

-- | Upload Id for the in-progress upload.
putChunk_uploadId :: Lens.Lens' PutChunk Prelude.Text
putChunk_uploadId = Lens.lens (\PutChunk' {uploadId} -> uploadId) (\s@PutChunk' {} a -> s {uploadId = a} :: PutChunk)

-- | Describes this chunk\'s position relative to the other chunks
putChunk_chunkIndex :: Lens.Lens' PutChunk Prelude.Integer
putChunk_chunkIndex = Lens.lens (\PutChunk' {chunkIndex} -> chunkIndex) (\s@PutChunk' {} a -> s {chunkIndex = a} :: PutChunk)

-- | Data length
putChunk_length :: Lens.Lens' PutChunk Prelude.Integer
putChunk_length = Lens.lens (\PutChunk' {length} -> length) (\s@PutChunk' {} a -> s {length = a} :: PutChunk)

-- | Data checksum
putChunk_checksum :: Lens.Lens' PutChunk Prelude.Text
putChunk_checksum = Lens.lens (\PutChunk' {checksum} -> checksum) (\s@PutChunk' {} a -> s {checksum = a} :: PutChunk)

-- | Checksum algorithm
putChunk_checksumAlgorithm :: Lens.Lens' PutChunk DataChecksumAlgorithm
putChunk_checksumAlgorithm = Lens.lens (\PutChunk' {checksumAlgorithm} -> checksumAlgorithm) (\s@PutChunk' {} a -> s {checksumAlgorithm = a} :: PutChunk)

-- | Data to be uploaded
putChunk_data :: Lens.Lens' PutChunk Core.HashedBody
putChunk_data = Lens.lens (\PutChunk' {data'} -> data') (\s@PutChunk' {} a -> s {data' = a} :: PutChunk)

instance Core.AWSRequest PutChunk where
  type AWSResponse PutChunk = PutChunkResponse
  request overrides =
    Request.putBody (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          PutChunkResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..:> "ChunkChecksum")
            Prelude.<*> (x Core..:> "ChunkChecksumAlgorithm")
      )

instance Core.ToBody PutChunk where
  toBody PutChunk' {..} = Core.toBody data'

instance Core.ToHeaders PutChunk where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath PutChunk where
  toPath PutChunk' {..} =
    Prelude.mconcat
      [ "/backup-jobs/",
        Core.toBS backupJobId,
        "/chunk/",
        Core.toBS uploadId,
        "/",
        Core.toBS chunkIndex
      ]

instance Core.ToQuery PutChunk where
  toQuery PutChunk' {..} =
    Prelude.mconcat
      [ "length" Core.=: length,
        "checksum" Core.=: checksum,
        "checksum-algorithm" Core.=: checksumAlgorithm
      ]

-- | /See:/ 'newPutChunkResponse' smart constructor.
data PutChunkResponse = PutChunkResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Chunk checksum
    chunkChecksum :: Prelude.Text,
    -- | Checksum algorithm
    chunkChecksumAlgorithm :: DataChecksumAlgorithm
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutChunkResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'putChunkResponse_httpStatus' - The response's http status code.
--
-- 'chunkChecksum', 'putChunkResponse_chunkChecksum' - Chunk checksum
--
-- 'chunkChecksumAlgorithm', 'putChunkResponse_chunkChecksumAlgorithm' - Checksum algorithm
newPutChunkResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'chunkChecksum'
  Prelude.Text ->
  -- | 'chunkChecksumAlgorithm'
  DataChecksumAlgorithm ->
  PutChunkResponse
newPutChunkResponse
  pHttpStatus_
  pChunkChecksum_
  pChunkChecksumAlgorithm_ =
    PutChunkResponse'
      { httpStatus = pHttpStatus_,
        chunkChecksum = pChunkChecksum_,
        chunkChecksumAlgorithm = pChunkChecksumAlgorithm_
      }

-- | The response's http status code.
putChunkResponse_httpStatus :: Lens.Lens' PutChunkResponse Prelude.Int
putChunkResponse_httpStatus = Lens.lens (\PutChunkResponse' {httpStatus} -> httpStatus) (\s@PutChunkResponse' {} a -> s {httpStatus = a} :: PutChunkResponse)

-- | Chunk checksum
putChunkResponse_chunkChecksum :: Lens.Lens' PutChunkResponse Prelude.Text
putChunkResponse_chunkChecksum = Lens.lens (\PutChunkResponse' {chunkChecksum} -> chunkChecksum) (\s@PutChunkResponse' {} a -> s {chunkChecksum = a} :: PutChunkResponse)

-- | Checksum algorithm
putChunkResponse_chunkChecksumAlgorithm :: Lens.Lens' PutChunkResponse DataChecksumAlgorithm
putChunkResponse_chunkChecksumAlgorithm = Lens.lens (\PutChunkResponse' {chunkChecksumAlgorithm} -> chunkChecksumAlgorithm) (\s@PutChunkResponse' {} a -> s {chunkChecksumAlgorithm = a} :: PutChunkResponse)

instance Prelude.NFData PutChunkResponse where
  rnf PutChunkResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf chunkChecksum
      `Prelude.seq` Prelude.rnf chunkChecksumAlgorithm
