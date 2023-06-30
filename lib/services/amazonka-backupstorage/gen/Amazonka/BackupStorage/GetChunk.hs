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
-- Module      : Amazonka.BackupStorage.GetChunk
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the specified object\'s chunk.
module Amazonka.BackupStorage.GetChunk
  ( -- * Creating a Request
    GetChunk (..),
    newGetChunk,

    -- * Request Lenses
    getChunk_storageJobId,
    getChunk_chunkToken,

    -- * Destructuring the Response
    GetChunkResponse (..),
    newGetChunkResponse,

    -- * Response Lenses
    getChunkResponse_httpStatus,
    getChunkResponse_length,
    getChunkResponse_checksum,
    getChunkResponse_checksumAlgorithm,
    getChunkResponse_data,
  )
where

import Amazonka.BackupStorage.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetChunk' smart constructor.
data GetChunk = GetChunk'
  { -- | Storage job id
    storageJobId :: Prelude.Text,
    -- | Chunk token
    chunkToken :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetChunk' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'storageJobId', 'getChunk_storageJobId' - Storage job id
--
-- 'chunkToken', 'getChunk_chunkToken' - Chunk token
newGetChunk ::
  -- | 'storageJobId'
  Prelude.Text ->
  -- | 'chunkToken'
  Prelude.Text ->
  GetChunk
newGetChunk pStorageJobId_ pChunkToken_ =
  GetChunk'
    { storageJobId = pStorageJobId_,
      chunkToken = pChunkToken_
    }

-- | Storage job id
getChunk_storageJobId :: Lens.Lens' GetChunk Prelude.Text
getChunk_storageJobId = Lens.lens (\GetChunk' {storageJobId} -> storageJobId) (\s@GetChunk' {} a -> s {storageJobId = a} :: GetChunk)

-- | Chunk token
getChunk_chunkToken :: Lens.Lens' GetChunk Prelude.Text
getChunk_chunkToken = Lens.lens (\GetChunk' {chunkToken} -> chunkToken) (\s@GetChunk' {} a -> s {chunkToken = a} :: GetChunk)

instance Core.AWSRequest GetChunk where
  type AWSResponse GetChunk = GetChunkResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveBody
      ( \s h x ->
          GetChunkResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (h Data..# "x-amz-data-length")
            Prelude.<*> (h Data..# "x-amz-checksum")
            Prelude.<*> (h Data..# "x-amz-checksum-algorithm")
            Prelude.<*> (Prelude.pure x)
      )

instance Prelude.Hashable GetChunk where
  hashWithSalt _salt GetChunk' {..} =
    _salt
      `Prelude.hashWithSalt` storageJobId
      `Prelude.hashWithSalt` chunkToken

instance Prelude.NFData GetChunk where
  rnf GetChunk' {..} =
    Prelude.rnf storageJobId
      `Prelude.seq` Prelude.rnf chunkToken

instance Data.ToHeaders GetChunk where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetChunk where
  toPath GetChunk' {..} =
    Prelude.mconcat
      [ "/restore-jobs/",
        Data.toBS storageJobId,
        "/chunk/",
        Data.toBS chunkToken
      ]

instance Data.ToQuery GetChunk where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetChunkResponse' smart constructor.
data GetChunkResponse = GetChunkResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Data length
    length :: Prelude.Integer,
    -- | Data checksum
    checksum :: Prelude.Text,
    -- | Checksum algorithm
    checksumAlgorithm :: DataChecksumAlgorithm,
    -- | Chunk data
    data' :: Data.ResponseBody
  }
  deriving (Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetChunkResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'getChunkResponse_httpStatus' - The response's http status code.
--
-- 'length', 'getChunkResponse_length' - Data length
--
-- 'checksum', 'getChunkResponse_checksum' - Data checksum
--
-- 'checksumAlgorithm', 'getChunkResponse_checksumAlgorithm' - Checksum algorithm
--
-- 'data'', 'getChunkResponse_data' - Chunk data
newGetChunkResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'length'
  Prelude.Integer ->
  -- | 'checksum'
  Prelude.Text ->
  -- | 'checksumAlgorithm'
  DataChecksumAlgorithm ->
  -- | 'data''
  Data.ResponseBody ->
  GetChunkResponse
newGetChunkResponse
  pHttpStatus_
  pLength_
  pChecksum_
  pChecksumAlgorithm_
  pData_ =
    GetChunkResponse'
      { httpStatus = pHttpStatus_,
        length = pLength_,
        checksum = pChecksum_,
        checksumAlgorithm = pChecksumAlgorithm_,
        data' = pData_
      }

-- | The response's http status code.
getChunkResponse_httpStatus :: Lens.Lens' GetChunkResponse Prelude.Int
getChunkResponse_httpStatus = Lens.lens (\GetChunkResponse' {httpStatus} -> httpStatus) (\s@GetChunkResponse' {} a -> s {httpStatus = a} :: GetChunkResponse)

-- | Data length
getChunkResponse_length :: Lens.Lens' GetChunkResponse Prelude.Integer
getChunkResponse_length = Lens.lens (\GetChunkResponse' {length} -> length) (\s@GetChunkResponse' {} a -> s {length = a} :: GetChunkResponse)

-- | Data checksum
getChunkResponse_checksum :: Lens.Lens' GetChunkResponse Prelude.Text
getChunkResponse_checksum = Lens.lens (\GetChunkResponse' {checksum} -> checksum) (\s@GetChunkResponse' {} a -> s {checksum = a} :: GetChunkResponse)

-- | Checksum algorithm
getChunkResponse_checksumAlgorithm :: Lens.Lens' GetChunkResponse DataChecksumAlgorithm
getChunkResponse_checksumAlgorithm = Lens.lens (\GetChunkResponse' {checksumAlgorithm} -> checksumAlgorithm) (\s@GetChunkResponse' {} a -> s {checksumAlgorithm = a} :: GetChunkResponse)

-- | Chunk data
getChunkResponse_data :: Lens.Lens' GetChunkResponse Data.ResponseBody
getChunkResponse_data = Lens.lens (\GetChunkResponse' {data'} -> data') (\s@GetChunkResponse' {} a -> s {data' = a} :: GetChunkResponse)
