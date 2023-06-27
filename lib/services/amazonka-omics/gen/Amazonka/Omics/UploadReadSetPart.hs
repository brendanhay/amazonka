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
-- Module      : Amazonka.Omics.UploadReadSetPart
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation uploads a specific part of a read set. If you upload a
-- new part using a previously used part number, the previously uploaded
-- part will be overwritten.
module Amazonka.Omics.UploadReadSetPart
  ( -- * Creating a Request
    UploadReadSetPart (..),
    newUploadReadSetPart,

    -- * Request Lenses
    uploadReadSetPart_sequenceStoreId,
    uploadReadSetPart_uploadId,
    uploadReadSetPart_partSource,
    uploadReadSetPart_partNumber,
    uploadReadSetPart_payload,

    -- * Destructuring the Response
    UploadReadSetPartResponse (..),
    newUploadReadSetPartResponse,

    -- * Response Lenses
    uploadReadSetPartResponse_httpStatus,
    uploadReadSetPartResponse_checksum,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Omics.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUploadReadSetPart' smart constructor.
data UploadReadSetPart = UploadReadSetPart'
  { -- | The Sequence Store ID used for the multipart upload.
    sequenceStoreId :: Prelude.Text,
    -- | The ID for the initiated multipart upload.
    uploadId :: Prelude.Text,
    -- | The source file for an upload part.
    partSource :: ReadSetPartSource,
    -- | The number of the part being uploaded.
    partNumber :: Prelude.Natural,
    -- | The read set data to upload for a part.
    payload :: Data.HashedBody
  }
  deriving (Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UploadReadSetPart' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sequenceStoreId', 'uploadReadSetPart_sequenceStoreId' - The Sequence Store ID used for the multipart upload.
--
-- 'uploadId', 'uploadReadSetPart_uploadId' - The ID for the initiated multipart upload.
--
-- 'partSource', 'uploadReadSetPart_partSource' - The source file for an upload part.
--
-- 'partNumber', 'uploadReadSetPart_partNumber' - The number of the part being uploaded.
--
-- 'payload', 'uploadReadSetPart_payload' - The read set data to upload for a part.
newUploadReadSetPart ::
  -- | 'sequenceStoreId'
  Prelude.Text ->
  -- | 'uploadId'
  Prelude.Text ->
  -- | 'partSource'
  ReadSetPartSource ->
  -- | 'partNumber'
  Prelude.Natural ->
  -- | 'payload'
  Data.HashedBody ->
  UploadReadSetPart
newUploadReadSetPart
  pSequenceStoreId_
  pUploadId_
  pPartSource_
  pPartNumber_
  pPayload_ =
    UploadReadSetPart'
      { sequenceStoreId =
          pSequenceStoreId_,
        uploadId = pUploadId_,
        partSource = pPartSource_,
        partNumber = pPartNumber_,
        payload = pPayload_
      }

-- | The Sequence Store ID used for the multipart upload.
uploadReadSetPart_sequenceStoreId :: Lens.Lens' UploadReadSetPart Prelude.Text
uploadReadSetPart_sequenceStoreId = Lens.lens (\UploadReadSetPart' {sequenceStoreId} -> sequenceStoreId) (\s@UploadReadSetPart' {} a -> s {sequenceStoreId = a} :: UploadReadSetPart)

-- | The ID for the initiated multipart upload.
uploadReadSetPart_uploadId :: Lens.Lens' UploadReadSetPart Prelude.Text
uploadReadSetPart_uploadId = Lens.lens (\UploadReadSetPart' {uploadId} -> uploadId) (\s@UploadReadSetPart' {} a -> s {uploadId = a} :: UploadReadSetPart)

-- | The source file for an upload part.
uploadReadSetPart_partSource :: Lens.Lens' UploadReadSetPart ReadSetPartSource
uploadReadSetPart_partSource = Lens.lens (\UploadReadSetPart' {partSource} -> partSource) (\s@UploadReadSetPart' {} a -> s {partSource = a} :: UploadReadSetPart)

-- | The number of the part being uploaded.
uploadReadSetPart_partNumber :: Lens.Lens' UploadReadSetPart Prelude.Natural
uploadReadSetPart_partNumber = Lens.lens (\UploadReadSetPart' {partNumber} -> partNumber) (\s@UploadReadSetPart' {} a -> s {partNumber = a} :: UploadReadSetPart)

-- | The read set data to upload for a part.
uploadReadSetPart_payload :: Lens.Lens' UploadReadSetPart Data.HashedBody
uploadReadSetPart_payload = Lens.lens (\UploadReadSetPart' {payload} -> payload) (\s@UploadReadSetPart' {} a -> s {payload = a} :: UploadReadSetPart)

instance Core.AWSRequest UploadReadSetPart where
  type
    AWSResponse UploadReadSetPart =
      UploadReadSetPartResponse
  request overrides =
    Request.putBody (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UploadReadSetPartResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "checksum")
      )

instance Data.ToBody UploadReadSetPart where
  toBody UploadReadSetPart' {..} = Data.toBody payload

instance Data.ToHeaders UploadReadSetPart where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath UploadReadSetPart where
  toPath UploadReadSetPart' {..} =
    Prelude.mconcat
      [ "/sequencestore/",
        Data.toBS sequenceStoreId,
        "/upload/",
        Data.toBS uploadId,
        "/part"
      ]

instance Data.ToQuery UploadReadSetPart where
  toQuery UploadReadSetPart' {..} =
    Prelude.mconcat
      [ "partSource" Data.=: partSource,
        "partNumber" Data.=: partNumber
      ]

-- | /See:/ 'newUploadReadSetPartResponse' smart constructor.
data UploadReadSetPartResponse = UploadReadSetPartResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | An identifier used to confirm that parts are being added to the intended
    -- upload.
    checksum :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UploadReadSetPartResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'uploadReadSetPartResponse_httpStatus' - The response's http status code.
--
-- 'checksum', 'uploadReadSetPartResponse_checksum' - An identifier used to confirm that parts are being added to the intended
-- upload.
newUploadReadSetPartResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'checksum'
  Prelude.Text ->
  UploadReadSetPartResponse
newUploadReadSetPartResponse pHttpStatus_ pChecksum_ =
  UploadReadSetPartResponse'
    { httpStatus =
        pHttpStatus_,
      checksum = pChecksum_
    }

-- | The response's http status code.
uploadReadSetPartResponse_httpStatus :: Lens.Lens' UploadReadSetPartResponse Prelude.Int
uploadReadSetPartResponse_httpStatus = Lens.lens (\UploadReadSetPartResponse' {httpStatus} -> httpStatus) (\s@UploadReadSetPartResponse' {} a -> s {httpStatus = a} :: UploadReadSetPartResponse)

-- | An identifier used to confirm that parts are being added to the intended
-- upload.
uploadReadSetPartResponse_checksum :: Lens.Lens' UploadReadSetPartResponse Prelude.Text
uploadReadSetPartResponse_checksum = Lens.lens (\UploadReadSetPartResponse' {checksum} -> checksum) (\s@UploadReadSetPartResponse' {} a -> s {checksum = a} :: UploadReadSetPartResponse)

instance Prelude.NFData UploadReadSetPartResponse where
  rnf UploadReadSetPartResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf checksum
