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
-- Module      : Amazonka.Omics.CompleteMultipartReadSetUpload
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Concludes a multipart upload once you have uploaded all the components.
module Amazonka.Omics.CompleteMultipartReadSetUpload
  ( -- * Creating a Request
    CompleteMultipartReadSetUpload (..),
    newCompleteMultipartReadSetUpload,

    -- * Request Lenses
    completeMultipartReadSetUpload_sequenceStoreId,
    completeMultipartReadSetUpload_uploadId,
    completeMultipartReadSetUpload_parts,

    -- * Destructuring the Response
    CompleteMultipartReadSetUploadResponse (..),
    newCompleteMultipartReadSetUploadResponse,

    -- * Response Lenses
    completeMultipartReadSetUploadResponse_httpStatus,
    completeMultipartReadSetUploadResponse_readSetId,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Omics.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCompleteMultipartReadSetUpload' smart constructor.
data CompleteMultipartReadSetUpload = CompleteMultipartReadSetUpload'
  { -- | The sequence store ID for the store involved in the multipart upload.
    sequenceStoreId :: Prelude.Text,
    -- | The ID for the multipart upload.
    uploadId :: Prelude.Text,
    -- | The individual uploads or parts of a multipart upload.
    parts :: [CompleteReadSetUploadPartListItem]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CompleteMultipartReadSetUpload' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sequenceStoreId', 'completeMultipartReadSetUpload_sequenceStoreId' - The sequence store ID for the store involved in the multipart upload.
--
-- 'uploadId', 'completeMultipartReadSetUpload_uploadId' - The ID for the multipart upload.
--
-- 'parts', 'completeMultipartReadSetUpload_parts' - The individual uploads or parts of a multipart upload.
newCompleteMultipartReadSetUpload ::
  -- | 'sequenceStoreId'
  Prelude.Text ->
  -- | 'uploadId'
  Prelude.Text ->
  CompleteMultipartReadSetUpload
newCompleteMultipartReadSetUpload
  pSequenceStoreId_
  pUploadId_ =
    CompleteMultipartReadSetUpload'
      { sequenceStoreId =
          pSequenceStoreId_,
        uploadId = pUploadId_,
        parts = Prelude.mempty
      }

-- | The sequence store ID for the store involved in the multipart upload.
completeMultipartReadSetUpload_sequenceStoreId :: Lens.Lens' CompleteMultipartReadSetUpload Prelude.Text
completeMultipartReadSetUpload_sequenceStoreId = Lens.lens (\CompleteMultipartReadSetUpload' {sequenceStoreId} -> sequenceStoreId) (\s@CompleteMultipartReadSetUpload' {} a -> s {sequenceStoreId = a} :: CompleteMultipartReadSetUpload)

-- | The ID for the multipart upload.
completeMultipartReadSetUpload_uploadId :: Lens.Lens' CompleteMultipartReadSetUpload Prelude.Text
completeMultipartReadSetUpload_uploadId = Lens.lens (\CompleteMultipartReadSetUpload' {uploadId} -> uploadId) (\s@CompleteMultipartReadSetUpload' {} a -> s {uploadId = a} :: CompleteMultipartReadSetUpload)

-- | The individual uploads or parts of a multipart upload.
completeMultipartReadSetUpload_parts :: Lens.Lens' CompleteMultipartReadSetUpload [CompleteReadSetUploadPartListItem]
completeMultipartReadSetUpload_parts = Lens.lens (\CompleteMultipartReadSetUpload' {parts} -> parts) (\s@CompleteMultipartReadSetUpload' {} a -> s {parts = a} :: CompleteMultipartReadSetUpload) Prelude.. Lens.coerced

instance
  Core.AWSRequest
    CompleteMultipartReadSetUpload
  where
  type
    AWSResponse CompleteMultipartReadSetUpload =
      CompleteMultipartReadSetUploadResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CompleteMultipartReadSetUploadResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "readSetId")
      )

instance
  Prelude.Hashable
    CompleteMultipartReadSetUpload
  where
  hashWithSalt
    _salt
    CompleteMultipartReadSetUpload' {..} =
      _salt
        `Prelude.hashWithSalt` sequenceStoreId
        `Prelude.hashWithSalt` uploadId
        `Prelude.hashWithSalt` parts

instance
  Prelude.NFData
    CompleteMultipartReadSetUpload
  where
  rnf CompleteMultipartReadSetUpload' {..} =
    Prelude.rnf sequenceStoreId
      `Prelude.seq` Prelude.rnf uploadId
      `Prelude.seq` Prelude.rnf parts

instance
  Data.ToHeaders
    CompleteMultipartReadSetUpload
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CompleteMultipartReadSetUpload where
  toJSON CompleteMultipartReadSetUpload' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("parts" Data..= parts)]
      )

instance Data.ToPath CompleteMultipartReadSetUpload where
  toPath CompleteMultipartReadSetUpload' {..} =
    Prelude.mconcat
      [ "/sequencestore/",
        Data.toBS sequenceStoreId,
        "/upload/",
        Data.toBS uploadId,
        "/complete"
      ]

instance Data.ToQuery CompleteMultipartReadSetUpload where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCompleteMultipartReadSetUploadResponse' smart constructor.
data CompleteMultipartReadSetUploadResponse = CompleteMultipartReadSetUploadResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The read set ID created for an uploaded read set.
    readSetId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CompleteMultipartReadSetUploadResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'completeMultipartReadSetUploadResponse_httpStatus' - The response's http status code.
--
-- 'readSetId', 'completeMultipartReadSetUploadResponse_readSetId' - The read set ID created for an uploaded read set.
newCompleteMultipartReadSetUploadResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'readSetId'
  Prelude.Text ->
  CompleteMultipartReadSetUploadResponse
newCompleteMultipartReadSetUploadResponse
  pHttpStatus_
  pReadSetId_ =
    CompleteMultipartReadSetUploadResponse'
      { httpStatus =
          pHttpStatus_,
        readSetId = pReadSetId_
      }

-- | The response's http status code.
completeMultipartReadSetUploadResponse_httpStatus :: Lens.Lens' CompleteMultipartReadSetUploadResponse Prelude.Int
completeMultipartReadSetUploadResponse_httpStatus = Lens.lens (\CompleteMultipartReadSetUploadResponse' {httpStatus} -> httpStatus) (\s@CompleteMultipartReadSetUploadResponse' {} a -> s {httpStatus = a} :: CompleteMultipartReadSetUploadResponse)

-- | The read set ID created for an uploaded read set.
completeMultipartReadSetUploadResponse_readSetId :: Lens.Lens' CompleteMultipartReadSetUploadResponse Prelude.Text
completeMultipartReadSetUploadResponse_readSetId = Lens.lens (\CompleteMultipartReadSetUploadResponse' {readSetId} -> readSetId) (\s@CompleteMultipartReadSetUploadResponse' {} a -> s {readSetId = a} :: CompleteMultipartReadSetUploadResponse)

instance
  Prelude.NFData
    CompleteMultipartReadSetUploadResponse
  where
  rnf CompleteMultipartReadSetUploadResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf readSetId
