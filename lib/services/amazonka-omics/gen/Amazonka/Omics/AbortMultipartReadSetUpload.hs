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
-- Module      : Amazonka.Omics.AbortMultipartReadSetUpload
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops a multipart upload.
module Amazonka.Omics.AbortMultipartReadSetUpload
  ( -- * Creating a Request
    AbortMultipartReadSetUpload (..),
    newAbortMultipartReadSetUpload,

    -- * Request Lenses
    abortMultipartReadSetUpload_sequenceStoreId,
    abortMultipartReadSetUpload_uploadId,

    -- * Destructuring the Response
    AbortMultipartReadSetUploadResponse (..),
    newAbortMultipartReadSetUploadResponse,

    -- * Response Lenses
    abortMultipartReadSetUploadResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Omics.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newAbortMultipartReadSetUpload' smart constructor.
data AbortMultipartReadSetUpload = AbortMultipartReadSetUpload'
  { -- | The sequence store ID for the store involved in the multipart upload.
    sequenceStoreId :: Prelude.Text,
    -- | The ID for the multipart upload.
    uploadId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AbortMultipartReadSetUpload' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sequenceStoreId', 'abortMultipartReadSetUpload_sequenceStoreId' - The sequence store ID for the store involved in the multipart upload.
--
-- 'uploadId', 'abortMultipartReadSetUpload_uploadId' - The ID for the multipart upload.
newAbortMultipartReadSetUpload ::
  -- | 'sequenceStoreId'
  Prelude.Text ->
  -- | 'uploadId'
  Prelude.Text ->
  AbortMultipartReadSetUpload
newAbortMultipartReadSetUpload
  pSequenceStoreId_
  pUploadId_ =
    AbortMultipartReadSetUpload'
      { sequenceStoreId =
          pSequenceStoreId_,
        uploadId = pUploadId_
      }

-- | The sequence store ID for the store involved in the multipart upload.
abortMultipartReadSetUpload_sequenceStoreId :: Lens.Lens' AbortMultipartReadSetUpload Prelude.Text
abortMultipartReadSetUpload_sequenceStoreId = Lens.lens (\AbortMultipartReadSetUpload' {sequenceStoreId} -> sequenceStoreId) (\s@AbortMultipartReadSetUpload' {} a -> s {sequenceStoreId = a} :: AbortMultipartReadSetUpload)

-- | The ID for the multipart upload.
abortMultipartReadSetUpload_uploadId :: Lens.Lens' AbortMultipartReadSetUpload Prelude.Text
abortMultipartReadSetUpload_uploadId = Lens.lens (\AbortMultipartReadSetUpload' {uploadId} -> uploadId) (\s@AbortMultipartReadSetUpload' {} a -> s {uploadId = a} :: AbortMultipartReadSetUpload)

instance Core.AWSRequest AbortMultipartReadSetUpload where
  type
    AWSResponse AbortMultipartReadSetUpload =
      AbortMultipartReadSetUploadResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          AbortMultipartReadSetUploadResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable AbortMultipartReadSetUpload where
  hashWithSalt _salt AbortMultipartReadSetUpload' {..} =
    _salt
      `Prelude.hashWithSalt` sequenceStoreId
      `Prelude.hashWithSalt` uploadId

instance Prelude.NFData AbortMultipartReadSetUpload where
  rnf AbortMultipartReadSetUpload' {..} =
    Prelude.rnf sequenceStoreId
      `Prelude.seq` Prelude.rnf uploadId

instance Data.ToHeaders AbortMultipartReadSetUpload where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath AbortMultipartReadSetUpload where
  toPath AbortMultipartReadSetUpload' {..} =
    Prelude.mconcat
      [ "/sequencestore/",
        Data.toBS sequenceStoreId,
        "/upload/",
        Data.toBS uploadId,
        "/abort"
      ]

instance Data.ToQuery AbortMultipartReadSetUpload where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAbortMultipartReadSetUploadResponse' smart constructor.
data AbortMultipartReadSetUploadResponse = AbortMultipartReadSetUploadResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AbortMultipartReadSetUploadResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'abortMultipartReadSetUploadResponse_httpStatus' - The response's http status code.
newAbortMultipartReadSetUploadResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  AbortMultipartReadSetUploadResponse
newAbortMultipartReadSetUploadResponse pHttpStatus_ =
  AbortMultipartReadSetUploadResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
abortMultipartReadSetUploadResponse_httpStatus :: Lens.Lens' AbortMultipartReadSetUploadResponse Prelude.Int
abortMultipartReadSetUploadResponse_httpStatus = Lens.lens (\AbortMultipartReadSetUploadResponse' {httpStatus} -> httpStatus) (\s@AbortMultipartReadSetUploadResponse' {} a -> s {httpStatus = a} :: AbortMultipartReadSetUploadResponse)

instance
  Prelude.NFData
    AbortMultipartReadSetUploadResponse
  where
  rnf AbortMultipartReadSetUploadResponse' {..} =
    Prelude.rnf httpStatus
