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
-- Module      : Amazonka.AuditManager.GetEvidenceFileUploadUrl
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a presigned Amazon S3 URL that can be used to upload a file as
-- manual evidence. For instructions on how to use this operation, see
-- <https://docs.aws.amazon.com/audit-manager/latest/userguide/upload-evidence.html#how-to-upload-manual-evidence-files Upload a file from your browser>
-- in the /Audit Manager User Guide/.
--
-- The following restrictions apply to this operation:
--
-- -   Maximum size of an individual evidence file: 100 MB
--
-- -   Number of daily manual evidence uploads per control: 100
--
-- -   Supported file formats: See
--     <https://docs.aws.amazon.com/audit-manager/latest/userguide/upload-evidence.html#supported-manual-evidence-files Supported file types for manual evidence>
--     in the /Audit Manager User Guide/
--
-- For more information about Audit Manager service restrictions, see
-- <https://docs.aws.amazon.com/audit-manager/latest/userguide/service-quotas.html Quotas and restrictions for Audit Manager>.
module Amazonka.AuditManager.GetEvidenceFileUploadUrl
  ( -- * Creating a Request
    GetEvidenceFileUploadUrl (..),
    newGetEvidenceFileUploadUrl,

    -- * Request Lenses
    getEvidenceFileUploadUrl_fileName,

    -- * Destructuring the Response
    GetEvidenceFileUploadUrlResponse (..),
    newGetEvidenceFileUploadUrlResponse,

    -- * Response Lenses
    getEvidenceFileUploadUrlResponse_evidenceFileName,
    getEvidenceFileUploadUrlResponse_uploadUrl,
    getEvidenceFileUploadUrlResponse_httpStatus,
  )
where

import Amazonka.AuditManager.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetEvidenceFileUploadUrl' smart constructor.
data GetEvidenceFileUploadUrl = GetEvidenceFileUploadUrl'
  { -- | The file that you want to upload. For a list of supported file formats,
    -- see
    -- <https://docs.aws.amazon.com/audit-manager/latest/userguide/upload-evidence.html#supported-manual-evidence-files Supported file types for manual evidence>
    -- in the /Audit Manager User Guide/.
    fileName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetEvidenceFileUploadUrl' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fileName', 'getEvidenceFileUploadUrl_fileName' - The file that you want to upload. For a list of supported file formats,
-- see
-- <https://docs.aws.amazon.com/audit-manager/latest/userguide/upload-evidence.html#supported-manual-evidence-files Supported file types for manual evidence>
-- in the /Audit Manager User Guide/.
newGetEvidenceFileUploadUrl ::
  -- | 'fileName'
  Prelude.Text ->
  GetEvidenceFileUploadUrl
newGetEvidenceFileUploadUrl pFileName_ =
  GetEvidenceFileUploadUrl' {fileName = pFileName_}

-- | The file that you want to upload. For a list of supported file formats,
-- see
-- <https://docs.aws.amazon.com/audit-manager/latest/userguide/upload-evidence.html#supported-manual-evidence-files Supported file types for manual evidence>
-- in the /Audit Manager User Guide/.
getEvidenceFileUploadUrl_fileName :: Lens.Lens' GetEvidenceFileUploadUrl Prelude.Text
getEvidenceFileUploadUrl_fileName = Lens.lens (\GetEvidenceFileUploadUrl' {fileName} -> fileName) (\s@GetEvidenceFileUploadUrl' {} a -> s {fileName = a} :: GetEvidenceFileUploadUrl)

instance Core.AWSRequest GetEvidenceFileUploadUrl where
  type
    AWSResponse GetEvidenceFileUploadUrl =
      GetEvidenceFileUploadUrlResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetEvidenceFileUploadUrlResponse'
            Prelude.<$> (x Data..?> "evidenceFileName")
            Prelude.<*> (x Data..?> "uploadUrl")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetEvidenceFileUploadUrl where
  hashWithSalt _salt GetEvidenceFileUploadUrl' {..} =
    _salt `Prelude.hashWithSalt` fileName

instance Prelude.NFData GetEvidenceFileUploadUrl where
  rnf GetEvidenceFileUploadUrl' {..} =
    Prelude.rnf fileName

instance Data.ToHeaders GetEvidenceFileUploadUrl where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetEvidenceFileUploadUrl where
  toPath = Prelude.const "/evidenceFileUploadUrl"

instance Data.ToQuery GetEvidenceFileUploadUrl where
  toQuery GetEvidenceFileUploadUrl' {..} =
    Prelude.mconcat ["fileName" Data.=: fileName]

-- | /See:/ 'newGetEvidenceFileUploadUrlResponse' smart constructor.
data GetEvidenceFileUploadUrlResponse = GetEvidenceFileUploadUrlResponse'
  { -- | The name of the uploaded manual evidence file that the presigned URL was
    -- generated for.
    evidenceFileName :: Prelude.Maybe Prelude.Text,
    -- | The presigned URL that was generated.
    uploadUrl :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetEvidenceFileUploadUrlResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'evidenceFileName', 'getEvidenceFileUploadUrlResponse_evidenceFileName' - The name of the uploaded manual evidence file that the presigned URL was
-- generated for.
--
-- 'uploadUrl', 'getEvidenceFileUploadUrlResponse_uploadUrl' - The presigned URL that was generated.
--
-- 'httpStatus', 'getEvidenceFileUploadUrlResponse_httpStatus' - The response's http status code.
newGetEvidenceFileUploadUrlResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetEvidenceFileUploadUrlResponse
newGetEvidenceFileUploadUrlResponse pHttpStatus_ =
  GetEvidenceFileUploadUrlResponse'
    { evidenceFileName =
        Prelude.Nothing,
      uploadUrl = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The name of the uploaded manual evidence file that the presigned URL was
-- generated for.
getEvidenceFileUploadUrlResponse_evidenceFileName :: Lens.Lens' GetEvidenceFileUploadUrlResponse (Prelude.Maybe Prelude.Text)
getEvidenceFileUploadUrlResponse_evidenceFileName = Lens.lens (\GetEvidenceFileUploadUrlResponse' {evidenceFileName} -> evidenceFileName) (\s@GetEvidenceFileUploadUrlResponse' {} a -> s {evidenceFileName = a} :: GetEvidenceFileUploadUrlResponse)

-- | The presigned URL that was generated.
getEvidenceFileUploadUrlResponse_uploadUrl :: Lens.Lens' GetEvidenceFileUploadUrlResponse (Prelude.Maybe Prelude.Text)
getEvidenceFileUploadUrlResponse_uploadUrl = Lens.lens (\GetEvidenceFileUploadUrlResponse' {uploadUrl} -> uploadUrl) (\s@GetEvidenceFileUploadUrlResponse' {} a -> s {uploadUrl = a} :: GetEvidenceFileUploadUrlResponse)

-- | The response's http status code.
getEvidenceFileUploadUrlResponse_httpStatus :: Lens.Lens' GetEvidenceFileUploadUrlResponse Prelude.Int
getEvidenceFileUploadUrlResponse_httpStatus = Lens.lens (\GetEvidenceFileUploadUrlResponse' {httpStatus} -> httpStatus) (\s@GetEvidenceFileUploadUrlResponse' {} a -> s {httpStatus = a} :: GetEvidenceFileUploadUrlResponse)

instance
  Prelude.NFData
    GetEvidenceFileUploadUrlResponse
  where
  rnf GetEvidenceFileUploadUrlResponse' {..} =
    Prelude.rnf evidenceFileName
      `Prelude.seq` Prelude.rnf uploadUrl
      `Prelude.seq` Prelude.rnf httpStatus
