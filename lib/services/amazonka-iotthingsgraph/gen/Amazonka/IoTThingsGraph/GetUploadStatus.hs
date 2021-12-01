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
-- Module      : Amazonka.IoTThingsGraph.GetUploadStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the status of the specified upload.
module Amazonka.IoTThingsGraph.GetUploadStatus
  ( -- * Creating a Request
    GetUploadStatus (..),
    newGetUploadStatus,

    -- * Request Lenses
    getUploadStatus_uploadId,

    -- * Destructuring the Response
    GetUploadStatusResponse (..),
    newGetUploadStatusResponse,

    -- * Response Lenses
    getUploadStatusResponse_failureReason,
    getUploadStatusResponse_namespaceArn,
    getUploadStatusResponse_namespaceVersion,
    getUploadStatusResponse_namespaceName,
    getUploadStatusResponse_httpStatus,
    getUploadStatusResponse_uploadId,
    getUploadStatusResponse_uploadStatus,
    getUploadStatusResponse_createdDate,
  )
where

import qualified Amazonka.Core as Core
import Amazonka.IoTThingsGraph.Types
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetUploadStatus' smart constructor.
data GetUploadStatus = GetUploadStatus'
  { -- | The ID of the upload. This value is returned by the
    -- @UploadEntityDefinitions@ action.
    uploadId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetUploadStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'uploadId', 'getUploadStatus_uploadId' - The ID of the upload. This value is returned by the
-- @UploadEntityDefinitions@ action.
newGetUploadStatus ::
  -- | 'uploadId'
  Prelude.Text ->
  GetUploadStatus
newGetUploadStatus pUploadId_ =
  GetUploadStatus' {uploadId = pUploadId_}

-- | The ID of the upload. This value is returned by the
-- @UploadEntityDefinitions@ action.
getUploadStatus_uploadId :: Lens.Lens' GetUploadStatus Prelude.Text
getUploadStatus_uploadId = Lens.lens (\GetUploadStatus' {uploadId} -> uploadId) (\s@GetUploadStatus' {} a -> s {uploadId = a} :: GetUploadStatus)

instance Core.AWSRequest GetUploadStatus where
  type
    AWSResponse GetUploadStatus =
      GetUploadStatusResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetUploadStatusResponse'
            Prelude.<$> (x Core..?> "failureReason" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "namespaceArn")
            Prelude.<*> (x Core..?> "namespaceVersion")
            Prelude.<*> (x Core..?> "namespaceName")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..:> "uploadId")
            Prelude.<*> (x Core..:> "uploadStatus")
            Prelude.<*> (x Core..:> "createdDate")
      )

instance Prelude.Hashable GetUploadStatus where
  hashWithSalt salt' GetUploadStatus' {..} =
    salt' `Prelude.hashWithSalt` uploadId

instance Prelude.NFData GetUploadStatus where
  rnf GetUploadStatus' {..} = Prelude.rnf uploadId

instance Core.ToHeaders GetUploadStatus where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "IotThingsGraphFrontEndService.GetUploadStatus" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GetUploadStatus where
  toJSON GetUploadStatus' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("uploadId" Core..= uploadId)]
      )

instance Core.ToPath GetUploadStatus where
  toPath = Prelude.const "/"

instance Core.ToQuery GetUploadStatus where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetUploadStatusResponse' smart constructor.
data GetUploadStatusResponse = GetUploadStatusResponse'
  { -- | The reason for an upload failure.
    failureReason :: Prelude.Maybe [Prelude.Text],
    -- | The ARN of the upload.
    namespaceArn :: Prelude.Maybe Prelude.Text,
    -- | The version of the user\'s namespace. Defaults to the latest version of
    -- the user\'s namespace.
    namespaceVersion :: Prelude.Maybe Prelude.Integer,
    -- | The name of the upload\'s namespace.
    namespaceName :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The ID of the upload.
    uploadId :: Prelude.Text,
    -- | The status of the upload. The initial status is @IN_PROGRESS@. The
    -- response show all validation failures if the upload fails.
    uploadStatus :: UploadStatus,
    -- | The date at which the upload was created.
    createdDate :: Core.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetUploadStatusResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'failureReason', 'getUploadStatusResponse_failureReason' - The reason for an upload failure.
--
-- 'namespaceArn', 'getUploadStatusResponse_namespaceArn' - The ARN of the upload.
--
-- 'namespaceVersion', 'getUploadStatusResponse_namespaceVersion' - The version of the user\'s namespace. Defaults to the latest version of
-- the user\'s namespace.
--
-- 'namespaceName', 'getUploadStatusResponse_namespaceName' - The name of the upload\'s namespace.
--
-- 'httpStatus', 'getUploadStatusResponse_httpStatus' - The response's http status code.
--
-- 'uploadId', 'getUploadStatusResponse_uploadId' - The ID of the upload.
--
-- 'uploadStatus', 'getUploadStatusResponse_uploadStatus' - The status of the upload. The initial status is @IN_PROGRESS@. The
-- response show all validation failures if the upload fails.
--
-- 'createdDate', 'getUploadStatusResponse_createdDate' - The date at which the upload was created.
newGetUploadStatusResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'uploadId'
  Prelude.Text ->
  -- | 'uploadStatus'
  UploadStatus ->
  -- | 'createdDate'
  Prelude.UTCTime ->
  GetUploadStatusResponse
newGetUploadStatusResponse
  pHttpStatus_
  pUploadId_
  pUploadStatus_
  pCreatedDate_ =
    GetUploadStatusResponse'
      { failureReason =
          Prelude.Nothing,
        namespaceArn = Prelude.Nothing,
        namespaceVersion = Prelude.Nothing,
        namespaceName = Prelude.Nothing,
        httpStatus = pHttpStatus_,
        uploadId = pUploadId_,
        uploadStatus = pUploadStatus_,
        createdDate = Core._Time Lens.# pCreatedDate_
      }

-- | The reason for an upload failure.
getUploadStatusResponse_failureReason :: Lens.Lens' GetUploadStatusResponse (Prelude.Maybe [Prelude.Text])
getUploadStatusResponse_failureReason = Lens.lens (\GetUploadStatusResponse' {failureReason} -> failureReason) (\s@GetUploadStatusResponse' {} a -> s {failureReason = a} :: GetUploadStatusResponse) Prelude.. Lens.mapping Lens.coerced

-- | The ARN of the upload.
getUploadStatusResponse_namespaceArn :: Lens.Lens' GetUploadStatusResponse (Prelude.Maybe Prelude.Text)
getUploadStatusResponse_namespaceArn = Lens.lens (\GetUploadStatusResponse' {namespaceArn} -> namespaceArn) (\s@GetUploadStatusResponse' {} a -> s {namespaceArn = a} :: GetUploadStatusResponse)

-- | The version of the user\'s namespace. Defaults to the latest version of
-- the user\'s namespace.
getUploadStatusResponse_namespaceVersion :: Lens.Lens' GetUploadStatusResponse (Prelude.Maybe Prelude.Integer)
getUploadStatusResponse_namespaceVersion = Lens.lens (\GetUploadStatusResponse' {namespaceVersion} -> namespaceVersion) (\s@GetUploadStatusResponse' {} a -> s {namespaceVersion = a} :: GetUploadStatusResponse)

-- | The name of the upload\'s namespace.
getUploadStatusResponse_namespaceName :: Lens.Lens' GetUploadStatusResponse (Prelude.Maybe Prelude.Text)
getUploadStatusResponse_namespaceName = Lens.lens (\GetUploadStatusResponse' {namespaceName} -> namespaceName) (\s@GetUploadStatusResponse' {} a -> s {namespaceName = a} :: GetUploadStatusResponse)

-- | The response's http status code.
getUploadStatusResponse_httpStatus :: Lens.Lens' GetUploadStatusResponse Prelude.Int
getUploadStatusResponse_httpStatus = Lens.lens (\GetUploadStatusResponse' {httpStatus} -> httpStatus) (\s@GetUploadStatusResponse' {} a -> s {httpStatus = a} :: GetUploadStatusResponse)

-- | The ID of the upload.
getUploadStatusResponse_uploadId :: Lens.Lens' GetUploadStatusResponse Prelude.Text
getUploadStatusResponse_uploadId = Lens.lens (\GetUploadStatusResponse' {uploadId} -> uploadId) (\s@GetUploadStatusResponse' {} a -> s {uploadId = a} :: GetUploadStatusResponse)

-- | The status of the upload. The initial status is @IN_PROGRESS@. The
-- response show all validation failures if the upload fails.
getUploadStatusResponse_uploadStatus :: Lens.Lens' GetUploadStatusResponse UploadStatus
getUploadStatusResponse_uploadStatus = Lens.lens (\GetUploadStatusResponse' {uploadStatus} -> uploadStatus) (\s@GetUploadStatusResponse' {} a -> s {uploadStatus = a} :: GetUploadStatusResponse)

-- | The date at which the upload was created.
getUploadStatusResponse_createdDate :: Lens.Lens' GetUploadStatusResponse Prelude.UTCTime
getUploadStatusResponse_createdDate = Lens.lens (\GetUploadStatusResponse' {createdDate} -> createdDate) (\s@GetUploadStatusResponse' {} a -> s {createdDate = a} :: GetUploadStatusResponse) Prelude.. Core._Time

instance Prelude.NFData GetUploadStatusResponse where
  rnf GetUploadStatusResponse' {..} =
    Prelude.rnf failureReason
      `Prelude.seq` Prelude.rnf createdDate
      `Prelude.seq` Prelude.rnf uploadStatus
      `Prelude.seq` Prelude.rnf uploadId
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf namespaceName
      `Prelude.seq` Prelude.rnf namespaceVersion
      `Prelude.seq` Prelude.rnf namespaceArn
