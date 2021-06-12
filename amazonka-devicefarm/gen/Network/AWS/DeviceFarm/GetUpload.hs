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
-- Module      : Network.AWS.DeviceFarm.GetUpload
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about an upload.
module Network.AWS.DeviceFarm.GetUpload
  ( -- * Creating a Request
    GetUpload (..),
    newGetUpload,

    -- * Request Lenses
    getUpload_arn,

    -- * Destructuring the Response
    GetUploadResponse (..),
    newGetUploadResponse,

    -- * Response Lenses
    getUploadResponse_upload,
    getUploadResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.DeviceFarm.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents a request to the get upload operation.
--
-- /See:/ 'newGetUpload' smart constructor.
data GetUpload = GetUpload'
  { -- | The upload\'s ARN.
    arn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetUpload' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'getUpload_arn' - The upload\'s ARN.
newGetUpload ::
  -- | 'arn'
  Core.Text ->
  GetUpload
newGetUpload pArn_ = GetUpload' {arn = pArn_}

-- | The upload\'s ARN.
getUpload_arn :: Lens.Lens' GetUpload Core.Text
getUpload_arn = Lens.lens (\GetUpload' {arn} -> arn) (\s@GetUpload' {} a -> s {arn = a} :: GetUpload)

instance Core.AWSRequest GetUpload where
  type AWSResponse GetUpload = GetUploadResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetUploadResponse'
            Core.<$> (x Core..?> "upload")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetUpload

instance Core.NFData GetUpload

instance Core.ToHeaders GetUpload where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("DeviceFarm_20150623.GetUpload" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GetUpload where
  toJSON GetUpload' {..} =
    Core.object
      (Core.catMaybes [Core.Just ("arn" Core..= arn)])

instance Core.ToPath GetUpload where
  toPath = Core.const "/"

instance Core.ToQuery GetUpload where
  toQuery = Core.const Core.mempty

-- | Represents the result of a get upload request.
--
-- /See:/ 'newGetUploadResponse' smart constructor.
data GetUploadResponse = GetUploadResponse'
  { -- | An app or a set of one or more tests to upload or that have been
    -- uploaded.
    upload :: Core.Maybe Upload,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetUploadResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'upload', 'getUploadResponse_upload' - An app or a set of one or more tests to upload or that have been
-- uploaded.
--
-- 'httpStatus', 'getUploadResponse_httpStatus' - The response's http status code.
newGetUploadResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetUploadResponse
newGetUploadResponse pHttpStatus_ =
  GetUploadResponse'
    { upload = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An app or a set of one or more tests to upload or that have been
-- uploaded.
getUploadResponse_upload :: Lens.Lens' GetUploadResponse (Core.Maybe Upload)
getUploadResponse_upload = Lens.lens (\GetUploadResponse' {upload} -> upload) (\s@GetUploadResponse' {} a -> s {upload = a} :: GetUploadResponse)

-- | The response's http status code.
getUploadResponse_httpStatus :: Lens.Lens' GetUploadResponse Core.Int
getUploadResponse_httpStatus = Lens.lens (\GetUploadResponse' {httpStatus} -> httpStatus) (\s@GetUploadResponse' {} a -> s {httpStatus = a} :: GetUploadResponse)

instance Core.NFData GetUploadResponse
