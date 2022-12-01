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
-- Module      : Amazonka.DeviceFarm.GetUpload
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about an upload.
module Amazonka.DeviceFarm.GetUpload
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DeviceFarm.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Represents a request to the get upload operation.
--
-- /See:/ 'newGetUpload' smart constructor.
data GetUpload = GetUpload'
  { -- | The upload\'s ARN.
    arn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  GetUpload
newGetUpload pArn_ = GetUpload' {arn = pArn_}

-- | The upload\'s ARN.
getUpload_arn :: Lens.Lens' GetUpload Prelude.Text
getUpload_arn = Lens.lens (\GetUpload' {arn} -> arn) (\s@GetUpload' {} a -> s {arn = a} :: GetUpload)

instance Core.AWSRequest GetUpload where
  type AWSResponse GetUpload = GetUploadResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetUploadResponse'
            Prelude.<$> (x Core..?> "upload")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetUpload where
  hashWithSalt _salt GetUpload' {..} =
    _salt `Prelude.hashWithSalt` arn

instance Prelude.NFData GetUpload where
  rnf GetUpload' {..} = Prelude.rnf arn

instance Core.ToHeaders GetUpload where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "DeviceFarm_20150623.GetUpload" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GetUpload where
  toJSON GetUpload' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("arn" Core..= arn)]
      )

instance Core.ToPath GetUpload where
  toPath = Prelude.const "/"

instance Core.ToQuery GetUpload where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the result of a get upload request.
--
-- /See:/ 'newGetUploadResponse' smart constructor.
data GetUploadResponse = GetUploadResponse'
  { -- | An app or a set of one or more tests to upload or that have been
    -- uploaded.
    upload :: Prelude.Maybe Upload,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  GetUploadResponse
newGetUploadResponse pHttpStatus_ =
  GetUploadResponse'
    { upload = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An app or a set of one or more tests to upload or that have been
-- uploaded.
getUploadResponse_upload :: Lens.Lens' GetUploadResponse (Prelude.Maybe Upload)
getUploadResponse_upload = Lens.lens (\GetUploadResponse' {upload} -> upload) (\s@GetUploadResponse' {} a -> s {upload = a} :: GetUploadResponse)

-- | The response's http status code.
getUploadResponse_httpStatus :: Lens.Lens' GetUploadResponse Prelude.Int
getUploadResponse_httpStatus = Lens.lens (\GetUploadResponse' {httpStatus} -> httpStatus) (\s@GetUploadResponse' {} a -> s {httpStatus = a} :: GetUploadResponse)

instance Prelude.NFData GetUploadResponse where
  rnf GetUploadResponse' {..} =
    Prelude.rnf upload
      `Prelude.seq` Prelude.rnf httpStatus
