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
-- Module      : Amazonka.DeviceFarm.UpdateUpload
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an uploaded test spec.
module Amazonka.DeviceFarm.UpdateUpload
  ( -- * Creating a Request
    UpdateUpload (..),
    newUpdateUpload,

    -- * Request Lenses
    updateUpload_name,
    updateUpload_editContent,
    updateUpload_contentType,
    updateUpload_arn,

    -- * Destructuring the Response
    UpdateUploadResponse (..),
    newUpdateUploadResponse,

    -- * Response Lenses
    updateUploadResponse_upload,
    updateUploadResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DeviceFarm.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateUpload' smart constructor.
data UpdateUpload = UpdateUpload'
  { -- | The upload\'s test spec file name. The name must not contain any forward
    -- slashes (\/). The test spec file name must end with the @.yaml@ or
    -- @.yml@ file extension.
    name :: Prelude.Maybe Prelude.Text,
    -- | Set to true if the YAML file has changed and must be updated. Otherwise,
    -- set to false.
    editContent :: Prelude.Maybe Prelude.Bool,
    -- | The upload\'s content type (for example, @application\/x-yaml@).
    contentType :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the uploaded test spec.
    arn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateUpload' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'updateUpload_name' - The upload\'s test spec file name. The name must not contain any forward
-- slashes (\/). The test spec file name must end with the @.yaml@ or
-- @.yml@ file extension.
--
-- 'editContent', 'updateUpload_editContent' - Set to true if the YAML file has changed and must be updated. Otherwise,
-- set to false.
--
-- 'contentType', 'updateUpload_contentType' - The upload\'s content type (for example, @application\/x-yaml@).
--
-- 'arn', 'updateUpload_arn' - The Amazon Resource Name (ARN) of the uploaded test spec.
newUpdateUpload ::
  -- | 'arn'
  Prelude.Text ->
  UpdateUpload
newUpdateUpload pArn_ =
  UpdateUpload'
    { name = Prelude.Nothing,
      editContent = Prelude.Nothing,
      contentType = Prelude.Nothing,
      arn = pArn_
    }

-- | The upload\'s test spec file name. The name must not contain any forward
-- slashes (\/). The test spec file name must end with the @.yaml@ or
-- @.yml@ file extension.
updateUpload_name :: Lens.Lens' UpdateUpload (Prelude.Maybe Prelude.Text)
updateUpload_name = Lens.lens (\UpdateUpload' {name} -> name) (\s@UpdateUpload' {} a -> s {name = a} :: UpdateUpload)

-- | Set to true if the YAML file has changed and must be updated. Otherwise,
-- set to false.
updateUpload_editContent :: Lens.Lens' UpdateUpload (Prelude.Maybe Prelude.Bool)
updateUpload_editContent = Lens.lens (\UpdateUpload' {editContent} -> editContent) (\s@UpdateUpload' {} a -> s {editContent = a} :: UpdateUpload)

-- | The upload\'s content type (for example, @application\/x-yaml@).
updateUpload_contentType :: Lens.Lens' UpdateUpload (Prelude.Maybe Prelude.Text)
updateUpload_contentType = Lens.lens (\UpdateUpload' {contentType} -> contentType) (\s@UpdateUpload' {} a -> s {contentType = a} :: UpdateUpload)

-- | The Amazon Resource Name (ARN) of the uploaded test spec.
updateUpload_arn :: Lens.Lens' UpdateUpload Prelude.Text
updateUpload_arn = Lens.lens (\UpdateUpload' {arn} -> arn) (\s@UpdateUpload' {} a -> s {arn = a} :: UpdateUpload)

instance Core.AWSRequest UpdateUpload where
  type AWSResponse UpdateUpload = UpdateUploadResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateUploadResponse'
            Prelude.<$> (x Core..?> "upload")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateUpload where
  hashWithSalt _salt UpdateUpload' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` editContent
      `Prelude.hashWithSalt` contentType
      `Prelude.hashWithSalt` arn

instance Prelude.NFData UpdateUpload where
  rnf UpdateUpload' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf editContent
      `Prelude.seq` Prelude.rnf contentType
      `Prelude.seq` Prelude.rnf arn

instance Core.ToHeaders UpdateUpload where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "DeviceFarm_20150623.UpdateUpload" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateUpload where
  toJSON UpdateUpload' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("name" Core..=) Prelude.<$> name,
            ("editContent" Core..=) Prelude.<$> editContent,
            ("contentType" Core..=) Prelude.<$> contentType,
            Prelude.Just ("arn" Core..= arn)
          ]
      )

instance Core.ToPath UpdateUpload where
  toPath = Prelude.const "/"

instance Core.ToQuery UpdateUpload where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateUploadResponse' smart constructor.
data UpdateUploadResponse = UpdateUploadResponse'
  { -- | A test spec uploaded to Device Farm.
    upload :: Prelude.Maybe Upload,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateUploadResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'upload', 'updateUploadResponse_upload' - A test spec uploaded to Device Farm.
--
-- 'httpStatus', 'updateUploadResponse_httpStatus' - The response's http status code.
newUpdateUploadResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateUploadResponse
newUpdateUploadResponse pHttpStatus_ =
  UpdateUploadResponse'
    { upload = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A test spec uploaded to Device Farm.
updateUploadResponse_upload :: Lens.Lens' UpdateUploadResponse (Prelude.Maybe Upload)
updateUploadResponse_upload = Lens.lens (\UpdateUploadResponse' {upload} -> upload) (\s@UpdateUploadResponse' {} a -> s {upload = a} :: UpdateUploadResponse)

-- | The response's http status code.
updateUploadResponse_httpStatus :: Lens.Lens' UpdateUploadResponse Prelude.Int
updateUploadResponse_httpStatus = Lens.lens (\UpdateUploadResponse' {httpStatus} -> httpStatus) (\s@UpdateUploadResponse' {} a -> s {httpStatus = a} :: UpdateUploadResponse)

instance Prelude.NFData UpdateUploadResponse where
  rnf UpdateUploadResponse' {..} =
    Prelude.rnf upload
      `Prelude.seq` Prelude.rnf httpStatus
