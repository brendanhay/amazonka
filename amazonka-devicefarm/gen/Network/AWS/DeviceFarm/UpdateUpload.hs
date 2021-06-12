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
-- Module      : Network.AWS.DeviceFarm.UpdateUpload
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an uploaded test spec.
module Network.AWS.DeviceFarm.UpdateUpload
  ( -- * Creating a Request
    UpdateUpload (..),
    newUpdateUpload,

    -- * Request Lenses
    updateUpload_contentType,
    updateUpload_editContent,
    updateUpload_name,
    updateUpload_arn,

    -- * Destructuring the Response
    UpdateUploadResponse (..),
    newUpdateUploadResponse,

    -- * Response Lenses
    updateUploadResponse_upload,
    updateUploadResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.DeviceFarm.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateUpload' smart constructor.
data UpdateUpload = UpdateUpload'
  { -- | The upload\'s content type (for example, @application\/x-yaml@).
    contentType :: Core.Maybe Core.Text,
    -- | Set to true if the YAML file has changed and must be updated. Otherwise,
    -- set to false.
    editContent :: Core.Maybe Core.Bool,
    -- | The upload\'s test spec file name. The name must not contain any forward
    -- slashes (\/). The test spec file name must end with the @.yaml@ or
    -- @.yml@ file extension.
    name :: Core.Maybe Core.Text,
    -- | The Amazon Resource Name (ARN) of the uploaded test spec.
    arn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateUpload' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'contentType', 'updateUpload_contentType' - The upload\'s content type (for example, @application\/x-yaml@).
--
-- 'editContent', 'updateUpload_editContent' - Set to true if the YAML file has changed and must be updated. Otherwise,
-- set to false.
--
-- 'name', 'updateUpload_name' - The upload\'s test spec file name. The name must not contain any forward
-- slashes (\/). The test spec file name must end with the @.yaml@ or
-- @.yml@ file extension.
--
-- 'arn', 'updateUpload_arn' - The Amazon Resource Name (ARN) of the uploaded test spec.
newUpdateUpload ::
  -- | 'arn'
  Core.Text ->
  UpdateUpload
newUpdateUpload pArn_ =
  UpdateUpload'
    { contentType = Core.Nothing,
      editContent = Core.Nothing,
      name = Core.Nothing,
      arn = pArn_
    }

-- | The upload\'s content type (for example, @application\/x-yaml@).
updateUpload_contentType :: Lens.Lens' UpdateUpload (Core.Maybe Core.Text)
updateUpload_contentType = Lens.lens (\UpdateUpload' {contentType} -> contentType) (\s@UpdateUpload' {} a -> s {contentType = a} :: UpdateUpload)

-- | Set to true if the YAML file has changed and must be updated. Otherwise,
-- set to false.
updateUpload_editContent :: Lens.Lens' UpdateUpload (Core.Maybe Core.Bool)
updateUpload_editContent = Lens.lens (\UpdateUpload' {editContent} -> editContent) (\s@UpdateUpload' {} a -> s {editContent = a} :: UpdateUpload)

-- | The upload\'s test spec file name. The name must not contain any forward
-- slashes (\/). The test spec file name must end with the @.yaml@ or
-- @.yml@ file extension.
updateUpload_name :: Lens.Lens' UpdateUpload (Core.Maybe Core.Text)
updateUpload_name = Lens.lens (\UpdateUpload' {name} -> name) (\s@UpdateUpload' {} a -> s {name = a} :: UpdateUpload)

-- | The Amazon Resource Name (ARN) of the uploaded test spec.
updateUpload_arn :: Lens.Lens' UpdateUpload Core.Text
updateUpload_arn = Lens.lens (\UpdateUpload' {arn} -> arn) (\s@UpdateUpload' {} a -> s {arn = a} :: UpdateUpload)

instance Core.AWSRequest UpdateUpload where
  type AWSResponse UpdateUpload = UpdateUploadResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateUploadResponse'
            Core.<$> (x Core..?> "upload")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable UpdateUpload

instance Core.NFData UpdateUpload

instance Core.ToHeaders UpdateUpload where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "DeviceFarm_20150623.UpdateUpload" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON UpdateUpload where
  toJSON UpdateUpload' {..} =
    Core.object
      ( Core.catMaybes
          [ ("contentType" Core..=) Core.<$> contentType,
            ("editContent" Core..=) Core.<$> editContent,
            ("name" Core..=) Core.<$> name,
            Core.Just ("arn" Core..= arn)
          ]
      )

instance Core.ToPath UpdateUpload where
  toPath = Core.const "/"

instance Core.ToQuery UpdateUpload where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newUpdateUploadResponse' smart constructor.
data UpdateUploadResponse = UpdateUploadResponse'
  { -- | A test spec uploaded to Device Farm.
    upload :: Core.Maybe Upload,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  UpdateUploadResponse
newUpdateUploadResponse pHttpStatus_ =
  UpdateUploadResponse'
    { upload = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A test spec uploaded to Device Farm.
updateUploadResponse_upload :: Lens.Lens' UpdateUploadResponse (Core.Maybe Upload)
updateUploadResponse_upload = Lens.lens (\UpdateUploadResponse' {upload} -> upload) (\s@UpdateUploadResponse' {} a -> s {upload = a} :: UpdateUploadResponse)

-- | The response's http status code.
updateUploadResponse_httpStatus :: Lens.Lens' UpdateUploadResponse Core.Int
updateUploadResponse_httpStatus = Lens.lens (\UpdateUploadResponse' {httpStatus} -> httpStatus) (\s@UpdateUploadResponse' {} a -> s {httpStatus = a} :: UpdateUploadResponse)

instance Core.NFData UpdateUploadResponse
