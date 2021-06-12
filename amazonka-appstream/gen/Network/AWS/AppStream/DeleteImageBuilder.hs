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
-- Module      : Network.AWS.AppStream.DeleteImageBuilder
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified image builder and releases the capacity.
module Network.AWS.AppStream.DeleteImageBuilder
  ( -- * Creating a Request
    DeleteImageBuilder (..),
    newDeleteImageBuilder,

    -- * Request Lenses
    deleteImageBuilder_name,

    -- * Destructuring the Response
    DeleteImageBuilderResponse (..),
    newDeleteImageBuilderResponse,

    -- * Response Lenses
    deleteImageBuilderResponse_imageBuilder,
    deleteImageBuilderResponse_httpStatus,
  )
where

import Network.AWS.AppStream.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteImageBuilder' smart constructor.
data DeleteImageBuilder = DeleteImageBuilder'
  { -- | The name of the image builder.
    name :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteImageBuilder' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'deleteImageBuilder_name' - The name of the image builder.
newDeleteImageBuilder ::
  -- | 'name'
  Core.Text ->
  DeleteImageBuilder
newDeleteImageBuilder pName_ =
  DeleteImageBuilder' {name = pName_}

-- | The name of the image builder.
deleteImageBuilder_name :: Lens.Lens' DeleteImageBuilder Core.Text
deleteImageBuilder_name = Lens.lens (\DeleteImageBuilder' {name} -> name) (\s@DeleteImageBuilder' {} a -> s {name = a} :: DeleteImageBuilder)

instance Core.AWSRequest DeleteImageBuilder where
  type
    AWSResponse DeleteImageBuilder =
      DeleteImageBuilderResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteImageBuilderResponse'
            Core.<$> (x Core..?> "ImageBuilder")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeleteImageBuilder

instance Core.NFData DeleteImageBuilder

instance Core.ToHeaders DeleteImageBuilder where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "PhotonAdminProxyService.DeleteImageBuilder" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DeleteImageBuilder where
  toJSON DeleteImageBuilder' {..} =
    Core.object
      (Core.catMaybes [Core.Just ("Name" Core..= name)])

instance Core.ToPath DeleteImageBuilder where
  toPath = Core.const "/"

instance Core.ToQuery DeleteImageBuilder where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeleteImageBuilderResponse' smart constructor.
data DeleteImageBuilderResponse = DeleteImageBuilderResponse'
  { -- | Information about the image builder.
    imageBuilder :: Core.Maybe ImageBuilder,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteImageBuilderResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'imageBuilder', 'deleteImageBuilderResponse_imageBuilder' - Information about the image builder.
--
-- 'httpStatus', 'deleteImageBuilderResponse_httpStatus' - The response's http status code.
newDeleteImageBuilderResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DeleteImageBuilderResponse
newDeleteImageBuilderResponse pHttpStatus_ =
  DeleteImageBuilderResponse'
    { imageBuilder =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the image builder.
deleteImageBuilderResponse_imageBuilder :: Lens.Lens' DeleteImageBuilderResponse (Core.Maybe ImageBuilder)
deleteImageBuilderResponse_imageBuilder = Lens.lens (\DeleteImageBuilderResponse' {imageBuilder} -> imageBuilder) (\s@DeleteImageBuilderResponse' {} a -> s {imageBuilder = a} :: DeleteImageBuilderResponse)

-- | The response's http status code.
deleteImageBuilderResponse_httpStatus :: Lens.Lens' DeleteImageBuilderResponse Core.Int
deleteImageBuilderResponse_httpStatus = Lens.lens (\DeleteImageBuilderResponse' {httpStatus} -> httpStatus) (\s@DeleteImageBuilderResponse' {} a -> s {httpStatus = a} :: DeleteImageBuilderResponse)

instance Core.NFData DeleteImageBuilderResponse
