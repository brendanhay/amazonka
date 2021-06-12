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
-- Module      : Network.AWS.AppStream.DeleteImage
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified image. You cannot delete an image when it is in
-- use. After you delete an image, you cannot provision new capacity using
-- the image.
module Network.AWS.AppStream.DeleteImage
  ( -- * Creating a Request
    DeleteImage (..),
    newDeleteImage,

    -- * Request Lenses
    deleteImage_name,

    -- * Destructuring the Response
    DeleteImageResponse (..),
    newDeleteImageResponse,

    -- * Response Lenses
    deleteImageResponse_image,
    deleteImageResponse_httpStatus,
  )
where

import Network.AWS.AppStream.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteImage' smart constructor.
data DeleteImage = DeleteImage'
  { -- | The name of the image.
    name :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteImage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'deleteImage_name' - The name of the image.
newDeleteImage ::
  -- | 'name'
  Core.Text ->
  DeleteImage
newDeleteImage pName_ = DeleteImage' {name = pName_}

-- | The name of the image.
deleteImage_name :: Lens.Lens' DeleteImage Core.Text
deleteImage_name = Lens.lens (\DeleteImage' {name} -> name) (\s@DeleteImage' {} a -> s {name = a} :: DeleteImage)

instance Core.AWSRequest DeleteImage where
  type AWSResponse DeleteImage = DeleteImageResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteImageResponse'
            Core.<$> (x Core..?> "Image")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeleteImage

instance Core.NFData DeleteImage

instance Core.ToHeaders DeleteImage where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "PhotonAdminProxyService.DeleteImage" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DeleteImage where
  toJSON DeleteImage' {..} =
    Core.object
      (Core.catMaybes [Core.Just ("Name" Core..= name)])

instance Core.ToPath DeleteImage where
  toPath = Core.const "/"

instance Core.ToQuery DeleteImage where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeleteImageResponse' smart constructor.
data DeleteImageResponse = DeleteImageResponse'
  { -- | Information about the image.
    image :: Core.Maybe Image,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteImageResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'image', 'deleteImageResponse_image' - Information about the image.
--
-- 'httpStatus', 'deleteImageResponse_httpStatus' - The response's http status code.
newDeleteImageResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DeleteImageResponse
newDeleteImageResponse pHttpStatus_ =
  DeleteImageResponse'
    { image = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the image.
deleteImageResponse_image :: Lens.Lens' DeleteImageResponse (Core.Maybe Image)
deleteImageResponse_image = Lens.lens (\DeleteImageResponse' {image} -> image) (\s@DeleteImageResponse' {} a -> s {image = a} :: DeleteImageResponse)

-- | The response's http status code.
deleteImageResponse_httpStatus :: Lens.Lens' DeleteImageResponse Core.Int
deleteImageResponse_httpStatus = Lens.lens (\DeleteImageResponse' {httpStatus} -> httpStatus) (\s@DeleteImageResponse' {} a -> s {httpStatus = a} :: DeleteImageResponse)

instance Core.NFData DeleteImageResponse
