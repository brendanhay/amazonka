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
-- Module      : Amazonka.AppStream.DeleteImage
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified image. You cannot delete an image when it is in
-- use. After you delete an image, you cannot provision new capacity using
-- the image.
module Amazonka.AppStream.DeleteImage
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

import Amazonka.AppStream.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteImage' smart constructor.
data DeleteImage = DeleteImage'
  { -- | The name of the image.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  DeleteImage
newDeleteImage pName_ = DeleteImage' {name = pName_}

-- | The name of the image.
deleteImage_name :: Lens.Lens' DeleteImage Prelude.Text
deleteImage_name = Lens.lens (\DeleteImage' {name} -> name) (\s@DeleteImage' {} a -> s {name = a} :: DeleteImage)

instance Core.AWSRequest DeleteImage where
  type AWSResponse DeleteImage = DeleteImageResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteImageResponse'
            Prelude.<$> (x Data..?> "Image")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteImage where
  hashWithSalt _salt DeleteImage' {..} =
    _salt `Prelude.hashWithSalt` name

instance Prelude.NFData DeleteImage where
  rnf DeleteImage' {..} = Prelude.rnf name

instance Data.ToHeaders DeleteImage where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "PhotonAdminProxyService.DeleteImage" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteImage where
  toJSON DeleteImage' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("Name" Data..= name)]
      )

instance Data.ToPath DeleteImage where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteImage where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteImageResponse' smart constructor.
data DeleteImageResponse = DeleteImageResponse'
  { -- | Information about the image.
    image :: Prelude.Maybe Image,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  DeleteImageResponse
newDeleteImageResponse pHttpStatus_ =
  DeleteImageResponse'
    { image = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the image.
deleteImageResponse_image :: Lens.Lens' DeleteImageResponse (Prelude.Maybe Image)
deleteImageResponse_image = Lens.lens (\DeleteImageResponse' {image} -> image) (\s@DeleteImageResponse' {} a -> s {image = a} :: DeleteImageResponse)

-- | The response's http status code.
deleteImageResponse_httpStatus :: Lens.Lens' DeleteImageResponse Prelude.Int
deleteImageResponse_httpStatus = Lens.lens (\DeleteImageResponse' {httpStatus} -> httpStatus) (\s@DeleteImageResponse' {} a -> s {httpStatus = a} :: DeleteImageResponse)

instance Prelude.NFData DeleteImageResponse where
  rnf DeleteImageResponse' {..} =
    Prelude.rnf image `Prelude.seq`
      Prelude.rnf httpStatus
