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
-- Module      : Amazonka.SageMaker.DeleteImage
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a SageMaker image and all versions of the image. The container
-- images aren\'t deleted.
module Amazonka.SageMaker.DeleteImage
  ( -- * Creating a Request
    DeleteImage (..),
    newDeleteImage,

    -- * Request Lenses
    deleteImage_imageName,

    -- * Destructuring the Response
    DeleteImageResponse (..),
    newDeleteImageResponse,

    -- * Response Lenses
    deleteImageResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newDeleteImage' smart constructor.
data DeleteImage = DeleteImage'
  { -- | The name of the image to delete.
    imageName :: Prelude.Text
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
-- 'imageName', 'deleteImage_imageName' - The name of the image to delete.
newDeleteImage ::
  -- | 'imageName'
  Prelude.Text ->
  DeleteImage
newDeleteImage pImageName_ =
  DeleteImage' {imageName = pImageName_}

-- | The name of the image to delete.
deleteImage_imageName :: Lens.Lens' DeleteImage Prelude.Text
deleteImage_imageName = Lens.lens (\DeleteImage' {imageName} -> imageName) (\s@DeleteImage' {} a -> s {imageName = a} :: DeleteImage)

instance Core.AWSRequest DeleteImage where
  type AWSResponse DeleteImage = DeleteImageResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteImageResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteImage where
  hashWithSalt _salt DeleteImage' {..} =
    _salt `Prelude.hashWithSalt` imageName

instance Prelude.NFData DeleteImage where
  rnf DeleteImage' {..} = Prelude.rnf imageName

instance Data.ToHeaders DeleteImage where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("SageMaker.DeleteImage" :: Prelude.ByteString),
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
          [Prelude.Just ("ImageName" Data..= imageName)]
      )

instance Data.ToPath DeleteImage where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteImage where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteImageResponse' smart constructor.
data DeleteImageResponse = DeleteImageResponse'
  { -- | The response's http status code.
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
-- 'httpStatus', 'deleteImageResponse_httpStatus' - The response's http status code.
newDeleteImageResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteImageResponse
newDeleteImageResponse pHttpStatus_ =
  DeleteImageResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
deleteImageResponse_httpStatus :: Lens.Lens' DeleteImageResponse Prelude.Int
deleteImageResponse_httpStatus = Lens.lens (\DeleteImageResponse' {httpStatus} -> httpStatus) (\s@DeleteImageResponse' {} a -> s {httpStatus = a} :: DeleteImageResponse)

instance Prelude.NFData DeleteImageResponse where
  rnf DeleteImageResponse' {..} = Prelude.rnf httpStatus
