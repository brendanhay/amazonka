{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.SageMaker.DeleteImage
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a SageMaker image and all versions of the image. The container
-- images aren\'t deleted.
module Network.AWS.SageMaker.DeleteImage
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newDeleteImage' smart constructor.
data DeleteImage = DeleteImage'
  { -- | The name of the image to delete.
    imageName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.AWSRequest DeleteImage where
  type Rs DeleteImage = DeleteImageResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteImageResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteImage

instance Prelude.NFData DeleteImage

instance Prelude.ToHeaders DeleteImage where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ("SageMaker.DeleteImage" :: Prelude.ByteString),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DeleteImage where
  toJSON DeleteImage' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [Prelude.Just ("ImageName" Prelude..= imageName)]
      )

instance Prelude.ToPath DeleteImage where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeleteImage where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteImageResponse' smart constructor.
data DeleteImageResponse = DeleteImageResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.NFData DeleteImageResponse
