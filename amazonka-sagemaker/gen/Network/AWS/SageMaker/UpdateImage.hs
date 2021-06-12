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
-- Module      : Network.AWS.SageMaker.UpdateImage
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the properties of a SageMaker image. To change the image\'s
-- tags, use the AddTags and DeleteTags APIs.
module Network.AWS.SageMaker.UpdateImage
  ( -- * Creating a Request
    UpdateImage (..),
    newUpdateImage,

    -- * Request Lenses
    updateImage_roleArn,
    updateImage_deleteProperties,
    updateImage_description,
    updateImage_displayName,
    updateImage_imageName,

    -- * Destructuring the Response
    UpdateImageResponse (..),
    newUpdateImageResponse,

    -- * Response Lenses
    updateImageResponse_imageArn,
    updateImageResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newUpdateImage' smart constructor.
data UpdateImage = UpdateImage'
  { -- | The new Amazon Resource Name (ARN) for the IAM role that enables Amazon
    -- SageMaker to perform tasks on your behalf.
    roleArn :: Core.Maybe Core.Text,
    -- | A list of properties to delete. Only the @Description@ and @DisplayName@
    -- properties can be deleted.
    deleteProperties :: Core.Maybe [Core.Text],
    -- | The new description for the image.
    description :: Core.Maybe Core.Text,
    -- | The new display name for the image.
    displayName :: Core.Maybe Core.Text,
    -- | The name of the image to update.
    imageName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateImage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'roleArn', 'updateImage_roleArn' - The new Amazon Resource Name (ARN) for the IAM role that enables Amazon
-- SageMaker to perform tasks on your behalf.
--
-- 'deleteProperties', 'updateImage_deleteProperties' - A list of properties to delete. Only the @Description@ and @DisplayName@
-- properties can be deleted.
--
-- 'description', 'updateImage_description' - The new description for the image.
--
-- 'displayName', 'updateImage_displayName' - The new display name for the image.
--
-- 'imageName', 'updateImage_imageName' - The name of the image to update.
newUpdateImage ::
  -- | 'imageName'
  Core.Text ->
  UpdateImage
newUpdateImage pImageName_ =
  UpdateImage'
    { roleArn = Core.Nothing,
      deleteProperties = Core.Nothing,
      description = Core.Nothing,
      displayName = Core.Nothing,
      imageName = pImageName_
    }

-- | The new Amazon Resource Name (ARN) for the IAM role that enables Amazon
-- SageMaker to perform tasks on your behalf.
updateImage_roleArn :: Lens.Lens' UpdateImage (Core.Maybe Core.Text)
updateImage_roleArn = Lens.lens (\UpdateImage' {roleArn} -> roleArn) (\s@UpdateImage' {} a -> s {roleArn = a} :: UpdateImage)

-- | A list of properties to delete. Only the @Description@ and @DisplayName@
-- properties can be deleted.
updateImage_deleteProperties :: Lens.Lens' UpdateImage (Core.Maybe [Core.Text])
updateImage_deleteProperties = Lens.lens (\UpdateImage' {deleteProperties} -> deleteProperties) (\s@UpdateImage' {} a -> s {deleteProperties = a} :: UpdateImage) Core.. Lens.mapping Lens._Coerce

-- | The new description for the image.
updateImage_description :: Lens.Lens' UpdateImage (Core.Maybe Core.Text)
updateImage_description = Lens.lens (\UpdateImage' {description} -> description) (\s@UpdateImage' {} a -> s {description = a} :: UpdateImage)

-- | The new display name for the image.
updateImage_displayName :: Lens.Lens' UpdateImage (Core.Maybe Core.Text)
updateImage_displayName = Lens.lens (\UpdateImage' {displayName} -> displayName) (\s@UpdateImage' {} a -> s {displayName = a} :: UpdateImage)

-- | The name of the image to update.
updateImage_imageName :: Lens.Lens' UpdateImage Core.Text
updateImage_imageName = Lens.lens (\UpdateImage' {imageName} -> imageName) (\s@UpdateImage' {} a -> s {imageName = a} :: UpdateImage)

instance Core.AWSRequest UpdateImage where
  type AWSResponse UpdateImage = UpdateImageResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateImageResponse'
            Core.<$> (x Core..?> "ImageArn")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable UpdateImage

instance Core.NFData UpdateImage

instance Core.ToHeaders UpdateImage where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("SageMaker.UpdateImage" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON UpdateImage where
  toJSON UpdateImage' {..} =
    Core.object
      ( Core.catMaybes
          [ ("RoleArn" Core..=) Core.<$> roleArn,
            ("DeleteProperties" Core..=)
              Core.<$> deleteProperties,
            ("Description" Core..=) Core.<$> description,
            ("DisplayName" Core..=) Core.<$> displayName,
            Core.Just ("ImageName" Core..= imageName)
          ]
      )

instance Core.ToPath UpdateImage where
  toPath = Core.const "/"

instance Core.ToQuery UpdateImage where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newUpdateImageResponse' smart constructor.
data UpdateImageResponse = UpdateImageResponse'
  { -- | The Amazon Resource Name (ARN) of the image.
    imageArn :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateImageResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'imageArn', 'updateImageResponse_imageArn' - The Amazon Resource Name (ARN) of the image.
--
-- 'httpStatus', 'updateImageResponse_httpStatus' - The response's http status code.
newUpdateImageResponse ::
  -- | 'httpStatus'
  Core.Int ->
  UpdateImageResponse
newUpdateImageResponse pHttpStatus_ =
  UpdateImageResponse'
    { imageArn = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the image.
updateImageResponse_imageArn :: Lens.Lens' UpdateImageResponse (Core.Maybe Core.Text)
updateImageResponse_imageArn = Lens.lens (\UpdateImageResponse' {imageArn} -> imageArn) (\s@UpdateImageResponse' {} a -> s {imageArn = a} :: UpdateImageResponse)

-- | The response's http status code.
updateImageResponse_httpStatus :: Lens.Lens' UpdateImageResponse Core.Int
updateImageResponse_httpStatus = Lens.lens (\UpdateImageResponse' {httpStatus} -> httpStatus) (\s@UpdateImageResponse' {} a -> s {httpStatus = a} :: UpdateImageResponse)

instance Core.NFData UpdateImageResponse
