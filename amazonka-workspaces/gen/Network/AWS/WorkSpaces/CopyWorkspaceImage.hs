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
-- Module      : Network.AWS.WorkSpaces.CopyWorkspaceImage
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Copies the specified image from the specified Region to the current
-- Region. For more information about copying images, see
-- <https://docs.aws.amazon.com/workspaces/latest/adminguide/copy-custom-image.html Copy a Custom WorkSpaces Image>.
--
-- In the China (Ningxia) Region, you can copy images only within the same
-- Region.
--
-- In the AWS GovCloud (US-West) Region, to copy images to and from other
-- AWS Regions, contact AWS Support.
--
-- Before copying a shared image, be sure to verify that it has been shared
-- from the correct AWS account. To determine if an image has been shared
-- and to see the AWS account ID that owns an image, use the
-- <https://docs.aws.amazon.com/workspaces/latest/api/API_DescribeWorkspaceImages.html DescribeWorkSpaceImages>
-- and
-- <https://docs.aws.amazon.com/workspaces/latest/api/API_DescribeWorkspaceImagePermissions.html DescribeWorkspaceImagePermissions>
-- API operations.
module Network.AWS.WorkSpaces.CopyWorkspaceImage
  ( -- * Creating a Request
    CopyWorkspaceImage (..),
    newCopyWorkspaceImage,

    -- * Request Lenses
    copyWorkspaceImage_tags,
    copyWorkspaceImage_description,
    copyWorkspaceImage_name,
    copyWorkspaceImage_sourceImageId,
    copyWorkspaceImage_sourceRegion,

    -- * Destructuring the Response
    CopyWorkspaceImageResponse (..),
    newCopyWorkspaceImageResponse,

    -- * Response Lenses
    copyWorkspaceImageResponse_imageId,
    copyWorkspaceImageResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.WorkSpaces.Types

-- | /See:/ 'newCopyWorkspaceImage' smart constructor.
data CopyWorkspaceImage = CopyWorkspaceImage'
  { -- | The tags for the image.
    tags :: Core.Maybe [Tag],
    -- | A description of the image.
    description :: Core.Maybe Core.Text,
    -- | The name of the image.
    name :: Core.Text,
    -- | The identifier of the source image.
    sourceImageId :: Core.Text,
    -- | The identifier of the source Region.
    sourceRegion :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CopyWorkspaceImage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'copyWorkspaceImage_tags' - The tags for the image.
--
-- 'description', 'copyWorkspaceImage_description' - A description of the image.
--
-- 'name', 'copyWorkspaceImage_name' - The name of the image.
--
-- 'sourceImageId', 'copyWorkspaceImage_sourceImageId' - The identifier of the source image.
--
-- 'sourceRegion', 'copyWorkspaceImage_sourceRegion' - The identifier of the source Region.
newCopyWorkspaceImage ::
  -- | 'name'
  Core.Text ->
  -- | 'sourceImageId'
  Core.Text ->
  -- | 'sourceRegion'
  Core.Text ->
  CopyWorkspaceImage
newCopyWorkspaceImage
  pName_
  pSourceImageId_
  pSourceRegion_ =
    CopyWorkspaceImage'
      { tags = Core.Nothing,
        description = Core.Nothing,
        name = pName_,
        sourceImageId = pSourceImageId_,
        sourceRegion = pSourceRegion_
      }

-- | The tags for the image.
copyWorkspaceImage_tags :: Lens.Lens' CopyWorkspaceImage (Core.Maybe [Tag])
copyWorkspaceImage_tags = Lens.lens (\CopyWorkspaceImage' {tags} -> tags) (\s@CopyWorkspaceImage' {} a -> s {tags = a} :: CopyWorkspaceImage) Core.. Lens.mapping Lens._Coerce

-- | A description of the image.
copyWorkspaceImage_description :: Lens.Lens' CopyWorkspaceImage (Core.Maybe Core.Text)
copyWorkspaceImage_description = Lens.lens (\CopyWorkspaceImage' {description} -> description) (\s@CopyWorkspaceImage' {} a -> s {description = a} :: CopyWorkspaceImage)

-- | The name of the image.
copyWorkspaceImage_name :: Lens.Lens' CopyWorkspaceImage Core.Text
copyWorkspaceImage_name = Lens.lens (\CopyWorkspaceImage' {name} -> name) (\s@CopyWorkspaceImage' {} a -> s {name = a} :: CopyWorkspaceImage)

-- | The identifier of the source image.
copyWorkspaceImage_sourceImageId :: Lens.Lens' CopyWorkspaceImage Core.Text
copyWorkspaceImage_sourceImageId = Lens.lens (\CopyWorkspaceImage' {sourceImageId} -> sourceImageId) (\s@CopyWorkspaceImage' {} a -> s {sourceImageId = a} :: CopyWorkspaceImage)

-- | The identifier of the source Region.
copyWorkspaceImage_sourceRegion :: Lens.Lens' CopyWorkspaceImage Core.Text
copyWorkspaceImage_sourceRegion = Lens.lens (\CopyWorkspaceImage' {sourceRegion} -> sourceRegion) (\s@CopyWorkspaceImage' {} a -> s {sourceRegion = a} :: CopyWorkspaceImage)

instance Core.AWSRequest CopyWorkspaceImage where
  type
    AWSResponse CopyWorkspaceImage =
      CopyWorkspaceImageResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CopyWorkspaceImageResponse'
            Core.<$> (x Core..?> "ImageId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CopyWorkspaceImage

instance Core.NFData CopyWorkspaceImage

instance Core.ToHeaders CopyWorkspaceImage where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "WorkspacesService.CopyWorkspaceImage" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON CopyWorkspaceImage where
  toJSON CopyWorkspaceImage' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Tags" Core..=) Core.<$> tags,
            ("Description" Core..=) Core.<$> description,
            Core.Just ("Name" Core..= name),
            Core.Just ("SourceImageId" Core..= sourceImageId),
            Core.Just ("SourceRegion" Core..= sourceRegion)
          ]
      )

instance Core.ToPath CopyWorkspaceImage where
  toPath = Core.const "/"

instance Core.ToQuery CopyWorkspaceImage where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newCopyWorkspaceImageResponse' smart constructor.
data CopyWorkspaceImageResponse = CopyWorkspaceImageResponse'
  { -- | The identifier of the image.
    imageId :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CopyWorkspaceImageResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'imageId', 'copyWorkspaceImageResponse_imageId' - The identifier of the image.
--
-- 'httpStatus', 'copyWorkspaceImageResponse_httpStatus' - The response's http status code.
newCopyWorkspaceImageResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CopyWorkspaceImageResponse
newCopyWorkspaceImageResponse pHttpStatus_ =
  CopyWorkspaceImageResponse'
    { imageId = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The identifier of the image.
copyWorkspaceImageResponse_imageId :: Lens.Lens' CopyWorkspaceImageResponse (Core.Maybe Core.Text)
copyWorkspaceImageResponse_imageId = Lens.lens (\CopyWorkspaceImageResponse' {imageId} -> imageId) (\s@CopyWorkspaceImageResponse' {} a -> s {imageId = a} :: CopyWorkspaceImageResponse)

-- | The response's http status code.
copyWorkspaceImageResponse_httpStatus :: Lens.Lens' CopyWorkspaceImageResponse Core.Int
copyWorkspaceImageResponse_httpStatus = Lens.lens (\CopyWorkspaceImageResponse' {httpStatus} -> httpStatus) (\s@CopyWorkspaceImageResponse' {} a -> s {httpStatus = a} :: CopyWorkspaceImageResponse)

instance Core.NFData CopyWorkspaceImageResponse
