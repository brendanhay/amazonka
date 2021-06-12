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
-- Module      : Network.AWS.WorkSpaces.ImportWorkspaceImage
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Imports the specified Windows 10 Bring Your Own License (BYOL) image
-- into Amazon WorkSpaces. The image must be an already licensed Amazon EC2
-- image that is in your AWS account, and you must own the image. For more
-- information about creating BYOL images, see
-- <https://docs.aws.amazon.com/workspaces/latest/adminguide/byol-windows-images.html Bring Your Own Windows Desktop Licenses>.
module Network.AWS.WorkSpaces.ImportWorkspaceImage
  ( -- * Creating a Request
    ImportWorkspaceImage (..),
    newImportWorkspaceImage,

    -- * Request Lenses
    importWorkspaceImage_tags,
    importWorkspaceImage_applications,
    importWorkspaceImage_ec2ImageId,
    importWorkspaceImage_ingestionProcess,
    importWorkspaceImage_imageName,
    importWorkspaceImage_imageDescription,

    -- * Destructuring the Response
    ImportWorkspaceImageResponse (..),
    newImportWorkspaceImageResponse,

    -- * Response Lenses
    importWorkspaceImageResponse_imageId,
    importWorkspaceImageResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.WorkSpaces.Types

-- | /See:/ 'newImportWorkspaceImage' smart constructor.
data ImportWorkspaceImage = ImportWorkspaceImage'
  { -- | The tags. Each WorkSpaces resource can have a maximum of 50 tags.
    tags :: Core.Maybe [Tag],
    -- | If specified, the version of Microsoft Office to subscribe to. Valid
    -- only for Windows 10 BYOL images. For more information about subscribing
    -- to Office for BYOL images, see
    -- <https://docs.aws.amazon.com/workspaces/latest/adminguide/byol-windows-images.html Bring Your Own Windows Desktop Licenses>.
    --
    -- Although this parameter is an array, only one item is allowed at this
    -- time.
    applications :: Core.Maybe (Core.NonEmpty Application),
    -- | The identifier of the EC2 image.
    ec2ImageId :: Core.Text,
    -- | The ingestion process to be used when importing the image, depending on
    -- which protocol you want to use for your BYOL Workspace image, either
    -- PCoIP or WorkSpaces Streaming Protocol (WSP). To use WSP, specify a
    -- value that ends in @_WSP@. To use PCoIP, specify a value that does not
    -- end in @_WSP@.
    --
    -- For non-GPU-enabled bundles (bundles other than Graphics or
    -- GraphicsPro), specify @BYOL_REGULAR@ or @BYOL_REGULAR_WSP@, depending on
    -- the protocol.
    ingestionProcess :: WorkspaceImageIngestionProcess,
    -- | The name of the WorkSpace image.
    imageName :: Core.Text,
    -- | The description of the WorkSpace image.
    imageDescription :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ImportWorkspaceImage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'importWorkspaceImage_tags' - The tags. Each WorkSpaces resource can have a maximum of 50 tags.
--
-- 'applications', 'importWorkspaceImage_applications' - If specified, the version of Microsoft Office to subscribe to. Valid
-- only for Windows 10 BYOL images. For more information about subscribing
-- to Office for BYOL images, see
-- <https://docs.aws.amazon.com/workspaces/latest/adminguide/byol-windows-images.html Bring Your Own Windows Desktop Licenses>.
--
-- Although this parameter is an array, only one item is allowed at this
-- time.
--
-- 'ec2ImageId', 'importWorkspaceImage_ec2ImageId' - The identifier of the EC2 image.
--
-- 'ingestionProcess', 'importWorkspaceImage_ingestionProcess' - The ingestion process to be used when importing the image, depending on
-- which protocol you want to use for your BYOL Workspace image, either
-- PCoIP or WorkSpaces Streaming Protocol (WSP). To use WSP, specify a
-- value that ends in @_WSP@. To use PCoIP, specify a value that does not
-- end in @_WSP@.
--
-- For non-GPU-enabled bundles (bundles other than Graphics or
-- GraphicsPro), specify @BYOL_REGULAR@ or @BYOL_REGULAR_WSP@, depending on
-- the protocol.
--
-- 'imageName', 'importWorkspaceImage_imageName' - The name of the WorkSpace image.
--
-- 'imageDescription', 'importWorkspaceImage_imageDescription' - The description of the WorkSpace image.
newImportWorkspaceImage ::
  -- | 'ec2ImageId'
  Core.Text ->
  -- | 'ingestionProcess'
  WorkspaceImageIngestionProcess ->
  -- | 'imageName'
  Core.Text ->
  -- | 'imageDescription'
  Core.Text ->
  ImportWorkspaceImage
newImportWorkspaceImage
  pEc2ImageId_
  pIngestionProcess_
  pImageName_
  pImageDescription_ =
    ImportWorkspaceImage'
      { tags = Core.Nothing,
        applications = Core.Nothing,
        ec2ImageId = pEc2ImageId_,
        ingestionProcess = pIngestionProcess_,
        imageName = pImageName_,
        imageDescription = pImageDescription_
      }

-- | The tags. Each WorkSpaces resource can have a maximum of 50 tags.
importWorkspaceImage_tags :: Lens.Lens' ImportWorkspaceImage (Core.Maybe [Tag])
importWorkspaceImage_tags = Lens.lens (\ImportWorkspaceImage' {tags} -> tags) (\s@ImportWorkspaceImage' {} a -> s {tags = a} :: ImportWorkspaceImage) Core.. Lens.mapping Lens._Coerce

-- | If specified, the version of Microsoft Office to subscribe to. Valid
-- only for Windows 10 BYOL images. For more information about subscribing
-- to Office for BYOL images, see
-- <https://docs.aws.amazon.com/workspaces/latest/adminguide/byol-windows-images.html Bring Your Own Windows Desktop Licenses>.
--
-- Although this parameter is an array, only one item is allowed at this
-- time.
importWorkspaceImage_applications :: Lens.Lens' ImportWorkspaceImage (Core.Maybe (Core.NonEmpty Application))
importWorkspaceImage_applications = Lens.lens (\ImportWorkspaceImage' {applications} -> applications) (\s@ImportWorkspaceImage' {} a -> s {applications = a} :: ImportWorkspaceImage) Core.. Lens.mapping Lens._Coerce

-- | The identifier of the EC2 image.
importWorkspaceImage_ec2ImageId :: Lens.Lens' ImportWorkspaceImage Core.Text
importWorkspaceImage_ec2ImageId = Lens.lens (\ImportWorkspaceImage' {ec2ImageId} -> ec2ImageId) (\s@ImportWorkspaceImage' {} a -> s {ec2ImageId = a} :: ImportWorkspaceImage)

-- | The ingestion process to be used when importing the image, depending on
-- which protocol you want to use for your BYOL Workspace image, either
-- PCoIP or WorkSpaces Streaming Protocol (WSP). To use WSP, specify a
-- value that ends in @_WSP@. To use PCoIP, specify a value that does not
-- end in @_WSP@.
--
-- For non-GPU-enabled bundles (bundles other than Graphics or
-- GraphicsPro), specify @BYOL_REGULAR@ or @BYOL_REGULAR_WSP@, depending on
-- the protocol.
importWorkspaceImage_ingestionProcess :: Lens.Lens' ImportWorkspaceImage WorkspaceImageIngestionProcess
importWorkspaceImage_ingestionProcess = Lens.lens (\ImportWorkspaceImage' {ingestionProcess} -> ingestionProcess) (\s@ImportWorkspaceImage' {} a -> s {ingestionProcess = a} :: ImportWorkspaceImage)

-- | The name of the WorkSpace image.
importWorkspaceImage_imageName :: Lens.Lens' ImportWorkspaceImage Core.Text
importWorkspaceImage_imageName = Lens.lens (\ImportWorkspaceImage' {imageName} -> imageName) (\s@ImportWorkspaceImage' {} a -> s {imageName = a} :: ImportWorkspaceImage)

-- | The description of the WorkSpace image.
importWorkspaceImage_imageDescription :: Lens.Lens' ImportWorkspaceImage Core.Text
importWorkspaceImage_imageDescription = Lens.lens (\ImportWorkspaceImage' {imageDescription} -> imageDescription) (\s@ImportWorkspaceImage' {} a -> s {imageDescription = a} :: ImportWorkspaceImage)

instance Core.AWSRequest ImportWorkspaceImage where
  type
    AWSResponse ImportWorkspaceImage =
      ImportWorkspaceImageResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ImportWorkspaceImageResponse'
            Core.<$> (x Core..?> "ImageId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ImportWorkspaceImage

instance Core.NFData ImportWorkspaceImage

instance Core.ToHeaders ImportWorkspaceImage where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "WorkspacesService.ImportWorkspaceImage" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ImportWorkspaceImage where
  toJSON ImportWorkspaceImage' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Tags" Core..=) Core.<$> tags,
            ("Applications" Core..=) Core.<$> applications,
            Core.Just ("Ec2ImageId" Core..= ec2ImageId),
            Core.Just
              ("IngestionProcess" Core..= ingestionProcess),
            Core.Just ("ImageName" Core..= imageName),
            Core.Just
              ("ImageDescription" Core..= imageDescription)
          ]
      )

instance Core.ToPath ImportWorkspaceImage where
  toPath = Core.const "/"

instance Core.ToQuery ImportWorkspaceImage where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newImportWorkspaceImageResponse' smart constructor.
data ImportWorkspaceImageResponse = ImportWorkspaceImageResponse'
  { -- | The identifier of the WorkSpace image.
    imageId :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ImportWorkspaceImageResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'imageId', 'importWorkspaceImageResponse_imageId' - The identifier of the WorkSpace image.
--
-- 'httpStatus', 'importWorkspaceImageResponse_httpStatus' - The response's http status code.
newImportWorkspaceImageResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ImportWorkspaceImageResponse
newImportWorkspaceImageResponse pHttpStatus_ =
  ImportWorkspaceImageResponse'
    { imageId =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The identifier of the WorkSpace image.
importWorkspaceImageResponse_imageId :: Lens.Lens' ImportWorkspaceImageResponse (Core.Maybe Core.Text)
importWorkspaceImageResponse_imageId = Lens.lens (\ImportWorkspaceImageResponse' {imageId} -> imageId) (\s@ImportWorkspaceImageResponse' {} a -> s {imageId = a} :: ImportWorkspaceImageResponse)

-- | The response's http status code.
importWorkspaceImageResponse_httpStatus :: Lens.Lens' ImportWorkspaceImageResponse Core.Int
importWorkspaceImageResponse_httpStatus = Lens.lens (\ImportWorkspaceImageResponse' {httpStatus} -> httpStatus) (\s@ImportWorkspaceImageResponse' {} a -> s {httpStatus = a} :: ImportWorkspaceImageResponse)

instance Core.NFData ImportWorkspaceImageResponse
