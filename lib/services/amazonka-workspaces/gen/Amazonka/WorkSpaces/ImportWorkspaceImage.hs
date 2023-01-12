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
-- Module      : Amazonka.WorkSpaces.ImportWorkspaceImage
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Imports the specified Windows 10 Bring Your Own License (BYOL) or
-- Windows Server 2016 BYOL image into Amazon WorkSpaces. The image must be
-- an already licensed Amazon EC2 image that is in your Amazon Web Services
-- account, and you must own the image. For more information about creating
-- BYOL images, see
-- <https://docs.aws.amazon.com/workspaces/latest/adminguide/byol-windows-images.html Bring Your Own Windows Desktop Licenses>.
module Amazonka.WorkSpaces.ImportWorkspaceImage
  ( -- * Creating a Request
    ImportWorkspaceImage (..),
    newImportWorkspaceImage,

    -- * Request Lenses
    importWorkspaceImage_applications,
    importWorkspaceImage_tags,
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WorkSpaces.Types

-- | /See:/ 'newImportWorkspaceImage' smart constructor.
data ImportWorkspaceImage = ImportWorkspaceImage'
  { -- | If specified, the version of Microsoft Office to subscribe to. Valid
    -- only for Windows 10 BYOL images. For more information about subscribing
    -- to Office for BYOL images, see
    -- <https://docs.aws.amazon.com/workspaces/latest/adminguide/byol-windows-images.html Bring Your Own Windows Desktop Licenses>.
    --
    -- Although this parameter is an array, only one item is allowed at this
    -- time.
    applications :: Prelude.Maybe (Prelude.NonEmpty Application),
    -- | The tags. Each WorkSpaces resource can have a maximum of 50 tags.
    tags :: Prelude.Maybe [Tag],
    -- | The identifier of the EC2 image.
    ec2ImageId :: Prelude.Text,
    -- | The ingestion process to be used when importing the image, depending on
    -- which protocol you want to use for your BYOL Workspace image, either
    -- PCoIP, WorkSpaces Streaming Protocol (WSP), or bring your own protocol
    -- (BYOP). To use WSP, specify a value that ends in @_WSP@. To use PCoIP,
    -- specify a value that does not end in @_WSP@. To use BYOP, specify a
    -- value that ends in @_BYOP@.
    --
    -- For non-GPU-enabled bundles (bundles other than Graphics or
    -- GraphicsPro), specify @BYOL_REGULAR@, @BYOL_REGULAR_WSP@, or
    -- @BYOL_REGULAR_BYOP@, depending on the protocol.
    --
    -- The @BYOL_REGULAR_BYOP@ and @BYOL_GRAPHICS_G4DN_BYOP@ values are only
    -- supported by Amazon WorkSpaces Core. Contact your account team to be
    -- allow-listed to use these values. For more information, see
    -- <http://aws.amazon.com/workspaces/core/ Amazon WorkSpaces Core>.
    ingestionProcess :: WorkspaceImageIngestionProcess,
    -- | The name of the WorkSpace image.
    imageName :: Prelude.Text,
    -- | The description of the WorkSpace image.
    imageDescription :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ImportWorkspaceImage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applications', 'importWorkspaceImage_applications' - If specified, the version of Microsoft Office to subscribe to. Valid
-- only for Windows 10 BYOL images. For more information about subscribing
-- to Office for BYOL images, see
-- <https://docs.aws.amazon.com/workspaces/latest/adminguide/byol-windows-images.html Bring Your Own Windows Desktop Licenses>.
--
-- Although this parameter is an array, only one item is allowed at this
-- time.
--
-- 'tags', 'importWorkspaceImage_tags' - The tags. Each WorkSpaces resource can have a maximum of 50 tags.
--
-- 'ec2ImageId', 'importWorkspaceImage_ec2ImageId' - The identifier of the EC2 image.
--
-- 'ingestionProcess', 'importWorkspaceImage_ingestionProcess' - The ingestion process to be used when importing the image, depending on
-- which protocol you want to use for your BYOL Workspace image, either
-- PCoIP, WorkSpaces Streaming Protocol (WSP), or bring your own protocol
-- (BYOP). To use WSP, specify a value that ends in @_WSP@. To use PCoIP,
-- specify a value that does not end in @_WSP@. To use BYOP, specify a
-- value that ends in @_BYOP@.
--
-- For non-GPU-enabled bundles (bundles other than Graphics or
-- GraphicsPro), specify @BYOL_REGULAR@, @BYOL_REGULAR_WSP@, or
-- @BYOL_REGULAR_BYOP@, depending on the protocol.
--
-- The @BYOL_REGULAR_BYOP@ and @BYOL_GRAPHICS_G4DN_BYOP@ values are only
-- supported by Amazon WorkSpaces Core. Contact your account team to be
-- allow-listed to use these values. For more information, see
-- <http://aws.amazon.com/workspaces/core/ Amazon WorkSpaces Core>.
--
-- 'imageName', 'importWorkspaceImage_imageName' - The name of the WorkSpace image.
--
-- 'imageDescription', 'importWorkspaceImage_imageDescription' - The description of the WorkSpace image.
newImportWorkspaceImage ::
  -- | 'ec2ImageId'
  Prelude.Text ->
  -- | 'ingestionProcess'
  WorkspaceImageIngestionProcess ->
  -- | 'imageName'
  Prelude.Text ->
  -- | 'imageDescription'
  Prelude.Text ->
  ImportWorkspaceImage
newImportWorkspaceImage
  pEc2ImageId_
  pIngestionProcess_
  pImageName_
  pImageDescription_ =
    ImportWorkspaceImage'
      { applications =
          Prelude.Nothing,
        tags = Prelude.Nothing,
        ec2ImageId = pEc2ImageId_,
        ingestionProcess = pIngestionProcess_,
        imageName = pImageName_,
        imageDescription = pImageDescription_
      }

-- | If specified, the version of Microsoft Office to subscribe to. Valid
-- only for Windows 10 BYOL images. For more information about subscribing
-- to Office for BYOL images, see
-- <https://docs.aws.amazon.com/workspaces/latest/adminguide/byol-windows-images.html Bring Your Own Windows Desktop Licenses>.
--
-- Although this parameter is an array, only one item is allowed at this
-- time.
importWorkspaceImage_applications :: Lens.Lens' ImportWorkspaceImage (Prelude.Maybe (Prelude.NonEmpty Application))
importWorkspaceImage_applications = Lens.lens (\ImportWorkspaceImage' {applications} -> applications) (\s@ImportWorkspaceImage' {} a -> s {applications = a} :: ImportWorkspaceImage) Prelude.. Lens.mapping Lens.coerced

-- | The tags. Each WorkSpaces resource can have a maximum of 50 tags.
importWorkspaceImage_tags :: Lens.Lens' ImportWorkspaceImage (Prelude.Maybe [Tag])
importWorkspaceImage_tags = Lens.lens (\ImportWorkspaceImage' {tags} -> tags) (\s@ImportWorkspaceImage' {} a -> s {tags = a} :: ImportWorkspaceImage) Prelude.. Lens.mapping Lens.coerced

-- | The identifier of the EC2 image.
importWorkspaceImage_ec2ImageId :: Lens.Lens' ImportWorkspaceImage Prelude.Text
importWorkspaceImage_ec2ImageId = Lens.lens (\ImportWorkspaceImage' {ec2ImageId} -> ec2ImageId) (\s@ImportWorkspaceImage' {} a -> s {ec2ImageId = a} :: ImportWorkspaceImage)

-- | The ingestion process to be used when importing the image, depending on
-- which protocol you want to use for your BYOL Workspace image, either
-- PCoIP, WorkSpaces Streaming Protocol (WSP), or bring your own protocol
-- (BYOP). To use WSP, specify a value that ends in @_WSP@. To use PCoIP,
-- specify a value that does not end in @_WSP@. To use BYOP, specify a
-- value that ends in @_BYOP@.
--
-- For non-GPU-enabled bundles (bundles other than Graphics or
-- GraphicsPro), specify @BYOL_REGULAR@, @BYOL_REGULAR_WSP@, or
-- @BYOL_REGULAR_BYOP@, depending on the protocol.
--
-- The @BYOL_REGULAR_BYOP@ and @BYOL_GRAPHICS_G4DN_BYOP@ values are only
-- supported by Amazon WorkSpaces Core. Contact your account team to be
-- allow-listed to use these values. For more information, see
-- <http://aws.amazon.com/workspaces/core/ Amazon WorkSpaces Core>.
importWorkspaceImage_ingestionProcess :: Lens.Lens' ImportWorkspaceImage WorkspaceImageIngestionProcess
importWorkspaceImage_ingestionProcess = Lens.lens (\ImportWorkspaceImage' {ingestionProcess} -> ingestionProcess) (\s@ImportWorkspaceImage' {} a -> s {ingestionProcess = a} :: ImportWorkspaceImage)

-- | The name of the WorkSpace image.
importWorkspaceImage_imageName :: Lens.Lens' ImportWorkspaceImage Prelude.Text
importWorkspaceImage_imageName = Lens.lens (\ImportWorkspaceImage' {imageName} -> imageName) (\s@ImportWorkspaceImage' {} a -> s {imageName = a} :: ImportWorkspaceImage)

-- | The description of the WorkSpace image.
importWorkspaceImage_imageDescription :: Lens.Lens' ImportWorkspaceImage Prelude.Text
importWorkspaceImage_imageDescription = Lens.lens (\ImportWorkspaceImage' {imageDescription} -> imageDescription) (\s@ImportWorkspaceImage' {} a -> s {imageDescription = a} :: ImportWorkspaceImage)

instance Core.AWSRequest ImportWorkspaceImage where
  type
    AWSResponse ImportWorkspaceImage =
      ImportWorkspaceImageResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ImportWorkspaceImageResponse'
            Prelude.<$> (x Data..?> "ImageId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ImportWorkspaceImage where
  hashWithSalt _salt ImportWorkspaceImage' {..} =
    _salt `Prelude.hashWithSalt` applications
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` ec2ImageId
      `Prelude.hashWithSalt` ingestionProcess
      `Prelude.hashWithSalt` imageName
      `Prelude.hashWithSalt` imageDescription

instance Prelude.NFData ImportWorkspaceImage where
  rnf ImportWorkspaceImage' {..} =
    Prelude.rnf applications
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf ec2ImageId
      `Prelude.seq` Prelude.rnf ingestionProcess
      `Prelude.seq` Prelude.rnf imageName
      `Prelude.seq` Prelude.rnf imageDescription

instance Data.ToHeaders ImportWorkspaceImage where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "WorkspacesService.ImportWorkspaceImage" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ImportWorkspaceImage where
  toJSON ImportWorkspaceImage' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Applications" Data..=) Prelude.<$> applications,
            ("Tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("Ec2ImageId" Data..= ec2ImageId),
            Prelude.Just
              ("IngestionProcess" Data..= ingestionProcess),
            Prelude.Just ("ImageName" Data..= imageName),
            Prelude.Just
              ("ImageDescription" Data..= imageDescription)
          ]
      )

instance Data.ToPath ImportWorkspaceImage where
  toPath = Prelude.const "/"

instance Data.ToQuery ImportWorkspaceImage where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newImportWorkspaceImageResponse' smart constructor.
data ImportWorkspaceImageResponse = ImportWorkspaceImageResponse'
  { -- | The identifier of the WorkSpace image.
    imageId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  ImportWorkspaceImageResponse
newImportWorkspaceImageResponse pHttpStatus_ =
  ImportWorkspaceImageResponse'
    { imageId =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The identifier of the WorkSpace image.
importWorkspaceImageResponse_imageId :: Lens.Lens' ImportWorkspaceImageResponse (Prelude.Maybe Prelude.Text)
importWorkspaceImageResponse_imageId = Lens.lens (\ImportWorkspaceImageResponse' {imageId} -> imageId) (\s@ImportWorkspaceImageResponse' {} a -> s {imageId = a} :: ImportWorkspaceImageResponse)

-- | The response's http status code.
importWorkspaceImageResponse_httpStatus :: Lens.Lens' ImportWorkspaceImageResponse Prelude.Int
importWorkspaceImageResponse_httpStatus = Lens.lens (\ImportWorkspaceImageResponse' {httpStatus} -> httpStatus) (\s@ImportWorkspaceImageResponse' {} a -> s {httpStatus = a} :: ImportWorkspaceImageResponse)

instance Prelude.NFData ImportWorkspaceImageResponse where
  rnf ImportWorkspaceImageResponse' {..} =
    Prelude.rnf imageId
      `Prelude.seq` Prelude.rnf httpStatus
