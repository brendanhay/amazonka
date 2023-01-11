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
-- Module      : Amazonka.WorkSpaces.CopyWorkspaceImage
-- Copyright   : (c) 2013-2023 Brendan Hay
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
-- In Amazon Web Services GovCloud (US), to copy images to and from other
-- Regions, contact Amazon Web Services Support.
--
-- Before copying a shared image, be sure to verify that it has been shared
-- from the correct Amazon Web Services account. To determine if an image
-- has been shared and to see the ID of the Amazon Web Services account
-- that owns an image, use the
-- <https://docs.aws.amazon.com/workspaces/latest/api/API_DescribeWorkspaceImages.html DescribeWorkSpaceImages>
-- and
-- <https://docs.aws.amazon.com/workspaces/latest/api/API_DescribeWorkspaceImagePermissions.html DescribeWorkspaceImagePermissions>
-- API operations.
module Amazonka.WorkSpaces.CopyWorkspaceImage
  ( -- * Creating a Request
    CopyWorkspaceImage (..),
    newCopyWorkspaceImage,

    -- * Request Lenses
    copyWorkspaceImage_description,
    copyWorkspaceImage_tags,
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WorkSpaces.Types

-- | /See:/ 'newCopyWorkspaceImage' smart constructor.
data CopyWorkspaceImage = CopyWorkspaceImage'
  { -- | A description of the image.
    description :: Prelude.Maybe Prelude.Text,
    -- | The tags for the image.
    tags :: Prelude.Maybe [Tag],
    -- | The name of the image.
    name :: Prelude.Text,
    -- | The identifier of the source image.
    sourceImageId :: Prelude.Text,
    -- | The identifier of the source Region.
    sourceRegion :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CopyWorkspaceImage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'copyWorkspaceImage_description' - A description of the image.
--
-- 'tags', 'copyWorkspaceImage_tags' - The tags for the image.
--
-- 'name', 'copyWorkspaceImage_name' - The name of the image.
--
-- 'sourceImageId', 'copyWorkspaceImage_sourceImageId' - The identifier of the source image.
--
-- 'sourceRegion', 'copyWorkspaceImage_sourceRegion' - The identifier of the source Region.
newCopyWorkspaceImage ::
  -- | 'name'
  Prelude.Text ->
  -- | 'sourceImageId'
  Prelude.Text ->
  -- | 'sourceRegion'
  Prelude.Text ->
  CopyWorkspaceImage
newCopyWorkspaceImage
  pName_
  pSourceImageId_
  pSourceRegion_ =
    CopyWorkspaceImage'
      { description = Prelude.Nothing,
        tags = Prelude.Nothing,
        name = pName_,
        sourceImageId = pSourceImageId_,
        sourceRegion = pSourceRegion_
      }

-- | A description of the image.
copyWorkspaceImage_description :: Lens.Lens' CopyWorkspaceImage (Prelude.Maybe Prelude.Text)
copyWorkspaceImage_description = Lens.lens (\CopyWorkspaceImage' {description} -> description) (\s@CopyWorkspaceImage' {} a -> s {description = a} :: CopyWorkspaceImage)

-- | The tags for the image.
copyWorkspaceImage_tags :: Lens.Lens' CopyWorkspaceImage (Prelude.Maybe [Tag])
copyWorkspaceImage_tags = Lens.lens (\CopyWorkspaceImage' {tags} -> tags) (\s@CopyWorkspaceImage' {} a -> s {tags = a} :: CopyWorkspaceImage) Prelude.. Lens.mapping Lens.coerced

-- | The name of the image.
copyWorkspaceImage_name :: Lens.Lens' CopyWorkspaceImage Prelude.Text
copyWorkspaceImage_name = Lens.lens (\CopyWorkspaceImage' {name} -> name) (\s@CopyWorkspaceImage' {} a -> s {name = a} :: CopyWorkspaceImage)

-- | The identifier of the source image.
copyWorkspaceImage_sourceImageId :: Lens.Lens' CopyWorkspaceImage Prelude.Text
copyWorkspaceImage_sourceImageId = Lens.lens (\CopyWorkspaceImage' {sourceImageId} -> sourceImageId) (\s@CopyWorkspaceImage' {} a -> s {sourceImageId = a} :: CopyWorkspaceImage)

-- | The identifier of the source Region.
copyWorkspaceImage_sourceRegion :: Lens.Lens' CopyWorkspaceImage Prelude.Text
copyWorkspaceImage_sourceRegion = Lens.lens (\CopyWorkspaceImage' {sourceRegion} -> sourceRegion) (\s@CopyWorkspaceImage' {} a -> s {sourceRegion = a} :: CopyWorkspaceImage)

instance Core.AWSRequest CopyWorkspaceImage where
  type
    AWSResponse CopyWorkspaceImage =
      CopyWorkspaceImageResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CopyWorkspaceImageResponse'
            Prelude.<$> (x Data..?> "ImageId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CopyWorkspaceImage where
  hashWithSalt _salt CopyWorkspaceImage' {..} =
    _salt `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` sourceImageId
      `Prelude.hashWithSalt` sourceRegion

instance Prelude.NFData CopyWorkspaceImage where
  rnf CopyWorkspaceImage' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf sourceImageId
      `Prelude.seq` Prelude.rnf sourceRegion

instance Data.ToHeaders CopyWorkspaceImage where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "WorkspacesService.CopyWorkspaceImage" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CopyWorkspaceImage where
  toJSON CopyWorkspaceImage' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Description" Data..=) Prelude.<$> description,
            ("Tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("Name" Data..= name),
            Prelude.Just ("SourceImageId" Data..= sourceImageId),
            Prelude.Just ("SourceRegion" Data..= sourceRegion)
          ]
      )

instance Data.ToPath CopyWorkspaceImage where
  toPath = Prelude.const "/"

instance Data.ToQuery CopyWorkspaceImage where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCopyWorkspaceImageResponse' smart constructor.
data CopyWorkspaceImageResponse = CopyWorkspaceImageResponse'
  { -- | The identifier of the image.
    imageId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  CopyWorkspaceImageResponse
newCopyWorkspaceImageResponse pHttpStatus_ =
  CopyWorkspaceImageResponse'
    { imageId =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The identifier of the image.
copyWorkspaceImageResponse_imageId :: Lens.Lens' CopyWorkspaceImageResponse (Prelude.Maybe Prelude.Text)
copyWorkspaceImageResponse_imageId = Lens.lens (\CopyWorkspaceImageResponse' {imageId} -> imageId) (\s@CopyWorkspaceImageResponse' {} a -> s {imageId = a} :: CopyWorkspaceImageResponse)

-- | The response's http status code.
copyWorkspaceImageResponse_httpStatus :: Lens.Lens' CopyWorkspaceImageResponse Prelude.Int
copyWorkspaceImageResponse_httpStatus = Lens.lens (\CopyWorkspaceImageResponse' {httpStatus} -> httpStatus) (\s@CopyWorkspaceImageResponse' {} a -> s {httpStatus = a} :: CopyWorkspaceImageResponse)

instance Prelude.NFData CopyWorkspaceImageResponse where
  rnf CopyWorkspaceImageResponse' {..} =
    Prelude.rnf imageId
      `Prelude.seq` Prelude.rnf httpStatus
