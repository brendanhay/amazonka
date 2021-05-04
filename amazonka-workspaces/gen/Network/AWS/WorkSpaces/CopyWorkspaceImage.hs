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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.WorkSpaces.Types

-- | /See:/ 'newCopyWorkspaceImage' smart constructor.
data CopyWorkspaceImage = CopyWorkspaceImage'
  { -- | The tags for the image.
    tags :: Prelude.Maybe [Tag],
    -- | A description of the image.
    description :: Prelude.Maybe Prelude.Text,
    -- | The name of the image.
    name :: Prelude.Text,
    -- | The identifier of the source image.
    sourceImageId :: Prelude.Text,
    -- | The identifier of the source Region.
    sourceRegion :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
      { tags = Prelude.Nothing,
        description = Prelude.Nothing,
        name = pName_,
        sourceImageId = pSourceImageId_,
        sourceRegion = pSourceRegion_
      }

-- | The tags for the image.
copyWorkspaceImage_tags :: Lens.Lens' CopyWorkspaceImage (Prelude.Maybe [Tag])
copyWorkspaceImage_tags = Lens.lens (\CopyWorkspaceImage' {tags} -> tags) (\s@CopyWorkspaceImage' {} a -> s {tags = a} :: CopyWorkspaceImage) Prelude.. Lens.mapping Prelude._Coerce

-- | A description of the image.
copyWorkspaceImage_description :: Lens.Lens' CopyWorkspaceImage (Prelude.Maybe Prelude.Text)
copyWorkspaceImage_description = Lens.lens (\CopyWorkspaceImage' {description} -> description) (\s@CopyWorkspaceImage' {} a -> s {description = a} :: CopyWorkspaceImage)

-- | The name of the image.
copyWorkspaceImage_name :: Lens.Lens' CopyWorkspaceImage Prelude.Text
copyWorkspaceImage_name = Lens.lens (\CopyWorkspaceImage' {name} -> name) (\s@CopyWorkspaceImage' {} a -> s {name = a} :: CopyWorkspaceImage)

-- | The identifier of the source image.
copyWorkspaceImage_sourceImageId :: Lens.Lens' CopyWorkspaceImage Prelude.Text
copyWorkspaceImage_sourceImageId = Lens.lens (\CopyWorkspaceImage' {sourceImageId} -> sourceImageId) (\s@CopyWorkspaceImage' {} a -> s {sourceImageId = a} :: CopyWorkspaceImage)

-- | The identifier of the source Region.
copyWorkspaceImage_sourceRegion :: Lens.Lens' CopyWorkspaceImage Prelude.Text
copyWorkspaceImage_sourceRegion = Lens.lens (\CopyWorkspaceImage' {sourceRegion} -> sourceRegion) (\s@CopyWorkspaceImage' {} a -> s {sourceRegion = a} :: CopyWorkspaceImage)

instance Prelude.AWSRequest CopyWorkspaceImage where
  type
    Rs CopyWorkspaceImage =
      CopyWorkspaceImageResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CopyWorkspaceImageResponse'
            Prelude.<$> (x Prelude..?> "ImageId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CopyWorkspaceImage

instance Prelude.NFData CopyWorkspaceImage

instance Prelude.ToHeaders CopyWorkspaceImage where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "WorkspacesService.CopyWorkspaceImage" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON CopyWorkspaceImage where
  toJSON CopyWorkspaceImage' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("Tags" Prelude..=) Prelude.<$> tags,
            ("Description" Prelude..=) Prelude.<$> description,
            Prelude.Just ("Name" Prelude..= name),
            Prelude.Just
              ("SourceImageId" Prelude..= sourceImageId),
            Prelude.Just
              ("SourceRegion" Prelude..= sourceRegion)
          ]
      )

instance Prelude.ToPath CopyWorkspaceImage where
  toPath = Prelude.const "/"

instance Prelude.ToQuery CopyWorkspaceImage where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCopyWorkspaceImageResponse' smart constructor.
data CopyWorkspaceImageResponse = CopyWorkspaceImageResponse'
  { -- | The identifier of the image.
    imageId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.NFData CopyWorkspaceImageResponse
