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
-- Module      : Network.AWS.AppStream.CopyImage
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Copies the image within the same region or to a new region within the
-- same AWS account. Note that any tags you added to the image will not be
-- copied.
module Network.AWS.AppStream.CopyImage
  ( -- * Creating a Request
    CopyImage (..),
    newCopyImage,

    -- * Request Lenses
    copyImage_destinationImageDescription,
    copyImage_sourceImageName,
    copyImage_destinationImageName,
    copyImage_destinationRegion,

    -- * Destructuring the Response
    CopyImageResponse (..),
    newCopyImageResponse,

    -- * Response Lenses
    copyImageResponse_destinationImageName,
    copyImageResponse_httpStatus,
  )
where

import Network.AWS.AppStream.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCopyImage' smart constructor.
data CopyImage = CopyImage'
  { -- | The description that the image will have when it is copied to the
    -- destination.
    destinationImageDescription :: Core.Maybe Core.Text,
    -- | The name of the image to copy.
    sourceImageName :: Core.Text,
    -- | The name that the image will have when it is copied to the destination.
    destinationImageName :: Core.Text,
    -- | The destination region to which the image will be copied. This parameter
    -- is required, even if you are copying an image within the same region.
    destinationRegion :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CopyImage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'destinationImageDescription', 'copyImage_destinationImageDescription' - The description that the image will have when it is copied to the
-- destination.
--
-- 'sourceImageName', 'copyImage_sourceImageName' - The name of the image to copy.
--
-- 'destinationImageName', 'copyImage_destinationImageName' - The name that the image will have when it is copied to the destination.
--
-- 'destinationRegion', 'copyImage_destinationRegion' - The destination region to which the image will be copied. This parameter
-- is required, even if you are copying an image within the same region.
newCopyImage ::
  -- | 'sourceImageName'
  Core.Text ->
  -- | 'destinationImageName'
  Core.Text ->
  -- | 'destinationRegion'
  Core.Text ->
  CopyImage
newCopyImage
  pSourceImageName_
  pDestinationImageName_
  pDestinationRegion_ =
    CopyImage'
      { destinationImageDescription =
          Core.Nothing,
        sourceImageName = pSourceImageName_,
        destinationImageName = pDestinationImageName_,
        destinationRegion = pDestinationRegion_
      }

-- | The description that the image will have when it is copied to the
-- destination.
copyImage_destinationImageDescription :: Lens.Lens' CopyImage (Core.Maybe Core.Text)
copyImage_destinationImageDescription = Lens.lens (\CopyImage' {destinationImageDescription} -> destinationImageDescription) (\s@CopyImage' {} a -> s {destinationImageDescription = a} :: CopyImage)

-- | The name of the image to copy.
copyImage_sourceImageName :: Lens.Lens' CopyImage Core.Text
copyImage_sourceImageName = Lens.lens (\CopyImage' {sourceImageName} -> sourceImageName) (\s@CopyImage' {} a -> s {sourceImageName = a} :: CopyImage)

-- | The name that the image will have when it is copied to the destination.
copyImage_destinationImageName :: Lens.Lens' CopyImage Core.Text
copyImage_destinationImageName = Lens.lens (\CopyImage' {destinationImageName} -> destinationImageName) (\s@CopyImage' {} a -> s {destinationImageName = a} :: CopyImage)

-- | The destination region to which the image will be copied. This parameter
-- is required, even if you are copying an image within the same region.
copyImage_destinationRegion :: Lens.Lens' CopyImage Core.Text
copyImage_destinationRegion = Lens.lens (\CopyImage' {destinationRegion} -> destinationRegion) (\s@CopyImage' {} a -> s {destinationRegion = a} :: CopyImage)

instance Core.AWSRequest CopyImage where
  type AWSResponse CopyImage = CopyImageResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CopyImageResponse'
            Core.<$> (x Core..?> "DestinationImageName")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CopyImage

instance Core.NFData CopyImage

instance Core.ToHeaders CopyImage where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "PhotonAdminProxyService.CopyImage" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON CopyImage where
  toJSON CopyImage' {..} =
    Core.object
      ( Core.catMaybes
          [ ("DestinationImageDescription" Core..=)
              Core.<$> destinationImageDescription,
            Core.Just
              ("SourceImageName" Core..= sourceImageName),
            Core.Just
              ( "DestinationImageName"
                  Core..= destinationImageName
              ),
            Core.Just
              ("DestinationRegion" Core..= destinationRegion)
          ]
      )

instance Core.ToPath CopyImage where
  toPath = Core.const "/"

instance Core.ToQuery CopyImage where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newCopyImageResponse' smart constructor.
data CopyImageResponse = CopyImageResponse'
  { -- | The name of the destination image.
    destinationImageName :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CopyImageResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'destinationImageName', 'copyImageResponse_destinationImageName' - The name of the destination image.
--
-- 'httpStatus', 'copyImageResponse_httpStatus' - The response's http status code.
newCopyImageResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CopyImageResponse
newCopyImageResponse pHttpStatus_ =
  CopyImageResponse'
    { destinationImageName =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The name of the destination image.
copyImageResponse_destinationImageName :: Lens.Lens' CopyImageResponse (Core.Maybe Core.Text)
copyImageResponse_destinationImageName = Lens.lens (\CopyImageResponse' {destinationImageName} -> destinationImageName) (\s@CopyImageResponse' {} a -> s {destinationImageName = a} :: CopyImageResponse)

-- | The response's http status code.
copyImageResponse_httpStatus :: Lens.Lens' CopyImageResponse Core.Int
copyImageResponse_httpStatus = Lens.lens (\CopyImageResponse' {httpStatus} -> httpStatus) (\s@CopyImageResponse' {} a -> s {httpStatus = a} :: CopyImageResponse)

instance Core.NFData CopyImageResponse
