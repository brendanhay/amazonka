{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECR.Types.ImageDetail
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECR.Types.ImageDetail where

import qualified Network.AWS.Core as Core
import Network.AWS.ECR.Types.ImageScanFindingsSummary
import Network.AWS.ECR.Types.ImageScanStatus
import qualified Network.AWS.Lens as Lens

-- | An object that describes an image returned by a DescribeImages
-- operation.
--
-- /See:/ 'newImageDetail' smart constructor.
data ImageDetail = ImageDetail'
  { -- | The @sha256@ digest of the image manifest.
    imageDigest :: Core.Maybe Core.Text,
    -- | The current state of the scan.
    imageScanStatus :: Core.Maybe ImageScanStatus,
    -- | The list of tags associated with this image.
    imageTags :: Core.Maybe [Core.Text],
    -- | The AWS account ID associated with the registry to which this image
    -- belongs.
    registryId :: Core.Maybe Core.Text,
    -- | The name of the repository to which this image belongs.
    repositoryName :: Core.Maybe Core.Text,
    -- | The artifact media type of the image.
    artifactMediaType :: Core.Maybe Core.Text,
    -- | The size, in bytes, of the image in the repository.
    --
    -- If the image is a manifest list, this will be the max size of all
    -- manifests in the list.
    --
    -- Beginning with Docker version 1.9, the Docker client compresses image
    -- layers before pushing them to a V2 Docker registry. The output of the
    -- @docker images@ command shows the uncompressed image size, so it may
    -- return a larger image size than the image sizes returned by
    -- DescribeImages.
    imageSizeInBytes :: Core.Maybe Core.Integer,
    -- | The media type of the image manifest.
    imageManifestMediaType :: Core.Maybe Core.Text,
    -- | The date and time, expressed in standard JavaScript date format, at
    -- which the current image was pushed to the repository.
    imagePushedAt :: Core.Maybe Core.POSIX,
    -- | A summary of the last completed image scan.
    imageScanFindingsSummary :: Core.Maybe ImageScanFindingsSummary
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ImageDetail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'imageDigest', 'imageDetail_imageDigest' - The @sha256@ digest of the image manifest.
--
-- 'imageScanStatus', 'imageDetail_imageScanStatus' - The current state of the scan.
--
-- 'imageTags', 'imageDetail_imageTags' - The list of tags associated with this image.
--
-- 'registryId', 'imageDetail_registryId' - The AWS account ID associated with the registry to which this image
-- belongs.
--
-- 'repositoryName', 'imageDetail_repositoryName' - The name of the repository to which this image belongs.
--
-- 'artifactMediaType', 'imageDetail_artifactMediaType' - The artifact media type of the image.
--
-- 'imageSizeInBytes', 'imageDetail_imageSizeInBytes' - The size, in bytes, of the image in the repository.
--
-- If the image is a manifest list, this will be the max size of all
-- manifests in the list.
--
-- Beginning with Docker version 1.9, the Docker client compresses image
-- layers before pushing them to a V2 Docker registry. The output of the
-- @docker images@ command shows the uncompressed image size, so it may
-- return a larger image size than the image sizes returned by
-- DescribeImages.
--
-- 'imageManifestMediaType', 'imageDetail_imageManifestMediaType' - The media type of the image manifest.
--
-- 'imagePushedAt', 'imageDetail_imagePushedAt' - The date and time, expressed in standard JavaScript date format, at
-- which the current image was pushed to the repository.
--
-- 'imageScanFindingsSummary', 'imageDetail_imageScanFindingsSummary' - A summary of the last completed image scan.
newImageDetail ::
  ImageDetail
newImageDetail =
  ImageDetail'
    { imageDigest = Core.Nothing,
      imageScanStatus = Core.Nothing,
      imageTags = Core.Nothing,
      registryId = Core.Nothing,
      repositoryName = Core.Nothing,
      artifactMediaType = Core.Nothing,
      imageSizeInBytes = Core.Nothing,
      imageManifestMediaType = Core.Nothing,
      imagePushedAt = Core.Nothing,
      imageScanFindingsSummary = Core.Nothing
    }

-- | The @sha256@ digest of the image manifest.
imageDetail_imageDigest :: Lens.Lens' ImageDetail (Core.Maybe Core.Text)
imageDetail_imageDigest = Lens.lens (\ImageDetail' {imageDigest} -> imageDigest) (\s@ImageDetail' {} a -> s {imageDigest = a} :: ImageDetail)

-- | The current state of the scan.
imageDetail_imageScanStatus :: Lens.Lens' ImageDetail (Core.Maybe ImageScanStatus)
imageDetail_imageScanStatus = Lens.lens (\ImageDetail' {imageScanStatus} -> imageScanStatus) (\s@ImageDetail' {} a -> s {imageScanStatus = a} :: ImageDetail)

-- | The list of tags associated with this image.
imageDetail_imageTags :: Lens.Lens' ImageDetail (Core.Maybe [Core.Text])
imageDetail_imageTags = Lens.lens (\ImageDetail' {imageTags} -> imageTags) (\s@ImageDetail' {} a -> s {imageTags = a} :: ImageDetail) Core.. Lens.mapping Lens._Coerce

-- | The AWS account ID associated with the registry to which this image
-- belongs.
imageDetail_registryId :: Lens.Lens' ImageDetail (Core.Maybe Core.Text)
imageDetail_registryId = Lens.lens (\ImageDetail' {registryId} -> registryId) (\s@ImageDetail' {} a -> s {registryId = a} :: ImageDetail)

-- | The name of the repository to which this image belongs.
imageDetail_repositoryName :: Lens.Lens' ImageDetail (Core.Maybe Core.Text)
imageDetail_repositoryName = Lens.lens (\ImageDetail' {repositoryName} -> repositoryName) (\s@ImageDetail' {} a -> s {repositoryName = a} :: ImageDetail)

-- | The artifact media type of the image.
imageDetail_artifactMediaType :: Lens.Lens' ImageDetail (Core.Maybe Core.Text)
imageDetail_artifactMediaType = Lens.lens (\ImageDetail' {artifactMediaType} -> artifactMediaType) (\s@ImageDetail' {} a -> s {artifactMediaType = a} :: ImageDetail)

-- | The size, in bytes, of the image in the repository.
--
-- If the image is a manifest list, this will be the max size of all
-- manifests in the list.
--
-- Beginning with Docker version 1.9, the Docker client compresses image
-- layers before pushing them to a V2 Docker registry. The output of the
-- @docker images@ command shows the uncompressed image size, so it may
-- return a larger image size than the image sizes returned by
-- DescribeImages.
imageDetail_imageSizeInBytes :: Lens.Lens' ImageDetail (Core.Maybe Core.Integer)
imageDetail_imageSizeInBytes = Lens.lens (\ImageDetail' {imageSizeInBytes} -> imageSizeInBytes) (\s@ImageDetail' {} a -> s {imageSizeInBytes = a} :: ImageDetail)

-- | The media type of the image manifest.
imageDetail_imageManifestMediaType :: Lens.Lens' ImageDetail (Core.Maybe Core.Text)
imageDetail_imageManifestMediaType = Lens.lens (\ImageDetail' {imageManifestMediaType} -> imageManifestMediaType) (\s@ImageDetail' {} a -> s {imageManifestMediaType = a} :: ImageDetail)

-- | The date and time, expressed in standard JavaScript date format, at
-- which the current image was pushed to the repository.
imageDetail_imagePushedAt :: Lens.Lens' ImageDetail (Core.Maybe Core.UTCTime)
imageDetail_imagePushedAt = Lens.lens (\ImageDetail' {imagePushedAt} -> imagePushedAt) (\s@ImageDetail' {} a -> s {imagePushedAt = a} :: ImageDetail) Core.. Lens.mapping Core._Time

-- | A summary of the last completed image scan.
imageDetail_imageScanFindingsSummary :: Lens.Lens' ImageDetail (Core.Maybe ImageScanFindingsSummary)
imageDetail_imageScanFindingsSummary = Lens.lens (\ImageDetail' {imageScanFindingsSummary} -> imageScanFindingsSummary) (\s@ImageDetail' {} a -> s {imageScanFindingsSummary = a} :: ImageDetail)

instance Core.FromJSON ImageDetail where
  parseJSON =
    Core.withObject
      "ImageDetail"
      ( \x ->
          ImageDetail'
            Core.<$> (x Core..:? "imageDigest")
            Core.<*> (x Core..:? "imageScanStatus")
            Core.<*> (x Core..:? "imageTags" Core..!= Core.mempty)
            Core.<*> (x Core..:? "registryId")
            Core.<*> (x Core..:? "repositoryName")
            Core.<*> (x Core..:? "artifactMediaType")
            Core.<*> (x Core..:? "imageSizeInBytes")
            Core.<*> (x Core..:? "imageManifestMediaType")
            Core.<*> (x Core..:? "imagePushedAt")
            Core.<*> (x Core..:? "imageScanFindingsSummary")
      )

instance Core.Hashable ImageDetail

instance Core.NFData ImageDetail
