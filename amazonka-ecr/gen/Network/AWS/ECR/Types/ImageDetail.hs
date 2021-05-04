{-# LANGUAGE DeriveDataTypeable #-}
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

import Network.AWS.ECR.Types.ImageScanFindingsSummary
import Network.AWS.ECR.Types.ImageScanStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | An object that describes an image returned by a DescribeImages
-- operation.
--
-- /See:/ 'newImageDetail' smart constructor.
data ImageDetail = ImageDetail'
  { -- | The @sha256@ digest of the image manifest.
    imageDigest :: Prelude.Maybe Prelude.Text,
    -- | The current state of the scan.
    imageScanStatus :: Prelude.Maybe ImageScanStatus,
    -- | The list of tags associated with this image.
    imageTags :: Prelude.Maybe [Prelude.Text],
    -- | The AWS account ID associated with the registry to which this image
    -- belongs.
    registryId :: Prelude.Maybe Prelude.Text,
    -- | The name of the repository to which this image belongs.
    repositoryName :: Prelude.Maybe Prelude.Text,
    -- | The artifact media type of the image.
    artifactMediaType :: Prelude.Maybe Prelude.Text,
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
    imageSizeInBytes :: Prelude.Maybe Prelude.Integer,
    -- | The media type of the image manifest.
    imageManifestMediaType :: Prelude.Maybe Prelude.Text,
    -- | The date and time, expressed in standard JavaScript date format, at
    -- which the current image was pushed to the repository.
    imagePushedAt :: Prelude.Maybe Prelude.POSIX,
    -- | A summary of the last completed image scan.
    imageScanFindingsSummary :: Prelude.Maybe ImageScanFindingsSummary
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { imageDigest = Prelude.Nothing,
      imageScanStatus = Prelude.Nothing,
      imageTags = Prelude.Nothing,
      registryId = Prelude.Nothing,
      repositoryName = Prelude.Nothing,
      artifactMediaType = Prelude.Nothing,
      imageSizeInBytes = Prelude.Nothing,
      imageManifestMediaType = Prelude.Nothing,
      imagePushedAt = Prelude.Nothing,
      imageScanFindingsSummary = Prelude.Nothing
    }

-- | The @sha256@ digest of the image manifest.
imageDetail_imageDigest :: Lens.Lens' ImageDetail (Prelude.Maybe Prelude.Text)
imageDetail_imageDigest = Lens.lens (\ImageDetail' {imageDigest} -> imageDigest) (\s@ImageDetail' {} a -> s {imageDigest = a} :: ImageDetail)

-- | The current state of the scan.
imageDetail_imageScanStatus :: Lens.Lens' ImageDetail (Prelude.Maybe ImageScanStatus)
imageDetail_imageScanStatus = Lens.lens (\ImageDetail' {imageScanStatus} -> imageScanStatus) (\s@ImageDetail' {} a -> s {imageScanStatus = a} :: ImageDetail)

-- | The list of tags associated with this image.
imageDetail_imageTags :: Lens.Lens' ImageDetail (Prelude.Maybe [Prelude.Text])
imageDetail_imageTags = Lens.lens (\ImageDetail' {imageTags} -> imageTags) (\s@ImageDetail' {} a -> s {imageTags = a} :: ImageDetail) Prelude.. Lens.mapping Prelude._Coerce

-- | The AWS account ID associated with the registry to which this image
-- belongs.
imageDetail_registryId :: Lens.Lens' ImageDetail (Prelude.Maybe Prelude.Text)
imageDetail_registryId = Lens.lens (\ImageDetail' {registryId} -> registryId) (\s@ImageDetail' {} a -> s {registryId = a} :: ImageDetail)

-- | The name of the repository to which this image belongs.
imageDetail_repositoryName :: Lens.Lens' ImageDetail (Prelude.Maybe Prelude.Text)
imageDetail_repositoryName = Lens.lens (\ImageDetail' {repositoryName} -> repositoryName) (\s@ImageDetail' {} a -> s {repositoryName = a} :: ImageDetail)

-- | The artifact media type of the image.
imageDetail_artifactMediaType :: Lens.Lens' ImageDetail (Prelude.Maybe Prelude.Text)
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
imageDetail_imageSizeInBytes :: Lens.Lens' ImageDetail (Prelude.Maybe Prelude.Integer)
imageDetail_imageSizeInBytes = Lens.lens (\ImageDetail' {imageSizeInBytes} -> imageSizeInBytes) (\s@ImageDetail' {} a -> s {imageSizeInBytes = a} :: ImageDetail)

-- | The media type of the image manifest.
imageDetail_imageManifestMediaType :: Lens.Lens' ImageDetail (Prelude.Maybe Prelude.Text)
imageDetail_imageManifestMediaType = Lens.lens (\ImageDetail' {imageManifestMediaType} -> imageManifestMediaType) (\s@ImageDetail' {} a -> s {imageManifestMediaType = a} :: ImageDetail)

-- | The date and time, expressed in standard JavaScript date format, at
-- which the current image was pushed to the repository.
imageDetail_imagePushedAt :: Lens.Lens' ImageDetail (Prelude.Maybe Prelude.UTCTime)
imageDetail_imagePushedAt = Lens.lens (\ImageDetail' {imagePushedAt} -> imagePushedAt) (\s@ImageDetail' {} a -> s {imagePushedAt = a} :: ImageDetail) Prelude.. Lens.mapping Prelude._Time

-- | A summary of the last completed image scan.
imageDetail_imageScanFindingsSummary :: Lens.Lens' ImageDetail (Prelude.Maybe ImageScanFindingsSummary)
imageDetail_imageScanFindingsSummary = Lens.lens (\ImageDetail' {imageScanFindingsSummary} -> imageScanFindingsSummary) (\s@ImageDetail' {} a -> s {imageScanFindingsSummary = a} :: ImageDetail)

instance Prelude.FromJSON ImageDetail where
  parseJSON =
    Prelude.withObject
      "ImageDetail"
      ( \x ->
          ImageDetail'
            Prelude.<$> (x Prelude..:? "imageDigest")
            Prelude.<*> (x Prelude..:? "imageScanStatus")
            Prelude.<*> ( x Prelude..:? "imageTags"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "registryId")
            Prelude.<*> (x Prelude..:? "repositoryName")
            Prelude.<*> (x Prelude..:? "artifactMediaType")
            Prelude.<*> (x Prelude..:? "imageSizeInBytes")
            Prelude.<*> (x Prelude..:? "imageManifestMediaType")
            Prelude.<*> (x Prelude..:? "imagePushedAt")
            Prelude.<*> (x Prelude..:? "imageScanFindingsSummary")
      )

instance Prelude.Hashable ImageDetail

instance Prelude.NFData ImageDetail
