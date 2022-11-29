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
-- Module      : Amazonka.ECR.Types.ImageDetail
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ECR.Types.ImageDetail where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.ECR.Types.ImageScanFindingsSummary
import Amazonka.ECR.Types.ImageScanStatus
import qualified Amazonka.Prelude as Prelude

-- | An object that describes an image returned by a DescribeImages
-- operation.
--
-- /See:/ 'newImageDetail' smart constructor.
data ImageDetail = ImageDetail'
  { -- | The artifact media type of the image.
    artifactMediaType :: Prelude.Maybe Prelude.Text,
    -- | The date and time, expressed in standard JavaScript date format, at
    -- which the current image was pushed to the repository.
    imagePushedAt :: Prelude.Maybe Core.POSIX,
    -- | The name of the repository to which this image belongs.
    repositoryName :: Prelude.Maybe Prelude.Text,
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
    -- | The list of tags associated with this image.
    imageTags :: Prelude.Maybe [Prelude.Text],
    -- | The Amazon Web Services account ID associated with the registry to which
    -- this image belongs.
    registryId :: Prelude.Maybe Prelude.Text,
    -- | The media type of the image manifest.
    imageManifestMediaType :: Prelude.Maybe Prelude.Text,
    -- | The @sha256@ digest of the image manifest.
    imageDigest :: Prelude.Maybe Prelude.Text,
    -- | The current state of the scan.
    imageScanStatus :: Prelude.Maybe ImageScanStatus,
    -- | A summary of the last completed image scan.
    imageScanFindingsSummary :: Prelude.Maybe ImageScanFindingsSummary,
    -- | The date and time, expressed in standard JavaScript date format, when
    -- Amazon ECR recorded the last image pull.
    --
    -- Amazon ECR refreshes the last image pull timestamp at least once every
    -- 24 hours. For example, if you pull an image once a day then the
    -- @lastRecordedPullTime@ timestamp will indicate the exact time that the
    -- image was last pulled. However, if you pull an image once an hour,
    -- because Amazon ECR refreshes the @lastRecordedPullTime@ timestamp at
    -- least once every 24 hours, the result may not be the exact time that the
    -- image was last pulled.
    lastRecordedPullTime :: Prelude.Maybe Core.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ImageDetail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'artifactMediaType', 'imageDetail_artifactMediaType' - The artifact media type of the image.
--
-- 'imagePushedAt', 'imageDetail_imagePushedAt' - The date and time, expressed in standard JavaScript date format, at
-- which the current image was pushed to the repository.
--
-- 'repositoryName', 'imageDetail_repositoryName' - The name of the repository to which this image belongs.
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
-- 'imageTags', 'imageDetail_imageTags' - The list of tags associated with this image.
--
-- 'registryId', 'imageDetail_registryId' - The Amazon Web Services account ID associated with the registry to which
-- this image belongs.
--
-- 'imageManifestMediaType', 'imageDetail_imageManifestMediaType' - The media type of the image manifest.
--
-- 'imageDigest', 'imageDetail_imageDigest' - The @sha256@ digest of the image manifest.
--
-- 'imageScanStatus', 'imageDetail_imageScanStatus' - The current state of the scan.
--
-- 'imageScanFindingsSummary', 'imageDetail_imageScanFindingsSummary' - A summary of the last completed image scan.
--
-- 'lastRecordedPullTime', 'imageDetail_lastRecordedPullTime' - The date and time, expressed in standard JavaScript date format, when
-- Amazon ECR recorded the last image pull.
--
-- Amazon ECR refreshes the last image pull timestamp at least once every
-- 24 hours. For example, if you pull an image once a day then the
-- @lastRecordedPullTime@ timestamp will indicate the exact time that the
-- image was last pulled. However, if you pull an image once an hour,
-- because Amazon ECR refreshes the @lastRecordedPullTime@ timestamp at
-- least once every 24 hours, the result may not be the exact time that the
-- image was last pulled.
newImageDetail ::
  ImageDetail
newImageDetail =
  ImageDetail'
    { artifactMediaType = Prelude.Nothing,
      imagePushedAt = Prelude.Nothing,
      repositoryName = Prelude.Nothing,
      imageSizeInBytes = Prelude.Nothing,
      imageTags = Prelude.Nothing,
      registryId = Prelude.Nothing,
      imageManifestMediaType = Prelude.Nothing,
      imageDigest = Prelude.Nothing,
      imageScanStatus = Prelude.Nothing,
      imageScanFindingsSummary = Prelude.Nothing,
      lastRecordedPullTime = Prelude.Nothing
    }

-- | The artifact media type of the image.
imageDetail_artifactMediaType :: Lens.Lens' ImageDetail (Prelude.Maybe Prelude.Text)
imageDetail_artifactMediaType = Lens.lens (\ImageDetail' {artifactMediaType} -> artifactMediaType) (\s@ImageDetail' {} a -> s {artifactMediaType = a} :: ImageDetail)

-- | The date and time, expressed in standard JavaScript date format, at
-- which the current image was pushed to the repository.
imageDetail_imagePushedAt :: Lens.Lens' ImageDetail (Prelude.Maybe Prelude.UTCTime)
imageDetail_imagePushedAt = Lens.lens (\ImageDetail' {imagePushedAt} -> imagePushedAt) (\s@ImageDetail' {} a -> s {imagePushedAt = a} :: ImageDetail) Prelude.. Lens.mapping Core._Time

-- | The name of the repository to which this image belongs.
imageDetail_repositoryName :: Lens.Lens' ImageDetail (Prelude.Maybe Prelude.Text)
imageDetail_repositoryName = Lens.lens (\ImageDetail' {repositoryName} -> repositoryName) (\s@ImageDetail' {} a -> s {repositoryName = a} :: ImageDetail)

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

-- | The list of tags associated with this image.
imageDetail_imageTags :: Lens.Lens' ImageDetail (Prelude.Maybe [Prelude.Text])
imageDetail_imageTags = Lens.lens (\ImageDetail' {imageTags} -> imageTags) (\s@ImageDetail' {} a -> s {imageTags = a} :: ImageDetail) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Web Services account ID associated with the registry to which
-- this image belongs.
imageDetail_registryId :: Lens.Lens' ImageDetail (Prelude.Maybe Prelude.Text)
imageDetail_registryId = Lens.lens (\ImageDetail' {registryId} -> registryId) (\s@ImageDetail' {} a -> s {registryId = a} :: ImageDetail)

-- | The media type of the image manifest.
imageDetail_imageManifestMediaType :: Lens.Lens' ImageDetail (Prelude.Maybe Prelude.Text)
imageDetail_imageManifestMediaType = Lens.lens (\ImageDetail' {imageManifestMediaType} -> imageManifestMediaType) (\s@ImageDetail' {} a -> s {imageManifestMediaType = a} :: ImageDetail)

-- | The @sha256@ digest of the image manifest.
imageDetail_imageDigest :: Lens.Lens' ImageDetail (Prelude.Maybe Prelude.Text)
imageDetail_imageDigest = Lens.lens (\ImageDetail' {imageDigest} -> imageDigest) (\s@ImageDetail' {} a -> s {imageDigest = a} :: ImageDetail)

-- | The current state of the scan.
imageDetail_imageScanStatus :: Lens.Lens' ImageDetail (Prelude.Maybe ImageScanStatus)
imageDetail_imageScanStatus = Lens.lens (\ImageDetail' {imageScanStatus} -> imageScanStatus) (\s@ImageDetail' {} a -> s {imageScanStatus = a} :: ImageDetail)

-- | A summary of the last completed image scan.
imageDetail_imageScanFindingsSummary :: Lens.Lens' ImageDetail (Prelude.Maybe ImageScanFindingsSummary)
imageDetail_imageScanFindingsSummary = Lens.lens (\ImageDetail' {imageScanFindingsSummary} -> imageScanFindingsSummary) (\s@ImageDetail' {} a -> s {imageScanFindingsSummary = a} :: ImageDetail)

-- | The date and time, expressed in standard JavaScript date format, when
-- Amazon ECR recorded the last image pull.
--
-- Amazon ECR refreshes the last image pull timestamp at least once every
-- 24 hours. For example, if you pull an image once a day then the
-- @lastRecordedPullTime@ timestamp will indicate the exact time that the
-- image was last pulled. However, if you pull an image once an hour,
-- because Amazon ECR refreshes the @lastRecordedPullTime@ timestamp at
-- least once every 24 hours, the result may not be the exact time that the
-- image was last pulled.
imageDetail_lastRecordedPullTime :: Lens.Lens' ImageDetail (Prelude.Maybe Prelude.UTCTime)
imageDetail_lastRecordedPullTime = Lens.lens (\ImageDetail' {lastRecordedPullTime} -> lastRecordedPullTime) (\s@ImageDetail' {} a -> s {lastRecordedPullTime = a} :: ImageDetail) Prelude.. Lens.mapping Core._Time

instance Core.FromJSON ImageDetail where
  parseJSON =
    Core.withObject
      "ImageDetail"
      ( \x ->
          ImageDetail'
            Prelude.<$> (x Core..:? "artifactMediaType")
            Prelude.<*> (x Core..:? "imagePushedAt")
            Prelude.<*> (x Core..:? "repositoryName")
            Prelude.<*> (x Core..:? "imageSizeInBytes")
            Prelude.<*> (x Core..:? "imageTags" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "registryId")
            Prelude.<*> (x Core..:? "imageManifestMediaType")
            Prelude.<*> (x Core..:? "imageDigest")
            Prelude.<*> (x Core..:? "imageScanStatus")
            Prelude.<*> (x Core..:? "imageScanFindingsSummary")
            Prelude.<*> (x Core..:? "lastRecordedPullTime")
      )

instance Prelude.Hashable ImageDetail where
  hashWithSalt _salt ImageDetail' {..} =
    _salt `Prelude.hashWithSalt` artifactMediaType
      `Prelude.hashWithSalt` imagePushedAt
      `Prelude.hashWithSalt` repositoryName
      `Prelude.hashWithSalt` imageSizeInBytes
      `Prelude.hashWithSalt` imageTags
      `Prelude.hashWithSalt` registryId
      `Prelude.hashWithSalt` imageManifestMediaType
      `Prelude.hashWithSalt` imageDigest
      `Prelude.hashWithSalt` imageScanStatus
      `Prelude.hashWithSalt` imageScanFindingsSummary
      `Prelude.hashWithSalt` lastRecordedPullTime

instance Prelude.NFData ImageDetail where
  rnf ImageDetail' {..} =
    Prelude.rnf artifactMediaType
      `Prelude.seq` Prelude.rnf imagePushedAt
      `Prelude.seq` Prelude.rnf repositoryName
      `Prelude.seq` Prelude.rnf imageSizeInBytes
      `Prelude.seq` Prelude.rnf imageTags
      `Prelude.seq` Prelude.rnf registryId
      `Prelude.seq` Prelude.rnf imageManifestMediaType
      `Prelude.seq` Prelude.rnf imageDigest
      `Prelude.seq` Prelude.rnf imageScanStatus
      `Prelude.seq` Prelude.rnf imageScanFindingsSummary
      `Prelude.seq` Prelude.rnf lastRecordedPullTime
