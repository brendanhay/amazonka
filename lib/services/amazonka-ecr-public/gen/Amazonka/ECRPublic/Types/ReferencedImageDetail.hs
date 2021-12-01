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
-- Module      : Amazonka.ECRPublic.Types.ReferencedImageDetail
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ECRPublic.Types.ReferencedImageDetail where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | An object that describes the image tag details returned by a
-- DescribeImageTags action.
--
-- /See:/ 'newReferencedImageDetail' smart constructor.
data ReferencedImageDetail = ReferencedImageDetail'
  { -- | The media type of the image manifest.
    imageManifestMediaType :: Prelude.Maybe Prelude.Text,
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
    -- | The @sha256@ digest of the image manifest.
    imageDigest :: Prelude.Maybe Prelude.Text,
    -- | The artifact media type of the image.
    artifactMediaType :: Prelude.Maybe Prelude.Text,
    -- | The date and time, expressed in standard JavaScript date format, at
    -- which the current image tag was pushed to the repository.
    imagePushedAt :: Prelude.Maybe Core.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ReferencedImageDetail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'imageManifestMediaType', 'referencedImageDetail_imageManifestMediaType' - The media type of the image manifest.
--
-- 'imageSizeInBytes', 'referencedImageDetail_imageSizeInBytes' - The size, in bytes, of the image in the repository.
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
-- 'imageDigest', 'referencedImageDetail_imageDigest' - The @sha256@ digest of the image manifest.
--
-- 'artifactMediaType', 'referencedImageDetail_artifactMediaType' - The artifact media type of the image.
--
-- 'imagePushedAt', 'referencedImageDetail_imagePushedAt' - The date and time, expressed in standard JavaScript date format, at
-- which the current image tag was pushed to the repository.
newReferencedImageDetail ::
  ReferencedImageDetail
newReferencedImageDetail =
  ReferencedImageDetail'
    { imageManifestMediaType =
        Prelude.Nothing,
      imageSizeInBytes = Prelude.Nothing,
      imageDigest = Prelude.Nothing,
      artifactMediaType = Prelude.Nothing,
      imagePushedAt = Prelude.Nothing
    }

-- | The media type of the image manifest.
referencedImageDetail_imageManifestMediaType :: Lens.Lens' ReferencedImageDetail (Prelude.Maybe Prelude.Text)
referencedImageDetail_imageManifestMediaType = Lens.lens (\ReferencedImageDetail' {imageManifestMediaType} -> imageManifestMediaType) (\s@ReferencedImageDetail' {} a -> s {imageManifestMediaType = a} :: ReferencedImageDetail)

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
referencedImageDetail_imageSizeInBytes :: Lens.Lens' ReferencedImageDetail (Prelude.Maybe Prelude.Integer)
referencedImageDetail_imageSizeInBytes = Lens.lens (\ReferencedImageDetail' {imageSizeInBytes} -> imageSizeInBytes) (\s@ReferencedImageDetail' {} a -> s {imageSizeInBytes = a} :: ReferencedImageDetail)

-- | The @sha256@ digest of the image manifest.
referencedImageDetail_imageDigest :: Lens.Lens' ReferencedImageDetail (Prelude.Maybe Prelude.Text)
referencedImageDetail_imageDigest = Lens.lens (\ReferencedImageDetail' {imageDigest} -> imageDigest) (\s@ReferencedImageDetail' {} a -> s {imageDigest = a} :: ReferencedImageDetail)

-- | The artifact media type of the image.
referencedImageDetail_artifactMediaType :: Lens.Lens' ReferencedImageDetail (Prelude.Maybe Prelude.Text)
referencedImageDetail_artifactMediaType = Lens.lens (\ReferencedImageDetail' {artifactMediaType} -> artifactMediaType) (\s@ReferencedImageDetail' {} a -> s {artifactMediaType = a} :: ReferencedImageDetail)

-- | The date and time, expressed in standard JavaScript date format, at
-- which the current image tag was pushed to the repository.
referencedImageDetail_imagePushedAt :: Lens.Lens' ReferencedImageDetail (Prelude.Maybe Prelude.UTCTime)
referencedImageDetail_imagePushedAt = Lens.lens (\ReferencedImageDetail' {imagePushedAt} -> imagePushedAt) (\s@ReferencedImageDetail' {} a -> s {imagePushedAt = a} :: ReferencedImageDetail) Prelude.. Lens.mapping Core._Time

instance Core.FromJSON ReferencedImageDetail where
  parseJSON =
    Core.withObject
      "ReferencedImageDetail"
      ( \x ->
          ReferencedImageDetail'
            Prelude.<$> (x Core..:? "imageManifestMediaType")
            Prelude.<*> (x Core..:? "imageSizeInBytes")
            Prelude.<*> (x Core..:? "imageDigest")
            Prelude.<*> (x Core..:? "artifactMediaType")
            Prelude.<*> (x Core..:? "imagePushedAt")
      )

instance Prelude.Hashable ReferencedImageDetail where
  hashWithSalt salt' ReferencedImageDetail' {..} =
    salt' `Prelude.hashWithSalt` imagePushedAt
      `Prelude.hashWithSalt` artifactMediaType
      `Prelude.hashWithSalt` imageDigest
      `Prelude.hashWithSalt` imageSizeInBytes
      `Prelude.hashWithSalt` imageManifestMediaType

instance Prelude.NFData ReferencedImageDetail where
  rnf ReferencedImageDetail' {..} =
    Prelude.rnf imageManifestMediaType
      `Prelude.seq` Prelude.rnf imagePushedAt
      `Prelude.seq` Prelude.rnf artifactMediaType
      `Prelude.seq` Prelude.rnf imageDigest
      `Prelude.seq` Prelude.rnf imageSizeInBytes
