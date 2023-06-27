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
-- Module      : Amazonka.ECRPublic.Types.ImageDetail
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ECRPublic.Types.ImageDetail where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An object that describes an image that\'s returned by a DescribeImages
-- operation.
--
-- /See:/ 'newImageDetail' smart constructor.
data ImageDetail = ImageDetail'
  { -- | The artifact media type of the image.
    artifactMediaType :: Prelude.Maybe Prelude.Text,
    -- | The @sha256@ digest of the image manifest.
    imageDigest :: Prelude.Maybe Prelude.Text,
    -- | The media type of the image manifest.
    imageManifestMediaType :: Prelude.Maybe Prelude.Text,
    -- | The date and time, expressed in standard JavaScript date format, that
    -- the current image was pushed to the repository at.
    imagePushedAt :: Prelude.Maybe Data.POSIX,
    -- | The size, in bytes, of the image in the repository.
    --
    -- If the image is a manifest list, this is the max size of all manifests
    -- in the list.
    --
    -- Beginning with Docker version 1.9, the Docker client compresses image
    -- layers before pushing them to a V2 Docker registry. The output of the
    -- @docker images@ command shows the uncompressed image size, so it might
    -- return a larger image size than the image sizes that are returned by
    -- DescribeImages.
    imageSizeInBytes :: Prelude.Maybe Prelude.Integer,
    -- | The list of tags that\'s associated with this image.
    imageTags :: Prelude.Maybe [Prelude.Text],
    -- | The Amazon Web Services account ID that\'s associated with the public
    -- registry where this image belongs.
    registryId :: Prelude.Maybe Prelude.Text,
    -- | The name of the repository where this image belongs.
    repositoryName :: Prelude.Maybe Prelude.Text
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
-- 'imageDigest', 'imageDetail_imageDigest' - The @sha256@ digest of the image manifest.
--
-- 'imageManifestMediaType', 'imageDetail_imageManifestMediaType' - The media type of the image manifest.
--
-- 'imagePushedAt', 'imageDetail_imagePushedAt' - The date and time, expressed in standard JavaScript date format, that
-- the current image was pushed to the repository at.
--
-- 'imageSizeInBytes', 'imageDetail_imageSizeInBytes' - The size, in bytes, of the image in the repository.
--
-- If the image is a manifest list, this is the max size of all manifests
-- in the list.
--
-- Beginning with Docker version 1.9, the Docker client compresses image
-- layers before pushing them to a V2 Docker registry. The output of the
-- @docker images@ command shows the uncompressed image size, so it might
-- return a larger image size than the image sizes that are returned by
-- DescribeImages.
--
-- 'imageTags', 'imageDetail_imageTags' - The list of tags that\'s associated with this image.
--
-- 'registryId', 'imageDetail_registryId' - The Amazon Web Services account ID that\'s associated with the public
-- registry where this image belongs.
--
-- 'repositoryName', 'imageDetail_repositoryName' - The name of the repository where this image belongs.
newImageDetail ::
  ImageDetail
newImageDetail =
  ImageDetail'
    { artifactMediaType = Prelude.Nothing,
      imageDigest = Prelude.Nothing,
      imageManifestMediaType = Prelude.Nothing,
      imagePushedAt = Prelude.Nothing,
      imageSizeInBytes = Prelude.Nothing,
      imageTags = Prelude.Nothing,
      registryId = Prelude.Nothing,
      repositoryName = Prelude.Nothing
    }

-- | The artifact media type of the image.
imageDetail_artifactMediaType :: Lens.Lens' ImageDetail (Prelude.Maybe Prelude.Text)
imageDetail_artifactMediaType = Lens.lens (\ImageDetail' {artifactMediaType} -> artifactMediaType) (\s@ImageDetail' {} a -> s {artifactMediaType = a} :: ImageDetail)

-- | The @sha256@ digest of the image manifest.
imageDetail_imageDigest :: Lens.Lens' ImageDetail (Prelude.Maybe Prelude.Text)
imageDetail_imageDigest = Lens.lens (\ImageDetail' {imageDigest} -> imageDigest) (\s@ImageDetail' {} a -> s {imageDigest = a} :: ImageDetail)

-- | The media type of the image manifest.
imageDetail_imageManifestMediaType :: Lens.Lens' ImageDetail (Prelude.Maybe Prelude.Text)
imageDetail_imageManifestMediaType = Lens.lens (\ImageDetail' {imageManifestMediaType} -> imageManifestMediaType) (\s@ImageDetail' {} a -> s {imageManifestMediaType = a} :: ImageDetail)

-- | The date and time, expressed in standard JavaScript date format, that
-- the current image was pushed to the repository at.
imageDetail_imagePushedAt :: Lens.Lens' ImageDetail (Prelude.Maybe Prelude.UTCTime)
imageDetail_imagePushedAt = Lens.lens (\ImageDetail' {imagePushedAt} -> imagePushedAt) (\s@ImageDetail' {} a -> s {imagePushedAt = a} :: ImageDetail) Prelude.. Lens.mapping Data._Time

-- | The size, in bytes, of the image in the repository.
--
-- If the image is a manifest list, this is the max size of all manifests
-- in the list.
--
-- Beginning with Docker version 1.9, the Docker client compresses image
-- layers before pushing them to a V2 Docker registry. The output of the
-- @docker images@ command shows the uncompressed image size, so it might
-- return a larger image size than the image sizes that are returned by
-- DescribeImages.
imageDetail_imageSizeInBytes :: Lens.Lens' ImageDetail (Prelude.Maybe Prelude.Integer)
imageDetail_imageSizeInBytes = Lens.lens (\ImageDetail' {imageSizeInBytes} -> imageSizeInBytes) (\s@ImageDetail' {} a -> s {imageSizeInBytes = a} :: ImageDetail)

-- | The list of tags that\'s associated with this image.
imageDetail_imageTags :: Lens.Lens' ImageDetail (Prelude.Maybe [Prelude.Text])
imageDetail_imageTags = Lens.lens (\ImageDetail' {imageTags} -> imageTags) (\s@ImageDetail' {} a -> s {imageTags = a} :: ImageDetail) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Web Services account ID that\'s associated with the public
-- registry where this image belongs.
imageDetail_registryId :: Lens.Lens' ImageDetail (Prelude.Maybe Prelude.Text)
imageDetail_registryId = Lens.lens (\ImageDetail' {registryId} -> registryId) (\s@ImageDetail' {} a -> s {registryId = a} :: ImageDetail)

-- | The name of the repository where this image belongs.
imageDetail_repositoryName :: Lens.Lens' ImageDetail (Prelude.Maybe Prelude.Text)
imageDetail_repositoryName = Lens.lens (\ImageDetail' {repositoryName} -> repositoryName) (\s@ImageDetail' {} a -> s {repositoryName = a} :: ImageDetail)

instance Data.FromJSON ImageDetail where
  parseJSON =
    Data.withObject
      "ImageDetail"
      ( \x ->
          ImageDetail'
            Prelude.<$> (x Data..:? "artifactMediaType")
            Prelude.<*> (x Data..:? "imageDigest")
            Prelude.<*> (x Data..:? "imageManifestMediaType")
            Prelude.<*> (x Data..:? "imagePushedAt")
            Prelude.<*> (x Data..:? "imageSizeInBytes")
            Prelude.<*> (x Data..:? "imageTags" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "registryId")
            Prelude.<*> (x Data..:? "repositoryName")
      )

instance Prelude.Hashable ImageDetail where
  hashWithSalt _salt ImageDetail' {..} =
    _salt
      `Prelude.hashWithSalt` artifactMediaType
      `Prelude.hashWithSalt` imageDigest
      `Prelude.hashWithSalt` imageManifestMediaType
      `Prelude.hashWithSalt` imagePushedAt
      `Prelude.hashWithSalt` imageSizeInBytes
      `Prelude.hashWithSalt` imageTags
      `Prelude.hashWithSalt` registryId
      `Prelude.hashWithSalt` repositoryName

instance Prelude.NFData ImageDetail where
  rnf ImageDetail' {..} =
    Prelude.rnf artifactMediaType
      `Prelude.seq` Prelude.rnf imageDigest
      `Prelude.seq` Prelude.rnf imageManifestMediaType
      `Prelude.seq` Prelude.rnf imagePushedAt
      `Prelude.seq` Prelude.rnf imageSizeInBytes
      `Prelude.seq` Prelude.rnf imageTags
      `Prelude.seq` Prelude.rnf registryId
      `Prelude.seq` Prelude.rnf repositoryName
