-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECR.Types.ImageDetail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECR.Types.ImageDetail
  ( ImageDetail (..),

    -- * Smart constructor
    mkImageDetail,

    -- * Lenses
    idRegistryId,
    idImageTags,
    idImageScanStatus,
    idImageManifestMediaType,
    idImageSizeInBytes,
    idImageDigest,
    idImageScanFindingsSummary,
    idArtifactMediaType,
    idImagePushedAt,
    idRepositoryName,
  )
where

import Network.AWS.ECR.Types.ImageScanFindingsSummary
import Network.AWS.ECR.Types.ImageScanStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | An object that describes an image returned by a 'DescribeImages' operation.
--
-- /See:/ 'mkImageDetail' smart constructor.
data ImageDetail = ImageDetail'
  { registryId :: Lude.Maybe Lude.Text,
    imageTags :: Lude.Maybe [Lude.Text],
    imageScanStatus :: Lude.Maybe ImageScanStatus,
    imageManifestMediaType :: Lude.Maybe Lude.Text,
    imageSizeInBytes :: Lude.Maybe Lude.Integer,
    imageDigest :: Lude.Maybe Lude.Text,
    imageScanFindingsSummary :: Lude.Maybe ImageScanFindingsSummary,
    artifactMediaType :: Lude.Maybe Lude.Text,
    imagePushedAt :: Lude.Maybe Lude.Timestamp,
    repositoryName :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ImageDetail' with the minimum fields required to make a request.
--
-- * 'artifactMediaType' - The artifact media type of the image.
-- * 'imageDigest' - The @sha256@ digest of the image manifest.
-- * 'imageManifestMediaType' - The media type of the image manifest.
-- * 'imagePushedAt' - The date and time, expressed in standard JavaScript date format, at which the current image was pushed to the repository.
-- * 'imageScanFindingsSummary' - A summary of the last completed image scan.
-- * 'imageScanStatus' - The current state of the scan.
-- * 'imageSizeInBytes' - The size, in bytes, of the image in the repository.
--
-- If the image is a manifest list, this will be the max size of all manifests in the list.
-- * 'imageTags' - The list of tags associated with this image.
-- * 'registryId' - The AWS account ID associated with the registry to which this image belongs.
-- * 'repositoryName' - The name of the repository to which this image belongs.
mkImageDetail ::
  ImageDetail
mkImageDetail =
  ImageDetail'
    { registryId = Lude.Nothing,
      imageTags = Lude.Nothing,
      imageScanStatus = Lude.Nothing,
      imageManifestMediaType = Lude.Nothing,
      imageSizeInBytes = Lude.Nothing,
      imageDigest = Lude.Nothing,
      imageScanFindingsSummary = Lude.Nothing,
      artifactMediaType = Lude.Nothing,
      imagePushedAt = Lude.Nothing,
      repositoryName = Lude.Nothing
    }

-- | The AWS account ID associated with the registry to which this image belongs.
--
-- /Note:/ Consider using 'registryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idRegistryId :: Lens.Lens' ImageDetail (Lude.Maybe Lude.Text)
idRegistryId = Lens.lens (registryId :: ImageDetail -> Lude.Maybe Lude.Text) (\s a -> s {registryId = a} :: ImageDetail)
{-# DEPRECATED idRegistryId "Use generic-lens or generic-optics with 'registryId' instead." #-}

-- | The list of tags associated with this image.
--
-- /Note:/ Consider using 'imageTags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idImageTags :: Lens.Lens' ImageDetail (Lude.Maybe [Lude.Text])
idImageTags = Lens.lens (imageTags :: ImageDetail -> Lude.Maybe [Lude.Text]) (\s a -> s {imageTags = a} :: ImageDetail)
{-# DEPRECATED idImageTags "Use generic-lens or generic-optics with 'imageTags' instead." #-}

-- | The current state of the scan.
--
-- /Note:/ Consider using 'imageScanStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idImageScanStatus :: Lens.Lens' ImageDetail (Lude.Maybe ImageScanStatus)
idImageScanStatus = Lens.lens (imageScanStatus :: ImageDetail -> Lude.Maybe ImageScanStatus) (\s a -> s {imageScanStatus = a} :: ImageDetail)
{-# DEPRECATED idImageScanStatus "Use generic-lens or generic-optics with 'imageScanStatus' instead." #-}

-- | The media type of the image manifest.
--
-- /Note:/ Consider using 'imageManifestMediaType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idImageManifestMediaType :: Lens.Lens' ImageDetail (Lude.Maybe Lude.Text)
idImageManifestMediaType = Lens.lens (imageManifestMediaType :: ImageDetail -> Lude.Maybe Lude.Text) (\s a -> s {imageManifestMediaType = a} :: ImageDetail)
{-# DEPRECATED idImageManifestMediaType "Use generic-lens or generic-optics with 'imageManifestMediaType' instead." #-}

-- | The size, in bytes, of the image in the repository.
--
-- If the image is a manifest list, this will be the max size of all manifests in the list.
--
-- /Note:/ Consider using 'imageSizeInBytes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idImageSizeInBytes :: Lens.Lens' ImageDetail (Lude.Maybe Lude.Integer)
idImageSizeInBytes = Lens.lens (imageSizeInBytes :: ImageDetail -> Lude.Maybe Lude.Integer) (\s a -> s {imageSizeInBytes = a} :: ImageDetail)
{-# DEPRECATED idImageSizeInBytes "Use generic-lens or generic-optics with 'imageSizeInBytes' instead." #-}

-- | The @sha256@ digest of the image manifest.
--
-- /Note:/ Consider using 'imageDigest' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idImageDigest :: Lens.Lens' ImageDetail (Lude.Maybe Lude.Text)
idImageDigest = Lens.lens (imageDigest :: ImageDetail -> Lude.Maybe Lude.Text) (\s a -> s {imageDigest = a} :: ImageDetail)
{-# DEPRECATED idImageDigest "Use generic-lens or generic-optics with 'imageDigest' instead." #-}

-- | A summary of the last completed image scan.
--
-- /Note:/ Consider using 'imageScanFindingsSummary' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idImageScanFindingsSummary :: Lens.Lens' ImageDetail (Lude.Maybe ImageScanFindingsSummary)
idImageScanFindingsSummary = Lens.lens (imageScanFindingsSummary :: ImageDetail -> Lude.Maybe ImageScanFindingsSummary) (\s a -> s {imageScanFindingsSummary = a} :: ImageDetail)
{-# DEPRECATED idImageScanFindingsSummary "Use generic-lens or generic-optics with 'imageScanFindingsSummary' instead." #-}

-- | The artifact media type of the image.
--
-- /Note:/ Consider using 'artifactMediaType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idArtifactMediaType :: Lens.Lens' ImageDetail (Lude.Maybe Lude.Text)
idArtifactMediaType = Lens.lens (artifactMediaType :: ImageDetail -> Lude.Maybe Lude.Text) (\s a -> s {artifactMediaType = a} :: ImageDetail)
{-# DEPRECATED idArtifactMediaType "Use generic-lens or generic-optics with 'artifactMediaType' instead." #-}

-- | The date and time, expressed in standard JavaScript date format, at which the current image was pushed to the repository.
--
-- /Note:/ Consider using 'imagePushedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idImagePushedAt :: Lens.Lens' ImageDetail (Lude.Maybe Lude.Timestamp)
idImagePushedAt = Lens.lens (imagePushedAt :: ImageDetail -> Lude.Maybe Lude.Timestamp) (\s a -> s {imagePushedAt = a} :: ImageDetail)
{-# DEPRECATED idImagePushedAt "Use generic-lens or generic-optics with 'imagePushedAt' instead." #-}

-- | The name of the repository to which this image belongs.
--
-- /Note:/ Consider using 'repositoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idRepositoryName :: Lens.Lens' ImageDetail (Lude.Maybe Lude.Text)
idRepositoryName = Lens.lens (repositoryName :: ImageDetail -> Lude.Maybe Lude.Text) (\s a -> s {repositoryName = a} :: ImageDetail)
{-# DEPRECATED idRepositoryName "Use generic-lens or generic-optics with 'repositoryName' instead." #-}

instance Lude.FromJSON ImageDetail where
  parseJSON =
    Lude.withObject
      "ImageDetail"
      ( \x ->
          ImageDetail'
            Lude.<$> (x Lude..:? "registryId")
            Lude.<*> (x Lude..:? "imageTags" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "imageScanStatus")
            Lude.<*> (x Lude..:? "imageManifestMediaType")
            Lude.<*> (x Lude..:? "imageSizeInBytes")
            Lude.<*> (x Lude..:? "imageDigest")
            Lude.<*> (x Lude..:? "imageScanFindingsSummary")
            Lude.<*> (x Lude..:? "artifactMediaType")
            Lude.<*> (x Lude..:? "imagePushedAt")
            Lude.<*> (x Lude..:? "repositoryName")
      )
