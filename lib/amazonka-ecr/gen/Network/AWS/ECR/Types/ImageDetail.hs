{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
    idArtifactMediaType,
    idImageDigest,
    idImageManifestMediaType,
    idImagePushedAt,
    idImageScanFindingsSummary,
    idImageScanStatus,
    idImageSizeInBytes,
    idImageTags,
    idRegistryId,
    idRepositoryName,
  )
where

import qualified Network.AWS.ECR.Types.ImageDigest as Types
import qualified Network.AWS.ECR.Types.ImageScanFindingsSummary as Types
import qualified Network.AWS.ECR.Types.ImageScanStatus as Types
import qualified Network.AWS.ECR.Types.ImageTag as Types
import qualified Network.AWS.ECR.Types.MediaType as Types
import qualified Network.AWS.ECR.Types.RegistryId as Types
import qualified Network.AWS.ECR.Types.RepositoryName as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | An object that describes an image returned by a 'DescribeImages' operation.
--
-- /See:/ 'mkImageDetail' smart constructor.
data ImageDetail = ImageDetail'
  { -- | The artifact media type of the image.
    artifactMediaType :: Core.Maybe Types.MediaType,
    -- | The @sha256@ digest of the image manifest.
    imageDigest :: Core.Maybe Types.ImageDigest,
    -- | The media type of the image manifest.
    imageManifestMediaType :: Core.Maybe Types.MediaType,
    -- | The date and time, expressed in standard JavaScript date format, at which the current image was pushed to the repository.
    imagePushedAt :: Core.Maybe Core.NominalDiffTime,
    -- | A summary of the last completed image scan.
    imageScanFindingsSummary :: Core.Maybe Types.ImageScanFindingsSummary,
    -- | The current state of the scan.
    imageScanStatus :: Core.Maybe Types.ImageScanStatus,
    -- | The size, in bytes, of the image in the repository.
    --
    -- If the image is a manifest list, this will be the max size of all manifests in the list.
    imageSizeInBytes :: Core.Maybe Core.Integer,
    -- | The list of tags associated with this image.
    imageTags :: Core.Maybe [Types.ImageTag],
    -- | The AWS account ID associated with the registry to which this image belongs.
    registryId :: Core.Maybe Types.RegistryId,
    -- | The name of the repository to which this image belongs.
    repositoryName :: Core.Maybe Types.RepositoryName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ImageDetail' value with any optional fields omitted.
mkImageDetail ::
  ImageDetail
mkImageDetail =
  ImageDetail'
    { artifactMediaType = Core.Nothing,
      imageDigest = Core.Nothing,
      imageManifestMediaType = Core.Nothing,
      imagePushedAt = Core.Nothing,
      imageScanFindingsSummary = Core.Nothing,
      imageScanStatus = Core.Nothing,
      imageSizeInBytes = Core.Nothing,
      imageTags = Core.Nothing,
      registryId = Core.Nothing,
      repositoryName = Core.Nothing
    }

-- | The artifact media type of the image.
--
-- /Note:/ Consider using 'artifactMediaType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idArtifactMediaType :: Lens.Lens' ImageDetail (Core.Maybe Types.MediaType)
idArtifactMediaType = Lens.field @"artifactMediaType"
{-# DEPRECATED idArtifactMediaType "Use generic-lens or generic-optics with 'artifactMediaType' instead." #-}

-- | The @sha256@ digest of the image manifest.
--
-- /Note:/ Consider using 'imageDigest' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idImageDigest :: Lens.Lens' ImageDetail (Core.Maybe Types.ImageDigest)
idImageDigest = Lens.field @"imageDigest"
{-# DEPRECATED idImageDigest "Use generic-lens or generic-optics with 'imageDigest' instead." #-}

-- | The media type of the image manifest.
--
-- /Note:/ Consider using 'imageManifestMediaType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idImageManifestMediaType :: Lens.Lens' ImageDetail (Core.Maybe Types.MediaType)
idImageManifestMediaType = Lens.field @"imageManifestMediaType"
{-# DEPRECATED idImageManifestMediaType "Use generic-lens or generic-optics with 'imageManifestMediaType' instead." #-}

-- | The date and time, expressed in standard JavaScript date format, at which the current image was pushed to the repository.
--
-- /Note:/ Consider using 'imagePushedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idImagePushedAt :: Lens.Lens' ImageDetail (Core.Maybe Core.NominalDiffTime)
idImagePushedAt = Lens.field @"imagePushedAt"
{-# DEPRECATED idImagePushedAt "Use generic-lens or generic-optics with 'imagePushedAt' instead." #-}

-- | A summary of the last completed image scan.
--
-- /Note:/ Consider using 'imageScanFindingsSummary' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idImageScanFindingsSummary :: Lens.Lens' ImageDetail (Core.Maybe Types.ImageScanFindingsSummary)
idImageScanFindingsSummary = Lens.field @"imageScanFindingsSummary"
{-# DEPRECATED idImageScanFindingsSummary "Use generic-lens or generic-optics with 'imageScanFindingsSummary' instead." #-}

-- | The current state of the scan.
--
-- /Note:/ Consider using 'imageScanStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idImageScanStatus :: Lens.Lens' ImageDetail (Core.Maybe Types.ImageScanStatus)
idImageScanStatus = Lens.field @"imageScanStatus"
{-# DEPRECATED idImageScanStatus "Use generic-lens or generic-optics with 'imageScanStatus' instead." #-}

-- | The size, in bytes, of the image in the repository.
--
-- If the image is a manifest list, this will be the max size of all manifests in the list.
--
-- /Note:/ Consider using 'imageSizeInBytes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idImageSizeInBytes :: Lens.Lens' ImageDetail (Core.Maybe Core.Integer)
idImageSizeInBytes = Lens.field @"imageSizeInBytes"
{-# DEPRECATED idImageSizeInBytes "Use generic-lens or generic-optics with 'imageSizeInBytes' instead." #-}

-- | The list of tags associated with this image.
--
-- /Note:/ Consider using 'imageTags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idImageTags :: Lens.Lens' ImageDetail (Core.Maybe [Types.ImageTag])
idImageTags = Lens.field @"imageTags"
{-# DEPRECATED idImageTags "Use generic-lens or generic-optics with 'imageTags' instead." #-}

-- | The AWS account ID associated with the registry to which this image belongs.
--
-- /Note:/ Consider using 'registryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idRegistryId :: Lens.Lens' ImageDetail (Core.Maybe Types.RegistryId)
idRegistryId = Lens.field @"registryId"
{-# DEPRECATED idRegistryId "Use generic-lens or generic-optics with 'registryId' instead." #-}

-- | The name of the repository to which this image belongs.
--
-- /Note:/ Consider using 'repositoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idRepositoryName :: Lens.Lens' ImageDetail (Core.Maybe Types.RepositoryName)
idRepositoryName = Lens.field @"repositoryName"
{-# DEPRECATED idRepositoryName "Use generic-lens or generic-optics with 'repositoryName' instead." #-}

instance Core.FromJSON ImageDetail where
  parseJSON =
    Core.withObject "ImageDetail" Core.$
      \x ->
        ImageDetail'
          Core.<$> (x Core..:? "artifactMediaType")
          Core.<*> (x Core..:? "imageDigest")
          Core.<*> (x Core..:? "imageManifestMediaType")
          Core.<*> (x Core..:? "imagePushedAt")
          Core.<*> (x Core..:? "imageScanFindingsSummary")
          Core.<*> (x Core..:? "imageScanStatus")
          Core.<*> (x Core..:? "imageSizeInBytes")
          Core.<*> (x Core..:? "imageTags")
          Core.<*> (x Core..:? "registryId")
          Core.<*> (x Core..:? "repositoryName")
