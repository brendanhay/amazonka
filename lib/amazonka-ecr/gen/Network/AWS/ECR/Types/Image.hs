{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECR.Types.Image
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECR.Types.Image
  ( Image (..),

    -- * Smart constructor
    mkImage,

    -- * Lenses
    iImageId,
    iImageManifest,
    iImageManifestMediaType,
    iRegistryId,
    iRepositoryName,
  )
where

import qualified Network.AWS.ECR.Types.ImageIdentifier as Types
import qualified Network.AWS.ECR.Types.ImageManifest as Types
import qualified Network.AWS.ECR.Types.MediaType as Types
import qualified Network.AWS.ECR.Types.RegistryId as Types
import qualified Network.AWS.ECR.Types.RepositoryName as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | An object representing an Amazon ECR image.
--
-- /See:/ 'mkImage' smart constructor.
data Image = Image'
  { -- | An object containing the image tag and image digest associated with an image.
    imageId :: Core.Maybe Types.ImageIdentifier,
    -- | The image manifest associated with the image.
    imageManifest :: Core.Maybe Types.ImageManifest,
    -- | The manifest media type of the image.
    imageManifestMediaType :: Core.Maybe Types.MediaType,
    -- | The AWS account ID associated with the registry containing the image.
    registryId :: Core.Maybe Types.RegistryId,
    -- | The name of the repository associated with the image.
    repositoryName :: Core.Maybe Types.RepositoryName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Image' value with any optional fields omitted.
mkImage ::
  Image
mkImage =
  Image'
    { imageId = Core.Nothing,
      imageManifest = Core.Nothing,
      imageManifestMediaType = Core.Nothing,
      registryId = Core.Nothing,
      repositoryName = Core.Nothing
    }

-- | An object containing the image tag and image digest associated with an image.
--
-- /Note:/ Consider using 'imageId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iImageId :: Lens.Lens' Image (Core.Maybe Types.ImageIdentifier)
iImageId = Lens.field @"imageId"
{-# DEPRECATED iImageId "Use generic-lens or generic-optics with 'imageId' instead." #-}

-- | The image manifest associated with the image.
--
-- /Note:/ Consider using 'imageManifest' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iImageManifest :: Lens.Lens' Image (Core.Maybe Types.ImageManifest)
iImageManifest = Lens.field @"imageManifest"
{-# DEPRECATED iImageManifest "Use generic-lens or generic-optics with 'imageManifest' instead." #-}

-- | The manifest media type of the image.
--
-- /Note:/ Consider using 'imageManifestMediaType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iImageManifestMediaType :: Lens.Lens' Image (Core.Maybe Types.MediaType)
iImageManifestMediaType = Lens.field @"imageManifestMediaType"
{-# DEPRECATED iImageManifestMediaType "Use generic-lens or generic-optics with 'imageManifestMediaType' instead." #-}

-- | The AWS account ID associated with the registry containing the image.
--
-- /Note:/ Consider using 'registryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iRegistryId :: Lens.Lens' Image (Core.Maybe Types.RegistryId)
iRegistryId = Lens.field @"registryId"
{-# DEPRECATED iRegistryId "Use generic-lens or generic-optics with 'registryId' instead." #-}

-- | The name of the repository associated with the image.
--
-- /Note:/ Consider using 'repositoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iRepositoryName :: Lens.Lens' Image (Core.Maybe Types.RepositoryName)
iRepositoryName = Lens.field @"repositoryName"
{-# DEPRECATED iRepositoryName "Use generic-lens or generic-optics with 'repositoryName' instead." #-}

instance Core.FromJSON Image where
  parseJSON =
    Core.withObject "Image" Core.$
      \x ->
        Image'
          Core.<$> (x Core..:? "imageId")
          Core.<*> (x Core..:? "imageManifest")
          Core.<*> (x Core..:? "imageManifestMediaType")
          Core.<*> (x Core..:? "registryId")
          Core.<*> (x Core..:? "repositoryName")
