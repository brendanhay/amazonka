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
    iRegistryId,
    iImageManifestMediaType,
    iImageId,
    iRepositoryName,
    iImageManifest,
  )
where

import Network.AWS.ECR.Types.ImageIdentifier
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | An object representing an Amazon ECR image.
--
-- /See:/ 'mkImage' smart constructor.
data Image = Image'
  { -- | The AWS account ID associated with the registry containing the image.
    registryId :: Lude.Maybe Lude.Text,
    -- | The manifest media type of the image.
    imageManifestMediaType :: Lude.Maybe Lude.Text,
    -- | An object containing the image tag and image digest associated with an image.
    imageId :: Lude.Maybe ImageIdentifier,
    -- | The name of the repository associated with the image.
    repositoryName :: Lude.Maybe Lude.Text,
    -- | The image manifest associated with the image.
    imageManifest :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Image' with the minimum fields required to make a request.
--
-- * 'registryId' - The AWS account ID associated with the registry containing the image.
-- * 'imageManifestMediaType' - The manifest media type of the image.
-- * 'imageId' - An object containing the image tag and image digest associated with an image.
-- * 'repositoryName' - The name of the repository associated with the image.
-- * 'imageManifest' - The image manifest associated with the image.
mkImage ::
  Image
mkImage =
  Image'
    { registryId = Lude.Nothing,
      imageManifestMediaType = Lude.Nothing,
      imageId = Lude.Nothing,
      repositoryName = Lude.Nothing,
      imageManifest = Lude.Nothing
    }

-- | The AWS account ID associated with the registry containing the image.
--
-- /Note:/ Consider using 'registryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iRegistryId :: Lens.Lens' Image (Lude.Maybe Lude.Text)
iRegistryId = Lens.lens (registryId :: Image -> Lude.Maybe Lude.Text) (\s a -> s {registryId = a} :: Image)
{-# DEPRECATED iRegistryId "Use generic-lens or generic-optics with 'registryId' instead." #-}

-- | The manifest media type of the image.
--
-- /Note:/ Consider using 'imageManifestMediaType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iImageManifestMediaType :: Lens.Lens' Image (Lude.Maybe Lude.Text)
iImageManifestMediaType = Lens.lens (imageManifestMediaType :: Image -> Lude.Maybe Lude.Text) (\s a -> s {imageManifestMediaType = a} :: Image)
{-# DEPRECATED iImageManifestMediaType "Use generic-lens or generic-optics with 'imageManifestMediaType' instead." #-}

-- | An object containing the image tag and image digest associated with an image.
--
-- /Note:/ Consider using 'imageId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iImageId :: Lens.Lens' Image (Lude.Maybe ImageIdentifier)
iImageId = Lens.lens (imageId :: Image -> Lude.Maybe ImageIdentifier) (\s a -> s {imageId = a} :: Image)
{-# DEPRECATED iImageId "Use generic-lens or generic-optics with 'imageId' instead." #-}

-- | The name of the repository associated with the image.
--
-- /Note:/ Consider using 'repositoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iRepositoryName :: Lens.Lens' Image (Lude.Maybe Lude.Text)
iRepositoryName = Lens.lens (repositoryName :: Image -> Lude.Maybe Lude.Text) (\s a -> s {repositoryName = a} :: Image)
{-# DEPRECATED iRepositoryName "Use generic-lens or generic-optics with 'repositoryName' instead." #-}

-- | The image manifest associated with the image.
--
-- /Note:/ Consider using 'imageManifest' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iImageManifest :: Lens.Lens' Image (Lude.Maybe Lude.Text)
iImageManifest = Lens.lens (imageManifest :: Image -> Lude.Maybe Lude.Text) (\s a -> s {imageManifest = a} :: Image)
{-# DEPRECATED iImageManifest "Use generic-lens or generic-optics with 'imageManifest' instead." #-}

instance Lude.FromJSON Image where
  parseJSON =
    Lude.withObject
      "Image"
      ( \x ->
          Image'
            Lude.<$> (x Lude..:? "registryId")
            Lude.<*> (x Lude..:? "imageManifestMediaType")
            Lude.<*> (x Lude..:? "imageId")
            Lude.<*> (x Lude..:? "repositoryName")
            Lude.<*> (x Lude..:? "imageManifest")
      )
