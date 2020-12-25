{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.ContainerImage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.ContainerImage
  ( ContainerImage (..),

    -- * Smart constructor
    mkContainerImage,

    -- * Lenses
    ciCreatedAt,
    ciDigest,
    ciImage,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types.String as Types
import qualified Network.AWS.Prelude as Core

-- | Describes a container image that is registered to an Amazon Lightsail container service.
--
-- /See:/ 'mkContainerImage' smart constructor.
data ContainerImage = ContainerImage'
  { -- | The timestamp when the container image was created.
    createdAt :: Core.Maybe Core.NominalDiffTime,
    -- | The digest of the container image.
    digest :: Core.Maybe Types.String,
    -- | The name of the container image.
    image :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ContainerImage' value with any optional fields omitted.
mkContainerImage ::
  ContainerImage
mkContainerImage =
  ContainerImage'
    { createdAt = Core.Nothing,
      digest = Core.Nothing,
      image = Core.Nothing
    }

-- | The timestamp when the container image was created.
--
-- /Note:/ Consider using 'createdAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciCreatedAt :: Lens.Lens' ContainerImage (Core.Maybe Core.NominalDiffTime)
ciCreatedAt = Lens.field @"createdAt"
{-# DEPRECATED ciCreatedAt "Use generic-lens or generic-optics with 'createdAt' instead." #-}

-- | The digest of the container image.
--
-- /Note:/ Consider using 'digest' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciDigest :: Lens.Lens' ContainerImage (Core.Maybe Types.String)
ciDigest = Lens.field @"digest"
{-# DEPRECATED ciDigest "Use generic-lens or generic-optics with 'digest' instead." #-}

-- | The name of the container image.
--
-- /Note:/ Consider using 'image' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciImage :: Lens.Lens' ContainerImage (Core.Maybe Types.String)
ciImage = Lens.field @"image"
{-# DEPRECATED ciImage "Use generic-lens or generic-optics with 'image' instead." #-}

instance Core.FromJSON ContainerImage where
  parseJSON =
    Core.withObject "ContainerImage" Core.$
      \x ->
        ContainerImage'
          Core.<$> (x Core..:? "createdAt")
          Core.<*> (x Core..:? "digest")
          Core.<*> (x Core..:? "image")
