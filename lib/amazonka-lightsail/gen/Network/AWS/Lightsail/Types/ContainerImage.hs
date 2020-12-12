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
    ciImage,
    ciCreatedAt,
    ciDigest,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a container image that is registered to an Amazon Lightsail container service.
--
-- /See:/ 'mkContainerImage' smart constructor.
data ContainerImage = ContainerImage'
  { image ::
      Lude.Maybe Lude.Text,
    createdAt :: Lude.Maybe Lude.Timestamp,
    digest :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ContainerImage' with the minimum fields required to make a request.
--
-- * 'createdAt' - The timestamp when the container image was created.
-- * 'digest' - The digest of the container image.
-- * 'image' - The name of the container image.
mkContainerImage ::
  ContainerImage
mkContainerImage =
  ContainerImage'
    { image = Lude.Nothing,
      createdAt = Lude.Nothing,
      digest = Lude.Nothing
    }

-- | The name of the container image.
--
-- /Note:/ Consider using 'image' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciImage :: Lens.Lens' ContainerImage (Lude.Maybe Lude.Text)
ciImage = Lens.lens (image :: ContainerImage -> Lude.Maybe Lude.Text) (\s a -> s {image = a} :: ContainerImage)
{-# DEPRECATED ciImage "Use generic-lens or generic-optics with 'image' instead." #-}

-- | The timestamp when the container image was created.
--
-- /Note:/ Consider using 'createdAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciCreatedAt :: Lens.Lens' ContainerImage (Lude.Maybe Lude.Timestamp)
ciCreatedAt = Lens.lens (createdAt :: ContainerImage -> Lude.Maybe Lude.Timestamp) (\s a -> s {createdAt = a} :: ContainerImage)
{-# DEPRECATED ciCreatedAt "Use generic-lens or generic-optics with 'createdAt' instead." #-}

-- | The digest of the container image.
--
-- /Note:/ Consider using 'digest' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciDigest :: Lens.Lens' ContainerImage (Lude.Maybe Lude.Text)
ciDigest = Lens.lens (digest :: ContainerImage -> Lude.Maybe Lude.Text) (\s a -> s {digest = a} :: ContainerImage)
{-# DEPRECATED ciDigest "Use generic-lens or generic-optics with 'digest' instead." #-}

instance Lude.FromJSON ContainerImage where
  parseJSON =
    Lude.withObject
      "ContainerImage"
      ( \x ->
          ContainerImage'
            Lude.<$> (x Lude..:? "image")
            Lude.<*> (x Lude..:? "createdAt")
            Lude.<*> (x Lude..:? "digest")
      )
