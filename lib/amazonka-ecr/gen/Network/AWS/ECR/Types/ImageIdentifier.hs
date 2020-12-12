{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECR.Types.ImageIdentifier
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECR.Types.ImageIdentifier
  ( ImageIdentifier (..),

    -- * Smart constructor
    mkImageIdentifier,

    -- * Lenses
    iiImageDigest,
    iiImageTag,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | An object with identifying information for an Amazon ECR image.
--
-- /See:/ 'mkImageIdentifier' smart constructor.
data ImageIdentifier = ImageIdentifier'
  { imageDigest ::
      Lude.Maybe Lude.Text,
    imageTag :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ImageIdentifier' with the minimum fields required to make a request.
--
-- * 'imageDigest' - The @sha256@ digest of the image manifest.
-- * 'imageTag' - The tag used for the image.
mkImageIdentifier ::
  ImageIdentifier
mkImageIdentifier =
  ImageIdentifier'
    { imageDigest = Lude.Nothing,
      imageTag = Lude.Nothing
    }

-- | The @sha256@ digest of the image manifest.
--
-- /Note:/ Consider using 'imageDigest' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iiImageDigest :: Lens.Lens' ImageIdentifier (Lude.Maybe Lude.Text)
iiImageDigest = Lens.lens (imageDigest :: ImageIdentifier -> Lude.Maybe Lude.Text) (\s a -> s {imageDigest = a} :: ImageIdentifier)
{-# DEPRECATED iiImageDigest "Use generic-lens or generic-optics with 'imageDigest' instead." #-}

-- | The tag used for the image.
--
-- /Note:/ Consider using 'imageTag' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iiImageTag :: Lens.Lens' ImageIdentifier (Lude.Maybe Lude.Text)
iiImageTag = Lens.lens (imageTag :: ImageIdentifier -> Lude.Maybe Lude.Text) (\s a -> s {imageTag = a} :: ImageIdentifier)
{-# DEPRECATED iiImageTag "Use generic-lens or generic-optics with 'imageTag' instead." #-}

instance Lude.FromJSON ImageIdentifier where
  parseJSON =
    Lude.withObject
      "ImageIdentifier"
      ( \x ->
          ImageIdentifier'
            Lude.<$> (x Lude..:? "imageDigest") Lude.<*> (x Lude..:? "imageTag")
      )

instance Lude.ToJSON ImageIdentifier where
  toJSON ImageIdentifier' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("imageDigest" Lude..=) Lude.<$> imageDigest,
            ("imageTag" Lude..=) Lude.<$> imageTag
          ]
      )
