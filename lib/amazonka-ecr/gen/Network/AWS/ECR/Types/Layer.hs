{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECR.Types.Layer
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECR.Types.Layer
  ( Layer (..),

    -- * Smart constructor
    mkLayer,

    -- * Lenses
    lMediaType,
    lLayerDigest,
    lLayerSize,
    lLayerAvailability,
  )
where

import Network.AWS.ECR.Types.LayerAvailability
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | An object representing an Amazon ECR image layer.
--
-- /See:/ 'mkLayer' smart constructor.
data Layer = Layer'
  { -- | The media type of the layer, such as @application/vnd.docker.image.rootfs.diff.tar.gzip@ or @application/vnd.oci.image.layer.v1.tar+gzip@ .
    mediaType :: Lude.Maybe Lude.Text,
    -- | The @sha256@ digest of the image layer.
    layerDigest :: Lude.Maybe Lude.Text,
    -- | The size, in bytes, of the image layer.
    layerSize :: Lude.Maybe Lude.Integer,
    -- | The availability status of the image layer.
    layerAvailability :: Lude.Maybe LayerAvailability
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Layer' with the minimum fields required to make a request.
--
-- * 'mediaType' - The media type of the layer, such as @application/vnd.docker.image.rootfs.diff.tar.gzip@ or @application/vnd.oci.image.layer.v1.tar+gzip@ .
-- * 'layerDigest' - The @sha256@ digest of the image layer.
-- * 'layerSize' - The size, in bytes, of the image layer.
-- * 'layerAvailability' - The availability status of the image layer.
mkLayer ::
  Layer
mkLayer =
  Layer'
    { mediaType = Lude.Nothing,
      layerDigest = Lude.Nothing,
      layerSize = Lude.Nothing,
      layerAvailability = Lude.Nothing
    }

-- | The media type of the layer, such as @application/vnd.docker.image.rootfs.diff.tar.gzip@ or @application/vnd.oci.image.layer.v1.tar+gzip@ .
--
-- /Note:/ Consider using 'mediaType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lMediaType :: Lens.Lens' Layer (Lude.Maybe Lude.Text)
lMediaType = Lens.lens (mediaType :: Layer -> Lude.Maybe Lude.Text) (\s a -> s {mediaType = a} :: Layer)
{-# DEPRECATED lMediaType "Use generic-lens or generic-optics with 'mediaType' instead." #-}

-- | The @sha256@ digest of the image layer.
--
-- /Note:/ Consider using 'layerDigest' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lLayerDigest :: Lens.Lens' Layer (Lude.Maybe Lude.Text)
lLayerDigest = Lens.lens (layerDigest :: Layer -> Lude.Maybe Lude.Text) (\s a -> s {layerDigest = a} :: Layer)
{-# DEPRECATED lLayerDigest "Use generic-lens or generic-optics with 'layerDigest' instead." #-}

-- | The size, in bytes, of the image layer.
--
-- /Note:/ Consider using 'layerSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lLayerSize :: Lens.Lens' Layer (Lude.Maybe Lude.Integer)
lLayerSize = Lens.lens (layerSize :: Layer -> Lude.Maybe Lude.Integer) (\s a -> s {layerSize = a} :: Layer)
{-# DEPRECATED lLayerSize "Use generic-lens or generic-optics with 'layerSize' instead." #-}

-- | The availability status of the image layer.
--
-- /Note:/ Consider using 'layerAvailability' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lLayerAvailability :: Lens.Lens' Layer (Lude.Maybe LayerAvailability)
lLayerAvailability = Lens.lens (layerAvailability :: Layer -> Lude.Maybe LayerAvailability) (\s a -> s {layerAvailability = a} :: Layer)
{-# DEPRECATED lLayerAvailability "Use generic-lens or generic-optics with 'layerAvailability' instead." #-}

instance Lude.FromJSON Layer where
  parseJSON =
    Lude.withObject
      "Layer"
      ( \x ->
          Layer'
            Lude.<$> (x Lude..:? "mediaType")
            Lude.<*> (x Lude..:? "layerDigest")
            Lude.<*> (x Lude..:? "layerSize")
            Lude.<*> (x Lude..:? "layerAvailability")
      )
