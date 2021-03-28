{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECR.Types.Layer
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ECR.Types.Layer
  ( Layer (..)
  -- * Smart constructor
  , mkLayer
  -- * Lenses
  , lLayerAvailability
  , lLayerDigest
  , lLayerSize
  , lMediaType
  ) where

import qualified Network.AWS.ECR.Types.LayerAvailability as Types
import qualified Network.AWS.ECR.Types.LayerDigest as Types
import qualified Network.AWS.ECR.Types.MediaType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | An object representing an Amazon ECR image layer.
--
-- /See:/ 'mkLayer' smart constructor.
data Layer = Layer'
  { layerAvailability :: Core.Maybe Types.LayerAvailability
    -- ^ The availability status of the image layer.
  , layerDigest :: Core.Maybe Types.LayerDigest
    -- ^ The @sha256@ digest of the image layer.
  , layerSize :: Core.Maybe Core.Integer
    -- ^ The size, in bytes, of the image layer.
  , mediaType :: Core.Maybe Types.MediaType
    -- ^ The media type of the layer, such as @application/vnd.docker.image.rootfs.diff.tar.gzip@ or @application/vnd.oci.image.layer.v1.tar+gzip@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Layer' value with any optional fields omitted.
mkLayer
    :: Layer
mkLayer
  = Layer'{layerAvailability = Core.Nothing,
           layerDigest = Core.Nothing, layerSize = Core.Nothing,
           mediaType = Core.Nothing}

-- | The availability status of the image layer.
--
-- /Note:/ Consider using 'layerAvailability' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lLayerAvailability :: Lens.Lens' Layer (Core.Maybe Types.LayerAvailability)
lLayerAvailability = Lens.field @"layerAvailability"
{-# INLINEABLE lLayerAvailability #-}
{-# DEPRECATED layerAvailability "Use generic-lens or generic-optics with 'layerAvailability' instead"  #-}

-- | The @sha256@ digest of the image layer.
--
-- /Note:/ Consider using 'layerDigest' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lLayerDigest :: Lens.Lens' Layer (Core.Maybe Types.LayerDigest)
lLayerDigest = Lens.field @"layerDigest"
{-# INLINEABLE lLayerDigest #-}
{-# DEPRECATED layerDigest "Use generic-lens or generic-optics with 'layerDigest' instead"  #-}

-- | The size, in bytes, of the image layer.
--
-- /Note:/ Consider using 'layerSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lLayerSize :: Lens.Lens' Layer (Core.Maybe Core.Integer)
lLayerSize = Lens.field @"layerSize"
{-# INLINEABLE lLayerSize #-}
{-# DEPRECATED layerSize "Use generic-lens or generic-optics with 'layerSize' instead"  #-}

-- | The media type of the layer, such as @application/vnd.docker.image.rootfs.diff.tar.gzip@ or @application/vnd.oci.image.layer.v1.tar+gzip@ .
--
-- /Note:/ Consider using 'mediaType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lMediaType :: Lens.Lens' Layer (Core.Maybe Types.MediaType)
lMediaType = Lens.field @"mediaType"
{-# INLINEABLE lMediaType #-}
{-# DEPRECATED mediaType "Use generic-lens or generic-optics with 'mediaType' instead"  #-}

instance Core.FromJSON Layer where
        parseJSON
          = Core.withObject "Layer" Core.$
              \ x ->
                Layer' Core.<$>
                  (x Core..:? "layerAvailability") Core.<*> x Core..:? "layerDigest"
                    Core.<*> x Core..:? "layerSize"
                    Core.<*> x Core..:? "mediaType"
