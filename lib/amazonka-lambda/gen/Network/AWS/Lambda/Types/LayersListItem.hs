{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.Types.LayersListItem
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lambda.Types.LayersListItem
  ( LayersListItem (..),

    -- * Smart constructor
    mkLayersListItem,

    -- * Lenses
    lliLatestMatchingVersion,
    lliLayerArn,
    lliLayerName,
  )
where

import qualified Network.AWS.Lambda.Types.LayerArn as Types
import qualified Network.AWS.Lambda.Types.LayerName as Types
import qualified Network.AWS.Lambda.Types.LayerVersionsListItem as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Details about an <https://docs.aws.amazon.com/lambda/latest/dg/configuration-layers.html AWS Lambda layer> .
--
-- /See:/ 'mkLayersListItem' smart constructor.
data LayersListItem = LayersListItem'
  { -- | The newest version of the layer.
    latestMatchingVersion :: Core.Maybe Types.LayerVersionsListItem,
    -- | The Amazon Resource Name (ARN) of the function layer.
    layerArn :: Core.Maybe Types.LayerArn,
    -- | The name of the layer.
    layerName :: Core.Maybe Types.LayerName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'LayersListItem' value with any optional fields omitted.
mkLayersListItem ::
  LayersListItem
mkLayersListItem =
  LayersListItem'
    { latestMatchingVersion = Core.Nothing,
      layerArn = Core.Nothing,
      layerName = Core.Nothing
    }

-- | The newest version of the layer.
--
-- /Note:/ Consider using 'latestMatchingVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lliLatestMatchingVersion :: Lens.Lens' LayersListItem (Core.Maybe Types.LayerVersionsListItem)
lliLatestMatchingVersion = Lens.field @"latestMatchingVersion"
{-# DEPRECATED lliLatestMatchingVersion "Use generic-lens or generic-optics with 'latestMatchingVersion' instead." #-}

-- | The Amazon Resource Name (ARN) of the function layer.
--
-- /Note:/ Consider using 'layerArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lliLayerArn :: Lens.Lens' LayersListItem (Core.Maybe Types.LayerArn)
lliLayerArn = Lens.field @"layerArn"
{-# DEPRECATED lliLayerArn "Use generic-lens or generic-optics with 'layerArn' instead." #-}

-- | The name of the layer.
--
-- /Note:/ Consider using 'layerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lliLayerName :: Lens.Lens' LayersListItem (Core.Maybe Types.LayerName)
lliLayerName = Lens.field @"layerName"
{-# DEPRECATED lliLayerName "Use generic-lens or generic-optics with 'layerName' instead." #-}

instance Core.FromJSON LayersListItem where
  parseJSON =
    Core.withObject "LayersListItem" Core.$
      \x ->
        LayersListItem'
          Core.<$> (x Core..:? "LatestMatchingVersion")
          Core.<*> (x Core..:? "LayerArn")
          Core.<*> (x Core..:? "LayerName")
