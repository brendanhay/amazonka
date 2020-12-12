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
    lliLayerName,
    lliLatestMatchingVersion,
    lliLayerARN,
  )
where

import Network.AWS.Lambda.Types.LayerVersionsListItem
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Details about an <https://docs.aws.amazon.com/lambda/latest/dg/configuration-layers.html AWS Lambda layer> .
--
-- /See:/ 'mkLayersListItem' smart constructor.
data LayersListItem = LayersListItem'
  { layerName ::
      Lude.Maybe Lude.Text,
    latestMatchingVersion :: Lude.Maybe LayerVersionsListItem,
    layerARN :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'LayersListItem' with the minimum fields required to make a request.
--
-- * 'latestMatchingVersion' - The newest version of the layer.
-- * 'layerARN' - The Amazon Resource Name (ARN) of the function layer.
-- * 'layerName' - The name of the layer.
mkLayersListItem ::
  LayersListItem
mkLayersListItem =
  LayersListItem'
    { layerName = Lude.Nothing,
      latestMatchingVersion = Lude.Nothing,
      layerARN = Lude.Nothing
    }

-- | The name of the layer.
--
-- /Note:/ Consider using 'layerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lliLayerName :: Lens.Lens' LayersListItem (Lude.Maybe Lude.Text)
lliLayerName = Lens.lens (layerName :: LayersListItem -> Lude.Maybe Lude.Text) (\s a -> s {layerName = a} :: LayersListItem)
{-# DEPRECATED lliLayerName "Use generic-lens or generic-optics with 'layerName' instead." #-}

-- | The newest version of the layer.
--
-- /Note:/ Consider using 'latestMatchingVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lliLatestMatchingVersion :: Lens.Lens' LayersListItem (Lude.Maybe LayerVersionsListItem)
lliLatestMatchingVersion = Lens.lens (latestMatchingVersion :: LayersListItem -> Lude.Maybe LayerVersionsListItem) (\s a -> s {latestMatchingVersion = a} :: LayersListItem)
{-# DEPRECATED lliLatestMatchingVersion "Use generic-lens or generic-optics with 'latestMatchingVersion' instead." #-}

-- | The Amazon Resource Name (ARN) of the function layer.
--
-- /Note:/ Consider using 'layerARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lliLayerARN :: Lens.Lens' LayersListItem (Lude.Maybe Lude.Text)
lliLayerARN = Lens.lens (layerARN :: LayersListItem -> Lude.Maybe Lude.Text) (\s a -> s {layerARN = a} :: LayersListItem)
{-# DEPRECATED lliLayerARN "Use generic-lens or generic-optics with 'layerARN' instead." #-}

instance Lude.FromJSON LayersListItem where
  parseJSON =
    Lude.withObject
      "LayersListItem"
      ( \x ->
          LayersListItem'
            Lude.<$> (x Lude..:? "LayerName")
            Lude.<*> (x Lude..:? "LatestMatchingVersion")
            Lude.<*> (x Lude..:? "LayerArn")
      )
