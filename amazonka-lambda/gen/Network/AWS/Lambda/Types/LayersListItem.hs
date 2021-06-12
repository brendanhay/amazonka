{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.Types.LayersListItem
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lambda.Types.LayersListItem where

import qualified Network.AWS.Core as Core
import Network.AWS.Lambda.Types.LayerVersionsListItem
import qualified Network.AWS.Lens as Lens

-- | Details about an
-- <https://docs.aws.amazon.com/lambda/latest/dg/configuration-layers.html AWS Lambda layer>.
--
-- /See:/ 'newLayersListItem' smart constructor.
data LayersListItem = LayersListItem'
  { -- | The Amazon Resource Name (ARN) of the function layer.
    layerArn :: Core.Maybe Core.Text,
    -- | The name of the layer.
    layerName :: Core.Maybe Core.Text,
    -- | The newest version of the layer.
    latestMatchingVersion :: Core.Maybe LayerVersionsListItem
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'LayersListItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'layerArn', 'layersListItem_layerArn' - The Amazon Resource Name (ARN) of the function layer.
--
-- 'layerName', 'layersListItem_layerName' - The name of the layer.
--
-- 'latestMatchingVersion', 'layersListItem_latestMatchingVersion' - The newest version of the layer.
newLayersListItem ::
  LayersListItem
newLayersListItem =
  LayersListItem'
    { layerArn = Core.Nothing,
      layerName = Core.Nothing,
      latestMatchingVersion = Core.Nothing
    }

-- | The Amazon Resource Name (ARN) of the function layer.
layersListItem_layerArn :: Lens.Lens' LayersListItem (Core.Maybe Core.Text)
layersListItem_layerArn = Lens.lens (\LayersListItem' {layerArn} -> layerArn) (\s@LayersListItem' {} a -> s {layerArn = a} :: LayersListItem)

-- | The name of the layer.
layersListItem_layerName :: Lens.Lens' LayersListItem (Core.Maybe Core.Text)
layersListItem_layerName = Lens.lens (\LayersListItem' {layerName} -> layerName) (\s@LayersListItem' {} a -> s {layerName = a} :: LayersListItem)

-- | The newest version of the layer.
layersListItem_latestMatchingVersion :: Lens.Lens' LayersListItem (Core.Maybe LayerVersionsListItem)
layersListItem_latestMatchingVersion = Lens.lens (\LayersListItem' {latestMatchingVersion} -> latestMatchingVersion) (\s@LayersListItem' {} a -> s {latestMatchingVersion = a} :: LayersListItem)

instance Core.FromJSON LayersListItem where
  parseJSON =
    Core.withObject
      "LayersListItem"
      ( \x ->
          LayersListItem'
            Core.<$> (x Core..:? "LayerArn")
            Core.<*> (x Core..:? "LayerName")
            Core.<*> (x Core..:? "LatestMatchingVersion")
      )

instance Core.Hashable LayersListItem

instance Core.NFData LayersListItem
