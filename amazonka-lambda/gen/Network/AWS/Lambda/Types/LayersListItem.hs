{-# LANGUAGE DeriveDataTypeable #-}
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

import Network.AWS.Lambda.Types.LayerVersionsListItem
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Details about an
-- <https://docs.aws.amazon.com/lambda/latest/dg/configuration-layers.html AWS Lambda layer>.
--
-- /See:/ 'newLayersListItem' smart constructor.
data LayersListItem = LayersListItem'
  { -- | The Amazon Resource Name (ARN) of the function layer.
    layerArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the layer.
    layerName :: Prelude.Maybe Prelude.Text,
    -- | The newest version of the layer.
    latestMatchingVersion :: Prelude.Maybe LayerVersionsListItem
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { layerArn = Prelude.Nothing,
      layerName = Prelude.Nothing,
      latestMatchingVersion = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the function layer.
layersListItem_layerArn :: Lens.Lens' LayersListItem (Prelude.Maybe Prelude.Text)
layersListItem_layerArn = Lens.lens (\LayersListItem' {layerArn} -> layerArn) (\s@LayersListItem' {} a -> s {layerArn = a} :: LayersListItem)

-- | The name of the layer.
layersListItem_layerName :: Lens.Lens' LayersListItem (Prelude.Maybe Prelude.Text)
layersListItem_layerName = Lens.lens (\LayersListItem' {layerName} -> layerName) (\s@LayersListItem' {} a -> s {layerName = a} :: LayersListItem)

-- | The newest version of the layer.
layersListItem_latestMatchingVersion :: Lens.Lens' LayersListItem (Prelude.Maybe LayerVersionsListItem)
layersListItem_latestMatchingVersion = Lens.lens (\LayersListItem' {latestMatchingVersion} -> latestMatchingVersion) (\s@LayersListItem' {} a -> s {latestMatchingVersion = a} :: LayersListItem)

instance Prelude.FromJSON LayersListItem where
  parseJSON =
    Prelude.withObject
      "LayersListItem"
      ( \x ->
          LayersListItem'
            Prelude.<$> (x Prelude..:? "LayerArn")
            Prelude.<*> (x Prelude..:? "LayerName")
            Prelude.<*> (x Prelude..:? "LatestMatchingVersion")
      )

instance Prelude.Hashable LayersListItem

instance Prelude.NFData LayersListItem
