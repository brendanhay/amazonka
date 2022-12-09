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
-- Module      : Amazonka.Lambda.Types.LayersListItem
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Lambda.Types.LayersListItem where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Lambda.Types.LayerVersionsListItem
import qualified Amazonka.Prelude as Prelude

-- | Details about an
-- <https://docs.aws.amazon.com/lambda/latest/dg/configuration-layers.html Lambda layer>.
--
-- /See:/ 'newLayersListItem' smart constructor.
data LayersListItem = LayersListItem'
  { -- | The newest version of the layer.
    latestMatchingVersion :: Prelude.Maybe LayerVersionsListItem,
    -- | The Amazon Resource Name (ARN) of the function layer.
    layerArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the layer.
    layerName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LayersListItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'latestMatchingVersion', 'layersListItem_latestMatchingVersion' - The newest version of the layer.
--
-- 'layerArn', 'layersListItem_layerArn' - The Amazon Resource Name (ARN) of the function layer.
--
-- 'layerName', 'layersListItem_layerName' - The name of the layer.
newLayersListItem ::
  LayersListItem
newLayersListItem =
  LayersListItem'
    { latestMatchingVersion =
        Prelude.Nothing,
      layerArn = Prelude.Nothing,
      layerName = Prelude.Nothing
    }

-- | The newest version of the layer.
layersListItem_latestMatchingVersion :: Lens.Lens' LayersListItem (Prelude.Maybe LayerVersionsListItem)
layersListItem_latestMatchingVersion = Lens.lens (\LayersListItem' {latestMatchingVersion} -> latestMatchingVersion) (\s@LayersListItem' {} a -> s {latestMatchingVersion = a} :: LayersListItem)

-- | The Amazon Resource Name (ARN) of the function layer.
layersListItem_layerArn :: Lens.Lens' LayersListItem (Prelude.Maybe Prelude.Text)
layersListItem_layerArn = Lens.lens (\LayersListItem' {layerArn} -> layerArn) (\s@LayersListItem' {} a -> s {layerArn = a} :: LayersListItem)

-- | The name of the layer.
layersListItem_layerName :: Lens.Lens' LayersListItem (Prelude.Maybe Prelude.Text)
layersListItem_layerName = Lens.lens (\LayersListItem' {layerName} -> layerName) (\s@LayersListItem' {} a -> s {layerName = a} :: LayersListItem)

instance Data.FromJSON LayersListItem where
  parseJSON =
    Data.withObject
      "LayersListItem"
      ( \x ->
          LayersListItem'
            Prelude.<$> (x Data..:? "LatestMatchingVersion")
            Prelude.<*> (x Data..:? "LayerArn")
            Prelude.<*> (x Data..:? "LayerName")
      )

instance Prelude.Hashable LayersListItem where
  hashWithSalt _salt LayersListItem' {..} =
    _salt `Prelude.hashWithSalt` latestMatchingVersion
      `Prelude.hashWithSalt` layerArn
      `Prelude.hashWithSalt` layerName

instance Prelude.NFData LayersListItem where
  rnf LayersListItem' {..} =
    Prelude.rnf latestMatchingVersion
      `Prelude.seq` Prelude.rnf layerArn
      `Prelude.seq` Prelude.rnf layerName
