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
-- Module      : Amazonka.Inspector2.Types.ImageLayerAggregation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Inspector2.Types.ImageLayerAggregation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Inspector2.Types.ImageLayerSortBy
import Amazonka.Inspector2.Types.SortOrder
import Amazonka.Inspector2.Types.StringFilter
import qualified Amazonka.Prelude as Prelude

-- | The details that define an aggregation based on container image layers.
--
-- /See:/ 'newImageLayerAggregation' smart constructor.
data ImageLayerAggregation = ImageLayerAggregation'
  { -- | The hashes associated with the layers.
    layerHashes :: Prelude.Maybe (Prelude.NonEmpty StringFilter),
    -- | The repository associated with the container image hosting the layers.
    repositories :: Prelude.Maybe (Prelude.NonEmpty StringFilter),
    -- | The ID of the container image layer.
    resourceIds :: Prelude.Maybe (Prelude.NonEmpty StringFilter),
    -- | The value to sort results by.
    sortBy :: Prelude.Maybe ImageLayerSortBy,
    -- | The order to sort results by.
    sortOrder :: Prelude.Maybe SortOrder
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ImageLayerAggregation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'layerHashes', 'imageLayerAggregation_layerHashes' - The hashes associated with the layers.
--
-- 'repositories', 'imageLayerAggregation_repositories' - The repository associated with the container image hosting the layers.
--
-- 'resourceIds', 'imageLayerAggregation_resourceIds' - The ID of the container image layer.
--
-- 'sortBy', 'imageLayerAggregation_sortBy' - The value to sort results by.
--
-- 'sortOrder', 'imageLayerAggregation_sortOrder' - The order to sort results by.
newImageLayerAggregation ::
  ImageLayerAggregation
newImageLayerAggregation =
  ImageLayerAggregation'
    { layerHashes =
        Prelude.Nothing,
      repositories = Prelude.Nothing,
      resourceIds = Prelude.Nothing,
      sortBy = Prelude.Nothing,
      sortOrder = Prelude.Nothing
    }

-- | The hashes associated with the layers.
imageLayerAggregation_layerHashes :: Lens.Lens' ImageLayerAggregation (Prelude.Maybe (Prelude.NonEmpty StringFilter))
imageLayerAggregation_layerHashes = Lens.lens (\ImageLayerAggregation' {layerHashes} -> layerHashes) (\s@ImageLayerAggregation' {} a -> s {layerHashes = a} :: ImageLayerAggregation) Prelude.. Lens.mapping Lens.coerced

-- | The repository associated with the container image hosting the layers.
imageLayerAggregation_repositories :: Lens.Lens' ImageLayerAggregation (Prelude.Maybe (Prelude.NonEmpty StringFilter))
imageLayerAggregation_repositories = Lens.lens (\ImageLayerAggregation' {repositories} -> repositories) (\s@ImageLayerAggregation' {} a -> s {repositories = a} :: ImageLayerAggregation) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the container image layer.
imageLayerAggregation_resourceIds :: Lens.Lens' ImageLayerAggregation (Prelude.Maybe (Prelude.NonEmpty StringFilter))
imageLayerAggregation_resourceIds = Lens.lens (\ImageLayerAggregation' {resourceIds} -> resourceIds) (\s@ImageLayerAggregation' {} a -> s {resourceIds = a} :: ImageLayerAggregation) Prelude.. Lens.mapping Lens.coerced

-- | The value to sort results by.
imageLayerAggregation_sortBy :: Lens.Lens' ImageLayerAggregation (Prelude.Maybe ImageLayerSortBy)
imageLayerAggregation_sortBy = Lens.lens (\ImageLayerAggregation' {sortBy} -> sortBy) (\s@ImageLayerAggregation' {} a -> s {sortBy = a} :: ImageLayerAggregation)

-- | The order to sort results by.
imageLayerAggregation_sortOrder :: Lens.Lens' ImageLayerAggregation (Prelude.Maybe SortOrder)
imageLayerAggregation_sortOrder = Lens.lens (\ImageLayerAggregation' {sortOrder} -> sortOrder) (\s@ImageLayerAggregation' {} a -> s {sortOrder = a} :: ImageLayerAggregation)

instance Prelude.Hashable ImageLayerAggregation where
  hashWithSalt _salt ImageLayerAggregation' {..} =
    _salt
      `Prelude.hashWithSalt` layerHashes
      `Prelude.hashWithSalt` repositories
      `Prelude.hashWithSalt` resourceIds
      `Prelude.hashWithSalt` sortBy
      `Prelude.hashWithSalt` sortOrder

instance Prelude.NFData ImageLayerAggregation where
  rnf ImageLayerAggregation' {..} =
    Prelude.rnf layerHashes
      `Prelude.seq` Prelude.rnf repositories
      `Prelude.seq` Prelude.rnf resourceIds
      `Prelude.seq` Prelude.rnf sortBy
      `Prelude.seq` Prelude.rnf sortOrder

instance Data.ToJSON ImageLayerAggregation where
  toJSON ImageLayerAggregation' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("layerHashes" Data..=) Prelude.<$> layerHashes,
            ("repositories" Data..=) Prelude.<$> repositories,
            ("resourceIds" Data..=) Prelude.<$> resourceIds,
            ("sortBy" Data..=) Prelude.<$> sortBy,
            ("sortOrder" Data..=) Prelude.<$> sortOrder
          ]
      )
