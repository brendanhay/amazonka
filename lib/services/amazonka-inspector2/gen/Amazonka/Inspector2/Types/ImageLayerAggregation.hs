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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
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
  { -- | The order to sort results by.
    sortOrder :: Prelude.Maybe SortOrder,
    -- | The value to sort results by.
    sortBy :: Prelude.Maybe ImageLayerSortBy,
    -- | The repository associated with the container image hosting the layers.
    repositories :: Prelude.Maybe (Prelude.NonEmpty StringFilter),
    -- | The ID of the container image layer.
    resourceIds :: Prelude.Maybe (Prelude.NonEmpty StringFilter),
    -- | The hashes associated with the layers.
    layerHashes :: Prelude.Maybe (Prelude.NonEmpty StringFilter)
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
-- 'sortOrder', 'imageLayerAggregation_sortOrder' - The order to sort results by.
--
-- 'sortBy', 'imageLayerAggregation_sortBy' - The value to sort results by.
--
-- 'repositories', 'imageLayerAggregation_repositories' - The repository associated with the container image hosting the layers.
--
-- 'resourceIds', 'imageLayerAggregation_resourceIds' - The ID of the container image layer.
--
-- 'layerHashes', 'imageLayerAggregation_layerHashes' - The hashes associated with the layers.
newImageLayerAggregation ::
  ImageLayerAggregation
newImageLayerAggregation =
  ImageLayerAggregation'
    { sortOrder = Prelude.Nothing,
      sortBy = Prelude.Nothing,
      repositories = Prelude.Nothing,
      resourceIds = Prelude.Nothing,
      layerHashes = Prelude.Nothing
    }

-- | The order to sort results by.
imageLayerAggregation_sortOrder :: Lens.Lens' ImageLayerAggregation (Prelude.Maybe SortOrder)
imageLayerAggregation_sortOrder = Lens.lens (\ImageLayerAggregation' {sortOrder} -> sortOrder) (\s@ImageLayerAggregation' {} a -> s {sortOrder = a} :: ImageLayerAggregation)

-- | The value to sort results by.
imageLayerAggregation_sortBy :: Lens.Lens' ImageLayerAggregation (Prelude.Maybe ImageLayerSortBy)
imageLayerAggregation_sortBy = Lens.lens (\ImageLayerAggregation' {sortBy} -> sortBy) (\s@ImageLayerAggregation' {} a -> s {sortBy = a} :: ImageLayerAggregation)

-- | The repository associated with the container image hosting the layers.
imageLayerAggregation_repositories :: Lens.Lens' ImageLayerAggregation (Prelude.Maybe (Prelude.NonEmpty StringFilter))
imageLayerAggregation_repositories = Lens.lens (\ImageLayerAggregation' {repositories} -> repositories) (\s@ImageLayerAggregation' {} a -> s {repositories = a} :: ImageLayerAggregation) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the container image layer.
imageLayerAggregation_resourceIds :: Lens.Lens' ImageLayerAggregation (Prelude.Maybe (Prelude.NonEmpty StringFilter))
imageLayerAggregation_resourceIds = Lens.lens (\ImageLayerAggregation' {resourceIds} -> resourceIds) (\s@ImageLayerAggregation' {} a -> s {resourceIds = a} :: ImageLayerAggregation) Prelude.. Lens.mapping Lens.coerced

-- | The hashes associated with the layers.
imageLayerAggregation_layerHashes :: Lens.Lens' ImageLayerAggregation (Prelude.Maybe (Prelude.NonEmpty StringFilter))
imageLayerAggregation_layerHashes = Lens.lens (\ImageLayerAggregation' {layerHashes} -> layerHashes) (\s@ImageLayerAggregation' {} a -> s {layerHashes = a} :: ImageLayerAggregation) Prelude.. Lens.mapping Lens.coerced

instance Prelude.Hashable ImageLayerAggregation where
  hashWithSalt _salt ImageLayerAggregation' {..} =
    _salt `Prelude.hashWithSalt` sortOrder
      `Prelude.hashWithSalt` sortBy
      `Prelude.hashWithSalt` repositories
      `Prelude.hashWithSalt` resourceIds
      `Prelude.hashWithSalt` layerHashes

instance Prelude.NFData ImageLayerAggregation where
  rnf ImageLayerAggregation' {..} =
    Prelude.rnf sortOrder
      `Prelude.seq` Prelude.rnf sortBy
      `Prelude.seq` Prelude.rnf repositories
      `Prelude.seq` Prelude.rnf resourceIds
      `Prelude.seq` Prelude.rnf layerHashes

instance Data.ToJSON ImageLayerAggregation where
  toJSON ImageLayerAggregation' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("sortOrder" Data..=) Prelude.<$> sortOrder,
            ("sortBy" Data..=) Prelude.<$> sortBy,
            ("repositories" Data..=) Prelude.<$> repositories,
            ("resourceIds" Data..=) Prelude.<$> resourceIds,
            ("layerHashes" Data..=) Prelude.<$> layerHashes
          ]
      )
