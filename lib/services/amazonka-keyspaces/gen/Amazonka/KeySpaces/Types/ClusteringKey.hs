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
-- Module      : Amazonka.KeySpaces.Types.ClusteringKey
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KeySpaces.Types.ClusteringKey where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.KeySpaces.Types.SortOrder
import qualified Amazonka.Prelude as Prelude

-- | The optional clustering column portion of your primary key determines
-- how the data is clustered and sorted within each partition.
--
-- /See:/ 'newClusteringKey' smart constructor.
data ClusteringKey = ClusteringKey'
  { -- | The name(s) of the clustering column(s).
    name :: Prelude.Text,
    -- | Sets the ascendant (@ASC@) or descendant (@DESC@) order modifier.
    orderBy :: SortOrder
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ClusteringKey' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'clusteringKey_name' - The name(s) of the clustering column(s).
--
-- 'orderBy', 'clusteringKey_orderBy' - Sets the ascendant (@ASC@) or descendant (@DESC@) order modifier.
newClusteringKey ::
  -- | 'name'
  Prelude.Text ->
  -- | 'orderBy'
  SortOrder ->
  ClusteringKey
newClusteringKey pName_ pOrderBy_ =
  ClusteringKey' {name = pName_, orderBy = pOrderBy_}

-- | The name(s) of the clustering column(s).
clusteringKey_name :: Lens.Lens' ClusteringKey Prelude.Text
clusteringKey_name = Lens.lens (\ClusteringKey' {name} -> name) (\s@ClusteringKey' {} a -> s {name = a} :: ClusteringKey)

-- | Sets the ascendant (@ASC@) or descendant (@DESC@) order modifier.
clusteringKey_orderBy :: Lens.Lens' ClusteringKey SortOrder
clusteringKey_orderBy = Lens.lens (\ClusteringKey' {orderBy} -> orderBy) (\s@ClusteringKey' {} a -> s {orderBy = a} :: ClusteringKey)

instance Core.FromJSON ClusteringKey where
  parseJSON =
    Core.withObject
      "ClusteringKey"
      ( \x ->
          ClusteringKey'
            Prelude.<$> (x Core..: "name") Prelude.<*> (x Core..: "orderBy")
      )

instance Prelude.Hashable ClusteringKey where
  hashWithSalt _salt ClusteringKey' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` orderBy

instance Prelude.NFData ClusteringKey where
  rnf ClusteringKey' {..} =
    Prelude.rnf name `Prelude.seq` Prelude.rnf orderBy

instance Core.ToJSON ClusteringKey where
  toJSON ClusteringKey' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("name" Core..= name),
            Prelude.Just ("orderBy" Core..= orderBy)
          ]
      )
