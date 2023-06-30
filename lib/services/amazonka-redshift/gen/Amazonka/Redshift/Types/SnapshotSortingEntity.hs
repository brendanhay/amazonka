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
-- Module      : Amazonka.Redshift.Types.SnapshotSortingEntity
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Redshift.Types.SnapshotSortingEntity where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Redshift.Internal
import Amazonka.Redshift.Types.SnapshotAttributeToSortBy
import Amazonka.Redshift.Types.SortByOrder

-- | Describes a sorting entity
--
-- /See:/ 'newSnapshotSortingEntity' smart constructor.
data SnapshotSortingEntity = SnapshotSortingEntity'
  { -- | The order for listing the attributes.
    sortOrder :: Prelude.Maybe SortByOrder,
    -- | The category for sorting the snapshots.
    attribute :: SnapshotAttributeToSortBy
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SnapshotSortingEntity' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sortOrder', 'snapshotSortingEntity_sortOrder' - The order for listing the attributes.
--
-- 'attribute', 'snapshotSortingEntity_attribute' - The category for sorting the snapshots.
newSnapshotSortingEntity ::
  -- | 'attribute'
  SnapshotAttributeToSortBy ->
  SnapshotSortingEntity
newSnapshotSortingEntity pAttribute_ =
  SnapshotSortingEntity'
    { sortOrder = Prelude.Nothing,
      attribute = pAttribute_
    }

-- | The order for listing the attributes.
snapshotSortingEntity_sortOrder :: Lens.Lens' SnapshotSortingEntity (Prelude.Maybe SortByOrder)
snapshotSortingEntity_sortOrder = Lens.lens (\SnapshotSortingEntity' {sortOrder} -> sortOrder) (\s@SnapshotSortingEntity' {} a -> s {sortOrder = a} :: SnapshotSortingEntity)

-- | The category for sorting the snapshots.
snapshotSortingEntity_attribute :: Lens.Lens' SnapshotSortingEntity SnapshotAttributeToSortBy
snapshotSortingEntity_attribute = Lens.lens (\SnapshotSortingEntity' {attribute} -> attribute) (\s@SnapshotSortingEntity' {} a -> s {attribute = a} :: SnapshotSortingEntity)

instance Prelude.Hashable SnapshotSortingEntity where
  hashWithSalt _salt SnapshotSortingEntity' {..} =
    _salt
      `Prelude.hashWithSalt` sortOrder
      `Prelude.hashWithSalt` attribute

instance Prelude.NFData SnapshotSortingEntity where
  rnf SnapshotSortingEntity' {..} =
    Prelude.rnf sortOrder
      `Prelude.seq` Prelude.rnf attribute

instance Data.ToQuery SnapshotSortingEntity where
  toQuery SnapshotSortingEntity' {..} =
    Prelude.mconcat
      [ "SortOrder" Data.=: sortOrder,
        "Attribute" Data.=: attribute
      ]
