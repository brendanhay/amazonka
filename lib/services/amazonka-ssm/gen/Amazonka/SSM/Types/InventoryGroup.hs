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
-- Module      : Amazonka.SSM.Types.InventoryGroup
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSM.Types.InventoryGroup where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SSM.Types.InventoryFilter

-- | A user-defined set of one or more filters on which to aggregate
-- inventory data. Groups return a count of resources that match and don\'t
-- match the specified criteria.
--
-- /See:/ 'newInventoryGroup' smart constructor.
data InventoryGroup = InventoryGroup'
  { -- | The name of the group.
    name :: Prelude.Text,
    -- | Filters define the criteria for the group. The @matchingCount@ field
    -- displays the number of resources that match the criteria. The
    -- @notMatchingCount@ field displays the number of resources that don\'t
    -- match the criteria.
    filters :: Prelude.NonEmpty InventoryFilter
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InventoryGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'inventoryGroup_name' - The name of the group.
--
-- 'filters', 'inventoryGroup_filters' - Filters define the criteria for the group. The @matchingCount@ field
-- displays the number of resources that match the criteria. The
-- @notMatchingCount@ field displays the number of resources that don\'t
-- match the criteria.
newInventoryGroup ::
  -- | 'name'
  Prelude.Text ->
  -- | 'filters'
  Prelude.NonEmpty InventoryFilter ->
  InventoryGroup
newInventoryGroup pName_ pFilters_ =
  InventoryGroup'
    { name = pName_,
      filters = Lens.coerced Lens.# pFilters_
    }

-- | The name of the group.
inventoryGroup_name :: Lens.Lens' InventoryGroup Prelude.Text
inventoryGroup_name = Lens.lens (\InventoryGroup' {name} -> name) (\s@InventoryGroup' {} a -> s {name = a} :: InventoryGroup)

-- | Filters define the criteria for the group. The @matchingCount@ field
-- displays the number of resources that match the criteria. The
-- @notMatchingCount@ field displays the number of resources that don\'t
-- match the criteria.
inventoryGroup_filters :: Lens.Lens' InventoryGroup (Prelude.NonEmpty InventoryFilter)
inventoryGroup_filters = Lens.lens (\InventoryGroup' {filters} -> filters) (\s@InventoryGroup' {} a -> s {filters = a} :: InventoryGroup) Prelude.. Lens.coerced

instance Prelude.Hashable InventoryGroup where
  hashWithSalt _salt InventoryGroup' {..} =
    _salt
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` filters

instance Prelude.NFData InventoryGroup where
  rnf InventoryGroup' {..} =
    Prelude.rnf name `Prelude.seq` Prelude.rnf filters

instance Data.ToJSON InventoryGroup where
  toJSON InventoryGroup' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Name" Data..= name),
            Prelude.Just ("Filters" Data..= filters)
          ]
      )
