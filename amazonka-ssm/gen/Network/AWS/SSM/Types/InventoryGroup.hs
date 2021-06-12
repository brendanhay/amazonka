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
-- Module      : Network.AWS.SSM.Types.InventoryGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.InventoryGroup where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.SSM.Types.InventoryFilter

-- | A user-defined set of one or more filters on which to aggregate
-- inventory data. Groups return a count of resources that match and don\'t
-- match the specified criteria.
--
-- /See:/ 'newInventoryGroup' smart constructor.
data InventoryGroup = InventoryGroup'
  { -- | The name of the group.
    name :: Core.Text,
    -- | Filters define the criteria for the group. The @matchingCount@ field
    -- displays the number of resources that match the criteria. The
    -- @notMatchingCount@ field displays the number of resources that don\'t
    -- match the criteria.
    filters :: Core.NonEmpty InventoryFilter
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  -- | 'filters'
  Core.NonEmpty InventoryFilter ->
  InventoryGroup
newInventoryGroup pName_ pFilters_ =
  InventoryGroup'
    { name = pName_,
      filters = Lens._Coerce Lens.# pFilters_
    }

-- | The name of the group.
inventoryGroup_name :: Lens.Lens' InventoryGroup Core.Text
inventoryGroup_name = Lens.lens (\InventoryGroup' {name} -> name) (\s@InventoryGroup' {} a -> s {name = a} :: InventoryGroup)

-- | Filters define the criteria for the group. The @matchingCount@ field
-- displays the number of resources that match the criteria. The
-- @notMatchingCount@ field displays the number of resources that don\'t
-- match the criteria.
inventoryGroup_filters :: Lens.Lens' InventoryGroup (Core.NonEmpty InventoryFilter)
inventoryGroup_filters = Lens.lens (\InventoryGroup' {filters} -> filters) (\s@InventoryGroup' {} a -> s {filters = a} :: InventoryGroup) Core.. Lens._Coerce

instance Core.Hashable InventoryGroup

instance Core.NFData InventoryGroup

instance Core.ToJSON InventoryGroup where
  toJSON InventoryGroup' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Name" Core..= name),
            Core.Just ("Filters" Core..= filters)
          ]
      )
