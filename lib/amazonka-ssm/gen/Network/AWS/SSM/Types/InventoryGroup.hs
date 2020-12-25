{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.InventoryGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.InventoryGroup
  ( InventoryGroup (..),

    -- * Smart constructor
    mkInventoryGroup,

    -- * Lenses
    igName,
    igFilters,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SSM.Types.InventoryFilter as Types
import qualified Network.AWS.SSM.Types.InventoryGroupName as Types

-- | A user-defined set of one or more filters on which to aggregate inventory data. Groups return a count of resources that match and don't match the specified criteria.
--
-- /See:/ 'mkInventoryGroup' smart constructor.
data InventoryGroup = InventoryGroup'
  { -- | The name of the group.
    name :: Types.InventoryGroupName,
    -- | Filters define the criteria for the group. The @matchingCount@ field displays the number of resources that match the criteria. The @notMatchingCount@ field displays the number of resources that don't match the criteria.
    filters :: Core.NonEmpty Types.InventoryFilter
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'InventoryGroup' value with any optional fields omitted.
mkInventoryGroup ::
  -- | 'name'
  Types.InventoryGroupName ->
  -- | 'filters'
  Core.NonEmpty Types.InventoryFilter ->
  InventoryGroup
mkInventoryGroup name filters = InventoryGroup' {name, filters}

-- | The name of the group.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
igName :: Lens.Lens' InventoryGroup Types.InventoryGroupName
igName = Lens.field @"name"
{-# DEPRECATED igName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | Filters define the criteria for the group. The @matchingCount@ field displays the number of resources that match the criteria. The @notMatchingCount@ field displays the number of resources that don't match the criteria.
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
igFilters :: Lens.Lens' InventoryGroup (Core.NonEmpty Types.InventoryFilter)
igFilters = Lens.field @"filters"
{-# DEPRECATED igFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

instance Core.FromJSON InventoryGroup where
  toJSON InventoryGroup {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Name" Core..= name),
            Core.Just ("Filters" Core..= filters)
          ]
      )
