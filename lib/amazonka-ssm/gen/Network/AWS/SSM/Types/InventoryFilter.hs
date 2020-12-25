{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.InventoryFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.InventoryFilter
  ( InventoryFilter (..),

    -- * Smart constructor
    mkInventoryFilter,

    -- * Lenses
    ifKey,
    ifValues,
    ifType,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SSM.Types.InventoryFilterValue as Types
import qualified Network.AWS.SSM.Types.InventoryQueryOperatorType as Types
import qualified Network.AWS.SSM.Types.Key as Types

-- | One or more filters. Use a filter to return a more specific list of results.
--
-- /See:/ 'mkInventoryFilter' smart constructor.
data InventoryFilter = InventoryFilter'
  { -- | The name of the filter key.
    key :: Types.Key,
    -- | Inventory filter values. Example: inventory filter where instance IDs are specified as values Key=AWS:InstanceInformation.InstanceId,Values= i-a12b3c4d5e6g, i-1a2b3c4d5e6,Type=Equal
    values :: Core.NonEmpty Types.InventoryFilterValue,
    -- | The type of filter.
    type' :: Core.Maybe Types.InventoryQueryOperatorType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'InventoryFilter' value with any optional fields omitted.
mkInventoryFilter ::
  -- | 'key'
  Types.Key ->
  -- | 'values'
  Core.NonEmpty Types.InventoryFilterValue ->
  InventoryFilter
mkInventoryFilter key values =
  InventoryFilter' {key, values, type' = Core.Nothing}

-- | The name of the filter key.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ifKey :: Lens.Lens' InventoryFilter Types.Key
ifKey = Lens.field @"key"
{-# DEPRECATED ifKey "Use generic-lens or generic-optics with 'key' instead." #-}

-- | Inventory filter values. Example: inventory filter where instance IDs are specified as values Key=AWS:InstanceInformation.InstanceId,Values= i-a12b3c4d5e6g, i-1a2b3c4d5e6,Type=Equal
--
-- /Note:/ Consider using 'values' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ifValues :: Lens.Lens' InventoryFilter (Core.NonEmpty Types.InventoryFilterValue)
ifValues = Lens.field @"values"
{-# DEPRECATED ifValues "Use generic-lens or generic-optics with 'values' instead." #-}

-- | The type of filter.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ifType :: Lens.Lens' InventoryFilter (Core.Maybe Types.InventoryQueryOperatorType)
ifType = Lens.field @"type'"
{-# DEPRECATED ifType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Core.FromJSON InventoryFilter where
  toJSON InventoryFilter {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Key" Core..= key),
            Core.Just ("Values" Core..= values),
            ("Type" Core..=) Core.<$> type'
          ]
      )
