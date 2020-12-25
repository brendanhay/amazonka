{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.MaintenanceWindowFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.MaintenanceWindowFilter
  ( MaintenanceWindowFilter (..),

    -- * Smart constructor
    mkMaintenanceWindowFilter,

    -- * Lenses
    mwfKey,
    mwfValues,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SSM.Types.MaintenanceWindowFilterKey as Types
import qualified Network.AWS.SSM.Types.MaintenanceWindowFilterValue as Types

-- | Filter used in the request. Supported filter keys are Name and Enabled.
--
-- /See:/ 'mkMaintenanceWindowFilter' smart constructor.
data MaintenanceWindowFilter = MaintenanceWindowFilter'
  { -- | The name of the filter.
    key :: Core.Maybe Types.MaintenanceWindowFilterKey,
    -- | The filter values.
    values :: Core.Maybe [Types.MaintenanceWindowFilterValue]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'MaintenanceWindowFilter' value with any optional fields omitted.
mkMaintenanceWindowFilter ::
  MaintenanceWindowFilter
mkMaintenanceWindowFilter =
  MaintenanceWindowFilter'
    { key = Core.Nothing,
      values = Core.Nothing
    }

-- | The name of the filter.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mwfKey :: Lens.Lens' MaintenanceWindowFilter (Core.Maybe Types.MaintenanceWindowFilterKey)
mwfKey = Lens.field @"key"
{-# DEPRECATED mwfKey "Use generic-lens or generic-optics with 'key' instead." #-}

-- | The filter values.
--
-- /Note:/ Consider using 'values' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mwfValues :: Lens.Lens' MaintenanceWindowFilter (Core.Maybe [Types.MaintenanceWindowFilterValue])
mwfValues = Lens.field @"values"
{-# DEPRECATED mwfValues "Use generic-lens or generic-optics with 'values' instead." #-}

instance Core.FromJSON MaintenanceWindowFilter where
  toJSON MaintenanceWindowFilter {..} =
    Core.object
      ( Core.catMaybes
          [("Key" Core..=) Core.<$> key, ("Values" Core..=) Core.<$> values]
      )
