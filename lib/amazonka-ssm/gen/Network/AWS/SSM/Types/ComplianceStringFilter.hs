{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.ComplianceStringFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.ComplianceStringFilter
  ( ComplianceStringFilter (..),

    -- * Smart constructor
    mkComplianceStringFilter,

    -- * Lenses
    csfKey,
    csfType,
    csfValues,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SSM.Types.ComplianceFilterValue as Types
import qualified Network.AWS.SSM.Types.ComplianceQueryOperatorType as Types
import qualified Network.AWS.SSM.Types.ComplianceStringFilterKey as Types

-- | One or more filters. Use a filter to return a more specific list of results.
--
-- /See:/ 'mkComplianceStringFilter' smart constructor.
data ComplianceStringFilter = ComplianceStringFilter'
  { -- | The name of the filter.
    key :: Core.Maybe Types.ComplianceStringFilterKey,
    -- | The type of comparison that should be performed for the value: Equal, NotEqual, BeginWith, LessThan, or GreaterThan.
    type' :: Core.Maybe Types.ComplianceQueryOperatorType,
    -- | The value for which to search.
    values :: Core.Maybe (Core.NonEmpty Types.ComplianceFilterValue)
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ComplianceStringFilter' value with any optional fields omitted.
mkComplianceStringFilter ::
  ComplianceStringFilter
mkComplianceStringFilter =
  ComplianceStringFilter'
    { key = Core.Nothing,
      type' = Core.Nothing,
      values = Core.Nothing
    }

-- | The name of the filter.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csfKey :: Lens.Lens' ComplianceStringFilter (Core.Maybe Types.ComplianceStringFilterKey)
csfKey = Lens.field @"key"
{-# DEPRECATED csfKey "Use generic-lens or generic-optics with 'key' instead." #-}

-- | The type of comparison that should be performed for the value: Equal, NotEqual, BeginWith, LessThan, or GreaterThan.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csfType :: Lens.Lens' ComplianceStringFilter (Core.Maybe Types.ComplianceQueryOperatorType)
csfType = Lens.field @"type'"
{-# DEPRECATED csfType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | The value for which to search.
--
-- /Note:/ Consider using 'values' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csfValues :: Lens.Lens' ComplianceStringFilter (Core.Maybe (Core.NonEmpty Types.ComplianceFilterValue))
csfValues = Lens.field @"values"
{-# DEPRECATED csfValues "Use generic-lens or generic-optics with 'values' instead." #-}

instance Core.FromJSON ComplianceStringFilter where
  toJSON ComplianceStringFilter {..} =
    Core.object
      ( Core.catMaybes
          [ ("Key" Core..=) Core.<$> key,
            ("Type" Core..=) Core.<$> type',
            ("Values" Core..=) Core.<$> values
          ]
      )
