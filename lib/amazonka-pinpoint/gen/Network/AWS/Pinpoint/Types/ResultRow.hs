{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.ResultRow
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Pinpoint.Types.ResultRow
  ( ResultRow (..)
  -- * Smart constructor
  , mkResultRow
  -- * Lenses
  , rrGroupedBys
  , rrValues
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pinpoint.Types.ResultRowValue as Types
import qualified Network.AWS.Prelude as Core

-- | Provides the results of a query that retrieved the data for a standard metric that applies to an application, campaign, or journey.
--
-- /See:/ 'mkResultRow' smart constructor.
data ResultRow = ResultRow'
  { groupedBys :: [Types.ResultRowValue]
    -- ^ An array of objects that defines the field and field values that were used to group data in a result set that contains multiple results. This value is null if the data in a result set isn’t grouped.
  , values :: [Types.ResultRowValue]
    -- ^ An array of objects that provides pre-aggregated values for a standard metric that applies to an application, campaign, or journey.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ResultRow' value with any optional fields omitted.
mkResultRow
    :: ResultRow
mkResultRow
  = ResultRow'{groupedBys = Core.mempty, values = Core.mempty}

-- | An array of objects that defines the field and field values that were used to group data in a result set that contains multiple results. This value is null if the data in a result set isn’t grouped.
--
-- /Note:/ Consider using 'groupedBys' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrGroupedBys :: Lens.Lens' ResultRow [Types.ResultRowValue]
rrGroupedBys = Lens.field @"groupedBys"
{-# INLINEABLE rrGroupedBys #-}
{-# DEPRECATED groupedBys "Use generic-lens or generic-optics with 'groupedBys' instead"  #-}

-- | An array of objects that provides pre-aggregated values for a standard metric that applies to an application, campaign, or journey.
--
-- /Note:/ Consider using 'values' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrValues :: Lens.Lens' ResultRow [Types.ResultRowValue]
rrValues = Lens.field @"values"
{-# INLINEABLE rrValues #-}
{-# DEPRECATED values "Use generic-lens or generic-optics with 'values' instead"  #-}

instance Core.FromJSON ResultRow where
        parseJSON
          = Core.withObject "ResultRow" Core.$
              \ x ->
                ResultRow' Core.<$>
                  (x Core..:? "GroupedBys" Core..!= Core.mempty) Core.<*>
                    x Core..:? "Values" Core..!= Core.mempty
