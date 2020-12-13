{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.ResultRow
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.ResultRow
  ( ResultRow (..),

    -- * Smart constructor
    mkResultRow,

    -- * Lenses
    rrValues,
    rrGroupedBys,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types.ResultRowValue
import qualified Network.AWS.Prelude as Lude

-- | Provides the results of a query that retrieved the data for a standard metric that applies to an application, campaign, or journey.
--
-- /See:/ 'mkResultRow' smart constructor.
data ResultRow = ResultRow'
  { -- | An array of objects that provides pre-aggregated values for a standard metric that applies to an application, campaign, or journey.
    values :: [ResultRowValue],
    -- | An array of objects that defines the field and field values that were used to group data in a result set that contains multiple results. This value is null if the data in a result set isn’t grouped.
    groupedBys :: [ResultRowValue]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ResultRow' with the minimum fields required to make a request.
--
-- * 'values' - An array of objects that provides pre-aggregated values for a standard metric that applies to an application, campaign, or journey.
-- * 'groupedBys' - An array of objects that defines the field and field values that were used to group data in a result set that contains multiple results. This value is null if the data in a result set isn’t grouped.
mkResultRow ::
  ResultRow
mkResultRow =
  ResultRow' {values = Lude.mempty, groupedBys = Lude.mempty}

-- | An array of objects that provides pre-aggregated values for a standard metric that applies to an application, campaign, or journey.
--
-- /Note:/ Consider using 'values' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrValues :: Lens.Lens' ResultRow [ResultRowValue]
rrValues = Lens.lens (values :: ResultRow -> [ResultRowValue]) (\s a -> s {values = a} :: ResultRow)
{-# DEPRECATED rrValues "Use generic-lens or generic-optics with 'values' instead." #-}

-- | An array of objects that defines the field and field values that were used to group data in a result set that contains multiple results. This value is null if the data in a result set isn’t grouped.
--
-- /Note:/ Consider using 'groupedBys' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrGroupedBys :: Lens.Lens' ResultRow [ResultRowValue]
rrGroupedBys = Lens.lens (groupedBys :: ResultRow -> [ResultRowValue]) (\s a -> s {groupedBys = a} :: ResultRow)
{-# DEPRECATED rrGroupedBys "Use generic-lens or generic-optics with 'groupedBys' instead." #-}

instance Lude.FromJSON ResultRow where
  parseJSON =
    Lude.withObject
      "ResultRow"
      ( \x ->
          ResultRow'
            Lude.<$> (x Lude..:? "Values" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "GroupedBys" Lude..!= Lude.mempty)
      )
