{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glacier.Types.InventoryRetrievalJobInput
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Glacier.Types.InventoryRetrievalJobInput
  ( InventoryRetrievalJobInput (..)
  -- * Smart constructor
  , mkInventoryRetrievalJobInput
  -- * Lenses
  , irjiEndDate
  , irjiLimit
  , irjiMarker
  , irjiStartDate
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Provides options for specifying a range inventory retrieval job.
--
-- /See:/ 'mkInventoryRetrievalJobInput' smart constructor.
data InventoryRetrievalJobInput = InventoryRetrievalJobInput'
  { endDate :: Core.Maybe Core.Text
    -- ^ The end of the date range in UTC for vault inventory retrieval that includes archives created before this date. This value should be a string in the ISO 8601 date format, for example @2013-03-20T17:03:43Z@ .
  , limit :: Core.Maybe Core.Text
    -- ^ Specifies the maximum number of inventory items returned per vault inventory retrieval request. Valid values are greater than or equal to 1.
  , marker :: Core.Maybe Core.Text
    -- ^ An opaque string that represents where to continue pagination of the vault inventory retrieval results. You use the marker in a new __InitiateJob__ request to obtain additional inventory items. If there are no more inventory items, this value is @null@ .
  , startDate :: Core.Maybe Core.Text
    -- ^ The start of the date range in UTC for vault inventory retrieval that includes archives created on or after this date. This value should be a string in the ISO 8601 date format, for example @2013-03-20T17:03:43Z@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'InventoryRetrievalJobInput' value with any optional fields omitted.
mkInventoryRetrievalJobInput
    :: InventoryRetrievalJobInput
mkInventoryRetrievalJobInput
  = InventoryRetrievalJobInput'{endDate = Core.Nothing,
                                limit = Core.Nothing, marker = Core.Nothing,
                                startDate = Core.Nothing}

-- | The end of the date range in UTC for vault inventory retrieval that includes archives created before this date. This value should be a string in the ISO 8601 date format, for example @2013-03-20T17:03:43Z@ .
--
-- /Note:/ Consider using 'endDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
irjiEndDate :: Lens.Lens' InventoryRetrievalJobInput (Core.Maybe Core.Text)
irjiEndDate = Lens.field @"endDate"
{-# INLINEABLE irjiEndDate #-}
{-# DEPRECATED endDate "Use generic-lens or generic-optics with 'endDate' instead"  #-}

-- | Specifies the maximum number of inventory items returned per vault inventory retrieval request. Valid values are greater than or equal to 1.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
irjiLimit :: Lens.Lens' InventoryRetrievalJobInput (Core.Maybe Core.Text)
irjiLimit = Lens.field @"limit"
{-# INLINEABLE irjiLimit #-}
{-# DEPRECATED limit "Use generic-lens or generic-optics with 'limit' instead"  #-}

-- | An opaque string that represents where to continue pagination of the vault inventory retrieval results. You use the marker in a new __InitiateJob__ request to obtain additional inventory items. If there are no more inventory items, this value is @null@ .
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
irjiMarker :: Lens.Lens' InventoryRetrievalJobInput (Core.Maybe Core.Text)
irjiMarker = Lens.field @"marker"
{-# INLINEABLE irjiMarker #-}
{-# DEPRECATED marker "Use generic-lens or generic-optics with 'marker' instead"  #-}

-- | The start of the date range in UTC for vault inventory retrieval that includes archives created on or after this date. This value should be a string in the ISO 8601 date format, for example @2013-03-20T17:03:43Z@ .
--
-- /Note:/ Consider using 'startDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
irjiStartDate :: Lens.Lens' InventoryRetrievalJobInput (Core.Maybe Core.Text)
irjiStartDate = Lens.field @"startDate"
{-# INLINEABLE irjiStartDate #-}
{-# DEPRECATED startDate "Use generic-lens or generic-optics with 'startDate' instead"  #-}

instance Core.FromJSON InventoryRetrievalJobInput where
        toJSON InventoryRetrievalJobInput{..}
          = Core.object
              (Core.catMaybes
                 [("EndDate" Core..=) Core.<$> endDate,
                  ("Limit" Core..=) Core.<$> limit,
                  ("Marker" Core..=) Core.<$> marker,
                  ("StartDate" Core..=) Core.<$> startDate])
