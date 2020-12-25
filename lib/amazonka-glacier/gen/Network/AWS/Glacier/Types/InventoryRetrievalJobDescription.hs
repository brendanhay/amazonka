{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glacier.Types.InventoryRetrievalJobDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glacier.Types.InventoryRetrievalJobDescription
  ( InventoryRetrievalJobDescription (..),

    -- * Smart constructor
    mkInventoryRetrievalJobDescription,

    -- * Lenses
    irjdEndDate,
    irjdFormat,
    irjdLimit,
    irjdMarker,
    irjdStartDate,
  )
where

import qualified Network.AWS.Glacier.Types.EndDate as Types
import qualified Network.AWS.Glacier.Types.Format as Types
import qualified Network.AWS.Glacier.Types.Limit as Types
import qualified Network.AWS.Glacier.Types.Marker as Types
import qualified Network.AWS.Glacier.Types.StartDate as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the options for a range inventory retrieval job.
--
-- /See:/ 'mkInventoryRetrievalJobDescription' smart constructor.
data InventoryRetrievalJobDescription = InventoryRetrievalJobDescription'
  { -- | The end of the date range in UTC for vault inventory retrieval that includes archives created before this date. This value should be a string in the ISO 8601 date format, for example @2013-03-20T17:03:43Z@ .
    endDate :: Core.Maybe Types.EndDate,
    -- | The output format for the vault inventory list, which is set by the __InitiateJob__ request when initiating a job to retrieve a vault inventory. Valid values are @CSV@ and @JSON@ .
    format :: Core.Maybe Types.Format,
    -- | The maximum number of inventory items returned per vault inventory retrieval request. This limit is set when initiating the job with the a __InitiateJob__ request.
    limit :: Core.Maybe Types.Limit,
    -- | An opaque string that represents where to continue pagination of the vault inventory retrieval results. You use the marker in a new __InitiateJob__ request to obtain additional inventory items. If there are no more inventory items, this value is @null@ . For more information, see <https://docs.aws.amazon.com/amazonglacier/latest/dev/api-initiate-job-post.html#api-initiate-job-post-vault-inventory-list-filtering Range Inventory Retrieval> .
    marker :: Core.Maybe Types.Marker,
    -- | The start of the date range in Universal Coordinated Time (UTC) for vault inventory retrieval that includes archives created on or after this date. This value should be a string in the ISO 8601 date format, for example @2013-03-20T17:03:43Z@ .
    startDate :: Core.Maybe Types.StartDate
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'InventoryRetrievalJobDescription' value with any optional fields omitted.
mkInventoryRetrievalJobDescription ::
  InventoryRetrievalJobDescription
mkInventoryRetrievalJobDescription =
  InventoryRetrievalJobDescription'
    { endDate = Core.Nothing,
      format = Core.Nothing,
      limit = Core.Nothing,
      marker = Core.Nothing,
      startDate = Core.Nothing
    }

-- | The end of the date range in UTC for vault inventory retrieval that includes archives created before this date. This value should be a string in the ISO 8601 date format, for example @2013-03-20T17:03:43Z@ .
--
-- /Note:/ Consider using 'endDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
irjdEndDate :: Lens.Lens' InventoryRetrievalJobDescription (Core.Maybe Types.EndDate)
irjdEndDate = Lens.field @"endDate"
{-# DEPRECATED irjdEndDate "Use generic-lens or generic-optics with 'endDate' instead." #-}

-- | The output format for the vault inventory list, which is set by the __InitiateJob__ request when initiating a job to retrieve a vault inventory. Valid values are @CSV@ and @JSON@ .
--
-- /Note:/ Consider using 'format' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
irjdFormat :: Lens.Lens' InventoryRetrievalJobDescription (Core.Maybe Types.Format)
irjdFormat = Lens.field @"format"
{-# DEPRECATED irjdFormat "Use generic-lens or generic-optics with 'format' instead." #-}

-- | The maximum number of inventory items returned per vault inventory retrieval request. This limit is set when initiating the job with the a __InitiateJob__ request.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
irjdLimit :: Lens.Lens' InventoryRetrievalJobDescription (Core.Maybe Types.Limit)
irjdLimit = Lens.field @"limit"
{-# DEPRECATED irjdLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

-- | An opaque string that represents where to continue pagination of the vault inventory retrieval results. You use the marker in a new __InitiateJob__ request to obtain additional inventory items. If there are no more inventory items, this value is @null@ . For more information, see <https://docs.aws.amazon.com/amazonglacier/latest/dev/api-initiate-job-post.html#api-initiate-job-post-vault-inventory-list-filtering Range Inventory Retrieval> .
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
irjdMarker :: Lens.Lens' InventoryRetrievalJobDescription (Core.Maybe Types.Marker)
irjdMarker = Lens.field @"marker"
{-# DEPRECATED irjdMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The start of the date range in Universal Coordinated Time (UTC) for vault inventory retrieval that includes archives created on or after this date. This value should be a string in the ISO 8601 date format, for example @2013-03-20T17:03:43Z@ .
--
-- /Note:/ Consider using 'startDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
irjdStartDate :: Lens.Lens' InventoryRetrievalJobDescription (Core.Maybe Types.StartDate)
irjdStartDate = Lens.field @"startDate"
{-# DEPRECATED irjdStartDate "Use generic-lens or generic-optics with 'startDate' instead." #-}

instance Core.FromJSON InventoryRetrievalJobDescription where
  parseJSON =
    Core.withObject "InventoryRetrievalJobDescription" Core.$
      \x ->
        InventoryRetrievalJobDescription'
          Core.<$> (x Core..:? "EndDate")
          Core.<*> (x Core..:? "Format")
          Core.<*> (x Core..:? "Limit")
          Core.<*> (x Core..:? "Marker")
          Core.<*> (x Core..:? "StartDate")
