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
    irjdFormat,
    irjdEndDate,
    irjdStartDate,
    irjdMarker,
    irjdLimit,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the options for a range inventory retrieval job.
--
-- /See:/ 'mkInventoryRetrievalJobDescription' smart constructor.
data InventoryRetrievalJobDescription = InventoryRetrievalJobDescription'
  { format ::
      Lude.Maybe Lude.Text,
    endDate ::
      Lude.Maybe Lude.Text,
    startDate ::
      Lude.Maybe Lude.Text,
    marker ::
      Lude.Maybe Lude.Text,
    limit ::
      Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'InventoryRetrievalJobDescription' with the minimum fields required to make a request.
--
-- * 'endDate' - The end of the date range in UTC for vault inventory retrieval that includes archives created before this date. This value should be a string in the ISO 8601 date format, for example @2013-03-20T17:03:43Z@ .
-- * 'format' - The output format for the vault inventory list, which is set by the __InitiateJob__ request when initiating a job to retrieve a vault inventory. Valid values are @CSV@ and @JSON@ .
-- * 'limit' - The maximum number of inventory items returned per vault inventory retrieval request. This limit is set when initiating the job with the a __InitiateJob__ request.
-- * 'marker' - An opaque string that represents where to continue pagination of the vault inventory retrieval results. You use the marker in a new __InitiateJob__ request to obtain additional inventory items. If there are no more inventory items, this value is @null@ . For more information, see <https://docs.aws.amazon.com/amazonglacier/latest/dev/api-initiate-job-post.html#api-initiate-job-post-vault-inventory-list-filtering Range Inventory Retrieval> .
-- * 'startDate' - The start of the date range in Universal Coordinated Time (UTC) for vault inventory retrieval that includes archives created on or after this date. This value should be a string in the ISO 8601 date format, for example @2013-03-20T17:03:43Z@ .
mkInventoryRetrievalJobDescription ::
  InventoryRetrievalJobDescription
mkInventoryRetrievalJobDescription =
  InventoryRetrievalJobDescription'
    { format = Lude.Nothing,
      endDate = Lude.Nothing,
      startDate = Lude.Nothing,
      marker = Lude.Nothing,
      limit = Lude.Nothing
    }

-- | The output format for the vault inventory list, which is set by the __InitiateJob__ request when initiating a job to retrieve a vault inventory. Valid values are @CSV@ and @JSON@ .
--
-- /Note:/ Consider using 'format' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
irjdFormat :: Lens.Lens' InventoryRetrievalJobDescription (Lude.Maybe Lude.Text)
irjdFormat = Lens.lens (format :: InventoryRetrievalJobDescription -> Lude.Maybe Lude.Text) (\s a -> s {format = a} :: InventoryRetrievalJobDescription)
{-# DEPRECATED irjdFormat "Use generic-lens or generic-optics with 'format' instead." #-}

-- | The end of the date range in UTC for vault inventory retrieval that includes archives created before this date. This value should be a string in the ISO 8601 date format, for example @2013-03-20T17:03:43Z@ .
--
-- /Note:/ Consider using 'endDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
irjdEndDate :: Lens.Lens' InventoryRetrievalJobDescription (Lude.Maybe Lude.Text)
irjdEndDate = Lens.lens (endDate :: InventoryRetrievalJobDescription -> Lude.Maybe Lude.Text) (\s a -> s {endDate = a} :: InventoryRetrievalJobDescription)
{-# DEPRECATED irjdEndDate "Use generic-lens or generic-optics with 'endDate' instead." #-}

-- | The start of the date range in Universal Coordinated Time (UTC) for vault inventory retrieval that includes archives created on or after this date. This value should be a string in the ISO 8601 date format, for example @2013-03-20T17:03:43Z@ .
--
-- /Note:/ Consider using 'startDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
irjdStartDate :: Lens.Lens' InventoryRetrievalJobDescription (Lude.Maybe Lude.Text)
irjdStartDate = Lens.lens (startDate :: InventoryRetrievalJobDescription -> Lude.Maybe Lude.Text) (\s a -> s {startDate = a} :: InventoryRetrievalJobDescription)
{-# DEPRECATED irjdStartDate "Use generic-lens or generic-optics with 'startDate' instead." #-}

-- | An opaque string that represents where to continue pagination of the vault inventory retrieval results. You use the marker in a new __InitiateJob__ request to obtain additional inventory items. If there are no more inventory items, this value is @null@ . For more information, see <https://docs.aws.amazon.com/amazonglacier/latest/dev/api-initiate-job-post.html#api-initiate-job-post-vault-inventory-list-filtering Range Inventory Retrieval> .
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
irjdMarker :: Lens.Lens' InventoryRetrievalJobDescription (Lude.Maybe Lude.Text)
irjdMarker = Lens.lens (marker :: InventoryRetrievalJobDescription -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: InventoryRetrievalJobDescription)
{-# DEPRECATED irjdMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The maximum number of inventory items returned per vault inventory retrieval request. This limit is set when initiating the job with the a __InitiateJob__ request.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
irjdLimit :: Lens.Lens' InventoryRetrievalJobDescription (Lude.Maybe Lude.Text)
irjdLimit = Lens.lens (limit :: InventoryRetrievalJobDescription -> Lude.Maybe Lude.Text) (\s a -> s {limit = a} :: InventoryRetrievalJobDescription)
{-# DEPRECATED irjdLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

instance Lude.FromJSON InventoryRetrievalJobDescription where
  parseJSON =
    Lude.withObject
      "InventoryRetrievalJobDescription"
      ( \x ->
          InventoryRetrievalJobDescription'
            Lude.<$> (x Lude..:? "Format")
            Lude.<*> (x Lude..:? "EndDate")
            Lude.<*> (x Lude..:? "StartDate")
            Lude.<*> (x Lude..:? "Marker")
            Lude.<*> (x Lude..:? "Limit")
      )
