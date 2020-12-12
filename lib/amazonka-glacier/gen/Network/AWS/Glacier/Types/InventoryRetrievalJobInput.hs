{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glacier.Types.InventoryRetrievalJobInput
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glacier.Types.InventoryRetrievalJobInput
  ( InventoryRetrievalJobInput (..),

    -- * Smart constructor
    mkInventoryRetrievalJobInput,

    -- * Lenses
    irjiEndDate,
    irjiStartDate,
    irjiMarker,
    irjiLimit,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Provides options for specifying a range inventory retrieval job.
--
-- /See:/ 'mkInventoryRetrievalJobInput' smart constructor.
data InventoryRetrievalJobInput = InventoryRetrievalJobInput'
  { endDate ::
      Lude.Maybe Lude.Text,
    startDate :: Lude.Maybe Lude.Text,
    marker :: Lude.Maybe Lude.Text,
    limit :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'InventoryRetrievalJobInput' with the minimum fields required to make a request.
--
-- * 'endDate' - The end of the date range in UTC for vault inventory retrieval that includes archives created before this date. This value should be a string in the ISO 8601 date format, for example @2013-03-20T17:03:43Z@ .
-- * 'limit' - Specifies the maximum number of inventory items returned per vault inventory retrieval request. Valid values are greater than or equal to 1.
-- * 'marker' - An opaque string that represents where to continue pagination of the vault inventory retrieval results. You use the marker in a new __InitiateJob__ request to obtain additional inventory items. If there are no more inventory items, this value is @null@ .
-- * 'startDate' - The start of the date range in UTC for vault inventory retrieval that includes archives created on or after this date. This value should be a string in the ISO 8601 date format, for example @2013-03-20T17:03:43Z@ .
mkInventoryRetrievalJobInput ::
  InventoryRetrievalJobInput
mkInventoryRetrievalJobInput =
  InventoryRetrievalJobInput'
    { endDate = Lude.Nothing,
      startDate = Lude.Nothing,
      marker = Lude.Nothing,
      limit = Lude.Nothing
    }

-- | The end of the date range in UTC for vault inventory retrieval that includes archives created before this date. This value should be a string in the ISO 8601 date format, for example @2013-03-20T17:03:43Z@ .
--
-- /Note:/ Consider using 'endDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
irjiEndDate :: Lens.Lens' InventoryRetrievalJobInput (Lude.Maybe Lude.Text)
irjiEndDate = Lens.lens (endDate :: InventoryRetrievalJobInput -> Lude.Maybe Lude.Text) (\s a -> s {endDate = a} :: InventoryRetrievalJobInput)
{-# DEPRECATED irjiEndDate "Use generic-lens or generic-optics with 'endDate' instead." #-}

-- | The start of the date range in UTC for vault inventory retrieval that includes archives created on or after this date. This value should be a string in the ISO 8601 date format, for example @2013-03-20T17:03:43Z@ .
--
-- /Note:/ Consider using 'startDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
irjiStartDate :: Lens.Lens' InventoryRetrievalJobInput (Lude.Maybe Lude.Text)
irjiStartDate = Lens.lens (startDate :: InventoryRetrievalJobInput -> Lude.Maybe Lude.Text) (\s a -> s {startDate = a} :: InventoryRetrievalJobInput)
{-# DEPRECATED irjiStartDate "Use generic-lens or generic-optics with 'startDate' instead." #-}

-- | An opaque string that represents where to continue pagination of the vault inventory retrieval results. You use the marker in a new __InitiateJob__ request to obtain additional inventory items. If there are no more inventory items, this value is @null@ .
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
irjiMarker :: Lens.Lens' InventoryRetrievalJobInput (Lude.Maybe Lude.Text)
irjiMarker = Lens.lens (marker :: InventoryRetrievalJobInput -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: InventoryRetrievalJobInput)
{-# DEPRECATED irjiMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | Specifies the maximum number of inventory items returned per vault inventory retrieval request. Valid values are greater than or equal to 1.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
irjiLimit :: Lens.Lens' InventoryRetrievalJobInput (Lude.Maybe Lude.Text)
irjiLimit = Lens.lens (limit :: InventoryRetrievalJobInput -> Lude.Maybe Lude.Text) (\s a -> s {limit = a} :: InventoryRetrievalJobInput)
{-# DEPRECATED irjiLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

instance Lude.ToJSON InventoryRetrievalJobInput where
  toJSON InventoryRetrievalJobInput' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("EndDate" Lude..=) Lude.<$> endDate,
            ("StartDate" Lude..=) Lude.<$> startDate,
            ("Marker" Lude..=) Lude.<$> marker,
            ("Limit" Lude..=) Lude.<$> limit
          ]
      )
