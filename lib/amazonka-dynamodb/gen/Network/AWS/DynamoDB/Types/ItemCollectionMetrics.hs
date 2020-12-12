{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.ItemCollectionMetrics
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.ItemCollectionMetrics
  ( ItemCollectionMetrics (..),

    -- * Smart constructor
    mkItemCollectionMetrics,

    -- * Lenses
    icmItemCollectionKey,
    icmSizeEstimateRangeGB,
  )
where

import Network.AWS.DynamoDB.Types.AttributeValue
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about item collections, if any, that were affected by the operation. @ItemCollectionMetrics@ is only returned if the request asked for it. If the table does not have any local secondary indexes, this information is not returned in the response.
--
-- /See:/ 'mkItemCollectionMetrics' smart constructor.
data ItemCollectionMetrics = ItemCollectionMetrics'
  { itemCollectionKey ::
      Lude.Maybe
        (Lude.HashMap Lude.Text (AttributeValue)),
    sizeEstimateRangeGB :: Lude.Maybe [Lude.Double]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ItemCollectionMetrics' with the minimum fields required to make a request.
--
-- * 'itemCollectionKey' - The partition key value of the item collection. This value is the same as the partition key value of the item.
-- * 'sizeEstimateRangeGB' - An estimate of item collection size, in gigabytes. This value is a two-element array containing a lower bound and an upper bound for the estimate. The estimate includes the size of all the items in the table, plus the size of all attributes projected into all of the local secondary indexes on that table. Use this estimate to measure whether a local secondary index is approaching its size limit.
--
-- The estimate is subject to change over time; therefore, do not rely on the precision or accuracy of the estimate.
mkItemCollectionMetrics ::
  ItemCollectionMetrics
mkItemCollectionMetrics =
  ItemCollectionMetrics'
    { itemCollectionKey = Lude.Nothing,
      sizeEstimateRangeGB = Lude.Nothing
    }

-- | The partition key value of the item collection. This value is the same as the partition key value of the item.
--
-- /Note:/ Consider using 'itemCollectionKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
icmItemCollectionKey :: Lens.Lens' ItemCollectionMetrics (Lude.Maybe (Lude.HashMap Lude.Text (AttributeValue)))
icmItemCollectionKey = Lens.lens (itemCollectionKey :: ItemCollectionMetrics -> Lude.Maybe (Lude.HashMap Lude.Text (AttributeValue))) (\s a -> s {itemCollectionKey = a} :: ItemCollectionMetrics)
{-# DEPRECATED icmItemCollectionKey "Use generic-lens or generic-optics with 'itemCollectionKey' instead." #-}

-- | An estimate of item collection size, in gigabytes. This value is a two-element array containing a lower bound and an upper bound for the estimate. The estimate includes the size of all the items in the table, plus the size of all attributes projected into all of the local secondary indexes on that table. Use this estimate to measure whether a local secondary index is approaching its size limit.
--
-- The estimate is subject to change over time; therefore, do not rely on the precision or accuracy of the estimate.
--
-- /Note:/ Consider using 'sizeEstimateRangeGB' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
icmSizeEstimateRangeGB :: Lens.Lens' ItemCollectionMetrics (Lude.Maybe [Lude.Double])
icmSizeEstimateRangeGB = Lens.lens (sizeEstimateRangeGB :: ItemCollectionMetrics -> Lude.Maybe [Lude.Double]) (\s a -> s {sizeEstimateRangeGB = a} :: ItemCollectionMetrics)
{-# DEPRECATED icmSizeEstimateRangeGB "Use generic-lens or generic-optics with 'sizeEstimateRangeGB' instead." #-}

instance Lude.FromJSON ItemCollectionMetrics where
  parseJSON =
    Lude.withObject
      "ItemCollectionMetrics"
      ( \x ->
          ItemCollectionMetrics'
            Lude.<$> (x Lude..:? "ItemCollectionKey" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "SizeEstimateRangeGB" Lude..!= Lude.mempty)
      )
