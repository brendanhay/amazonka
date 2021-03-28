{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.ItemCollectionMetrics
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DynamoDB.Types.ItemCollectionMetrics
  ( ItemCollectionMetrics (..)
  -- * Smart constructor
  , mkItemCollectionMetrics
  -- * Lenses
  , icmItemCollectionKey
  , icmSizeEstimateRangeGB
  ) where

import qualified Network.AWS.DynamoDB.Types.AttributeName as Types
import qualified Network.AWS.DynamoDB.Types.AttributeValue as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about item collections, if any, that were affected by the operation. @ItemCollectionMetrics@ is only returned if the request asked for it. If the table does not have any local secondary indexes, this information is not returned in the response.
--
-- /See:/ 'mkItemCollectionMetrics' smart constructor.
data ItemCollectionMetrics = ItemCollectionMetrics'
  { itemCollectionKey :: Core.Maybe (Core.HashMap Types.AttributeName Types.AttributeValue)
    -- ^ The partition key value of the item collection. This value is the same as the partition key value of the item.
  , sizeEstimateRangeGB :: Core.Maybe [Core.Double]
    -- ^ An estimate of item collection size, in gigabytes. This value is a two-element array containing a lower bound and an upper bound for the estimate. The estimate includes the size of all the items in the table, plus the size of all attributes projected into all of the local secondary indexes on that table. Use this estimate to measure whether a local secondary index is approaching its size limit.
--
-- The estimate is subject to change over time; therefore, do not rely on the precision or accuracy of the estimate.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ItemCollectionMetrics' value with any optional fields omitted.
mkItemCollectionMetrics
    :: ItemCollectionMetrics
mkItemCollectionMetrics
  = ItemCollectionMetrics'{itemCollectionKey = Core.Nothing,
                           sizeEstimateRangeGB = Core.Nothing}

-- | The partition key value of the item collection. This value is the same as the partition key value of the item.
--
-- /Note:/ Consider using 'itemCollectionKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
icmItemCollectionKey :: Lens.Lens' ItemCollectionMetrics (Core.Maybe (Core.HashMap Types.AttributeName Types.AttributeValue))
icmItemCollectionKey = Lens.field @"itemCollectionKey"
{-# INLINEABLE icmItemCollectionKey #-}
{-# DEPRECATED itemCollectionKey "Use generic-lens or generic-optics with 'itemCollectionKey' instead"  #-}

-- | An estimate of item collection size, in gigabytes. This value is a two-element array containing a lower bound and an upper bound for the estimate. The estimate includes the size of all the items in the table, plus the size of all attributes projected into all of the local secondary indexes on that table. Use this estimate to measure whether a local secondary index is approaching its size limit.
--
-- The estimate is subject to change over time; therefore, do not rely on the precision or accuracy of the estimate.
--
-- /Note:/ Consider using 'sizeEstimateRangeGB' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
icmSizeEstimateRangeGB :: Lens.Lens' ItemCollectionMetrics (Core.Maybe [Core.Double])
icmSizeEstimateRangeGB = Lens.field @"sizeEstimateRangeGB"
{-# INLINEABLE icmSizeEstimateRangeGB #-}
{-# DEPRECATED sizeEstimateRangeGB "Use generic-lens or generic-optics with 'sizeEstimateRangeGB' instead"  #-}

instance Core.FromJSON ItemCollectionMetrics where
        parseJSON
          = Core.withObject "ItemCollectionMetrics" Core.$
              \ x ->
                ItemCollectionMetrics' Core.<$>
                  (x Core..:? "ItemCollectionKey") Core.<*>
                    x Core..:? "SizeEstimateRangeGB"
