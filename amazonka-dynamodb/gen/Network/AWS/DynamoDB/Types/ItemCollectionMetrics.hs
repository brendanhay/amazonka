{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.ItemCollectionMetrics
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.ItemCollectionMetrics where

import qualified Network.AWS.Core as Core
import Network.AWS.DynamoDB.Types.AttributeValue
import qualified Network.AWS.Lens as Lens

-- | Information about item collections, if any, that were affected by the
-- operation. @ItemCollectionMetrics@ is only returned if the request asked
-- for it. If the table does not have any local secondary indexes, this
-- information is not returned in the response.
--
-- /See:/ 'newItemCollectionMetrics' smart constructor.
data ItemCollectionMetrics = ItemCollectionMetrics'
  { -- | The partition key value of the item collection. This value is the same
    -- as the partition key value of the item.
    itemCollectionKey :: Core.Maybe (Core.HashMap Core.Text AttributeValue),
    -- | An estimate of item collection size, in gigabytes. This value is a
    -- two-element array containing a lower bound and an upper bound for the
    -- estimate. The estimate includes the size of all the items in the table,
    -- plus the size of all attributes projected into all of the local
    -- secondary indexes on that table. Use this estimate to measure whether a
    -- local secondary index is approaching its size limit.
    --
    -- The estimate is subject to change over time; therefore, do not rely on
    -- the precision or accuracy of the estimate.
    sizeEstimateRangeGB :: Core.Maybe [Core.Double]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ItemCollectionMetrics' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'itemCollectionKey', 'itemCollectionMetrics_itemCollectionKey' - The partition key value of the item collection. This value is the same
-- as the partition key value of the item.
--
-- 'sizeEstimateRangeGB', 'itemCollectionMetrics_sizeEstimateRangeGB' - An estimate of item collection size, in gigabytes. This value is a
-- two-element array containing a lower bound and an upper bound for the
-- estimate. The estimate includes the size of all the items in the table,
-- plus the size of all attributes projected into all of the local
-- secondary indexes on that table. Use this estimate to measure whether a
-- local secondary index is approaching its size limit.
--
-- The estimate is subject to change over time; therefore, do not rely on
-- the precision or accuracy of the estimate.
newItemCollectionMetrics ::
  ItemCollectionMetrics
newItemCollectionMetrics =
  ItemCollectionMetrics'
    { itemCollectionKey =
        Core.Nothing,
      sizeEstimateRangeGB = Core.Nothing
    }

-- | The partition key value of the item collection. This value is the same
-- as the partition key value of the item.
itemCollectionMetrics_itemCollectionKey :: Lens.Lens' ItemCollectionMetrics (Core.Maybe (Core.HashMap Core.Text AttributeValue))
itemCollectionMetrics_itemCollectionKey = Lens.lens (\ItemCollectionMetrics' {itemCollectionKey} -> itemCollectionKey) (\s@ItemCollectionMetrics' {} a -> s {itemCollectionKey = a} :: ItemCollectionMetrics) Core.. Lens.mapping Lens._Coerce

-- | An estimate of item collection size, in gigabytes. This value is a
-- two-element array containing a lower bound and an upper bound for the
-- estimate. The estimate includes the size of all the items in the table,
-- plus the size of all attributes projected into all of the local
-- secondary indexes on that table. Use this estimate to measure whether a
-- local secondary index is approaching its size limit.
--
-- The estimate is subject to change over time; therefore, do not rely on
-- the precision or accuracy of the estimate.
itemCollectionMetrics_sizeEstimateRangeGB :: Lens.Lens' ItemCollectionMetrics (Core.Maybe [Core.Double])
itemCollectionMetrics_sizeEstimateRangeGB = Lens.lens (\ItemCollectionMetrics' {sizeEstimateRangeGB} -> sizeEstimateRangeGB) (\s@ItemCollectionMetrics' {} a -> s {sizeEstimateRangeGB = a} :: ItemCollectionMetrics) Core.. Lens.mapping Lens._Coerce

instance Core.FromJSON ItemCollectionMetrics where
  parseJSON =
    Core.withObject
      "ItemCollectionMetrics"
      ( \x ->
          ItemCollectionMetrics'
            Core.<$> (x Core..:? "ItemCollectionKey" Core..!= Core.mempty)
            Core.<*> ( x Core..:? "SizeEstimateRangeGB"
                         Core..!= Core.mempty
                     )
      )

instance Core.Hashable ItemCollectionMetrics

instance Core.NFData ItemCollectionMetrics
