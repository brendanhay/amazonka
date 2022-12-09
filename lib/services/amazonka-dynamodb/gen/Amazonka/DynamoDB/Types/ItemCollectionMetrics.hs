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
-- Module      : Amazonka.DynamoDB.Types.ItemCollectionMetrics
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DynamoDB.Types.ItemCollectionMetrics where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DynamoDB.Types.AttributeValue
import Amazonka.DynamoDB.Types.WriteRequest
import qualified Amazonka.Prelude as Prelude

-- | Information about item collections, if any, that were affected by the
-- operation. @ItemCollectionMetrics@ is only returned if the request asked
-- for it. If the table does not have any local secondary indexes, this
-- information is not returned in the response.
--
-- /See:/ 'newItemCollectionMetrics' smart constructor.
data ItemCollectionMetrics = ItemCollectionMetrics'
  { -- | The partition key value of the item collection. This value is the same
    -- as the partition key value of the item.
    itemCollectionKey :: Prelude.Maybe (Prelude.HashMap Prelude.Text AttributeValue),
    -- | An estimate of item collection size, in gigabytes. This value is a
    -- two-element array containing a lower bound and an upper bound for the
    -- estimate. The estimate includes the size of all the items in the table,
    -- plus the size of all attributes projected into all of the local
    -- secondary indexes on that table. Use this estimate to measure whether a
    -- local secondary index is approaching its size limit.
    --
    -- The estimate is subject to change over time; therefore, do not rely on
    -- the precision or accuracy of the estimate.
    sizeEstimateRangeGB :: Prelude.Maybe [Prelude.Double]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
        Prelude.Nothing,
      sizeEstimateRangeGB = Prelude.Nothing
    }

-- | The partition key value of the item collection. This value is the same
-- as the partition key value of the item.
itemCollectionMetrics_itemCollectionKey :: Lens.Lens' ItemCollectionMetrics (Prelude.Maybe (Prelude.HashMap Prelude.Text AttributeValue))
itemCollectionMetrics_itemCollectionKey = Lens.lens (\ItemCollectionMetrics' {itemCollectionKey} -> itemCollectionKey) (\s@ItemCollectionMetrics' {} a -> s {itemCollectionKey = a} :: ItemCollectionMetrics) Prelude.. Lens.mapping Lens.coerced

-- | An estimate of item collection size, in gigabytes. This value is a
-- two-element array containing a lower bound and an upper bound for the
-- estimate. The estimate includes the size of all the items in the table,
-- plus the size of all attributes projected into all of the local
-- secondary indexes on that table. Use this estimate to measure whether a
-- local secondary index is approaching its size limit.
--
-- The estimate is subject to change over time; therefore, do not rely on
-- the precision or accuracy of the estimate.
itemCollectionMetrics_sizeEstimateRangeGB :: Lens.Lens' ItemCollectionMetrics (Prelude.Maybe [Prelude.Double])
itemCollectionMetrics_sizeEstimateRangeGB = Lens.lens (\ItemCollectionMetrics' {sizeEstimateRangeGB} -> sizeEstimateRangeGB) (\s@ItemCollectionMetrics' {} a -> s {sizeEstimateRangeGB = a} :: ItemCollectionMetrics) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON ItemCollectionMetrics where
  parseJSON =
    Data.withObject
      "ItemCollectionMetrics"
      ( \x ->
          ItemCollectionMetrics'
            Prelude.<$> ( x Data..:? "ItemCollectionKey"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> ( x Data..:? "SizeEstimateRangeGB"
                            Data..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable ItemCollectionMetrics where
  hashWithSalt _salt ItemCollectionMetrics' {..} =
    _salt `Prelude.hashWithSalt` itemCollectionKey
      `Prelude.hashWithSalt` sizeEstimateRangeGB

instance Prelude.NFData ItemCollectionMetrics where
  rnf ItemCollectionMetrics' {..} =
    Prelude.rnf itemCollectionKey
      `Prelude.seq` Prelude.rnf sizeEstimateRangeGB
