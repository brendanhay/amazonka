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
-- Module      : Amazonka.CloudFront.Types.DistributionIdList
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudFront.Types.DistributionIdList where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A list of distribution IDs.
--
-- /See:/ 'newDistributionIdList' smart constructor.
data DistributionIdList = DistributionIdList'
  { -- | Contains the distribution IDs in the list.
    items :: Prelude.Maybe [Prelude.Text],
    -- | Contains the value that you should use in the @Marker@ field of a
    -- subsequent request to continue listing distribution IDs where you left
    -- off.
    nextMarker :: Prelude.Maybe Prelude.Text,
    -- | The value provided in the @Marker@ request field.
    marker :: Prelude.Text,
    -- | The maximum number of distribution IDs requested.
    maxItems :: Prelude.Int,
    -- | A flag that indicates whether more distribution IDs remain to be listed.
    -- If your results were truncated, you can make a subsequent request using
    -- the @Marker@ request field to retrieve more distribution IDs in the
    -- list.
    isTruncated :: Prelude.Bool,
    -- | The total number of distribution IDs returned in the response.
    quantity :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DistributionIdList' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'items', 'distributionIdList_items' - Contains the distribution IDs in the list.
--
-- 'nextMarker', 'distributionIdList_nextMarker' - Contains the value that you should use in the @Marker@ field of a
-- subsequent request to continue listing distribution IDs where you left
-- off.
--
-- 'marker', 'distributionIdList_marker' - The value provided in the @Marker@ request field.
--
-- 'maxItems', 'distributionIdList_maxItems' - The maximum number of distribution IDs requested.
--
-- 'isTruncated', 'distributionIdList_isTruncated' - A flag that indicates whether more distribution IDs remain to be listed.
-- If your results were truncated, you can make a subsequent request using
-- the @Marker@ request field to retrieve more distribution IDs in the
-- list.
--
-- 'quantity', 'distributionIdList_quantity' - The total number of distribution IDs returned in the response.
newDistributionIdList ::
  -- | 'marker'
  Prelude.Text ->
  -- | 'maxItems'
  Prelude.Int ->
  -- | 'isTruncated'
  Prelude.Bool ->
  -- | 'quantity'
  Prelude.Int ->
  DistributionIdList
newDistributionIdList
  pMarker_
  pMaxItems_
  pIsTruncated_
  pQuantity_ =
    DistributionIdList'
      { items = Prelude.Nothing,
        nextMarker = Prelude.Nothing,
        marker = pMarker_,
        maxItems = pMaxItems_,
        isTruncated = pIsTruncated_,
        quantity = pQuantity_
      }

-- | Contains the distribution IDs in the list.
distributionIdList_items :: Lens.Lens' DistributionIdList (Prelude.Maybe [Prelude.Text])
distributionIdList_items = Lens.lens (\DistributionIdList' {items} -> items) (\s@DistributionIdList' {} a -> s {items = a} :: DistributionIdList) Prelude.. Lens.mapping Lens.coerced

-- | Contains the value that you should use in the @Marker@ field of a
-- subsequent request to continue listing distribution IDs where you left
-- off.
distributionIdList_nextMarker :: Lens.Lens' DistributionIdList (Prelude.Maybe Prelude.Text)
distributionIdList_nextMarker = Lens.lens (\DistributionIdList' {nextMarker} -> nextMarker) (\s@DistributionIdList' {} a -> s {nextMarker = a} :: DistributionIdList)

-- | The value provided in the @Marker@ request field.
distributionIdList_marker :: Lens.Lens' DistributionIdList Prelude.Text
distributionIdList_marker = Lens.lens (\DistributionIdList' {marker} -> marker) (\s@DistributionIdList' {} a -> s {marker = a} :: DistributionIdList)

-- | The maximum number of distribution IDs requested.
distributionIdList_maxItems :: Lens.Lens' DistributionIdList Prelude.Int
distributionIdList_maxItems = Lens.lens (\DistributionIdList' {maxItems} -> maxItems) (\s@DistributionIdList' {} a -> s {maxItems = a} :: DistributionIdList)

-- | A flag that indicates whether more distribution IDs remain to be listed.
-- If your results were truncated, you can make a subsequent request using
-- the @Marker@ request field to retrieve more distribution IDs in the
-- list.
distributionIdList_isTruncated :: Lens.Lens' DistributionIdList Prelude.Bool
distributionIdList_isTruncated = Lens.lens (\DistributionIdList' {isTruncated} -> isTruncated) (\s@DistributionIdList' {} a -> s {isTruncated = a} :: DistributionIdList)

-- | The total number of distribution IDs returned in the response.
distributionIdList_quantity :: Lens.Lens' DistributionIdList Prelude.Int
distributionIdList_quantity = Lens.lens (\DistributionIdList' {quantity} -> quantity) (\s@DistributionIdList' {} a -> s {quantity = a} :: DistributionIdList)

instance Data.FromXML DistributionIdList where
  parseXML x =
    DistributionIdList'
      Prelude.<$> ( x
                      Data..@? "Items"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "DistributionId")
                  )
      Prelude.<*> (x Data..@? "NextMarker")
      Prelude.<*> (x Data..@ "Marker")
      Prelude.<*> (x Data..@ "MaxItems")
      Prelude.<*> (x Data..@ "IsTruncated")
      Prelude.<*> (x Data..@ "Quantity")

instance Prelude.Hashable DistributionIdList where
  hashWithSalt _salt DistributionIdList' {..} =
    _salt
      `Prelude.hashWithSalt` items
      `Prelude.hashWithSalt` nextMarker
      `Prelude.hashWithSalt` marker
      `Prelude.hashWithSalt` maxItems
      `Prelude.hashWithSalt` isTruncated
      `Prelude.hashWithSalt` quantity

instance Prelude.NFData DistributionIdList where
  rnf DistributionIdList' {..} =
    Prelude.rnf items
      `Prelude.seq` Prelude.rnf nextMarker
      `Prelude.seq` Prelude.rnf marker
      `Prelude.seq` Prelude.rnf maxItems
      `Prelude.seq` Prelude.rnf isTruncated
      `Prelude.seq` Prelude.rnf quantity
