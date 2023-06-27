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
-- Module      : Amazonka.CloudFront.Types.DistributionList
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudFront.Types.DistributionList where

import Amazonka.CloudFront.Types.DistributionSummary
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A distribution list.
--
-- /See:/ 'newDistributionList' smart constructor.
data DistributionList = DistributionList'
  { -- | A complex type that contains one @DistributionSummary@ element for each
    -- distribution that was created by the current Amazon Web Services
    -- account.
    items :: Prelude.Maybe [DistributionSummary],
    -- | If @IsTruncated@ is @true@, this element is present and contains the
    -- value you can use for the @Marker@ request parameter to continue listing
    -- your distributions where they left off.
    nextMarker :: Prelude.Maybe Prelude.Text,
    -- | The value you provided for the @Marker@ request parameter.
    marker :: Prelude.Text,
    -- | The value you provided for the @MaxItems@ request parameter.
    maxItems :: Prelude.Int,
    -- | A flag that indicates whether more distributions remain to be listed. If
    -- your results were truncated, you can make a follow-up pagination request
    -- using the @Marker@ request parameter to retrieve more distributions in
    -- the list.
    isTruncated :: Prelude.Bool,
    -- | The number of distributions that were created by the current Amazon Web
    -- Services account.
    quantity :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DistributionList' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'items', 'distributionList_items' - A complex type that contains one @DistributionSummary@ element for each
-- distribution that was created by the current Amazon Web Services
-- account.
--
-- 'nextMarker', 'distributionList_nextMarker' - If @IsTruncated@ is @true@, this element is present and contains the
-- value you can use for the @Marker@ request parameter to continue listing
-- your distributions where they left off.
--
-- 'marker', 'distributionList_marker' - The value you provided for the @Marker@ request parameter.
--
-- 'maxItems', 'distributionList_maxItems' - The value you provided for the @MaxItems@ request parameter.
--
-- 'isTruncated', 'distributionList_isTruncated' - A flag that indicates whether more distributions remain to be listed. If
-- your results were truncated, you can make a follow-up pagination request
-- using the @Marker@ request parameter to retrieve more distributions in
-- the list.
--
-- 'quantity', 'distributionList_quantity' - The number of distributions that were created by the current Amazon Web
-- Services account.
newDistributionList ::
  -- | 'marker'
  Prelude.Text ->
  -- | 'maxItems'
  Prelude.Int ->
  -- | 'isTruncated'
  Prelude.Bool ->
  -- | 'quantity'
  Prelude.Int ->
  DistributionList
newDistributionList
  pMarker_
  pMaxItems_
  pIsTruncated_
  pQuantity_ =
    DistributionList'
      { items = Prelude.Nothing,
        nextMarker = Prelude.Nothing,
        marker = pMarker_,
        maxItems = pMaxItems_,
        isTruncated = pIsTruncated_,
        quantity = pQuantity_
      }

-- | A complex type that contains one @DistributionSummary@ element for each
-- distribution that was created by the current Amazon Web Services
-- account.
distributionList_items :: Lens.Lens' DistributionList (Prelude.Maybe [DistributionSummary])
distributionList_items = Lens.lens (\DistributionList' {items} -> items) (\s@DistributionList' {} a -> s {items = a} :: DistributionList) Prelude.. Lens.mapping Lens.coerced

-- | If @IsTruncated@ is @true@, this element is present and contains the
-- value you can use for the @Marker@ request parameter to continue listing
-- your distributions where they left off.
distributionList_nextMarker :: Lens.Lens' DistributionList (Prelude.Maybe Prelude.Text)
distributionList_nextMarker = Lens.lens (\DistributionList' {nextMarker} -> nextMarker) (\s@DistributionList' {} a -> s {nextMarker = a} :: DistributionList)

-- | The value you provided for the @Marker@ request parameter.
distributionList_marker :: Lens.Lens' DistributionList Prelude.Text
distributionList_marker = Lens.lens (\DistributionList' {marker} -> marker) (\s@DistributionList' {} a -> s {marker = a} :: DistributionList)

-- | The value you provided for the @MaxItems@ request parameter.
distributionList_maxItems :: Lens.Lens' DistributionList Prelude.Int
distributionList_maxItems = Lens.lens (\DistributionList' {maxItems} -> maxItems) (\s@DistributionList' {} a -> s {maxItems = a} :: DistributionList)

-- | A flag that indicates whether more distributions remain to be listed. If
-- your results were truncated, you can make a follow-up pagination request
-- using the @Marker@ request parameter to retrieve more distributions in
-- the list.
distributionList_isTruncated :: Lens.Lens' DistributionList Prelude.Bool
distributionList_isTruncated = Lens.lens (\DistributionList' {isTruncated} -> isTruncated) (\s@DistributionList' {} a -> s {isTruncated = a} :: DistributionList)

-- | The number of distributions that were created by the current Amazon Web
-- Services account.
distributionList_quantity :: Lens.Lens' DistributionList Prelude.Int
distributionList_quantity = Lens.lens (\DistributionList' {quantity} -> quantity) (\s@DistributionList' {} a -> s {quantity = a} :: DistributionList)

instance Data.FromXML DistributionList where
  parseXML x =
    DistributionList'
      Prelude.<$> ( x
                      Data..@? "Items"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "DistributionSummary")
                  )
      Prelude.<*> (x Data..@? "NextMarker")
      Prelude.<*> (x Data..@ "Marker")
      Prelude.<*> (x Data..@ "MaxItems")
      Prelude.<*> (x Data..@ "IsTruncated")
      Prelude.<*> (x Data..@ "Quantity")

instance Prelude.Hashable DistributionList where
  hashWithSalt _salt DistributionList' {..} =
    _salt
      `Prelude.hashWithSalt` items
      `Prelude.hashWithSalt` nextMarker
      `Prelude.hashWithSalt` marker
      `Prelude.hashWithSalt` maxItems
      `Prelude.hashWithSalt` isTruncated
      `Prelude.hashWithSalt` quantity

instance Prelude.NFData DistributionList where
  rnf DistributionList' {..} =
    Prelude.rnf items
      `Prelude.seq` Prelude.rnf nextMarker
      `Prelude.seq` Prelude.rnf marker
      `Prelude.seq` Prelude.rnf maxItems
      `Prelude.seq` Prelude.rnf isTruncated
      `Prelude.seq` Prelude.rnf quantity
