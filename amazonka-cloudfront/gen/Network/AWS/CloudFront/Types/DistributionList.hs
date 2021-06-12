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
-- Module      : Network.AWS.CloudFront.Types.DistributionList
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.DistributionList where

import Network.AWS.CloudFront.Types.DistributionSummary
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | A distribution list.
--
-- /See:/ 'newDistributionList' smart constructor.
data DistributionList = DistributionList'
  { -- | A complex type that contains one @DistributionSummary@ element for each
    -- distribution that was created by the current AWS account.
    items :: Core.Maybe [DistributionSummary],
    -- | If @IsTruncated@ is @true@, this element is present and contains the
    -- value you can use for the @Marker@ request parameter to continue listing
    -- your distributions where they left off.
    nextMarker :: Core.Maybe Core.Text,
    -- | The value you provided for the @Marker@ request parameter.
    marker :: Core.Text,
    -- | The value you provided for the @MaxItems@ request parameter.
    maxItems :: Core.Int,
    -- | A flag that indicates whether more distributions remain to be listed. If
    -- your results were truncated, you can make a follow-up pagination request
    -- using the @Marker@ request parameter to retrieve more distributions in
    -- the list.
    isTruncated :: Core.Bool,
    -- | The number of distributions that were created by the current AWS
    -- account.
    quantity :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DistributionList' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'items', 'distributionList_items' - A complex type that contains one @DistributionSummary@ element for each
-- distribution that was created by the current AWS account.
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
-- 'quantity', 'distributionList_quantity' - The number of distributions that were created by the current AWS
-- account.
newDistributionList ::
  -- | 'marker'
  Core.Text ->
  -- | 'maxItems'
  Core.Int ->
  -- | 'isTruncated'
  Core.Bool ->
  -- | 'quantity'
  Core.Int ->
  DistributionList
newDistributionList
  pMarker_
  pMaxItems_
  pIsTruncated_
  pQuantity_ =
    DistributionList'
      { items = Core.Nothing,
        nextMarker = Core.Nothing,
        marker = pMarker_,
        maxItems = pMaxItems_,
        isTruncated = pIsTruncated_,
        quantity = pQuantity_
      }

-- | A complex type that contains one @DistributionSummary@ element for each
-- distribution that was created by the current AWS account.
distributionList_items :: Lens.Lens' DistributionList (Core.Maybe [DistributionSummary])
distributionList_items = Lens.lens (\DistributionList' {items} -> items) (\s@DistributionList' {} a -> s {items = a} :: DistributionList) Core.. Lens.mapping Lens._Coerce

-- | If @IsTruncated@ is @true@, this element is present and contains the
-- value you can use for the @Marker@ request parameter to continue listing
-- your distributions where they left off.
distributionList_nextMarker :: Lens.Lens' DistributionList (Core.Maybe Core.Text)
distributionList_nextMarker = Lens.lens (\DistributionList' {nextMarker} -> nextMarker) (\s@DistributionList' {} a -> s {nextMarker = a} :: DistributionList)

-- | The value you provided for the @Marker@ request parameter.
distributionList_marker :: Lens.Lens' DistributionList Core.Text
distributionList_marker = Lens.lens (\DistributionList' {marker} -> marker) (\s@DistributionList' {} a -> s {marker = a} :: DistributionList)

-- | The value you provided for the @MaxItems@ request parameter.
distributionList_maxItems :: Lens.Lens' DistributionList Core.Int
distributionList_maxItems = Lens.lens (\DistributionList' {maxItems} -> maxItems) (\s@DistributionList' {} a -> s {maxItems = a} :: DistributionList)

-- | A flag that indicates whether more distributions remain to be listed. If
-- your results were truncated, you can make a follow-up pagination request
-- using the @Marker@ request parameter to retrieve more distributions in
-- the list.
distributionList_isTruncated :: Lens.Lens' DistributionList Core.Bool
distributionList_isTruncated = Lens.lens (\DistributionList' {isTruncated} -> isTruncated) (\s@DistributionList' {} a -> s {isTruncated = a} :: DistributionList)

-- | The number of distributions that were created by the current AWS
-- account.
distributionList_quantity :: Lens.Lens' DistributionList Core.Int
distributionList_quantity = Lens.lens (\DistributionList' {quantity} -> quantity) (\s@DistributionList' {} a -> s {quantity = a} :: DistributionList)

instance Core.FromXML DistributionList where
  parseXML x =
    DistributionList'
      Core.<$> ( x Core..@? "Items" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "DistributionSummary")
               )
      Core.<*> (x Core..@? "NextMarker")
      Core.<*> (x Core..@ "Marker")
      Core.<*> (x Core..@ "MaxItems")
      Core.<*> (x Core..@ "IsTruncated")
      Core.<*> (x Core..@ "Quantity")

instance Core.Hashable DistributionList

instance Core.NFData DistributionList
