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
-- Module      : Network.AWS.CloudFront.Types.DistributionIdList
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.DistributionIdList where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | A list of distribution IDs.
--
-- /See:/ 'newDistributionIdList' smart constructor.
data DistributionIdList = DistributionIdList'
  { -- | Contains the distribution IDs in the list.
    items :: Core.Maybe [Core.Text],
    -- | Contains the value that you should use in the @Marker@ field of a
    -- subsequent request to continue listing distribution IDs where you left
    -- off.
    nextMarker :: Core.Maybe Core.Text,
    -- | The value provided in the @Marker@ request field.
    marker :: Core.Text,
    -- | The maximum number of distribution IDs requested.
    maxItems :: Core.Int,
    -- | A flag that indicates whether more distribution IDs remain to be listed.
    -- If your results were truncated, you can make a subsequent request using
    -- the @Marker@ request field to retrieve more distribution IDs in the
    -- list.
    isTruncated :: Core.Bool,
    -- | The total number of distribution IDs returned in the response.
    quantity :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  -- | 'maxItems'
  Core.Int ->
  -- | 'isTruncated'
  Core.Bool ->
  -- | 'quantity'
  Core.Int ->
  DistributionIdList
newDistributionIdList
  pMarker_
  pMaxItems_
  pIsTruncated_
  pQuantity_ =
    DistributionIdList'
      { items = Core.Nothing,
        nextMarker = Core.Nothing,
        marker = pMarker_,
        maxItems = pMaxItems_,
        isTruncated = pIsTruncated_,
        quantity = pQuantity_
      }

-- | Contains the distribution IDs in the list.
distributionIdList_items :: Lens.Lens' DistributionIdList (Core.Maybe [Core.Text])
distributionIdList_items = Lens.lens (\DistributionIdList' {items} -> items) (\s@DistributionIdList' {} a -> s {items = a} :: DistributionIdList) Core.. Lens.mapping Lens._Coerce

-- | Contains the value that you should use in the @Marker@ field of a
-- subsequent request to continue listing distribution IDs where you left
-- off.
distributionIdList_nextMarker :: Lens.Lens' DistributionIdList (Core.Maybe Core.Text)
distributionIdList_nextMarker = Lens.lens (\DistributionIdList' {nextMarker} -> nextMarker) (\s@DistributionIdList' {} a -> s {nextMarker = a} :: DistributionIdList)

-- | The value provided in the @Marker@ request field.
distributionIdList_marker :: Lens.Lens' DistributionIdList Core.Text
distributionIdList_marker = Lens.lens (\DistributionIdList' {marker} -> marker) (\s@DistributionIdList' {} a -> s {marker = a} :: DistributionIdList)

-- | The maximum number of distribution IDs requested.
distributionIdList_maxItems :: Lens.Lens' DistributionIdList Core.Int
distributionIdList_maxItems = Lens.lens (\DistributionIdList' {maxItems} -> maxItems) (\s@DistributionIdList' {} a -> s {maxItems = a} :: DistributionIdList)

-- | A flag that indicates whether more distribution IDs remain to be listed.
-- If your results were truncated, you can make a subsequent request using
-- the @Marker@ request field to retrieve more distribution IDs in the
-- list.
distributionIdList_isTruncated :: Lens.Lens' DistributionIdList Core.Bool
distributionIdList_isTruncated = Lens.lens (\DistributionIdList' {isTruncated} -> isTruncated) (\s@DistributionIdList' {} a -> s {isTruncated = a} :: DistributionIdList)

-- | The total number of distribution IDs returned in the response.
distributionIdList_quantity :: Lens.Lens' DistributionIdList Core.Int
distributionIdList_quantity = Lens.lens (\DistributionIdList' {quantity} -> quantity) (\s@DistributionIdList' {} a -> s {quantity = a} :: DistributionIdList)

instance Core.FromXML DistributionIdList where
  parseXML x =
    DistributionIdList'
      Core.<$> ( x Core..@? "Items" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "DistributionId")
               )
      Core.<*> (x Core..@? "NextMarker")
      Core.<*> (x Core..@ "Marker")
      Core.<*> (x Core..@ "MaxItems")
      Core.<*> (x Core..@ "IsTruncated")
      Core.<*> (x Core..@ "Quantity")

instance Core.Hashable DistributionIdList

instance Core.NFData DistributionIdList
