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
-- Module      : Network.AWS.CloudFront.Types.InvalidationList
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.InvalidationList where

import Network.AWS.CloudFront.Types.InvalidationSummary
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | The @InvalidationList@ complex type describes the list of invalidation
-- objects. For more information about invalidation, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/Invalidation.html Invalidating Objects (Web Distributions Only)>
-- in the /Amazon CloudFront Developer Guide/.
--
-- /See:/ 'newInvalidationList' smart constructor.
data InvalidationList = InvalidationList'
  { -- | A complex type that contains one @InvalidationSummary@ element for each
    -- invalidation batch created by the current AWS account.
    items :: Core.Maybe [InvalidationSummary],
    -- | If @IsTruncated@ is @true@, this element is present and contains the
    -- value that you can use for the @Marker@ request parameter to continue
    -- listing your invalidation batches where they left off.
    nextMarker :: Core.Maybe Core.Text,
    -- | The value that you provided for the @Marker@ request parameter.
    marker :: Core.Text,
    -- | The value that you provided for the @MaxItems@ request parameter.
    maxItems :: Core.Int,
    -- | A flag that indicates whether more invalidation batch requests remain to
    -- be listed. If your results were truncated, you can make a follow-up
    -- pagination request using the @Marker@ request parameter to retrieve more
    -- invalidation batches in the list.
    isTruncated :: Core.Bool,
    -- | The number of invalidation batches that were created by the current AWS
    -- account.
    quantity :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'InvalidationList' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'items', 'invalidationList_items' - A complex type that contains one @InvalidationSummary@ element for each
-- invalidation batch created by the current AWS account.
--
-- 'nextMarker', 'invalidationList_nextMarker' - If @IsTruncated@ is @true@, this element is present and contains the
-- value that you can use for the @Marker@ request parameter to continue
-- listing your invalidation batches where they left off.
--
-- 'marker', 'invalidationList_marker' - The value that you provided for the @Marker@ request parameter.
--
-- 'maxItems', 'invalidationList_maxItems' - The value that you provided for the @MaxItems@ request parameter.
--
-- 'isTruncated', 'invalidationList_isTruncated' - A flag that indicates whether more invalidation batch requests remain to
-- be listed. If your results were truncated, you can make a follow-up
-- pagination request using the @Marker@ request parameter to retrieve more
-- invalidation batches in the list.
--
-- 'quantity', 'invalidationList_quantity' - The number of invalidation batches that were created by the current AWS
-- account.
newInvalidationList ::
  -- | 'marker'
  Core.Text ->
  -- | 'maxItems'
  Core.Int ->
  -- | 'isTruncated'
  Core.Bool ->
  -- | 'quantity'
  Core.Int ->
  InvalidationList
newInvalidationList
  pMarker_
  pMaxItems_
  pIsTruncated_
  pQuantity_ =
    InvalidationList'
      { items = Core.Nothing,
        nextMarker = Core.Nothing,
        marker = pMarker_,
        maxItems = pMaxItems_,
        isTruncated = pIsTruncated_,
        quantity = pQuantity_
      }

-- | A complex type that contains one @InvalidationSummary@ element for each
-- invalidation batch created by the current AWS account.
invalidationList_items :: Lens.Lens' InvalidationList (Core.Maybe [InvalidationSummary])
invalidationList_items = Lens.lens (\InvalidationList' {items} -> items) (\s@InvalidationList' {} a -> s {items = a} :: InvalidationList) Core.. Lens.mapping Lens._Coerce

-- | If @IsTruncated@ is @true@, this element is present and contains the
-- value that you can use for the @Marker@ request parameter to continue
-- listing your invalidation batches where they left off.
invalidationList_nextMarker :: Lens.Lens' InvalidationList (Core.Maybe Core.Text)
invalidationList_nextMarker = Lens.lens (\InvalidationList' {nextMarker} -> nextMarker) (\s@InvalidationList' {} a -> s {nextMarker = a} :: InvalidationList)

-- | The value that you provided for the @Marker@ request parameter.
invalidationList_marker :: Lens.Lens' InvalidationList Core.Text
invalidationList_marker = Lens.lens (\InvalidationList' {marker} -> marker) (\s@InvalidationList' {} a -> s {marker = a} :: InvalidationList)

-- | The value that you provided for the @MaxItems@ request parameter.
invalidationList_maxItems :: Lens.Lens' InvalidationList Core.Int
invalidationList_maxItems = Lens.lens (\InvalidationList' {maxItems} -> maxItems) (\s@InvalidationList' {} a -> s {maxItems = a} :: InvalidationList)

-- | A flag that indicates whether more invalidation batch requests remain to
-- be listed. If your results were truncated, you can make a follow-up
-- pagination request using the @Marker@ request parameter to retrieve more
-- invalidation batches in the list.
invalidationList_isTruncated :: Lens.Lens' InvalidationList Core.Bool
invalidationList_isTruncated = Lens.lens (\InvalidationList' {isTruncated} -> isTruncated) (\s@InvalidationList' {} a -> s {isTruncated = a} :: InvalidationList)

-- | The number of invalidation batches that were created by the current AWS
-- account.
invalidationList_quantity :: Lens.Lens' InvalidationList Core.Int
invalidationList_quantity = Lens.lens (\InvalidationList' {quantity} -> quantity) (\s@InvalidationList' {} a -> s {quantity = a} :: InvalidationList)

instance Core.FromXML InvalidationList where
  parseXML x =
    InvalidationList'
      Core.<$> ( x Core..@? "Items" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "InvalidationSummary")
               )
      Core.<*> (x Core..@? "NextMarker")
      Core.<*> (x Core..@ "Marker")
      Core.<*> (x Core..@ "MaxItems")
      Core.<*> (x Core..@ "IsTruncated")
      Core.<*> (x Core..@ "Quantity")

instance Core.Hashable InvalidationList

instance Core.NFData InvalidationList
