{-# LANGUAGE DeriveDataTypeable #-}
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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The @InvalidationList@ complex type describes the list of invalidation
-- objects. For more information about invalidation, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/Invalidation.html Invalidating Objects (Web Distributions Only)>
-- in the /Amazon CloudFront Developer Guide/.
--
-- /See:/ 'newInvalidationList' smart constructor.
data InvalidationList = InvalidationList'
  { -- | A complex type that contains one @InvalidationSummary@ element for each
    -- invalidation batch created by the current AWS account.
    items :: Prelude.Maybe [InvalidationSummary],
    -- | If @IsTruncated@ is @true@, this element is present and contains the
    -- value that you can use for the @Marker@ request parameter to continue
    -- listing your invalidation batches where they left off.
    nextMarker :: Prelude.Maybe Prelude.Text,
    -- | The value that you provided for the @Marker@ request parameter.
    marker :: Prelude.Text,
    -- | The value that you provided for the @MaxItems@ request parameter.
    maxItems :: Prelude.Int,
    -- | A flag that indicates whether more invalidation batch requests remain to
    -- be listed. If your results were truncated, you can make a follow-up
    -- pagination request using the @Marker@ request parameter to retrieve more
    -- invalidation batches in the list.
    isTruncated :: Prelude.Bool,
    -- | The number of invalidation batches that were created by the current AWS
    -- account.
    quantity :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'maxItems'
  Prelude.Int ->
  -- | 'isTruncated'
  Prelude.Bool ->
  -- | 'quantity'
  Prelude.Int ->
  InvalidationList
newInvalidationList
  pMarker_
  pMaxItems_
  pIsTruncated_
  pQuantity_ =
    InvalidationList'
      { items = Prelude.Nothing,
        nextMarker = Prelude.Nothing,
        marker = pMarker_,
        maxItems = pMaxItems_,
        isTruncated = pIsTruncated_,
        quantity = pQuantity_
      }

-- | A complex type that contains one @InvalidationSummary@ element for each
-- invalidation batch created by the current AWS account.
invalidationList_items :: Lens.Lens' InvalidationList (Prelude.Maybe [InvalidationSummary])
invalidationList_items = Lens.lens (\InvalidationList' {items} -> items) (\s@InvalidationList' {} a -> s {items = a} :: InvalidationList) Prelude.. Lens.mapping Prelude._Coerce

-- | If @IsTruncated@ is @true@, this element is present and contains the
-- value that you can use for the @Marker@ request parameter to continue
-- listing your invalidation batches where they left off.
invalidationList_nextMarker :: Lens.Lens' InvalidationList (Prelude.Maybe Prelude.Text)
invalidationList_nextMarker = Lens.lens (\InvalidationList' {nextMarker} -> nextMarker) (\s@InvalidationList' {} a -> s {nextMarker = a} :: InvalidationList)

-- | The value that you provided for the @Marker@ request parameter.
invalidationList_marker :: Lens.Lens' InvalidationList Prelude.Text
invalidationList_marker = Lens.lens (\InvalidationList' {marker} -> marker) (\s@InvalidationList' {} a -> s {marker = a} :: InvalidationList)

-- | The value that you provided for the @MaxItems@ request parameter.
invalidationList_maxItems :: Lens.Lens' InvalidationList Prelude.Int
invalidationList_maxItems = Lens.lens (\InvalidationList' {maxItems} -> maxItems) (\s@InvalidationList' {} a -> s {maxItems = a} :: InvalidationList)

-- | A flag that indicates whether more invalidation batch requests remain to
-- be listed. If your results were truncated, you can make a follow-up
-- pagination request using the @Marker@ request parameter to retrieve more
-- invalidation batches in the list.
invalidationList_isTruncated :: Lens.Lens' InvalidationList Prelude.Bool
invalidationList_isTruncated = Lens.lens (\InvalidationList' {isTruncated} -> isTruncated) (\s@InvalidationList' {} a -> s {isTruncated = a} :: InvalidationList)

-- | The number of invalidation batches that were created by the current AWS
-- account.
invalidationList_quantity :: Lens.Lens' InvalidationList Prelude.Int
invalidationList_quantity = Lens.lens (\InvalidationList' {quantity} -> quantity) (\s@InvalidationList' {} a -> s {quantity = a} :: InvalidationList)

instance Prelude.FromXML InvalidationList where
  parseXML x =
    InvalidationList'
      Prelude.<$> ( x Prelude..@? "Items" Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may
                        (Prelude.parseXMLList "InvalidationSummary")
                  )
      Prelude.<*> (x Prelude..@? "NextMarker")
      Prelude.<*> (x Prelude..@ "Marker")
      Prelude.<*> (x Prelude..@ "MaxItems")
      Prelude.<*> (x Prelude..@ "IsTruncated")
      Prelude.<*> (x Prelude..@ "Quantity")

instance Prelude.Hashable InvalidationList

instance Prelude.NFData InvalidationList
