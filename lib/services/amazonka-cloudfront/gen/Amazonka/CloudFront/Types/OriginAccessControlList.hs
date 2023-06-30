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
-- Module      : Amazonka.CloudFront.Types.OriginAccessControlList
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudFront.Types.OriginAccessControlList where

import Amazonka.CloudFront.Types.OriginAccessControlSummary
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A list of CloudFront origin access controls.
--
-- /See:/ 'newOriginAccessControlList' smart constructor.
data OriginAccessControlList = OriginAccessControlList'
  { -- | Contains the origin access controls in the list.
    items :: Prelude.Maybe [OriginAccessControlSummary],
    -- | If there are more items in the list than are in this response, this
    -- element is present. It contains the value to use in the @Marker@ field
    -- of another request to continue listing origin access controls.
    nextMarker :: Prelude.Maybe Prelude.Text,
    -- | The value of the @Marker@ field that was provided in the request.
    marker :: Prelude.Text,
    -- | The maximum number of origin access controls requested.
    maxItems :: Prelude.Int,
    -- | If there are more items in the list than are in this response, this
    -- value is @true@.
    isTruncated :: Prelude.Bool,
    -- | The number of origin access controls returned in the response.
    quantity :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OriginAccessControlList' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'items', 'originAccessControlList_items' - Contains the origin access controls in the list.
--
-- 'nextMarker', 'originAccessControlList_nextMarker' - If there are more items in the list than are in this response, this
-- element is present. It contains the value to use in the @Marker@ field
-- of another request to continue listing origin access controls.
--
-- 'marker', 'originAccessControlList_marker' - The value of the @Marker@ field that was provided in the request.
--
-- 'maxItems', 'originAccessControlList_maxItems' - The maximum number of origin access controls requested.
--
-- 'isTruncated', 'originAccessControlList_isTruncated' - If there are more items in the list than are in this response, this
-- value is @true@.
--
-- 'quantity', 'originAccessControlList_quantity' - The number of origin access controls returned in the response.
newOriginAccessControlList ::
  -- | 'marker'
  Prelude.Text ->
  -- | 'maxItems'
  Prelude.Int ->
  -- | 'isTruncated'
  Prelude.Bool ->
  -- | 'quantity'
  Prelude.Int ->
  OriginAccessControlList
newOriginAccessControlList
  pMarker_
  pMaxItems_
  pIsTruncated_
  pQuantity_ =
    OriginAccessControlList'
      { items = Prelude.Nothing,
        nextMarker = Prelude.Nothing,
        marker = pMarker_,
        maxItems = pMaxItems_,
        isTruncated = pIsTruncated_,
        quantity = pQuantity_
      }

-- | Contains the origin access controls in the list.
originAccessControlList_items :: Lens.Lens' OriginAccessControlList (Prelude.Maybe [OriginAccessControlSummary])
originAccessControlList_items = Lens.lens (\OriginAccessControlList' {items} -> items) (\s@OriginAccessControlList' {} a -> s {items = a} :: OriginAccessControlList) Prelude.. Lens.mapping Lens.coerced

-- | If there are more items in the list than are in this response, this
-- element is present. It contains the value to use in the @Marker@ field
-- of another request to continue listing origin access controls.
originAccessControlList_nextMarker :: Lens.Lens' OriginAccessControlList (Prelude.Maybe Prelude.Text)
originAccessControlList_nextMarker = Lens.lens (\OriginAccessControlList' {nextMarker} -> nextMarker) (\s@OriginAccessControlList' {} a -> s {nextMarker = a} :: OriginAccessControlList)

-- | The value of the @Marker@ field that was provided in the request.
originAccessControlList_marker :: Lens.Lens' OriginAccessControlList Prelude.Text
originAccessControlList_marker = Lens.lens (\OriginAccessControlList' {marker} -> marker) (\s@OriginAccessControlList' {} a -> s {marker = a} :: OriginAccessControlList)

-- | The maximum number of origin access controls requested.
originAccessControlList_maxItems :: Lens.Lens' OriginAccessControlList Prelude.Int
originAccessControlList_maxItems = Lens.lens (\OriginAccessControlList' {maxItems} -> maxItems) (\s@OriginAccessControlList' {} a -> s {maxItems = a} :: OriginAccessControlList)

-- | If there are more items in the list than are in this response, this
-- value is @true@.
originAccessControlList_isTruncated :: Lens.Lens' OriginAccessControlList Prelude.Bool
originAccessControlList_isTruncated = Lens.lens (\OriginAccessControlList' {isTruncated} -> isTruncated) (\s@OriginAccessControlList' {} a -> s {isTruncated = a} :: OriginAccessControlList)

-- | The number of origin access controls returned in the response.
originAccessControlList_quantity :: Lens.Lens' OriginAccessControlList Prelude.Int
originAccessControlList_quantity = Lens.lens (\OriginAccessControlList' {quantity} -> quantity) (\s@OriginAccessControlList' {} a -> s {quantity = a} :: OriginAccessControlList)

instance Data.FromXML OriginAccessControlList where
  parseXML x =
    OriginAccessControlList'
      Prelude.<$> ( x
                      Data..@? "Items"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may
                        (Data.parseXMLList "OriginAccessControlSummary")
                  )
      Prelude.<*> (x Data..@? "NextMarker")
      Prelude.<*> (x Data..@ "Marker")
      Prelude.<*> (x Data..@ "MaxItems")
      Prelude.<*> (x Data..@ "IsTruncated")
      Prelude.<*> (x Data..@ "Quantity")

instance Prelude.Hashable OriginAccessControlList where
  hashWithSalt _salt OriginAccessControlList' {..} =
    _salt
      `Prelude.hashWithSalt` items
      `Prelude.hashWithSalt` nextMarker
      `Prelude.hashWithSalt` marker
      `Prelude.hashWithSalt` maxItems
      `Prelude.hashWithSalt` isTruncated
      `Prelude.hashWithSalt` quantity

instance Prelude.NFData OriginAccessControlList where
  rnf OriginAccessControlList' {..} =
    Prelude.rnf items
      `Prelude.seq` Prelude.rnf nextMarker
      `Prelude.seq` Prelude.rnf marker
      `Prelude.seq` Prelude.rnf maxItems
      `Prelude.seq` Prelude.rnf isTruncated
      `Prelude.seq` Prelude.rnf quantity
