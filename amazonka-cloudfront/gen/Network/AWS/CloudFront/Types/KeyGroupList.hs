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
-- Module      : Network.AWS.CloudFront.Types.KeyGroupList
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.KeyGroupList where

import Network.AWS.CloudFront.Types.KeyGroupSummary
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A list of key groups.
--
-- /See:/ 'newKeyGroupList' smart constructor.
data KeyGroupList = KeyGroupList'
  { -- | A list of key groups.
    items :: Prelude.Maybe [KeyGroupSummary],
    -- | If there are more items in the list than are in this response, this
    -- element is present. It contains the value that you should use in the
    -- @Marker@ field of a subsequent request to continue listing key groups.
    nextMarker :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of key groups requested.
    maxItems :: Prelude.Int,
    -- | The number of key groups returned in the response.
    quantity :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'KeyGroupList' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'items', 'keyGroupList_items' - A list of key groups.
--
-- 'nextMarker', 'keyGroupList_nextMarker' - If there are more items in the list than are in this response, this
-- element is present. It contains the value that you should use in the
-- @Marker@ field of a subsequent request to continue listing key groups.
--
-- 'maxItems', 'keyGroupList_maxItems' - The maximum number of key groups requested.
--
-- 'quantity', 'keyGroupList_quantity' - The number of key groups returned in the response.
newKeyGroupList ::
  -- | 'maxItems'
  Prelude.Int ->
  -- | 'quantity'
  Prelude.Int ->
  KeyGroupList
newKeyGroupList pMaxItems_ pQuantity_ =
  KeyGroupList'
    { items = Prelude.Nothing,
      nextMarker = Prelude.Nothing,
      maxItems = pMaxItems_,
      quantity = pQuantity_
    }

-- | A list of key groups.
keyGroupList_items :: Lens.Lens' KeyGroupList (Prelude.Maybe [KeyGroupSummary])
keyGroupList_items = Lens.lens (\KeyGroupList' {items} -> items) (\s@KeyGroupList' {} a -> s {items = a} :: KeyGroupList) Prelude.. Lens.mapping Prelude._Coerce

-- | If there are more items in the list than are in this response, this
-- element is present. It contains the value that you should use in the
-- @Marker@ field of a subsequent request to continue listing key groups.
keyGroupList_nextMarker :: Lens.Lens' KeyGroupList (Prelude.Maybe Prelude.Text)
keyGroupList_nextMarker = Lens.lens (\KeyGroupList' {nextMarker} -> nextMarker) (\s@KeyGroupList' {} a -> s {nextMarker = a} :: KeyGroupList)

-- | The maximum number of key groups requested.
keyGroupList_maxItems :: Lens.Lens' KeyGroupList Prelude.Int
keyGroupList_maxItems = Lens.lens (\KeyGroupList' {maxItems} -> maxItems) (\s@KeyGroupList' {} a -> s {maxItems = a} :: KeyGroupList)

-- | The number of key groups returned in the response.
keyGroupList_quantity :: Lens.Lens' KeyGroupList Prelude.Int
keyGroupList_quantity = Lens.lens (\KeyGroupList' {quantity} -> quantity) (\s@KeyGroupList' {} a -> s {quantity = a} :: KeyGroupList)

instance Prelude.FromXML KeyGroupList where
  parseXML x =
    KeyGroupList'
      Prelude.<$> ( x Prelude..@? "Items" Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "KeyGroupSummary")
                  )
      Prelude.<*> (x Prelude..@? "NextMarker")
      Prelude.<*> (x Prelude..@ "MaxItems")
      Prelude.<*> (x Prelude..@ "Quantity")

instance Prelude.Hashable KeyGroupList

instance Prelude.NFData KeyGroupList
