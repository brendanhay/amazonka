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
-- Module      : Amazonka.CloudFront.Types.CachePolicyList
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudFront.Types.CachePolicyList where

import Amazonka.CloudFront.Types.CachePolicySummary
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A list of cache policies.
--
-- /See:/ 'newCachePolicyList' smart constructor.
data CachePolicyList = CachePolicyList'
  { -- | Contains the cache policies in the list.
    items :: Prelude.Maybe [CachePolicySummary],
    -- | If there are more items in the list than are in this response, this
    -- element is present. It contains the value that you should use in the
    -- @Marker@ field of a subsequent request to continue listing cache
    -- policies where you left off.
    nextMarker :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of cache policies requested.
    maxItems :: Prelude.Int,
    -- | The total number of cache policies returned in the response.
    quantity :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CachePolicyList' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'items', 'cachePolicyList_items' - Contains the cache policies in the list.
--
-- 'nextMarker', 'cachePolicyList_nextMarker' - If there are more items in the list than are in this response, this
-- element is present. It contains the value that you should use in the
-- @Marker@ field of a subsequent request to continue listing cache
-- policies where you left off.
--
-- 'maxItems', 'cachePolicyList_maxItems' - The maximum number of cache policies requested.
--
-- 'quantity', 'cachePolicyList_quantity' - The total number of cache policies returned in the response.
newCachePolicyList ::
  -- | 'maxItems'
  Prelude.Int ->
  -- | 'quantity'
  Prelude.Int ->
  CachePolicyList
newCachePolicyList pMaxItems_ pQuantity_ =
  CachePolicyList'
    { items = Prelude.Nothing,
      nextMarker = Prelude.Nothing,
      maxItems = pMaxItems_,
      quantity = pQuantity_
    }

-- | Contains the cache policies in the list.
cachePolicyList_items :: Lens.Lens' CachePolicyList (Prelude.Maybe [CachePolicySummary])
cachePolicyList_items = Lens.lens (\CachePolicyList' {items} -> items) (\s@CachePolicyList' {} a -> s {items = a} :: CachePolicyList) Prelude.. Lens.mapping Lens.coerced

-- | If there are more items in the list than are in this response, this
-- element is present. It contains the value that you should use in the
-- @Marker@ field of a subsequent request to continue listing cache
-- policies where you left off.
cachePolicyList_nextMarker :: Lens.Lens' CachePolicyList (Prelude.Maybe Prelude.Text)
cachePolicyList_nextMarker = Lens.lens (\CachePolicyList' {nextMarker} -> nextMarker) (\s@CachePolicyList' {} a -> s {nextMarker = a} :: CachePolicyList)

-- | The maximum number of cache policies requested.
cachePolicyList_maxItems :: Lens.Lens' CachePolicyList Prelude.Int
cachePolicyList_maxItems = Lens.lens (\CachePolicyList' {maxItems} -> maxItems) (\s@CachePolicyList' {} a -> s {maxItems = a} :: CachePolicyList)

-- | The total number of cache policies returned in the response.
cachePolicyList_quantity :: Lens.Lens' CachePolicyList Prelude.Int
cachePolicyList_quantity = Lens.lens (\CachePolicyList' {quantity} -> quantity) (\s@CachePolicyList' {} a -> s {quantity = a} :: CachePolicyList)

instance Data.FromXML CachePolicyList where
  parseXML x =
    CachePolicyList'
      Prelude.<$> ( x Data..@? "Items" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "CachePolicySummary")
                  )
      Prelude.<*> (x Data..@? "NextMarker")
      Prelude.<*> (x Data..@ "MaxItems")
      Prelude.<*> (x Data..@ "Quantity")

instance Prelude.Hashable CachePolicyList where
  hashWithSalt _salt CachePolicyList' {..} =
    _salt `Prelude.hashWithSalt` items
      `Prelude.hashWithSalt` nextMarker
      `Prelude.hashWithSalt` maxItems
      `Prelude.hashWithSalt` quantity

instance Prelude.NFData CachePolicyList where
  rnf CachePolicyList' {..} =
    Prelude.rnf items
      `Prelude.seq` Prelude.rnf nextMarker
      `Prelude.seq` Prelude.rnf maxItems
      `Prelude.seq` Prelude.rnf quantity
