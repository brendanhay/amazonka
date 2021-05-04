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
-- Module      : Network.AWS.CloudFront.Types.CachePolicyList
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.CachePolicyList where

import Network.AWS.CloudFront.Types.CachePolicySummary
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
cachePolicyList_items = Lens.lens (\CachePolicyList' {items} -> items) (\s@CachePolicyList' {} a -> s {items = a} :: CachePolicyList) Prelude.. Lens.mapping Prelude._Coerce

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

instance Prelude.FromXML CachePolicyList where
  parseXML x =
    CachePolicyList'
      Prelude.<$> ( x Prelude..@? "Items" Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may
                        (Prelude.parseXMLList "CachePolicySummary")
                  )
      Prelude.<*> (x Prelude..@? "NextMarker")
      Prelude.<*> (x Prelude..@ "MaxItems")
      Prelude.<*> (x Prelude..@ "Quantity")

instance Prelude.Hashable CachePolicyList

instance Prelude.NFData CachePolicyList
