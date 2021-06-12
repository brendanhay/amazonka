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
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | A list of cache policies.
--
-- /See:/ 'newCachePolicyList' smart constructor.
data CachePolicyList = CachePolicyList'
  { -- | Contains the cache policies in the list.
    items :: Core.Maybe [CachePolicySummary],
    -- | If there are more items in the list than are in this response, this
    -- element is present. It contains the value that you should use in the
    -- @Marker@ field of a subsequent request to continue listing cache
    -- policies where you left off.
    nextMarker :: Core.Maybe Core.Text,
    -- | The maximum number of cache policies requested.
    maxItems :: Core.Int,
    -- | The total number of cache policies returned in the response.
    quantity :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  -- | 'quantity'
  Core.Int ->
  CachePolicyList
newCachePolicyList pMaxItems_ pQuantity_ =
  CachePolicyList'
    { items = Core.Nothing,
      nextMarker = Core.Nothing,
      maxItems = pMaxItems_,
      quantity = pQuantity_
    }

-- | Contains the cache policies in the list.
cachePolicyList_items :: Lens.Lens' CachePolicyList (Core.Maybe [CachePolicySummary])
cachePolicyList_items = Lens.lens (\CachePolicyList' {items} -> items) (\s@CachePolicyList' {} a -> s {items = a} :: CachePolicyList) Core.. Lens.mapping Lens._Coerce

-- | If there are more items in the list than are in this response, this
-- element is present. It contains the value that you should use in the
-- @Marker@ field of a subsequent request to continue listing cache
-- policies where you left off.
cachePolicyList_nextMarker :: Lens.Lens' CachePolicyList (Core.Maybe Core.Text)
cachePolicyList_nextMarker = Lens.lens (\CachePolicyList' {nextMarker} -> nextMarker) (\s@CachePolicyList' {} a -> s {nextMarker = a} :: CachePolicyList)

-- | The maximum number of cache policies requested.
cachePolicyList_maxItems :: Lens.Lens' CachePolicyList Core.Int
cachePolicyList_maxItems = Lens.lens (\CachePolicyList' {maxItems} -> maxItems) (\s@CachePolicyList' {} a -> s {maxItems = a} :: CachePolicyList)

-- | The total number of cache policies returned in the response.
cachePolicyList_quantity :: Lens.Lens' CachePolicyList Core.Int
cachePolicyList_quantity = Lens.lens (\CachePolicyList' {quantity} -> quantity) (\s@CachePolicyList' {} a -> s {quantity = a} :: CachePolicyList)

instance Core.FromXML CachePolicyList where
  parseXML x =
    CachePolicyList'
      Core.<$> ( x Core..@? "Items" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "CachePolicySummary")
               )
      Core.<*> (x Core..@? "NextMarker")
      Core.<*> (x Core..@ "MaxItems")
      Core.<*> (x Core..@ "Quantity")

instance Core.Hashable CachePolicyList

instance Core.NFData CachePolicyList
