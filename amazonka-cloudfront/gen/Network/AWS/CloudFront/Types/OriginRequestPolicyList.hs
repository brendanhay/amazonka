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
-- Module      : Network.AWS.CloudFront.Types.OriginRequestPolicyList
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.OriginRequestPolicyList where

import Network.AWS.CloudFront.Types.OriginRequestPolicySummary
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | A list of origin request policies.
--
-- /See:/ 'newOriginRequestPolicyList' smart constructor.
data OriginRequestPolicyList = OriginRequestPolicyList'
  { -- | Contains the origin request policies in the list.
    items :: Core.Maybe [OriginRequestPolicySummary],
    -- | If there are more items in the list than are in this response, this
    -- element is present. It contains the value that you should use in the
    -- @Marker@ field of a subsequent request to continue listing origin
    -- request policies where you left off.
    nextMarker :: Core.Maybe Core.Text,
    -- | The maximum number of origin request policies requested.
    maxItems :: Core.Int,
    -- | The total number of origin request policies returned in the response.
    quantity :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'OriginRequestPolicyList' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'items', 'originRequestPolicyList_items' - Contains the origin request policies in the list.
--
-- 'nextMarker', 'originRequestPolicyList_nextMarker' - If there are more items in the list than are in this response, this
-- element is present. It contains the value that you should use in the
-- @Marker@ field of a subsequent request to continue listing origin
-- request policies where you left off.
--
-- 'maxItems', 'originRequestPolicyList_maxItems' - The maximum number of origin request policies requested.
--
-- 'quantity', 'originRequestPolicyList_quantity' - The total number of origin request policies returned in the response.
newOriginRequestPolicyList ::
  -- | 'maxItems'
  Core.Int ->
  -- | 'quantity'
  Core.Int ->
  OriginRequestPolicyList
newOriginRequestPolicyList pMaxItems_ pQuantity_ =
  OriginRequestPolicyList'
    { items = Core.Nothing,
      nextMarker = Core.Nothing,
      maxItems = pMaxItems_,
      quantity = pQuantity_
    }

-- | Contains the origin request policies in the list.
originRequestPolicyList_items :: Lens.Lens' OriginRequestPolicyList (Core.Maybe [OriginRequestPolicySummary])
originRequestPolicyList_items = Lens.lens (\OriginRequestPolicyList' {items} -> items) (\s@OriginRequestPolicyList' {} a -> s {items = a} :: OriginRequestPolicyList) Core.. Lens.mapping Lens._Coerce

-- | If there are more items in the list than are in this response, this
-- element is present. It contains the value that you should use in the
-- @Marker@ field of a subsequent request to continue listing origin
-- request policies where you left off.
originRequestPolicyList_nextMarker :: Lens.Lens' OriginRequestPolicyList (Core.Maybe Core.Text)
originRequestPolicyList_nextMarker = Lens.lens (\OriginRequestPolicyList' {nextMarker} -> nextMarker) (\s@OriginRequestPolicyList' {} a -> s {nextMarker = a} :: OriginRequestPolicyList)

-- | The maximum number of origin request policies requested.
originRequestPolicyList_maxItems :: Lens.Lens' OriginRequestPolicyList Core.Int
originRequestPolicyList_maxItems = Lens.lens (\OriginRequestPolicyList' {maxItems} -> maxItems) (\s@OriginRequestPolicyList' {} a -> s {maxItems = a} :: OriginRequestPolicyList)

-- | The total number of origin request policies returned in the response.
originRequestPolicyList_quantity :: Lens.Lens' OriginRequestPolicyList Core.Int
originRequestPolicyList_quantity = Lens.lens (\OriginRequestPolicyList' {quantity} -> quantity) (\s@OriginRequestPolicyList' {} a -> s {quantity = a} :: OriginRequestPolicyList)

instance Core.FromXML OriginRequestPolicyList where
  parseXML x =
    OriginRequestPolicyList'
      Core.<$> ( x Core..@? "Items" Core..!@ Core.mempty
                   Core.>>= Core.may
                     (Core.parseXMLList "OriginRequestPolicySummary")
               )
      Core.<*> (x Core..@? "NextMarker")
      Core.<*> (x Core..@ "MaxItems")
      Core.<*> (x Core..@ "Quantity")

instance Core.Hashable OriginRequestPolicyList

instance Core.NFData OriginRequestPolicyList
