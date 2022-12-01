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
-- Module      : Amazonka.CloudFront.Types.OriginRequestPolicyList
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudFront.Types.OriginRequestPolicyList where

import Amazonka.CloudFront.Types.OriginRequestPolicySummary
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | A list of origin request policies.
--
-- /See:/ 'newOriginRequestPolicyList' smart constructor.
data OriginRequestPolicyList = OriginRequestPolicyList'
  { -- | Contains the origin request policies in the list.
    items :: Prelude.Maybe [OriginRequestPolicySummary],
    -- | If there are more items in the list than are in this response, this
    -- element is present. It contains the value that you should use in the
    -- @Marker@ field of a subsequent request to continue listing origin
    -- request policies where you left off.
    nextMarker :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of origin request policies requested.
    maxItems :: Prelude.Int,
    -- | The total number of origin request policies returned in the response.
    quantity :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  -- | 'quantity'
  Prelude.Int ->
  OriginRequestPolicyList
newOriginRequestPolicyList pMaxItems_ pQuantity_ =
  OriginRequestPolicyList'
    { items = Prelude.Nothing,
      nextMarker = Prelude.Nothing,
      maxItems = pMaxItems_,
      quantity = pQuantity_
    }

-- | Contains the origin request policies in the list.
originRequestPolicyList_items :: Lens.Lens' OriginRequestPolicyList (Prelude.Maybe [OriginRequestPolicySummary])
originRequestPolicyList_items = Lens.lens (\OriginRequestPolicyList' {items} -> items) (\s@OriginRequestPolicyList' {} a -> s {items = a} :: OriginRequestPolicyList) Prelude.. Lens.mapping Lens.coerced

-- | If there are more items in the list than are in this response, this
-- element is present. It contains the value that you should use in the
-- @Marker@ field of a subsequent request to continue listing origin
-- request policies where you left off.
originRequestPolicyList_nextMarker :: Lens.Lens' OriginRequestPolicyList (Prelude.Maybe Prelude.Text)
originRequestPolicyList_nextMarker = Lens.lens (\OriginRequestPolicyList' {nextMarker} -> nextMarker) (\s@OriginRequestPolicyList' {} a -> s {nextMarker = a} :: OriginRequestPolicyList)

-- | The maximum number of origin request policies requested.
originRequestPolicyList_maxItems :: Lens.Lens' OriginRequestPolicyList Prelude.Int
originRequestPolicyList_maxItems = Lens.lens (\OriginRequestPolicyList' {maxItems} -> maxItems) (\s@OriginRequestPolicyList' {} a -> s {maxItems = a} :: OriginRequestPolicyList)

-- | The total number of origin request policies returned in the response.
originRequestPolicyList_quantity :: Lens.Lens' OriginRequestPolicyList Prelude.Int
originRequestPolicyList_quantity = Lens.lens (\OriginRequestPolicyList' {quantity} -> quantity) (\s@OriginRequestPolicyList' {} a -> s {quantity = a} :: OriginRequestPolicyList)

instance Core.FromXML OriginRequestPolicyList where
  parseXML x =
    OriginRequestPolicyList'
      Prelude.<$> ( x Core..@? "Items" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may
                        (Core.parseXMLList "OriginRequestPolicySummary")
                  )
      Prelude.<*> (x Core..@? "NextMarker")
      Prelude.<*> (x Core..@ "MaxItems")
      Prelude.<*> (x Core..@ "Quantity")

instance Prelude.Hashable OriginRequestPolicyList where
  hashWithSalt _salt OriginRequestPolicyList' {..} =
    _salt `Prelude.hashWithSalt` items
      `Prelude.hashWithSalt` nextMarker
      `Prelude.hashWithSalt` maxItems
      `Prelude.hashWithSalt` quantity

instance Prelude.NFData OriginRequestPolicyList where
  rnf OriginRequestPolicyList' {..} =
    Prelude.rnf items
      `Prelude.seq` Prelude.rnf nextMarker
      `Prelude.seq` Prelude.rnf maxItems
      `Prelude.seq` Prelude.rnf quantity
