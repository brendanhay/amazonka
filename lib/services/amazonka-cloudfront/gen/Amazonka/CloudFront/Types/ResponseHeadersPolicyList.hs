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
-- Module      : Amazonka.CloudFront.Types.ResponseHeadersPolicyList
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudFront.Types.ResponseHeadersPolicyList where

import Amazonka.CloudFront.Types.ResponseHeadersPolicySummary
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A list of response headers policies.
--
-- /See:/ 'newResponseHeadersPolicyList' smart constructor.
data ResponseHeadersPolicyList = ResponseHeadersPolicyList'
  { -- | The response headers policies in the list.
    items :: Prelude.Maybe [ResponseHeadersPolicySummary],
    -- | If there are more items in the list than are in this response, this
    -- element is present. It contains the value that you should use in the
    -- @Marker@ field of a subsequent request to continue listing response
    -- headers policies where you left off.
    nextMarker :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of response headers policies requested.
    maxItems :: Prelude.Int,
    -- | The number of response headers policies returned.
    quantity :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResponseHeadersPolicyList' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'items', 'responseHeadersPolicyList_items' - The response headers policies in the list.
--
-- 'nextMarker', 'responseHeadersPolicyList_nextMarker' - If there are more items in the list than are in this response, this
-- element is present. It contains the value that you should use in the
-- @Marker@ field of a subsequent request to continue listing response
-- headers policies where you left off.
--
-- 'maxItems', 'responseHeadersPolicyList_maxItems' - The maximum number of response headers policies requested.
--
-- 'quantity', 'responseHeadersPolicyList_quantity' - The number of response headers policies returned.
newResponseHeadersPolicyList ::
  -- | 'maxItems'
  Prelude.Int ->
  -- | 'quantity'
  Prelude.Int ->
  ResponseHeadersPolicyList
newResponseHeadersPolicyList pMaxItems_ pQuantity_ =
  ResponseHeadersPolicyList'
    { items = Prelude.Nothing,
      nextMarker = Prelude.Nothing,
      maxItems = pMaxItems_,
      quantity = pQuantity_
    }

-- | The response headers policies in the list.
responseHeadersPolicyList_items :: Lens.Lens' ResponseHeadersPolicyList (Prelude.Maybe [ResponseHeadersPolicySummary])
responseHeadersPolicyList_items = Lens.lens (\ResponseHeadersPolicyList' {items} -> items) (\s@ResponseHeadersPolicyList' {} a -> s {items = a} :: ResponseHeadersPolicyList) Prelude.. Lens.mapping Lens.coerced

-- | If there are more items in the list than are in this response, this
-- element is present. It contains the value that you should use in the
-- @Marker@ field of a subsequent request to continue listing response
-- headers policies where you left off.
responseHeadersPolicyList_nextMarker :: Lens.Lens' ResponseHeadersPolicyList (Prelude.Maybe Prelude.Text)
responseHeadersPolicyList_nextMarker = Lens.lens (\ResponseHeadersPolicyList' {nextMarker} -> nextMarker) (\s@ResponseHeadersPolicyList' {} a -> s {nextMarker = a} :: ResponseHeadersPolicyList)

-- | The maximum number of response headers policies requested.
responseHeadersPolicyList_maxItems :: Lens.Lens' ResponseHeadersPolicyList Prelude.Int
responseHeadersPolicyList_maxItems = Lens.lens (\ResponseHeadersPolicyList' {maxItems} -> maxItems) (\s@ResponseHeadersPolicyList' {} a -> s {maxItems = a} :: ResponseHeadersPolicyList)

-- | The number of response headers policies returned.
responseHeadersPolicyList_quantity :: Lens.Lens' ResponseHeadersPolicyList Prelude.Int
responseHeadersPolicyList_quantity = Lens.lens (\ResponseHeadersPolicyList' {quantity} -> quantity) (\s@ResponseHeadersPolicyList' {} a -> s {quantity = a} :: ResponseHeadersPolicyList)

instance Data.FromXML ResponseHeadersPolicyList where
  parseXML x =
    ResponseHeadersPolicyList'
      Prelude.<$> ( x
                      Data..@? "Items"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may
                        (Data.parseXMLList "ResponseHeadersPolicySummary")
                  )
      Prelude.<*> (x Data..@? "NextMarker")
      Prelude.<*> (x Data..@ "MaxItems")
      Prelude.<*> (x Data..@ "Quantity")

instance Prelude.Hashable ResponseHeadersPolicyList where
  hashWithSalt _salt ResponseHeadersPolicyList' {..} =
    _salt
      `Prelude.hashWithSalt` items
      `Prelude.hashWithSalt` nextMarker
      `Prelude.hashWithSalt` maxItems
      `Prelude.hashWithSalt` quantity

instance Prelude.NFData ResponseHeadersPolicyList where
  rnf ResponseHeadersPolicyList' {..} =
    Prelude.rnf items
      `Prelude.seq` Prelude.rnf nextMarker
      `Prelude.seq` Prelude.rnf maxItems
      `Prelude.seq` Prelude.rnf quantity
