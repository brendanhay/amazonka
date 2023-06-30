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
-- Module      : Amazonka.CloudFront.Types.FunctionList
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudFront.Types.FunctionList where

import Amazonka.CloudFront.Types.FunctionSummary
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A list of CloudFront functions.
--
-- /See:/ 'newFunctionList' smart constructor.
data FunctionList = FunctionList'
  { -- | Contains the functions in the list.
    items :: Prelude.Maybe [FunctionSummary],
    -- | If there are more items in the list than are in this response, this
    -- element is present. It contains the value that you should use in the
    -- @Marker@ field of a subsequent request to continue listing functions
    -- where you left off.
    nextMarker :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of functions requested.
    maxItems :: Prelude.Int,
    -- | The number of functions returned in the response.
    quantity :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FunctionList' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'items', 'functionList_items' - Contains the functions in the list.
--
-- 'nextMarker', 'functionList_nextMarker' - If there are more items in the list than are in this response, this
-- element is present. It contains the value that you should use in the
-- @Marker@ field of a subsequent request to continue listing functions
-- where you left off.
--
-- 'maxItems', 'functionList_maxItems' - The maximum number of functions requested.
--
-- 'quantity', 'functionList_quantity' - The number of functions returned in the response.
newFunctionList ::
  -- | 'maxItems'
  Prelude.Int ->
  -- | 'quantity'
  Prelude.Int ->
  FunctionList
newFunctionList pMaxItems_ pQuantity_ =
  FunctionList'
    { items = Prelude.Nothing,
      nextMarker = Prelude.Nothing,
      maxItems = pMaxItems_,
      quantity = pQuantity_
    }

-- | Contains the functions in the list.
functionList_items :: Lens.Lens' FunctionList (Prelude.Maybe [FunctionSummary])
functionList_items = Lens.lens (\FunctionList' {items} -> items) (\s@FunctionList' {} a -> s {items = a} :: FunctionList) Prelude.. Lens.mapping Lens.coerced

-- | If there are more items in the list than are in this response, this
-- element is present. It contains the value that you should use in the
-- @Marker@ field of a subsequent request to continue listing functions
-- where you left off.
functionList_nextMarker :: Lens.Lens' FunctionList (Prelude.Maybe Prelude.Text)
functionList_nextMarker = Lens.lens (\FunctionList' {nextMarker} -> nextMarker) (\s@FunctionList' {} a -> s {nextMarker = a} :: FunctionList)

-- | The maximum number of functions requested.
functionList_maxItems :: Lens.Lens' FunctionList Prelude.Int
functionList_maxItems = Lens.lens (\FunctionList' {maxItems} -> maxItems) (\s@FunctionList' {} a -> s {maxItems = a} :: FunctionList)

-- | The number of functions returned in the response.
functionList_quantity :: Lens.Lens' FunctionList Prelude.Int
functionList_quantity = Lens.lens (\FunctionList' {quantity} -> quantity) (\s@FunctionList' {} a -> s {quantity = a} :: FunctionList)

instance Data.FromXML FunctionList where
  parseXML x =
    FunctionList'
      Prelude.<$> ( x
                      Data..@? "Items"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "FunctionSummary")
                  )
      Prelude.<*> (x Data..@? "NextMarker")
      Prelude.<*> (x Data..@ "MaxItems")
      Prelude.<*> (x Data..@ "Quantity")

instance Prelude.Hashable FunctionList where
  hashWithSalt _salt FunctionList' {..} =
    _salt
      `Prelude.hashWithSalt` items
      `Prelude.hashWithSalt` nextMarker
      `Prelude.hashWithSalt` maxItems
      `Prelude.hashWithSalt` quantity

instance Prelude.NFData FunctionList where
  rnf FunctionList' {..} =
    Prelude.rnf items
      `Prelude.seq` Prelude.rnf nextMarker
      `Prelude.seq` Prelude.rnf maxItems
      `Prelude.seq` Prelude.rnf quantity
