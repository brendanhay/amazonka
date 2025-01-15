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
-- Module      : Amazonka.CloudFront.Types.QueryStringNames
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudFront.Types.QueryStringNames where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains a list of query string names.
--
-- /See:/ 'newQueryStringNames' smart constructor.
data QueryStringNames = QueryStringNames'
  { -- | A list of query string names.
    items :: Prelude.Maybe [Prelude.Text],
    -- | The number of query string names in the @Items@ list.
    quantity :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'QueryStringNames' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'items', 'queryStringNames_items' - A list of query string names.
--
-- 'quantity', 'queryStringNames_quantity' - The number of query string names in the @Items@ list.
newQueryStringNames ::
  -- | 'quantity'
  Prelude.Int ->
  QueryStringNames
newQueryStringNames pQuantity_ =
  QueryStringNames'
    { items = Prelude.Nothing,
      quantity = pQuantity_
    }

-- | A list of query string names.
queryStringNames_items :: Lens.Lens' QueryStringNames (Prelude.Maybe [Prelude.Text])
queryStringNames_items = Lens.lens (\QueryStringNames' {items} -> items) (\s@QueryStringNames' {} a -> s {items = a} :: QueryStringNames) Prelude.. Lens.mapping Lens.coerced

-- | The number of query string names in the @Items@ list.
queryStringNames_quantity :: Lens.Lens' QueryStringNames Prelude.Int
queryStringNames_quantity = Lens.lens (\QueryStringNames' {quantity} -> quantity) (\s@QueryStringNames' {} a -> s {quantity = a} :: QueryStringNames)

instance Data.FromXML QueryStringNames where
  parseXML x =
    QueryStringNames'
      Prelude.<$> ( x Data..@? "Items" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "Name")
                  )
      Prelude.<*> (x Data..@ "Quantity")

instance Prelude.Hashable QueryStringNames where
  hashWithSalt _salt QueryStringNames' {..} =
    _salt
      `Prelude.hashWithSalt` items
      `Prelude.hashWithSalt` quantity

instance Prelude.NFData QueryStringNames where
  rnf QueryStringNames' {..} =
    Prelude.rnf items `Prelude.seq`
      Prelude.rnf quantity

instance Data.ToXML QueryStringNames where
  toXML QueryStringNames' {..} =
    Prelude.mconcat
      [ "Items"
          Data.@= Data.toXML (Data.toXMLList "Name" Prelude.<$> items),
        "Quantity" Data.@= quantity
      ]
