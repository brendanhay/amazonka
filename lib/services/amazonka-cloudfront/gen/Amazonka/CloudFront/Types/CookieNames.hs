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
-- Module      : Amazonka.CloudFront.Types.CookieNames
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudFront.Types.CookieNames where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains a list of cookie names.
--
-- /See:/ 'newCookieNames' smart constructor.
data CookieNames = CookieNames'
  { -- | A list of cookie names.
    items :: Prelude.Maybe [Prelude.Text],
    -- | The number of cookie names in the @Items@ list.
    quantity :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CookieNames' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'items', 'cookieNames_items' - A list of cookie names.
--
-- 'quantity', 'cookieNames_quantity' - The number of cookie names in the @Items@ list.
newCookieNames ::
  -- | 'quantity'
  Prelude.Int ->
  CookieNames
newCookieNames pQuantity_ =
  CookieNames'
    { items = Prelude.Nothing,
      quantity = pQuantity_
    }

-- | A list of cookie names.
cookieNames_items :: Lens.Lens' CookieNames (Prelude.Maybe [Prelude.Text])
cookieNames_items = Lens.lens (\CookieNames' {items} -> items) (\s@CookieNames' {} a -> s {items = a} :: CookieNames) Prelude.. Lens.mapping Lens.coerced

-- | The number of cookie names in the @Items@ list.
cookieNames_quantity :: Lens.Lens' CookieNames Prelude.Int
cookieNames_quantity = Lens.lens (\CookieNames' {quantity} -> quantity) (\s@CookieNames' {} a -> s {quantity = a} :: CookieNames)

instance Data.FromXML CookieNames where
  parseXML x =
    CookieNames'
      Prelude.<$> ( x Data..@? "Items" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "Name")
                  )
      Prelude.<*> (x Data..@ "Quantity")

instance Prelude.Hashable CookieNames where
  hashWithSalt _salt CookieNames' {..} =
    _salt
      `Prelude.hashWithSalt` items
      `Prelude.hashWithSalt` quantity

instance Prelude.NFData CookieNames where
  rnf CookieNames' {..} =
    Prelude.rnf items `Prelude.seq`
      Prelude.rnf quantity

instance Data.ToXML CookieNames where
  toXML CookieNames' {..} =
    Prelude.mconcat
      [ "Items"
          Data.@= Data.toXML (Data.toXMLList "Name" Prelude.<$> items),
        "Quantity" Data.@= quantity
      ]
