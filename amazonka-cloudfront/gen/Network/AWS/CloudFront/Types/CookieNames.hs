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
-- Module      : Network.AWS.CloudFront.Types.CookieNames
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.CookieNames where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Contains a list of cookie names.
--
-- /See:/ 'newCookieNames' smart constructor.
data CookieNames = CookieNames'
  { -- | A list of cookie names.
    items :: Prelude.Maybe [Prelude.Text],
    -- | The number of cookie names in the @Items@ list.
    quantity :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
cookieNames_items = Lens.lens (\CookieNames' {items} -> items) (\s@CookieNames' {} a -> s {items = a} :: CookieNames) Prelude.. Lens.mapping Prelude._Coerce

-- | The number of cookie names in the @Items@ list.
cookieNames_quantity :: Lens.Lens' CookieNames Prelude.Int
cookieNames_quantity = Lens.lens (\CookieNames' {quantity} -> quantity) (\s@CookieNames' {} a -> s {quantity = a} :: CookieNames)

instance Prelude.FromXML CookieNames where
  parseXML x =
    CookieNames'
      Prelude.<$> ( x Prelude..@? "Items" Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "Name")
                  )
      Prelude.<*> (x Prelude..@ "Quantity")

instance Prelude.Hashable CookieNames

instance Prelude.NFData CookieNames

instance Prelude.ToXML CookieNames where
  toXML CookieNames' {..} =
    Prelude.mconcat
      [ "Items"
          Prelude.@= Prelude.toXML
            (Prelude.toXMLList "Name" Prelude.<$> items),
        "Quantity" Prelude.@= quantity
      ]
