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
-- Module      : Network.AWS.CloudFront.Types.Headers
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.Headers where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Contains a list of HTTP header names.
--
-- /See:/ 'newHeaders' smart constructor.
data Headers = Headers'
  { -- | A list of HTTP header names.
    items :: Prelude.Maybe [Prelude.Text],
    -- | The number of header names in the @Items@ list.
    quantity :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'Headers' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'items', 'headers_items' - A list of HTTP header names.
--
-- 'quantity', 'headers_quantity' - The number of header names in the @Items@ list.
newHeaders ::
  -- | 'quantity'
  Prelude.Int ->
  Headers
newHeaders pQuantity_ =
  Headers'
    { items = Prelude.Nothing,
      quantity = pQuantity_
    }

-- | A list of HTTP header names.
headers_items :: Lens.Lens' Headers (Prelude.Maybe [Prelude.Text])
headers_items = Lens.lens (\Headers' {items} -> items) (\s@Headers' {} a -> s {items = a} :: Headers) Prelude.. Lens.mapping Prelude._Coerce

-- | The number of header names in the @Items@ list.
headers_quantity :: Lens.Lens' Headers Prelude.Int
headers_quantity = Lens.lens (\Headers' {quantity} -> quantity) (\s@Headers' {} a -> s {quantity = a} :: Headers)

instance Prelude.FromXML Headers where
  parseXML x =
    Headers'
      Prelude.<$> ( x Prelude..@? "Items" Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "Name")
                  )
      Prelude.<*> (x Prelude..@ "Quantity")

instance Prelude.Hashable Headers

instance Prelude.NFData Headers

instance Prelude.ToXML Headers where
  toXML Headers' {..} =
    Prelude.mconcat
      [ "Items"
          Prelude.@= Prelude.toXML
            (Prelude.toXMLList "Name" Prelude.<$> items),
        "Quantity" Prelude.@= quantity
      ]
