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
-- Module      : Amazonka.CloudFront.Types.Headers
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudFront.Types.Headers where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Contains a list of HTTP header names.
--
-- /See:/ 'newHeaders' smart constructor.
data Headers = Headers'
  { -- | A list of HTTP header names.
    items :: Prelude.Maybe [Prelude.Text],
    -- | The number of header names in the @Items@ list.
    quantity :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
headers_items = Lens.lens (\Headers' {items} -> items) (\s@Headers' {} a -> s {items = a} :: Headers) Prelude.. Lens.mapping Lens.coerced

-- | The number of header names in the @Items@ list.
headers_quantity :: Lens.Lens' Headers Prelude.Int
headers_quantity = Lens.lens (\Headers' {quantity} -> quantity) (\s@Headers' {} a -> s {quantity = a} :: Headers)

instance Core.FromXML Headers where
  parseXML x =
    Headers'
      Prelude.<$> ( x Core..@? "Items" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "Name")
                  )
      Prelude.<*> (x Core..@ "Quantity")

instance Prelude.Hashable Headers where
  hashWithSalt _salt Headers' {..} =
    _salt `Prelude.hashWithSalt` items
      `Prelude.hashWithSalt` quantity

instance Prelude.NFData Headers where
  rnf Headers' {..} =
    Prelude.rnf items
      `Prelude.seq` Prelude.rnf quantity

instance Core.ToXML Headers where
  toXML Headers' {..} =
    Prelude.mconcat
      [ "Items"
          Core.@= Core.toXML (Core.toXMLList "Name" Prelude.<$> items),
        "Quantity" Core.@= quantity
      ]
