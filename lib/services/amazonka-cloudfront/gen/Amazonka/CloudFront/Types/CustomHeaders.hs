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
-- Module      : Amazonka.CloudFront.Types.CustomHeaders
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudFront.Types.CustomHeaders where

import Amazonka.CloudFront.Types.OriginCustomHeader
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A complex type that contains the list of Custom Headers for each origin.
--
-- /See:/ 'newCustomHeaders' smart constructor.
data CustomHeaders = CustomHeaders'
  { -- | __Optional__: A list that contains one @OriginCustomHeader@ element for
    -- each custom header that you want CloudFront to forward to the origin. If
    -- Quantity is @0@, omit @Items@.
    items :: Prelude.Maybe [OriginCustomHeader],
    -- | The number of custom headers, if any, for this distribution.
    quantity :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CustomHeaders' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'items', 'customHeaders_items' - __Optional__: A list that contains one @OriginCustomHeader@ element for
-- each custom header that you want CloudFront to forward to the origin. If
-- Quantity is @0@, omit @Items@.
--
-- 'quantity', 'customHeaders_quantity' - The number of custom headers, if any, for this distribution.
newCustomHeaders ::
  -- | 'quantity'
  Prelude.Int ->
  CustomHeaders
newCustomHeaders pQuantity_ =
  CustomHeaders'
    { items = Prelude.Nothing,
      quantity = pQuantity_
    }

-- | __Optional__: A list that contains one @OriginCustomHeader@ element for
-- each custom header that you want CloudFront to forward to the origin. If
-- Quantity is @0@, omit @Items@.
customHeaders_items :: Lens.Lens' CustomHeaders (Prelude.Maybe [OriginCustomHeader])
customHeaders_items = Lens.lens (\CustomHeaders' {items} -> items) (\s@CustomHeaders' {} a -> s {items = a} :: CustomHeaders) Prelude.. Lens.mapping Lens.coerced

-- | The number of custom headers, if any, for this distribution.
customHeaders_quantity :: Lens.Lens' CustomHeaders Prelude.Int
customHeaders_quantity = Lens.lens (\CustomHeaders' {quantity} -> quantity) (\s@CustomHeaders' {} a -> s {quantity = a} :: CustomHeaders)

instance Data.FromXML CustomHeaders where
  parseXML x =
    CustomHeaders'
      Prelude.<$> ( x
                      Data..@? "Items"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "OriginCustomHeader")
                  )
      Prelude.<*> (x Data..@ "Quantity")

instance Prelude.Hashable CustomHeaders where
  hashWithSalt _salt CustomHeaders' {..} =
    _salt
      `Prelude.hashWithSalt` items
      `Prelude.hashWithSalt` quantity

instance Prelude.NFData CustomHeaders where
  rnf CustomHeaders' {..} =
    Prelude.rnf items
      `Prelude.seq` Prelude.rnf quantity

instance Data.ToXML CustomHeaders where
  toXML CustomHeaders' {..} =
    Prelude.mconcat
      [ "Items"
          Data.@= Data.toXML
            ( Data.toXMLList "OriginCustomHeader"
                Prelude.<$> items
            ),
        "Quantity" Data.@= quantity
      ]
