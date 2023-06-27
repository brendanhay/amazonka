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
-- Module      : Amazonka.CloudFront.Types.CustomErrorResponses
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudFront.Types.CustomErrorResponses where

import Amazonka.CloudFront.Types.CustomErrorResponse
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A complex type that controls:
--
-- -   Whether CloudFront replaces HTTP status codes in the 4xx and 5xx
--     range with custom error messages before returning the response to
--     the viewer.
--
-- -   How long CloudFront caches HTTP status codes in the 4xx and 5xx
--     range.
--
-- For more information about custom error pages, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/custom-error-pages.html Customizing Error Responses>
-- in the /Amazon CloudFront Developer Guide/.
--
-- /See:/ 'newCustomErrorResponses' smart constructor.
data CustomErrorResponses = CustomErrorResponses'
  { -- | A complex type that contains a @CustomErrorResponse@ element for each
    -- HTTP status code for which you want to specify a custom error page
    -- and\/or a caching duration.
    items :: Prelude.Maybe [CustomErrorResponse],
    -- | The number of HTTP status codes for which you want to specify a custom
    -- error page and\/or a caching duration. If @Quantity@ is @0@, you can
    -- omit @Items@.
    quantity :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CustomErrorResponses' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'items', 'customErrorResponses_items' - A complex type that contains a @CustomErrorResponse@ element for each
-- HTTP status code for which you want to specify a custom error page
-- and\/or a caching duration.
--
-- 'quantity', 'customErrorResponses_quantity' - The number of HTTP status codes for which you want to specify a custom
-- error page and\/or a caching duration. If @Quantity@ is @0@, you can
-- omit @Items@.
newCustomErrorResponses ::
  -- | 'quantity'
  Prelude.Int ->
  CustomErrorResponses
newCustomErrorResponses pQuantity_ =
  CustomErrorResponses'
    { items = Prelude.Nothing,
      quantity = pQuantity_
    }

-- | A complex type that contains a @CustomErrorResponse@ element for each
-- HTTP status code for which you want to specify a custom error page
-- and\/or a caching duration.
customErrorResponses_items :: Lens.Lens' CustomErrorResponses (Prelude.Maybe [CustomErrorResponse])
customErrorResponses_items = Lens.lens (\CustomErrorResponses' {items} -> items) (\s@CustomErrorResponses' {} a -> s {items = a} :: CustomErrorResponses) Prelude.. Lens.mapping Lens.coerced

-- | The number of HTTP status codes for which you want to specify a custom
-- error page and\/or a caching duration. If @Quantity@ is @0@, you can
-- omit @Items@.
customErrorResponses_quantity :: Lens.Lens' CustomErrorResponses Prelude.Int
customErrorResponses_quantity = Lens.lens (\CustomErrorResponses' {quantity} -> quantity) (\s@CustomErrorResponses' {} a -> s {quantity = a} :: CustomErrorResponses)

instance Data.FromXML CustomErrorResponses where
  parseXML x =
    CustomErrorResponses'
      Prelude.<$> ( x
                      Data..@? "Items"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "CustomErrorResponse")
                  )
      Prelude.<*> (x Data..@ "Quantity")

instance Prelude.Hashable CustomErrorResponses where
  hashWithSalt _salt CustomErrorResponses' {..} =
    _salt
      `Prelude.hashWithSalt` items
      `Prelude.hashWithSalt` quantity

instance Prelude.NFData CustomErrorResponses where
  rnf CustomErrorResponses' {..} =
    Prelude.rnf items
      `Prelude.seq` Prelude.rnf quantity

instance Data.ToXML CustomErrorResponses where
  toXML CustomErrorResponses' {..} =
    Prelude.mconcat
      [ "Items"
          Data.@= Data.toXML
            ( Data.toXMLList "CustomErrorResponse"
                Prelude.<$> items
            ),
        "Quantity" Data.@= quantity
      ]
