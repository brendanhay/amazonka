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
-- Module      : Amazonka.CloudFront.Types.ResponseHeadersPolicyAccessControlExposeHeaders
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudFront.Types.ResponseHeadersPolicyAccessControlExposeHeaders where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A list of HTTP headers that CloudFront includes as values for the
-- @Access-Control-Expose-Headers@ HTTP response header.
--
-- For more information about the @Access-Control-Expose-Headers@ HTTP
-- response header, see
-- <https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Access-Control-Expose-Headers Access-Control-Expose-Headers>
-- in the MDN Web Docs.
--
-- /See:/ 'newResponseHeadersPolicyAccessControlExposeHeaders' smart constructor.
data ResponseHeadersPolicyAccessControlExposeHeaders = ResponseHeadersPolicyAccessControlExposeHeaders'
  { -- | The list of HTTP headers. You can specify @*@ to expose all headers.
    items :: Prelude.Maybe [Prelude.Text],
    -- | The number of HTTP headers in the list.
    quantity :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResponseHeadersPolicyAccessControlExposeHeaders' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'items', 'responseHeadersPolicyAccessControlExposeHeaders_items' - The list of HTTP headers. You can specify @*@ to expose all headers.
--
-- 'quantity', 'responseHeadersPolicyAccessControlExposeHeaders_quantity' - The number of HTTP headers in the list.
newResponseHeadersPolicyAccessControlExposeHeaders ::
  -- | 'quantity'
  Prelude.Int ->
  ResponseHeadersPolicyAccessControlExposeHeaders
newResponseHeadersPolicyAccessControlExposeHeaders
  pQuantity_ =
    ResponseHeadersPolicyAccessControlExposeHeaders'
      { items =
          Prelude.Nothing,
        quantity = pQuantity_
      }

-- | The list of HTTP headers. You can specify @*@ to expose all headers.
responseHeadersPolicyAccessControlExposeHeaders_items :: Lens.Lens' ResponseHeadersPolicyAccessControlExposeHeaders (Prelude.Maybe [Prelude.Text])
responseHeadersPolicyAccessControlExposeHeaders_items = Lens.lens (\ResponseHeadersPolicyAccessControlExposeHeaders' {items} -> items) (\s@ResponseHeadersPolicyAccessControlExposeHeaders' {} a -> s {items = a} :: ResponseHeadersPolicyAccessControlExposeHeaders) Prelude.. Lens.mapping Lens.coerced

-- | The number of HTTP headers in the list.
responseHeadersPolicyAccessControlExposeHeaders_quantity :: Lens.Lens' ResponseHeadersPolicyAccessControlExposeHeaders Prelude.Int
responseHeadersPolicyAccessControlExposeHeaders_quantity = Lens.lens (\ResponseHeadersPolicyAccessControlExposeHeaders' {quantity} -> quantity) (\s@ResponseHeadersPolicyAccessControlExposeHeaders' {} a -> s {quantity = a} :: ResponseHeadersPolicyAccessControlExposeHeaders)

instance
  Data.FromXML
    ResponseHeadersPolicyAccessControlExposeHeaders
  where
  parseXML x =
    ResponseHeadersPolicyAccessControlExposeHeaders'
      Prelude.<$> ( x Data..@? "Items" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "Header")
                  )
      Prelude.<*> (x Data..@ "Quantity")

instance
  Prelude.Hashable
    ResponseHeadersPolicyAccessControlExposeHeaders
  where
  hashWithSalt
    _salt
    ResponseHeadersPolicyAccessControlExposeHeaders' {..} =
      _salt
        `Prelude.hashWithSalt` items
        `Prelude.hashWithSalt` quantity

instance
  Prelude.NFData
    ResponseHeadersPolicyAccessControlExposeHeaders
  where
  rnf
    ResponseHeadersPolicyAccessControlExposeHeaders' {..} =
      Prelude.rnf items `Prelude.seq`
        Prelude.rnf quantity

instance
  Data.ToXML
    ResponseHeadersPolicyAccessControlExposeHeaders
  where
  toXML
    ResponseHeadersPolicyAccessControlExposeHeaders' {..} =
      Prelude.mconcat
        [ "Items"
            Data.@= Data.toXML
              (Data.toXMLList "Header" Prelude.<$> items),
          "Quantity" Data.@= quantity
        ]
