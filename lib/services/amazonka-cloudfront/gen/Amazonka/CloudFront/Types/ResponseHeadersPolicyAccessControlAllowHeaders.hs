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
-- Module      : Amazonka.CloudFront.Types.ResponseHeadersPolicyAccessControlAllowHeaders
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudFront.Types.ResponseHeadersPolicyAccessControlAllowHeaders where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | A list of HTTP header names that CloudFront includes as values for the
-- @Access-Control-Allow-Headers@ HTTP response header.
--
-- For more information about the @Access-Control-Allow-Headers@ HTTP
-- response header, see
-- <https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Access-Control-Allow-Headers Access-Control-Allow-Headers>
-- in the MDN Web Docs.
--
-- /See:/ 'newResponseHeadersPolicyAccessControlAllowHeaders' smart constructor.
data ResponseHeadersPolicyAccessControlAllowHeaders = ResponseHeadersPolicyAccessControlAllowHeaders'
  { -- | The number of HTTP header names in the list.
    quantity :: Prelude.Int,
    -- | The list of HTTP header names. You can specify @*@ to allow all headers.
    items :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResponseHeadersPolicyAccessControlAllowHeaders' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'quantity', 'responseHeadersPolicyAccessControlAllowHeaders_quantity' - The number of HTTP header names in the list.
--
-- 'items', 'responseHeadersPolicyAccessControlAllowHeaders_items' - The list of HTTP header names. You can specify @*@ to allow all headers.
newResponseHeadersPolicyAccessControlAllowHeaders ::
  -- | 'quantity'
  Prelude.Int ->
  ResponseHeadersPolicyAccessControlAllowHeaders
newResponseHeadersPolicyAccessControlAllowHeaders
  pQuantity_ =
    ResponseHeadersPolicyAccessControlAllowHeaders'
      { quantity =
          pQuantity_,
        items = Prelude.mempty
      }

-- | The number of HTTP header names in the list.
responseHeadersPolicyAccessControlAllowHeaders_quantity :: Lens.Lens' ResponseHeadersPolicyAccessControlAllowHeaders Prelude.Int
responseHeadersPolicyAccessControlAllowHeaders_quantity = Lens.lens (\ResponseHeadersPolicyAccessControlAllowHeaders' {quantity} -> quantity) (\s@ResponseHeadersPolicyAccessControlAllowHeaders' {} a -> s {quantity = a} :: ResponseHeadersPolicyAccessControlAllowHeaders)

-- | The list of HTTP header names. You can specify @*@ to allow all headers.
responseHeadersPolicyAccessControlAllowHeaders_items :: Lens.Lens' ResponseHeadersPolicyAccessControlAllowHeaders [Prelude.Text]
responseHeadersPolicyAccessControlAllowHeaders_items = Lens.lens (\ResponseHeadersPolicyAccessControlAllowHeaders' {items} -> items) (\s@ResponseHeadersPolicyAccessControlAllowHeaders' {} a -> s {items = a} :: ResponseHeadersPolicyAccessControlAllowHeaders) Prelude.. Lens.coerced

instance
  Core.FromXML
    ResponseHeadersPolicyAccessControlAllowHeaders
  where
  parseXML x =
    ResponseHeadersPolicyAccessControlAllowHeaders'
      Prelude.<$> (x Core..@ "Quantity")
        Prelude.<*> ( x Core..@? "Items" Core..!@ Prelude.mempty
                        Prelude.>>= Core.parseXMLList "Header"
                    )

instance
  Prelude.Hashable
    ResponseHeadersPolicyAccessControlAllowHeaders
  where
  hashWithSalt
    _salt
    ResponseHeadersPolicyAccessControlAllowHeaders' {..} =
      _salt `Prelude.hashWithSalt` quantity
        `Prelude.hashWithSalt` items

instance
  Prelude.NFData
    ResponseHeadersPolicyAccessControlAllowHeaders
  where
  rnf
    ResponseHeadersPolicyAccessControlAllowHeaders' {..} =
      Prelude.rnf quantity
        `Prelude.seq` Prelude.rnf items

instance
  Core.ToXML
    ResponseHeadersPolicyAccessControlAllowHeaders
  where
  toXML
    ResponseHeadersPolicyAccessControlAllowHeaders' {..} =
      Prelude.mconcat
        [ "Quantity" Core.@= quantity,
          "Items" Core.@= Core.toXMLList "Header" items
        ]
