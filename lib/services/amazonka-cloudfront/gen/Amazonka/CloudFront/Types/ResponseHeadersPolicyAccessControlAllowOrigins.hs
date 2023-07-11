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
-- Module      : Amazonka.CloudFront.Types.ResponseHeadersPolicyAccessControlAllowOrigins
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudFront.Types.ResponseHeadersPolicyAccessControlAllowOrigins where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A list of origins (domain names) that CloudFront can use as the value
-- for the @Access-Control-Allow-Origin@ HTTP response header.
--
-- For more information about the @Access-Control-Allow-Origin@ HTTP
-- response header, see
-- <https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Access-Control-Allow-Origin Access-Control-Allow-Origin>
-- in the MDN Web Docs.
--
-- /See:/ 'newResponseHeadersPolicyAccessControlAllowOrigins' smart constructor.
data ResponseHeadersPolicyAccessControlAllowOrigins = ResponseHeadersPolicyAccessControlAllowOrigins'
  { -- | The number of origins in the list.
    quantity :: Prelude.Int,
    -- | The list of origins (domain names). You can specify @*@ to allow all
    -- origins.
    items :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResponseHeadersPolicyAccessControlAllowOrigins' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'quantity', 'responseHeadersPolicyAccessControlAllowOrigins_quantity' - The number of origins in the list.
--
-- 'items', 'responseHeadersPolicyAccessControlAllowOrigins_items' - The list of origins (domain names). You can specify @*@ to allow all
-- origins.
newResponseHeadersPolicyAccessControlAllowOrigins ::
  -- | 'quantity'
  Prelude.Int ->
  ResponseHeadersPolicyAccessControlAllowOrigins
newResponseHeadersPolicyAccessControlAllowOrigins
  pQuantity_ =
    ResponseHeadersPolicyAccessControlAllowOrigins'
      { quantity =
          pQuantity_,
        items = Prelude.mempty
      }

-- | The number of origins in the list.
responseHeadersPolicyAccessControlAllowOrigins_quantity :: Lens.Lens' ResponseHeadersPolicyAccessControlAllowOrigins Prelude.Int
responseHeadersPolicyAccessControlAllowOrigins_quantity = Lens.lens (\ResponseHeadersPolicyAccessControlAllowOrigins' {quantity} -> quantity) (\s@ResponseHeadersPolicyAccessControlAllowOrigins' {} a -> s {quantity = a} :: ResponseHeadersPolicyAccessControlAllowOrigins)

-- | The list of origins (domain names). You can specify @*@ to allow all
-- origins.
responseHeadersPolicyAccessControlAllowOrigins_items :: Lens.Lens' ResponseHeadersPolicyAccessControlAllowOrigins [Prelude.Text]
responseHeadersPolicyAccessControlAllowOrigins_items = Lens.lens (\ResponseHeadersPolicyAccessControlAllowOrigins' {items} -> items) (\s@ResponseHeadersPolicyAccessControlAllowOrigins' {} a -> s {items = a} :: ResponseHeadersPolicyAccessControlAllowOrigins) Prelude.. Lens.coerced

instance
  Data.FromXML
    ResponseHeadersPolicyAccessControlAllowOrigins
  where
  parseXML x =
    ResponseHeadersPolicyAccessControlAllowOrigins'
      Prelude.<$> (x Data..@ "Quantity")
      Prelude.<*> ( x
                      Data..@? "Items"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Data.parseXMLList "Origin"
                  )

instance
  Prelude.Hashable
    ResponseHeadersPolicyAccessControlAllowOrigins
  where
  hashWithSalt
    _salt
    ResponseHeadersPolicyAccessControlAllowOrigins' {..} =
      _salt
        `Prelude.hashWithSalt` quantity
        `Prelude.hashWithSalt` items

instance
  Prelude.NFData
    ResponseHeadersPolicyAccessControlAllowOrigins
  where
  rnf
    ResponseHeadersPolicyAccessControlAllowOrigins' {..} =
      Prelude.rnf quantity
        `Prelude.seq` Prelude.rnf items

instance
  Data.ToXML
    ResponseHeadersPolicyAccessControlAllowOrigins
  where
  toXML
    ResponseHeadersPolicyAccessControlAllowOrigins' {..} =
      Prelude.mconcat
        [ "Quantity" Data.@= quantity,
          "Items" Data.@= Data.toXMLList "Origin" items
        ]
