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
-- Module      : Amazonka.CloudFront.Types.ResponseHeadersPolicyCustomHeadersConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudFront.Types.ResponseHeadersPolicyCustomHeadersConfig where

import Amazonka.CloudFront.Types.ResponseHeadersPolicyCustomHeader
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A list of HTTP response header names and their values. CloudFront
-- includes these headers in HTTP responses that it sends for requests that
-- match a cache behavior that\'s associated with this response headers
-- policy.
--
-- /See:/ 'newResponseHeadersPolicyCustomHeadersConfig' smart constructor.
data ResponseHeadersPolicyCustomHeadersConfig = ResponseHeadersPolicyCustomHeadersConfig'
  { -- | The list of HTTP response headers and their values.
    items :: Prelude.Maybe [ResponseHeadersPolicyCustomHeader],
    -- | The number of HTTP response headers in the list.
    quantity :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResponseHeadersPolicyCustomHeadersConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'items', 'responseHeadersPolicyCustomHeadersConfig_items' - The list of HTTP response headers and their values.
--
-- 'quantity', 'responseHeadersPolicyCustomHeadersConfig_quantity' - The number of HTTP response headers in the list.
newResponseHeadersPolicyCustomHeadersConfig ::
  -- | 'quantity'
  Prelude.Int ->
  ResponseHeadersPolicyCustomHeadersConfig
newResponseHeadersPolicyCustomHeadersConfig
  pQuantity_ =
    ResponseHeadersPolicyCustomHeadersConfig'
      { items =
          Prelude.Nothing,
        quantity = pQuantity_
      }

-- | The list of HTTP response headers and their values.
responseHeadersPolicyCustomHeadersConfig_items :: Lens.Lens' ResponseHeadersPolicyCustomHeadersConfig (Prelude.Maybe [ResponseHeadersPolicyCustomHeader])
responseHeadersPolicyCustomHeadersConfig_items = Lens.lens (\ResponseHeadersPolicyCustomHeadersConfig' {items} -> items) (\s@ResponseHeadersPolicyCustomHeadersConfig' {} a -> s {items = a} :: ResponseHeadersPolicyCustomHeadersConfig) Prelude.. Lens.mapping Lens.coerced

-- | The number of HTTP response headers in the list.
responseHeadersPolicyCustomHeadersConfig_quantity :: Lens.Lens' ResponseHeadersPolicyCustomHeadersConfig Prelude.Int
responseHeadersPolicyCustomHeadersConfig_quantity = Lens.lens (\ResponseHeadersPolicyCustomHeadersConfig' {quantity} -> quantity) (\s@ResponseHeadersPolicyCustomHeadersConfig' {} a -> s {quantity = a} :: ResponseHeadersPolicyCustomHeadersConfig)

instance
  Data.FromXML
    ResponseHeadersPolicyCustomHeadersConfig
  where
  parseXML x =
    ResponseHeadersPolicyCustomHeadersConfig'
      Prelude.<$> ( x
                      Data..@? "Items"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may
                        ( Data.parseXMLList
                            "ResponseHeadersPolicyCustomHeader"
                        )
                  )
      Prelude.<*> (x Data..@ "Quantity")

instance
  Prelude.Hashable
    ResponseHeadersPolicyCustomHeadersConfig
  where
  hashWithSalt
    _salt
    ResponseHeadersPolicyCustomHeadersConfig' {..} =
      _salt
        `Prelude.hashWithSalt` items
        `Prelude.hashWithSalt` quantity

instance
  Prelude.NFData
    ResponseHeadersPolicyCustomHeadersConfig
  where
  rnf ResponseHeadersPolicyCustomHeadersConfig' {..} =
    Prelude.rnf items
      `Prelude.seq` Prelude.rnf quantity

instance
  Data.ToXML
    ResponseHeadersPolicyCustomHeadersConfig
  where
  toXML ResponseHeadersPolicyCustomHeadersConfig' {..} =
    Prelude.mconcat
      [ "Items"
          Data.@= Data.toXML
            ( Data.toXMLList "ResponseHeadersPolicyCustomHeader"
                Prelude.<$> items
            ),
        "Quantity" Data.@= quantity
      ]
