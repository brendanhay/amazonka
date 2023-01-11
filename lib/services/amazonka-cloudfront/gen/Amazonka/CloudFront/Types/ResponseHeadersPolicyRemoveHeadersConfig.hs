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
-- Module      : Amazonka.CloudFront.Types.ResponseHeadersPolicyRemoveHeadersConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudFront.Types.ResponseHeadersPolicyRemoveHeadersConfig where

import Amazonka.CloudFront.Types.ResponseHeadersPolicyRemoveHeader
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A list of HTTP header names that CloudFront removes from HTTP responses
-- to requests that match the cache behavior that this response headers
-- policy is attached to.
--
-- /See:/ 'newResponseHeadersPolicyRemoveHeadersConfig' smart constructor.
data ResponseHeadersPolicyRemoveHeadersConfig = ResponseHeadersPolicyRemoveHeadersConfig'
  { -- | The list of HTTP header names.
    items :: Prelude.Maybe [ResponseHeadersPolicyRemoveHeader],
    -- | The number of HTTP header names in the list.
    quantity :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResponseHeadersPolicyRemoveHeadersConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'items', 'responseHeadersPolicyRemoveHeadersConfig_items' - The list of HTTP header names.
--
-- 'quantity', 'responseHeadersPolicyRemoveHeadersConfig_quantity' - The number of HTTP header names in the list.
newResponseHeadersPolicyRemoveHeadersConfig ::
  -- | 'quantity'
  Prelude.Int ->
  ResponseHeadersPolicyRemoveHeadersConfig
newResponseHeadersPolicyRemoveHeadersConfig
  pQuantity_ =
    ResponseHeadersPolicyRemoveHeadersConfig'
      { items =
          Prelude.Nothing,
        quantity = pQuantity_
      }

-- | The list of HTTP header names.
responseHeadersPolicyRemoveHeadersConfig_items :: Lens.Lens' ResponseHeadersPolicyRemoveHeadersConfig (Prelude.Maybe [ResponseHeadersPolicyRemoveHeader])
responseHeadersPolicyRemoveHeadersConfig_items = Lens.lens (\ResponseHeadersPolicyRemoveHeadersConfig' {items} -> items) (\s@ResponseHeadersPolicyRemoveHeadersConfig' {} a -> s {items = a} :: ResponseHeadersPolicyRemoveHeadersConfig) Prelude.. Lens.mapping Lens.coerced

-- | The number of HTTP header names in the list.
responseHeadersPolicyRemoveHeadersConfig_quantity :: Lens.Lens' ResponseHeadersPolicyRemoveHeadersConfig Prelude.Int
responseHeadersPolicyRemoveHeadersConfig_quantity = Lens.lens (\ResponseHeadersPolicyRemoveHeadersConfig' {quantity} -> quantity) (\s@ResponseHeadersPolicyRemoveHeadersConfig' {} a -> s {quantity = a} :: ResponseHeadersPolicyRemoveHeadersConfig)

instance
  Data.FromXML
    ResponseHeadersPolicyRemoveHeadersConfig
  where
  parseXML x =
    ResponseHeadersPolicyRemoveHeadersConfig'
      Prelude.<$> ( x Data..@? "Items" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may
                        ( Data.parseXMLList
                            "ResponseHeadersPolicyRemoveHeader"
                        )
                  )
        Prelude.<*> (x Data..@ "Quantity")

instance
  Prelude.Hashable
    ResponseHeadersPolicyRemoveHeadersConfig
  where
  hashWithSalt
    _salt
    ResponseHeadersPolicyRemoveHeadersConfig' {..} =
      _salt `Prelude.hashWithSalt` items
        `Prelude.hashWithSalt` quantity

instance
  Prelude.NFData
    ResponseHeadersPolicyRemoveHeadersConfig
  where
  rnf ResponseHeadersPolicyRemoveHeadersConfig' {..} =
    Prelude.rnf items
      `Prelude.seq` Prelude.rnf quantity

instance
  Data.ToXML
    ResponseHeadersPolicyRemoveHeadersConfig
  where
  toXML ResponseHeadersPolicyRemoveHeadersConfig' {..} =
    Prelude.mconcat
      [ "Items"
          Data.@= Data.toXML
            ( Data.toXMLList "ResponseHeadersPolicyRemoveHeader"
                Prelude.<$> items
            ),
        "Quantity" Data.@= quantity
      ]
