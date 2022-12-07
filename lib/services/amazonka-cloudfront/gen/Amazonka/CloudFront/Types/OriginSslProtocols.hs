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
-- Module      : Amazonka.CloudFront.Types.OriginSslProtocols
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudFront.Types.OriginSslProtocols where

import Amazonka.CloudFront.Types.SslProtocol
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A complex type that contains information about the SSL\/TLS protocols
-- that CloudFront can use when establishing an HTTPS connection with your
-- origin.
--
-- /See:/ 'newOriginSslProtocols' smart constructor.
data OriginSslProtocols = OriginSslProtocols'
  { -- | The number of SSL\/TLS protocols that you want to allow CloudFront to
    -- use when establishing an HTTPS connection with this origin.
    quantity :: Prelude.Int,
    -- | A list that contains allowed SSL\/TLS protocols for this distribution.
    items :: [SslProtocol]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OriginSslProtocols' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'quantity', 'originSslProtocols_quantity' - The number of SSL\/TLS protocols that you want to allow CloudFront to
-- use when establishing an HTTPS connection with this origin.
--
-- 'items', 'originSslProtocols_items' - A list that contains allowed SSL\/TLS protocols for this distribution.
newOriginSslProtocols ::
  -- | 'quantity'
  Prelude.Int ->
  OriginSslProtocols
newOriginSslProtocols pQuantity_ =
  OriginSslProtocols'
    { quantity = pQuantity_,
      items = Prelude.mempty
    }

-- | The number of SSL\/TLS protocols that you want to allow CloudFront to
-- use when establishing an HTTPS connection with this origin.
originSslProtocols_quantity :: Lens.Lens' OriginSslProtocols Prelude.Int
originSslProtocols_quantity = Lens.lens (\OriginSslProtocols' {quantity} -> quantity) (\s@OriginSslProtocols' {} a -> s {quantity = a} :: OriginSslProtocols)

-- | A list that contains allowed SSL\/TLS protocols for this distribution.
originSslProtocols_items :: Lens.Lens' OriginSslProtocols [SslProtocol]
originSslProtocols_items = Lens.lens (\OriginSslProtocols' {items} -> items) (\s@OriginSslProtocols' {} a -> s {items = a} :: OriginSslProtocols) Prelude.. Lens.coerced

instance Data.FromXML OriginSslProtocols where
  parseXML x =
    OriginSslProtocols'
      Prelude.<$> (x Data..@ "Quantity")
      Prelude.<*> ( x Data..@? "Items" Core..!@ Prelude.mempty
                      Prelude.>>= Data.parseXMLList "SslProtocol"
                  )

instance Prelude.Hashable OriginSslProtocols where
  hashWithSalt _salt OriginSslProtocols' {..} =
    _salt `Prelude.hashWithSalt` quantity
      `Prelude.hashWithSalt` items

instance Prelude.NFData OriginSslProtocols where
  rnf OriginSslProtocols' {..} =
    Prelude.rnf quantity
      `Prelude.seq` Prelude.rnf items

instance Data.ToXML OriginSslProtocols where
  toXML OriginSslProtocols' {..} =
    Prelude.mconcat
      [ "Quantity" Data.@= quantity,
        "Items" Data.@= Data.toXMLList "SslProtocol" items
      ]
