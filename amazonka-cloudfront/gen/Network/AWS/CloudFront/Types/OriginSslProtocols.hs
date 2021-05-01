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
-- Module      : Network.AWS.CloudFront.Types.OriginSslProtocols
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.OriginSslProtocols where

import Network.AWS.CloudFront.Types.SslProtocol
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
originSslProtocols_items = Lens.lens (\OriginSslProtocols' {items} -> items) (\s@OriginSslProtocols' {} a -> s {items = a} :: OriginSslProtocols) Prelude.. Prelude._Coerce

instance Prelude.FromXML OriginSslProtocols where
  parseXML x =
    OriginSslProtocols'
      Prelude.<$> (x Prelude..@ "Quantity")
      Prelude.<*> ( x Prelude..@? "Items" Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.parseXMLList "SslProtocol"
                  )

instance Prelude.Hashable OriginSslProtocols

instance Prelude.NFData OriginSslProtocols

instance Prelude.ToXML OriginSslProtocols where
  toXML OriginSslProtocols' {..} =
    Prelude.mconcat
      [ "Quantity" Prelude.@= quantity,
        "Items"
          Prelude.@= Prelude.toXMLList "SslProtocol" items
      ]
