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
-- Module      : Amazonka.NetworkFirewall.Types.Address
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.NetworkFirewall.Types.Address where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A single IP address specification. This is used in the MatchAttributes
-- source and destination specifications.
--
-- /See:/ 'newAddress' smart constructor.
data Address = Address'
  { -- | Specify an IP address or a block of IP addresses in Classless
    -- Inter-Domain Routing (CIDR) notation. Network Firewall supports all
    -- address ranges for IPv4.
    --
    -- Examples:
    --
    -- -   To configure Network Firewall to inspect for the IP address
    --     192.0.2.44, specify @192.0.2.44\/32@.
    --
    -- -   To configure Network Firewall to inspect for IP addresses from
    --     192.0.2.0 to 192.0.2.255, specify @192.0.2.0\/24@.
    --
    -- For more information about CIDR notation, see the Wikipedia entry
    -- <https://en.wikipedia.org/wiki/Classless_Inter-Domain_Routing Classless Inter-Domain Routing>.
    addressDefinition :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Address' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'addressDefinition', 'address_addressDefinition' - Specify an IP address or a block of IP addresses in Classless
-- Inter-Domain Routing (CIDR) notation. Network Firewall supports all
-- address ranges for IPv4.
--
-- Examples:
--
-- -   To configure Network Firewall to inspect for the IP address
--     192.0.2.44, specify @192.0.2.44\/32@.
--
-- -   To configure Network Firewall to inspect for IP addresses from
--     192.0.2.0 to 192.0.2.255, specify @192.0.2.0\/24@.
--
-- For more information about CIDR notation, see the Wikipedia entry
-- <https://en.wikipedia.org/wiki/Classless_Inter-Domain_Routing Classless Inter-Domain Routing>.
newAddress ::
  -- | 'addressDefinition'
  Prelude.Text ->
  Address
newAddress pAddressDefinition_ =
  Address' {addressDefinition = pAddressDefinition_}

-- | Specify an IP address or a block of IP addresses in Classless
-- Inter-Domain Routing (CIDR) notation. Network Firewall supports all
-- address ranges for IPv4.
--
-- Examples:
--
-- -   To configure Network Firewall to inspect for the IP address
--     192.0.2.44, specify @192.0.2.44\/32@.
--
-- -   To configure Network Firewall to inspect for IP addresses from
--     192.0.2.0 to 192.0.2.255, specify @192.0.2.0\/24@.
--
-- For more information about CIDR notation, see the Wikipedia entry
-- <https://en.wikipedia.org/wiki/Classless_Inter-Domain_Routing Classless Inter-Domain Routing>.
address_addressDefinition :: Lens.Lens' Address Prelude.Text
address_addressDefinition = Lens.lens (\Address' {addressDefinition} -> addressDefinition) (\s@Address' {} a -> s {addressDefinition = a} :: Address)

instance Data.FromJSON Address where
  parseJSON =
    Data.withObject
      "Address"
      ( \x ->
          Address' Prelude.<$> (x Data..: "AddressDefinition")
      )

instance Prelude.Hashable Address where
  hashWithSalt _salt Address' {..} =
    _salt `Prelude.hashWithSalt` addressDefinition

instance Prelude.NFData Address where
  rnf Address' {..} = Prelude.rnf addressDefinition

instance Data.ToJSON Address where
  toJSON Address' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("AddressDefinition" Data..= addressDefinition)
          ]
      )
