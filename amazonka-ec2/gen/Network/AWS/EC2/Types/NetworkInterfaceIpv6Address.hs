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
-- Module      : Network.AWS.EC2.Types.NetworkInterfaceIpv6Address
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.NetworkInterfaceIpv6Address where

import Network.AWS.EC2.Internal
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes an IPv6 address associated with a network interface.
--
-- /See:/ 'newNetworkInterfaceIpv6Address' smart constructor.
data NetworkInterfaceIpv6Address = NetworkInterfaceIpv6Address'
  { -- | The IPv6 address.
    ipv6Address :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'NetworkInterfaceIpv6Address' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ipv6Address', 'networkInterfaceIpv6Address_ipv6Address' - The IPv6 address.
newNetworkInterfaceIpv6Address ::
  NetworkInterfaceIpv6Address
newNetworkInterfaceIpv6Address =
  NetworkInterfaceIpv6Address'
    { ipv6Address =
        Prelude.Nothing
    }

-- | The IPv6 address.
networkInterfaceIpv6Address_ipv6Address :: Lens.Lens' NetworkInterfaceIpv6Address (Prelude.Maybe Prelude.Text)
networkInterfaceIpv6Address_ipv6Address = Lens.lens (\NetworkInterfaceIpv6Address' {ipv6Address} -> ipv6Address) (\s@NetworkInterfaceIpv6Address' {} a -> s {ipv6Address = a} :: NetworkInterfaceIpv6Address)

instance Prelude.FromXML NetworkInterfaceIpv6Address where
  parseXML x =
    NetworkInterfaceIpv6Address'
      Prelude.<$> (x Prelude..@? "ipv6Address")

instance Prelude.Hashable NetworkInterfaceIpv6Address

instance Prelude.NFData NetworkInterfaceIpv6Address
