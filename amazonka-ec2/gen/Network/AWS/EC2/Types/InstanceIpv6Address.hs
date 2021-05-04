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
-- Module      : Network.AWS.EC2.Types.InstanceIpv6Address
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.InstanceIpv6Address where

import Network.AWS.EC2.Internal
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes an IPv6 address.
--
-- /See:/ 'newInstanceIpv6Address' smart constructor.
data InstanceIpv6Address = InstanceIpv6Address'
  { -- | The IPv6 address.
    ipv6Address :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'InstanceIpv6Address' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ipv6Address', 'instanceIpv6Address_ipv6Address' - The IPv6 address.
newInstanceIpv6Address ::
  InstanceIpv6Address
newInstanceIpv6Address =
  InstanceIpv6Address' {ipv6Address = Prelude.Nothing}

-- | The IPv6 address.
instanceIpv6Address_ipv6Address :: Lens.Lens' InstanceIpv6Address (Prelude.Maybe Prelude.Text)
instanceIpv6Address_ipv6Address = Lens.lens (\InstanceIpv6Address' {ipv6Address} -> ipv6Address) (\s@InstanceIpv6Address' {} a -> s {ipv6Address = a} :: InstanceIpv6Address)

instance Prelude.FromXML InstanceIpv6Address where
  parseXML x =
    InstanceIpv6Address'
      Prelude.<$> (x Prelude..@? "ipv6Address")

instance Prelude.Hashable InstanceIpv6Address

instance Prelude.NFData InstanceIpv6Address

instance Prelude.ToQuery InstanceIpv6Address where
  toQuery InstanceIpv6Address' {..} =
    Prelude.mconcat
      ["Ipv6Address" Prelude.=: ipv6Address]
