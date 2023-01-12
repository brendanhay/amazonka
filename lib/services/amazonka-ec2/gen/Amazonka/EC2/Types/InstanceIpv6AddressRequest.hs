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
-- Module      : Amazonka.EC2.Types.InstanceIpv6AddressRequest
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.InstanceIpv6AddressRequest where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

-- | Describes an IPv6 address.
--
-- /See:/ 'newInstanceIpv6AddressRequest' smart constructor.
data InstanceIpv6AddressRequest = InstanceIpv6AddressRequest'
  { -- | The IPv6 address.
    ipv6Address :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InstanceIpv6AddressRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ipv6Address', 'instanceIpv6AddressRequest_ipv6Address' - The IPv6 address.
newInstanceIpv6AddressRequest ::
  InstanceIpv6AddressRequest
newInstanceIpv6AddressRequest =
  InstanceIpv6AddressRequest'
    { ipv6Address =
        Prelude.Nothing
    }

-- | The IPv6 address.
instanceIpv6AddressRequest_ipv6Address :: Lens.Lens' InstanceIpv6AddressRequest (Prelude.Maybe Prelude.Text)
instanceIpv6AddressRequest_ipv6Address = Lens.lens (\InstanceIpv6AddressRequest' {ipv6Address} -> ipv6Address) (\s@InstanceIpv6AddressRequest' {} a -> s {ipv6Address = a} :: InstanceIpv6AddressRequest)

instance Prelude.Hashable InstanceIpv6AddressRequest where
  hashWithSalt _salt InstanceIpv6AddressRequest' {..} =
    _salt `Prelude.hashWithSalt` ipv6Address

instance Prelude.NFData InstanceIpv6AddressRequest where
  rnf InstanceIpv6AddressRequest' {..} =
    Prelude.rnf ipv6Address

instance Data.ToQuery InstanceIpv6AddressRequest where
  toQuery InstanceIpv6AddressRequest' {..} =
    Prelude.mconcat ["Ipv6Address" Data.=: ipv6Address]
