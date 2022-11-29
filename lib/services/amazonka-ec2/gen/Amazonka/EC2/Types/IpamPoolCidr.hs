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
-- Module      : Amazonka.EC2.Types.IpamPoolCidr
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.IpamPoolCidr where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.IpamPoolCidrFailureReason
import Amazonka.EC2.Types.IpamPoolCidrState
import qualified Amazonka.Prelude as Prelude

-- | A CIDR provisioned to an IPAM pool.
--
-- /See:/ 'newIpamPoolCidr' smart constructor.
data IpamPoolCidr = IpamPoolCidr'
  { -- | The CIDR provisioned to the IPAM pool. A CIDR is a representation of an
    -- IP address and its associated network mask (or netmask) and refers to a
    -- range of IP addresses. An IPv4 CIDR example is @10.24.34.0\/23@. An IPv6
    -- CIDR example is @2001:DB8::\/32@.
    cidr :: Prelude.Maybe Prelude.Text,
    -- | The state of the CIDR.
    state :: Prelude.Maybe IpamPoolCidrState,
    -- | Details related to why an IPAM pool CIDR failed to be provisioned.
    failureReason :: Prelude.Maybe IpamPoolCidrFailureReason
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'IpamPoolCidr' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cidr', 'ipamPoolCidr_cidr' - The CIDR provisioned to the IPAM pool. A CIDR is a representation of an
-- IP address and its associated network mask (or netmask) and refers to a
-- range of IP addresses. An IPv4 CIDR example is @10.24.34.0\/23@. An IPv6
-- CIDR example is @2001:DB8::\/32@.
--
-- 'state', 'ipamPoolCidr_state' - The state of the CIDR.
--
-- 'failureReason', 'ipamPoolCidr_failureReason' - Details related to why an IPAM pool CIDR failed to be provisioned.
newIpamPoolCidr ::
  IpamPoolCidr
newIpamPoolCidr =
  IpamPoolCidr'
    { cidr = Prelude.Nothing,
      state = Prelude.Nothing,
      failureReason = Prelude.Nothing
    }

-- | The CIDR provisioned to the IPAM pool. A CIDR is a representation of an
-- IP address and its associated network mask (or netmask) and refers to a
-- range of IP addresses. An IPv4 CIDR example is @10.24.34.0\/23@. An IPv6
-- CIDR example is @2001:DB8::\/32@.
ipamPoolCidr_cidr :: Lens.Lens' IpamPoolCidr (Prelude.Maybe Prelude.Text)
ipamPoolCidr_cidr = Lens.lens (\IpamPoolCidr' {cidr} -> cidr) (\s@IpamPoolCidr' {} a -> s {cidr = a} :: IpamPoolCidr)

-- | The state of the CIDR.
ipamPoolCidr_state :: Lens.Lens' IpamPoolCidr (Prelude.Maybe IpamPoolCidrState)
ipamPoolCidr_state = Lens.lens (\IpamPoolCidr' {state} -> state) (\s@IpamPoolCidr' {} a -> s {state = a} :: IpamPoolCidr)

-- | Details related to why an IPAM pool CIDR failed to be provisioned.
ipamPoolCidr_failureReason :: Lens.Lens' IpamPoolCidr (Prelude.Maybe IpamPoolCidrFailureReason)
ipamPoolCidr_failureReason = Lens.lens (\IpamPoolCidr' {failureReason} -> failureReason) (\s@IpamPoolCidr' {} a -> s {failureReason = a} :: IpamPoolCidr)

instance Core.FromXML IpamPoolCidr where
  parseXML x =
    IpamPoolCidr'
      Prelude.<$> (x Core..@? "cidr")
      Prelude.<*> (x Core..@? "state")
      Prelude.<*> (x Core..@? "failureReason")

instance Prelude.Hashable IpamPoolCidr where
  hashWithSalt _salt IpamPoolCidr' {..} =
    _salt `Prelude.hashWithSalt` cidr
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` failureReason

instance Prelude.NFData IpamPoolCidr where
  rnf IpamPoolCidr' {..} =
    Prelude.rnf cidr
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf failureReason
