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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.IpamPoolCidr where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
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
    -- | Details related to why an IPAM pool CIDR failed to be provisioned.
    failureReason :: Prelude.Maybe IpamPoolCidrFailureReason,
    -- | The IPAM pool CIDR ID.
    ipamPoolCidrId :: Prelude.Maybe Prelude.Text,
    -- | The netmask length of the CIDR you\'d like to provision to a pool. Can
    -- be used for provisioning Amazon-provided IPv6 CIDRs to top-level pools
    -- and for provisioning CIDRs to pools with source pools. Cannot be used to
    -- provision BYOIP CIDRs to top-level pools. \"NetmaskLength\" or \"Cidr\"
    -- is required.
    netmaskLength :: Prelude.Maybe Prelude.Int,
    -- | The state of the CIDR.
    state :: Prelude.Maybe IpamPoolCidrState
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
-- 'failureReason', 'ipamPoolCidr_failureReason' - Details related to why an IPAM pool CIDR failed to be provisioned.
--
-- 'ipamPoolCidrId', 'ipamPoolCidr_ipamPoolCidrId' - The IPAM pool CIDR ID.
--
-- 'netmaskLength', 'ipamPoolCidr_netmaskLength' - The netmask length of the CIDR you\'d like to provision to a pool. Can
-- be used for provisioning Amazon-provided IPv6 CIDRs to top-level pools
-- and for provisioning CIDRs to pools with source pools. Cannot be used to
-- provision BYOIP CIDRs to top-level pools. \"NetmaskLength\" or \"Cidr\"
-- is required.
--
-- 'state', 'ipamPoolCidr_state' - The state of the CIDR.
newIpamPoolCidr ::
  IpamPoolCidr
newIpamPoolCidr =
  IpamPoolCidr'
    { cidr = Prelude.Nothing,
      failureReason = Prelude.Nothing,
      ipamPoolCidrId = Prelude.Nothing,
      netmaskLength = Prelude.Nothing,
      state = Prelude.Nothing
    }

-- | The CIDR provisioned to the IPAM pool. A CIDR is a representation of an
-- IP address and its associated network mask (or netmask) and refers to a
-- range of IP addresses. An IPv4 CIDR example is @10.24.34.0\/23@. An IPv6
-- CIDR example is @2001:DB8::\/32@.
ipamPoolCidr_cidr :: Lens.Lens' IpamPoolCidr (Prelude.Maybe Prelude.Text)
ipamPoolCidr_cidr = Lens.lens (\IpamPoolCidr' {cidr} -> cidr) (\s@IpamPoolCidr' {} a -> s {cidr = a} :: IpamPoolCidr)

-- | Details related to why an IPAM pool CIDR failed to be provisioned.
ipamPoolCidr_failureReason :: Lens.Lens' IpamPoolCidr (Prelude.Maybe IpamPoolCidrFailureReason)
ipamPoolCidr_failureReason = Lens.lens (\IpamPoolCidr' {failureReason} -> failureReason) (\s@IpamPoolCidr' {} a -> s {failureReason = a} :: IpamPoolCidr)

-- | The IPAM pool CIDR ID.
ipamPoolCidr_ipamPoolCidrId :: Lens.Lens' IpamPoolCidr (Prelude.Maybe Prelude.Text)
ipamPoolCidr_ipamPoolCidrId = Lens.lens (\IpamPoolCidr' {ipamPoolCidrId} -> ipamPoolCidrId) (\s@IpamPoolCidr' {} a -> s {ipamPoolCidrId = a} :: IpamPoolCidr)

-- | The netmask length of the CIDR you\'d like to provision to a pool. Can
-- be used for provisioning Amazon-provided IPv6 CIDRs to top-level pools
-- and for provisioning CIDRs to pools with source pools. Cannot be used to
-- provision BYOIP CIDRs to top-level pools. \"NetmaskLength\" or \"Cidr\"
-- is required.
ipamPoolCidr_netmaskLength :: Lens.Lens' IpamPoolCidr (Prelude.Maybe Prelude.Int)
ipamPoolCidr_netmaskLength = Lens.lens (\IpamPoolCidr' {netmaskLength} -> netmaskLength) (\s@IpamPoolCidr' {} a -> s {netmaskLength = a} :: IpamPoolCidr)

-- | The state of the CIDR.
ipamPoolCidr_state :: Lens.Lens' IpamPoolCidr (Prelude.Maybe IpamPoolCidrState)
ipamPoolCidr_state = Lens.lens (\IpamPoolCidr' {state} -> state) (\s@IpamPoolCidr' {} a -> s {state = a} :: IpamPoolCidr)

instance Data.FromXML IpamPoolCidr where
  parseXML x =
    IpamPoolCidr'
      Prelude.<$> (x Data..@? "cidr")
      Prelude.<*> (x Data..@? "failureReason")
      Prelude.<*> (x Data..@? "ipamPoolCidrId")
      Prelude.<*> (x Data..@? "netmaskLength")
      Prelude.<*> (x Data..@? "state")

instance Prelude.Hashable IpamPoolCidr where
  hashWithSalt _salt IpamPoolCidr' {..} =
    _salt
      `Prelude.hashWithSalt` cidr
      `Prelude.hashWithSalt` failureReason
      `Prelude.hashWithSalt` ipamPoolCidrId
      `Prelude.hashWithSalt` netmaskLength
      `Prelude.hashWithSalt` state

instance Prelude.NFData IpamPoolCidr where
  rnf IpamPoolCidr' {..} =
    Prelude.rnf cidr
      `Prelude.seq` Prelude.rnf failureReason
      `Prelude.seq` Prelude.rnf ipamPoolCidrId
      `Prelude.seq` Prelude.rnf netmaskLength
      `Prelude.seq` Prelude.rnf state
