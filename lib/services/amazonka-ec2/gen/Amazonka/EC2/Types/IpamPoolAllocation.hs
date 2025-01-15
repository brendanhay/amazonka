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
-- Module      : Amazonka.EC2.Types.IpamPoolAllocation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.IpamPoolAllocation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.IpamPoolAllocationResourceType
import qualified Amazonka.Prelude as Prelude

-- | In IPAM, an allocation is a CIDR assignment from an IPAM pool to another
-- resource or IPAM pool.
--
-- /See:/ 'newIpamPoolAllocation' smart constructor.
data IpamPoolAllocation = IpamPoolAllocation'
  { -- | The CIDR for the allocation. A CIDR is a representation of an IP address
    -- and its associated network mask (or netmask) and refers to a range of IP
    -- addresses. An IPv4 CIDR example is @10.24.34.0\/23@. An IPv6 CIDR
    -- example is @2001:DB8::\/32@.
    cidr :: Prelude.Maybe Prelude.Text,
    -- | A description of the pool allocation.
    description :: Prelude.Maybe Prelude.Text,
    -- | The ID of an allocation.
    ipamPoolAllocationId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the resource.
    resourceId :: Prelude.Maybe Prelude.Text,
    -- | The owner of the resource.
    resourceOwner :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services Region of the resource.
    resourceRegion :: Prelude.Maybe Prelude.Text,
    -- | The type of the resource.
    resourceType :: Prelude.Maybe IpamPoolAllocationResourceType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'IpamPoolAllocation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cidr', 'ipamPoolAllocation_cidr' - The CIDR for the allocation. A CIDR is a representation of an IP address
-- and its associated network mask (or netmask) and refers to a range of IP
-- addresses. An IPv4 CIDR example is @10.24.34.0\/23@. An IPv6 CIDR
-- example is @2001:DB8::\/32@.
--
-- 'description', 'ipamPoolAllocation_description' - A description of the pool allocation.
--
-- 'ipamPoolAllocationId', 'ipamPoolAllocation_ipamPoolAllocationId' - The ID of an allocation.
--
-- 'resourceId', 'ipamPoolAllocation_resourceId' - The ID of the resource.
--
-- 'resourceOwner', 'ipamPoolAllocation_resourceOwner' - The owner of the resource.
--
-- 'resourceRegion', 'ipamPoolAllocation_resourceRegion' - The Amazon Web Services Region of the resource.
--
-- 'resourceType', 'ipamPoolAllocation_resourceType' - The type of the resource.
newIpamPoolAllocation ::
  IpamPoolAllocation
newIpamPoolAllocation =
  IpamPoolAllocation'
    { cidr = Prelude.Nothing,
      description = Prelude.Nothing,
      ipamPoolAllocationId = Prelude.Nothing,
      resourceId = Prelude.Nothing,
      resourceOwner = Prelude.Nothing,
      resourceRegion = Prelude.Nothing,
      resourceType = Prelude.Nothing
    }

-- | The CIDR for the allocation. A CIDR is a representation of an IP address
-- and its associated network mask (or netmask) and refers to a range of IP
-- addresses. An IPv4 CIDR example is @10.24.34.0\/23@. An IPv6 CIDR
-- example is @2001:DB8::\/32@.
ipamPoolAllocation_cidr :: Lens.Lens' IpamPoolAllocation (Prelude.Maybe Prelude.Text)
ipamPoolAllocation_cidr = Lens.lens (\IpamPoolAllocation' {cidr} -> cidr) (\s@IpamPoolAllocation' {} a -> s {cidr = a} :: IpamPoolAllocation)

-- | A description of the pool allocation.
ipamPoolAllocation_description :: Lens.Lens' IpamPoolAllocation (Prelude.Maybe Prelude.Text)
ipamPoolAllocation_description = Lens.lens (\IpamPoolAllocation' {description} -> description) (\s@IpamPoolAllocation' {} a -> s {description = a} :: IpamPoolAllocation)

-- | The ID of an allocation.
ipamPoolAllocation_ipamPoolAllocationId :: Lens.Lens' IpamPoolAllocation (Prelude.Maybe Prelude.Text)
ipamPoolAllocation_ipamPoolAllocationId = Lens.lens (\IpamPoolAllocation' {ipamPoolAllocationId} -> ipamPoolAllocationId) (\s@IpamPoolAllocation' {} a -> s {ipamPoolAllocationId = a} :: IpamPoolAllocation)

-- | The ID of the resource.
ipamPoolAllocation_resourceId :: Lens.Lens' IpamPoolAllocation (Prelude.Maybe Prelude.Text)
ipamPoolAllocation_resourceId = Lens.lens (\IpamPoolAllocation' {resourceId} -> resourceId) (\s@IpamPoolAllocation' {} a -> s {resourceId = a} :: IpamPoolAllocation)

-- | The owner of the resource.
ipamPoolAllocation_resourceOwner :: Lens.Lens' IpamPoolAllocation (Prelude.Maybe Prelude.Text)
ipamPoolAllocation_resourceOwner = Lens.lens (\IpamPoolAllocation' {resourceOwner} -> resourceOwner) (\s@IpamPoolAllocation' {} a -> s {resourceOwner = a} :: IpamPoolAllocation)

-- | The Amazon Web Services Region of the resource.
ipamPoolAllocation_resourceRegion :: Lens.Lens' IpamPoolAllocation (Prelude.Maybe Prelude.Text)
ipamPoolAllocation_resourceRegion = Lens.lens (\IpamPoolAllocation' {resourceRegion} -> resourceRegion) (\s@IpamPoolAllocation' {} a -> s {resourceRegion = a} :: IpamPoolAllocation)

-- | The type of the resource.
ipamPoolAllocation_resourceType :: Lens.Lens' IpamPoolAllocation (Prelude.Maybe IpamPoolAllocationResourceType)
ipamPoolAllocation_resourceType = Lens.lens (\IpamPoolAllocation' {resourceType} -> resourceType) (\s@IpamPoolAllocation' {} a -> s {resourceType = a} :: IpamPoolAllocation)

instance Data.FromXML IpamPoolAllocation where
  parseXML x =
    IpamPoolAllocation'
      Prelude.<$> (x Data..@? "cidr")
      Prelude.<*> (x Data..@? "description")
      Prelude.<*> (x Data..@? "ipamPoolAllocationId")
      Prelude.<*> (x Data..@? "resourceId")
      Prelude.<*> (x Data..@? "resourceOwner")
      Prelude.<*> (x Data..@? "resourceRegion")
      Prelude.<*> (x Data..@? "resourceType")

instance Prelude.Hashable IpamPoolAllocation where
  hashWithSalt _salt IpamPoolAllocation' {..} =
    _salt
      `Prelude.hashWithSalt` cidr
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` ipamPoolAllocationId
      `Prelude.hashWithSalt` resourceId
      `Prelude.hashWithSalt` resourceOwner
      `Prelude.hashWithSalt` resourceRegion
      `Prelude.hashWithSalt` resourceType

instance Prelude.NFData IpamPoolAllocation where
  rnf IpamPoolAllocation' {..} =
    Prelude.rnf cidr `Prelude.seq`
      Prelude.rnf description `Prelude.seq`
        Prelude.rnf ipamPoolAllocationId `Prelude.seq`
          Prelude.rnf resourceId `Prelude.seq`
            Prelude.rnf resourceOwner `Prelude.seq`
              Prelude.rnf resourceRegion `Prelude.seq`
                Prelude.rnf resourceType
