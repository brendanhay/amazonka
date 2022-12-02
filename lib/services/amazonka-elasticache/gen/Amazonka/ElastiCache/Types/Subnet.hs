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
-- Module      : Amazonka.ElastiCache.Types.Subnet
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ElastiCache.Types.Subnet where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ElastiCache.Types.AvailabilityZone
import Amazonka.ElastiCache.Types.NetworkType
import Amazonka.ElastiCache.Types.SubnetOutpost
import qualified Amazonka.Prelude as Prelude

-- | Represents the subnet associated with a cluster. This parameter refers
-- to subnets defined in Amazon Virtual Private Cloud (Amazon VPC) and used
-- with ElastiCache.
--
-- /See:/ 'newSubnet' smart constructor.
data Subnet = Subnet'
  { -- | The outpost ARN of the subnet.
    subnetOutpost :: Prelude.Maybe SubnetOutpost,
    -- | The unique identifier for the subnet.
    subnetIdentifier :: Prelude.Maybe Prelude.Text,
    -- | The Availability Zone associated with the subnet.
    subnetAvailabilityZone :: Prelude.Maybe AvailabilityZone,
    -- | Either @ipv4@ | @ipv6@ | @dual_stack@. IPv6 is supported for workloads
    -- using Redis engine version 6.2 onward or Memcached engine version 1.6.6
    -- on all instances built on the
    -- <https://aws.amazon.com/ec2/nitro/ Nitro system>.
    supportedNetworkTypes :: Prelude.Maybe [NetworkType]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Subnet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'subnetOutpost', 'subnet_subnetOutpost' - The outpost ARN of the subnet.
--
-- 'subnetIdentifier', 'subnet_subnetIdentifier' - The unique identifier for the subnet.
--
-- 'subnetAvailabilityZone', 'subnet_subnetAvailabilityZone' - The Availability Zone associated with the subnet.
--
-- 'supportedNetworkTypes', 'subnet_supportedNetworkTypes' - Either @ipv4@ | @ipv6@ | @dual_stack@. IPv6 is supported for workloads
-- using Redis engine version 6.2 onward or Memcached engine version 1.6.6
-- on all instances built on the
-- <https://aws.amazon.com/ec2/nitro/ Nitro system>.
newSubnet ::
  Subnet
newSubnet =
  Subnet'
    { subnetOutpost = Prelude.Nothing,
      subnetIdentifier = Prelude.Nothing,
      subnetAvailabilityZone = Prelude.Nothing,
      supportedNetworkTypes = Prelude.Nothing
    }

-- | The outpost ARN of the subnet.
subnet_subnetOutpost :: Lens.Lens' Subnet (Prelude.Maybe SubnetOutpost)
subnet_subnetOutpost = Lens.lens (\Subnet' {subnetOutpost} -> subnetOutpost) (\s@Subnet' {} a -> s {subnetOutpost = a} :: Subnet)

-- | The unique identifier for the subnet.
subnet_subnetIdentifier :: Lens.Lens' Subnet (Prelude.Maybe Prelude.Text)
subnet_subnetIdentifier = Lens.lens (\Subnet' {subnetIdentifier} -> subnetIdentifier) (\s@Subnet' {} a -> s {subnetIdentifier = a} :: Subnet)

-- | The Availability Zone associated with the subnet.
subnet_subnetAvailabilityZone :: Lens.Lens' Subnet (Prelude.Maybe AvailabilityZone)
subnet_subnetAvailabilityZone = Lens.lens (\Subnet' {subnetAvailabilityZone} -> subnetAvailabilityZone) (\s@Subnet' {} a -> s {subnetAvailabilityZone = a} :: Subnet)

-- | Either @ipv4@ | @ipv6@ | @dual_stack@. IPv6 is supported for workloads
-- using Redis engine version 6.2 onward or Memcached engine version 1.6.6
-- on all instances built on the
-- <https://aws.amazon.com/ec2/nitro/ Nitro system>.
subnet_supportedNetworkTypes :: Lens.Lens' Subnet (Prelude.Maybe [NetworkType])
subnet_supportedNetworkTypes = Lens.lens (\Subnet' {supportedNetworkTypes} -> supportedNetworkTypes) (\s@Subnet' {} a -> s {supportedNetworkTypes = a} :: Subnet) Prelude.. Lens.mapping Lens.coerced

instance Data.FromXML Subnet where
  parseXML x =
    Subnet'
      Prelude.<$> (x Data..@? "SubnetOutpost")
      Prelude.<*> (x Data..@? "SubnetIdentifier")
      Prelude.<*> (x Data..@? "SubnetAvailabilityZone")
      Prelude.<*> ( x Data..@? "SupportedNetworkTypes"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "member")
                  )

instance Prelude.Hashable Subnet where
  hashWithSalt _salt Subnet' {..} =
    _salt `Prelude.hashWithSalt` subnetOutpost
      `Prelude.hashWithSalt` subnetIdentifier
      `Prelude.hashWithSalt` subnetAvailabilityZone
      `Prelude.hashWithSalt` supportedNetworkTypes

instance Prelude.NFData Subnet where
  rnf Subnet' {..} =
    Prelude.rnf subnetOutpost
      `Prelude.seq` Prelude.rnf subnetIdentifier
      `Prelude.seq` Prelude.rnf subnetAvailabilityZone
      `Prelude.seq` Prelude.rnf supportedNetworkTypes
