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
-- Module      : Network.AWS.ElastiCache.Types.Subnet
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElastiCache.Types.Subnet where

import qualified Network.AWS.Core as Core
import Network.AWS.ElastiCache.Types.AvailabilityZone
import Network.AWS.ElastiCache.Types.SubnetOutpost
import qualified Network.AWS.Lens as Lens

-- | Represents the subnet associated with a cluster. This parameter refers
-- to subnets defined in Amazon Virtual Private Cloud (Amazon VPC) and used
-- with ElastiCache.
--
-- /See:/ 'newSubnet' smart constructor.
data Subnet = Subnet'
  { -- | The unique identifier for the subnet.
    subnetIdentifier :: Core.Maybe Core.Text,
    -- | The Availability Zone associated with the subnet.
    subnetAvailabilityZone :: Core.Maybe AvailabilityZone,
    -- | The outpost ARN of the subnet.
    subnetOutpost :: Core.Maybe SubnetOutpost
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Subnet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'subnetIdentifier', 'subnet_subnetIdentifier' - The unique identifier for the subnet.
--
-- 'subnetAvailabilityZone', 'subnet_subnetAvailabilityZone' - The Availability Zone associated with the subnet.
--
-- 'subnetOutpost', 'subnet_subnetOutpost' - The outpost ARN of the subnet.
newSubnet ::
  Subnet
newSubnet =
  Subnet'
    { subnetIdentifier = Core.Nothing,
      subnetAvailabilityZone = Core.Nothing,
      subnetOutpost = Core.Nothing
    }

-- | The unique identifier for the subnet.
subnet_subnetIdentifier :: Lens.Lens' Subnet (Core.Maybe Core.Text)
subnet_subnetIdentifier = Lens.lens (\Subnet' {subnetIdentifier} -> subnetIdentifier) (\s@Subnet' {} a -> s {subnetIdentifier = a} :: Subnet)

-- | The Availability Zone associated with the subnet.
subnet_subnetAvailabilityZone :: Lens.Lens' Subnet (Core.Maybe AvailabilityZone)
subnet_subnetAvailabilityZone = Lens.lens (\Subnet' {subnetAvailabilityZone} -> subnetAvailabilityZone) (\s@Subnet' {} a -> s {subnetAvailabilityZone = a} :: Subnet)

-- | The outpost ARN of the subnet.
subnet_subnetOutpost :: Lens.Lens' Subnet (Core.Maybe SubnetOutpost)
subnet_subnetOutpost = Lens.lens (\Subnet' {subnetOutpost} -> subnetOutpost) (\s@Subnet' {} a -> s {subnetOutpost = a} :: Subnet)

instance Core.FromXML Subnet where
  parseXML x =
    Subnet'
      Core.<$> (x Core..@? "SubnetIdentifier")
      Core.<*> (x Core..@? "SubnetAvailabilityZone")
      Core.<*> (x Core..@? "SubnetOutpost")

instance Core.Hashable Subnet

instance Core.NFData Subnet
