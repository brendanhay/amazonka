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
-- Module      : Network.AWS.ElastiCache.Types.Subnet
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElastiCache.Types.Subnet where

import Network.AWS.ElastiCache.Types.AvailabilityZone
import Network.AWS.ElastiCache.Types.SubnetOutpost
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Represents the subnet associated with a cluster. This parameter refers
-- to subnets defined in Amazon Virtual Private Cloud (Amazon VPC) and used
-- with ElastiCache.
--
-- /See:/ 'newSubnet' smart constructor.
data Subnet = Subnet'
  { -- | The unique identifier for the subnet.
    subnetIdentifier :: Prelude.Maybe Prelude.Text,
    -- | The Availability Zone associated with the subnet.
    subnetAvailabilityZone :: Prelude.Maybe AvailabilityZone,
    -- | The outpost ARN of the subnet.
    subnetOutpost :: Prelude.Maybe SubnetOutpost
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { subnetIdentifier = Prelude.Nothing,
      subnetAvailabilityZone = Prelude.Nothing,
      subnetOutpost = Prelude.Nothing
    }

-- | The unique identifier for the subnet.
subnet_subnetIdentifier :: Lens.Lens' Subnet (Prelude.Maybe Prelude.Text)
subnet_subnetIdentifier = Lens.lens (\Subnet' {subnetIdentifier} -> subnetIdentifier) (\s@Subnet' {} a -> s {subnetIdentifier = a} :: Subnet)

-- | The Availability Zone associated with the subnet.
subnet_subnetAvailabilityZone :: Lens.Lens' Subnet (Prelude.Maybe AvailabilityZone)
subnet_subnetAvailabilityZone = Lens.lens (\Subnet' {subnetAvailabilityZone} -> subnetAvailabilityZone) (\s@Subnet' {} a -> s {subnetAvailabilityZone = a} :: Subnet)

-- | The outpost ARN of the subnet.
subnet_subnetOutpost :: Lens.Lens' Subnet (Prelude.Maybe SubnetOutpost)
subnet_subnetOutpost = Lens.lens (\Subnet' {subnetOutpost} -> subnetOutpost) (\s@Subnet' {} a -> s {subnetOutpost = a} :: Subnet)

instance Prelude.FromXML Subnet where
  parseXML x =
    Subnet'
      Prelude.<$> (x Prelude..@? "SubnetIdentifier")
      Prelude.<*> (x Prelude..@? "SubnetAvailabilityZone")
      Prelude.<*> (x Prelude..@? "SubnetOutpost")

instance Prelude.Hashable Subnet

instance Prelude.NFData Subnet
