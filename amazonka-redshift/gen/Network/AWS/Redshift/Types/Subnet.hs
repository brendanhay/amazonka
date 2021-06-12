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
-- Module      : Network.AWS.Redshift.Types.Subnet
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.Subnet where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Redshift.Internal
import Network.AWS.Redshift.Types.AvailabilityZone

-- | Describes a subnet.
--
-- /See:/ 'newSubnet' smart constructor.
data Subnet = Subnet'
  { -- | The status of the subnet.
    subnetStatus :: Core.Maybe Core.Text,
    -- | The identifier of the subnet.
    subnetIdentifier :: Core.Maybe Core.Text,
    subnetAvailabilityZone :: Core.Maybe AvailabilityZone
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
-- 'subnetStatus', 'subnet_subnetStatus' - The status of the subnet.
--
-- 'subnetIdentifier', 'subnet_subnetIdentifier' - The identifier of the subnet.
--
-- 'subnetAvailabilityZone', 'subnet_subnetAvailabilityZone' -
newSubnet ::
  Subnet
newSubnet =
  Subnet'
    { subnetStatus = Core.Nothing,
      subnetIdentifier = Core.Nothing,
      subnetAvailabilityZone = Core.Nothing
    }

-- | The status of the subnet.
subnet_subnetStatus :: Lens.Lens' Subnet (Core.Maybe Core.Text)
subnet_subnetStatus = Lens.lens (\Subnet' {subnetStatus} -> subnetStatus) (\s@Subnet' {} a -> s {subnetStatus = a} :: Subnet)

-- | The identifier of the subnet.
subnet_subnetIdentifier :: Lens.Lens' Subnet (Core.Maybe Core.Text)
subnet_subnetIdentifier = Lens.lens (\Subnet' {subnetIdentifier} -> subnetIdentifier) (\s@Subnet' {} a -> s {subnetIdentifier = a} :: Subnet)

-- |
subnet_subnetAvailabilityZone :: Lens.Lens' Subnet (Core.Maybe AvailabilityZone)
subnet_subnetAvailabilityZone = Lens.lens (\Subnet' {subnetAvailabilityZone} -> subnetAvailabilityZone) (\s@Subnet' {} a -> s {subnetAvailabilityZone = a} :: Subnet)

instance Core.FromXML Subnet where
  parseXML x =
    Subnet'
      Core.<$> (x Core..@? "SubnetStatus")
      Core.<*> (x Core..@? "SubnetIdentifier")
      Core.<*> (x Core..@? "SubnetAvailabilityZone")

instance Core.Hashable Subnet

instance Core.NFData Subnet
