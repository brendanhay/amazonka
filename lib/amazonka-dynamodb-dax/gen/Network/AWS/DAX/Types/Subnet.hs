{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DAX.Types.Subnet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DAX.Types.Subnet
  ( Subnet (..),

    -- * Smart constructor
    mkSubnet,

    -- * Lenses
    sSubnetAvailabilityZone,
    sSubnetIdentifier,
  )
where

import qualified Network.AWS.DAX.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents the subnet associated with a DAX cluster. This parameter refers to subnets defined in Amazon Virtual Private Cloud (Amazon VPC) and used with DAX.
--
-- /See:/ 'mkSubnet' smart constructor.
data Subnet = Subnet'
  { -- | The Availability Zone (AZ) for the subnet.
    subnetAvailabilityZone :: Core.Maybe Types.String,
    -- | The system-assigned identifier for the subnet.
    subnetIdentifier :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Subnet' value with any optional fields omitted.
mkSubnet ::
  Subnet
mkSubnet =
  Subnet'
    { subnetAvailabilityZone = Core.Nothing,
      subnetIdentifier = Core.Nothing
    }

-- | The Availability Zone (AZ) for the subnet.
--
-- /Note:/ Consider using 'subnetAvailabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sSubnetAvailabilityZone :: Lens.Lens' Subnet (Core.Maybe Types.String)
sSubnetAvailabilityZone = Lens.field @"subnetAvailabilityZone"
{-# DEPRECATED sSubnetAvailabilityZone "Use generic-lens or generic-optics with 'subnetAvailabilityZone' instead." #-}

-- | The system-assigned identifier for the subnet.
--
-- /Note:/ Consider using 'subnetIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sSubnetIdentifier :: Lens.Lens' Subnet (Core.Maybe Types.String)
sSubnetIdentifier = Lens.field @"subnetIdentifier"
{-# DEPRECATED sSubnetIdentifier "Use generic-lens or generic-optics with 'subnetIdentifier' instead." #-}

instance Core.FromJSON Subnet where
  parseJSON =
    Core.withObject "Subnet" Core.$
      \x ->
        Subnet'
          Core.<$> (x Core..:? "SubnetAvailabilityZone")
          Core.<*> (x Core..:? "SubnetIdentifier")
