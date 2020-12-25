{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.NatGatewayAddress
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.NatGatewayAddress
  ( NatGatewayAddress (..),

    -- * Smart constructor
    mkNatGatewayAddress,

    -- * Lenses
    ngaAllocationId,
    ngaNetworkInterfaceId,
    ngaPrivateIp,
    ngaPublicIp,
  )
where

import qualified Network.AWS.EC2.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the IP addresses and network interface associated with a NAT gateway.
--
-- /See:/ 'mkNatGatewayAddress' smart constructor.
data NatGatewayAddress = NatGatewayAddress'
  { -- | The allocation ID of the Elastic IP address that's associated with the NAT gateway.
    allocationId :: Core.Maybe Types.String,
    -- | The ID of the network interface associated with the NAT gateway.
    networkInterfaceId :: Core.Maybe Types.String,
    -- | The private IP address associated with the Elastic IP address.
    privateIp :: Core.Maybe Types.String,
    -- | The Elastic IP address associated with the NAT gateway.
    publicIp :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'NatGatewayAddress' value with any optional fields omitted.
mkNatGatewayAddress ::
  NatGatewayAddress
mkNatGatewayAddress =
  NatGatewayAddress'
    { allocationId = Core.Nothing,
      networkInterfaceId = Core.Nothing,
      privateIp = Core.Nothing,
      publicIp = Core.Nothing
    }

-- | The allocation ID of the Elastic IP address that's associated with the NAT gateway.
--
-- /Note:/ Consider using 'allocationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ngaAllocationId :: Lens.Lens' NatGatewayAddress (Core.Maybe Types.String)
ngaAllocationId = Lens.field @"allocationId"
{-# DEPRECATED ngaAllocationId "Use generic-lens or generic-optics with 'allocationId' instead." #-}

-- | The ID of the network interface associated with the NAT gateway.
--
-- /Note:/ Consider using 'networkInterfaceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ngaNetworkInterfaceId :: Lens.Lens' NatGatewayAddress (Core.Maybe Types.String)
ngaNetworkInterfaceId = Lens.field @"networkInterfaceId"
{-# DEPRECATED ngaNetworkInterfaceId "Use generic-lens or generic-optics with 'networkInterfaceId' instead." #-}

-- | The private IP address associated with the Elastic IP address.
--
-- /Note:/ Consider using 'privateIp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ngaPrivateIp :: Lens.Lens' NatGatewayAddress (Core.Maybe Types.String)
ngaPrivateIp = Lens.field @"privateIp"
{-# DEPRECATED ngaPrivateIp "Use generic-lens or generic-optics with 'privateIp' instead." #-}

-- | The Elastic IP address associated with the NAT gateway.
--
-- /Note:/ Consider using 'publicIp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ngaPublicIp :: Lens.Lens' NatGatewayAddress (Core.Maybe Types.String)
ngaPublicIp = Lens.field @"publicIp"
{-# DEPRECATED ngaPublicIp "Use generic-lens or generic-optics with 'publicIp' instead." #-}

instance Core.FromXML NatGatewayAddress where
  parseXML x =
    NatGatewayAddress'
      Core.<$> (x Core..@? "allocationId")
      Core.<*> (x Core..@? "networkInterfaceId")
      Core.<*> (x Core..@? "privateIp")
      Core.<*> (x Core..@? "publicIp")
