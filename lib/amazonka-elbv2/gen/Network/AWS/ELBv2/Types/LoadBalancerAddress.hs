{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELBv2.Types.LoadBalancerAddress
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ELBv2.Types.LoadBalancerAddress
  ( LoadBalancerAddress (..),

    -- * Smart constructor
    mkLoadBalancerAddress,

    -- * Lenses
    lbaIPv6Address,
    lbaIPAddress,
    lbaAllocationId,
    lbaPrivateIPv4Address,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about a static IP address for a load balancer.
--
-- /See:/ 'mkLoadBalancerAddress' smart constructor.
data LoadBalancerAddress = LoadBalancerAddress'
  { -- | [Network Load Balancers] The IPv6 address.
    ipv6Address :: Lude.Maybe Lude.Text,
    -- | The static IP address.
    ipAddress :: Lude.Maybe Lude.Text,
    -- | [Network Load Balancers] The allocation ID of the Elastic IP address for an internal-facing load balancer.
    allocationId :: Lude.Maybe Lude.Text,
    -- | [Network Load Balancers] The private IPv4 address for an internal load balancer.
    privateIPv4Address :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'LoadBalancerAddress' with the minimum fields required to make a request.
--
-- * 'ipv6Address' - [Network Load Balancers] The IPv6 address.
-- * 'ipAddress' - The static IP address.
-- * 'allocationId' - [Network Load Balancers] The allocation ID of the Elastic IP address for an internal-facing load balancer.
-- * 'privateIPv4Address' - [Network Load Balancers] The private IPv4 address for an internal load balancer.
mkLoadBalancerAddress ::
  LoadBalancerAddress
mkLoadBalancerAddress =
  LoadBalancerAddress'
    { ipv6Address = Lude.Nothing,
      ipAddress = Lude.Nothing,
      allocationId = Lude.Nothing,
      privateIPv4Address = Lude.Nothing
    }

-- | [Network Load Balancers] The IPv6 address.
--
-- /Note:/ Consider using 'ipv6Address' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbaIPv6Address :: Lens.Lens' LoadBalancerAddress (Lude.Maybe Lude.Text)
lbaIPv6Address = Lens.lens (ipv6Address :: LoadBalancerAddress -> Lude.Maybe Lude.Text) (\s a -> s {ipv6Address = a} :: LoadBalancerAddress)
{-# DEPRECATED lbaIPv6Address "Use generic-lens or generic-optics with 'ipv6Address' instead." #-}

-- | The static IP address.
--
-- /Note:/ Consider using 'ipAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbaIPAddress :: Lens.Lens' LoadBalancerAddress (Lude.Maybe Lude.Text)
lbaIPAddress = Lens.lens (ipAddress :: LoadBalancerAddress -> Lude.Maybe Lude.Text) (\s a -> s {ipAddress = a} :: LoadBalancerAddress)
{-# DEPRECATED lbaIPAddress "Use generic-lens or generic-optics with 'ipAddress' instead." #-}

-- | [Network Load Balancers] The allocation ID of the Elastic IP address for an internal-facing load balancer.
--
-- /Note:/ Consider using 'allocationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbaAllocationId :: Lens.Lens' LoadBalancerAddress (Lude.Maybe Lude.Text)
lbaAllocationId = Lens.lens (allocationId :: LoadBalancerAddress -> Lude.Maybe Lude.Text) (\s a -> s {allocationId = a} :: LoadBalancerAddress)
{-# DEPRECATED lbaAllocationId "Use generic-lens or generic-optics with 'allocationId' instead." #-}

-- | [Network Load Balancers] The private IPv4 address for an internal load balancer.
--
-- /Note:/ Consider using 'privateIPv4Address' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbaPrivateIPv4Address :: Lens.Lens' LoadBalancerAddress (Lude.Maybe Lude.Text)
lbaPrivateIPv4Address = Lens.lens (privateIPv4Address :: LoadBalancerAddress -> Lude.Maybe Lude.Text) (\s a -> s {privateIPv4Address = a} :: LoadBalancerAddress)
{-# DEPRECATED lbaPrivateIPv4Address "Use generic-lens or generic-optics with 'privateIPv4Address' instead." #-}

instance Lude.FromXML LoadBalancerAddress where
  parseXML x =
    LoadBalancerAddress'
      Lude.<$> (x Lude..@? "IPv6Address")
      Lude.<*> (x Lude..@? "IpAddress")
      Lude.<*> (x Lude..@? "AllocationId")
      Lude.<*> (x Lude..@? "PrivateIPv4Address")
