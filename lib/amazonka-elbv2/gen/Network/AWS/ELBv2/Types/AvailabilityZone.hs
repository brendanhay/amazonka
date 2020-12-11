-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELBv2.Types.AvailabilityZone
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ELBv2.Types.AvailabilityZone
  ( AvailabilityZone (..),

    -- * Smart constructor
    mkAvailabilityZone,

    -- * Lenses
    azSubnetId,
    azZoneName,
    azLoadBalancerAddresses,
    azOutpostId,
  )
where

import Network.AWS.ELBv2.Types.LoadBalancerAddress
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about an Availability Zone.
--
-- /See:/ 'mkAvailabilityZone' smart constructor.
data AvailabilityZone = AvailabilityZone'
  { subnetId ::
      Lude.Maybe Lude.Text,
    zoneName :: Lude.Maybe Lude.Text,
    loadBalancerAddresses :: Lude.Maybe [LoadBalancerAddress],
    outpostId :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AvailabilityZone' with the minimum fields required to make a request.
--
-- * 'loadBalancerAddresses' - [Network Load Balancers] If you need static IP addresses for your load balancer, you can specify one Elastic IP address per Availability Zone when you create an internal-facing load balancer. For internal load balancers, you can specify a private IP address from the IPv4 range of the subnet.
-- * 'outpostId' - [Application Load Balancers on Outposts] The ID of the Outpost.
-- * 'subnetId' - The ID of the subnet. You can specify one subnet per Availability Zone.
-- * 'zoneName' - The name of the Availability Zone.
mkAvailabilityZone ::
  AvailabilityZone
mkAvailabilityZone =
  AvailabilityZone'
    { subnetId = Lude.Nothing,
      zoneName = Lude.Nothing,
      loadBalancerAddresses = Lude.Nothing,
      outpostId = Lude.Nothing
    }

-- | The ID of the subnet. You can specify one subnet per Availability Zone.
--
-- /Note:/ Consider using 'subnetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
azSubnetId :: Lens.Lens' AvailabilityZone (Lude.Maybe Lude.Text)
azSubnetId = Lens.lens (subnetId :: AvailabilityZone -> Lude.Maybe Lude.Text) (\s a -> s {subnetId = a} :: AvailabilityZone)
{-# DEPRECATED azSubnetId "Use generic-lens or generic-optics with 'subnetId' instead." #-}

-- | The name of the Availability Zone.
--
-- /Note:/ Consider using 'zoneName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
azZoneName :: Lens.Lens' AvailabilityZone (Lude.Maybe Lude.Text)
azZoneName = Lens.lens (zoneName :: AvailabilityZone -> Lude.Maybe Lude.Text) (\s a -> s {zoneName = a} :: AvailabilityZone)
{-# DEPRECATED azZoneName "Use generic-lens or generic-optics with 'zoneName' instead." #-}

-- | [Network Load Balancers] If you need static IP addresses for your load balancer, you can specify one Elastic IP address per Availability Zone when you create an internal-facing load balancer. For internal load balancers, you can specify a private IP address from the IPv4 range of the subnet.
--
-- /Note:/ Consider using 'loadBalancerAddresses' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
azLoadBalancerAddresses :: Lens.Lens' AvailabilityZone (Lude.Maybe [LoadBalancerAddress])
azLoadBalancerAddresses = Lens.lens (loadBalancerAddresses :: AvailabilityZone -> Lude.Maybe [LoadBalancerAddress]) (\s a -> s {loadBalancerAddresses = a} :: AvailabilityZone)
{-# DEPRECATED azLoadBalancerAddresses "Use generic-lens or generic-optics with 'loadBalancerAddresses' instead." #-}

-- | [Application Load Balancers on Outposts] The ID of the Outpost.
--
-- /Note:/ Consider using 'outpostId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
azOutpostId :: Lens.Lens' AvailabilityZone (Lude.Maybe Lude.Text)
azOutpostId = Lens.lens (outpostId :: AvailabilityZone -> Lude.Maybe Lude.Text) (\s a -> s {outpostId = a} :: AvailabilityZone)
{-# DEPRECATED azOutpostId "Use generic-lens or generic-optics with 'outpostId' instead." #-}

instance Lude.FromXML AvailabilityZone where
  parseXML x =
    AvailabilityZone'
      Lude.<$> (x Lude..@? "SubnetId")
      Lude.<*> (x Lude..@? "ZoneName")
      Lude.<*> ( x Lude..@? "LoadBalancerAddresses" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "member")
               )
      Lude.<*> (x Lude..@? "OutpostId")
