-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELBv2.Types.SubnetMapping
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ELBv2.Types.SubnetMapping
  ( SubnetMapping (..),

    -- * Smart constructor
    mkSubnetMapping,

    -- * Lenses
    smIPv6Address,
    smAllocationId,
    smPrivateIPv4Address,
    smSubnetId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about a subnet mapping.
--
-- /See:/ 'mkSubnetMapping' smart constructor.
data SubnetMapping = SubnetMapping'
  { ipv6Address ::
      Lude.Maybe Lude.Text,
    allocationId :: Lude.Maybe Lude.Text,
    privateIPv4Address :: Lude.Maybe Lude.Text,
    subnetId :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SubnetMapping' with the minimum fields required to make a request.
--
-- * 'allocationId' - [Network Load Balancers] The allocation ID of the Elastic IP address for an internet-facing load balancer.
-- * 'ipv6Address' - [Network Load Balancers] The IPv6 address.
-- * 'privateIPv4Address' - [Network Load Balancers] The private IPv4 address for an internal load balancer.
-- * 'subnetId' - The ID of the subnet.
mkSubnetMapping ::
  SubnetMapping
mkSubnetMapping =
  SubnetMapping'
    { ipv6Address = Lude.Nothing,
      allocationId = Lude.Nothing,
      privateIPv4Address = Lude.Nothing,
      subnetId = Lude.Nothing
    }

-- | [Network Load Balancers] The IPv6 address.
--
-- /Note:/ Consider using 'ipv6Address' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smIPv6Address :: Lens.Lens' SubnetMapping (Lude.Maybe Lude.Text)
smIPv6Address = Lens.lens (ipv6Address :: SubnetMapping -> Lude.Maybe Lude.Text) (\s a -> s {ipv6Address = a} :: SubnetMapping)
{-# DEPRECATED smIPv6Address "Use generic-lens or generic-optics with 'ipv6Address' instead." #-}

-- | [Network Load Balancers] The allocation ID of the Elastic IP address for an internet-facing load balancer.
--
-- /Note:/ Consider using 'allocationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smAllocationId :: Lens.Lens' SubnetMapping (Lude.Maybe Lude.Text)
smAllocationId = Lens.lens (allocationId :: SubnetMapping -> Lude.Maybe Lude.Text) (\s a -> s {allocationId = a} :: SubnetMapping)
{-# DEPRECATED smAllocationId "Use generic-lens or generic-optics with 'allocationId' instead." #-}

-- | [Network Load Balancers] The private IPv4 address for an internal load balancer.
--
-- /Note:/ Consider using 'privateIPv4Address' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smPrivateIPv4Address :: Lens.Lens' SubnetMapping (Lude.Maybe Lude.Text)
smPrivateIPv4Address = Lens.lens (privateIPv4Address :: SubnetMapping -> Lude.Maybe Lude.Text) (\s a -> s {privateIPv4Address = a} :: SubnetMapping)
{-# DEPRECATED smPrivateIPv4Address "Use generic-lens or generic-optics with 'privateIPv4Address' instead." #-}

-- | The ID of the subnet.
--
-- /Note:/ Consider using 'subnetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smSubnetId :: Lens.Lens' SubnetMapping (Lude.Maybe Lude.Text)
smSubnetId = Lens.lens (subnetId :: SubnetMapping -> Lude.Maybe Lude.Text) (\s a -> s {subnetId = a} :: SubnetMapping)
{-# DEPRECATED smSubnetId "Use generic-lens or generic-optics with 'subnetId' instead." #-}

instance Lude.ToQuery SubnetMapping where
  toQuery SubnetMapping' {..} =
    Lude.mconcat
      [ "IPv6Address" Lude.=: ipv6Address,
        "AllocationId" Lude.=: allocationId,
        "PrivateIPv4Address" Lude.=: privateIPv4Address,
        "SubnetId" Lude.=: subnetId
      ]
