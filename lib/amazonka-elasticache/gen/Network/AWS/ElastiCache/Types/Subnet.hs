-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.Types.Subnet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElastiCache.Types.Subnet
  ( Subnet (..),

    -- * Smart constructor
    mkSubnet,

    -- * Lenses
    sSubnetIdentifier,
    sSubnetAvailabilityZone,
    sSubnetOutpost,
  )
where

import Network.AWS.ElastiCache.Types.AvailabilityZone
import Network.AWS.ElastiCache.Types.SubnetOutpost
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents the subnet associated with a cluster. This parameter refers to subnets defined in Amazon Virtual Private Cloud (Amazon VPC) and used with ElastiCache.
--
-- /See:/ 'mkSubnet' smart constructor.
data Subnet = Subnet'
  { subnetIdentifier :: Lude.Maybe Lude.Text,
    subnetAvailabilityZone :: Lude.Maybe AvailabilityZone,
    subnetOutpost :: Lude.Maybe SubnetOutpost
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Subnet' with the minimum fields required to make a request.
--
-- * 'subnetAvailabilityZone' - The Availability Zone associated with the subnet.
-- * 'subnetIdentifier' - The unique identifier for the subnet.
-- * 'subnetOutpost' - The outpost ARN of the subnet.
mkSubnet ::
  Subnet
mkSubnet =
  Subnet'
    { subnetIdentifier = Lude.Nothing,
      subnetAvailabilityZone = Lude.Nothing,
      subnetOutpost = Lude.Nothing
    }

-- | The unique identifier for the subnet.
--
-- /Note:/ Consider using 'subnetIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sSubnetIdentifier :: Lens.Lens' Subnet (Lude.Maybe Lude.Text)
sSubnetIdentifier = Lens.lens (subnetIdentifier :: Subnet -> Lude.Maybe Lude.Text) (\s a -> s {subnetIdentifier = a} :: Subnet)
{-# DEPRECATED sSubnetIdentifier "Use generic-lens or generic-optics with 'subnetIdentifier' instead." #-}

-- | The Availability Zone associated with the subnet.
--
-- /Note:/ Consider using 'subnetAvailabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sSubnetAvailabilityZone :: Lens.Lens' Subnet (Lude.Maybe AvailabilityZone)
sSubnetAvailabilityZone = Lens.lens (subnetAvailabilityZone :: Subnet -> Lude.Maybe AvailabilityZone) (\s a -> s {subnetAvailabilityZone = a} :: Subnet)
{-# DEPRECATED sSubnetAvailabilityZone "Use generic-lens or generic-optics with 'subnetAvailabilityZone' instead." #-}

-- | The outpost ARN of the subnet.
--
-- /Note:/ Consider using 'subnetOutpost' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sSubnetOutpost :: Lens.Lens' Subnet (Lude.Maybe SubnetOutpost)
sSubnetOutpost = Lens.lens (subnetOutpost :: Subnet -> Lude.Maybe SubnetOutpost) (\s a -> s {subnetOutpost = a} :: Subnet)
{-# DEPRECATED sSubnetOutpost "Use generic-lens or generic-optics with 'subnetOutpost' instead." #-}

instance Lude.FromXML Subnet where
  parseXML x =
    Subnet'
      Lude.<$> (x Lude..@? "SubnetIdentifier")
      Lude.<*> (x Lude..@? "SubnetAvailabilityZone")
      Lude.<*> (x Lude..@? "SubnetOutpost")
