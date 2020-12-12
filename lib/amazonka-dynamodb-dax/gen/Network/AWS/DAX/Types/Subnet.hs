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
    sSubnetIdentifier,
    sSubnetAvailabilityZone,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents the subnet associated with a DAX cluster. This parameter refers to subnets defined in Amazon Virtual Private Cloud (Amazon VPC) and used with DAX.
--
-- /See:/ 'mkSubnet' smart constructor.
data Subnet = Subnet'
  { subnetIdentifier :: Lude.Maybe Lude.Text,
    subnetAvailabilityZone :: Lude.Maybe Lude.Text
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
-- * 'subnetAvailabilityZone' - The Availability Zone (AZ) for the subnet.
-- * 'subnetIdentifier' - The system-assigned identifier for the subnet.
mkSubnet ::
  Subnet
mkSubnet =
  Subnet'
    { subnetIdentifier = Lude.Nothing,
      subnetAvailabilityZone = Lude.Nothing
    }

-- | The system-assigned identifier for the subnet.
--
-- /Note:/ Consider using 'subnetIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sSubnetIdentifier :: Lens.Lens' Subnet (Lude.Maybe Lude.Text)
sSubnetIdentifier = Lens.lens (subnetIdentifier :: Subnet -> Lude.Maybe Lude.Text) (\s a -> s {subnetIdentifier = a} :: Subnet)
{-# DEPRECATED sSubnetIdentifier "Use generic-lens or generic-optics with 'subnetIdentifier' instead." #-}

-- | The Availability Zone (AZ) for the subnet.
--
-- /Note:/ Consider using 'subnetAvailabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sSubnetAvailabilityZone :: Lens.Lens' Subnet (Lude.Maybe Lude.Text)
sSubnetAvailabilityZone = Lens.lens (subnetAvailabilityZone :: Subnet -> Lude.Maybe Lude.Text) (\s a -> s {subnetAvailabilityZone = a} :: Subnet)
{-# DEPRECATED sSubnetAvailabilityZone "Use generic-lens or generic-optics with 'subnetAvailabilityZone' instead." #-}

instance Lude.FromJSON Subnet where
  parseJSON =
    Lude.withObject
      "Subnet"
      ( \x ->
          Subnet'
            Lude.<$> (x Lude..:? "SubnetIdentifier")
            Lude.<*> (x Lude..:? "SubnetAvailabilityZone")
      )
