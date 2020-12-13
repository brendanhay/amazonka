{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.SpotPrice
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.SpotPrice
  ( SpotPrice (..),

    -- * Smart constructor
    mkSpotPrice,

    -- * Lenses
    spfProductDescription,
    spfSpotPrice,
    spfInstanceType,
    spfAvailabilityZone,
    spfTimestamp,
  )
where

import Network.AWS.EC2.Types.InstanceType
import Network.AWS.EC2.Types.RIProductDescription
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the maximum price per hour that you are willing to pay for a Spot Instance.
--
-- /See:/ 'mkSpotPrice' smart constructor.
data SpotPrice = SpotPrice'
  { -- | A general description of the AMI.
    productDescription :: Lude.Maybe RIProductDescription,
    -- | The maximum price per hour that you are willing to pay for a Spot Instance.
    spotPrice :: Lude.Maybe Lude.Text,
    -- | The instance type.
    instanceType :: Lude.Maybe InstanceType,
    -- | The Availability Zone.
    availabilityZone :: Lude.Maybe Lude.Text,
    -- | The date and time the request was created, in UTC format (for example, /YYYY/ -/MM/ -/DD/ T/HH/ :/MM/ :/SS/ Z).
    timestamp :: Lude.Maybe Lude.DateTime
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SpotPrice' with the minimum fields required to make a request.
--
-- * 'productDescription' - A general description of the AMI.
-- * 'spotPrice' - The maximum price per hour that you are willing to pay for a Spot Instance.
-- * 'instanceType' - The instance type.
-- * 'availabilityZone' - The Availability Zone.
-- * 'timestamp' - The date and time the request was created, in UTC format (for example, /YYYY/ -/MM/ -/DD/ T/HH/ :/MM/ :/SS/ Z).
mkSpotPrice ::
  SpotPrice
mkSpotPrice =
  SpotPrice'
    { productDescription = Lude.Nothing,
      spotPrice = Lude.Nothing,
      instanceType = Lude.Nothing,
      availabilityZone = Lude.Nothing,
      timestamp = Lude.Nothing
    }

-- | A general description of the AMI.
--
-- /Note:/ Consider using 'productDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spfProductDescription :: Lens.Lens' SpotPrice (Lude.Maybe RIProductDescription)
spfProductDescription = Lens.lens (productDescription :: SpotPrice -> Lude.Maybe RIProductDescription) (\s a -> s {productDescription = a} :: SpotPrice)
{-# DEPRECATED spfProductDescription "Use generic-lens or generic-optics with 'productDescription' instead." #-}

-- | The maximum price per hour that you are willing to pay for a Spot Instance.
--
-- /Note:/ Consider using 'spotPrice' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spfSpotPrice :: Lens.Lens' SpotPrice (Lude.Maybe Lude.Text)
spfSpotPrice = Lens.lens (spotPrice :: SpotPrice -> Lude.Maybe Lude.Text) (\s a -> s {spotPrice = a} :: SpotPrice)
{-# DEPRECATED spfSpotPrice "Use generic-lens or generic-optics with 'spotPrice' instead." #-}

-- | The instance type.
--
-- /Note:/ Consider using 'instanceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spfInstanceType :: Lens.Lens' SpotPrice (Lude.Maybe InstanceType)
spfInstanceType = Lens.lens (instanceType :: SpotPrice -> Lude.Maybe InstanceType) (\s a -> s {instanceType = a} :: SpotPrice)
{-# DEPRECATED spfInstanceType "Use generic-lens or generic-optics with 'instanceType' instead." #-}

-- | The Availability Zone.
--
-- /Note:/ Consider using 'availabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spfAvailabilityZone :: Lens.Lens' SpotPrice (Lude.Maybe Lude.Text)
spfAvailabilityZone = Lens.lens (availabilityZone :: SpotPrice -> Lude.Maybe Lude.Text) (\s a -> s {availabilityZone = a} :: SpotPrice)
{-# DEPRECATED spfAvailabilityZone "Use generic-lens or generic-optics with 'availabilityZone' instead." #-}

-- | The date and time the request was created, in UTC format (for example, /YYYY/ -/MM/ -/DD/ T/HH/ :/MM/ :/SS/ Z).
--
-- /Note:/ Consider using 'timestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spfTimestamp :: Lens.Lens' SpotPrice (Lude.Maybe Lude.DateTime)
spfTimestamp = Lens.lens (timestamp :: SpotPrice -> Lude.Maybe Lude.DateTime) (\s a -> s {timestamp = a} :: SpotPrice)
{-# DEPRECATED spfTimestamp "Use generic-lens or generic-optics with 'timestamp' instead." #-}

instance Lude.FromXML SpotPrice where
  parseXML x =
    SpotPrice'
      Lude.<$> (x Lude..@? "productDescription")
      Lude.<*> (x Lude..@? "spotPrice")
      Lude.<*> (x Lude..@? "instanceType")
      Lude.<*> (x Lude..@? "availabilityZone")
      Lude.<*> (x Lude..@? "timestamp")
