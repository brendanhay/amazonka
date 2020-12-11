-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.EC2InstanceDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.EC2InstanceDetails
  ( EC2InstanceDetails (..),

    -- * Smart constructor
    mkEC2InstanceDetails,

    -- * Lenses
    eidCurrentGeneration,
    eidPlatform,
    eidFamily,
    eidInstanceType,
    eidAvailabilityZone,
    eidSizeFlexEligible,
    eidTenancy,
    eidRegion,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Details about the Amazon EC2 instances that AWS recommends that you purchase.
--
-- /See:/ 'mkEC2InstanceDetails' smart constructor.
data EC2InstanceDetails = EC2InstanceDetails'
  { currentGeneration ::
      Lude.Maybe Lude.Bool,
    platform :: Lude.Maybe Lude.Text,
    family :: Lude.Maybe Lude.Text,
    instanceType :: Lude.Maybe Lude.Text,
    availabilityZone :: Lude.Maybe Lude.Text,
    sizeFlexEligible :: Lude.Maybe Lude.Bool,
    tenancy :: Lude.Maybe Lude.Text,
    region :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EC2InstanceDetails' with the minimum fields required to make a request.
--
-- * 'availabilityZone' - The Availability Zone of the recommended reservation.
-- * 'currentGeneration' - Whether the recommendation is for a current-generation instance.
-- * 'family' - The instance family of the recommended reservation.
-- * 'instanceType' - The type of instance that AWS recommends.
-- * 'platform' - The platform of the recommended reservation. The platform is the specific combination of operating system, license model, and software on an instance.
-- * 'region' - The AWS Region of the recommended reservation.
-- * 'sizeFlexEligible' - Whether the recommended reservation is size flexible.
-- * 'tenancy' - Whether the recommended reservation is dedicated or shared.
mkEC2InstanceDetails ::
  EC2InstanceDetails
mkEC2InstanceDetails =
  EC2InstanceDetails'
    { currentGeneration = Lude.Nothing,
      platform = Lude.Nothing,
      family = Lude.Nothing,
      instanceType = Lude.Nothing,
      availabilityZone = Lude.Nothing,
      sizeFlexEligible = Lude.Nothing,
      tenancy = Lude.Nothing,
      region = Lude.Nothing
    }

-- | Whether the recommendation is for a current-generation instance.
--
-- /Note:/ Consider using 'currentGeneration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eidCurrentGeneration :: Lens.Lens' EC2InstanceDetails (Lude.Maybe Lude.Bool)
eidCurrentGeneration = Lens.lens (currentGeneration :: EC2InstanceDetails -> Lude.Maybe Lude.Bool) (\s a -> s {currentGeneration = a} :: EC2InstanceDetails)
{-# DEPRECATED eidCurrentGeneration "Use generic-lens or generic-optics with 'currentGeneration' instead." #-}

-- | The platform of the recommended reservation. The platform is the specific combination of operating system, license model, and software on an instance.
--
-- /Note:/ Consider using 'platform' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eidPlatform :: Lens.Lens' EC2InstanceDetails (Lude.Maybe Lude.Text)
eidPlatform = Lens.lens (platform :: EC2InstanceDetails -> Lude.Maybe Lude.Text) (\s a -> s {platform = a} :: EC2InstanceDetails)
{-# DEPRECATED eidPlatform "Use generic-lens or generic-optics with 'platform' instead." #-}

-- | The instance family of the recommended reservation.
--
-- /Note:/ Consider using 'family' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eidFamily :: Lens.Lens' EC2InstanceDetails (Lude.Maybe Lude.Text)
eidFamily = Lens.lens (family :: EC2InstanceDetails -> Lude.Maybe Lude.Text) (\s a -> s {family = a} :: EC2InstanceDetails)
{-# DEPRECATED eidFamily "Use generic-lens or generic-optics with 'family' instead." #-}

-- | The type of instance that AWS recommends.
--
-- /Note:/ Consider using 'instanceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eidInstanceType :: Lens.Lens' EC2InstanceDetails (Lude.Maybe Lude.Text)
eidInstanceType = Lens.lens (instanceType :: EC2InstanceDetails -> Lude.Maybe Lude.Text) (\s a -> s {instanceType = a} :: EC2InstanceDetails)
{-# DEPRECATED eidInstanceType "Use generic-lens or generic-optics with 'instanceType' instead." #-}

-- | The Availability Zone of the recommended reservation.
--
-- /Note:/ Consider using 'availabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eidAvailabilityZone :: Lens.Lens' EC2InstanceDetails (Lude.Maybe Lude.Text)
eidAvailabilityZone = Lens.lens (availabilityZone :: EC2InstanceDetails -> Lude.Maybe Lude.Text) (\s a -> s {availabilityZone = a} :: EC2InstanceDetails)
{-# DEPRECATED eidAvailabilityZone "Use generic-lens or generic-optics with 'availabilityZone' instead." #-}

-- | Whether the recommended reservation is size flexible.
--
-- /Note:/ Consider using 'sizeFlexEligible' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eidSizeFlexEligible :: Lens.Lens' EC2InstanceDetails (Lude.Maybe Lude.Bool)
eidSizeFlexEligible = Lens.lens (sizeFlexEligible :: EC2InstanceDetails -> Lude.Maybe Lude.Bool) (\s a -> s {sizeFlexEligible = a} :: EC2InstanceDetails)
{-# DEPRECATED eidSizeFlexEligible "Use generic-lens or generic-optics with 'sizeFlexEligible' instead." #-}

-- | Whether the recommended reservation is dedicated or shared.
--
-- /Note:/ Consider using 'tenancy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eidTenancy :: Lens.Lens' EC2InstanceDetails (Lude.Maybe Lude.Text)
eidTenancy = Lens.lens (tenancy :: EC2InstanceDetails -> Lude.Maybe Lude.Text) (\s a -> s {tenancy = a} :: EC2InstanceDetails)
{-# DEPRECATED eidTenancy "Use generic-lens or generic-optics with 'tenancy' instead." #-}

-- | The AWS Region of the recommended reservation.
--
-- /Note:/ Consider using 'region' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eidRegion :: Lens.Lens' EC2InstanceDetails (Lude.Maybe Lude.Text)
eidRegion = Lens.lens (region :: EC2InstanceDetails -> Lude.Maybe Lude.Text) (\s a -> s {region = a} :: EC2InstanceDetails)
{-# DEPRECATED eidRegion "Use generic-lens or generic-optics with 'region' instead." #-}

instance Lude.FromJSON EC2InstanceDetails where
  parseJSON =
    Lude.withObject
      "EC2InstanceDetails"
      ( \x ->
          EC2InstanceDetails'
            Lude.<$> (x Lude..:? "CurrentGeneration")
            Lude.<*> (x Lude..:? "Platform")
            Lude.<*> (x Lude..:? "Family")
            Lude.<*> (x Lude..:? "InstanceType")
            Lude.<*> (x Lude..:? "AvailabilityZone")
            Lude.<*> (x Lude..:? "SizeFlexEligible")
            Lude.<*> (x Lude..:? "Tenancy")
            Lude.<*> (x Lude..:? "Region")
      )
