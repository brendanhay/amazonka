{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.EC2ResourceDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.EC2ResourceDetails
  ( EC2ResourceDetails (..),

    -- * Smart constructor
    mkEC2ResourceDetails,

    -- * Lenses
    erdPlatform,
    erdVcpu,
    erdNetworkPerformance,
    erdMemory,
    erdInstanceType,
    erdStorage,
    erdSku,
    erdRegion,
    erdHourlyOnDemandRate,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Details on the Amazon EC2 Resource.
--
-- /See:/ 'mkEC2ResourceDetails' smart constructor.
data EC2ResourceDetails = EC2ResourceDetails'
  { -- | The platform of the AWS instance. The platform is the specific combination of operating system, license model, and software on an instance.
    platform :: Lude.Maybe Lude.Text,
    -- | Number of VCPU cores in the AWS instance type.
    vcpu :: Lude.Maybe Lude.Text,
    -- | Network performance capacity of the AWS instance.
    networkPerformance :: Lude.Maybe Lude.Text,
    -- | Memory capacity of the AWS instance.
    memory :: Lude.Maybe Lude.Text,
    -- | The type of AWS instance.
    instanceType :: Lude.Maybe Lude.Text,
    -- | The disk storage of the AWS instance (not EBS storage).
    storage :: Lude.Maybe Lude.Text,
    -- | The SKU of the product.
    sku :: Lude.Maybe Lude.Text,
    -- | The AWS Region of the instance.
    region :: Lude.Maybe Lude.Text,
    -- | Hourly public On-Demand rate for the instance type.
    hourlyOnDemandRate :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EC2ResourceDetails' with the minimum fields required to make a request.
--
-- * 'platform' - The platform of the AWS instance. The platform is the specific combination of operating system, license model, and software on an instance.
-- * 'vcpu' - Number of VCPU cores in the AWS instance type.
-- * 'networkPerformance' - Network performance capacity of the AWS instance.
-- * 'memory' - Memory capacity of the AWS instance.
-- * 'instanceType' - The type of AWS instance.
-- * 'storage' - The disk storage of the AWS instance (not EBS storage).
-- * 'sku' - The SKU of the product.
-- * 'region' - The AWS Region of the instance.
-- * 'hourlyOnDemandRate' - Hourly public On-Demand rate for the instance type.
mkEC2ResourceDetails ::
  EC2ResourceDetails
mkEC2ResourceDetails =
  EC2ResourceDetails'
    { platform = Lude.Nothing,
      vcpu = Lude.Nothing,
      networkPerformance = Lude.Nothing,
      memory = Lude.Nothing,
      instanceType = Lude.Nothing,
      storage = Lude.Nothing,
      sku = Lude.Nothing,
      region = Lude.Nothing,
      hourlyOnDemandRate = Lude.Nothing
    }

-- | The platform of the AWS instance. The platform is the specific combination of operating system, license model, and software on an instance.
--
-- /Note:/ Consider using 'platform' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
erdPlatform :: Lens.Lens' EC2ResourceDetails (Lude.Maybe Lude.Text)
erdPlatform = Lens.lens (platform :: EC2ResourceDetails -> Lude.Maybe Lude.Text) (\s a -> s {platform = a} :: EC2ResourceDetails)
{-# DEPRECATED erdPlatform "Use generic-lens or generic-optics with 'platform' instead." #-}

-- | Number of VCPU cores in the AWS instance type.
--
-- /Note:/ Consider using 'vcpu' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
erdVcpu :: Lens.Lens' EC2ResourceDetails (Lude.Maybe Lude.Text)
erdVcpu = Lens.lens (vcpu :: EC2ResourceDetails -> Lude.Maybe Lude.Text) (\s a -> s {vcpu = a} :: EC2ResourceDetails)
{-# DEPRECATED erdVcpu "Use generic-lens or generic-optics with 'vcpu' instead." #-}

-- | Network performance capacity of the AWS instance.
--
-- /Note:/ Consider using 'networkPerformance' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
erdNetworkPerformance :: Lens.Lens' EC2ResourceDetails (Lude.Maybe Lude.Text)
erdNetworkPerformance = Lens.lens (networkPerformance :: EC2ResourceDetails -> Lude.Maybe Lude.Text) (\s a -> s {networkPerformance = a} :: EC2ResourceDetails)
{-# DEPRECATED erdNetworkPerformance "Use generic-lens or generic-optics with 'networkPerformance' instead." #-}

-- | Memory capacity of the AWS instance.
--
-- /Note:/ Consider using 'memory' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
erdMemory :: Lens.Lens' EC2ResourceDetails (Lude.Maybe Lude.Text)
erdMemory = Lens.lens (memory :: EC2ResourceDetails -> Lude.Maybe Lude.Text) (\s a -> s {memory = a} :: EC2ResourceDetails)
{-# DEPRECATED erdMemory "Use generic-lens or generic-optics with 'memory' instead." #-}

-- | The type of AWS instance.
--
-- /Note:/ Consider using 'instanceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
erdInstanceType :: Lens.Lens' EC2ResourceDetails (Lude.Maybe Lude.Text)
erdInstanceType = Lens.lens (instanceType :: EC2ResourceDetails -> Lude.Maybe Lude.Text) (\s a -> s {instanceType = a} :: EC2ResourceDetails)
{-# DEPRECATED erdInstanceType "Use generic-lens or generic-optics with 'instanceType' instead." #-}

-- | The disk storage of the AWS instance (not EBS storage).
--
-- /Note:/ Consider using 'storage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
erdStorage :: Lens.Lens' EC2ResourceDetails (Lude.Maybe Lude.Text)
erdStorage = Lens.lens (storage :: EC2ResourceDetails -> Lude.Maybe Lude.Text) (\s a -> s {storage = a} :: EC2ResourceDetails)
{-# DEPRECATED erdStorage "Use generic-lens or generic-optics with 'storage' instead." #-}

-- | The SKU of the product.
--
-- /Note:/ Consider using 'sku' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
erdSku :: Lens.Lens' EC2ResourceDetails (Lude.Maybe Lude.Text)
erdSku = Lens.lens (sku :: EC2ResourceDetails -> Lude.Maybe Lude.Text) (\s a -> s {sku = a} :: EC2ResourceDetails)
{-# DEPRECATED erdSku "Use generic-lens or generic-optics with 'sku' instead." #-}

-- | The AWS Region of the instance.
--
-- /Note:/ Consider using 'region' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
erdRegion :: Lens.Lens' EC2ResourceDetails (Lude.Maybe Lude.Text)
erdRegion = Lens.lens (region :: EC2ResourceDetails -> Lude.Maybe Lude.Text) (\s a -> s {region = a} :: EC2ResourceDetails)
{-# DEPRECATED erdRegion "Use generic-lens or generic-optics with 'region' instead." #-}

-- | Hourly public On-Demand rate for the instance type.
--
-- /Note:/ Consider using 'hourlyOnDemandRate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
erdHourlyOnDemandRate :: Lens.Lens' EC2ResourceDetails (Lude.Maybe Lude.Text)
erdHourlyOnDemandRate = Lens.lens (hourlyOnDemandRate :: EC2ResourceDetails -> Lude.Maybe Lude.Text) (\s a -> s {hourlyOnDemandRate = a} :: EC2ResourceDetails)
{-# DEPRECATED erdHourlyOnDemandRate "Use generic-lens or generic-optics with 'hourlyOnDemandRate' instead." #-}

instance Lude.FromJSON EC2ResourceDetails where
  parseJSON =
    Lude.withObject
      "EC2ResourceDetails"
      ( \x ->
          EC2ResourceDetails'
            Lude.<$> (x Lude..:? "Platform")
            Lude.<*> (x Lude..:? "Vcpu")
            Lude.<*> (x Lude..:? "NetworkPerformance")
            Lude.<*> (x Lude..:? "Memory")
            Lude.<*> (x Lude..:? "InstanceType")
            Lude.<*> (x Lude..:? "Storage")
            Lude.<*> (x Lude..:? "Sku")
            Lude.<*> (x Lude..:? "Region")
            Lude.<*> (x Lude..:? "HourlyOnDemandRate")
      )
