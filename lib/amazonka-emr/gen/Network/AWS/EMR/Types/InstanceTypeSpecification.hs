{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.InstanceTypeSpecification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.InstanceTypeSpecification
  ( InstanceTypeSpecification (..),

    -- * Smart constructor
    mkInstanceTypeSpecification,

    -- * Lenses
    itsBidPrice,
    itsWeightedCapacity,
    itsConfigurations,
    itsEBSBlockDevices,
    itsInstanceType,
    itsEBSOptimized,
    itsBidPriceAsPercentageOfOnDemandPrice,
  )
where

import Network.AWS.EMR.Types.Configuration
import Network.AWS.EMR.Types.EBSBlockDevice
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The configuration specification for each instance type in an instance fleet.
--
-- /See:/ 'mkInstanceTypeSpecification' smart constructor.
data InstanceTypeSpecification = InstanceTypeSpecification'
  { -- | The bid price for each EC2 Spot Instance type as defined by @InstanceType@ . Expressed in USD.
    bidPrice :: Lude.Maybe Lude.Text,
    -- | The number of units that a provisioned instance of this type provides toward fulfilling the target capacities defined in 'InstanceFleetConfig' . Capacity values represent performance characteristics such as vCPUs, memory, or I/O. If not specified, the default value is 1.
    weightedCapacity :: Lude.Maybe Lude.Natural,
    -- | A configuration classification that applies when provisioning cluster instances, which can include configurations for applications and software bundled with Amazon EMR.
    configurations :: Lude.Maybe [Configuration],
    -- | The configuration of Amazon Elastic Block Storage (Amazon EBS) attached to each instance as defined by @InstanceType@ .
    ebsBlockDevices :: Lude.Maybe [EBSBlockDevice],
    -- | The EC2 instance type, for example @m3.xlarge@ .
    instanceType :: Lude.Maybe Lude.Text,
    -- | Evaluates to @TRUE@ when the specified @InstanceType@ is EBS-optimized.
    ebsOptimized :: Lude.Maybe Lude.Bool,
    -- | The bid price, as a percentage of On-Demand price, for each EC2 Spot Instance as defined by @InstanceType@ . Expressed as a number (for example, 20 specifies 20%).
    bidPriceAsPercentageOfOnDemandPrice :: Lude.Maybe Lude.Double
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'InstanceTypeSpecification' with the minimum fields required to make a request.
--
-- * 'bidPrice' - The bid price for each EC2 Spot Instance type as defined by @InstanceType@ . Expressed in USD.
-- * 'weightedCapacity' - The number of units that a provisioned instance of this type provides toward fulfilling the target capacities defined in 'InstanceFleetConfig' . Capacity values represent performance characteristics such as vCPUs, memory, or I/O. If not specified, the default value is 1.
-- * 'configurations' - A configuration classification that applies when provisioning cluster instances, which can include configurations for applications and software bundled with Amazon EMR.
-- * 'ebsBlockDevices' - The configuration of Amazon Elastic Block Storage (Amazon EBS) attached to each instance as defined by @InstanceType@ .
-- * 'instanceType' - The EC2 instance type, for example @m3.xlarge@ .
-- * 'ebsOptimized' - Evaluates to @TRUE@ when the specified @InstanceType@ is EBS-optimized.
-- * 'bidPriceAsPercentageOfOnDemandPrice' - The bid price, as a percentage of On-Demand price, for each EC2 Spot Instance as defined by @InstanceType@ . Expressed as a number (for example, 20 specifies 20%).
mkInstanceTypeSpecification ::
  InstanceTypeSpecification
mkInstanceTypeSpecification =
  InstanceTypeSpecification'
    { bidPrice = Lude.Nothing,
      weightedCapacity = Lude.Nothing,
      configurations = Lude.Nothing,
      ebsBlockDevices = Lude.Nothing,
      instanceType = Lude.Nothing,
      ebsOptimized = Lude.Nothing,
      bidPriceAsPercentageOfOnDemandPrice = Lude.Nothing
    }

-- | The bid price for each EC2 Spot Instance type as defined by @InstanceType@ . Expressed in USD.
--
-- /Note:/ Consider using 'bidPrice' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
itsBidPrice :: Lens.Lens' InstanceTypeSpecification (Lude.Maybe Lude.Text)
itsBidPrice = Lens.lens (bidPrice :: InstanceTypeSpecification -> Lude.Maybe Lude.Text) (\s a -> s {bidPrice = a} :: InstanceTypeSpecification)
{-# DEPRECATED itsBidPrice "Use generic-lens or generic-optics with 'bidPrice' instead." #-}

-- | The number of units that a provisioned instance of this type provides toward fulfilling the target capacities defined in 'InstanceFleetConfig' . Capacity values represent performance characteristics such as vCPUs, memory, or I/O. If not specified, the default value is 1.
--
-- /Note:/ Consider using 'weightedCapacity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
itsWeightedCapacity :: Lens.Lens' InstanceTypeSpecification (Lude.Maybe Lude.Natural)
itsWeightedCapacity = Lens.lens (weightedCapacity :: InstanceTypeSpecification -> Lude.Maybe Lude.Natural) (\s a -> s {weightedCapacity = a} :: InstanceTypeSpecification)
{-# DEPRECATED itsWeightedCapacity "Use generic-lens or generic-optics with 'weightedCapacity' instead." #-}

-- | A configuration classification that applies when provisioning cluster instances, which can include configurations for applications and software bundled with Amazon EMR.
--
-- /Note:/ Consider using 'configurations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
itsConfigurations :: Lens.Lens' InstanceTypeSpecification (Lude.Maybe [Configuration])
itsConfigurations = Lens.lens (configurations :: InstanceTypeSpecification -> Lude.Maybe [Configuration]) (\s a -> s {configurations = a} :: InstanceTypeSpecification)
{-# DEPRECATED itsConfigurations "Use generic-lens or generic-optics with 'configurations' instead." #-}

-- | The configuration of Amazon Elastic Block Storage (Amazon EBS) attached to each instance as defined by @InstanceType@ .
--
-- /Note:/ Consider using 'ebsBlockDevices' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
itsEBSBlockDevices :: Lens.Lens' InstanceTypeSpecification (Lude.Maybe [EBSBlockDevice])
itsEBSBlockDevices = Lens.lens (ebsBlockDevices :: InstanceTypeSpecification -> Lude.Maybe [EBSBlockDevice]) (\s a -> s {ebsBlockDevices = a} :: InstanceTypeSpecification)
{-# DEPRECATED itsEBSBlockDevices "Use generic-lens or generic-optics with 'ebsBlockDevices' instead." #-}

-- | The EC2 instance type, for example @m3.xlarge@ .
--
-- /Note:/ Consider using 'instanceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
itsInstanceType :: Lens.Lens' InstanceTypeSpecification (Lude.Maybe Lude.Text)
itsInstanceType = Lens.lens (instanceType :: InstanceTypeSpecification -> Lude.Maybe Lude.Text) (\s a -> s {instanceType = a} :: InstanceTypeSpecification)
{-# DEPRECATED itsInstanceType "Use generic-lens or generic-optics with 'instanceType' instead." #-}

-- | Evaluates to @TRUE@ when the specified @InstanceType@ is EBS-optimized.
--
-- /Note:/ Consider using 'ebsOptimized' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
itsEBSOptimized :: Lens.Lens' InstanceTypeSpecification (Lude.Maybe Lude.Bool)
itsEBSOptimized = Lens.lens (ebsOptimized :: InstanceTypeSpecification -> Lude.Maybe Lude.Bool) (\s a -> s {ebsOptimized = a} :: InstanceTypeSpecification)
{-# DEPRECATED itsEBSOptimized "Use generic-lens or generic-optics with 'ebsOptimized' instead." #-}

-- | The bid price, as a percentage of On-Demand price, for each EC2 Spot Instance as defined by @InstanceType@ . Expressed as a number (for example, 20 specifies 20%).
--
-- /Note:/ Consider using 'bidPriceAsPercentageOfOnDemandPrice' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
itsBidPriceAsPercentageOfOnDemandPrice :: Lens.Lens' InstanceTypeSpecification (Lude.Maybe Lude.Double)
itsBidPriceAsPercentageOfOnDemandPrice = Lens.lens (bidPriceAsPercentageOfOnDemandPrice :: InstanceTypeSpecification -> Lude.Maybe Lude.Double) (\s a -> s {bidPriceAsPercentageOfOnDemandPrice = a} :: InstanceTypeSpecification)
{-# DEPRECATED itsBidPriceAsPercentageOfOnDemandPrice "Use generic-lens or generic-optics with 'bidPriceAsPercentageOfOnDemandPrice' instead." #-}

instance Lude.FromJSON InstanceTypeSpecification where
  parseJSON =
    Lude.withObject
      "InstanceTypeSpecification"
      ( \x ->
          InstanceTypeSpecification'
            Lude.<$> (x Lude..:? "BidPrice")
            Lude.<*> (x Lude..:? "WeightedCapacity")
            Lude.<*> (x Lude..:? "Configurations" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "EbsBlockDevices" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "InstanceType")
            Lude.<*> (x Lude..:? "EbsOptimized")
            Lude.<*> (x Lude..:? "BidPriceAsPercentageOfOnDemandPrice")
      )
