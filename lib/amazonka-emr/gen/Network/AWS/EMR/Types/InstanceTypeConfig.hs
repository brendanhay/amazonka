-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.InstanceTypeConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.InstanceTypeConfig
  ( InstanceTypeConfig (..),

    -- * Smart constructor
    mkInstanceTypeConfig,

    -- * Lenses
    itcEBSConfiguration,
    itcBidPrice,
    itcWeightedCapacity,
    itcConfigurations,
    itcBidPriceAsPercentageOfOnDemandPrice,
    itcInstanceType,
  )
where

import Network.AWS.EMR.Types.Configuration
import Network.AWS.EMR.Types.EBSConfiguration
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | An instance type configuration for each instance type in an instance fleet, which determines the EC2 instances Amazon EMR attempts to provision to fulfill On-Demand and Spot target capacities. There can be a maximum of five instance type configurations in a fleet.
--
-- /See:/ 'mkInstanceTypeConfig' smart constructor.
data InstanceTypeConfig = InstanceTypeConfig'
  { ebsConfiguration ::
      Lude.Maybe EBSConfiguration,
    bidPrice :: Lude.Maybe Lude.Text,
    weightedCapacity :: Lude.Maybe Lude.Natural,
    configurations :: Lude.Maybe [Configuration],
    bidPriceAsPercentageOfOnDemandPrice ::
      Lude.Maybe Lude.Double,
    instanceType :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'InstanceTypeConfig' with the minimum fields required to make a request.
--
-- * 'bidPrice' - The bid price for each EC2 Spot Instance type as defined by @InstanceType@ . Expressed in USD. If neither @BidPrice@ nor @BidPriceAsPercentageOfOnDemandPrice@ is provided, @BidPriceAsPercentageOfOnDemandPrice@ defaults to 100%.
-- * 'bidPriceAsPercentageOfOnDemandPrice' - The bid price, as a percentage of On-Demand price, for each EC2 Spot Instance as defined by @InstanceType@ . Expressed as a number (for example, 20 specifies 20%). If neither @BidPrice@ nor @BidPriceAsPercentageOfOnDemandPrice@ is provided, @BidPriceAsPercentageOfOnDemandPrice@ defaults to 100%.
-- * 'configurations' - A configuration classification that applies when provisioning cluster instances, which can include configurations for applications and software that run on the cluster.
-- * 'ebsConfiguration' - The configuration of Amazon Elastic Block Storage (Amazon EBS) attached to each instance as defined by @InstanceType@ .
-- * 'instanceType' - An EC2 instance type, such as @m3.xlarge@ .
-- * 'weightedCapacity' - The number of units that a provisioned instance of this type provides toward fulfilling the target capacities defined in 'InstanceFleetConfig' . This value is 1 for a master instance fleet, and must be 1 or greater for core and task instance fleets. Defaults to 1 if not specified.
mkInstanceTypeConfig ::
  -- | 'instanceType'
  Lude.Text ->
  InstanceTypeConfig
mkInstanceTypeConfig pInstanceType_ =
  InstanceTypeConfig'
    { ebsConfiguration = Lude.Nothing,
      bidPrice = Lude.Nothing,
      weightedCapacity = Lude.Nothing,
      configurations = Lude.Nothing,
      bidPriceAsPercentageOfOnDemandPrice = Lude.Nothing,
      instanceType = pInstanceType_
    }

-- | The configuration of Amazon Elastic Block Storage (Amazon EBS) attached to each instance as defined by @InstanceType@ .
--
-- /Note:/ Consider using 'ebsConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
itcEBSConfiguration :: Lens.Lens' InstanceTypeConfig (Lude.Maybe EBSConfiguration)
itcEBSConfiguration = Lens.lens (ebsConfiguration :: InstanceTypeConfig -> Lude.Maybe EBSConfiguration) (\s a -> s {ebsConfiguration = a} :: InstanceTypeConfig)
{-# DEPRECATED itcEBSConfiguration "Use generic-lens or generic-optics with 'ebsConfiguration' instead." #-}

-- | The bid price for each EC2 Spot Instance type as defined by @InstanceType@ . Expressed in USD. If neither @BidPrice@ nor @BidPriceAsPercentageOfOnDemandPrice@ is provided, @BidPriceAsPercentageOfOnDemandPrice@ defaults to 100%.
--
-- /Note:/ Consider using 'bidPrice' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
itcBidPrice :: Lens.Lens' InstanceTypeConfig (Lude.Maybe Lude.Text)
itcBidPrice = Lens.lens (bidPrice :: InstanceTypeConfig -> Lude.Maybe Lude.Text) (\s a -> s {bidPrice = a} :: InstanceTypeConfig)
{-# DEPRECATED itcBidPrice "Use generic-lens or generic-optics with 'bidPrice' instead." #-}

-- | The number of units that a provisioned instance of this type provides toward fulfilling the target capacities defined in 'InstanceFleetConfig' . This value is 1 for a master instance fleet, and must be 1 or greater for core and task instance fleets. Defaults to 1 if not specified.
--
-- /Note:/ Consider using 'weightedCapacity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
itcWeightedCapacity :: Lens.Lens' InstanceTypeConfig (Lude.Maybe Lude.Natural)
itcWeightedCapacity = Lens.lens (weightedCapacity :: InstanceTypeConfig -> Lude.Maybe Lude.Natural) (\s a -> s {weightedCapacity = a} :: InstanceTypeConfig)
{-# DEPRECATED itcWeightedCapacity "Use generic-lens or generic-optics with 'weightedCapacity' instead." #-}

-- | A configuration classification that applies when provisioning cluster instances, which can include configurations for applications and software that run on the cluster.
--
-- /Note:/ Consider using 'configurations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
itcConfigurations :: Lens.Lens' InstanceTypeConfig (Lude.Maybe [Configuration])
itcConfigurations = Lens.lens (configurations :: InstanceTypeConfig -> Lude.Maybe [Configuration]) (\s a -> s {configurations = a} :: InstanceTypeConfig)
{-# DEPRECATED itcConfigurations "Use generic-lens or generic-optics with 'configurations' instead." #-}

-- | The bid price, as a percentage of On-Demand price, for each EC2 Spot Instance as defined by @InstanceType@ . Expressed as a number (for example, 20 specifies 20%). If neither @BidPrice@ nor @BidPriceAsPercentageOfOnDemandPrice@ is provided, @BidPriceAsPercentageOfOnDemandPrice@ defaults to 100%.
--
-- /Note:/ Consider using 'bidPriceAsPercentageOfOnDemandPrice' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
itcBidPriceAsPercentageOfOnDemandPrice :: Lens.Lens' InstanceTypeConfig (Lude.Maybe Lude.Double)
itcBidPriceAsPercentageOfOnDemandPrice = Lens.lens (bidPriceAsPercentageOfOnDemandPrice :: InstanceTypeConfig -> Lude.Maybe Lude.Double) (\s a -> s {bidPriceAsPercentageOfOnDemandPrice = a} :: InstanceTypeConfig)
{-# DEPRECATED itcBidPriceAsPercentageOfOnDemandPrice "Use generic-lens or generic-optics with 'bidPriceAsPercentageOfOnDemandPrice' instead." #-}

-- | An EC2 instance type, such as @m3.xlarge@ .
--
-- /Note:/ Consider using 'instanceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
itcInstanceType :: Lens.Lens' InstanceTypeConfig Lude.Text
itcInstanceType = Lens.lens (instanceType :: InstanceTypeConfig -> Lude.Text) (\s a -> s {instanceType = a} :: InstanceTypeConfig)
{-# DEPRECATED itcInstanceType "Use generic-lens or generic-optics with 'instanceType' instead." #-}

instance Lude.ToJSON InstanceTypeConfig where
  toJSON InstanceTypeConfig' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("EbsConfiguration" Lude..=) Lude.<$> ebsConfiguration,
            ("BidPrice" Lude..=) Lude.<$> bidPrice,
            ("WeightedCapacity" Lude..=) Lude.<$> weightedCapacity,
            ("Configurations" Lude..=) Lude.<$> configurations,
            ("BidPriceAsPercentageOfOnDemandPrice" Lude..=)
              Lude.<$> bidPriceAsPercentageOfOnDemandPrice,
            Lude.Just ("InstanceType" Lude..= instanceType)
          ]
      )
