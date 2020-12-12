{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.InstanceGroupConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.InstanceGroupConfig
  ( InstanceGroupConfig (..),

    -- * Smart constructor
    mkInstanceGroupConfig,

    -- * Lenses
    igcEBSConfiguration,
    igcBidPrice,
    igcConfigurations,
    igcMarket,
    igcName,
    igcAutoScalingPolicy,
    igcInstanceRole,
    igcInstanceType,
    igcInstanceCount,
  )
where

import Network.AWS.EMR.Types.AutoScalingPolicy
import Network.AWS.EMR.Types.Configuration
import Network.AWS.EMR.Types.EBSConfiguration
import Network.AWS.EMR.Types.InstanceRoleType
import Network.AWS.EMR.Types.MarketType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Configuration defining a new instance group.
--
-- /See:/ 'mkInstanceGroupConfig' smart constructor.
data InstanceGroupConfig = InstanceGroupConfig'
  { ebsConfiguration ::
      Lude.Maybe EBSConfiguration,
    bidPrice :: Lude.Maybe Lude.Text,
    configurations :: Lude.Maybe [Configuration],
    market :: Lude.Maybe MarketType,
    name :: Lude.Maybe Lude.Text,
    autoScalingPolicy :: Lude.Maybe AutoScalingPolicy,
    instanceRole :: InstanceRoleType,
    instanceType :: Lude.Text,
    instanceCount :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'InstanceGroupConfig' with the minimum fields required to make a request.
--
-- * 'autoScalingPolicy' - An automatic scaling policy for a core instance group or task instance group in an Amazon EMR cluster. The automatic scaling policy defines how an instance group dynamically adds and terminates EC2 instances in response to the value of a CloudWatch metric. See 'PutAutoScalingPolicy' .
-- * 'bidPrice' - The bid price for each EC2 Spot Instance type as defined by @InstanceType@ . Expressed in USD. If neither @BidPrice@ nor @BidPriceAsPercentageOfOnDemandPrice@ is provided, @BidPriceAsPercentageOfOnDemandPrice@ defaults to 100%.
-- * 'configurations' - The list of configurations supplied for an EMR cluster instance group. You can specify a separate configuration for each instance group (master, core, and task).
-- * 'ebsConfiguration' - EBS configurations that will be attached to each EC2 instance in the instance group.
-- * 'instanceCount' - Target number of instances for the instance group.
-- * 'instanceRole' - The role of the instance group in the cluster.
-- * 'instanceType' - The EC2 instance type for all instances in the instance group.
-- * 'market' - Market type of the EC2 instances used to create a cluster node.
-- * 'name' - Friendly name given to the instance group.
mkInstanceGroupConfig ::
  -- | 'instanceRole'
  InstanceRoleType ->
  -- | 'instanceType'
  Lude.Text ->
  -- | 'instanceCount'
  Lude.Int ->
  InstanceGroupConfig
mkInstanceGroupConfig pInstanceRole_ pInstanceType_ pInstanceCount_ =
  InstanceGroupConfig'
    { ebsConfiguration = Lude.Nothing,
      bidPrice = Lude.Nothing,
      configurations = Lude.Nothing,
      market = Lude.Nothing,
      name = Lude.Nothing,
      autoScalingPolicy = Lude.Nothing,
      instanceRole = pInstanceRole_,
      instanceType = pInstanceType_,
      instanceCount = pInstanceCount_
    }

-- | EBS configurations that will be attached to each EC2 instance in the instance group.
--
-- /Note:/ Consider using 'ebsConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
igcEBSConfiguration :: Lens.Lens' InstanceGroupConfig (Lude.Maybe EBSConfiguration)
igcEBSConfiguration = Lens.lens (ebsConfiguration :: InstanceGroupConfig -> Lude.Maybe EBSConfiguration) (\s a -> s {ebsConfiguration = a} :: InstanceGroupConfig)
{-# DEPRECATED igcEBSConfiguration "Use generic-lens or generic-optics with 'ebsConfiguration' instead." #-}

-- | The bid price for each EC2 Spot Instance type as defined by @InstanceType@ . Expressed in USD. If neither @BidPrice@ nor @BidPriceAsPercentageOfOnDemandPrice@ is provided, @BidPriceAsPercentageOfOnDemandPrice@ defaults to 100%.
--
-- /Note:/ Consider using 'bidPrice' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
igcBidPrice :: Lens.Lens' InstanceGroupConfig (Lude.Maybe Lude.Text)
igcBidPrice = Lens.lens (bidPrice :: InstanceGroupConfig -> Lude.Maybe Lude.Text) (\s a -> s {bidPrice = a} :: InstanceGroupConfig)
{-# DEPRECATED igcBidPrice "Use generic-lens or generic-optics with 'bidPrice' instead." #-}

-- | The list of configurations supplied for an EMR cluster instance group. You can specify a separate configuration for each instance group (master, core, and task).
--
-- /Note:/ Consider using 'configurations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
igcConfigurations :: Lens.Lens' InstanceGroupConfig (Lude.Maybe [Configuration])
igcConfigurations = Lens.lens (configurations :: InstanceGroupConfig -> Lude.Maybe [Configuration]) (\s a -> s {configurations = a} :: InstanceGroupConfig)
{-# DEPRECATED igcConfigurations "Use generic-lens or generic-optics with 'configurations' instead." #-}

-- | Market type of the EC2 instances used to create a cluster node.
--
-- /Note:/ Consider using 'market' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
igcMarket :: Lens.Lens' InstanceGroupConfig (Lude.Maybe MarketType)
igcMarket = Lens.lens (market :: InstanceGroupConfig -> Lude.Maybe MarketType) (\s a -> s {market = a} :: InstanceGroupConfig)
{-# DEPRECATED igcMarket "Use generic-lens or generic-optics with 'market' instead." #-}

-- | Friendly name given to the instance group.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
igcName :: Lens.Lens' InstanceGroupConfig (Lude.Maybe Lude.Text)
igcName = Lens.lens (name :: InstanceGroupConfig -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: InstanceGroupConfig)
{-# DEPRECATED igcName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | An automatic scaling policy for a core instance group or task instance group in an Amazon EMR cluster. The automatic scaling policy defines how an instance group dynamically adds and terminates EC2 instances in response to the value of a CloudWatch metric. See 'PutAutoScalingPolicy' .
--
-- /Note:/ Consider using 'autoScalingPolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
igcAutoScalingPolicy :: Lens.Lens' InstanceGroupConfig (Lude.Maybe AutoScalingPolicy)
igcAutoScalingPolicy = Lens.lens (autoScalingPolicy :: InstanceGroupConfig -> Lude.Maybe AutoScalingPolicy) (\s a -> s {autoScalingPolicy = a} :: InstanceGroupConfig)
{-# DEPRECATED igcAutoScalingPolicy "Use generic-lens or generic-optics with 'autoScalingPolicy' instead." #-}

-- | The role of the instance group in the cluster.
--
-- /Note:/ Consider using 'instanceRole' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
igcInstanceRole :: Lens.Lens' InstanceGroupConfig InstanceRoleType
igcInstanceRole = Lens.lens (instanceRole :: InstanceGroupConfig -> InstanceRoleType) (\s a -> s {instanceRole = a} :: InstanceGroupConfig)
{-# DEPRECATED igcInstanceRole "Use generic-lens or generic-optics with 'instanceRole' instead." #-}

-- | The EC2 instance type for all instances in the instance group.
--
-- /Note:/ Consider using 'instanceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
igcInstanceType :: Lens.Lens' InstanceGroupConfig Lude.Text
igcInstanceType = Lens.lens (instanceType :: InstanceGroupConfig -> Lude.Text) (\s a -> s {instanceType = a} :: InstanceGroupConfig)
{-# DEPRECATED igcInstanceType "Use generic-lens or generic-optics with 'instanceType' instead." #-}

-- | Target number of instances for the instance group.
--
-- /Note:/ Consider using 'instanceCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
igcInstanceCount :: Lens.Lens' InstanceGroupConfig Lude.Int
igcInstanceCount = Lens.lens (instanceCount :: InstanceGroupConfig -> Lude.Int) (\s a -> s {instanceCount = a} :: InstanceGroupConfig)
{-# DEPRECATED igcInstanceCount "Use generic-lens or generic-optics with 'instanceCount' instead." #-}

instance Lude.ToJSON InstanceGroupConfig where
  toJSON InstanceGroupConfig' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("EbsConfiguration" Lude..=) Lude.<$> ebsConfiguration,
            ("BidPrice" Lude..=) Lude.<$> bidPrice,
            ("Configurations" Lude..=) Lude.<$> configurations,
            ("Market" Lude..=) Lude.<$> market,
            ("Name" Lude..=) Lude.<$> name,
            ("AutoScalingPolicy" Lude..=) Lude.<$> autoScalingPolicy,
            Lude.Just ("InstanceRole" Lude..= instanceRole),
            Lude.Just ("InstanceType" Lude..= instanceType),
            Lude.Just ("InstanceCount" Lude..= instanceCount)
          ]
      )
