{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.Types.ElasticLoadBalancer
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.OpsWorks.Types.ElasticLoadBalancer
  ( ElasticLoadBalancer (..),

    -- * Smart constructor
    mkElasticLoadBalancer,

    -- * Lenses
    elbSubnetIds,
    elbVPCId,
    elbAvailabilityZones,
    elbRegion,
    elbElasticLoadBalancerName,
    elbStackId,
    elbEC2InstanceIds,
    elbLayerId,
    elbDNSName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes an Elastic Load Balancing instance.
--
-- /See:/ 'mkElasticLoadBalancer' smart constructor.
data ElasticLoadBalancer = ElasticLoadBalancer'
  { -- | A list of subnet IDs, if the stack is running in a VPC.
    subnetIds :: Lude.Maybe [Lude.Text],
    -- | The VPC ID.
    vpcId :: Lude.Maybe Lude.Text,
    -- | A list of Availability Zones.
    availabilityZones :: Lude.Maybe [Lude.Text],
    -- | The instance's AWS region.
    region :: Lude.Maybe Lude.Text,
    -- | The Elastic Load Balancing instance's name.
    elasticLoadBalancerName :: Lude.Maybe Lude.Text,
    -- | The ID of the stack that the instance is associated with.
    stackId :: Lude.Maybe Lude.Text,
    -- | A list of the EC2 instances that the Elastic Load Balancing instance is managing traffic for.
    ec2InstanceIds :: Lude.Maybe [Lude.Text],
    -- | The ID of the layer that the instance is attached to.
    layerId :: Lude.Maybe Lude.Text,
    -- | The instance's public DNS name.
    dnsName :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ElasticLoadBalancer' with the minimum fields required to make a request.
--
-- * 'subnetIds' - A list of subnet IDs, if the stack is running in a VPC.
-- * 'vpcId' - The VPC ID.
-- * 'availabilityZones' - A list of Availability Zones.
-- * 'region' - The instance's AWS region.
-- * 'elasticLoadBalancerName' - The Elastic Load Balancing instance's name.
-- * 'stackId' - The ID of the stack that the instance is associated with.
-- * 'ec2InstanceIds' - A list of the EC2 instances that the Elastic Load Balancing instance is managing traffic for.
-- * 'layerId' - The ID of the layer that the instance is attached to.
-- * 'dnsName' - The instance's public DNS name.
mkElasticLoadBalancer ::
  ElasticLoadBalancer
mkElasticLoadBalancer =
  ElasticLoadBalancer'
    { subnetIds = Lude.Nothing,
      vpcId = Lude.Nothing,
      availabilityZones = Lude.Nothing,
      region = Lude.Nothing,
      elasticLoadBalancerName = Lude.Nothing,
      stackId = Lude.Nothing,
      ec2InstanceIds = Lude.Nothing,
      layerId = Lude.Nothing,
      dnsName = Lude.Nothing
    }

-- | A list of subnet IDs, if the stack is running in a VPC.
--
-- /Note:/ Consider using 'subnetIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
elbSubnetIds :: Lens.Lens' ElasticLoadBalancer (Lude.Maybe [Lude.Text])
elbSubnetIds = Lens.lens (subnetIds :: ElasticLoadBalancer -> Lude.Maybe [Lude.Text]) (\s a -> s {subnetIds = a} :: ElasticLoadBalancer)
{-# DEPRECATED elbSubnetIds "Use generic-lens or generic-optics with 'subnetIds' instead." #-}

-- | The VPC ID.
--
-- /Note:/ Consider using 'vpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
elbVPCId :: Lens.Lens' ElasticLoadBalancer (Lude.Maybe Lude.Text)
elbVPCId = Lens.lens (vpcId :: ElasticLoadBalancer -> Lude.Maybe Lude.Text) (\s a -> s {vpcId = a} :: ElasticLoadBalancer)
{-# DEPRECATED elbVPCId "Use generic-lens or generic-optics with 'vpcId' instead." #-}

-- | A list of Availability Zones.
--
-- /Note:/ Consider using 'availabilityZones' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
elbAvailabilityZones :: Lens.Lens' ElasticLoadBalancer (Lude.Maybe [Lude.Text])
elbAvailabilityZones = Lens.lens (availabilityZones :: ElasticLoadBalancer -> Lude.Maybe [Lude.Text]) (\s a -> s {availabilityZones = a} :: ElasticLoadBalancer)
{-# DEPRECATED elbAvailabilityZones "Use generic-lens or generic-optics with 'availabilityZones' instead." #-}

-- | The instance's AWS region.
--
-- /Note:/ Consider using 'region' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
elbRegion :: Lens.Lens' ElasticLoadBalancer (Lude.Maybe Lude.Text)
elbRegion = Lens.lens (region :: ElasticLoadBalancer -> Lude.Maybe Lude.Text) (\s a -> s {region = a} :: ElasticLoadBalancer)
{-# DEPRECATED elbRegion "Use generic-lens or generic-optics with 'region' instead." #-}

-- | The Elastic Load Balancing instance's name.
--
-- /Note:/ Consider using 'elasticLoadBalancerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
elbElasticLoadBalancerName :: Lens.Lens' ElasticLoadBalancer (Lude.Maybe Lude.Text)
elbElasticLoadBalancerName = Lens.lens (elasticLoadBalancerName :: ElasticLoadBalancer -> Lude.Maybe Lude.Text) (\s a -> s {elasticLoadBalancerName = a} :: ElasticLoadBalancer)
{-# DEPRECATED elbElasticLoadBalancerName "Use generic-lens or generic-optics with 'elasticLoadBalancerName' instead." #-}

-- | The ID of the stack that the instance is associated with.
--
-- /Note:/ Consider using 'stackId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
elbStackId :: Lens.Lens' ElasticLoadBalancer (Lude.Maybe Lude.Text)
elbStackId = Lens.lens (stackId :: ElasticLoadBalancer -> Lude.Maybe Lude.Text) (\s a -> s {stackId = a} :: ElasticLoadBalancer)
{-# DEPRECATED elbStackId "Use generic-lens or generic-optics with 'stackId' instead." #-}

-- | A list of the EC2 instances that the Elastic Load Balancing instance is managing traffic for.
--
-- /Note:/ Consider using 'ec2InstanceIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
elbEC2InstanceIds :: Lens.Lens' ElasticLoadBalancer (Lude.Maybe [Lude.Text])
elbEC2InstanceIds = Lens.lens (ec2InstanceIds :: ElasticLoadBalancer -> Lude.Maybe [Lude.Text]) (\s a -> s {ec2InstanceIds = a} :: ElasticLoadBalancer)
{-# DEPRECATED elbEC2InstanceIds "Use generic-lens or generic-optics with 'ec2InstanceIds' instead." #-}

-- | The ID of the layer that the instance is attached to.
--
-- /Note:/ Consider using 'layerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
elbLayerId :: Lens.Lens' ElasticLoadBalancer (Lude.Maybe Lude.Text)
elbLayerId = Lens.lens (layerId :: ElasticLoadBalancer -> Lude.Maybe Lude.Text) (\s a -> s {layerId = a} :: ElasticLoadBalancer)
{-# DEPRECATED elbLayerId "Use generic-lens or generic-optics with 'layerId' instead." #-}

-- | The instance's public DNS name.
--
-- /Note:/ Consider using 'dnsName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
elbDNSName :: Lens.Lens' ElasticLoadBalancer (Lude.Maybe Lude.Text)
elbDNSName = Lens.lens (dnsName :: ElasticLoadBalancer -> Lude.Maybe Lude.Text) (\s a -> s {dnsName = a} :: ElasticLoadBalancer)
{-# DEPRECATED elbDNSName "Use generic-lens or generic-optics with 'dnsName' instead." #-}

instance Lude.FromJSON ElasticLoadBalancer where
  parseJSON =
    Lude.withObject
      "ElasticLoadBalancer"
      ( \x ->
          ElasticLoadBalancer'
            Lude.<$> (x Lude..:? "SubnetIds" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "VpcId")
            Lude.<*> (x Lude..:? "AvailabilityZones" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "Region")
            Lude.<*> (x Lude..:? "ElasticLoadBalancerName")
            Lude.<*> (x Lude..:? "StackId")
            Lude.<*> (x Lude..:? "Ec2InstanceIds" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "LayerId")
            Lude.<*> (x Lude..:? "DnsName")
      )
