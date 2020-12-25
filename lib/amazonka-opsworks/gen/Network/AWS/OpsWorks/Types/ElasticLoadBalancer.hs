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
    elbAvailabilityZones,
    elbDnsName,
    elbEc2InstanceIds,
    elbElasticLoadBalancerName,
    elbLayerId,
    elbRegion,
    elbStackId,
    elbSubnetIds,
    elbVpcId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.OpsWorks.Types.DnsName as Types
import qualified Network.AWS.OpsWorks.Types.ElasticLoadBalancerName as Types
import qualified Network.AWS.OpsWorks.Types.LayerId as Types
import qualified Network.AWS.OpsWorks.Types.Region as Types
import qualified Network.AWS.OpsWorks.Types.StackId as Types
import qualified Network.AWS.OpsWorks.Types.String as Types
import qualified Network.AWS.OpsWorks.Types.VpcId as Types
import qualified Network.AWS.Prelude as Core

-- | Describes an Elastic Load Balancing instance.
--
-- /See:/ 'mkElasticLoadBalancer' smart constructor.
data ElasticLoadBalancer = ElasticLoadBalancer'
  { -- | A list of Availability Zones.
    availabilityZones :: Core.Maybe [Types.String],
    -- | The instance's public DNS name.
    dnsName :: Core.Maybe Types.DnsName,
    -- | A list of the EC2 instances that the Elastic Load Balancing instance is managing traffic for.
    ec2InstanceIds :: Core.Maybe [Types.String],
    -- | The Elastic Load Balancing instance's name.
    elasticLoadBalancerName :: Core.Maybe Types.ElasticLoadBalancerName,
    -- | The ID of the layer that the instance is attached to.
    layerId :: Core.Maybe Types.LayerId,
    -- | The instance's AWS region.
    region :: Core.Maybe Types.Region,
    -- | The ID of the stack that the instance is associated with.
    stackId :: Core.Maybe Types.StackId,
    -- | A list of subnet IDs, if the stack is running in a VPC.
    subnetIds :: Core.Maybe [Types.String],
    -- | The VPC ID.
    vpcId :: Core.Maybe Types.VpcId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ElasticLoadBalancer' value with any optional fields omitted.
mkElasticLoadBalancer ::
  ElasticLoadBalancer
mkElasticLoadBalancer =
  ElasticLoadBalancer'
    { availabilityZones = Core.Nothing,
      dnsName = Core.Nothing,
      ec2InstanceIds = Core.Nothing,
      elasticLoadBalancerName = Core.Nothing,
      layerId = Core.Nothing,
      region = Core.Nothing,
      stackId = Core.Nothing,
      subnetIds = Core.Nothing,
      vpcId = Core.Nothing
    }

-- | A list of Availability Zones.
--
-- /Note:/ Consider using 'availabilityZones' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
elbAvailabilityZones :: Lens.Lens' ElasticLoadBalancer (Core.Maybe [Types.String])
elbAvailabilityZones = Lens.field @"availabilityZones"
{-# DEPRECATED elbAvailabilityZones "Use generic-lens or generic-optics with 'availabilityZones' instead." #-}

-- | The instance's public DNS name.
--
-- /Note:/ Consider using 'dnsName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
elbDnsName :: Lens.Lens' ElasticLoadBalancer (Core.Maybe Types.DnsName)
elbDnsName = Lens.field @"dnsName"
{-# DEPRECATED elbDnsName "Use generic-lens or generic-optics with 'dnsName' instead." #-}

-- | A list of the EC2 instances that the Elastic Load Balancing instance is managing traffic for.
--
-- /Note:/ Consider using 'ec2InstanceIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
elbEc2InstanceIds :: Lens.Lens' ElasticLoadBalancer (Core.Maybe [Types.String])
elbEc2InstanceIds = Lens.field @"ec2InstanceIds"
{-# DEPRECATED elbEc2InstanceIds "Use generic-lens or generic-optics with 'ec2InstanceIds' instead." #-}

-- | The Elastic Load Balancing instance's name.
--
-- /Note:/ Consider using 'elasticLoadBalancerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
elbElasticLoadBalancerName :: Lens.Lens' ElasticLoadBalancer (Core.Maybe Types.ElasticLoadBalancerName)
elbElasticLoadBalancerName = Lens.field @"elasticLoadBalancerName"
{-# DEPRECATED elbElasticLoadBalancerName "Use generic-lens or generic-optics with 'elasticLoadBalancerName' instead." #-}

-- | The ID of the layer that the instance is attached to.
--
-- /Note:/ Consider using 'layerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
elbLayerId :: Lens.Lens' ElasticLoadBalancer (Core.Maybe Types.LayerId)
elbLayerId = Lens.field @"layerId"
{-# DEPRECATED elbLayerId "Use generic-lens or generic-optics with 'layerId' instead." #-}

-- | The instance's AWS region.
--
-- /Note:/ Consider using 'region' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
elbRegion :: Lens.Lens' ElasticLoadBalancer (Core.Maybe Types.Region)
elbRegion = Lens.field @"region"
{-# DEPRECATED elbRegion "Use generic-lens or generic-optics with 'region' instead." #-}

-- | The ID of the stack that the instance is associated with.
--
-- /Note:/ Consider using 'stackId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
elbStackId :: Lens.Lens' ElasticLoadBalancer (Core.Maybe Types.StackId)
elbStackId = Lens.field @"stackId"
{-# DEPRECATED elbStackId "Use generic-lens or generic-optics with 'stackId' instead." #-}

-- | A list of subnet IDs, if the stack is running in a VPC.
--
-- /Note:/ Consider using 'subnetIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
elbSubnetIds :: Lens.Lens' ElasticLoadBalancer (Core.Maybe [Types.String])
elbSubnetIds = Lens.field @"subnetIds"
{-# DEPRECATED elbSubnetIds "Use generic-lens or generic-optics with 'subnetIds' instead." #-}

-- | The VPC ID.
--
-- /Note:/ Consider using 'vpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
elbVpcId :: Lens.Lens' ElasticLoadBalancer (Core.Maybe Types.VpcId)
elbVpcId = Lens.field @"vpcId"
{-# DEPRECATED elbVpcId "Use generic-lens or generic-optics with 'vpcId' instead." #-}

instance Core.FromJSON ElasticLoadBalancer where
  parseJSON =
    Core.withObject "ElasticLoadBalancer" Core.$
      \x ->
        ElasticLoadBalancer'
          Core.<$> (x Core..:? "AvailabilityZones")
          Core.<*> (x Core..:? "DnsName")
          Core.<*> (x Core..:? "Ec2InstanceIds")
          Core.<*> (x Core..:? "ElasticLoadBalancerName")
          Core.<*> (x Core..:? "LayerId")
          Core.<*> (x Core..:? "Region")
          Core.<*> (x Core..:? "StackId")
          Core.<*> (x Core..:? "SubnetIds")
          Core.<*> (x Core..:? "VpcId")
