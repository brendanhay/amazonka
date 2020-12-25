{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELB.Types.LoadBalancerDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ELB.Types.LoadBalancerDescription
  ( LoadBalancerDescription (..),

    -- * Smart constructor
    mkLoadBalancerDescription,

    -- * Lenses
    lbdAvailabilityZones,
    lbdBackendServerDescriptions,
    lbdCanonicalHostedZoneName,
    lbdCanonicalHostedZoneNameID,
    lbdCreatedTime,
    lbdDNSName,
    lbdHealthCheck,
    lbdInstances,
    lbdListenerDescriptions,
    lbdLoadBalancerName,
    lbdPolicies,
    lbdScheme,
    lbdSecurityGroups,
    lbdSourceSecurityGroup,
    lbdSubnets,
    lbdVPCId,
  )
where

import qualified Network.AWS.ELB.Internal as Types
import qualified Network.AWS.ELB.Types.AccessPointName as Types
import qualified Network.AWS.ELB.Types.AvailabilityZone as Types
import qualified Network.AWS.ELB.Types.BackendServerDescription as Types
import qualified Network.AWS.ELB.Types.CanonicalHostedZoneName as Types
import qualified Network.AWS.ELB.Types.CanonicalHostedZoneNameID as Types
import qualified Network.AWS.ELB.Types.DNSName as Types
import qualified Network.AWS.ELB.Types.HealthCheck as Types
import qualified Network.AWS.ELB.Types.Instance as Types
import qualified Network.AWS.ELB.Types.ListenerDescription as Types
import qualified Network.AWS.ELB.Types.LoadBalancerScheme as Types
import qualified Network.AWS.ELB.Types.Policies as Types
import qualified Network.AWS.ELB.Types.SecurityGroupId as Types
import qualified Network.AWS.ELB.Types.SourceSecurityGroup as Types
import qualified Network.AWS.ELB.Types.SubnetId as Types
import qualified Network.AWS.ELB.Types.VPCId as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about a load balancer.
--
-- /See:/ 'mkLoadBalancerDescription' smart constructor.
data LoadBalancerDescription = LoadBalancerDescription'
  { -- | The Availability Zones for the load balancer.
    availabilityZones :: Core.Maybe [Types.AvailabilityZone],
    -- | Information about your EC2 instances.
    backendServerDescriptions :: Core.Maybe [Types.BackendServerDescription],
    -- | The DNS name of the load balancer.
    --
    -- For more information, see <https://docs.aws.amazon.com/elasticloadbalancing/latest/classic/using-domain-names-with-elb.html Configure a Custom Domain Name> in the /Classic Load Balancers Guide/ .
    canonicalHostedZoneName :: Core.Maybe Types.CanonicalHostedZoneName,
    -- | The ID of the Amazon Route 53 hosted zone for the load balancer.
    canonicalHostedZoneNameID :: Core.Maybe Types.CanonicalHostedZoneNameID,
    -- | The date and time the load balancer was created.
    createdTime :: Core.Maybe Core.UTCTime,
    -- | The DNS name of the load balancer.
    dNSName :: Core.Maybe Types.DNSName,
    -- | Information about the health checks conducted on the load balancer.
    healthCheck :: Core.Maybe Types.HealthCheck,
    -- | The IDs of the instances for the load balancer.
    instances :: Core.Maybe [Types.Instance],
    -- | The listeners for the load balancer.
    listenerDescriptions :: Core.Maybe [Types.ListenerDescription],
    -- | The name of the load balancer.
    loadBalancerName :: Core.Maybe Types.AccessPointName,
    -- | The policies defined for the load balancer.
    policies :: Core.Maybe Types.Policies,
    -- | The type of load balancer. Valid only for load balancers in a VPC.
    --
    -- If @Scheme@ is @internet-facing@ , the load balancer has a public DNS name that resolves to a public IP address.
    -- If @Scheme@ is @internal@ , the load balancer has a public DNS name that resolves to a private IP address.
    scheme :: Core.Maybe Types.LoadBalancerScheme,
    -- | The security groups for the load balancer. Valid only for load balancers in a VPC.
    securityGroups :: Core.Maybe [Types.SecurityGroupId],
    -- | The security group for the load balancer, which you can use as part of your inbound rules for your registered instances. To only allow traffic from load balancers, add a security group rule that specifies this source security group as the inbound source.
    sourceSecurityGroup :: Core.Maybe Types.SourceSecurityGroup,
    -- | The IDs of the subnets for the load balancer.
    subnets :: Core.Maybe [Types.SubnetId],
    -- | The ID of the VPC for the load balancer.
    vPCId :: Core.Maybe Types.VPCId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'LoadBalancerDescription' value with any optional fields omitted.
mkLoadBalancerDescription ::
  LoadBalancerDescription
mkLoadBalancerDescription =
  LoadBalancerDescription'
    { availabilityZones = Core.Nothing,
      backendServerDescriptions = Core.Nothing,
      canonicalHostedZoneName = Core.Nothing,
      canonicalHostedZoneNameID = Core.Nothing,
      createdTime = Core.Nothing,
      dNSName = Core.Nothing,
      healthCheck = Core.Nothing,
      instances = Core.Nothing,
      listenerDescriptions = Core.Nothing,
      loadBalancerName = Core.Nothing,
      policies = Core.Nothing,
      scheme = Core.Nothing,
      securityGroups = Core.Nothing,
      sourceSecurityGroup = Core.Nothing,
      subnets = Core.Nothing,
      vPCId = Core.Nothing
    }

-- | The Availability Zones for the load balancer.
--
-- /Note:/ Consider using 'availabilityZones' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbdAvailabilityZones :: Lens.Lens' LoadBalancerDescription (Core.Maybe [Types.AvailabilityZone])
lbdAvailabilityZones = Lens.field @"availabilityZones"
{-# DEPRECATED lbdAvailabilityZones "Use generic-lens or generic-optics with 'availabilityZones' instead." #-}

-- | Information about your EC2 instances.
--
-- /Note:/ Consider using 'backendServerDescriptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbdBackendServerDescriptions :: Lens.Lens' LoadBalancerDescription (Core.Maybe [Types.BackendServerDescription])
lbdBackendServerDescriptions = Lens.field @"backendServerDescriptions"
{-# DEPRECATED lbdBackendServerDescriptions "Use generic-lens or generic-optics with 'backendServerDescriptions' instead." #-}

-- | The DNS name of the load balancer.
--
-- For more information, see <https://docs.aws.amazon.com/elasticloadbalancing/latest/classic/using-domain-names-with-elb.html Configure a Custom Domain Name> in the /Classic Load Balancers Guide/ .
--
-- /Note:/ Consider using 'canonicalHostedZoneName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbdCanonicalHostedZoneName :: Lens.Lens' LoadBalancerDescription (Core.Maybe Types.CanonicalHostedZoneName)
lbdCanonicalHostedZoneName = Lens.field @"canonicalHostedZoneName"
{-# DEPRECATED lbdCanonicalHostedZoneName "Use generic-lens or generic-optics with 'canonicalHostedZoneName' instead." #-}

-- | The ID of the Amazon Route 53 hosted zone for the load balancer.
--
-- /Note:/ Consider using 'canonicalHostedZoneNameID' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbdCanonicalHostedZoneNameID :: Lens.Lens' LoadBalancerDescription (Core.Maybe Types.CanonicalHostedZoneNameID)
lbdCanonicalHostedZoneNameID = Lens.field @"canonicalHostedZoneNameID"
{-# DEPRECATED lbdCanonicalHostedZoneNameID "Use generic-lens or generic-optics with 'canonicalHostedZoneNameID' instead." #-}

-- | The date and time the load balancer was created.
--
-- /Note:/ Consider using 'createdTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbdCreatedTime :: Lens.Lens' LoadBalancerDescription (Core.Maybe Core.UTCTime)
lbdCreatedTime = Lens.field @"createdTime"
{-# DEPRECATED lbdCreatedTime "Use generic-lens or generic-optics with 'createdTime' instead." #-}

-- | The DNS name of the load balancer.
--
-- /Note:/ Consider using 'dNSName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbdDNSName :: Lens.Lens' LoadBalancerDescription (Core.Maybe Types.DNSName)
lbdDNSName = Lens.field @"dNSName"
{-# DEPRECATED lbdDNSName "Use generic-lens or generic-optics with 'dNSName' instead." #-}

-- | Information about the health checks conducted on the load balancer.
--
-- /Note:/ Consider using 'healthCheck' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbdHealthCheck :: Lens.Lens' LoadBalancerDescription (Core.Maybe Types.HealthCheck)
lbdHealthCheck = Lens.field @"healthCheck"
{-# DEPRECATED lbdHealthCheck "Use generic-lens or generic-optics with 'healthCheck' instead." #-}

-- | The IDs of the instances for the load balancer.
--
-- /Note:/ Consider using 'instances' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbdInstances :: Lens.Lens' LoadBalancerDescription (Core.Maybe [Types.Instance])
lbdInstances = Lens.field @"instances"
{-# DEPRECATED lbdInstances "Use generic-lens or generic-optics with 'instances' instead." #-}

-- | The listeners for the load balancer.
--
-- /Note:/ Consider using 'listenerDescriptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbdListenerDescriptions :: Lens.Lens' LoadBalancerDescription (Core.Maybe [Types.ListenerDescription])
lbdListenerDescriptions = Lens.field @"listenerDescriptions"
{-# DEPRECATED lbdListenerDescriptions "Use generic-lens or generic-optics with 'listenerDescriptions' instead." #-}

-- | The name of the load balancer.
--
-- /Note:/ Consider using 'loadBalancerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbdLoadBalancerName :: Lens.Lens' LoadBalancerDescription (Core.Maybe Types.AccessPointName)
lbdLoadBalancerName = Lens.field @"loadBalancerName"
{-# DEPRECATED lbdLoadBalancerName "Use generic-lens or generic-optics with 'loadBalancerName' instead." #-}

-- | The policies defined for the load balancer.
--
-- /Note:/ Consider using 'policies' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbdPolicies :: Lens.Lens' LoadBalancerDescription (Core.Maybe Types.Policies)
lbdPolicies = Lens.field @"policies"
{-# DEPRECATED lbdPolicies "Use generic-lens or generic-optics with 'policies' instead." #-}

-- | The type of load balancer. Valid only for load balancers in a VPC.
--
-- If @Scheme@ is @internet-facing@ , the load balancer has a public DNS name that resolves to a public IP address.
-- If @Scheme@ is @internal@ , the load balancer has a public DNS name that resolves to a private IP address.
--
-- /Note:/ Consider using 'scheme' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbdScheme :: Lens.Lens' LoadBalancerDescription (Core.Maybe Types.LoadBalancerScheme)
lbdScheme = Lens.field @"scheme"
{-# DEPRECATED lbdScheme "Use generic-lens or generic-optics with 'scheme' instead." #-}

-- | The security groups for the load balancer. Valid only for load balancers in a VPC.
--
-- /Note:/ Consider using 'securityGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbdSecurityGroups :: Lens.Lens' LoadBalancerDescription (Core.Maybe [Types.SecurityGroupId])
lbdSecurityGroups = Lens.field @"securityGroups"
{-# DEPRECATED lbdSecurityGroups "Use generic-lens or generic-optics with 'securityGroups' instead." #-}

-- | The security group for the load balancer, which you can use as part of your inbound rules for your registered instances. To only allow traffic from load balancers, add a security group rule that specifies this source security group as the inbound source.
--
-- /Note:/ Consider using 'sourceSecurityGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbdSourceSecurityGroup :: Lens.Lens' LoadBalancerDescription (Core.Maybe Types.SourceSecurityGroup)
lbdSourceSecurityGroup = Lens.field @"sourceSecurityGroup"
{-# DEPRECATED lbdSourceSecurityGroup "Use generic-lens or generic-optics with 'sourceSecurityGroup' instead." #-}

-- | The IDs of the subnets for the load balancer.
--
-- /Note:/ Consider using 'subnets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbdSubnets :: Lens.Lens' LoadBalancerDescription (Core.Maybe [Types.SubnetId])
lbdSubnets = Lens.field @"subnets"
{-# DEPRECATED lbdSubnets "Use generic-lens or generic-optics with 'subnets' instead." #-}

-- | The ID of the VPC for the load balancer.
--
-- /Note:/ Consider using 'vPCId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbdVPCId :: Lens.Lens' LoadBalancerDescription (Core.Maybe Types.VPCId)
lbdVPCId = Lens.field @"vPCId"
{-# DEPRECATED lbdVPCId "Use generic-lens or generic-optics with 'vPCId' instead." #-}

instance Core.FromXML LoadBalancerDescription where
  parseXML x =
    LoadBalancerDescription'
      Core.<$> ( x Core..@? "AvailabilityZones"
                   Core..<@> Core.parseXMLList "member"
               )
      Core.<*> ( x Core..@? "BackendServerDescriptions"
                   Core..<@> Core.parseXMLList "member"
               )
      Core.<*> (x Core..@? "CanonicalHostedZoneName")
      Core.<*> (x Core..@? "CanonicalHostedZoneNameID")
      Core.<*> (x Core..@? "CreatedTime")
      Core.<*> (x Core..@? "DNSName")
      Core.<*> (x Core..@? "HealthCheck")
      Core.<*> (x Core..@? "Instances" Core..<@> Core.parseXMLList "member")
      Core.<*> ( x Core..@? "ListenerDescriptions"
                   Core..<@> Core.parseXMLList "member"
               )
      Core.<*> (x Core..@? "LoadBalancerName")
      Core.<*> (x Core..@? "Policies")
      Core.<*> (x Core..@? "Scheme")
      Core.<*> (x Core..@? "SecurityGroups" Core..<@> Core.parseXMLList "member")
      Core.<*> (x Core..@? "SourceSecurityGroup")
      Core.<*> (x Core..@? "Subnets" Core..<@> Core.parseXMLList "member")
      Core.<*> (x Core..@? "VPCId")
