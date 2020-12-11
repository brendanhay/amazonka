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
    lbdSourceSecurityGroup,
    lbdCanonicalHostedZoneName,
    lbdSecurityGroups,
    lbdHealthCheck,
    lbdLoadBalancerName,
    lbdCreatedTime,
    lbdVPCId,
    lbdSubnets,
    lbdAvailabilityZones,
    lbdBackendServerDescriptions,
    lbdCanonicalHostedZoneNameId,
    lbdInstances,
    lbdScheme,
    lbdListenerDescriptions,
    lbdDNSName,
    lbdPolicies,
  )
where

import Network.AWS.ELB.Internal
import Network.AWS.ELB.Types.BackendServerDescription
import Network.AWS.ELB.Types.HealthCheck
import Network.AWS.ELB.Types.Instance
import Network.AWS.ELB.Types.ListenerDescription
import Network.AWS.ELB.Types.Policies
import Network.AWS.ELB.Types.SourceSecurityGroup
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about a load balancer.
--
-- /See:/ 'mkLoadBalancerDescription' smart constructor.
data LoadBalancerDescription = LoadBalancerDescription'
  { sourceSecurityGroup ::
      Lude.Maybe SourceSecurityGroup,
    canonicalHostedZoneName ::
      Lude.Maybe Lude.Text,
    securityGroups :: Lude.Maybe [Lude.Text],
    healthCheck :: Lude.Maybe HealthCheck,
    loadBalancerName :: Lude.Maybe Lude.Text,
    createdTime :: Lude.Maybe Lude.ISO8601,
    vpcId :: Lude.Maybe Lude.Text,
    subnets :: Lude.Maybe [Lude.Text],
    availabilityZones :: Lude.Maybe [Lude.Text],
    backendServerDescriptions ::
      Lude.Maybe [BackendServerDescription],
    canonicalHostedZoneNameId ::
      Lude.Maybe Lude.Text,
    instances :: Lude.Maybe [Instance],
    scheme :: Lude.Maybe Lude.Text,
    listenerDescriptions ::
      Lude.Maybe [ListenerDescription],
    dnsName :: Lude.Maybe Lude.Text,
    policies :: Lude.Maybe Policies
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'LoadBalancerDescription' with the minimum fields required to make a request.
--
-- * 'availabilityZones' - The Availability Zones for the load balancer.
-- * 'backendServerDescriptions' - Information about your EC2 instances.
-- * 'canonicalHostedZoneName' - The DNS name of the load balancer.
--
-- For more information, see <https://docs.aws.amazon.com/elasticloadbalancing/latest/classic/using-domain-names-with-elb.html Configure a Custom Domain Name> in the /Classic Load Balancers Guide/ .
-- * 'canonicalHostedZoneNameId' - The ID of the Amazon Route 53 hosted zone for the load balancer.
-- * 'createdTime' - The date and time the load balancer was created.
-- * 'dnsName' - The DNS name of the load balancer.
-- * 'healthCheck' - Information about the health checks conducted on the load balancer.
-- * 'instances' - The IDs of the instances for the load balancer.
-- * 'listenerDescriptions' - The listeners for the load balancer.
-- * 'loadBalancerName' - The name of the load balancer.
-- * 'policies' - The policies defined for the load balancer.
-- * 'scheme' - The type of load balancer. Valid only for load balancers in a VPC.
--
-- If @Scheme@ is @internet-facing@ , the load balancer has a public DNS name that resolves to a public IP address.
-- If @Scheme@ is @internal@ , the load balancer has a public DNS name that resolves to a private IP address.
-- * 'securityGroups' - The security groups for the load balancer. Valid only for load balancers in a VPC.
-- * 'sourceSecurityGroup' - The security group for the load balancer, which you can use as part of your inbound rules for your registered instances. To only allow traffic from load balancers, add a security group rule that specifies this source security group as the inbound source.
-- * 'subnets' - The IDs of the subnets for the load balancer.
-- * 'vpcId' - The ID of the VPC for the load balancer.
mkLoadBalancerDescription ::
  LoadBalancerDescription
mkLoadBalancerDescription =
  LoadBalancerDescription'
    { sourceSecurityGroup = Lude.Nothing,
      canonicalHostedZoneName = Lude.Nothing,
      securityGroups = Lude.Nothing,
      healthCheck = Lude.Nothing,
      loadBalancerName = Lude.Nothing,
      createdTime = Lude.Nothing,
      vpcId = Lude.Nothing,
      subnets = Lude.Nothing,
      availabilityZones = Lude.Nothing,
      backendServerDescriptions = Lude.Nothing,
      canonicalHostedZoneNameId = Lude.Nothing,
      instances = Lude.Nothing,
      scheme = Lude.Nothing,
      listenerDescriptions = Lude.Nothing,
      dnsName = Lude.Nothing,
      policies = Lude.Nothing
    }

-- | The security group for the load balancer, which you can use as part of your inbound rules for your registered instances. To only allow traffic from load balancers, add a security group rule that specifies this source security group as the inbound source.
--
-- /Note:/ Consider using 'sourceSecurityGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbdSourceSecurityGroup :: Lens.Lens' LoadBalancerDescription (Lude.Maybe SourceSecurityGroup)
lbdSourceSecurityGroup = Lens.lens (sourceSecurityGroup :: LoadBalancerDescription -> Lude.Maybe SourceSecurityGroup) (\s a -> s {sourceSecurityGroup = a} :: LoadBalancerDescription)
{-# DEPRECATED lbdSourceSecurityGroup "Use generic-lens or generic-optics with 'sourceSecurityGroup' instead." #-}

-- | The DNS name of the load balancer.
--
-- For more information, see <https://docs.aws.amazon.com/elasticloadbalancing/latest/classic/using-domain-names-with-elb.html Configure a Custom Domain Name> in the /Classic Load Balancers Guide/ .
--
-- /Note:/ Consider using 'canonicalHostedZoneName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbdCanonicalHostedZoneName :: Lens.Lens' LoadBalancerDescription (Lude.Maybe Lude.Text)
lbdCanonicalHostedZoneName = Lens.lens (canonicalHostedZoneName :: LoadBalancerDescription -> Lude.Maybe Lude.Text) (\s a -> s {canonicalHostedZoneName = a} :: LoadBalancerDescription)
{-# DEPRECATED lbdCanonicalHostedZoneName "Use generic-lens or generic-optics with 'canonicalHostedZoneName' instead." #-}

-- | The security groups for the load balancer. Valid only for load balancers in a VPC.
--
-- /Note:/ Consider using 'securityGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbdSecurityGroups :: Lens.Lens' LoadBalancerDescription (Lude.Maybe [Lude.Text])
lbdSecurityGroups = Lens.lens (securityGroups :: LoadBalancerDescription -> Lude.Maybe [Lude.Text]) (\s a -> s {securityGroups = a} :: LoadBalancerDescription)
{-# DEPRECATED lbdSecurityGroups "Use generic-lens or generic-optics with 'securityGroups' instead." #-}

-- | Information about the health checks conducted on the load balancer.
--
-- /Note:/ Consider using 'healthCheck' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbdHealthCheck :: Lens.Lens' LoadBalancerDescription (Lude.Maybe HealthCheck)
lbdHealthCheck = Lens.lens (healthCheck :: LoadBalancerDescription -> Lude.Maybe HealthCheck) (\s a -> s {healthCheck = a} :: LoadBalancerDescription)
{-# DEPRECATED lbdHealthCheck "Use generic-lens or generic-optics with 'healthCheck' instead." #-}

-- | The name of the load balancer.
--
-- /Note:/ Consider using 'loadBalancerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbdLoadBalancerName :: Lens.Lens' LoadBalancerDescription (Lude.Maybe Lude.Text)
lbdLoadBalancerName = Lens.lens (loadBalancerName :: LoadBalancerDescription -> Lude.Maybe Lude.Text) (\s a -> s {loadBalancerName = a} :: LoadBalancerDescription)
{-# DEPRECATED lbdLoadBalancerName "Use generic-lens or generic-optics with 'loadBalancerName' instead." #-}

-- | The date and time the load balancer was created.
--
-- /Note:/ Consider using 'createdTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbdCreatedTime :: Lens.Lens' LoadBalancerDescription (Lude.Maybe Lude.ISO8601)
lbdCreatedTime = Lens.lens (createdTime :: LoadBalancerDescription -> Lude.Maybe Lude.ISO8601) (\s a -> s {createdTime = a} :: LoadBalancerDescription)
{-# DEPRECATED lbdCreatedTime "Use generic-lens or generic-optics with 'createdTime' instead." #-}

-- | The ID of the VPC for the load balancer.
--
-- /Note:/ Consider using 'vpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbdVPCId :: Lens.Lens' LoadBalancerDescription (Lude.Maybe Lude.Text)
lbdVPCId = Lens.lens (vpcId :: LoadBalancerDescription -> Lude.Maybe Lude.Text) (\s a -> s {vpcId = a} :: LoadBalancerDescription)
{-# DEPRECATED lbdVPCId "Use generic-lens or generic-optics with 'vpcId' instead." #-}

-- | The IDs of the subnets for the load balancer.
--
-- /Note:/ Consider using 'subnets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbdSubnets :: Lens.Lens' LoadBalancerDescription (Lude.Maybe [Lude.Text])
lbdSubnets = Lens.lens (subnets :: LoadBalancerDescription -> Lude.Maybe [Lude.Text]) (\s a -> s {subnets = a} :: LoadBalancerDescription)
{-# DEPRECATED lbdSubnets "Use generic-lens or generic-optics with 'subnets' instead." #-}

-- | The Availability Zones for the load balancer.
--
-- /Note:/ Consider using 'availabilityZones' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbdAvailabilityZones :: Lens.Lens' LoadBalancerDescription (Lude.Maybe [Lude.Text])
lbdAvailabilityZones = Lens.lens (availabilityZones :: LoadBalancerDescription -> Lude.Maybe [Lude.Text]) (\s a -> s {availabilityZones = a} :: LoadBalancerDescription)
{-# DEPRECATED lbdAvailabilityZones "Use generic-lens or generic-optics with 'availabilityZones' instead." #-}

-- | Information about your EC2 instances.
--
-- /Note:/ Consider using 'backendServerDescriptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbdBackendServerDescriptions :: Lens.Lens' LoadBalancerDescription (Lude.Maybe [BackendServerDescription])
lbdBackendServerDescriptions = Lens.lens (backendServerDescriptions :: LoadBalancerDescription -> Lude.Maybe [BackendServerDescription]) (\s a -> s {backendServerDescriptions = a} :: LoadBalancerDescription)
{-# DEPRECATED lbdBackendServerDescriptions "Use generic-lens or generic-optics with 'backendServerDescriptions' instead." #-}

-- | The ID of the Amazon Route 53 hosted zone for the load balancer.
--
-- /Note:/ Consider using 'canonicalHostedZoneNameId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbdCanonicalHostedZoneNameId :: Lens.Lens' LoadBalancerDescription (Lude.Maybe Lude.Text)
lbdCanonicalHostedZoneNameId = Lens.lens (canonicalHostedZoneNameId :: LoadBalancerDescription -> Lude.Maybe Lude.Text) (\s a -> s {canonicalHostedZoneNameId = a} :: LoadBalancerDescription)
{-# DEPRECATED lbdCanonicalHostedZoneNameId "Use generic-lens or generic-optics with 'canonicalHostedZoneNameId' instead." #-}

-- | The IDs of the instances for the load balancer.
--
-- /Note:/ Consider using 'instances' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbdInstances :: Lens.Lens' LoadBalancerDescription (Lude.Maybe [Instance])
lbdInstances = Lens.lens (instances :: LoadBalancerDescription -> Lude.Maybe [Instance]) (\s a -> s {instances = a} :: LoadBalancerDescription)
{-# DEPRECATED lbdInstances "Use generic-lens or generic-optics with 'instances' instead." #-}

-- | The type of load balancer. Valid only for load balancers in a VPC.
--
-- If @Scheme@ is @internet-facing@ , the load balancer has a public DNS name that resolves to a public IP address.
-- If @Scheme@ is @internal@ , the load balancer has a public DNS name that resolves to a private IP address.
--
-- /Note:/ Consider using 'scheme' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbdScheme :: Lens.Lens' LoadBalancerDescription (Lude.Maybe Lude.Text)
lbdScheme = Lens.lens (scheme :: LoadBalancerDescription -> Lude.Maybe Lude.Text) (\s a -> s {scheme = a} :: LoadBalancerDescription)
{-# DEPRECATED lbdScheme "Use generic-lens or generic-optics with 'scheme' instead." #-}

-- | The listeners for the load balancer.
--
-- /Note:/ Consider using 'listenerDescriptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbdListenerDescriptions :: Lens.Lens' LoadBalancerDescription (Lude.Maybe [ListenerDescription])
lbdListenerDescriptions = Lens.lens (listenerDescriptions :: LoadBalancerDescription -> Lude.Maybe [ListenerDescription]) (\s a -> s {listenerDescriptions = a} :: LoadBalancerDescription)
{-# DEPRECATED lbdListenerDescriptions "Use generic-lens or generic-optics with 'listenerDescriptions' instead." #-}

-- | The DNS name of the load balancer.
--
-- /Note:/ Consider using 'dnsName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbdDNSName :: Lens.Lens' LoadBalancerDescription (Lude.Maybe Lude.Text)
lbdDNSName = Lens.lens (dnsName :: LoadBalancerDescription -> Lude.Maybe Lude.Text) (\s a -> s {dnsName = a} :: LoadBalancerDescription)
{-# DEPRECATED lbdDNSName "Use generic-lens or generic-optics with 'dnsName' instead." #-}

-- | The policies defined for the load balancer.
--
-- /Note:/ Consider using 'policies' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbdPolicies :: Lens.Lens' LoadBalancerDescription (Lude.Maybe Policies)
lbdPolicies = Lens.lens (policies :: LoadBalancerDescription -> Lude.Maybe Policies) (\s a -> s {policies = a} :: LoadBalancerDescription)
{-# DEPRECATED lbdPolicies "Use generic-lens or generic-optics with 'policies' instead." #-}

instance Lude.FromXML LoadBalancerDescription where
  parseXML x =
    LoadBalancerDescription'
      Lude.<$> (x Lude..@? "SourceSecurityGroup")
      Lude.<*> (x Lude..@? "CanonicalHostedZoneName")
      Lude.<*> ( x Lude..@? "SecurityGroups" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "member")
               )
      Lude.<*> (x Lude..@? "HealthCheck")
      Lude.<*> (x Lude..@? "LoadBalancerName")
      Lude.<*> (x Lude..@? "CreatedTime")
      Lude.<*> (x Lude..@? "VPCId")
      Lude.<*> ( x Lude..@? "Subnets" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "member")
               )
      Lude.<*> ( x Lude..@? "AvailabilityZones" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "member")
               )
      Lude.<*> ( x Lude..@? "BackendServerDescriptions" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "member")
               )
      Lude.<*> (x Lude..@? "CanonicalHostedZoneNameID")
      Lude.<*> ( x Lude..@? "Instances" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "member")
               )
      Lude.<*> (x Lude..@? "Scheme")
      Lude.<*> ( x Lude..@? "ListenerDescriptions" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "member")
               )
      Lude.<*> (x Lude..@? "DNSName")
      Lude.<*> (x Lude..@? "Policies")
