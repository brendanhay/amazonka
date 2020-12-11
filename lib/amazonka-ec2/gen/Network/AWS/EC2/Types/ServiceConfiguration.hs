-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ServiceConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ServiceConfiguration
  ( ServiceConfiguration (..),

    -- * Smart constructor
    mkServiceConfiguration,

    -- * Lenses
    scNetworkLoadBalancerARNs,
    scBaseEndpointDNSNames,
    scAvailabilityZones,
    scGatewayLoadBalancerARNs,
    scManagesVPCEndpoints,
    scServiceName,
    scServiceState,
    scServiceType,
    scAcceptanceRequired,
    scServiceId,
    scPrivateDNSName,
    scPrivateDNSNameConfiguration,
    scTags,
  )
where

import Network.AWS.EC2.Types.PrivateDNSNameConfiguration
import Network.AWS.EC2.Types.ServiceState
import Network.AWS.EC2.Types.ServiceTypeDetail
import Network.AWS.EC2.Types.Tag
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a service configuration for a VPC endpoint service.
--
-- /See:/ 'mkServiceConfiguration' smart constructor.
data ServiceConfiguration = ServiceConfiguration'
  { networkLoadBalancerARNs ::
      Lude.Maybe [Lude.Text],
    baseEndpointDNSNames :: Lude.Maybe [Lude.Text],
    availabilityZones :: Lude.Maybe [Lude.Text],
    gatewayLoadBalancerARNs :: Lude.Maybe [Lude.Text],
    managesVPCEndpoints :: Lude.Maybe Lude.Bool,
    serviceName :: Lude.Maybe Lude.Text,
    serviceState :: Lude.Maybe ServiceState,
    serviceType :: Lude.Maybe [ServiceTypeDetail],
    acceptanceRequired :: Lude.Maybe Lude.Bool,
    serviceId :: Lude.Maybe Lude.Text,
    privateDNSName :: Lude.Maybe Lude.Text,
    privateDNSNameConfiguration ::
      Lude.Maybe PrivateDNSNameConfiguration,
    tags :: Lude.Maybe [Tag]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ServiceConfiguration' with the minimum fields required to make a request.
--
-- * 'acceptanceRequired' - Indicates whether requests from other AWS accounts to create an endpoint to the service must first be accepted.
-- * 'availabilityZones' - The Availability Zones in which the service is available.
-- * 'baseEndpointDNSNames' - The DNS names for the service.
-- * 'gatewayLoadBalancerARNs' - The Amazon Resource Names (ARNs) of the Gateway Load Balancers for the service.
-- * 'managesVPCEndpoints' - Indicates whether the service manages its VPC endpoints. Management of the service VPC endpoints using the VPC endpoint API is restricted.
-- * 'networkLoadBalancerARNs' - The Amazon Resource Names (ARNs) of the Network Load Balancers for the service.
-- * 'privateDNSName' - The private DNS name for the service.
-- * 'privateDNSNameConfiguration' - Information about the endpoint service private DNS name configuration.
-- * 'serviceId' - The ID of the service.
-- * 'serviceName' - The name of the service.
-- * 'serviceState' - The service state.
-- * 'serviceType' - The type of service.
-- * 'tags' - Any tags assigned to the service.
mkServiceConfiguration ::
  ServiceConfiguration
mkServiceConfiguration =
  ServiceConfiguration'
    { networkLoadBalancerARNs = Lude.Nothing,
      baseEndpointDNSNames = Lude.Nothing,
      availabilityZones = Lude.Nothing,
      gatewayLoadBalancerARNs = Lude.Nothing,
      managesVPCEndpoints = Lude.Nothing,
      serviceName = Lude.Nothing,
      serviceState = Lude.Nothing,
      serviceType = Lude.Nothing,
      acceptanceRequired = Lude.Nothing,
      serviceId = Lude.Nothing,
      privateDNSName = Lude.Nothing,
      privateDNSNameConfiguration = Lude.Nothing,
      tags = Lude.Nothing
    }

-- | The Amazon Resource Names (ARNs) of the Network Load Balancers for the service.
--
-- /Note:/ Consider using 'networkLoadBalancerARNs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scNetworkLoadBalancerARNs :: Lens.Lens' ServiceConfiguration (Lude.Maybe [Lude.Text])
scNetworkLoadBalancerARNs = Lens.lens (networkLoadBalancerARNs :: ServiceConfiguration -> Lude.Maybe [Lude.Text]) (\s a -> s {networkLoadBalancerARNs = a} :: ServiceConfiguration)
{-# DEPRECATED scNetworkLoadBalancerARNs "Use generic-lens or generic-optics with 'networkLoadBalancerARNs' instead." #-}

-- | The DNS names for the service.
--
-- /Note:/ Consider using 'baseEndpointDNSNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scBaseEndpointDNSNames :: Lens.Lens' ServiceConfiguration (Lude.Maybe [Lude.Text])
scBaseEndpointDNSNames = Lens.lens (baseEndpointDNSNames :: ServiceConfiguration -> Lude.Maybe [Lude.Text]) (\s a -> s {baseEndpointDNSNames = a} :: ServiceConfiguration)
{-# DEPRECATED scBaseEndpointDNSNames "Use generic-lens or generic-optics with 'baseEndpointDNSNames' instead." #-}

-- | The Availability Zones in which the service is available.
--
-- /Note:/ Consider using 'availabilityZones' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scAvailabilityZones :: Lens.Lens' ServiceConfiguration (Lude.Maybe [Lude.Text])
scAvailabilityZones = Lens.lens (availabilityZones :: ServiceConfiguration -> Lude.Maybe [Lude.Text]) (\s a -> s {availabilityZones = a} :: ServiceConfiguration)
{-# DEPRECATED scAvailabilityZones "Use generic-lens or generic-optics with 'availabilityZones' instead." #-}

-- | The Amazon Resource Names (ARNs) of the Gateway Load Balancers for the service.
--
-- /Note:/ Consider using 'gatewayLoadBalancerARNs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scGatewayLoadBalancerARNs :: Lens.Lens' ServiceConfiguration (Lude.Maybe [Lude.Text])
scGatewayLoadBalancerARNs = Lens.lens (gatewayLoadBalancerARNs :: ServiceConfiguration -> Lude.Maybe [Lude.Text]) (\s a -> s {gatewayLoadBalancerARNs = a} :: ServiceConfiguration)
{-# DEPRECATED scGatewayLoadBalancerARNs "Use generic-lens or generic-optics with 'gatewayLoadBalancerARNs' instead." #-}

-- | Indicates whether the service manages its VPC endpoints. Management of the service VPC endpoints using the VPC endpoint API is restricted.
--
-- /Note:/ Consider using 'managesVPCEndpoints' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scManagesVPCEndpoints :: Lens.Lens' ServiceConfiguration (Lude.Maybe Lude.Bool)
scManagesVPCEndpoints = Lens.lens (managesVPCEndpoints :: ServiceConfiguration -> Lude.Maybe Lude.Bool) (\s a -> s {managesVPCEndpoints = a} :: ServiceConfiguration)
{-# DEPRECATED scManagesVPCEndpoints "Use generic-lens or generic-optics with 'managesVPCEndpoints' instead." #-}

-- | The name of the service.
--
-- /Note:/ Consider using 'serviceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scServiceName :: Lens.Lens' ServiceConfiguration (Lude.Maybe Lude.Text)
scServiceName = Lens.lens (serviceName :: ServiceConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {serviceName = a} :: ServiceConfiguration)
{-# DEPRECATED scServiceName "Use generic-lens or generic-optics with 'serviceName' instead." #-}

-- | The service state.
--
-- /Note:/ Consider using 'serviceState' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scServiceState :: Lens.Lens' ServiceConfiguration (Lude.Maybe ServiceState)
scServiceState = Lens.lens (serviceState :: ServiceConfiguration -> Lude.Maybe ServiceState) (\s a -> s {serviceState = a} :: ServiceConfiguration)
{-# DEPRECATED scServiceState "Use generic-lens or generic-optics with 'serviceState' instead." #-}

-- | The type of service.
--
-- /Note:/ Consider using 'serviceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scServiceType :: Lens.Lens' ServiceConfiguration (Lude.Maybe [ServiceTypeDetail])
scServiceType = Lens.lens (serviceType :: ServiceConfiguration -> Lude.Maybe [ServiceTypeDetail]) (\s a -> s {serviceType = a} :: ServiceConfiguration)
{-# DEPRECATED scServiceType "Use generic-lens or generic-optics with 'serviceType' instead." #-}

-- | Indicates whether requests from other AWS accounts to create an endpoint to the service must first be accepted.
--
-- /Note:/ Consider using 'acceptanceRequired' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scAcceptanceRequired :: Lens.Lens' ServiceConfiguration (Lude.Maybe Lude.Bool)
scAcceptanceRequired = Lens.lens (acceptanceRequired :: ServiceConfiguration -> Lude.Maybe Lude.Bool) (\s a -> s {acceptanceRequired = a} :: ServiceConfiguration)
{-# DEPRECATED scAcceptanceRequired "Use generic-lens or generic-optics with 'acceptanceRequired' instead." #-}

-- | The ID of the service.
--
-- /Note:/ Consider using 'serviceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scServiceId :: Lens.Lens' ServiceConfiguration (Lude.Maybe Lude.Text)
scServiceId = Lens.lens (serviceId :: ServiceConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {serviceId = a} :: ServiceConfiguration)
{-# DEPRECATED scServiceId "Use generic-lens or generic-optics with 'serviceId' instead." #-}

-- | The private DNS name for the service.
--
-- /Note:/ Consider using 'privateDNSName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scPrivateDNSName :: Lens.Lens' ServiceConfiguration (Lude.Maybe Lude.Text)
scPrivateDNSName = Lens.lens (privateDNSName :: ServiceConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {privateDNSName = a} :: ServiceConfiguration)
{-# DEPRECATED scPrivateDNSName "Use generic-lens or generic-optics with 'privateDNSName' instead." #-}

-- | Information about the endpoint service private DNS name configuration.
--
-- /Note:/ Consider using 'privateDNSNameConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scPrivateDNSNameConfiguration :: Lens.Lens' ServiceConfiguration (Lude.Maybe PrivateDNSNameConfiguration)
scPrivateDNSNameConfiguration = Lens.lens (privateDNSNameConfiguration :: ServiceConfiguration -> Lude.Maybe PrivateDNSNameConfiguration) (\s a -> s {privateDNSNameConfiguration = a} :: ServiceConfiguration)
{-# DEPRECATED scPrivateDNSNameConfiguration "Use generic-lens or generic-optics with 'privateDNSNameConfiguration' instead." #-}

-- | Any tags assigned to the service.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scTags :: Lens.Lens' ServiceConfiguration (Lude.Maybe [Tag])
scTags = Lens.lens (tags :: ServiceConfiguration -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: ServiceConfiguration)
{-# DEPRECATED scTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.FromXML ServiceConfiguration where
  parseXML x =
    ServiceConfiguration'
      Lude.<$> ( x Lude..@? "networkLoadBalancerArnSet" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
      Lude.<*> ( x Lude..@? "baseEndpointDnsNameSet" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
      Lude.<*> ( x Lude..@? "availabilityZoneSet" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
      Lude.<*> ( x Lude..@? "gatewayLoadBalancerArnSet" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
      Lude.<*> (x Lude..@? "managesVpcEndpoints")
      Lude.<*> (x Lude..@? "serviceName")
      Lude.<*> (x Lude..@? "serviceState")
      Lude.<*> ( x Lude..@? "serviceType" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
      Lude.<*> (x Lude..@? "acceptanceRequired")
      Lude.<*> (x Lude..@? "serviceId")
      Lude.<*> (x Lude..@? "privateDnsName")
      Lude.<*> (x Lude..@? "privateDnsNameConfiguration")
      Lude.<*> ( x Lude..@? "tagSet" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
