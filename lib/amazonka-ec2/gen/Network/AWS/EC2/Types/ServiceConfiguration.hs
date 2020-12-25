{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
    scAcceptanceRequired,
    scAvailabilityZones,
    scBaseEndpointDnsNames,
    scGatewayLoadBalancerArns,
    scManagesVpcEndpoints,
    scNetworkLoadBalancerArns,
    scPrivateDnsName,
    scPrivateDnsNameConfiguration,
    scServiceId,
    scServiceName,
    scServiceState,
    scServiceType,
    scTags,
  )
where

import qualified Network.AWS.EC2.Types.PrivateDnsNameConfiguration as Types
import qualified Network.AWS.EC2.Types.ServiceState as Types
import qualified Network.AWS.EC2.Types.ServiceTypeDetail as Types
import qualified Network.AWS.EC2.Types.String as Types
import qualified Network.AWS.EC2.Types.Tag as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a service configuration for a VPC endpoint service.
--
-- /See:/ 'mkServiceConfiguration' smart constructor.
data ServiceConfiguration = ServiceConfiguration'
  { -- | Indicates whether requests from other AWS accounts to create an endpoint to the service must first be accepted.
    acceptanceRequired :: Core.Maybe Core.Bool,
    -- | The Availability Zones in which the service is available.
    availabilityZones :: Core.Maybe [Types.String],
    -- | The DNS names for the service.
    baseEndpointDnsNames :: Core.Maybe [Types.String],
    -- | The Amazon Resource Names (ARNs) of the Gateway Load Balancers for the service.
    gatewayLoadBalancerArns :: Core.Maybe [Types.String],
    -- | Indicates whether the service manages its VPC endpoints. Management of the service VPC endpoints using the VPC endpoint API is restricted.
    managesVpcEndpoints :: Core.Maybe Core.Bool,
    -- | The Amazon Resource Names (ARNs) of the Network Load Balancers for the service.
    networkLoadBalancerArns :: Core.Maybe [Types.String],
    -- | The private DNS name for the service.
    privateDnsName :: Core.Maybe Types.String,
    -- | Information about the endpoint service private DNS name configuration.
    privateDnsNameConfiguration :: Core.Maybe Types.PrivateDnsNameConfiguration,
    -- | The ID of the service.
    serviceId :: Core.Maybe Types.String,
    -- | The name of the service.
    serviceName :: Core.Maybe Types.String,
    -- | The service state.
    serviceState :: Core.Maybe Types.ServiceState,
    -- | The type of service.
    serviceType :: Core.Maybe [Types.ServiceTypeDetail],
    -- | Any tags assigned to the service.
    tags :: Core.Maybe [Types.Tag]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ServiceConfiguration' value with any optional fields omitted.
mkServiceConfiguration ::
  ServiceConfiguration
mkServiceConfiguration =
  ServiceConfiguration'
    { acceptanceRequired = Core.Nothing,
      availabilityZones = Core.Nothing,
      baseEndpointDnsNames = Core.Nothing,
      gatewayLoadBalancerArns = Core.Nothing,
      managesVpcEndpoints = Core.Nothing,
      networkLoadBalancerArns = Core.Nothing,
      privateDnsName = Core.Nothing,
      privateDnsNameConfiguration = Core.Nothing,
      serviceId = Core.Nothing,
      serviceName = Core.Nothing,
      serviceState = Core.Nothing,
      serviceType = Core.Nothing,
      tags = Core.Nothing
    }

-- | Indicates whether requests from other AWS accounts to create an endpoint to the service must first be accepted.
--
-- /Note:/ Consider using 'acceptanceRequired' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scAcceptanceRequired :: Lens.Lens' ServiceConfiguration (Core.Maybe Core.Bool)
scAcceptanceRequired = Lens.field @"acceptanceRequired"
{-# DEPRECATED scAcceptanceRequired "Use generic-lens or generic-optics with 'acceptanceRequired' instead." #-}

-- | The Availability Zones in which the service is available.
--
-- /Note:/ Consider using 'availabilityZones' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scAvailabilityZones :: Lens.Lens' ServiceConfiguration (Core.Maybe [Types.String])
scAvailabilityZones = Lens.field @"availabilityZones"
{-# DEPRECATED scAvailabilityZones "Use generic-lens or generic-optics with 'availabilityZones' instead." #-}

-- | The DNS names for the service.
--
-- /Note:/ Consider using 'baseEndpointDnsNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scBaseEndpointDnsNames :: Lens.Lens' ServiceConfiguration (Core.Maybe [Types.String])
scBaseEndpointDnsNames = Lens.field @"baseEndpointDnsNames"
{-# DEPRECATED scBaseEndpointDnsNames "Use generic-lens or generic-optics with 'baseEndpointDnsNames' instead." #-}

-- | The Amazon Resource Names (ARNs) of the Gateway Load Balancers for the service.
--
-- /Note:/ Consider using 'gatewayLoadBalancerArns' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scGatewayLoadBalancerArns :: Lens.Lens' ServiceConfiguration (Core.Maybe [Types.String])
scGatewayLoadBalancerArns = Lens.field @"gatewayLoadBalancerArns"
{-# DEPRECATED scGatewayLoadBalancerArns "Use generic-lens or generic-optics with 'gatewayLoadBalancerArns' instead." #-}

-- | Indicates whether the service manages its VPC endpoints. Management of the service VPC endpoints using the VPC endpoint API is restricted.
--
-- /Note:/ Consider using 'managesVpcEndpoints' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scManagesVpcEndpoints :: Lens.Lens' ServiceConfiguration (Core.Maybe Core.Bool)
scManagesVpcEndpoints = Lens.field @"managesVpcEndpoints"
{-# DEPRECATED scManagesVpcEndpoints "Use generic-lens or generic-optics with 'managesVpcEndpoints' instead." #-}

-- | The Amazon Resource Names (ARNs) of the Network Load Balancers for the service.
--
-- /Note:/ Consider using 'networkLoadBalancerArns' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scNetworkLoadBalancerArns :: Lens.Lens' ServiceConfiguration (Core.Maybe [Types.String])
scNetworkLoadBalancerArns = Lens.field @"networkLoadBalancerArns"
{-# DEPRECATED scNetworkLoadBalancerArns "Use generic-lens or generic-optics with 'networkLoadBalancerArns' instead." #-}

-- | The private DNS name for the service.
--
-- /Note:/ Consider using 'privateDnsName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scPrivateDnsName :: Lens.Lens' ServiceConfiguration (Core.Maybe Types.String)
scPrivateDnsName = Lens.field @"privateDnsName"
{-# DEPRECATED scPrivateDnsName "Use generic-lens or generic-optics with 'privateDnsName' instead." #-}

-- | Information about the endpoint service private DNS name configuration.
--
-- /Note:/ Consider using 'privateDnsNameConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scPrivateDnsNameConfiguration :: Lens.Lens' ServiceConfiguration (Core.Maybe Types.PrivateDnsNameConfiguration)
scPrivateDnsNameConfiguration = Lens.field @"privateDnsNameConfiguration"
{-# DEPRECATED scPrivateDnsNameConfiguration "Use generic-lens or generic-optics with 'privateDnsNameConfiguration' instead." #-}

-- | The ID of the service.
--
-- /Note:/ Consider using 'serviceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scServiceId :: Lens.Lens' ServiceConfiguration (Core.Maybe Types.String)
scServiceId = Lens.field @"serviceId"
{-# DEPRECATED scServiceId "Use generic-lens or generic-optics with 'serviceId' instead." #-}

-- | The name of the service.
--
-- /Note:/ Consider using 'serviceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scServiceName :: Lens.Lens' ServiceConfiguration (Core.Maybe Types.String)
scServiceName = Lens.field @"serviceName"
{-# DEPRECATED scServiceName "Use generic-lens or generic-optics with 'serviceName' instead." #-}

-- | The service state.
--
-- /Note:/ Consider using 'serviceState' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scServiceState :: Lens.Lens' ServiceConfiguration (Core.Maybe Types.ServiceState)
scServiceState = Lens.field @"serviceState"
{-# DEPRECATED scServiceState "Use generic-lens or generic-optics with 'serviceState' instead." #-}

-- | The type of service.
--
-- /Note:/ Consider using 'serviceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scServiceType :: Lens.Lens' ServiceConfiguration (Core.Maybe [Types.ServiceTypeDetail])
scServiceType = Lens.field @"serviceType"
{-# DEPRECATED scServiceType "Use generic-lens or generic-optics with 'serviceType' instead." #-}

-- | Any tags assigned to the service.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scTags :: Lens.Lens' ServiceConfiguration (Core.Maybe [Types.Tag])
scTags = Lens.field @"tags"
{-# DEPRECATED scTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Core.FromXML ServiceConfiguration where
  parseXML x =
    ServiceConfiguration'
      Core.<$> (x Core..@? "acceptanceRequired")
      Core.<*> ( x Core..@? "availabilityZoneSet"
                   Core..<@> Core.parseXMLList "item"
               )
      Core.<*> ( x Core..@? "baseEndpointDnsNameSet"
                   Core..<@> Core.parseXMLList "item"
               )
      Core.<*> ( x Core..@? "gatewayLoadBalancerArnSet"
                   Core..<@> Core.parseXMLList "item"
               )
      Core.<*> (x Core..@? "managesVpcEndpoints")
      Core.<*> ( x Core..@? "networkLoadBalancerArnSet"
                   Core..<@> Core.parseXMLList "item"
               )
      Core.<*> (x Core..@? "privateDnsName")
      Core.<*> (x Core..@? "privateDnsNameConfiguration")
      Core.<*> (x Core..@? "serviceId")
      Core.<*> (x Core..@? "serviceName")
      Core.<*> (x Core..@? "serviceState")
      Core.<*> (x Core..@? "serviceType" Core..<@> Core.parseXMLList "item")
      Core.<*> (x Core..@? "tagSet" Core..<@> Core.parseXMLList "item")
