{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ServiceConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.ServiceConfiguration
  ( ServiceConfiguration (..)
  -- * Smart constructor
  , mkServiceConfiguration
  -- * Lenses
  , scAcceptanceRequired
  , scAvailabilityZones
  , scBaseEndpointDnsNames
  , scGatewayLoadBalancerArns
  , scManagesVpcEndpoints
  , scNetworkLoadBalancerArns
  , scPrivateDnsName
  , scPrivateDnsNameConfiguration
  , scServiceId
  , scServiceName
  , scServiceState
  , scServiceType
  , scTags
  ) where

import qualified Network.AWS.EC2.Types.PrivateDnsNameConfiguration as Types
import qualified Network.AWS.EC2.Types.ServiceState as Types
import qualified Network.AWS.EC2.Types.ServiceTypeDetail as Types
import qualified Network.AWS.EC2.Types.Tag as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a service configuration for a VPC endpoint service.
--
-- /See:/ 'mkServiceConfiguration' smart constructor.
data ServiceConfiguration = ServiceConfiguration'
  { acceptanceRequired :: Core.Maybe Core.Bool
    -- ^ Indicates whether requests from other AWS accounts to create an endpoint to the service must first be accepted.
  , availabilityZones :: Core.Maybe [Core.Text]
    -- ^ The Availability Zones in which the service is available.
  , baseEndpointDnsNames :: Core.Maybe [Core.Text]
    -- ^ The DNS names for the service.
  , gatewayLoadBalancerArns :: Core.Maybe [Core.Text]
    -- ^ The Amazon Resource Names (ARNs) of the Gateway Load Balancers for the service.
  , managesVpcEndpoints :: Core.Maybe Core.Bool
    -- ^ Indicates whether the service manages its VPC endpoints. Management of the service VPC endpoints using the VPC endpoint API is restricted.
  , networkLoadBalancerArns :: Core.Maybe [Core.Text]
    -- ^ The Amazon Resource Names (ARNs) of the Network Load Balancers for the service.
  , privateDnsName :: Core.Maybe Core.Text
    -- ^ The private DNS name for the service.
  , privateDnsNameConfiguration :: Core.Maybe Types.PrivateDnsNameConfiguration
    -- ^ Information about the endpoint service private DNS name configuration.
  , serviceId :: Core.Maybe Core.Text
    -- ^ The ID of the service.
  , serviceName :: Core.Maybe Core.Text
    -- ^ The name of the service.
  , serviceState :: Core.Maybe Types.ServiceState
    -- ^ The service state.
  , serviceType :: Core.Maybe [Types.ServiceTypeDetail]
    -- ^ The type of service.
  , tags :: Core.Maybe [Types.Tag]
    -- ^ Any tags assigned to the service.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ServiceConfiguration' value with any optional fields omitted.
mkServiceConfiguration
    :: ServiceConfiguration
mkServiceConfiguration
  = ServiceConfiguration'{acceptanceRequired = Core.Nothing,
                          availabilityZones = Core.Nothing,
                          baseEndpointDnsNames = Core.Nothing,
                          gatewayLoadBalancerArns = Core.Nothing,
                          managesVpcEndpoints = Core.Nothing,
                          networkLoadBalancerArns = Core.Nothing,
                          privateDnsName = Core.Nothing,
                          privateDnsNameConfiguration = Core.Nothing,
                          serviceId = Core.Nothing, serviceName = Core.Nothing,
                          serviceState = Core.Nothing, serviceType = Core.Nothing,
                          tags = Core.Nothing}

-- | Indicates whether requests from other AWS accounts to create an endpoint to the service must first be accepted.
--
-- /Note:/ Consider using 'acceptanceRequired' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scAcceptanceRequired :: Lens.Lens' ServiceConfiguration (Core.Maybe Core.Bool)
scAcceptanceRequired = Lens.field @"acceptanceRequired"
{-# INLINEABLE scAcceptanceRequired #-}
{-# DEPRECATED acceptanceRequired "Use generic-lens or generic-optics with 'acceptanceRequired' instead"  #-}

-- | The Availability Zones in which the service is available.
--
-- /Note:/ Consider using 'availabilityZones' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scAvailabilityZones :: Lens.Lens' ServiceConfiguration (Core.Maybe [Core.Text])
scAvailabilityZones = Lens.field @"availabilityZones"
{-# INLINEABLE scAvailabilityZones #-}
{-# DEPRECATED availabilityZones "Use generic-lens or generic-optics with 'availabilityZones' instead"  #-}

-- | The DNS names for the service.
--
-- /Note:/ Consider using 'baseEndpointDnsNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scBaseEndpointDnsNames :: Lens.Lens' ServiceConfiguration (Core.Maybe [Core.Text])
scBaseEndpointDnsNames = Lens.field @"baseEndpointDnsNames"
{-# INLINEABLE scBaseEndpointDnsNames #-}
{-# DEPRECATED baseEndpointDnsNames "Use generic-lens or generic-optics with 'baseEndpointDnsNames' instead"  #-}

-- | The Amazon Resource Names (ARNs) of the Gateway Load Balancers for the service.
--
-- /Note:/ Consider using 'gatewayLoadBalancerArns' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scGatewayLoadBalancerArns :: Lens.Lens' ServiceConfiguration (Core.Maybe [Core.Text])
scGatewayLoadBalancerArns = Lens.field @"gatewayLoadBalancerArns"
{-# INLINEABLE scGatewayLoadBalancerArns #-}
{-# DEPRECATED gatewayLoadBalancerArns "Use generic-lens or generic-optics with 'gatewayLoadBalancerArns' instead"  #-}

-- | Indicates whether the service manages its VPC endpoints. Management of the service VPC endpoints using the VPC endpoint API is restricted.
--
-- /Note:/ Consider using 'managesVpcEndpoints' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scManagesVpcEndpoints :: Lens.Lens' ServiceConfiguration (Core.Maybe Core.Bool)
scManagesVpcEndpoints = Lens.field @"managesVpcEndpoints"
{-# INLINEABLE scManagesVpcEndpoints #-}
{-# DEPRECATED managesVpcEndpoints "Use generic-lens or generic-optics with 'managesVpcEndpoints' instead"  #-}

-- | The Amazon Resource Names (ARNs) of the Network Load Balancers for the service.
--
-- /Note:/ Consider using 'networkLoadBalancerArns' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scNetworkLoadBalancerArns :: Lens.Lens' ServiceConfiguration (Core.Maybe [Core.Text])
scNetworkLoadBalancerArns = Lens.field @"networkLoadBalancerArns"
{-# INLINEABLE scNetworkLoadBalancerArns #-}
{-# DEPRECATED networkLoadBalancerArns "Use generic-lens or generic-optics with 'networkLoadBalancerArns' instead"  #-}

-- | The private DNS name for the service.
--
-- /Note:/ Consider using 'privateDnsName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scPrivateDnsName :: Lens.Lens' ServiceConfiguration (Core.Maybe Core.Text)
scPrivateDnsName = Lens.field @"privateDnsName"
{-# INLINEABLE scPrivateDnsName #-}
{-# DEPRECATED privateDnsName "Use generic-lens or generic-optics with 'privateDnsName' instead"  #-}

-- | Information about the endpoint service private DNS name configuration.
--
-- /Note:/ Consider using 'privateDnsNameConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scPrivateDnsNameConfiguration :: Lens.Lens' ServiceConfiguration (Core.Maybe Types.PrivateDnsNameConfiguration)
scPrivateDnsNameConfiguration = Lens.field @"privateDnsNameConfiguration"
{-# INLINEABLE scPrivateDnsNameConfiguration #-}
{-# DEPRECATED privateDnsNameConfiguration "Use generic-lens or generic-optics with 'privateDnsNameConfiguration' instead"  #-}

-- | The ID of the service.
--
-- /Note:/ Consider using 'serviceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scServiceId :: Lens.Lens' ServiceConfiguration (Core.Maybe Core.Text)
scServiceId = Lens.field @"serviceId"
{-# INLINEABLE scServiceId #-}
{-# DEPRECATED serviceId "Use generic-lens or generic-optics with 'serviceId' instead"  #-}

-- | The name of the service.
--
-- /Note:/ Consider using 'serviceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scServiceName :: Lens.Lens' ServiceConfiguration (Core.Maybe Core.Text)
scServiceName = Lens.field @"serviceName"
{-# INLINEABLE scServiceName #-}
{-# DEPRECATED serviceName "Use generic-lens or generic-optics with 'serviceName' instead"  #-}

-- | The service state.
--
-- /Note:/ Consider using 'serviceState' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scServiceState :: Lens.Lens' ServiceConfiguration (Core.Maybe Types.ServiceState)
scServiceState = Lens.field @"serviceState"
{-# INLINEABLE scServiceState #-}
{-# DEPRECATED serviceState "Use generic-lens or generic-optics with 'serviceState' instead"  #-}

-- | The type of service.
--
-- /Note:/ Consider using 'serviceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scServiceType :: Lens.Lens' ServiceConfiguration (Core.Maybe [Types.ServiceTypeDetail])
scServiceType = Lens.field @"serviceType"
{-# INLINEABLE scServiceType #-}
{-# DEPRECATED serviceType "Use generic-lens or generic-optics with 'serviceType' instead"  #-}

-- | Any tags assigned to the service.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scTags :: Lens.Lens' ServiceConfiguration (Core.Maybe [Types.Tag])
scTags = Lens.field @"tags"
{-# INLINEABLE scTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

instance Core.FromXML ServiceConfiguration where
        parseXML x
          = ServiceConfiguration' Core.<$>
              (x Core..@? "acceptanceRequired") Core.<*>
                x Core..@? "availabilityZoneSet" Core..<@> Core.parseXMLList "item"
                Core.<*>
                x Core..@? "baseEndpointDnsNameSet" Core..<@>
                  Core.parseXMLList "item"
                Core.<*>
                x Core..@? "gatewayLoadBalancerArnSet" Core..<@>
                  Core.parseXMLList "item"
                Core.<*> x Core..@? "managesVpcEndpoints"
                Core.<*>
                x Core..@? "networkLoadBalancerArnSet" Core..<@>
                  Core.parseXMLList "item"
                Core.<*> x Core..@? "privateDnsName"
                Core.<*> x Core..@? "privateDnsNameConfiguration"
                Core.<*> x Core..@? "serviceId"
                Core.<*> x Core..@? "serviceName"
                Core.<*> x Core..@? "serviceState"
                Core.<*>
                x Core..@? "serviceType" Core..<@> Core.parseXMLList "item"
                Core.<*> x Core..@? "tagSet" Core..<@> Core.parseXMLList "item"
