{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.VpcEndpointConnection
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.VpcEndpointConnection
  ( VpcEndpointConnection (..)
  -- * Smart constructor
  , mkVpcEndpointConnection
  -- * Lenses
  , vecCreationTimestamp
  , vecDnsEntries
  , vecGatewayLoadBalancerArns
  , vecNetworkLoadBalancerArns
  , vecServiceId
  , vecVpcEndpointId
  , vecVpcEndpointOwner
  , vecVpcEndpointState
  ) where

import qualified Network.AWS.EC2.Types.DnsEntry as Types
import qualified Network.AWS.EC2.Types.State as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a VPC endpoint connection to a service.
--
-- /See:/ 'mkVpcEndpointConnection' smart constructor.
data VpcEndpointConnection = VpcEndpointConnection'
  { creationTimestamp :: Core.Maybe Core.UTCTime
    -- ^ The date and time that the VPC endpoint was created.
  , dnsEntries :: Core.Maybe [Types.DnsEntry]
    -- ^ The DNS entries for the VPC endpoint.
  , gatewayLoadBalancerArns :: Core.Maybe [Core.Text]
    -- ^ The Amazon Resource Names (ARNs) of the Gateway Load Balancers for the service.
  , networkLoadBalancerArns :: Core.Maybe [Core.Text]
    -- ^ The Amazon Resource Names (ARNs) of the network load balancers for the service.
  , serviceId :: Core.Maybe Core.Text
    -- ^ The ID of the service to which the endpoint is connected.
  , vpcEndpointId :: Core.Maybe Core.Text
    -- ^ The ID of the VPC endpoint.
  , vpcEndpointOwner :: Core.Maybe Core.Text
    -- ^ The AWS account ID of the owner of the VPC endpoint.
  , vpcEndpointState :: Core.Maybe Types.State
    -- ^ The state of the VPC endpoint.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'VpcEndpointConnection' value with any optional fields omitted.
mkVpcEndpointConnection
    :: VpcEndpointConnection
mkVpcEndpointConnection
  = VpcEndpointConnection'{creationTimestamp = Core.Nothing,
                           dnsEntries = Core.Nothing, gatewayLoadBalancerArns = Core.Nothing,
                           networkLoadBalancerArns = Core.Nothing, serviceId = Core.Nothing,
                           vpcEndpointId = Core.Nothing, vpcEndpointOwner = Core.Nothing,
                           vpcEndpointState = Core.Nothing}

-- | The date and time that the VPC endpoint was created.
--
-- /Note:/ Consider using 'creationTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vecCreationTimestamp :: Lens.Lens' VpcEndpointConnection (Core.Maybe Core.UTCTime)
vecCreationTimestamp = Lens.field @"creationTimestamp"
{-# INLINEABLE vecCreationTimestamp #-}
{-# DEPRECATED creationTimestamp "Use generic-lens or generic-optics with 'creationTimestamp' instead"  #-}

-- | The DNS entries for the VPC endpoint.
--
-- /Note:/ Consider using 'dnsEntries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vecDnsEntries :: Lens.Lens' VpcEndpointConnection (Core.Maybe [Types.DnsEntry])
vecDnsEntries = Lens.field @"dnsEntries"
{-# INLINEABLE vecDnsEntries #-}
{-# DEPRECATED dnsEntries "Use generic-lens or generic-optics with 'dnsEntries' instead"  #-}

-- | The Amazon Resource Names (ARNs) of the Gateway Load Balancers for the service.
--
-- /Note:/ Consider using 'gatewayLoadBalancerArns' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vecGatewayLoadBalancerArns :: Lens.Lens' VpcEndpointConnection (Core.Maybe [Core.Text])
vecGatewayLoadBalancerArns = Lens.field @"gatewayLoadBalancerArns"
{-# INLINEABLE vecGatewayLoadBalancerArns #-}
{-# DEPRECATED gatewayLoadBalancerArns "Use generic-lens or generic-optics with 'gatewayLoadBalancerArns' instead"  #-}

-- | The Amazon Resource Names (ARNs) of the network load balancers for the service.
--
-- /Note:/ Consider using 'networkLoadBalancerArns' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vecNetworkLoadBalancerArns :: Lens.Lens' VpcEndpointConnection (Core.Maybe [Core.Text])
vecNetworkLoadBalancerArns = Lens.field @"networkLoadBalancerArns"
{-# INLINEABLE vecNetworkLoadBalancerArns #-}
{-# DEPRECATED networkLoadBalancerArns "Use generic-lens or generic-optics with 'networkLoadBalancerArns' instead"  #-}

-- | The ID of the service to which the endpoint is connected.
--
-- /Note:/ Consider using 'serviceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vecServiceId :: Lens.Lens' VpcEndpointConnection (Core.Maybe Core.Text)
vecServiceId = Lens.field @"serviceId"
{-# INLINEABLE vecServiceId #-}
{-# DEPRECATED serviceId "Use generic-lens or generic-optics with 'serviceId' instead"  #-}

-- | The ID of the VPC endpoint.
--
-- /Note:/ Consider using 'vpcEndpointId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vecVpcEndpointId :: Lens.Lens' VpcEndpointConnection (Core.Maybe Core.Text)
vecVpcEndpointId = Lens.field @"vpcEndpointId"
{-# INLINEABLE vecVpcEndpointId #-}
{-# DEPRECATED vpcEndpointId "Use generic-lens or generic-optics with 'vpcEndpointId' instead"  #-}

-- | The AWS account ID of the owner of the VPC endpoint.
--
-- /Note:/ Consider using 'vpcEndpointOwner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vecVpcEndpointOwner :: Lens.Lens' VpcEndpointConnection (Core.Maybe Core.Text)
vecVpcEndpointOwner = Lens.field @"vpcEndpointOwner"
{-# INLINEABLE vecVpcEndpointOwner #-}
{-# DEPRECATED vpcEndpointOwner "Use generic-lens or generic-optics with 'vpcEndpointOwner' instead"  #-}

-- | The state of the VPC endpoint.
--
-- /Note:/ Consider using 'vpcEndpointState' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vecVpcEndpointState :: Lens.Lens' VpcEndpointConnection (Core.Maybe Types.State)
vecVpcEndpointState = Lens.field @"vpcEndpointState"
{-# INLINEABLE vecVpcEndpointState #-}
{-# DEPRECATED vpcEndpointState "Use generic-lens or generic-optics with 'vpcEndpointState' instead"  #-}

instance Core.FromXML VpcEndpointConnection where
        parseXML x
          = VpcEndpointConnection' Core.<$>
              (x Core..@? "creationTimestamp") Core.<*>
                x Core..@? "dnsEntrySet" Core..<@> Core.parseXMLList "item"
                Core.<*>
                x Core..@? "gatewayLoadBalancerArnSet" Core..<@>
                  Core.parseXMLList "item"
                Core.<*>
                x Core..@? "networkLoadBalancerArnSet" Core..<@>
                  Core.parseXMLList "item"
                Core.<*> x Core..@? "serviceId"
                Core.<*> x Core..@? "vpcEndpointId"
                Core.<*> x Core..@? "vpcEndpointOwner"
                Core.<*> x Core..@? "vpcEndpointState"
