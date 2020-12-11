-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.VPCEndpointConnection
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.VPCEndpointConnection
  ( VPCEndpointConnection (..),

    -- * Smart constructor
    mkVPCEndpointConnection,

    -- * Lenses
    vecVPCEndpointOwner,
    vecNetworkLoadBalancerARNs,
    vecDNSEntries,
    vecVPCEndpointState,
    vecGatewayLoadBalancerARNs,
    vecCreationTimestamp,
    vecServiceId,
    vecVPCEndpointId,
  )
where

import Network.AWS.EC2.Types.DNSEntry
import Network.AWS.EC2.Types.State
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a VPC endpoint connection to a service.
--
-- /See:/ 'mkVPCEndpointConnection' smart constructor.
data VPCEndpointConnection = VPCEndpointConnection'
  { vpcEndpointOwner ::
      Lude.Maybe Lude.Text,
    networkLoadBalancerARNs ::
      Lude.Maybe [Lude.Text],
    dnsEntries :: Lude.Maybe [DNSEntry],
    vpcEndpointState :: Lude.Maybe State,
    gatewayLoadBalancerARNs ::
      Lude.Maybe [Lude.Text],
    creationTimestamp :: Lude.Maybe Lude.ISO8601,
    serviceId :: Lude.Maybe Lude.Text,
    vpcEndpointId :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'VPCEndpointConnection' with the minimum fields required to make a request.
--
-- * 'creationTimestamp' - The date and time that the VPC endpoint was created.
-- * 'dnsEntries' - The DNS entries for the VPC endpoint.
-- * 'gatewayLoadBalancerARNs' - The Amazon Resource Names (ARNs) of the Gateway Load Balancers for the service.
-- * 'networkLoadBalancerARNs' - The Amazon Resource Names (ARNs) of the network load balancers for the service.
-- * 'serviceId' - The ID of the service to which the endpoint is connected.
-- * 'vpcEndpointId' - The ID of the VPC endpoint.
-- * 'vpcEndpointOwner' - The AWS account ID of the owner of the VPC endpoint.
-- * 'vpcEndpointState' - The state of the VPC endpoint.
mkVPCEndpointConnection ::
  VPCEndpointConnection
mkVPCEndpointConnection =
  VPCEndpointConnection'
    { vpcEndpointOwner = Lude.Nothing,
      networkLoadBalancerARNs = Lude.Nothing,
      dnsEntries = Lude.Nothing,
      vpcEndpointState = Lude.Nothing,
      gatewayLoadBalancerARNs = Lude.Nothing,
      creationTimestamp = Lude.Nothing,
      serviceId = Lude.Nothing,
      vpcEndpointId = Lude.Nothing
    }

-- | The AWS account ID of the owner of the VPC endpoint.
--
-- /Note:/ Consider using 'vpcEndpointOwner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vecVPCEndpointOwner :: Lens.Lens' VPCEndpointConnection (Lude.Maybe Lude.Text)
vecVPCEndpointOwner = Lens.lens (vpcEndpointOwner :: VPCEndpointConnection -> Lude.Maybe Lude.Text) (\s a -> s {vpcEndpointOwner = a} :: VPCEndpointConnection)
{-# DEPRECATED vecVPCEndpointOwner "Use generic-lens or generic-optics with 'vpcEndpointOwner' instead." #-}

-- | The Amazon Resource Names (ARNs) of the network load balancers for the service.
--
-- /Note:/ Consider using 'networkLoadBalancerARNs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vecNetworkLoadBalancerARNs :: Lens.Lens' VPCEndpointConnection (Lude.Maybe [Lude.Text])
vecNetworkLoadBalancerARNs = Lens.lens (networkLoadBalancerARNs :: VPCEndpointConnection -> Lude.Maybe [Lude.Text]) (\s a -> s {networkLoadBalancerARNs = a} :: VPCEndpointConnection)
{-# DEPRECATED vecNetworkLoadBalancerARNs "Use generic-lens or generic-optics with 'networkLoadBalancerARNs' instead." #-}

-- | The DNS entries for the VPC endpoint.
--
-- /Note:/ Consider using 'dnsEntries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vecDNSEntries :: Lens.Lens' VPCEndpointConnection (Lude.Maybe [DNSEntry])
vecDNSEntries = Lens.lens (dnsEntries :: VPCEndpointConnection -> Lude.Maybe [DNSEntry]) (\s a -> s {dnsEntries = a} :: VPCEndpointConnection)
{-# DEPRECATED vecDNSEntries "Use generic-lens or generic-optics with 'dnsEntries' instead." #-}

-- | The state of the VPC endpoint.
--
-- /Note:/ Consider using 'vpcEndpointState' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vecVPCEndpointState :: Lens.Lens' VPCEndpointConnection (Lude.Maybe State)
vecVPCEndpointState = Lens.lens (vpcEndpointState :: VPCEndpointConnection -> Lude.Maybe State) (\s a -> s {vpcEndpointState = a} :: VPCEndpointConnection)
{-# DEPRECATED vecVPCEndpointState "Use generic-lens or generic-optics with 'vpcEndpointState' instead." #-}

-- | The Amazon Resource Names (ARNs) of the Gateway Load Balancers for the service.
--
-- /Note:/ Consider using 'gatewayLoadBalancerARNs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vecGatewayLoadBalancerARNs :: Lens.Lens' VPCEndpointConnection (Lude.Maybe [Lude.Text])
vecGatewayLoadBalancerARNs = Lens.lens (gatewayLoadBalancerARNs :: VPCEndpointConnection -> Lude.Maybe [Lude.Text]) (\s a -> s {gatewayLoadBalancerARNs = a} :: VPCEndpointConnection)
{-# DEPRECATED vecGatewayLoadBalancerARNs "Use generic-lens or generic-optics with 'gatewayLoadBalancerARNs' instead." #-}

-- | The date and time that the VPC endpoint was created.
--
-- /Note:/ Consider using 'creationTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vecCreationTimestamp :: Lens.Lens' VPCEndpointConnection (Lude.Maybe Lude.ISO8601)
vecCreationTimestamp = Lens.lens (creationTimestamp :: VPCEndpointConnection -> Lude.Maybe Lude.ISO8601) (\s a -> s {creationTimestamp = a} :: VPCEndpointConnection)
{-# DEPRECATED vecCreationTimestamp "Use generic-lens or generic-optics with 'creationTimestamp' instead." #-}

-- | The ID of the service to which the endpoint is connected.
--
-- /Note:/ Consider using 'serviceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vecServiceId :: Lens.Lens' VPCEndpointConnection (Lude.Maybe Lude.Text)
vecServiceId = Lens.lens (serviceId :: VPCEndpointConnection -> Lude.Maybe Lude.Text) (\s a -> s {serviceId = a} :: VPCEndpointConnection)
{-# DEPRECATED vecServiceId "Use generic-lens or generic-optics with 'serviceId' instead." #-}

-- | The ID of the VPC endpoint.
--
-- /Note:/ Consider using 'vpcEndpointId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vecVPCEndpointId :: Lens.Lens' VPCEndpointConnection (Lude.Maybe Lude.Text)
vecVPCEndpointId = Lens.lens (vpcEndpointId :: VPCEndpointConnection -> Lude.Maybe Lude.Text) (\s a -> s {vpcEndpointId = a} :: VPCEndpointConnection)
{-# DEPRECATED vecVPCEndpointId "Use generic-lens or generic-optics with 'vpcEndpointId' instead." #-}

instance Lude.FromXML VPCEndpointConnection where
  parseXML x =
    VPCEndpointConnection'
      Lude.<$> (x Lude..@? "vpcEndpointOwner")
      Lude.<*> ( x Lude..@? "networkLoadBalancerArnSet" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
      Lude.<*> ( x Lude..@? "dnsEntrySet" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
      Lude.<*> (x Lude..@? "vpcEndpointState")
      Lude.<*> ( x Lude..@? "gatewayLoadBalancerArnSet" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
      Lude.<*> (x Lude..@? "creationTimestamp")
      Lude.<*> (x Lude..@? "serviceId")
      Lude.<*> (x Lude..@? "vpcEndpointId")
