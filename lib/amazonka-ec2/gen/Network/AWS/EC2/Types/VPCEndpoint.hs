-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.VPCEndpoint
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.VPCEndpoint
  ( VPCEndpoint (..),

    -- * Smart constructor
    mkVPCEndpoint,

    -- * Lenses
    veGroups,
    veState,
    vePolicyDocument,
    veSubnetIds,
    veNetworkInterfaceIds,
    veVPCId,
    veRequesterManaged,
    veDNSEntries,
    veVPCEndpointType,
    vePrivateDNSEnabled,
    veOwnerId,
    veCreationTimestamp,
    veServiceName,
    veLastError,
    veVPCEndpointId,
    veTags,
    veRouteTableIds,
  )
where

import Network.AWS.EC2.Types.DNSEntry
import Network.AWS.EC2.Types.LastError
import Network.AWS.EC2.Types.SecurityGroupIdentifier
import Network.AWS.EC2.Types.State
import Network.AWS.EC2.Types.Tag
import Network.AWS.EC2.Types.VPCEndpointType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a VPC endpoint.
--
-- /See:/ 'mkVPCEndpoint' smart constructor.
data VPCEndpoint = VPCEndpoint'
  { groups ::
      Lude.Maybe [SecurityGroupIdentifier],
    state :: Lude.Maybe State,
    policyDocument :: Lude.Maybe Lude.Text,
    subnetIds :: Lude.Maybe [Lude.Text],
    networkInterfaceIds :: Lude.Maybe [Lude.Text],
    vpcId :: Lude.Maybe Lude.Text,
    requesterManaged :: Lude.Maybe Lude.Bool,
    dnsEntries :: Lude.Maybe [DNSEntry],
    vpcEndpointType :: Lude.Maybe VPCEndpointType,
    privateDNSEnabled :: Lude.Maybe Lude.Bool,
    ownerId :: Lude.Maybe Lude.Text,
    creationTimestamp :: Lude.Maybe Lude.ISO8601,
    serviceName :: Lude.Maybe Lude.Text,
    lastError :: Lude.Maybe LastError,
    vpcEndpointId :: Lude.Maybe Lude.Text,
    tags :: Lude.Maybe [Tag],
    routeTableIds :: Lude.Maybe [Lude.Text]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'VPCEndpoint' with the minimum fields required to make a request.
--
-- * 'creationTimestamp' - The date and time that the VPC endpoint was created.
-- * 'dnsEntries' - (Interface endpoint) The DNS entries for the endpoint.
-- * 'groups' - (Interface endpoint) Information about the security groups that are associated with the network interface.
-- * 'lastError' - The last error that occurred for VPC endpoint.
-- * 'networkInterfaceIds' - (Interface endpoint) One or more network interfaces for the endpoint.
-- * 'ownerId' - The ID of the AWS account that owns the VPC endpoint.
-- * 'policyDocument' - The policy document associated with the endpoint, if applicable.
-- * 'privateDNSEnabled' - (Interface endpoint) Indicates whether the VPC is associated with a private hosted zone.
-- * 'requesterManaged' - Indicates whether the VPC endpoint is being managed by its service.
-- * 'routeTableIds' - (Gateway endpoint) One or more route tables associated with the endpoint.
-- * 'serviceName' - The name of the service to which the endpoint is associated.
-- * 'state' - The state of the VPC endpoint.
-- * 'subnetIds' - (Interface endpoint) One or more subnets in which the endpoint is located.
-- * 'tags' - Any tags assigned to the VPC endpoint.
-- * 'vpcEndpointId' - The ID of the VPC endpoint.
-- * 'vpcEndpointType' - The type of endpoint.
-- * 'vpcId' - The ID of the VPC to which the endpoint is associated.
mkVPCEndpoint ::
  VPCEndpoint
mkVPCEndpoint =
  VPCEndpoint'
    { groups = Lude.Nothing,
      state = Lude.Nothing,
      policyDocument = Lude.Nothing,
      subnetIds = Lude.Nothing,
      networkInterfaceIds = Lude.Nothing,
      vpcId = Lude.Nothing,
      requesterManaged = Lude.Nothing,
      dnsEntries = Lude.Nothing,
      vpcEndpointType = Lude.Nothing,
      privateDNSEnabled = Lude.Nothing,
      ownerId = Lude.Nothing,
      creationTimestamp = Lude.Nothing,
      serviceName = Lude.Nothing,
      lastError = Lude.Nothing,
      vpcEndpointId = Lude.Nothing,
      tags = Lude.Nothing,
      routeTableIds = Lude.Nothing
    }

-- | (Interface endpoint) Information about the security groups that are associated with the network interface.
--
-- /Note:/ Consider using 'groups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
veGroups :: Lens.Lens' VPCEndpoint (Lude.Maybe [SecurityGroupIdentifier])
veGroups = Lens.lens (groups :: VPCEndpoint -> Lude.Maybe [SecurityGroupIdentifier]) (\s a -> s {groups = a} :: VPCEndpoint)
{-# DEPRECATED veGroups "Use generic-lens or generic-optics with 'groups' instead." #-}

-- | The state of the VPC endpoint.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
veState :: Lens.Lens' VPCEndpoint (Lude.Maybe State)
veState = Lens.lens (state :: VPCEndpoint -> Lude.Maybe State) (\s a -> s {state = a} :: VPCEndpoint)
{-# DEPRECATED veState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The policy document associated with the endpoint, if applicable.
--
-- /Note:/ Consider using 'policyDocument' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vePolicyDocument :: Lens.Lens' VPCEndpoint (Lude.Maybe Lude.Text)
vePolicyDocument = Lens.lens (policyDocument :: VPCEndpoint -> Lude.Maybe Lude.Text) (\s a -> s {policyDocument = a} :: VPCEndpoint)
{-# DEPRECATED vePolicyDocument "Use generic-lens or generic-optics with 'policyDocument' instead." #-}

-- | (Interface endpoint) One or more subnets in which the endpoint is located.
--
-- /Note:/ Consider using 'subnetIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
veSubnetIds :: Lens.Lens' VPCEndpoint (Lude.Maybe [Lude.Text])
veSubnetIds = Lens.lens (subnetIds :: VPCEndpoint -> Lude.Maybe [Lude.Text]) (\s a -> s {subnetIds = a} :: VPCEndpoint)
{-# DEPRECATED veSubnetIds "Use generic-lens or generic-optics with 'subnetIds' instead." #-}

-- | (Interface endpoint) One or more network interfaces for the endpoint.
--
-- /Note:/ Consider using 'networkInterfaceIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
veNetworkInterfaceIds :: Lens.Lens' VPCEndpoint (Lude.Maybe [Lude.Text])
veNetworkInterfaceIds = Lens.lens (networkInterfaceIds :: VPCEndpoint -> Lude.Maybe [Lude.Text]) (\s a -> s {networkInterfaceIds = a} :: VPCEndpoint)
{-# DEPRECATED veNetworkInterfaceIds "Use generic-lens or generic-optics with 'networkInterfaceIds' instead." #-}

-- | The ID of the VPC to which the endpoint is associated.
--
-- /Note:/ Consider using 'vpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
veVPCId :: Lens.Lens' VPCEndpoint (Lude.Maybe Lude.Text)
veVPCId = Lens.lens (vpcId :: VPCEndpoint -> Lude.Maybe Lude.Text) (\s a -> s {vpcId = a} :: VPCEndpoint)
{-# DEPRECATED veVPCId "Use generic-lens or generic-optics with 'vpcId' instead." #-}

-- | Indicates whether the VPC endpoint is being managed by its service.
--
-- /Note:/ Consider using 'requesterManaged' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
veRequesterManaged :: Lens.Lens' VPCEndpoint (Lude.Maybe Lude.Bool)
veRequesterManaged = Lens.lens (requesterManaged :: VPCEndpoint -> Lude.Maybe Lude.Bool) (\s a -> s {requesterManaged = a} :: VPCEndpoint)
{-# DEPRECATED veRequesterManaged "Use generic-lens or generic-optics with 'requesterManaged' instead." #-}

-- | (Interface endpoint) The DNS entries for the endpoint.
--
-- /Note:/ Consider using 'dnsEntries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
veDNSEntries :: Lens.Lens' VPCEndpoint (Lude.Maybe [DNSEntry])
veDNSEntries = Lens.lens (dnsEntries :: VPCEndpoint -> Lude.Maybe [DNSEntry]) (\s a -> s {dnsEntries = a} :: VPCEndpoint)
{-# DEPRECATED veDNSEntries "Use generic-lens or generic-optics with 'dnsEntries' instead." #-}

-- | The type of endpoint.
--
-- /Note:/ Consider using 'vpcEndpointType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
veVPCEndpointType :: Lens.Lens' VPCEndpoint (Lude.Maybe VPCEndpointType)
veVPCEndpointType = Lens.lens (vpcEndpointType :: VPCEndpoint -> Lude.Maybe VPCEndpointType) (\s a -> s {vpcEndpointType = a} :: VPCEndpoint)
{-# DEPRECATED veVPCEndpointType "Use generic-lens or generic-optics with 'vpcEndpointType' instead." #-}

-- | (Interface endpoint) Indicates whether the VPC is associated with a private hosted zone.
--
-- /Note:/ Consider using 'privateDNSEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vePrivateDNSEnabled :: Lens.Lens' VPCEndpoint (Lude.Maybe Lude.Bool)
vePrivateDNSEnabled = Lens.lens (privateDNSEnabled :: VPCEndpoint -> Lude.Maybe Lude.Bool) (\s a -> s {privateDNSEnabled = a} :: VPCEndpoint)
{-# DEPRECATED vePrivateDNSEnabled "Use generic-lens or generic-optics with 'privateDNSEnabled' instead." #-}

-- | The ID of the AWS account that owns the VPC endpoint.
--
-- /Note:/ Consider using 'ownerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
veOwnerId :: Lens.Lens' VPCEndpoint (Lude.Maybe Lude.Text)
veOwnerId = Lens.lens (ownerId :: VPCEndpoint -> Lude.Maybe Lude.Text) (\s a -> s {ownerId = a} :: VPCEndpoint)
{-# DEPRECATED veOwnerId "Use generic-lens or generic-optics with 'ownerId' instead." #-}

-- | The date and time that the VPC endpoint was created.
--
-- /Note:/ Consider using 'creationTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
veCreationTimestamp :: Lens.Lens' VPCEndpoint (Lude.Maybe Lude.ISO8601)
veCreationTimestamp = Lens.lens (creationTimestamp :: VPCEndpoint -> Lude.Maybe Lude.ISO8601) (\s a -> s {creationTimestamp = a} :: VPCEndpoint)
{-# DEPRECATED veCreationTimestamp "Use generic-lens or generic-optics with 'creationTimestamp' instead." #-}

-- | The name of the service to which the endpoint is associated.
--
-- /Note:/ Consider using 'serviceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
veServiceName :: Lens.Lens' VPCEndpoint (Lude.Maybe Lude.Text)
veServiceName = Lens.lens (serviceName :: VPCEndpoint -> Lude.Maybe Lude.Text) (\s a -> s {serviceName = a} :: VPCEndpoint)
{-# DEPRECATED veServiceName "Use generic-lens or generic-optics with 'serviceName' instead." #-}

-- | The last error that occurred for VPC endpoint.
--
-- /Note:/ Consider using 'lastError' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
veLastError :: Lens.Lens' VPCEndpoint (Lude.Maybe LastError)
veLastError = Lens.lens (lastError :: VPCEndpoint -> Lude.Maybe LastError) (\s a -> s {lastError = a} :: VPCEndpoint)
{-# DEPRECATED veLastError "Use generic-lens or generic-optics with 'lastError' instead." #-}

-- | The ID of the VPC endpoint.
--
-- /Note:/ Consider using 'vpcEndpointId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
veVPCEndpointId :: Lens.Lens' VPCEndpoint (Lude.Maybe Lude.Text)
veVPCEndpointId = Lens.lens (vpcEndpointId :: VPCEndpoint -> Lude.Maybe Lude.Text) (\s a -> s {vpcEndpointId = a} :: VPCEndpoint)
{-# DEPRECATED veVPCEndpointId "Use generic-lens or generic-optics with 'vpcEndpointId' instead." #-}

-- | Any tags assigned to the VPC endpoint.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
veTags :: Lens.Lens' VPCEndpoint (Lude.Maybe [Tag])
veTags = Lens.lens (tags :: VPCEndpoint -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: VPCEndpoint)
{-# DEPRECATED veTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | (Gateway endpoint) One or more route tables associated with the endpoint.
--
-- /Note:/ Consider using 'routeTableIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
veRouteTableIds :: Lens.Lens' VPCEndpoint (Lude.Maybe [Lude.Text])
veRouteTableIds = Lens.lens (routeTableIds :: VPCEndpoint -> Lude.Maybe [Lude.Text]) (\s a -> s {routeTableIds = a} :: VPCEndpoint)
{-# DEPRECATED veRouteTableIds "Use generic-lens or generic-optics with 'routeTableIds' instead." #-}

instance Lude.FromXML VPCEndpoint where
  parseXML x =
    VPCEndpoint'
      Lude.<$> ( x Lude..@? "groupSet" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
      Lude.<*> (x Lude..@? "state")
      Lude.<*> (x Lude..@? "policyDocument")
      Lude.<*> ( x Lude..@? "subnetIdSet" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
      Lude.<*> ( x Lude..@? "networkInterfaceIdSet" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
      Lude.<*> (x Lude..@? "vpcId")
      Lude.<*> (x Lude..@? "requesterManaged")
      Lude.<*> ( x Lude..@? "dnsEntrySet" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
      Lude.<*> (x Lude..@? "vpcEndpointType")
      Lude.<*> (x Lude..@? "privateDnsEnabled")
      Lude.<*> (x Lude..@? "ownerId")
      Lude.<*> (x Lude..@? "creationTimestamp")
      Lude.<*> (x Lude..@? "serviceName")
      Lude.<*> (x Lude..@? "lastError")
      Lude.<*> (x Lude..@? "vpcEndpointId")
      Lude.<*> ( x Lude..@? "tagSet" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
      Lude.<*> ( x Lude..@? "routeTableIdSet" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
