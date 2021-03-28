{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.VpcEndpoint
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.VpcEndpoint
  ( VpcEndpoint (..)
  -- * Smart constructor
  , mkVpcEndpoint
  -- * Lenses
  , veCreationTimestamp
  , veDnsEntries
  , veGroups
  , veLastError
  , veNetworkInterfaceIds
  , veOwnerId
  , vePolicyDocument
  , vePrivateDnsEnabled
  , veRequesterManaged
  , veRouteTableIds
  , veServiceName
  , veState
  , veSubnetIds
  , veTags
  , veVpcEndpointId
  , veVpcEndpointType
  , veVpcId
  ) where

import qualified Network.AWS.EC2.Types.DnsEntry as Types
import qualified Network.AWS.EC2.Types.LastError as Types
import qualified Network.AWS.EC2.Types.SecurityGroupIdentifier as Types
import qualified Network.AWS.EC2.Types.State as Types
import qualified Network.AWS.EC2.Types.Tag as Types
import qualified Network.AWS.EC2.Types.VpcEndpointType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a VPC endpoint.
--
-- /See:/ 'mkVpcEndpoint' smart constructor.
data VpcEndpoint = VpcEndpoint'
  { creationTimestamp :: Core.Maybe Core.UTCTime
    -- ^ The date and time that the VPC endpoint was created.
  , dnsEntries :: Core.Maybe [Types.DnsEntry]
    -- ^ (Interface endpoint) The DNS entries for the endpoint.
  , groups :: Core.Maybe [Types.SecurityGroupIdentifier]
    -- ^ (Interface endpoint) Information about the security groups that are associated with the network interface.
  , lastError :: Core.Maybe Types.LastError
    -- ^ The last error that occurred for VPC endpoint.
  , networkInterfaceIds :: Core.Maybe [Core.Text]
    -- ^ (Interface endpoint) One or more network interfaces for the endpoint.
  , ownerId :: Core.Maybe Core.Text
    -- ^ The ID of the AWS account that owns the VPC endpoint.
  , policyDocument :: Core.Maybe Core.Text
    -- ^ The policy document associated with the endpoint, if applicable.
  , privateDnsEnabled :: Core.Maybe Core.Bool
    -- ^ (Interface endpoint) Indicates whether the VPC is associated with a private hosted zone.
  , requesterManaged :: Core.Maybe Core.Bool
    -- ^ Indicates whether the VPC endpoint is being managed by its service.
  , routeTableIds :: Core.Maybe [Core.Text]
    -- ^ (Gateway endpoint) One or more route tables associated with the endpoint.
  , serviceName :: Core.Maybe Core.Text
    -- ^ The name of the service to which the endpoint is associated.
  , state :: Core.Maybe Types.State
    -- ^ The state of the VPC endpoint.
  , subnetIds :: Core.Maybe [Core.Text]
    -- ^ (Interface endpoint) One or more subnets in which the endpoint is located.
  , tags :: Core.Maybe [Types.Tag]
    -- ^ Any tags assigned to the VPC endpoint.
  , vpcEndpointId :: Core.Maybe Core.Text
    -- ^ The ID of the VPC endpoint.
  , vpcEndpointType :: Core.Maybe Types.VpcEndpointType
    -- ^ The type of endpoint.
  , vpcId :: Core.Maybe Core.Text
    -- ^ The ID of the VPC to which the endpoint is associated.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'VpcEndpoint' value with any optional fields omitted.
mkVpcEndpoint
    :: VpcEndpoint
mkVpcEndpoint
  = VpcEndpoint'{creationTimestamp = Core.Nothing,
                 dnsEntries = Core.Nothing, groups = Core.Nothing,
                 lastError = Core.Nothing, networkInterfaceIds = Core.Nothing,
                 ownerId = Core.Nothing, policyDocument = Core.Nothing,
                 privateDnsEnabled = Core.Nothing, requesterManaged = Core.Nothing,
                 routeTableIds = Core.Nothing, serviceName = Core.Nothing,
                 state = Core.Nothing, subnetIds = Core.Nothing,
                 tags = Core.Nothing, vpcEndpointId = Core.Nothing,
                 vpcEndpointType = Core.Nothing, vpcId = Core.Nothing}

-- | The date and time that the VPC endpoint was created.
--
-- /Note:/ Consider using 'creationTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
veCreationTimestamp :: Lens.Lens' VpcEndpoint (Core.Maybe Core.UTCTime)
veCreationTimestamp = Lens.field @"creationTimestamp"
{-# INLINEABLE veCreationTimestamp #-}
{-# DEPRECATED creationTimestamp "Use generic-lens or generic-optics with 'creationTimestamp' instead"  #-}

-- | (Interface endpoint) The DNS entries for the endpoint.
--
-- /Note:/ Consider using 'dnsEntries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
veDnsEntries :: Lens.Lens' VpcEndpoint (Core.Maybe [Types.DnsEntry])
veDnsEntries = Lens.field @"dnsEntries"
{-# INLINEABLE veDnsEntries #-}
{-# DEPRECATED dnsEntries "Use generic-lens or generic-optics with 'dnsEntries' instead"  #-}

-- | (Interface endpoint) Information about the security groups that are associated with the network interface.
--
-- /Note:/ Consider using 'groups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
veGroups :: Lens.Lens' VpcEndpoint (Core.Maybe [Types.SecurityGroupIdentifier])
veGroups = Lens.field @"groups"
{-# INLINEABLE veGroups #-}
{-# DEPRECATED groups "Use generic-lens or generic-optics with 'groups' instead"  #-}

-- | The last error that occurred for VPC endpoint.
--
-- /Note:/ Consider using 'lastError' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
veLastError :: Lens.Lens' VpcEndpoint (Core.Maybe Types.LastError)
veLastError = Lens.field @"lastError"
{-# INLINEABLE veLastError #-}
{-# DEPRECATED lastError "Use generic-lens or generic-optics with 'lastError' instead"  #-}

-- | (Interface endpoint) One or more network interfaces for the endpoint.
--
-- /Note:/ Consider using 'networkInterfaceIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
veNetworkInterfaceIds :: Lens.Lens' VpcEndpoint (Core.Maybe [Core.Text])
veNetworkInterfaceIds = Lens.field @"networkInterfaceIds"
{-# INLINEABLE veNetworkInterfaceIds #-}
{-# DEPRECATED networkInterfaceIds "Use generic-lens or generic-optics with 'networkInterfaceIds' instead"  #-}

-- | The ID of the AWS account that owns the VPC endpoint.
--
-- /Note:/ Consider using 'ownerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
veOwnerId :: Lens.Lens' VpcEndpoint (Core.Maybe Core.Text)
veOwnerId = Lens.field @"ownerId"
{-# INLINEABLE veOwnerId #-}
{-# DEPRECATED ownerId "Use generic-lens or generic-optics with 'ownerId' instead"  #-}

-- | The policy document associated with the endpoint, if applicable.
--
-- /Note:/ Consider using 'policyDocument' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vePolicyDocument :: Lens.Lens' VpcEndpoint (Core.Maybe Core.Text)
vePolicyDocument = Lens.field @"policyDocument"
{-# INLINEABLE vePolicyDocument #-}
{-# DEPRECATED policyDocument "Use generic-lens or generic-optics with 'policyDocument' instead"  #-}

-- | (Interface endpoint) Indicates whether the VPC is associated with a private hosted zone.
--
-- /Note:/ Consider using 'privateDnsEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vePrivateDnsEnabled :: Lens.Lens' VpcEndpoint (Core.Maybe Core.Bool)
vePrivateDnsEnabled = Lens.field @"privateDnsEnabled"
{-# INLINEABLE vePrivateDnsEnabled #-}
{-# DEPRECATED privateDnsEnabled "Use generic-lens or generic-optics with 'privateDnsEnabled' instead"  #-}

-- | Indicates whether the VPC endpoint is being managed by its service.
--
-- /Note:/ Consider using 'requesterManaged' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
veRequesterManaged :: Lens.Lens' VpcEndpoint (Core.Maybe Core.Bool)
veRequesterManaged = Lens.field @"requesterManaged"
{-# INLINEABLE veRequesterManaged #-}
{-# DEPRECATED requesterManaged "Use generic-lens or generic-optics with 'requesterManaged' instead"  #-}

-- | (Gateway endpoint) One or more route tables associated with the endpoint.
--
-- /Note:/ Consider using 'routeTableIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
veRouteTableIds :: Lens.Lens' VpcEndpoint (Core.Maybe [Core.Text])
veRouteTableIds = Lens.field @"routeTableIds"
{-# INLINEABLE veRouteTableIds #-}
{-# DEPRECATED routeTableIds "Use generic-lens or generic-optics with 'routeTableIds' instead"  #-}

-- | The name of the service to which the endpoint is associated.
--
-- /Note:/ Consider using 'serviceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
veServiceName :: Lens.Lens' VpcEndpoint (Core.Maybe Core.Text)
veServiceName = Lens.field @"serviceName"
{-# INLINEABLE veServiceName #-}
{-# DEPRECATED serviceName "Use generic-lens or generic-optics with 'serviceName' instead"  #-}

-- | The state of the VPC endpoint.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
veState :: Lens.Lens' VpcEndpoint (Core.Maybe Types.State)
veState = Lens.field @"state"
{-# INLINEABLE veState #-}
{-# DEPRECATED state "Use generic-lens or generic-optics with 'state' instead"  #-}

-- | (Interface endpoint) One or more subnets in which the endpoint is located.
--
-- /Note:/ Consider using 'subnetIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
veSubnetIds :: Lens.Lens' VpcEndpoint (Core.Maybe [Core.Text])
veSubnetIds = Lens.field @"subnetIds"
{-# INLINEABLE veSubnetIds #-}
{-# DEPRECATED subnetIds "Use generic-lens or generic-optics with 'subnetIds' instead"  #-}

-- | Any tags assigned to the VPC endpoint.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
veTags :: Lens.Lens' VpcEndpoint (Core.Maybe [Types.Tag])
veTags = Lens.field @"tags"
{-# INLINEABLE veTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

-- | The ID of the VPC endpoint.
--
-- /Note:/ Consider using 'vpcEndpointId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
veVpcEndpointId :: Lens.Lens' VpcEndpoint (Core.Maybe Core.Text)
veVpcEndpointId = Lens.field @"vpcEndpointId"
{-# INLINEABLE veVpcEndpointId #-}
{-# DEPRECATED vpcEndpointId "Use generic-lens or generic-optics with 'vpcEndpointId' instead"  #-}

-- | The type of endpoint.
--
-- /Note:/ Consider using 'vpcEndpointType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
veVpcEndpointType :: Lens.Lens' VpcEndpoint (Core.Maybe Types.VpcEndpointType)
veVpcEndpointType = Lens.field @"vpcEndpointType"
{-# INLINEABLE veVpcEndpointType #-}
{-# DEPRECATED vpcEndpointType "Use generic-lens or generic-optics with 'vpcEndpointType' instead"  #-}

-- | The ID of the VPC to which the endpoint is associated.
--
-- /Note:/ Consider using 'vpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
veVpcId :: Lens.Lens' VpcEndpoint (Core.Maybe Core.Text)
veVpcId = Lens.field @"vpcId"
{-# INLINEABLE veVpcId #-}
{-# DEPRECATED vpcId "Use generic-lens or generic-optics with 'vpcId' instead"  #-}

instance Core.FromXML VpcEndpoint where
        parseXML x
          = VpcEndpoint' Core.<$>
              (x Core..@? "creationTimestamp") Core.<*>
                x Core..@? "dnsEntrySet" Core..<@> Core.parseXMLList "item"
                Core.<*> x Core..@? "groupSet" Core..<@> Core.parseXMLList "item"
                Core.<*> x Core..@? "lastError"
                Core.<*>
                x Core..@? "networkInterfaceIdSet" Core..<@>
                  Core.parseXMLList "item"
                Core.<*> x Core..@? "ownerId"
                Core.<*> x Core..@? "policyDocument"
                Core.<*> x Core..@? "privateDnsEnabled"
                Core.<*> x Core..@? "requesterManaged"
                Core.<*>
                x Core..@? "routeTableIdSet" Core..<@> Core.parseXMLList "item"
                Core.<*> x Core..@? "serviceName"
                Core.<*> x Core..@? "state"
                Core.<*>
                x Core..@? "subnetIdSet" Core..<@> Core.parseXMLList "item"
                Core.<*> x Core..@? "tagSet" Core..<@> Core.parseXMLList "item"
                Core.<*> x Core..@? "vpcEndpointId"
                Core.<*> x Core..@? "vpcEndpointType"
                Core.<*> x Core..@? "vpcId"
