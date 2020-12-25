{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.TagSpecification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.TagSpecification
  ( TagSpecification (..),

    -- * Smart constructor
    mkTagSpecification,

    -- * Lenses
    tsResourceType,
    tsTags,
  )
where

import qualified Network.AWS.EC2.Types.ResourceType as Types
import qualified Network.AWS.EC2.Types.Tag as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The tags to apply to a resource when the resource is being created.
--
-- /See:/ 'mkTagSpecification' smart constructor.
data TagSpecification = TagSpecification'
  { -- | The type of resource to tag. Currently, the resource types that support tagging on creation are: @capacity-reservation@ | @carrier-gateway@ | @client-vpn-endpoint@ | @customer-gateway@ | @dedicated-host@ | @dhcp-options@ | @export-image-task@ | @export-instance-task@ | @fleet@ | @fpga-image@ | @host-reservation@ | @import-image-task@ | @import-snapshot-task@ | @instance@ | @internet-gateway@ | @ipv4pool-ec2@ | @ipv6pool-ec2@ | @key-pair@ | @launch-template@ | @placement-group@ | @prefix-list@ | @natgateway@ | @network-acl@ | @route-table@ | @security-group@ | @spot-fleet-request@ | @spot-instances-request@ | @snapshot@ | @subnet@ | @traffic-mirror-filter@ | @traffic-mirror-session@ | @traffic-mirror-target@ | @transit-gateway@ | @transit-gateway-attachment@ | @transit-gateway-route-table@ | @volume@ |@vpc@ | @vpc-peering-connection@ | @vpc-endpoint@ (for interface and gateway endpoints) | @vpc-endpoint-service@ (for AWS PrivateLink) | @vpc-flow-log@ | @vpn-connection@ | @vpn-gateway@ .
    --
    -- To tag a resource after it has been created, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateTags.html CreateTags> .
    resourceType :: Core.Maybe Types.ResourceType,
    -- | The tags to apply to the resource.
    tags :: Core.Maybe [Types.Tag]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TagSpecification' value with any optional fields omitted.
mkTagSpecification ::
  TagSpecification
mkTagSpecification =
  TagSpecification'
    { resourceType = Core.Nothing,
      tags = Core.Nothing
    }

-- | The type of resource to tag. Currently, the resource types that support tagging on creation are: @capacity-reservation@ | @carrier-gateway@ | @client-vpn-endpoint@ | @customer-gateway@ | @dedicated-host@ | @dhcp-options@ | @export-image-task@ | @export-instance-task@ | @fleet@ | @fpga-image@ | @host-reservation@ | @import-image-task@ | @import-snapshot-task@ | @instance@ | @internet-gateway@ | @ipv4pool-ec2@ | @ipv6pool-ec2@ | @key-pair@ | @launch-template@ | @placement-group@ | @prefix-list@ | @natgateway@ | @network-acl@ | @route-table@ | @security-group@ | @spot-fleet-request@ | @spot-instances-request@ | @snapshot@ | @subnet@ | @traffic-mirror-filter@ | @traffic-mirror-session@ | @traffic-mirror-target@ | @transit-gateway@ | @transit-gateway-attachment@ | @transit-gateway-route-table@ | @volume@ |@vpc@ | @vpc-peering-connection@ | @vpc-endpoint@ (for interface and gateway endpoints) | @vpc-endpoint-service@ (for AWS PrivateLink) | @vpc-flow-log@ | @vpn-connection@ | @vpn-gateway@ .
--
-- To tag a resource after it has been created, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateTags.html CreateTags> .
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsResourceType :: Lens.Lens' TagSpecification (Core.Maybe Types.ResourceType)
tsResourceType = Lens.field @"resourceType"
{-# DEPRECATED tsResourceType "Use generic-lens or generic-optics with 'resourceType' instead." #-}

-- | The tags to apply to the resource.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsTags :: Lens.Lens' TagSpecification (Core.Maybe [Types.Tag])
tsTags = Lens.field @"tags"
{-# DEPRECATED tsTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Core.FromXML TagSpecification where
  parseXML x =
    TagSpecification'
      Core.<$> (x Core..@? "resourceType")
      Core.<*> (x Core..@? "Tag" Core..<@> Core.parseXMLList "item")
