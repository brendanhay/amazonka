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

import Network.AWS.EC2.Types.ResourceType
import Network.AWS.EC2.Types.Tag
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The tags to apply to a resource when the resource is being created.
--
-- /See:/ 'mkTagSpecification' smart constructor.
data TagSpecification = TagSpecification'
  { resourceType ::
      Lude.Maybe ResourceType,
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

-- | Creates a value of 'TagSpecification' with the minimum fields required to make a request.
--
-- * 'resourceType' - The type of resource to tag. Currently, the resource types that support tagging on creation are: @capacity-reservation@ | @carrier-gateway@ | @client-vpn-endpoint@ | @customer-gateway@ | @dedicated-host@ | @dhcp-options@ | @export-image-task@ | @export-instance-task@ | @fleet@ | @fpga-image@ | @host-reservation@ | @import-image-task@ | @import-snapshot-task@ | @instance@ | @internet-gateway@ | @ipv4pool-ec2@ | @ipv6pool-ec2@ | @key-pair@ | @launch-template@ | @placement-group@ | @prefix-list@ | @natgateway@ | @network-acl@ | @route-table@ | @security-group@ | @spot-fleet-request@ | @spot-instances-request@ | @snapshot@ | @subnet@ | @traffic-mirror-filter@ | @traffic-mirror-session@ | @traffic-mirror-target@ | @transit-gateway@ | @transit-gateway-attachment@ | @transit-gateway-route-table@ | @volume@ |@vpc@ | @vpc-peering-connection@ | @vpc-endpoint@ (for interface and gateway endpoints) | @vpc-endpoint-service@ (for AWS PrivateLink) | @vpc-flow-log@ | @vpn-connection@ | @vpn-gateway@ .
--
-- To tag a resource after it has been created, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateTags.html CreateTags> .
-- * 'tags' - The tags to apply to the resource.
mkTagSpecification ::
  TagSpecification
mkTagSpecification =
  TagSpecification'
    { resourceType = Lude.Nothing,
      tags = Lude.Nothing
    }

-- | The type of resource to tag. Currently, the resource types that support tagging on creation are: @capacity-reservation@ | @carrier-gateway@ | @client-vpn-endpoint@ | @customer-gateway@ | @dedicated-host@ | @dhcp-options@ | @export-image-task@ | @export-instance-task@ | @fleet@ | @fpga-image@ | @host-reservation@ | @import-image-task@ | @import-snapshot-task@ | @instance@ | @internet-gateway@ | @ipv4pool-ec2@ | @ipv6pool-ec2@ | @key-pair@ | @launch-template@ | @placement-group@ | @prefix-list@ | @natgateway@ | @network-acl@ | @route-table@ | @security-group@ | @spot-fleet-request@ | @spot-instances-request@ | @snapshot@ | @subnet@ | @traffic-mirror-filter@ | @traffic-mirror-session@ | @traffic-mirror-target@ | @transit-gateway@ | @transit-gateway-attachment@ | @transit-gateway-route-table@ | @volume@ |@vpc@ | @vpc-peering-connection@ | @vpc-endpoint@ (for interface and gateway endpoints) | @vpc-endpoint-service@ (for AWS PrivateLink) | @vpc-flow-log@ | @vpn-connection@ | @vpn-gateway@ .
--
-- To tag a resource after it has been created, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateTags.html CreateTags> .
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsResourceType :: Lens.Lens' TagSpecification (Lude.Maybe ResourceType)
tsResourceType = Lens.lens (resourceType :: TagSpecification -> Lude.Maybe ResourceType) (\s a -> s {resourceType = a} :: TagSpecification)
{-# DEPRECATED tsResourceType "Use generic-lens or generic-optics with 'resourceType' instead." #-}

-- | The tags to apply to the resource.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsTags :: Lens.Lens' TagSpecification (Lude.Maybe [Tag])
tsTags = Lens.lens (tags :: TagSpecification -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: TagSpecification)
{-# DEPRECATED tsTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.FromXML TagSpecification where
  parseXML x =
    TagSpecification'
      Lude.<$> (x Lude..@? "resourceType")
      Lude.<*> ( x Lude..@? "Tag" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )

instance Lude.ToQuery TagSpecification where
  toQuery TagSpecification' {..} =
    Lude.mconcat
      [ "ResourceType" Lude.=: resourceType,
        Lude.toQuery (Lude.toQueryList "Tag" Lude.<$> tags)
      ]
