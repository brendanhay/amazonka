{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.TagSpecification
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.TagSpecification where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.ResourceType
import Network.AWS.EC2.Types.Tag
import qualified Network.AWS.Lens as Lens

-- | The tags to apply to a resource when the resource is being created.
--
-- /See:/ 'newTagSpecification' smart constructor.
data TagSpecification = TagSpecification'
  { -- | The type of resource to tag. Currently, the resource types that support
    -- tagging on creation are: @capacity-reservation@ | @carrier-gateway@ |
    -- @client-vpn-endpoint@ | @customer-gateway@ | @dedicated-host@ |
    -- @dhcp-options@ | @egress-only-internet-gateway@ | @elastic-ip@ |
    -- @elastic-gpu@ | @export-image-task@ | @export-instance-task@ | @fleet@ |
    -- @fpga-image@ | @host-reservation@ | @image@| @import-image-task@ |
    -- @import-snapshot-task@ | @instance@ | @internet-gateway@ |
    -- @ipv4pool-ec2@ | @ipv6pool-ec2@ | @key-pair@ | @launch-template@ |
    -- @local-gateway-route-table-vpc-association@ | @placement-group@ |
    -- @prefix-list@ | @natgateway@ | @network-acl@ | @network-interface@ |
    -- @reserved-instances@ |@route-table@ | @security-group@| @snapshot@ |
    -- @spot-fleet-request@ | @spot-instances-request@ | @snapshot@ | @subnet@
    -- | @traffic-mirror-filter@ | @traffic-mirror-session@ |
    -- @traffic-mirror-target@ | @transit-gateway@ |
    -- @transit-gateway-attachment@ | @transit-gateway-multicast-domain@ |
    -- @transit-gateway-route-table@ | @volume@ |@vpc@ |
    -- @ vpc-peering-connection@ | @vpc-endpoint@ (for interface and gateway
    -- endpoints) | @vpc-endpoint-service@ (for AWS PrivateLink) |
    -- @vpc-flow-log@ | @vpn-connection@ | @vpn-gateway@.
    --
    -- To tag a resource after it has been created, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateTags.html CreateTags>.
    resourceType :: Core.Maybe ResourceType,
    -- | The tags to apply to the resource.
    tags :: Core.Maybe [Tag]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'TagSpecification' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceType', 'tagSpecification_resourceType' - The type of resource to tag. Currently, the resource types that support
-- tagging on creation are: @capacity-reservation@ | @carrier-gateway@ |
-- @client-vpn-endpoint@ | @customer-gateway@ | @dedicated-host@ |
-- @dhcp-options@ | @egress-only-internet-gateway@ | @elastic-ip@ |
-- @elastic-gpu@ | @export-image-task@ | @export-instance-task@ | @fleet@ |
-- @fpga-image@ | @host-reservation@ | @image@| @import-image-task@ |
-- @import-snapshot-task@ | @instance@ | @internet-gateway@ |
-- @ipv4pool-ec2@ | @ipv6pool-ec2@ | @key-pair@ | @launch-template@ |
-- @local-gateway-route-table-vpc-association@ | @placement-group@ |
-- @prefix-list@ | @natgateway@ | @network-acl@ | @network-interface@ |
-- @reserved-instances@ |@route-table@ | @security-group@| @snapshot@ |
-- @spot-fleet-request@ | @spot-instances-request@ | @snapshot@ | @subnet@
-- | @traffic-mirror-filter@ | @traffic-mirror-session@ |
-- @traffic-mirror-target@ | @transit-gateway@ |
-- @transit-gateway-attachment@ | @transit-gateway-multicast-domain@ |
-- @transit-gateway-route-table@ | @volume@ |@vpc@ |
-- @ vpc-peering-connection@ | @vpc-endpoint@ (for interface and gateway
-- endpoints) | @vpc-endpoint-service@ (for AWS PrivateLink) |
-- @vpc-flow-log@ | @vpn-connection@ | @vpn-gateway@.
--
-- To tag a resource after it has been created, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateTags.html CreateTags>.
--
-- 'tags', 'tagSpecification_tags' - The tags to apply to the resource.
newTagSpecification ::
  TagSpecification
newTagSpecification =
  TagSpecification'
    { resourceType = Core.Nothing,
      tags = Core.Nothing
    }

-- | The type of resource to tag. Currently, the resource types that support
-- tagging on creation are: @capacity-reservation@ | @carrier-gateway@ |
-- @client-vpn-endpoint@ | @customer-gateway@ | @dedicated-host@ |
-- @dhcp-options@ | @egress-only-internet-gateway@ | @elastic-ip@ |
-- @elastic-gpu@ | @export-image-task@ | @export-instance-task@ | @fleet@ |
-- @fpga-image@ | @host-reservation@ | @image@| @import-image-task@ |
-- @import-snapshot-task@ | @instance@ | @internet-gateway@ |
-- @ipv4pool-ec2@ | @ipv6pool-ec2@ | @key-pair@ | @launch-template@ |
-- @local-gateway-route-table-vpc-association@ | @placement-group@ |
-- @prefix-list@ | @natgateway@ | @network-acl@ | @network-interface@ |
-- @reserved-instances@ |@route-table@ | @security-group@| @snapshot@ |
-- @spot-fleet-request@ | @spot-instances-request@ | @snapshot@ | @subnet@
-- | @traffic-mirror-filter@ | @traffic-mirror-session@ |
-- @traffic-mirror-target@ | @transit-gateway@ |
-- @transit-gateway-attachment@ | @transit-gateway-multicast-domain@ |
-- @transit-gateway-route-table@ | @volume@ |@vpc@ |
-- @ vpc-peering-connection@ | @vpc-endpoint@ (for interface and gateway
-- endpoints) | @vpc-endpoint-service@ (for AWS PrivateLink) |
-- @vpc-flow-log@ | @vpn-connection@ | @vpn-gateway@.
--
-- To tag a resource after it has been created, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateTags.html CreateTags>.
tagSpecification_resourceType :: Lens.Lens' TagSpecification (Core.Maybe ResourceType)
tagSpecification_resourceType = Lens.lens (\TagSpecification' {resourceType} -> resourceType) (\s@TagSpecification' {} a -> s {resourceType = a} :: TagSpecification)

-- | The tags to apply to the resource.
tagSpecification_tags :: Lens.Lens' TagSpecification (Core.Maybe [Tag])
tagSpecification_tags = Lens.lens (\TagSpecification' {tags} -> tags) (\s@TagSpecification' {} a -> s {tags = a} :: TagSpecification) Core.. Lens.mapping Lens._Coerce

instance Core.FromXML TagSpecification where
  parseXML x =
    TagSpecification'
      Core.<$> (x Core..@? "resourceType")
      Core.<*> ( x Core..@? "Tag" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "item")
               )

instance Core.Hashable TagSpecification

instance Core.NFData TagSpecification

instance Core.ToQuery TagSpecification where
  toQuery TagSpecification' {..} =
    Core.mconcat
      [ "ResourceType" Core.=: resourceType,
        Core.toQuery (Core.toQueryList "Tag" Core.<$> tags)
      ]
