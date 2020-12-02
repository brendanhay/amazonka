{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.TagSpecification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.TagSpecification where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.ResourceType
import Network.AWS.EC2.Types.Tag
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The tags to apply to a resource when the resource is being created.
--
--
--
-- /See:/ 'tagSpecification' smart constructor.
data TagSpecification = TagSpecification'
  { _tsResourceType ::
      !(Maybe ResourceType),
    _tsTags :: !(Maybe [Tag])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TagSpecification' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tsResourceType' - The type of resource to tag. Currently, the resource types that support tagging on creation are: @capacity-reservation@ | @carrier-gateway@ | @client-vpn-endpoint@ | @customer-gateway@ | @dedicated-host@ | @dhcp-options@ | @export-image-task@ | @export-instance-task@ | @fleet@ | @fpga-image@ | @host-reservation@ | @import-image-task@ | @import-snapshot-task@ | @instance@ | @internet-gateway@ | @ipv4pool-ec2@ | @ipv6pool-ec2@ | @key-pair@ | @launch-template@ | @placement-group@ | @prefix-list@ | @natgateway@ | @network-acl@ | @route-table@ | @security-group@ | @spot-fleet-request@ | @spot-instances-request@ | @snapshot@ | @subnet@ | @traffic-mirror-filter@ | @traffic-mirror-session@ | @traffic-mirror-target@ | @transit-gateway@ | @transit-gateway-attachment@ | @transit-gateway-route-table@ | @volume@ |@vpc@ | @vpc-peering-connection@ | @vpc-endpoint@ (for interface and gateway endpoints) | @vpc-endpoint-service@ (for AWS PrivateLink) | @vpc-flow-log@ | @vpn-connection@ | @vpn-gateway@ . To tag a resource after it has been created, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateTags.html CreateTags> .
--
-- * 'tsTags' - The tags to apply to the resource.
tagSpecification ::
  TagSpecification
tagSpecification =
  TagSpecification' {_tsResourceType = Nothing, _tsTags = Nothing}

-- | The type of resource to tag. Currently, the resource types that support tagging on creation are: @capacity-reservation@ | @carrier-gateway@ | @client-vpn-endpoint@ | @customer-gateway@ | @dedicated-host@ | @dhcp-options@ | @export-image-task@ | @export-instance-task@ | @fleet@ | @fpga-image@ | @host-reservation@ | @import-image-task@ | @import-snapshot-task@ | @instance@ | @internet-gateway@ | @ipv4pool-ec2@ | @ipv6pool-ec2@ | @key-pair@ | @launch-template@ | @placement-group@ | @prefix-list@ | @natgateway@ | @network-acl@ | @route-table@ | @security-group@ | @spot-fleet-request@ | @spot-instances-request@ | @snapshot@ | @subnet@ | @traffic-mirror-filter@ | @traffic-mirror-session@ | @traffic-mirror-target@ | @transit-gateway@ | @transit-gateway-attachment@ | @transit-gateway-route-table@ | @volume@ |@vpc@ | @vpc-peering-connection@ | @vpc-endpoint@ (for interface and gateway endpoints) | @vpc-endpoint-service@ (for AWS PrivateLink) | @vpc-flow-log@ | @vpn-connection@ | @vpn-gateway@ . To tag a resource after it has been created, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateTags.html CreateTags> .
tsResourceType :: Lens' TagSpecification (Maybe ResourceType)
tsResourceType = lens _tsResourceType (\s a -> s {_tsResourceType = a})

-- | The tags to apply to the resource.
tsTags :: Lens' TagSpecification [Tag]
tsTags = lens _tsTags (\s a -> s {_tsTags = a}) . _Default . _Coerce

instance FromXML TagSpecification where
  parseXML x =
    TagSpecification'
      <$> (x .@? "resourceType")
      <*> (x .@? "Tag" .!@ mempty >>= may (parseXMLList "item"))

instance Hashable TagSpecification

instance NFData TagSpecification

instance ToQuery TagSpecification where
  toQuery TagSpecification' {..} =
    mconcat
      [ "ResourceType" =: _tsResourceType,
        toQuery (toQueryList "Tag" <$> _tsTags)
      ]
