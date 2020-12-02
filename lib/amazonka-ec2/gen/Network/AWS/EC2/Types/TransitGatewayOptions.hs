{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.TransitGatewayOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.TransitGatewayOptions where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.AutoAcceptSharedAttachmentsValue
import Network.AWS.EC2.Types.DNSSupportValue
import Network.AWS.EC2.Types.DefaultRouteTableAssociationValue
import Network.AWS.EC2.Types.DefaultRouteTablePropagationValue
import Network.AWS.EC2.Types.MulticastSupportValue
import Network.AWS.EC2.Types.VPNEcmpSupportValue
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the options for a transit gateway.
--
--
--
-- /See:/ 'transitGatewayOptions' smart constructor.
data TransitGatewayOptions = TransitGatewayOptions'
  { _tgoVPNEcmpSupport ::
      !(Maybe VPNEcmpSupportValue),
    _tgoAutoAcceptSharedAttachments ::
      !(Maybe AutoAcceptSharedAttachmentsValue),
    _tgoPropagationDefaultRouteTableId ::
      !(Maybe Text),
    _tgoDefaultRouteTableAssociation ::
      !(Maybe DefaultRouteTableAssociationValue),
    _tgoAssociationDefaultRouteTableId ::
      !(Maybe Text),
    _tgoAmazonSideASN :: !(Maybe Integer),
    _tgoDefaultRouteTablePropagation ::
      !(Maybe DefaultRouteTablePropagationValue),
    _tgoMulticastSupport ::
      !(Maybe MulticastSupportValue),
    _tgoDNSSupport :: !(Maybe DNSSupportValue)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TransitGatewayOptions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tgoVPNEcmpSupport' - Indicates whether Equal Cost Multipath Protocol support is enabled.
--
-- * 'tgoAutoAcceptSharedAttachments' - Indicates whether attachment requests are automatically accepted.
--
-- * 'tgoPropagationDefaultRouteTableId' - The ID of the default propagation route table.
--
-- * 'tgoDefaultRouteTableAssociation' - Indicates whether resource attachments are automatically associated with the default association route table.
--
-- * 'tgoAssociationDefaultRouteTableId' - The ID of the default association route table.
--
-- * 'tgoAmazonSideASN' - A private Autonomous System Number (ASN) for the Amazon side of a BGP session. The range is 64512 to 65534 for 16-bit ASNs and 4200000000 to 4294967294 for 32-bit ASNs.
--
-- * 'tgoDefaultRouteTablePropagation' - Indicates whether resource attachments automatically propagate routes to the default propagation route table.
--
-- * 'tgoMulticastSupport' - Indicates whether multicast is enabled on the transit gateway
--
-- * 'tgoDNSSupport' - Indicates whether DNS support is enabled.
transitGatewayOptions ::
  TransitGatewayOptions
transitGatewayOptions =
  TransitGatewayOptions'
    { _tgoVPNEcmpSupport = Nothing,
      _tgoAutoAcceptSharedAttachments = Nothing,
      _tgoPropagationDefaultRouteTableId = Nothing,
      _tgoDefaultRouteTableAssociation = Nothing,
      _tgoAssociationDefaultRouteTableId = Nothing,
      _tgoAmazonSideASN = Nothing,
      _tgoDefaultRouteTablePropagation = Nothing,
      _tgoMulticastSupport = Nothing,
      _tgoDNSSupport = Nothing
    }

-- | Indicates whether Equal Cost Multipath Protocol support is enabled.
tgoVPNEcmpSupport :: Lens' TransitGatewayOptions (Maybe VPNEcmpSupportValue)
tgoVPNEcmpSupport = lens _tgoVPNEcmpSupport (\s a -> s {_tgoVPNEcmpSupport = a})

-- | Indicates whether attachment requests are automatically accepted.
tgoAutoAcceptSharedAttachments :: Lens' TransitGatewayOptions (Maybe AutoAcceptSharedAttachmentsValue)
tgoAutoAcceptSharedAttachments = lens _tgoAutoAcceptSharedAttachments (\s a -> s {_tgoAutoAcceptSharedAttachments = a})

-- | The ID of the default propagation route table.
tgoPropagationDefaultRouteTableId :: Lens' TransitGatewayOptions (Maybe Text)
tgoPropagationDefaultRouteTableId = lens _tgoPropagationDefaultRouteTableId (\s a -> s {_tgoPropagationDefaultRouteTableId = a})

-- | Indicates whether resource attachments are automatically associated with the default association route table.
tgoDefaultRouteTableAssociation :: Lens' TransitGatewayOptions (Maybe DefaultRouteTableAssociationValue)
tgoDefaultRouteTableAssociation = lens _tgoDefaultRouteTableAssociation (\s a -> s {_tgoDefaultRouteTableAssociation = a})

-- | The ID of the default association route table.
tgoAssociationDefaultRouteTableId :: Lens' TransitGatewayOptions (Maybe Text)
tgoAssociationDefaultRouteTableId = lens _tgoAssociationDefaultRouteTableId (\s a -> s {_tgoAssociationDefaultRouteTableId = a})

-- | A private Autonomous System Number (ASN) for the Amazon side of a BGP session. The range is 64512 to 65534 for 16-bit ASNs and 4200000000 to 4294967294 for 32-bit ASNs.
tgoAmazonSideASN :: Lens' TransitGatewayOptions (Maybe Integer)
tgoAmazonSideASN = lens _tgoAmazonSideASN (\s a -> s {_tgoAmazonSideASN = a})

-- | Indicates whether resource attachments automatically propagate routes to the default propagation route table.
tgoDefaultRouteTablePropagation :: Lens' TransitGatewayOptions (Maybe DefaultRouteTablePropagationValue)
tgoDefaultRouteTablePropagation = lens _tgoDefaultRouteTablePropagation (\s a -> s {_tgoDefaultRouteTablePropagation = a})

-- | Indicates whether multicast is enabled on the transit gateway
tgoMulticastSupport :: Lens' TransitGatewayOptions (Maybe MulticastSupportValue)
tgoMulticastSupport = lens _tgoMulticastSupport (\s a -> s {_tgoMulticastSupport = a})

-- | Indicates whether DNS support is enabled.
tgoDNSSupport :: Lens' TransitGatewayOptions (Maybe DNSSupportValue)
tgoDNSSupport = lens _tgoDNSSupport (\s a -> s {_tgoDNSSupport = a})

instance FromXML TransitGatewayOptions where
  parseXML x =
    TransitGatewayOptions'
      <$> (x .@? "vpnEcmpSupport")
      <*> (x .@? "autoAcceptSharedAttachments")
      <*> (x .@? "propagationDefaultRouteTableId")
      <*> (x .@? "defaultRouteTableAssociation")
      <*> (x .@? "associationDefaultRouteTableId")
      <*> (x .@? "amazonSideAsn")
      <*> (x .@? "defaultRouteTablePropagation")
      <*> (x .@? "multicastSupport")
      <*> (x .@? "dnsSupport")

instance Hashable TransitGatewayOptions

instance NFData TransitGatewayOptions
