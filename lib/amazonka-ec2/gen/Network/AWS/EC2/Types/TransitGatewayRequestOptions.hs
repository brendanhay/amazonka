{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.TransitGatewayRequestOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.TransitGatewayRequestOptions where

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
-- /See:/ 'transitGatewayRequestOptions' smart constructor.
data TransitGatewayRequestOptions = TransitGatewayRequestOptions'
  { _tgroVPNEcmpSupport ::
      !(Maybe VPNEcmpSupportValue),
    _tgroAutoAcceptSharedAttachments ::
      !( Maybe
           AutoAcceptSharedAttachmentsValue
       ),
    _tgroDefaultRouteTableAssociation ::
      !( Maybe
           DefaultRouteTableAssociationValue
       ),
    _tgroAmazonSideASN ::
      !(Maybe Integer),
    _tgroDefaultRouteTablePropagation ::
      !( Maybe
           DefaultRouteTablePropagationValue
       ),
    _tgroMulticastSupport ::
      !(Maybe MulticastSupportValue),
    _tgroDNSSupport ::
      !(Maybe DNSSupportValue)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TransitGatewayRequestOptions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tgroVPNEcmpSupport' - Enable or disable Equal Cost Multipath Protocol support. Enabled by default.
--
-- * 'tgroAutoAcceptSharedAttachments' - Enable or disable automatic acceptance of attachment requests. Disabled by default.
--
-- * 'tgroDefaultRouteTableAssociation' - Enable or disable automatic association with the default association route table. Enabled by default.
--
-- * 'tgroAmazonSideASN' - A private Autonomous System Number (ASN) for the Amazon side of a BGP session. The range is 64512 to 65534 for 16-bit ASNs and 4200000000 to 4294967294 for 32-bit ASNs. The default is @64512@ .
--
-- * 'tgroDefaultRouteTablePropagation' - Enable or disable automatic propagation of routes to the default propagation route table. Enabled by default.
--
-- * 'tgroMulticastSupport' - Indicates whether multicast is enabled on the transit gateway
--
-- * 'tgroDNSSupport' - Enable or disable DNS support. Enabled by default.
transitGatewayRequestOptions ::
  TransitGatewayRequestOptions
transitGatewayRequestOptions =
  TransitGatewayRequestOptions'
    { _tgroVPNEcmpSupport = Nothing,
      _tgroAutoAcceptSharedAttachments = Nothing,
      _tgroDefaultRouteTableAssociation = Nothing,
      _tgroAmazonSideASN = Nothing,
      _tgroDefaultRouteTablePropagation = Nothing,
      _tgroMulticastSupport = Nothing,
      _tgroDNSSupport = Nothing
    }

-- | Enable or disable Equal Cost Multipath Protocol support. Enabled by default.
tgroVPNEcmpSupport :: Lens' TransitGatewayRequestOptions (Maybe VPNEcmpSupportValue)
tgroVPNEcmpSupport = lens _tgroVPNEcmpSupport (\s a -> s {_tgroVPNEcmpSupport = a})

-- | Enable or disable automatic acceptance of attachment requests. Disabled by default.
tgroAutoAcceptSharedAttachments :: Lens' TransitGatewayRequestOptions (Maybe AutoAcceptSharedAttachmentsValue)
tgroAutoAcceptSharedAttachments = lens _tgroAutoAcceptSharedAttachments (\s a -> s {_tgroAutoAcceptSharedAttachments = a})

-- | Enable or disable automatic association with the default association route table. Enabled by default.
tgroDefaultRouteTableAssociation :: Lens' TransitGatewayRequestOptions (Maybe DefaultRouteTableAssociationValue)
tgroDefaultRouteTableAssociation = lens _tgroDefaultRouteTableAssociation (\s a -> s {_tgroDefaultRouteTableAssociation = a})

-- | A private Autonomous System Number (ASN) for the Amazon side of a BGP session. The range is 64512 to 65534 for 16-bit ASNs and 4200000000 to 4294967294 for 32-bit ASNs. The default is @64512@ .
tgroAmazonSideASN :: Lens' TransitGatewayRequestOptions (Maybe Integer)
tgroAmazonSideASN = lens _tgroAmazonSideASN (\s a -> s {_tgroAmazonSideASN = a})

-- | Enable or disable automatic propagation of routes to the default propagation route table. Enabled by default.
tgroDefaultRouteTablePropagation :: Lens' TransitGatewayRequestOptions (Maybe DefaultRouteTablePropagationValue)
tgroDefaultRouteTablePropagation = lens _tgroDefaultRouteTablePropagation (\s a -> s {_tgroDefaultRouteTablePropagation = a})

-- | Indicates whether multicast is enabled on the transit gateway
tgroMulticastSupport :: Lens' TransitGatewayRequestOptions (Maybe MulticastSupportValue)
tgroMulticastSupport = lens _tgroMulticastSupport (\s a -> s {_tgroMulticastSupport = a})

-- | Enable or disable DNS support. Enabled by default.
tgroDNSSupport :: Lens' TransitGatewayRequestOptions (Maybe DNSSupportValue)
tgroDNSSupport = lens _tgroDNSSupport (\s a -> s {_tgroDNSSupport = a})

instance Hashable TransitGatewayRequestOptions

instance NFData TransitGatewayRequestOptions

instance ToQuery TransitGatewayRequestOptions where
  toQuery TransitGatewayRequestOptions' {..} =
    mconcat
      [ "VpnEcmpSupport" =: _tgroVPNEcmpSupport,
        "AutoAcceptSharedAttachments" =: _tgroAutoAcceptSharedAttachments,
        "DefaultRouteTableAssociation"
          =: _tgroDefaultRouteTableAssociation,
        "AmazonSideAsn" =: _tgroAmazonSideASN,
        "DefaultRouteTablePropagation"
          =: _tgroDefaultRouteTablePropagation,
        "MulticastSupport" =: _tgroMulticastSupport,
        "DnsSupport" =: _tgroDNSSupport
      ]
