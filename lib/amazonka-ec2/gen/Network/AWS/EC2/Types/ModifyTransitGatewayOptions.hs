{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ModifyTransitGatewayOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ModifyTransitGatewayOptions where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.AutoAcceptSharedAttachmentsValue
import Network.AWS.EC2.Types.DNSSupportValue
import Network.AWS.EC2.Types.DefaultRouteTableAssociationValue
import Network.AWS.EC2.Types.DefaultRouteTablePropagationValue
import Network.AWS.EC2.Types.VPNEcmpSupportValue
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The transit gateway options.
--
--
--
-- /See:/ 'modifyTransitGatewayOptions' smart constructor.
data ModifyTransitGatewayOptions = ModifyTransitGatewayOptions'
  { _mtgoVPNEcmpSupport ::
      !(Maybe VPNEcmpSupportValue),
    _mtgoAutoAcceptSharedAttachments ::
      !( Maybe
           AutoAcceptSharedAttachmentsValue
       ),
    _mtgoPropagationDefaultRouteTableId ::
      !(Maybe Text),
    _mtgoDefaultRouteTableAssociation ::
      !( Maybe
           DefaultRouteTableAssociationValue
       ),
    _mtgoAssociationDefaultRouteTableId ::
      !(Maybe Text),
    _mtgoDefaultRouteTablePropagation ::
      !( Maybe
           DefaultRouteTablePropagationValue
       ),
    _mtgoDNSSupport ::
      !(Maybe DNSSupportValue)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ModifyTransitGatewayOptions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mtgoVPNEcmpSupport' - Enable or disable Equal Cost Multipath Protocol support.
--
-- * 'mtgoAutoAcceptSharedAttachments' - Enable or disable automatic acceptance of attachment requests.
--
-- * 'mtgoPropagationDefaultRouteTableId' - The ID of the default propagation route table.
--
-- * 'mtgoDefaultRouteTableAssociation' - Enable or disable automatic association with the default association route table.
--
-- * 'mtgoAssociationDefaultRouteTableId' - The ID of the default association route table.
--
-- * 'mtgoDefaultRouteTablePropagation' - Enable or disable automatic propagation of routes to the default propagation route table.
--
-- * 'mtgoDNSSupport' - Enable or disable DNS support.
modifyTransitGatewayOptions ::
  ModifyTransitGatewayOptions
modifyTransitGatewayOptions =
  ModifyTransitGatewayOptions'
    { _mtgoVPNEcmpSupport = Nothing,
      _mtgoAutoAcceptSharedAttachments = Nothing,
      _mtgoPropagationDefaultRouteTableId = Nothing,
      _mtgoDefaultRouteTableAssociation = Nothing,
      _mtgoAssociationDefaultRouteTableId = Nothing,
      _mtgoDefaultRouteTablePropagation = Nothing,
      _mtgoDNSSupport = Nothing
    }

-- | Enable or disable Equal Cost Multipath Protocol support.
mtgoVPNEcmpSupport :: Lens' ModifyTransitGatewayOptions (Maybe VPNEcmpSupportValue)
mtgoVPNEcmpSupport = lens _mtgoVPNEcmpSupport (\s a -> s {_mtgoVPNEcmpSupport = a})

-- | Enable or disable automatic acceptance of attachment requests.
mtgoAutoAcceptSharedAttachments :: Lens' ModifyTransitGatewayOptions (Maybe AutoAcceptSharedAttachmentsValue)
mtgoAutoAcceptSharedAttachments = lens _mtgoAutoAcceptSharedAttachments (\s a -> s {_mtgoAutoAcceptSharedAttachments = a})

-- | The ID of the default propagation route table.
mtgoPropagationDefaultRouteTableId :: Lens' ModifyTransitGatewayOptions (Maybe Text)
mtgoPropagationDefaultRouteTableId = lens _mtgoPropagationDefaultRouteTableId (\s a -> s {_mtgoPropagationDefaultRouteTableId = a})

-- | Enable or disable automatic association with the default association route table.
mtgoDefaultRouteTableAssociation :: Lens' ModifyTransitGatewayOptions (Maybe DefaultRouteTableAssociationValue)
mtgoDefaultRouteTableAssociation = lens _mtgoDefaultRouteTableAssociation (\s a -> s {_mtgoDefaultRouteTableAssociation = a})

-- | The ID of the default association route table.
mtgoAssociationDefaultRouteTableId :: Lens' ModifyTransitGatewayOptions (Maybe Text)
mtgoAssociationDefaultRouteTableId = lens _mtgoAssociationDefaultRouteTableId (\s a -> s {_mtgoAssociationDefaultRouteTableId = a})

-- | Enable or disable automatic propagation of routes to the default propagation route table.
mtgoDefaultRouteTablePropagation :: Lens' ModifyTransitGatewayOptions (Maybe DefaultRouteTablePropagationValue)
mtgoDefaultRouteTablePropagation = lens _mtgoDefaultRouteTablePropagation (\s a -> s {_mtgoDefaultRouteTablePropagation = a})

-- | Enable or disable DNS support.
mtgoDNSSupport :: Lens' ModifyTransitGatewayOptions (Maybe DNSSupportValue)
mtgoDNSSupport = lens _mtgoDNSSupport (\s a -> s {_mtgoDNSSupport = a})

instance Hashable ModifyTransitGatewayOptions

instance NFData ModifyTransitGatewayOptions

instance ToQuery ModifyTransitGatewayOptions where
  toQuery ModifyTransitGatewayOptions' {..} =
    mconcat
      [ "VpnEcmpSupport" =: _mtgoVPNEcmpSupport,
        "AutoAcceptSharedAttachments" =: _mtgoAutoAcceptSharedAttachments,
        "PropagationDefaultRouteTableId"
          =: _mtgoPropagationDefaultRouteTableId,
        "DefaultRouteTableAssociation"
          =: _mtgoDefaultRouteTableAssociation,
        "AssociationDefaultRouteTableId"
          =: _mtgoAssociationDefaultRouteTableId,
        "DefaultRouteTablePropagation"
          =: _mtgoDefaultRouteTablePropagation,
        "DnsSupport" =: _mtgoDNSSupport
      ]
