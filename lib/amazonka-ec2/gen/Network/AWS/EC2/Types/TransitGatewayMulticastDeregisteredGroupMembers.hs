{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.TransitGatewayMulticastDeregisteredGroupMembers
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.TransitGatewayMulticastDeregisteredGroupMembers where

import Network.AWS.EC2.Internal
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the deregistered transit gateway multicast group members.
--
--
--
-- /See:/ 'transitGatewayMulticastDeregisteredGroupMembers' smart constructor.
data TransitGatewayMulticastDeregisteredGroupMembers = TransitGatewayMulticastDeregisteredGroupMembers'
  { _tgmdgmDeregisteredNetworkInterfaceIds ::
      !( Maybe
           [Text]
       ),
    _tgmdgmTransitGatewayMulticastDomainId ::
      !( Maybe
           Text
       ),
    _tgmdgmGroupIPAddress ::
      !( Maybe
           Text
       )
  }
  deriving
    ( Eq,
      Read,
      Show,
      Data,
      Typeable,
      Generic
    )

-- | Creates a value of 'TransitGatewayMulticastDeregisteredGroupMembers' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tgmdgmDeregisteredNetworkInterfaceIds' - The network interface IDs of the deregistered members.
--
-- * 'tgmdgmTransitGatewayMulticastDomainId' - The ID of the transit gateway multicast domain.
--
-- * 'tgmdgmGroupIPAddress' - The IP address assigned to the transit gateway multicast group.
transitGatewayMulticastDeregisteredGroupMembers ::
  TransitGatewayMulticastDeregisteredGroupMembers
transitGatewayMulticastDeregisteredGroupMembers =
  TransitGatewayMulticastDeregisteredGroupMembers'
    { _tgmdgmDeregisteredNetworkInterfaceIds =
        Nothing,
      _tgmdgmTransitGatewayMulticastDomainId =
        Nothing,
      _tgmdgmGroupIPAddress = Nothing
    }

-- | The network interface IDs of the deregistered members.
tgmdgmDeregisteredNetworkInterfaceIds :: Lens' TransitGatewayMulticastDeregisteredGroupMembers [Text]
tgmdgmDeregisteredNetworkInterfaceIds = lens _tgmdgmDeregisteredNetworkInterfaceIds (\s a -> s {_tgmdgmDeregisteredNetworkInterfaceIds = a}) . _Default . _Coerce

-- | The ID of the transit gateway multicast domain.
tgmdgmTransitGatewayMulticastDomainId :: Lens' TransitGatewayMulticastDeregisteredGroupMembers (Maybe Text)
tgmdgmTransitGatewayMulticastDomainId = lens _tgmdgmTransitGatewayMulticastDomainId (\s a -> s {_tgmdgmTransitGatewayMulticastDomainId = a})

-- | The IP address assigned to the transit gateway multicast group.
tgmdgmGroupIPAddress :: Lens' TransitGatewayMulticastDeregisteredGroupMembers (Maybe Text)
tgmdgmGroupIPAddress = lens _tgmdgmGroupIPAddress (\s a -> s {_tgmdgmGroupIPAddress = a})

instance FromXML TransitGatewayMulticastDeregisteredGroupMembers where
  parseXML x =
    TransitGatewayMulticastDeregisteredGroupMembers'
      <$> ( x .@? "deregisteredNetworkInterfaceIds" .!@ mempty
              >>= may (parseXMLList "item")
          )
      <*> (x .@? "transitGatewayMulticastDomainId")
      <*> (x .@? "groupIpAddress")

instance Hashable TransitGatewayMulticastDeregisteredGroupMembers

instance NFData TransitGatewayMulticastDeregisteredGroupMembers
