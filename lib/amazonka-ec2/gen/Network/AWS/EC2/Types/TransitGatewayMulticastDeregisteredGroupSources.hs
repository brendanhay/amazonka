{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.TransitGatewayMulticastDeregisteredGroupSources
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.TransitGatewayMulticastDeregisteredGroupSources where

import Network.AWS.EC2.Internal
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the deregistered transit gateway multicast group sources.
--
--
--
-- /See:/ 'transitGatewayMulticastDeregisteredGroupSources' smart constructor.
data TransitGatewayMulticastDeregisteredGroupSources = TransitGatewayMulticastDeregisteredGroupSources'
  { _tgmdgsDeregisteredNetworkInterfaceIds ::
      !( Maybe
           [Text]
       ),
    _tgmdgsTransitGatewayMulticastDomainId ::
      !( Maybe
           Text
       ),
    _tgmdgsGroupIPAddress ::
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

-- | Creates a value of 'TransitGatewayMulticastDeregisteredGroupSources' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tgmdgsDeregisteredNetworkInterfaceIds' - The network interface IDs of the non-registered members.
--
-- * 'tgmdgsTransitGatewayMulticastDomainId' - The ID of the transit gateway multicast domain.
--
-- * 'tgmdgsGroupIPAddress' - The IP address assigned to the transit gateway multicast group.
transitGatewayMulticastDeregisteredGroupSources ::
  TransitGatewayMulticastDeregisteredGroupSources
transitGatewayMulticastDeregisteredGroupSources =
  TransitGatewayMulticastDeregisteredGroupSources'
    { _tgmdgsDeregisteredNetworkInterfaceIds =
        Nothing,
      _tgmdgsTransitGatewayMulticastDomainId =
        Nothing,
      _tgmdgsGroupIPAddress = Nothing
    }

-- | The network interface IDs of the non-registered members.
tgmdgsDeregisteredNetworkInterfaceIds :: Lens' TransitGatewayMulticastDeregisteredGroupSources [Text]
tgmdgsDeregisteredNetworkInterfaceIds = lens _tgmdgsDeregisteredNetworkInterfaceIds (\s a -> s {_tgmdgsDeregisteredNetworkInterfaceIds = a}) . _Default . _Coerce

-- | The ID of the transit gateway multicast domain.
tgmdgsTransitGatewayMulticastDomainId :: Lens' TransitGatewayMulticastDeregisteredGroupSources (Maybe Text)
tgmdgsTransitGatewayMulticastDomainId = lens _tgmdgsTransitGatewayMulticastDomainId (\s a -> s {_tgmdgsTransitGatewayMulticastDomainId = a})

-- | The IP address assigned to the transit gateway multicast group.
tgmdgsGroupIPAddress :: Lens' TransitGatewayMulticastDeregisteredGroupSources (Maybe Text)
tgmdgsGroupIPAddress = lens _tgmdgsGroupIPAddress (\s a -> s {_tgmdgsGroupIPAddress = a})

instance FromXML TransitGatewayMulticastDeregisteredGroupSources where
  parseXML x =
    TransitGatewayMulticastDeregisteredGroupSources'
      <$> ( x .@? "deregisteredNetworkInterfaceIds" .!@ mempty
              >>= may (parseXMLList "item")
          )
      <*> (x .@? "transitGatewayMulticastDomainId")
      <*> (x .@? "groupIpAddress")

instance Hashable TransitGatewayMulticastDeregisteredGroupSources

instance NFData TransitGatewayMulticastDeregisteredGroupSources
