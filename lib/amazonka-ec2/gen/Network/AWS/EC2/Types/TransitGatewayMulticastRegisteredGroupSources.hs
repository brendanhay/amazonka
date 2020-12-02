{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.TransitGatewayMulticastRegisteredGroupSources
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.TransitGatewayMulticastRegisteredGroupSources where

import Network.AWS.EC2.Internal
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the members registered with the transit gateway multicast group.
--
--
--
-- /See:/ 'transitGatewayMulticastRegisteredGroupSources' smart constructor.
data TransitGatewayMulticastRegisteredGroupSources = TransitGatewayMulticastRegisteredGroupSources'
  { _tgmrgsTransitGatewayMulticastDomainId ::
      !( Maybe
           Text
       ),
    _tgmrgsRegisteredNetworkInterfaceIds ::
      !( Maybe
           [Text]
       ),
    _tgmrgsGroupIPAddress ::
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

-- | Creates a value of 'TransitGatewayMulticastRegisteredGroupSources' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tgmrgsTransitGatewayMulticastDomainId' - The ID of the transit gateway multicast domain.
--
-- * 'tgmrgsRegisteredNetworkInterfaceIds' - The IDs of the network interfaces members registered with the transit gateway multicast group.
--
-- * 'tgmrgsGroupIPAddress' - The IP address assigned to the transit gateway multicast group.
transitGatewayMulticastRegisteredGroupSources ::
  TransitGatewayMulticastRegisteredGroupSources
transitGatewayMulticastRegisteredGroupSources =
  TransitGatewayMulticastRegisteredGroupSources'
    { _tgmrgsTransitGatewayMulticastDomainId =
        Nothing,
      _tgmrgsRegisteredNetworkInterfaceIds = Nothing,
      _tgmrgsGroupIPAddress = Nothing
    }

-- | The ID of the transit gateway multicast domain.
tgmrgsTransitGatewayMulticastDomainId :: Lens' TransitGatewayMulticastRegisteredGroupSources (Maybe Text)
tgmrgsTransitGatewayMulticastDomainId = lens _tgmrgsTransitGatewayMulticastDomainId (\s a -> s {_tgmrgsTransitGatewayMulticastDomainId = a})

-- | The IDs of the network interfaces members registered with the transit gateway multicast group.
tgmrgsRegisteredNetworkInterfaceIds :: Lens' TransitGatewayMulticastRegisteredGroupSources [Text]
tgmrgsRegisteredNetworkInterfaceIds = lens _tgmrgsRegisteredNetworkInterfaceIds (\s a -> s {_tgmrgsRegisteredNetworkInterfaceIds = a}) . _Default . _Coerce

-- | The IP address assigned to the transit gateway multicast group.
tgmrgsGroupIPAddress :: Lens' TransitGatewayMulticastRegisteredGroupSources (Maybe Text)
tgmrgsGroupIPAddress = lens _tgmrgsGroupIPAddress (\s a -> s {_tgmrgsGroupIPAddress = a})

instance FromXML TransitGatewayMulticastRegisteredGroupSources where
  parseXML x =
    TransitGatewayMulticastRegisteredGroupSources'
      <$> (x .@? "transitGatewayMulticastDomainId")
      <*> ( x .@? "registeredNetworkInterfaceIds" .!@ mempty
              >>= may (parseXMLList "item")
          )
      <*> (x .@? "groupIpAddress")

instance Hashable TransitGatewayMulticastRegisteredGroupSources

instance NFData TransitGatewayMulticastRegisteredGroupSources
