{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.TransitGatewayMulticastDomain
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.TransitGatewayMulticastDomain where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.Tag
import Network.AWS.EC2.Types.TransitGatewayMulticastDomainState
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the transit gateway multicast domain.
--
--
--
-- /See:/ 'transitGatewayMulticastDomain' smart constructor.
data TransitGatewayMulticastDomain = TransitGatewayMulticastDomain'
  { _tgmdCreationTime ::
      !(Maybe ISO8601),
    _tgmdState ::
      !( Maybe
           TransitGatewayMulticastDomainState
       ),
    _tgmdTransitGatewayMulticastDomainId ::
      !(Maybe Text),
    _tgmdTransitGatewayId ::
      !(Maybe Text),
    _tgmdTags :: !(Maybe [Tag])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TransitGatewayMulticastDomain' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tgmdCreationTime' - The time the transit gateway multicast domain was created.
--
-- * 'tgmdState' - The state of the transit gateway multicast domain.
--
-- * 'tgmdTransitGatewayMulticastDomainId' - The ID of the transit gateway multicast domain.
--
-- * 'tgmdTransitGatewayId' - The ID of the transit gateway.
--
-- * 'tgmdTags' - The tags for the transit gateway multicast domain.
transitGatewayMulticastDomain ::
  TransitGatewayMulticastDomain
transitGatewayMulticastDomain =
  TransitGatewayMulticastDomain'
    { _tgmdCreationTime = Nothing,
      _tgmdState = Nothing,
      _tgmdTransitGatewayMulticastDomainId = Nothing,
      _tgmdTransitGatewayId = Nothing,
      _tgmdTags = Nothing
    }

-- | The time the transit gateway multicast domain was created.
tgmdCreationTime :: Lens' TransitGatewayMulticastDomain (Maybe UTCTime)
tgmdCreationTime = lens _tgmdCreationTime (\s a -> s {_tgmdCreationTime = a}) . mapping _Time

-- | The state of the transit gateway multicast domain.
tgmdState :: Lens' TransitGatewayMulticastDomain (Maybe TransitGatewayMulticastDomainState)
tgmdState = lens _tgmdState (\s a -> s {_tgmdState = a})

-- | The ID of the transit gateway multicast domain.
tgmdTransitGatewayMulticastDomainId :: Lens' TransitGatewayMulticastDomain (Maybe Text)
tgmdTransitGatewayMulticastDomainId = lens _tgmdTransitGatewayMulticastDomainId (\s a -> s {_tgmdTransitGatewayMulticastDomainId = a})

-- | The ID of the transit gateway.
tgmdTransitGatewayId :: Lens' TransitGatewayMulticastDomain (Maybe Text)
tgmdTransitGatewayId = lens _tgmdTransitGatewayId (\s a -> s {_tgmdTransitGatewayId = a})

-- | The tags for the transit gateway multicast domain.
tgmdTags :: Lens' TransitGatewayMulticastDomain [Tag]
tgmdTags = lens _tgmdTags (\s a -> s {_tgmdTags = a}) . _Default . _Coerce

instance FromXML TransitGatewayMulticastDomain where
  parseXML x =
    TransitGatewayMulticastDomain'
      <$> (x .@? "creationTime")
      <*> (x .@? "state")
      <*> (x .@? "transitGatewayMulticastDomainId")
      <*> (x .@? "transitGatewayId")
      <*> (x .@? "tagSet" .!@ mempty >>= may (parseXMLList "item"))

instance Hashable TransitGatewayMulticastDomain

instance NFData TransitGatewayMulticastDomain
