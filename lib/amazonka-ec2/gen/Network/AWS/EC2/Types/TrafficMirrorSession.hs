{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.TrafficMirrorSession
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.TrafficMirrorSession where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.Tag
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes a Traffic Mirror session.
--
--
--
-- /See:/ 'trafficMirrorSession' smart constructor.
data TrafficMirrorSession = TrafficMirrorSession'
  { _tmsTrafficMirrorTargetId ::
      !(Maybe Text),
    _tmsNetworkInterfaceId :: !(Maybe Text),
    _tmsTrafficMirrorFilterId :: !(Maybe Text),
    _tmsPacketLength :: !(Maybe Int),
    _tmsOwnerId :: !(Maybe Text),
    _tmsTrafficMirrorSessionId :: !(Maybe Text),
    _tmsVirtualNetworkId :: !(Maybe Int),
    _tmsSessionNumber :: !(Maybe Int),
    _tmsDescription :: !(Maybe Text),
    _tmsTags :: !(Maybe [Tag])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TrafficMirrorSession' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tmsTrafficMirrorTargetId' - The ID of the Traffic Mirror target.
--
-- * 'tmsNetworkInterfaceId' - The ID of the Traffic Mirror session's network interface.
--
-- * 'tmsTrafficMirrorFilterId' - The ID of the Traffic Mirror filter.
--
-- * 'tmsPacketLength' - The number of bytes in each packet to mirror. These are the bytes after the VXLAN header. To mirror a subset, set this to the length (in bytes) to mirror. For example, if you set this value to 100, then the first 100 bytes that meet the filter criteria are copied to the target. Do not specify this parameter when you want to mirror the entire packet
--
-- * 'tmsOwnerId' - The ID of the account that owns the Traffic Mirror session.
--
-- * 'tmsTrafficMirrorSessionId' - The ID for the Traffic Mirror session.
--
-- * 'tmsVirtualNetworkId' - The virtual network ID associated with the Traffic Mirror session.
--
-- * 'tmsSessionNumber' - The session number determines the order in which sessions are evaluated when an interface is used by multiple sessions. The first session with a matching filter is the one that mirrors the packets. Valid values are 1-32766.
--
-- * 'tmsDescription' - The description of the Traffic Mirror session.
--
-- * 'tmsTags' - The tags assigned to the Traffic Mirror session.
trafficMirrorSession ::
  TrafficMirrorSession
trafficMirrorSession =
  TrafficMirrorSession'
    { _tmsTrafficMirrorTargetId = Nothing,
      _tmsNetworkInterfaceId = Nothing,
      _tmsTrafficMirrorFilterId = Nothing,
      _tmsPacketLength = Nothing,
      _tmsOwnerId = Nothing,
      _tmsTrafficMirrorSessionId = Nothing,
      _tmsVirtualNetworkId = Nothing,
      _tmsSessionNumber = Nothing,
      _tmsDescription = Nothing,
      _tmsTags = Nothing
    }

-- | The ID of the Traffic Mirror target.
tmsTrafficMirrorTargetId :: Lens' TrafficMirrorSession (Maybe Text)
tmsTrafficMirrorTargetId = lens _tmsTrafficMirrorTargetId (\s a -> s {_tmsTrafficMirrorTargetId = a})

-- | The ID of the Traffic Mirror session's network interface.
tmsNetworkInterfaceId :: Lens' TrafficMirrorSession (Maybe Text)
tmsNetworkInterfaceId = lens _tmsNetworkInterfaceId (\s a -> s {_tmsNetworkInterfaceId = a})

-- | The ID of the Traffic Mirror filter.
tmsTrafficMirrorFilterId :: Lens' TrafficMirrorSession (Maybe Text)
tmsTrafficMirrorFilterId = lens _tmsTrafficMirrorFilterId (\s a -> s {_tmsTrafficMirrorFilterId = a})

-- | The number of bytes in each packet to mirror. These are the bytes after the VXLAN header. To mirror a subset, set this to the length (in bytes) to mirror. For example, if you set this value to 100, then the first 100 bytes that meet the filter criteria are copied to the target. Do not specify this parameter when you want to mirror the entire packet
tmsPacketLength :: Lens' TrafficMirrorSession (Maybe Int)
tmsPacketLength = lens _tmsPacketLength (\s a -> s {_tmsPacketLength = a})

-- | The ID of the account that owns the Traffic Mirror session.
tmsOwnerId :: Lens' TrafficMirrorSession (Maybe Text)
tmsOwnerId = lens _tmsOwnerId (\s a -> s {_tmsOwnerId = a})

-- | The ID for the Traffic Mirror session.
tmsTrafficMirrorSessionId :: Lens' TrafficMirrorSession (Maybe Text)
tmsTrafficMirrorSessionId = lens _tmsTrafficMirrorSessionId (\s a -> s {_tmsTrafficMirrorSessionId = a})

-- | The virtual network ID associated with the Traffic Mirror session.
tmsVirtualNetworkId :: Lens' TrafficMirrorSession (Maybe Int)
tmsVirtualNetworkId = lens _tmsVirtualNetworkId (\s a -> s {_tmsVirtualNetworkId = a})

-- | The session number determines the order in which sessions are evaluated when an interface is used by multiple sessions. The first session with a matching filter is the one that mirrors the packets. Valid values are 1-32766.
tmsSessionNumber :: Lens' TrafficMirrorSession (Maybe Int)
tmsSessionNumber = lens _tmsSessionNumber (\s a -> s {_tmsSessionNumber = a})

-- | The description of the Traffic Mirror session.
tmsDescription :: Lens' TrafficMirrorSession (Maybe Text)
tmsDescription = lens _tmsDescription (\s a -> s {_tmsDescription = a})

-- | The tags assigned to the Traffic Mirror session.
tmsTags :: Lens' TrafficMirrorSession [Tag]
tmsTags = lens _tmsTags (\s a -> s {_tmsTags = a}) . _Default . _Coerce

instance FromXML TrafficMirrorSession where
  parseXML x =
    TrafficMirrorSession'
      <$> (x .@? "trafficMirrorTargetId")
      <*> (x .@? "networkInterfaceId")
      <*> (x .@? "trafficMirrorFilterId")
      <*> (x .@? "packetLength")
      <*> (x .@? "ownerId")
      <*> (x .@? "trafficMirrorSessionId")
      <*> (x .@? "virtualNetworkId")
      <*> (x .@? "sessionNumber")
      <*> (x .@? "description")
      <*> (x .@? "tagSet" .!@ mempty >>= may (parseXMLList "item"))

instance Hashable TrafficMirrorSession

instance NFData TrafficMirrorSession
