{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.VPCPeeringConnection
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.VPCPeeringConnection where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.Tag
import Network.AWS.EC2.Types.VPCPeeringConnectionStateReason
import Network.AWS.EC2.Types.VPCPeeringConnectionVPCInfo
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes a VPC peering connection.
--
--
--
-- /See:/ 'vpcPeeringConnection' smart constructor.
data VPCPeeringConnection = VPCPeeringConnection'
  { _vpcpcVPCPeeringConnectionId ::
      !(Maybe Text),
    _vpcpcStatus ::
      !(Maybe VPCPeeringConnectionStateReason),
    _vpcpcAccepterVPCInfo ::
      !(Maybe VPCPeeringConnectionVPCInfo),
    _vpcpcRequesterVPCInfo ::
      !(Maybe VPCPeeringConnectionVPCInfo),
    _vpcpcExpirationTime :: !(Maybe ISO8601),
    _vpcpcTags :: !(Maybe [Tag])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'VPCPeeringConnection' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vpcpcVPCPeeringConnectionId' - The ID of the VPC peering connection.
--
-- * 'vpcpcStatus' - The status of the VPC peering connection.
--
-- * 'vpcpcAccepterVPCInfo' - Information about the accepter VPC. CIDR block information is only returned when describing an active VPC peering connection.
--
-- * 'vpcpcRequesterVPCInfo' - Information about the requester VPC. CIDR block information is only returned when describing an active VPC peering connection.
--
-- * 'vpcpcExpirationTime' - The time that an unaccepted VPC peering connection will expire.
--
-- * 'vpcpcTags' - Any tags assigned to the resource.
vpcPeeringConnection ::
  VPCPeeringConnection
vpcPeeringConnection =
  VPCPeeringConnection'
    { _vpcpcVPCPeeringConnectionId = Nothing,
      _vpcpcStatus = Nothing,
      _vpcpcAccepterVPCInfo = Nothing,
      _vpcpcRequesterVPCInfo = Nothing,
      _vpcpcExpirationTime = Nothing,
      _vpcpcTags = Nothing
    }

-- | The ID of the VPC peering connection.
vpcpcVPCPeeringConnectionId :: Lens' VPCPeeringConnection (Maybe Text)
vpcpcVPCPeeringConnectionId = lens _vpcpcVPCPeeringConnectionId (\s a -> s {_vpcpcVPCPeeringConnectionId = a})

-- | The status of the VPC peering connection.
vpcpcStatus :: Lens' VPCPeeringConnection (Maybe VPCPeeringConnectionStateReason)
vpcpcStatus = lens _vpcpcStatus (\s a -> s {_vpcpcStatus = a})

-- | Information about the accepter VPC. CIDR block information is only returned when describing an active VPC peering connection.
vpcpcAccepterVPCInfo :: Lens' VPCPeeringConnection (Maybe VPCPeeringConnectionVPCInfo)
vpcpcAccepterVPCInfo = lens _vpcpcAccepterVPCInfo (\s a -> s {_vpcpcAccepterVPCInfo = a})

-- | Information about the requester VPC. CIDR block information is only returned when describing an active VPC peering connection.
vpcpcRequesterVPCInfo :: Lens' VPCPeeringConnection (Maybe VPCPeeringConnectionVPCInfo)
vpcpcRequesterVPCInfo = lens _vpcpcRequesterVPCInfo (\s a -> s {_vpcpcRequesterVPCInfo = a})

-- | The time that an unaccepted VPC peering connection will expire.
vpcpcExpirationTime :: Lens' VPCPeeringConnection (Maybe UTCTime)
vpcpcExpirationTime = lens _vpcpcExpirationTime (\s a -> s {_vpcpcExpirationTime = a}) . mapping _Time

-- | Any tags assigned to the resource.
vpcpcTags :: Lens' VPCPeeringConnection [Tag]
vpcpcTags = lens _vpcpcTags (\s a -> s {_vpcpcTags = a}) . _Default . _Coerce

instance FromXML VPCPeeringConnection where
  parseXML x =
    VPCPeeringConnection'
      <$> (x .@? "vpcPeeringConnectionId")
      <*> (x .@? "status")
      <*> (x .@? "accepterVpcInfo")
      <*> (x .@? "requesterVpcInfo")
      <*> (x .@? "expirationTime")
      <*> (x .@? "tagSet" .!@ mempty >>= may (parseXMLList "item"))

instance Hashable VPCPeeringConnection

instance NFData VPCPeeringConnection
