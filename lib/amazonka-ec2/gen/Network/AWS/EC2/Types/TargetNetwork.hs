{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.TargetNetwork
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.TargetNetwork where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.AssociationStatus
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes a target network associated with a Client VPN endpoint.
--
--
--
-- /See:/ 'targetNetwork' smart constructor.
data TargetNetwork = TargetNetwork'
  { _tnAssociationId ::
      !(Maybe Text),
    _tnStatus :: !(Maybe AssociationStatus),
    _tnSecurityGroups :: !(Maybe [Text]),
    _tnTargetNetworkId :: !(Maybe Text),
    _tnVPCId :: !(Maybe Text),
    _tnClientVPNEndpointId :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TargetNetwork' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tnAssociationId' - The ID of the association.
--
-- * 'tnStatus' - The current state of the target network association.
--
-- * 'tnSecurityGroups' - The IDs of the security groups applied to the target network association.
--
-- * 'tnTargetNetworkId' - The ID of the subnet specified as the target network.
--
-- * 'tnVPCId' - The ID of the VPC in which the target network (subnet) is located.
--
-- * 'tnClientVPNEndpointId' - The ID of the Client VPN endpoint with which the target network is associated.
targetNetwork ::
  TargetNetwork
targetNetwork =
  TargetNetwork'
    { _tnAssociationId = Nothing,
      _tnStatus = Nothing,
      _tnSecurityGroups = Nothing,
      _tnTargetNetworkId = Nothing,
      _tnVPCId = Nothing,
      _tnClientVPNEndpointId = Nothing
    }

-- | The ID of the association.
tnAssociationId :: Lens' TargetNetwork (Maybe Text)
tnAssociationId = lens _tnAssociationId (\s a -> s {_tnAssociationId = a})

-- | The current state of the target network association.
tnStatus :: Lens' TargetNetwork (Maybe AssociationStatus)
tnStatus = lens _tnStatus (\s a -> s {_tnStatus = a})

-- | The IDs of the security groups applied to the target network association.
tnSecurityGroups :: Lens' TargetNetwork [Text]
tnSecurityGroups = lens _tnSecurityGroups (\s a -> s {_tnSecurityGroups = a}) . _Default . _Coerce

-- | The ID of the subnet specified as the target network.
tnTargetNetworkId :: Lens' TargetNetwork (Maybe Text)
tnTargetNetworkId = lens _tnTargetNetworkId (\s a -> s {_tnTargetNetworkId = a})

-- | The ID of the VPC in which the target network (subnet) is located.
tnVPCId :: Lens' TargetNetwork (Maybe Text)
tnVPCId = lens _tnVPCId (\s a -> s {_tnVPCId = a})

-- | The ID of the Client VPN endpoint with which the target network is associated.
tnClientVPNEndpointId :: Lens' TargetNetwork (Maybe Text)
tnClientVPNEndpointId = lens _tnClientVPNEndpointId (\s a -> s {_tnClientVPNEndpointId = a})

instance FromXML TargetNetwork where
  parseXML x =
    TargetNetwork'
      <$> (x .@? "associationId")
      <*> (x .@? "status")
      <*> (x .@? "securityGroups" .!@ mempty >>= may (parseXMLList "item"))
      <*> (x .@? "targetNetworkId")
      <*> (x .@? "vpcId")
      <*> (x .@? "clientVpnEndpointId")

instance Hashable TargetNetwork

instance NFData TargetNetwork
