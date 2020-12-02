{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types.VPCPeeringConnectionStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GameLift.Types.VPCPeeringConnectionStatus where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents status information for a VPC peering connection. Status is associated with a 'VpcPeeringConnection' object. Status codes and messages are provided from EC2 (see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_VpcPeeringConnectionStateReason.html VpcPeeringConnectionStateReason> ). Connection status information is also communicated as a fleet 'Event' .
--
--
--
-- /See:/ 'vpcPeeringConnectionStatus' smart constructor.
data VPCPeeringConnectionStatus = VPCPeeringConnectionStatus'
  { _vpcsCode ::
      !(Maybe Text),
    _vpcsMessage :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'VPCPeeringConnectionStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vpcsCode' - Code indicating the status of a VPC peering connection.
--
-- * 'vpcsMessage' - Additional messaging associated with the connection status.
vpcPeeringConnectionStatus ::
  VPCPeeringConnectionStatus
vpcPeeringConnectionStatus =
  VPCPeeringConnectionStatus'
    { _vpcsCode = Nothing,
      _vpcsMessage = Nothing
    }

-- | Code indicating the status of a VPC peering connection.
vpcsCode :: Lens' VPCPeeringConnectionStatus (Maybe Text)
vpcsCode = lens _vpcsCode (\s a -> s {_vpcsCode = a})

-- | Additional messaging associated with the connection status.
vpcsMessage :: Lens' VPCPeeringConnectionStatus (Maybe Text)
vpcsMessage = lens _vpcsMessage (\s a -> s {_vpcsMessage = a})

instance FromJSON VPCPeeringConnectionStatus where
  parseJSON =
    withObject
      "VPCPeeringConnectionStatus"
      ( \x ->
          VPCPeeringConnectionStatus'
            <$> (x .:? "Code") <*> (x .:? "Message")
      )

instance Hashable VPCPeeringConnectionStatus

instance NFData VPCPeeringConnectionStatus
