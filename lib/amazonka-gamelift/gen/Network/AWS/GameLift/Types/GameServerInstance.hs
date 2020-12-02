{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types.GameServerInstance
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GameLift.Types.GameServerInstance where

import Network.AWS.GameLift.Types.GameServerInstanceStatus
import Network.AWS.Lens
import Network.AWS.Prelude

-- | __This data type is used with the Amazon GameLift FleetIQ and game server groups.__
--
--
-- Additional properties, including status, that describe an EC2 instance in a game server group. Instance configurations are set with game server group properties (see @DescribeGameServerGroup@ and with the EC2 launch template that was used when creating the game server group.
--
-- Retrieve game server instances for a game server group by calling @DescribeGameServerInstances@ .
--
--     * 'CreateGameServerGroup'
--
--     * 'ListGameServerGroups'
--
--     * 'DescribeGameServerGroup'
--
--     * 'UpdateGameServerGroup'
--
--     * 'DeleteGameServerGroup'
--
--     * 'ResumeGameServerGroup'
--
--     * 'SuspendGameServerGroup'
--
--     * 'DescribeGameServerInstances'
--
--
--
--
-- /See:/ 'gameServerInstance' smart constructor.
data GameServerInstance = GameServerInstance'
  { _gsiInstanceId ::
      !(Maybe Text),
    _gsiGameServerGroupName :: !(Maybe Text),
    _gsiInstanceStatus ::
      !(Maybe GameServerInstanceStatus),
    _gsiGameServerGroupARN :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GameServerInstance' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gsiInstanceId' - The unique identifier for the instance where the game server is running. This ID is available in the instance metadata. EC2 instance IDs use a 17-character format, for example: @i-1234567890abcdef0@ .
--
-- * 'gsiGameServerGroupName' - A developer-defined identifier for the game server group that includes the game server instance. The name is unique for each Region in each AWS account.
--
-- * 'gsiInstanceStatus' - Current status of the game server instance.      * __ACTIVE__ -- The instance is viable for hosting game servers.      * __DRAINING__ -- The instance is not viable for hosting game servers. Existing game servers are in the process of ending, and new game servers are not started on this instance unless no other resources are available. When the instance is put in DRAINING, a new instance is started up to replace it. Once the instance has no UTILIZED game servers, it will be terminated in favor of the new instance.     * __SPOT_TERMINATING__ -- The instance is in the process of shutting down due to a Spot instance interruption. No new game servers are started on this instance.
--
-- * 'gsiGameServerGroupARN' - A generated unique identifier for the game server group that includes the game server instance.
gameServerInstance ::
  GameServerInstance
gameServerInstance =
  GameServerInstance'
    { _gsiInstanceId = Nothing,
      _gsiGameServerGroupName = Nothing,
      _gsiInstanceStatus = Nothing,
      _gsiGameServerGroupARN = Nothing
    }

-- | The unique identifier for the instance where the game server is running. This ID is available in the instance metadata. EC2 instance IDs use a 17-character format, for example: @i-1234567890abcdef0@ .
gsiInstanceId :: Lens' GameServerInstance (Maybe Text)
gsiInstanceId = lens _gsiInstanceId (\s a -> s {_gsiInstanceId = a})

-- | A developer-defined identifier for the game server group that includes the game server instance. The name is unique for each Region in each AWS account.
gsiGameServerGroupName :: Lens' GameServerInstance (Maybe Text)
gsiGameServerGroupName = lens _gsiGameServerGroupName (\s a -> s {_gsiGameServerGroupName = a})

-- | Current status of the game server instance.      * __ACTIVE__ -- The instance is viable for hosting game servers.      * __DRAINING__ -- The instance is not viable for hosting game servers. Existing game servers are in the process of ending, and new game servers are not started on this instance unless no other resources are available. When the instance is put in DRAINING, a new instance is started up to replace it. Once the instance has no UTILIZED game servers, it will be terminated in favor of the new instance.     * __SPOT_TERMINATING__ -- The instance is in the process of shutting down due to a Spot instance interruption. No new game servers are started on this instance.
gsiInstanceStatus :: Lens' GameServerInstance (Maybe GameServerInstanceStatus)
gsiInstanceStatus = lens _gsiInstanceStatus (\s a -> s {_gsiInstanceStatus = a})

-- | A generated unique identifier for the game server group that includes the game server instance.
gsiGameServerGroupARN :: Lens' GameServerInstance (Maybe Text)
gsiGameServerGroupARN = lens _gsiGameServerGroupARN (\s a -> s {_gsiGameServerGroupARN = a})

instance FromJSON GameServerInstance where
  parseJSON =
    withObject
      "GameServerInstance"
      ( \x ->
          GameServerInstance'
            <$> (x .:? "InstanceId")
            <*> (x .:? "GameServerGroupName")
            <*> (x .:? "InstanceStatus")
            <*> (x .:? "GameServerGroupArn")
      )

instance Hashable GameServerInstance

instance NFData GameServerInstance
