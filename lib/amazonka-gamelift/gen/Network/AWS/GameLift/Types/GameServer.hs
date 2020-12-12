{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types.GameServer
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GameLift.Types.GameServer
  ( GameServer (..),

    -- * Smart constructor
    mkGameServer,

    -- * Lenses
    gsInstanceId,
    gsLastClaimTime,
    gsGameServerGroupName,
    gsGameServerData,
    gsClaimStatus,
    gsGameServerId,
    gsUtilizationStatus,
    gsRegistrationTime,
    gsLastHealthCheckTime,
    gsConnectionInfo,
    gsGameServerGroupARN,
  )
where

import Network.AWS.GameLift.Types.GameServerClaimStatus
import Network.AWS.GameLift.Types.GameServerUtilizationStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | __This data type is used with the Amazon GameLift FleetIQ and game server groups.__
--
-- Properties describing a game server that is running on an instance in a 'GameServerGroup' .
-- A game server is created by a successful call to @RegisterGameServer@ and deleted by calling @DeregisterGameServer@ . A game server is claimed to host a game session by calling @ClaimGameServer@ .
--
--     * 'RegisterGameServer'
--
--
--     * 'ListGameServers'
--
--
--     * 'ClaimGameServer'
--
--
--     * 'DescribeGameServer'
--
--
--     * 'UpdateGameServer'
--
--
--     * 'DeregisterGameServer'
--
--
--
-- /See:/ 'mkGameServer' smart constructor.
data GameServer = GameServer'
  { instanceId :: Lude.Maybe Lude.Text,
    lastClaimTime :: Lude.Maybe Lude.Timestamp,
    gameServerGroupName :: Lude.Maybe Lude.Text,
    gameServerData :: Lude.Maybe Lude.Text,
    claimStatus :: Lude.Maybe GameServerClaimStatus,
    gameServerId :: Lude.Maybe Lude.Text,
    utilizationStatus :: Lude.Maybe GameServerUtilizationStatus,
    registrationTime :: Lude.Maybe Lude.Timestamp,
    lastHealthCheckTime :: Lude.Maybe Lude.Timestamp,
    connectionInfo :: Lude.Maybe Lude.Text,
    gameServerGroupARN :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GameServer' with the minimum fields required to make a request.
--
-- * 'claimStatus' - Indicates when an available game server has been reserved for gameplay but has not yet started hosting a game. Once it is claimed, the game server remains in @CLAIMED@ status for a maximum of one minute. During this time, game clients connect to the game server to start the game and trigger the game server to update its utilization status. After one minute, the game server claim status reverts to null.
-- * 'connectionInfo' - The port and IP address that must be used to establish a client connection to the game server.
-- * 'gameServerData' - A set of custom game server properties, formatted as a single string value. This data is passed to a game client or service when it requests information on game servers using 'ListGameServers' or 'ClaimGameServer' .
-- * 'gameServerGroupARN' - The ARN identifier for the game server group where the game server is located.
-- * 'gameServerGroupName' - A unique identifier for the game server group where the game server is running. Use either the 'GameServerGroup' name or ARN value.
-- * 'gameServerId' - A custom string that uniquely identifies the game server. Game server IDs are developer-defined and are unique across all game server groups in an AWS account.
-- * 'instanceId' - The unique identifier for the instance where the game server is running. This ID is available in the instance metadata. EC2 instance IDs use a 17-character format, for example: @i-1234567890abcdef0@ .
-- * 'lastClaimTime' - Timestamp that indicates the last time the game server was claimed with a 'ClaimGameServer' request. The format is a number expressed in Unix time as milliseconds (for example @"1469498468.057"@ ). This value is used to calculate when a claimed game server's status should revert to null.
-- * 'lastHealthCheckTime' - Timestamp that indicates the last time the game server was updated with health status using an 'UpdateGameServer' request. The format is a number expressed in Unix time as milliseconds (for example @"1469498468.057"@ ). After game server registration, this property is only changed when a game server update specifies a health check value.
-- * 'registrationTime' - Timestamp that indicates when the game server was created with a 'RegisterGameServer' request. The format is a number expressed in Unix time as milliseconds (for example @"1469498468.057"@ ).
-- * 'utilizationStatus' - Indicates whether the game server is currently available for new games or is busy. Possible statuses include:
--
--
--     * @AVAILABLE@ - The game server is available to be claimed. A game server that has been claimed remains in this status until it reports game hosting activity.
--
--
--     * @UTILIZED@ - The game server is currently hosting a game session with players.
mkGameServer ::
  GameServer
mkGameServer =
  GameServer'
    { instanceId = Lude.Nothing,
      lastClaimTime = Lude.Nothing,
      gameServerGroupName = Lude.Nothing,
      gameServerData = Lude.Nothing,
      claimStatus = Lude.Nothing,
      gameServerId = Lude.Nothing,
      utilizationStatus = Lude.Nothing,
      registrationTime = Lude.Nothing,
      lastHealthCheckTime = Lude.Nothing,
      connectionInfo = Lude.Nothing,
      gameServerGroupARN = Lude.Nothing
    }

-- | The unique identifier for the instance where the game server is running. This ID is available in the instance metadata. EC2 instance IDs use a 17-character format, for example: @i-1234567890abcdef0@ .
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsInstanceId :: Lens.Lens' GameServer (Lude.Maybe Lude.Text)
gsInstanceId = Lens.lens (instanceId :: GameServer -> Lude.Maybe Lude.Text) (\s a -> s {instanceId = a} :: GameServer)
{-# DEPRECATED gsInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | Timestamp that indicates the last time the game server was claimed with a 'ClaimGameServer' request. The format is a number expressed in Unix time as milliseconds (for example @"1469498468.057"@ ). This value is used to calculate when a claimed game server's status should revert to null.
--
-- /Note:/ Consider using 'lastClaimTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsLastClaimTime :: Lens.Lens' GameServer (Lude.Maybe Lude.Timestamp)
gsLastClaimTime = Lens.lens (lastClaimTime :: GameServer -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastClaimTime = a} :: GameServer)
{-# DEPRECATED gsLastClaimTime "Use generic-lens or generic-optics with 'lastClaimTime' instead." #-}

-- | A unique identifier for the game server group where the game server is running. Use either the 'GameServerGroup' name or ARN value.
--
-- /Note:/ Consider using 'gameServerGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsGameServerGroupName :: Lens.Lens' GameServer (Lude.Maybe Lude.Text)
gsGameServerGroupName = Lens.lens (gameServerGroupName :: GameServer -> Lude.Maybe Lude.Text) (\s a -> s {gameServerGroupName = a} :: GameServer)
{-# DEPRECATED gsGameServerGroupName "Use generic-lens or generic-optics with 'gameServerGroupName' instead." #-}

-- | A set of custom game server properties, formatted as a single string value. This data is passed to a game client or service when it requests information on game servers using 'ListGameServers' or 'ClaimGameServer' .
--
-- /Note:/ Consider using 'gameServerData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsGameServerData :: Lens.Lens' GameServer (Lude.Maybe Lude.Text)
gsGameServerData = Lens.lens (gameServerData :: GameServer -> Lude.Maybe Lude.Text) (\s a -> s {gameServerData = a} :: GameServer)
{-# DEPRECATED gsGameServerData "Use generic-lens or generic-optics with 'gameServerData' instead." #-}

-- | Indicates when an available game server has been reserved for gameplay but has not yet started hosting a game. Once it is claimed, the game server remains in @CLAIMED@ status for a maximum of one minute. During this time, game clients connect to the game server to start the game and trigger the game server to update its utilization status. After one minute, the game server claim status reverts to null.
--
-- /Note:/ Consider using 'claimStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsClaimStatus :: Lens.Lens' GameServer (Lude.Maybe GameServerClaimStatus)
gsClaimStatus = Lens.lens (claimStatus :: GameServer -> Lude.Maybe GameServerClaimStatus) (\s a -> s {claimStatus = a} :: GameServer)
{-# DEPRECATED gsClaimStatus "Use generic-lens or generic-optics with 'claimStatus' instead." #-}

-- | A custom string that uniquely identifies the game server. Game server IDs are developer-defined and are unique across all game server groups in an AWS account.
--
-- /Note:/ Consider using 'gameServerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsGameServerId :: Lens.Lens' GameServer (Lude.Maybe Lude.Text)
gsGameServerId = Lens.lens (gameServerId :: GameServer -> Lude.Maybe Lude.Text) (\s a -> s {gameServerId = a} :: GameServer)
{-# DEPRECATED gsGameServerId "Use generic-lens or generic-optics with 'gameServerId' instead." #-}

-- | Indicates whether the game server is currently available for new games or is busy. Possible statuses include:
--
--
--     * @AVAILABLE@ - The game server is available to be claimed. A game server that has been claimed remains in this status until it reports game hosting activity.
--
--
--     * @UTILIZED@ - The game server is currently hosting a game session with players.
--
--
--
-- /Note:/ Consider using 'utilizationStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsUtilizationStatus :: Lens.Lens' GameServer (Lude.Maybe GameServerUtilizationStatus)
gsUtilizationStatus = Lens.lens (utilizationStatus :: GameServer -> Lude.Maybe GameServerUtilizationStatus) (\s a -> s {utilizationStatus = a} :: GameServer)
{-# DEPRECATED gsUtilizationStatus "Use generic-lens or generic-optics with 'utilizationStatus' instead." #-}

-- | Timestamp that indicates when the game server was created with a 'RegisterGameServer' request. The format is a number expressed in Unix time as milliseconds (for example @"1469498468.057"@ ).
--
-- /Note:/ Consider using 'registrationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsRegistrationTime :: Lens.Lens' GameServer (Lude.Maybe Lude.Timestamp)
gsRegistrationTime = Lens.lens (registrationTime :: GameServer -> Lude.Maybe Lude.Timestamp) (\s a -> s {registrationTime = a} :: GameServer)
{-# DEPRECATED gsRegistrationTime "Use generic-lens or generic-optics with 'registrationTime' instead." #-}

-- | Timestamp that indicates the last time the game server was updated with health status using an 'UpdateGameServer' request. The format is a number expressed in Unix time as milliseconds (for example @"1469498468.057"@ ). After game server registration, this property is only changed when a game server update specifies a health check value.
--
-- /Note:/ Consider using 'lastHealthCheckTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsLastHealthCheckTime :: Lens.Lens' GameServer (Lude.Maybe Lude.Timestamp)
gsLastHealthCheckTime = Lens.lens (lastHealthCheckTime :: GameServer -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastHealthCheckTime = a} :: GameServer)
{-# DEPRECATED gsLastHealthCheckTime "Use generic-lens or generic-optics with 'lastHealthCheckTime' instead." #-}

-- | The port and IP address that must be used to establish a client connection to the game server.
--
-- /Note:/ Consider using 'connectionInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsConnectionInfo :: Lens.Lens' GameServer (Lude.Maybe Lude.Text)
gsConnectionInfo = Lens.lens (connectionInfo :: GameServer -> Lude.Maybe Lude.Text) (\s a -> s {connectionInfo = a} :: GameServer)
{-# DEPRECATED gsConnectionInfo "Use generic-lens or generic-optics with 'connectionInfo' instead." #-}

-- | The ARN identifier for the game server group where the game server is located.
--
-- /Note:/ Consider using 'gameServerGroupARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsGameServerGroupARN :: Lens.Lens' GameServer (Lude.Maybe Lude.Text)
gsGameServerGroupARN = Lens.lens (gameServerGroupARN :: GameServer -> Lude.Maybe Lude.Text) (\s a -> s {gameServerGroupARN = a} :: GameServer)
{-# DEPRECATED gsGameServerGroupARN "Use generic-lens or generic-optics with 'gameServerGroupARN' instead." #-}

instance Lude.FromJSON GameServer where
  parseJSON =
    Lude.withObject
      "GameServer"
      ( \x ->
          GameServer'
            Lude.<$> (x Lude..:? "InstanceId")
            Lude.<*> (x Lude..:? "LastClaimTime")
            Lude.<*> (x Lude..:? "GameServerGroupName")
            Lude.<*> (x Lude..:? "GameServerData")
            Lude.<*> (x Lude..:? "ClaimStatus")
            Lude.<*> (x Lude..:? "GameServerId")
            Lude.<*> (x Lude..:? "UtilizationStatus")
            Lude.<*> (x Lude..:? "RegistrationTime")
            Lude.<*> (x Lude..:? "LastHealthCheckTime")
            Lude.<*> (x Lude..:? "ConnectionInfo")
            Lude.<*> (x Lude..:? "GameServerGroupArn")
      )
