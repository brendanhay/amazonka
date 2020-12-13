{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types.PlayerSession
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GameLift.Types.PlayerSession
  ( PlayerSession (..),

    -- * Smart constructor
    mkPlayerSession,

    -- * Lenses
    psCreationTime,
    psStatus,
    psIPAddress,
    psGameSessionId,
    psFleetARN,
    psTerminationTime,
    psPlayerSessionId,
    psFleetId,
    psPlayerData,
    psPlayerId,
    psDNSName,
    psPort,
  )
where

import Network.AWS.GameLift.Types.PlayerSessionStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Properties describing a player session. Player session objects are created either by creating a player session for a specific game session, or as part of a game session placement. A player session represents either a player reservation for a game session (status @RESERVED@ ) or actual player activity in a game session (status @ACTIVE@ ). A player session object (including player data) is automatically passed to a game session when the player connects to the game session and is validated.
--
-- When a player disconnects, the player session status changes to @COMPLETED@ . Once the session ends, the player session object is retained for 30 days and then removed.
--
--     * 'CreatePlayerSession'
--
--
--     * 'CreatePlayerSessions'
--
--
--     * 'DescribePlayerSessions'
--
--
--     * Game session placements
--
--     * 'StartGameSessionPlacement'
--
--
--     * 'DescribeGameSessionPlacement'
--
--
--     * 'StopGameSessionPlacement'
--
--
--
--
--
-- /See:/ 'mkPlayerSession' smart constructor.
data PlayerSession = PlayerSession'
  { -- | Time stamp indicating when this data object was created. Format is a number expressed in Unix time as milliseconds (for example "1469498468.057").
    creationTime :: Lude.Maybe Lude.Timestamp,
    -- | Current status of the player session.
    --
    -- Possible player session statuses include the following:
    --
    --     * __RESERVED__ -- The player session request has been received, but the player has not yet connected to the server process and/or been validated.
    --
    --
    --     * __ACTIVE__ -- The player has been validated by the server process and is currently connected.
    --
    --
    --     * __COMPLETED__ -- The player connection has been dropped.
    --
    --
    --     * __TIMEDOUT__ -- A player session request was received, but the player did not connect and/or was not validated within the timeout limit (60 seconds).
    status :: Lude.Maybe PlayerSessionStatus,
    -- | IP address of the instance that is running the game session. When connecting to a Amazon GameLift game server, a client needs to reference an IP address (or DNS name) and port number.
    ipAddress :: Lude.Maybe Lude.Text,
    -- | A unique identifier for the game session that the player session is connected to.
    gameSessionId :: Lude.Maybe Lude.Text,
    -- | The Amazon Resource Name (<https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN> ) associated with the GameLift fleet that the player's game session is running on.
    fleetARN :: Lude.Maybe Lude.Text,
    -- | Time stamp indicating when this data object was terminated. Format is a number expressed in Unix time as milliseconds (for example "1469498468.057").
    terminationTime :: Lude.Maybe Lude.Timestamp,
    -- | A unique identifier for a player session.
    playerSessionId :: Lude.Maybe Lude.Text,
    -- | A unique identifier for a fleet that the player's game session is running on.
    fleetId :: Lude.Maybe Lude.Text,
    -- | Developer-defined information related to a player. Amazon GameLift does not use this data, so it can be formatted as needed for use in the game.
    playerData :: Lude.Maybe Lude.Text,
    -- | A unique identifier for a player that is associated with this player session.
    playerId :: Lude.Maybe Lude.Text,
    -- | DNS identifier assigned to the instance that is running the game session. Values have the following format:
    --
    --
    --     * TLS-enabled fleets: @<unique identifier>.<region identifier>.amazongamelift.com@ .
    --
    --
    --     * Non-TLS-enabled fleets: @ec2-<unique identifier>.compute.amazonaws.com@ . (See <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/using-instance-addressing.html#concepts-public-addresses Amazon EC2 Instance IP Addressing> .)
    --
    --
    -- When connecting to a game session that is running on a TLS-enabled fleet, you must use the DNS name, not the IP address.
    dnsName :: Lude.Maybe Lude.Text,
    -- | Port number for the game session. To connect to a Amazon GameLift server process, an app needs both the IP address and port number.
    port :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PlayerSession' with the minimum fields required to make a request.
--
-- * 'creationTime' - Time stamp indicating when this data object was created. Format is a number expressed in Unix time as milliseconds (for example "1469498468.057").
-- * 'status' - Current status of the player session.
--
-- Possible player session statuses include the following:
--
--     * __RESERVED__ -- The player session request has been received, but the player has not yet connected to the server process and/or been validated.
--
--
--     * __ACTIVE__ -- The player has been validated by the server process and is currently connected.
--
--
--     * __COMPLETED__ -- The player connection has been dropped.
--
--
--     * __TIMEDOUT__ -- A player session request was received, but the player did not connect and/or was not validated within the timeout limit (60 seconds).
--
--
-- * 'ipAddress' - IP address of the instance that is running the game session. When connecting to a Amazon GameLift game server, a client needs to reference an IP address (or DNS name) and port number.
-- * 'gameSessionId' - A unique identifier for the game session that the player session is connected to.
-- * 'fleetARN' - The Amazon Resource Name (<https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN> ) associated with the GameLift fleet that the player's game session is running on.
-- * 'terminationTime' - Time stamp indicating when this data object was terminated. Format is a number expressed in Unix time as milliseconds (for example "1469498468.057").
-- * 'playerSessionId' - A unique identifier for a player session.
-- * 'fleetId' - A unique identifier for a fleet that the player's game session is running on.
-- * 'playerData' - Developer-defined information related to a player. Amazon GameLift does not use this data, so it can be formatted as needed for use in the game.
-- * 'playerId' - A unique identifier for a player that is associated with this player session.
-- * 'dnsName' - DNS identifier assigned to the instance that is running the game session. Values have the following format:
--
--
--     * TLS-enabled fleets: @<unique identifier>.<region identifier>.amazongamelift.com@ .
--
--
--     * Non-TLS-enabled fleets: @ec2-<unique identifier>.compute.amazonaws.com@ . (See <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/using-instance-addressing.html#concepts-public-addresses Amazon EC2 Instance IP Addressing> .)
--
--
-- When connecting to a game session that is running on a TLS-enabled fleet, you must use the DNS name, not the IP address.
-- * 'port' - Port number for the game session. To connect to a Amazon GameLift server process, an app needs both the IP address and port number.
mkPlayerSession ::
  PlayerSession
mkPlayerSession =
  PlayerSession'
    { creationTime = Lude.Nothing,
      status = Lude.Nothing,
      ipAddress = Lude.Nothing,
      gameSessionId = Lude.Nothing,
      fleetARN = Lude.Nothing,
      terminationTime = Lude.Nothing,
      playerSessionId = Lude.Nothing,
      fleetId = Lude.Nothing,
      playerData = Lude.Nothing,
      playerId = Lude.Nothing,
      dnsName = Lude.Nothing,
      port = Lude.Nothing
    }

-- | Time stamp indicating when this data object was created. Format is a number expressed in Unix time as milliseconds (for example "1469498468.057").
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psCreationTime :: Lens.Lens' PlayerSession (Lude.Maybe Lude.Timestamp)
psCreationTime = Lens.lens (creationTime :: PlayerSession -> Lude.Maybe Lude.Timestamp) (\s a -> s {creationTime = a} :: PlayerSession)
{-# DEPRECATED psCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | Current status of the player session.
--
-- Possible player session statuses include the following:
--
--     * __RESERVED__ -- The player session request has been received, but the player has not yet connected to the server process and/or been validated.
--
--
--     * __ACTIVE__ -- The player has been validated by the server process and is currently connected.
--
--
--     * __COMPLETED__ -- The player connection has been dropped.
--
--
--     * __TIMEDOUT__ -- A player session request was received, but the player did not connect and/or was not validated within the timeout limit (60 seconds).
--
--
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psStatus :: Lens.Lens' PlayerSession (Lude.Maybe PlayerSessionStatus)
psStatus = Lens.lens (status :: PlayerSession -> Lude.Maybe PlayerSessionStatus) (\s a -> s {status = a} :: PlayerSession)
{-# DEPRECATED psStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | IP address of the instance that is running the game session. When connecting to a Amazon GameLift game server, a client needs to reference an IP address (or DNS name) and port number.
--
-- /Note:/ Consider using 'ipAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psIPAddress :: Lens.Lens' PlayerSession (Lude.Maybe Lude.Text)
psIPAddress = Lens.lens (ipAddress :: PlayerSession -> Lude.Maybe Lude.Text) (\s a -> s {ipAddress = a} :: PlayerSession)
{-# DEPRECATED psIPAddress "Use generic-lens or generic-optics with 'ipAddress' instead." #-}

-- | A unique identifier for the game session that the player session is connected to.
--
-- /Note:/ Consider using 'gameSessionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psGameSessionId :: Lens.Lens' PlayerSession (Lude.Maybe Lude.Text)
psGameSessionId = Lens.lens (gameSessionId :: PlayerSession -> Lude.Maybe Lude.Text) (\s a -> s {gameSessionId = a} :: PlayerSession)
{-# DEPRECATED psGameSessionId "Use generic-lens or generic-optics with 'gameSessionId' instead." #-}

-- | The Amazon Resource Name (<https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN> ) associated with the GameLift fleet that the player's game session is running on.
--
-- /Note:/ Consider using 'fleetARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psFleetARN :: Lens.Lens' PlayerSession (Lude.Maybe Lude.Text)
psFleetARN = Lens.lens (fleetARN :: PlayerSession -> Lude.Maybe Lude.Text) (\s a -> s {fleetARN = a} :: PlayerSession)
{-# DEPRECATED psFleetARN "Use generic-lens or generic-optics with 'fleetARN' instead." #-}

-- | Time stamp indicating when this data object was terminated. Format is a number expressed in Unix time as milliseconds (for example "1469498468.057").
--
-- /Note:/ Consider using 'terminationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psTerminationTime :: Lens.Lens' PlayerSession (Lude.Maybe Lude.Timestamp)
psTerminationTime = Lens.lens (terminationTime :: PlayerSession -> Lude.Maybe Lude.Timestamp) (\s a -> s {terminationTime = a} :: PlayerSession)
{-# DEPRECATED psTerminationTime "Use generic-lens or generic-optics with 'terminationTime' instead." #-}

-- | A unique identifier for a player session.
--
-- /Note:/ Consider using 'playerSessionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psPlayerSessionId :: Lens.Lens' PlayerSession (Lude.Maybe Lude.Text)
psPlayerSessionId = Lens.lens (playerSessionId :: PlayerSession -> Lude.Maybe Lude.Text) (\s a -> s {playerSessionId = a} :: PlayerSession)
{-# DEPRECATED psPlayerSessionId "Use generic-lens or generic-optics with 'playerSessionId' instead." #-}

-- | A unique identifier for a fleet that the player's game session is running on.
--
-- /Note:/ Consider using 'fleetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psFleetId :: Lens.Lens' PlayerSession (Lude.Maybe Lude.Text)
psFleetId = Lens.lens (fleetId :: PlayerSession -> Lude.Maybe Lude.Text) (\s a -> s {fleetId = a} :: PlayerSession)
{-# DEPRECATED psFleetId "Use generic-lens or generic-optics with 'fleetId' instead." #-}

-- | Developer-defined information related to a player. Amazon GameLift does not use this data, so it can be formatted as needed for use in the game.
--
-- /Note:/ Consider using 'playerData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psPlayerData :: Lens.Lens' PlayerSession (Lude.Maybe Lude.Text)
psPlayerData = Lens.lens (playerData :: PlayerSession -> Lude.Maybe Lude.Text) (\s a -> s {playerData = a} :: PlayerSession)
{-# DEPRECATED psPlayerData "Use generic-lens or generic-optics with 'playerData' instead." #-}

-- | A unique identifier for a player that is associated with this player session.
--
-- /Note:/ Consider using 'playerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psPlayerId :: Lens.Lens' PlayerSession (Lude.Maybe Lude.Text)
psPlayerId = Lens.lens (playerId :: PlayerSession -> Lude.Maybe Lude.Text) (\s a -> s {playerId = a} :: PlayerSession)
{-# DEPRECATED psPlayerId "Use generic-lens or generic-optics with 'playerId' instead." #-}

-- | DNS identifier assigned to the instance that is running the game session. Values have the following format:
--
--
--     * TLS-enabled fleets: @<unique identifier>.<region identifier>.amazongamelift.com@ .
--
--
--     * Non-TLS-enabled fleets: @ec2-<unique identifier>.compute.amazonaws.com@ . (See <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/using-instance-addressing.html#concepts-public-addresses Amazon EC2 Instance IP Addressing> .)
--
--
-- When connecting to a game session that is running on a TLS-enabled fleet, you must use the DNS name, not the IP address.
--
-- /Note:/ Consider using 'dnsName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psDNSName :: Lens.Lens' PlayerSession (Lude.Maybe Lude.Text)
psDNSName = Lens.lens (dnsName :: PlayerSession -> Lude.Maybe Lude.Text) (\s a -> s {dnsName = a} :: PlayerSession)
{-# DEPRECATED psDNSName "Use generic-lens or generic-optics with 'dnsName' instead." #-}

-- | Port number for the game session. To connect to a Amazon GameLift server process, an app needs both the IP address and port number.
--
-- /Note:/ Consider using 'port' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psPort :: Lens.Lens' PlayerSession (Lude.Maybe Lude.Natural)
psPort = Lens.lens (port :: PlayerSession -> Lude.Maybe Lude.Natural) (\s a -> s {port = a} :: PlayerSession)
{-# DEPRECATED psPort "Use generic-lens or generic-optics with 'port' instead." #-}

instance Lude.FromJSON PlayerSession where
  parseJSON =
    Lude.withObject
      "PlayerSession"
      ( \x ->
          PlayerSession'
            Lude.<$> (x Lude..:? "CreationTime")
            Lude.<*> (x Lude..:? "Status")
            Lude.<*> (x Lude..:? "IpAddress")
            Lude.<*> (x Lude..:? "GameSessionId")
            Lude.<*> (x Lude..:? "FleetArn")
            Lude.<*> (x Lude..:? "TerminationTime")
            Lude.<*> (x Lude..:? "PlayerSessionId")
            Lude.<*> (x Lude..:? "FleetId")
            Lude.<*> (x Lude..:? "PlayerData")
            Lude.<*> (x Lude..:? "PlayerId")
            Lude.<*> (x Lude..:? "DnsName")
            Lude.<*> (x Lude..:? "Port")
      )
