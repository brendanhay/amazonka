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
    psDnsName,
    psFleetArn,
    psFleetId,
    psGameSessionId,
    psIpAddress,
    psPlayerData,
    psPlayerId,
    psPlayerSessionId,
    psPort,
    psStatus,
    psTerminationTime,
  )
where

import qualified Network.AWS.GameLift.Types.DnsName as Types
import qualified Network.AWS.GameLift.Types.FleetArn as Types
import qualified Network.AWS.GameLift.Types.FleetId as Types
import qualified Network.AWS.GameLift.Types.IpAddress as Types
import qualified Network.AWS.GameLift.Types.NonZeroAndMaxString as Types
import qualified Network.AWS.GameLift.Types.PlayerData as Types
import qualified Network.AWS.GameLift.Types.PlayerSessionId as Types
import qualified Network.AWS.GameLift.Types.PlayerSessionStatus as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

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
    creationTime :: Core.Maybe Core.NominalDiffTime,
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
    dnsName :: Core.Maybe Types.DnsName,
    -- | The Amazon Resource Name (<https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN> ) associated with the GameLift fleet that the player's game session is running on.
    fleetArn :: Core.Maybe Types.FleetArn,
    -- | A unique identifier for a fleet that the player's game session is running on.
    fleetId :: Core.Maybe Types.FleetId,
    -- | A unique identifier for the game session that the player session is connected to.
    gameSessionId :: Core.Maybe Types.NonZeroAndMaxString,
    -- | IP address of the instance that is running the game session. When connecting to a Amazon GameLift game server, a client needs to reference an IP address (or DNS name) and port number.
    ipAddress :: Core.Maybe Types.IpAddress,
    -- | Developer-defined information related to a player. Amazon GameLift does not use this data, so it can be formatted as needed for use in the game.
    playerData :: Core.Maybe Types.PlayerData,
    -- | A unique identifier for a player that is associated with this player session.
    playerId :: Core.Maybe Types.NonZeroAndMaxString,
    -- | A unique identifier for a player session.
    playerSessionId :: Core.Maybe Types.PlayerSessionId,
    -- | Port number for the game session. To connect to a Amazon GameLift server process, an app needs both the IP address and port number.
    port :: Core.Maybe Core.Natural,
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
    status :: Core.Maybe Types.PlayerSessionStatus,
    -- | Time stamp indicating when this data object was terminated. Format is a number expressed in Unix time as milliseconds (for example "1469498468.057").
    terminationTime :: Core.Maybe Core.NominalDiffTime
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'PlayerSession' value with any optional fields omitted.
mkPlayerSession ::
  PlayerSession
mkPlayerSession =
  PlayerSession'
    { creationTime = Core.Nothing,
      dnsName = Core.Nothing,
      fleetArn = Core.Nothing,
      fleetId = Core.Nothing,
      gameSessionId = Core.Nothing,
      ipAddress = Core.Nothing,
      playerData = Core.Nothing,
      playerId = Core.Nothing,
      playerSessionId = Core.Nothing,
      port = Core.Nothing,
      status = Core.Nothing,
      terminationTime = Core.Nothing
    }

-- | Time stamp indicating when this data object was created. Format is a number expressed in Unix time as milliseconds (for example "1469498468.057").
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psCreationTime :: Lens.Lens' PlayerSession (Core.Maybe Core.NominalDiffTime)
psCreationTime = Lens.field @"creationTime"
{-# DEPRECATED psCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

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
psDnsName :: Lens.Lens' PlayerSession (Core.Maybe Types.DnsName)
psDnsName = Lens.field @"dnsName"
{-# DEPRECATED psDnsName "Use generic-lens or generic-optics with 'dnsName' instead." #-}

-- | The Amazon Resource Name (<https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN> ) associated with the GameLift fleet that the player's game session is running on.
--
-- /Note:/ Consider using 'fleetArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psFleetArn :: Lens.Lens' PlayerSession (Core.Maybe Types.FleetArn)
psFleetArn = Lens.field @"fleetArn"
{-# DEPRECATED psFleetArn "Use generic-lens or generic-optics with 'fleetArn' instead." #-}

-- | A unique identifier for a fleet that the player's game session is running on.
--
-- /Note:/ Consider using 'fleetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psFleetId :: Lens.Lens' PlayerSession (Core.Maybe Types.FleetId)
psFleetId = Lens.field @"fleetId"
{-# DEPRECATED psFleetId "Use generic-lens or generic-optics with 'fleetId' instead." #-}

-- | A unique identifier for the game session that the player session is connected to.
--
-- /Note:/ Consider using 'gameSessionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psGameSessionId :: Lens.Lens' PlayerSession (Core.Maybe Types.NonZeroAndMaxString)
psGameSessionId = Lens.field @"gameSessionId"
{-# DEPRECATED psGameSessionId "Use generic-lens or generic-optics with 'gameSessionId' instead." #-}

-- | IP address of the instance that is running the game session. When connecting to a Amazon GameLift game server, a client needs to reference an IP address (or DNS name) and port number.
--
-- /Note:/ Consider using 'ipAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psIpAddress :: Lens.Lens' PlayerSession (Core.Maybe Types.IpAddress)
psIpAddress = Lens.field @"ipAddress"
{-# DEPRECATED psIpAddress "Use generic-lens or generic-optics with 'ipAddress' instead." #-}

-- | Developer-defined information related to a player. Amazon GameLift does not use this data, so it can be formatted as needed for use in the game.
--
-- /Note:/ Consider using 'playerData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psPlayerData :: Lens.Lens' PlayerSession (Core.Maybe Types.PlayerData)
psPlayerData = Lens.field @"playerData"
{-# DEPRECATED psPlayerData "Use generic-lens or generic-optics with 'playerData' instead." #-}

-- | A unique identifier for a player that is associated with this player session.
--
-- /Note:/ Consider using 'playerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psPlayerId :: Lens.Lens' PlayerSession (Core.Maybe Types.NonZeroAndMaxString)
psPlayerId = Lens.field @"playerId"
{-# DEPRECATED psPlayerId "Use generic-lens or generic-optics with 'playerId' instead." #-}

-- | A unique identifier for a player session.
--
-- /Note:/ Consider using 'playerSessionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psPlayerSessionId :: Lens.Lens' PlayerSession (Core.Maybe Types.PlayerSessionId)
psPlayerSessionId = Lens.field @"playerSessionId"
{-# DEPRECATED psPlayerSessionId "Use generic-lens or generic-optics with 'playerSessionId' instead." #-}

-- | Port number for the game session. To connect to a Amazon GameLift server process, an app needs both the IP address and port number.
--
-- /Note:/ Consider using 'port' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psPort :: Lens.Lens' PlayerSession (Core.Maybe Core.Natural)
psPort = Lens.field @"port"
{-# DEPRECATED psPort "Use generic-lens or generic-optics with 'port' instead." #-}

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
psStatus :: Lens.Lens' PlayerSession (Core.Maybe Types.PlayerSessionStatus)
psStatus = Lens.field @"status"
{-# DEPRECATED psStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | Time stamp indicating when this data object was terminated. Format is a number expressed in Unix time as milliseconds (for example "1469498468.057").
--
-- /Note:/ Consider using 'terminationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psTerminationTime :: Lens.Lens' PlayerSession (Core.Maybe Core.NominalDiffTime)
psTerminationTime = Lens.field @"terminationTime"
{-# DEPRECATED psTerminationTime "Use generic-lens or generic-optics with 'terminationTime' instead." #-}

instance Core.FromJSON PlayerSession where
  parseJSON =
    Core.withObject "PlayerSession" Core.$
      \x ->
        PlayerSession'
          Core.<$> (x Core..:? "CreationTime")
          Core.<*> (x Core..:? "DnsName")
          Core.<*> (x Core..:? "FleetArn")
          Core.<*> (x Core..:? "FleetId")
          Core.<*> (x Core..:? "GameSessionId")
          Core.<*> (x Core..:? "IpAddress")
          Core.<*> (x Core..:? "PlayerData")
          Core.<*> (x Core..:? "PlayerId")
          Core.<*> (x Core..:? "PlayerSessionId")
          Core.<*> (x Core..:? "Port")
          Core.<*> (x Core..:? "Status")
          Core.<*> (x Core..:? "TerminationTime")
