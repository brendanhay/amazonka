{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types.GameServer
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.GameLift.Types.GameServer
  ( GameServer (..)
  -- * Smart constructor
  , mkGameServer
  -- * Lenses
  , gsClaimStatus
  , gsConnectionInfo
  , gsGameServerData
  , gsGameServerGroupArn
  , gsGameServerGroupName
  , gsGameServerId
  , gsInstanceId
  , gsLastClaimTime
  , gsLastHealthCheckTime
  , gsRegistrationTime
  , gsUtilizationStatus
  ) where

import qualified Network.AWS.GameLift.Types.ConnectionInfo as Types
import qualified Network.AWS.GameLift.Types.GameServerClaimStatus as Types
import qualified Network.AWS.GameLift.Types.GameServerData as Types
import qualified Network.AWS.GameLift.Types.GameServerGroupArn as Types
import qualified Network.AWS.GameLift.Types.GameServerGroupName as Types
import qualified Network.AWS.GameLift.Types.GameServerId as Types
import qualified Network.AWS.GameLift.Types.GameServerInstanceId as Types
import qualified Network.AWS.GameLift.Types.GameServerUtilizationStatus as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

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
  { claimStatus :: Core.Maybe Types.GameServerClaimStatus
    -- ^ Indicates when an available game server has been reserved for gameplay but has not yet started hosting a game. Once it is claimed, the game server remains in @CLAIMED@ status for a maximum of one minute. During this time, game clients connect to the game server to start the game and trigger the game server to update its utilization status. After one minute, the game server claim status reverts to null.
  , connectionInfo :: Core.Maybe Types.ConnectionInfo
    -- ^ The port and IP address that must be used to establish a client connection to the game server.
  , gameServerData :: Core.Maybe Types.GameServerData
    -- ^ A set of custom game server properties, formatted as a single string value. This data is passed to a game client or service when it requests information on game servers using 'ListGameServers' or 'ClaimGameServer' .
  , gameServerGroupArn :: Core.Maybe Types.GameServerGroupArn
    -- ^ The ARN identifier for the game server group where the game server is located.
  , gameServerGroupName :: Core.Maybe Types.GameServerGroupName
    -- ^ A unique identifier for the game server group where the game server is running. Use either the 'GameServerGroup' name or ARN value.
  , gameServerId :: Core.Maybe Types.GameServerId
    -- ^ A custom string that uniquely identifies the game server. Game server IDs are developer-defined and are unique across all game server groups in an AWS account.
  , instanceId :: Core.Maybe Types.GameServerInstanceId
    -- ^ The unique identifier for the instance where the game server is running. This ID is available in the instance metadata. EC2 instance IDs use a 17-character format, for example: @i-1234567890abcdef0@ .
  , lastClaimTime :: Core.Maybe Core.NominalDiffTime
    -- ^ Timestamp that indicates the last time the game server was claimed with a 'ClaimGameServer' request. The format is a number expressed in Unix time as milliseconds (for example @"1469498468.057"@ ). This value is used to calculate when a claimed game server's status should revert to null.
  , lastHealthCheckTime :: Core.Maybe Core.NominalDiffTime
    -- ^ Timestamp that indicates the last time the game server was updated with health status using an 'UpdateGameServer' request. The format is a number expressed in Unix time as milliseconds (for example @"1469498468.057"@ ). After game server registration, this property is only changed when a game server update specifies a health check value.
  , registrationTime :: Core.Maybe Core.NominalDiffTime
    -- ^ Timestamp that indicates when the game server was created with a 'RegisterGameServer' request. The format is a number expressed in Unix time as milliseconds (for example @"1469498468.057"@ ).
  , utilizationStatus :: Core.Maybe Types.GameServerUtilizationStatus
    -- ^ Indicates whether the game server is currently available for new games or is busy. Possible statuses include:
--
--
--     * @AVAILABLE@ - The game server is available to be claimed. A game server that has been claimed remains in this status until it reports game hosting activity. 
--
--
--     * @UTILIZED@ - The game server is currently hosting a game session with players. 
--
--
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'GameServer' value with any optional fields omitted.
mkGameServer
    :: GameServer
mkGameServer
  = GameServer'{claimStatus = Core.Nothing,
                connectionInfo = Core.Nothing, gameServerData = Core.Nothing,
                gameServerGroupArn = Core.Nothing,
                gameServerGroupName = Core.Nothing, gameServerId = Core.Nothing,
                instanceId = Core.Nothing, lastClaimTime = Core.Nothing,
                lastHealthCheckTime = Core.Nothing,
                registrationTime = Core.Nothing, utilizationStatus = Core.Nothing}

-- | Indicates when an available game server has been reserved for gameplay but has not yet started hosting a game. Once it is claimed, the game server remains in @CLAIMED@ status for a maximum of one minute. During this time, game clients connect to the game server to start the game and trigger the game server to update its utilization status. After one minute, the game server claim status reverts to null.
--
-- /Note:/ Consider using 'claimStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsClaimStatus :: Lens.Lens' GameServer (Core.Maybe Types.GameServerClaimStatus)
gsClaimStatus = Lens.field @"claimStatus"
{-# INLINEABLE gsClaimStatus #-}
{-# DEPRECATED claimStatus "Use generic-lens or generic-optics with 'claimStatus' instead"  #-}

-- | The port and IP address that must be used to establish a client connection to the game server.
--
-- /Note:/ Consider using 'connectionInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsConnectionInfo :: Lens.Lens' GameServer (Core.Maybe Types.ConnectionInfo)
gsConnectionInfo = Lens.field @"connectionInfo"
{-# INLINEABLE gsConnectionInfo #-}
{-# DEPRECATED connectionInfo "Use generic-lens or generic-optics with 'connectionInfo' instead"  #-}

-- | A set of custom game server properties, formatted as a single string value. This data is passed to a game client or service when it requests information on game servers using 'ListGameServers' or 'ClaimGameServer' .
--
-- /Note:/ Consider using 'gameServerData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsGameServerData :: Lens.Lens' GameServer (Core.Maybe Types.GameServerData)
gsGameServerData = Lens.field @"gameServerData"
{-# INLINEABLE gsGameServerData #-}
{-# DEPRECATED gameServerData "Use generic-lens or generic-optics with 'gameServerData' instead"  #-}

-- | The ARN identifier for the game server group where the game server is located.
--
-- /Note:/ Consider using 'gameServerGroupArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsGameServerGroupArn :: Lens.Lens' GameServer (Core.Maybe Types.GameServerGroupArn)
gsGameServerGroupArn = Lens.field @"gameServerGroupArn"
{-# INLINEABLE gsGameServerGroupArn #-}
{-# DEPRECATED gameServerGroupArn "Use generic-lens or generic-optics with 'gameServerGroupArn' instead"  #-}

-- | A unique identifier for the game server group where the game server is running. Use either the 'GameServerGroup' name or ARN value.
--
-- /Note:/ Consider using 'gameServerGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsGameServerGroupName :: Lens.Lens' GameServer (Core.Maybe Types.GameServerGroupName)
gsGameServerGroupName = Lens.field @"gameServerGroupName"
{-# INLINEABLE gsGameServerGroupName #-}
{-# DEPRECATED gameServerGroupName "Use generic-lens or generic-optics with 'gameServerGroupName' instead"  #-}

-- | A custom string that uniquely identifies the game server. Game server IDs are developer-defined and are unique across all game server groups in an AWS account.
--
-- /Note:/ Consider using 'gameServerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsGameServerId :: Lens.Lens' GameServer (Core.Maybe Types.GameServerId)
gsGameServerId = Lens.field @"gameServerId"
{-# INLINEABLE gsGameServerId #-}
{-# DEPRECATED gameServerId "Use generic-lens or generic-optics with 'gameServerId' instead"  #-}

-- | The unique identifier for the instance where the game server is running. This ID is available in the instance metadata. EC2 instance IDs use a 17-character format, for example: @i-1234567890abcdef0@ .
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsInstanceId :: Lens.Lens' GameServer (Core.Maybe Types.GameServerInstanceId)
gsInstanceId = Lens.field @"instanceId"
{-# INLINEABLE gsInstanceId #-}
{-# DEPRECATED instanceId "Use generic-lens or generic-optics with 'instanceId' instead"  #-}

-- | Timestamp that indicates the last time the game server was claimed with a 'ClaimGameServer' request. The format is a number expressed in Unix time as milliseconds (for example @"1469498468.057"@ ). This value is used to calculate when a claimed game server's status should revert to null.
--
-- /Note:/ Consider using 'lastClaimTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsLastClaimTime :: Lens.Lens' GameServer (Core.Maybe Core.NominalDiffTime)
gsLastClaimTime = Lens.field @"lastClaimTime"
{-# INLINEABLE gsLastClaimTime #-}
{-# DEPRECATED lastClaimTime "Use generic-lens or generic-optics with 'lastClaimTime' instead"  #-}

-- | Timestamp that indicates the last time the game server was updated with health status using an 'UpdateGameServer' request. The format is a number expressed in Unix time as milliseconds (for example @"1469498468.057"@ ). After game server registration, this property is only changed when a game server update specifies a health check value.
--
-- /Note:/ Consider using 'lastHealthCheckTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsLastHealthCheckTime :: Lens.Lens' GameServer (Core.Maybe Core.NominalDiffTime)
gsLastHealthCheckTime = Lens.field @"lastHealthCheckTime"
{-# INLINEABLE gsLastHealthCheckTime #-}
{-# DEPRECATED lastHealthCheckTime "Use generic-lens or generic-optics with 'lastHealthCheckTime' instead"  #-}

-- | Timestamp that indicates when the game server was created with a 'RegisterGameServer' request. The format is a number expressed in Unix time as milliseconds (for example @"1469498468.057"@ ).
--
-- /Note:/ Consider using 'registrationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsRegistrationTime :: Lens.Lens' GameServer (Core.Maybe Core.NominalDiffTime)
gsRegistrationTime = Lens.field @"registrationTime"
{-# INLINEABLE gsRegistrationTime #-}
{-# DEPRECATED registrationTime "Use generic-lens or generic-optics with 'registrationTime' instead"  #-}

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
gsUtilizationStatus :: Lens.Lens' GameServer (Core.Maybe Types.GameServerUtilizationStatus)
gsUtilizationStatus = Lens.field @"utilizationStatus"
{-# INLINEABLE gsUtilizationStatus #-}
{-# DEPRECATED utilizationStatus "Use generic-lens or generic-optics with 'utilizationStatus' instead"  #-}

instance Core.FromJSON GameServer where
        parseJSON
          = Core.withObject "GameServer" Core.$
              \ x ->
                GameServer' Core.<$>
                  (x Core..:? "ClaimStatus") Core.<*> x Core..:? "ConnectionInfo"
                    Core.<*> x Core..:? "GameServerData"
                    Core.<*> x Core..:? "GameServerGroupArn"
                    Core.<*> x Core..:? "GameServerGroupName"
                    Core.<*> x Core..:? "GameServerId"
                    Core.<*> x Core..:? "InstanceId"
                    Core.<*> x Core..:? "LastClaimTime"
                    Core.<*> x Core..:? "LastHealthCheckTime"
                    Core.<*> x Core..:? "RegistrationTime"
                    Core.<*> x Core..:? "UtilizationStatus"
