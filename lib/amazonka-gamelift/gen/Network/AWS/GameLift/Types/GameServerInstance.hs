{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types.GameServerInstance
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GameLift.Types.GameServerInstance
  ( GameServerInstance (..),

    -- * Smart constructor
    mkGameServerInstance,

    -- * Lenses
    gsiGameServerGroupArn,
    gsiGameServerGroupName,
    gsiInstanceId,
    gsiInstanceStatus,
  )
where

import qualified Network.AWS.GameLift.Types.GameServerGroupArn as Types
import qualified Network.AWS.GameLift.Types.GameServerGroupName as Types
import qualified Network.AWS.GameLift.Types.GameServerInstanceId as Types
import qualified Network.AWS.GameLift.Types.GameServerInstanceStatus as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | __This data type is used with the Amazon GameLift FleetIQ and game server groups.__
--
-- Additional properties, including status, that describe an EC2 instance in a game server group. Instance configurations are set with game server group properties (see @DescribeGameServerGroup@ and with the EC2 launch template that was used when creating the game server group.
-- Retrieve game server instances for a game server group by calling @DescribeGameServerInstances@ .
--
--     * 'CreateGameServerGroup'
--
--
--     * 'ListGameServerGroups'
--
--
--     * 'DescribeGameServerGroup'
--
--
--     * 'UpdateGameServerGroup'
--
--
--     * 'DeleteGameServerGroup'
--
--
--     * 'ResumeGameServerGroup'
--
--
--     * 'SuspendGameServerGroup'
--
--
--     * 'DescribeGameServerInstances'
--
--
--
-- /See:/ 'mkGameServerInstance' smart constructor.
data GameServerInstance = GameServerInstance'
  { -- | A generated unique identifier for the game server group that includes the game server instance.
    gameServerGroupArn :: Core.Maybe Types.GameServerGroupArn,
    -- | A developer-defined identifier for the game server group that includes the game server instance. The name is unique for each Region in each AWS account.
    gameServerGroupName :: Core.Maybe Types.GameServerGroupName,
    -- | The unique identifier for the instance where the game server is running. This ID is available in the instance metadata. EC2 instance IDs use a 17-character format, for example: @i-1234567890abcdef0@ .
    instanceId :: Core.Maybe Types.GameServerInstanceId,
    -- | Current status of the game server instance.
    --
    --
    --     * __ACTIVE__ -- The instance is viable for hosting game servers.
    --
    --
    --     * __DRAINING__ -- The instance is not viable for hosting game servers. Existing game servers are in the process of ending, and new game servers are not started on this instance unless no other resources are available. When the instance is put in DRAINING, a new instance is started up to replace it. Once the instance has no UTILIZED game servers, it will be terminated in favor of the new instance.
    --
    --
    --     * __SPOT_TERMINATING__ -- The instance is in the process of shutting down due to a Spot instance interruption. No new game servers are started on this instance.
    instanceStatus :: Core.Maybe Types.GameServerInstanceStatus
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GameServerInstance' value with any optional fields omitted.
mkGameServerInstance ::
  GameServerInstance
mkGameServerInstance =
  GameServerInstance'
    { gameServerGroupArn = Core.Nothing,
      gameServerGroupName = Core.Nothing,
      instanceId = Core.Nothing,
      instanceStatus = Core.Nothing
    }

-- | A generated unique identifier for the game server group that includes the game server instance.
--
-- /Note:/ Consider using 'gameServerGroupArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsiGameServerGroupArn :: Lens.Lens' GameServerInstance (Core.Maybe Types.GameServerGroupArn)
gsiGameServerGroupArn = Lens.field @"gameServerGroupArn"
{-# DEPRECATED gsiGameServerGroupArn "Use generic-lens or generic-optics with 'gameServerGroupArn' instead." #-}

-- | A developer-defined identifier for the game server group that includes the game server instance. The name is unique for each Region in each AWS account.
--
-- /Note:/ Consider using 'gameServerGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsiGameServerGroupName :: Lens.Lens' GameServerInstance (Core.Maybe Types.GameServerGroupName)
gsiGameServerGroupName = Lens.field @"gameServerGroupName"
{-# DEPRECATED gsiGameServerGroupName "Use generic-lens or generic-optics with 'gameServerGroupName' instead." #-}

-- | The unique identifier for the instance where the game server is running. This ID is available in the instance metadata. EC2 instance IDs use a 17-character format, for example: @i-1234567890abcdef0@ .
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsiInstanceId :: Lens.Lens' GameServerInstance (Core.Maybe Types.GameServerInstanceId)
gsiInstanceId = Lens.field @"instanceId"
{-# DEPRECATED gsiInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | Current status of the game server instance.
--
--
--     * __ACTIVE__ -- The instance is viable for hosting game servers.
--
--
--     * __DRAINING__ -- The instance is not viable for hosting game servers. Existing game servers are in the process of ending, and new game servers are not started on this instance unless no other resources are available. When the instance is put in DRAINING, a new instance is started up to replace it. Once the instance has no UTILIZED game servers, it will be terminated in favor of the new instance.
--
--
--     * __SPOT_TERMINATING__ -- The instance is in the process of shutting down due to a Spot instance interruption. No new game servers are started on this instance.
--
--
--
-- /Note:/ Consider using 'instanceStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsiInstanceStatus :: Lens.Lens' GameServerInstance (Core.Maybe Types.GameServerInstanceStatus)
gsiInstanceStatus = Lens.field @"instanceStatus"
{-# DEPRECATED gsiInstanceStatus "Use generic-lens or generic-optics with 'instanceStatus' instead." #-}

instance Core.FromJSON GameServerInstance where
  parseJSON =
    Core.withObject "GameServerInstance" Core.$
      \x ->
        GameServerInstance'
          Core.<$> (x Core..:? "GameServerGroupArn")
          Core.<*> (x Core..:? "GameServerGroupName")
          Core.<*> (x Core..:? "InstanceId")
          Core.<*> (x Core..:? "InstanceStatus")
