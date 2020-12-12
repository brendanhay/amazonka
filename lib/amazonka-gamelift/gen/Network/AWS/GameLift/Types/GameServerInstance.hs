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
    gsiInstanceId,
    gsiGameServerGroupName,
    gsiInstanceStatus,
    gsiGameServerGroupARN,
  )
where

import Network.AWS.GameLift.Types.GameServerInstanceStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

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
  { instanceId ::
      Lude.Maybe Lude.Text,
    gameServerGroupName :: Lude.Maybe Lude.Text,
    instanceStatus :: Lude.Maybe GameServerInstanceStatus,
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

-- | Creates a value of 'GameServerInstance' with the minimum fields required to make a request.
--
-- * 'gameServerGroupARN' - A generated unique identifier for the game server group that includes the game server instance.
-- * 'gameServerGroupName' - A developer-defined identifier for the game server group that includes the game server instance. The name is unique for each Region in each AWS account.
-- * 'instanceId' - The unique identifier for the instance where the game server is running. This ID is available in the instance metadata. EC2 instance IDs use a 17-character format, for example: @i-1234567890abcdef0@ .
-- * 'instanceStatus' - Current status of the game server instance.
--
--
--     * __ACTIVE__ -- The instance is viable for hosting game servers.
--
--
--     * __DRAINING__ -- The instance is not viable for hosting game servers. Existing game servers are in the process of ending, and new game servers are not started on this instance unless no other resources are available. When the instance is put in DRAINING, a new instance is started up to replace it. Once the instance has no UTILIZED game servers, it will be terminated in favor of the new instance.
--
--
--     * __SPOT_TERMINATING__ -- The instance is in the process of shutting down due to a Spot instance interruption. No new game servers are started on this instance.
mkGameServerInstance ::
  GameServerInstance
mkGameServerInstance =
  GameServerInstance'
    { instanceId = Lude.Nothing,
      gameServerGroupName = Lude.Nothing,
      instanceStatus = Lude.Nothing,
      gameServerGroupARN = Lude.Nothing
    }

-- | The unique identifier for the instance where the game server is running. This ID is available in the instance metadata. EC2 instance IDs use a 17-character format, for example: @i-1234567890abcdef0@ .
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsiInstanceId :: Lens.Lens' GameServerInstance (Lude.Maybe Lude.Text)
gsiInstanceId = Lens.lens (instanceId :: GameServerInstance -> Lude.Maybe Lude.Text) (\s a -> s {instanceId = a} :: GameServerInstance)
{-# DEPRECATED gsiInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | A developer-defined identifier for the game server group that includes the game server instance. The name is unique for each Region in each AWS account.
--
-- /Note:/ Consider using 'gameServerGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsiGameServerGroupName :: Lens.Lens' GameServerInstance (Lude.Maybe Lude.Text)
gsiGameServerGroupName = Lens.lens (gameServerGroupName :: GameServerInstance -> Lude.Maybe Lude.Text) (\s a -> s {gameServerGroupName = a} :: GameServerInstance)
{-# DEPRECATED gsiGameServerGroupName "Use generic-lens or generic-optics with 'gameServerGroupName' instead." #-}

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
gsiInstanceStatus :: Lens.Lens' GameServerInstance (Lude.Maybe GameServerInstanceStatus)
gsiInstanceStatus = Lens.lens (instanceStatus :: GameServerInstance -> Lude.Maybe GameServerInstanceStatus) (\s a -> s {instanceStatus = a} :: GameServerInstance)
{-# DEPRECATED gsiInstanceStatus "Use generic-lens or generic-optics with 'instanceStatus' instead." #-}

-- | A generated unique identifier for the game server group that includes the game server instance.
--
-- /Note:/ Consider using 'gameServerGroupARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsiGameServerGroupARN :: Lens.Lens' GameServerInstance (Lude.Maybe Lude.Text)
gsiGameServerGroupARN = Lens.lens (gameServerGroupARN :: GameServerInstance -> Lude.Maybe Lude.Text) (\s a -> s {gameServerGroupARN = a} :: GameServerInstance)
{-# DEPRECATED gsiGameServerGroupARN "Use generic-lens or generic-optics with 'gameServerGroupARN' instead." #-}

instance Lude.FromJSON GameServerInstance where
  parseJSON =
    Lude.withObject
      "GameServerInstance"
      ( \x ->
          GameServerInstance'
            Lude.<$> (x Lude..:? "InstanceId")
            Lude.<*> (x Lude..:? "GameServerGroupName")
            Lude.<*> (x Lude..:? "InstanceStatus")
            Lude.<*> (x Lude..:? "GameServerGroupArn")
      )
