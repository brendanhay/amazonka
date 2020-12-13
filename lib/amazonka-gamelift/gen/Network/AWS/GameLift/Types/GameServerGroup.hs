{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types.GameServerGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GameLift.Types.GameServerGroup
  ( GameServerGroup (..),

    -- * Smart constructor
    mkGameServerGroup,

    -- * Lenses
    gsgCreationTime,
    gsgStatus,
    gsgInstanceDefinitions,
    gsgLastUpdatedTime,
    gsgBalancingStrategy,
    gsgGameServerGroupName,
    gsgSuspendedActions,
    gsgAutoScalingGroupARN,
    gsgStatusReason,
    gsgGameServerProtectionPolicy,
    gsgGameServerGroupARN,
    gsgRoleARN,
  )
where

import Network.AWS.GameLift.Types.BalancingStrategy
import Network.AWS.GameLift.Types.GameServerGroupAction
import Network.AWS.GameLift.Types.GameServerGroupStatus
import Network.AWS.GameLift.Types.GameServerProtectionPolicy
import Network.AWS.GameLift.Types.InstanceDefinition
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | __This data type is used with the Amazon GameLift FleetIQ and game server groups.__
--
-- Properties that describe a game server group resource. A game server group manages certain properties related to a corresponding EC2 Auto Scaling group.
-- A game server group is created by a successful call to @CreateGameServerGroup@ and deleted by calling @DeleteGameServerGroup@ . Game server group activity can be temporarily suspended and resumed by calling @SuspendGameServerGroup@ and @ResumeGameServerGroup@ , respectively.
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
-- /See:/ 'mkGameServerGroup' smart constructor.
data GameServerGroup = GameServerGroup'
  { -- | A timestamp that indicates when this data object was created. Format is a number expressed in Unix time as milliseconds (for example @"1469498468.057"@ ).
    creationTime :: Lude.Maybe Lude.Timestamp,
    -- | The current status of the game server group. Possible statuses include:
    --
    --
    --     * @NEW@ - GameLift FleetIQ has validated the @CreateGameServerGroup()@ request.
    --
    --
    --     * @ACTIVATING@ - GameLift FleetIQ is setting up a game server group, which includes creating an Auto Scaling group in your AWS account.
    --
    --
    --     * @ACTIVE@ - The game server group has been successfully created.
    --
    --
    --     * @DELETE_SCHEDULED@ - A request to delete the game server group has been received.
    --
    --
    --     * @DELETING@ - GameLift FleetIQ has received a valid @DeleteGameServerGroup()@ request and is processing it. GameLift FleetIQ must first complete and release hosts before it deletes the Auto Scaling group and the game server group.
    --
    --
    --     * @DELETED@ - The game server group has been successfully deleted.
    --
    --
    --     * @ERROR@ - The asynchronous processes of activating or deleting a game server group has failed, resulting in an error state.
    status :: Lude.Maybe GameServerGroupStatus,
    -- | The set of EC2 instance types that GameLift FleetIQ can use when balancing and automatically scaling instances in the corresponding Auto Scaling group.
    instanceDefinitions :: Lude.Maybe (Lude.NonEmpty InstanceDefinition),
    -- | A timestamp that indicates when this game server group was last updated.
    lastUpdatedTime :: Lude.Maybe Lude.Timestamp,
    -- | Indicates how GameLift FleetIQ balances the use of Spot Instances and On-Demand Instances in the game server group. Method options include the following:
    --
    --
    --     * @SPOT_ONLY@ - Only Spot Instances are used in the game server group. If Spot Instances are unavailable or not viable for game hosting, the game server group provides no hosting capacity until Spot Instances can again be used. Until then, no new instances are started, and the existing nonviable Spot Instances are terminated (after current gameplay ends) and are not replaced.
    --
    --
    --     * @SPOT_PREFERRED@ - (default value) Spot Instances are used whenever available in the game server group. If Spot Instances are unavailable, the game server group continues to provide hosting capacity by falling back to On-Demand Instances. Existing nonviable Spot Instances are terminated (after current gameplay ends) and are replaced with new On-Demand Instances.
    --
    --
    --     * @ON_DEMAND_ONLY@ - Only On-Demand Instances are used in the game server group. No Spot Instances are used, even when available, while this balancing strategy is in force.
    balancingStrategy :: Lude.Maybe BalancingStrategy,
    -- | A developer-defined identifier for the game server group. The name is unique for each Region in each AWS account.
    gameServerGroupName :: Lude.Maybe Lude.Text,
    -- | A list of activities that are currently suspended for this game server group. If this property is empty, all activities are occurring.
    suspendedActions :: Lude.Maybe (Lude.NonEmpty GameServerGroupAction),
    -- | A generated unique ID for the EC2 Auto Scaling group that is associated with this game server group.
    autoScalingGroupARN :: Lude.Maybe Lude.Text,
    -- | Additional information about the current game server group status. This information might provide additional insight on groups that are in @ERROR@ status.
    statusReason :: Lude.Maybe Lude.Text,
    -- | A flag that indicates whether instances in the game server group are protected from early termination. Unprotected instances that have active game servers running might be terminated during a scale-down event, causing players to be dropped from the game. Protected instances cannot be terminated while there are active game servers running except in the event of a forced game server group deletion (see ). An exception to this is with Spot Instances, which can be terminated by AWS regardless of protection status.
    gameServerProtectionPolicy :: Lude.Maybe GameServerProtectionPolicy,
    -- | A generated unique ID for the game server group.
    gameServerGroupARN :: Lude.Maybe Lude.Text,
    -- | The Amazon Resource Name (<https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-arn-format.html ARN> ) for an IAM role that allows Amazon GameLift to access your EC2 Auto Scaling groups.
    roleARN :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GameServerGroup' with the minimum fields required to make a request.
--
-- * 'creationTime' - A timestamp that indicates when this data object was created. Format is a number expressed in Unix time as milliseconds (for example @"1469498468.057"@ ).
-- * 'status' - The current status of the game server group. Possible statuses include:
--
--
--     * @NEW@ - GameLift FleetIQ has validated the @CreateGameServerGroup()@ request.
--
--
--     * @ACTIVATING@ - GameLift FleetIQ is setting up a game server group, which includes creating an Auto Scaling group in your AWS account.
--
--
--     * @ACTIVE@ - The game server group has been successfully created.
--
--
--     * @DELETE_SCHEDULED@ - A request to delete the game server group has been received.
--
--
--     * @DELETING@ - GameLift FleetIQ has received a valid @DeleteGameServerGroup()@ request and is processing it. GameLift FleetIQ must first complete and release hosts before it deletes the Auto Scaling group and the game server group.
--
--
--     * @DELETED@ - The game server group has been successfully deleted.
--
--
--     * @ERROR@ - The asynchronous processes of activating or deleting a game server group has failed, resulting in an error state.
--
--
-- * 'instanceDefinitions' - The set of EC2 instance types that GameLift FleetIQ can use when balancing and automatically scaling instances in the corresponding Auto Scaling group.
-- * 'lastUpdatedTime' - A timestamp that indicates when this game server group was last updated.
-- * 'balancingStrategy' - Indicates how GameLift FleetIQ balances the use of Spot Instances and On-Demand Instances in the game server group. Method options include the following:
--
--
--     * @SPOT_ONLY@ - Only Spot Instances are used in the game server group. If Spot Instances are unavailable or not viable for game hosting, the game server group provides no hosting capacity until Spot Instances can again be used. Until then, no new instances are started, and the existing nonviable Spot Instances are terminated (after current gameplay ends) and are not replaced.
--
--
--     * @SPOT_PREFERRED@ - (default value) Spot Instances are used whenever available in the game server group. If Spot Instances are unavailable, the game server group continues to provide hosting capacity by falling back to On-Demand Instances. Existing nonviable Spot Instances are terminated (after current gameplay ends) and are replaced with new On-Demand Instances.
--
--
--     * @ON_DEMAND_ONLY@ - Only On-Demand Instances are used in the game server group. No Spot Instances are used, even when available, while this balancing strategy is in force.
--
--
-- * 'gameServerGroupName' - A developer-defined identifier for the game server group. The name is unique for each Region in each AWS account.
-- * 'suspendedActions' - A list of activities that are currently suspended for this game server group. If this property is empty, all activities are occurring.
-- * 'autoScalingGroupARN' - A generated unique ID for the EC2 Auto Scaling group that is associated with this game server group.
-- * 'statusReason' - Additional information about the current game server group status. This information might provide additional insight on groups that are in @ERROR@ status.
-- * 'gameServerProtectionPolicy' - A flag that indicates whether instances in the game server group are protected from early termination. Unprotected instances that have active game servers running might be terminated during a scale-down event, causing players to be dropped from the game. Protected instances cannot be terminated while there are active game servers running except in the event of a forced game server group deletion (see ). An exception to this is with Spot Instances, which can be terminated by AWS regardless of protection status.
-- * 'gameServerGroupARN' - A generated unique ID for the game server group.
-- * 'roleARN' - The Amazon Resource Name (<https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-arn-format.html ARN> ) for an IAM role that allows Amazon GameLift to access your EC2 Auto Scaling groups.
mkGameServerGroup ::
  GameServerGroup
mkGameServerGroup =
  GameServerGroup'
    { creationTime = Lude.Nothing,
      status = Lude.Nothing,
      instanceDefinitions = Lude.Nothing,
      lastUpdatedTime = Lude.Nothing,
      balancingStrategy = Lude.Nothing,
      gameServerGroupName = Lude.Nothing,
      suspendedActions = Lude.Nothing,
      autoScalingGroupARN = Lude.Nothing,
      statusReason = Lude.Nothing,
      gameServerProtectionPolicy = Lude.Nothing,
      gameServerGroupARN = Lude.Nothing,
      roleARN = Lude.Nothing
    }

-- | A timestamp that indicates when this data object was created. Format is a number expressed in Unix time as milliseconds (for example @"1469498468.057"@ ).
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsgCreationTime :: Lens.Lens' GameServerGroup (Lude.Maybe Lude.Timestamp)
gsgCreationTime = Lens.lens (creationTime :: GameServerGroup -> Lude.Maybe Lude.Timestamp) (\s a -> s {creationTime = a} :: GameServerGroup)
{-# DEPRECATED gsgCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | The current status of the game server group. Possible statuses include:
--
--
--     * @NEW@ - GameLift FleetIQ has validated the @CreateGameServerGroup()@ request.
--
--
--     * @ACTIVATING@ - GameLift FleetIQ is setting up a game server group, which includes creating an Auto Scaling group in your AWS account.
--
--
--     * @ACTIVE@ - The game server group has been successfully created.
--
--
--     * @DELETE_SCHEDULED@ - A request to delete the game server group has been received.
--
--
--     * @DELETING@ - GameLift FleetIQ has received a valid @DeleteGameServerGroup()@ request and is processing it. GameLift FleetIQ must first complete and release hosts before it deletes the Auto Scaling group and the game server group.
--
--
--     * @DELETED@ - The game server group has been successfully deleted.
--
--
--     * @ERROR@ - The asynchronous processes of activating or deleting a game server group has failed, resulting in an error state.
--
--
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsgStatus :: Lens.Lens' GameServerGroup (Lude.Maybe GameServerGroupStatus)
gsgStatus = Lens.lens (status :: GameServerGroup -> Lude.Maybe GameServerGroupStatus) (\s a -> s {status = a} :: GameServerGroup)
{-# DEPRECATED gsgStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The set of EC2 instance types that GameLift FleetIQ can use when balancing and automatically scaling instances in the corresponding Auto Scaling group.
--
-- /Note:/ Consider using 'instanceDefinitions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsgInstanceDefinitions :: Lens.Lens' GameServerGroup (Lude.Maybe (Lude.NonEmpty InstanceDefinition))
gsgInstanceDefinitions = Lens.lens (instanceDefinitions :: GameServerGroup -> Lude.Maybe (Lude.NonEmpty InstanceDefinition)) (\s a -> s {instanceDefinitions = a} :: GameServerGroup)
{-# DEPRECATED gsgInstanceDefinitions "Use generic-lens or generic-optics with 'instanceDefinitions' instead." #-}

-- | A timestamp that indicates when this game server group was last updated.
--
-- /Note:/ Consider using 'lastUpdatedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsgLastUpdatedTime :: Lens.Lens' GameServerGroup (Lude.Maybe Lude.Timestamp)
gsgLastUpdatedTime = Lens.lens (lastUpdatedTime :: GameServerGroup -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastUpdatedTime = a} :: GameServerGroup)
{-# DEPRECATED gsgLastUpdatedTime "Use generic-lens or generic-optics with 'lastUpdatedTime' instead." #-}

-- | Indicates how GameLift FleetIQ balances the use of Spot Instances and On-Demand Instances in the game server group. Method options include the following:
--
--
--     * @SPOT_ONLY@ - Only Spot Instances are used in the game server group. If Spot Instances are unavailable or not viable for game hosting, the game server group provides no hosting capacity until Spot Instances can again be used. Until then, no new instances are started, and the existing nonviable Spot Instances are terminated (after current gameplay ends) and are not replaced.
--
--
--     * @SPOT_PREFERRED@ - (default value) Spot Instances are used whenever available in the game server group. If Spot Instances are unavailable, the game server group continues to provide hosting capacity by falling back to On-Demand Instances. Existing nonviable Spot Instances are terminated (after current gameplay ends) and are replaced with new On-Demand Instances.
--
--
--     * @ON_DEMAND_ONLY@ - Only On-Demand Instances are used in the game server group. No Spot Instances are used, even when available, while this balancing strategy is in force.
--
--
--
-- /Note:/ Consider using 'balancingStrategy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsgBalancingStrategy :: Lens.Lens' GameServerGroup (Lude.Maybe BalancingStrategy)
gsgBalancingStrategy = Lens.lens (balancingStrategy :: GameServerGroup -> Lude.Maybe BalancingStrategy) (\s a -> s {balancingStrategy = a} :: GameServerGroup)
{-# DEPRECATED gsgBalancingStrategy "Use generic-lens or generic-optics with 'balancingStrategy' instead." #-}

-- | A developer-defined identifier for the game server group. The name is unique for each Region in each AWS account.
--
-- /Note:/ Consider using 'gameServerGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsgGameServerGroupName :: Lens.Lens' GameServerGroup (Lude.Maybe Lude.Text)
gsgGameServerGroupName = Lens.lens (gameServerGroupName :: GameServerGroup -> Lude.Maybe Lude.Text) (\s a -> s {gameServerGroupName = a} :: GameServerGroup)
{-# DEPRECATED gsgGameServerGroupName "Use generic-lens or generic-optics with 'gameServerGroupName' instead." #-}

-- | A list of activities that are currently suspended for this game server group. If this property is empty, all activities are occurring.
--
-- /Note:/ Consider using 'suspendedActions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsgSuspendedActions :: Lens.Lens' GameServerGroup (Lude.Maybe (Lude.NonEmpty GameServerGroupAction))
gsgSuspendedActions = Lens.lens (suspendedActions :: GameServerGroup -> Lude.Maybe (Lude.NonEmpty GameServerGroupAction)) (\s a -> s {suspendedActions = a} :: GameServerGroup)
{-# DEPRECATED gsgSuspendedActions "Use generic-lens or generic-optics with 'suspendedActions' instead." #-}

-- | A generated unique ID for the EC2 Auto Scaling group that is associated with this game server group.
--
-- /Note:/ Consider using 'autoScalingGroupARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsgAutoScalingGroupARN :: Lens.Lens' GameServerGroup (Lude.Maybe Lude.Text)
gsgAutoScalingGroupARN = Lens.lens (autoScalingGroupARN :: GameServerGroup -> Lude.Maybe Lude.Text) (\s a -> s {autoScalingGroupARN = a} :: GameServerGroup)
{-# DEPRECATED gsgAutoScalingGroupARN "Use generic-lens or generic-optics with 'autoScalingGroupARN' instead." #-}

-- | Additional information about the current game server group status. This information might provide additional insight on groups that are in @ERROR@ status.
--
-- /Note:/ Consider using 'statusReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsgStatusReason :: Lens.Lens' GameServerGroup (Lude.Maybe Lude.Text)
gsgStatusReason = Lens.lens (statusReason :: GameServerGroup -> Lude.Maybe Lude.Text) (\s a -> s {statusReason = a} :: GameServerGroup)
{-# DEPRECATED gsgStatusReason "Use generic-lens or generic-optics with 'statusReason' instead." #-}

-- | A flag that indicates whether instances in the game server group are protected from early termination. Unprotected instances that have active game servers running might be terminated during a scale-down event, causing players to be dropped from the game. Protected instances cannot be terminated while there are active game servers running except in the event of a forced game server group deletion (see ). An exception to this is with Spot Instances, which can be terminated by AWS regardless of protection status.
--
-- /Note:/ Consider using 'gameServerProtectionPolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsgGameServerProtectionPolicy :: Lens.Lens' GameServerGroup (Lude.Maybe GameServerProtectionPolicy)
gsgGameServerProtectionPolicy = Lens.lens (gameServerProtectionPolicy :: GameServerGroup -> Lude.Maybe GameServerProtectionPolicy) (\s a -> s {gameServerProtectionPolicy = a} :: GameServerGroup)
{-# DEPRECATED gsgGameServerProtectionPolicy "Use generic-lens or generic-optics with 'gameServerProtectionPolicy' instead." #-}

-- | A generated unique ID for the game server group.
--
-- /Note:/ Consider using 'gameServerGroupARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsgGameServerGroupARN :: Lens.Lens' GameServerGroup (Lude.Maybe Lude.Text)
gsgGameServerGroupARN = Lens.lens (gameServerGroupARN :: GameServerGroup -> Lude.Maybe Lude.Text) (\s a -> s {gameServerGroupARN = a} :: GameServerGroup)
{-# DEPRECATED gsgGameServerGroupARN "Use generic-lens or generic-optics with 'gameServerGroupARN' instead." #-}

-- | The Amazon Resource Name (<https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-arn-format.html ARN> ) for an IAM role that allows Amazon GameLift to access your EC2 Auto Scaling groups.
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsgRoleARN :: Lens.Lens' GameServerGroup (Lude.Maybe Lude.Text)
gsgRoleARN = Lens.lens (roleARN :: GameServerGroup -> Lude.Maybe Lude.Text) (\s a -> s {roleARN = a} :: GameServerGroup)
{-# DEPRECATED gsgRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

instance Lude.FromJSON GameServerGroup where
  parseJSON =
    Lude.withObject
      "GameServerGroup"
      ( \x ->
          GameServerGroup'
            Lude.<$> (x Lude..:? "CreationTime")
            Lude.<*> (x Lude..:? "Status")
            Lude.<*> (x Lude..:? "InstanceDefinitions")
            Lude.<*> (x Lude..:? "LastUpdatedTime")
            Lude.<*> (x Lude..:? "BalancingStrategy")
            Lude.<*> (x Lude..:? "GameServerGroupName")
            Lude.<*> (x Lude..:? "SuspendedActions")
            Lude.<*> (x Lude..:? "AutoScalingGroupArn")
            Lude.<*> (x Lude..:? "StatusReason")
            Lude.<*> (x Lude..:? "GameServerProtectionPolicy")
            Lude.<*> (x Lude..:? "GameServerGroupArn")
            Lude.<*> (x Lude..:? "RoleArn")
      )
