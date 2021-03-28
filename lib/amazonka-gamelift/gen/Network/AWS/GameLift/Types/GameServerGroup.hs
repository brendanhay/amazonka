{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types.GameServerGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.GameLift.Types.GameServerGroup
  ( GameServerGroup (..)
  -- * Smart constructor
  , mkGameServerGroup
  -- * Lenses
  , gsgAutoScalingGroupArn
  , gsgBalancingStrategy
  , gsgCreationTime
  , gsgGameServerGroupArn
  , gsgGameServerGroupName
  , gsgGameServerProtectionPolicy
  , gsgInstanceDefinitions
  , gsgLastUpdatedTime
  , gsgRoleArn
  , gsgStatus
  , gsgStatusReason
  , gsgSuspendedActions
  ) where

import qualified Network.AWS.GameLift.Types.AutoScalingGroupArn as Types
import qualified Network.AWS.GameLift.Types.BalancingStrategy as Types
import qualified Network.AWS.GameLift.Types.GameServerGroupAction as Types
import qualified Network.AWS.GameLift.Types.GameServerGroupArn as Types
import qualified Network.AWS.GameLift.Types.GameServerGroupName as Types
import qualified Network.AWS.GameLift.Types.GameServerGroupStatus as Types
import qualified Network.AWS.GameLift.Types.GameServerProtectionPolicy as Types
import qualified Network.AWS.GameLift.Types.IamRoleArn as Types
import qualified Network.AWS.GameLift.Types.InstanceDefinition as Types
import qualified Network.AWS.GameLift.Types.NonZeroAndMaxString as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

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
  { autoScalingGroupArn :: Core.Maybe Types.AutoScalingGroupArn
    -- ^ A generated unique ID for the EC2 Auto Scaling group that is associated with this game server group.
  , balancingStrategy :: Core.Maybe Types.BalancingStrategy
    -- ^ Indicates how GameLift FleetIQ balances the use of Spot Instances and On-Demand Instances in the game server group. Method options include the following:
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
  , creationTime :: Core.Maybe Core.NominalDiffTime
    -- ^ A timestamp that indicates when this data object was created. Format is a number expressed in Unix time as milliseconds (for example @"1469498468.057"@ ).
  , gameServerGroupArn :: Core.Maybe Types.GameServerGroupArn
    -- ^ A generated unique ID for the game server group.
  , gameServerGroupName :: Core.Maybe Types.GameServerGroupName
    -- ^ A developer-defined identifier for the game server group. The name is unique for each Region in each AWS account.
  , gameServerProtectionPolicy :: Core.Maybe Types.GameServerProtectionPolicy
    -- ^ A flag that indicates whether instances in the game server group are protected from early termination. Unprotected instances that have active game servers running might be terminated during a scale-down event, causing players to be dropped from the game. Protected instances cannot be terminated while there are active game servers running except in the event of a forced game server group deletion (see ). An exception to this is with Spot Instances, which can be terminated by AWS regardless of protection status. 
  , instanceDefinitions :: Core.Maybe (Core.NonEmpty Types.InstanceDefinition)
    -- ^ The set of EC2 instance types that GameLift FleetIQ can use when balancing and automatically scaling instances in the corresponding Auto Scaling group. 
  , lastUpdatedTime :: Core.Maybe Core.NominalDiffTime
    -- ^ A timestamp that indicates when this game server group was last updated.
  , roleArn :: Core.Maybe Types.IamRoleArn
    -- ^ The Amazon Resource Name (<https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-arn-format.html ARN> ) for an IAM role that allows Amazon GameLift to access your EC2 Auto Scaling groups.
  , status :: Core.Maybe Types.GameServerGroupStatus
    -- ^ The current status of the game server group. Possible statuses include:
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
  , statusReason :: Core.Maybe Types.NonZeroAndMaxString
    -- ^ Additional information about the current game server group status. This information might provide additional insight on groups that are in @ERROR@ status.
  , suspendedActions :: Core.Maybe (Core.NonEmpty Types.GameServerGroupAction)
    -- ^ A list of activities that are currently suspended for this game server group. If this property is empty, all activities are occurring.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'GameServerGroup' value with any optional fields omitted.
mkGameServerGroup
    :: GameServerGroup
mkGameServerGroup
  = GameServerGroup'{autoScalingGroupArn = Core.Nothing,
                     balancingStrategy = Core.Nothing, creationTime = Core.Nothing,
                     gameServerGroupArn = Core.Nothing,
                     gameServerGroupName = Core.Nothing,
                     gameServerProtectionPolicy = Core.Nothing,
                     instanceDefinitions = Core.Nothing, lastUpdatedTime = Core.Nothing,
                     roleArn = Core.Nothing, status = Core.Nothing,
                     statusReason = Core.Nothing, suspendedActions = Core.Nothing}

-- | A generated unique ID for the EC2 Auto Scaling group that is associated with this game server group.
--
-- /Note:/ Consider using 'autoScalingGroupArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsgAutoScalingGroupArn :: Lens.Lens' GameServerGroup (Core.Maybe Types.AutoScalingGroupArn)
gsgAutoScalingGroupArn = Lens.field @"autoScalingGroupArn"
{-# INLINEABLE gsgAutoScalingGroupArn #-}
{-# DEPRECATED autoScalingGroupArn "Use generic-lens or generic-optics with 'autoScalingGroupArn' instead"  #-}

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
gsgBalancingStrategy :: Lens.Lens' GameServerGroup (Core.Maybe Types.BalancingStrategy)
gsgBalancingStrategy = Lens.field @"balancingStrategy"
{-# INLINEABLE gsgBalancingStrategy #-}
{-# DEPRECATED balancingStrategy "Use generic-lens or generic-optics with 'balancingStrategy' instead"  #-}

-- | A timestamp that indicates when this data object was created. Format is a number expressed in Unix time as milliseconds (for example @"1469498468.057"@ ).
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsgCreationTime :: Lens.Lens' GameServerGroup (Core.Maybe Core.NominalDiffTime)
gsgCreationTime = Lens.field @"creationTime"
{-# INLINEABLE gsgCreationTime #-}
{-# DEPRECATED creationTime "Use generic-lens or generic-optics with 'creationTime' instead"  #-}

-- | A generated unique ID for the game server group.
--
-- /Note:/ Consider using 'gameServerGroupArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsgGameServerGroupArn :: Lens.Lens' GameServerGroup (Core.Maybe Types.GameServerGroupArn)
gsgGameServerGroupArn = Lens.field @"gameServerGroupArn"
{-# INLINEABLE gsgGameServerGroupArn #-}
{-# DEPRECATED gameServerGroupArn "Use generic-lens or generic-optics with 'gameServerGroupArn' instead"  #-}

-- | A developer-defined identifier for the game server group. The name is unique for each Region in each AWS account.
--
-- /Note:/ Consider using 'gameServerGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsgGameServerGroupName :: Lens.Lens' GameServerGroup (Core.Maybe Types.GameServerGroupName)
gsgGameServerGroupName = Lens.field @"gameServerGroupName"
{-# INLINEABLE gsgGameServerGroupName #-}
{-# DEPRECATED gameServerGroupName "Use generic-lens or generic-optics with 'gameServerGroupName' instead"  #-}

-- | A flag that indicates whether instances in the game server group are protected from early termination. Unprotected instances that have active game servers running might be terminated during a scale-down event, causing players to be dropped from the game. Protected instances cannot be terminated while there are active game servers running except in the event of a forced game server group deletion (see ). An exception to this is with Spot Instances, which can be terminated by AWS regardless of protection status. 
--
-- /Note:/ Consider using 'gameServerProtectionPolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsgGameServerProtectionPolicy :: Lens.Lens' GameServerGroup (Core.Maybe Types.GameServerProtectionPolicy)
gsgGameServerProtectionPolicy = Lens.field @"gameServerProtectionPolicy"
{-# INLINEABLE gsgGameServerProtectionPolicy #-}
{-# DEPRECATED gameServerProtectionPolicy "Use generic-lens or generic-optics with 'gameServerProtectionPolicy' instead"  #-}

-- | The set of EC2 instance types that GameLift FleetIQ can use when balancing and automatically scaling instances in the corresponding Auto Scaling group. 
--
-- /Note:/ Consider using 'instanceDefinitions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsgInstanceDefinitions :: Lens.Lens' GameServerGroup (Core.Maybe (Core.NonEmpty Types.InstanceDefinition))
gsgInstanceDefinitions = Lens.field @"instanceDefinitions"
{-# INLINEABLE gsgInstanceDefinitions #-}
{-# DEPRECATED instanceDefinitions "Use generic-lens or generic-optics with 'instanceDefinitions' instead"  #-}

-- | A timestamp that indicates when this game server group was last updated.
--
-- /Note:/ Consider using 'lastUpdatedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsgLastUpdatedTime :: Lens.Lens' GameServerGroup (Core.Maybe Core.NominalDiffTime)
gsgLastUpdatedTime = Lens.field @"lastUpdatedTime"
{-# INLINEABLE gsgLastUpdatedTime #-}
{-# DEPRECATED lastUpdatedTime "Use generic-lens or generic-optics with 'lastUpdatedTime' instead"  #-}

-- | The Amazon Resource Name (<https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-arn-format.html ARN> ) for an IAM role that allows Amazon GameLift to access your EC2 Auto Scaling groups.
--
-- /Note:/ Consider using 'roleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsgRoleArn :: Lens.Lens' GameServerGroup (Core.Maybe Types.IamRoleArn)
gsgRoleArn = Lens.field @"roleArn"
{-# INLINEABLE gsgRoleArn #-}
{-# DEPRECATED roleArn "Use generic-lens or generic-optics with 'roleArn' instead"  #-}

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
gsgStatus :: Lens.Lens' GameServerGroup (Core.Maybe Types.GameServerGroupStatus)
gsgStatus = Lens.field @"status"
{-# INLINEABLE gsgStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

-- | Additional information about the current game server group status. This information might provide additional insight on groups that are in @ERROR@ status.
--
-- /Note:/ Consider using 'statusReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsgStatusReason :: Lens.Lens' GameServerGroup (Core.Maybe Types.NonZeroAndMaxString)
gsgStatusReason = Lens.field @"statusReason"
{-# INLINEABLE gsgStatusReason #-}
{-# DEPRECATED statusReason "Use generic-lens or generic-optics with 'statusReason' instead"  #-}

-- | A list of activities that are currently suspended for this game server group. If this property is empty, all activities are occurring.
--
-- /Note:/ Consider using 'suspendedActions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsgSuspendedActions :: Lens.Lens' GameServerGroup (Core.Maybe (Core.NonEmpty Types.GameServerGroupAction))
gsgSuspendedActions = Lens.field @"suspendedActions"
{-# INLINEABLE gsgSuspendedActions #-}
{-# DEPRECATED suspendedActions "Use generic-lens or generic-optics with 'suspendedActions' instead"  #-}

instance Core.FromJSON GameServerGroup where
        parseJSON
          = Core.withObject "GameServerGroup" Core.$
              \ x ->
                GameServerGroup' Core.<$>
                  (x Core..:? "AutoScalingGroupArn") Core.<*>
                    x Core..:? "BalancingStrategy"
                    Core.<*> x Core..:? "CreationTime"
                    Core.<*> x Core..:? "GameServerGroupArn"
                    Core.<*> x Core..:? "GameServerGroupName"
                    Core.<*> x Core..:? "GameServerProtectionPolicy"
                    Core.<*> x Core..:? "InstanceDefinitions"
                    Core.<*> x Core..:? "LastUpdatedTime"
                    Core.<*> x Core..:? "RoleArn"
                    Core.<*> x Core..:? "Status"
                    Core.<*> x Core..:? "StatusReason"
                    Core.<*> x Core..:? "SuspendedActions"
