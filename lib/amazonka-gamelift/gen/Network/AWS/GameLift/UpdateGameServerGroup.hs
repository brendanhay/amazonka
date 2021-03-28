{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.UpdateGameServerGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- __This operation is used with the Amazon GameLift FleetIQ solution and game server groups.__ 
--
-- Updates GameLift FleetIQ-specific properties for a game server group. Many Auto Scaling group properties are updated on the Auto Scaling group directly, including the launch template, Auto Scaling policies, and maximum/minimum/desired instance counts.
-- To update the game server group, specify the game server group ID and provide the updated values. Before applying the updates, the new values are validated to ensure that GameLift FleetIQ can continue to perform instance balancing activity. If successful, a 'GameServerGroup' object is returned.
-- __Learn more__ 
-- <https://docs.aws.amazon.com/gamelift/latest/fleetiqguide/gsg-intro.html GameLift FleetIQ Guide> 
-- __Related operations__ 
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
module Network.AWS.GameLift.UpdateGameServerGroup
    (
    -- * Creating a request
      UpdateGameServerGroup (..)
    , mkUpdateGameServerGroup
    -- ** Request lenses
    , ugsgGameServerGroupName
    , ugsgBalancingStrategy
    , ugsgGameServerProtectionPolicy
    , ugsgInstanceDefinitions
    , ugsgRoleArn

    -- * Destructuring the response
    , UpdateGameServerGroupResponse (..)
    , mkUpdateGameServerGroupResponse
    -- ** Response lenses
    , ugsgrrsGameServerGroup
    , ugsgrrsResponseStatus
    ) where

import qualified Network.AWS.GameLift.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateGameServerGroup' smart constructor.
data UpdateGameServerGroup = UpdateGameServerGroup'
  { gameServerGroupName :: Types.GameServerGroupName
    -- ^ A unique identifier for the game server group. Use either the 'GameServerGroup' name or ARN value.
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
  , gameServerProtectionPolicy :: Core.Maybe Types.GameServerProtectionPolicy
    -- ^ A flag that indicates whether instances in the game server group are protected from early termination. Unprotected instances that have active game servers running might be terminated during a scale-down event, causing players to be dropped from the game. Protected instances cannot be terminated while there are active game servers running except in the event of a forced game server group deletion (see ). An exception to this is with Spot Instances, which can be terminated by AWS regardless of protection status. This property is set to @NO_PROTECTION@ by default.
  , instanceDefinitions :: Core.Maybe (Core.NonEmpty Types.InstanceDefinition)
    -- ^ An updated list of EC2 instance types to use in the Auto Scaling group. The instance definitions must specify at least two different instance types that are supported by GameLift FleetIQ. This updated list replaces the entire current list of instance definitions for the game server group. For more information on instance types, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html EC2 Instance Types> in the /Amazon EC2 User Guide/ . You can optionally specify capacity weighting for each instance type. If no weight value is specified for an instance type, it is set to the default value "1". For more information about capacity weighting, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/asg-instance-weighting.html Instance Weighting for Amazon EC2 Auto Scaling> in the Amazon EC2 Auto Scaling User Guide.
  , roleArn :: Core.Maybe Types.IamRoleArn
    -- ^ The Amazon Resource Name (<https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-arn-format.html ARN> ) for an IAM role that allows Amazon GameLift to access your EC2 Auto Scaling groups.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateGameServerGroup' value with any optional fields omitted.
mkUpdateGameServerGroup
    :: Types.GameServerGroupName -- ^ 'gameServerGroupName'
    -> UpdateGameServerGroup
mkUpdateGameServerGroup gameServerGroupName
  = UpdateGameServerGroup'{gameServerGroupName,
                           balancingStrategy = Core.Nothing,
                           gameServerProtectionPolicy = Core.Nothing,
                           instanceDefinitions = Core.Nothing, roleArn = Core.Nothing}

-- | A unique identifier for the game server group. Use either the 'GameServerGroup' name or ARN value.
--
-- /Note:/ Consider using 'gameServerGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugsgGameServerGroupName :: Lens.Lens' UpdateGameServerGroup Types.GameServerGroupName
ugsgGameServerGroupName = Lens.field @"gameServerGroupName"
{-# INLINEABLE ugsgGameServerGroupName #-}
{-# DEPRECATED gameServerGroupName "Use generic-lens or generic-optics with 'gameServerGroupName' instead"  #-}

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
ugsgBalancingStrategy :: Lens.Lens' UpdateGameServerGroup (Core.Maybe Types.BalancingStrategy)
ugsgBalancingStrategy = Lens.field @"balancingStrategy"
{-# INLINEABLE ugsgBalancingStrategy #-}
{-# DEPRECATED balancingStrategy "Use generic-lens or generic-optics with 'balancingStrategy' instead"  #-}

-- | A flag that indicates whether instances in the game server group are protected from early termination. Unprotected instances that have active game servers running might be terminated during a scale-down event, causing players to be dropped from the game. Protected instances cannot be terminated while there are active game servers running except in the event of a forced game server group deletion (see ). An exception to this is with Spot Instances, which can be terminated by AWS regardless of protection status. This property is set to @NO_PROTECTION@ by default.
--
-- /Note:/ Consider using 'gameServerProtectionPolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugsgGameServerProtectionPolicy :: Lens.Lens' UpdateGameServerGroup (Core.Maybe Types.GameServerProtectionPolicy)
ugsgGameServerProtectionPolicy = Lens.field @"gameServerProtectionPolicy"
{-# INLINEABLE ugsgGameServerProtectionPolicy #-}
{-# DEPRECATED gameServerProtectionPolicy "Use generic-lens or generic-optics with 'gameServerProtectionPolicy' instead"  #-}

-- | An updated list of EC2 instance types to use in the Auto Scaling group. The instance definitions must specify at least two different instance types that are supported by GameLift FleetIQ. This updated list replaces the entire current list of instance definitions for the game server group. For more information on instance types, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html EC2 Instance Types> in the /Amazon EC2 User Guide/ . You can optionally specify capacity weighting for each instance type. If no weight value is specified for an instance type, it is set to the default value "1". For more information about capacity weighting, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/asg-instance-weighting.html Instance Weighting for Amazon EC2 Auto Scaling> in the Amazon EC2 Auto Scaling User Guide.
--
-- /Note:/ Consider using 'instanceDefinitions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugsgInstanceDefinitions :: Lens.Lens' UpdateGameServerGroup (Core.Maybe (Core.NonEmpty Types.InstanceDefinition))
ugsgInstanceDefinitions = Lens.field @"instanceDefinitions"
{-# INLINEABLE ugsgInstanceDefinitions #-}
{-# DEPRECATED instanceDefinitions "Use generic-lens or generic-optics with 'instanceDefinitions' instead"  #-}

-- | The Amazon Resource Name (<https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-arn-format.html ARN> ) for an IAM role that allows Amazon GameLift to access your EC2 Auto Scaling groups.
--
-- /Note:/ Consider using 'roleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugsgRoleArn :: Lens.Lens' UpdateGameServerGroup (Core.Maybe Types.IamRoleArn)
ugsgRoleArn = Lens.field @"roleArn"
{-# INLINEABLE ugsgRoleArn #-}
{-# DEPRECATED roleArn "Use generic-lens or generic-optics with 'roleArn' instead"  #-}

instance Core.ToQuery UpdateGameServerGroup where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateGameServerGroup where
        toHeaders UpdateGameServerGroup{..}
          = Core.pure ("X-Amz-Target", "GameLift.UpdateGameServerGroup")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON UpdateGameServerGroup where
        toJSON UpdateGameServerGroup{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("GameServerGroupName" Core..= gameServerGroupName),
                  ("BalancingStrategy" Core..=) Core.<$> balancingStrategy,
                  ("GameServerProtectionPolicy" Core..=) Core.<$>
                    gameServerProtectionPolicy,
                  ("InstanceDefinitions" Core..=) Core.<$> instanceDefinitions,
                  ("RoleArn" Core..=) Core.<$> roleArn])

instance Core.AWSRequest UpdateGameServerGroup where
        type Rs UpdateGameServerGroup = UpdateGameServerGroupResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 UpdateGameServerGroupResponse' Core.<$>
                   (x Core..:? "GameServerGroup") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkUpdateGameServerGroupResponse' smart constructor.
data UpdateGameServerGroupResponse = UpdateGameServerGroupResponse'
  { gameServerGroup :: Core.Maybe Types.GameServerGroup
    -- ^ An object that describes the game server group resource with updated properties. 
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'UpdateGameServerGroupResponse' value with any optional fields omitted.
mkUpdateGameServerGroupResponse
    :: Core.Int -- ^ 'responseStatus'
    -> UpdateGameServerGroupResponse
mkUpdateGameServerGroupResponse responseStatus
  = UpdateGameServerGroupResponse'{gameServerGroup = Core.Nothing,
                                   responseStatus}

-- | An object that describes the game server group resource with updated properties. 
--
-- /Note:/ Consider using 'gameServerGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugsgrrsGameServerGroup :: Lens.Lens' UpdateGameServerGroupResponse (Core.Maybe Types.GameServerGroup)
ugsgrrsGameServerGroup = Lens.field @"gameServerGroup"
{-# INLINEABLE ugsgrrsGameServerGroup #-}
{-# DEPRECATED gameServerGroup "Use generic-lens or generic-optics with 'gameServerGroup' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugsgrrsResponseStatus :: Lens.Lens' UpdateGameServerGroupResponse Core.Int
ugsgrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ugsgrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
