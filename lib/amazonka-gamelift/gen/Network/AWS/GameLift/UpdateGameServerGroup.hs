{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
module Network.AWS.GameLift.UpdateGameServerGroup
  ( -- * Creating a request
    UpdateGameServerGroup (..),
    mkUpdateGameServerGroup,

    -- ** Request lenses
    ugsgInstanceDefinitions,
    ugsgBalancingStrategy,
    ugsgGameServerProtectionPolicy,
    ugsgRoleARN,
    ugsgGameServerGroupName,

    -- * Destructuring the response
    UpdateGameServerGroupResponse (..),
    mkUpdateGameServerGroupResponse,

    -- ** Response lenses
    ugsgrsGameServerGroup,
    ugsgrsResponseStatus,
  )
where

import Network.AWS.GameLift.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateGameServerGroup' smart constructor.
data UpdateGameServerGroup = UpdateGameServerGroup'
  { instanceDefinitions ::
      Lude.Maybe (Lude.NonEmpty InstanceDefinition),
    balancingStrategy ::
      Lude.Maybe BalancingStrategy,
    gameServerProtectionPolicy ::
      Lude.Maybe GameServerProtectionPolicy,
    roleARN :: Lude.Maybe Lude.Text,
    gameServerGroupName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateGameServerGroup' with the minimum fields required to make a request.
--
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
-- * 'gameServerGroupName' - A unique identifier for the game server group. Use either the 'GameServerGroup' name or ARN value.
-- * 'gameServerProtectionPolicy' - A flag that indicates whether instances in the game server group are protected from early termination. Unprotected instances that have active game servers running might be terminated during a scale-down event, causing players to be dropped from the game. Protected instances cannot be terminated while there are active game servers running except in the event of a forced game server group deletion (see ). An exception to this is with Spot Instances, which can be terminated by AWS regardless of protection status. This property is set to @NO_PROTECTION@ by default.
-- * 'instanceDefinitions' - An updated list of EC2 instance types to use in the Auto Scaling group. The instance definitions must specify at least two different instance types that are supported by GameLift FleetIQ. This updated list replaces the entire current list of instance definitions for the game server group. For more information on instance types, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html EC2 Instance Types> in the /Amazon EC2 User Guide/ . You can optionally specify capacity weighting for each instance type. If no weight value is specified for an instance type, it is set to the default value "1". For more information about capacity weighting, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/asg-instance-weighting.html Instance Weighting for Amazon EC2 Auto Scaling> in the Amazon EC2 Auto Scaling User Guide.
-- * 'roleARN' - The Amazon Resource Name (<https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-arn-format.html ARN> ) for an IAM role that allows Amazon GameLift to access your EC2 Auto Scaling groups.
mkUpdateGameServerGroup ::
  -- | 'gameServerGroupName'
  Lude.Text ->
  UpdateGameServerGroup
mkUpdateGameServerGroup pGameServerGroupName_ =
  UpdateGameServerGroup'
    { instanceDefinitions = Lude.Nothing,
      balancingStrategy = Lude.Nothing,
      gameServerProtectionPolicy = Lude.Nothing,
      roleARN = Lude.Nothing,
      gameServerGroupName = pGameServerGroupName_
    }

-- | An updated list of EC2 instance types to use in the Auto Scaling group. The instance definitions must specify at least two different instance types that are supported by GameLift FleetIQ. This updated list replaces the entire current list of instance definitions for the game server group. For more information on instance types, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html EC2 Instance Types> in the /Amazon EC2 User Guide/ . You can optionally specify capacity weighting for each instance type. If no weight value is specified for an instance type, it is set to the default value "1". For more information about capacity weighting, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/asg-instance-weighting.html Instance Weighting for Amazon EC2 Auto Scaling> in the Amazon EC2 Auto Scaling User Guide.
--
-- /Note:/ Consider using 'instanceDefinitions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugsgInstanceDefinitions :: Lens.Lens' UpdateGameServerGroup (Lude.Maybe (Lude.NonEmpty InstanceDefinition))
ugsgInstanceDefinitions = Lens.lens (instanceDefinitions :: UpdateGameServerGroup -> Lude.Maybe (Lude.NonEmpty InstanceDefinition)) (\s a -> s {instanceDefinitions = a} :: UpdateGameServerGroup)
{-# DEPRECATED ugsgInstanceDefinitions "Use generic-lens or generic-optics with 'instanceDefinitions' instead." #-}

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
ugsgBalancingStrategy :: Lens.Lens' UpdateGameServerGroup (Lude.Maybe BalancingStrategy)
ugsgBalancingStrategy = Lens.lens (balancingStrategy :: UpdateGameServerGroup -> Lude.Maybe BalancingStrategy) (\s a -> s {balancingStrategy = a} :: UpdateGameServerGroup)
{-# DEPRECATED ugsgBalancingStrategy "Use generic-lens or generic-optics with 'balancingStrategy' instead." #-}

-- | A flag that indicates whether instances in the game server group are protected from early termination. Unprotected instances that have active game servers running might be terminated during a scale-down event, causing players to be dropped from the game. Protected instances cannot be terminated while there are active game servers running except in the event of a forced game server group deletion (see ). An exception to this is with Spot Instances, which can be terminated by AWS regardless of protection status. This property is set to @NO_PROTECTION@ by default.
--
-- /Note:/ Consider using 'gameServerProtectionPolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugsgGameServerProtectionPolicy :: Lens.Lens' UpdateGameServerGroup (Lude.Maybe GameServerProtectionPolicy)
ugsgGameServerProtectionPolicy = Lens.lens (gameServerProtectionPolicy :: UpdateGameServerGroup -> Lude.Maybe GameServerProtectionPolicy) (\s a -> s {gameServerProtectionPolicy = a} :: UpdateGameServerGroup)
{-# DEPRECATED ugsgGameServerProtectionPolicy "Use generic-lens or generic-optics with 'gameServerProtectionPolicy' instead." #-}

-- | The Amazon Resource Name (<https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-arn-format.html ARN> ) for an IAM role that allows Amazon GameLift to access your EC2 Auto Scaling groups.
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugsgRoleARN :: Lens.Lens' UpdateGameServerGroup (Lude.Maybe Lude.Text)
ugsgRoleARN = Lens.lens (roleARN :: UpdateGameServerGroup -> Lude.Maybe Lude.Text) (\s a -> s {roleARN = a} :: UpdateGameServerGroup)
{-# DEPRECATED ugsgRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

-- | A unique identifier for the game server group. Use either the 'GameServerGroup' name or ARN value.
--
-- /Note:/ Consider using 'gameServerGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugsgGameServerGroupName :: Lens.Lens' UpdateGameServerGroup Lude.Text
ugsgGameServerGroupName = Lens.lens (gameServerGroupName :: UpdateGameServerGroup -> Lude.Text) (\s a -> s {gameServerGroupName = a} :: UpdateGameServerGroup)
{-# DEPRECATED ugsgGameServerGroupName "Use generic-lens or generic-optics with 'gameServerGroupName' instead." #-}

instance Lude.AWSRequest UpdateGameServerGroup where
  type Rs UpdateGameServerGroup = UpdateGameServerGroupResponse
  request = Req.postJSON gameLiftService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateGameServerGroupResponse'
            Lude.<$> (x Lude..?> "GameServerGroup")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateGameServerGroup where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("GameLift.UpdateGameServerGroup" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateGameServerGroup where
  toJSON UpdateGameServerGroup' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("InstanceDefinitions" Lude..=) Lude.<$> instanceDefinitions,
            ("BalancingStrategy" Lude..=) Lude.<$> balancingStrategy,
            ("GameServerProtectionPolicy" Lude..=)
              Lude.<$> gameServerProtectionPolicy,
            ("RoleArn" Lude..=) Lude.<$> roleARN,
            Lude.Just ("GameServerGroupName" Lude..= gameServerGroupName)
          ]
      )

instance Lude.ToPath UpdateGameServerGroup where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateGameServerGroup where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateGameServerGroupResponse' smart constructor.
data UpdateGameServerGroupResponse = UpdateGameServerGroupResponse'
  { gameServerGroup ::
      Lude.Maybe GameServerGroup,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateGameServerGroupResponse' with the minimum fields required to make a request.
--
-- * 'gameServerGroup' - An object that describes the game server group resource with updated properties.
-- * 'responseStatus' - The response status code.
mkUpdateGameServerGroupResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateGameServerGroupResponse
mkUpdateGameServerGroupResponse pResponseStatus_ =
  UpdateGameServerGroupResponse'
    { gameServerGroup = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An object that describes the game server group resource with updated properties.
--
-- /Note:/ Consider using 'gameServerGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugsgrsGameServerGroup :: Lens.Lens' UpdateGameServerGroupResponse (Lude.Maybe GameServerGroup)
ugsgrsGameServerGroup = Lens.lens (gameServerGroup :: UpdateGameServerGroupResponse -> Lude.Maybe GameServerGroup) (\s a -> s {gameServerGroup = a} :: UpdateGameServerGroupResponse)
{-# DEPRECATED ugsgrsGameServerGroup "Use generic-lens or generic-optics with 'gameServerGroup' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugsgrsResponseStatus :: Lens.Lens' UpdateGameServerGroupResponse Lude.Int
ugsgrsResponseStatus = Lens.lens (responseStatus :: UpdateGameServerGroupResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateGameServerGroupResponse)
{-# DEPRECATED ugsgrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
