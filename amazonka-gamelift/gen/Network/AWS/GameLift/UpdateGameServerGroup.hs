{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.UpdateGameServerGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- __This operation is used with the Amazon GameLift FleetIQ solution and
-- game server groups.__
--
-- Updates GameLift FleetIQ-specific properties for a game server group.
-- Many Auto Scaling group properties are updated on the Auto Scaling group
-- directly, including the launch template, Auto Scaling policies, and
-- maximum\/minimum\/desired instance counts.
--
-- To update the game server group, specify the game server group ID and
-- provide the updated values. Before applying the updates, the new values
-- are validated to ensure that GameLift FleetIQ can continue to perform
-- instance balancing activity. If successful, a GameServerGroup object is
-- returned.
--
-- __Learn more__
--
-- <https://docs.aws.amazon.com/gamelift/latest/fleetiqguide/gsg-intro.html GameLift FleetIQ Guide>
--
-- __Related operations__
--
-- -   CreateGameServerGroup
--
-- -   ListGameServerGroups
--
-- -   DescribeGameServerGroup
--
-- -   UpdateGameServerGroup
--
-- -   DeleteGameServerGroup
--
-- -   ResumeGameServerGroup
--
-- -   SuspendGameServerGroup
--
-- -   DescribeGameServerInstances
module Network.AWS.GameLift.UpdateGameServerGroup
  ( -- * Creating a Request
    UpdateGameServerGroup (..),
    newUpdateGameServerGroup,

    -- * Request Lenses
    updateGameServerGroup_roleArn,
    updateGameServerGroup_instanceDefinitions,
    updateGameServerGroup_balancingStrategy,
    updateGameServerGroup_gameServerProtectionPolicy,
    updateGameServerGroup_gameServerGroupName,

    -- * Destructuring the Response
    UpdateGameServerGroupResponse (..),
    newUpdateGameServerGroupResponse,

    -- * Response Lenses
    updateGameServerGroupResponse_gameServerGroup,
    updateGameServerGroupResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.GameLift.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateGameServerGroup' smart constructor.
data UpdateGameServerGroup = UpdateGameServerGroup'
  { -- | The Amazon Resource Name
    -- (<https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-arn-format.html ARN>)
    -- for an IAM role that allows Amazon GameLift to access your EC2 Auto
    -- Scaling groups.
    roleArn :: Prelude.Maybe Prelude.Text,
    -- | An updated list of EC2 instance types to use in the Auto Scaling group.
    -- The instance definitions must specify at least two different instance
    -- types that are supported by GameLift FleetIQ. This updated list replaces
    -- the entire current list of instance definitions for the game server
    -- group. For more information on instance types, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html EC2 Instance Types>
    -- in the /Amazon EC2 User Guide/. You can optionally specify capacity
    -- weighting for each instance type. If no weight value is specified for an
    -- instance type, it is set to the default value \"1\". For more
    -- information about capacity weighting, see
    -- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/asg-instance-weighting.html Instance Weighting for Amazon EC2 Auto Scaling>
    -- in the Amazon EC2 Auto Scaling User Guide.
    instanceDefinitions :: Prelude.Maybe (Prelude.NonEmpty InstanceDefinition),
    -- | Indicates how GameLift FleetIQ balances the use of Spot Instances and
    -- On-Demand Instances in the game server group. Method options include the
    -- following:
    --
    -- -   @SPOT_ONLY@ - Only Spot Instances are used in the game server group.
    --     If Spot Instances are unavailable or not viable for game hosting,
    --     the game server group provides no hosting capacity until Spot
    --     Instances can again be used. Until then, no new instances are
    --     started, and the existing nonviable Spot Instances are terminated
    --     (after current gameplay ends) and are not replaced.
    --
    -- -   @SPOT_PREFERRED@ - (default value) Spot Instances are used whenever
    --     available in the game server group. If Spot Instances are
    --     unavailable, the game server group continues to provide hosting
    --     capacity by falling back to On-Demand Instances. Existing nonviable
    --     Spot Instances are terminated (after current gameplay ends) and are
    --     replaced with new On-Demand Instances.
    --
    -- -   @ON_DEMAND_ONLY@ - Only On-Demand Instances are used in the game
    --     server group. No Spot Instances are used, even when available, while
    --     this balancing strategy is in force.
    balancingStrategy :: Prelude.Maybe BalancingStrategy,
    -- | A flag that indicates whether instances in the game server group are
    -- protected from early termination. Unprotected instances that have active
    -- game servers running might be terminated during a scale-down event,
    -- causing players to be dropped from the game. Protected instances cannot
    -- be terminated while there are active game servers running except in the
    -- event of a forced game server group deletion (see ). An exception to
    -- this is with Spot Instances, which can be terminated by AWS regardless
    -- of protection status. This property is set to @NO_PROTECTION@ by
    -- default.
    gameServerProtectionPolicy :: Prelude.Maybe GameServerProtectionPolicy,
    -- | A unique identifier for the game server group. Use either the
    -- GameServerGroup name or ARN value.
    gameServerGroupName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateGameServerGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'roleArn', 'updateGameServerGroup_roleArn' - The Amazon Resource Name
-- (<https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-arn-format.html ARN>)
-- for an IAM role that allows Amazon GameLift to access your EC2 Auto
-- Scaling groups.
--
-- 'instanceDefinitions', 'updateGameServerGroup_instanceDefinitions' - An updated list of EC2 instance types to use in the Auto Scaling group.
-- The instance definitions must specify at least two different instance
-- types that are supported by GameLift FleetIQ. This updated list replaces
-- the entire current list of instance definitions for the game server
-- group. For more information on instance types, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html EC2 Instance Types>
-- in the /Amazon EC2 User Guide/. You can optionally specify capacity
-- weighting for each instance type. If no weight value is specified for an
-- instance type, it is set to the default value \"1\". For more
-- information about capacity weighting, see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/asg-instance-weighting.html Instance Weighting for Amazon EC2 Auto Scaling>
-- in the Amazon EC2 Auto Scaling User Guide.
--
-- 'balancingStrategy', 'updateGameServerGroup_balancingStrategy' - Indicates how GameLift FleetIQ balances the use of Spot Instances and
-- On-Demand Instances in the game server group. Method options include the
-- following:
--
-- -   @SPOT_ONLY@ - Only Spot Instances are used in the game server group.
--     If Spot Instances are unavailable or not viable for game hosting,
--     the game server group provides no hosting capacity until Spot
--     Instances can again be used. Until then, no new instances are
--     started, and the existing nonviable Spot Instances are terminated
--     (after current gameplay ends) and are not replaced.
--
-- -   @SPOT_PREFERRED@ - (default value) Spot Instances are used whenever
--     available in the game server group. If Spot Instances are
--     unavailable, the game server group continues to provide hosting
--     capacity by falling back to On-Demand Instances. Existing nonviable
--     Spot Instances are terminated (after current gameplay ends) and are
--     replaced with new On-Demand Instances.
--
-- -   @ON_DEMAND_ONLY@ - Only On-Demand Instances are used in the game
--     server group. No Spot Instances are used, even when available, while
--     this balancing strategy is in force.
--
-- 'gameServerProtectionPolicy', 'updateGameServerGroup_gameServerProtectionPolicy' - A flag that indicates whether instances in the game server group are
-- protected from early termination. Unprotected instances that have active
-- game servers running might be terminated during a scale-down event,
-- causing players to be dropped from the game. Protected instances cannot
-- be terminated while there are active game servers running except in the
-- event of a forced game server group deletion (see ). An exception to
-- this is with Spot Instances, which can be terminated by AWS regardless
-- of protection status. This property is set to @NO_PROTECTION@ by
-- default.
--
-- 'gameServerGroupName', 'updateGameServerGroup_gameServerGroupName' - A unique identifier for the game server group. Use either the
-- GameServerGroup name or ARN value.
newUpdateGameServerGroup ::
  -- | 'gameServerGroupName'
  Prelude.Text ->
  UpdateGameServerGroup
newUpdateGameServerGroup pGameServerGroupName_ =
  UpdateGameServerGroup'
    { roleArn = Prelude.Nothing,
      instanceDefinitions = Prelude.Nothing,
      balancingStrategy = Prelude.Nothing,
      gameServerProtectionPolicy = Prelude.Nothing,
      gameServerGroupName = pGameServerGroupName_
    }

-- | The Amazon Resource Name
-- (<https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-arn-format.html ARN>)
-- for an IAM role that allows Amazon GameLift to access your EC2 Auto
-- Scaling groups.
updateGameServerGroup_roleArn :: Lens.Lens' UpdateGameServerGroup (Prelude.Maybe Prelude.Text)
updateGameServerGroup_roleArn = Lens.lens (\UpdateGameServerGroup' {roleArn} -> roleArn) (\s@UpdateGameServerGroup' {} a -> s {roleArn = a} :: UpdateGameServerGroup)

-- | An updated list of EC2 instance types to use in the Auto Scaling group.
-- The instance definitions must specify at least two different instance
-- types that are supported by GameLift FleetIQ. This updated list replaces
-- the entire current list of instance definitions for the game server
-- group. For more information on instance types, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html EC2 Instance Types>
-- in the /Amazon EC2 User Guide/. You can optionally specify capacity
-- weighting for each instance type. If no weight value is specified for an
-- instance type, it is set to the default value \"1\". For more
-- information about capacity weighting, see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/asg-instance-weighting.html Instance Weighting for Amazon EC2 Auto Scaling>
-- in the Amazon EC2 Auto Scaling User Guide.
updateGameServerGroup_instanceDefinitions :: Lens.Lens' UpdateGameServerGroup (Prelude.Maybe (Prelude.NonEmpty InstanceDefinition))
updateGameServerGroup_instanceDefinitions = Lens.lens (\UpdateGameServerGroup' {instanceDefinitions} -> instanceDefinitions) (\s@UpdateGameServerGroup' {} a -> s {instanceDefinitions = a} :: UpdateGameServerGroup) Prelude.. Lens.mapping Lens._Coerce

-- | Indicates how GameLift FleetIQ balances the use of Spot Instances and
-- On-Demand Instances in the game server group. Method options include the
-- following:
--
-- -   @SPOT_ONLY@ - Only Spot Instances are used in the game server group.
--     If Spot Instances are unavailable or not viable for game hosting,
--     the game server group provides no hosting capacity until Spot
--     Instances can again be used. Until then, no new instances are
--     started, and the existing nonviable Spot Instances are terminated
--     (after current gameplay ends) and are not replaced.
--
-- -   @SPOT_PREFERRED@ - (default value) Spot Instances are used whenever
--     available in the game server group. If Spot Instances are
--     unavailable, the game server group continues to provide hosting
--     capacity by falling back to On-Demand Instances. Existing nonviable
--     Spot Instances are terminated (after current gameplay ends) and are
--     replaced with new On-Demand Instances.
--
-- -   @ON_DEMAND_ONLY@ - Only On-Demand Instances are used in the game
--     server group. No Spot Instances are used, even when available, while
--     this balancing strategy is in force.
updateGameServerGroup_balancingStrategy :: Lens.Lens' UpdateGameServerGroup (Prelude.Maybe BalancingStrategy)
updateGameServerGroup_balancingStrategy = Lens.lens (\UpdateGameServerGroup' {balancingStrategy} -> balancingStrategy) (\s@UpdateGameServerGroup' {} a -> s {balancingStrategy = a} :: UpdateGameServerGroup)

-- | A flag that indicates whether instances in the game server group are
-- protected from early termination. Unprotected instances that have active
-- game servers running might be terminated during a scale-down event,
-- causing players to be dropped from the game. Protected instances cannot
-- be terminated while there are active game servers running except in the
-- event of a forced game server group deletion (see ). An exception to
-- this is with Spot Instances, which can be terminated by AWS regardless
-- of protection status. This property is set to @NO_PROTECTION@ by
-- default.
updateGameServerGroup_gameServerProtectionPolicy :: Lens.Lens' UpdateGameServerGroup (Prelude.Maybe GameServerProtectionPolicy)
updateGameServerGroup_gameServerProtectionPolicy = Lens.lens (\UpdateGameServerGroup' {gameServerProtectionPolicy} -> gameServerProtectionPolicy) (\s@UpdateGameServerGroup' {} a -> s {gameServerProtectionPolicy = a} :: UpdateGameServerGroup)

-- | A unique identifier for the game server group. Use either the
-- GameServerGroup name or ARN value.
updateGameServerGroup_gameServerGroupName :: Lens.Lens' UpdateGameServerGroup Prelude.Text
updateGameServerGroup_gameServerGroupName = Lens.lens (\UpdateGameServerGroup' {gameServerGroupName} -> gameServerGroupName) (\s@UpdateGameServerGroup' {} a -> s {gameServerGroupName = a} :: UpdateGameServerGroup)

instance Core.AWSRequest UpdateGameServerGroup where
  type
    AWSResponse UpdateGameServerGroup =
      UpdateGameServerGroupResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateGameServerGroupResponse'
            Prelude.<$> (x Core..?> "GameServerGroup")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateGameServerGroup

instance Prelude.NFData UpdateGameServerGroup

instance Core.ToHeaders UpdateGameServerGroup where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "GameLift.UpdateGameServerGroup" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateGameServerGroup where
  toJSON UpdateGameServerGroup' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("RoleArn" Core..=) Prelude.<$> roleArn,
            ("InstanceDefinitions" Core..=)
              Prelude.<$> instanceDefinitions,
            ("BalancingStrategy" Core..=)
              Prelude.<$> balancingStrategy,
            ("GameServerProtectionPolicy" Core..=)
              Prelude.<$> gameServerProtectionPolicy,
            Prelude.Just
              ("GameServerGroupName" Core..= gameServerGroupName)
          ]
      )

instance Core.ToPath UpdateGameServerGroup where
  toPath = Prelude.const "/"

instance Core.ToQuery UpdateGameServerGroup where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateGameServerGroupResponse' smart constructor.
data UpdateGameServerGroupResponse = UpdateGameServerGroupResponse'
  { -- | An object that describes the game server group resource with updated
    -- properties.
    gameServerGroup :: Prelude.Maybe GameServerGroup,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateGameServerGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'gameServerGroup', 'updateGameServerGroupResponse_gameServerGroup' - An object that describes the game server group resource with updated
-- properties.
--
-- 'httpStatus', 'updateGameServerGroupResponse_httpStatus' - The response's http status code.
newUpdateGameServerGroupResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateGameServerGroupResponse
newUpdateGameServerGroupResponse pHttpStatus_ =
  UpdateGameServerGroupResponse'
    { gameServerGroup =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An object that describes the game server group resource with updated
-- properties.
updateGameServerGroupResponse_gameServerGroup :: Lens.Lens' UpdateGameServerGroupResponse (Prelude.Maybe GameServerGroup)
updateGameServerGroupResponse_gameServerGroup = Lens.lens (\UpdateGameServerGroupResponse' {gameServerGroup} -> gameServerGroup) (\s@UpdateGameServerGroupResponse' {} a -> s {gameServerGroup = a} :: UpdateGameServerGroupResponse)

-- | The response's http status code.
updateGameServerGroupResponse_httpStatus :: Lens.Lens' UpdateGameServerGroupResponse Prelude.Int
updateGameServerGroupResponse_httpStatus = Lens.lens (\UpdateGameServerGroupResponse' {httpStatus} -> httpStatus) (\s@UpdateGameServerGroupResponse' {} a -> s {httpStatus = a} :: UpdateGameServerGroupResponse)

instance Prelude.NFData UpdateGameServerGroupResponse
