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
-- Module      : Amazonka.GameLift.CreateGameServerGroup
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- __This operation is used with the GameLift FleetIQ solution and game
-- server groups.__
--
-- Creates a GameLift FleetIQ game server group for managing game hosting
-- on a collection of Amazon Elastic Compute Cloud instances for game
-- hosting. This operation creates the game server group, creates an Auto
-- Scaling group in your Amazon Web Services account, and establishes a
-- link between the two groups. You can view the status of your game server
-- groups in the GameLift console. Game server group metrics and events are
-- emitted to Amazon CloudWatch.
--
-- Before creating a new game server group, you must have the following:
--
-- -   An Amazon Elastic Compute Cloud launch template that specifies how
--     to launch Amazon Elastic Compute Cloud instances with your game
--     server build. For more information, see
--     <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-launch-templates.html Launching an Instance from a Launch Template>
--     in the /Amazon Elastic Compute Cloud User Guide/.
--
-- -   An IAM role that extends limited access to your Amazon Web Services
--     account to allow GameLift FleetIQ to create and interact with the
--     Auto Scaling group. For more information, see
--     <https://docs.aws.amazon.com/gamelift/latest/fleetiqguide/gsg-iam-permissions-roles.html Create IAM roles for cross-service interaction>
--     in the /GameLift FleetIQ Developer Guide/.
--
-- To create a new game server group, specify a unique group name, IAM role
-- and Amazon Elastic Compute Cloud launch template, and provide a list of
-- instance types that can be used in the group. You must also set initial
-- maximum and minimum limits on the group\'s instance count. You can
-- optionally set an Auto Scaling policy with target tracking based on a
-- GameLift FleetIQ metric.
--
-- Once the game server group and corresponding Auto Scaling group are
-- created, you have full access to change the Auto Scaling group\'s
-- configuration as needed. Several properties that are set when creating a
-- game server group, including maximum\/minimum size and auto-scaling
-- policy settings, must be updated directly in the Auto Scaling group.
-- Keep in mind that some Auto Scaling group properties are periodically
-- updated by GameLift FleetIQ as part of its balancing activities to
-- optimize for availability and cost.
--
-- __Learn more__
--
-- <https://docs.aws.amazon.com/gamelift/latest/fleetiqguide/gsg-intro.html GameLift FleetIQ Guide>
module Amazonka.GameLift.CreateGameServerGroup
  ( -- * Creating a Request
    CreateGameServerGroup (..),
    newCreateGameServerGroup,

    -- * Request Lenses
    createGameServerGroup_autoScalingPolicy,
    createGameServerGroup_balancingStrategy,
    createGameServerGroup_gameServerProtectionPolicy,
    createGameServerGroup_tags,
    createGameServerGroup_vpcSubnets,
    createGameServerGroup_gameServerGroupName,
    createGameServerGroup_roleArn,
    createGameServerGroup_minSize,
    createGameServerGroup_maxSize,
    createGameServerGroup_launchTemplate,
    createGameServerGroup_instanceDefinitions,

    -- * Destructuring the Response
    CreateGameServerGroupResponse (..),
    newCreateGameServerGroupResponse,

    -- * Response Lenses
    createGameServerGroupResponse_gameServerGroup,
    createGameServerGroupResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GameLift.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateGameServerGroup' smart constructor.
data CreateGameServerGroup = CreateGameServerGroup'
  { -- | Configuration settings to define a scaling policy for the Auto Scaling
    -- group that is optimized for game hosting. The scaling policy uses the
    -- metric @\"PercentUtilizedGameServers\"@ to maintain a buffer of idle
    -- game servers that can immediately accommodate new games and players.
    -- After the Auto Scaling group is created, update this value directly in
    -- the Auto Scaling group using the Amazon Web Services console or APIs.
    autoScalingPolicy :: Prelude.Maybe GameServerGroupAutoScalingPolicy,
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
    -- this is with Spot Instances, which can be terminated by Amazon Web
    -- Services regardless of protection status. This property is set to
    -- @NO_PROTECTION@ by default.
    gameServerProtectionPolicy :: Prelude.Maybe GameServerProtectionPolicy,
    -- | A list of labels to assign to the new game server group resource. Tags
    -- are developer-defined key-value pairs. Tagging Amazon Web Services
    -- resources is useful for resource management, access management, and cost
    -- allocation. For more information, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging Amazon Web Services Resources>
    -- in the /Amazon Web Services General Reference/.
    tags :: Prelude.Maybe [Tag],
    -- | A list of virtual private cloud (VPC) subnets to use with instances in
    -- the game server group. By default, all GameLift FleetIQ-supported
    -- Availability Zones are used. You can use this parameter to specify VPCs
    -- that you\'ve set up. This property cannot be updated after the game
    -- server group is created, and the corresponding Auto Scaling group will
    -- always use the property value that is set with this request, even if the
    -- Auto Scaling group is updated directly.
    vpcSubnets :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | An identifier for the new game server group. This value is used to
    -- generate unique ARN identifiers for the Amazon EC2 Auto Scaling group
    -- and the GameLift FleetIQ game server group. The name must be unique per
    -- Region per Amazon Web Services account.
    gameServerGroupName :: Prelude.Text,
    -- | The Amazon Resource Name
    -- (<https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-arn-format.html ARN>)
    -- for an IAM role that allows Amazon GameLift to access your Amazon EC2
    -- Auto Scaling groups.
    roleArn :: Prelude.Text,
    -- | The minimum number of instances allowed in the Amazon EC2 Auto Scaling
    -- group. During automatic scaling events, GameLift FleetIQ and Amazon EC2
    -- do not scale down the group below this minimum. In production, this
    -- value should be set to at least 1. After the Auto Scaling group is
    -- created, update this value directly in the Auto Scaling group using the
    -- Amazon Web Services console or APIs.
    minSize :: Prelude.Natural,
    -- | The maximum number of instances allowed in the Amazon EC2 Auto Scaling
    -- group. During automatic scaling events, GameLift FleetIQ and EC2 do not
    -- scale up the group above this maximum. After the Auto Scaling group is
    -- created, update this value directly in the Auto Scaling group using the
    -- Amazon Web Services console or APIs.
    maxSize :: Prelude.Natural,
    -- | The Amazon EC2 launch template that contains configuration settings and
    -- game server code to be deployed to all instances in the game server
    -- group. You can specify the template using either the template name or
    -- ID. For help with creating a launch template, see
    -- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/create-launch-template.html Creating a Launch Template for an Auto Scaling Group>
    -- in the /Amazon Elastic Compute Cloud Auto Scaling User Guide/. After the
    -- Auto Scaling group is created, update this value directly in the Auto
    -- Scaling group using the Amazon Web Services console or APIs.
    --
    -- If you specify network interfaces in your launch template, you must
    -- explicitly set the property @AssociatePublicIpAddress@ to \"true\". If
    -- no network interface is specified in the launch template, GameLift
    -- FleetIQ uses your account\'s default VPC.
    launchTemplate :: LaunchTemplateSpecification,
    -- | The Amazon EC2 instance types and sizes to use in the Auto Scaling
    -- group. The instance definitions must specify at least two different
    -- instance types that are supported by GameLift FleetIQ. For more
    -- information on instance types, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html EC2 Instance Types>
    -- in the /Amazon Elastic Compute Cloud User Guide/. You can optionally
    -- specify capacity weighting for each instance type. If no weight value is
    -- specified for an instance type, it is set to the default value \"1\".
    -- For more information about capacity weighting, see
    -- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/asg-instance-weighting.html Instance Weighting for Amazon EC2 Auto Scaling>
    -- in the Amazon EC2 Auto Scaling User Guide.
    instanceDefinitions :: Prelude.NonEmpty InstanceDefinition
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateGameServerGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'autoScalingPolicy', 'createGameServerGroup_autoScalingPolicy' - Configuration settings to define a scaling policy for the Auto Scaling
-- group that is optimized for game hosting. The scaling policy uses the
-- metric @\"PercentUtilizedGameServers\"@ to maintain a buffer of idle
-- game servers that can immediately accommodate new games and players.
-- After the Auto Scaling group is created, update this value directly in
-- the Auto Scaling group using the Amazon Web Services console or APIs.
--
-- 'balancingStrategy', 'createGameServerGroup_balancingStrategy' - Indicates how GameLift FleetIQ balances the use of Spot Instances and
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
-- 'gameServerProtectionPolicy', 'createGameServerGroup_gameServerProtectionPolicy' - A flag that indicates whether instances in the game server group are
-- protected from early termination. Unprotected instances that have active
-- game servers running might be terminated during a scale-down event,
-- causing players to be dropped from the game. Protected instances cannot
-- be terminated while there are active game servers running except in the
-- event of a forced game server group deletion (see ). An exception to
-- this is with Spot Instances, which can be terminated by Amazon Web
-- Services regardless of protection status. This property is set to
-- @NO_PROTECTION@ by default.
--
-- 'tags', 'createGameServerGroup_tags' - A list of labels to assign to the new game server group resource. Tags
-- are developer-defined key-value pairs. Tagging Amazon Web Services
-- resources is useful for resource management, access management, and cost
-- allocation. For more information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging Amazon Web Services Resources>
-- in the /Amazon Web Services General Reference/.
--
-- 'vpcSubnets', 'createGameServerGroup_vpcSubnets' - A list of virtual private cloud (VPC) subnets to use with instances in
-- the game server group. By default, all GameLift FleetIQ-supported
-- Availability Zones are used. You can use this parameter to specify VPCs
-- that you\'ve set up. This property cannot be updated after the game
-- server group is created, and the corresponding Auto Scaling group will
-- always use the property value that is set with this request, even if the
-- Auto Scaling group is updated directly.
--
-- 'gameServerGroupName', 'createGameServerGroup_gameServerGroupName' - An identifier for the new game server group. This value is used to
-- generate unique ARN identifiers for the Amazon EC2 Auto Scaling group
-- and the GameLift FleetIQ game server group. The name must be unique per
-- Region per Amazon Web Services account.
--
-- 'roleArn', 'createGameServerGroup_roleArn' - The Amazon Resource Name
-- (<https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-arn-format.html ARN>)
-- for an IAM role that allows Amazon GameLift to access your Amazon EC2
-- Auto Scaling groups.
--
-- 'minSize', 'createGameServerGroup_minSize' - The minimum number of instances allowed in the Amazon EC2 Auto Scaling
-- group. During automatic scaling events, GameLift FleetIQ and Amazon EC2
-- do not scale down the group below this minimum. In production, this
-- value should be set to at least 1. After the Auto Scaling group is
-- created, update this value directly in the Auto Scaling group using the
-- Amazon Web Services console or APIs.
--
-- 'maxSize', 'createGameServerGroup_maxSize' - The maximum number of instances allowed in the Amazon EC2 Auto Scaling
-- group. During automatic scaling events, GameLift FleetIQ and EC2 do not
-- scale up the group above this maximum. After the Auto Scaling group is
-- created, update this value directly in the Auto Scaling group using the
-- Amazon Web Services console or APIs.
--
-- 'launchTemplate', 'createGameServerGroup_launchTemplate' - The Amazon EC2 launch template that contains configuration settings and
-- game server code to be deployed to all instances in the game server
-- group. You can specify the template using either the template name or
-- ID. For help with creating a launch template, see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/create-launch-template.html Creating a Launch Template for an Auto Scaling Group>
-- in the /Amazon Elastic Compute Cloud Auto Scaling User Guide/. After the
-- Auto Scaling group is created, update this value directly in the Auto
-- Scaling group using the Amazon Web Services console or APIs.
--
-- If you specify network interfaces in your launch template, you must
-- explicitly set the property @AssociatePublicIpAddress@ to \"true\". If
-- no network interface is specified in the launch template, GameLift
-- FleetIQ uses your account\'s default VPC.
--
-- 'instanceDefinitions', 'createGameServerGroup_instanceDefinitions' - The Amazon EC2 instance types and sizes to use in the Auto Scaling
-- group. The instance definitions must specify at least two different
-- instance types that are supported by GameLift FleetIQ. For more
-- information on instance types, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html EC2 Instance Types>
-- in the /Amazon Elastic Compute Cloud User Guide/. You can optionally
-- specify capacity weighting for each instance type. If no weight value is
-- specified for an instance type, it is set to the default value \"1\".
-- For more information about capacity weighting, see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/asg-instance-weighting.html Instance Weighting for Amazon EC2 Auto Scaling>
-- in the Amazon EC2 Auto Scaling User Guide.
newCreateGameServerGroup ::
  -- | 'gameServerGroupName'
  Prelude.Text ->
  -- | 'roleArn'
  Prelude.Text ->
  -- | 'minSize'
  Prelude.Natural ->
  -- | 'maxSize'
  Prelude.Natural ->
  -- | 'launchTemplate'
  LaunchTemplateSpecification ->
  -- | 'instanceDefinitions'
  Prelude.NonEmpty InstanceDefinition ->
  CreateGameServerGroup
newCreateGameServerGroup
  pGameServerGroupName_
  pRoleArn_
  pMinSize_
  pMaxSize_
  pLaunchTemplate_
  pInstanceDefinitions_ =
    CreateGameServerGroup'
      { autoScalingPolicy =
          Prelude.Nothing,
        balancingStrategy = Prelude.Nothing,
        gameServerProtectionPolicy = Prelude.Nothing,
        tags = Prelude.Nothing,
        vpcSubnets = Prelude.Nothing,
        gameServerGroupName = pGameServerGroupName_,
        roleArn = pRoleArn_,
        minSize = pMinSize_,
        maxSize = pMaxSize_,
        launchTemplate = pLaunchTemplate_,
        instanceDefinitions =
          Lens.coerced Lens.# pInstanceDefinitions_
      }

-- | Configuration settings to define a scaling policy for the Auto Scaling
-- group that is optimized for game hosting. The scaling policy uses the
-- metric @\"PercentUtilizedGameServers\"@ to maintain a buffer of idle
-- game servers that can immediately accommodate new games and players.
-- After the Auto Scaling group is created, update this value directly in
-- the Auto Scaling group using the Amazon Web Services console or APIs.
createGameServerGroup_autoScalingPolicy :: Lens.Lens' CreateGameServerGroup (Prelude.Maybe GameServerGroupAutoScalingPolicy)
createGameServerGroup_autoScalingPolicy = Lens.lens (\CreateGameServerGroup' {autoScalingPolicy} -> autoScalingPolicy) (\s@CreateGameServerGroup' {} a -> s {autoScalingPolicy = a} :: CreateGameServerGroup)

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
createGameServerGroup_balancingStrategy :: Lens.Lens' CreateGameServerGroup (Prelude.Maybe BalancingStrategy)
createGameServerGroup_balancingStrategy = Lens.lens (\CreateGameServerGroup' {balancingStrategy} -> balancingStrategy) (\s@CreateGameServerGroup' {} a -> s {balancingStrategy = a} :: CreateGameServerGroup)

-- | A flag that indicates whether instances in the game server group are
-- protected from early termination. Unprotected instances that have active
-- game servers running might be terminated during a scale-down event,
-- causing players to be dropped from the game. Protected instances cannot
-- be terminated while there are active game servers running except in the
-- event of a forced game server group deletion (see ). An exception to
-- this is with Spot Instances, which can be terminated by Amazon Web
-- Services regardless of protection status. This property is set to
-- @NO_PROTECTION@ by default.
createGameServerGroup_gameServerProtectionPolicy :: Lens.Lens' CreateGameServerGroup (Prelude.Maybe GameServerProtectionPolicy)
createGameServerGroup_gameServerProtectionPolicy = Lens.lens (\CreateGameServerGroup' {gameServerProtectionPolicy} -> gameServerProtectionPolicy) (\s@CreateGameServerGroup' {} a -> s {gameServerProtectionPolicy = a} :: CreateGameServerGroup)

-- | A list of labels to assign to the new game server group resource. Tags
-- are developer-defined key-value pairs. Tagging Amazon Web Services
-- resources is useful for resource management, access management, and cost
-- allocation. For more information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging Amazon Web Services Resources>
-- in the /Amazon Web Services General Reference/.
createGameServerGroup_tags :: Lens.Lens' CreateGameServerGroup (Prelude.Maybe [Tag])
createGameServerGroup_tags = Lens.lens (\CreateGameServerGroup' {tags} -> tags) (\s@CreateGameServerGroup' {} a -> s {tags = a} :: CreateGameServerGroup) Prelude.. Lens.mapping Lens.coerced

-- | A list of virtual private cloud (VPC) subnets to use with instances in
-- the game server group. By default, all GameLift FleetIQ-supported
-- Availability Zones are used. You can use this parameter to specify VPCs
-- that you\'ve set up. This property cannot be updated after the game
-- server group is created, and the corresponding Auto Scaling group will
-- always use the property value that is set with this request, even if the
-- Auto Scaling group is updated directly.
createGameServerGroup_vpcSubnets :: Lens.Lens' CreateGameServerGroup (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
createGameServerGroup_vpcSubnets = Lens.lens (\CreateGameServerGroup' {vpcSubnets} -> vpcSubnets) (\s@CreateGameServerGroup' {} a -> s {vpcSubnets = a} :: CreateGameServerGroup) Prelude.. Lens.mapping Lens.coerced

-- | An identifier for the new game server group. This value is used to
-- generate unique ARN identifiers for the Amazon EC2 Auto Scaling group
-- and the GameLift FleetIQ game server group. The name must be unique per
-- Region per Amazon Web Services account.
createGameServerGroup_gameServerGroupName :: Lens.Lens' CreateGameServerGroup Prelude.Text
createGameServerGroup_gameServerGroupName = Lens.lens (\CreateGameServerGroup' {gameServerGroupName} -> gameServerGroupName) (\s@CreateGameServerGroup' {} a -> s {gameServerGroupName = a} :: CreateGameServerGroup)

-- | The Amazon Resource Name
-- (<https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-arn-format.html ARN>)
-- for an IAM role that allows Amazon GameLift to access your Amazon EC2
-- Auto Scaling groups.
createGameServerGroup_roleArn :: Lens.Lens' CreateGameServerGroup Prelude.Text
createGameServerGroup_roleArn = Lens.lens (\CreateGameServerGroup' {roleArn} -> roleArn) (\s@CreateGameServerGroup' {} a -> s {roleArn = a} :: CreateGameServerGroup)

-- | The minimum number of instances allowed in the Amazon EC2 Auto Scaling
-- group. During automatic scaling events, GameLift FleetIQ and Amazon EC2
-- do not scale down the group below this minimum. In production, this
-- value should be set to at least 1. After the Auto Scaling group is
-- created, update this value directly in the Auto Scaling group using the
-- Amazon Web Services console or APIs.
createGameServerGroup_minSize :: Lens.Lens' CreateGameServerGroup Prelude.Natural
createGameServerGroup_minSize = Lens.lens (\CreateGameServerGroup' {minSize} -> minSize) (\s@CreateGameServerGroup' {} a -> s {minSize = a} :: CreateGameServerGroup)

-- | The maximum number of instances allowed in the Amazon EC2 Auto Scaling
-- group. During automatic scaling events, GameLift FleetIQ and EC2 do not
-- scale up the group above this maximum. After the Auto Scaling group is
-- created, update this value directly in the Auto Scaling group using the
-- Amazon Web Services console or APIs.
createGameServerGroup_maxSize :: Lens.Lens' CreateGameServerGroup Prelude.Natural
createGameServerGroup_maxSize = Lens.lens (\CreateGameServerGroup' {maxSize} -> maxSize) (\s@CreateGameServerGroup' {} a -> s {maxSize = a} :: CreateGameServerGroup)

-- | The Amazon EC2 launch template that contains configuration settings and
-- game server code to be deployed to all instances in the game server
-- group. You can specify the template using either the template name or
-- ID. For help with creating a launch template, see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/create-launch-template.html Creating a Launch Template for an Auto Scaling Group>
-- in the /Amazon Elastic Compute Cloud Auto Scaling User Guide/. After the
-- Auto Scaling group is created, update this value directly in the Auto
-- Scaling group using the Amazon Web Services console or APIs.
--
-- If you specify network interfaces in your launch template, you must
-- explicitly set the property @AssociatePublicIpAddress@ to \"true\". If
-- no network interface is specified in the launch template, GameLift
-- FleetIQ uses your account\'s default VPC.
createGameServerGroup_launchTemplate :: Lens.Lens' CreateGameServerGroup LaunchTemplateSpecification
createGameServerGroup_launchTemplate = Lens.lens (\CreateGameServerGroup' {launchTemplate} -> launchTemplate) (\s@CreateGameServerGroup' {} a -> s {launchTemplate = a} :: CreateGameServerGroup)

-- | The Amazon EC2 instance types and sizes to use in the Auto Scaling
-- group. The instance definitions must specify at least two different
-- instance types that are supported by GameLift FleetIQ. For more
-- information on instance types, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html EC2 Instance Types>
-- in the /Amazon Elastic Compute Cloud User Guide/. You can optionally
-- specify capacity weighting for each instance type. If no weight value is
-- specified for an instance type, it is set to the default value \"1\".
-- For more information about capacity weighting, see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/asg-instance-weighting.html Instance Weighting for Amazon EC2 Auto Scaling>
-- in the Amazon EC2 Auto Scaling User Guide.
createGameServerGroup_instanceDefinitions :: Lens.Lens' CreateGameServerGroup (Prelude.NonEmpty InstanceDefinition)
createGameServerGroup_instanceDefinitions = Lens.lens (\CreateGameServerGroup' {instanceDefinitions} -> instanceDefinitions) (\s@CreateGameServerGroup' {} a -> s {instanceDefinitions = a} :: CreateGameServerGroup) Prelude.. Lens.coerced

instance Core.AWSRequest CreateGameServerGroup where
  type
    AWSResponse CreateGameServerGroup =
      CreateGameServerGroupResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateGameServerGroupResponse'
            Prelude.<$> (x Data..?> "GameServerGroup")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateGameServerGroup where
  hashWithSalt _salt CreateGameServerGroup' {..} =
    _salt `Prelude.hashWithSalt` autoScalingPolicy
      `Prelude.hashWithSalt` balancingStrategy
      `Prelude.hashWithSalt` gameServerProtectionPolicy
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` vpcSubnets
      `Prelude.hashWithSalt` gameServerGroupName
      `Prelude.hashWithSalt` roleArn
      `Prelude.hashWithSalt` minSize
      `Prelude.hashWithSalt` maxSize
      `Prelude.hashWithSalt` launchTemplate
      `Prelude.hashWithSalt` instanceDefinitions

instance Prelude.NFData CreateGameServerGroup where
  rnf CreateGameServerGroup' {..} =
    Prelude.rnf autoScalingPolicy
      `Prelude.seq` Prelude.rnf balancingStrategy
      `Prelude.seq` Prelude.rnf gameServerProtectionPolicy
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf vpcSubnets
      `Prelude.seq` Prelude.rnf gameServerGroupName
      `Prelude.seq` Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf minSize
      `Prelude.seq` Prelude.rnf maxSize
      `Prelude.seq` Prelude.rnf launchTemplate
      `Prelude.seq` Prelude.rnf instanceDefinitions

instance Data.ToHeaders CreateGameServerGroup where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "GameLift.CreateGameServerGroup" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateGameServerGroup where
  toJSON CreateGameServerGroup' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AutoScalingPolicy" Data..=)
              Prelude.<$> autoScalingPolicy,
            ("BalancingStrategy" Data..=)
              Prelude.<$> balancingStrategy,
            ("GameServerProtectionPolicy" Data..=)
              Prelude.<$> gameServerProtectionPolicy,
            ("Tags" Data..=) Prelude.<$> tags,
            ("VpcSubnets" Data..=) Prelude.<$> vpcSubnets,
            Prelude.Just
              ("GameServerGroupName" Data..= gameServerGroupName),
            Prelude.Just ("RoleArn" Data..= roleArn),
            Prelude.Just ("MinSize" Data..= minSize),
            Prelude.Just ("MaxSize" Data..= maxSize),
            Prelude.Just
              ("LaunchTemplate" Data..= launchTemplate),
            Prelude.Just
              ("InstanceDefinitions" Data..= instanceDefinitions)
          ]
      )

instance Data.ToPath CreateGameServerGroup where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateGameServerGroup where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateGameServerGroupResponse' smart constructor.
data CreateGameServerGroupResponse = CreateGameServerGroupResponse'
  { -- | The newly created game server group object, including the new ARN value
    -- for the GameLift FleetIQ game server group and the object\'s status. The
    -- Amazon EC2 Auto Scaling group ARN is initially null, since the group has
    -- not yet been created. This value is added once the game server group
    -- status reaches @ACTIVE@.
    gameServerGroup :: Prelude.Maybe GameServerGroup,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateGameServerGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'gameServerGroup', 'createGameServerGroupResponse_gameServerGroup' - The newly created game server group object, including the new ARN value
-- for the GameLift FleetIQ game server group and the object\'s status. The
-- Amazon EC2 Auto Scaling group ARN is initially null, since the group has
-- not yet been created. This value is added once the game server group
-- status reaches @ACTIVE@.
--
-- 'httpStatus', 'createGameServerGroupResponse_httpStatus' - The response's http status code.
newCreateGameServerGroupResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateGameServerGroupResponse
newCreateGameServerGroupResponse pHttpStatus_ =
  CreateGameServerGroupResponse'
    { gameServerGroup =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The newly created game server group object, including the new ARN value
-- for the GameLift FleetIQ game server group and the object\'s status. The
-- Amazon EC2 Auto Scaling group ARN is initially null, since the group has
-- not yet been created. This value is added once the game server group
-- status reaches @ACTIVE@.
createGameServerGroupResponse_gameServerGroup :: Lens.Lens' CreateGameServerGroupResponse (Prelude.Maybe GameServerGroup)
createGameServerGroupResponse_gameServerGroup = Lens.lens (\CreateGameServerGroupResponse' {gameServerGroup} -> gameServerGroup) (\s@CreateGameServerGroupResponse' {} a -> s {gameServerGroup = a} :: CreateGameServerGroupResponse)

-- | The response's http status code.
createGameServerGroupResponse_httpStatus :: Lens.Lens' CreateGameServerGroupResponse Prelude.Int
createGameServerGroupResponse_httpStatus = Lens.lens (\CreateGameServerGroupResponse' {httpStatus} -> httpStatus) (\s@CreateGameServerGroupResponse' {} a -> s {httpStatus = a} :: CreateGameServerGroupResponse)

instance Prelude.NFData CreateGameServerGroupResponse where
  rnf CreateGameServerGroupResponse' {..} =
    Prelude.rnf gameServerGroup
      `Prelude.seq` Prelude.rnf httpStatus
