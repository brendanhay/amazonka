{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.GameLift.Types.GameServerGroup
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GameLift.Types.GameServerGroup where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GameLift.Types.BalancingStrategy
import Amazonka.GameLift.Types.GameServerGroupAction
import Amazonka.GameLift.Types.GameServerGroupStatus
import Amazonka.GameLift.Types.GameServerProtectionPolicy
import Amazonka.GameLift.Types.InstanceDefinition
import qualified Amazonka.Prelude as Prelude

-- | __This data type is used with the GameLift FleetIQ and game server
-- groups.__
--
-- Properties that describe a game server group resource. A game server
-- group manages certain properties related to a corresponding Amazon EC2
-- Auto Scaling group.
--
-- A game server group is created by a successful call to
-- @CreateGameServerGroup@ and deleted by calling @DeleteGameServerGroup@.
-- Game server group activity can be temporarily suspended and resumed by
-- calling @SuspendGameServerGroup@ and @ResumeGameServerGroup@,
-- respectively.
--
-- /See:/ 'newGameServerGroup' smart constructor.
data GameServerGroup = GameServerGroup'
  { -- | A generated unique ID for the Amazon EC2 Auto Scaling group that is
    -- associated with this game server group.
    autoScalingGroupArn :: Prelude.Maybe Prelude.Text,
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
    -- | A time stamp indicating when this data object was created. Format is a
    -- number expressed in Unix time as milliseconds (for example
    -- @\"1469498468.057\"@).
    creationTime :: Prelude.Maybe Data.POSIX,
    -- | A generated unique ID for the game server group.
    gameServerGroupArn :: Prelude.Maybe Prelude.Text,
    -- | A developer-defined identifier for the game server group. The name is
    -- unique for each Region in each Amazon Web Services account.
    gameServerGroupName :: Prelude.Maybe Prelude.Text,
    -- | A flag that indicates whether instances in the game server group are
    -- protected from early termination. Unprotected instances that have active
    -- game servers running might be terminated during a scale-down event,
    -- causing players to be dropped from the game. Protected instances cannot
    -- be terminated while there are active game servers running except in the
    -- event of a forced game server group deletion (see ). An exception to
    -- this is with Spot Instances, which can be terminated by Amazon Web
    -- Services regardless of protection status.
    gameServerProtectionPolicy :: Prelude.Maybe GameServerProtectionPolicy,
    -- | The set of Amazon EC2 instance types that GameLift FleetIQ can use when
    -- balancing and automatically scaling instances in the corresponding Auto
    -- Scaling group.
    instanceDefinitions :: Prelude.Maybe (Prelude.NonEmpty InstanceDefinition),
    -- | A timestamp that indicates when this game server group was last updated.
    lastUpdatedTime :: Prelude.Maybe Data.POSIX,
    -- | The Amazon Resource Name
    -- (<https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-arn-format.html ARN>)
    -- for an IAM role that allows Amazon GameLift to access your Amazon EC2
    -- Auto Scaling groups.
    roleArn :: Prelude.Maybe Prelude.Text,
    -- | The current status of the game server group. Possible statuses include:
    --
    -- -   @NEW@ - GameLift FleetIQ has validated the @CreateGameServerGroup()@
    --     request.
    --
    -- -   @ACTIVATING@ - GameLift FleetIQ is setting up a game server group,
    --     which includes creating an Auto Scaling group in your Amazon Web
    --     Services account.
    --
    -- -   @ACTIVE@ - The game server group has been successfully created.
    --
    -- -   @DELETE_SCHEDULED@ - A request to delete the game server group has
    --     been received.
    --
    -- -   @DELETING@ - GameLift FleetIQ has received a valid
    --     @DeleteGameServerGroup()@ request and is processing it. GameLift
    --     FleetIQ must first complete and release hosts before it deletes the
    --     Auto Scaling group and the game server group.
    --
    -- -   @DELETED@ - The game server group has been successfully deleted.
    --
    -- -   @ERROR@ - The asynchronous processes of activating or deleting a
    --     game server group has failed, resulting in an error state.
    status :: Prelude.Maybe GameServerGroupStatus,
    -- | Additional information about the current game server group status. This
    -- information might provide additional insight on groups that are in
    -- @ERROR@ status.
    statusReason :: Prelude.Maybe Prelude.Text,
    -- | A list of activities that are currently suspended for this game server
    -- group. If this property is empty, all activities are occurring.
    suspendedActions :: Prelude.Maybe (Prelude.NonEmpty GameServerGroupAction)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GameServerGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'autoScalingGroupArn', 'gameServerGroup_autoScalingGroupArn' - A generated unique ID for the Amazon EC2 Auto Scaling group that is
-- associated with this game server group.
--
-- 'balancingStrategy', 'gameServerGroup_balancingStrategy' - Indicates how GameLift FleetIQ balances the use of Spot Instances and
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
-- 'creationTime', 'gameServerGroup_creationTime' - A time stamp indicating when this data object was created. Format is a
-- number expressed in Unix time as milliseconds (for example
-- @\"1469498468.057\"@).
--
-- 'gameServerGroupArn', 'gameServerGroup_gameServerGroupArn' - A generated unique ID for the game server group.
--
-- 'gameServerGroupName', 'gameServerGroup_gameServerGroupName' - A developer-defined identifier for the game server group. The name is
-- unique for each Region in each Amazon Web Services account.
--
-- 'gameServerProtectionPolicy', 'gameServerGroup_gameServerProtectionPolicy' - A flag that indicates whether instances in the game server group are
-- protected from early termination. Unprotected instances that have active
-- game servers running might be terminated during a scale-down event,
-- causing players to be dropped from the game. Protected instances cannot
-- be terminated while there are active game servers running except in the
-- event of a forced game server group deletion (see ). An exception to
-- this is with Spot Instances, which can be terminated by Amazon Web
-- Services regardless of protection status.
--
-- 'instanceDefinitions', 'gameServerGroup_instanceDefinitions' - The set of Amazon EC2 instance types that GameLift FleetIQ can use when
-- balancing and automatically scaling instances in the corresponding Auto
-- Scaling group.
--
-- 'lastUpdatedTime', 'gameServerGroup_lastUpdatedTime' - A timestamp that indicates when this game server group was last updated.
--
-- 'roleArn', 'gameServerGroup_roleArn' - The Amazon Resource Name
-- (<https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-arn-format.html ARN>)
-- for an IAM role that allows Amazon GameLift to access your Amazon EC2
-- Auto Scaling groups.
--
-- 'status', 'gameServerGroup_status' - The current status of the game server group. Possible statuses include:
--
-- -   @NEW@ - GameLift FleetIQ has validated the @CreateGameServerGroup()@
--     request.
--
-- -   @ACTIVATING@ - GameLift FleetIQ is setting up a game server group,
--     which includes creating an Auto Scaling group in your Amazon Web
--     Services account.
--
-- -   @ACTIVE@ - The game server group has been successfully created.
--
-- -   @DELETE_SCHEDULED@ - A request to delete the game server group has
--     been received.
--
-- -   @DELETING@ - GameLift FleetIQ has received a valid
--     @DeleteGameServerGroup()@ request and is processing it. GameLift
--     FleetIQ must first complete and release hosts before it deletes the
--     Auto Scaling group and the game server group.
--
-- -   @DELETED@ - The game server group has been successfully deleted.
--
-- -   @ERROR@ - The asynchronous processes of activating or deleting a
--     game server group has failed, resulting in an error state.
--
-- 'statusReason', 'gameServerGroup_statusReason' - Additional information about the current game server group status. This
-- information might provide additional insight on groups that are in
-- @ERROR@ status.
--
-- 'suspendedActions', 'gameServerGroup_suspendedActions' - A list of activities that are currently suspended for this game server
-- group. If this property is empty, all activities are occurring.
newGameServerGroup ::
  GameServerGroup
newGameServerGroup =
  GameServerGroup'
    { autoScalingGroupArn =
        Prelude.Nothing,
      balancingStrategy = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      gameServerGroupArn = Prelude.Nothing,
      gameServerGroupName = Prelude.Nothing,
      gameServerProtectionPolicy = Prelude.Nothing,
      instanceDefinitions = Prelude.Nothing,
      lastUpdatedTime = Prelude.Nothing,
      roleArn = Prelude.Nothing,
      status = Prelude.Nothing,
      statusReason = Prelude.Nothing,
      suspendedActions = Prelude.Nothing
    }

-- | A generated unique ID for the Amazon EC2 Auto Scaling group that is
-- associated with this game server group.
gameServerGroup_autoScalingGroupArn :: Lens.Lens' GameServerGroup (Prelude.Maybe Prelude.Text)
gameServerGroup_autoScalingGroupArn = Lens.lens (\GameServerGroup' {autoScalingGroupArn} -> autoScalingGroupArn) (\s@GameServerGroup' {} a -> s {autoScalingGroupArn = a} :: GameServerGroup)

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
gameServerGroup_balancingStrategy :: Lens.Lens' GameServerGroup (Prelude.Maybe BalancingStrategy)
gameServerGroup_balancingStrategy = Lens.lens (\GameServerGroup' {balancingStrategy} -> balancingStrategy) (\s@GameServerGroup' {} a -> s {balancingStrategy = a} :: GameServerGroup)

-- | A time stamp indicating when this data object was created. Format is a
-- number expressed in Unix time as milliseconds (for example
-- @\"1469498468.057\"@).
gameServerGroup_creationTime :: Lens.Lens' GameServerGroup (Prelude.Maybe Prelude.UTCTime)
gameServerGroup_creationTime = Lens.lens (\GameServerGroup' {creationTime} -> creationTime) (\s@GameServerGroup' {} a -> s {creationTime = a} :: GameServerGroup) Prelude.. Lens.mapping Data._Time

-- | A generated unique ID for the game server group.
gameServerGroup_gameServerGroupArn :: Lens.Lens' GameServerGroup (Prelude.Maybe Prelude.Text)
gameServerGroup_gameServerGroupArn = Lens.lens (\GameServerGroup' {gameServerGroupArn} -> gameServerGroupArn) (\s@GameServerGroup' {} a -> s {gameServerGroupArn = a} :: GameServerGroup)

-- | A developer-defined identifier for the game server group. The name is
-- unique for each Region in each Amazon Web Services account.
gameServerGroup_gameServerGroupName :: Lens.Lens' GameServerGroup (Prelude.Maybe Prelude.Text)
gameServerGroup_gameServerGroupName = Lens.lens (\GameServerGroup' {gameServerGroupName} -> gameServerGroupName) (\s@GameServerGroup' {} a -> s {gameServerGroupName = a} :: GameServerGroup)

-- | A flag that indicates whether instances in the game server group are
-- protected from early termination. Unprotected instances that have active
-- game servers running might be terminated during a scale-down event,
-- causing players to be dropped from the game. Protected instances cannot
-- be terminated while there are active game servers running except in the
-- event of a forced game server group deletion (see ). An exception to
-- this is with Spot Instances, which can be terminated by Amazon Web
-- Services regardless of protection status.
gameServerGroup_gameServerProtectionPolicy :: Lens.Lens' GameServerGroup (Prelude.Maybe GameServerProtectionPolicy)
gameServerGroup_gameServerProtectionPolicy = Lens.lens (\GameServerGroup' {gameServerProtectionPolicy} -> gameServerProtectionPolicy) (\s@GameServerGroup' {} a -> s {gameServerProtectionPolicy = a} :: GameServerGroup)

-- | The set of Amazon EC2 instance types that GameLift FleetIQ can use when
-- balancing and automatically scaling instances in the corresponding Auto
-- Scaling group.
gameServerGroup_instanceDefinitions :: Lens.Lens' GameServerGroup (Prelude.Maybe (Prelude.NonEmpty InstanceDefinition))
gameServerGroup_instanceDefinitions = Lens.lens (\GameServerGroup' {instanceDefinitions} -> instanceDefinitions) (\s@GameServerGroup' {} a -> s {instanceDefinitions = a} :: GameServerGroup) Prelude.. Lens.mapping Lens.coerced

-- | A timestamp that indicates when this game server group was last updated.
gameServerGroup_lastUpdatedTime :: Lens.Lens' GameServerGroup (Prelude.Maybe Prelude.UTCTime)
gameServerGroup_lastUpdatedTime = Lens.lens (\GameServerGroup' {lastUpdatedTime} -> lastUpdatedTime) (\s@GameServerGroup' {} a -> s {lastUpdatedTime = a} :: GameServerGroup) Prelude.. Lens.mapping Data._Time

-- | The Amazon Resource Name
-- (<https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-arn-format.html ARN>)
-- for an IAM role that allows Amazon GameLift to access your Amazon EC2
-- Auto Scaling groups.
gameServerGroup_roleArn :: Lens.Lens' GameServerGroup (Prelude.Maybe Prelude.Text)
gameServerGroup_roleArn = Lens.lens (\GameServerGroup' {roleArn} -> roleArn) (\s@GameServerGroup' {} a -> s {roleArn = a} :: GameServerGroup)

-- | The current status of the game server group. Possible statuses include:
--
-- -   @NEW@ - GameLift FleetIQ has validated the @CreateGameServerGroup()@
--     request.
--
-- -   @ACTIVATING@ - GameLift FleetIQ is setting up a game server group,
--     which includes creating an Auto Scaling group in your Amazon Web
--     Services account.
--
-- -   @ACTIVE@ - The game server group has been successfully created.
--
-- -   @DELETE_SCHEDULED@ - A request to delete the game server group has
--     been received.
--
-- -   @DELETING@ - GameLift FleetIQ has received a valid
--     @DeleteGameServerGroup()@ request and is processing it. GameLift
--     FleetIQ must first complete and release hosts before it deletes the
--     Auto Scaling group and the game server group.
--
-- -   @DELETED@ - The game server group has been successfully deleted.
--
-- -   @ERROR@ - The asynchronous processes of activating or deleting a
--     game server group has failed, resulting in an error state.
gameServerGroup_status :: Lens.Lens' GameServerGroup (Prelude.Maybe GameServerGroupStatus)
gameServerGroup_status = Lens.lens (\GameServerGroup' {status} -> status) (\s@GameServerGroup' {} a -> s {status = a} :: GameServerGroup)

-- | Additional information about the current game server group status. This
-- information might provide additional insight on groups that are in
-- @ERROR@ status.
gameServerGroup_statusReason :: Lens.Lens' GameServerGroup (Prelude.Maybe Prelude.Text)
gameServerGroup_statusReason = Lens.lens (\GameServerGroup' {statusReason} -> statusReason) (\s@GameServerGroup' {} a -> s {statusReason = a} :: GameServerGroup)

-- | A list of activities that are currently suspended for this game server
-- group. If this property is empty, all activities are occurring.
gameServerGroup_suspendedActions :: Lens.Lens' GameServerGroup (Prelude.Maybe (Prelude.NonEmpty GameServerGroupAction))
gameServerGroup_suspendedActions = Lens.lens (\GameServerGroup' {suspendedActions} -> suspendedActions) (\s@GameServerGroup' {} a -> s {suspendedActions = a} :: GameServerGroup) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON GameServerGroup where
  parseJSON =
    Data.withObject
      "GameServerGroup"
      ( \x ->
          GameServerGroup'
            Prelude.<$> (x Data..:? "AutoScalingGroupArn")
            Prelude.<*> (x Data..:? "BalancingStrategy")
            Prelude.<*> (x Data..:? "CreationTime")
            Prelude.<*> (x Data..:? "GameServerGroupArn")
            Prelude.<*> (x Data..:? "GameServerGroupName")
            Prelude.<*> (x Data..:? "GameServerProtectionPolicy")
            Prelude.<*> (x Data..:? "InstanceDefinitions")
            Prelude.<*> (x Data..:? "LastUpdatedTime")
            Prelude.<*> (x Data..:? "RoleArn")
            Prelude.<*> (x Data..:? "Status")
            Prelude.<*> (x Data..:? "StatusReason")
            Prelude.<*> (x Data..:? "SuspendedActions")
      )

instance Prelude.Hashable GameServerGroup where
  hashWithSalt _salt GameServerGroup' {..} =
    _salt
      `Prelude.hashWithSalt` autoScalingGroupArn
      `Prelude.hashWithSalt` balancingStrategy
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` gameServerGroupArn
      `Prelude.hashWithSalt` gameServerGroupName
      `Prelude.hashWithSalt` gameServerProtectionPolicy
      `Prelude.hashWithSalt` instanceDefinitions
      `Prelude.hashWithSalt` lastUpdatedTime
      `Prelude.hashWithSalt` roleArn
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` statusReason
      `Prelude.hashWithSalt` suspendedActions

instance Prelude.NFData GameServerGroup where
  rnf GameServerGroup' {..} =
    Prelude.rnf autoScalingGroupArn
      `Prelude.seq` Prelude.rnf balancingStrategy
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf gameServerGroupArn
      `Prelude.seq` Prelude.rnf gameServerGroupName
      `Prelude.seq` Prelude.rnf gameServerProtectionPolicy
      `Prelude.seq` Prelude.rnf instanceDefinitions
      `Prelude.seq` Prelude.rnf lastUpdatedTime
      `Prelude.seq` Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf statusReason
      `Prelude.seq` Prelude.rnf suspendedActions
