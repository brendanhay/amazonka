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
-- Module      : Amazonka.AutoScaling.Types.RefreshPreferences
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AutoScaling.Types.RefreshPreferences where

import Amazonka.AutoScaling.Types.ScaleInProtectedInstances
import Amazonka.AutoScaling.Types.StandbyInstances
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes the preferences for an instance refresh.
--
-- /See:/ 'newRefreshPreferences' smart constructor.
data RefreshPreferences = RefreshPreferences'
  { -- | (Optional) Indicates whether to roll back the Auto Scaling group to its
    -- previous configuration if the instance refresh fails. The default is
    -- @false@.
    --
    -- A rollback is not supported in the following situations:
    --
    -- -   There is no desired configuration specified for the instance
    --     refresh.
    --
    -- -   The Auto Scaling group has a launch template that uses an Amazon Web
    --     Services Systems Manager parameter instead of an AMI ID for the
    --     @ImageId@ property.
    --
    -- -   The Auto Scaling group uses the launch template\'s @$Latest@ or
    --     @$Default@ version.
    autoRollback :: Prelude.Maybe Prelude.Bool,
    -- | (Optional) The amount of time, in seconds, to wait after a checkpoint
    -- before continuing. This property is optional, but if you specify a value
    -- for it, you must also specify a value for @CheckpointPercentages@. If
    -- you specify a value for @CheckpointPercentages@ and not for
    -- @CheckpointDelay@, the @CheckpointDelay@ defaults to @3600@ (1 hour).
    checkpointDelay :: Prelude.Maybe Prelude.Natural,
    -- | (Optional) Threshold values for each checkpoint in ascending order. Each
    -- number must be unique. To replace all instances in the Auto Scaling
    -- group, the last number in the array must be @100@.
    --
    -- For usage examples, see
    -- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/asg-adding-checkpoints-instance-refresh.html Adding checkpoints to an instance refresh>
    -- in the /Amazon EC2 Auto Scaling User Guide/.
    checkpointPercentages :: Prelude.Maybe [Prelude.Natural],
    -- | A time period, in seconds, during which an instance refresh waits before
    -- moving on to replacing the next instance after a new instance enters the
    -- @InService@ state.
    --
    -- This property is not required for normal usage. Instead, use the
    -- @DefaultInstanceWarmup@ property of the Auto Scaling group. The
    -- @InstanceWarmup@ and @DefaultInstanceWarmup@ properties work the same
    -- way. Only specify this property if you must override the
    -- @DefaultInstanceWarmup@ property.
    --
    -- If you do not specify this property, the instance warmup by default is
    -- the value of the @DefaultInstanceWarmup@ property, if defined (which is
    -- recommended in all cases), or the @HealthCheckGracePeriod@ property
    -- otherwise.
    instanceWarmup :: Prelude.Maybe Prelude.Natural,
    -- | The amount of capacity in the Auto Scaling group that must pass your
    -- group\'s health checks to allow the operation to continue. The value is
    -- expressed as a percentage of the desired capacity of the Auto Scaling
    -- group (rounded up to the nearest integer). The default is @90@.
    --
    -- Setting the minimum healthy percentage to 100 percent limits the rate of
    -- replacement to one instance at a time. In contrast, setting it to 0
    -- percent has the effect of replacing all instances at the same time.
    minHealthyPercentage :: Prelude.Maybe Prelude.Natural,
    -- | Choose the behavior that you want Amazon EC2 Auto Scaling to use if
    -- instances protected from scale in are found.
    --
    -- The following lists the valid values:
    --
    -- [Refresh]
    --     Amazon EC2 Auto Scaling replaces instances that are protected from
    --     scale in.
    --
    -- [Ignore]
    --     Amazon EC2 Auto Scaling ignores instances that are protected from
    --     scale in and continues to replace instances that are not protected.
    --
    -- [Wait (default)]
    --     Amazon EC2 Auto Scaling waits one hour for you to remove scale-in
    --     protection. Otherwise, the instance refresh will fail.
    scaleInProtectedInstances :: Prelude.Maybe ScaleInProtectedInstances,
    -- | (Optional) Indicates whether skip matching is enabled. If enabled
    -- (@true@), then Amazon EC2 Auto Scaling skips replacing instances that
    -- match the desired configuration. If no desired configuration is
    -- specified, then it skips replacing instances that have the same launch
    -- template and instance types that the Auto Scaling group was using before
    -- the start of the instance refresh. The default is @false@.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/asg-instance-refresh-skip-matching.html Use an instance refresh with skip matching>
    -- in the /Amazon EC2 Auto Scaling User Guide/.
    skipMatching :: Prelude.Maybe Prelude.Bool,
    -- | Choose the behavior that you want Amazon EC2 Auto Scaling to use if
    -- instances in @Standby@ state are found.
    --
    -- The following lists the valid values:
    --
    -- [Terminate]
    --     Amazon EC2 Auto Scaling terminates instances that are in @Standby@.
    --
    -- [Ignore]
    --     Amazon EC2 Auto Scaling ignores instances that are in @Standby@ and
    --     continues to replace instances that are in the @InService@ state.
    --
    -- [Wait (default)]
    --     Amazon EC2 Auto Scaling waits one hour for you to return the
    --     instances to service. Otherwise, the instance refresh will fail.
    standbyInstances :: Prelude.Maybe StandbyInstances
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RefreshPreferences' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'autoRollback', 'refreshPreferences_autoRollback' - (Optional) Indicates whether to roll back the Auto Scaling group to its
-- previous configuration if the instance refresh fails. The default is
-- @false@.
--
-- A rollback is not supported in the following situations:
--
-- -   There is no desired configuration specified for the instance
--     refresh.
--
-- -   The Auto Scaling group has a launch template that uses an Amazon Web
--     Services Systems Manager parameter instead of an AMI ID for the
--     @ImageId@ property.
--
-- -   The Auto Scaling group uses the launch template\'s @$Latest@ or
--     @$Default@ version.
--
-- 'checkpointDelay', 'refreshPreferences_checkpointDelay' - (Optional) The amount of time, in seconds, to wait after a checkpoint
-- before continuing. This property is optional, but if you specify a value
-- for it, you must also specify a value for @CheckpointPercentages@. If
-- you specify a value for @CheckpointPercentages@ and not for
-- @CheckpointDelay@, the @CheckpointDelay@ defaults to @3600@ (1 hour).
--
-- 'checkpointPercentages', 'refreshPreferences_checkpointPercentages' - (Optional) Threshold values for each checkpoint in ascending order. Each
-- number must be unique. To replace all instances in the Auto Scaling
-- group, the last number in the array must be @100@.
--
-- For usage examples, see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/asg-adding-checkpoints-instance-refresh.html Adding checkpoints to an instance refresh>
-- in the /Amazon EC2 Auto Scaling User Guide/.
--
-- 'instanceWarmup', 'refreshPreferences_instanceWarmup' - A time period, in seconds, during which an instance refresh waits before
-- moving on to replacing the next instance after a new instance enters the
-- @InService@ state.
--
-- This property is not required for normal usage. Instead, use the
-- @DefaultInstanceWarmup@ property of the Auto Scaling group. The
-- @InstanceWarmup@ and @DefaultInstanceWarmup@ properties work the same
-- way. Only specify this property if you must override the
-- @DefaultInstanceWarmup@ property.
--
-- If you do not specify this property, the instance warmup by default is
-- the value of the @DefaultInstanceWarmup@ property, if defined (which is
-- recommended in all cases), or the @HealthCheckGracePeriod@ property
-- otherwise.
--
-- 'minHealthyPercentage', 'refreshPreferences_minHealthyPercentage' - The amount of capacity in the Auto Scaling group that must pass your
-- group\'s health checks to allow the operation to continue. The value is
-- expressed as a percentage of the desired capacity of the Auto Scaling
-- group (rounded up to the nearest integer). The default is @90@.
--
-- Setting the minimum healthy percentage to 100 percent limits the rate of
-- replacement to one instance at a time. In contrast, setting it to 0
-- percent has the effect of replacing all instances at the same time.
--
-- 'scaleInProtectedInstances', 'refreshPreferences_scaleInProtectedInstances' - Choose the behavior that you want Amazon EC2 Auto Scaling to use if
-- instances protected from scale in are found.
--
-- The following lists the valid values:
--
-- [Refresh]
--     Amazon EC2 Auto Scaling replaces instances that are protected from
--     scale in.
--
-- [Ignore]
--     Amazon EC2 Auto Scaling ignores instances that are protected from
--     scale in and continues to replace instances that are not protected.
--
-- [Wait (default)]
--     Amazon EC2 Auto Scaling waits one hour for you to remove scale-in
--     protection. Otherwise, the instance refresh will fail.
--
-- 'skipMatching', 'refreshPreferences_skipMatching' - (Optional) Indicates whether skip matching is enabled. If enabled
-- (@true@), then Amazon EC2 Auto Scaling skips replacing instances that
-- match the desired configuration. If no desired configuration is
-- specified, then it skips replacing instances that have the same launch
-- template and instance types that the Auto Scaling group was using before
-- the start of the instance refresh. The default is @false@.
--
-- For more information, see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/asg-instance-refresh-skip-matching.html Use an instance refresh with skip matching>
-- in the /Amazon EC2 Auto Scaling User Guide/.
--
-- 'standbyInstances', 'refreshPreferences_standbyInstances' - Choose the behavior that you want Amazon EC2 Auto Scaling to use if
-- instances in @Standby@ state are found.
--
-- The following lists the valid values:
--
-- [Terminate]
--     Amazon EC2 Auto Scaling terminates instances that are in @Standby@.
--
-- [Ignore]
--     Amazon EC2 Auto Scaling ignores instances that are in @Standby@ and
--     continues to replace instances that are in the @InService@ state.
--
-- [Wait (default)]
--     Amazon EC2 Auto Scaling waits one hour for you to return the
--     instances to service. Otherwise, the instance refresh will fail.
newRefreshPreferences ::
  RefreshPreferences
newRefreshPreferences =
  RefreshPreferences'
    { autoRollback = Prelude.Nothing,
      checkpointDelay = Prelude.Nothing,
      checkpointPercentages = Prelude.Nothing,
      instanceWarmup = Prelude.Nothing,
      minHealthyPercentage = Prelude.Nothing,
      scaleInProtectedInstances = Prelude.Nothing,
      skipMatching = Prelude.Nothing,
      standbyInstances = Prelude.Nothing
    }

-- | (Optional) Indicates whether to roll back the Auto Scaling group to its
-- previous configuration if the instance refresh fails. The default is
-- @false@.
--
-- A rollback is not supported in the following situations:
--
-- -   There is no desired configuration specified for the instance
--     refresh.
--
-- -   The Auto Scaling group has a launch template that uses an Amazon Web
--     Services Systems Manager parameter instead of an AMI ID for the
--     @ImageId@ property.
--
-- -   The Auto Scaling group uses the launch template\'s @$Latest@ or
--     @$Default@ version.
refreshPreferences_autoRollback :: Lens.Lens' RefreshPreferences (Prelude.Maybe Prelude.Bool)
refreshPreferences_autoRollback = Lens.lens (\RefreshPreferences' {autoRollback} -> autoRollback) (\s@RefreshPreferences' {} a -> s {autoRollback = a} :: RefreshPreferences)

-- | (Optional) The amount of time, in seconds, to wait after a checkpoint
-- before continuing. This property is optional, but if you specify a value
-- for it, you must also specify a value for @CheckpointPercentages@. If
-- you specify a value for @CheckpointPercentages@ and not for
-- @CheckpointDelay@, the @CheckpointDelay@ defaults to @3600@ (1 hour).
refreshPreferences_checkpointDelay :: Lens.Lens' RefreshPreferences (Prelude.Maybe Prelude.Natural)
refreshPreferences_checkpointDelay = Lens.lens (\RefreshPreferences' {checkpointDelay} -> checkpointDelay) (\s@RefreshPreferences' {} a -> s {checkpointDelay = a} :: RefreshPreferences)

-- | (Optional) Threshold values for each checkpoint in ascending order. Each
-- number must be unique. To replace all instances in the Auto Scaling
-- group, the last number in the array must be @100@.
--
-- For usage examples, see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/asg-adding-checkpoints-instance-refresh.html Adding checkpoints to an instance refresh>
-- in the /Amazon EC2 Auto Scaling User Guide/.
refreshPreferences_checkpointPercentages :: Lens.Lens' RefreshPreferences (Prelude.Maybe [Prelude.Natural])
refreshPreferences_checkpointPercentages = Lens.lens (\RefreshPreferences' {checkpointPercentages} -> checkpointPercentages) (\s@RefreshPreferences' {} a -> s {checkpointPercentages = a} :: RefreshPreferences) Prelude.. Lens.mapping Lens.coerced

-- | A time period, in seconds, during which an instance refresh waits before
-- moving on to replacing the next instance after a new instance enters the
-- @InService@ state.
--
-- This property is not required for normal usage. Instead, use the
-- @DefaultInstanceWarmup@ property of the Auto Scaling group. The
-- @InstanceWarmup@ and @DefaultInstanceWarmup@ properties work the same
-- way. Only specify this property if you must override the
-- @DefaultInstanceWarmup@ property.
--
-- If you do not specify this property, the instance warmup by default is
-- the value of the @DefaultInstanceWarmup@ property, if defined (which is
-- recommended in all cases), or the @HealthCheckGracePeriod@ property
-- otherwise.
refreshPreferences_instanceWarmup :: Lens.Lens' RefreshPreferences (Prelude.Maybe Prelude.Natural)
refreshPreferences_instanceWarmup = Lens.lens (\RefreshPreferences' {instanceWarmup} -> instanceWarmup) (\s@RefreshPreferences' {} a -> s {instanceWarmup = a} :: RefreshPreferences)

-- | The amount of capacity in the Auto Scaling group that must pass your
-- group\'s health checks to allow the operation to continue. The value is
-- expressed as a percentage of the desired capacity of the Auto Scaling
-- group (rounded up to the nearest integer). The default is @90@.
--
-- Setting the minimum healthy percentage to 100 percent limits the rate of
-- replacement to one instance at a time. In contrast, setting it to 0
-- percent has the effect of replacing all instances at the same time.
refreshPreferences_minHealthyPercentage :: Lens.Lens' RefreshPreferences (Prelude.Maybe Prelude.Natural)
refreshPreferences_minHealthyPercentage = Lens.lens (\RefreshPreferences' {minHealthyPercentage} -> minHealthyPercentage) (\s@RefreshPreferences' {} a -> s {minHealthyPercentage = a} :: RefreshPreferences)

-- | Choose the behavior that you want Amazon EC2 Auto Scaling to use if
-- instances protected from scale in are found.
--
-- The following lists the valid values:
--
-- [Refresh]
--     Amazon EC2 Auto Scaling replaces instances that are protected from
--     scale in.
--
-- [Ignore]
--     Amazon EC2 Auto Scaling ignores instances that are protected from
--     scale in and continues to replace instances that are not protected.
--
-- [Wait (default)]
--     Amazon EC2 Auto Scaling waits one hour for you to remove scale-in
--     protection. Otherwise, the instance refresh will fail.
refreshPreferences_scaleInProtectedInstances :: Lens.Lens' RefreshPreferences (Prelude.Maybe ScaleInProtectedInstances)
refreshPreferences_scaleInProtectedInstances = Lens.lens (\RefreshPreferences' {scaleInProtectedInstances} -> scaleInProtectedInstances) (\s@RefreshPreferences' {} a -> s {scaleInProtectedInstances = a} :: RefreshPreferences)

-- | (Optional) Indicates whether skip matching is enabled. If enabled
-- (@true@), then Amazon EC2 Auto Scaling skips replacing instances that
-- match the desired configuration. If no desired configuration is
-- specified, then it skips replacing instances that have the same launch
-- template and instance types that the Auto Scaling group was using before
-- the start of the instance refresh. The default is @false@.
--
-- For more information, see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/asg-instance-refresh-skip-matching.html Use an instance refresh with skip matching>
-- in the /Amazon EC2 Auto Scaling User Guide/.
refreshPreferences_skipMatching :: Lens.Lens' RefreshPreferences (Prelude.Maybe Prelude.Bool)
refreshPreferences_skipMatching = Lens.lens (\RefreshPreferences' {skipMatching} -> skipMatching) (\s@RefreshPreferences' {} a -> s {skipMatching = a} :: RefreshPreferences)

-- | Choose the behavior that you want Amazon EC2 Auto Scaling to use if
-- instances in @Standby@ state are found.
--
-- The following lists the valid values:
--
-- [Terminate]
--     Amazon EC2 Auto Scaling terminates instances that are in @Standby@.
--
-- [Ignore]
--     Amazon EC2 Auto Scaling ignores instances that are in @Standby@ and
--     continues to replace instances that are in the @InService@ state.
--
-- [Wait (default)]
--     Amazon EC2 Auto Scaling waits one hour for you to return the
--     instances to service. Otherwise, the instance refresh will fail.
refreshPreferences_standbyInstances :: Lens.Lens' RefreshPreferences (Prelude.Maybe StandbyInstances)
refreshPreferences_standbyInstances = Lens.lens (\RefreshPreferences' {standbyInstances} -> standbyInstances) (\s@RefreshPreferences' {} a -> s {standbyInstances = a} :: RefreshPreferences)

instance Data.FromXML RefreshPreferences where
  parseXML x =
    RefreshPreferences'
      Prelude.<$> (x Data..@? "AutoRollback")
      Prelude.<*> (x Data..@? "CheckpointDelay")
      Prelude.<*> ( x
                      Data..@? "CheckpointPercentages"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "member")
                  )
      Prelude.<*> (x Data..@? "InstanceWarmup")
      Prelude.<*> (x Data..@? "MinHealthyPercentage")
      Prelude.<*> (x Data..@? "ScaleInProtectedInstances")
      Prelude.<*> (x Data..@? "SkipMatching")
      Prelude.<*> (x Data..@? "StandbyInstances")

instance Prelude.Hashable RefreshPreferences where
  hashWithSalt _salt RefreshPreferences' {..} =
    _salt
      `Prelude.hashWithSalt` autoRollback
      `Prelude.hashWithSalt` checkpointDelay
      `Prelude.hashWithSalt` checkpointPercentages
      `Prelude.hashWithSalt` instanceWarmup
      `Prelude.hashWithSalt` minHealthyPercentage
      `Prelude.hashWithSalt` scaleInProtectedInstances
      `Prelude.hashWithSalt` skipMatching
      `Prelude.hashWithSalt` standbyInstances

instance Prelude.NFData RefreshPreferences where
  rnf RefreshPreferences' {..} =
    Prelude.rnf autoRollback
      `Prelude.seq` Prelude.rnf checkpointDelay
      `Prelude.seq` Prelude.rnf checkpointPercentages
      `Prelude.seq` Prelude.rnf instanceWarmup
      `Prelude.seq` Prelude.rnf minHealthyPercentage
      `Prelude.seq` Prelude.rnf scaleInProtectedInstances
      `Prelude.seq` Prelude.rnf skipMatching
      `Prelude.seq` Prelude.rnf standbyInstances

instance Data.ToQuery RefreshPreferences where
  toQuery RefreshPreferences' {..} =
    Prelude.mconcat
      [ "AutoRollback" Data.=: autoRollback,
        "CheckpointDelay" Data.=: checkpointDelay,
        "CheckpointPercentages"
          Data.=: Data.toQuery
            ( Data.toQueryList "member"
                Prelude.<$> checkpointPercentages
            ),
        "InstanceWarmup" Data.=: instanceWarmup,
        "MinHealthyPercentage" Data.=: minHealthyPercentage,
        "ScaleInProtectedInstances"
          Data.=: scaleInProtectedInstances,
        "SkipMatching" Data.=: skipMatching,
        "StandbyInstances" Data.=: standbyInstances
      ]
