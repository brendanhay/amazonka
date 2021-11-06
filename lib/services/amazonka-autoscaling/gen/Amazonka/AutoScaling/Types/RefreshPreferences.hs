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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AutoScaling.Types.RefreshPreferences where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Describes the preferences for an instance refresh.
--
-- /See:/ 'newRefreshPreferences' smart constructor.
data RefreshPreferences = RefreshPreferences'
  { -- | The amount of capacity in the Auto Scaling group that must remain
    -- healthy during an instance refresh to allow the operation to continue.
    -- The value is expressed as a percentage of the desired capacity of the
    -- Auto Scaling group (rounded up to the nearest integer). The default is
    -- @90@.
    --
    -- Setting the minimum healthy percentage to 100 percent limits the rate of
    -- replacement to one instance at a time. In contrast, setting it to 0
    -- percent has the effect of replacing all instances at the same time.
    minHealthyPercentage :: Prelude.Maybe Prelude.Natural,
    -- | A boolean value that indicates whether skip matching is enabled. If
    -- true, then Amazon EC2 Auto Scaling skips replacing instances that match
    -- the desired configuration. If no desired configuration is specified,
    -- then it skips replacing instances that have the same configuration that
    -- is already set on the group. The default is @false@.
    skipMatching :: Prelude.Maybe Prelude.Bool,
    -- | Threshold values for each checkpoint in ascending order. Each number
    -- must be unique. To replace all instances in the Auto Scaling group, the
    -- last number in the array must be @100@.
    --
    -- For usage examples, see
    -- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/asg-adding-checkpoints-instance-refresh.html Adding checkpoints to an instance refresh>
    -- in the /Amazon EC2 Auto Scaling User Guide/.
    checkpointPercentages :: Prelude.Maybe [Prelude.Natural],
    -- | The amount of time, in seconds, to wait after a checkpoint before
    -- continuing. This property is optional, but if you specify a value for
    -- it, you must also specify a value for @CheckpointPercentages@. If you
    -- specify a value for @CheckpointPercentages@ and not for
    -- @CheckpointDelay@, the @CheckpointDelay@ defaults to @3600@ (1 hour).
    checkpointDelay :: Prelude.Maybe Prelude.Natural,
    -- | The number of seconds until a newly launched instance is configured and
    -- ready to use. During this time, Amazon EC2 Auto Scaling does not
    -- immediately move on to the next replacement. The default is to use the
    -- value for the health check grace period defined for the group.
    instanceWarmup :: Prelude.Maybe Prelude.Natural
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
-- 'minHealthyPercentage', 'refreshPreferences_minHealthyPercentage' - The amount of capacity in the Auto Scaling group that must remain
-- healthy during an instance refresh to allow the operation to continue.
-- The value is expressed as a percentage of the desired capacity of the
-- Auto Scaling group (rounded up to the nearest integer). The default is
-- @90@.
--
-- Setting the minimum healthy percentage to 100 percent limits the rate of
-- replacement to one instance at a time. In contrast, setting it to 0
-- percent has the effect of replacing all instances at the same time.
--
-- 'skipMatching', 'refreshPreferences_skipMatching' - A boolean value that indicates whether skip matching is enabled. If
-- true, then Amazon EC2 Auto Scaling skips replacing instances that match
-- the desired configuration. If no desired configuration is specified,
-- then it skips replacing instances that have the same configuration that
-- is already set on the group. The default is @false@.
--
-- 'checkpointPercentages', 'refreshPreferences_checkpointPercentages' - Threshold values for each checkpoint in ascending order. Each number
-- must be unique. To replace all instances in the Auto Scaling group, the
-- last number in the array must be @100@.
--
-- For usage examples, see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/asg-adding-checkpoints-instance-refresh.html Adding checkpoints to an instance refresh>
-- in the /Amazon EC2 Auto Scaling User Guide/.
--
-- 'checkpointDelay', 'refreshPreferences_checkpointDelay' - The amount of time, in seconds, to wait after a checkpoint before
-- continuing. This property is optional, but if you specify a value for
-- it, you must also specify a value for @CheckpointPercentages@. If you
-- specify a value for @CheckpointPercentages@ and not for
-- @CheckpointDelay@, the @CheckpointDelay@ defaults to @3600@ (1 hour).
--
-- 'instanceWarmup', 'refreshPreferences_instanceWarmup' - The number of seconds until a newly launched instance is configured and
-- ready to use. During this time, Amazon EC2 Auto Scaling does not
-- immediately move on to the next replacement. The default is to use the
-- value for the health check grace period defined for the group.
newRefreshPreferences ::
  RefreshPreferences
newRefreshPreferences =
  RefreshPreferences'
    { minHealthyPercentage =
        Prelude.Nothing,
      skipMatching = Prelude.Nothing,
      checkpointPercentages = Prelude.Nothing,
      checkpointDelay = Prelude.Nothing,
      instanceWarmup = Prelude.Nothing
    }

-- | The amount of capacity in the Auto Scaling group that must remain
-- healthy during an instance refresh to allow the operation to continue.
-- The value is expressed as a percentage of the desired capacity of the
-- Auto Scaling group (rounded up to the nearest integer). The default is
-- @90@.
--
-- Setting the minimum healthy percentage to 100 percent limits the rate of
-- replacement to one instance at a time. In contrast, setting it to 0
-- percent has the effect of replacing all instances at the same time.
refreshPreferences_minHealthyPercentage :: Lens.Lens' RefreshPreferences (Prelude.Maybe Prelude.Natural)
refreshPreferences_minHealthyPercentage = Lens.lens (\RefreshPreferences' {minHealthyPercentage} -> minHealthyPercentage) (\s@RefreshPreferences' {} a -> s {minHealthyPercentage = a} :: RefreshPreferences)

-- | A boolean value that indicates whether skip matching is enabled. If
-- true, then Amazon EC2 Auto Scaling skips replacing instances that match
-- the desired configuration. If no desired configuration is specified,
-- then it skips replacing instances that have the same configuration that
-- is already set on the group. The default is @false@.
refreshPreferences_skipMatching :: Lens.Lens' RefreshPreferences (Prelude.Maybe Prelude.Bool)
refreshPreferences_skipMatching = Lens.lens (\RefreshPreferences' {skipMatching} -> skipMatching) (\s@RefreshPreferences' {} a -> s {skipMatching = a} :: RefreshPreferences)

-- | Threshold values for each checkpoint in ascending order. Each number
-- must be unique. To replace all instances in the Auto Scaling group, the
-- last number in the array must be @100@.
--
-- For usage examples, see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/asg-adding-checkpoints-instance-refresh.html Adding checkpoints to an instance refresh>
-- in the /Amazon EC2 Auto Scaling User Guide/.
refreshPreferences_checkpointPercentages :: Lens.Lens' RefreshPreferences (Prelude.Maybe [Prelude.Natural])
refreshPreferences_checkpointPercentages = Lens.lens (\RefreshPreferences' {checkpointPercentages} -> checkpointPercentages) (\s@RefreshPreferences' {} a -> s {checkpointPercentages = a} :: RefreshPreferences) Prelude.. Lens.mapping Lens.coerced

-- | The amount of time, in seconds, to wait after a checkpoint before
-- continuing. This property is optional, but if you specify a value for
-- it, you must also specify a value for @CheckpointPercentages@. If you
-- specify a value for @CheckpointPercentages@ and not for
-- @CheckpointDelay@, the @CheckpointDelay@ defaults to @3600@ (1 hour).
refreshPreferences_checkpointDelay :: Lens.Lens' RefreshPreferences (Prelude.Maybe Prelude.Natural)
refreshPreferences_checkpointDelay = Lens.lens (\RefreshPreferences' {checkpointDelay} -> checkpointDelay) (\s@RefreshPreferences' {} a -> s {checkpointDelay = a} :: RefreshPreferences)

-- | The number of seconds until a newly launched instance is configured and
-- ready to use. During this time, Amazon EC2 Auto Scaling does not
-- immediately move on to the next replacement. The default is to use the
-- value for the health check grace period defined for the group.
refreshPreferences_instanceWarmup :: Lens.Lens' RefreshPreferences (Prelude.Maybe Prelude.Natural)
refreshPreferences_instanceWarmup = Lens.lens (\RefreshPreferences' {instanceWarmup} -> instanceWarmup) (\s@RefreshPreferences' {} a -> s {instanceWarmup = a} :: RefreshPreferences)

instance Core.FromXML RefreshPreferences where
  parseXML x =
    RefreshPreferences'
      Prelude.<$> (x Core..@? "MinHealthyPercentage")
      Prelude.<*> (x Core..@? "SkipMatching")
      Prelude.<*> ( x Core..@? "CheckpointPercentages"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "member")
                  )
      Prelude.<*> (x Core..@? "CheckpointDelay")
      Prelude.<*> (x Core..@? "InstanceWarmup")

instance Prelude.Hashable RefreshPreferences

instance Prelude.NFData RefreshPreferences

instance Core.ToQuery RefreshPreferences where
  toQuery RefreshPreferences' {..} =
    Prelude.mconcat
      [ "MinHealthyPercentage" Core.=: minHealthyPercentage,
        "SkipMatching" Core.=: skipMatching,
        "CheckpointPercentages"
          Core.=: Core.toQuery
            ( Core.toQueryList "member"
                Prelude.<$> checkpointPercentages
            ),
        "CheckpointDelay" Core.=: checkpointDelay,
        "InstanceWarmup" Core.=: instanceWarmup
      ]
