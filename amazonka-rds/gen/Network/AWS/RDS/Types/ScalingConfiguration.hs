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
-- Module      : Network.AWS.RDS.Types.ScalingConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.ScalingConfiguration where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Contains the scaling configuration of an Aurora Serverless DB cluster.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/aurora-serverless.html Using Amazon Aurora Serverless>
-- in the /Amazon Aurora User Guide/.
--
-- /See:/ 'newScalingConfiguration' smart constructor.
data ScalingConfiguration = ScalingConfiguration'
  { -- | The maximum capacity for an Aurora DB cluster in @serverless@ DB engine
    -- mode.
    --
    -- For Aurora MySQL, valid capacity values are @1@, @2@, @4@, @8@, @16@,
    -- @32@, @64@, @128@, and @256@.
    --
    -- For Aurora PostgreSQL, valid capacity values are @2@, @4@, @8@, @16@,
    -- @32@, @64@, @192@, and @384@.
    --
    -- The maximum capacity must be greater than or equal to the minimum
    -- capacity.
    maxCapacity :: Core.Maybe Core.Int,
    -- | A value that indicates whether to allow or disallow automatic pause for
    -- an Aurora DB cluster in @serverless@ DB engine mode. A DB cluster can be
    -- paused only when it\'s idle (it has no connections).
    --
    -- If a DB cluster is paused for more than seven days, the DB cluster might
    -- be backed up with a snapshot. In this case, the DB cluster is restored
    -- when there is a request to connect to it.
    autoPause :: Core.Maybe Core.Bool,
    -- | The action to take when the timeout is reached, either
    -- @ForceApplyCapacityChange@ or @RollbackCapacityChange@.
    --
    -- @ForceApplyCapacityChange@ sets the capacity to the specified value as
    -- soon as possible.
    --
    -- @RollbackCapacityChange@, the default, ignores the capacity change if a
    -- scaling point isn\'t found in the timeout period.
    --
    -- If you specify @ForceApplyCapacityChange@, connections that prevent
    -- Aurora Serverless from finding a scaling point might be dropped.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/aurora-serverless.how-it-works.html#aurora-serverless.how-it-works.auto-scaling Autoscaling for Aurora Serverless>
    -- in the /Amazon Aurora User Guide/.
    timeoutAction :: Core.Maybe Core.Text,
    -- | The time, in seconds, before an Aurora DB cluster in @serverless@ mode
    -- is paused.
    secondsUntilAutoPause :: Core.Maybe Core.Int,
    -- | The minimum capacity for an Aurora DB cluster in @serverless@ DB engine
    -- mode.
    --
    -- For Aurora MySQL, valid capacity values are @1@, @2@, @4@, @8@, @16@,
    -- @32@, @64@, @128@, and @256@.
    --
    -- For Aurora PostgreSQL, valid capacity values are @2@, @4@, @8@, @16@,
    -- @32@, @64@, @192@, and @384@.
    --
    -- The minimum capacity must be less than or equal to the maximum capacity.
    minCapacity :: Core.Maybe Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ScalingConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxCapacity', 'scalingConfiguration_maxCapacity' - The maximum capacity for an Aurora DB cluster in @serverless@ DB engine
-- mode.
--
-- For Aurora MySQL, valid capacity values are @1@, @2@, @4@, @8@, @16@,
-- @32@, @64@, @128@, and @256@.
--
-- For Aurora PostgreSQL, valid capacity values are @2@, @4@, @8@, @16@,
-- @32@, @64@, @192@, and @384@.
--
-- The maximum capacity must be greater than or equal to the minimum
-- capacity.
--
-- 'autoPause', 'scalingConfiguration_autoPause' - A value that indicates whether to allow or disallow automatic pause for
-- an Aurora DB cluster in @serverless@ DB engine mode. A DB cluster can be
-- paused only when it\'s idle (it has no connections).
--
-- If a DB cluster is paused for more than seven days, the DB cluster might
-- be backed up with a snapshot. In this case, the DB cluster is restored
-- when there is a request to connect to it.
--
-- 'timeoutAction', 'scalingConfiguration_timeoutAction' - The action to take when the timeout is reached, either
-- @ForceApplyCapacityChange@ or @RollbackCapacityChange@.
--
-- @ForceApplyCapacityChange@ sets the capacity to the specified value as
-- soon as possible.
--
-- @RollbackCapacityChange@, the default, ignores the capacity change if a
-- scaling point isn\'t found in the timeout period.
--
-- If you specify @ForceApplyCapacityChange@, connections that prevent
-- Aurora Serverless from finding a scaling point might be dropped.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/aurora-serverless.how-it-works.html#aurora-serverless.how-it-works.auto-scaling Autoscaling for Aurora Serverless>
-- in the /Amazon Aurora User Guide/.
--
-- 'secondsUntilAutoPause', 'scalingConfiguration_secondsUntilAutoPause' - The time, in seconds, before an Aurora DB cluster in @serverless@ mode
-- is paused.
--
-- 'minCapacity', 'scalingConfiguration_minCapacity' - The minimum capacity for an Aurora DB cluster in @serverless@ DB engine
-- mode.
--
-- For Aurora MySQL, valid capacity values are @1@, @2@, @4@, @8@, @16@,
-- @32@, @64@, @128@, and @256@.
--
-- For Aurora PostgreSQL, valid capacity values are @2@, @4@, @8@, @16@,
-- @32@, @64@, @192@, and @384@.
--
-- The minimum capacity must be less than or equal to the maximum capacity.
newScalingConfiguration ::
  ScalingConfiguration
newScalingConfiguration =
  ScalingConfiguration'
    { maxCapacity = Core.Nothing,
      autoPause = Core.Nothing,
      timeoutAction = Core.Nothing,
      secondsUntilAutoPause = Core.Nothing,
      minCapacity = Core.Nothing
    }

-- | The maximum capacity for an Aurora DB cluster in @serverless@ DB engine
-- mode.
--
-- For Aurora MySQL, valid capacity values are @1@, @2@, @4@, @8@, @16@,
-- @32@, @64@, @128@, and @256@.
--
-- For Aurora PostgreSQL, valid capacity values are @2@, @4@, @8@, @16@,
-- @32@, @64@, @192@, and @384@.
--
-- The maximum capacity must be greater than or equal to the minimum
-- capacity.
scalingConfiguration_maxCapacity :: Lens.Lens' ScalingConfiguration (Core.Maybe Core.Int)
scalingConfiguration_maxCapacity = Lens.lens (\ScalingConfiguration' {maxCapacity} -> maxCapacity) (\s@ScalingConfiguration' {} a -> s {maxCapacity = a} :: ScalingConfiguration)

-- | A value that indicates whether to allow or disallow automatic pause for
-- an Aurora DB cluster in @serverless@ DB engine mode. A DB cluster can be
-- paused only when it\'s idle (it has no connections).
--
-- If a DB cluster is paused for more than seven days, the DB cluster might
-- be backed up with a snapshot. In this case, the DB cluster is restored
-- when there is a request to connect to it.
scalingConfiguration_autoPause :: Lens.Lens' ScalingConfiguration (Core.Maybe Core.Bool)
scalingConfiguration_autoPause = Lens.lens (\ScalingConfiguration' {autoPause} -> autoPause) (\s@ScalingConfiguration' {} a -> s {autoPause = a} :: ScalingConfiguration)

-- | The action to take when the timeout is reached, either
-- @ForceApplyCapacityChange@ or @RollbackCapacityChange@.
--
-- @ForceApplyCapacityChange@ sets the capacity to the specified value as
-- soon as possible.
--
-- @RollbackCapacityChange@, the default, ignores the capacity change if a
-- scaling point isn\'t found in the timeout period.
--
-- If you specify @ForceApplyCapacityChange@, connections that prevent
-- Aurora Serverless from finding a scaling point might be dropped.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/aurora-serverless.how-it-works.html#aurora-serverless.how-it-works.auto-scaling Autoscaling for Aurora Serverless>
-- in the /Amazon Aurora User Guide/.
scalingConfiguration_timeoutAction :: Lens.Lens' ScalingConfiguration (Core.Maybe Core.Text)
scalingConfiguration_timeoutAction = Lens.lens (\ScalingConfiguration' {timeoutAction} -> timeoutAction) (\s@ScalingConfiguration' {} a -> s {timeoutAction = a} :: ScalingConfiguration)

-- | The time, in seconds, before an Aurora DB cluster in @serverless@ mode
-- is paused.
scalingConfiguration_secondsUntilAutoPause :: Lens.Lens' ScalingConfiguration (Core.Maybe Core.Int)
scalingConfiguration_secondsUntilAutoPause = Lens.lens (\ScalingConfiguration' {secondsUntilAutoPause} -> secondsUntilAutoPause) (\s@ScalingConfiguration' {} a -> s {secondsUntilAutoPause = a} :: ScalingConfiguration)

-- | The minimum capacity for an Aurora DB cluster in @serverless@ DB engine
-- mode.
--
-- For Aurora MySQL, valid capacity values are @1@, @2@, @4@, @8@, @16@,
-- @32@, @64@, @128@, and @256@.
--
-- For Aurora PostgreSQL, valid capacity values are @2@, @4@, @8@, @16@,
-- @32@, @64@, @192@, and @384@.
--
-- The minimum capacity must be less than or equal to the maximum capacity.
scalingConfiguration_minCapacity :: Lens.Lens' ScalingConfiguration (Core.Maybe Core.Int)
scalingConfiguration_minCapacity = Lens.lens (\ScalingConfiguration' {minCapacity} -> minCapacity) (\s@ScalingConfiguration' {} a -> s {minCapacity = a} :: ScalingConfiguration)

instance Core.Hashable ScalingConfiguration

instance Core.NFData ScalingConfiguration

instance Core.ToQuery ScalingConfiguration where
  toQuery ScalingConfiguration' {..} =
    Core.mconcat
      [ "MaxCapacity" Core.=: maxCapacity,
        "AutoPause" Core.=: autoPause,
        "TimeoutAction" Core.=: timeoutAction,
        "SecondsUntilAutoPause"
          Core.=: secondsUntilAutoPause,
        "MinCapacity" Core.=: minCapacity
      ]
