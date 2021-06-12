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
-- Module      : Network.AWS.RDS.Types.ScalingConfigurationInfo
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.ScalingConfigurationInfo where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Shows the scaling configuration for an Aurora DB cluster in @serverless@
-- DB engine mode.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/aurora-serverless.html Using Amazon Aurora Serverless>
-- in the /Amazon Aurora User Guide/.
--
-- /See:/ 'newScalingConfigurationInfo' smart constructor.
data ScalingConfigurationInfo = ScalingConfigurationInfo'
  { -- | The maximum capacity for an Aurora DB cluster in @serverless@ DB engine
    -- mode.
    maxCapacity :: Core.Maybe Core.Int,
    -- | A value that indicates whether automatic pause is allowed for the Aurora
    -- DB cluster in @serverless@ DB engine mode.
    --
    -- When the value is set to false for an Aurora Serverless DB cluster, the
    -- DB cluster automatically resumes.
    autoPause :: Core.Maybe Core.Bool,
    -- | The timeout action of a call to @ModifyCurrentDBClusterCapacity@, either
    -- @ForceApplyCapacityChange@ or @RollbackCapacityChange@.
    timeoutAction :: Core.Maybe Core.Text,
    -- | The remaining amount of time, in seconds, before the Aurora DB cluster
    -- in @serverless@ mode is paused. A DB cluster can be paused only when
    -- it\'s idle (it has no connections).
    secondsUntilAutoPause :: Core.Maybe Core.Int,
    -- | The maximum capacity for the Aurora DB cluster in @serverless@ DB engine
    -- mode.
    minCapacity :: Core.Maybe Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ScalingConfigurationInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxCapacity', 'scalingConfigurationInfo_maxCapacity' - The maximum capacity for an Aurora DB cluster in @serverless@ DB engine
-- mode.
--
-- 'autoPause', 'scalingConfigurationInfo_autoPause' - A value that indicates whether automatic pause is allowed for the Aurora
-- DB cluster in @serverless@ DB engine mode.
--
-- When the value is set to false for an Aurora Serverless DB cluster, the
-- DB cluster automatically resumes.
--
-- 'timeoutAction', 'scalingConfigurationInfo_timeoutAction' - The timeout action of a call to @ModifyCurrentDBClusterCapacity@, either
-- @ForceApplyCapacityChange@ or @RollbackCapacityChange@.
--
-- 'secondsUntilAutoPause', 'scalingConfigurationInfo_secondsUntilAutoPause' - The remaining amount of time, in seconds, before the Aurora DB cluster
-- in @serverless@ mode is paused. A DB cluster can be paused only when
-- it\'s idle (it has no connections).
--
-- 'minCapacity', 'scalingConfigurationInfo_minCapacity' - The maximum capacity for the Aurora DB cluster in @serverless@ DB engine
-- mode.
newScalingConfigurationInfo ::
  ScalingConfigurationInfo
newScalingConfigurationInfo =
  ScalingConfigurationInfo'
    { maxCapacity =
        Core.Nothing,
      autoPause = Core.Nothing,
      timeoutAction = Core.Nothing,
      secondsUntilAutoPause = Core.Nothing,
      minCapacity = Core.Nothing
    }

-- | The maximum capacity for an Aurora DB cluster in @serverless@ DB engine
-- mode.
scalingConfigurationInfo_maxCapacity :: Lens.Lens' ScalingConfigurationInfo (Core.Maybe Core.Int)
scalingConfigurationInfo_maxCapacity = Lens.lens (\ScalingConfigurationInfo' {maxCapacity} -> maxCapacity) (\s@ScalingConfigurationInfo' {} a -> s {maxCapacity = a} :: ScalingConfigurationInfo)

-- | A value that indicates whether automatic pause is allowed for the Aurora
-- DB cluster in @serverless@ DB engine mode.
--
-- When the value is set to false for an Aurora Serverless DB cluster, the
-- DB cluster automatically resumes.
scalingConfigurationInfo_autoPause :: Lens.Lens' ScalingConfigurationInfo (Core.Maybe Core.Bool)
scalingConfigurationInfo_autoPause = Lens.lens (\ScalingConfigurationInfo' {autoPause} -> autoPause) (\s@ScalingConfigurationInfo' {} a -> s {autoPause = a} :: ScalingConfigurationInfo)

-- | The timeout action of a call to @ModifyCurrentDBClusterCapacity@, either
-- @ForceApplyCapacityChange@ or @RollbackCapacityChange@.
scalingConfigurationInfo_timeoutAction :: Lens.Lens' ScalingConfigurationInfo (Core.Maybe Core.Text)
scalingConfigurationInfo_timeoutAction = Lens.lens (\ScalingConfigurationInfo' {timeoutAction} -> timeoutAction) (\s@ScalingConfigurationInfo' {} a -> s {timeoutAction = a} :: ScalingConfigurationInfo)

-- | The remaining amount of time, in seconds, before the Aurora DB cluster
-- in @serverless@ mode is paused. A DB cluster can be paused only when
-- it\'s idle (it has no connections).
scalingConfigurationInfo_secondsUntilAutoPause :: Lens.Lens' ScalingConfigurationInfo (Core.Maybe Core.Int)
scalingConfigurationInfo_secondsUntilAutoPause = Lens.lens (\ScalingConfigurationInfo' {secondsUntilAutoPause} -> secondsUntilAutoPause) (\s@ScalingConfigurationInfo' {} a -> s {secondsUntilAutoPause = a} :: ScalingConfigurationInfo)

-- | The maximum capacity for the Aurora DB cluster in @serverless@ DB engine
-- mode.
scalingConfigurationInfo_minCapacity :: Lens.Lens' ScalingConfigurationInfo (Core.Maybe Core.Int)
scalingConfigurationInfo_minCapacity = Lens.lens (\ScalingConfigurationInfo' {minCapacity} -> minCapacity) (\s@ScalingConfigurationInfo' {} a -> s {minCapacity = a} :: ScalingConfigurationInfo)

instance Core.FromXML ScalingConfigurationInfo where
  parseXML x =
    ScalingConfigurationInfo'
      Core.<$> (x Core..@? "MaxCapacity")
      Core.<*> (x Core..@? "AutoPause")
      Core.<*> (x Core..@? "TimeoutAction")
      Core.<*> (x Core..@? "SecondsUntilAutoPause")
      Core.<*> (x Core..@? "MinCapacity")

instance Core.Hashable ScalingConfigurationInfo

instance Core.NFData ScalingConfigurationInfo
