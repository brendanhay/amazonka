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
-- Module      : Amazonka.RDS.Types.ScalingConfigurationInfo
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RDS.Types.ScalingConfigurationInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The scaling configuration for an Aurora DB cluster in @serverless@ DB
-- engine mode.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/aurora-serverless.html Using Amazon Aurora Serverless v1>
-- in the /Amazon Aurora User Guide/.
--
-- /See:/ 'newScalingConfigurationInfo' smart constructor.
data ScalingConfigurationInfo = ScalingConfigurationInfo'
  { -- | A value that indicates whether automatic pause is allowed for the Aurora
    -- DB cluster in @serverless@ DB engine mode.
    --
    -- When the value is set to false for an Aurora Serverless v1 DB cluster,
    -- the DB cluster automatically resumes.
    autoPause :: Prelude.Maybe Prelude.Bool,
    -- | The maximum capacity for an Aurora DB cluster in @serverless@ DB engine
    -- mode.
    maxCapacity :: Prelude.Maybe Prelude.Int,
    -- | The minimum capacity for an Aurora DB cluster in @serverless@ DB engine
    -- mode.
    minCapacity :: Prelude.Maybe Prelude.Int,
    -- | The number of seconds before scaling times out. What happens when an
    -- attempted scaling action times out is determined by the @TimeoutAction@
    -- setting.
    secondsBeforeTimeout :: Prelude.Maybe Prelude.Int,
    -- | The remaining amount of time, in seconds, before the Aurora DB cluster
    -- in @serverless@ mode is paused. A DB cluster can be paused only when
    -- it\'s idle (it has no connections).
    secondsUntilAutoPause :: Prelude.Maybe Prelude.Int,
    -- | The action that occurs when Aurora times out while attempting to change
    -- the capacity of an Aurora Serverless v1 cluster. The value is either
    -- @ForceApplyCapacityChange@ or @RollbackCapacityChange@.
    --
    -- @ForceApplyCapacityChange@, the default, sets the capacity to the
    -- specified value as soon as possible.
    --
    -- @RollbackCapacityChange@ ignores the capacity change if a scaling point
    -- isn\'t found in the timeout period.
    timeoutAction :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ScalingConfigurationInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'autoPause', 'scalingConfigurationInfo_autoPause' - A value that indicates whether automatic pause is allowed for the Aurora
-- DB cluster in @serverless@ DB engine mode.
--
-- When the value is set to false for an Aurora Serverless v1 DB cluster,
-- the DB cluster automatically resumes.
--
-- 'maxCapacity', 'scalingConfigurationInfo_maxCapacity' - The maximum capacity for an Aurora DB cluster in @serverless@ DB engine
-- mode.
--
-- 'minCapacity', 'scalingConfigurationInfo_minCapacity' - The minimum capacity for an Aurora DB cluster in @serverless@ DB engine
-- mode.
--
-- 'secondsBeforeTimeout', 'scalingConfigurationInfo_secondsBeforeTimeout' - The number of seconds before scaling times out. What happens when an
-- attempted scaling action times out is determined by the @TimeoutAction@
-- setting.
--
-- 'secondsUntilAutoPause', 'scalingConfigurationInfo_secondsUntilAutoPause' - The remaining amount of time, in seconds, before the Aurora DB cluster
-- in @serverless@ mode is paused. A DB cluster can be paused only when
-- it\'s idle (it has no connections).
--
-- 'timeoutAction', 'scalingConfigurationInfo_timeoutAction' - The action that occurs when Aurora times out while attempting to change
-- the capacity of an Aurora Serverless v1 cluster. The value is either
-- @ForceApplyCapacityChange@ or @RollbackCapacityChange@.
--
-- @ForceApplyCapacityChange@, the default, sets the capacity to the
-- specified value as soon as possible.
--
-- @RollbackCapacityChange@ ignores the capacity change if a scaling point
-- isn\'t found in the timeout period.
newScalingConfigurationInfo ::
  ScalingConfigurationInfo
newScalingConfigurationInfo =
  ScalingConfigurationInfo'
    { autoPause =
        Prelude.Nothing,
      maxCapacity = Prelude.Nothing,
      minCapacity = Prelude.Nothing,
      secondsBeforeTimeout = Prelude.Nothing,
      secondsUntilAutoPause = Prelude.Nothing,
      timeoutAction = Prelude.Nothing
    }

-- | A value that indicates whether automatic pause is allowed for the Aurora
-- DB cluster in @serverless@ DB engine mode.
--
-- When the value is set to false for an Aurora Serverless v1 DB cluster,
-- the DB cluster automatically resumes.
scalingConfigurationInfo_autoPause :: Lens.Lens' ScalingConfigurationInfo (Prelude.Maybe Prelude.Bool)
scalingConfigurationInfo_autoPause = Lens.lens (\ScalingConfigurationInfo' {autoPause} -> autoPause) (\s@ScalingConfigurationInfo' {} a -> s {autoPause = a} :: ScalingConfigurationInfo)

-- | The maximum capacity for an Aurora DB cluster in @serverless@ DB engine
-- mode.
scalingConfigurationInfo_maxCapacity :: Lens.Lens' ScalingConfigurationInfo (Prelude.Maybe Prelude.Int)
scalingConfigurationInfo_maxCapacity = Lens.lens (\ScalingConfigurationInfo' {maxCapacity} -> maxCapacity) (\s@ScalingConfigurationInfo' {} a -> s {maxCapacity = a} :: ScalingConfigurationInfo)

-- | The minimum capacity for an Aurora DB cluster in @serverless@ DB engine
-- mode.
scalingConfigurationInfo_minCapacity :: Lens.Lens' ScalingConfigurationInfo (Prelude.Maybe Prelude.Int)
scalingConfigurationInfo_minCapacity = Lens.lens (\ScalingConfigurationInfo' {minCapacity} -> minCapacity) (\s@ScalingConfigurationInfo' {} a -> s {minCapacity = a} :: ScalingConfigurationInfo)

-- | The number of seconds before scaling times out. What happens when an
-- attempted scaling action times out is determined by the @TimeoutAction@
-- setting.
scalingConfigurationInfo_secondsBeforeTimeout :: Lens.Lens' ScalingConfigurationInfo (Prelude.Maybe Prelude.Int)
scalingConfigurationInfo_secondsBeforeTimeout = Lens.lens (\ScalingConfigurationInfo' {secondsBeforeTimeout} -> secondsBeforeTimeout) (\s@ScalingConfigurationInfo' {} a -> s {secondsBeforeTimeout = a} :: ScalingConfigurationInfo)

-- | The remaining amount of time, in seconds, before the Aurora DB cluster
-- in @serverless@ mode is paused. A DB cluster can be paused only when
-- it\'s idle (it has no connections).
scalingConfigurationInfo_secondsUntilAutoPause :: Lens.Lens' ScalingConfigurationInfo (Prelude.Maybe Prelude.Int)
scalingConfigurationInfo_secondsUntilAutoPause = Lens.lens (\ScalingConfigurationInfo' {secondsUntilAutoPause} -> secondsUntilAutoPause) (\s@ScalingConfigurationInfo' {} a -> s {secondsUntilAutoPause = a} :: ScalingConfigurationInfo)

-- | The action that occurs when Aurora times out while attempting to change
-- the capacity of an Aurora Serverless v1 cluster. The value is either
-- @ForceApplyCapacityChange@ or @RollbackCapacityChange@.
--
-- @ForceApplyCapacityChange@, the default, sets the capacity to the
-- specified value as soon as possible.
--
-- @RollbackCapacityChange@ ignores the capacity change if a scaling point
-- isn\'t found in the timeout period.
scalingConfigurationInfo_timeoutAction :: Lens.Lens' ScalingConfigurationInfo (Prelude.Maybe Prelude.Text)
scalingConfigurationInfo_timeoutAction = Lens.lens (\ScalingConfigurationInfo' {timeoutAction} -> timeoutAction) (\s@ScalingConfigurationInfo' {} a -> s {timeoutAction = a} :: ScalingConfigurationInfo)

instance Data.FromXML ScalingConfigurationInfo where
  parseXML x =
    ScalingConfigurationInfo'
      Prelude.<$> (x Data..@? "AutoPause")
      Prelude.<*> (x Data..@? "MaxCapacity")
      Prelude.<*> (x Data..@? "MinCapacity")
      Prelude.<*> (x Data..@? "SecondsBeforeTimeout")
      Prelude.<*> (x Data..@? "SecondsUntilAutoPause")
      Prelude.<*> (x Data..@? "TimeoutAction")

instance Prelude.Hashable ScalingConfigurationInfo where
  hashWithSalt _salt ScalingConfigurationInfo' {..} =
    _salt
      `Prelude.hashWithSalt` autoPause
      `Prelude.hashWithSalt` maxCapacity
      `Prelude.hashWithSalt` minCapacity
      `Prelude.hashWithSalt` secondsBeforeTimeout
      `Prelude.hashWithSalt` secondsUntilAutoPause
      `Prelude.hashWithSalt` timeoutAction

instance Prelude.NFData ScalingConfigurationInfo where
  rnf ScalingConfigurationInfo' {..} =
    Prelude.rnf autoPause
      `Prelude.seq` Prelude.rnf maxCapacity
      `Prelude.seq` Prelude.rnf minCapacity
      `Prelude.seq` Prelude.rnf secondsBeforeTimeout
      `Prelude.seq` Prelude.rnf secondsUntilAutoPause
      `Prelude.seq` Prelude.rnf timeoutAction
