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
-- Module      : Amazonka.Synthetics.Types.CanaryScheduleInput
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Synthetics.Types.CanaryScheduleInput where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | This structure specifies how often a canary is to make runs and the date
-- and time when it should stop making runs.
--
-- /See:/ 'newCanaryScheduleInput' smart constructor.
data CanaryScheduleInput = CanaryScheduleInput'
  { -- | How long, in seconds, for the canary to continue making regular runs
    -- according to the schedule in the @Expression@ value. If you specify 0,
    -- the canary continues making runs until you stop it. If you omit this
    -- field, the default of 0 is used.
    durationInSeconds :: Prelude.Maybe Prelude.Natural,
    -- | A @rate@ expression or a @cron@ expression that defines how often the
    -- canary is to run.
    --
    -- For a rate expression, The syntax is @rate(number unit)@. /unit/ can be
    -- @minute@, @minutes@, or @hour@.
    --
    -- For example, @rate(1 minute)@ runs the canary once a minute,
    -- @rate(10 minutes)@ runs it once every 10 minutes, and @rate(1 hour)@
    -- runs it once every hour. You can specify a frequency between
    -- @rate(1 minute)@ and @rate(1 hour)@.
    --
    -- Specifying @rate(0 minute)@ or @rate(0 hour)@ is a special value that
    -- causes the canary to run only once when it is started.
    --
    -- Use @cron(expression)@ to specify a cron expression. You can\'t schedule
    -- a canary to wait for more than a year before running. For information
    -- about the syntax for cron expressions, see
    -- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/CloudWatch_Synthetics_Canaries_cron.html Scheduling canary runs using cron>.
    expression :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CanaryScheduleInput' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'durationInSeconds', 'canaryScheduleInput_durationInSeconds' - How long, in seconds, for the canary to continue making regular runs
-- according to the schedule in the @Expression@ value. If you specify 0,
-- the canary continues making runs until you stop it. If you omit this
-- field, the default of 0 is used.
--
-- 'expression', 'canaryScheduleInput_expression' - A @rate@ expression or a @cron@ expression that defines how often the
-- canary is to run.
--
-- For a rate expression, The syntax is @rate(number unit)@. /unit/ can be
-- @minute@, @minutes@, or @hour@.
--
-- For example, @rate(1 minute)@ runs the canary once a minute,
-- @rate(10 minutes)@ runs it once every 10 minutes, and @rate(1 hour)@
-- runs it once every hour. You can specify a frequency between
-- @rate(1 minute)@ and @rate(1 hour)@.
--
-- Specifying @rate(0 minute)@ or @rate(0 hour)@ is a special value that
-- causes the canary to run only once when it is started.
--
-- Use @cron(expression)@ to specify a cron expression. You can\'t schedule
-- a canary to wait for more than a year before running. For information
-- about the syntax for cron expressions, see
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/CloudWatch_Synthetics_Canaries_cron.html Scheduling canary runs using cron>.
newCanaryScheduleInput ::
  -- | 'expression'
  Prelude.Text ->
  CanaryScheduleInput
newCanaryScheduleInput pExpression_ =
  CanaryScheduleInput'
    { durationInSeconds =
        Prelude.Nothing,
      expression = pExpression_
    }

-- | How long, in seconds, for the canary to continue making regular runs
-- according to the schedule in the @Expression@ value. If you specify 0,
-- the canary continues making runs until you stop it. If you omit this
-- field, the default of 0 is used.
canaryScheduleInput_durationInSeconds :: Lens.Lens' CanaryScheduleInput (Prelude.Maybe Prelude.Natural)
canaryScheduleInput_durationInSeconds = Lens.lens (\CanaryScheduleInput' {durationInSeconds} -> durationInSeconds) (\s@CanaryScheduleInput' {} a -> s {durationInSeconds = a} :: CanaryScheduleInput)

-- | A @rate@ expression or a @cron@ expression that defines how often the
-- canary is to run.
--
-- For a rate expression, The syntax is @rate(number unit)@. /unit/ can be
-- @minute@, @minutes@, or @hour@.
--
-- For example, @rate(1 minute)@ runs the canary once a minute,
-- @rate(10 minutes)@ runs it once every 10 minutes, and @rate(1 hour)@
-- runs it once every hour. You can specify a frequency between
-- @rate(1 minute)@ and @rate(1 hour)@.
--
-- Specifying @rate(0 minute)@ or @rate(0 hour)@ is a special value that
-- causes the canary to run only once when it is started.
--
-- Use @cron(expression)@ to specify a cron expression. You can\'t schedule
-- a canary to wait for more than a year before running. For information
-- about the syntax for cron expressions, see
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/CloudWatch_Synthetics_Canaries_cron.html Scheduling canary runs using cron>.
canaryScheduleInput_expression :: Lens.Lens' CanaryScheduleInput Prelude.Text
canaryScheduleInput_expression = Lens.lens (\CanaryScheduleInput' {expression} -> expression) (\s@CanaryScheduleInput' {} a -> s {expression = a} :: CanaryScheduleInput)

instance Prelude.Hashable CanaryScheduleInput where
  hashWithSalt _salt CanaryScheduleInput' {..} =
    _salt `Prelude.hashWithSalt` durationInSeconds
      `Prelude.hashWithSalt` expression

instance Prelude.NFData CanaryScheduleInput where
  rnf CanaryScheduleInput' {..} =
    Prelude.rnf durationInSeconds
      `Prelude.seq` Prelude.rnf expression

instance Data.ToJSON CanaryScheduleInput where
  toJSON CanaryScheduleInput' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DurationInSeconds" Data..=)
              Prelude.<$> durationInSeconds,
            Prelude.Just ("Expression" Data..= expression)
          ]
      )
