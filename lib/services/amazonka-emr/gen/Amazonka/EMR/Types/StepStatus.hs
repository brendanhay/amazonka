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
-- Module      : Amazonka.EMR.Types.StepStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EMR.Types.StepStatus where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EMR.Types.FailureDetails
import Amazonka.EMR.Types.StepState
import Amazonka.EMR.Types.StepStateChangeReason
import Amazonka.EMR.Types.StepTimeline
import qualified Amazonka.Prelude as Prelude

-- | The execution status details of the cluster step.
--
-- /See:/ 'newStepStatus' smart constructor.
data StepStatus = StepStatus'
  { -- | The reason for the step execution status change.
    stateChangeReason :: Prelude.Maybe StepStateChangeReason,
    -- | The timeline of the cluster step status over time.
    timeline :: Prelude.Maybe StepTimeline,
    -- | The execution state of the cluster step.
    state :: Prelude.Maybe StepState,
    -- | The details for the step failure including reason, message, and log file
    -- path where the root cause was identified.
    failureDetails :: Prelude.Maybe FailureDetails
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StepStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'stateChangeReason', 'stepStatus_stateChangeReason' - The reason for the step execution status change.
--
-- 'timeline', 'stepStatus_timeline' - The timeline of the cluster step status over time.
--
-- 'state', 'stepStatus_state' - The execution state of the cluster step.
--
-- 'failureDetails', 'stepStatus_failureDetails' - The details for the step failure including reason, message, and log file
-- path where the root cause was identified.
newStepStatus ::
  StepStatus
newStepStatus =
  StepStatus'
    { stateChangeReason = Prelude.Nothing,
      timeline = Prelude.Nothing,
      state = Prelude.Nothing,
      failureDetails = Prelude.Nothing
    }

-- | The reason for the step execution status change.
stepStatus_stateChangeReason :: Lens.Lens' StepStatus (Prelude.Maybe StepStateChangeReason)
stepStatus_stateChangeReason = Lens.lens (\StepStatus' {stateChangeReason} -> stateChangeReason) (\s@StepStatus' {} a -> s {stateChangeReason = a} :: StepStatus)

-- | The timeline of the cluster step status over time.
stepStatus_timeline :: Lens.Lens' StepStatus (Prelude.Maybe StepTimeline)
stepStatus_timeline = Lens.lens (\StepStatus' {timeline} -> timeline) (\s@StepStatus' {} a -> s {timeline = a} :: StepStatus)

-- | The execution state of the cluster step.
stepStatus_state :: Lens.Lens' StepStatus (Prelude.Maybe StepState)
stepStatus_state = Lens.lens (\StepStatus' {state} -> state) (\s@StepStatus' {} a -> s {state = a} :: StepStatus)

-- | The details for the step failure including reason, message, and log file
-- path where the root cause was identified.
stepStatus_failureDetails :: Lens.Lens' StepStatus (Prelude.Maybe FailureDetails)
stepStatus_failureDetails = Lens.lens (\StepStatus' {failureDetails} -> failureDetails) (\s@StepStatus' {} a -> s {failureDetails = a} :: StepStatus)

instance Data.FromJSON StepStatus where
  parseJSON =
    Data.withObject
      "StepStatus"
      ( \x ->
          StepStatus'
            Prelude.<$> (x Data..:? "StateChangeReason")
            Prelude.<*> (x Data..:? "Timeline")
            Prelude.<*> (x Data..:? "State")
            Prelude.<*> (x Data..:? "FailureDetails")
      )

instance Prelude.Hashable StepStatus where
  hashWithSalt _salt StepStatus' {..} =
    _salt `Prelude.hashWithSalt` stateChangeReason
      `Prelude.hashWithSalt` timeline
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` failureDetails

instance Prelude.NFData StepStatus where
  rnf StepStatus' {..} =
    Prelude.rnf stateChangeReason
      `Prelude.seq` Prelude.rnf timeline
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf failureDetails
