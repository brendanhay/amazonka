{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.EMR.Types.StepStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.StepStatus where

import Network.AWS.EMR.Types.FailureDetails
import Network.AWS.EMR.Types.StepState
import Network.AWS.EMR.Types.StepStateChangeReason
import Network.AWS.EMR.Types.StepTimeline
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The execution status details of the cluster step.
--
-- /See:/ 'newStepStatus' smart constructor.
data StepStatus = StepStatus'
  { -- | The reason for the step execution status change.
    stateChangeReason :: Prelude.Maybe StepStateChangeReason,
    -- | The details for the step failure including reason, message, and log file
    -- path where the root cause was identified.
    failureDetails :: Prelude.Maybe FailureDetails,
    -- | The execution state of the cluster step.
    state :: Prelude.Maybe StepState,
    -- | The timeline of the cluster step status over time.
    timeline :: Prelude.Maybe StepTimeline
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
-- 'failureDetails', 'stepStatus_failureDetails' - The details for the step failure including reason, message, and log file
-- path where the root cause was identified.
--
-- 'state', 'stepStatus_state' - The execution state of the cluster step.
--
-- 'timeline', 'stepStatus_timeline' - The timeline of the cluster step status over time.
newStepStatus ::
  StepStatus
newStepStatus =
  StepStatus'
    { stateChangeReason = Prelude.Nothing,
      failureDetails = Prelude.Nothing,
      state = Prelude.Nothing,
      timeline = Prelude.Nothing
    }

-- | The reason for the step execution status change.
stepStatus_stateChangeReason :: Lens.Lens' StepStatus (Prelude.Maybe StepStateChangeReason)
stepStatus_stateChangeReason = Lens.lens (\StepStatus' {stateChangeReason} -> stateChangeReason) (\s@StepStatus' {} a -> s {stateChangeReason = a} :: StepStatus)

-- | The details for the step failure including reason, message, and log file
-- path where the root cause was identified.
stepStatus_failureDetails :: Lens.Lens' StepStatus (Prelude.Maybe FailureDetails)
stepStatus_failureDetails = Lens.lens (\StepStatus' {failureDetails} -> failureDetails) (\s@StepStatus' {} a -> s {failureDetails = a} :: StepStatus)

-- | The execution state of the cluster step.
stepStatus_state :: Lens.Lens' StepStatus (Prelude.Maybe StepState)
stepStatus_state = Lens.lens (\StepStatus' {state} -> state) (\s@StepStatus' {} a -> s {state = a} :: StepStatus)

-- | The timeline of the cluster step status over time.
stepStatus_timeline :: Lens.Lens' StepStatus (Prelude.Maybe StepTimeline)
stepStatus_timeline = Lens.lens (\StepStatus' {timeline} -> timeline) (\s@StepStatus' {} a -> s {timeline = a} :: StepStatus)

instance Prelude.FromJSON StepStatus where
  parseJSON =
    Prelude.withObject
      "StepStatus"
      ( \x ->
          StepStatus'
            Prelude.<$> (x Prelude..:? "StateChangeReason")
            Prelude.<*> (x Prelude..:? "FailureDetails")
            Prelude.<*> (x Prelude..:? "State")
            Prelude.<*> (x Prelude..:? "Timeline")
      )

instance Prelude.Hashable StepStatus

instance Prelude.NFData StepStatus
