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
-- Module      : Network.AWS.Config.Types.RemediationExecutionStep
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.RemediationExecutionStep where

import Network.AWS.Config.Types.RemediationExecutionStepState
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Name of the step from the SSM document.
--
-- /See:/ 'newRemediationExecutionStep' smart constructor.
data RemediationExecutionStep = RemediationExecutionStep'
  { -- | The time when the step started.
    startTime :: Core.Maybe Core.POSIX,
    -- | The time when the step stopped.
    stopTime :: Core.Maybe Core.POSIX,
    -- | The valid status of the step.
    state :: Core.Maybe RemediationExecutionStepState,
    -- | The details of the step.
    name :: Core.Maybe Core.Text,
    -- | An error message if the step was interrupted during execution.
    errorMessage :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'RemediationExecutionStep' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'startTime', 'remediationExecutionStep_startTime' - The time when the step started.
--
-- 'stopTime', 'remediationExecutionStep_stopTime' - The time when the step stopped.
--
-- 'state', 'remediationExecutionStep_state' - The valid status of the step.
--
-- 'name', 'remediationExecutionStep_name' - The details of the step.
--
-- 'errorMessage', 'remediationExecutionStep_errorMessage' - An error message if the step was interrupted during execution.
newRemediationExecutionStep ::
  RemediationExecutionStep
newRemediationExecutionStep =
  RemediationExecutionStep'
    { startTime = Core.Nothing,
      stopTime = Core.Nothing,
      state = Core.Nothing,
      name = Core.Nothing,
      errorMessage = Core.Nothing
    }

-- | The time when the step started.
remediationExecutionStep_startTime :: Lens.Lens' RemediationExecutionStep (Core.Maybe Core.UTCTime)
remediationExecutionStep_startTime = Lens.lens (\RemediationExecutionStep' {startTime} -> startTime) (\s@RemediationExecutionStep' {} a -> s {startTime = a} :: RemediationExecutionStep) Core.. Lens.mapping Core._Time

-- | The time when the step stopped.
remediationExecutionStep_stopTime :: Lens.Lens' RemediationExecutionStep (Core.Maybe Core.UTCTime)
remediationExecutionStep_stopTime = Lens.lens (\RemediationExecutionStep' {stopTime} -> stopTime) (\s@RemediationExecutionStep' {} a -> s {stopTime = a} :: RemediationExecutionStep) Core.. Lens.mapping Core._Time

-- | The valid status of the step.
remediationExecutionStep_state :: Lens.Lens' RemediationExecutionStep (Core.Maybe RemediationExecutionStepState)
remediationExecutionStep_state = Lens.lens (\RemediationExecutionStep' {state} -> state) (\s@RemediationExecutionStep' {} a -> s {state = a} :: RemediationExecutionStep)

-- | The details of the step.
remediationExecutionStep_name :: Lens.Lens' RemediationExecutionStep (Core.Maybe Core.Text)
remediationExecutionStep_name = Lens.lens (\RemediationExecutionStep' {name} -> name) (\s@RemediationExecutionStep' {} a -> s {name = a} :: RemediationExecutionStep)

-- | An error message if the step was interrupted during execution.
remediationExecutionStep_errorMessage :: Lens.Lens' RemediationExecutionStep (Core.Maybe Core.Text)
remediationExecutionStep_errorMessage = Lens.lens (\RemediationExecutionStep' {errorMessage} -> errorMessage) (\s@RemediationExecutionStep' {} a -> s {errorMessage = a} :: RemediationExecutionStep)

instance Core.FromJSON RemediationExecutionStep where
  parseJSON =
    Core.withObject
      "RemediationExecutionStep"
      ( \x ->
          RemediationExecutionStep'
            Core.<$> (x Core..:? "StartTime")
            Core.<*> (x Core..:? "StopTime")
            Core.<*> (x Core..:? "State")
            Core.<*> (x Core..:? "Name")
            Core.<*> (x Core..:? "ErrorMessage")
      )

instance Core.Hashable RemediationExecutionStep

instance Core.NFData RemediationExecutionStep
