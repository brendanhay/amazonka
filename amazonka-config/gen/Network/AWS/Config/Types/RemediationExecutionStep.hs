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
-- Module      : Network.AWS.Config.Types.RemediationExecutionStep
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.RemediationExecutionStep where

import Network.AWS.Config.Types.RemediationExecutionStepState
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Name of the step from the SSM document.
--
-- /See:/ 'newRemediationExecutionStep' smart constructor.
data RemediationExecutionStep = RemediationExecutionStep'
  { -- | The time when the step started.
    startTime :: Prelude.Maybe Prelude.POSIX,
    -- | The time when the step stopped.
    stopTime :: Prelude.Maybe Prelude.POSIX,
    -- | The valid status of the step.
    state :: Prelude.Maybe RemediationExecutionStepState,
    -- | The details of the step.
    name :: Prelude.Maybe Prelude.Text,
    -- | An error message if the step was interrupted during execution.
    errorMessage :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { startTime =
        Prelude.Nothing,
      stopTime = Prelude.Nothing,
      state = Prelude.Nothing,
      name = Prelude.Nothing,
      errorMessage = Prelude.Nothing
    }

-- | The time when the step started.
remediationExecutionStep_startTime :: Lens.Lens' RemediationExecutionStep (Prelude.Maybe Prelude.UTCTime)
remediationExecutionStep_startTime = Lens.lens (\RemediationExecutionStep' {startTime} -> startTime) (\s@RemediationExecutionStep' {} a -> s {startTime = a} :: RemediationExecutionStep) Prelude.. Lens.mapping Prelude._Time

-- | The time when the step stopped.
remediationExecutionStep_stopTime :: Lens.Lens' RemediationExecutionStep (Prelude.Maybe Prelude.UTCTime)
remediationExecutionStep_stopTime = Lens.lens (\RemediationExecutionStep' {stopTime} -> stopTime) (\s@RemediationExecutionStep' {} a -> s {stopTime = a} :: RemediationExecutionStep) Prelude.. Lens.mapping Prelude._Time

-- | The valid status of the step.
remediationExecutionStep_state :: Lens.Lens' RemediationExecutionStep (Prelude.Maybe RemediationExecutionStepState)
remediationExecutionStep_state = Lens.lens (\RemediationExecutionStep' {state} -> state) (\s@RemediationExecutionStep' {} a -> s {state = a} :: RemediationExecutionStep)

-- | The details of the step.
remediationExecutionStep_name :: Lens.Lens' RemediationExecutionStep (Prelude.Maybe Prelude.Text)
remediationExecutionStep_name = Lens.lens (\RemediationExecutionStep' {name} -> name) (\s@RemediationExecutionStep' {} a -> s {name = a} :: RemediationExecutionStep)

-- | An error message if the step was interrupted during execution.
remediationExecutionStep_errorMessage :: Lens.Lens' RemediationExecutionStep (Prelude.Maybe Prelude.Text)
remediationExecutionStep_errorMessage = Lens.lens (\RemediationExecutionStep' {errorMessage} -> errorMessage) (\s@RemediationExecutionStep' {} a -> s {errorMessage = a} :: RemediationExecutionStep)

instance Prelude.FromJSON RemediationExecutionStep where
  parseJSON =
    Prelude.withObject
      "RemediationExecutionStep"
      ( \x ->
          RemediationExecutionStep'
            Prelude.<$> (x Prelude..:? "StartTime")
            Prelude.<*> (x Prelude..:? "StopTime")
            Prelude.<*> (x Prelude..:? "State")
            Prelude.<*> (x Prelude..:? "Name")
            Prelude.<*> (x Prelude..:? "ErrorMessage")
      )

instance Prelude.Hashable RemediationExecutionStep

instance Prelude.NFData RemediationExecutionStep
