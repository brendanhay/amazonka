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
-- Module      : Amazonka.Inspector.Types.AssessmentRunStateChange
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Inspector.Types.AssessmentRunStateChange where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Inspector.Types.AssessmentRunState
import qualified Amazonka.Prelude as Prelude

-- | Used as one of the elements of the AssessmentRun data type.
--
-- /See:/ 'newAssessmentRunStateChange' smart constructor.
data AssessmentRunStateChange = AssessmentRunStateChange'
  { -- | The last time the assessment run state changed.
    stateChangedAt :: Data.POSIX,
    -- | The assessment run state.
    state :: AssessmentRunState
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssessmentRunStateChange' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'stateChangedAt', 'assessmentRunStateChange_stateChangedAt' - The last time the assessment run state changed.
--
-- 'state', 'assessmentRunStateChange_state' - The assessment run state.
newAssessmentRunStateChange ::
  -- | 'stateChangedAt'
  Prelude.UTCTime ->
  -- | 'state'
  AssessmentRunState ->
  AssessmentRunStateChange
newAssessmentRunStateChange pStateChangedAt_ pState_ =
  AssessmentRunStateChange'
    { stateChangedAt =
        Data._Time Lens.# pStateChangedAt_,
      state = pState_
    }

-- | The last time the assessment run state changed.
assessmentRunStateChange_stateChangedAt :: Lens.Lens' AssessmentRunStateChange Prelude.UTCTime
assessmentRunStateChange_stateChangedAt = Lens.lens (\AssessmentRunStateChange' {stateChangedAt} -> stateChangedAt) (\s@AssessmentRunStateChange' {} a -> s {stateChangedAt = a} :: AssessmentRunStateChange) Prelude.. Data._Time

-- | The assessment run state.
assessmentRunStateChange_state :: Lens.Lens' AssessmentRunStateChange AssessmentRunState
assessmentRunStateChange_state = Lens.lens (\AssessmentRunStateChange' {state} -> state) (\s@AssessmentRunStateChange' {} a -> s {state = a} :: AssessmentRunStateChange)

instance Data.FromJSON AssessmentRunStateChange where
  parseJSON =
    Data.withObject
      "AssessmentRunStateChange"
      ( \x ->
          AssessmentRunStateChange'
            Prelude.<$> (x Data..: "stateChangedAt")
            Prelude.<*> (x Data..: "state")
      )

instance Prelude.Hashable AssessmentRunStateChange where
  hashWithSalt _salt AssessmentRunStateChange' {..} =
    _salt `Prelude.hashWithSalt` stateChangedAt
      `Prelude.hashWithSalt` state

instance Prelude.NFData AssessmentRunStateChange where
  rnf AssessmentRunStateChange' {..} =
    Prelude.rnf stateChangedAt
      `Prelude.seq` Prelude.rnf state
