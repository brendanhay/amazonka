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
-- Module      : Network.AWS.Inspector.Types.AssessmentRunStateChange
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Inspector.Types.AssessmentRunStateChange where

import Network.AWS.Inspector.Types.AssessmentRunState
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Used as one of the elements of the AssessmentRun data type.
--
-- /See:/ 'newAssessmentRunStateChange' smart constructor.
data AssessmentRunStateChange = AssessmentRunStateChange'
  { -- | The last time the assessment run state changed.
    stateChangedAt :: Prelude.POSIX,
    -- | The assessment run state.
    state :: AssessmentRunState
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude._Time Lens.# pStateChangedAt_,
      state = pState_
    }

-- | The last time the assessment run state changed.
assessmentRunStateChange_stateChangedAt :: Lens.Lens' AssessmentRunStateChange Prelude.UTCTime
assessmentRunStateChange_stateChangedAt = Lens.lens (\AssessmentRunStateChange' {stateChangedAt} -> stateChangedAt) (\s@AssessmentRunStateChange' {} a -> s {stateChangedAt = a} :: AssessmentRunStateChange) Prelude.. Prelude._Time

-- | The assessment run state.
assessmentRunStateChange_state :: Lens.Lens' AssessmentRunStateChange AssessmentRunState
assessmentRunStateChange_state = Lens.lens (\AssessmentRunStateChange' {state} -> state) (\s@AssessmentRunStateChange' {} a -> s {state = a} :: AssessmentRunStateChange)

instance Prelude.FromJSON AssessmentRunStateChange where
  parseJSON =
    Prelude.withObject
      "AssessmentRunStateChange"
      ( \x ->
          AssessmentRunStateChange'
            Prelude.<$> (x Prelude..: "stateChangedAt")
            Prelude.<*> (x Prelude..: "state")
      )

instance Prelude.Hashable AssessmentRunStateChange

instance Prelude.NFData AssessmentRunStateChange
