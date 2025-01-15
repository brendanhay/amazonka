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
-- Module      : Amazonka.IoTEventsData.Types.DetectorStateDefinition
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTEventsData.Types.DetectorStateDefinition where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTEventsData.Types.TimerDefinition
import Amazonka.IoTEventsData.Types.VariableDefinition
import qualified Amazonka.Prelude as Prelude

-- | The new state, variable values, and timer settings of the detector
-- (instance).
--
-- /See:/ 'newDetectorStateDefinition' smart constructor.
data DetectorStateDefinition = DetectorStateDefinition'
  { -- | The name of the new state of the detector (instance).
    stateName :: Prelude.Text,
    -- | The new values of the detector\'s variables. Any variable whose value
    -- isn\'t specified is cleared.
    variables :: [VariableDefinition],
    -- | The new values of the detector\'s timers. Any timer whose value isn\'t
    -- specified is cleared, and its timeout event won\'t occur.
    timers :: [TimerDefinition]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DetectorStateDefinition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'stateName', 'detectorStateDefinition_stateName' - The name of the new state of the detector (instance).
--
-- 'variables', 'detectorStateDefinition_variables' - The new values of the detector\'s variables. Any variable whose value
-- isn\'t specified is cleared.
--
-- 'timers', 'detectorStateDefinition_timers' - The new values of the detector\'s timers. Any timer whose value isn\'t
-- specified is cleared, and its timeout event won\'t occur.
newDetectorStateDefinition ::
  -- | 'stateName'
  Prelude.Text ->
  DetectorStateDefinition
newDetectorStateDefinition pStateName_ =
  DetectorStateDefinition'
    { stateName = pStateName_,
      variables = Prelude.mempty,
      timers = Prelude.mempty
    }

-- | The name of the new state of the detector (instance).
detectorStateDefinition_stateName :: Lens.Lens' DetectorStateDefinition Prelude.Text
detectorStateDefinition_stateName = Lens.lens (\DetectorStateDefinition' {stateName} -> stateName) (\s@DetectorStateDefinition' {} a -> s {stateName = a} :: DetectorStateDefinition)

-- | The new values of the detector\'s variables. Any variable whose value
-- isn\'t specified is cleared.
detectorStateDefinition_variables :: Lens.Lens' DetectorStateDefinition [VariableDefinition]
detectorStateDefinition_variables = Lens.lens (\DetectorStateDefinition' {variables} -> variables) (\s@DetectorStateDefinition' {} a -> s {variables = a} :: DetectorStateDefinition) Prelude.. Lens.coerced

-- | The new values of the detector\'s timers. Any timer whose value isn\'t
-- specified is cleared, and its timeout event won\'t occur.
detectorStateDefinition_timers :: Lens.Lens' DetectorStateDefinition [TimerDefinition]
detectorStateDefinition_timers = Lens.lens (\DetectorStateDefinition' {timers} -> timers) (\s@DetectorStateDefinition' {} a -> s {timers = a} :: DetectorStateDefinition) Prelude.. Lens.coerced

instance Prelude.Hashable DetectorStateDefinition where
  hashWithSalt _salt DetectorStateDefinition' {..} =
    _salt
      `Prelude.hashWithSalt` stateName
      `Prelude.hashWithSalt` variables
      `Prelude.hashWithSalt` timers

instance Prelude.NFData DetectorStateDefinition where
  rnf DetectorStateDefinition' {..} =
    Prelude.rnf stateName `Prelude.seq`
      Prelude.rnf variables `Prelude.seq`
        Prelude.rnf timers

instance Data.ToJSON DetectorStateDefinition where
  toJSON DetectorStateDefinition' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("stateName" Data..= stateName),
            Prelude.Just ("variables" Data..= variables),
            Prelude.Just ("timers" Data..= timers)
          ]
      )
