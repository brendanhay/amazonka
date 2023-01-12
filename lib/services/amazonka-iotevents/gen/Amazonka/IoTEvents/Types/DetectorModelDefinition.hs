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
-- Module      : Amazonka.IoTEvents.Types.DetectorModelDefinition
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTEvents.Types.DetectorModelDefinition where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTEvents.Types.State
import qualified Amazonka.Prelude as Prelude

-- | Information that defines how a detector operates.
--
-- /See:/ 'newDetectorModelDefinition' smart constructor.
data DetectorModelDefinition = DetectorModelDefinition'
  { -- | Information about the states of the detector.
    states :: Prelude.NonEmpty State,
    -- | The state that is entered at the creation of each detector (instance).
    initialStateName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DetectorModelDefinition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'states', 'detectorModelDefinition_states' - Information about the states of the detector.
--
-- 'initialStateName', 'detectorModelDefinition_initialStateName' - The state that is entered at the creation of each detector (instance).
newDetectorModelDefinition ::
  -- | 'states'
  Prelude.NonEmpty State ->
  -- | 'initialStateName'
  Prelude.Text ->
  DetectorModelDefinition
newDetectorModelDefinition
  pStates_
  pInitialStateName_ =
    DetectorModelDefinition'
      { states =
          Lens.coerced Lens.# pStates_,
        initialStateName = pInitialStateName_
      }

-- | Information about the states of the detector.
detectorModelDefinition_states :: Lens.Lens' DetectorModelDefinition (Prelude.NonEmpty State)
detectorModelDefinition_states = Lens.lens (\DetectorModelDefinition' {states} -> states) (\s@DetectorModelDefinition' {} a -> s {states = a} :: DetectorModelDefinition) Prelude.. Lens.coerced

-- | The state that is entered at the creation of each detector (instance).
detectorModelDefinition_initialStateName :: Lens.Lens' DetectorModelDefinition Prelude.Text
detectorModelDefinition_initialStateName = Lens.lens (\DetectorModelDefinition' {initialStateName} -> initialStateName) (\s@DetectorModelDefinition' {} a -> s {initialStateName = a} :: DetectorModelDefinition)

instance Data.FromJSON DetectorModelDefinition where
  parseJSON =
    Data.withObject
      "DetectorModelDefinition"
      ( \x ->
          DetectorModelDefinition'
            Prelude.<$> (x Data..: "states")
            Prelude.<*> (x Data..: "initialStateName")
      )

instance Prelude.Hashable DetectorModelDefinition where
  hashWithSalt _salt DetectorModelDefinition' {..} =
    _salt `Prelude.hashWithSalt` states
      `Prelude.hashWithSalt` initialStateName

instance Prelude.NFData DetectorModelDefinition where
  rnf DetectorModelDefinition' {..} =
    Prelude.rnf states
      `Prelude.seq` Prelude.rnf initialStateName

instance Data.ToJSON DetectorModelDefinition where
  toJSON DetectorModelDefinition' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("states" Data..= states),
            Prelude.Just
              ("initialStateName" Data..= initialStateName)
          ]
      )
