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
-- Module      : Amazonka.MediaLive.Types.FeatureActivations
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.FeatureActivations where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaLive.Types.FeatureActivationsInputPrepareScheduleActions
import qualified Amazonka.Prelude as Prelude

-- | Feature Activations
--
-- /See:/ 'newFeatureActivations' smart constructor.
data FeatureActivations = FeatureActivations'
  { -- | Enables the Input Prepare feature. You can create Input Prepare actions
    -- in the schedule only if this feature is enabled. If you disable the
    -- feature on an existing schedule, make sure that you first delete all
    -- input prepare actions from the schedule.
    inputPrepareScheduleActions :: Prelude.Maybe FeatureActivationsInputPrepareScheduleActions
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FeatureActivations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'inputPrepareScheduleActions', 'featureActivations_inputPrepareScheduleActions' - Enables the Input Prepare feature. You can create Input Prepare actions
-- in the schedule only if this feature is enabled. If you disable the
-- feature on an existing schedule, make sure that you first delete all
-- input prepare actions from the schedule.
newFeatureActivations ::
  FeatureActivations
newFeatureActivations =
  FeatureActivations'
    { inputPrepareScheduleActions =
        Prelude.Nothing
    }

-- | Enables the Input Prepare feature. You can create Input Prepare actions
-- in the schedule only if this feature is enabled. If you disable the
-- feature on an existing schedule, make sure that you first delete all
-- input prepare actions from the schedule.
featureActivations_inputPrepareScheduleActions :: Lens.Lens' FeatureActivations (Prelude.Maybe FeatureActivationsInputPrepareScheduleActions)
featureActivations_inputPrepareScheduleActions = Lens.lens (\FeatureActivations' {inputPrepareScheduleActions} -> inputPrepareScheduleActions) (\s@FeatureActivations' {} a -> s {inputPrepareScheduleActions = a} :: FeatureActivations)

instance Data.FromJSON FeatureActivations where
  parseJSON =
    Data.withObject
      "FeatureActivations"
      ( \x ->
          FeatureActivations'
            Prelude.<$> (x Data..:? "inputPrepareScheduleActions")
      )

instance Prelude.Hashable FeatureActivations where
  hashWithSalt _salt FeatureActivations' {..} =
    _salt
      `Prelude.hashWithSalt` inputPrepareScheduleActions

instance Prelude.NFData FeatureActivations where
  rnf FeatureActivations' {..} =
    Prelude.rnf inputPrepareScheduleActions

instance Data.ToJSON FeatureActivations where
  toJSON FeatureActivations' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("inputPrepareScheduleActions" Data..=)
              Prelude.<$> inputPrepareScheduleActions
          ]
      )
