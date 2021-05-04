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
-- Module      : Network.AWS.MediaLive.Types.FeatureActivations
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.FeatureActivations where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.FeatureActivationsInputPrepareScheduleActions
import qualified Network.AWS.Prelude as Prelude

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.FromJSON FeatureActivations where
  parseJSON =
    Prelude.withObject
      "FeatureActivations"
      ( \x ->
          FeatureActivations'
            Prelude.<$> (x Prelude..:? "inputPrepareScheduleActions")
      )

instance Prelude.Hashable FeatureActivations

instance Prelude.NFData FeatureActivations

instance Prelude.ToJSON FeatureActivations where
  toJSON FeatureActivations' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("inputPrepareScheduleActions" Prelude..=)
              Prelude.<$> inputPrepareScheduleActions
          ]
      )
