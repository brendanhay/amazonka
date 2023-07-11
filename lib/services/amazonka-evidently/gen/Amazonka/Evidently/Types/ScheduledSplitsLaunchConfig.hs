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
-- Module      : Amazonka.Evidently.Types.ScheduledSplitsLaunchConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Evidently.Types.ScheduledSplitsLaunchConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Evidently.Types.ScheduledSplitConfig
import qualified Amazonka.Prelude as Prelude

-- | An array of structures that define the traffic allocation percentages
-- among the feature variations during each step of a launch. This also
-- defines the start time of each step.
--
-- /See:/ 'newScheduledSplitsLaunchConfig' smart constructor.
data ScheduledSplitsLaunchConfig = ScheduledSplitsLaunchConfig'
  { -- | An array of structures that define the traffic allocation percentages
    -- among the feature variations during each step of the launch. This also
    -- defines the start time of each step.
    steps :: Prelude.NonEmpty ScheduledSplitConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ScheduledSplitsLaunchConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'steps', 'scheduledSplitsLaunchConfig_steps' - An array of structures that define the traffic allocation percentages
-- among the feature variations during each step of the launch. This also
-- defines the start time of each step.
newScheduledSplitsLaunchConfig ::
  -- | 'steps'
  Prelude.NonEmpty ScheduledSplitConfig ->
  ScheduledSplitsLaunchConfig
newScheduledSplitsLaunchConfig pSteps_ =
  ScheduledSplitsLaunchConfig'
    { steps =
        Lens.coerced Lens.# pSteps_
    }

-- | An array of structures that define the traffic allocation percentages
-- among the feature variations during each step of the launch. This also
-- defines the start time of each step.
scheduledSplitsLaunchConfig_steps :: Lens.Lens' ScheduledSplitsLaunchConfig (Prelude.NonEmpty ScheduledSplitConfig)
scheduledSplitsLaunchConfig_steps = Lens.lens (\ScheduledSplitsLaunchConfig' {steps} -> steps) (\s@ScheduledSplitsLaunchConfig' {} a -> s {steps = a} :: ScheduledSplitsLaunchConfig) Prelude.. Lens.coerced

instance Prelude.Hashable ScheduledSplitsLaunchConfig where
  hashWithSalt _salt ScheduledSplitsLaunchConfig' {..} =
    _salt `Prelude.hashWithSalt` steps

instance Prelude.NFData ScheduledSplitsLaunchConfig where
  rnf ScheduledSplitsLaunchConfig' {..} =
    Prelude.rnf steps

instance Data.ToJSON ScheduledSplitsLaunchConfig where
  toJSON ScheduledSplitsLaunchConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("steps" Data..= steps)]
      )
