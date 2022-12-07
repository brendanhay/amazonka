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
-- Module      : Amazonka.Evidently.Types.ScheduledSplitsLaunchDefinition
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Evidently.Types.ScheduledSplitsLaunchDefinition where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Evidently.Types.ScheduledSplit
import qualified Amazonka.Prelude as Prelude

-- | An array of structures that define the traffic allocation percentages
-- among the feature variations during each step of a launch. This also
-- defines the start time of each step.
--
-- /See:/ 'newScheduledSplitsLaunchDefinition' smart constructor.
data ScheduledSplitsLaunchDefinition = ScheduledSplitsLaunchDefinition'
  { -- | An array of structures that define the traffic allocation percentages
    -- among the feature variations during each step of the launch. This also
    -- defines the start time of each step.
    steps :: Prelude.Maybe (Prelude.NonEmpty ScheduledSplit)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ScheduledSplitsLaunchDefinition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'steps', 'scheduledSplitsLaunchDefinition_steps' - An array of structures that define the traffic allocation percentages
-- among the feature variations during each step of the launch. This also
-- defines the start time of each step.
newScheduledSplitsLaunchDefinition ::
  ScheduledSplitsLaunchDefinition
newScheduledSplitsLaunchDefinition =
  ScheduledSplitsLaunchDefinition'
    { steps =
        Prelude.Nothing
    }

-- | An array of structures that define the traffic allocation percentages
-- among the feature variations during each step of the launch. This also
-- defines the start time of each step.
scheduledSplitsLaunchDefinition_steps :: Lens.Lens' ScheduledSplitsLaunchDefinition (Prelude.Maybe (Prelude.NonEmpty ScheduledSplit))
scheduledSplitsLaunchDefinition_steps = Lens.lens (\ScheduledSplitsLaunchDefinition' {steps} -> steps) (\s@ScheduledSplitsLaunchDefinition' {} a -> s {steps = a} :: ScheduledSplitsLaunchDefinition) Prelude.. Lens.mapping Lens.coerced

instance
  Data.FromJSON
    ScheduledSplitsLaunchDefinition
  where
  parseJSON =
    Data.withObject
      "ScheduledSplitsLaunchDefinition"
      ( \x ->
          ScheduledSplitsLaunchDefinition'
            Prelude.<$> (x Data..:? "steps")
      )

instance
  Prelude.Hashable
    ScheduledSplitsLaunchDefinition
  where
  hashWithSalt
    _salt
    ScheduledSplitsLaunchDefinition' {..} =
      _salt `Prelude.hashWithSalt` steps

instance
  Prelude.NFData
    ScheduledSplitsLaunchDefinition
  where
  rnf ScheduledSplitsLaunchDefinition' {..} =
    Prelude.rnf steps
