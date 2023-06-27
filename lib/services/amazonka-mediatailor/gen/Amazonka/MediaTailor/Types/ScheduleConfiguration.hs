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
-- Module      : Amazonka.MediaTailor.Types.ScheduleConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaTailor.Types.ScheduleConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaTailor.Types.ClipRange
import Amazonka.MediaTailor.Types.Transition
import qualified Amazonka.Prelude as Prelude

-- | Schedule configuration parameters. A channel must be stopped before
-- changes can be made to the schedule.
--
-- /See:/ 'newScheduleConfiguration' smart constructor.
data ScheduleConfiguration = ScheduleConfiguration'
  { -- | Program clip range configuration.
    clipRange :: Prelude.Maybe ClipRange,
    -- | Program transition configurations.
    transition :: Transition
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ScheduleConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clipRange', 'scheduleConfiguration_clipRange' - Program clip range configuration.
--
-- 'transition', 'scheduleConfiguration_transition' - Program transition configurations.
newScheduleConfiguration ::
  -- | 'transition'
  Transition ->
  ScheduleConfiguration
newScheduleConfiguration pTransition_ =
  ScheduleConfiguration'
    { clipRange = Prelude.Nothing,
      transition = pTransition_
    }

-- | Program clip range configuration.
scheduleConfiguration_clipRange :: Lens.Lens' ScheduleConfiguration (Prelude.Maybe ClipRange)
scheduleConfiguration_clipRange = Lens.lens (\ScheduleConfiguration' {clipRange} -> clipRange) (\s@ScheduleConfiguration' {} a -> s {clipRange = a} :: ScheduleConfiguration)

-- | Program transition configurations.
scheduleConfiguration_transition :: Lens.Lens' ScheduleConfiguration Transition
scheduleConfiguration_transition = Lens.lens (\ScheduleConfiguration' {transition} -> transition) (\s@ScheduleConfiguration' {} a -> s {transition = a} :: ScheduleConfiguration)

instance Prelude.Hashable ScheduleConfiguration where
  hashWithSalt _salt ScheduleConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` clipRange
      `Prelude.hashWithSalt` transition

instance Prelude.NFData ScheduleConfiguration where
  rnf ScheduleConfiguration' {..} =
    Prelude.rnf clipRange
      `Prelude.seq` Prelude.rnf transition

instance Data.ToJSON ScheduleConfiguration where
  toJSON ScheduleConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ClipRange" Data..=) Prelude.<$> clipRange,
            Prelude.Just ("Transition" Data..= transition)
          ]
      )
