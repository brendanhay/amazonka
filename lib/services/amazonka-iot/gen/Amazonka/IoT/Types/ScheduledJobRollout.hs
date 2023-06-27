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
-- Module      : Amazonka.IoT.Types.ScheduledJobRollout
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.ScheduledJobRollout where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Displays the next seven maintenance window occurrences and their start
-- times.
--
-- /See:/ 'newScheduledJobRollout' smart constructor.
data ScheduledJobRollout = ScheduledJobRollout'
  { -- | Displays the start times of the next seven maintenance window
    -- occurrences.
    startTime :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ScheduledJobRollout' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'startTime', 'scheduledJobRollout_startTime' - Displays the start times of the next seven maintenance window
-- occurrences.
newScheduledJobRollout ::
  ScheduledJobRollout
newScheduledJobRollout =
  ScheduledJobRollout' {startTime = Prelude.Nothing}

-- | Displays the start times of the next seven maintenance window
-- occurrences.
scheduledJobRollout_startTime :: Lens.Lens' ScheduledJobRollout (Prelude.Maybe Prelude.Text)
scheduledJobRollout_startTime = Lens.lens (\ScheduledJobRollout' {startTime} -> startTime) (\s@ScheduledJobRollout' {} a -> s {startTime = a} :: ScheduledJobRollout)

instance Data.FromJSON ScheduledJobRollout where
  parseJSON =
    Data.withObject
      "ScheduledJobRollout"
      ( \x ->
          ScheduledJobRollout'
            Prelude.<$> (x Data..:? "startTime")
      )

instance Prelude.Hashable ScheduledJobRollout where
  hashWithSalt _salt ScheduledJobRollout' {..} =
    _salt `Prelude.hashWithSalt` startTime

instance Prelude.NFData ScheduledJobRollout where
  rnf ScheduledJobRollout' {..} = Prelude.rnf startTime
