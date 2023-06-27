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
-- Module      : Amazonka.MediaTailor.Types.UpdateProgramTransition
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaTailor.Types.UpdateProgramTransition where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Program transition configuration.
--
-- /See:/ 'newUpdateProgramTransition' smart constructor.
data UpdateProgramTransition = UpdateProgramTransition'
  { -- | The duration of the live program in seconds.
    durationMillis :: Prelude.Maybe Prelude.Integer,
    -- | The date and time that the program is scheduled to start, in epoch
    -- milliseconds.
    scheduledStartTimeMillis :: Prelude.Maybe Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateProgramTransition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'durationMillis', 'updateProgramTransition_durationMillis' - The duration of the live program in seconds.
--
-- 'scheduledStartTimeMillis', 'updateProgramTransition_scheduledStartTimeMillis' - The date and time that the program is scheduled to start, in epoch
-- milliseconds.
newUpdateProgramTransition ::
  UpdateProgramTransition
newUpdateProgramTransition =
  UpdateProgramTransition'
    { durationMillis =
        Prelude.Nothing,
      scheduledStartTimeMillis = Prelude.Nothing
    }

-- | The duration of the live program in seconds.
updateProgramTransition_durationMillis :: Lens.Lens' UpdateProgramTransition (Prelude.Maybe Prelude.Integer)
updateProgramTransition_durationMillis = Lens.lens (\UpdateProgramTransition' {durationMillis} -> durationMillis) (\s@UpdateProgramTransition' {} a -> s {durationMillis = a} :: UpdateProgramTransition)

-- | The date and time that the program is scheduled to start, in epoch
-- milliseconds.
updateProgramTransition_scheduledStartTimeMillis :: Lens.Lens' UpdateProgramTransition (Prelude.Maybe Prelude.Integer)
updateProgramTransition_scheduledStartTimeMillis = Lens.lens (\UpdateProgramTransition' {scheduledStartTimeMillis} -> scheduledStartTimeMillis) (\s@UpdateProgramTransition' {} a -> s {scheduledStartTimeMillis = a} :: UpdateProgramTransition)

instance Prelude.Hashable UpdateProgramTransition where
  hashWithSalt _salt UpdateProgramTransition' {..} =
    _salt
      `Prelude.hashWithSalt` durationMillis
      `Prelude.hashWithSalt` scheduledStartTimeMillis

instance Prelude.NFData UpdateProgramTransition where
  rnf UpdateProgramTransition' {..} =
    Prelude.rnf durationMillis
      `Prelude.seq` Prelude.rnf scheduledStartTimeMillis

instance Data.ToJSON UpdateProgramTransition where
  toJSON UpdateProgramTransition' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DurationMillis" Data..=)
              Prelude.<$> durationMillis,
            ("ScheduledStartTimeMillis" Data..=)
              Prelude.<$> scheduledStartTimeMillis
          ]
      )
