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
-- Module      : Amazonka.Pinpoint.Types.WaitActivity
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pinpoint.Types.WaitActivity where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Pinpoint.Types.WaitTime
import qualified Amazonka.Prelude as Prelude

-- | Specifies the settings for a wait activity in a journey. This type of
-- activity waits for a certain amount of time or until a specific date and
-- time before moving participants to the next activity in a journey.
--
-- /See:/ 'newWaitActivity' smart constructor.
data WaitActivity = WaitActivity'
  { -- | The unique identifier for the next activity to perform, after performing
    -- the wait activity.
    nextActivity :: Prelude.Maybe Prelude.Text,
    -- | The amount of time to wait or the date and time when the activity moves
    -- participants to the next activity in the journey.
    waitTime :: Prelude.Maybe WaitTime
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'WaitActivity' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextActivity', 'waitActivity_nextActivity' - The unique identifier for the next activity to perform, after performing
-- the wait activity.
--
-- 'waitTime', 'waitActivity_waitTime' - The amount of time to wait or the date and time when the activity moves
-- participants to the next activity in the journey.
newWaitActivity ::
  WaitActivity
newWaitActivity =
  WaitActivity'
    { nextActivity = Prelude.Nothing,
      waitTime = Prelude.Nothing
    }

-- | The unique identifier for the next activity to perform, after performing
-- the wait activity.
waitActivity_nextActivity :: Lens.Lens' WaitActivity (Prelude.Maybe Prelude.Text)
waitActivity_nextActivity = Lens.lens (\WaitActivity' {nextActivity} -> nextActivity) (\s@WaitActivity' {} a -> s {nextActivity = a} :: WaitActivity)

-- | The amount of time to wait or the date and time when the activity moves
-- participants to the next activity in the journey.
waitActivity_waitTime :: Lens.Lens' WaitActivity (Prelude.Maybe WaitTime)
waitActivity_waitTime = Lens.lens (\WaitActivity' {waitTime} -> waitTime) (\s@WaitActivity' {} a -> s {waitTime = a} :: WaitActivity)

instance Core.FromJSON WaitActivity where
  parseJSON =
    Core.withObject
      "WaitActivity"
      ( \x ->
          WaitActivity'
            Prelude.<$> (x Core..:? "NextActivity")
            Prelude.<*> (x Core..:? "WaitTime")
      )

instance Prelude.Hashable WaitActivity where
  hashWithSalt _salt WaitActivity' {..} =
    _salt `Prelude.hashWithSalt` nextActivity
      `Prelude.hashWithSalt` waitTime

instance Prelude.NFData WaitActivity where
  rnf WaitActivity' {..} =
    Prelude.rnf nextActivity
      `Prelude.seq` Prelude.rnf waitTime

instance Core.ToJSON WaitActivity where
  toJSON WaitActivity' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextActivity" Core..=) Prelude.<$> nextActivity,
            ("WaitTime" Core..=) Prelude.<$> waitTime
          ]
      )
