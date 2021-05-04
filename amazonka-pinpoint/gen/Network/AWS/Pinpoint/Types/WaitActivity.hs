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
-- Module      : Network.AWS.Pinpoint.Types.WaitActivity
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.WaitActivity where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types.WaitTime
import qualified Network.AWS.Prelude as Prelude

-- | Specifies the settings for a wait activity in a journey. This type of
-- activity waits for a certain amount of time or until a specific date and
-- time before moving participants to the next activity in a journey.
--
-- /See:/ 'newWaitActivity' smart constructor.
data WaitActivity = WaitActivity'
  { -- | The amount of time to wait or the date and time when the activity moves
    -- participants to the next activity in the journey.
    waitTime :: Prelude.Maybe WaitTime,
    -- | The unique identifier for the next activity to perform, after performing
    -- the wait activity.
    nextActivity :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'WaitActivity' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'waitTime', 'waitActivity_waitTime' - The amount of time to wait or the date and time when the activity moves
-- participants to the next activity in the journey.
--
-- 'nextActivity', 'waitActivity_nextActivity' - The unique identifier for the next activity to perform, after performing
-- the wait activity.
newWaitActivity ::
  WaitActivity
newWaitActivity =
  WaitActivity'
    { waitTime = Prelude.Nothing,
      nextActivity = Prelude.Nothing
    }

-- | The amount of time to wait or the date and time when the activity moves
-- participants to the next activity in the journey.
waitActivity_waitTime :: Lens.Lens' WaitActivity (Prelude.Maybe WaitTime)
waitActivity_waitTime = Lens.lens (\WaitActivity' {waitTime} -> waitTime) (\s@WaitActivity' {} a -> s {waitTime = a} :: WaitActivity)

-- | The unique identifier for the next activity to perform, after performing
-- the wait activity.
waitActivity_nextActivity :: Lens.Lens' WaitActivity (Prelude.Maybe Prelude.Text)
waitActivity_nextActivity = Lens.lens (\WaitActivity' {nextActivity} -> nextActivity) (\s@WaitActivity' {} a -> s {nextActivity = a} :: WaitActivity)

instance Prelude.FromJSON WaitActivity where
  parseJSON =
    Prelude.withObject
      "WaitActivity"
      ( \x ->
          WaitActivity'
            Prelude.<$> (x Prelude..:? "WaitTime")
            Prelude.<*> (x Prelude..:? "NextActivity")
      )

instance Prelude.Hashable WaitActivity

instance Prelude.NFData WaitActivity

instance Prelude.ToJSON WaitActivity where
  toJSON WaitActivity' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("WaitTime" Prelude..=) Prelude.<$> waitTime,
            ("NextActivity" Prelude..=)
              Prelude.<$> nextActivity
          ]
      )
