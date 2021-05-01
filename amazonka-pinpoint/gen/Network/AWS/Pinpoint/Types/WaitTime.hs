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
-- Module      : Network.AWS.Pinpoint.Types.WaitTime
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.WaitTime where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Specifies a duration or a date and time that indicates when Amazon
-- Pinpoint determines whether an activity\'s conditions have been met or
-- an activity moves participants to the next activity in a journey.
--
-- /See:/ 'newWaitTime' smart constructor.
data WaitTime = WaitTime'
  { -- | The date and time, in ISO 8601 format, when Amazon Pinpoint determines
    -- whether the activity\'s conditions have been met or the activity moves
    -- participants to the next activity in the journey.
    waitUntil :: Prelude.Maybe Prelude.Text,
    -- | The amount of time to wait, as a duration in ISO 8601 format, before
    -- determining whether the activity\'s conditions have been met or moving
    -- participants to the next activity in the journey.
    waitFor :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'WaitTime' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'waitUntil', 'waitTime_waitUntil' - The date and time, in ISO 8601 format, when Amazon Pinpoint determines
-- whether the activity\'s conditions have been met or the activity moves
-- participants to the next activity in the journey.
--
-- 'waitFor', 'waitTime_waitFor' - The amount of time to wait, as a duration in ISO 8601 format, before
-- determining whether the activity\'s conditions have been met or moving
-- participants to the next activity in the journey.
newWaitTime ::
  WaitTime
newWaitTime =
  WaitTime'
    { waitUntil = Prelude.Nothing,
      waitFor = Prelude.Nothing
    }

-- | The date and time, in ISO 8601 format, when Amazon Pinpoint determines
-- whether the activity\'s conditions have been met or the activity moves
-- participants to the next activity in the journey.
waitTime_waitUntil :: Lens.Lens' WaitTime (Prelude.Maybe Prelude.Text)
waitTime_waitUntil = Lens.lens (\WaitTime' {waitUntil} -> waitUntil) (\s@WaitTime' {} a -> s {waitUntil = a} :: WaitTime)

-- | The amount of time to wait, as a duration in ISO 8601 format, before
-- determining whether the activity\'s conditions have been met or moving
-- participants to the next activity in the journey.
waitTime_waitFor :: Lens.Lens' WaitTime (Prelude.Maybe Prelude.Text)
waitTime_waitFor = Lens.lens (\WaitTime' {waitFor} -> waitFor) (\s@WaitTime' {} a -> s {waitFor = a} :: WaitTime)

instance Prelude.FromJSON WaitTime where
  parseJSON =
    Prelude.withObject
      "WaitTime"
      ( \x ->
          WaitTime'
            Prelude.<$> (x Prelude..:? "WaitUntil")
            Prelude.<*> (x Prelude..:? "WaitFor")
      )

instance Prelude.Hashable WaitTime

instance Prelude.NFData WaitTime

instance Prelude.ToJSON WaitTime where
  toJSON WaitTime' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("WaitUntil" Prelude..=) Prelude.<$> waitUntil,
            ("WaitFor" Prelude..=) Prelude.<$> waitFor
          ]
      )
