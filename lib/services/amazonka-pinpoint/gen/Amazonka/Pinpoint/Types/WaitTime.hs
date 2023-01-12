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
-- Module      : Amazonka.Pinpoint.Types.WaitTime
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pinpoint.Types.WaitTime where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specifies a duration or a date and time that indicates when Amazon
-- Pinpoint determines whether an activity\'s conditions have been met or
-- an activity moves participants to the next activity in a journey.
--
-- /See:/ 'newWaitTime' smart constructor.
data WaitTime = WaitTime'
  { -- | The amount of time to wait, as a duration in ISO 8601 format, before
    -- determining whether the activity\'s conditions have been met or moving
    -- participants to the next activity in the journey.
    waitFor :: Prelude.Maybe Prelude.Text,
    -- | The date and time, in ISO 8601 format, when Amazon Pinpoint determines
    -- whether the activity\'s conditions have been met or the activity moves
    -- participants to the next activity in the journey.
    waitUntil :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'WaitTime' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'waitFor', 'waitTime_waitFor' - The amount of time to wait, as a duration in ISO 8601 format, before
-- determining whether the activity\'s conditions have been met or moving
-- participants to the next activity in the journey.
--
-- 'waitUntil', 'waitTime_waitUntil' - The date and time, in ISO 8601 format, when Amazon Pinpoint determines
-- whether the activity\'s conditions have been met or the activity moves
-- participants to the next activity in the journey.
newWaitTime ::
  WaitTime
newWaitTime =
  WaitTime'
    { waitFor = Prelude.Nothing,
      waitUntil = Prelude.Nothing
    }

-- | The amount of time to wait, as a duration in ISO 8601 format, before
-- determining whether the activity\'s conditions have been met or moving
-- participants to the next activity in the journey.
waitTime_waitFor :: Lens.Lens' WaitTime (Prelude.Maybe Prelude.Text)
waitTime_waitFor = Lens.lens (\WaitTime' {waitFor} -> waitFor) (\s@WaitTime' {} a -> s {waitFor = a} :: WaitTime)

-- | The date and time, in ISO 8601 format, when Amazon Pinpoint determines
-- whether the activity\'s conditions have been met or the activity moves
-- participants to the next activity in the journey.
waitTime_waitUntil :: Lens.Lens' WaitTime (Prelude.Maybe Prelude.Text)
waitTime_waitUntil = Lens.lens (\WaitTime' {waitUntil} -> waitUntil) (\s@WaitTime' {} a -> s {waitUntil = a} :: WaitTime)

instance Data.FromJSON WaitTime where
  parseJSON =
    Data.withObject
      "WaitTime"
      ( \x ->
          WaitTime'
            Prelude.<$> (x Data..:? "WaitFor")
            Prelude.<*> (x Data..:? "WaitUntil")
      )

instance Prelude.Hashable WaitTime where
  hashWithSalt _salt WaitTime' {..} =
    _salt `Prelude.hashWithSalt` waitFor
      `Prelude.hashWithSalt` waitUntil

instance Prelude.NFData WaitTime where
  rnf WaitTime' {..} =
    Prelude.rnf waitFor
      `Prelude.seq` Prelude.rnf waitUntil

instance Data.ToJSON WaitTime where
  toJSON WaitTime' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("WaitFor" Data..=) Prelude.<$> waitFor,
            ("WaitUntil" Data..=) Prelude.<$> waitUntil
          ]
      )
