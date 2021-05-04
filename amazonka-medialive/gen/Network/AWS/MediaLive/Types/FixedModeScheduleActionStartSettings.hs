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
-- Module      : Network.AWS.MediaLive.Types.FixedModeScheduleActionStartSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.FixedModeScheduleActionStartSettings where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Start time for the action.
--
-- /See:/ 'newFixedModeScheduleActionStartSettings' smart constructor.
data FixedModeScheduleActionStartSettings = FixedModeScheduleActionStartSettings'
  { -- | Start time for the action to start in the channel. (Not the time for the
    -- action to be added to the schedule: actions are always added to the
    -- schedule immediately.) UTC format: yyyy-mm-ddThh:mm:ss.nnnZ. All the
    -- letters are digits (for example, mm might be 01) except for the two
    -- constants \"T\" for time and \"Z\" for \"UTC format\".
    time :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'FixedModeScheduleActionStartSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'time', 'fixedModeScheduleActionStartSettings_time' - Start time for the action to start in the channel. (Not the time for the
-- action to be added to the schedule: actions are always added to the
-- schedule immediately.) UTC format: yyyy-mm-ddThh:mm:ss.nnnZ. All the
-- letters are digits (for example, mm might be 01) except for the two
-- constants \"T\" for time and \"Z\" for \"UTC format\".
newFixedModeScheduleActionStartSettings ::
  -- | 'time'
  Prelude.Text ->
  FixedModeScheduleActionStartSettings
newFixedModeScheduleActionStartSettings pTime_ =
  FixedModeScheduleActionStartSettings'
    { time =
        pTime_
    }

-- | Start time for the action to start in the channel. (Not the time for the
-- action to be added to the schedule: actions are always added to the
-- schedule immediately.) UTC format: yyyy-mm-ddThh:mm:ss.nnnZ. All the
-- letters are digits (for example, mm might be 01) except for the two
-- constants \"T\" for time and \"Z\" for \"UTC format\".
fixedModeScheduleActionStartSettings_time :: Lens.Lens' FixedModeScheduleActionStartSettings Prelude.Text
fixedModeScheduleActionStartSettings_time = Lens.lens (\FixedModeScheduleActionStartSettings' {time} -> time) (\s@FixedModeScheduleActionStartSettings' {} a -> s {time = a} :: FixedModeScheduleActionStartSettings)

instance
  Prelude.FromJSON
    FixedModeScheduleActionStartSettings
  where
  parseJSON =
    Prelude.withObject
      "FixedModeScheduleActionStartSettings"
      ( \x ->
          FixedModeScheduleActionStartSettings'
            Prelude.<$> (x Prelude..: "time")
      )

instance
  Prelude.Hashable
    FixedModeScheduleActionStartSettings

instance
  Prelude.NFData
    FixedModeScheduleActionStartSettings

instance
  Prelude.ToJSON
    FixedModeScheduleActionStartSettings
  where
  toJSON FixedModeScheduleActionStartSettings' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [Prelude.Just ("time" Prelude..= time)]
      )
