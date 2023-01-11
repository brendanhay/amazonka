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
-- Module      : Amazonka.MediaLive.Types.Scte35SpliceInsertScheduleActionSettings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.Scte35SpliceInsertScheduleActionSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Settings for a SCTE-35 splice_insert message.
--
-- /See:/ 'newScte35SpliceInsertScheduleActionSettings' smart constructor.
data Scte35SpliceInsertScheduleActionSettings = Scte35SpliceInsertScheduleActionSettings'
  { -- | Optional, the duration for the splice_insert, in 90 KHz ticks. To
    -- convert seconds to ticks, multiple the seconds by 90,000. If you enter a
    -- duration, there is an expectation that the downstream system can read
    -- the duration and cue in at that time. If you do not enter a duration,
    -- the splice_insert will continue indefinitely and there is an expectation
    -- that you will enter a return_to_network to end the splice_insert at the
    -- appropriate time.
    duration :: Prelude.Maybe Prelude.Natural,
    -- | The splice_event_id for the SCTE-35 splice_insert, as defined in
    -- SCTE-35.
    spliceEventId :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Scte35SpliceInsertScheduleActionSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'duration', 'scte35SpliceInsertScheduleActionSettings_duration' - Optional, the duration for the splice_insert, in 90 KHz ticks. To
-- convert seconds to ticks, multiple the seconds by 90,000. If you enter a
-- duration, there is an expectation that the downstream system can read
-- the duration and cue in at that time. If you do not enter a duration,
-- the splice_insert will continue indefinitely and there is an expectation
-- that you will enter a return_to_network to end the splice_insert at the
-- appropriate time.
--
-- 'spliceEventId', 'scte35SpliceInsertScheduleActionSettings_spliceEventId' - The splice_event_id for the SCTE-35 splice_insert, as defined in
-- SCTE-35.
newScte35SpliceInsertScheduleActionSettings ::
  -- | 'spliceEventId'
  Prelude.Natural ->
  Scte35SpliceInsertScheduleActionSettings
newScte35SpliceInsertScheduleActionSettings
  pSpliceEventId_ =
    Scte35SpliceInsertScheduleActionSettings'
      { duration =
          Prelude.Nothing,
        spliceEventId = pSpliceEventId_
      }

-- | Optional, the duration for the splice_insert, in 90 KHz ticks. To
-- convert seconds to ticks, multiple the seconds by 90,000. If you enter a
-- duration, there is an expectation that the downstream system can read
-- the duration and cue in at that time. If you do not enter a duration,
-- the splice_insert will continue indefinitely and there is an expectation
-- that you will enter a return_to_network to end the splice_insert at the
-- appropriate time.
scte35SpliceInsertScheduleActionSettings_duration :: Lens.Lens' Scte35SpliceInsertScheduleActionSettings (Prelude.Maybe Prelude.Natural)
scte35SpliceInsertScheduleActionSettings_duration = Lens.lens (\Scte35SpliceInsertScheduleActionSettings' {duration} -> duration) (\s@Scte35SpliceInsertScheduleActionSettings' {} a -> s {duration = a} :: Scte35SpliceInsertScheduleActionSettings)

-- | The splice_event_id for the SCTE-35 splice_insert, as defined in
-- SCTE-35.
scte35SpliceInsertScheduleActionSettings_spliceEventId :: Lens.Lens' Scte35SpliceInsertScheduleActionSettings Prelude.Natural
scte35SpliceInsertScheduleActionSettings_spliceEventId = Lens.lens (\Scte35SpliceInsertScheduleActionSettings' {spliceEventId} -> spliceEventId) (\s@Scte35SpliceInsertScheduleActionSettings' {} a -> s {spliceEventId = a} :: Scte35SpliceInsertScheduleActionSettings)

instance
  Data.FromJSON
    Scte35SpliceInsertScheduleActionSettings
  where
  parseJSON =
    Data.withObject
      "Scte35SpliceInsertScheduleActionSettings"
      ( \x ->
          Scte35SpliceInsertScheduleActionSettings'
            Prelude.<$> (x Data..:? "duration")
            Prelude.<*> (x Data..: "spliceEventId")
      )

instance
  Prelude.Hashable
    Scte35SpliceInsertScheduleActionSettings
  where
  hashWithSalt
    _salt
    Scte35SpliceInsertScheduleActionSettings' {..} =
      _salt `Prelude.hashWithSalt` duration
        `Prelude.hashWithSalt` spliceEventId

instance
  Prelude.NFData
    Scte35SpliceInsertScheduleActionSettings
  where
  rnf Scte35SpliceInsertScheduleActionSettings' {..} =
    Prelude.rnf duration
      `Prelude.seq` Prelude.rnf spliceEventId

instance
  Data.ToJSON
    Scte35SpliceInsertScheduleActionSettings
  where
  toJSON Scte35SpliceInsertScheduleActionSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("duration" Data..=) Prelude.<$> duration,
            Prelude.Just
              ("spliceEventId" Data..= spliceEventId)
          ]
      )
